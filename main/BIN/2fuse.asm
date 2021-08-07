.model small
.stack 128

;=======================================================================
; CONSTANTS
;=======================================================================
VIDEO_PALLETE_PORT = 3C8h
COLOR_SELECTION_PORT = 3C9h
PALLETE_INDEX_BACKGROUND = 0
SET_VIDEO_MODE = 0
GET_VIDEO_MODE = 0Fh
VIDE0_SEGMENT = 0A000h
WAIT_FOR_KEYSTROKE = 10h
MODE_13 = 13h

;=======================================================================
; DATA SEGMENT
;=======================================================================
.data

;=======================================================================
; COORDINATES VARIABLES
;=======================================================================
xcoor dw 0
ycoor dw 0
ctr1 db 0
delays dw 0ffffh
COLOR_INDEX db 1fh
xDim dw 0
yDim dw 0
xSprite dw 0
ySprite dw 0
colorBox db 0 ;"r" = red, "g" = green, "b" = blue
divisor db 0
randomNum db 0
randomColor db 0

c11 db 90, 50, 0, 0, 11
c12 db 125, 50, 0, 0, 12
c13 db 160, 50, 0, 0, 13
c14 db 195, 50, 0, 0, 14
c21 db 90, 85, 0, 0, 21
c22 db 125, 85, 0, 0, 22
c23 db 160, 85, 0, 0, 23
c24 db 195, 85, 0, 0, 24
c31 db 90, 120, 0, 0, 31
c32 db 125, 120, 0, 0, 32
c33 db 160, 120, 0, 0, 33
c34 db 195, 120, 0, 0, 34
c41 db 90, 155, 0, 0, 41
c42 db 125, 155, 0, 0, 42
c43 db 160, 155, 0, 0, 43
c44 db 195, 155, 0, 0, 44

;=======================================================================
; RANDOM VARIABLES
;=======================================================================
randomRed db 0
randomGreen db 0
randomBlue db 0
recentRed db 0
recentGreen db 0
recentBlue db 0
boxBGColor db 0
boxType db 0
boxColor db 0 ;0 = Don't Change Color, 1 = Change Color
boxBGChecker db 0, 0;xCoor, yCoor
boxChecker db 0, 0 ;xCoor, yCoor
prevBox db 0, 0 ;Type, Color
tempBox db 0, 0 ;Type, Color
mouseButton dw 0
mouseRow dw 0
mouseCol dw 0
prevTick dw 0
currTick dw 0

;===============================================================================
; FILE HANDLING VARIABLES
;===============================================================================
errorOpenStr db "Error open$"
errorReadStr db "Error read$"
errorWriteStr db "Error write$"
errorCloseStr db "Error close$"
errorCreateStr db "Error create$"

filename db "hs2fuse.txt", 0
filehandle dw ?
bufferread db 3 dup ("0")
HighScoreString db "0", "5", "0", "0"
highScoreNum db 3 dup ("0")

scoresNum dw 0
highscoreName db "HIGHSCORE: ", 0
myscoreName db "MYSCORE: ", 0
timeName db "TIME", 0
hsNum dw 0
msNum dw 0
timeNum dw 60

timesUp db "TIME IS UP", 0
playAgain db "PLAY AGAIN", 0
exitGameNow db "EXIT", 0
gameOverColor db 1Fh

;=======================================================================
; GUI SPRITES VARIABLES
;=======================================================================
star db 0
db 35 dup (10h)
db 35 dup (10h)
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,15 dup (69),10h,15 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,10h,10h,14 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,2Ch,10h,14 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,2Ch,10h,14 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,2Ch,10h,14 dup (69),10h,10h
db 10h,10h,13 dup (69),10h,2Ch,2Ch,2Ch,10h,13 dup (69),10h,10h
db 10h,10h,13 dup (69),10h,2Ch,2Ch,2Ch,10h,13 dup (69),10h,10h
db 10h,10h,13 dup (69),10h,2Ch,2Ch,2Ch,10h,13 dup (69),10h,10h
db 10h,10h,12 dup (69),10h,5 dup (2Ch),10h,12 dup (69),10h,10h
db 10h,10h,12 dup (69),10h,5 dup (2Ch),10h,12 dup (69),10h,10h
db 10h,10h,69,69,11 dup (10h),5 dup (2Ch),11 dup (10h),69,69,10h,10h
db 10h,10h,69,69,69,10h,23 dup (2Ch),10h,69,69,69,10h,10h
db 10h,10h,69,69,69,69,10h,10h,19 dup (2Ch),10h,10h,69,69,69,69,10h,10h
db 10h,10h,6 dup (69),10h,17 dup (2Ch),10h,6 dup (69),10h,10h
db 10h,10h,7 dup (69),10h,15 dup (2Ch),10h,7 dup (69),10h,10h
db 10h,10h,8 dup (69),10h,13 dup (2Ch),10h,8 dup (69),10h,10h
db 10h,10h,9 dup (69),10h,10h,9 dup (2Ch),10h,10h,9 dup (69),10h,10h
db 10h,10h,10 dup (69),10h,9 dup (2Ch),10h,10 dup (69),10h,10h
db 10h,10h,9 dup (69),10h,11 dup (2Ch),10h,9 dup (69),10h,10h
db 10h,10h,9 dup (69),10h,11 dup (2Ch),10h,9 dup (69),10h,10h
db 10h,10h,9 dup (69),10h,5 dup (2Ch),10h,5 dup (2Ch),10h,9 dup (69),10h,10h
db 10h,10h,8 dup (69),10h,5 dup (2Ch),10h,69,10h,5 dup (2Ch),10h,8 dup (69),10h,10h
db 10h,10h,8 dup (69),10h,2Ch,2Ch,2Ch,10h,10h,69,69,69,10h,10h,2Ch,2Ch,2Ch,10h,8 dup (69),10h,10h
db 10h,10h,8 dup (69),10h,2Ch,2Ch,10h,7 dup (69),10h,2Ch,2Ch,10h,8 dup (69),10h,10h
db 10h,10h,8 dup (69),10h,2Ch,10h,9 dup (69),10h,2Ch,10h,8 dup (69),10h,10h
db 10h,10h,7 dup (69),10h,10h,10h,11 dup (69),10h,10h,10h,7 dup (69),10h,10h
db 10h,10h,7 dup (69),10h,15 dup (69),10h,7 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 35 dup (10h)
db 35 dup (10h)

one db 0
db 35 dup (10h)
db 35 dup (10h)
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,10 dup (69),11 dup (10h),10 dup (69),10h,10h
db 10h,10h,10 dup (69),10h,9 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,10 dup (69),10h,9 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,10 dup (69),10h,9 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,10 dup (69),10h,9 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,10 dup (69),10h,9 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,10 dup (69),5 dup (10h),5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),10h,5 dup (1Fh),10h,10 dup (69),10h,10h
db 10h,10h,14 dup (69),7 dup (10h),10 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 35 dup (10h)
db 35 dup (10h)

two db 0
db 35 dup (10h)
db 35 dup (10h)
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,5 dup (69),21 dup (10h),5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),13 dup (10h),7 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,17 dup (69),10h,7 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,17 dup (69),10h,7 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),13 dup (10h),7 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h, 7 dup (1Fh),12 dup (10h),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,7 dup (1Fh),10h,17 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,7 dup (1Fh),10h,17 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,7 dup (1Fh),13 dup (10h),5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),10h,19 dup (1Fh),10h,5 dup (69),10h,10h
db 10h,10h,5 dup (69),21 dup (10h),5 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 10h,10h,31 dup (69),10h,10h
db 35 dup (10h)
db 35 dup (10h)

.386
.code

;--------------------------------------------------------------------------------
;COLOR MACRO
;--------------------------------------------------------------------------------
color macro rcolor, gcolor, bcolor
	pusha
	mov dx, VIDEO_PALLETE_PORT
	mov al, COLOR_INDEX
	out dx, al
	mov dx,COLOR_SELECTION_PORT
	mov al, rcolor ; red
	out dx,al
	mov al, gcolor ; green
	out dx,al
	mov al,bcolor ; blue
	out dx,al
	popa
endm

;--------------------------------------------------------------------------------
;INITIALIZE SPRITES MACROS
;--------------------------------------------------------------------------------
InitializeSprite macro xVal, yVal, spriteW, spriteH ; X-coor position, Y-coor position, X-dimension, Y-dimension
	pusha
		mov ax, xVal 
		mov xDim, ax
		mov ax, yVal 
		mov yDim, ax
		mov ax, spriteW
		mov xSprite, ax
		mov ax, spriteH
		mov ySprite, ax
	popa
endm


;--------------------------------------------------------------------------------
;MAIN PROCEDURE
;--------------------------------------------------------------------------------
main proc far
	_begin:
		mov ax, @data
		mov ds, ax
		call SetVideoMode
		call SetScreenBackground
		call background
		call initializeNames 
		call initializeBox
		call mouseInt
		call endGame
main endp

;=======================================================================
; INITIALIZE GAMES PROCEDURE
;=======================================================================
initializeGame proc
	pop es

	mov randomRed, 0
	mov randomGreen, 0
	mov randomBlue, 0
	mov recentRed, 0
	mov recentGreen, 0
	mov recentBlue, 0
	mov boxBGColor, 0
	mov boxType, 0
	mov boxColor, 0 ;0 = Don't Change Color, 1 = Change Color	
	mov hsNum, 0
	mov msNum, 0
	mov timeNum, 60
	mov gameOverColor, 1Fh

	call SetVideoMode
	call SetScreenBackground
	call background
	call initializeNames 
	call initializeBox
	call mouseInt
initializeGame endp

;--------------------------------------------------------------------------------
; SETTING THE VIDEO MODE
; Saves the current video mode, switches to a
; new mode, and points ES to the video segment.
;--------------------------------------------------------------------------------
SetVideoMode PROC
	mov ah, SET_VIDEO_MODE
	mov al, MODE_13 ; to mode 13h
	int 10h

	push VIDE0_SEGMENT ; video segment address
	pop es ; ES points to video segment
	ret
SetVideoMode ENDP


;--------------------------------------------------------------------------------
; SETTING THE SCREEN BACKGROUND
; Sets the screen's background color. Video
; palette index 0 is the background color.
;--------------------------------------------------------------------------------
SetScreenBackground PROC
COMMENT !
VIDEO_PALLETE_PORT = 3C8h
COLOR_SELECTION_PORT = 3C9h
PALLETE_INDEX_BACKGROUND = 0
SET_VIDEO_MODE = 0
GET_VIDEO_MODE = 0Fh
VIDE0_SEGMENT = 0A000h
WAIT_FOR_KEYSTROKE = 10h
MODE_13 = 13h
!
	mov dx,VIDEO_PALLETE_PORT
	mov al,PALLETE_INDEX_BACKGROUND
	out dx,al
; Set the screen background color to dark blue.
	mov dx, COLOR_SELECTION_PORT
	mov al, 0;FFh
	out dx, al
	mov al, 0;FFh
	out dx, al
	mov al, 0;FFh
	out dx, al

	ret
SetScreenBackground endp


;--------------------------------------------------------------------------------
;PRINTING THE PIXEL
;--------------------------------------------------------------------------------
prntpixel proc
	pusha
	mov ax, 320
	mul ycoor
	add ax, xcoor
	mov di, ax

	mov bl, COLOR_INDEX
	mov BYTE PTR es:[di], bl
	inc di
	popa
	ret
prntpixel endp

;--------------------------------------------------------------------------------
;GENERATING THE GAME BACKGROUND
;--------------------------------------------------------------------------------
background proc		
	mov xcoor, 88
	mov ycoor, 48
	mov cx, 144
	_bgloop1:
		mov COLOR_INDEX, 2Ch
		call prntpixel
		inc xcoor
		loop _bgloop1
	mov xcoor, 88
	mov ycoor, 49
	mov cx, 142
	_bgloop2:
		mov COLOR_INDEX, 2Ch
		call prntpixel
		add xcoor, 143
		call prntpixel
		sub xcoor, 143
		inc ycoor
		loop _bgloop2
	mov xcoor, 88
	mov ycoor, 191
	mov cx, 144
	_bgloop3:
		mov COLOR_INDEX, 2Ch
		call prntpixel
		inc xcoor
		loop _bgloop3

	mov xcoor, 99
	mov ycoor, 41
	mov cx, 122
	_bgloop4:
		mov COLOR_INDEX, 1Fh
		call prntpixel
		inc xcoor
		loop _bgloop4

	mov xcoor, 99
	mov ycoor, 42
	mov cx, 3
	_bgloop5:
		mov COLOR_INDEX, 1Fh
		call prntpixel
		add xcoor, 121
		call prntpixel
		sub xcoor, 121
		inc ycoor
		loop _bgloop5

	mov xcoor, 99
	mov ycoor, 45
	mov cx, 122
	_bgloop6:
		mov COLOR_INDEX, 1Fh
		call prntpixel
		inc xcoor
		loop _bgloop6

	mov xcoor, 100
	mov ycoor, 42
	mov cx, 120
	_bgloop7:
		mov COLOR_INDEX, 28h
		call prntpixel
		inc ycoor
		mov COLOR_INDEX, 28h
		call prntpixel
		inc ycoor
		mov COLOR_INDEX, 28h
		call prntpixel
		inc xcoor
		sub ycoor, 2
		loop _bgloop7
	ret
background endp

;=======================================================================
; COUNTDOWN LINE PROCEDURE
;=======================================================================
timerLine proc
	pusha
	mov xcoor, 219
	mov ycoor, 42
	mov ax, timeNum
	mov bl, 2
	mul bl
	xor ah, ah
	mov cx, 120
	sub cx, ax

	CMP cx, 0
	JE _endTimerLine

	_timerLine:
		mov COLOR_INDEX, 10h
		call prntpixel
		inc ycoor
		mov COLOR_INDEX, 10h
		call prntpixel
		inc ycoor
		mov COLOR_INDEX, 10h
		call prntpixel
		dec xcoor
		sub ycoor, 2
		loop _timerLine

	_endTimerLine:
		popa
		ret
timerLine endp

;=======================================================================
; CHANGE PARTICULAR BOX PROCEDURE
;=======================================================================
clickedBG proc
	pusha
	mov ax, xDim
	mov xcoor, ax
	mov ax, yDim
	mov ycoor, ax
		mov cx, 35
		_clickedBGLoop1:
			mov al, boxBGColor
			mov COLOR_INDEX, al
			call prntpixel
			inc xcoor
			loop _clickedBGLoop1
		sub xcoor, 35
		inc ycoor
		mov cx, 35
		_clickedBGLoop2:
			mov al, boxBGColor
			mov COLOR_INDEX, al
			call prntpixel
			inc xcoor
			loop _clickedBGLoop2
		sub xcoor, 35
		inc ycoor
		mov cx, 31
		_clickedBGLoop3:
			mov al, boxBGColor
			mov COLOR_INDEX, al
			call prntpixel
			inc xcoor
			call prntpixel
			add xcoor, 32
			call prntpixel
			inc xcoor
			call prntpixel
			sub xcoor, 34
			inc ycoor
			loop _clickedBGLoop3
		mov cx, 35
		_clickedBGLoop4:
			mov al, boxBGColor
			mov COLOR_INDEX, al
			call prntpixel
			inc xcoor
			loop _clickedBGLoop4
		sub xcoor, 35
		inc ycoor
		mov cx, 35
		_clickedBGLoop5:
			mov al, boxBGColor
			mov COLOR_INDEX, al
			call prntpixel
			inc xcoor
			loop _clickedBGLoop5
	popa
	ret
clickedBG endp

;=======================================================================
; GAMEOVER MOUSE PROCEDURE
;=======================================================================
gameOverProc proc
	popa
	pusha
		mov ax, 2h 
		int 33h

		mov xcoor, 115
		mov ycoor, 85
		mov cx, 70
		_loopY:
			push cx
			mov cx, 90
			_loopX:
				mov COLOR_INDEX, 10h
				call prntpixel
				inc xcoor
				loop _loopX
			mov xcoor, 115
			inc ycoor
			pop cx
			loop _loopY

	call timesUpProc
	mov gameOverColor, 1Fh
	call playAgainNameProc
	call exitNameProc
	
	popa
	call mouseGameover
	ret
gameOverProc endp

;=======================================================================
; TIME EQUAL TO ZERO PROCEDURE
;=======================================================================
timesUpProc proc
	pusha
	_timesUpName:
		MOV SI, 0
 		mov ah, 2
 		mov bh, 0
 		mov dh, 12
 		mov dl, 15
 		int 10h

	_timesUpName2:    
		MOV AL, timesUp[SI]
   		CMP AL, 0  
  		JZ _endTimesUpsProc
		MOV AH, 0Ah 
   		MOV BL, 0Bh 
   		MOV CX, 1  
   		INT 10H    
		INC SI     
		ADD DL,1  
       		MOV AH, 2   
       		INT 10H     
		jmp _timesUpName2

	_endTimesUpsProc:
		popa
		ret
timesUpProc endp

;=======================================================================
; PROCEDURE FOR PLAY AGAIN WORD
;=======================================================================
playAgainNameProc proc
	pusha
	_playAgainName:	 
		MOV SI, 0
 		mov ah, 2
 		mov bh, 0
 		mov dh, 15
 		mov dl, 15
 		int 10h	

	_playAgainName2:    
		MOV AL, playAgain[SI]
   		CMP AL, 0  
  		JZ _endPlayAgainNameProc
		MOV AH, 0Ah 
   		MOV BL, gameOverColor
   		MOV CX, 1  
   		INT 10H    
		INC SI     
		ADD DL,1  
       	 	MOV AH, 2   
       	 	INT 10H     
		jmp _playAgainName2

	_endPlayAgainNameProc:
		popa
		ret
playAgainNameProc endp

;=======================================================================
; PROCEDURE FOR EXIT WORD
;=======================================================================
exitNameProc proc
	pusha
	_exitGameNowName:	 
		MOV SI, 0	
 		mov ah, 2
 		mov bh, 0
 		mov dh, 17
 		mov dl, 18
 		int 10h	

	_exitGameNowName2:    
		MOV AL, exitGameNow[SI]
   		CMP AL, 0  
  		JZ _endExitNameProc
		MOV AH, 0Ah 
   		MOV BL, gameOverColor
   		MOV CX, 1  
   		INT 10H    
		INC SI     
		ADD DL,1  
        	MOV AH, 2   
       	 	INT 10H     
		jmp _exitGameNowName2
	
	_endExitNameProc:
		popa
		ret
exitNameProc endp

;--------------------------------------------------------------------------------
;END OF GAME
;--------------------------------------------------------------------------------
endGame proc
	pusha
		mov ax, 4c00h
		int 21h
	popa
endGame endp

;--------------------------------------------------------------------------------
;DRAWING THE SPRITES
;--------------------------------------------------------------------------------
DrawSprite proc
	mov ax, yDim
	mov ycoor, ax
	pusha
	mov cx, ySprite
	_ycoor:
		mov ax, xDim
		mov xcoor, ax
		mov ax, 320
		mul ycoor
		add ax, xcoor
		mov di, ax
		push cx
		mov cx, xSprite
		_xcoor:
			mov al, [si+1]
			CMP al, 69
			JE _boxpixel
			mov BYTE PTR es:[di], al
			JMP _ignorePixel

			_boxpixel:
				CMP colorBox, "r"
				JE _redPixel
				CMP colorBox, "g"
				JE _greenPixel
				CMP colorBox, "b"
				JE _bluePixel
				JMP _ignorePixel

				_redPixel:
					mov al, 28h
					mov BYTE PTR es:[di], al
					JMP _ignorePixel

				_greenPixel:
					mov al, 2Fh
					mov BYTE PTR es:[di], al
					JMP _ignorePixel

				_bluePixel:
					mov al, 20h
					mov BYTE PTR es:[di], al
					JMP _ignorePixel

			_ignorePixel:
				add di, 1
				inc si
				loop _xcoor
		pop cx
		inc ycoor
		loop _ycoor
	popa	
	
	ret
DrawSprite endp

;=======================================================================
; PROCEDURE FOR INTIALIZING NAMES
;=======================================================================
initializeNames proc
	pusha

	_timeName:
		MOV SI, 0
 		mov ah, 2
 		mov bh, 0
 		mov dh, 1
 		mov dl, 18
 		int 10h

	_timeName2:    
		MOV AL, timeName[SI]
   		CMP AL, 0  
  		JZ _hsName
		MOV AH, 0Ah 
   		MOV BL, 1Fh 
   		MOV CX, 1  
   		INT 10H    
		INC SI     
		ADD DL,1  
       		MOV AH, 2   
       		INT 10H     
		jmp _timeName2

	_hsName:	 
		MOV SI, 0
 		mov ah, 2
 		mov bh, 0
 		mov dh, 3
 		mov dl, 24
 		int 10h	

	_hsName2:    
		MOV AL, highscoreName[SI]
   		CMP AL, 0  
  		JZ _msName
		MOV AH, 0Ah 
   		MOV BL, 1Fh 
   		MOV CX, 1  
   		INT 10H    
		INC SI     
		ADD DL,1  
       	 	MOV AH, 2   
       	 	INT 10H     
		jmp _hsName2

	_msName:	 
		MOV SI, 0	
 		mov ah, 2
 		mov bh, 0
 		mov dh, 3
 		mov dl, 8
 		int 10h	

	_msName2:    
		MOV AL, myscoreName[SI]
   		CMP AL, 0  
  		JZ _endInitializeNames
		MOV AH, 0Ah 
   		MOV BL, 1Fh 
   		MOV CX, 1  
   		INT 10H    
		INC SI     
		ADD DL,1  
        	MOV AH, 2   
       	 	INT 10H     
		jmp _msName2

	_endInitializeNames:
		mov ax, 0h
		int 1Ah

		mov ax, dx
		mov dx, cx
		mov bx, 1007h
		div bx

		mov prevTick, dx
		popa
		ret
initializeNames endp

;=======================================================================
; PROCEDURE FOR UPDATING HIGHSCORE
;=======================================================================
updateScore proc
	pusha
		mov ah, 02h
		mov bh, 0
		mov dh, 4
		mov dl, 10
		int 10h

		mov cx, 3
		mov ax, msNum
		mov scoresNum, ax

		_displayMSNum:    
			push cx
			mov ax, scoresNum
			mov bl, 10
			div bl
			xor bx, bx
			mov bl, al
			mov scoresNum, bx
		
			mov al, ah
			add al, 30h
 	 		mov ah, 09h     
   	 		mov bl, 1Fh
   	 		mov cx, 1   
   	 		int 10h     
	 		inc si 
	 		dec dl
	 		mov ah, 02h
	 		int 10h        
			pop cx
	 		loop _displayMSNum

		mov ah, 02h
		mov bh, 0
		mov dh, 4
		mov dl, 26
		int 10h

		mov cx, 3
		mov ax, hsNum
		mov scoresNum, ax

		_displayHSNum:    
			push cx
			mov ax, scoresNum
			mov bl, 10
			div bl
			xor bx, bx
			mov bl, al
			mov scoresNum, bx
		
			mov al, ah
			add al, 30h
 	 		mov ah, 09h     
   	 		mov bl, 1Fh
   	 		mov cx, 1   
   	 		int 10h     
	 		inc si 
	 		dec dl
	 		mov ah, 02h
	 		int 10h        
			pop cx
	 		loop _displayHSNum
	popa
	ret
updateScore endp

;=======================================================================
; PROCEDURE FOR DISPLAYING TIME
;=======================================================================
displayTime proc
	pusha
		mov ah, 02h
		mov bh, 0
		mov dh, 3
		mov dl, 20
		int 10h

		mov cx, 2
		mov ax, timeNum
		mov scoresNum, ax

		_displayTime:    
			push cx
			mov ax, scoresNum
			mov bl, 10
			div bl
			xor bx, bx
			mov bl, al
			mov scoresNum, bx
		
			mov al, ah
			add al, 30h
 	 		mov ah, 09h     
   	 		mov bl, 1Fh
   	 		mov cx, 1   
   	 		int 10h     
	 		inc si 
	 		dec dl
	 		mov ah, 02h
	 		int 10h        
			pop cx
	 		loop _displayTime

			call timerLine

			CMP timeNum, 0
			JBE _endGameNow
			JMP _exitDisplayTime

	_endGameNow:
		call gameOverProc

	_exitDisplayTime:
		popa
		ret
displayTime endp

;=======================================================================
; PROCEDURE FOR UPDATING TIME
;=======================================================================
updateTime proc
	pusha
		mov ax, 0h
		int 1Ah

		mov ax, dx
		mov dx, cx
		mov bx, 1007h
		div bx

		mov currTick, dx
		mov ax, prevTick
		mov bx, currTick

		sub bx, ax

		CMP bx, 18
		JAE _decTime
		JMP _exitUpdateTime

		_decTime:
			mov ax, currTick
			mov prevTick, ax
			dec timeNum
	
	_exitUpdateTime:	
		popa
		ret
updateTime endp

;--------------------------------------------------------------------------------
;box dimension
;--------------------------------------------------------------------------------
box proc
	pusha
		mov xSprite, 35
		mov ySprite, 35
	popa
	ret
box endp

;--------------------------------------------------------------------------------
;coor 11
;--------------------------------------------------------------------------------
coor_11 proc
	pusha
		mov xDim, 90
		mov yDim, 50
		mov al, c11[2]
		mov ah, c11[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_11 endp

;--------------------------------------------------------------------------------
;coor 12
;--------------------------------------------------------------------------------
coor_12 proc
	pusha
		mov xDim, 125
		mov yDim, 50
		mov al, c12[2]
		mov ah, c12[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_12 endp

;--------------------------------------------------------------------------------
;coor 13
;--------------------------------------------------------------------------------
coor_13 proc
	pusha
		mov xDim, 160
		mov yDim, 50
		mov al, c13[2]
		mov ah, c13[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_13 endp

;--------------------------------------------------------------------------------
;coor 14
;--------------------------------------------------------------------------------
coor_14 proc
	pusha
		mov xDim, 195
		mov yDim, 50
		mov al, c14[2]
		mov ah, c14[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_14 endp

;--------------------------------------------------------------------------------
;coor 21
;--------------------------------------------------------------------------------
coor_21 proc
	pusha
		mov xDim, 90
		mov yDim, 85
		mov al, c21[2]
		mov ah, c21[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_21 endp

;--------------------------------------------------------------------------------
;coor 22
;--------------------------------------------------------------------------------
coor_22 proc
	pusha
		mov xDim, 125
		mov yDim, 85
		mov al, c22[2]
		mov ah, c22[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_22 endp

;--------------------------------------------------------------------------------
;coor 23
;--------------------------------------------------------------------------------
coor_23 proc
	pusha
		mov xDim, 160
		mov yDim, 85
		mov al, c23[2]
		mov ah, c23[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_23 endp

;--------------------------------------------------------------------------------
;coor 24
;--------------------------------------------------------------------------------
coor_24 proc
	pusha
		mov xDim, 195
		mov yDim, 85
		mov al, c24[2]
		mov ah, c24[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_24 endp

;--------------------------------------------------------------------------------
;coor 31
;--------------------------------------------------------------------------------
coor_31 proc
	pusha
		mov xDim, 90
		mov yDim, 120
		mov al, c31[2]
		mov ah, c31[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_31 endp

;--------------------------------------------------------------------------------
;coor 32
;--------------------------------------------------------------------------------
coor_32 proc
	pusha
		mov xDim, 125
		mov yDim, 120
		mov al, c32[2]
		mov ah, c32[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_32 endp

;--------------------------------------------------------------------------------
;coor 33
;--------------------------------------------------------------------------------
coor_33 proc
	pusha
		mov xDim, 160
		mov yDim, 120
		mov al, c33[2]
		mov ah, c33[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_33 endp

;--------------------------------------------------------------------------------
;coor 34
;--------------------------------------------------------------------------------
coor_34 proc
	pusha
		mov xDim, 195
		mov yDim, 120
		mov al, c34[2]
		mov ah, c34[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_34 endp

;--------------------------------------------------------------------------------
;coor 41
;--------------------------------------------------------------------------------
coor_41 proc
	pusha
		mov xDim, 90
		mov yDim, 155
		mov al, c41[2]
		mov ah, c41[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_41 endp

;--------------------------------------------------------------------------------
;coor 42
;--------------------------------------------------------------------------------
coor_42 proc
	pusha
		mov xDim, 125
		mov yDim, 155
		mov al, c42[2]
		mov ah, c42[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_42 endp

;--------------------------------------------------------------------------------
;coor 43
;--------------------------------------------------------------------------------
coor_43 proc
	pusha
		mov xDim, 160
		mov yDim, 155
		mov al, c43[2]
		mov ah, c43[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_43 endp

;--------------------------------------------------------------------------------
;coor 44
;--------------------------------------------------------------------------------
coor_44 proc
	pusha
		mov xDim, 195
		mov yDim, 155
		mov al, c44[2]
		mov ah, c44[3]
		mov tempBox[0], al
		mov tempBox[1], ah
	popa
	ret
coor_44 endp

;================================================================================
; GET A RANDOM NUMBER PROC
;================================================================================
getRandomNum proc
	pusha
		mov ah, 00h
		int 1ah
	
		add dl, dh
		mov al, dl
		mov bl, divisor
		div bl

		mov randomNum, ah
	popa
	ret
getRandomNum endp

initializeBox proc
	pusha
		mov cx, 16
		mov bx, 0
		_loopInitializeBox:
			call getRandomColor
			mov c11[bx + 2], 1
			mov al, randomColor
			mov c11[bx + 3], al
			mov colorBox, al
			mov al, c11[bx]
			xor ah, ah
			mov xDim, ax
			mov al, c11[bx + 1]
			xor ah, ah
			mov yDim, ax
			add bx, 5
			call box
			lea si, one
			call DrawSprite
			loop _loopInitializeBox		
	popa
	ret
initializeBox endp

initializeOne proc
	pusha
	call initializeRandomColor
	mov boxType, 1
	call box
	lea si, one
	call DrawSprite
	call writeMatrix
	popa
	ret
initializeOne endp

initializeTwo proc
	pusha
	call initializeRandomColor
	mov boxType, 2
	call box
	lea si, two
	call DrawSprite
	call writeMatrix
	popa
	ret
initializeTwo endp

initializeStar proc
	pusha
	call initializeRandomColor
	mov boxType, 3
	call box
	lea si, star
	call DrawSprite
	call writeMatrix
	popa
	ret
initializeStar endp

writeMatrix proc
	pusha
	mov ax, xDim
	mov bx, yDim
	
	CMP al, 90
	JE _x90
	CMP al, 125
	JE _x125
	CMP al, 160
	JE _x160
	CMP al, 195
	JE _x195
	call endGame

	_x90:
		CMP bl, 50
		JE _x90y50
		CMP bl, 85
		JE _x90y85
		CMP bl, 120
		JE _x90y120
		CMP bl, 155
		JE _x90y155
		call endGame

		_x90y50:
			mov al, boxType
			mov bl, colorBox
			mov c11[2], al
			mov c11[3], bl
			JMP _endWriteMatrix

		_x90y85:
			mov al, boxType
			mov bl, colorBox
			mov c21[2], al
			mov c21[3], bl
			JMP _endWriteMatrix

		_x90y120:
			mov al, boxType
			mov bl, colorBox
			mov c31[2], al
			mov c31[3], bl
			JMP _endWriteMatrix

		_x90y155:
			mov al, boxType
			mov bl, colorBox
			mov c41[2], al
			mov c41[3], bl
			JMP _endWriteMatrix

	_x125:
		CMP bl, 50
		JE _x125y50
		CMP bl, 85
		JE _x125y85
		CMP bl, 120
		JE _x125y120
		CMP bl, 155
		JE _x125y155
		call endGame

		_x125y50:
			mov al, boxType
			mov bl, colorBox
			mov c12[2], al
			mov c12[3], bl
			JMP _endWriteMatrix

		_x125y85:
			mov al, boxType
			mov bl, colorBox
			mov c22[2], al
			mov c22[3], bl
			JMP _endWriteMatrix

		_x125y120:
			mov al, boxType
			mov bl, colorBox
			mov c32[2], al
			mov c32[3], bl
			JMP _endWriteMatrix

		_x125y155:
			mov al, boxType
			mov bl, colorBox
			mov c42[2], al
			mov c42[3], bl
			JMP _endWriteMatrix

	_x160:
		CMP bl, 50
		JE _x160y50
		CMP bl, 85
		JE _x160y85
		CMP bl, 120
		JE _x160y120
		CMP bl, 155
		JE _x160y155
		call endGame

		_x160y50:
			mov al, boxType
			mov bl, colorBox
			mov c13[2], al
			mov c13[3], bl
			JMP _endWriteMatrix

		_x160y85:
			mov al, boxType
			mov bl, colorBox
			mov c23[2], al
			mov c23[3], bl
			JMP _endWriteMatrix

		_x160y120:
			mov al, boxType
			mov bl, colorBox
			mov c33[2], al
			mov c33[3], bl
			JMP _endWriteMatrix

		_x160y155:
			mov al, boxType
			mov bl, colorBox
			mov c43[2], al
			mov c43[3], bl
			JMP _endWriteMatrix

	_x195:
		CMP bl, 50
		JE _x195y50
		CMP bl, 85
		JE _x195y85
		CMP bl, 120
		JE _x195y120
		CMP bl, 155
		JE _x195y155
		call endGame
		
		_x195y50:
			mov al, boxType
			mov bl, colorBox
			mov c14[2], al
			mov c14[3], bl
			JMP _endWriteMatrix

		_x195y85:
			mov al, boxType
			mov bl, colorBox
			mov c24[2], al
			mov c24[3], bl
			JMP _endWriteMatrix

		_x195y120:
			mov al, boxType
			mov bl, colorBox
			mov c34[2], al
			mov c34[3], bl
			JMP _endWriteMatrix

		_x195y155:
			mov al, boxType
			mov bl, colorBox
			mov c44[2], al
			mov c44[3], bl
			JMP _endWriteMatrix

	_endWriteMatrix:
		popa
		ret
writeMatrix endp

getRandomColor proc
	pusha
	_startRandomColor:
		mov divisor, 3
		call getRandomNum
		mov al, randomNum
		CMP al, 0
		JE _colorRed
		CMP al, 1
		JE _colorGreen
		CMP al, 2
		JE _colorBlue

		_colorRed:
			CMP randomRed, 6
			JAE _startRandomColor
			CMP recentRed, 2
			JB _nextColorRed
			mov recentRed, 0
			mov al, randomGreen
			CMP al, randomBlue
			JBE _colorGreen
			JMP _colorBlue
			_nextColorRed: 
			mov randomColor, "r"
			inc randomRed
			inc recentRed
			JMP _endRandomColor

		_colorGreen:
			CMP randomGreen, 6
			JAE _startRandomColor
			CMP recentGreen, 2
			JB _nextColorGreen
			mov recentGreen, 0
			mov al, randomBlue
			CMP al, randomRed
			JBE _colorBlue
			JMP _colorRed
			_nextColorGreen: 
			mov randomColor, "g"
			inc randomGreen	
			inc recentGreen
			JMP _endRandomColor

		_colorBlue:
			CMP randomBlue, 6
			JAE _startRandomColor
			CMP recentBlue, 2
			JB _nextColorBlue
			mov recentBlue, 0
			mov al, randomRed
			CMP al, randomGreen
			JBE _colorRed
			JMP _colorGreen
			_nextColorBlue: 
			mov randomColor, "b"
			inc randomBlue
			inc recentBlue
			JMP _endRandomColor

	_endRandomColor:
		popa
		ret
getRandomColor endp

initializeRandomColor proc
	pusha
	CMP boxColor, 0
	JE _DoNotChangeColor
	JMP _ChangeColor

	_DoNotChangeColor:
		mov al, prevBox[1]
		mov colorBox, al
		JMP _endReColor

	_ChangeColor:
		mov divisor, 6
		call getRandomNum
		mov al, randomNum
		CMP al, 0
		JE _initColorRed
		CMP al, 1
		JE _initColorGreen
		CMP al, 2
		JE _initColorBlue
		CMP al, 5
		JE _initColorRed
		CMP al, 4
		JE _initColorGreen
		CMP al, 3
		JE _initColorBlue

		_initColorRed:
			mov colorBox, "r"
			JMP _endReColor

		_initColorGreen:
			mov colorBox, "g"
			JMP _endReColor

		_initColorBlue:
			mov colorBox, "b"
			JMP _endReColor

	_endReColor:
		popa
		ret
initializeRandomColor endp

;================================================================================
; MOUSE INTERACTION PROCEDURE
;================================================================================
mouseInt proc
	push es
	pusha
	call getHighScore
	mov ax, 0h
	int 33h      ;show mouse
	mov ax, 1h 
	int 33h      ;get mouse place and status
	mov ax, 07
	mov cx, 5
	mov dx, 635  ;horizontal limits
	int 33h
	mov ax, 08
	mov cx, 2
	mov dx, 198  ;vertical limits
	int 33h
	mov ax, 04h
	mov cx, 200
	mov dx, 100
	int 33h

	_resetMouse:
		mov ax, 03
		int 33h
		mov mouseButton, bx
		mov mouseRow, dx
		mov mouseCol, cx

		CMP mouseRow, 52
		JB _mouseLoop
		CMP mouseCol, 184
		JB _mouseLoop
		CMP mouseRow, 187
		JA _mouseLoop
		CMP mouseCol, 454
		JA _mouseLoop

		CMP mouseRow, 82
		JBE _mouseCoor11
		JMP _mouseCoor21

		_mouseCoor11:
			CMP mouseCol, 244
			JA _mouseCoor12
		
			test mouseButton, 1
			JNE _mCoor11
			test mouseButton, 2
			JNE _mCoor11
			test mouseButton, 4
			JNE _mCoor11
			JMP _mouseLoop

		_mouseCoor12:
			CMP mouseCol, 254
			JB _mouseLoop
			
			CMP mouseCol, 314
			JA _mouseCoor13

			test mouseButton, 1
			JNE _mCoor12
			test mouseButton, 2
			JNE _mCoor12
			test mouseButton, 4
			JNE _mCoor12
			JMP _mouseLoop

		_mouseCoor13:
			CMP mouseCol, 324
			JB _mouseLoop

			CMP mouseCol, 384
			JA _mouseCoor14

			test mouseButton, 1
			JNE _mCoor13
			test mouseButton, 2
			JNE _mCoor13
			test mouseButton, 4
			JNE _mCoor13
			JMP _mouseLoop

		_mouseCoor14:
			CMP mouseCol, 394
			JB _mouseLoop

			test mouseButton, 1
			JNE _mCoor14
			test mouseButton, 2
			JNE _mCoor14
			test mouseButton, 4
			JNE _mCoor14
			JMP _mouseLoop

		_mouseCoor21:
			CMP mouseRow, 87
			JB _mouseLoop

			CMP mouseRow, 117
			JA _mouseCoor31
			
			CMP mouseCol, 244
			JA _mouseCoor22
		
			test mouseButton, 1
			JNE _mCoor21
			test mouseButton, 2
			JNE _mCoor21
			test mouseButton, 4
			JNE _mCoor21
			JMP _mouseLoop

		_mouseCoor22:
			CMP mouseCol, 254
			JB _mouseLoop
			
			CMP mouseCol, 314
			JA _mouseCoor23

			test mouseButton, 1
			JNE _mCoor22
			test mouseButton, 2
			JNE _mCoor22
			test mouseButton, 4
			JNE _mCoor22
			JMP _mouseLoop

		_mouseCoor23:
			CMP mouseCol, 324
			JB _mouseLoop

			CMP mouseCol, 384
			JA _mouseCoor24

			test mouseButton, 1
			JNE _mCoor23
			test mouseButton, 2
			JNE _mCoor23
			test mouseButton, 4
			JNE _mCoor23
			JMP _mouseLoop

		_mouseCoor24:
			CMP mouseCol, 394
			JB _mouseLoop

			test mouseButton, 1
			JNE _mCoor24
			test mouseButton, 2
			JNE _mCoor24
			test mouseButton, 4
			JNE _mCoor24
			JMP _mouseLoop	

		_mouseCoor31:
			CMP mouseRow, 122
			JB _mouseLoop

			CMP mouseRow, 152
			JA _mouseCoor41
			
			CMP mouseCol, 244
			JA _mouseCoor32
		
			test mouseButton, 1
			JNE _mCoor31
			test mouseButton, 2
			JNE _mCoor31
			test mouseButton, 4
			JNE _mCoor31
			JMP _mouseLoop

		_mouseCoor32:
			CMP mouseCol, 254
			JB _mouseLoop
			
			CMP mouseCol, 314
			JA _mouseCoor33

			test mouseButton, 1
			JNE _mCoor32
			test mouseButton, 2
			JNE _mCoor32
			test mouseButton, 4
			JNE _mCoor32
			JMP _mouseLoop

		_mouseCoor33:
			CMP mouseCol, 324
			JB _mouseLoop

			CMP mouseCol, 384
			JA _mouseCoor34

			test mouseButton, 1
			JNE _mCoor33
			test mouseButton, 2
			JNE _mCoor33
			test mouseButton, 4
			JNE _mCoor33
			JMP _mouseLoop

		_mouseCoor34:
			CMP mouseCol, 394
			JB _mouseLoop

			test mouseButton, 1
			JNE _mCoor34
			test mouseButton, 2
			JNE _mCoor34
			test mouseButton, 4
			JNE _mCoor34
			JMP _mouseLoop

		_mouseCoor41:
			CMP mouseRow, 157
			JB _mouseLoop
			
			CMP mouseCol, 244
			JA _mouseCoor42
		
			test mouseButton, 1
			JNE _mCoor41
			test mouseButton, 2
			JNE _mCoor41
			test mouseButton, 4
			JNE _mCoor41
			JMP _mouseLoop

		_mouseCoor42:
			CMP mouseCol, 254
			JB _mouseLoop
			
			CMP mouseCol, 314
			JA _mouseCoor43

			test mouseButton, 1
			JNE _mCoor42
			test mouseButton, 2
			JNE _mCoor42
			test mouseButton, 4
			JNE _mCoor42
			JMP _mouseLoop

		_mouseCoor43:
			CMP mouseCol, 324
			JB _mouseLoop

			CMP mouseCol, 384
			JA _mouseCoor44

			test mouseButton, 1
			JNE _mCoor43
			test mouseButton, 2
			JNE _mCoor43
			test mouseButton, 4
			JNE _mCoor43
			JMP _mouseLoop

		_mouseCoor44:
			CMP mouseCol, 394
			JB _mouseLoop

			test mouseButton, 1
			JNE _mCoor44
			test mouseButton, 2
			JNE _mCoor44
			test mouseButton, 4
			JNE _mCoor44
			JMP _mouseLoop

	_mouseLoop:
		call updateScore
		call updateTime
		call displayTime
		JMP _resetMouse

	_mCoor11:
		mov ax, 2h 
		int 33h
		call coor_11
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor12:
		mov ax, 2h 
		int 33h
		call coor_12
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor13:
		mov ax, 2h 
		int 33h
		call coor_13
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor14:
		mov ax, 2h 
		int 33h
		call coor_14
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor21:
		mov ax, 2h 
		int 33h
		call coor_21
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor22:
		mov ax, 2h 
		int 33h
		call coor_22
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor23:
		mov ax, 2h 
		int 33h
		call coor_23
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor24:
		mov ax, 2h 
		int 33h
		call coor_24
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor31:
		mov ax, 2h 
		int 33h
		call coor_31
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor32:
		mov ax, 2h 
		int 33h
		call coor_32
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor33:
		mov ax, 2h 
		int 33h
		call coor_33
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor34:
		mov ax, 2h 
		int 33h
		call coor_34
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor41:
		mov ax, 2h 
		int 33h
		call coor_41
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor42:
		mov ax, 2h 
		int 33h
		call coor_42
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor43:
		mov ax, 2h 
		int 33h
		call coor_43
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_mCoor44:
		mov ax, 2h 
		int 33h
		call coor_44
		mov boxBGColor, 1Fh
		call clickedBG
		call boxBGCheckerProc
		mov ax, 1h 
		int 33h
		call CMPHighScore2MyScore
		call updateScore
		call changeHighScore
		call updateTime
		call displayTime
		call delaying
		JMP _mouseLoop

	_endMouseInt:
		popa
		pop es
		call endGame
	ret
mouseInt endp

;=======================================================================
; GAMEOVER MOUSE PROCEDURE
;=======================================================================
mouseGameover proc
	push es
	pusha
	
	mov ax, 0h
	int 33h      ;show mouse
	mov ax, 1h 
	int 33h      ;get mouse place and status
	mov ax, 07
	mov cx, 5
	mov dx, 635  ;horizontal limits
	int 33h
	mov ax, 08
	mov cx, 2
	mov dx, 198  ;vertical limits
	int 33h
	mov ax, 04h
	mov cx, 200
	mov dx, 100
	int 33h

	_resetMouseGameover:
		mov ax, 03
		int 33h
		mov mouseButton, bx
		mov mouseRow, dx
		mov mouseCol, cx

		CMP mouseRow, 120
		JB _mouseGameoverLoop
		CMP mouseCol, 238
		JB _mouseGameoverLoop
		CMP mouseRow, 142
		JA _mouseGameoverLoop
		CMP mouseCol, 396
		JA _mouseGameoverLoop

		CMP mouseRow, 126
		JBE _playAgain
		CMP mouseRow, 136
		JAE _exitNow
		JMP _mouseGameoverLoop

		_playAgain:
			mov ax, 02
			int 33h

			mov gameOverColor, 0Ch
			call playAgainNameProc
			mov gameOverColor, 1Fh
			call exitNameProc

			mov ax, 01
			int 33h
			mov ax, 03
			int 33h
			mov mouseButton, bx
			mov mouseRow, dx
			mov mouseCol, cx
		
			test mouseButton, 1
			JNE _mPlayAgain
			test mouseButton, 2
			JNE _mPlayAgain
			test mouseButton, 4
			JNE _mPlayAgain
			JMP _mouseGameoverLoop

		_exitNow:
			CMP mouseCol, 288
			JB _mouseGameoverLoop
			CMP mouseCol, 346
			JA _mouseGameoverLoop

			mov ax, 02
			int 33h

			mov gameOverColor, 1Fh
			call playAgainNameProc
			mov gameOverColor, 0Ch
			call exitNameProc

			mov ax, 01
			int 33h
			mov ax, 03
			int 33h
			mov mouseButton, bx
			mov mouseRow, dx
			mov mouseCol, cx
		
			test mouseButton, 1
			JNE _endMouseGameover
			test mouseButton, 2
			JNE _endMouseGameover
			test mouseButton, 4
			JNE _endMouseGameover
			JMP _mouseGameoverLoop	

	_mouseGameoverLoop:
		JMP _resetMouseGameover

	_mPlayAgain:
		popa
		pop es
		call initializeGame

	_endMouseGameover:
		popa
		pop es
		call endGame
	ret
mouseGameover endp

;=======================================================================
; CHECK WHAT BOX PROCEDURE
;=======================================================================
boxBGCheckerProc proc
	pusha
		mov ax, xDim
		mov bx, yDim
		mov boxChecker[0], al
		mov boxChecker[1], bl

		CMP boxBGChecker[0], 0
		JE _initBoxCheckerProc

		pusha
		mov ax, xDim
		CMP boxBGChecker[0], al
		JNE _nextBoxCheckerProc
		mov ax, yDim
		CMP boxBGChecker[1], al
		JNE _nextBoxCheckerProc

		xor ah, ah
		mov al, boxBGChecker[0]
		mov xDim, ax
		mov al, boxBGChecker[1]
		mov yDim, ax
		mov boxBGColor, 10h
		call clickedBG

		mov al, 0
		mov boxBGChecker[0], al
		mov boxBGChecker[1], al
		popa		
		JMP _endBoxCheckerProc

		_nextBoxCheckerProc:
			popa
			call boxBGColorCheckProc
			JMP _endBoxCheckerProc
		
		_initBoxCheckerProc:
			mov boxBGChecker[0], al
			mov boxBGChecker[1], bl
			mov al, tempBox[0]
			mov bl, tempBox[1]
			mov prevBox[0], al
			mov prevBox[1], bl
		
		_endBoxCheckerProc:
			popa
			ret
boxBGCheckerProc endp

boxBGColorCheckProc proc
	pusha
		mov al, tempBox[0]
		mov bl, tempBox[1]

		CMP prevBox[0], al
		JNE _boxAreNotEq
		CMP prevBox[1], bl
		JNE _boxAreNotEq

		CMP al, 1
		JE _getNewTwo
		CMP al, 2
		JE _getNewStar
		JMP _getNewOne

		_getNewOne:
			add msNum, 5
			mov boxColor, 1
			xor ax, ax
			xor bx, bx
			mov al, boxBGChecker[0]
			mov bl, boxBGChecker[1]
			mov xDim, ax
			mov yDim, bx
			call initializeOne

			mov boxColor, 1
			xor ax, ax
			xor bx, bx
			mov al, boxChecker[0]
			mov bl, boxChecker[1]
			mov xDim, ax
			mov yDim, bx
			call initializeOne
			JMP _initBoxBGColorCheckProc

		_getNewTwo:
			add msNum, 1
			mov boxColor, 1
			xor ax, ax
			xor bx, bx
			mov al, boxBGChecker[0]
			mov bl, boxBGChecker[1]
			mov xDim, ax
			mov yDim, bx
			call initializeOne

			mov boxColor, 0
			xor ax, ax
			xor bx, bx
			mov al, boxChecker[0]
			mov bl, boxChecker[1]
			mov xDim, ax
			mov yDim, bx
			call initializeTwo
			JMP _initBoxBGColorCheckProc

		_getNewStar:
			add msNum, 2
			mov boxColor, 1
			xor ax, ax
			xor bx, bx
			mov al, boxBGChecker[0]
			mov bl, boxBGChecker[1]
			mov xDim, ax
			mov yDim, bx
			call initializeOne

			mov boxColor, 0
			xor ax, ax
			xor bx, bx
			mov al, boxChecker[0]
			mov bl, boxChecker[1]
			mov xDim, ax
			mov yDim, bx
			call initializeStar
			JMP _initBoxBGColorCheckProc

	_boxAreNotEq:
		xor ah, ah
		mov al, boxBGChecker[0]
		mov xDim, ax
		mov al, boxBGChecker[1]
		mov yDim, ax
		mov boxBGColor, 10h
		call clickedBG

		xor ah, ah
		mov al, boxChecker[0]
		mov xDim, ax
		mov al, boxChecker[1]
		mov yDim, ax
		mov boxBGColor, 10h
		call clickedBG

	_initBoxBGColorCheckProc:
		mov al, 0
		mov boxBGChecker[0], al
		mov boxBGChecker[1], al

	_endBoxBGColorCheckProc:
		popa
		ret
boxBGColorCheckProc endp

;================================================================================
; GET HIGH SCORE PROC
;================================================================================
getHighScore proc
	pusha
	push es
	mov ax, @data
	mov ds, ax
	mov es, ax

	call openFile
	call readFile
	call closeFile

	lea si, bufferread
	lea di, highScoreNum
	mov cx, 3
	cld
	rep MOVSB
	
	lea si, highScoreNum
	dec si
	add si, 3

	std
	LODSB
	sub al, 30h
	mov ah, 0h
	mov hsNum, ax
	LODSB
	sub al, 30h
	mov ah, 0h
	mov bx, 10
	mul bx
	add hsNum, ax
	LODSB
	sub al, 30h
	mov ah, 0h
	mov bx, 100
	mul bx
	add hsNum, ax

	pop es		
	popa
	ret
getHighScore endp


;================================================================================
; CHANGE HIGH SCORE PROC
;================================================================================
changeHighScore proc
	pusha
	push es
	mov ax, @data
	mov ds, ax
	mov es, ax	

	lea di, HighScoreString
	dec di
	add di, 3

	std
	mov cx, 3
	mov ax, hsNum
	mov scoresNum, ax
	_changeHS:
		mov ax, scoresNum
		mov bl, 10
		div bl
		mov bl, ah
		mov ah, 0h
		mov scoresNum, ax
		add bl, 30h
		mov al, bl
		STOSB
		loop _changeHS

	call openFile
	call writeFile
	call closeFile
	
	pop es	
	popa
	ret
changeHighScore endp

;================================================================================
; COMPARE IF HIGHSCORE < MYSCORE PROC
;================================================================================
CMPHighScore2MyScore proc
	pusha
		mov ax, hsNum
		CMP msNum, ax
		JAE _myScoreNumAbove
		JMP _endCMPHighScore2MyScore 

		_myScoreNumAbove:
			mov ax, msNum
			mov hsNum, ax

		_endCMPHighScore2MyScore:
			popa
			ret
CMPHighScore2MyScore endp

;===============================================================================
; CREATE A FILE PROC
;================================================================================
createFile proc
	pusha
	mov ah, 3Ch 
	lea dx, filename

	xor cx, cx
	int 21h 
	jc _error_create
	mov filehandle, ax
	popa
	ret	
createFile endp

;================================================================================
; OPEN A FILE PROC
;================================================================================
openFile proc
	pusha
	_startOpenFile:
		mov ah, 3Dh	
		mov al, 02
		lea dx, filename
		int 21h
		jc _gotoCreate
		mov filehandle, ax
		JMP _exitOpenFile

	_gotoCreate:
		call createFile
		call writeFile
		call closeFile
		call openFile
		JMP _startOpenFile

	_exitOpenFile:
		popa
		ret	
openFile endp

;================================================================================
; READ A FILE PROC
;================================================================================
readFile proc
	pusha
	mov ah, 3fh
	mov al, 00h
	mov bx, filehandle
	mov cx, 3
	lea dx, bufferread
	int 21h 
	jc _error_read	

	popa
	ret
readFile endp


;================================================================================
; WRITE A FILE PROC
;================================================================================
writeFile proc
	pusha
	mov ah, 40h 
	mov bx, filehandle
	mov cx, 3
	lea dx, HighScoreString
	int 21h 
	jc _error_write

	popa
	ret
writeFile endp

;================================================================================
; CLOSE A FILE PROC
;================================================================================
closeFile proc
	pusha
	mov ah, 3eh 
	mov bx, filehandle
	int 21h 	
	jc _error_close
	popa
	ret
closeFile endp


;================================================================================
; FILE ERROR PROC
;================================================================================
errorFile proc
	pusha
	_error_open:
		call endGame
		mov ah, 09h
		lea dx, errorOpenStr
		int 21h

	_error_read:
		call endGame
		mov ah, 09h
		lea dx, errorReadStr
		int 21h

	_error_write:
		call endGame
		mov ah, 09h
		lea dx, errorWriteStr
		int 21h

	_error_close:
		call endGame
		mov ah, 09h
		lea dx, errorCloseStr
		int 21h

	_error_create:
		call endGame
		mov ah, 09h
		lea dx, errorCreateStr
		int 21h
	popa
errorFile endp

;--------------------------------------------------------------------------------
;HAVING A DELAY
;--------------------------------------------------------------------------------
delaying proc
	pusha
	mov ax, delays
	_delay1:
		mov bx, 3
		_delay2:
			NOP
			dec bx
			CMP bx, 0
			JNE _delay2
		dec ax
		CMP ax, 0
		JNE _delay1
	popa

	ret
delaying endp
end main