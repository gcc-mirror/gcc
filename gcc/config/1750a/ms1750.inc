;; GCC assembler includefile for AS1750
;;
;; Macros defined:
;;   EFLR.M  #d,#s	Load the three regs starting at R#s to R#d following.
;;   RET.M   #fs	Return from function (uses the framesize #fs)


UC	SET	15

; Return from function ; parameter: framesize
     MACRO RET.M
	IF `1` > 0
	    IF `1` <= 16
		AISP R14,`1`
	    ELSE
		AIM  R14,`1`
	    ENDIF
	ENDIF
	LR   R15,R14
	URS  R15
     ENDMACRO

; Useful instructions missing from the 1750A standard:

; Extended Float Load from Registers
     MACRO EFLR.M	; args : #1=dest-regno, #2=source-regno
ONE 	SET  `1` + 2
TWO 	SET  `2` + 2
	IF `1` >= `2` || `1`+2 < `2`
	    LR    R`ONE`,R`TWO`
	    DLR   R`1`,R`2`
	ELSE
	    DLR   R`1`,R`2`
	    LR    R`ONE`,R`TWO`
	    DLR   R`1`,R`1`	; Just to update condition codes
	ENDIF
     ENDMACRO

; The following leave the condition codes haywire. But that is
; accounted for (see notice_update_cc in config/1750a.c.)

; Double ANd Register with Register
     MACRO DANR.M
ONE 	SET  `1` + 1
TWO 	SET  `2` + 1
	ANDR  R`1`,R`2`
	ANDR  R`ONE`,R`TWO`
     ENDMACRO

; Double OR Register with Register
     MACRO DORR.M
ONE 	SET  `1` + 1
TWO 	SET  `2` + 1
	ORR   R`1`,R`2`
	ORR   R`ONE`,R`TWO`
     ENDMACRO

; Double eXoR Register with Register
     MACRO DXRR.M
ONE 	SET  `1` + 1
TWO 	SET  `2` + 1
	XORR  R`1`,R`2`
	XORR  R`ONE`,R`TWO`
     ENDMACRO

; Double Nand Register with register
     MACRO DNR.M
ONE 	SET  `1` + 1
TWO 	SET  `2` + 1
	NR    R`1`,R`2`
	NR    R`ONE`,R`TWO`
     ENDMACRO

; Unsigned Compare Immediate

     MACRO UCIM.M
LAST	SET   `1` + 3
	PSHM  R`1`,R`LAST`
LO	SET   `1` + 1
	LR    R`LO`,R`1`
	XORR  R`1`,R`1`
HI	SET   `1` + 2
	XORR  R`HI`,R`HI`
	LIM   R`LAST`,`2`
	DCR   R`1`,R`HI`
	POPM  R`1`,R`LAST`
     ENDMACRO


; Unsigned Compare Register with register

     MACRO UCR.M
	PSHM  R10,R13    ; R12 and R13 are assumed not to be input parameters
	LR    R13,R`2`
	LR    R11,R`1`
	XORR  R12,R12
	XORR  R10,R10
	DCR   R10,R12
	POPM  R10,R13
     ENDMACRO


; Unsigned Compare register with memory

     MACRO UC.M
	PSHM  R10,R13
	L     R13,`2`
	LR    R11,R`1`
	XORR  R12,R12
	XORR  R10,R10
	DCR   R10,R12
	POPM  R10,R13
     ENDMACRO

