;;  Mips.md	     Machine Description for MIPS based processors
;;  Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;  1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
;;  Contributed by   A. Lichnewsky, lich@inria.inria.fr
;;  Changes by       Michael Meissner, meissner@osf.org
;;  64 bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
;;  Brendan Eich, brendan@microunity.com.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ??? Currently does not have define_function_unit support for the R8000.
;; Must include new entries for fmadd in addition to existing entries.

(define_constants
  [(UNSPEC_LOAD_DF_LOW		 0)
   (UNSPEC_LOAD_DF_HIGH		 1)
   (UNSPEC_STORE_DF_HIGH	 2)
   (UNSPEC_GET_FNADDR		 3)
   (UNSPEC_BLOCKAGE		 4)
   (UNSPEC_CPRESTORE		 5)
   (UNSPEC_EH_RECEIVER		 6)
   (UNSPEC_EH_RETURN		 7)
   (UNSPEC_CONSTTABLE_QI	 8)
   (UNSPEC_CONSTTABLE_HI	 9)
   (UNSPEC_CONSTTABLE_SI	10)
   (UNSPEC_CONSTTABLE_DI	11)
   (UNSPEC_CONSTTABLE_SF	12)
   (UNSPEC_CONSTTABLE_DF	13)
   (UNSPEC_ALIGN_2		14)
   (UNSPEC_ALIGN_4		15)
   (UNSPEC_ALIGN_8		16)
   (UNSPEC_HIGH			17)
   (UNSPEC_LWL			18)
   (UNSPEC_LWR			19)
   (UNSPEC_SWL			20)
   (UNSPEC_SWR			21)
   (UNSPEC_LDL			22)
   (UNSPEC_LDR			23)
   (UNSPEC_SDL			24)
   (UNSPEC_SDR			25)
   (UNSPEC_LOADGP		26)
   (UNSPEC_LOAD_CALL		27)
   (UNSPEC_GP			29)

   (UNSPEC_ADDRESS_FIRST	100)

   (FAKE_CALL_REGNO		79)])

;; ....................
;;
;;	Attributes
;;
;; ....................

(define_attr "got" "unset,xgot_high,load"
  (const_string "unset"))

;; For jal instructions, this attribute is DIRECT when the target address
;; is symbolic and INDIRECT when it is a register.
(define_attr "jal" "unset,direct,indirect"
  (const_string "unset"))

;; This attribute is YES if the instruction is a jal macro (not a
;; real jal instruction).
;;
;; jal is always a macro in SVR4 PIC since it includes an instruction to
;; restore $gp.  Direct jals are also macros in NewABI PIC since they
;; load the target address into $25.
(define_attr "jal_macro" "no,yes"
  (cond [(eq_attr "jal" "direct")
	 (symbol_ref "TARGET_ABICALLS != 0")
	 (eq_attr "jal" "indirect")
	 (symbol_ref "(TARGET_ABICALLS && !TARGET_NEWABI) != 0")]
	(const_string "no")))

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump
;; call		unconditional call
;; load		load instruction(s)
;; store	store instruction(s)
;; prefetch	memory prefetch (register + offset)
;; prefetchx	memory indexed prefetch (register + register)
;; move		data movement within same register set
;; condmove	conditional moves
;; xfer		transfer to/from coprocessor
;; hilo		transfer of hi/lo registers
;; arith	integer arithmetic instruction
;; darith	double precision integer arithmetic instructions
;; const	load constant
;; imul		integer multiply
;; imadd	integer multiply-add
;; idiv		integer divide
;; icmp		integer compare
;; fadd		floating point add/subtract
;; fmul		floating point multiply
;; fmadd	floating point multiply-add
;; fdiv		floating point divide
;; fabs		floating point absolute value
;; fneg		floating point negation
;; fcmp		floating point compare
;; fcvt		floating point convert
;; fsqrt	floating point square root
;; frsqrt       floating point reciprocal square root
;; multi	multiword sequence (or user asm statements)
;; nop		no operation
(define_attr "type"
  "unknown,branch,jump,call,load,store,prefetch,prefetchx,move,condmove,xfer,hilo,const,arith,darith,imul,imadd,idiv,icmp,fadd,fmul,fmadd,fdiv,fabs,fneg,fcmp,fcvt,fsqrt,frsqrt,multi,nop"
  (cond [(eq_attr "jal" "!unset") (const_string "call")
	 (eq_attr "got" "load") (const_string "load")]
	(const_string "unknown")))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,SF,DF,FPSW"
  (const_string "unknown"))

;; Is this an extended instruction in mips16 mode?
(define_attr "extended_mips16" "no,yes"
  (const_string "no"))

;; Length of instruction in bytes.
(define_attr "length" ""
   (cond [;; Direct branch instructions have a range of [-0x40000,0x3fffc].
	  ;; If a branch is outside this range, we have a choice of two
	  ;; sequences.  For PIC, an out-of-range branch like:
	  ;;
	  ;;	bne	r1,r2,target
	  ;;	dslot
	  ;;
	  ;; becomes the equivalent of:
	  ;;
	  ;;	beq	r1,r2,1f
	  ;;	dslot
	  ;;	la	$at,target
	  ;;	jr	$at
	  ;;	nop
	  ;; 1:
	  ;;
	  ;; where the load address can be up to three instructions long
	  ;; (lw, nop, addiu).
	  ;;
	  ;; The non-PIC case is similar except that we use a direct
	  ;; jump instead of an la/jr pair.  Since the target of this
	  ;; jump is an absolute 28-bit bit address (the other bits
	  ;; coming from the address of the delay slot) this form cannot
	  ;; cross a 256MB boundary.  We could provide the option of
	  ;; using la/jr in this case too, but we do not do so at
	  ;; present.
	  ;;
	  ;; Note that this value does not account for the delay slot
	  ;; instruction, whose length is added separately.  If the RTL
	  ;; pattern has no explicit delay slot, mips_adjust_insn_length
	  ;; will add the length of the implicit nop.
	  (eq_attr "type" "branch")
          (cond [(lt (abs (minus (match_dup 1) (plus (pc) (const_int 4))))
                     (const_int 131072))
                 (const_int 4)
		 (ne (symbol_ref "flag_pic && ! TARGET_EMBEDDED_PIC")
		     (const_int 0))
		 (const_int 24)
		 ] (const_int 12))

	  (eq_attr "got" "load")
	  (const_int 4)
	  (eq_attr "got" "xgot_high")
	  (const_int 8)

	  (eq_attr "type" "const")
	  (symbol_ref "mips_const_insns (operands[1]) * 4")
	  (eq_attr "type" "load")
	  (symbol_ref "mips_fetch_insns (operands[1]) * 4")
	  (eq_attr "type" "store")
	  (symbol_ref "mips_fetch_insns (operands[0]) * 4")

	  ;; In the worst case, a call macro will take 8 instructions:
	  ;;
	  ;;	 lui $25,%call_hi(FOO)
	  ;;	 addu $25,$25,$28
	  ;;     lw $25,%call_lo(FOO)($25)
	  ;;	 nop
	  ;;	 jalr $25
	  ;;	 nop
	  ;;	 lw $gp,X($sp)
	  ;;	 nop
	  (eq_attr "jal_macro" "yes")
	  (const_int 32)

	  (and (eq_attr "extended_mips16" "yes")
	       (ne (symbol_ref "TARGET_MIPS16") (const_int 0)))
	  (const_int 8)

	  (and (eq_attr "type" "idiv")
	       (ne (symbol_ref "TARGET_CHECK_ZERO_DIV") (const_int 0)))
	  (cond [(ne (symbol_ref "TARGET_MIPS16") (const_int 0))
		 (const_int 12)]
		(const_int 16))
	  ] (const_int 4)))

;; Attribute describing the processor.  This attribute must match exactly
;; with the processor_type enumeration in mips.h.
(define_attr "cpu"
  "default,4kc,5kc,20kc,m4k,r3000,r3900,r6000,r4000,r4100,r4111,r4120,r4300,r4600,r4650,r5000,r5400,r5500,r7000,r8000,r9000,sb1,sr71000"
  (const (symbol_ref "mips_tune")))

;; The type of hardware hazard associated with this instruction.
;; DELAY means that the next instruction cannot read the result
;; of this one.  HILO means that the next two instructions cannot
;; write to HI or LO.
(define_attr "hazard" "none,delay,hilo"
  (cond [(and (eq_attr "type" "load")
	      (ne (symbol_ref "ISA_HAS_LOAD_DELAY") (const_int 0)))
	 (const_string "delay")

	 (and (eq_attr "type" "xfer")
	      (ne (symbol_ref "ISA_HAS_XFER_DELAY") (const_int 0)))
	 (const_string "delay")

	 (and (eq_attr "type" "fcmp")
	      (ne (symbol_ref "ISA_HAS_FCMP_DELAY") (const_int 0)))
	 (const_string "delay")

	 ;; The r4000 multiplication patterns include an mflo instruction.
	 (and (eq_attr "type" "imul")
	      (ne (symbol_ref "TARGET_MIPS4000") (const_int 0)))
	 (const_string "hilo")

	 (and (eq_attr "type" "hilo")
	      (and (eq (symbol_ref "ISA_HAS_HILO_INTERLOCKS") (const_int 0))
		   (match_operand 1 "hilo_operand" "")))
	 (const_string "hilo")]
	(const_string "none")))

;; Is it a single instruction?
(define_attr "single_insn" "no,yes"
  (symbol_ref "get_attr_length (insn) == (TARGET_MIPS16 ? 2 : 4)"))

;; Can the instruction be put into a delay slot?
(define_attr "can_delay" "no,yes"
  (if_then_else (and (eq_attr "type" "!branch,call,jump")
		     (and (eq_attr "hazard" "none")
			  (eq_attr "single_insn" "yes")))
		(const_string "yes")
		(const_string "no")))

;; Attribute defining whether or not we can use the branch-likely instructions
(define_attr "branch_likely" "no,yes"
  (const
   (if_then_else (ne (symbol_ref "GENERATE_BRANCHLIKELY") (const_int 0))
		 (const_string "yes")
		 (const_string "no"))))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")
   (set_attr "can_delay" "no")])

;; .........................
;;
;;	Branch, call and jump delay slots
;;
;; .........................

(define_delay (and (eq_attr "type" "branch")
		   (eq (symbol_ref "TARGET_MIPS16") (const_int 0)))
  [(eq_attr "can_delay" "yes")
   (nil)
   (and (eq_attr "branch_likely" "yes")
	(eq_attr "can_delay" "yes"))])

(define_delay (eq_attr "type" "jump")
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

(define_delay (and (eq_attr "type" "call")
		   (eq_attr "jal_macro" "no"))
  [(eq_attr "can_delay" "yes")
   (nil)
   (nil)])

;; .........................
;;
;;	Functional units
;;
;; .........................

; (define_function_unit NAME MULTIPLICITY SIMULTANEITY
;			TEST READY-DELAY ISSUE-DELAY [CONFLICT-LIST])

;; Make the default case (PROCESSOR_DEFAULT) handle the worst case

(define_function_unit "memory" 1 0
  (and (eq_attr "type" "load")
       (eq_attr "cpu" "!r3000,r3900,r4600,r4650,r4100,r4120,r4300,r5000"))
  3 0)

(define_function_unit "memory" 1 0
  (and (eq_attr "type" "load")
       (eq_attr "cpu" "r3000,r3900,r4600,r4650,r4100,r4120,r4300,r5000"))
  2 0)

(define_function_unit "memory"   1 0 (eq_attr "type" "store") 1 0)

(define_function_unit "memory"   1 0 (eq_attr "type" "xfer") 2 0)

(define_function_unit "imuldiv"  1 0
  (eq_attr "type" "hilo")
  1 3)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd")
       (eq_attr "cpu" "!r3000,r3900,r4000,r4600,r4650,r4100,r4120,r4300,r5000"))
  17 17)

;; On them mips16, we want to stronly discourage a mult from appearing
;; after an mflo, since that requires explicit nop instructions.  We
;; do this by pretending that mflo ties up the function unit for long
;; enough that the scheduler will ignore load stalls and the like when
;; selecting instructions to between the two instructions.

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "hilo") (ne (symbol_ref "TARGET_MIPS16") (const_int 0)))
  1 5)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd") (eq_attr "cpu" "r3000,r3900"))
  12 12)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd") (eq_attr "cpu" "r4000,r4600"))
  10 10)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd") (eq_attr "cpu" "r4650"))
  4 4)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4100,r4120")))
  1 1)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4100,r4120")))
  4 4)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4300,r5000")))
  5 5)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4300")))
  8 8)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul,imadd")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r5000")))
  9 9)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "!r3000,r3900,r4000,r4600,r4650,r4100,r4120,r4300,r5000"))
  38 38)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "r3000,r3900"))
  35 35)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "r4600"))
  42 42)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "r4650"))
  36 36)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "r4000"))
  69 69)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4100,r4120")))
  35 35)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4100,r4120")))
  67 67)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4300")))
  37 37)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4300")))
  69 69)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r5000")))
  36 36)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r5000")))
  68 68)

;; The R4300 does *NOT* have a separate Floating Point Unit, instead
;; the FP hardware is part of the normal ALU circuitry.  This means FP
;; instructions affect the pipe-line, and no functional unit
;; parallelism can occur on R4300 processors.  To force GCC into coding
;; for only a single functional unit, we force the R4300 FP
;; instructions to be processed in the "imuldiv" unit.

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fcmp") (eq_attr "cpu" "!r3000,r3900,r6000,r4300,r5000"))
  3 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fcmp") (eq_attr "cpu" "r3000,r3900,r6000"))
  2 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fcmp") (eq_attr "cpu" "r5000"))
  1 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "!r3000,r3900,r6000,r4300"))
  4 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "r3000,r3900"))
  2 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "r6000"))
  3 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fabs,fneg")
       (eq_attr "cpu" "!r3000,r3900,r4600,r4650,r4300,r5000"))
  2 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fabs,fneg") (eq_attr "cpu" "r3000,r3900,r4600,r4650,r5000"))
  1 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "SF")
	    (eq_attr "cpu" "!r3000,r3900,r6000,r4600,r4650,r4300,r5000")))
  7 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r3000,r3900,r5000")))
  4 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r6000")))
  5 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4600,r4650")))
  8 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "!r3000,r3900,r6000,r4300,r5000")))
  8 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r3000,r3900,r5000")))
  5 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r6000")))
  6 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "SF")
	    (eq_attr "cpu" "!r3000,r3900,r6000,r4600,r4650,r4300,r5000")))
  23 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r3000,r3900")))
  12 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r6000")))
  15 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4600,r4650")))
  32 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r5000")))
  21 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "DF")
	    (eq_attr "cpu" "!r3000,r3900,r6000,r4600,r4650,r4300")))
  36 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r3000,r3900")))
  19 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r6000")))
  16 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r4600,r4650")))
  61 0)

;;; ??? Is this number right?
(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt,frsqrt")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "!r4600,r4650,r4300,r5000")))
  54 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt,frsqrt")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4600,r4650")))
  31 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt,frsqrt")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r5000")))
  21 0)

;;; ??? Is this number right?
(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt,frsqrt")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "!r4600,r4650,r4300,r5000")))
  112 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt,frsqrt")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r4600,r4650")))
  60 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt,frsqrt")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r5000")))
  36 0)

;; R4300 FP instruction classes treated as part of the "imuldiv"
;; functional unit:

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "r4300"))
  3 3)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "fcmp,fabs,fneg") (eq_attr "cpu" "r4300"))
  1 1)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4300")))
  5 5)
(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "DF") (eq_attr "cpu" "r4300")))
  8 8)

(define_function_unit "imuldiv" 1 0
  (and (and (eq_attr "type" "fdiv") (eq_attr "type" "fsqrt,frsqrt"))
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4300")))
  29 29)
(define_function_unit "imuldiv" 1 0
  (and (and (eq_attr "type" "fdiv") (eq_attr "type" "fsqrt,frsqrt"))
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r4300")))
  58 58)

;; Include scheduling descriptions.

(include "5400.md")
(include "5500.md")
(include "7000.md")
(include "9000.md")
(include "sr71k.md")

;;
;;  ....................
;;
;;	CONDITIONAL TRAPS
;;
;;  ....................
;;

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 0))]
  ""
{
  if (ISA_HAS_COND_TRAP)
    return "teq\t$0,$0";
  /* The IRIX 6 O32 assembler requires the first break operand.  */
  else if (TARGET_MIPS16 || !TARGET_GAS)
    return "break 0";
  else
    return "break";
})

(define_expand "conditional_trap"
  [(trap_if (match_operator 0 "cmp_op"
			    [(match_dup 2) (match_dup 3)])
	    (match_operand 1 "const_int_operand" ""))]
  "ISA_HAS_COND_TRAP"
{
  if (operands[1] == const0_rtx)
    {
      mips_gen_conditional_trap (operands);
      DONE;
    }
  else
    FAIL;
})

(define_insn ""
  [(trap_if (match_operator 0 "trap_cmp_op"
                            [(match_operand:SI 1 "reg_or_0_operand" "dJ")
                             (match_operand:SI 2 "arith_operand" "dI")])
	    (const_int 0))]
  "ISA_HAS_COND_TRAP"
  "t%C0\t%z1,%z2")

(define_insn ""
  [(trap_if (match_operator 0 "trap_cmp_op"
                            [(match_operand:DI 1 "reg_or_0_operand" "dJ")
                             (match_operand:DI 2 "arith_operand" "dI")])
	    (const_int 0))]
  "TARGET_64BIT && ISA_HAS_COND_TRAP"
  "t%C0\t%z1,%z2")

;;
;;  ....................
;;
;;	ADDITION
;;
;;  ....................
;;

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "add.d\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"DF")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "add.s\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"SF")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "")
		 (match_operand:SI 2 "arith_operand" "")))]
  ""
{
  /* If a large stack adjustment was forced into a register, we may be
     asked to generate rtx such as:

	(set (reg:SI sp) (plus:SI (reg:SI sp) (reg:SI pseudo)))

     but no such instruction is available in mips16.  Handle it by
     using a temporary.  */
  if (TARGET_MIPS16
      && REGNO (operands[0]) == STACK_POINTER_REGNUM
      && ((GET_CODE (operands[1]) == REG
	   && REGNO (operands[1]) != STACK_POINTER_REGNUM)
	  || GET_CODE (operands[2]) != CONST_INT))
    {
      rtx tmp = gen_reg_rtx (SImode);

      emit_move_insn (tmp, operands[1]);
      emit_insn (gen_addsi3 (tmp, tmp, operands[2]));
      emit_move_insn (operands[0], tmp);
      DONE;
    }
})

(define_insn "addsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ,dJ")
		 (match_operand:SI 2 "arith_operand" "d,Q")))]
  "!TARGET_MIPS16"
  "@
    addu\t%0,%z1,%2
    addiu\t%0,%z1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

;; For the mips16, we need to recognize stack pointer additions
;; explicitly, since we don't have a constraint for $sp.  These insns
;; will be generated by the save_restore_insns functions.

(define_insn ""
  [(set (reg:SI 29)
	(plus:SI (reg:SI 29)
		 (match_operand:SI 0 "small_int" "I")))]
  "TARGET_MIPS16"
  "addu\t%$,%$,%0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_simm8_8" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(plus:SI (reg:SI 29)
		 (match_operand:SI 1 "small_int" "I")))]
  "TARGET_MIPS16"
  "addu\t%0,%$,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")	(if_then_else (match_operand:VOID 1 "m16_uimm8_4" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(plus:SI (match_operand:SI 1 "register_operand" "0,d,d")
		 (match_operand:SI 2 "arith_operand" "Q,O,d")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[1]) != REG
       || REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[1]))
       || REGNO (operands[1]) == ARG_POINTER_REGNUM
       || REGNO (operands[1]) == FRAME_POINTER_REGNUM
       || REGNO (operands[1]) == STACK_POINTER_REGNUM)
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[2]))
       || REGNO (operands[2]) == ARG_POINTER_REGNUM
       || REGNO (operands[2]) == FRAME_POINTER_REGNUM
       || REGNO (operands[2]) == STACK_POINTER_REGNUM)"
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "addu\t%0,%2";
  else
    return "addu\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_simm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 2 "m16_simm4_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])


;; On the mips16, we can sometimes split an add of a constant which is
;; a 4 byte instruction into two adds which are both 2 byte
;; instructions.  There are two cases: one where we are adding a
;; constant plus a register to another register, and one where we are
;; simply adding a constant to a register.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_dup 0)
		 (match_operand:SI 1 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x7f
	&& INTVAL (operands[1]) <= 0x7f + 0x7f)
       || (INTVAL (operands[1]) < - 0x80
	   && INTVAL (operands[1]) >= - 0x80 - 0x80))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 2)))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val >= 0)
    {
      operands[1] = GEN_INT (0x7f);
      operands[2] = GEN_INT (val - 0x7f);
    }
  else
    {
      operands[1] = GEN_INT (- 0x80);
      operands[2] = GEN_INT (val + 0x80);
    }
})

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0x7f)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x80))"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
{
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val >= 0)
    {
      operands[2] = GEN_INT (0x7);
      operands[3] = GEN_INT (val - 0x7);
    }
  else
    {
      operands[2] = GEN_INT (- 0x8);
      operands[3] = GEN_INT (val + 0x8);
    }
})

(define_expand "adddi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (plus:DI (match_operand:DI 1 "register_operand" "")
			    (match_operand:DI 2 "arith_operand" "")))
	      (clobber (match_dup 3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
{
  /* If a large stack adjustment was forced into a register, we may be
     asked to generate rtx such as:

	(set (reg:DI sp) (plus:DI (reg:DI sp) (reg:DI pseudo)))

     but no such instruction is available in mips16.  Handle it by
     using a temporary.  */
  if (TARGET_MIPS16
      && REGNO (operands[0]) == STACK_POINTER_REGNUM
      && ((GET_CODE (operands[1]) == REG
	   && REGNO (operands[1]) != STACK_POINTER_REGNUM)
	  || GET_CODE (operands[2]) != CONST_INT))
    {
      rtx tmp = gen_reg_rtx (DImode);

      emit_move_insn (tmp, operands[1]);
      emit_insn (gen_adddi3 (tmp, tmp, operands[2]));
      emit_move_insn (operands[0], tmp);
      DONE;
    }

  if (TARGET_64BIT)
    {
      emit_insn (gen_adddi3_internal_3 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "adddi3_internal_1"
  [(set (match_operand:DI 0 "register_operand" "=d,&d")
	(plus:DI (match_operand:DI 1 "register_operand" "0,d")
		 (match_operand:DI 2 "register_operand" "d,d")))
   (clobber (match_operand:SI 3 "register_operand" "=d,d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
{
  return (REGNO (operands[0]) == REGNO (operands[1])
	  && REGNO (operands[0]) == REGNO (operands[2]))
    ? "srl\t%3,%L0,31\;sll\t%M0,%M0,1\;sll\t%L0,%L1,1\;addu\t%M0,%M0,%3"
    : "addu\t%L0,%L1,%L2\;sltu\t%3,%L0,%L2\;addu\t%M0,%M1,%M2\;addu\t%M0,%M0,%3";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "register_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1])
       || REGNO (operands[0]) != REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 1) 0)
		 (subreg:SI (match_dup 2) 0)))

   (set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 0) 0)
		(subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 4)
	(plus:SI (subreg:SI (match_dup 1) 4)
		 (subreg:SI (match_dup 2) 4)))

   (set (subreg:SI (match_dup 0) 4)
	(plus:SI (subreg:SI (match_dup 0) 4)
		 (match_dup 3)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "register_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1])
       || REGNO (operands[0]) != REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 4)
	(plus:SI (subreg:SI (match_dup 1) 4)
		 (subreg:SI (match_dup 2) 4)))

   (set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 0) 4)
		(subreg:SI (match_dup 2) 4)))

   (set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 1) 0)
		 (subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 0) 0)
		 (match_dup 3)))]
  "")

(define_insn "adddi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(plus:DI (match_operand:DI 1 "register_operand" "%d,d,d")
		 (match_operand:DI 2 "small_int" "P,J,N")))
   (clobber (match_operand:SI 3 "register_operand" "=d,d,d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "@
   addu\t%L0,%L1,%2\;sltu\t%3,%L0,%2\;addu\t%M0,%M1,%3
   move\t%L0,%L1\;move\t%M0,%M1
   subu\t%L0,%L1,%n2\;sltu\t%3,%L0,%2\;subu\t%M0,%M1,1\;addu\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12,8,16")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0"

  [(set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 1) 0)
		 (match_dup 2)))

   (set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 0) 0)
		(match_dup 2)))

   (set (subreg:SI (match_dup 0) 4)
	(plus:SI (subreg:SI (match_dup 1) 4)
		 (match_dup 3)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0"

  [(set (subreg:SI (match_dup 0) 4)
	(plus:SI (subreg:SI (match_dup 1) 4)
		 (match_dup 2)))

   (set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 0) 4)
		(match_dup 2)))

   (set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 1) 0)
		 (match_dup 3)))]
  "")

(define_insn "adddi3_internal_3"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(plus:DI (match_operand:DI 1 "reg_or_0_operand" "dJ,dJ")
		 (match_operand:DI 2 "arith_operand" "d,Q")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
    daddu\t%0,%z1,%2
    daddiu\t%0,%z1,%2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

;; For the mips16, we need to recognize stack pointer additions
;; explicitly, since we don't have a constraint for $sp.  These insns
;; will be generated by the save_restore_insns functions.

(define_insn ""
  [(set (reg:DI 29)
	(plus:DI (reg:DI 29)
		 (match_operand:DI 0 "small_int" "I")))]
  "TARGET_MIPS16 && TARGET_64BIT"
  "daddu\t%$,%$,%0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_simm8_8" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(plus:DI (reg:DI 29)
		 (match_operand:DI 1 "small_int" "I")))]
  "TARGET_MIPS16 && TARGET_64BIT"
  "daddu\t%0,%$,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_uimm5_4" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(plus:DI (match_operand:DI 1 "register_operand" "0,d,d")
		 (match_operand:DI 2 "arith_operand" "Q,O,d")))]
  "TARGET_MIPS16 && TARGET_64BIT
   && (GET_CODE (operands[1]) != REG
       || REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[1]))
       || REGNO (operands[1]) == ARG_POINTER_REGNUM
       || REGNO (operands[1]) == FRAME_POINTER_REGNUM
       || REGNO (operands[1]) == STACK_POINTER_REGNUM)
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[2]))
       || REGNO (operands[2]) == ARG_POINTER_REGNUM
       || REGNO (operands[2]) == FRAME_POINTER_REGNUM
       || REGNO (operands[2]) == STACK_POINTER_REGNUM)"
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "daddu\t%0,%2";
  else
    return "daddu\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_simm5_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 2 "m16_simm4_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])


;; On the mips16, we can sometimes split an add of a constant which is
;; a 4 byte instruction into two adds which are both 2 byte
;; instructions.  There are two cases: one where we are adding a
;; constant plus a register to another register, and one where we are
;; simply adding a constant to a register.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_dup 0)
		 (match_operand:DI 1 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0xf
	&& INTVAL (operands[1]) <= 0xf + 0xf)
       || (INTVAL (operands[1]) < - 0x10
	   && INTVAL (operands[1]) >= - 0x10 - 0x10))"
  [(set (match_dup 0) (plus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 2)))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val >= 0)
    {
      operands[1] = GEN_INT (0xf);
      operands[2] = GEN_INT (val - 0xf);
    }
  else
    {
      operands[1] = GEN_INT (- 0x10);
      operands[2] = GEN_INT (val + 0x10);
    }
})

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0xf)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x10))"
  [(set (match_dup 0) (plus:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 3)))]
{
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val >= 0)
    {
      operands[2] = GEN_INT (0x7);
      operands[3] = GEN_INT (val - 0x7);
    }
  else
    {
      operands[2] = GEN_INT (- 0x8);
      operands[3] = GEN_INT (val + 0x8);
    }
})

(define_insn "addsi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(sign_extend:DI (plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ,dJ")
				 (match_operand:SI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
    addu\t%0,%z1,%2
    addiu\t%0,%z1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(sign_extend:DI (plus:SI (match_operand:SI 1 "register_operand" "0,d,d")
				 (match_operand:SI 2 "arith_operand" "Q,O,d"))))]
  "TARGET_MIPS16 && TARGET_64BIT"
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "addu\t%0,%2";
  else
    return "addu\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_simm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 2 "m16_simm4_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

;;
;;  ....................
;;
;;	SUBTRACTION
;;
;;  ....................
;;

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "sub.d\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"DF")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "sub.s\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"SF")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "register_operand" "")
		  (match_operand:SI 2 "register_operand" "")))]
  ""
  "")

(define_insn "subsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (match_operand:SI 1 "register_operand" "d")
		  (match_operand:SI 2 "register_operand" "d")))]
  ""
  "subu\t%0,%z1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_expand "subdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=d")
		   (minus:DI (match_operand:DI 1 "register_operand" "d")
			     (match_operand:DI 2 "register_operand" "d")))
	      (clobber (match_dup 3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_subdi3_internal_3 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "subdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "sltu\t%3,%L1,%L2\;subu\t%L0,%L1,%L2\;subu\t%M0,%M1,%M2\;subu\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "register_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 1) 0)
		(subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 1) 0)
		  (subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 4)
	(minus:SI (subreg:SI (match_dup 1) 4)
		  (subreg:SI (match_dup 2) 4)))

   (set (subreg:SI (match_dup 0) 4)
	(minus:SI (subreg:SI (match_dup 0) 4)
		  (match_dup 3)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "register_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 1) 4)
	        (subreg:SI (match_dup 2) 4)))

   (set (subreg:SI (match_dup 0) 4)
	(minus:SI (subreg:SI (match_dup 1) 4)
		  (subreg:SI (match_dup 2) 4)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 1) 0)
		  (subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 0) 0)
		  (match_dup 3)))]
  "")

(define_insn "subdi3_internal_3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT"
  "dsubu\t%0,%1,%2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

(define_insn "subsi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "register_operand" "d")
		      (match_operand:SI 2 "register_operand" "d"))))]
  "TARGET_64BIT"
  "subu\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

(define_expand "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "")

(define_insn "muldf3_internal"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_4300_MUL_FIX"
  "mul.d\t%0,%1,%2"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"DF")])

;; Early VR4300 silicon has a CPU bug where multiplies with certain
;; operands may corrupt immediately following multiplies. This is a
;; simple fix to insert NOPs.

(define_insn "muldf3_r4300"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_4300_MUL_FIX"
  "mul.d\t%0,%1,%2\;nop"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8")])

(define_expand "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "")

(define_insn "mulsf3_internal"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && !TARGET_4300_MUL_FIX"
  "mul.s\t%0,%1,%2"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"SF")])

;; See muldf3_r4300.

(define_insn "mulsf3_r4300"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_4300_MUL_FIX"
  "mul.s\t%0,%1,%2\;nop"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"SF")
   (set_attr "length"	"8")])


;; ??? The R4000 (only) has a cpu bug.  If a double-word shift executes while
;; a multiply is in progress, it may give an incorrect result.  Avoid
;; this by keeping the mflo with the mult on the R4000.

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))]
  ""
{
  if (GENERATE_MULT3_SI || TARGET_MAD)
    emit_insn (gen_mulsi3_mult3 (operands[0], operands[1], operands[2]));
  else if (!TARGET_MIPS4000 || TARGET_MIPS16)
    emit_insn (gen_mulsi3_internal (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_mulsi3_r4000 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "mulsi3_mult3"
  [(set (match_operand:SI 0 "register_operand" "=d,l")
	(mult:SI (match_operand:SI 1 "register_operand" "d,d")
		 (match_operand:SI 2 "register_operand" "d,d")))
   (clobber (match_scratch:SI 3 "=h,h"))
   (clobber (match_scratch:SI 4 "=l,X"))]
  "GENERATE_MULT3_SI
   || TARGET_MAD"
{
  if (which_alternative == 1)
    return "mult\t%1,%2";
  if (TARGET_MAD
      || TARGET_MIPS5400
      || TARGET_MIPS5500
      || TARGET_MIPS7000
      || TARGET_MIPS9000
      || ISA_MIPS32
      || ISA_MIPS32R2
      || ISA_MIPS64)
    return "mul\t%0,%1,%2";
  return "mult\t%0,%1,%2";
}
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

;; If a register gets allocated to LO, and we spill to memory, the reload
;; will include a move from LO to a GPR.  Merge it into the multiplication
;; if it can set the GPR directly.
;;
;; Operand 0: LO
;; Operand 1: GPR (1st multiplication operand)
;; Operand 2: GPR (2nd multiplication operand)
;; Operand 3: HI
;; Operand 4: GPR (destination)
(define_peephole2
  [(parallel
       [(set (match_operand:SI 0 "register_operand" "")
	     (mult:SI (match_operand:SI 1 "register_operand" "")
		      (match_operand:SI 2 "register_operand" "")))
        (clobber (match_operand:SI 3 "register_operand" ""))
        (clobber (scratch:SI))])
   (set (match_operand:SI 4 "register_operand" "")
        (match_dup 0))]
  "GENERATE_MULT3_SI
   && true_regnum (operands[0]) == LO_REGNUM
   && GP_REG_P (true_regnum (operands[4]))
   && peep2_reg_dead_p (2, operands[0])"
  [(parallel
       [(set (match_dup 4)
	     (mult:SI (match_dup 1)
		      (match_dup 2)))
        (clobber (match_dup 3))
        (clobber (match_dup 0))])])

(define_insn "mulsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))]
  "!TARGET_MIPS4000 || TARGET_MIPS16"
  "mult\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "mulsi3_r4000"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=l"))]
  "TARGET_MIPS4000 && !TARGET_MIPS16"
  "mult\t%1,%2\;mflo\t%0"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"   "8")])

;; Multiply-accumulate patterns

;; For processors that can copy the output to a general register:
;;
;; The all-d alternative is needed because the combiner will find this
;; pattern and then register alloc/reload will move registers around to
;; make them fit, and we don't want to trigger unnecessary loads to LO.
;;
;; The last alternative should be made slightly less desirable, but adding
;; "?" to the constraint is too strong, and causes values to be loaded into
;; LO even when that's more costly.  For now, using "*d" mostly does the
;; trick.
(define_insn "*mul_acc_si"
  [(set (match_operand:SI 0 "register_operand" "=l,*d,*d")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d,d")
			  (match_operand:SI 2 "register_operand" "d,d,d"))
		 (match_operand:SI 3 "register_operand" "0,l,*d")))
   (clobber (match_scratch:SI 4 "=h,h,h"))
   (clobber (match_scratch:SI 5 "=X,3,l"))
   (clobber (match_scratch:SI 6 "=X,X,&d"))]
  "(TARGET_MIPS3900
   || ISA_HAS_MADD_MSUB)
   && !TARGET_MIPS16"
{
  static const char *const madd[] = { "madd\t%1,%2", "madd\t%0,%1,%2" };
  if (which_alternative == 2)
    return "#";
  if (ISA_HAS_MADD_MSUB && which_alternative != 0)
    return "#";
  return madd[which_alternative];
}
  [(set_attr "type"	"imadd,imadd,multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,4,8")])

;; Split the above insn if we failed to get LO allocated.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
			  (match_operand:SI 2 "register_operand" ""))
		 (match_operand:SI 3 "register_operand" "")))
   (clobber (match_scratch:SI 4 ""))
   (clobber (match_scratch:SI 5 ""))
   (clobber (match_scratch:SI 6 ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && GP_REG_P (true_regnum (operands[3]))"
  [(parallel [(set (match_dup 6)
		   (mult:SI (match_dup 1) (match_dup 2)))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))])
   (set (match_dup 0) (plus:SI (match_dup 6) (match_dup 3)))]
  "")

;; Splitter to copy result of MADD to a general register
(define_split
  [(set (match_operand:SI                   0 "register_operand" "")
        (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
                          (match_operand:SI 2 "register_operand" ""))
                 (match_operand:SI          3 "register_operand" "")))
   (clobber (match_scratch:SI               4 ""))
   (clobber (match_scratch:SI               5 ""))
   (clobber (match_scratch:SI               6 ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && true_regnum (operands[3]) == LO_REGNUM"
  [(parallel [(set (match_dup 3)
                   (plus:SI (mult:SI (match_dup 1) (match_dup 2))
                            (match_dup 3)))
              (clobber (match_dup 4))
              (clobber (match_dup 5))
              (clobber (match_dup 6))])
   (set (match_dup 0) (match_dup 3))]
  "")

(define_insn "*macc"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d")
			  (match_operand:SI 2 "register_operand" "d,d"))
		 (match_operand:SI 3 "register_operand" "0,l")))
   (clobber (match_scratch:SI 4 "=h,h"))
   (clobber (match_scratch:SI 5 "=X,3"))]
  "ISA_HAS_MACC"
{
  if (which_alternative == 1)
    return "macc\t%0,%1,%2";
  else if (TARGET_MIPS5500)
    return "madd\t%1,%2";
  else
    return "macc\t%.,%1,%2";
}
  [(set_attr "type" "imadd")
   (set_attr "mode" "SI")])

;; Pattern generated by define_peephole2 below
(define_insn "*macc2"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d")
			  (match_operand:SI 2 "register_operand" "d"))
		 (match_dup 0)))
   (set (match_operand:SI 3 "register_operand" "=d")
	(plus:SI (mult:SI (match_dup 1)
			  (match_dup 2))
		 (match_dup 0)))
   (clobber (match_scratch:SI 4 "=h"))]
  "ISA_HAS_MACC && reload_completed"
  "macc\t%3,%1,%2"
  [(set_attr "type"	"imadd")
   (set_attr "mode"	"SI")])

;; Convert macc $0,<r1>,<r2> & mflo <r3> into macc <r3>,<r1>,<r2>
;;
;; Operand 0: LO
;; Operand 1: GPR (1st multiplication operand)
;; Operand 2: GPR (2nd multiplication operand)
;; Operand 3: HI
;; Operand 4: GPR (destination)
(define_peephole2
  [(parallel
       [(set (match_operand:SI 0 "register_operand" "")
	     (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
			       (match_operand:SI 2 "register_operand" ""))
		      (match_dup 0)))
	(clobber (match_operand:SI 3 "register_operand" ""))
	(clobber (scratch:SI))])
   (set (match_operand:SI 4 "register_operand" "")
	(match_dup 0))]
  "ISA_HAS_MACC
   && true_regnum (operands[0]) == LO_REGNUM
   && GP_REG_P (true_regnum (operands[4]))"
  [(parallel [(set (match_dup 0)
		   (plus:SI (mult:SI (match_dup 1)
				     (match_dup 2))
			    (match_dup 0)))
	      (set (match_dup 4)
		   (plus:SI (mult:SI (match_dup 1)
				     (match_dup 2))
			    (match_dup 0)))
	      (clobber (match_dup 3))])]
  "")

;; When we have a three-address multiplication instruction, it should
;; be faster to do a separate multiply and add, rather than moving
;; something into LO in order to use a macc instruction.
;;
;; This peephole needs a scratch register to cater for the case when one
;; of the multiplication operands is the same as the destination.
;;
;; Operand 0: GPR (scratch)
;; Operand 1: LO
;; Operand 2: GPR (addend)
;; Operand 3: GPR (destination)
;; Operand 4: GPR (1st multiplication operand)
;; Operand 5: GPR (2nd multiplication operand)
;; Operand 6: HI
(define_peephole2
  [(match_scratch:SI 0 "d")
   (set (match_operand:SI 1 "register_operand" "")
	(match_operand:SI 2 "register_operand" ""))
   (match_dup 0)
   (parallel
       [(set (match_operand:SI 3 "register_operand" "")
	     (plus:SI (mult:SI (match_operand:SI 4 "register_operand" "")
			       (match_operand:SI 5 "register_operand" ""))
		      (match_dup 1)))
	(clobber (match_operand:SI 6 "register_operand" ""))
	(clobber (match_dup 1))])]
  "ISA_HAS_MACC && GENERATE_MULT3_SI
   && true_regnum (operands[1]) == LO_REGNUM
   && peep2_reg_dead_p (2, operands[1])
   && GP_REG_P (true_regnum (operands[3]))"
  [(parallel [(set (match_dup 0)
		   (mult:SI (match_dup 4)
			    (match_dup 5)))
	      (clobber (match_dup 6))
	      (clobber (match_dup 1))])
   (set (match_dup 3)
	(plus:SI (match_dup 0)
		 (match_dup 2)))]
  "")

;; Same as above, except LO is the initial target of the macc.
;;
;; Operand 0: GPR (scratch)
;; Operand 1: LO
;; Operand 2: GPR (addend)
;; Operand 3: GPR (1st multiplication operand)
;; Operand 4: GPR (2nd multiplication operand)
;; Operand 5: HI
;; Operand 6: GPR (destination)
(define_peephole2
  [(match_scratch:SI 0 "d")
   (set (match_operand:SI 1 "register_operand" "")
	(match_operand:SI 2 "register_operand" ""))
   (match_dup 0)
   (parallel
       [(set (match_dup 1)
	     (plus:SI (mult:SI (match_operand:SI 3 "register_operand" "")
			       (match_operand:SI 4 "register_operand" ""))
		      (match_dup 1)))
	(clobber (match_operand:SI 5 "register_operand" ""))
	(clobber (scratch:SI))])
   (match_dup 0)
   (set (match_operand:SI 6 "register_operand" "")
	(match_dup 1))]
  "ISA_HAS_MACC && GENERATE_MULT3_SI
   && true_regnum (operands[1]) == LO_REGNUM
   && peep2_reg_dead_p (3, operands[1])
   && GP_REG_P (true_regnum (operands[6]))"
  [(parallel [(set (match_dup 0)
		   (mult:SI (match_dup 3)
			    (match_dup 4)))
	      (clobber (match_dup 5))
	      (clobber (match_dup 1))])
   (set (match_dup 6)
	(plus:SI (match_dup 0)
		 (match_dup 2)))]
  "")

(define_insn "*mul_sub_si"
  [(set (match_operand:SI 0 "register_operand" "=l,*d,*d")
        (minus:SI (match_operand:SI 1 "register_operand" "0,l,*d")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d,d")
                           (match_operand:SI 3 "register_operand" "d,d,d"))))
   (clobber (match_scratch:SI 4 "=h,h,h"))
   (clobber (match_scratch:SI 5 "=X,1,l"))
   (clobber (match_scratch:SI 6 "=X,X,&d"))]
  "ISA_HAS_MADD_MSUB"
  "@
   msub\t%2,%3
   #
   #"
  [(set_attr "type"     "imadd,multi,multi")
   (set_attr "mode"     "SI")
   (set_attr "length"   "4,8,8")])

;; Split the above insn if we failed to get LO allocated.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (minus:SI (match_operand:SI 1 "register_operand" "")
                  (mult:SI (match_operand:SI 2 "register_operand" "")
                           (match_operand:SI 3 "register_operand" ""))))
   (clobber (match_scratch:SI 4 ""))
   (clobber (match_scratch:SI 5 ""))
   (clobber (match_scratch:SI 6 ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && GP_REG_P (true_regnum (operands[1]))"
  [(parallel [(set (match_dup 6)
                   (mult:SI (match_dup 2) (match_dup 3)))
              (clobber (match_dup 4))
              (clobber (match_dup 5))])
   (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 6)))]
  "")

;; Splitter to copy result of MSUB to a general register
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (minus:SI (match_operand:SI 1 "register_operand" "")
                  (mult:SI (match_operand:SI 2 "register_operand" "")
                           (match_operand:SI 3 "register_operand" ""))))
   (clobber (match_scratch:SI 4 ""))
   (clobber (match_scratch:SI 5 ""))
   (clobber (match_scratch:SI 6 ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE
   && GP_REG_P (true_regnum (operands[0]))
   && true_regnum (operands[1]) == LO_REGNUM"
  [(parallel [(set (match_dup 1)
                   (minus:SI (match_dup 1)
                             (mult:SI (match_dup 2) (match_dup 3))))
              (clobber (match_dup 4))
              (clobber (match_dup 5))
              (clobber (match_dup 6))])
   (set (match_dup 0) (match_dup 1))]
  "")

(define_insn "*muls"
  [(set (match_operand:SI                  0 "register_operand" "=l,d")
        (neg:SI (mult:SI (match_operand:SI 1 "register_operand" "d,d")
                         (match_operand:SI 2 "register_operand" "d,d"))))
   (clobber (match_scratch:SI              3                    "=h,h"))
   (clobber (match_scratch:SI              4                    "=X,l"))]
  "ISA_HAS_MULS"
  "@
   muls\t$0,%1,%2
   muls\t%0,%1,%2"
  [(set_attr "type"     "imul")
   (set_attr "mode"     "SI")])

(define_insn "*msac"
  [(set (match_operand:SI 0 "register_operand" "=l,d")
        (minus:SI (match_operand:SI 1 "register_operand" "0,l")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d")
                           (match_operand:SI 3 "register_operand" "d,d"))))
   (clobber (match_scratch:SI 4 "=h,h"))
   (clobber (match_scratch:SI 5 "=X,1"))]
  "ISA_HAS_MSAC"
{
  if (which_alternative == 1)
    return "msac\t%0,%2,%3";
  else if (TARGET_MIPS5500)
    return "msub\t%2,%3";
  else
    return "msac\t$0,%2,%3";
}
  [(set_attr "type"     "imadd")
   (set_attr "mode"     "SI")])

(define_expand "muldi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "register_operand" "")))]
  "TARGET_64BIT"
{
  if (GENERATE_MULT3_DI || TARGET_MIPS4000)
    emit_insn (gen_muldi3_internal2 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_muldi3_internal (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "muldi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(mult:DI (match_operand:DI 1 "register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_scratch:DI 3 "=h"))]
  "TARGET_64BIT && !TARGET_MIPS4000"
  "dmult\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")])

(define_insn "muldi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operand:DI 1 "register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_scratch:DI 3 "=h"))
   (clobber (match_scratch:DI 4 "=l"))]
  "TARGET_64BIT && (GENERATE_MULT3_DI || TARGET_MIPS4000)"
{
  if (GENERATE_MULT3_DI)
    return "dmult\t%0,%1,%2";
  else
    return "dmult\t%1,%2\;mflo\t%0";
}
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ne (symbol_ref "GENERATE_MULT3_DI") (const_int 0))
		      (const_int 4)
		      (const_int 8)))])

;; ??? We could define a mulditi3 pattern when TARGET_64BIT.

(define_expand "mulsidi3"
  [(parallel
      [(set (match_operand:DI 0 "register_operand" "")
	    (mult:DI
	       (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
	       (sign_extend:DI (match_operand:SI 2 "register_operand" ""))))
       (clobber (scratch:DI))
       (clobber (scratch:DI))
       (clobber (scratch:DI))])]
  ""
{
  if (!TARGET_64BIT)
    {
      emit_insn (gen_mulsidi3_32bit (operands[0], operands[1], operands[2]));
      DONE;
    }
})

(define_insn "mulsidi3_32bit"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI
	   (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
	   (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_64BIT"
  "mult\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn_and_split "*mulsidi3_64bit"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operator:DI 1 "extend_operator"
		    [(match_operand:SI 3 "register_operand" "d")])
		 (match_operator:DI 2 "extend_operator"
		    [(match_operand:SI 4 "register_operand" "d")])))
   (clobber (match_scratch:DI 5 "=l"))
   (clobber (match_scratch:DI 6 "=h"))
   (clobber (match_scratch:DI 7 "=d"))]
  "TARGET_64BIT && GET_CODE (operands[1]) == GET_CODE (operands[2])"
  "#"
  "&& reload_completed"
  [(parallel
       [(set (match_dup 5)
	     (sign_extend:DI
		(mult:SI (match_dup 3)
		         (match_dup 4))))
	(set (match_dup 6)
	     (ashiftrt:DI
		(mult:DI (match_dup 1)
			 (match_dup 2))
		(const_int 32)))])

   ;; OP7 <- LO, OP0 <- HI
   (set (match_dup 7) (match_dup 5))
   (set (match_dup 0) (match_dup 6))

   ;; Zero-extend OP7.
   (set (match_dup 7)
	(ashift:DI (match_dup 7)
		   (const_int 32)))
   (set (match_dup 7)
	(lshiftrt:DI (match_dup 7)
		     (const_int 32)))

   ;; Shift OP0 into place.
   (set (match_dup 0)
	(ashift:DI (match_dup 0)
		   (const_int 32)))

   ;; OR the two halves together
   (set (match_dup 0)
	(ior:DI (match_dup 0)
		(match_dup 7)))]
  ""
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"	"24")])

(define_insn "*mulsidi3_64bit_parts"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(sign_extend:DI
	   (mult:SI (match_operand:SI 2 "register_operand" "d")
		    (match_operand:SI 3 "register_operand" "d"))))
   (set (match_operand:DI 1 "register_operand" "=h")
	(ashiftrt:DI
	   (mult:DI
	      (match_operator:DI 4 "extend_operator" [(match_dup 2)])
	      (match_operator:DI 5 "extend_operator" [(match_dup 3)]))
	   (const_int 32)))]
  "TARGET_64BIT && GET_CODE (operands[4]) == GET_CODE (operands[5])"
{
  if (GET_CODE (operands[4]) == SIGN_EXTEND)
    return "mult\t%2,%3";
  else
    return "multu\t%2,%3";
}
  [(set_attr "type" "imul")
   (set_attr "mode" "SI")])

(define_expand "umulsidi3"
  [(parallel
      [(set (match_operand:DI 0 "register_operand" "")
	    (mult:DI
	       (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
	       (zero_extend:DI (match_operand:SI 2 "register_operand" ""))))
       (clobber (scratch:DI))
       (clobber (scratch:DI))
       (clobber (scratch:DI))])]
  ""
{
  if (!TARGET_64BIT)
    {
      emit_insn (gen_umulsidi3_32bit (operands[0], operands[1],
				      operands[2]));
      DONE;
    }
})

(define_insn "umulsidi3_32bit"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI
	   (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
	   (zero_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_64BIT"
  "multu\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

;; Widening multiply with negation.
(define_insn "*muls_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (neg:DI
	 (mult:DI
	  (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
	  (sign_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && ISA_HAS_MULS"
  "muls\t$0,%1,%2"
  [(set_attr "type"     "imul")
   (set_attr "length"   "4")
   (set_attr "mode"     "SI")])

(define_insn "*umuls_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(neg:DI
	 (mult:DI
	  (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
	  (zero_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && ISA_HAS_MULS"
  "mulsu\t$0,%1,%2"
  [(set_attr "type"     "imul")
   (set_attr "length"   "4")
   (set_attr "mode"     "SI")])

(define_insn "*smsac_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (minus:DI
	   (match_operand:DI 3 "register_operand" "0")
	   (mult:DI
	      (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
	      (sign_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && ISA_HAS_MSAC"
{
  if (TARGET_MIPS5500)
    return "msub\t%1,%2";
  else
    return "msac\t$0,%1,%2";
}
  [(set_attr "type"     "imadd")
   (set_attr "length"   "4")
   (set_attr "mode"     "SI")])

(define_insn "*umsac_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(minus:DI
	   (match_operand:DI 3 "register_operand" "0")
	   (mult:DI
	      (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
	      (zero_extend:DI (match_operand:SI 2 "register_operand" "d")))))]
  "!TARGET_64BIT && ISA_HAS_MSAC"
{
  if (TARGET_MIPS5500)
    return "msubu\t%1,%2";
  else
    return "msacu\t$0,%1,%2";
}
  [(set_attr "type"     "imadd")
   (set_attr "length"   "4")
   (set_attr "mode"     "SI")])

;; _highpart patterns
(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
		   (zero_extend:DI (match_operand:SI 2 "register_operand" "")))
	  (const_int 32))))]
  ""
{
  if (ISA_HAS_MULHI)
    emit_insn (gen_umulsi3_highpart_mulhi_internal (operands[0], operands[1],
						    operands[2]));
  else
    emit_insn (gen_umulsi3_highpart_internal (operands[0], operands[1],
					      operands[2]));
  DONE;
})

(define_insn "umulsi3_highpart_internal"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
		   (zero_extend:DI (match_operand:SI 2 "register_operand" "d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l"))]
  "!ISA_HAS_MULHI"
  "multu\t%1,%2"
  [(set_attr "type"   "imul")
   (set_attr "mode"   "SI")
   (set_attr "length" "4")])

(define_insn "umulsi3_highpart_mulhi_internal"
  [(set (match_operand:SI 0 "register_operand" "=h,d")
        (truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d,d"))
		   (zero_extend:DI (match_operand:SI 2 "register_operand" "d,d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l,l"))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "ISA_HAS_MULHI"
  "@
   multu\t%1,%2
   mulhiu\t%0,%1,%2"
  [(set_attr "type"   "imul")
   (set_attr "mode"   "SI")
   (set_attr "length" "4")])

(define_insn "umulsi3_highpart_neg_mulhi_internal"
  [(set (match_operand:SI 0 "register_operand" "=h,d")
        (truncate:SI
	 (lshiftrt:DI
	  (neg:DI
	   (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d,d"))
		    (zero_extend:DI (match_operand:SI 2 "register_operand" "d,d"))))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l,l"))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "ISA_HAS_MULHI"
  "@
   mulshiu\t%.,%1,%2
   mulshiu\t%0,%1,%2"
  [(set_attr "type"   "imul")
   (set_attr "mode"   "SI")
   (set_attr "length" "4")])

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
		   (sign_extend:DI (match_operand:SI 2 "register_operand" "")))
         (const_int 32))))]
  ""
{
  if (ISA_HAS_MULHI)
    emit_insn (gen_smulsi3_highpart_mulhi_internal (operands[0], operands[1],
						    operands[2]));
  else
    emit_insn (gen_smulsi3_highpart_internal (operands[0], operands[1],
					      operands[2]));
  DONE;
})

(define_insn "smulsi3_highpart_internal"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
		   (sign_extend:DI (match_operand:SI 2 "register_operand" "d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l"))]
  "!ISA_HAS_MULHI"
  "mult\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"   "4")])

(define_insn "smulsi3_highpart_mulhi_internal"
  [(set (match_operand:SI 0 "register_operand" "=h,d")
        (truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d,d"))
		   (sign_extend:DI (match_operand:SI 2 "register_operand" "d,d")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l,l"))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "ISA_HAS_MULHI"
  "@
   mult\t%1,%2
   mulhi\t%0,%1,%2"
  [(set_attr "type"   "imul")
   (set_attr "mode"   "SI")
   (set_attr "length" "4")])

(define_insn "smulsi3_highpart_neg_mulhi_internal"
  [(set (match_operand:SI 0 "register_operand" "=h,d")
        (truncate:SI
	 (lshiftrt:DI
	  (neg:DI
	   (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d,d"))
		    (sign_extend:DI (match_operand:SI 2 "register_operand" "d,d"))))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=l,l"))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "ISA_HAS_MULHI"
  "@
   mulshi\t%.,%1,%2
   mulshi\t%0,%1,%2"
  [(set_attr "type"   "imul")
   (set_attr "mode"   "SI")])

(define_insn "smuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI
	   (sign_extend:TI (match_operand:DI 1 "register_operand" "d"))
	   (sign_extend:TI (match_operand:DI 2 "register_operand" "d")))
         (const_int 64))))
   (clobber (match_scratch:DI 3 "=l"))]
  "TARGET_64BIT"
  "dmult\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")])

(define_insn "umuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(truncate:DI
	 (lshiftrt:TI
	  (mult:TI
	   (zero_extend:TI (match_operand:DI 1 "register_operand" "d"))
	   (zero_extend:TI (match_operand:DI 2 "register_operand" "d")))
	  (const_int 64))))
   (clobber (match_scratch:DI 3 "=l"))]
  "TARGET_64BIT"
  "dmultu\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")])


;; The R4650 supports a 32 bit multiply/ 64 bit accumulate
;; instruction.  The HI/LO registers are used as a 64 bit accumulator.

(define_insn "madsi"
  [(set (match_operand:SI 0 "register_operand" "+l")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d")
			  (match_operand:SI 2 "register_operand" "d"))
		 (match_dup 0)))
   (clobber (match_scratch:SI 3 "=h"))]
  "TARGET_MAD"
  "mad\t%1,%2"
  [(set_attr "type"	"imadd")
   (set_attr "mode"	"SI")])

(define_insn "*umul_acc_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(plus:DI
	 (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
		  (zero_extend:DI (match_operand:SI 2 "register_operand" "d")))
	 (match_operand:DI 3 "register_operand" "0")))]
  "(TARGET_MAD || ISA_HAS_MACC)
   && !TARGET_64BIT"
{
  if (TARGET_MAD)
    return "madu\t%1,%2";
  else if (TARGET_MIPS5500)
    return "maddu\t%1,%2";
  else
    return "maccu\t%.,%1,%2";
}
  [(set_attr "type"   "imadd")
   (set_attr "mode"   "SI")])


(define_insn "*smul_acc_di"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(plus:DI
	 (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
		  (sign_extend:DI (match_operand:SI 2 "register_operand" "d")))
	 (match_operand:DI 3 "register_operand" "0")))]
  "(TARGET_MAD || ISA_HAS_MACC)
   && !TARGET_64BIT"
{
  if (TARGET_MAD)
    return "mad\t%1,%2";
  else if (TARGET_MIPS5500)
    return "madd\t%1,%2";
  else
    return "macc\t%.,%1,%2";
}
  [(set_attr "type"   "imadd")
   (set_attr "mode"   "SI")])

;; Floating point multiply accumulate instructions.

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (mult:DF (match_operand:DF 1 "register_operand" "f")
			  (match_operand:DF 2 "register_operand" "f"))
		 (match_operand:DF 3 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD"
  "madd.d\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (mult:SF (match_operand:SF 1 "register_operand" "f")
			  (match_operand:SF 2 "register_operand" "f"))
		 (match_operand:SF 3 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_FUSED_MADD"
  "madd.s\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (mult:DF (match_operand:DF 1 "register_operand" "f")
			   (match_operand:DF 2 "register_operand" "f"))
		  (match_operand:DF 3 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD"
  "msub.d\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (mult:SF (match_operand:SF 1 "register_operand" "f")
			   (match_operand:SF 2 "register_operand" "f"))
		  (match_operand:SF 3 "register_operand" "f")))]

  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_FUSED_MADD"
  "msub.s\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (plus:DF (mult:DF (match_operand:DF 1 "register_operand" "f")
				  (match_operand:DF 2 "register_operand" "f"))
			 (match_operand:DF 3 "register_operand" "f"))))]
  "ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD"
  "nmadd.d\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (plus:SF (mult:SF (match_operand:SF 1 "register_operand" "f")
				  (match_operand:SF 2 "register_operand" "f"))
			 (match_operand:SF 3 "register_operand" "f"))))]
  "ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_FUSED_MADD"
  "nmadd.s\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (mult:DF (match_operand:DF 2 "register_operand" "f")
			   (match_operand:DF 3 "register_operand" "f"))))]
  "ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_FUSED_MADD"
  "nmsub.d\t%0,%1,%2,%3"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (mult:SF (match_operand:SF 2 "register_operand" "f")
			   (match_operand:SF 3 "register_operand" "f"))))]
  "ISA_HAS_NMADD_NMSUB && TARGET_HARD_FLOAT && TARGET_FUSED_MADD"
  "nmsub.s\t%0,%1,%2,%3"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

;;
;;  ....................
;;
;;	DIVISION and REMAINDER
;;
;;  ....................
;;

(define_expand "divdf3"
  [(set (match_operand:DF 0 "register_operand" "")
	(div:DF (match_operand:DF 1 "reg_or_const_float_1_operand" "")
		(match_operand:DF 2 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  if (const_float_1_operand (operands[1], DFmode))
    if (!(ISA_HAS_FP4 && flag_unsafe_math_optimizations))
      operands[1] = force_reg (DFmode, operands[1]);
})

;; This pattern works around the early SB-1 rev2 core "F1" erratum:
;;
;; If an mfc1 or dmfc1 happens to access the floating point register
;; file at the same time a long latency operation (div, sqrt, recip,
;; sqrt) iterates an intermediate result back through the floating
;; point register file bypass, then instead returning the correct
;; register value the mfc1 or dmfc1 operation returns the intermediate
;; result of the long latency operation.
;;
;; The workaround is to insert an unconditional 'mov' from/to the
;; long latency op destination register.

(define_insn "*divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  if (TARGET_FIX_SB1)
    return "div.d\t%0,%1,%2\;mov.d\t%0,%0";
  else
    return "div.d\t%0,%1,%2";
}
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"DF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])


;; This pattern works around the early SB-1 rev2 core "F2" erratum:
;;
;; In certain cases, div.s and div.ps may have a rounding error
;; and/or wrong inexact flag.
;;
;; Therefore, we only allow div.s if not working around SB-1 rev2
;; errata, or if working around those errata and a slight loss of
;; precision is OK (i.e., flag_unsafe_math_optimizations is set).
(define_expand "divsf3"
  [(set (match_operand:SF 0 "register_operand" "")
	(div:SF (match_operand:SF 1 "reg_or_const_float_1_operand" "")
		(match_operand:SF 2 "register_operand" "")))]
  "TARGET_HARD_FLOAT && (!TARGET_FIX_SB1 || flag_unsafe_math_optimizations)"
{
  if (const_float_1_operand (operands[1], SFmode))
    if (!(ISA_HAS_FP4 && flag_unsafe_math_optimizations))
      operands[1] = force_reg (SFmode, operands[1]);
})

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
;;
;; This pattern works around the early SB-1 rev2 core "F2" erratum (see
;; "divsf3" comment for details).
(define_insn "*divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && (!TARGET_FIX_SB1 || flag_unsafe_math_optimizations)"
{
  if (TARGET_FIX_SB1)
    return "div.s\t%0,%1,%2\;mov.s\t%0,%0";
  else
    return "div.s\t%0,%1,%2";
}
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "const_float_1_operand" "")
		(match_operand:DF 2 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "recip.d\t%0,%2\;mov.d\t%0,%0";
  else
    return "recip.d\t%0,%2";
}
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"DF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "const_float_1_operand" "")
		(match_operand:SF 2 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "recip.s\t%0,%2\;mov.s\t%0,%0";
  else
    return "recip.s\t%0,%2";
}
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

(define_insn "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=h")
	(mod:SI (match_dup 1)
		(match_dup 2)))]
  ""
  { return mips_output_division ("div\t$0,%1,%2", operands); }
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_insn "divmoddi4"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(div:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "register_operand" "d")))
   (set (match_operand:DI 3 "register_operand" "=h")
	(mod:DI (match_dup 1)
		(match_dup 2)))]
  "TARGET_64BIT"
  { return mips_output_division ("ddiv\t$0,%1,%2", operands); }
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"DI")])

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=h")
	(umod:SI (match_dup 1)
		 (match_dup 2)))]
  ""
  { return mips_output_division ("divu\t$0,%1,%2", operands); }
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_insn "udivmoddi4"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(udiv:DI (match_operand:DI 1 "register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))
   (set (match_operand:DI 3 "register_operand" "=h")
	(umod:DI (match_dup 1)
		 (match_dup 2)))]
  "TARGET_64BIT"
  { return mips_output_division ("ddivu\t$0,%1,%2", operands); }
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"DI")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && HAVE_SQRT_P() && TARGET_DOUBLE_FLOAT"
{
  if (TARGET_FIX_SB1)
    return "sqrt.d\t%0,%1\;mov.d\t%0,%0";
  else
    return "sqrt.d\t%0,%1";
}
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"DF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && HAVE_SQRT_P()"
{
  if (TARGET_FIX_SB1)
    return "sqrt.s\t%0,%1\;mov.s\t%0,%0";
  else
    return "sqrt.s\t%0,%1";
}
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"SF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "const_float_1_operand" "")
		(sqrt:DF (match_operand:DF 2 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "rsqrt.d\t%0,%2\;mov.d\t%0,%0";
  else
    return "rsqrt.d\t%0,%2";
}
  [(set_attr "type"	"frsqrt")
   (set_attr "mode"	"DF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;; This pattern works around the early SB-1 rev2 core "F1" erratum (see
;; "divdf3" comment for details).
(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "const_float_1_operand" "")
		(sqrt:SF (match_operand:SF 2 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && flag_unsafe_math_optimizations"
{
  if (TARGET_FIX_SB1)
    return "rsqrt.s\t%0,%2\;mov.s\t%0,%0";
  else
    return "rsqrt.s\t%0,%2";
}
  [(set_attr "type"	"frsqrt")
   (set_attr "mode"	"SF")
   (set (attr "length")
        (if_then_else (ne (symbol_ref "TARGET_FIX_SB1") (const_int 0))
                      (const_int 8)
                      (const_int 4)))])

;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................

;; Do not use the integer abs macro instruction, since that signals an
;; exception on -2147483648 (sigh).

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(abs:SI (match_operand:SI 1 "register_operand" "d")))]
  "!TARGET_MIPS16"
{
  operands[2] = const0_rtx;

  if (REGNO (operands[0]) == REGNO (operands[1]))
    {
      if (GENERATE_BRANCHLIKELY)
	return "%(bltzl\t%1,1f\;subu\t%0,%z2,%0\n%~1:%)";
      else
	return "bgez\t%1,1f%#\;subu\t%0,%z2,%0\n%~1:";
    }
  else
    return "%(bgez\t%1,1f\;move\t%0,%1\;subu\t%0,%z2,%0\n%~1:%)";
}
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"12")])

(define_insn "absdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(abs:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  unsigned int regno1;
  operands[2] = const0_rtx;

  if (GET_CODE (operands[1]) == REG)
    regno1 = REGNO (operands[1]);
  else
    regno1 = REGNO (XEXP (operands[1], 0));

  if (REGNO (operands[0]) == regno1)
    return "%(bltzl\t%1,1f\;dsubu\t%0,%z2,%0\n%~1:%)";
  else
    return "%(bgez\t%1,1f\;move\t%0,%1\;dsubu\t%0,%z2,%0\n%~1:%)";
}
  [(set_attr "type"	"multi")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "abs.d\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"DF")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs.s\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"SF")])

;;
;;  ....................
;;
;;	FIND FIRST BIT INSTRUCTION
;;
;;  ....................
;;

(define_insn "ffssi2"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(ffs:SI (match_operand:SI 1 "register_operand" "d")))
   (clobber (match_scratch:SI 2 "=&d"))
   (clobber (match_scratch:SI 3 "=&d"))]
  "!TARGET_MIPS16"
{
  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return "%(\
move\t%0,%.\;\
beq\t%1,%.,2f\n\
%~1:\tand\t%2,%1,0x0001\;\
addu\t%0,%0,1\;\
beq\t%2,%.,1b\;\
srl\t%1,%1,1\n\
%~2:%)";

  return "%(\
move\t%0,%.\;\
move\t%3,%1\;\
beq\t%3,%.,2f\n\
%~1:\tand\t%2,%3,0x0001\;\
addu\t%0,%0,1\;\
beq\t%2,%.,1b\;\
srl\t%3,%3,1\n\
%~2:%)";
}
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"28")])

(define_insn "ffsdi2"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ffs:DI (match_operand:DI 1 "register_operand" "d")))
   (clobber (match_scratch:DI 2 "=&d"))
   (clobber (match_scratch:DI 3 "=&d"))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return "%(\
move\t%0,%.\;\
beq\t%1,%.,2f\n\
%~1:\tand\t%2,%1,0x0001\;\
daddu\t%0,%0,1\;\
beq\t%2,%.,1b\;\
dsrl\t%1,%1,1\n\
%~2:%)";

  return "%(\
move\t%0,%.\;\
move\t%3,%1\;\
beq\t%3,%.,2f\n\
%~1:\tand\t%2,%3,0x0001\;\
daddu\t%0,%0,1\;\
beq\t%2,%.,1b\;\
dsrl\t%3,%3,1\n\
%~2:%)";
}
  [(set_attr "type"	"multi")
   (set_attr "mode"	"DI")
   (set_attr "length"	"28")])

;;
;;  ...................
;;
;;  Count leading zeroes.
;;
;;  ...................
;;

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(clz:SI (match_operand:SI 1 "register_operand" "d")))]
  "ISA_HAS_CLZ_CLO"
  "clz\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "clzdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(clz:DI (match_operand:DI 1 "register_operand" "d")))]
  "ISA_HAS_DCLZ_DCLO"
  "dclz\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

;;
;;  ....................
;;
;;	NEGATION and ONE'S COMPLEMENT
;;
;;  ....................

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(neg:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
{
  if (TARGET_MIPS16)
    return "neg\t%0,%1";
  else
    return "subu\t%0,%.,%1";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_expand "negdi2"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=d")
		   (neg:DI (match_operand:DI 1 "register_operand" "d")))
	      (clobber (match_dup 2))])]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16"
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_negdi2_internal_2 (operands[0], operands[1]));
      DONE;
    }

  operands[2] = gen_reg_rtx (SImode);
})

(define_insn "negdi2_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "! TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "subu\t%L0,%.,%L1\;subu\t%M0,%.,%M1\;sltu\t%2,%.,%L0\;subu\t%M0,%M0,%2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])

(define_insn "negdi2_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "dsubu\t%0,%.,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "neg.d\t%0,%1"
  [(set_attr "type"	"fneg")
   (set_attr "mode"	"DF")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "neg.s\t%0,%1"
  [(set_attr "type"	"fneg")
   (set_attr "mode"	"SF")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(not:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
{
  if (TARGET_MIPS16)
    return "not\t%0,%1";
  else
    return "nor\t%0,%.,%1";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT"
{
  if (TARGET_MIPS16)
    return "not\t%0,%1";
  else
    return "nor\t%0,%.,%1";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

;; Many of these instructions use trivial define_expands, because we
;; want to use a different set of constraints when TARGET_MIPS16.

(define_expand "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(and:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
{
  if (TARGET_MIPS16)
    {
      operands[1] = force_reg (SImode, operands[1]);
      operands[2] = force_reg (SImode, operands[2]);
    }
})

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(and:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   and\t%0,%1,%2
   andi\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "and\t%0,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_expand "anddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "uns_arith_operand" "")))]
  "TARGET_64BIT"
{
  if (TARGET_MIPS16)
    {
      operands[1] = force_reg (DImode, operands[1]);
      operands[2] = force_reg (DImode, operands[2]);
    }
})

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(and:DI (match_operand:DI 1 "register_operand" "d,d")
		(match_operand:DI 2 "uns_arith_operand" "d,K")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
   and\t%0,%1,%2
   andi\t%0,%1,%x2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "register_operand" "0")
		(match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "and\t%0,%2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
{
  if (TARGET_MIPS16)
    {
      operands[1] = force_reg (SImode, operands[1]);
      operands[2] = force_reg (SImode, operands[2]);
    }
})

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   or\t%0,%1,%2
   ori\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "or\t%0,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_expand "iordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "uns_arith_operand" "")))]
  "TARGET_64BIT"
{
  if (TARGET_MIPS16)
    {
      operands[1] = force_reg (DImode, operands[1]);
      operands[2] = force_reg (DImode, operands[2]);
    }
})

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ior:DI (match_operand:DI 1 "register_operand" "d,d")
		(match_operand:DI 2 "uns_arith_operand" "d,K")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
   or\t%0,%1,%2
   ori\t%0,%1,%x2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "register_operand" "0")
		(match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "or\t%0,%2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

(define_expand "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(xor:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(xor:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   xor\t%0,%1,%2
   xori\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,t,t")
	(xor:SI (match_operand:SI 1 "uns_arith_operand" "%0,d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K,d")))]
  "TARGET_MIPS16"
  "@
   xor\t%0,%2
   cmpi\t%1,%2
   cmp\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

(define_expand "xordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "uns_arith_operand" "")))]
  "TARGET_64BIT"
{
  if (TARGET_MIPS16)
    {
      operands[1] = force_reg (DImode, operands[1]);
      operands[2] = force_reg (DImode, operands[2]);
    }
})

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(xor:DI (match_operand:DI 1 "register_operand" "d,d")
		(match_operand:DI 2 "uns_arith_operand" "d,K")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
   xor\t%0,%1,%2
   xori\t%0,%1,%x2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,t,t")
	(xor:DI (match_operand:DI 1 "register_operand" "%0,d,d")
		(match_operand:DI 2 "uns_arith_operand" "d,K,d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "@
   xor\t%0,%2
   cmpi\t%1,%2
   cmp\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

(define_insn "*norsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "d"))
		(not:SI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_MIPS16"
  "nor\t%0,%z1,%z2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "*nordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "d"))
		(not:DI (match_operand:DI 2 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "nor\t%0,%z1,%z2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

;;
;;  ....................
;;
;;	TRUNCATION
;;
;;  ....................



(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "cvt.s.d\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")])

;; Integer truncation patterns.  Truncating SImode values to smaller
;; modes is a no-op, as it is for most other GCC ports.  Truncating
;; DImode values to SImode is not a no-op for TARGET_64BIT since we
;; need to make sure that the lower 32 bits are properly sign-extended
;; (see TRULY_NOOP_TRUNCATION).  Truncating DImode values into modes
;; smaller than SImode is equivalent to two separate truncations:
;;
;;                        A       B
;;    DI ---> HI  ==  DI ---> SI ---> HI
;;    DI ---> QI  ==  DI ---> SI ---> QI
;;
;; Step A needs a real instruction but step B does not.

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,m")
        (truncate:SI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sll\t%0,%1,0
    sw\t%1,%0"
  [(set_attr "type" "darith,store")
   (set_attr "mode" "SI")
   (set_attr "extended_mips16" "yes,*")])

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,m")
        (truncate:HI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sll\t%0,%1,0
    sh\t%1,%0"
  [(set_attr "type" "darith,store")
   (set_attr "mode" "SI")
   (set_attr "extended_mips16" "yes,*")])

(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,m")
        (truncate:QI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sll\t%0,%1,0
    sb\t%1,%0"
  [(set_attr "type" "darith,store")
   (set_attr "mode" "SI")
   (set_attr "extended_mips16" "yes,*")])

;; Combiner patterns to optimize shift/truncate combinations.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (truncate:SI (ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
                                  (match_operand:DI 2 "small_int" "I"))))]
  "TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) >= 32"
  "dsra\t%0,%1,%2"
  [(set_attr "type" "darith")
   (set_attr "mode" "SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (truncate:SI (lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
                                  (const_int 32))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "dsra\t%0,%1,32"
  [(set_attr "type" "darith")
   (set_attr "mode" "SI")])


;; Combiner patterns for truncate/sign_extend combinations.  They use
;; the shift/truncate patterns above.

(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(sign_extend:SI
	    (truncate:HI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (const_int 48)))
   (set (match_dup 0)
	(truncate:SI (ashiftrt:DI (match_dup 2)
				  (const_int 48))))]
  { operands[2] = gen_lowpart (DImode, operands[0]); })

(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(sign_extend:SI
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (const_int 56)))
   (set (match_dup 0)
	(truncate:SI (ashiftrt:DI (match_dup 2)
				  (const_int 56))))]
  { operands[2] = gen_lowpart (DImode, operands[0]); })


;; Combiner patterns to optimize truncate/zero_extend combinations.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI (truncate:HI
                         (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\t%0,%1,0xffff"
  [(set_attr "type"     "darith")
   (set_attr "mode"     "SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI (truncate:QI
                         (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\t%0,%1,0xff"
  [(set_attr "type"     "darith")
   (set_attr "mode"     "SI")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d")
        (zero_extend:HI (truncate:QI
                         (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\t%0,%1,0xff"
  [(set_attr "type"     "darith")
   (set_attr "mode"     "HI")])

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
        (lshiftrt:DI (match_dup 0) (const_int 32)))]
  "operands[1] = gen_lowpart (DImode, operands[1]);"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "*zero_extendsidi2_mem"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (zero_extend:DI (match_operand:SI 1 "memory_operand" "W")))]
  "TARGET_64BIT"
  "lwu\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "DI")])

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  ""
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (SImode, operands[1]);
      rtx temp = force_reg (SImode, GEN_INT (0xffff));

      emit_insn (gen_andsi3 (operands[0], op, temp));
      DONE;
    }
})

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  "!TARGET_MIPS16"
  "@
   andi\t%0,%1,0xffff
   lhu\t%0,%1"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "SI")
   (set_attr "length"   "4,*")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_MIPS16"
  "lhu\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "SI")])

(define_expand "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (DImode, operands[1]);
      rtx temp = force_reg (DImode, GEN_INT (0xffff));

      emit_insn (gen_anddi3 (operands[0], op, temp));
      DONE;
    }
})

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
   andi\t%0,%1,0xffff
   lhu\t%0,%1"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "DI")
   (set_attr "length"   "4,*")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
        (zero_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "lhu\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "DI")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op0 = gen_lowpart (SImode, operands[0]);
      rtx op1 = gen_lowpart (SImode, operands[1]);
      rtx temp = force_reg (SImode, GEN_INT (0xff));

      emit_insn (gen_andsi3 (op0, op1, temp));
      DONE;
    }
})

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "!TARGET_MIPS16"
  "@
   andi\t%0,%1,0x00ff
   lbu\t%0,%1"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "HI")
   (set_attr "length"   "4,*")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d")
        (zero_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_MIPS16"
  "lbu\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "HI")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (SImode, operands[1]);
      rtx temp = force_reg (SImode, GEN_INT (0xff));

      emit_insn (gen_andsi3 (operands[0], op, temp));
      DONE;
    }
})

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "!TARGET_MIPS16"
  "@
   andi\t%0,%1,0x00ff
   lbu\t%0,%1"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "SI")
   (set_attr "length"   "4,*")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_MIPS16"
  "lbu\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "SI")])

(define_expand "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (DImode, operands[1]);
      rtx temp = force_reg (DImode, GEN_INT (0xff));

      emit_insn (gen_anddi3 (operands[0], op, temp));
      DONE;
    }
})

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
   andi\t%0,%1,0x00ff
   lbu\t%0,%1"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "DI")
   (set_attr "length"   "4,*")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(zero_extend:DI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "lbu\t%0,%1"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DI")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "d,m")))]
  "TARGET_64BIT"
  "@
   sll\t%0,%1,0
   lw\t%0,%1"
  [(set_attr "type" "arith,load")
   (set_attr "mode" "DI")
   (set_attr "extended_mips16" "yes,*")])

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the lh and lb insns.

;; These expanders originally put values in registers first. We split
;; all non-mem patterns after reload.

(define_expand "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (sign_extend:DI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "")

(define_insn "*extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:HI 1 "register_operand" "d")))]
  "TARGET_64BIT"
  "#")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (sign_extend:DI (match_operand:HI 1 "register_operand" "")))]
  "TARGET_64BIT && reload_completed"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 48)))
   (set (match_dup 0)
        (ashiftrt:DI (match_dup 0) (const_int 48)))]
  "operands[1] = gen_lowpart (DImode, operands[1]);")

(define_insn "*extendhidi2_mem"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_64BIT"
  "lh\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "DI")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  ""
{
  if (ISA_HAS_SEB_SEH)
    {
      emit_insn (gen_extendhisi2_hw (operands[0],
				     force_reg (HImode, operands[1])));
      DONE;
    }
})

(define_insn "*extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "#")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  "reload_completed"
  [(set (match_dup 0)
        (ashift:SI (match_dup 1) (const_int 16)))
   (set (match_dup 0)
        (ashiftrt:SI (match_dup 0) (const_int 16)))]
  "operands[1] = gen_lowpart (SImode, operands[1]);")

(define_insn "extendhisi2_mem"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "lh\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "SI")])

(define_insn "extendhisi2_hw"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  "ISA_HAS_SEB_SEH"
  "seh\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
        (sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
  "")

(define_insn "*extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
        (sign_extend:HI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "#")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
        (sign_extend:HI (match_operand:QI 1 "register_operand" "")))]
  "reload_completed"
  [(set (match_dup 0)
        (ashift:SI (match_dup 1) (const_int 24)))
   (set (match_dup 0)
        (ashiftrt:SI (match_dup 0) (const_int 24)))]
  "operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);")

(define_insn "*extendqihi2_internal_mem"
  [(set (match_operand:HI 0 "register_operand" "=d")
        (sign_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "lb\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "SI")])


(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
{
  if (ISA_HAS_SEB_SEH)
    {
      emit_insn (gen_extendqisi2_hw (operands[0],
				     force_reg (QImode, operands[1])));
      DONE;
    }
})

(define_insn "*extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (sign_extend:SI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "#")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  "reload_completed"
  [(set (match_dup 0)
        (ashift:SI (match_dup 1) (const_int 24)))
   (set (match_dup 0)
        (ashiftrt:SI (match_dup 0) (const_int 24)))]
  "operands[1] = gen_lowpart (SImode, operands[1]);")

(define_insn "*extendqisi2_mem"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "lb\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "SI")])

(define_insn "extendqisi2_hw"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  "ISA_HAS_SEB_SEH"
  "seb\t%0,%1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_expand "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
        (sign_extend:DI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "")

(define_insn "*extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:QI 1 "register_operand" "d")))]
  "TARGET_64BIT"
  "#")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (sign_extend:DI (match_operand:QI 1 "register_operand" "")))]
  "TARGET_64BIT && reload_completed"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 56)))
   (set (match_dup 0)
        (ashiftrt:DI (match_dup 0) (const_int 56)))]
  "operands[1] = gen_lowpart (DImode, operands[1]);")

(define_insn "*extendqidi2_mem"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_64BIT"
  "lb\t%0,%1"
  [(set_attr "type"     "load")
   (set_attr "mode"     "DI")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "cvt.d.s\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

(define_expand "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  if (!ISA_HAS_TRUNC_W)
    {
      emit_insn (gen_fix_truncdfsi2_macro (operands[0], operands[1]));
      DONE;
    }
})

(define_insn "fix_truncdfsi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && ISA_HAS_TRUNC_W"
  "trunc.w.d %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4")])

(define_insn "fix_truncdfsi2_macro"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))
   (clobber (match_scratch:DF 2 "=d"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !ISA_HAS_TRUNC_W"
{
  if (set_nomacro)
    return ".set\tmacro\;trunc.w.d %0,%1,%2\;.set\tnomacro";
  else
    return "trunc.w.d %0,%1,%2";
}
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"36")])

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
{
  if (!ISA_HAS_TRUNC_W)
    {
      emit_insn (gen_fix_truncsfsi2_macro (operands[0], operands[1]));
      DONE;
    }
})

(define_insn "fix_truncsfsi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && ISA_HAS_TRUNC_W"
  "trunc.w.s %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4")])

(define_insn "fix_truncsfsi2_macro"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))
   (clobber (match_scratch:SF 2 "=d"))]
  "TARGET_HARD_FLOAT && !ISA_HAS_TRUNC_W"
{
  if (set_nomacro)
    return ".set\tmacro\;trunc.w.s %0,%1,%2\;.set\tnomacro";
  else
    return "trunc.w.s %0,%1,%2";
}
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"36")])


(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "trunc.l.d %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4")])


(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "trunc.l.s %0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4")])


(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "cvt.d.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4")])


(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "cvt.d.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4")])


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "cvt.s.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4")])


(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FLOAT64 && TARGET_DOUBLE_FLOAT"
  "cvt.s.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4")])


(define_expand "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31);

  if (reg1)			/* Turn off complaints about unreached code.  */
    {
      emit_move_insn (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, DFmode));
      do_pending_stack_adjust ();

      emit_insn (gen_cmpdf (operands[1], reg1));
      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncdfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
      emit_move_insn (reg3, GEN_INT (trunc_int_for_mode
				     (BITMASK_HIGH, SImode)));

      emit_insn (gen_fix_truncdfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* Allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
      DONE;
    }
})


(define_expand "fixuns_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unsigned_fix:DI (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (DImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 63);

  emit_move_insn (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, DFmode));
  do_pending_stack_adjust ();

  emit_insn (gen_cmpdf (operands[1], reg1));
  emit_jump_insn (gen_bge (label1));

  emit_insn (gen_fix_truncdfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  emit_move_insn (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
  emit_move_insn (reg3, GEN_INT (BITMASK_HIGH));
  emit_insn (gen_ashldi3 (reg3, reg3, GEN_INT (32)));

  emit_insn (gen_fix_truncdfdi2 (operands[0], reg2));
  emit_insn (gen_iordi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
  DONE;
})


(define_expand "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT"
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31);

  emit_move_insn (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, SFmode));
  do_pending_stack_adjust ();

  emit_insn (gen_cmpsf (operands[1], reg1));
  emit_jump_insn (gen_bge (label1));

  emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  emit_move_insn (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
  emit_move_insn (reg3, GEN_INT (trunc_int_for_mode
				 (BITMASK_HIGH, SImode)));

  emit_insn (gen_fix_truncsfsi2 (operands[0], reg2));
  emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
  DONE;
})


(define_expand "fixuns_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unsigned_fix:DI (match_operand:SF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (DImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 63);

  emit_move_insn (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, SFmode));
  do_pending_stack_adjust ();

  emit_insn (gen_cmpsf (operands[1], reg1));
  emit_jump_insn (gen_bge (label1));

  emit_insn (gen_fix_truncsfdi2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, label2)));
  emit_barrier ();

  emit_label (label1);
  emit_move_insn (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
  emit_move_insn (reg3, GEN_INT (BITMASK_HIGH));
  emit_insn (gen_ashldi3 (reg3, reg3, GEN_INT (32)));

  emit_insn (gen_fix_truncsfdi2 (operands[0], reg2));
  emit_insn (gen_iordi3 (operands[0], operands[0], reg3));

  emit_label (label2);

  /* Allow REG_NOTES to be set on last insn (labels don't have enough
     fields, and can't be used for REG_NOTES anyway).  */
  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
  DONE;
})

;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; Bit field extract patterns which use lwl/lwr or ldl/ldr.

(define_expand "extv"
  [(set (match_operand 0 "register_operand" "")
	(sign_extract (match_operand:QI 1 "memory_operand" "")
		      (match_operand 2 "immediate_operand" "")
		      (match_operand 3 "immediate_operand" "")))]
  "!TARGET_MIPS16"
{
  if (mips_expand_unaligned_load (operands[0], operands[1],
				  INTVAL (operands[2]),
				  INTVAL (operands[3])))
    DONE;
  else
    FAIL;
})

(define_expand "extzv"
  [(set (match_operand 0 "register_operand" "")
	(zero_extract (match_operand:QI 1 "memory_operand" "")
		      (match_operand 2 "immediate_operand" "")
		      (match_operand 3 "immediate_operand" "")))]
  "!TARGET_MIPS16"
{
  if (mips_expand_unaligned_load (operands[0], operands[1],
				  INTVAL (operands[2]),
				  INTVAL (operands[3])))
    DONE;
  else
    FAIL;
})

(define_expand "insv"
  [(set (zero_extract (match_operand:QI 0 "memory_operand" "")
		      (match_operand 1 "immediate_operand" "")
		      (match_operand 2 "immediate_operand" ""))
	(match_operand 3 "reg_or_0_operand" ""))]
  "!TARGET_MIPS16"
{
  if (mips_expand_unaligned_store (operands[0], operands[3],
				   INTVAL (operands[1]),
				   INTVAL (operands[2])))
    DONE;
  else
    FAIL;
})

;; Unaligned word moves generated by the bit field patterns.
;;
;; As far as the rtl is concerned, both the left-part and right-part
;; instructions can access the whole field.  However, the real operand
;; refers to just the first or the last byte (depending on endianness).
;; We therefore use two memory operands to each instruction, one to
;; describe the rtl effect and one to use in the assembly output.
;;
;; Operands 0 and 1 are the rtl-level target and source respectively.
;; This allows us to use the standard length calculations for the "load"
;; and "store" type attributes.

(define_insn "mov_lwl"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:BLK 1 "memory_operand" "m")
		    (match_operand:QI 2 "memory_operand" "m")]
		   UNSPEC_LWL))]
  "!TARGET_MIPS16"
  "lwl\t%0,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "SI")])

(define_insn "mov_lwr"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:BLK 1 "memory_operand" "m")
		    (match_operand:QI 2 "memory_operand" "m")
		    (match_operand:SI 3 "register_operand" "0")]
		   UNSPEC_LWR))]
  "!TARGET_MIPS16"
  "lwr\t%0,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "SI")])


(define_insn "mov_swl"
  [(set (match_operand:BLK 0 "memory_operand" "=m")
	(unspec:BLK [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "m")]
		    UNSPEC_SWL))]
  "!TARGET_MIPS16"
  "swl\t%z1,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])

(define_insn "mov_swr"
  [(set (match_operand:BLK 0 "memory_operand" "+m")
	(unspec:BLK [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "m")
		     (match_dup 0)]
		    UNSPEC_SWR))]
  "!TARGET_MIPS16"
  "swr\t%z1,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])


(define_insn "mov_ldl"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(unspec:DI [(match_operand:BLK 1 "memory_operand" "m")
		    (match_operand:QI 2 "memory_operand" "m")]
		   UNSPEC_LDL))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "ldl\t%0,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "DI")])

(define_insn "mov_ldr"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(unspec:DI [(match_operand:BLK 1 "memory_operand" "m")
		    (match_operand:QI 2 "memory_operand" "m")
		    (match_operand:DI 3 "register_operand" "0")]
		   UNSPEC_LDR))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "ldr\t%0,%2"
  [(set_attr "type" "load")
   (set_attr "mode" "DI")])


(define_insn "mov_sdl"
  [(set (match_operand:BLK 0 "memory_operand" "=m")
	(unspec:BLK [(match_operand:DI 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "m")]
		    UNSPEC_SDL))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sdl\t%z1,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "DI")])

(define_insn "mov_sdr"
  [(set (match_operand:BLK 0 "memory_operand" "+m")
	(unspec:BLK [(match_operand:DI 1 "reg_or_0_operand" "dJ")
		     (match_operand:QI 2 "memory_operand" "m")
		     (match_dup 0)]
		    UNSPEC_SDR))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sdr\t%z1,%2"
  [(set_attr "type" "store")
   (set_attr "mode" "DI")])

;; Insns to fetch a global symbol from a big GOT.

(define_insn_and_split "*xgot_hisi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(high:SI (match_operand:SI 1 "global_got_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (high:SI (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
{
  operands[2] = mips_gotoff_global (operands[1]);
  operands[3] = pic_offset_table_rtx;
}
  [(set_attr "got" "xgot_high")])

(define_insn_and_split "*xgot_losi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "global_got_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 3))]
  { operands[3] = mips_load_got_global (operands[1], operands[2]); }
  [(set_attr "got" "load")])

(define_insn_and_split "*xgot_hidi"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(high:DI (match_operand:DI 1 "global_got_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (high:DI (match_dup 2)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 3)))]
{
  operands[2] = mips_gotoff_global (operands[1]);
  operands[3] = pic_offset_table_rtx;
}
  [(set_attr "got" "xgot_high")])

(define_insn_and_split "*xgot_lodi"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:DI 2 "global_got_operand" "")))]
  "TARGET_EXPLICIT_RELOCS && TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 3))]
  { operands[3] = mips_load_got_global (operands[1], operands[2]); }
  [(set_attr "got" "load")])

;; Insns to fetch a global symbol from a normal GOT.

(define_insn_and_split "*got_dispsi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(match_operand:SI 1 "global_got_operand" ""))]
  "TARGET_EXPLICIT_RELOCS && !TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_load_got_global (pic_offset_table_rtx, operands[1]); }
  [(set_attr "got" "load")])

(define_insn_and_split "*got_dispdi"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(match_operand:DI 1 "global_got_operand" ""))]
  "TARGET_EXPLICIT_RELOCS && !TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_load_got_global (pic_offset_table_rtx, operands[1]); }
  [(set_attr "got" "load")])

;; Insns for loading the high part of a local symbol.

(define_insn_and_split "*got_pagesi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(high:SI (match_operand:SI 1 "local_got_operand" "")))]
  "TARGET_EXPLICIT_RELOCS"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_load_got_page (operands[1]); }
  [(set_attr "got" "load")])

(define_insn_and_split "*got_pagedi"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(high:DI (match_operand:DI 1 "local_got_operand" "")))]
  "TARGET_EXPLICIT_RELOCS"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_load_got_page (operands[1]); }
  [(set_attr "got" "load")])

;; Instructions for adding the low 16 bits of an address to a register.
;; Operand 2 is the address: print_operand works out which relocation
;; should be applied.

(define_insn "*lowsi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "immediate_operand" "")))]
  "!TARGET_MIPS16"
  "addiu\t%0,%1,%R2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "*lowdi"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:DI 2 "immediate_operand" "")))]
  "!TARGET_MIPS16 && TARGET_64BIT"
  "daddiu\t%0,%1,%R2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "*lowsi_mips16"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "immediate_operand" "")))]
  "TARGET_MIPS16"
  "addiu\t%0,%R2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_insn "*lowdi_mips16"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:DI 2 "immediate_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT"
  "daddiu\t%0,%R2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

;; 64-bit integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "" ""))]
  ""
{
  if (mips_legitimize_move (DImode, operands[0], operands[1]))
    DONE;

  /* If we are generating embedded PIC code, and we are referring to a
     symbol in the .text section, we must use an offset from the start
     of the function.  */
  if (TARGET_EMBEDDED_PIC
      && (GET_CODE (operands[1]) == LABEL_REF
	  || (GET_CODE (operands[1]) == SYMBOL_REF
	      && ! SYMBOL_REF_FLAG (operands[1]))))
    {
      rtx temp;

      temp = embedded_pic_offset (operands[1]);
      temp = gen_rtx_PLUS (Pmode, embedded_pic_fnaddr_reg (),
			   force_reg (DImode, temp));
      emit_move_insn (operands[0], force_reg (DImode, temp));
      DONE;
    }
})

;; For mips16, we need a special case to handle storing $31 into
;; memory, since we don't have a constraint to match $31.  This
;; instruction can be generated by save_restore_insns.

(define_insn ""
  [(set (match_operand:DI 0 "stack_operand" "=m")
	(reg:DI 31))]
  "TARGET_MIPS16 && TARGET_64BIT"
  "sd\t$31,%0"
  [(set_attr "type"	"store")
   (set_attr "mode"	"DI")])

(define_insn "movdi_internal"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,m,*x,*d,*x,*B*C*D,*B*C*D,*d,*m")
	(match_operand:DI 1 "general_operand" "d,iF,m,d,J,*x,*d,*d,*m,*B*C*D,*B*C*D"))]
  "!TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,arith,load,store,hilo,hilo,hilo,xfer,load,xfer,store")
   (set_attr "mode"	"DI")
   (set_attr "length"   "8,16,*,*,8,8,8,8,*,8,*")])

(define_insn ""
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:DI 1 "general_operand" "d,d,y,K,N,m,d,*x"))]
  "!TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,move,move,arith,arith,load,store,hilo")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8,8,8,8,12,*,*,8")])

(define_insn "movdi_internal2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*f,*d,*m,*x,*d,*x,*B*C*D,*B*C*D,*d,*m")
	(match_operand:DI 1 "move_operand" "d,U,T,m,dJ,*f,*d*J,*m,*f,*f,*J,*x,*d,*d,*m,*B*C*D,*B*C*D"))]
  "TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,const,const,load,store,move,xfer,load,xfer,store,hilo,hilo,hilo,xfer,load,xfer,store")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,*,*,*,*,4,4,*,4,*,4,4,4,8,*,8,*")])

(define_insn "*movdi_internal2_mips16"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,m,*d")
	(match_operand:DI 1 "move_operand" "d,d,y,K,N,U,m,d,*x"))]
  "TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,move,move,arith,arith,const,load,store,hilo")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (const_int 4)
		 (const_int 4)
		 (if_then_else (match_operand:VOID 1 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 1 "m16_nuimm8_1" "")
			       (const_int 8)
			       (const_int 12))
		 (const_string "*")
		 (const_string "*")
		 (const_string "*")
		 (const_int 4)])])


;; On the mips16, we can split ld $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(mem:DI (plus:DI (match_dup 0)
			 (match_operand:DI 1 "const_int_operand" ""))))]
  "TARGET_64BIT && TARGET_MIPS16 && reload_completed
   && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x10)
       || (INTVAL (operands[1]) >= 32 * 8
	   && INTVAL (operands[1]) <= 31 * 8 + 0x8)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 8
	   && (INTVAL (operands[1]) & 7) != 0))"
  [(set (match_dup 0) (plus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:DI (plus:DI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = GEN_INT (0);
  else if (val >= 32 * 8)
    {
      int off = val & 7;

      operands[1] = GEN_INT (0x8 + off);
      operands[2] = GEN_INT (val - off - 0x8);
    }
  else
    {
      int off = val & 7;

      operands[1] = GEN_INT (off);
      operands[2] = GEN_INT (val - off);
    }
})

;; 32-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "" ""))]
  ""
{
  if (mips_legitimize_move (SImode, operands[0], operands[1]))
    DONE;

  /* If we are generating embedded PIC code, and we are referring to a
     symbol in the .text section, we must use an offset from the start
     of the function.  */
  if (TARGET_EMBEDDED_PIC
      && (GET_CODE (operands[1]) == LABEL_REF
	  || (GET_CODE (operands[1]) == SYMBOL_REF
	      && ! SYMBOL_REF_FLAG (operands[1]))))
    {
      rtx temp;

      temp = embedded_pic_offset (operands[1]);
      temp = gen_rtx_PLUS (Pmode, embedded_pic_fnaddr_reg (),
			   force_reg (SImode, temp));
      emit_move_insn (operands[0], force_reg (SImode, temp));
      DONE;
    }
})

;; We can only store $ra directly into a small sp offset.

(define_insn ""
  [(set (match_operand:SI 0 "stack_operand" "=m")
	(reg:SI 31))]
  "TARGET_MIPS16"
  "sw\t$31,%0"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SI")])

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*f,*d,*m,*d,*z,*x,*d,*x,*B*C*D,*B*C*D,*d,*m")
	(match_operand:SI 1 "move_operand" "d,U,T,m,dJ,*f,*d*J,*m,*f,*f,*z,*d,J,*x,*d,*d,*m,*B*C*D,*B*C*D"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,const,const,load,store,move,xfer,load,xfer,store,xfer,xfer,hilo,hilo,hilo,xfer,load,xfer,store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,*,*,*,*,4,4,*,4,*,4,4,4,4,4,4,*,4,*")])

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,m,*d")
	(match_operand:SI 1 "move_operand" "d,d,y,K,N,U,m,d,*x"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,move,move,arith,arith,const,load,store,hilo")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (const_int 4)
		 (const_int 4)
		 (if_then_else (match_operand:VOID 1 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 1 "m16_nuimm8_1" "")
			       (const_int 8)
			       (const_int 12))
		 (const_string "*")
		 (const_string "*")
		 (const_string "*")
		 (const_int 4)])])

;; On the mips16, we can split lw $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" ""))))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 4
	   && INTVAL (operands[1]) <= 31 * 4 + 0x7c)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 4
	   && (INTVAL (operands[1]) & 3) != 0))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:SI (plus:SI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = GEN_INT (0);
  else if (val >= 32 * 4)
    {
      int off = val & 3;

      operands[1] = GEN_INT (0x7c + off);
      operands[2] = GEN_INT (val - off - 0x7c);
    }
  else
    {
      int off = val & 3;

      operands[1] = GEN_INT (off);
      operands[2] = GEN_INT (val - off);
    }
})

;; On the mips16, we can split a load of certain constants into a load
;; and an add.  This turns a 4 byte instruction into 2 2 byte
;; instructions.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) >= 0x100
   && INTVAL (operands[1]) <= 0xff + 0x7f"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 2)))]
{
  int val = INTVAL (operands[1]);

  operands[1] = GEN_INT (0xff);
  operands[2] = GEN_INT (val - 0xff);
})

;; On the mips16, we can split a load of a negative constant into a
;; load and a neg.  That's what mips_output_move will generate anyhow.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) < 0
   && INTVAL (operands[1]) > - 0x8000"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (neg:SI (match_dup 0)))]
  { operands[1] = GEN_INT (- INTVAL (operands[1])); })

;; This insn handles moving CCmode values.  It's really just a
;; slightly simplified copy of movsi_internal2, with additional cases
;; to move a condition register to a general register and to move
;; between the general registers and the floating point registers.

(define_insn "movcc"
  [(set (match_operand:CC 0 "nonimmediate_operand" "=d,*d,*d,*m,*d,*f,*f,*f,*m")
	(match_operand:CC 1 "general_operand" "z,*d,*m,*d,*f,*d,*f,*m,*f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,move,load,store,xfer,xfer,move,load,store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8,4,*,*,4,4,4,*,*")])

;; Reload condition code registers.  reload_incc and reload_outcc
;; both handle moves from arbitrary operands into condition code
;; registers.  reload_incc handles the more common case in which
;; a source operand is constrained to be in a condition-code
;; register, but has not been allocated to one.
;;
;; Sometimes, such as in movcc, we have a CCmode destination whose
;; constraints do not include 'z'.  reload_outcc handles the case
;; when such an operand is allocated to a condition-code register.
;;
;; Note that reloads from a condition code register to some
;; other location can be done using ordinary moves.  Moving
;; into a GPR takes a single movcc, moving elsewhere takes
;; two.  We can leave these cases to the generic reload code.
(define_expand "reload_incc"
  [(set (match_operand:CC 0 "fcc_register_operand" "=z")
	(match_operand:CC 1 "general_operand" ""))
   (clobber (match_operand:TF 2 "register_operand" "=&f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
{
  mips_emit_fcc_reload (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "reload_outcc"
  [(set (match_operand:CC 0 "fcc_register_operand" "=z")
	(match_operand:CC 1 "register_operand" ""))
   (clobber (match_operand:TF 2 "register_operand" "=&f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
{
  mips_emit_fcc_reload (operands[0], operands[1], operands[2]);
  DONE;
})

;; MIPS4 supports loading and storing a floating point register from
;; the sum of two general registers.  We use two versions for each of
;; these four instructions: one where the two general registers are
;; SImode, and one where they are DImode.  This is because general
;; registers will be in SImode when they hold 32 bit values, but,
;; since the 32 bit values are always sign extended, the [ls][wd]xc1
;; instructions will still work correctly.

;; ??? Perhaps it would be better to support these instructions by
;; modifying GO_IF_LEGITIMATE_ADDRESS and friends.  However, since
;; these instructions can only be used to load and store floating
;; point registers, that would probably cause trouble in reload.

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mem:SF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "lwxc1\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mem:SF (plus:DI (match_operand:DI 1 "register_operand" "d")
			 (match_operand:DI 2 "register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "lwxc1\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mem:DF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "ldxc1\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mem:DF (plus:DI (match_operand:DI 1 "register_operand" "d")
			 (match_operand:DI 2 "register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "ldxc1\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (mem:SF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d")))
	(match_operand:SF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "swxc1\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (mem:SF (plus:DI (match_operand:DI 1 "register_operand" "d")
			 (match_operand:DI 2 "register_operand" "d")))
	(match_operand:SF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "swxc1\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (mem:DF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d")))
	(match_operand:DF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "sdxc1\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"DF")
   (set_attr "length"   "4")])

(define_insn ""
  [(set (mem:DF (plus:DI (match_operand:DI 1 "register_operand" "d")
			 (match_operand:DI 2 "register_operand" "d")))
	(match_operand:DF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "sdxc1\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"DF")
   (set_attr "length"   "4")])

;; 16-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because LOAD_EXTEND_OP returns ZERO_EXTEND.

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
{
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], HImode)
      && !register_operand (operands[1], HImode)
      && (TARGET_MIPS16
	  || (GET_CODE (operands[1]) != CONST_INT
	  || INTVAL (operands[1]) != 0)))
    {
      rtx temp = force_reg (HImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
})

(define_insn "movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,m,*d,*f,*f,*x,*d")
	(match_operand:HI 1 "general_operand"       "d,IK,m,dJ,*f,*d,*f,*d,*x"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "@
    move\t%0,%1
    li\t%0,%1
    lhu\t%0,%1
    sh\t%z1,%0
    mfc1\t%0,%1
    mtc1\t%1,%0
    mov.s\t%0,%1
    mt%0\t%1
    mf%1\t%0"
  [(set_attr "type"	"move,arith,load,store,xfer,xfer,move,hilo,hilo")
   (set_attr "mode"	"HI")
   (set_attr "length"	"4,4,*,*,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:HI 1 "general_operand"      "d,d,y,K,N,m,d,*x"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "@
    move\t%0,%1
    move\t%0,%1
    move\t%0,%1
    li\t%0,%1
    li\t%0,%n1\;neg\t%0
    lhu\t%0,%1
    sh\t%1,%0
    mf%1\t%0"
  [(set_attr "type"	"move,move,move,arith,arith,load,store,hilo")
   (set_attr "mode"	"HI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (const_int 4)
		 (const_int 4)
		 (if_then_else (match_operand:VOID 1 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 1 "m16_nuimm8_1" "")
			       (const_int 8)
			       (const_int 12))
		 (const_string "*")
		 (const_string "*")
		 (const_int 4)])])


;; On the mips16, we can split lh $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(mem:HI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" ""))))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 2
	   && INTVAL (operands[1]) <= 31 * 2 + 0x7e)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 2
	   && (INTVAL (operands[1]) & 1) != 0))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:HI (plus:SI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = GEN_INT (0);
  else if (val >= 32 * 2)
    {
      int off = val & 1;

      operands[1] = GEN_INT (0x7e + off);
      operands[2] = GEN_INT (val - off - 0x7e);
    }
  else
    {
      int off = val & 1;

      operands[1] = GEN_INT (off);
      operands[2] = GEN_INT (val - off);
    }
})

;; 8-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because LOAD_EXTEND_OP returns ZERO_EXTEND.

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
{
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], QImode)
      && !register_operand (operands[1], QImode)
      && (TARGET_MIPS16
	  || (GET_CODE (operands[1]) != CONST_INT
	  || INTVAL (operands[1]) != 0)))
    {
      rtx temp = force_reg (QImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
})

(define_insn "movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,m,*d,*f,*f,*x,*d")
	(match_operand:QI 1 "general_operand"       "d,IK,m,dJ,*f,*d,*f,*d,*x"))]
  "!TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "@
    move\t%0,%1
    li\t%0,%1
    lbu\t%0,%1
    sb\t%z1,%0
    mfc1\t%0,%1
    mtc1\t%1,%0
    mov.s\t%0,%1
    mt%0\t%1
    mf%1\t%0"
  [(set_attr "type"	"move,arith,load,store,xfer,xfer,move,hilo,hilo")
   (set_attr "mode"	"QI")
   (set_attr "length"	"4,4,*,*,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,y,d,d,d,d,m,*d")
	(match_operand:QI 1 "general_operand"      "d,d,y,K,N,m,d,*x"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
    move\t%0,%1
    move\t%0,%1
    move\t%0,%1
    li\t%0,%1
    li\t%0,%n1\;neg\t%0
    lbu\t%0,%1
    sb\t%1,%0
    mf%1\t%0"
  [(set_attr "type"	"move,move,move,arith,arith,load,store,hilo")
   (set_attr "mode"	"QI")
   (set_attr "length"	"4,4,4,4,8,*,*,4")])

;; On the mips16, we can split lb $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:QI 0 "register_operand" "")
	(mem:QI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" ""))))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32
	   && INTVAL (operands[1]) <= 31 + 0x7f))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:QI (plus:SI (match_dup 0) (match_dup 2))))]
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = GEN_INT (0);
  else
    {
      operands[1] = GEN_INT (0x7f);
      operands[2] = GEN_INT (val - 0x7f);
    }
})

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
{
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SFmode)
      && !nonmemory_operand (operands[1], SFmode))
    operands[1] = force_reg (SFmode, operands[1]);
})

(define_insn "movsf_internal1"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,m,*f,*d,*d,*d,*m")
	(match_operand:SF 1 "general_operand" "f,G,m,fG,*d,*f,*G*d,*m,*d"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || nonmemory_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,xfer,load,store,xfer,xfer,move,load,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4,4,*,*,4,4,4,*,*")])

(define_insn "movsf_internal2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,m")
	(match_operand:SF 1 "general_operand" "      Gd,m,d"))]
  "TARGET_SOFT_FLOAT && !TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || nonmemory_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,load,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4,*,*")])

(define_insn ""
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,y,d,d,m")
	(match_operand:SF 1 "nonimmediate_operand" "d,d,y,m,d"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,move,move,load,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4,4,4,*,*")])


;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
{
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], DFmode)
      && !nonmemory_operand (operands[1], DFmode))
    operands[1] = force_reg (DFmode, operands[1]);
})

(define_insn "movdf_internal1a"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,*f,*d,*d,*d,*m")
	(match_operand:DF 1 "general_operand" "f,G,m,fG,*d,*f,*d*G,*m,*d"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && TARGET_64BIT
   && (register_operand (operands[0], DFmode)
       || nonmemory_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,xfer,load,store,xfer,xfer,move,load,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4,4,*,*,4,4,4,*,*")])

(define_insn "movdf_internal1b"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,*f,*d,*d,*d,*m")
	(match_operand:DF 1 "general_operand" "f,G,m,fG,*d,*f,*d*G,*m,*d"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_64BIT
   && (register_operand (operands[0], DFmode)
       || nonmemory_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,xfer,load,store,xfer,xfer,move,load,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4,8,*,*,8,8,8,*,*")])

(define_insn "movdf_internal2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,m,d,f,f")
	(match_operand:DF 1 "general_operand" "dG,m,dG,f,d,f"))]
  "(TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || nonmemory_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,load,store,xfer,xfer,move")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8,*,*,4,4,4")])

(define_insn ""
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,y,d,d,m")
	(match_operand:DF 1 "nonimmediate_operand" "d,d,y,m,d"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "type"	"move,move,move,load,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8,8,8,*,*")])

(define_split
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  "reload_completed && !TARGET_64BIT
   && mips_split_64bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  mips_split_64bit_move (operands[0], operands[1]);
  DONE;
})

(define_split
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  "reload_completed && !TARGET_64BIT
   && mips_split_64bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  mips_split_64bit_move (operands[0], operands[1]);
  DONE;
})

;; Patterns for loading or storing part of a paired floating point
;; register.  We need them because odd-numbered floating-point registers
;; are not fully independent: see mips_split_64bit_move.

;; Load the low word of operand 0 with operand 1.
(define_insn "load_df_low"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(unspec:DF [(match_operand:SI 1 "general_operand" "dJ,m")]
		   UNSPEC_LOAD_DF_LOW))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_64BIT"
{
  operands[0] = mips_subword (operands[0], 0);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "type"	"xfer,load")
   (set_attr "mode"	"SF")])

;; Load the high word of operand 0 from operand 1, preserving the value
;; in the low word.
(define_insn "load_df_high"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(unspec:DF [(match_operand:SI 1 "general_operand" "dJ,m")
		    (match_operand:DF 2 "register_operand" "0,0")]
		   UNSPEC_LOAD_DF_HIGH))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_64BIT"
{
  operands[0] = mips_subword (operands[0], 1);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "type"	"xfer,load")
   (set_attr "mode"	"SF")])

;; Store the high word of operand 1 in operand 0.  The corresponding
;; low-word move is done in the normal way.
(define_insn "store_df_high"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,m")
	(unspec:SI [(match_operand:DF 1 "register_operand" "f,f")]
		   UNSPEC_STORE_DF_HIGH))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && !TARGET_64BIT"
{
  operands[1] = mips_subword (operands[1], 1);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "type"	"xfer,store")
   (set_attr "mode"	"SF")])

;; Insn to initialize $gp for n32/n64 abicalls.  Operand 0 is the offset
;; of _gp from the start of this function.  Operand 1 is the incoming
;; function address.
(define_insn_and_split "loadgp"
  [(unspec_volatile [(match_operand 0 "" "")
		     (match_operand 1 "register_operand" "")] UNSPEC_LOADGP)]
  "TARGET_ABICALLS && TARGET_NEWABI"
  "#"
  ""
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 2) (match_dup 4))
   (set (match_dup 2) (match_dup 5))]
{
  operands[2] = pic_offset_table_rtx;
  operands[3] = gen_rtx_HIGH (Pmode, operands[0]);
  operands[4] = gen_rtx_PLUS (Pmode, operands[2], operands[1]);
  operands[5] = gen_rtx_LO_SUM (Pmode, operands[2], operands[0]);
}
  [(set_attr "length" "12")])

;; The use of gp is hidden when not using explicit relocations.
;; This blockage instruction prevents the gp load from being
;; scheduled after an implicit use of gp.  It also prevents
;; the load from being deleted as dead.
(define_insn "loadgp_blockage"
  [(unspec_volatile [(reg:DI 28)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

;; Emit a .cprestore directive, which normally expands to a single store
;; instruction.  Note that we continue to use .cprestore for explicit reloc
;; code so that jals inside inline asms will work correctly.
(define_insn "cprestore"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "I,i")]
		    UNSPEC_CPRESTORE)]
  ""
{
  if (set_nomacro && which_alternative == 1)
    return ".set\tmacro\;.cprestore\t%0\;.set\tnomacro";
  else
    return ".cprestore\t%0";
}
  [(set_attr "type" "store")
   (set_attr "length" "4,12")])

;; Block moves, see mips.c for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movstrsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand" "")
		   (match_operand:BLK 1 "general_operand" ""))
	      (use (match_operand:SI 2 "" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))])]
  "!TARGET_MIPS16 && !TARGET_MEMCPY"
{
  if (mips_expand_block_move (operands[0], operands[1], operands[2]))
    DONE;
  else
    FAIL;
})

;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................

;; Many of these instructions use trivial define_expands, because we
;; want to use a different set of constraints when TARGET_MIPS16.

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  ""
{
  /* On the mips16, a shift of more than 8 is a four byte instruction,
     so, for a shift between 8 and 16, it is just as fast to do two
     shifts of 8 or less.  If there is a lot of shifting going on, we
     may win in CSE.  Otherwise combine will put the shifts back
     together again.  This can be called by function_arg, so we must
     be careful not to allocate a new register if we've reached the
     reload pass.  */
  if (TARGET_MIPS16
      && optimize
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 8
      && INTVAL (operands[2]) <= 16
      && ! reload_in_progress
      && ! reload_completed)
    {
      rtx temp = gen_reg_rtx (SImode);

      emit_insn (gen_ashlsi3_internal2 (temp, operands[1], GEN_INT (8)));
      emit_insn (gen_ashlsi3_internal2 (operands[0], temp,
					GEN_INT (INTVAL (operands[2]) - 8)));
      DONE;
    }
})

(define_insn "ashlsi3_internal1"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "ashlsi3_internal1_extend"
  [(set (match_operand:DI 0 "register_operand" "=d")
       (sign_extend:DI (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (match_operand:SI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
  [(set_attr "type"    "arith")
   (set_attr "mode"    "DI")])


(define_insn "ashlsi3_internal2"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,d")
		   (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "sll\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 4)
			       (const_int 8))])])

;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashift:SI (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

(define_expand "ashldi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashift:DI (match_operand:DI 1 "register_operand" "")
			      (match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
{
  if (TARGET_64BIT)
    {
      /* On the mips16, a shift of more than 8 is a four byte
	 instruction, so, for a shift between 8 and 16, it is just as
	 fast to do two shifts of 8 or less.  If there is a lot of
	 shifting going on, we may win in CSE.  Otherwise combine will
	 put the shifts back together again.  This can be called by
	 function_arg, so we must be careful not to allocate a new
	 register if we've reached the reload pass.  */
      if (TARGET_MIPS16
	  && optimize
	  && GET_CODE (operands[2]) == CONST_INT
	  && INTVAL (operands[2]) > 8
	  && INTVAL (operands[2]) <= 16
	  && ! reload_in_progress
	  && ! reload_completed)
	{
	  rtx temp = gen_reg_rtx (DImode);

	  emit_insn (gen_ashldi3_internal4 (temp, operands[1], GEN_INT (8)));
	  emit_insn (gen_ashldi3_internal4 (operands[0], temp,
					    GEN_INT (INTVAL (operands[2]) - 8)));
	  DONE;
	}

      emit_insn (gen_ashldi3_internal4 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
})


(define_insn "ashldi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "sll\t%3,%2,26\;\
bgez\t%3,1f%#\;\
sll\t%M0,%L1,%2\;\
%(b\t3f\;\
move\t%L0,%.%)\
\n\n\
%~1:\;\
%(beq\t%3,%.,2f\;\
sll\t%M0,%M1,%2%)\
\n\;\
subu\t%3,%.,%2\;\
srl\t%3,%L1,%3\;\
or\t%M0,%M0,%3\n\
%~2:\;\
sll\t%L0,%L1,%2\n\
%~3:"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"48")])


(define_insn "ashldi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 32) != 0"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sll\t%M0,%L1,%2\;move\t%L0,%.";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 4) (ashift:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 0) (const_int 0))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (ashift:SI (subreg:SI (match_dup 1) 4) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 4) (const_int 0))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_insn "ashldi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return "sll\t%M0,%M1,%2\;srl\t%3,%L1,%4\;or\t%M0,%M0,%3\;sll\t%L0,%L1,%2";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"

  [(set (subreg:SI (match_dup 0) 4)
	(ashift:SI (subreg:SI (match_dup 1) 4)
		   (match_dup 2)))

   (set (match_dup 3)
	(lshiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 4)))

   (set (subreg:SI (match_dup 0) 4)
	(ior:SI (subreg:SI (match_dup 0) 4)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 0)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 2)))]
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
})


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"

  [(set (subreg:SI (match_dup 0) 0)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 2)))

   (set (match_dup 3)
	(lshiftrt:SI (subreg:SI (match_dup 1) 4)
		     (match_dup 4)))

   (set (subreg:SI (match_dup 0) 0)
	(ior:SI (subreg:SI (match_dup 0) 0)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 4)
	(ashift:SI (subreg:SI (match_dup 1) 4)
		   (match_dup 2)))]
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
})


(define_insn "ashldi3_internal4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsll\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ashift:DI (match_operand:DI 1 "register_operand" "0,d")
		   (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "dsll\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsll\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 4)
			       (const_int 8))])])


;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && !TARGET_DEBUG_D_MODE
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashift:DI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashift:DI (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  ""
{
  /* On the mips16, a shift of more than 8 is a four byte instruction,
     so, for a shift between 8 and 16, it is just as fast to do two
     shifts of 8 or less.  If there is a lot of shifting going on, we
     may win in CSE.  Otherwise combine will put the shifts back
     together again.  */
  if (TARGET_MIPS16
      && optimize
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 8
      && INTVAL (operands[2]) <= 16)
    {
      rtx temp = gen_reg_rtx (SImode);

      emit_insn (gen_ashrsi3_internal2 (temp, operands[1], GEN_INT (8)));
      emit_insn (gen_ashrsi3_internal2 (operands[0], temp,
					GEN_INT (INTVAL (operands[2]) - 8)));
      DONE;
    }
})

(define_insn "ashrsi3_internal1"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sra\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "ashrsi3_internal2"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,d")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "sra\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sra\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 4)
			       (const_int 8))])])


;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashiftrt:SI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

(define_expand "ashrdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashiftrt:DI (match_operand:DI 1 "register_operand" "")
				(match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
{
  if (TARGET_64BIT)
    {
      /* On the mips16, a shift of more than 8 is a four byte
	 instruction, so, for a shift between 8 and 16, it is just as
	 fast to do two shifts of 8 or less.  If there is a lot of
	 shifting going on, we may win in CSE.  Otherwise combine will
	 put the shifts back together again.  */
      if (TARGET_MIPS16
	  && optimize
	  && GET_CODE (operands[2]) == CONST_INT
	  && INTVAL (operands[2]) > 8
	  && INTVAL (operands[2]) <= 16)
	{
	  rtx temp = gen_reg_rtx (DImode);

	  emit_insn (gen_ashrdi3_internal4 (temp, operands[1], GEN_INT (8)));
	  emit_insn (gen_ashrdi3_internal4 (operands[0], temp,
					    GEN_INT (INTVAL (operands[2]) - 8)));
	  DONE;
	}

      emit_insn (gen_ashrdi3_internal4 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
})


(define_insn "ashrdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "sll\t%3,%2,26\;\
bgez\t%3,1f%#\;\
sra\t%L0,%M1,%2\;\
%(b\t3f\;\
sra\t%M0,%M1,31%)\
\n\n\
%~1:\;\
%(beq\t%3,%.,2f\;\
srl\t%L0,%L1,%2%)\
\n\;\
subu\t%3,%.,%2\;\
sll\t%3,%M1,%3\;\
or\t%L0,%L0,%3\n\
%~2:\;\
sra\t%M0,%M1,%2\n\
%~3:"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"48")])


(define_insn "ashrdi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sra\t%L0,%M1,%2\;sra\t%M0,%M1,31";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (ashiftrt:SI (subreg:SI (match_dup 1) 4) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 4) (ashiftrt:SI (subreg:SI (match_dup 1) 4) (const_int 31)))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 4) (ashiftrt:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 0) (ashiftrt:SI (subreg:SI (match_dup 1) 0) (const_int 31)))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_insn "ashrdi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return "srl\t%L0,%L1,%2\;sll\t%3,%M1,%4\;or\t%L0,%L0,%3\;sra\t%M0,%M1,%2";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"

  [(set (subreg:SI (match_dup 0) 0)
	(lshiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 2)))

   (set (match_dup 3)
	(ashift:SI (subreg:SI (match_dup 1) 4)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 0)
	(ior:SI (subreg:SI (match_dup 0) 0)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 4)
	(ashiftrt:SI (subreg:SI (match_dup 1) 4)
		     (match_dup 2)))]
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
})


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"

  [(set (subreg:SI (match_dup 0) 4)
	(lshiftrt:SI (subreg:SI (match_dup 1) 4)
		     (match_dup 2)))

   (set (match_dup 3)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 4)
	(ior:SI (subreg:SI (match_dup 0) 4)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 0)
	(ashiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 2)))]
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
})


(define_insn "ashrdi3_internal4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 4)
			       (const_int 8))])])

;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && !TARGET_DEBUG_D_MODE
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashiftrt:DI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashiftrt:DI (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  ""
{
  /* On the mips16, a shift of more than 8 is a four byte instruction,
     so, for a shift between 8 and 16, it is just as fast to do two
     shifts of 8 or less.  If there is a lot of shifting going on, we
     may win in CSE.  Otherwise combine will put the shifts back
     together again.  */
  if (TARGET_MIPS16
      && optimize
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 8
      && INTVAL (operands[2]) <= 16)
    {
      rtx temp = gen_reg_rtx (SImode);

      emit_insn (gen_lshrsi3_internal2 (temp, operands[1], GEN_INT (8)));
      emit_insn (gen_lshrsi3_internal2 (operands[0], temp,
					GEN_INT (INTVAL (operands[2]) - 8)));
      DONE;
    }
})

(define_insn "lshrsi3_internal1"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srl\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "lshrsi3_internal2"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,d")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
{
  if (which_alternative == 0)
    return "srl\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srl\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 4)
			       (const_int 8))])])


;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (lshiftrt:SI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

;; If we load a byte on the mips16 as a bitfield, the resulting
;; sequence of instructions is too complicated for combine, because it
;; involves four instructions: a load, a shift, a constant load into a
;; register, and an and (the key problem here is that the mips16 does
;; not have and immediate).  We recognize a shift of a load in order
;; to make it simple enough for combine to understand.
;;
;; The length here is the worst case: the length of the split version
;; will be more accurate. 
(define_insn_and_split ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (match_operand:SI 2 "immediate_operand" "I")))]
  "TARGET_MIPS16"
  "#"
  ""
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 2)))]
  ""
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"16")])

(define_expand "lshrdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (lshiftrt:DI (match_operand:DI 1 "register_operand" "")
				(match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
{
  if (TARGET_64BIT)
    {
      /* On the mips16, a shift of more than 8 is a four byte
	 instruction, so, for a shift between 8 and 16, it is just as
	 fast to do two shifts of 8 or less.  If there is a lot of
	 shifting going on, we may win in CSE.  Otherwise combine will
	 put the shifts back together again.  */
      if (TARGET_MIPS16
	  && optimize
	  && GET_CODE (operands[2]) == CONST_INT
	  && INTVAL (operands[2]) > 8
	  && INTVAL (operands[2]) <= 16)
	{
	  rtx temp = gen_reg_rtx (DImode);

	  emit_insn (gen_lshrdi3_internal4 (temp, operands[1], GEN_INT (8)));
	  emit_insn (gen_lshrdi3_internal4 (operands[0], temp,
					    GEN_INT (INTVAL (operands[2]) - 8)));
	  DONE;
	}

      emit_insn (gen_lshrdi3_internal4 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
})


(define_insn "lshrdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "sll\t%3,%2,26\;\
bgez\t%3,1f%#\;\
srl\t%L0,%M1,%2\;\
%(b\t3f\;\
move\t%M0,%.%)\
\n\n\
%~1:\;\
%(beq\t%3,%.,2f\;\
srl\t%L0,%L1,%2%)\
\n\;\
subu\t%3,%.,%2\;\
sll\t%3,%M1,%3\;\
or\t%L0,%L0,%3\n\
%~2:\;\
srl\t%M0,%M1,%2\n\
%~3:"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"48")])


(define_insn "lshrdi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 32) != 0"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "srl\t%L0,%M1,%2\;move\t%M0,%.";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (lshiftrt:SI (subreg:SI (match_dup 1) 4) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 4) (const_int 0))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 4) (lshiftrt:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 0) (const_int 0))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_insn "lshrdi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return "srl\t%L0,%L1,%2\;sll\t%3,%M1,%4\;or\t%L0,%L0,%3\;srl\t%M0,%M1,%2";
}
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"

  [(set (subreg:SI (match_dup 0) 0)
	(lshiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 2)))

   (set (match_dup 3)
	(ashift:SI (subreg:SI (match_dup 1) 4)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 0)
	(ior:SI (subreg:SI (match_dup 0) 0)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 4)
	(lshiftrt:SI (subreg:SI (match_dup 1) 4)
		     (match_dup 2)))]
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
})


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"

  [(set (subreg:SI (match_dup 0) 4)
	(lshiftrt:SI (subreg:SI (match_dup 1) 4)
		     (match_dup 2)))

   (set (match_dup 3)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 4)
	(ior:SI (subreg:SI (match_dup 0) 4)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 0)
	(lshiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 2)))]
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
})


(define_insn "lshrdi3_internal4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "0,0")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "rotrsi3"
  [(set (match_operand:SI              0 "register_operand" "=d")
        (rotatert:SI (match_operand:SI 1 "register_operand" "d")
                     (match_operand:SI 2 "arith_operand"    "dn")))]
  "ISA_HAS_ROTR_SI"
{
  if (TARGET_SR71K && GET_CODE (operands[2]) != CONST_INT)
    return "rorv\t%0,%1,%2";

  if ((GET_CODE (operands[2]) == CONST_INT)
      && (INTVAL (operands[2]) < 0 || INTVAL (operands[2]) >= 32))
    abort ();

  return "ror\t%0,%1,%2";
}
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])

(define_insn "rotrdi3"
  [(set (match_operand:DI              0 "register_operand" "=d")
        (rotatert:DI (match_operand:DI 1 "register_operand" "d")
                     (match_operand:DI 2 "arith_operand"    "dn")))]
  "ISA_HAS_ROTR_DI"
{
  if (TARGET_SR71K)
    {
      if (GET_CODE (operands[2]) != CONST_INT)
	return "drorv\t%0,%1,%2";

      if (INTVAL (operands[2]) >= 32 && INTVAL (operands[2]) <= 63)
	return "dror32\t%0,%1,%2";
    }

  if ((GET_CODE (operands[2]) == CONST_INT)
      && (INTVAL (operands[2]) < 0 || INTVAL (operands[2]) >= 64))
    abort ();

  return "dror\t%0,%1,%2";
}
  [(set_attr "type"     "arith")
   (set_attr "mode"     "DI")])


;; On the mips16, we can split a 4 byte shift into 2 2 byte shifts.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed && !TARGET_DEBUG_D_MODE
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (lshiftrt:DI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (lshiftrt:DI (match_dup 0) (match_dup 2)))]
  { operands[2] = GEN_INT (INTVAL (operands[2]) - 8); })

;;
;;  ....................
;;
;;	COMPARISONS
;;
;;  ....................

;; Flow here is rather complex:
;;
;;  1)	The cmp{si,di,sf,df} routine is called.  It deposits the
;;	arguments into the branch_cmp array, and the type into
;;	branch_type.  No RTL is generated.
;;
;;  2)	The appropriate branch define_expand is called, which then
;;	creates the appropriate RTL for the comparison and branch.
;;	Different CC modes are used, based on what type of branch is
;;	done, so that we can constrain things appropriately.  There
;;	are assumptions in the rest of GCC that break if we fold the
;;	operands into the branches for integer operations, and use cc0
;;	for floating point, so we use the fp status register instead.
;;	If needed, an appropriate temporary is created to hold the
;;	of the integer compare.

(define_expand "cmpsi"
  [(set (cc0)
	(compare:CC (match_operand:SI 0 "register_operand" "")
		    (match_operand:SI 1 "arith_operand" "")))]
  ""
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = operands[1];
  branch_type = CMP_SI;
  DONE;
})

(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" ""))]
  ""
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = const0_rtx;
  branch_type = CMP_SI;
  DONE;
})

(define_expand "cmpdi"
  [(set (cc0)
	(compare:CC (match_operand:DI 0 "register_operand" "")
		    (match_operand:DI 1 "arith_operand" "")))]
  "TARGET_64BIT"
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = operands[1];
  branch_type = CMP_DI;
  DONE;
})

(define_expand "tstdi"
  [(set (cc0)
	(match_operand:DI 0 "register_operand" ""))]
  "TARGET_64BIT"
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = const0_rtx;
  branch_type = CMP_DI;
  DONE;
})

(define_expand "cmpdf"
  [(set (cc0)
	(compare:CC (match_operand:DF 0 "register_operand" "")
		    (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = operands[1];
  branch_type = CMP_DF;
  DONE;
})

(define_expand "cmpsf"
  [(set (cc0)
	(compare:CC (match_operand:SF 0 "register_operand" "")
		    (match_operand:SF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT"
{
  branch_cmp[0] = operands[0];
  branch_cmp[1] = operands[1];
  branch_type = CMP_SF;
  DONE;
})

;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches on floating-point equality tests.

(define_insn "branch_fp"
  [(set (pc)
        (if_then_else
         (match_operator:CC 0 "cmp_op"
                            [(match_operand:CC 2 "register_operand" "z")
			     (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "TARGET_HARD_FLOAT"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/1,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_fp_inverted"
  [(set (pc)
        (if_then_else
         (match_operator:CC 0 "cmp_op"
                            [(match_operand:CC 2 "register_operand" "z")
			     (const_int 0)])
         (pc)
         (label_ref (match_operand 1 "" ""))))]
  "TARGET_HARD_FLOAT"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/1,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

;; Conditional branches on comparisons with zero.

(define_insn "branch_zero"
  [(set (pc)
	(if_then_else
         (match_operator:SI 0 "cmp_op"
			    [(match_operand:SI 2 "register_operand" "d")
			     (const_int 0)])
        (label_ref (match_operand 1 "" ""))
        (pc)))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_zero_inverted"
  [(set (pc)
	(if_then_else
         (match_operator:SI 0 "cmp_op"
		            [(match_operand:SI 2 "register_operand" "d")
			     (const_int 0)])
        (pc)
        (label_ref (match_operand 1 "" ""))))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_zero_di"
  [(set (pc)
	(if_then_else
         (match_operator:DI 0 "cmp_op"
		            [(match_operand:DI 2 "register_operand" "d")
			     (const_int 0)])
        (label_ref (match_operand 1 "" ""))
        (pc)))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_zero_di_inverted"
  [(set (pc)
	(if_then_else
         (match_operator:DI 0 "cmp_op"
			    [(match_operand:DI 2 "register_operand" "d")
			     (const_int 0)])
        (pc)
        (label_ref (match_operand 1 "" ""))))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

;; Conditional branch on equality comparison.

(define_insn "branch_equality"
  [(set (pc)
	(if_then_else
         (match_operator:SI 0 "equality_op"
		   	    [(match_operand:SI 2 "register_operand" "d")
			     (match_operand:SI 3 "register_operand" "d")])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_equality_di"
  [(set (pc)
	(if_then_else
         (match_operator:DI 0 "equality_op"
			    [(match_operand:DI 2 "register_operand" "d")
			     (match_operand:DI 3 "register_operand" "d")])
        (label_ref (match_operand 1 "" ""))
        (pc)))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_equality_inverted"
  [(set (pc)
	(if_then_else
         (match_operator:SI 0 "equality_op"
		   	    [(match_operand:SI 2 "register_operand" "d")
			     (match_operand:SI 3 "register_operand" "d")])
         (pc)
         (label_ref (match_operand 1 "" ""))))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_equality_di_inverted"
  [(set (pc)
	(if_then_else
         (match_operator:DI 0 "equality_op"
			    [(match_operand:DI 2 "register_operand" "d")
			     (match_operand:DI 3 "register_operand" "d")])
        (pc)
        (label_ref (match_operand 1 "" ""))))]
  "!TARGET_MIPS16"
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

;; MIPS16 branches

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator:SI 0 "equality_op"
					 [(match_operand:SI 1 "register_operand" "d,t")
					  (const_int 0)])
	(match_operand 2 "pc_or_label_operand" "")
	(match_operand 3 "pc_or_label_operand" "")))]
  "TARGET_MIPS16"
{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "b%C0z\t%1,%2";
      else
	return "bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "b%N0z\t%1,%3";
      else
	return "bt%N0z\t%3";
    }
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator:DI 0 "equality_op"
					 [(match_operand:DI 1 "register_operand" "d,t")
					  (const_int 0)])
	(match_operand 2 "pc_or_label_operand" "")
	(match_operand 3 "pc_or_label_operand" "")))]
  "TARGET_MIPS16"
{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "b%C0z\t%1,%2";
      else
	return "bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "b%N0z\t%1,%3";
      else
	return "bt%N0z\t%3";
    }
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_expand "bunordered"
  [(set (pc)
	(if_then_else (unordered:CC (cc0)
				    (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, UNORDERED);
  DONE;
})

(define_expand "bordered"
  [(set (pc)
	(if_then_else (ordered:CC (cc0)
				  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, ORDERED);
  DONE;
})

(define_expand "bunlt"
  [(set (pc)
	(if_then_else (unlt:CC (cc0)
			       (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, UNLT);
  DONE;
})

(define_expand "bunge"
  [(set (pc)
	(if_then_else (unge:CC (cc0)
			       (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, UNGE);
  DONE;
})

(define_expand "buneq"
  [(set (pc)
	(if_then_else (uneq:CC (cc0)
			       (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, UNEQ);
  DONE;
})

(define_expand "bltgt"
  [(set (pc)
	(if_then_else (ltgt:CC (cc0)
			       (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, LTGT);
  DONE;
})

(define_expand "bunle"
  [(set (pc)
	(if_then_else (unle:CC (cc0)
			       (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, UNLE);
  DONE;
})

(define_expand "bungt"
  [(set (pc)
	(if_then_else (ungt:CC (cc0)
			       (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, UNGT);
  DONE;
})

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, EQ);
  DONE;
})

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, NE);
  DONE;
})

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, GT);
  DONE;
})

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, GE);
  DONE;
})

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, LT);
  DONE;
})

(define_expand "ble"
  [(set (pc)
	(if_then_else (le:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, LE);
  DONE;
})

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, GTU);
  DONE;
})

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, GEU);
  DONE;
})

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, LTU);
  DONE;
})

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  gen_conditional_branch (operands, LEU);
  DONE;
})

;;
;;  ....................
;;
;;	SETTING A REGISTER FROM A COMPARISON
;;
;;  ....................

(define_expand "seq"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(eq:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (EQ, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* Fall through and generate default code.  */
})


(define_insn "seq_si_zero"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(eq:SI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  "!TARGET_MIPS16"
  "sltu\t%0,%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(eq:SI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  "TARGET_MIPS16"
  "sltu\t%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "seq_di_zero"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(eq:DI (match_operand:DI 1 "register_operand" "d")
	       (const_int 0)))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\t%0,%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(eq:DI (match_operand:DI 1 "register_operand" "d")
	       (const_int 0)))]
  "TARGET_64BIT && TARGET_MIPS16"
  "sltu\t%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "seq_si"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(eq:SI (match_operand:SI 1 "register_operand" "%d,d")
	       (match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
   xor\t%0,%1,%2\;sltu\t%0,%0,1
   xori\t%0,%1,%2\;sltu\t%0,%0,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "uns_arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)"
  [(set (match_dup 0)
	(xor:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(ltu:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_insn "seq_di"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(eq:DI (match_operand:DI 1 "register_operand" "%d,d")
	       (match_operand:DI 2 "uns_arith_operand" "d,K")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
   xor\t%0,%1,%2\;sltu\t%0,%0,1
   xori\t%0,%1,%2\;sltu\t%0,%0,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(eq:DI (match_operand:DI 1 "register_operand" "")
	       (match_operand:DI 2 "uns_arith_operand" "")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)"
  [(set (match_dup 0)
	(xor:DI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(ltu:DI (match_dup 0)
		(const_int 1)))]
  "")

;; On the mips16 the default code is better than using sltu.

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ne:SI (match_dup 1)
	       (match_dup 2)))]
  "!TARGET_MIPS16"
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE)
    {
      gen_int_relational (NE, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* Fall through and generate default code.  */
})

(define_insn "sne_si_zero"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ne:SI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  "!TARGET_MIPS16"
  "sltu\t%0,%.,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "sne_di_zero"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ne:DI (match_operand:DI 1 "register_operand" "d")
	       (const_int 0)))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\t%0,%.,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "sne_si"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ne:SI (match_operand:SI 1 "register_operand" "%d,d")
	       (match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
    xor\t%0,%1,%2\;sltu\t%0,%.,%0
    xori\t%0,%1,%x2\;sltu\t%0,%.,%0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "uns_arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)"
  [(set (match_dup 0)
	(xor:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(gtu:SI (match_dup 0)
		(const_int 0)))]
  "")

(define_insn "sne_di"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ne:DI (match_operand:DI 1 "register_operand" "%d,d")
	       (match_operand:DI 2 "uns_arith_operand" "d,K")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
    xor\t%0,%1,%2\;sltu\t%0,%.,%0
    xori\t%0,%1,%x2\;sltu\t%0,%.,%0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ne:DI (match_operand:DI 1 "register_operand" "")
	       (match_operand:DI 2 "uns_arith_operand" "")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)"
  [(set (match_dup 0)
	(xor:DI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(gtu:DI (match_dup 0)
		(const_int 0)))]
  "")

(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GT, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* Fall through and generate default code.  */
})

(define_insn "sgt_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gt:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "reg_or_0_operand" "dJ")))]
  "!TARGET_MIPS16"
  "slt\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(gt:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "slt\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "sgt_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(gt:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "reg_or_0_operand" "dJ")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "slt\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(gt:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "slt\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ge:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GE, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* Fall through and generate default code.  */
})

(define_insn "sge_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ge:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\t%0,%1,%2\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ge:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16"
  [(set (match_dup 0)
	(lt:SI (match_dup 1)
	       (match_dup 2)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_insn "sge_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ge:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\t%0,%1,%2\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ge:DI (match_operand:DI 1 "register_operand" "")
	       (match_operand:DI 2 "arith_operand" "")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16"
  [(set (match_dup 0)
	(lt:DI (match_dup 1)
	       (match_dup 2)))
   (set (match_dup 0)
	(xor:DI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LT, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* Fall through and generate default code.  */
})

(define_insn "slt_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lt:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "slt\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t,t")
	(lt:SI (match_operand:SI 1 "register_operand" "d,d")
	       (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "slt\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "slt_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lt:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "slt\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t,t")
	(lt:DI (match_operand:DI 1 "register_operand" "d,d")
	       (match_operand:DI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "slt\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))])])

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(le:SI (match_dup 1)
	       (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LE, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 32767)
    operands[2] = force_reg (SImode, operands[2]);

  /* Fall through and generate default code.  */
})

(define_insn "sle_si_const"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(le:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "small_int" "I")))]
  "!TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(le:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "small_int" "I")))]
  "TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length") (if_then_else (match_operand:VOID 2 "m16_uimm8_m1_1" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn "sle_di_const"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(le:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(le:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length") (if_then_else (match_operand:VOID 2 "m16_uimm8_m1_1" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn "sle_si_reg"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(le:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "register_operand" "d")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\t%0,%z2,%1\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(le:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "register_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16"
  [(set (match_dup 0)
	(lt:SI (match_dup 2)
	       (match_dup 1)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_insn "sle_di_reg"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(le:DI (match_operand:DI 1 "register_operand" "d")
	       (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\t%0,%z2,%1\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(le:DI (match_operand:DI 1 "register_operand" "")
	       (match_operand:DI 2 "register_operand" "")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16"
  [(set (match_dup 0)
	(lt:DI (match_dup 2)
	       (match_dup 1)))
   (set (match_dup 0)
	(xor:DI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gtu:SI (match_dup 1)
		(match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GTU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* Fall through and generate default code.  */
})

(define_insn "sgtu_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gtu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "reg_or_0_operand" "dJ")))]
  "!TARGET_MIPS16"
  "sltu\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(gtu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "sltu\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "sgtu_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(gtu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "reg_or_0_operand" "dJ")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(gtu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "sltu\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (geu:SI (match_dup 1)
                (match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GEU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* Fall through and generate default code.  */
})

(define_insn "sgeu_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(geu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\t%0,%1,%2\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(geu:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16"
  [(set (match_dup 0)
	(ltu:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_insn "sgeu_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(geu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\t%0,%1,%2\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(geu:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "arith_operand" "")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16"
  [(set (match_dup 0)
	(ltu:DI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(xor:DI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ltu:SI (match_dup 1)
		(match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LTU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* Fall through and generate default code.  */
})

(define_insn "sltu_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ltu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "sltu\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t,t")
	(ltu:SI (match_operand:SI 1 "register_operand" "d,d")
		(match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "sltu\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "sltu_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ltu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t,t")
	(ltu:DI (match_operand:DI 1 "register_operand" "d,d")
		(match_operand:DI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "sltu\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))])])

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(leu:SI (match_dup 1)
		(match_dup 2)))]
  ""
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* Set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LEU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 32767)
    operands[2] = force_reg (SImode, operands[2]);

  /* Fall through and generate default code.  */
})

(define_insn "sleu_si_const"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(leu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "small_int" "I")))]
  "!TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "sltu\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(leu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "small_int" "I")))]
  "TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length") (if_then_else (match_operand:VOID 2 "m16_uimm8_m1_1" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn "sleu_di_const"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(leu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "sltu\t%0,%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(leu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%1,%2";
}
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length") (if_then_else (match_operand:VOID 2 "m16_uimm8_m1_1" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn "sleu_si_reg"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(leu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\t%0,%z2,%1\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(leu:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16"
  [(set (match_dup 0)
	(ltu:SI (match_dup 2)
		(match_dup 1)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_insn "sleu_di_reg"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(leu:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\t%0,%z2,%1\;xori\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(leu:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16"
  [(set (match_dup 0)
	(ltu:DI (match_dup 2)
		(match_dup 1)))
   (set (match_dup 0)
	(xor:DI (match_dup 0)
		(const_int 1)))]
  "")

;;
;;  ....................
;;
;;	FLOATING POINT COMPARISONS
;;
;;  ....................

(define_insn "sunordered_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unordered:CC (match_operand:DF 1 "register_operand" "f")
		      (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.un.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunlt_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unlt:CC (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.ult.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "suneq_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(uneq:CC (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.ueq.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunle_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unle:CC (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.ule.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sungt_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(ungt:CC (match_operand:DF 1 "register_operand" "f")
	         (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.ult.d\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunge_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unge:CC (match_operand:DF 1 "register_operand" "f")
	         (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.ule.d\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "seq_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(eq:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.eq.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "slt_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(lt:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.lt.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sle_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(le:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.le.d\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sgt_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(gt:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.lt.d\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sge_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(ge:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "c.le.d\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunordered_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unordered:CC (match_operand:SF 1 "register_operand" "f")
		      (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.un.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunlt_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unlt:CC (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.ult.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "suneq_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(uneq:CC (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.ueq.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunle_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unle:CC (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.ule.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sungt_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(ungt:CC (match_operand:SF 1 "register_operand" "f")
	         (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.ult.s\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sunge_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(unge:CC (match_operand:SF 1 "register_operand" "f")
	         (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.ule.s\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "seq_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(eq:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.eq.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "slt_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(lt:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.lt.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sle_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(le:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.le.s\t%Z0%1,%2"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sgt_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(gt:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.lt.s\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

(define_insn "sge_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(ge:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "c.le.s\t%Z0%2,%1"
  [(set_attr "type" "fcmp")
   (set_attr "mode" "FPSW")])

;;
;;  ....................
;;
;;	UNCONDITIONAL BRANCHES
;;
;;  ....................

;; Unconditional branches.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "!TARGET_MIPS16"
{
  if (flag_pic && ! TARGET_EMBEDDED_PIC)
    {
      if (get_attr_length (insn) <= 8)
	return "%*b\t%l0%/";
      else
	{
	  output_asm_insn (mips_output_load_label (), operands);
	  return "%*jr\t%@%/%]";
	}
    }
  else
    return "%*j\t%l0%/";
}
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set (attr "length")
	;; we can't use `j' when emitting non-embedded PIC, so we emit
	;; branch, if it's in range, or load the address of the branch
	;; target into $at in a PIC-compatible way and then jump to it.
	(if_then_else
	 (ior (eq (symbol_ref "flag_pic && ! TARGET_EMBEDDED_PIC")
		  (const_int 0))
	      (lt (abs (minus (match_dup 0)
			      (plus (pc) (const_int 4))))
		  (const_int 131072)))
	 (const_int 4) (const_int 16)))])

;; We need a different insn for the mips16, because a mips16 branch
;; does not have a delay slot.

(define_insn ""
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_MIPS16"
  "b\t%l0"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand" "d"))]
  ""
{
  rtx dest;

  dest = operands[0];
  if (GET_CODE (dest) != REG || GET_MODE (dest) != Pmode)
    operands[0] = copy_to_mode_reg (Pmode, dest);

  if (!(Pmode == DImode))
    emit_jump_insn (gen_indirect_jump_internal1 (operands[0]));
  else
    emit_jump_insn (gen_indirect_jump_internal2 (operands[0]));

  DONE;
})

(define_insn "indirect_jump_internal1"
  [(set (pc) (match_operand:SI 0 "register_operand" "d"))]
  "!(Pmode == DImode)"
  "%*j\t%0%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_insn "indirect_jump_internal2"
  [(set (pc) (match_operand:DI 0 "register_operand" "d"))]
  "Pmode == DImode"
  "%*j\t%0%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
{
  if (TARGET_MIPS16)
    {
      if (GET_MODE (operands[0]) != HImode)
	abort ();
      if (!(Pmode == DImode))
	emit_insn (gen_tablejump_mips161 (operands[0], operands[1]));
      else
	emit_insn (gen_tablejump_mips162 (operands[0], operands[1]));
      DONE;
    }

  if (GET_MODE (operands[0]) != ptr_mode)
    abort ();

  if (TARGET_GPWORD)
    operands[0] = expand_binop (ptr_mode, add_optab, operands[0],
				pic_offset_table_rtx, 0, 0, OPTAB_WIDEN);

  if (Pmode == SImode)
    emit_jump_insn (gen_tablejump_internal1 (operands[0], operands[1]));
  else
    emit_jump_insn (gen_tablejump_internal2 (operands[0], operands[1]));
  DONE;
})

(define_insn "tablejump_internal1"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "%*j\t%0%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_insn "tablejump_internal2"
  [(set (pc)
	(match_operand:DI 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_64BIT"
  "%*j\t%0%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "tablejump_mips161"
  [(set (pc) (plus:SI (sign_extend:SI
		       (match_operand:HI 0 "register_operand" "d"))
		      (label_ref:SI (match_operand 1 "" ""))))]
  "TARGET_MIPS16 && !(Pmode == DImode)"
{
  rtx t1, t2, t3;

  t1 = gen_reg_rtx (SImode);
  t2 = gen_reg_rtx (SImode);
  t3 = gen_reg_rtx (SImode);
  emit_insn (gen_extendhisi2 (t1, operands[0]));
  emit_move_insn (t2, gen_rtx_LABEL_REF (SImode, operands[1]));
  emit_insn (gen_addsi3 (t3, t1, t2));
  emit_jump_insn (gen_tablejump_internal1 (t3, operands[1]));
  DONE;
})

(define_expand "tablejump_mips162"
  [(set (pc) (plus:DI (sign_extend:DI
		       (match_operand:HI 0 "register_operand" "d"))
		      (label_ref:DI (match_operand 1 "" ""))))]
  "TARGET_MIPS16 && Pmode == DImode"
{
  rtx t1, t2, t3;

  t1 = gen_reg_rtx (DImode);
  t2 = gen_reg_rtx (DImode);
  t3 = gen_reg_rtx (DImode);
  emit_insn (gen_extendhidi2 (t1, operands[0]));
  emit_move_insn (t2, gen_rtx_LABEL_REF (DImode, operands[1]));
  emit_insn (gen_adddi3 (t3, t1, t2));
  emit_jump_insn (gen_tablejump_internal2 (t3, operands[1]));
  DONE;
})

;; Implement a switch statement when generating embedded PIC code.
;; Switches are implemented by `tablejump' when not using -membedded-pic.

(define_expand "casesi"
  [(set (match_dup 5)
	(minus:SI (match_operand:SI 0 "register_operand" "")
		  (match_operand:SI 1 "const_int_operand" "")))
   (set (cc0)
	(compare:CC (match_dup 5)
		    (match_operand:SI 2 "arith_operand" "")))
   (set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (parallel
    [(set (pc)
	  (mem:SI (plus:SI (mult:SI (match_dup 5)
				    (const_int 4))
			   (label_ref (match_operand 3 "" "")))))
     (clobber (match_scratch:SI 6 ""))
     (clobber (reg:SI 31))])]
  "TARGET_EMBEDDED_PIC"
{
  rtx index;

  /* If the index is too large, go to the default label.  */
  index = expand_binop (SImode, sub_optab, operands[0],
			operands[1], 0, 0, OPTAB_WIDEN);
  emit_insn (gen_cmpsi (index, operands[2]));
  emit_insn (gen_bgtu (operands[4]));

  /* Do the PIC jump.  */
  if (Pmode != DImode)
    emit_jump_insn (gen_casesi_internal (index, operands[3],
					 gen_reg_rtx (SImode)));
  else
    emit_jump_insn (gen_casesi_internal_di (index, operands[3],
					    gen_reg_rtx (DImode)));

  DONE;
})

;; An embedded PIC switch statement looks like this:
;;	bal	$LS1
;;	sll	$reg,$index,2
;; $LS1:
;;	addu	$reg,$reg,$31
;;	lw	$reg,$L1-$LS1($reg)
;;	addu	$reg,$reg,$31
;;	j	$reg
;; $L1:
;;	.word	case1-$LS1
;;	.word	case2-$LS1
;;	...

(define_insn "casesi_internal"
  [(set (pc)
	(mem:SI (plus:SI (mult:SI (match_operand:SI 0 "register_operand" "d")
				  (const_int 4))
			 (label_ref (match_operand 1 "" "")))))
   (clobber (match_operand:SI 2 "register_operand" "=d"))
   (clobber (reg:SI 31))]
  "TARGET_EMBEDDED_PIC"
  {
    if (set_nomacro)
      return "%(bal\\t%S1\;sll\\t%2,%0,2\\n%~%S1:\;addu\\t%2,%2,$31%)\;\\
.set macro\;lw\\t%2,%1-%S1(%2)\;.set nomacro\;addu\\t%2,%2,$31\\n\\t%*j\\t%2%/";
    return
  "%(bal\\t%S1\;sll\\t%2,%0,2\\n%~%S1:\;addu\\t%2,%2,$31%)\;\\
lw\\t%2,%1-%S1(%2)\;addu\\t%2,%2,$31\\n\\t%*j\\t%2%/"
    ;
  }
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"24")])

;; This code assumes that the table index will never be >= 29 bits wide,
;; which allows the 'sign extend' from SI to DI be a no-op.
(define_insn "casesi_internal_di"
  [(set (pc)
	(mem:DI (plus:DI (sign_extend:DI
			  (mult:SI (match_operand:SI 0 "register_operand" "d")
				  (const_int 8)))
			 (label_ref (match_operand 1 "" "")))))
   (clobber (match_operand:DI 2 "register_operand" "=d"))
   (clobber (reg:DI 31))]
  "TARGET_EMBEDDED_PIC"
  {
    if (set_nomacro)
      return "%(bal\\t%S1\;sll\\t%2,%0,3\\n%~%S1:\;daddu\\t%2,%2,$31%)\;\\
.set macro\;ld\\t%2,%1-%S1(%2)\;.set nomacro\;daddu\\t%2,%2,$31\\n\\t%*j\\t%2%/";
    return
  "%(bal\\t%S1\;sll\\t%2,%0,3\\n%~%S1:\;daddu\\t%2,%2,$31%)\;\\
ld\\t%2,%1-%S1(%2)\;daddu\\t%2,%2,$31\\n\\t%*j\\t%2%/"
    ;
  }
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"24")])

;; For TARGET_ABICALLS, we save the gp in the jmp_buf as well.
;; While it is possible to either pull it off the stack (in the
;; o32 case) or recalculate it given t9 and our target label,
;; it takes 3 or 4 insns to do so.

(define_expand "builtin_setjmp_setup"
  [(use (match_operand 0 "register_operand" ""))]
  "TARGET_ABICALLS"
{
  rtx addr;

  addr = plus_constant (operands[0], GET_MODE_SIZE (Pmode) * 3);
  emit_move_insn (gen_rtx_MEM (Pmode, addr), pic_offset_table_rtx);
  DONE;
})

;; Restore the gp that we saved above.  Despite the earlier comment, it seems
;; that older code did recalculate the gp from $25.  Continue to jump through
;; $25 for compatibility (we lose nothing by doing so).

(define_expand "builtin_longjmp"
  [(use (match_operand 0 "register_operand" "r"))]
  "TARGET_ABICALLS"
{
  /* The elements of the buffer are, in order:  */
  int W = GET_MODE_SIZE (Pmode);
  rtx fp = gen_rtx_MEM (Pmode, operands[0]);
  rtx lab = gen_rtx_MEM (Pmode, plus_constant (operands[0], 1*W));
  rtx stack = gen_rtx_MEM (Pmode, plus_constant (operands[0], 2*W));
  rtx gpv = gen_rtx_MEM (Pmode, plus_constant (operands[0], 3*W));
  rtx pv = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
  /* Use gen_raw_REG to avoid being given pic_offset_table_rtx.
     The target is bound to be using $28 as the global pointer
     but the current function might not be.  */
  rtx gp = gen_raw_REG (Pmode, GLOBAL_POINTER_REGNUM);

  /* This bit is similar to expand_builtin_longjmp except that it
     restores $gp as well.  */
  emit_move_insn (hard_frame_pointer_rtx, fp);
  emit_move_insn (pv, lab);
  emit_stack_restore (SAVE_NONLOCAL, stack, NULL_RTX);
  emit_move_insn (gp, gpv);
  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
  emit_insn (gen_rtx_USE (VOIDmode, gp));
  emit_indirect_jump (pv);
  DONE;
})

;;
;;  ....................
;;
;;	Function prologue/epilogue
;;
;;  ....................
;;

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  mips_expand_prologue ();
  DONE;
})

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  mips_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  mips_expand_epilogue (true);
  DONE;
})

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better.

(define_insn "return"
  [(return)]
  "mips_can_use_return_insn ()"
  "%*j\t$31%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "return_internal"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "%*j\t%0%/"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; When generating embedded PIC code we need to get the address of the
;; current function.  This specialized instruction does just that.

(define_insn "get_fnaddr"
  [(set (match_operand 0 "register_operand" "=d")
	(unspec [(match_operand 1 "" "")] UNSPEC_GET_FNADDR))
   (clobber (reg:SI 31))]
  "TARGET_EMBEDDED_PIC
   && GET_CODE (operands[1]) == SYMBOL_REF"
  "%($LF%= = . + 8\;bal\t$LF%=\;nop;la\t%0,%1-$LF%=%)\;addu\t%0,%0,$31"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"20")])

;; This is used in compiling the unwind routines.
(define_expand "eh_return"
  [(use (match_operand 0 "general_operand" ""))]
  ""
{
  enum machine_mode gpr_mode = TARGET_64BIT ? DImode : SImode;

  if (GET_MODE (operands[0]) != gpr_mode)
    operands[0] = convert_to_mode (gpr_mode, operands[0], 0);
  if (TARGET_64BIT)
    emit_insn (gen_eh_set_lr_di (operands[0]));
  else
    emit_insn (gen_eh_set_lr_si (operands[0]));

  DONE;
})

;; Clobber the return address on the stack.  We can't expand this
;; until we know where it will be put in the stack frame.

(define_insn "eh_set_lr_si"
  [(unspec [(match_operand:SI 0 "register_operand" "d")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&d"))]
  "! TARGET_64BIT"
  "#")

(define_insn "eh_set_lr_di"
  [(unspec [(match_operand:DI 0 "register_operand" "d")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:DI 1 "=&d"))]
  "TARGET_64BIT"
  "#")

(define_split
  [(unspec [(match_operand 0 "register_operand" "")] UNSPEC_EH_RETURN)
   (clobber (match_scratch 1 ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE"
  [(const_int 0)]
{
  mips_set_return_address (operands[0], operands[1]);
  DONE;
})

(define_insn_and_split "exception_receiver"
  [(set (reg:SI 28)
	(unspec_volatile:SI [(const_int 0)] UNSPEC_EH_RECEIVER))]
  "TARGET_ABICALLS && (mips_abi == ABI_32 || mips_abi == ABI_O64)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  mips_restore_gp ();
  DONE;
}
  [(set_attr "type"   "load")
   (set_attr "length" "12")])

;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

;; Instructions to load a call address from the GOT.  The address might
;; point to a function or to a lazy binding stub.  In the latter case,
;; the stub will use the dynamic linker to resolve the function, which
;; in turn will change the GOT entry to point to the function's real
;; address.
;;
;; This means that every call, even pure and constant ones, can
;; potentially modify the GOT entry.  And once a stub has been called,
;; we must not call it again.
;;
;; We represent this restriction using an imaginary fixed register that
;; acts like a GOT version number.  By making the register call-clobbered,
;; we tell the target-independent code that the address could be changed
;; by any call insn.
(define_insn "load_callsi"
  [(set (match_operand:SI 0 "register_operand" "=c")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "immediate_operand" "")
		    (reg:SI FAKE_CALL_REGNO)]
		   UNSPEC_LOAD_CALL))]
  "TARGET_ABICALLS"
  "lw\t%0,%R2(%1)"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn "load_calldi"
  [(set (match_operand:DI 0 "register_operand" "=c")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")
		    (match_operand:DI 2 "immediate_operand" "")
		    (reg:DI FAKE_CALL_REGNO)]
		   UNSPEC_LOAD_CALL))]
  "TARGET_ABICALLS"
  "ld\t%0,%R2(%1)"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

;; Sibling calls.  All these patterns use jump instructions.

;; If TARGET_SIBCALLS, call_insn_operand will only accept constant
;; addresses if a direct jump is acceptable.  Since the 'S' constraint
;; is defined in terms of call_insn_operand, the same is true of the
;; constraints.

;; When we use an indirect jump, we need a register that will be
;; preserved by the epilogue.  Since TARGET_ABICALLS forces us to
;; use $25 for this purpose -- and $25 is never clobbered by the
;; epilogue -- we might as well use it for !TARGET_ABICALLS as well.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))	;; next_arg_reg
	      (use (match_operand 3 "" ""))])]	;; struct_value_size_rtx
  "TARGET_SIBCALLS"
{
  mips_expand_call (0, XEXP (operands[0], 0), operands[1], operands[2], true);
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,S"))
	 (match_operand 1 "" ""))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  "@
    %*jr\t%0%/
    %*j\t%0%/"
  [(set_attr "type" "call")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))])]		;; next_arg_reg
  "TARGET_SIBCALLS"
{
  mips_expand_call (operands[0], XEXP (operands[1], 0),
		    operands[2], operands[3], true);
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "=df,df")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  "@
    %*jr\t%1%/
    %*j\t%1%/"
  [(set_attr "type" "call")])

(define_insn "sibcall_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "=df,df")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "=df,df")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))]
  "TARGET_SIBCALLS && SIBLING_CALL_P (insn)"
  "@
    %*jr\t%1%/
    %*j\t%1%/"
  [(set_attr "type" "call")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))	;; next_arg_reg
	      (use (match_operand 3 "" ""))])]	;; struct_value_size_rtx
  ""
{
  mips_expand_call (0, XEXP (operands[0], 0), operands[1], operands[2], false);
  DONE;
})

;; This instruction directly corresponds to an assembly-language "jal".
;; There are four cases:
;;
;;    - -mno-abicalls:
;;	  Both symbolic and register destinations are OK.  The pattern
;;	  always expands to a single mips instruction.
;;
;;    - -mabicalls/-mno-explicit-relocs:
;;	  Again, both symbolic and register destinations are OK.
;;	  The call is treated as a multi-instruction black box.
;;
;;    - -mabicalls/-mexplicit-relocs with n32 or n64:
;;	  Only "jal $25" is allowed.  This expands to a single "jalr $25"
;;	  instruction.
;;
;;    - -mabicalls/-mexplicit-relocs with o32 or o64:
;;	  Only "jal $25" is allowed.  The call is actually two instructions:
;;	  "jalr $25" followed by an insn to reload $gp.
;;
;; In the last case, we can generate the individual instructions with
;; a define_split.  There are several things to be wary of:
;;
;;   - We can't expose the load of $gp before reload.  If we did,
;;     it might get removed as dead, but reload can introduce new
;;     uses of $gp by rematerializing constants.
;;
;;   - We shouldn't restore $gp after calls that never return.
;;     It isn't valid to insert instructions between a noreturn
;;     call and the following barrier.
;;
;;   - The splitter deliberately changes the liveness of $gp.  The unsplit
;;     instruction preserves $gp and so have no effect on its liveness.
;;     But once we generate the separate insns, it becomes obvious that
;;     $gp is not live on entry to the call.
;;
;; ??? The operands[2] = insn check is a hack to make the original insn
;; available to the splitter.
(define_insn_and_split "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "c,S"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : "%*jal\t%0%/"; }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[2] = insn)"
  [(const_int 0)]
{
  emit_call_insn (gen_call_split (operands[0], operands[1]));
  if (!find_reg_note (operands[2], REG_NORETURN, 0))
    mips_restore_gp ();
  DONE;
}
  [(set_attr "jal" "indirect,direct")
   (set_attr "extended_mips16" "no,yes")])

(define_insn "call_split"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "c"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  "%*jalr\t%0%/"
  [(set_attr "type" "call")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))])]		;; next_arg_reg
  ""
{
  mips_expand_call (operands[0], XEXP (operands[1], 0),
		    operands[2], operands[3], false);
  DONE;
})

;; See comment for call_internal.
(define_insn_and_split "call_value_internal"
  [(set (match_operand 0 "register_operand" "=df,df")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : "%*jal\t%1%/"; }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[3] = insn)"
  [(const_int 0)]
{
  emit_call_insn (gen_call_value_split (operands[0], operands[1],
					operands[2]));
  if (!find_reg_note (operands[3], REG_NORETURN, 0))
    mips_restore_gp ();
  DONE;
}
  [(set_attr "jal" "indirect,direct")
   (set_attr "extended_mips16" "no,yes")])

(define_insn "call_value_split"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c"))
              (match_operand 2 "" "")))
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  "%*jalr\t%1%/"
  [(set_attr "type" "call")])

;; See comment for call_internal.
(define_insn_and_split "call_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "=df,df")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "=df,df")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI 31))]
  ""
  { return TARGET_SPLIT_CALLS ? "#" : "%*jal\t%1%/"; }
  "reload_completed && TARGET_SPLIT_CALLS && (operands[4] = insn)"
  [(const_int 0)]
{
  emit_call_insn (gen_call_value_multiple_split (operands[0], operands[1],
						 operands[2], operands[3]));
  if (!find_reg_note (operands[4], REG_NORETURN, 0))
    mips_restore_gp ();
  DONE;
}
  [(set_attr "jal" "indirect,direct")
   (set_attr "extended_mips16" "no,yes")])

(define_insn "call_value_multiple_split"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "=df")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI 31))
   (clobber (reg:SI 28))]
  "TARGET_SPLIT_CALLS"
  "%*jalr\t%1%/"
  [(set_attr "type" "call")])

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

;;
;;  ....................
;;
;;	MISC.
;;
;;  ....................
;;


(define_expand "prefetch"
  [(prefetch (match_operand 0 "address_operand" "")
	     (match_operand 1 "const_int_operand" "")
	     (match_operand 2 "const_int_operand" ""))]
  "ISA_HAS_PREFETCH"
{
  if (symbolic_operand (operands[0], GET_MODE (operands[0])))
    operands[0] = force_reg (GET_MODE (operands[0]), operands[0]);
})

(define_insn "prefetch_si_address"
  [(prefetch (plus:SI (match_operand:SI 0 "register_operand" "r")
		      (match_operand:SI 3 "const_int_operand" "I"))
	     (match_operand:SI 1 "const_int_operand" "n")
	     (match_operand:SI 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCH && Pmode == SImode"
  { return mips_emit_prefetch (operands); }
  [(set_attr "type" "prefetch")])

(define_insn "prefetch_indexed_si"
  [(prefetch (plus:SI (match_operand:SI 0 "register_operand" "r")
		      (match_operand:SI 3 "register_operand" "r"))
	     (match_operand:SI 1 "const_int_operand" "n")
	     (match_operand:SI 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCHX && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && Pmode == SImode"
  { return mips_emit_prefetch (operands); }
  [(set_attr "type" "prefetchx")])

(define_insn "prefetch_si"
  [(prefetch (match_operand:SI 0 "register_operand" "r")
	     (match_operand:SI 1 "const_int_operand" "n")
	     (match_operand:SI 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCH && Pmode == SImode"
{
  operands[3] = const0_rtx;
  return mips_emit_prefetch (operands);
}
  [(set_attr "type" "prefetch")])

(define_insn "prefetch_di_address"
  [(prefetch (plus:DI (match_operand:DI 0 "register_operand" "r")
		      (match_operand:DI 3 "const_int_operand" "I"))
	     (match_operand:DI 1 "const_int_operand" "n")
	     (match_operand:DI 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCH && Pmode == DImode"
  { return mips_emit_prefetch (operands); }
  [(set_attr "type" "prefetch")])

(define_insn "prefetch_indexed_di"
  [(prefetch (plus:DI (match_operand:DI 0 "register_operand" "r")
		      (match_operand:DI 3 "register_operand" "r"))
	     (match_operand:DI 1 "const_int_operand" "n")
	     (match_operand:DI 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCHX && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && Pmode == DImode"
  { return mips_emit_prefetch (operands); }
  [(set_attr "type" "prefetchx")])

(define_insn "prefetch_di"
  [(prefetch (match_operand:DI 0 "register_operand" "r")
	     (match_operand:DI 1 "const_int_operand" "n")
	     (match_operand:DI 2 "const_int_operand" "n"))]
  "ISA_HAS_PREFETCH && Pmode == DImode"
{
  operands[3] = const0_rtx;
  return mips_emit_prefetch (operands);
}
  [(set_attr "type" "prefetch")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "%(nop%)"
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")])

;; Like nop, but commented out when outside a .set noreorder block.
(define_insn "hazard_nop"
  [(const_int 1)]
  ""
  {
    if (set_noreorder)
      return "nop";
    else
      return "#nop";
  }
  [(set_attr "type"	"arith")])

;; MIPS4 Conditional move instructions.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(if_then_else:SI
	 (match_operator 4 "equality_op"
			 [(match_operand:SI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:SI 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:SI 3 "reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
  "@
    mov%B4\t%0,%z2,%1
    mov%b4\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(if_then_else:SI
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:SI 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:SI 3 "reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
  "@
    mov%B4\t%0,%z2,%1
    mov%b4\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(if_then_else:SI
	 (match_operator 3 "equality_op" [(match_operand:CC 4
							    "register_operand"
							    "z,z")
					  (const_int 0)])
	 (match_operand:SI 1 "reg_or_0_operand" "dJ,0")
	 (match_operand:SI 2 "reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "@
    mov%T3\t%0,%z1,%4
    mov%t3\t%0,%z2,%4"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(if_then_else:DI
	 (match_operator 4 "equality_op"
			 [(match_operand:SI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DI 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:DI 3 "reg_or_0_operand" "0,dJ")))]
  "(ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE) && TARGET_64BIT"
  "@
    mov%B4\t%0,%z2,%1
    mov%b4\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(if_then_else:DI
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DI 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:DI 3 "reg_or_0_operand" "0,dJ")))]
  "(ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE) && TARGET_64BIT"
  "@
    mov%B4\t%0,%z2,%1
    mov%b4\t%0,%z3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(if_then_else:DI
	 (match_operator 3 "equality_op" [(match_operand:CC 4
							    "register_operand"
							    "z,z")
					  (const_int 0)])
	 (match_operand:DI 1 "reg_or_0_operand" "dJ,0")
	 (match_operand:DI 2 "reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_64BIT"
  "@
    mov%T3\t%0,%z1,%4
    mov%t3\t%0,%z2,%4"
  [(set_attr "type" "condmove")
   (set_attr "mode" "DI")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF
	 (match_operator 4 "equality_op"
			 [(match_operand:SI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:SF 2 "register_operand" "f,0")
	 (match_operand:SF 3 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "@
    mov%B4.s\t%0,%2,%1
    mov%b4.s\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:SF 2 "register_operand" "f,0")
	 (match_operand:SF 3 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "@
    mov%B4.s\t%0,%2,%1
    mov%b4.s\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF
	 (match_operator 3 "equality_op" [(match_operand:CC 4
							    "register_operand"
							    "z,z")
					  (const_int 0)])
	 (match_operand:SF 1 "register_operand" "f,0")
	 (match_operand:SF 2 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "@
    mov%T3.s\t%0,%1,%4
    mov%t3.s\t%0,%2,%4"
  [(set_attr "type" "condmove")
   (set_attr "mode" "SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF
	 (match_operator 4 "equality_op"
			 [(match_operand:SI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DF 2 "register_operand" "f,0")
	 (match_operand:DF 3 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "@
    mov%B4.d\t%0,%2,%1
    mov%b4.d\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "DF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DF 2 "register_operand" "f,0")
	 (match_operand:DF 3 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "@
    mov%B4.d\t%0,%2,%1
    mov%b4.d\t%0,%3,%1"
  [(set_attr "type" "condmove")
   (set_attr "mode" "DF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF
	 (match_operator 3 "equality_op" [(match_operand:CC 4
							    "register_operand"
							    "z,z")
					  (const_int 0)])
	 (match_operand:DF 1 "register_operand" "f,0")
	 (match_operand:DF 2 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "@
    mov%T3.d\t%0,%1,%4
    mov%t3.d\t%0,%2,%4"
  [(set_attr "type" "condmove")
   (set_attr "mode" "DF")])

;; These are the main define_expand's used to make conditional moves.

(define_expand "movsicc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI (match_dup 5)
			 (match_operand:SI 2 "reg_or_0_operand" "")
			 (match_operand:SI 3 "reg_or_0_operand" "")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
{
  gen_conditional_move (operands);
  DONE;
})

(define_expand "movdicc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (match_dup 5)
			 (match_operand:DI 2 "reg_or_0_operand" "")
			 (match_operand:DI 3 "reg_or_0_operand" "")))]
  "(ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE) && TARGET_64BIT"
{
  gen_conditional_move (operands);
  DONE;
})

(define_expand "movsfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (match_dup 5)
			 (match_operand:SF 2 "register_operand" "")
			 (match_operand:SF 3 "register_operand" "")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
{
  gen_conditional_move (operands);
  DONE;
})

(define_expand "movdfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (match_dup 5)
			 (match_operand:DF 2 "register_operand" "")
			 (match_operand:DF 3 "register_operand" "")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
{
  gen_conditional_move (operands);
  DONE;
})

;;
;;  ....................
;;
;;	mips16 inline constant tables
;;
;;  ....................
;;

(define_insn "consttable_qi"
  [(unspec_volatile [(match_operand:QI 0 "consttable_operand" "=g")]
		    UNSPEC_CONSTTABLE_QI)]
  "TARGET_MIPS16"
{
  assemble_integer (operands[0], 1, BITS_PER_UNIT, 1);
  return "";
}
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"QI")
   (set_attr "length"	"8")])

(define_insn "consttable_hi"
  [(unspec_volatile [(match_operand:HI 0 "consttable_operand" "=g")]
		    UNSPEC_CONSTTABLE_HI)]
  "TARGET_MIPS16"
{
  assemble_integer (operands[0], 2, BITS_PER_UNIT * 2, 1);
  return "";
}
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"HI")
   (set_attr "length"	"8")])

(define_insn "consttable_si"
  [(unspec_volatile [(match_operand:SI 0 "consttable_operand" "=g")]
		    UNSPEC_CONSTTABLE_SI)]
  "TARGET_MIPS16"
{
  assemble_integer (operands[0], 4, BITS_PER_UNIT * 4, 1);
  return "";
}
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_insn "consttable_di"
  [(unspec_volatile [(match_operand:DI 0 "consttable_operand" "=g")]
		    UNSPEC_CONSTTABLE_DI)]
  "TARGET_MIPS16"
{
  assemble_integer (operands[0], 8, BITS_PER_UNIT * 8, 1);
  return "";
}
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])

(define_insn "consttable_sf"
  [(unspec_volatile [(match_operand:SF 0 "consttable_operand" "=g")]
		    UNSPEC_CONSTTABLE_SF)]
  "TARGET_MIPS16"
{
  REAL_VALUE_TYPE d;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  REAL_VALUE_FROM_CONST_DOUBLE (d, operands[0]);
  assemble_real (d, SFmode, GET_MODE_ALIGNMENT (SFmode));
  return "";
}
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"SF")
   (set_attr "length"	"8")])

(define_insn "consttable_df"
  [(unspec_volatile [(match_operand:DF 0 "consttable_operand" "=g")]
		    UNSPEC_CONSTTABLE_DF)]
  "TARGET_MIPS16"
{
  REAL_VALUE_TYPE d;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  REAL_VALUE_FROM_CONST_DOUBLE (d, operands[0]);
  assemble_real (d, DFmode, GET_MODE_ALIGNMENT (DFmode));
  return "";
}
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"DF")
   (set_attr "length"	"16")])

(define_insn "align_2"
  [(unspec_volatile [(const_int 0)] UNSPEC_ALIGN_2)]
  "TARGET_MIPS16"
  ".align 1"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"HI")
   (set_attr "length"	"8")])

(define_insn "align_4"
  [(unspec_volatile [(const_int 0)] UNSPEC_ALIGN_4)]
  "TARGET_MIPS16"
  ".align 2"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_insn "align_8"
  [(unspec_volatile [(const_int 0)] UNSPEC_ALIGN_8)]
  "TARGET_MIPS16"
  ".align 3"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12")])

;;
;;  ....................
;;
;;	mips16 peepholes
;;
;;  ....................
;;

;; On the mips16, reload will sometimes decide that a pseudo register
;; should go into $24, and then later on have to reload that register.
;; When that happens, we get a load of a general register followed by
;; a move from the general register to $24 followed by a branch.
;; These peepholes catch the common case, and fix it to just use the
;; general register for the branch.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=t")
	(match_operand:SI 1 "register_operand" "d"))
   (set (pc)
	(if_then_else (match_operator:SI 2 "equality_op" [(match_dup 0)
							  (const_int 0)])
		      (match_operand 3 "pc_or_label_operand" "")
		      (match_operand 4 "pc_or_label_operand" "")))]
  "TARGET_MIPS16
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))"
{
  if (operands[3] != pc_rtx)
    return "b%C2z\t%1,%3";
  else
    return "b%N2z\t%1,%4";
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=t")
	(match_operand:DI 1 "register_operand" "d"))
   (set (pc)
	(if_then_else (match_operator:DI 2 "equality_op" [(match_dup 0)
							  (const_int 0)])
		      (match_operand 3 "pc_or_label_operand" "")
		      (match_operand 4 "pc_or_label_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))"
{
  if (operands[3] != pc_rtx)
    return "b%C2z\t%1,%3";
  else
    return "b%N2z\t%1,%4";
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

;; We can also have the reverse reload: reload will spill $24 into
;; another register, and then do a branch on that register when it
;; could have just stuck with $24.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=d")
	(match_operand:SI 1 "register_operand" "t"))
   (set (pc)
	(if_then_else (match_operator:SI 2 "equality_op" [(match_dup 0)
							  (const_int 0)])
		      (match_operand 3 "pc_or_label_operand" "")
		      (match_operand 4 "pc_or_label_operand" "")))]
  "TARGET_MIPS16
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0])"
{
  if (operands[3] != pc_rtx)
    return "bt%C2z\t%3";
  else
    return "bt%N2z\t%4";
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=d")
	(match_operand:DI 1 "register_operand" "t"))
   (set (pc)
	(if_then_else (match_operator:DI 2 "equality_op" [(match_dup 0)
							  (const_int 0)])
		      (match_operand 3 "pc_or_label_operand" "")
		      (match_operand 4 "pc_or_label_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0])"
{
  if (operands[3] != pc_rtx)
    return "bt%C2z\t%3";
  else
    return "bt%N2z\t%4";
}
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_split
  [(match_operand 0 "small_data_pattern" "")]
  "reload_completed"
  [(match_dup 0)]
  { operands[0] = mips_rewrite_small_data (operands[0]); })
