;;  Mips.md	     Machine Description for MIPS based processors
;;  Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;  1999, 2000 Free Software Foundation, Inc.
;;  Contributed by   A. Lichnewsky, lich@inria.inria.fr
;;  Changes by       Michael Meissner, meissner@osf.org
;;  64 bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
;;  Brendan Eich, brendan@microunity.com.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ??? Currently does not have define_function_unit support for the R8000.
;; Must include new entries for fmadd in addition to existing entries.



;; ....................
;;
;;	Attributes
;;
;; ....................

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump
;; call		unconditional call
;; load		load instruction(s)
;; store	store instruction(s)
;; move		data movement within same register set
;; xfer		transfer to/from coprocessor
;; hilo		transfer of hi/lo registers
;; arith	integer arithmetic instruction
;; darith	double precision integer arithmetic instructions
;; imul		integer multiply
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
;; multi	multiword sequence (or user asm statements)
;; nop		no operation

(define_attr "type"
  "unknown,branch,jump,call,load,store,move,xfer,hilo,arith,darith,imul,idiv,icmp,fadd,fmul,fmadd,fdiv,fabs,fneg,fcmp,fcvt,fsqrt,multi,nop"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,SF,DF,FPSW" (const_string "unknown"))

;; Length (in # of bytes).  A conditional branch is allowed only to a
;; location within a signed 18-bit offset of the delay slot.  If that
;; provides too smal a range, we use the `j' instruction.  This
;; instruction takes a 28-bit value, but that value is not an offset.
;; Instead, it's bitwise-ored with the high-order four bits of the
;; instruction in the delay slot, which means it cannot be used to
;; cross a 256MB boundary.  We could fall back back on the jr,
;; instruction which allows full access to the entire address space,
;; but we do not do so at present.

(define_attr "length" "" 
   (cond [(eq_attr "type" "branch")
          (cond [(lt (abs (minus (match_dup 1) (plus (pc) (const_int 4))))
                     (const_int 131072))
                 (const_int 4)]
	         (const_int 12))]
          (const_int 4)))

;; Attribute describing the processor.  This attribute must match exactly
;; with the processor_type enumeration in mips.h.

;; Attribute describing the processor
;; (define_attr "cpu" "default,r3000,r6000,r4000"
;;   (const
;;    (cond [(eq (symbol_ref "mips_cpu") (symbol_ref "PROCESSOR_R3000"))   (const_string "r3000")
;;           (eq (symbol_ref "mips_cpu") (symbol_ref "PROCESSOR_R4000"))   (const_string "r4000")
;;           (eq (symbol_ref "mips_cpu") (symbol_ref "PROCESSOR_R6000"))   (const_string "r6000")]
;;          (const_string "default"))))

;; ??? Fix everything that tests this attribute.
(define_attr "cpu"
  "default,r3000,r3900,r6000,r4000,r4100,r4300,r4600,r4650,r5000,r8000"
  (const (symbol_ref "mips_cpu_attr")))

;; Does the instruction have a mandatory delay slot?
;;   The 3900, is (mostly) mips1, but does not have a mandatory load delay
;;   slot. 
(define_attr "dslot" "no,yes"
  (if_then_else (ior (eq_attr "type" "branch,jump,call,xfer,hilo,fcmp")
		     (and (eq_attr "type" "load")
			  (and (eq (symbol_ref "mips_isa") (const_int 1))
			       (and (eq (symbol_ref "mips16") (const_int 0))
                                    (eq_attr "cpu" "!r3900")))))
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
  [(set_attr "type" "multi")])

;; whether or not generating calls to position independent functions
(define_attr "abicalls" "no,yes"
  (const (symbol_ref "mips_abicalls_attr")))



;; .........................
;;
;;	Delay slots, can't describe load/fcmp/xfer delay slots here
;;
;; .........................

(define_delay (and (eq_attr "type" "branch")
		   (eq (symbol_ref "mips16") (const_int 0)))
  [(and (eq_attr "dslot" "no") (eq_attr "length" "4"))
   (nil)
   (and (eq_attr "branch_likely" "yes") (and (eq_attr "dslot" "no") (eq_attr "length" "4")))])

(define_delay (eq_attr "type" "jump")
  [(and (eq_attr "dslot" "no") (eq_attr "length" "4"))
   (nil)
   (nil)])

(define_delay (and (eq_attr "type" "call") (eq_attr "abicalls" "no"))
  [(and (eq_attr "dslot" "no") (eq_attr "length" "4"))
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
       (eq_attr "cpu" "!r3000,r3900,r4600,r4650,r4100,r4300,r5000"))
  3 0)

(define_function_unit "memory" 1 0
  (and (eq_attr "type" "load")
       (eq_attr "cpu" "r3000,r3900,r4600,r4650,r4100,r4300,r5000"))
  2 0)

(define_function_unit "memory"   1 0 (eq_attr "type" "store") 1 0)

(define_function_unit "memory"   1 0 (eq_attr "type" "xfer") 2 0)

(define_function_unit "imuldiv"  1 0
  (eq_attr "type" "hilo")
  1 3)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul")
       (eq_attr "cpu" "!r3000,r3900,r4000,r4600,r4650,r4100,r4300,r5000"))
  17 17)

;; On them mips16, we want to stronly discourage a mult from appearing
;; after an mflo, since that requires explicit nop instructions.  We
;; do this by pretending that mflo ties up the function unit for long
;; enough that the scheduler will ignore load stalls and the like when
;; selecting instructions to between the two instructions.

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "hilo") (ne (symbol_ref "mips16") (const_int 0)))
  1 5)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul") (eq_attr "cpu" "r3000,r3900"))
  12 12)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul") (eq_attr "cpu" "r4000,r4600"))
  10 10)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul") (eq_attr "cpu" "r4650"))
  4 4)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4100")))
  1 1)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4100")))
  4 4)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul")
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4300,r5000")))
  5 5)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4300")))
  8 8)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "imul")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r5000")))
  9 9)

(define_function_unit "imuldiv"  1 0
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "!r3000,r3900,r4000,r4600,r4650,r4100,r4300,r5000"))
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
       (and (eq_attr "mode" "SI") (eq_attr "cpu" "r4100")))
  35 35)

(define_function_unit "imuldiv" 1 0
  (and (eq_attr "type" "idiv")
       (and (eq_attr "mode" "DI") (eq_attr "cpu" "r4100")))
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
  (and (eq_attr "type" "fsqrt")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "!r4600,r4650,r4300,r5000")))
  54 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4600,r4650")))
  31 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt")
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r5000")))
  21 0)

;;; ??? Is this number right?
(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "!r4600,r4650,r4300,r5000")))
  112 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt")
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r4600,r4650")))
  60 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fsqrt")
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
  (and (and (eq_attr "type" "fdiv") (eq_attr "type" "fsqrt"))
       (and (eq_attr "mode" "SF") (eq_attr "cpu" "r4300")))
  29 29)
(define_function_unit "imuldiv" 1 0
  (and (and (eq_attr "type" "fdiv") (eq_attr "type" "fsqrt"))
       (and (eq_attr "mode" "DF") (eq_attr "cpu" "r4300")))
  58 58)

;; The following functional units do not use the cpu type, and use
;; much less memory in genattrtab.c.

;; (define_function_unit "memory"   1 0 (eq_attr "type" "load")                                3 0)
;; (define_function_unit "memory"   1 0 (eq_attr "type" "store")                               1 0)
;;       
;; (define_function_unit "fp_comp"  1 0 (eq_attr "type" "fcmp")                                2 0)
;;       
;; (define_function_unit "transfer" 1 0 (eq_attr "type" "xfer")                                2 0)
;; (define_function_unit "transfer" 1 0 (eq_attr "type" "hilo")                                3 0)
;;   
;; (define_function_unit "imuldiv"  1 1 (eq_attr "type" "imul")                               17 0)
;; (define_function_unit "imuldiv"  1 1 (eq_attr "type" "idiv")                               38 0)
;;   
;; (define_function_unit "adder"    1 1 (eq_attr "type" "fadd")                                4 0)
;; (define_function_unit "adder"    1 1 (eq_attr "type" "fabs,fneg")                           2 0)
;;   
;; (define_function_unit "mult"     1 1 (and (eq_attr "type" "fmul") (eq_attr "mode" "SF"))    7 0)
;; (define_function_unit "mult"     1 1 (and (eq_attr "type" "fmul") (eq_attr "mode" "DF"))    8 0)
;;   
;; (define_function_unit "divide"   1 1 (and (eq_attr "type" "fdiv") (eq_attr "mode" "SF"))   23 0)
;; (define_function_unit "divide"   1 1 (and (eq_attr "type" "fdiv") (eq_attr "mode" "DF"))   36 0)
;; 
;; (define_function_unit "sqrt"     1 1 (and (eq_attr "type" "fsqrt") (eq_attr "mode" "SF"))  54 0)
;; (define_function_unit "sqrt"     1 1 (and (eq_attr "type" "fsqrt") (eq_attr "mode" "DF")) 112 0)


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
  "add.d\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"DF")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "add.s\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"SF")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		 (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
{
  /* The mips16 assembler handles -32768 correctly, and so does gas,
     but some other MIPS assemblers think that -32768 needs to be
     loaded into a register before it can be added in.  */
  if (! TARGET_MIPS16
      && ! TARGET_GAS
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) == -32768)
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn "addsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		 (match_operand:SI 2 "arith_operand" "dI")))]
  "! TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)"
  "addu\\t%0,%z1,%2"
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
  "addu\\t%$,%$,%0"
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
  "addu\\t%0,%$,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")	(if_then_else (match_operand:VOID 1 "m16_uimm8_4" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(plus:SI (match_operand:SI 1 "register_operand" "0,d,d")
		 (match_operand:SI 2 "arith_operand" "IQ,O,d")))]
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
  "*
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"addu\\t%0,%2\";
  return \"addu\\t%0,%1,%2\";
}"
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
  "TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x7f
	&& INTVAL (operands[1]) <= 0x7f + 0x7f)
       || (INTVAL (operands[1]) < - 0x80
	   && INTVAL (operands[1]) >= - 0x80 - 0x80))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 2)))]
  "
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
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed
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
  "
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
}")

(define_expand "adddi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (plus:DI (match_operand:DI 1 "se_register_operand" "")
			    (match_operand:DI 2 "se_arith_operand" "")))
	      (clobber (match_dup 3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
  "
{
  /* The mips16 assembler handles -32768 correctly, and so does gas,
     but some other MIPS assemblers think that -32768 needs to be
     loaded into a register before it can be added in.  */
  if (! TARGET_MIPS16
      && ! TARGET_GAS
      && GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) == -32768)
    operands[2] = force_reg (DImode, operands[2]);

  if (TARGET_64BIT)
    {
      emit_insn (gen_adddi3_internal_3 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
}")

(define_insn "adddi3_internal_1"
  [(set (match_operand:DI 0 "register_operand" "=d,&d")
	(plus:DI (match_operand:DI 1 "register_operand" "0,d")
		 (match_operand:DI 2 "register_operand" "d,d")))
   (clobber (match_operand:SI 3 "register_operand" "=d,d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "*
{
  return (REGNO (operands[0]) == REGNO (operands[1])
	  && REGNO (operands[0]) == REGNO (operands[2]))
    ? \"srl\\t%3,%L0,31\;sll\\t%M0,%M0,1\;sll\\t%L0,%L1,1\;addu\\t%M0,%M0,%3\"
    : \"addu\\t%L0,%L1,%L2\;sltu\\t%3,%L0,%L2\;addu\\t%M0,%M1,%M2\;addu\\t%M0,%M0,%3\";
}"
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

   (set (subreg:SI (match_dup 0) 1)
	(plus:SI (subreg:SI (match_dup 1) 1)
		 (subreg:SI (match_dup 2) 1)))

   (set (subreg:SI (match_dup 0) 1)
	(plus:SI (subreg:SI (match_dup 0) 1)
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

  [(set (subreg:SI (match_dup 0) 1)
	(plus:SI (subreg:SI (match_dup 1) 1)
		 (subreg:SI (match_dup 2) 1)))

   (set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 0) 1)
		(subreg:SI (match_dup 2) 1)))

   (set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 1) 0)
		 (subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 0) 0)
		 (match_dup 3)))]
  "")

(define_insn "adddi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(plus:DI (match_operand:DI 1 "register_operand" "%d,%d,%d")
		 (match_operand:DI 2 "small_int" "P,J,N")))
   (clobber (match_operand:SI 3 "register_operand" "=d,d,d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)"
  "@
   addu\\t%L0,%L1,%2\;sltu\\t%3,%L0,%2\;addu\\t%M0,%M1,%3
   move\\t%L0,%L1\;move\\t%M0,%M1
   subu\\t%L0,%L1,%n2\;sltu\\t%3,%L0,%2\;subu\\t%M0,%M1,1\;addu\\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12,8,16")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
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

   (set (subreg:SI (match_dup 0) 1)
	(plus:SI (subreg:SI (match_dup 1) 1)
		 (match_dup 3)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0"

  [(set (subreg:SI (match_dup 0) 1)
	(plus:SI (subreg:SI (match_dup 1) 1)
		 (match_dup 2)))

   (set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 0) 1)
		(match_dup 2)))

   (set (subreg:SI (match_dup 0) 0)
	(plus:SI (subreg:SI (match_dup 1) 0)
		 (match_dup 3)))]
  "")

(define_insn "adddi3_internal_3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(plus:DI (match_operand:DI 1 "se_reg_or_0_operand" "dJ")
		 (match_operand:DI 2 "se_arith_operand" "dI")))]
  "TARGET_64BIT
   && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)"
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"dsubu\\t%0,%z1,%n2\"
    : \"daddu\\t%0,%z1,%2\";
}"
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
  "daddu\\t%$,%$,%0"
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
  "daddu\\t%0,%$,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_uimm5_4" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(plus:DI (match_operand:DI 1 "register_operand" "0,d,d")
		 (match_operand:DI 2 "arith_operand" "IQ,O,d")))]
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
  "*
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"daddu\\t%0,%2\";
  return \"daddu\\t%0,%1,%2\";
}"
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
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0xf
	&& INTVAL (operands[1]) <= 0xf + 0xf)
       || (INTVAL (operands[1]) < - 0x10
	   && INTVAL (operands[1]) >= - 0x10 - 0x10))"
  [(set (match_dup 0) (plus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (plus:DI (match_dup 0) (match_dup 2)))]
  "
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
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed
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
  "
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
}")

(define_insn "addsi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI (plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
				 (match_operand:SI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT
   && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768)"
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"subu\\t%0,%z1,%n2\"
    : \"addu\\t%0,%z1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(sign_extend:DI (plus:SI (match_operand:SI 1 "register_operand" "0,d,d")
				 (match_operand:SI 2 "arith_operand" "I,O,d"))))]
  "TARGET_MIPS16 && TARGET_64BIT"
  "*
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"addu\\t%0,%2\";
  return \"addu\\t%0,%1,%2\";
}"
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
  "sub.d\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"DF")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "sub.s\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"SF")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		  (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (INTVAL (operands[2]) == -32768
	  || (TARGET_MIPS16
	      && INTVAL (operands[2]) == -0x4000)))
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn "subsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		  (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "subu\\t%0,%z1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

;; For the mips16, we need to recognize stack pointer subtractions
;; explicitly, since we don't have a constraint for $sp.  These insns
;; will be generated by the save_restore_insns functions.

(define_insn ""
  [(set (reg:SI 29)
	(minus:SI (reg:SI 29)
		  (match_operand:SI 0 "small_int" "I")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "addu\\t%$,%$,%n0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_nsimm8_8" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (reg:SI 29)
		  (match_operand:SI 1 "small_int" "I")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "addu\\t%0,%$,%n1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length")	(if_then_else (match_operand:VOID 1 "m16_nuimm8_4" "")
				      (const_int 4)
				      (const_int 8)))])


(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(minus:SI (match_operand:SI 1 "register_operand" "0,d,d")
		  (match_operand:SI 2 "arith_operand" "I,O,d")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000))"
  "*
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"subu\\t%0,%2\";
  return \"subu\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_nsimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 2 "m16_nsimm4_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

;; On the mips16, we can sometimes split an subtract of a constant
;; which is a 4 byte instruction into two adds which are both 2 byte
;; instructions.  There are two cases: one where we are setting a
;; register to a register minus a constant, and one where we are
;; simply subtracting a constant from a register.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_dup 0)
		  (match_operand:SI 1 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x80
	&& INTVAL (operands[1]) <= 0x80 + 0x80)
       || (INTVAL (operands[1]) < - 0x7f
	   && INTVAL (operands[1]) >= - 0x7f - 0x7f))"
  [(set (match_dup 0) (minus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (minus:SI (match_dup 0) (match_dup 2)))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val >= 0)
    {
      operands[1] = GEN_INT (0x80);
      operands[2] = GEN_INT (val - 0x80);
    }
  else
    {
      operands[1] = GEN_INT (- 0x7f);
      operands[2] = GEN_INT (val + 0x7f);
    }
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "register_operand" "")
		  (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x8
	&& INTVAL (operands[2]) <= 0x8 + 0x80)
       || (INTVAL (operands[2]) < - 0x7
	   && INTVAL (operands[2]) >= - 0x7 - 0x7f))"
  [(set (match_dup 0) (minus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (minus:SI (match_dup 0) (match_dup 3)))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val >= 0)
    {
      operands[2] = GEN_INT (0x8);
      operands[3] = GEN_INT (val - 0x8);
    }
  else
    {
      operands[2] = GEN_INT (- 0x7);
      operands[3] = GEN_INT (val + 0x7);
    }
}")

(define_expand "subdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=d")
		   (minus:DI (match_operand:DI 1 "se_register_operand" "d")
			     (match_operand:DI 2 "se_register_operand" "d")))
	      (clobber (match_dup 3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
  "
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_subdi3_internal_3 (operands[0], operands[1],
					operands[2]));
      DONE;
    }

  operands[3] = gen_reg_rtx (SImode);
}")

(define_insn "subdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "sltu\\t%3,%L1,%L2\;subu\\t%L0,%L1,%L2\;subu\\t%M0,%M1,%M2\;subu\\t%M0,%M0,%3"
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

   (set (subreg:SI (match_dup 0) 1)
	(minus:SI (subreg:SI (match_dup 1) 1)
		  (subreg:SI (match_dup 2) 1)))

   (set (subreg:SI (match_dup 0) 1)
	(minus:SI (subreg:SI (match_dup 0) 1)
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
	(ltu:SI (subreg:SI (match_dup 1) 1)
	        (subreg:SI (match_dup 2) 1)))

   (set (subreg:SI (match_dup 0) 1)
	(minus:SI (subreg:SI (match_dup 1) 1)
		  (subreg:SI (match_dup 2) 1)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 1) 0)
		  (subreg:SI (match_dup 2) 0)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 0) 0)
		  (match_dup 3)))]
  "")

(define_insn "subdi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(minus:DI (match_operand:DI 1 "register_operand" "d,d,d")
		  (match_operand:DI 2 "small_int" "P,J,N")))
   (clobber (match_operand:SI 3 "register_operand" "=d,d,d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && INTVAL (operands[2]) != -32768"
  "@
   sltu\\t%3,%L1,%2\;subu\\t%L0,%L1,%2\;subu\\t%M0,%M1,%3
   move\\t%L0,%L1\;move\\t%M0,%M1
   sltu\\t%3,%L1,%2\;subu\\t%L0,%L1,%2\;subu\\t%M0,%M1,1\;subu\\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12,8,16")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0"

  [(set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 1) 0)
		(match_dup 2)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 1) 0)
		  (match_dup 2)))

   (set (subreg:SI (match_dup 0) 1)
	(minus:SI (subreg:SI (match_dup 1) 1)
		  (match_dup 3)))]
  "")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0"

  [(set (match_dup 3)
	(ltu:SI (subreg:SI (match_dup 1) 1)
		(match_dup 2)))

   (set (subreg:SI (match_dup 0) 1)
	(minus:SI (subreg:SI (match_dup 1) 1)
		  (match_dup 2)))

   (set (subreg:SI (match_dup 0) 0)
	(minus:SI (subreg:SI (match_dup 1) 0)
		  (match_dup 3)))]
  "")

(define_insn "subdi3_internal_3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "se_reg_or_0_operand" "dJ")
		  (match_operand:DI 2 "se_arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"daddu\\t%0,%z1,%n2\"
    : \"dsubu\\t%0,%z1,%2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")])

;; For the mips16, we need to recognize stack pointer subtractions
;; explicitly, since we don't have a constraint for $sp.  These insns
;; will be generated by the save_restore_insns functions.

(define_insn ""
  [(set (reg:DI 29)
	(minus:DI (reg:DI 29)
		  (match_operand:DI 0 "small_int" "I")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "daddu\\t%$,%$,%n0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_nsimm8_8" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (reg:DI 29)
		  (match_operand:DI 1 "small_int" "I")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "daddu\\t%0,%$,%n1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set (attr "length")	(if_then_else (match_operand:VOID 0 "m16_nuimm5_4" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(minus:DI (match_operand:DI 1 "register_operand" "0,d,d")
		  (match_operand:DI 2 "arith_operand" "I,O,d")))]
  "TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000))"
  "*
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"dsubu\\t%0,%2\";
  return \"dsubu\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_nsimm5_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 2 "m16_nsimm4_1" "")
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
	(minus:DI (match_dup 0)
		  (match_operand:DI 1 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x10
	&& INTVAL (operands[1]) <= 0x10 + 0x10)
       || (INTVAL (operands[1]) < - 0xf
	   && INTVAL (operands[1]) >= - 0xf - 0xf))"
  [(set (match_dup 0) (minus:DI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (minus:DI (match_dup 0) (match_dup 2)))]
  "
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
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "const_int_operand" "")))]
  "TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x8
	&& INTVAL (operands[2]) <= 0x8 + 0x10)
       || (INTVAL (operands[2]) < - 0x7
	   && INTVAL (operands[2]) >= - 0x7 - 0xf))"
  [(set (match_dup 0) (minus:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (minus:DI (match_dup 0) (match_dup 3)))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[2]);

  if (val >= 0)
    {
      operands[2] = GEN_INT (0x8);
      operands[3] = GEN_INT (val - 0x8);
    }
  else
    {
      operands[2] = GEN_INT (- 0x7);
      operands[3] = GEN_INT (val + 0x7);
    }
}")

(define_insn "subsi3_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
				  (match_operand:SI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT && !TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768)"
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"addu\\t%0,%z1,%n2\"
    : \"subu\\t%0,%z1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(sign_extend:DI (minus:SI (match_operand:SI 1 "register_operand" "0,d,d")
				  (match_operand:SI 2 "arith_operand" "I,O,d"))))]
  "TARGET_64BIT && TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000))"
  "*
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"subu\\t%0,%2\";
  return \"subu\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_nsimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 2 "m16_nsimm4_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])
  


;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

;; Early Vr4300 silicon has a CPU bug where multiplies with certain
;; operands may corrupt immediately following multiplies. This is a
;; simple fix to insert NOPs.

(define_expand "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "
{
  if (mips_cpu != PROCESSOR_R4300)
    emit_insn (gen_muldf3_internal (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_muldf3_r4300 (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn "muldf3_internal"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && mips_cpu != PROCESSOR_R4300"
  "mul.d\\t%0,%1,%2"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"DF")])

(define_insn "muldf3_r4300"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && mips_cpu == PROCESSOR_R4300"
  "*
{
  output_asm_insn (\"mul.d\\t%0,%1,%2\", operands);
  if (TARGET_4300_MUL_FIX)
    output_asm_insn (\"nop\", operands);
  return \"\";
}"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8")])	;; mul.d + nop

(define_expand "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "
{
  if (mips_cpu != PROCESSOR_R4300)
    emit_insn( gen_mulsf3_internal (operands[0], operands[1], operands[2]));
  else
    emit_insn( gen_mulsf3_r4300 (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn "mulsf3_internal"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && mips_cpu != PROCESSOR_R4300"
  "mul.s\\t%0,%1,%2"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"SF")])

(define_insn "mulsf3_r4300"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && mips_cpu == PROCESSOR_R4300"
  "*
{
  output_asm_insn (\"mul.s\\t%0,%1,%2\", operands);
  if (TARGET_4300_MUL_FIX)
    output_asm_insn (\"nop\", operands);
  return \"\";
}"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"SF")
   (set_attr "length"	"8")])	;; mul.s + nop


;; ??? The R4000 (only) has a cpu bug.  If a double-word shift executes while
;; a multiply is in progress, it may give an incorrect result.  Avoid
;; this by keeping the mflo with the mult on the R4000.

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  ""
  "
{
  if (HAVE_mulsi3_mult3)
    emit_insn (gen_mulsi3_mult3 (operands[0], operands[1], operands[2]));
  else if (mips_cpu != PROCESSOR_R4000 || TARGET_MIPS16)
    emit_insn (gen_mulsi3_internal (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_mulsi3_r4000 (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn "mulsi3_mult3"
  [(set (match_operand:SI 0 "register_operand" "=d,l")
	(mult:SI (match_operand:SI 1 "register_operand" "d,d")
		 (match_operand:SI 2 "register_operand" "d,d")))
   (clobber (match_scratch:SI 3 "=h,h"))
   (clobber (match_scratch:SI 4 "=l,X"))
   (clobber (match_scratch:SI 5 "=a,a"))]
  "GENERATE_MULT3
   || TARGET_MAD"
  "*
{
  if (which_alternative == 1)
    return \"mult\\t%1,%2\";
  if (TARGET_MAD)
    return \"mul\\t%0,%1,%2\";
  return \"mult\\t%0,%1,%2\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "mulsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "mips_cpu != PROCESSOR_R4000 || TARGET_MIPS16"
  "mult\\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "mulsi3_r4000"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=l"))
   (clobber (match_scratch:SI 5 "=a"))]
  "mips_cpu == PROCESSOR_R4000 && !TARGET_MIPS16"
  "*
{
  rtx xoperands[10];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx_REG (SImode, LO_REGNUM);

  output_asm_insn (\"mult\\t%1,%2\", operands);
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return \"\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"   "12")])		;; mult + mflo + delay

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
   (clobber (match_scratch:SI 6 "=a,a,a"))
   (clobber (match_scratch:SI 7 "=X,X,d"))]
  "TARGET_MIPS3900
   && !TARGET_MIPS16"
  "*
{
  static const char *const madd[] = { \"madd\\t%1,%2\", \"madd\\t%0,%1,%2\" };
  if (which_alternative == 2)
    return \"#\";
  return madd[which_alternative];
}"
  [(set_attr "type"	"imul,imul,multi")
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
   (clobber (match_scratch:SI 6 ""))
   (clobber (match_scratch:SI 7 ""))]
  "reload_completed && GP_REG_P (true_regnum (operands[0])) && GP_REG_P (true_regnum (operands[3]))"
  [(parallel [(set (match_dup 7)
		   (mult:SI (match_dup 1) (match_dup 2)))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))])
   (set (match_dup 0) (plus:SI (match_dup 7) (match_dup 3)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "register_operand" "")
		  (mult:SI (match_operand:SI 2 "register_operand" "")
			   (match_operand:SI 3 "register_operand" ""))))
   (clobber (match_scratch:SI 4 ""))
   (clobber (match_scratch:SI 5 ""))
   (clobber (match_scratch:SI 6 ""))
   (clobber (match_scratch:SI 7 ""))]
  "reload_completed && GP_REG_P (true_regnum (operands[0])) && GP_REG_P (true_regnum (operands[1]))"
  [(parallel [(set (match_dup 7)
		   (mult:SI (match_dup 2) (match_dup 3)))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))])
   (set (match_dup 0) (minus:SI (match_dup 1) (match_dup 7)))]
  "")

(define_expand "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(mult:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_scratch:DI 3 "=h"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT"

  "
{
  if (GENERATE_MULT3 || mips_cpu == PROCESSOR_R4000 || TARGET_MIPS16)
    emit_insn (gen_muldi3_internal2 (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_muldi3_internal (operands[0], operands[1], operands[2]));
  DONE;
}")

;; Don't accept both operands using se_register_operand, because if
;; both operands are sign extended we would prefer to use mult in the
;; mulsidi3 pattern.  Commutativity should permit either operand to be
;; sign extended.

(define_insn "muldi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(mult:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_scratch:DI 3 "=h"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT && mips_cpu != PROCESSOR_R4000 && !TARGET_MIPS16"
  "dmult\\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")])

(define_insn "muldi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_scratch:DI 3 "=h"))
   (clobber (match_scratch:DI 4 "=l"))
   (clobber (match_scratch:DI 5 "=a"))]
  "TARGET_64BIT && (GENERATE_MULT3 || mips_cpu == PROCESSOR_R4000 || TARGET_MIPS16)"
  "*
{
  if (GENERATE_MULT3)
    output_asm_insn (\"dmult\\t%0,%1,%2\", operands);
  else 
    {
      rtx xoperands[10];

      xoperands[0] = operands[0];
      xoperands[1] = gen_rtx_REG (DImode, LO_REGNUM);

      output_asm_insn (\"dmult\\t%1,%2\", operands);
      output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
    }
  return \"\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ne (symbol_ref "GENERATE_MULT3") (const_int 0))
		       (const_int 4)
		       (const_int 12)))]) 	;; mult + mflo + delay

;; ??? We could define a mulditi3 pattern when TARGET_64BIT.

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  ""
  "
{
  rtx dummy = gen_rtx (SIGN_EXTEND, DImode, const0_rtx);
  if (TARGET_64BIT)
    emit_insn (gen_mulsidi3_64bit (operands[0], operands[1], operands[2],
				   dummy, dummy));
  else
    emit_insn (gen_mulsidi3_internal (operands[0], operands[1], operands[2],
				      dummy, dummy));
  DONE;
}")

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "d"))))]
  ""
  "
{
  rtx dummy = gen_rtx (ZERO_EXTEND, DImode, const0_rtx);
  if (TARGET_64BIT)
    emit_insn (gen_mulsidi3_64bit (operands[0], operands[1], operands[2],
				   dummy, dummy));
  else
    emit_insn (gen_mulsidi3_internal (operands[0], operands[1], operands[2],
				      dummy, dummy));
  DONE;
}")

(define_insn "mulsidi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI (match_operator:DI 3 "extend_operator"
				    [(match_operand:SI 1 "register_operand" "d")])
		 (match_operator:DI 4 "extend_operator"
				    [(match_operand:SI 2 "register_operand" "d")])))
   (clobber (match_scratch:SI 5 "=a"))]
  "!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "*
{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return \"mult\\t%1,%2\";
  return \"multu\\t%1,%2\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "mulsidi3_64bit"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(mult:DI (match_operator:DI 3 "extend_operator"
				    [(match_operand:SI 1 "register_operand" "d")])
		 (match_operator:DI 4 "extend_operator"
				    [(match_operand:SI 2 "register_operand" "d")])))
   (clobber (match_scratch:DI 5 "=l"))
   (clobber (match_scratch:DI 6 "=h"))]
  "TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "*
{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return \"mult\\t%1,%2\";
  return \"multu\\t%1,%2\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

;; _highpart patterns
(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
			       (sign_extend:DI (match_operand:SI 2 "register_operand" "d")))
		      (const_int 32))))]
  ""
  "
{
  rtx dummy = gen_rtx (SIGN_EXTEND, DImode, const0_rtx);
  rtx dummy2 = gen_rtx_LSHIFTRT (DImode, const0_rtx, const0_rtx);
#ifndef NO_MD_PROTOTYPES
  rtx (*genfn) PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
#else
  rtx (*genfn) ();
#endif
  genfn = gen_xmulsi3_highpart_internal;
  emit_insn ((*genfn) (operands[0], operands[1], operands[2], dummy,
		       dummy, dummy2));
  DONE;
}")

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
			       (zero_extend:DI (match_operand:SI 2 "register_operand" "d")))
		      (const_int 32))))]
  ""
  "
{
  rtx dummy = gen_rtx (ZERO_EXTEND, DImode, const0_rtx);
  rtx dummy2 = gen_rtx_LSHIFTRT (DImode, const0_rtx, const0_rtx);
#ifndef NO_MD_PROTOTYPES
  rtx (*genfn) PARAMS ((rtx, rtx, rtx, rtx, rtx, rtx));
#else
  rtx (*genfn) ();
#endif
  genfn = gen_xmulsi3_highpart_internal;
  emit_insn ((*genfn) (operands[0], operands[1], operands[2], dummy,
		       dummy, dummy2));
  DONE;
}")

(define_insn "xmulsi3_highpart_internal"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(truncate:SI
	 (match_operator:DI 5 "highpart_shift_operator"
			    [(mult:DI (match_operator:DI 3 "extend_operator"
							 [(match_operand:SI 1 "register_operand" "d")])
				      (match_operator:DI 4 "extend_operator"
							 [(match_operand:SI 2 "register_operand" "d")]))
			     (const_int 32)])))
   (clobber (match_scratch:SI 6 "=l"))
   (clobber (match_scratch:SI 7 "=a"))]
  "GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "*
{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return \"mult\\t%1,%2\";
  else
    return \"multu\\t%1,%2\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "smuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(truncate:DI
	 (lshiftrt:TI (mult:TI (sign_extend:TI (match_operand:DI 1 "se_register_operand" "d"))
			       (sign_extend:TI (match_operand:DI 2 "se_register_operand" "d")))
		      (const_int 64))))
   (clobber (match_scratch:DI 3 "=l"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT"
  "dmult\\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")])

(define_insn "umuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(truncate:DI
	 (lshiftrt:TI (mult:TI (zero_extend:TI (match_operand:DI 1 "se_register_operand" "d"))
			       (zero_extend:TI (match_operand:DI 2 "se_register_operand" "d")))
		      (const_int 64))))
   (clobber (match_scratch:DI 3 "=l"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT"
  "dmultu\\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"DI")])

;; The R4650 supports a 32 bit multiply/ 64 bit accumulate
;; instruction.  The HI/LO registers are used as a 64 bit accumulator.

(define_insn "madsi"
  [(set (match_operand:SI 0 "register_operand" "+l")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "d")
			  (match_operand:SI 2 "register_operand" "d"))
		 (match_dup 0)))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "TARGET_MAD"
  "mad\\t%1,%2"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "*mul_acc_di"
  [(set (match_operand:DI 0 "register_operand" "+x")
	(plus:DI (mult:DI (match_operator:DI 3 "extend_operator"
			   [(match_operand:SI 1 "register_operand" "d")])
			  (match_operator:DI 4 "extend_operator"
			   [(match_operand:SI 2 "register_operand" "d")]))
		 (match_dup 0)))
   (clobber (match_scratch:SI 5 "=a"))]
  "TARGET_MAD
   && ! TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "*
{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return \"mad\\t%1,%2\";
  else
    return \"madu\\t%1,%2\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

(define_insn "*mul_acc_64bit_di"
  [(set (match_operand:DI 0 "register_operand" "+a")
	(plus:DI (mult:DI (match_operator:DI 3 "extend_operator"
			   [(match_operand:SI 1 "register_operand" "d")])
			  (match_operator:DI 4 "extend_operator"
			   [(match_operand:SI 2 "register_operand" "d")]))
		 (match_dup 0)))
   (clobber (match_scratch:SI 5 "=h"))
   (clobber (match_scratch:SI 6 "=l"))]
  "TARGET_MAD
   && TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4])"
  "*
{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return \"mad\\t%1,%2\";
  else
    return \"madu\\t%1,%2\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")])

;; Floating point multiply accumulate instructions.

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (mult:DF (match_operand:DF 1 "register_operand" "f")
			  (match_operand:DF 2 "register_operand" "f"))
		 (match_operand:DF 3 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "madd.d\\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (mult:SF (match_operand:SF 1 "register_operand" "f")
			  (match_operand:SF 2 "register_operand" "f"))
		 (match_operand:SF 3 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "madd.s\\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (mult:DF (match_operand:DF 1 "register_operand" "f")
			   (match_operand:DF 2 "register_operand" "f"))
		  (match_operand:DF 3 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "msub.d\\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (mult:SF (match_operand:SF 1 "register_operand" "f")
			   (match_operand:SF 2 "register_operand" "f"))
		  (match_operand:SF 3 "register_operand" "f")))]
		  
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "msub.s\\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (plus:DF (mult:DF (match_operand:DF 1 "register_operand" "f")
				  (match_operand:DF 2 "register_operand" "f"))
			 (match_operand:DF 3 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "nmadd.d\\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (plus:SF (mult:SF (match_operand:SF 1 "register_operand" "f")
				  (match_operand:SF 2 "register_operand" "f"))
			 (match_operand:SF 3 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "nmadd.s\\t%0,%3,%1,%2"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (mult:DF (match_operand:DF 2 "register_operand" "f")
			   (match_operand:DF 3 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "nmsub.d\\t%0,%1,%2,%3"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (mult:SF (match_operand:SF 2 "register_operand" "f")
			   (match_operand:SF 3 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "nmsub.s\\t%0,%1,%2,%3"
  [(set_attr "type"	"fmadd")
   (set_attr "mode"	"SF")])

;;
;;  ....................
;;
;;	DIVISION and REMAINDER
;;
;;  ....................
;;

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "div.d\\t%0,%1,%2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"DF")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "div.s\\t%0,%1,%2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "const_float_1_operand" "")
		(match_operand:DF 2 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_fast_math"
  "recip.d\\t%0,%2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "const_float_1_operand" "")
		(match_operand:SF 2 "register_operand" "f")))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && flag_fast_math"
  "recip.s\\t%0,%2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")])

;; If optimizing, prefer the divmod functions over separate div and
;; mod functions, since this will allow using one instruction for both
;; the quotient and remainder.  At present, the divmod is not moved out
;; of loops if it is constant within the loop, so allow -mdebugc to
;; use the old method of doing things.

;; 64 is the multiply/divide hi register
;; 65 is the multiply/divide lo register

;; ??? We can't accept constants here, because the MIPS assembler will replace
;; a divide by power of 2 with a shift, and then the remainder is no longer
;; available.

(define_expand "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=d")
	(mod:SI (match_dup 1)
		(match_dup 2)))
   (clobber (match_scratch:SI 4 "=l"))
   (clobber (match_scratch:SI 5 "=h"))
   (clobber (match_scratch:SI 6 "=a"))]
  "optimize"
  "
{
  emit_insn (gen_divmodsi4_internal (operands[0], operands[1], operands[2],
	     operands[3]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  if (TARGET_CHECK_RANGE_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (SImode, GEN_INT (-1)),
			       GEN_INT (0x6)));
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (SImode, GEN_INT (0x80000000)),
			       GEN_INT (0x6)));
    }
  
  DONE;
}")

(define_insn "divmodsi4_internal"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=h")
	(mod:SI (match_dup 1)
		(match_dup 2)))
   (clobber (match_scratch:SI 6 "=a"))]
  "optimize"
  "div\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "divmoddi4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(div:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))
   (set (match_operand:DI 3 "register_operand" "=d")
	(mod:DI (match_dup 1)
		(match_dup 2)))
   (clobber (match_scratch:DI 4 "=l"))
   (clobber (match_scratch:DI 5 "=h"))
   (clobber (match_scratch:DI 6 "=a"))]
  "TARGET_64BIT && optimize"
  "
{
  emit_insn (gen_divmoddi4_internal (operands[0], operands[1], operands[2],
             operands[3]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  if (TARGET_CHECK_RANGE_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (DImode, GEN_INT (-1)),
			       GEN_INT (0x6)));
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (DImode, GEN_INT (0x80000000)),
			       GEN_INT (0x6)));
    }
  
  DONE;
}")

(define_insn "divmoddi4_internal"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(div:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))
   (set (match_operand:DI 3 "register_operand" "=h")
	(mod:DI (match_dup 1)
		(match_dup 2)))
   (clobber (match_scratch:DI 6 "=a"))]
  "TARGET_64BIT && optimize"
  "ddiv\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=d")
	(umod:SI (match_dup 1)
		 (match_dup 2)))
   (clobber (match_scratch:SI 4 "=l"))
   (clobber (match_scratch:SI 5 "=h"))
   (clobber (match_scratch:SI 6 "=a"))]
  "optimize"
  "
{
  emit_insn (gen_udivmodsi4_internal (operands[0], operands[1], operands[2],
                                      operands[3]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  
  DONE;
}")

(define_insn "udivmodsi4_internal"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=h")
	(umod:SI (match_dup 1)
		 (match_dup 2)))
   (clobber (match_scratch:SI 6 "=a"))]
  "optimize"
  "divu\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "udivmoddi4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(udiv:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "se_register_operand" "d")))
   (set (match_operand:DI 3 "register_operand" "=d")
	(umod:DI (match_dup 1)
		 (match_dup 2)))
   (clobber (match_scratch:DI 4 "=l"))
   (clobber (match_scratch:DI 5 "=h"))
   (clobber (match_scratch:DI 6 "=a"))]
  "TARGET_64BIT && optimize"
  "
{
  emit_insn (gen_udivmoddi4_internal (operands[0], operands[1], operands[2],
                                      operands[3]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  
  DONE;
}")

(define_insn "udivmoddi4_internal"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(udiv:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "se_register_operand" "d")))
   (set (match_operand:DI 3 "register_operand" "=h")
	(umod:DI (match_dup 1)
		 (match_dup 2)))
   (clobber (match_scratch:DI 6 "=a"))]
  "TARGET_64BIT && optimize"
  "ddivu\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

;; Division trap

(define_expand "div_trap"
  [(trap_if (eq (match_operand 0 "register_operand" "d")
		(match_operand 1 "true_reg_or_0_operand" "dJ"))
            (match_operand 2 "immediate_operand" ""))]
  ""
  "
{
  if (TARGET_MIPS16)
    emit_insn (gen_div_trap_mips16 (operands[0],operands[1],operands[2]));
  else
    emit_insn (gen_div_trap_normal (operands[0],operands[1],operands[2]));
  DONE;
}")

(define_insn "div_trap_normal"
  [(trap_if (eq (match_operand 0 "register_operand" "d,d")
		(match_operand 1 "true_reg_or_0_operand" "d,J"))
            (match_operand 2 "immediate_operand" ""))]
  "!TARGET_MIPS16"
  "*
{
  rtx link;
  int have_dep_anti = 0;

  /* For divmod if one division is not needed then we don't need an extra
     divide by zero trap, which is anti dependent on previous trap */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))

    if ((int) REG_DEP_ANTI == (int) REG_NOTE_KIND (link)
        && GET_CODE (XEXP (link, 0)) == INSN
        && GET_CODE (PATTERN (XEXP (link, 0))) == TRAP_IF
	&& which_alternative == 1)
      have_dep_anti = 1;
  if (! have_dep_anti)
    {
      if (GENERATE_BRANCHLIKELY)
	{
          if (which_alternative == 1)
	    return \"%(beql\\t%0,$0,1f\\n\\tbreak\\t%2\\n%~1:%)\";
	  else
	    return \"%(beql\\t%0,%1,1f\\n\\tbreak\\t%2\\n%~1:%)\";
	}
      else
	{
          if (which_alternative == 1)
	    return \"%(bne\\t%0,$0,1f\\n\\tnop\\n\\tbreak\\t%2\\n%~1:%)\";
	  else
	    return \"%(bne\\t%0,%1,1f\\n\\tnop\\n\\tbreak\\t%2\\n%~1:%)\";
	}
    }
  return \"\";
}"
  [(set_attr "type" "unknown")
   (set_attr "length" "12")])


;; The mips16 bne insns is a macro which uses reg 24 as an intermediate.

(define_insn "div_trap_mips16"
  [(trap_if (eq (match_operand 0 "register_operand" "d,d")
		(match_operand 1 "true_reg_or_0_operand" "d,J"))
            (match_operand 2 "immediate_operand" ""))
   (clobber (reg:SI 24))]
  "TARGET_MIPS16"
  "*
{
  rtx link;
  int have_dep_anti = 0;

  /* For divmod if one division is not needed then we don't need an extra
     divide by zero trap, which is anti dependent on previous trap */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))

    if ((int) REG_DEP_ANTI == (int) REG_NOTE_KIND (link)
        && GET_CODE (XEXP (link, 0)) == INSN
        && GET_CODE (PATTERN (XEXP (link, 0))) == TRAP_IF
	&& which_alternative == 1)
      have_dep_anti = 1;
  if (! have_dep_anti)
    {
      /* No branch delay slots on mips16. */ 
      if (which_alternative == 1)
        return \"%(bnez\\t%0,1f\\n\\tbreak\\t%2\\n%~1:%)\";
      else
        return \"%(bne\\t%0,%1,1f\\n\\tbreak\\t%2\\n%~1:%)\";
    }
  return \"\";
}"
  [(set_attr "type" "unknown")
   (set_attr "length" "12")])

(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "
{
  emit_insn (gen_divsi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  if (TARGET_CHECK_RANGE_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (SImode, GEN_INT (-1)),
			       GEN_INT (0x6)));
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (SImode, GEN_INT (0x80000000)),
			       GEN_INT (0x6)));
    }
  
  DONE;
}")

(define_insn "divsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "div\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(div:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d"))) 
   (clobber (match_scratch:DI 3 "=h"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "
{
  emit_insn (gen_divdi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  if (TARGET_CHECK_RANGE_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (DImode, GEN_INT (-1)),
			       GEN_INT (0x6)));
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (DImode, GEN_INT (0x80000000)),
			       GEN_INT (0x6)));
    }
  
  DONE;
}")

(define_insn "divdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(div:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "ddiv\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"DI")])

(define_expand "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(mod:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "
{
  emit_insn (gen_modsi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  if (TARGET_CHECK_RANGE_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (SImode, GEN_INT (-1)),
			       GEN_INT (0x6)));
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (SImode, GEN_INT (0x80000000)),
			       GEN_INT (0x6)));
    }
  
  DONE;
}")

(define_insn "modsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(mod:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "div\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "moddi3"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(mod:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))
   (clobber (match_scratch:DI 3 "=l"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "
{
  emit_insn (gen_moddi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  if (TARGET_CHECK_RANGE_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (DImode, GEN_INT (-1)),
			       GEN_INT (0x6)));
      emit_insn (gen_div_trap (operands[2],
			       copy_to_mode_reg (DImode, GEN_INT (0x80000000)),
			       GEN_INT (0x6)));
    }
  
  DONE;
}")

(define_insn "moddi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(mod:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "ddiv\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"DI")])

(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "
{
  emit_insn (gen_udivsi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  
  DONE;
}")

(define_insn "udivsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=l")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "divu\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(udiv:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "se_register_operand" "di")))
   (clobber (match_scratch:DI 3 "=h"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "
{
  emit_insn (gen_udivdi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  
  DONE;
}")

(define_insn "udivdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=l")
	(udiv:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "se_nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (match_scratch:SI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "ddivu\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"DI")])

(define_expand "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(umod:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "
{
  emit_insn (gen_umodsi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  
  DONE;
}")

(define_insn "umodsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=h")
	(umod:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=a"))]
  "!optimize"
  "divu\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")])

(define_expand "umoddi3"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(umod:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "se_register_operand" "di")))
   (clobber (match_scratch:DI 3 "=l"))
   (clobber (match_scratch:DI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "
{
  emit_insn (gen_umoddi3_internal (operands[0], operands[1], operands[2]));
  if (!TARGET_NO_CHECK_ZERO_DIV)
    {
      emit_insn (gen_div_trap (operands[2],
			       GEN_INT (0),
			       GEN_INT (0x7)));
    }
  
  DONE;
}")

(define_insn "umoddi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(umod:DI (match_operand:DI 1 "se_register_operand" "d")
		 (match_operand:DI 2 "se_nonmemory_operand" "di")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=a"))]
  "TARGET_64BIT && !optimize"
  "ddivu\\t$0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"DI")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && HAVE_SQRT_P() && TARGET_DOUBLE_FLOAT"
  "sqrt.d\\t%0,%1"
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"DF")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && HAVE_SQRT_P()"
  "sqrt.s\\t%0,%1"
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "const_float_1_operand" "")
		(sqrt:DF (match_operand:DF 2 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_fast_math"
  "rsqrt.d\\t%0,%2"
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "const_float_1_operand" "")
		(sqrt:SF (match_operand:SF 2 "register_operand" "f"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && flag_fast_math"
  "rsqrt.s\\t%0,%2"
  [(set_attr "type"	"fsqrt")
   (set_attr "mode"	"SF")])


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
  "*
{
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  if (REGNO (operands[0]) == REGNO (operands[1]))
    {
      if (GENERATE_BRANCHLIKELY)
	return \"%(bltzl\\t%1,1f\\n\\tsubu\\t%0,%z2,%0\\n%~1:%)\";
      else
	return \"bgez\\t%1,1f%#\\n\\tsubu\\t%0,%z2,%0\\n%~1:\";
    }	  
  else
    return \"%(bgez\\t%1,1f\\n\\tmove\\t%0,%1\\n\\tsubu\\t%0,%z2,%0\\n%~1:%)\";
}"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"12")])

(define_insn "absdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(abs:DI (match_operand:DI 1 "se_register_operand" "d")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  if (REGNO (operands[0]) == REGNO (operands[1]))
    return \"%(bltzl\\t%1,1f\\n\\tdsubu\\t%0,%z2,%0\\n%~1:%)\";
  else
    return \"%(bgez\\t%1,1f\\n\\tmove\\t%0,%1\\n\\tdsubu\\t%0,%z2,%0\\n%~1:%)\";
}"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "abs.d\\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"DF")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs.s\\t%0,%1"
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
  "*
{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return \"%(\\
move\\t%0,%z4\\n\\
\\tbeq\\t%1,%z4,2f\\n\\
%~1:\\tand\\t%2,%1,0x0001\\n\\
\\taddu\\t%0,%0,1\\n\\
\\tbeq\\t%2,%z4,1b\\n\\
\\tsrl\\t%1,%1,1\\n\\
%~2:%)\";

  return \"%(\\
move\\t%0,%z4\\n\\
\\tmove\\t%3,%1\\n\\
\\tbeq\\t%3,%z4,2f\\n\\
%~1:\\tand\\t%2,%3,0x0001\\n\\
\\taddu\\t%0,%0,1\\n\\
\\tbeq\\t%2,%z4,1b\\n\\
\\tsrl\\t%3,%3,1\\n\\
%~2:%)\";
}"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"12")])

(define_insn "ffsdi2"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ffs:DI (match_operand:DI 1 "se_register_operand" "d")))
   (clobber (match_scratch:DI 2 "=&d"))
   (clobber (match_scratch:DI 3 "=&d"))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return \"%(\\
move\\t%0,%z4\\n\\
\\tbeq\\t%1,%z4,2f\\n\\
%~1:\\tand\\t%2,%1,0x0001\\n\\
\\tdaddu\\t%0,%0,1\\n\\
\\tbeq\\t%2,%z4,1b\\n\\
\\tdsrl\\t%1,%1,1\\n\\
%~2:%)\";

  return \"%(\\
move\\t%0,%z4\\n\\
\\tmove\\t%3,%1\\n\\
\\tbeq\\t%3,%z4,2f\\n\\
%~1:\\tand\\t%2,%3,0x0001\\n\\
\\tdaddu\\t%0,%0,1\\n\\
\\tbeq\\t%2,%z4,1b\\n\\
\\tdsrl\\t%3,%3,1\\n\\
%~2:%)\";
}"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"DI")
   (set_attr "length"	"24")])


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
  "*
{
  if (TARGET_MIPS16)
    return \"neg\\t%0,%1\";
  operands[2] = const0_rtx;
  return \"subu\\t%0,%z2,%1\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_expand "negdi2"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=d")
		   (neg:DI (match_operand:DI 1 "se_register_operand" "d")))
	      (clobber (match_dup 2))])]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16"
  "
{
  if (TARGET_64BIT)
    {
      emit_insn (gen_negdi2_internal_2 (operands[0], operands[1]));
      DONE;
    }

  operands[2] = gen_reg_rtx (SImode);
}")

(define_insn "negdi2_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "! TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "*
{
  operands[3] = const0_rtx;
  return \"subu\\t%L0,%z3,%L1\;subu\\t%M0,%z3,%M1\;sltu\\t%2,%z3,%L0\;subu\\t%M0,%M0,%2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])

(define_insn "negdi2_internal_2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "se_register_operand" "d")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  operands[2] = const0_rtx;
  return \"dsubu\\t%0,%z2,%1\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "neg.d\\t%0,%1"
  [(set_attr "type"	"fneg")
   (set_attr "mode"	"DF")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "neg.s\\t%0,%1"
  [(set_attr "type"	"fneg")
   (set_attr "mode"	"SF")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(not:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "*
{
  if (TARGET_MIPS16)
    return \"not\\t%0,%1\";
  operands[2] = const0_rtx;
  return \"nor\\t%0,%z2,%1\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:DI (match_operand:DI 1 "se_register_operand" "d")))]
  ""
  "*
{
  if (TARGET_MIPS16)
    {
      if (TARGET_64BIT)
	return \"not\\t%0,%1\";
      return \"not\\t%M0,%M1\;not\\t%L0,%L1\";
    }
  operands[2] = const0_rtx;
  if (TARGET_64BIT)
    return \"nor\\t%0,%z2,%1\";
  return \"nor\\t%M0,%z2,%M1\;nor\\t%L0,%z2,%L1\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ge (symbol_ref "mips_isa") (const_int 3))
		       (const_int 4)
		       (const_int 8)))])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (match_operand:DI 1 "register_operand" "")))]
  "reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"

  [(set (subreg:SI (match_dup 0) 0) (not:SI (subreg:SI (match_dup 1) 0)))
   (set (subreg:SI (match_dup 0) 1) (not:SI (subreg:SI (match_dup 1) 1)))]
  "")


;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

;; Many of these instructions uses trivial define_expands, because we
;; want to use a different set of constraints when TARGET_MIPS16.

(define_expand "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(and:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
  "
{
  if (TARGET_MIPS16)
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(and:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   and\\t%0,%1,%2
   andi\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "and\\t%0,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_expand "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT || !TARGET_DEBUG_G_MODE"
  "
{
  if (TARGET_MIPS16)
    operands[2] = force_reg (DImode, operands[2]);
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16"
  "*
{
  if (TARGET_64BIT)
    return \"and\\t%0,%1,%2\";
  return \"and\\t%M0,%M1,%M2\;and\\t%L0,%L1,%L2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ne (symbol_ref "TARGET_64BIT") (const_int 0))
		       (const_int 4)
		       (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "se_register_operand" "0")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && TARGET_MIPS16"
  "*
{
  if (TARGET_64BIT)
    return \"and\\t%0,%2\";
  return \"and\\t%M0,%M2\;and\\t%L0,%L2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ge (symbol_ref "mips_isa") (const_int 3))
		       (const_int 4)
		       (const_int 8)))])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (and:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0)))
   (set (subreg:SI (match_dup 0) 1) (and:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1)))]
  "")

(define_insn "anddi3_internal1"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(and:DI (match_operand:DI 1 "se_register_operand" "%d,d")
		(match_operand:DI 2 "se_uns_arith_operand" "d,K")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "@
   and\\t%0,%1,%2
   andi\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
  "
{
  if (TARGET_MIPS16)
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "!TARGET_MIPS16"
  "@
   or\\t%0,%1,%2
   ori\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "or\\t%0,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

;;; ??? There is no iordi3 pattern which accepts 'K' constants when
;;; TARGET_64BIT

(define_expand "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT || !TARGET_DEBUG_G_MODE"
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16"
  "*
{
  if (TARGET_64BIT)
    return \"or\\t%0,%1,%2\";
  return \"or\\t%M0,%M1,%M2\;or\\t%L0,%L1,%L2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ne (symbol_ref "TARGET_64BIT") (const_int 0))
		       (const_int 4)
		       (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "se_register_operand" "0")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && TARGET_MIPS16"
  "*
{
  if (TARGET_64BIT)
    return \"or\\t%0,%2\";
  return \"or\\t%M0,%M2\;or\\t%L0,%L2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ge (symbol_ref "mips_isa") (const_int 3))
		       (const_int 4)
		       (const_int 8)))])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (ior:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0)))
   (set (subreg:SI (match_dup 0) 1) (ior:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1)))]
  "")

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
   xor\\t%0,%1,%2
   xori\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,t,t")
	(xor:SI (match_operand:SI 1 "uns_arith_operand" "%0,d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K,d")))]
  "TARGET_MIPS16"
  "@
   xor\\t%0,%2
   cmpi\\t%1,%2
   cmp\\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

;; ??? If delete the 32-bit long long patterns, then could merge this with
;; the following xordi3_internal pattern.
(define_expand "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT || !TARGET_DEBUG_G_MODE"
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "(TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16"
  "*
{
  if (TARGET_64BIT)
    return \"xor\\t%0,%1,%2\";
  return \"xor\\t%M0,%M1,%M2\;xor\\t%L0,%L1,%L2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ne (symbol_ref "TARGET_64BIT") (const_int 0))
		       (const_int 4)
		       (const_int 8)))])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "se_register_operand" "0")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "!TARGET_64BIT && TARGET_MIPS16"
  "xor\\t%M0,%M2\;xor\\t%L0,%L2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,t,t")
	(xor:DI (match_operand:DI 1 "se_register_operand" "%0,d,d")
		(match_operand:DI 2 "se_uns_arith_operand" "d,K,d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "@
   xor\\t%0,%2
   cmpi\\t%1,%2
   cmp\\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)])])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (xor:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0)))
   (set (subreg:SI (match_dup 0) 1) (xor:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1)))]
  "")

(define_insn "xordi3_immed"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_uns_arith_operand" "K")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "xori\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "*norsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "d"))
		(not:SI (match_operand:SI 2 "register_operand" "d"))))]
  "!TARGET_MIPS16"
  "nor\\t%0,%z1,%z2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "*nordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (not:DI (match_operand:DI 1 "se_register_operand" "d"))
		(not:DI (match_operand:DI 2 "se_register_operand" "d"))))]
  "!TARGET_MIPS16"
  "*
{
  if (TARGET_64BIT)
    return \"nor\\t%0,%z1,%z2\";
  return \"nor\\t%M0,%M1,%M2\;nor\\t%L0,%L1,%L2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set (attr "length")
	(if_then_else (ne (symbol_ref "TARGET_64BIT") (const_int 0))
		       (const_int 4)
		       (const_int 8)))])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" ""))
		(not:DI (match_operand:DI 2 "register_operand" ""))))]
  "reload_completed && !TARGET_MIPS16 && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (and:SI (not:SI (subreg:SI (match_dup 1) 0)) (not:SI (subreg:SI (match_dup 2) 0))))
   (set (subreg:SI (match_dup 0) 1) (and:SI (not:SI (subreg:SI (match_dup 1) 1)) (not:SI (subreg:SI (match_dup 2) 1))))]
  "")

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
  "cvt.s.d\\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")])

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI (match_operand:DI 1 "se_register_operand" "d")))]
  "TARGET_64BIT"
  "*
{
  if (TARGET_MIPS16)
    return \"dsll\\t%0,%1,32\;dsra\\t%0,32\";
  return \"dsll\\t%0,%1,32\;dsra\\t%0,%0,32\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")
   (set (attr "length")	(if_then_else (eq (symbol_ref "mips16") (const_int 0))
				      (const_int 8)
				      (const_int 16)))])

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(truncate:HI (match_operand:DI 1 "se_register_operand" "d")))]
  "TARGET_64BIT"
  "*
{
  if (TARGET_MIPS16)
    return \"dsll\\t%0,%1,48\;dsra\\t%0,48\";
  return \"andi\\t%0,%1,0xffff\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"HI")
   (set (attr "length")	(if_then_else (eq (symbol_ref "mips16") (const_int 0))
				      (const_int 4)
				      (const_int 16)))])
(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(truncate:QI (match_operand:DI 1 "se_register_operand" "d")))]
  "TARGET_64BIT"
  "*
{
  if (TARGET_MIPS16)
    return \"dsll\\t%0,%1,56\;dsra\\t%0,56\";
  return \"andi\\t%0,%1,0x00ff\"; 
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"QI")
   (set (attr "length")	(if_then_else (eq (symbol_ref "mips16") (const_int 0))
				      (const_int 4)
				      (const_int 16)))])

;; Combiner patterns to optimize shift/truncate combinations.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI (ashiftrt:DI (match_operand:DI 1 "se_register_operand" "d")
				  (match_operand:DI 2 "small_int" "I"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 - shift_amt);
      return \"dsll\\t%0,%1,%2\;dsra\\t%0,%0,32\";
    }
  else
    {
      operands[2] = GEN_INT (shift_amt);
      return \"dsra\\t%0,%1,%2\";
    }
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])
	
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI (lshiftrt:DI (match_operand:DI 1 "se_register_operand" "d")
				  (match_operand:DI 2 "small_int" "I"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 - shift_amt);
      return \"dsll\\t%0,%1,%2\;dsra\\t%0,%0,32\";
    }
  else if (shift_amt == 32)
    return \"dsra\\t%0,%1,32\";
  else
    {
      operands[2] = GEN_INT (shift_amt);
      return \"dsrl\\t%0,%1,%2\";
    }
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI (ashift:DI (match_operand:DI 1 "se_register_operand" "d")
				(match_operand:DI 2 "small_int" "I"))))]
  "TARGET_64BIT"
  "*
{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 + shift_amt);
      if (TARGET_MIPS16)
	return \"dsll\\t%0,%1,%2\;dsra\\t%0,32\";
      return \"dsll\\t%0,%1,%2\;dsra\\t%0,%0,32\";
    }
  else
    return \"move\\t%0,%.\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

;; Combiner patterns to optimize truncate/zero_extend combinations.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(zero_extend:SI (truncate:HI
			 (match_operand:DI 1 "se_register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\\t%0,%1,0xffff"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(zero_extend:SI (truncate:QI
			 (match_operand:DI 1 "se_register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\\t%0,%1,0xff"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d")
	(zero_extend:HI (truncate:QI
			 (match_operand:DI 1 "se_register_operand" "d"))))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "andi\\t%0,%1,0xff"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"HI")])

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "
{
  if ((optimize || TARGET_MIPS16) && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (DImode, operands[1]);
      rtx temp  = gen_reg_rtx (DImode);
      rtx shift = GEN_INT (32);

      emit_insn (gen_ashldi3 (temp, op1, shift));
      emit_insn (gen_lshrdi3 (operands[0], temp, shift));
      DONE;
    }
}")

(define_insn "zero_extendsidi2_internal"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(zero_extend:DI (match_operand:SI 1 "memory_operand" "R,m")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  ""
  "
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (SImode, operands[1]);
      rtx temp = force_reg (SImode, GEN_INT (0xffff));

      emit_insn (gen_andsi3 (operands[0], op, temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,R,m")))]
  "!TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0xffff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,4,8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(zero_extend:SI (match_operand:HI 1 "memory_operand" "R,m")))]
  "TARGET_MIPS16"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])

(define_expand "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (DImode, operands[1]);
      rtx temp = force_reg (DImode, GEN_INT (0xffff));

      emit_insn (gen_anddi3 (operands[0], op, temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(zero_extend:DI (match_operand:HI 1 "nonimmediate_operand" "d,R,m")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0xffff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,4,8")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(zero_extend:DI (match_operand:HI 1 "memory_operand" "R,m")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
  "
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op0 = gen_lowpart (SImode, operands[0]);
      rtx op1 = gen_lowpart (SImode, operands[1]);
      rtx temp = force_reg (SImode, GEN_INT (0xff));

      emit_insn (gen_andsi3 (op0, op1, temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d,d,d")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  "!TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0x00ff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"HI")
   (set_attr "length"	"4,4,8")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d,d")
	(zero_extend:HI (match_operand:QI 1 "memory_operand" "R,m")))]
  "TARGET_MIPS16"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"HI")
   (set_attr "length"	"4,8")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
  "
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (SImode, operands[1]);
      rtx temp = force_reg (SImode, GEN_INT (0xff));

      emit_insn (gen_andsi3 (operands[0], op, temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  "!TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0x00ff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,4,8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(zero_extend:SI (match_operand:QI 1 "memory_operand" "R,m")))]
  "TARGET_MIPS16"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])

(define_expand "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "
{
  if (TARGET_MIPS16 && GET_CODE (operands[1]) != MEM)
    {
      rtx op = gen_lowpart (DImode, operands[1]);
      rtx temp = force_reg (DImode, GEN_INT (0xff));

      emit_insn (gen_anddi3 (operands[0], op, temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d,d")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0x00ff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,4,8")])

;; These can be created when a paradoxical subreg operand with an implicit
;; sign_extend operator is reloaded.  Because of the subreg, this is really
;; a zero extend.
;; ??? It might be possible to eliminate the need for these patterns by adding
;; more support to reload for implicit sign_extend operators.
(define_insn "*paradoxical_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(sign_extend:DI
	 (subreg:SI (match_operand:HI 1 "memory_operand" "R,m") 0)))]
  "TARGET_64BIT"
  "*
{
  return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(zero_extend:DI (match_operand:QI 1 "memory_operand" "R,m")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

;; In 64 bit mode, 32 bit values in general registers are always
;; correctly sign extended.  That means that if the target is a
;; general register, we can sign extend from SImode to DImode just by
;; doing a move.

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,y,d,*d,d,d")
	(sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "d,d,y,*x,R,m")))]
  "TARGET_64BIT"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,move,move,hilo,load,load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,4,4,4,4,8")])

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the lh and lb insns.

(define_expand "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (DImode, operands[1]);
      rtx temp  = gen_reg_rtx (DImode);
      rtx shift = GEN_INT (48);

      emit_insn (gen_ashldi3 (temp, op1, shift));
      emit_insn (gen_ashrdi3 (operands[0], temp, shift));
      DONE;
    }
}")

(define_insn "extendhidi2_internal"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(sign_extend:DI (match_operand:HI 1 "memory_operand" "R,m")))]
  "TARGET_64BIT"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "")))]
  ""
  "
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (SImode, operands[1]);
      rtx temp  = gen_reg_rtx (SImode);
      rtx shift = GEN_INT (16);

      emit_insn (gen_ashlsi3 (temp, op1, shift));
      emit_insn (gen_ashrsi3 (operands[0], temp, shift));
      DONE;
    }
}")

(define_insn "extendhisi2_internal"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "R,m")))]
  ""
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
  "
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op0   = gen_lowpart (SImode, operands[0]);
      rtx op1   = gen_lowpart (SImode, operands[1]);
      rtx temp  = gen_reg_rtx (SImode);
      rtx shift = GEN_INT (24);

      emit_insn (gen_ashlsi3 (temp, op1, shift));
      emit_insn (gen_ashrsi3 (op0, temp, shift));
      DONE;
    }
}")

(define_insn "extendqihi2_internal"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "R,m")))]
  ""
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])


(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  ""
  "
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (SImode, operands[1]);
      rtx temp  = gen_reg_rtx (SImode);
      rtx shift = GEN_INT (24);

      emit_insn (gen_ashlsi3 (temp, op1, shift));
      emit_insn (gen_ashrsi3 (operands[0], temp, shift));
      DONE;
    }
}")

(define_insn "extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "R,m")))]
  ""
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])

(define_expand "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_64BIT"
  "
{
  if (optimize && GET_CODE (operands[1]) == MEM)
    operands[1] = force_not_mem (operands[1]);

  if (GET_CODE (operands[1]) != MEM)
    {
      rtx op1   = gen_lowpart (DImode, operands[1]);
      rtx temp  = gen_reg_rtx (DImode);
      rtx shift = GEN_INT (56);

      emit_insn (gen_ashldi3 (temp, op1, shift));
      emit_insn (gen_ashrdi3 (operands[0], temp, shift));
      DONE;
    }
}")

(define_insn "extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(sign_extend:DI (match_operand:QI 1 "memory_operand" "R,m")))]
  "TARGET_64BIT"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])


(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "cvt.d.s\\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")])



;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

;; The SImode scratch register can not be shared with address regs used for
;; operand zero, because then the address in the move instruction will be
;; clobbered.  We mark the scratch register as early clobbered to prevent this.

;; We need the ?X in alternative 1 so that it will be choosen only if the
;; destination is a floating point register.  Otherwise, alternative 1 can
;; have lower cost than alternative 0 (because there is one less loser), and
;; can be choosen when it won't work (because integral reloads into FP
;; registers are not supported).

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,*f,R,To")
	(fix:SI (match_operand:DF 1 "register_operand" "f,*f,f,f")))
   (clobber (match_scratch:SI 2 "=d,*d,&d,&d"))
   (clobber (match_scratch:DF 3 "=f,?*X,f,f"))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  rtx xoperands[10];

  if (which_alternative == 1)
    return \"trunc.w.d %0,%1,%2\";

  output_asm_insn (\"trunc.w.d %3,%1,%2\", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[3];
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return \"\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"44,36,40,44")])


(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,*f,R,To")
	(fix:SI (match_operand:SF 1 "register_operand" "f,*f,f,f")))
   (clobber (match_scratch:SI 2 "=d,*d,&d,&d"))
   (clobber (match_scratch:SF 3 "=f,?*X,f,f"))]
  "TARGET_HARD_FLOAT"
  "*
{
  rtx xoperands[10];

  if (which_alternative == 1)
    return \"trunc.w.s %0,%1,%2\";

  output_asm_insn (\"trunc.w.s %3,%1,%2\", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[3];
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return \"\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"44,36,40,44")])


;;; ??? trunc.l.d is mentioned in the appendix of the 1993 r4000/r4600 manuals
;;; but not in the chapter that describes the FPU.  It is not mentioned at all
;;; in the 1991 manuals.  The r4000 at Cygnus does not have this instruction.

;;; Deleting this means that we now need two libgcc2.a libraries.  One for
;;; the 32 bit calling convention and one for the 64 bit calling convention.

;;; If this is disabled, then fixuns_truncdfdi2 must be disabled also.

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,*f,R,To")
	(fix:DI (match_operand:DF 1 "register_operand" "f,*f,f,f")))
   (clobber (match_scratch:DF 2 "=f,?*X,f,f"))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
  "*
{
  rtx xoperands[10];

  if (which_alternative == 1)
    return \"trunc.l.d %0,%1\";

  output_asm_insn (\"trunc.l.d %2,%1\", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[2];
  output_asm_insn (mips_move_2words (xoperands, insn), xoperands);
  return \"\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8,4,8,12")])


;;; ??? trunc.l.s is mentioned in the appendix of the 1993 r4000/r4600 manuals
;;; but not in the chapter that describes the FPU.  It is not mentioned at all
;;; in the 1991 manuals.  The r4000 at Cygnus does not have this instruction.
(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,*f,R,To")
	(fix:DI (match_operand:SF 1 "register_operand" "f,*f,f,f")))
   (clobber (match_scratch:DF 2 "=f,?*X,f,f"))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
  "*
{
  rtx xoperands[10];

  if (which_alternative == 1)
    return \"trunc.l.s %0,%1\";

  output_asm_insn (\"trunc.l.s %2,%1\", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[2];
  output_asm_insn (mips_move_2words (xoperands, insn), xoperands);
  return \"\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"8,4,8,12")])


(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f,f")
	(float:DF (match_operand:SI 1 "nonimmediate_operand" "d,R,m")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return \"l.s\\t%0,%1%#\;cvt.d.w\\t%0,%0\";

  return \"mtc1\\t%1,%0%#\;cvt.d.w\\t%0,%0\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"12,16,12")])


(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f,f")
	(float:DF (match_operand:DI 1 "se_nonimmediate_operand" "d,R,m")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
  "*
{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return \"l.d\\t%0,%1%#\;cvt.d.l\\t%0,%0\";

  return \"dmtc1\\t%1,%0%#\;cvt.d.l\\t%0,%0\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"12,16,12")])


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f,f,f")
	(float:SF (match_operand:SI 1 "nonimmediate_operand" "d,R,m")))]
  "TARGET_HARD_FLOAT"
  "*
{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return \"l.s\\t%0,%1%#\;cvt.s.w\\t%0,%0\";

  return \"mtc1\\t%1,%0%#\;cvt.s.w\\t%0,%0\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"12,16,12")])


(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f,f,f")
	(float:SF (match_operand:DI 1 "se_nonimmediate_operand" "d,R,m")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
  "*
{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return \"l.d\\t%0,%1%#\;cvt.s.l\\t%0,%0\";

  return \"dmtc1\\t%1,%0%#\;cvt.s.l\\t%0,%0\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"12,16,12")])


(define_expand "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (1.0, 31);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, immed_real_const_1 (offset, DFmode));
      do_pending_stack_adjust ();

      emit_insn (gen_cmpdf (operands[1], reg1));
      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncdfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
      emit_move_insn (reg3, GEN_INT (0x80000000));

      emit_insn (gen_fix_truncdfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")


(define_expand "fixuns_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unsigned_fix:DI (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
  "
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (DImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (1.0, 63);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, immed_real_const_1 (offset, DFmode));
      do_pending_stack_adjust ();

      emit_insn (gen_cmpdf (operands[1], reg1));
      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncdfdi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx_MINUS (DFmode, operands[1], reg1));
      emit_move_insn (reg3, GEN_INT (0x80000000));
      emit_insn (gen_ashldi3 (reg3, reg3, GEN_INT (32)));

      emit_insn (gen_fix_truncdfdi2 (operands[0], reg2));
      emit_insn (gen_iordi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")


(define_expand "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT"
  "
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (1.0, 31);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, immed_real_const_1 (offset, SFmode));
      do_pending_stack_adjust ();

      emit_insn (gen_cmpsf (operands[1], reg1));
      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
      emit_move_insn (reg3, GEN_INT (0x80000000));

      emit_insn (gen_fix_truncsfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")


(define_expand "fixuns_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(unsigned_fix:DI (match_operand:SF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT"
  "
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (DImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (1.0, 63);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, immed_real_const_1 (offset, SFmode));
      do_pending_stack_adjust ();

      emit_insn (gen_cmpsf (operands[1], reg1));
      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncsfdi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx_MINUS (SFmode, operands[1], reg1));
      emit_move_insn (reg3, GEN_INT (0x80000000));
      emit_insn (gen_ashldi3 (reg3, reg3, GEN_INT (32)));

      emit_insn (gen_fix_truncsfdi2 (operands[0], reg2));
      emit_insn (gen_iordi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")


;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; Bit field extract patterns which use lwl/lwr.

;; ??? There could be HImode variants for the ulh/ulhu/ush macros.
;; It isn't clear whether this will give better code.

;; Only specify the mode operand 1, the rest are assumed to be word_mode.
(define_expand "extv"
  [(set (match_operand 0 "register_operand" "")
	(sign_extract (match_operand:QI 1 "memory_operand" "")
		      (match_operand 2 "immediate_operand" "")
		      (match_operand 3 "immediate_operand" "")))]
  "!TARGET_MIPS16"
  "
{
  /* If the field does not start on a byte boundary, then fail.  */
  if (INTVAL (operands[3]) % 8 != 0) 
    FAIL;

  /* MIPS I and MIPS II can only handle a 32bit field.  */
  if (!TARGET_64BIT && INTVAL (operands[2]) != 32)
    FAIL;

  /* MIPS III and MIPS IV can handle both 32bit and 64bit fields.  */
  if (TARGET_64BIT
      && INTVAL (operands[2]) != 64
      && INTVAL (operands[2]) != 32)
    FAIL;

  /* This can happen for a 64 bit target, when extracting a value from
     a 64 bit union member.  extract_bit_field doesn't verify that our
     source matches the predicate, so we force it to be a MEM here.  */
  if (GET_CODE (operands[1]) != MEM)
    FAIL;

  /* Change the mode to BLKmode for aliasing purposes.  */
  operands[1] = change_address (operands[1], BLKmode, XEXP (operands[1], 0));

  /* Otherwise, emit a l[wd]l/l[wd]r pair to load the value.  */
  if (INTVAL (operands[2]) == 64)
    emit_insn (gen_movdi_uld (operands[0], operands[1]));
  else
    {
      if (TARGET_64BIT)
	{
	  operands[0] = gen_lowpart (SImode, operands[0]);
	  if (operands[0] == NULL_RTX)
	    FAIL;
	}
      emit_insn (gen_movsi_ulw (operands[0], operands[1]));
    }
  DONE;
}")

;; Only specify the mode operand 1, the rest are assumed to be word_mode.
(define_expand "extzv"
  [(set (match_operand 0 "register_operand" "")
	(zero_extract (match_operand:QI 1 "memory_operand" "")
		      (match_operand 2 "immediate_operand" "")
		      (match_operand 3 "immediate_operand" "")))]
  "!TARGET_MIPS16"
  "
{
  /* If the field does not start on a byte boundary, then fail.  */
  if (INTVAL (operands[3]) % 8 != 0) 
    FAIL;

  /* MIPS I and MIPS II can only handle a 32bit field.  */
  if (!TARGET_64BIT && INTVAL (operands[2]) != 32)
    FAIL;

  /* MIPS III and MIPS IV can handle both 32bit and 64bit fields.  */
  if (TARGET_64BIT
      && INTVAL (operands[2]) != 64
      && INTVAL (operands[2]) != 32)
    FAIL;

  /* This can happen for a 64 bit target, when extracting a value from
     a 64 bit union member.  extract_bit_field doesn't verify that our
     source matches the predicate, so we force it to be a MEM here.  */
  if (GET_CODE (operands[1]) != MEM)
    FAIL;

  /* Change the mode to BLKmode for aliasing purposes.  */
  operands[1] = change_address (operands[1], BLKmode, XEXP (operands[1], 0));

  /* Otherwise, emit a lwl/lwr pair to load the value.  */
  if (INTVAL (operands[2]) == 64)
    emit_insn (gen_movdi_uld (operands[0], operands[1]));
  else
    {
      if (TARGET_64BIT)
	{
	  operands[0] = gen_lowpart (SImode, operands[0]);
	  if (operands[0] == NULL_RTX)
	    FAIL;
	}
      emit_insn (gen_movsi_ulw (operands[0], operands[1]));
    }
  DONE;
}")

;; Only specify the mode operands 0, the rest are assumed to be word_mode.
(define_expand "insv"
  [(set (zero_extract (match_operand:QI 0 "memory_operand" "")
		      (match_operand 1 "immediate_operand" "")
		      (match_operand 2 "immediate_operand" ""))
	(match_operand 3 "register_operand" ""))]
  "!TARGET_MIPS16"
  "
{
  /* If the field does not start on a byte boundary, then fail.  */
  if (INTVAL (operands[2]) % 8 != 0) 
    FAIL;

  /* MIPS I and MIPS II can only handle a 32bit field.  */
  if (!TARGET_64BIT && INTVAL (operands[1]) != 32)
    FAIL;

  /* MIPS III and MIPS IV can handle both 32bit and 64bit fields.  */
  if (TARGET_64BIT
      && INTVAL (operands[1]) != 64
      && INTVAL (operands[1]) != 32)
    FAIL;

  /* This can happen for a 64 bit target, when storing into a 32 bit union
     member.  store_bit_field doesn't verify that our target matches the
     predicate, so we force it to be a MEM here.  */
  if (GET_CODE (operands[0]) != MEM)
    FAIL;

  /* Change the mode to BLKmode for aliasing purposes.  */
  operands[0] = change_address (operands[0], BLKmode, XEXP (operands[0], 0));

  /* Otherwise, emit a s[wd]l/s[wd]r pair to load the value.  */
  if (INTVAL (operands[1]) == 64)
    emit_insn (gen_movdi_usd (operands[0], operands[3]));
  else
    {
      if (TARGET_64BIT)
	{
	  operands[3] = gen_lowpart (SImode, operands[3]);
	  if (operands[3] == NULL_RTX)
	    FAIL;
	}
      emit_insn (gen_movsi_usw (operands[0], operands[3]));
    }
  DONE;
}")

;; unaligned word moves generated by the bit field patterns

(define_insn "movsi_ulw"
  [(set (match_operand:SI 0 "register_operand" "=&d,&d")
	(unspec:SI [(match_operand:BLK 1 "general_operand" "R,o")] 0))]
  "!TARGET_MIPS16"
  "*
{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[1], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);
  const char *ret;

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 3) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = \"lw\\t%0,%1\";
  else
    ret = \"ulw\\t%0,%1\";

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8,16")])

(define_insn "movsi_usw"
  [(set (match_operand:BLK 0 "memory_operand" "=R,o")
	(unspec:BLK [(match_operand:SI 1 "reg_or_0_operand" "dJ,dJ")] 1))]
  "!TARGET_MIPS16"
  "*
{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[0], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[0], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster sw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 3) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return \"sw\\t%z1,%0\";

  return \"usw\\t%z1,%0\";
}"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8,16")])

;; Bit field extract patterns which use ldl/ldr.

;; unaligned double word moves generated by the bit field patterns

(define_insn "movdi_uld"
  [(set (match_operand:DI 0 "register_operand" "=&d,&d")
	(unspec:DI [(match_operand:BLK 1 "general_operand" "R,o")] 0))]
  ""
  "*
{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[1], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);
  const char *ret;

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 7) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = \"ld\\t%0,%1\";
  else
    ret = \"uld\\t%0,%1\";

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}"
  [(set_attr "type"	"load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8,16")])

(define_insn "movdi_usd"
  [(set (match_operand:BLK 0 "memory_operand" "=R,o")
	(unspec:BLK [(match_operand:DI 1 "reg_or_0_operand" "dJ,dJ")] 1))]
  ""
  "*
{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[0], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[0], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster sw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 7) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return \"sd\\t%1,%0\";

  return \"usd\\t%z1,%0\";
}"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8,16")])

;; These two patterns support loading addresses with two instructions instead
;; of using the macro instruction la.

;; ??? mips_move_1word has support for HIGH, so this pattern may be
;; unnecessary.

(define_insn "high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "")))]
  "mips_split_addresses && !TARGET_MIPS16"
  "lui\\t%0,%%hi(%1) # high"
  [(set_attr "type"	"move")])

(define_insn "low"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "")))]
  "mips_split_addresses && !TARGET_MIPS16"
  "addiu\\t%0,%1,%%lo(%2) # low"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

;; 64-bit integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (mips_split_addresses && mips_check_split (operands[1], DImode))
    {
      enum machine_mode mode = GET_MODE (operands[0]);
      rtx tem = ((reload_in_progress | reload_completed)
		 ? operands[0] : gen_reg_rtx (mode));

      emit_insn (gen_rtx_SET (VOIDmode, tem,
			      gen_rtx_HIGH (mode, operands[1])));

      operands[1] = gen_rtx_LO_SUM (mode, tem, operands[1]);
    }

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
      temp = gen_rtx_PLUS (Pmode, embedded_pic_fnaddr_rtx,
			   force_reg (DImode, temp));
      emit_move_insn (operands[0], force_reg (DImode, temp));
      DONE;
    }

  /* If operands[1] is a constant address illegal for pic, then we need to
     handle it just like LEGITIMIZE_ADDRESS does.  */
  if (flag_pic && pic_address_needs_scratch (operands[1]))
    {
      rtx temp = force_reg (DImode, XEXP (XEXP (operands[1], 0), 0));
      rtx temp2 = XEXP (XEXP (operands[1], 0), 1);

      if (! SMALL_INT (temp2))
	temp2 = force_reg (DImode, temp2);

      emit_move_insn (operands[0], gen_rtx_PLUS (DImode, temp, temp2));
      DONE;
    }

  /* On the mips16, we can handle a GP relative reference by adding in
     $gp.  We need to check the name to see whether this is a string
     constant.  */
  if (TARGET_MIPS16
      && register_operand (operands[0], DImode)
      && GET_CODE (operands[1]) == SYMBOL_REF
      && SYMBOL_REF_FLAG (operands[1]))
    {
      char *name = XSTR (operands[1], 0);

      if (name[0] != '*'
	  || strncmp (name + 1, LOCAL_LABEL_PREFIX,
		      sizeof LOCAL_LABEL_PREFIX - 1) != 0)
	{
	  rtx base_reg;

	  if (reload_in_progress || reload_completed)
	    {
	      /* In movsi we use the constant table here.  However, in
                 this case, we're better off copying $28 into a
                 register and adding, because the constant table entry
                 would be 8 bytes.  */
	      base_reg = operands[0];
	      emit_move_insn (base_reg,
			      gen_rtx (CONST, DImode,
				       gen_rtx (REG, DImode,
						GP_REG_FIRST + 28)));
	    }
	  else
	    {
	      base_reg = gen_reg_rtx (Pmode);
	      emit_move_insn (base_reg, mips16_gp_pseudo_reg ());
	    }

	  emit_move_insn (operands[0],
			  gen_rtx (PLUS, Pmode, base_reg,
				   mips16_gp_offset (operands[1])));
	  DONE;
	}
    }

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], DImode)
      && !register_operand (operands[1], DImode)
      && (TARGET_MIPS16
	  || ((GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
	       && operands[1] != CONST0_RTX (DImode))))
    {
      rtx temp = force_reg (DImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

;; For mips16, we need a special case to handle storing $31 into
;; memory, since we don't have a constraint to match $31.  This
;; instruction can be generated by save_restore_insns.

(define_insn ""
  [(set (match_operand:DI 0 "memory_operand" "=R,m")
	(reg:DI 31))]
  "TARGET_MIPS16 && TARGET_64BIT"
  "*
{
  operands[1] = gen_rtx (REG, DImode, 31);
  return mips_move_2words (operands, insn);
}"
  [(set_attr "type"	"store")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8")])

(define_insn "movdi_internal"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,d,R,o,*x,*d,*x")
	(match_operand:DI 1 "general_operand" "d,iF,R,o,d,d,J,*x,*d"))]
  "!TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,arith,load,load,store,store,hilo,hilo,hilo")
   (set_attr "mode"	"DI")
   (set_attr "length"   "8,16,8,16,8,16,8,8,8")])

(define_insn ""
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,R,To,*d")
	(match_operand:DI 1 "general_operand" "d,d,y,K,N,R,To,d,d,*x"))]
  "!TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "* return mips_move_2words (operands, insn);"
  [(set_attr "type"	"move,move,move,arith,arith,load,load,store,store,hilo")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8,8,8,8,12,8,16,8,16,8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "register_operand" ""))]
  "reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"

  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 1) (subreg:SI (match_dup 1) 1))]
  "")

(define_insn "movdi_internal2"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,d,d,d,R,m,*x,*d,*x,*a")
	(match_operand:DI 1 "movdi_operand" "d,S,IKL,Mnis,R,m,dJ,dJ,J,*x,*d,*J"))]
  "TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || se_register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,load,arith,arith,load,load,store,store,hilo,hilo,hilo,hilo")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4,8,4,8,4,8,4,8,4,4,4,8")])

(define_insn ""
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,d,R,m,*d")
	(match_operand:DI 1 "movdi_operand" "d,d,y,K,N,s,R,m,d,d,*x"))]
  "TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || se_register_operand (operands[1], DImode))"
  "* return mips_move_2words (operands, insn);"
  [(set_attr "type"	"move,move,move,arith,arith,arith,load,load,store,store,hilo")
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
		 (if_then_else (match_operand:VOID 1 "m16_usym5_4" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)])])

;; On the mips16, we can split ld $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(mem:DI (plus:DI (match_dup 0)
			 (match_operand:DI 1 "const_int_operand" ""))))]
  "TARGET_64BIT && TARGET_MIPS16 && reload_completed
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
  "
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
}")

;; Handle input reloads in DImode.
;; This is mainly to handle reloading HILO_REGNUM.  Note that we may
;; see it as the source or the destination, depending upon which way
;; reload handles the instruction.
;; Making the second operand TImode is a trick.  The compiler may
;; reuse the same register for operand 0 and operand 2.  Using TImode
;; gives us two registers, so we can always use the one which is not
;; used.

(define_expand "reload_indi"
  [(set (match_operand:DI 0 "register_operand" "=b")
	(match_operand:DI 1 "" "b"))
   (clobber (match_operand:TI 2 "register_operand" "=&d"))]
  "TARGET_64BIT"
  "
{
  rtx scratch = gen_rtx_REG (DImode,
			     (REGNO (operands[0]) == REGNO (operands[2]) 
			      ? REGNO (operands[2]) + 1
			      : REGNO (operands[2])));

  if (GET_CODE (operands[0]) == REG && REGNO (operands[0]) == HILO_REGNUM)
    {
      if (GET_CODE (operands[1]) == MEM)
	{
	  rtx memword, offword, hiword, loword;
	  rtx addr = find_replacement (&XEXP (operands[1], 0));
	  rtx op1 = change_address (operands[1], VOIDmode, addr);

	  scratch = gen_rtx_REG (SImode, REGNO (scratch));
	  memword = change_address (op1, SImode, NULL_RTX);
	  offword = change_address (adj_offsettable_operand (op1, 4),
				    SImode, NULL_RTX);
	  if (BYTES_BIG_ENDIAN)
	    {
	      hiword = memword;
	      loword = offword;
	    }
	  else
	    {
	      hiword = offword;
	      loword = memword;
	    }
	  emit_move_insn (scratch, hiword);
	  emit_move_insn (gen_rtx_REG (SImode, 64), scratch);
	  emit_move_insn (scratch, loword);
	  emit_move_insn (gen_rtx (REG, SImode, 65), scratch);
          emit_insn (gen_rtx_USE (VOIDmode, operands[0]));
	}
      else
	{
	  emit_insn (gen_ashrdi3 (scratch, operands[1], GEN_INT (32)));
	  emit_insn (gen_movdi (gen_rtx_REG (DImode, 64), scratch));
	  emit_insn (gen_ashldi3 (scratch, operands[1], GEN_INT (32)));
	  emit_insn (gen_ashrdi3 (scratch, scratch, GEN_INT (32)));
	  emit_insn (gen_movdi (gen_rtx (REG, DImode, 65), scratch));
          emit_insn (gen_rtx_USE (VOIDmode, operands[0]));
	}
      DONE;
    }
  if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) == HILO_REGNUM)
    {
      emit_insn (gen_movdi (scratch, gen_rtx_REG (DImode, 65)));
      emit_insn (gen_ashldi3 (scratch, scratch, GEN_INT (32)));
      emit_insn (gen_lshrdi3 (scratch, scratch, GEN_INT (32)));
      emit_insn (gen_movdi (operands[0], gen_rtx_REG (DImode, 64)));
      emit_insn (gen_ashldi3 (operands[0], operands[0], GEN_INT (32)));
      emit_insn (gen_iordi3 (operands[0], operands[0], scratch));
      emit_insn (gen_rtx_USE (VOIDmode, operands[1]));
      DONE;
    }
  /* This handles moves between a float register and HI/LO.  */
  emit_move_insn (scratch, operands[1]);
  emit_move_insn (operands[0], scratch);
  DONE;
}")

;; Handle output reloads in DImode.

;; Reloading HILO_REG in MIPS16 mode requires two scratch registers, so we
;; use a TImode scratch reg.

(define_expand "reload_outdi"
  [(set (match_operand:DI 0 "" "=b")
	(match_operand:DI 1 "se_register_operand" "b"))
   (clobber (match_operand:TI 2 "register_operand" "=&d"))]
  "TARGET_64BIT"
  "
{
  rtx scratch = gen_rtx_REG (DImode, REGNO (operands[2]));

  if (GET_CODE (operands[0]) == REG && REGNO (operands[0]) == HILO_REGNUM)
    {
      emit_insn (gen_ashrdi3 (scratch, operands[1], GEN_INT (32)));
      emit_insn (gen_movdi (gen_rtx (REG, DImode, 64), scratch));
      emit_insn (gen_ashldi3 (scratch, operands[1], GEN_INT (32)));
      emit_insn (gen_ashrdi3 (scratch, scratch, GEN_INT (32)));
      emit_insn (gen_movdi (gen_rtx (REG, DImode, 65), scratch));
      emit_insn (gen_rtx_USE (VOIDmode, operands[0]));
      DONE;
    }
  if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) == HILO_REGNUM)
    {
      if (GET_CODE (operands[0]) == MEM)
	{
	  rtx scratch, memword, offword, hiword, loword;
	  rtx addr = find_replacement (&XEXP (operands[0], 0));
	  rtx op0 = change_address (operands[0], VOIDmode, addr);

	  scratch = gen_rtx_REG (SImode, REGNO (operands[2]));
	  memword = change_address (op0, SImode, NULL_RTX);
	  offword = change_address (adj_offsettable_operand (op0, 4),
				    SImode, NULL_RTX);
	  if (BYTES_BIG_ENDIAN)
	    {
	      hiword = memword;
	      loword = offword;
	    }
	  else
	    {
	      hiword = offword;
	      loword = memword;
	    }
	  emit_move_insn (scratch, gen_rtx_REG (SImode, 64));
	  emit_move_insn (hiword, scratch);
	  emit_move_insn (scratch, gen_rtx_REG (SImode, 65));
	  emit_move_insn (loword, scratch);
	  emit_insn (gen_rtx_USE (VOIDmode, operands[1]));
	}
      else if (TARGET_MIPS16 && ! M16_REG_P (REGNO (operands[0])))
	{
	  /* Handle the case where operand[0] is not a 'd' register,
	     and hence we can not directly move from the HILO register
	     into it.  */
	  rtx scratch2 = gen_rtx_REG (DImode, REGNO (operands[2]) + 1);
	  emit_insn (gen_movdi (scratch, gen_rtx (REG, DImode, 65)));
	  emit_insn (gen_ashldi3 (scratch, scratch, GEN_INT (32)));
	  emit_insn (gen_lshrdi3 (scratch, scratch, GEN_INT (32)));
	  emit_insn (gen_movdi (scratch2, gen_rtx (REG, DImode, 64)));
	  emit_insn (gen_ashldi3 (scratch2, scratch2, GEN_INT (32)));
	  emit_insn (gen_iordi3 (scratch, scratch, scratch2));
	  emit_insn (gen_movdi (operands[0], scratch));
	  emit_insn (gen_rtx_USE (VOIDmode, operands[1]));
	}
      else
	{
	  emit_insn (gen_movdi (scratch, gen_rtx (REG, DImode, 65)));
	  emit_insn (gen_ashldi3 (scratch, scratch, GEN_INT (32)));
	  emit_insn (gen_lshrdi3 (scratch, scratch, GEN_INT (32)));
	  emit_insn (gen_movdi (operands[0], gen_rtx (REG, DImode, 64)));
	  emit_insn (gen_ashldi3 (operands[0], operands[0], GEN_INT (32)));
	  emit_insn (gen_iordi3 (operands[0], operands[0], scratch));
	  emit_insn (gen_rtx_USE (VOIDmode, operands[1]));
	}
      DONE;
    }
  /* This handles moves between a float register and HI/LO.  */
  emit_move_insn (scratch, operands[1]);
  emit_move_insn (operands[0], scratch);
  DONE;
}")

;; 32-bit Integer moves

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "large_int" ""))]
  "!TARGET_DEBUG_D_MODE && !TARGET_MIPS16"
  [(set (match_dup 0)
	(match_dup 2))
   (set (match_dup 0)
     	(ior:SI (match_dup 0)
		(match_dup 3)))]
  "
{
  operands[2] = GEN_INT (INTVAL (operands[1]) & 0xffff0000);
  operands[3] = GEN_INT (INTVAL (operands[1]) & 0x0000ffff);
}")

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (mips_split_addresses && mips_check_split (operands[1], SImode))
    {
      enum machine_mode mode = GET_MODE (operands[0]);
      rtx tem = ((reload_in_progress | reload_completed)
		 ? operands[0] : gen_reg_rtx (mode));

      emit_insn (gen_rtx_SET (VOIDmode, tem,
			      gen_rtx_HIGH (mode, operands[1])));

      operands[1] = gen_rtx_LO_SUM (mode, tem, operands[1]);
    }

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
      temp = gen_rtx_PLUS (Pmode, embedded_pic_fnaddr_rtx,
			   force_reg (SImode, temp));
      emit_move_insn (operands[0], force_reg (SImode, temp));
      DONE;
    }

  /* If operands[1] is a constant address invalid for pic, then we need to
     handle it just like LEGITIMIZE_ADDRESS does.  */
  if (flag_pic && pic_address_needs_scratch (operands[1]))
    {
      rtx temp = force_reg (SImode, XEXP (XEXP (operands[1], 0), 0));
      rtx temp2 = XEXP (XEXP (operands[1], 0), 1);

      if (! SMALL_INT (temp2))
	temp2 = force_reg (SImode, temp2);

      emit_move_insn (operands[0], gen_rtx_PLUS (SImode, temp, temp2));
      DONE;
    }

  /* On the mips16, we can handle a GP relative reference by adding in
     $gp.  We need to check the name to see whether this is a string
     constant.  */
  if (TARGET_MIPS16
      && register_operand (operands[0], SImode)
      && GET_CODE (operands[1]) == SYMBOL_REF
      && SYMBOL_REF_FLAG (operands[1]))
    {
      char *name = XSTR (operands[1], 0);

      if (name[0] != '*'
	  || strncmp (name + 1, LOCAL_LABEL_PREFIX,
		      sizeof LOCAL_LABEL_PREFIX - 1) != 0)
	{
	  rtx base_reg;

	  if (reload_in_progress || reload_completed)
	    {
	      /* We need to reload this address.  In this case we
                 aren't going to have a chance to combine loading the
                 address with the load or store.  That means that we
                 can either generate a 2 byte move followed by a 4
                 byte addition, or a 2 byte load with a 4 byte entry
                 in the constant table.  Since the entry in the
                 constant table might be shared, we're better off, on
                 average, loading the address from the constant table.  */
	      emit_move_insn (operands[0],
			      force_const_mem (SImode, operands[1]));
	      DONE;
	    }

	  base_reg = gen_reg_rtx (Pmode);
	  emit_move_insn (base_reg, mips16_gp_pseudo_reg ());

	  emit_move_insn (operands[0],
			  gen_rtx (PLUS, Pmode, base_reg,
				   mips16_gp_offset (operands[1])));
	  DONE;
	}
    }

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SImode)
      && !register_operand (operands[1], SImode)
      && (TARGET_MIPS16
	  || GET_CODE (operands[1]) != CONST_INT
	  || INTVAL (operands[1]) != 0))
    {
      rtx temp = force_reg (SImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

;; For mips16, we need a special case to handle storing $31 into
;; memory, since we don't have a constraint to match $31.  This
;; instruction can be generated by save_restore_insns.

(define_insn ""
  [(set (match_operand:SI 0 "memory_operand" "=R,m")
	(reg:SI 31))]
  "TARGET_MIPS16"
  "*
{
  operands[1] = gen_rtx (REG, SImode, 31);
  return mips_move_1word (operands, insn, FALSE);
}"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8")])

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "movsi_internal1"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d,d,d,d,R,m,*d,*f*z,*f,*f,*f,*R,*m,*x,*x,*d,*d")
	(match_operand:SI 1 "move_operand" "d,S,IKL,Mnis,R,m,dJ,dJ,*f*z,*d,*f,*R,*m,*f,*f,J,*d,*x,*a"))]
  "TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,load,arith,arith,load,load,store,store,xfer,xfer,move,load,load,store,store,hilo,hilo,hilo,hilo")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8,4,8,4,8,4,8,4,4,4,4,8,4,8,4,4,4,4")])

(define_insn "movsi_internal2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d,d,d,d,R,m,*d,*z,*x,*d,*x,*d")
	(match_operand:SI 1 "move_operand" "d,S,IKL,Mnis,R,m,dJ,dJ,*z,*d,J,*x,*d,*a"))]
  "!TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,load,arith,arith,load,load,store,store,xfer,xfer,hilo,hilo,hilo,hilo")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4,8,4,8,4,8,4,8,4,4,4,4,4,4")])

;; This is the mips16 movsi instruction.  We accept a small integer as
;; the source if the destination is a GP memory reference.  This is
;; because we want the combine pass to turn adding a GP reference to a
;; register into a direct GP reference, but the combine pass will pass
;; in the source as a constant if it finds an equivalent one.  If the
;; instruction is recognized, reload will force the constant back out
;; into a register.

(define_insn ""
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,d,d,R,m,*d,*d")
	(match_operand:SI 1 "move_operand" "d,d,y,S,K,N,s,R,m,d,d,*x,*a"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[0]) == MEM
	   && GET_CODE (XEXP (operands[0], 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == CONST
	   && mips16_gp_offset_p (XEXP (XEXP (operands[0], 0), 1))
	   && GET_CODE (operands[1]) == CONST_INT
	   && (SMALL_INT (operands[1])
	       || SMALL_INT_UNSIGNED (operands[1]))))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,move,move,load,arith,arith,arith,load,load,store,store,hilo,hilo")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (const_int 4)
		 (const_int 4)
		 (const_int 8)
		 (if_then_else (match_operand:VOID 1 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))
		 (if_then_else (match_operand:VOID 1 "m16_nuimm8_1" "")
			       (const_int 8)
			       (const_int 12))
		 (if_then_else (match_operand:VOID 1 "m16_usym8_4" "")
			       (const_int 4)
			       (const_int 8))
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)
		 (const_int 4)])])

;; On the mips16, we can split lw $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(mem:SI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" ""))))]
  "TARGET_MIPS16 && reload_completed
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
  "
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
}")

;; On the mips16, we can split a load of certain constants into a load
;; and an add.  This turns a 4 byte instruction into 2 2 byte
;; instructions.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) >= 0x100
   && INTVAL (operands[1]) <= 0xff + 0x7f"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 2)))]
  "
{
  int val = INTVAL (operands[1]);

  operands[1] = GEN_INT (0xff);
  operands[2] = GEN_INT (val - 0xff);
}")

;; On the mips16, we can split a load of a negative constant into a
;; load and a neg.  That's what mips_move_1word will generate anyhow.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) < 0
   && INTVAL (operands[1]) > - 0x8000"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (neg:SI (match_dup 0)))]
  "
{
  operands[1] = GEN_INT (- INTVAL (operands[1]));
}")

;; Reload HILO_REGNUM in SI mode.  This needs a scratch register in
;; order to set the sign bit correctly in the HI register.

(define_expand "reload_outsi"
  [(set (match_operand:SI 0 "general_operand" "=b")
	(match_operand:SI 1 "register_operand" "b"))
   (clobber (match_operand:SI 2 "register_operand" "=&d"))]
  "TARGET_64BIT || TARGET_MIPS16"
  "
{
  if (TARGET_64BIT
      && GET_CODE (operands[0]) == REG && REGNO (operands[0]) == HILO_REGNUM)
    {
      emit_insn (gen_movsi (gen_rtx_REG (SImode, 65), operands[1]));
      emit_insn (gen_ashrsi3 (operands[2], operands[1], GEN_INT (31)));
      emit_insn (gen_movsi (gen_rtx (REG, SImode, 64), operands[2]));
      emit_insn (gen_rtx_USE (VOIDmode, operands[0]));
      DONE;
    }
  /* Use a mult to reload LO on mips16.  ??? This is hideous.  */
  if (TARGET_MIPS16
      && GET_CODE (operands[0]) == REG && REGNO (operands[0]) == LO_REGNUM)
    {
      emit_insn (gen_movsi (operands[2], GEN_INT (1)));
      /* This is gen_mulsi3_internal, but we need to fill in the
	 scratch registers.  */
      emit_insn (gen_rtx (PARALLEL, VOIDmode,
			  gen_rtvec (3,
				     gen_rtx (SET, VOIDmode,
					      operands[0],
					      gen_rtx (MULT, SImode,
						       operands[1],
						       operands[2])),
				     gen_rtx (CLOBBER, VOIDmode,
					      gen_rtx (REG, SImode, 64)),
				     gen_rtx (CLOBBER, VOIDmode,
					      gen_rtx (REG, SImode, 66)))));
      DONE;
    }
  /* FIXME: I don't know how to get a value into the HI register.  */
  if (GET_CODE (operands[0]) == REG
      && (TARGET_MIPS16 ? M16_REG_P (REGNO (operands[0]))
	  : GP_REG_P (REGNO (operands[0]))))
    {
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }
  /* This handles moves between a float register and HI/LO.  */
  emit_move_insn (operands[2], operands[1]);
  emit_move_insn (operands[0], operands[2]);
  DONE;
}")

;; Reload a value into HI or LO.  There is no mthi or mtlo on mips16,
;; so we use a mult.  ??? This is hideous, and we ought to figure out
;; something better.

;; We use no predicate for operand1, because it may be a PLUS, and there
;; is no convenient predicate for that.

(define_expand "reload_insi"
  [(set (match_operand:SI 0 "register_operand" "=b")
	(match_operand:SI 1 "" "b"))
   (clobber (match_operand:SI 2 "register_operand" "=&d"))]
  "TARGET_MIPS16"
  "
{
  if (TARGET_MIPS16
      && GET_CODE (operands[0]) == REG && REGNO (operands[0]) == LO_REGNUM)
    {
      emit_insn (gen_movsi (operands[2], GEN_INT (1)));
      /* This is gen_mulsi3_internal, but we need to fill in the
	 scratch registers.  */
      emit_insn (gen_rtx (PARALLEL, VOIDmode,
			  gen_rtvec (3,
				     gen_rtx (SET, VOIDmode,
					      operands[0],
					      gen_rtx (MULT, SImode,
						       operands[1],
						       operands[2])),
				     gen_rtx (CLOBBER, VOIDmode,
					      gen_rtx (REG, SImode, 64)),
				     gen_rtx (CLOBBER, VOIDmode,
					      gen_rtx (REG, SImode, 66)))));
      DONE;
    }

  /* If this is a plus, then this must be an add of the stack pointer against
     either a hard register or a pseudo.  */
  if (TARGET_MIPS16 && GET_CODE (operands[1]) == PLUS)
    {
      rtx plus_op;

      if (XEXP (operands[1], 0) == stack_pointer_rtx)
	plus_op = XEXP (operands[1], 1);
      else if (XEXP (operands[1], 1) == stack_pointer_rtx)
	plus_op = XEXP (operands[1], 0);
      else
	abort ();

      /* We should have a register now.  */
      if (GET_CODE (plus_op) != REG)
	abort ();

      if (REGNO (plus_op) < FIRST_PSEUDO_REGISTER)
	{
	  /* We have to have at least one temporary register which is not
	     overlapping plus_op.  */
	  if (! rtx_equal_p (plus_op, operands[0]))
	    {
	      emit_move_insn (operands[0], stack_pointer_rtx);
	      emit_insn (gen_addsi3 (operands[0], operands[0], plus_op));
	    }
	  else if (! rtx_equal_p (plus_op, operands[2]))
	    {
	      emit_move_insn (operands[2], stack_pointer_rtx);
	      emit_insn (gen_addsi3 (operands[0], plus_op, operands[2]));
	    }
	  else
	    abort ();
	}
      else
	{
	  /* We need two registers in this case.  */
	  if (! rtx_equal_p (operands[0], operands[2]))
	    {
	      emit_move_insn (operands[0], stack_pointer_rtx);
	      emit_move_insn (operands[2], plus_op);
	      emit_insn (gen_addsi3 (operands[0], operands[0], operands[2]));
	    }
	  else
	    abort ();
	}
      DONE;
    }

  /* FIXME: I don't know how to get a value into the HI register.  */
  emit_move_insn (operands[0], operands[1]);
  DONE;
}")

;; This insn handles moving CCmode values.  It's really just a
;; slightly simplified copy of movsi_internal2, with additional cases
;; to move a condition register to a general register and to move
;; between the general registers and the floating point registers.

(define_insn "movcc"
  [(set (match_operand:CC 0 "nonimmediate_operand" "=d,*d,*d,*d,*R,*m,*d,*f,*f,*f,*f,*R,*m")
	(match_operand:CC 1 "general_operand" "z,*d,*R,*m,*d,*d,*f,*d,*f,*R,*m,*f,*f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,move,load,load,store,store,xfer,xfer,move,load,load,store,store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8,4,4,8,4,8,4,4,4,4,8,4,8")])

;; Reload condition code registers.  These need scratch registers.

(define_expand "reload_incc"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(match_operand:CC 1 "general_operand" "z"))
   (clobber (match_operand:TF 2 "register_operand" "=&f"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
  "
{
  rtx source;
  rtx fp1, fp2;

  /* This is called when are copying some value into a condition code
     register.  Operand 0 is the condition code register.  Operand 1
     is the source.  Operand 2 is a scratch register; we use TFmode
     because we actually need two floating point registers.  */
  if (! ST_REG_P (true_regnum (operands[0]))
      || ! FP_REG_P (true_regnum (operands[2])))
    abort ();

  /* We need to get the source in SFmode so that the insn is
     recognized.  */
  if (GET_CODE (operands[1]) == MEM)
    source = change_address (operands[1], SFmode, NULL_RTX);
  else if (GET_CODE (operands[1]) == REG || GET_CODE (operands[1]) == SUBREG)
    source = gen_rtx_REG (SFmode, true_regnum (operands[1]));
  else
    source = operands[1];

  fp1 = gen_rtx_REG (SFmode, REGNO (operands[2]));
  fp2 = gen_rtx_REG (SFmode, REGNO (operands[2]) + 1);

  emit_insn (gen_move_insn (fp1, source));
  emit_insn (gen_move_insn (fp2, gen_rtx_REG (SFmode, 0)));
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_LT (CCmode, fp2, fp1)));

  DONE;
}")

(define_expand "reload_outcc"
  [(set (match_operand:CC 0 "general_operand" "=z")
	(match_operand:CC 1 "register_operand" "z"))
   (clobber (match_operand:CC 2 "register_operand" "=&d"))]
  "ISA_HAS_8CC && TARGET_HARD_FLOAT"
  "
{
  /* This is called when we are copying a condition code register out
     to save it somewhere.  Operand 0 should be the location we are
     going to save it to.  Operand 1 should be the condition code
     register.  Operand 2 should be a scratch general purpose register
     created for us by reload.  The mips_secondary_reload_class
     function should have told reload that we don't need a scratch
     register if the destination is a general purpose register anyhow.  */
  if (ST_REG_P (true_regnum (operands[0]))
      || GP_REG_P (true_regnum (operands[0]))
      || ! ST_REG_P (true_regnum (operands[1]))
      || ! GP_REG_P (true_regnum (operands[2])))
    abort ();

  /* All we have to do is copy the value from the condition code to
     the data register, which movcc can handle, and then store the
     value into the real final destination.  */
  emit_insn (gen_move_insn (operands[2], operands[1]));
  emit_insn (gen_move_insn (operands[0], operands[2]));

  DONE;
}")

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
  "lwxc1\\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mem:SF (plus:DI (match_operand:DI 1 "se_register_operand" "d")
			 (match_operand:DI 2 "se_register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "lwxc1\\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mem:DF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "ldxc1\\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mem:DF (plus:DI (match_operand:DI 1 "se_register_operand" "d")
			 (match_operand:DI 2 "se_register_operand" "d"))))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "ldxc1\\t%0,%1(%2)"
  [(set_attr "type"	"load")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (mem:SF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d")))
	(match_operand:SF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "swxc1\\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (mem:SF (plus:DI (match_operand:DI 1 "se_register_operand" "d")
			 (match_operand:DI 2 "se_register_operand" "d")))
	(match_operand:SF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT"
  "swxc1\\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SF")])

(define_insn ""
  [(set (mem:DF (plus:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "register_operand" "d")))
	(match_operand:DF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "sdxc1\\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"DF")])

(define_insn ""
  [(set (mem:DF (plus:DI (match_operand:DI 1 "se_register_operand" "d")
			 (match_operand:DI 2 "se_register_operand" "d")))
	(match_operand:DF 0 "register_operand" "f"))]
  "ISA_HAS_FP4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "sdxc1\\t%0,%1(%2)"
  [(set_attr "type"	"store")
   (set_attr "mode"	"DF")])

;; 16-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because BYTE_LOADS_ZERO_EXTEND is defined

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
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
}")

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "movhi_internal1"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,d,R,m,*d,*f,*f*z,*x,*d")
	(match_operand:HI 1 "general_operand"       "d,IK,R,m,dJ,dJ,*f*z,*d,*f,*d,*x"))]
  "TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,move,hilo,hilo")
   (set_attr "mode"	"HI")
   (set_attr "length"	"4,4,4,8,4,8,4,4,4,4,4")])

(define_insn "movhi_internal2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,d,R,m,*d,*z,*x,*d")
	(match_operand:HI 1 "general_operand"       "d,IK,R,m,dJ,dJ,*z,*d,*d,*x"))]
  "!TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,hilo,hilo")
   (set_attr "mode"	"HI")
   (set_attr "length"	"4,4,4,8,4,8,4,4,4,4")])

(define_insn ""
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,R,m,*d")
	(match_operand:HI 1 "general_operand"      "d,d,y,K,N,R,m,d,d,*x"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,move,move,arith,arith,load,load,store,store,hilo")
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
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)])])


;; On the mips16, we can split lh $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(mem:HI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" ""))))]
  "TARGET_MIPS16 && reload_completed
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
  "
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
}")

;; 8-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because BYTE_LOADS_ZERO_EXTEND is defined

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
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
}")

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "movqi_internal1"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,d,R,m,*d,*f*z,*f,*x,*d")
	(match_operand:QI 1 "general_operand"       "d,IK,R,m,dJ,dJ,*f*z,*d,*f,*d,*x"))]
  "TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,move,hilo,hilo")
   (set_attr "mode"	"QI")
   (set_attr "length"	"4,4,4,8,4,8,4,4,4,4,4")])

(define_insn "movqi_internal2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,d,R,m,*d,*z,*x,*d")
	(match_operand:QI 1 "general_operand"       "d,IK,R,m,dJ,dJ,*z,*d,*d,*x"))]
  "!TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,hilo,hilo")
   (set_attr "mode"	"QI")
   (set_attr "length"	"4,4,4,8,4,8,4,4,4,4")])

(define_insn ""
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,y,d,d,d,d,d,R,m,*d")
	(match_operand:QI 1 "general_operand"      "d,d,y,K,N,R,m,d,d,*x"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,move,move,arith,arith,load,load,store,store,hilo")
   (set_attr "mode"	"QI")
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
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)
		 (const_int 8)
		 (const_int 4)])])


;; On the mips16, we can split lb $r,N($r) into an add and a load,
;; when the original load is a 4 byte instruction but the add and the
;; load are 2 2 byte instructions.

(define_split
  [(set (match_operand:QI 0 "register_operand" "")
	(mem:QI (plus:SI (match_dup 0)
			 (match_operand:SI 1 "const_int_operand" ""))))]
  "TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32
	   && INTVAL (operands[1]) <= 31 + 0x7f))"
  [(set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))
   (set (match_dup 0) (mem:QI (plus:SI (match_dup 0) (match_dup 2))))]
  "
{
  HOST_WIDE_INT val = INTVAL (operands[1]);

  if (val < 0)
    operands[2] = GEN_INT (0);
  else
    {
      operands[1] = GEN_INT (0x7f);
      operands[2] = GEN_INT (val - 0x7f);
    }
}")

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SFmode)
      && !register_operand (operands[1], SFmode)
      && (TARGET_MIPS16
	  || ((GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
	       && operands[1] != CONST0_RTX (SFmode))))
    {
      rtx temp = force_reg (SFmode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

(define_insn "movsf_internal1"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,f,R,m,*f,*d,*d,*d,*d,*R,*m")
	(match_operand:SF 1 "general_operand" "f,G,R,Fm,fG,fG,*d,*f,*G*d,*R,*F*m,*d,*d"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (SFmode))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,xfer,load,load,store,store,xfer,xfer,move,load,load,store,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4,4,4,8,4,8,4,4,4,4,8,4,8")])


(define_insn "movsf_internal2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,d,R,m")
	(match_operand:SF 1 "general_operand" "      Gd,R,Fm,d,d"))]
  "TARGET_SOFT_FLOAT && !TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (SFmode))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,load,load,store,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4,4,8,4,8")])

(define_insn ""
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,y,d,d,d,R,m")
	(match_operand:SF 1 "general_operand"      "d,d,y,R,Fm,d,d"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,move,move,load,load,store,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"4,4,4,4,8,4,8")])


;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], DFmode)
      && !register_operand (operands[1], DFmode)
      && (TARGET_MIPS16
	  || ((GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
	       && operands[1] != CONST0_RTX (DFmode))))
    {
      rtx temp = force_reg (DFmode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

(define_insn "movdf_internal1"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,R,To,f,*f,*d,*d,*d,*d,*R,*T")
	(match_operand:DF 1 "general_operand" "f,R,To,fG,fG,F,*d,*f,*d*G,*R,*T*F,*d,*d"))]
  "TARGET_HARD_FLOAT && !(TARGET_FLOAT64 && !TARGET_64BIT)
   && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DFmode))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,load,load,store,store,load,xfer,xfer,move,load,load,store,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4,8,16,8,16,16,8,8,8,8,16,8,16")])

(define_insn "movdf_internal1a"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,R,R,To,To,f,*d,*d,*d,*To,*R,*d")
 	(match_operand:DF 1 "general_operand"      " f,To,f,G,f,G,F,*F,*To,*R,*d,*d,*d"))]
  "TARGET_HARD_FLOAT && (TARGET_FLOAT64 && !TARGET_64BIT)
   && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands [0]) == MEM
	   && ((GET_CODE (operands[1]) == CONST_INT
		&& INTVAL (operands[1]) == 0)
	       || operands[1] == CONST0_RTX (DFmode))))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,load,store,store,store,store,load,load,load,load,store,store,move")
   (set_attr "mode"	"DF")
   (set_attr "length"	"4,8,4,4,8,8,8,8,8,4,8,4,4")])

(define_insn "movdf_internal2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,d,R,To")
	(match_operand:DF 1 "general_operand" "dG,R,ToF,d,d"))]
  "(TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DFmode))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,load,load,store,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8,8,16,8,16")])

(define_insn ""
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,y,d,d,d,R,To")
	(match_operand:DF 1 "general_operand" "d,d,y,R,ToF,d,d"))]
  "TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "* return mips_move_2words (operands, insn);"
  [(set_attr "type"	"move,move,move,load,load,store,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"8,8,8,8,16,8,16")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(match_operand:DF 1 "register_operand" ""))]
  "reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"
  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 1) (subreg:SI (match_dup 1) 1))]
  "")

;; Instructions to load the global pointer register.
;; This is volatile to make sure that the scheduler won't move any symbol_ref
;; uses in front of it.  All symbol_refs implicitly use the gp reg.

(define_insn "loadgp"
  [(set (reg:DI 28)
	(unspec_volatile:DI [(match_operand:DI 0 "address_operand" "")
			     (match_operand:DI 1 "register_operand" "")] 2))
   (clobber (reg:DI 1))]
  ""
  "%[lui\\t$1,%%hi(%%neg(%%gp_rel(%a0)))\\n\\taddiu\\t$1,$1,%%lo(%%neg(%%gp_rel(%a0)))\\n\\tdaddu\\t$gp,$1,%1%]"
  [(set_attr "type"	"move")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12")])

;; Block moves, see mips.c for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movstrsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand" "")
		   (match_operand:BLK 1 "general_operand" ""))
	      (use (match_operand:SI 2 "arith32_operand" ""))
	      (use (match_operand:SI 3 "immediate_operand" ""))])]
  "!TARGET_MIPS16"
  "
{
  if (operands[0])		/* avoid unused code messages */
    {
      expand_block_move (operands);
      DONE;
    }
}")

;; Insn generated by block moves

(define_insn "movstrsi_internal"
  [(set (match_operand:BLK 0 "memory_operand" "=o")	;; destination
	(match_operand:BLK 1 "memory_operand" "o"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 0))]					;; normal block move
  ""
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);"
  [(set_attr "type"	"store")
   (set_attr "mode"	"none")
   (set_attr "length"	"80")])

;; We need mips16 versions, because an offset from the stack pointer
;; is not offsettable, since the stack pointer can only handle 4 and 8
;; byte loads.

(define_insn ""
  [(set (match_operand:BLK 0 "memory_operand" "=o")	;; destination
	(match_operand:BLK 1 "memory_operand" "o"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 0))]					;; normal block move
  "TARGET_MIPS16"
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"none")
   (set_attr "length"	"80")])

;; Split a block move into 2 parts, the first part is everything
;; except for the last move, and the second part is just the last
;; store, which is exactly 1 instruction (ie, not a usw), so it can
;; fill a delay slot.  This also prevents a bug in delayed branches
;; from showing up, which reuses one of the registers in our clobbers.

(define_split
  [(set (mem:BLK (match_operand:SI 0 "register_operand" ""))
	(mem:BLK (match_operand:SI 1 "register_operand" "")))
   (clobber (match_operand:SI 4 "register_operand" ""))
   (clobber (match_operand:SI 5 "register_operand" ""))
   (clobber (match_operand:SI 6 "register_operand" ""))
   (clobber (match_operand:SI 7 "register_operand" ""))
   (use (match_operand:SI 2 "small_int" ""))
   (use (match_operand:SI 3 "small_int" ""))
   (use (const_int 0))]

  "reload_completed && !TARGET_DEBUG_D_MODE && INTVAL (operands[2]) > 0"

  ;; All but the last move
  [(parallel [(set (mem:BLK (match_dup 0))
		   (mem:BLK (match_dup 1)))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))
	      (clobber (match_dup 7))
	      (use (match_dup 2))
	      (use (match_dup 3))
	      (use (const_int 1))])

   ;; The last store, so it can fill a delay slot
   (parallel [(set (mem:BLK (match_dup 0))
		   (mem:BLK (match_dup 1)))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))
	      (clobber (match_dup 7))
	      (use (match_dup 2))
	      (use (match_dup 3))
	      (use (const_int 2))])]

  "")

(define_insn "movstrsi_internal2"
  [(set (match_operand:BLK 0 "memory_operand" "=o")	;; destination
	(match_operand:BLK 1 "memory_operand" "o"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 1))]					;; all but last store
  ""
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);"
  [(set_attr "type"	"store")
   (set_attr "mode"	"none")
   (set_attr "length"	"80")])

(define_insn ""
  [(set (match_operand:BLK 0 "memory_operand" "=o")	;; destination
	(match_operand:BLK 1 "memory_operand" "o"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 1))]					;; all but last store
  "TARGET_MIPS16"
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"none")
   (set_attr "length"	"80")])

(define_insn "movstrsi_internal3"
  [(set (match_operand:BLK 0 "memory_operand" "=Ro")	;; destination
	(match_operand:BLK 1 "memory_operand" "Ro"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 2))]					;; just last store of block move
  ""
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_LAST);"
  [(set_attr "type"	"store")
   (set_attr "mode"	"none")])

;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................

;; Many of these instructions uses trivial define_expands, because we
;; want to use a different set of constraints when TARGET_MIPS16.

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
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
}")

(define_insn "ashlsi3_internal1"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"sll\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "ashlsi3_internal2"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,d")
		   (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"sll\\t%0,%2\";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"sll\\t%0,%1,%2\";
}"
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
  "TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashift:SI (match_dup 0) (match_dup 2)))]
"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
}")

(define_expand "ashldi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashift:DI (match_operand:DI 1 "se_register_operand" "")
			      (match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
  "
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
}")


(define_insn "ashldi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "* 
{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return \"sll\\t%3,%2,26\\n\\
\\tbgez\\t%3,1f\\n\\
\\tsll\\t%M0,%L1,%2\\n\\
\\t%(b\\t3f\\n\\
\\tmove\\t%L0,%z4%)\\n\\
\\n\\
%~1:\\n\\
\\t%(beq\\t%3,%z4,2f\\n\\
\\tsll\\t%M0,%M1,%2%)\\n\\
\\n\\
\\tsubu\\t%3,%z4,%2\\n\\
\\tsrl\\t%3,%L1,%3\\n\\
\\tor\\t%M0,%M0,%3\\n\\
%~2:\\n\\
\\tsll\\t%L0,%L1,%2\\n\\
%~3:\";
}"
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
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  operands[4] = const0_rtx;
  return \"sll\\t%M0,%L1,%2\;move\\t%L0,%z4\";
}"
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

  [(set (subreg:SI (match_dup 0) 1) (ashift:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
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

  [(set (subreg:SI (match_dup 0) 0) (ashift:SI (subreg:SI (match_dup 1) 1) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 1) (const_int 0))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_insn "ashldi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
  "*
{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = const0_rtx;
  operands[5] = GEN_INT ((-amount) & 31);

  return \"sll\\t%M0,%M1,%2\;srl\\t%3,%L1,%5\;or\\t%M0,%M0,%3\;sll\\t%L0,%L1,%2\";
}"
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

  [(set (subreg:SI (match_dup 0) 1)
	(ashift:SI (subreg:SI (match_dup 1) 1)
		   (match_dup 2)))

   (set (match_dup 3)
	(lshiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 4)))

   (set (subreg:SI (match_dup 0) 1)
	(ior:SI (subreg:SI (match_dup 0) 1)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 0)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 2)))]
  "
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
}")


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
	(lshiftrt:SI (subreg:SI (match_dup 1) 1)
		     (match_dup 4)))

   (set (subreg:SI (match_dup 0) 0)
	(ior:SI (subreg:SI (match_dup 0) 0)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 1)
	(ashift:SI (subreg:SI (match_dup 1) 1)
		   (match_dup 2)))]
  "
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
}")


(define_insn "ashldi3_internal4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "se_register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"dsll\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ashift:DI (match_operand:DI 1 "se_register_operand" "0,d")
		   (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"dsll\\t%0,%2\";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"dsll\\t%0,%1,%2\";
}"
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
  "TARGET_MIPS16 && TARGET_64BIT
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashift:DI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashift:DI (match_dup 0) (match_dup 2)))]
"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
}")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
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
}")

(define_insn "ashrsi3_internal1"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"sra\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "ashrsi3_internal2"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,d")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"sra\\t%0,%2\";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"sra\\t%0,%1,%2\";
}"
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
  "TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashiftrt:SI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 2)))]
"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
}")

(define_expand "ashrdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashiftrt:DI (match_operand:DI 1 "se_register_operand" "")
				(match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
  "
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
}")


(define_insn "ashrdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "* 
{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return \"sll\\t%3,%2,26\\n\\
\\tbgez\\t%3,1f\\n\\
\\tsra\\t%L0,%M1,%2\\n\\
\\t%(b\\t3f\\n\\
\\tsra\\t%M0,%M1,31%)\\n\\
\\n\\
%~1:\\n\\
\\t%(beq\\t%3,%z4,2f\\n\\
\\tsrl\\t%L0,%L1,%2%)\\n\\
\\n\\
\\tsubu\\t%3,%z4,%2\\n\\
\\tsll\\t%3,%M1,%3\\n\\
\\tor\\t%L0,%L0,%3\\n\\
%~2:\\n\\
\\tsra\\t%M0,%M1,%2\\n\\
%~3:\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"48")])


(define_insn "ashrdi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return \"sra\\t%L0,%M1,%2\;sra\\t%M0,%M1,31\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (ashiftrt:SI (subreg:SI (match_dup 1) 1) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 1) (ashiftrt:SI (subreg:SI (match_dup 1) 1) (const_int 31)))]

  "operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 1) (ashiftrt:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
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
  "*
{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return \"srl\\t%L0,%L1,%2\;sll\\t%3,%M1,%4\;or\\t%L0,%L0,%3\;sra\\t%M0,%M1,%2\";
}"
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
	(ashift:SI (subreg:SI (match_dup 1) 1)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 0)
	(ior:SI (subreg:SI (match_dup 0) 0)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 1)
	(ashiftrt:SI (subreg:SI (match_dup 1) 1)
		     (match_dup 2)))]
  "
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
}")


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

  [(set (subreg:SI (match_dup 0) 1)
	(lshiftrt:SI (subreg:SI (match_dup 1) 1)
		     (match_dup 2)))

   (set (match_dup 3)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 1)
	(ior:SI (subreg:SI (match_dup 0) 1)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 0)
	(ashiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 2)))]
  "
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
}")


(define_insn "ashrdi3_internal4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "se_register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"dsra\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(ashiftrt:DI (match_operand:DI 1 "se_register_operand" "0,0")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"dsra\\t%0,%2\";
}"
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
  "TARGET_MIPS16 && TARGET_64BIT
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (ashiftrt:DI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (ashiftrt:DI (match_dup 0) (match_dup 2)))]
"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
}")

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
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
}")

(define_insn "lshrsi3_internal1"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"srl\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "lshrsi3_internal2"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,d")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "*
{
  if (which_alternative == 0)
    return \"srl\\t%0,%2\";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"srl\\t%0,%1,%2\";
}"
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
  "TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (lshiftrt:SI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 2)))]
"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
}")

;; If we load a byte on the mips16 as a bitfield, the resulting
;; sequence of instructions is too complicated for combine, because it
;; involves four instructions: a load, a shift, a constant load into a
;; register, and an and (the key problem here is that the mips16 does
;; not have and immediate).  We recognize a shift of a load in order
;; to make it simple enough for combine to understand.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "R,m")
		     (match_operand:SI 2 "immediate_operand" "I,I")))]
  "TARGET_MIPS16"
  "lw\\t%0,%1\;srl\\t%0,%2"
  [(set_attr "type"	"load")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 8)
			       (const_int 12))
		 (if_then_else (match_operand:VOID 2 "m16_uimm3_b" "")
			       (const_int 12)
			       (const_int 16))])])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "")
		     (match_operand:SI 2 "immediate_operand" "")))]
  "TARGET_MIPS16"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 2)))]
  "")

(define_expand "lshrdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (lshiftrt:DI (match_operand:DI 1 "se_register_operand" "")
				(match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16)"
  "
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
}")


(define_insn "lshrdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16"
  "* 
{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return \"sll\\t%3,%2,26\\n\\
\\tbgez\\t%3,1f\\n\\
\\tsrl\\t%L0,%M1,%2\\n\\
\\t%(b\\t3f\\n\\
\\tmove\\t%M0,%z4%)\\n\\
\\n\\
%~1:\\n\\
\\t%(beq\\t%3,%z4,2f\\n\\
\\tsrl\\t%L0,%L1,%2%)\\n\\
\\n\\
\\tsubu\\t%3,%z4,%2\\n\\
\\tsll\\t%3,%M1,%3\\n\\
\\tor\\t%L0,%L0,%3\\n\\
%~2:\\n\\
\\tsrl\\t%M0,%M1,%2\\n\\
%~3:\";
}"
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
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  operands[4] = const0_rtx;
  return \"srl\\t%L0,%M1,%2\;move\\t%M0,%z4\";
}"
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

  [(set (subreg:SI (match_dup 0) 0) (lshiftrt:SI (subreg:SI (match_dup 1) 1) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 1) (const_int 0))]

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

  [(set (subreg:SI (match_dup 0) 1) (lshiftrt:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
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
  "*
{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);

  return \"srl\\t%L0,%L1,%2\;sll\\t%3,%M1,%4\;or\\t%L0,%L0,%3\;srl\\t%M0,%M1,%2\";
}"
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
	(ashift:SI (subreg:SI (match_dup 1) 1)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 0)
	(ior:SI (subreg:SI (match_dup 0) 0)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 1)
	(lshiftrt:SI (subreg:SI (match_dup 1) 1)
		     (match_dup 2)))]
  "
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
}")


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

  [(set (subreg:SI (match_dup 0) 1)
	(lshiftrt:SI (subreg:SI (match_dup 1) 1)
		     (match_dup 2)))

   (set (match_dup 3)
	(ashift:SI (subreg:SI (match_dup 1) 0)
		   (match_dup 4)))

   (set (subreg:SI (match_dup 0) 1)
	(ior:SI (subreg:SI (match_dup 0) 1)
		(match_dup 3)))

   (set (subreg:SI (match_dup 0) 0)
	(lshiftrt:SI (subreg:SI (match_dup 1) 0)
		     (match_dup 2)))]
  "
{
  int amount = INTVAL (operands[2]);
  operands[2] = GEN_INT (amount & 31);
  operands[4] = GEN_INT ((-amount) & 31);
}")


(define_insn "lshrdi3_internal4"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "se_register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"dsrl\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(lshiftrt:DI (match_operand:DI 1 "se_register_operand" "0,0")
		     (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"dsrl\\t%0,%2\";
}"
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
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  "TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16"
  [(set (match_dup 0) (lshiftrt:DI (match_dup 1) (const_int 8)))
   (set (match_dup 0) (lshiftrt:DI (match_dup 0) (match_dup 2)))]
"
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 8);
}")


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
;;	operands into the branchs for integer operations, and use cc0
;;	for floating point, so we use the fp status register instead.
;;	If needed, an appropriate temporary is created to hold the
;;	of the integer compare.

(define_expand "cmpsi"
  [(set (cc0)
	(compare:CC (match_operand:SI 0 "register_operand" "")
		    (match_operand:SI 1 "arith_operand" "")))]
  ""
  "
{
  if (operands[0])		/* avoid unused code message */
    {
      branch_cmp[0] = operands[0];
      branch_cmp[1] = operands[1];
      branch_type = CMP_SI;
      DONE;
    }
}")

(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" ""))]
  ""
  "
{
  if (operands[0])		/* avoid unused code message */
    {
      branch_cmp[0] = operands[0];
      branch_cmp[1] = const0_rtx;
      branch_type = CMP_SI;
      DONE;
    }
}")

(define_expand "cmpdi"
  [(set (cc0)
	(compare:CC (match_operand:DI 0 "se_register_operand" "")
		    (match_operand:DI 1 "se_arith_operand" "")))]
  "TARGET_64BIT"
  "
{
  if (operands[0])		/* avoid unused code message */
    {
      branch_cmp[0] = operands[0];
      branch_cmp[1] = operands[1];
      branch_type = CMP_DI;
      DONE;
    }
}")

(define_expand "tstdi"
  [(set (cc0)
	(match_operand:DI 0 "se_register_operand" ""))]
  "TARGET_64BIT"
  "
{
  if (operands[0])		/* avoid unused code message */
    {
      branch_cmp[0] = operands[0];
      branch_cmp[1] = const0_rtx;
      branch_type = CMP_DI;
      DONE;
    }
}")

(define_expand "cmpdf"
  [(set (cc0)
	(compare:CC (match_operand:DF 0 "register_operand" "")
		    (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "
{
  if (operands[0])		/* avoid unused code message */
    {
      branch_cmp[0] = operands[0];
      branch_cmp[1] = operands[1];
      branch_type = CMP_DF;
      DONE;
    }
}")

(define_expand "cmpsf"
  [(set (cc0)
	(compare:CC (match_operand:SF 0 "register_operand" "")
		    (match_operand:SF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT"
  "
{
  if (operands[0])		/* avoid unused code message */
    {
      branch_cmp[0] = operands[0];
      branch_cmp[1] = operands[1];
      branch_type = CMP_SF;
      DONE;
    }
}")


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
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/1,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}"
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
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/1,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}"
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
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}"
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
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_zero_di"
  [(set (pc)
	(if_then_else 
         (match_operator:DI 0 "cmp_op"
		            [(match_operand:DI 2 "se_register_operand" "d")
			     (const_int 0)])
        (label_ref (match_operand 1 "" ""))
        (pc)))]
  "!TARGET_MIPS16"
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_zero_di_inverted"
  [(set (pc)
	(if_then_else 
         (match_operator:DI 0 "cmp_op"
			    [(match_operand:DI 2 "se_register_operand" "d")
			     (const_int 0)])
        (pc)
        (label_ref (match_operand 1 "" ""))))]
  "!TARGET_MIPS16"
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/0,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

;; Conditional branch on equality comparision.

(define_insn "branch_equality"
  [(set (pc)
	(if_then_else 
         (match_operator:SI 0 "equality_op"
		   	    [(match_operand:SI 2 "register_operand" "d")
			     (match_operand:SI 3 "register_operand" "d")])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "!TARGET_MIPS16"
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_equality_di"
  [(set (pc)
	(if_then_else 
         (match_operator:DI 0 "equality_op"
			    [(match_operand:DI 2 "se_register_operand" "d")
			     (match_operand:DI 3 "se_register_operand" "d")])
        (label_ref (match_operand 1 "" ""))
        (pc)))]
  "!TARGET_MIPS16"
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/0,
					 get_attr_length (insn));
}"
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
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")])

(define_insn "branch_equality_di_inverted"
  [(set (pc)
	(if_then_else 
         (match_operator:DI 0 "equality_op"
			    [(match_operand:DI 2 "se_register_operand" "d")
			     (match_operand:DI 3 "se_register_operand" "d")])
        (pc)
        (label_ref (match_operand 1 "" ""))))]
  "!TARGET_MIPS16"
  "*
{
  return mips_output_conditional_branch (insn,
					 operands,
					 /*two_operands_p=*/1,
					 /*float_p=*/0,
					 /*inverted_p=*/1,
					 get_attr_length (insn));
}"
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
  "*
{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return \"%*b%C0z\\t%1,%2\";
      else
	return \"%*bt%C0z\\t%2\";
    }
  else
    {
      if (which_alternative == 0)
	return \"%*b%N0z\\t%1,%3\";
      else
	return \"%*bt%N0z\\t%3\";
    }
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator:DI 0 "equality_op"
					 [(match_operand:DI 1 "se_register_operand" "d,t")
					  (const_int 0)])
	(match_operand 2 "pc_or_label_operand" "")
	(match_operand 3 "pc_or_label_operand" "")))]
  "TARGET_MIPS16"
  "*
{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return \"%*b%C0z\\t%1,%2\";
      else
	return \"%*bt%C0z\\t%2\";
    }
  else
    {
      if (which_alternative == 0)
	return \"%*b%N0z\\t%1,%3\";
      else
	return \"%*bt%N0z\\t%3\";
    }
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, EQ);
      DONE;
    }
}")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, NE);
      DONE;
    }
}")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, GT);
      DONE;
    }
}")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, GE);
      DONE;
    }
}")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, LT);
      DONE;
    }
}")

(define_expand "ble"
  [(set (pc)
	(if_then_else (le:CC (cc0)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, LE);
      DONE;
    }
}")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, GTU);
      DONE;
    }
}")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, GEU);
      DONE;
    }
}")


(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, LTU);
      DONE;
    }
}")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu:CC (cc0)
			      (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (operands[0])		/* avoid unused code warning */
    {
      gen_conditional_branch (operands, LEU);
      DONE;
    }
}")


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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (EQ, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* fall through and generate default code */
}")


(define_insn "seq_si_zero"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(eq:SI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  "!TARGET_MIPS16"
  "sltu\\t%0,%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(eq:SI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  "TARGET_MIPS16"
  "sltu\\t%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "seq_di_zero"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(eq:DI (match_operand:DI 1 "se_register_operand" "d")
	       (const_int 0)))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\\t%0,%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(eq:DI (match_operand:DI 1 "se_register_operand" "d")
	       (const_int 0)))]
  "TARGET_64BIT && TARGET_MIPS16"
  "sltu\\t%1,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "seq_si"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(eq:SI (match_operand:SI 1 "register_operand" "%d,d")
	       (match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
   xor\\t%0,%1,%2\;sltu\\t%0,%0,1
   xori\\t%0,%1,%2\;sltu\\t%0,%0,1"
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
	(eq:DI (match_operand:DI 1 "se_register_operand" "%d,d")
	       (match_operand:DI 2 "se_uns_arith_operand" "d,K")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
   xor\\t%0,%1,%2\;sltu\\t%0,%0,1
   xori\\t%0,%1,%2\;sltu\\t%0,%0,1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(eq:DI (match_operand:DI 1 "se_register_operand" "")
	       (match_operand:DI 2 "se_uns_arith_operand" "")))]
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE)
    {
      gen_int_relational (NE, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* fall through and generate default code */
}")

(define_insn "sne_si_zero"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ne:SI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  "!TARGET_MIPS16"
  "sltu\\t%0,%.,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "sne_di_zero"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ne:DI (match_operand:DI 1 "se_register_operand" "d")
	       (const_int 0)))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\\t%0,%.,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "sne_si"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ne:SI (match_operand:SI 1 "register_operand" "%d,d")
	       (match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
    xor\\t%0,%1,%2\;sltu\\t%0,%.,%0
    xori\\t%0,%1,%x2\;sltu\\t%0,%.,%0"
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
	(ne:DI (match_operand:DI 1 "se_register_operand" "%d,d")
	       (match_operand:DI 2 "se_uns_arith_operand" "d,K")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "@
    xor\\t%0,%1,%2\;sltu\\t%0,%.,%0
    xori\\t%0,%1,%x2\;sltu\\t%0,%.,%0"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ne:DI (match_operand:DI 1 "se_register_operand" "")
	       (match_operand:DI 2 "se_uns_arith_operand" "")))]
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GT, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* fall through and generate default code */
}")

(define_insn "sgt_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gt:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "reg_or_0_operand" "dJ")))]
  "!TARGET_MIPS16"
  "slt\\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(gt:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "register_operand" "d")))]
  "TARGET_MIPS16"
  "slt\\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "sgt_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(gt:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "se_reg_or_0_operand" "dJ")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "slt\\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d")
	(gt:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "slt\\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ge:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GE, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* fall through and generate default code */
}")

(define_insn "sge_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ge:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\\t%0,%1,%2\;xori\\t%0,%0,0x0001"
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
	(ge:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "se_arith_operand" "dI")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\\t%0,%1,%2\;xori\\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ge:DI (match_operand:DI 1 "se_register_operand" "")
	       (match_operand:DI 2 "se_arith_operand" "")))]
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LT, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* fall through and generate default code */
}")

(define_insn "slt_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lt:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "slt\\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t,t")
	(lt:SI (match_operand:SI 1 "register_operand" "d,d")
	       (match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "slt\\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "slt_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lt:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "se_arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "slt\\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t,t")
	(lt:DI (match_operand:DI 1 "se_register_operand" "d,d")
	       (match_operand:DI 2 "se_arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "slt\\t%1,%2"
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LE, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 32767)
    operands[2] = force_reg (SImode, operands[2]);

  /* fall through and generate default code */
}")

(define_insn "sle_si_const"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(le:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "small_int" "I")))]
  "!TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return \"slt\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(le:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "small_int" "I")))]
  "TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return \"slt\\t%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length") (if_then_else (match_operand:VOID 2 "m16_uimm8_m1_1" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn "sle_di_const"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(le:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return \"slt\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(le:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return \"slt\\t%1,%2\";
}"
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
  "slt\\t%0,%z2,%1\;xori\\t%0,%0,0x0001"
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
	(le:DI (match_operand:DI 1 "se_register_operand" "d")
	       (match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "slt\\t%0,%z2,%1\;xori\\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(le:DI (match_operand:DI 1 "se_register_operand" "")
	       (match_operand:DI 2 "se_register_operand" "")))]
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GTU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    operands[2] = force_reg (SImode, operands[2]);

  /* fall through and generate default code */
}")

(define_insn "sgtu_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gtu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "reg_or_0_operand" "dJ")))]
  ""
  "sltu\\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(gtu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))]
  ""
  "sltu\\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "sgtu_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(gtu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_reg_or_0_operand" "dJ")))]
  "TARGET_64BIT"
  "sltu\\t%0,%z2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(gtu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT"
  "sltu\\t%2,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (geu:SI (match_dup 1)
                (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (GEU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* fall through and generate default code */
}")

(define_insn "sgeu_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(geu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "arith_operand" "dI")))]
  "TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\\t%0,%1,%2\;xori\\t%0,%0,0x0001"
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
	(geu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_arith_operand" "dI")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\\t%0,%1,%2\;xori\\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(geu:DI (match_operand:DI 1 "se_register_operand" "")
		(match_operand:DI 2 "se_arith_operand" "")))]
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LTU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  /* fall through and generate default code */
}")

(define_insn "sltu_si"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ltu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "arith_operand" "dI")))]
  "!TARGET_MIPS16"
  "sltu\\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t,t")
	(ltu:SI (match_operand:SI 1 "register_operand" "d,d")
		(match_operand:SI 2 "arith_operand" "d,I")))]
  "TARGET_MIPS16"
  "sltu\\t%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr_alternative "length"
		[(const_int 4)
		 (if_then_else (match_operand:VOID 2 "m16_uimm8_1" "")
			       (const_int 4)
			       (const_int 8))])])

(define_insn "sltu_di"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ltu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_arith_operand" "dI")))]
  "TARGET_64BIT && !TARGET_MIPS16"
  "sltu\\t%0,%1,%2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t,t")
	(ltu:DI (match_operand:DI 1 "se_register_operand" "d,d")
		(match_operand:DI 2 "se_arith_operand" "d,I")))]
  "TARGET_64BIT && TARGET_MIPS16"
  "sltu\\t%1,%2"
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
  "
{
  if (branch_type != CMP_SI && (!TARGET_64BIT || branch_type != CMP_DI))
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (TARGET_64BIT || !TARGET_DEBUG_C_MODE || TARGET_MIPS16)
    {
      gen_int_relational (LEU, operands[0], operands[1], operands[2], (int *)0);
      DONE;
    }

  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 32767)
    operands[2] = force_reg (SImode, operands[2]);

  /* fall through and generate default code */
}")

(define_insn "sleu_si_const"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(leu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "small_int" "I")))]
  "!TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return \"sltu\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=t")
	(leu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "small_int" "I")))]
  "TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return \"sltu\\t%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set (attr "length") (if_then_else (match_operand:VOID 2 "m16_uimm8_m1_1" "")
				      (const_int 4)
				      (const_int 8)))])

(define_insn "sleu_di_const"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(leu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return \"sltu\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=t")
	(leu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "small_int" "I")))]
  "TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return \"sltu\\t%1,%2\";
}"
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
  "sltu\\t%0,%z2,%1\;xori\\t%0,%0,0x0001"
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
	(leu:DI (match_operand:DI 1 "se_register_operand" "d")
		(match_operand:DI 2 "se_register_operand" "d")))]
  "TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16"
  "sltu\\t%0,%z2,%1\;xori\\t%0,%0,0x0001"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"8")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(leu:DI (match_operand:DI 1 "se_register_operand" "")
		(match_operand:DI 2 "se_register_operand" "")))]
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

(define_insn "seq_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(eq:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.eq.d\\t%Z0%1,%2\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "slt_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(lt:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.lt.d\\t%Z0%1,%2\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "sle_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(le:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.le.d\\t%Z0%1,%2\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "sgt_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(gt:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.lt.d\\t%Z0%2,%1\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "sge_df"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(ge:CC (match_operand:DF 1 "register_operand" "f")
	       (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.le.d\\t%Z0%2,%1\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "seq_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(eq:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.eq.s\\t%Z0%1,%2\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "slt_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(lt:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.lt.s\\t%Z0%1,%2\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "sle_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(le:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.le.s\\t%Z0%1,%2\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "sgt_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(gt:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.lt.s\\t%Z0%2,%1\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])

(define_insn "sge_sf"
  [(set (match_operand:CC 0 "register_operand" "=z")
	(ge:CC (match_operand:SF 1 "register_operand" "f")
	       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "*
{
  return mips_fill_delay_slot (\"c.le.s\\t%Z0%2,%1\", DELAY_FCMP, operands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")])


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
  "*
{
  if (GET_CODE (operands[0]) == REG)
    return \"%*j\\t%0\";
  /* ??? I don't know why this is necessary.  This works around an
     assembler problem that appears when a label is defined, then referenced
     in a switch table, then used in a `j' instruction.  */
  else if (mips_abi != ABI_32 && mips_abi != ABI_O64)
    return \"%*b\\t%l0\";
  else	
    return \"%*j\\t%l0\";
}"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; We need a different insn for the mips16, because a mips16 branch
;; does not have a delay slot.

(define_insn ""
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_MIPS16 && GET_CODE (operands[0]) != REG"
  "b\\t%l0"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand" "d"))]
  ""
  "
{
  rtx dest;

  if (operands[0])		/* eliminate unused code warnings */
    {
      dest = operands[0];
      if (GET_CODE (dest) != REG || GET_MODE (dest) != Pmode)
	operands[0] = copy_to_mode_reg (Pmode, dest);

      if (!(Pmode == DImode))
	emit_jump_insn (gen_indirect_jump_internal1 (operands[0]));
      else
	emit_jump_insn (gen_indirect_jump_internal2 (operands[0]));

      DONE;
    }
}")

(define_insn "indirect_jump_internal1"
  [(set (pc) (match_operand:SI 0 "register_operand" "d"))]
  "!(Pmode == DImode)"
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_insn "indirect_jump_internal2"
  [(set (pc) (match_operand:DI 0 "se_register_operand" "d"))]
  "Pmode == DImode"
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "
{
  if (operands[0])		/* eliminate unused code warnings */
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

      if (GET_MODE (operands[0]) != Pmode)
	abort ();

      if (! flag_pic)
	{
	  if (!(Pmode == DImode))
	    emit_jump_insn (gen_tablejump_internal1 (operands[0], operands[1]));
	  else
	    emit_jump_insn (gen_tablejump_internal2 (operands[0], operands[1]));
	}
      else
	{
	  if (!(Pmode == DImode))
	    emit_jump_insn (gen_tablejump_internal3 (operands[0], operands[1]));
	  else
	    emit_jump_insn (gen_tablejump_internal4 (operands[0], operands[1]));
	}

      DONE;
    }
}")

(define_insn "tablejump_internal1"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  "!(Pmode == DImode)"
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_insn "tablejump_internal2"
  [(set (pc)
	(match_operand:DI 0 "se_register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  "Pmode == DImode"
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "tablejump_internal3"
  [(parallel [(set (pc)
		   (plus:SI (match_operand:SI 0 "register_operand" "d")
			    (label_ref:SI (match_operand 1 "" ""))))
	      (use (label_ref:SI (match_dup 1)))])]
  ""
  "")

(define_expand "tablejump_mips161"
  [(set (pc) (plus:SI (sign_extend:SI
		       (match_operand:HI 0 "register_operand" "d"))
		      (label_ref:SI (match_operand 1 "" ""))))]
  "TARGET_MIPS16 && !(Pmode == DImode)"
  "
{
  if (operands[0])	/* eliminate unused code warnings.  */
    {
      rtx t1, t2, t3;

      t1 = gen_reg_rtx (SImode);
      t2 = gen_reg_rtx (SImode);
      t3 = gen_reg_rtx (SImode);
      emit_insn (gen_extendhisi2 (t1, operands[0]));
      emit_move_insn (t2, gen_rtx (LABEL_REF, SImode, operands[1]));
      emit_insn (gen_addsi3 (t3, t1, t2));
      emit_jump_insn (gen_tablejump_internal1 (t3, operands[1]));
      DONE;
    }
}")

(define_expand "tablejump_mips162"
  [(set (pc) (plus:DI (sign_extend:DI
		       (match_operand:HI 0 "register_operand" "d"))
		      (label_ref:DI (match_operand 1 "" ""))))]
  "TARGET_MIPS16 && Pmode == DImode"
  "
{
  if (operands[0])	/* eliminate unused code warnings.  */
    {
      rtx t1, t2, t3;

      t1 = gen_reg_rtx (DImode);
      t2 = gen_reg_rtx (DImode);
      t3 = gen_reg_rtx (DImode);
      emit_insn (gen_extendhidi2 (t1, operands[0]));
      emit_move_insn (t2, gen_rtx (LABEL_REF, DImode, operands[1]));
      emit_insn (gen_adddi3 (t3, t1, t2));
      emit_jump_insn (gen_tablejump_internal2 (t3, operands[1]));
      DONE;
    }
}")

;;; Make sure that this only matches the insn before ADDR_DIFF_VEC.  Otherwise
;;; it is not valid.  ??? With the USE, the condition tests may not be required
;;; any longer.

;;; ??? The length depends on the ABI.  It is two for o32, and one for n32.
;;; We just use the conservative number here.

(define_insn ""
  [(set (pc)
	(plus:SI (match_operand:SI 0 "register_operand" "d")
		 (label_ref:SI (match_operand 1 "" ""))))
   (use (label_ref:SI (match_dup 1)))]
  "!(Pmode == DImode) && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1]"
  "*
{
  /* .cpadd expands to add REG,REG,$gp when pic, and nothing when not pic.  */
  if (mips_abi == ABI_32 || mips_abi == ABI_O64)
    output_asm_insn (\".cpadd\\t%0\", operands);
  return \"%*j\\t%0\";
}"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_expand "tablejump_internal4"
  [(parallel [(set (pc)
		   (plus:DI (match_operand:DI 0 "se_register_operand" "d")
			    (label_ref:DI (match_operand 1 "" ""))))
	      (use (label_ref:DI (match_dup 1)))])]
  ""
  "")

;;; Make sure that this only matches the insn before ADDR_DIFF_VEC.  Otherwise
;;; it is not valid.  ??? With the USE, the condition tests may not be required
;;; any longer.

(define_insn ""
  [(set (pc)
	(plus:DI (match_operand:DI 0 "se_register_operand" "d")
		 (label_ref:DI (match_operand 1 "" ""))))
   (use (label_ref:DI (match_dup 1)))]
  "Pmode == DImode && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1]"
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Implement a switch statement when generating embedded PIC code.
;; Switches are implemented by `tablejump' when not using -membedded-pic.

(define_expand "casesi"
  [(set (match_dup 5)
	(minus:SI (match_operand:SI 0 "register_operand" "d")
		  (match_operand:SI 1 "arith_operand" "dI")))
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
  "
{
  if (operands[0])
    {
      rtx reg = gen_reg_rtx (SImode);

      /* If the index is too large, go to the default label.  */
      emit_insn (gen_subsi3 (reg, operands[0], operands[1]));
      emit_insn (gen_cmpsi (reg, operands[2]));
      emit_insn (gen_bgtu (operands[4]));

      /* Do the PIC jump.  */
      if (Pmode != DImode)
        emit_jump_insn (gen_casesi_internal (reg, operands[3], 
					     gen_reg_rtx (SImode)));
      else
        emit_jump_insn (gen_casesi_internal_di (reg, operands[3], 
						gen_reg_rtx (DImode)));

      DONE;
    }
}")

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
  "%(bal\\t%S1\;sll\\t%2,%0,2\\n%~%S1:\;addu\\t%2,%2,$31%)\;\\
lw\\t%2,%1-%S1(%2)\;addu\\t%2,%2,$31\;j\\t%2"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"24")])

(define_insn "casesi_internal_di"
  [(set (pc)
	(mem:DI (plus:DI (sign_extend:DI 
			  (mult:SI (match_operand:SI 0 "register_operand" "d")
				  (const_int 4)))
			 (label_ref (match_operand 1 "" "")))))
   (clobber (match_operand:DI 2 "register_operand" "=d"))
   (clobber (reg:DI 31))]
  "TARGET_EMBEDDED_PIC"
  "%(bal\\t%S1\;sll\\t%2,%0,2\\n%~%S1:\;addu\\t%2,%2,$31%)\;\\
ld\\t%2,%1-%S1(%2)\;daddu\\t%2,%2,$31\;j\\t%2"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"24")])

;; For o32/n32/n64, we save the gp in the jmp_buf as well.  While it is
;; possible to either pull it off the stack (in the o32 case) or recalculate
;; it given t9 and our target label, it takes 3 or 4 insns to do so, and
;; this is easy.

(define_expand "builtin_setjmp_setup"
  [(unspec [(match_operand 0 "register_operand" "r")] 20)]
  "TARGET_ABICALLS"
  "
{
  if (Pmode == DImode)
    emit_insn (gen_builtin_setjmp_setup_64 (operands[0]));
  else
    emit_insn (gen_builtin_setjmp_setup_32 (operands[0]));
  DONE;
}")

(define_expand "builtin_setjmp_setup_32"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "register_operand" "r")
		   (const_int 12)))
      (reg:SI 28))]
  "TARGET_ABICALLS && ! (Pmode == DImode)"
  "")

(define_expand "builtin_setjmp_setup_64"
  [(set (mem:DI (plus:DI (match_operand:DI 0 "register_operand" "r")
		   (const_int 24)))
      (reg:DI 28))]
  "TARGET_ABICALLS && Pmode == DImode"
  "")

;; For o32/n32/n64, we need to arrange for longjmp to put the 
;; target address in t9 so that we can use it for loading $gp.

(define_expand "builtin_longjmp"
  [(unspec_volatile [(match_operand 0 "register_operand" "r")] 3)]
  "TARGET_ABICALLS"
  "
{
  /* The elements of the buffer are, in order:  */
  int W = (Pmode == DImode ? 8 : 4);
  rtx fp = gen_rtx_MEM (Pmode, operands[0]);
  rtx lab = gen_rtx_MEM (Pmode, plus_constant (operands[0], 1*W));
  rtx stack = gen_rtx_MEM (Pmode, plus_constant (operands[0], 2*W));
  rtx gpv = gen_rtx_MEM (Pmode, plus_constant (operands[0], 3*W));
  rtx pv = gen_rtx_REG (Pmode, 25);
  rtx gp = gen_rtx_REG (Pmode, 28);

  /* This bit is the same as expand_builtin_longjmp.  */
  emit_move_insn (hard_frame_pointer_rtx, fp);
  emit_move_insn (pv, lab);
  emit_stack_restore (SAVE_NONLOCAL, stack, NULL_RTX);
  emit_move_insn (gp, gpv);
  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
  emit_insn (gen_rtx_USE (VOIDmode, gp));
  emit_indirect_jump (pv);
  DONE;
}")

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
  "
{
  if (mips_isa >= 0)		/* avoid unused code warnings */
    {
      mips_expand_prologue ();
      DONE;
    }
}")

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  ""
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"none")
   (set_attr "length"	"0")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
  "
{
  if (mips_isa >= 0)            /* avoid unused code warnings */
    {
      mips_expand_epilogue ();
      DONE;
    }
}")

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better .
(define_insn "return"
  [(return)]
  "mips_can_use_return_insn ()"
  "%*j\\t$31"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "return_internal"
  [(use (match_operand 0 "pmode_register_operand" ""))
   (return)]
  ""
  "*
{
  return \"%*j\\t%0\";
}"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])
  
;; When generating embedded PIC code we need to get the address of the
;; current function.  This specialized instruction does just that.

(define_insn "get_fnaddr"
  [(set (match_operand 0 "register_operand" "=d")
	(unspec [(match_operand 1 "" "")] 1))
   (clobber (reg:SI 31))]
  "TARGET_EMBEDDED_PIC
   && GET_CODE (operands[1]) == SYMBOL_REF"
  "%($LF%= = . + 8\;bal\\t$LF%=\;la\\t%0,%1-$LF%=%)\;addu\\t%0,%0,$31"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"16")])


;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

;; calls.c now passes a third argument, make saber happy

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "m")
		    (match_operand 1 "" "i"))
	      (clobber (reg:SI 31))
	      (use (match_operand 2 "" ""))		;; next_arg_reg
	      (use (match_operand 3 "" ""))])]		;; struct_value_size_rtx
  ""
  "
{
  rtx addr;

  if (operands[0])		/* eliminate unused code warnings */
    {
      addr = XEXP (operands[0], 0);
      if ((GET_CODE (addr) != REG && (!CONSTANT_ADDRESS_P (addr) || TARGET_LONG_CALLS))
	  || ! call_insn_operand (addr, VOIDmode))
	XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, addr);

      /* In order to pass small structures by value in registers
	 compatibly with the MIPS compiler, we need to shift the value
	 into the high part of the register.  Function_arg has encoded
	 a PARALLEL rtx, holding a vector of adjustments to be made
	 as the next_arg_reg variable, so we split up the insns,
	 and emit them separately.  */

      if (operands[2] != (rtx)0 && GET_CODE (operands[2]) == PARALLEL)
	{
	  rtvec adjust = XVEC (operands[2], 0);
	  int num = GET_NUM_ELEM (adjust);
	  int i;

	  for (i = 0; i < num; i++)
	    emit_insn (RTVEC_ELT (adjust, i));
	}

      if (TARGET_MIPS16
	  && mips16_hard_float
	  && operands[2] != 0
	  && (int) GET_MODE (operands[2]) != 0)
	{
	  if (build_mips16_call_stub (NULL_RTX, operands[0], operands[1],
				      (int) GET_MODE (operands[2])))
	    DONE;
	}

      emit_call_insn (gen_call_internal0 (operands[0], operands[1],
					  gen_rtx_REG (SImode,
						       GP_REG_FIRST + 31)));
      DONE;
    }
}")

(define_expand "call_internal0"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (clobber (match_operand:SI 2 "" ""))])]
  ""
  "")

;; We need to recognize reg:SI 31 specially for the mips16, because we
;; don't have a constraint letter for it.

(define_insn ""
  [(call (mem (match_operand 0 "call_insn_operand" "ei"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=y"))]
  "TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31"
  "%*jal\\t%0"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn "call_internal1"
  [(call (mem (match_operand 0 "call_insn_operand" "ri"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "!TARGET_ABICALLS && !TARGET_LONG_CALLS"
  "*
{
  register rtx target = operands[0];

  if (GET_CODE (target) == SYMBOL_REF)
    return \"%*jal\\t%0\";
  else if (GET_CODE (target) == CONST_INT)
    return \"%[li\\t%@,%0\\n\\t%*jal\\t%2,%@%]\";
  else
    return \"%*jal\\t%2,%0\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_internal2"
  [(call (mem (match_operand 0 "call_insn_operand" "ri"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "TARGET_ABICALLS && !TARGET_LONG_CALLS"
  "*
{
  register rtx target = operands[0];

  if (GET_CODE (target) == SYMBOL_REF)
    {
      if (GET_MODE (target) == SImode)
	return \"la\\t%^,%0\\n\\tjal\\t%2,%^\";
      else
	return \"dla\\t%^,%0\\n\\tjal\\t%2,%^\";
    }
  else if (GET_CODE (target) == CONST_INT)
    return \"li\\t%^,%0\\n\\tjal\\t%2,%^\";
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%0\\n\\tjal\\t%2,%^\";
  else
    return \"jal\\t%2,%0\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn "call_internal3a"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "!TARGET_MIPS16
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS"
  "%*jal\\t%2,%0"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_internal3b"
  [(call (mem:DI (match_operand:DI 0 "se_register_operand" "r"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "!TARGET_MIPS16
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS"
  "%*jal\\t%2,%0"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_insn "call_internal3c"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "e"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=y"))]
  "TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31"
  "%*jal\\t%2,%0"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_internal4a"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS"
  "*
{
  if (REGNO (operands[0]) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%0\\n\\tjal\\t%2,%^\";
  else
    return \"jal\\t%2,%0\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn "call_internal4b"
  [(call (mem:DI (match_operand:DI 0 "se_register_operand" "r"))
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS"
  "*
{
  if (REGNO (operands[0]) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%0\\n\\tjal\\t%2,%^\";
  else
    return \"jal\\t%2,%0\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

;; calls.c now passes a fourth argument, make saber happy

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "=df")
		   (call (match_operand 1 "memory_operand" "m")
			 (match_operand 2 "" "i")))
	      (clobber (reg:SI 31))
	      (use (match_operand 3 "" ""))])]		;; next_arg_reg
  ""
  "
{
  rtx addr;

  if (operands[0])		/* eliminate unused code warning */
    {
      addr = XEXP (operands[1], 0);
      if ((GET_CODE (addr) != REG && (!CONSTANT_ADDRESS_P (addr) || TARGET_LONG_CALLS))
	  || ! call_insn_operand (addr, VOIDmode))
	XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, addr);

      /* In order to pass small structures by value in registers
	 compatibly with the MIPS compiler, we need to shift the value
	 into the high part of the register.  Function_arg has encoded
	 a PARALLEL rtx, holding a vector of adjustments to be made
	 as the next_arg_reg variable, so we split up the insns,
	 and emit them separately.  */

      if (operands[3] != (rtx)0 && GET_CODE (operands[3]) == PARALLEL)
	{
	  rtvec adjust = XVEC (operands[3], 0);
	  int num = GET_NUM_ELEM (adjust);
	  int i;

	  for (i = 0; i < num; i++)
	    emit_insn (RTVEC_ELT (adjust, i));
	}

      if (TARGET_MIPS16
	  && mips16_hard_float
	  && ((operands[3] != 0
	       && (int) GET_MODE (operands[3]) != 0)
	      || GET_MODE_CLASS (GET_MODE (operands[0])) == MODE_FLOAT))
	{
	  if (build_mips16_call_stub (operands[0], operands[1], operands[2],
				      (operands[3] == 0 ? 0
				       : (int) GET_MODE (operands[3]))))
	    DONE;
	}

      /* Handle Irix6 function calls that have multiple non-contiguous
	 results.  */
      if (GET_CODE (operands[0]) == PARALLEL && XVECLEN (operands[0], 0) > 1)
	{
	  emit_call_insn (gen_call_value_multiple_internal0
			  (XEXP (XVECEXP (operands[0], 0, 0), 0),
			   operands[1], operands[2],
			   XEXP (XVECEXP (operands[0], 0, 1), 0),
			   gen_rtx_REG (SImode, GP_REG_FIRST + 31)));
	  DONE;
	}

      /* We have a call returning a DImode structure in an FP reg.
	 Strip off the now unnecessary PARALLEL.  */
      if (GET_CODE (operands[0]) == PARALLEL)
	operands[0] = XEXP (XVECEXP (operands[0], 0, 0), 0);

      emit_call_insn (gen_call_value_internal0 (operands[0], operands[1], operands[2],
					        gen_rtx_REG (SImode,
							     GP_REG_FIRST + 31)));

      DONE;
    }
}")

(define_expand "call_value_internal0"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (clobber (match_operand:SI 3 "" ""))])]
  ""
  "")

;; Recognize $31 specially on the mips16, because we don't have a
;; constraint letter for it.

(define_insn ""
  [(set (match_operand 0 "register_operand" "=d")
        (call (mem (match_operand 1 "call_insn_operand" "ei"))
              (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=y"))]
  "TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31"
  "%*jal\\t%1"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn "call_value_internal1"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem (match_operand 1 "call_insn_operand" "ri"))
              (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_ABICALLS && !TARGET_LONG_CALLS"
  "*
{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    return \"%*jal\\t%1\";
  else if (GET_CODE (target) == CONST_INT)
    return \"%[li\\t%@,%1\\n\\t%*jal\\t%3,%@%]\";
  else
    return \"%*jal\\t%3,%1\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_value_internal2"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem (match_operand 1 "call_insn_operand" "ri"))
              (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "TARGET_ABICALLS && !TARGET_LONG_CALLS"
  "*
{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    {
      if (GET_MODE (target) == SImode)
	return \"la\\t%^,%1\\n\\tjal\\t%3,%^\";
      else
	return \"dla\\t%^,%1\\n\\tjal\\t%3,%^\";
    }
  else if (GET_CODE (target) == CONST_INT)
    return \"li\\t%^,%1\\n\\tjal\\t%3,%^\";
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%1\\n\\tjal\\t%3,%^\";
  else
    return \"jal\\t%3,%1\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn "call_value_internal3a"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:SI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_MIPS16 
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS"
  "%*jal\\t%3,%1"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_value_internal3b"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:DI (match_operand:DI 1 "se_register_operand" "r"))
	      (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_MIPS16 
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS"
  "%*jal\\t%3,%1"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_value_internal3c"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:SI (match_operand:SI 1 "register_operand" "e"))
	      (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=y"))]
  "TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31"
  "%*jal\\t%3,%1"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_value_internal4a"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:SI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS"
  "*
{
  if (REGNO (operands[1]) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%1\\n\\tjal\\t%3,%^\";
  else
    return \"jal\\t%3,%1\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_insn "call_value_internal4b"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem:DI (match_operand:DI 1 "se_register_operand" "r"))
	      (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS"
  "*
{
  if (REGNO (operands[1]) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%1\\n\\tjal\\t%3,%^\";
  else
    return \"jal\\t%3,%1\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

(define_expand "call_value_multiple_internal0"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand 2 "" "")))
	      (set (match_operand 3 "" "")
		   (call (match_dup 1)
			 (match_dup 2)))
	      (clobber (match_operand:SI 4 "" ""))])]
  ""
  "")

;; ??? May eventually need all 6 versions of the call patterns with multiple
;; return values.

(define_insn "call_value_multiple_internal1"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem (match_operand 1 "call_insn_operand" "ri"))
              (match_operand 2 "" "i")))
   (set (match_operand 3 "register_operand" "=df")
   	(call (mem (match_dup 1))
              (match_dup 2)))
  (clobber (match_operand:SI 4 "register_operand" "=d"))]
  "!TARGET_ABICALLS && !TARGET_LONG_CALLS"
  "*
{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    return \"%*jal\\t%1\";
  else if (GET_CODE (target) == CONST_INT)
    return \"%[li\\t%@,%1\\n\\t%*jal\\t%4,%@%]\";
  else
    return \"%*jal\\t%4,%1\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")])

(define_insn "call_value_multiple_internal2"
  [(set (match_operand 0 "register_operand" "=df")
        (call (mem (match_operand 1 "call_insn_operand" "ri"))
              (match_operand 2 "" "i")))
   (set (match_operand 3 "register_operand" "=df")
        (call (mem (match_dup 1))
              (match_dup 2)))
   (clobber (match_operand:SI 4 "register_operand" "=d"))]
  "TARGET_ABICALLS && !TARGET_LONG_CALLS"
  "*
{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    {
      if (GET_MODE (target) == SImode)
	return \"la\\t%^,%1\\n\\tjal\\t%4,%^\";
      else
	return \"la\\t%^,%1\\n\\tjal\\t%4,%^\";
    }
  else if (GET_CODE (target) == CONST_INT)
    return \"li\\t%^,%1\\n\\tjal\\t%4,%^\";
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return \"move\\t%^,%1\\n\\tjal\\t%4,%^\";
  else
    return \"jal\\t%4,%1\";
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])


;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  if (operands[0])		/* silence statement not reached warnings */
    {
      int i;

      emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

      for (i = 0; i < XVECLEN (operands[2], 0); i++)
	{
	  rtx set = XVECEXP (operands[2], 0, i);
	  emit_move_insn (SET_DEST (set), SET_SRC (set));
	}

      emit_insn (gen_blockage ());
      DONE;
    }
}")

;;
;;  ....................
;;
;;	MISC.
;;
;;  ....................
;;

(define_insn "nop"
  [(const_int 0)]
  ""
  "%(nop%)"
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")])

;; The MIPS chip does not seem to require stack probes.
;;
;; (define_expand "probe"
;;   [(set (match_dup 0)
;; 	(match_dup 1))]
;;   ""
;;   "
;; {
;;   operands[0] = gen_reg_rtx (SImode);
;;   operands[1] = gen_rtx_MEM (SImode, stack_pointer_rtx);
;;   MEM_VOLATILE_P (operands[1]) = TRUE;
;; 
;;   /* fall through and generate default code */
;; }")
;;

;;
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
    mov%B4\\t%0,%z2,%1
    mov%b4\\t%0,%z3,%1"
  [(set_attr "type" "move")
   (set_attr "mode" "SI")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(if_then_else:SI
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "se_register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:SI 2 "reg_or_0_operand" "dJ,0")
	 (match_operand:SI 3 "reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
  "@
    mov%B4\\t%0,%z2,%1
    mov%b4\\t%0,%z3,%1"
  [(set_attr "type" "move")
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
    mov%T3\\t%0,%z1,%4
    mov%t3\\t%0,%z2,%4"
  [(set_attr "type" "move")
   (set_attr "mode" "SI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(if_then_else:DI
	 (match_operator 4 "equality_op"
			 [(match_operand:SI 1 "register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DI 2 "se_reg_or_0_operand" "dJ,0")
	 (match_operand:DI 3 "se_reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
  "@
    mov%B4\\t%0,%z2,%1
    mov%b4\\t%0,%z3,%1"
  [(set_attr "type" "move")
   (set_attr "mode" "DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(if_then_else:DI
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "se_register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DI 2 "se_reg_or_0_operand" "dJ,0")
	 (match_operand:DI 3 "se_reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
  "@
    mov%B4\\t%0,%z2,%1
    mov%b4\\t%0,%z3,%1"
  [(set_attr "type" "move")
   (set_attr "mode" "DI")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(if_then_else:DI
	 (match_operator 3 "equality_op" [(match_operand:CC 4
							    "register_operand"
							    "z,z")
					  (const_int 0)])
	 (match_operand:DI 1 "se_reg_or_0_operand" "dJ,0")
	 (match_operand:DI 2 "se_reg_or_0_operand" "0,dJ")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "@
    mov%T3\\t%0,%z1,%4
    mov%t3\\t%0,%z2,%4"
  [(set_attr "type" "move")
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
    mov%B4.s\\t%0,%2,%1
    mov%b4.s\\t%0,%3,%1"
  [(set_attr "type" "move")
   (set_attr "mode" "SF")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "se_register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:SF 2 "register_operand" "f,0")
	 (match_operand:SF 3 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "@
    mov%B4.s\\t%0,%2,%1
    mov%b4.s\\t%0,%3,%1"
  [(set_attr "type" "move")
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
    mov%T3.s\\t%0,%1,%4
    mov%t3.s\\t%0,%2,%4"
  [(set_attr "type" "move")
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
    mov%B4.d\\t%0,%2,%1
    mov%b4.d\\t%0,%3,%1"
  [(set_attr "type" "move")
   (set_attr "mode" "DF")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(if_then_else:DF
	 (match_operator 4 "equality_op"
			 [(match_operand:DI 1 "se_register_operand" "d,d")
			  (const_int 0)])
	 (match_operand:DF 2 "register_operand" "f,0")
	 (match_operand:DF 3 "register_operand" "0,f")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "@
    mov%B4.d\\t%0,%2,%1
    mov%b4.d\\t%0,%3,%1"
  [(set_attr "type" "move")
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
    mov%T3.d\\t%0,%1,%4
    mov%t3.d\\t%0,%2,%4"
  [(set_attr "type" "move")
   (set_attr "mode" "DF")])

;; These are the main define_expand's used to make conditional moves.

(define_expand "movsicc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI (match_dup 5)
			 (match_operand:SI 2 "reg_or_0_operand" "")
			 (match_operand:SI 3 "reg_or_0_operand" "")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE"
  "
{
  gen_conditional_move (operands);
  DONE;
}")

(define_expand "movdicc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (match_dup 5)
			 (match_operand:DI 2 "se_reg_or_0_operand" "")
			 (match_operand:DI 3 "se_reg_or_0_operand" "")))]
  "ISA_HAS_CONDMOVE || ISA_HAS_INT_CONDMOVE" 
  "
{
  gen_conditional_move (operands);
  DONE;
}")

(define_expand "movsfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (match_dup 5)
			 (match_operand:SF 2 "register_operand" "")
			 (match_operand:SF 3 "register_operand" "")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT"
  "
{
  gen_conditional_move (operands);
  DONE;
}")

(define_expand "movdfcc"
  [(set (match_dup 4) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (match_dup 5)
			 (match_operand:DF 2 "register_operand" "")
			 (match_operand:DF 3 "register_operand" "")))]
  "ISA_HAS_CONDMOVE && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "
{
  gen_conditional_move (operands);
  DONE;
}")

;;
;;  ....................
;;
;;	mips16 inline constant tables
;;
;;  ....................
;;

(define_insn "consttable_qi"
  [(unspec_volatile [(match_operand:QI 0 "consttable_operand" "=g")] 10)]
  "TARGET_MIPS16"
  "*
{
  assemble_integer (operands[0], 1, 1);
  return \"\";
}"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"QI")
   (set_attr "length"	"8")])

(define_insn "consttable_hi"
  [(unspec_volatile [(match_operand:HI 0 "consttable_operand" "=g")] 11)]
  "TARGET_MIPS16"
  "*
{
  assemble_integer (operands[0], 2, 1);
  return \"\";
}"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"HI")
   (set_attr "length"	"8")])

(define_insn "consttable_si"
  [(unspec_volatile [(match_operand:SI 0 "consttable_operand" "=g")] 12)]
  "TARGET_MIPS16"
  "*
{
  assemble_integer (operands[0], 4, 1);
  return \"\";
}"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_insn "consttable_di"
  [(unspec_volatile [(match_operand:DI 0 "consttable_operand" "=g")] 13)]
  "TARGET_MIPS16"
  "*
{
  assemble_integer (operands[0], 8, 1);
  return \"\";
}"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"DI")
   (set_attr "length"	"16")])

(define_insn "consttable_sf"
  [(unspec_volatile [(match_operand:SF 0 "consttable_operand" "=g")] 14)]
  "TARGET_MIPS16"
  "*
{
  union real_extract u;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  bcopy ((char *) &CONST_DOUBLE_LOW (operands[0]), (char *) &u, sizeof u);
  assemble_real (u.d, SFmode);
  return \"\";
}"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"SF")
   (set_attr "length"	"8")])

(define_insn "consttable_df"
  [(unspec_volatile [(match_operand:DF 0 "consttable_operand" "=g")] 15)]
  "TARGET_MIPS16"
  "*
{
  union real_extract u;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  bcopy ((char *) &CONST_DOUBLE_LOW (operands[0]), (char *) &u, sizeof u);
  assemble_real (u.d, DFmode);
  return \"\";
}"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"DF")
   (set_attr "length"	"16")])

(define_insn "align_2"
  [(unspec_volatile [(const_int 0)] 16)]
  "TARGET_MIPS16"
  ".align 1"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"HI")
   (set_attr "length"	"8")])

(define_insn "align_4"
  [(unspec_volatile [(const_int 0)] 17)]
  "TARGET_MIPS16"
  ".align 2"
  [(set_attr "type"	"unknown")
   (set_attr "mode"	"SI")
   (set_attr "length"	"8")])

(define_insn "align_8"
  [(unspec_volatile [(const_int 0)] 18)]
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
  "*
{
  if (operands[3] != pc_rtx)
    return \"%*b%C2z\\t%1,%3\";
  else
    return \"%*b%N2z\\t%1,%4\";
}"
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
  "*
{
  if (operands[3] != pc_rtx)
    return \"%*b%C2z\\t%1,%3\";
  else
    return \"%*b%N2z\\t%1,%4\";
}"
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
  "*
{
  if (operands[3] != pc_rtx)
    return \"%*bt%C2z\\t%3\";
  else
    return \"%*bt%N2z\\t%4\";
}"
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
  "*
{
  if (operands[3] != pc_rtx)
    return \"%*bt%C2z\\t%3\";
  else
    return \"%*bt%N2z\\t%4\";
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"8")])

;; For the rare case where we need to load an address into a register
;; that can not be recognized by the normal movsi/addsi instructions.
;; I have no idea how many insns this can actually generate.  It should
;; be rare, so over-estimating as 10 instructions should not have any
;; real performance impact.
(define_insn "leasi"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (match_operand:SI 1 "address_operand" "p"))]
  "Pmode == SImode"
  "la %0,%a1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"40")])

;; Similarly for targets where we have 64bit pointers.
(define_insn "leadi"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (match_operand:DI 1 "address_operand" "p"))]
  "Pmode == DImode"
  "la %0,%a1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"40")])
