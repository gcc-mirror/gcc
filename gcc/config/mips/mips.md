;;  Mips.md	     Machine Description for MIPS based processors
;;  Contributed by   A. Lichnewsky, lich@inria.inria.fr
;;  Changes by       Michael Meissner, meissner@osf.org
;;  Copyright (C) 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.



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
;; fdiv		floating point divide
;; fabs		floating point absolute value
;; fneg		floating point negation
;; fcmp		floating point compare
;; fcvt		floating point convert
;; fsqrt	floating point square root
;; multi	multiword sequence (or user asm statements)
;; nop		no operation

(define_attr "type"
  "unknown,branch,jump,call,load,store,move,xfer,hilo,arith,darith,imul,idiv,icmp,fadd,fmul,fdiv,fabs,fneg,fcmp,fcvt,fsqrt,multi,nop"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,SF,DF,FPSW" (const_string "unknown"))

;; # instructions (4 bytes each)
(define_attr "length" "" (const_int 1))

;; whether or not an instruction has a mandatory delay slot
(define_attr "dslot" "no,yes"
  (if_then_else (eq_attr "type" "branch,jump,call,load,xfer,hilo,fcmp")
		(const_string "yes")
		(const_string "no")))

;; Attribute describing the processor.  This attribute must match exactly
;; with the processor_type enumeration in mips.h.

;; Attribute describing the processor
;; (define_attr "cpu" "default,r3000,r6000,r4000"
;;   (const
;;    (cond [(eq (symbol_ref "mips_cpu") (symbol_ref "PROCESSOR_R3000"))   (const_string "r3000")
;;           (eq (symbol_ref "mips_cpu") (symbol_ref "PROCESSOR_R4000"))   (const_string "r4000")
;;           (eq (symbol_ref "mips_cpu") (symbol_ref "PROCESSOR_R6000"))   (const_string "r6000")]
;;          (const_string "default"))))

(define_attr "cpu" "default,r3000,r6000,r4000"
  (const (symbol_ref "mips_cpu_attr")))

;; Attribute defining whether or not we can use the branch-likely instructions
;; (MIPS ISA level 2)

(define_attr "branch_likely" "no,yes"
  (const
   (if_then_else (ge (symbol_ref "mips_isa") (const_int 2))
		 (const_string "yes")
		 (const_string "no"))))


;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])



;; .........................
;;
;;	Delay slots, can't describe load/fcmp/xfer delay slots here
;;
;; .........................

(define_delay (eq_attr "type" "branch")
  [(and (eq_attr "dslot" "no") (eq_attr "length" "1"))
   (nil)
   (and (eq_attr "branch_likely" "yes") (and (eq_attr "dslot" "no") (eq_attr "length" "1")))])

(define_delay (eq_attr "type" "call,jump")
  [(and (eq_attr "dslot" "no") (eq_attr "length" "1"))
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
  (and (eq_attr "type" "load") (eq_attr "cpu" "!r3000"))
  3 0)

(define_function_unit "memory" 1 0
  (and (eq_attr "type" "load") (eq_attr "cpu" "r3000"))
  2 0)

(define_function_unit "memory"   1 0 (eq_attr "type" "store") 1 0)

(define_function_unit "addr"     1 0 (eq_attr "type" "fcmp") 2 0)

(define_function_unit "memory"   1 0 (eq_attr "type" "xfer") 2 0)
(define_function_unit "memory"   1 0 (eq_attr "type" "hilo") 3 0)

(define_function_unit "imuldiv"  1 1
  (and (eq_attr "type" "imul") (eq_attr "cpu" "!r3000,r4000"))
  17 0)

(define_function_unit "imuldiv"  1 1
  (and (eq_attr "type" "imul") (eq_attr "cpu" "r3000"))
  12 0)

(define_function_unit "imuldiv" 1 1
  (and (eq_attr "type" "imul") (eq_attr "cpu" "r4000"))
  10 0)

(define_function_unit "imuldiv"  1 1
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "!r3000,r4000"))
  38 0)

(define_function_unit "imuldiv"  1 1
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "r3000"))
  35 0)

(define_function_unit "imuldiv" 1 1
  (and (eq_attr "type" "idiv") (eq_attr "cpu" "r4000"))
  69 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "!r3000,r6000"))
  4 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "r3000"))
  2 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fadd") (eq_attr "cpu" "r6000"))
  3 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fabs,fneg") (eq_attr "cpu" "!r3000"))
  2 0)

(define_function_unit "adder" 1 1
  (and (eq_attr "type" "fabs,fneg") (eq_attr "cpu" "r3000"))
  1 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "SF") (eq_attr "cpu" "!r3000,r6000")))
  7 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "SF") (eq_attr "cpu" "r3000")))
  4 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "SF") (eq_attr "cpu" "r6000")))
  5 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "DF") (eq_attr "cpu" "!r3000,r6000")))
  8 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "DF") (eq_attr "cpu" "r3000")))
  5 0)

(define_function_unit "mult" 1 1
  (and (eq_attr "type" "fmul") (and (eq_attr "mode" "DF") (eq_attr "cpu" "r6000")))
  6 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv") (and (eq_attr "mode" "SF") (eq_attr "cpu" "!r3000,r6000")))
  23 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv") (and (eq_attr "mode" "SF") (eq_attr "cpu" "r3000")))
  12 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv") (and (eq_attr "mode" "SF") (eq_attr "cpu" "r6000")))
  15 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv") (and (eq_attr "mode" "DF") (eq_attr "cpu" "!r3000,r6000")))
  36 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv") (and (eq_attr "mode" "DF") (eq_attr "cpu" "r3000")))
  19 0)

(define_function_unit "divide" 1 1
  (and (eq_attr "type" "fdiv") (and (eq_attr "mode" "DF") (eq_attr "cpu" "r6000")))
  16 0)

(define_function_unit "divide" 1 1 (and (eq_attr "type" "fsqrt") (eq_attr "mode" "SF"))  54 0)
(define_function_unit "divide" 1 1 (and (eq_attr "type" "fsqrt") (eq_attr "mode" "DF")) 112 0)


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
  "TARGET_HARD_FLOAT"
  "add.d\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "add.s\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		 (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == -32768)
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn "addsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		 (match_operand:SI 2 "arith_operand" "dI")))]
  "GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768"
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"subu\\t%0,%z1,%n2\"
    : \"addu\\t%0,%z1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "adddi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (plus:DI (match_operand:DI 1 "register_operand" "")
			    (match_operand:DI 2 "arith_operand" "")))
	      (clobber (match_dup 3))])]
  "!TARGET_DEBUG_G_MODE"
  "
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == -32768)
    operands[2] = force_reg (SImode, operands[2]);

  operands[3] = gen_reg_rtx (SImode);
}")

(define_insn "adddi3_internal_1"
  [(set (match_operand:DI 0 "register_operand" "=d,&d")
	(plus:DI (match_operand:DI 1 "register_operand" "0,d")
		 (match_operand:DI 2 "register_operand" "d,d")))
   (clobber (match_operand:SI 3 "register_operand" "=d,d"))]
  "!TARGET_DEBUG_G_MODE"
  "*
{
  return (REGNO (operands[0]) == REGNO (operands[1])
	  && REGNO (operands[0]) == REGNO (operands[2]))
    ? \"srl\\t%3,%L0,31\;sll\\t%M0,%M0,1\;sll\\t%L0,%L1,1\;addu\\t%M0,%M0,%3\"
    : \"addu\\t%L0,%L1,%L2\;sltu\\t%3,%L0,%L2\;addu\\t%M0,%M1,%M2\;addu\\t%M0,%M0,%3\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "register_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "!TARGET_DEBUG_G_MODE && INTVAL (operands[2]) != -32768"
  "@
   addu\\t%L0,%L1,%2\;sltu\\t%3,%L0,%2\;addu\\t%M0,%M1,%3
   move\\t%L0,%L1\;move\\t%M0,%M1
   subu\\t%L0,%L1,%n2\;sltu\\t%3,%L0,%2\;subu\\t%M0,%M1,1\;addu\\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"3,2,4")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(plus:DI (match_operand:DI 1 "register_operand" "")
		 (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "TARGET_HARD_FLOAT"
  "sub.d\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "sub.s\\t%0,%1,%2"
  [(set_attr "type"	"fadd")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		  (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == -32768)
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn "subsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
		  (match_operand:SI 2 "arith_operand" "dI")))]
  "GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768"
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"addu\\t%0,%z1,%n2\"
    : \"subu\\t%0,%z1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "subdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=d")
		   (minus:DI (match_operand:DI 1 "register_operand" "d")
			     (match_operand:DI 2 "register_operand" "d")))
	      (clobber (match_dup 3))])]
  "!TARGET_DEBUG_G_MODE"
  "operands[3] = gen_reg_rtx (SImode);")

(define_insn "subdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE"
  "sltu\\t%3,%L1,%L2\;subu\\t%L0,%L1,%L2\;subu\\t%M0,%M1,%M2\;subu\\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "register_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "!TARGET_DEBUG_G_MODE && INTVAL (operands[2]) != -32768"
  "@
   sltu\\t%3,%L1,%2\;subu\\t%L0,%L1,%2\;subu\\t%M0,%M1,%3
   move\\t%L0,%L1\;move\\t%M0,%M1
   sltu\\t%3,%L1,%2\;subu\\t%L0,%L1,%2\;subu\\t%M0,%M1,1\;subu\\t%M0,%M0,%3"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"3,2,4")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(minus:DI (match_operand:DI 1 "register_operand" "")
		  (match_operand:DI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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


;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mul.d\\t%0,%1,%2"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mul.s\\t%0,%1,%2"
  [(set_attr "type"	"fmul")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  ""
  "*
{
  rtx xoperands[10];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx (REG, SImode, LO_REGNUM);

  output_asm_insn (\"mult\\t%1,%2\", operands);
  output_asm_insn (mips_move_1word (xoperands, insn), xoperands);
  return \"\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])		;; mult + mflo + delay

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "!TARGET_DEBUG_D_MODE"
  [(parallel [(set (reg:SI 65)		;; low register
		   (mult:SI (match_dup 1)
			    (match_dup 2)))
	      (clobber (reg:SI 64))])
   (set (match_dup 0)
	(reg:SI 65))]
  "")

(define_insn "mulsi3_internal"
  [(set (reg:SI 65)		;; low register
	(mult:SI (match_operand:SI 0 "register_operand" "d")
		 (match_operand:SI 1 "register_operand" "d")))
   (clobber (reg:SI 64))]
  ""
  "mult\\t%0,%1"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (reg:DI 64))]
  ""
  "*
{
  rtx xoperands[10];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx (REG, DImode, MD_REG_FIRST);

  output_asm_insn (\"mult\\t%1,%2\", operands);
  output_asm_insn (mips_move_2words (xoperands, insn), xoperands);
  return \"\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")])		;; mult + mflo + mfhi + delay

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "d"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "d"))))
   (clobber (reg:DI 64))]
  ""
  "*
{
  rtx xoperands[10];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx (REG, DImode, MD_REG_FIRST);

  output_asm_insn (\"multu\\t%1,%2\", operands);
  output_asm_insn (mips_move_2words (xoperands, insn), xoperands);
  return \"\";
}"
  [(set_attr "type"	"imul")
   (set_attr "mode"	"SI")
   (set_attr "length"	"4")])		;; mult + mflo + mfhi + delay


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
  "TARGET_HARD_FLOAT"
  "div.d\\t%0,%1,%2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "div.s\\t%0,%1,%2"
  [(set_attr "type"	"fdiv")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])

;; If optimizing, prefer the divmod functions over separate div and
;; mod functions, since this will allow using one instruction for both
;; the quotient and remainder.  At present, the divmod is not moved out
;; of loops if it is constant within the loop, so allow -mdebugc to
;; use the old method of doing things.

;; 64 is the multiply/divide hi register
;; 65 is the multiply/divide lo register

(define_insn "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=d")
	(mod:SI (match_dup 1)
		(match_dup 2)))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "optimize"
  "*
{
  if (find_reg_note (insn, REG_UNUSED, operands[3]))
    return \"div\\t%0,%1,%2\";

  if (find_reg_note (insn, REG_UNUSED, operands[0]))
    return \"rem\\t%3,%1,%2\";

  return \"div\\t%0,%1,%2\;mfhi\\t%3\";
}"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")
   (set_attr "length"	"13")])		;; various tests for dividing by 0 and such

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=d")
	(umod:SI (match_dup 1)
		 (match_dup 2)))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "optimize"
  "*
{
  if (find_reg_note (insn, REG_UNUSED, operands[3]))
    return \"divu\\t%0,%1,%2\";

  if (find_reg_note (insn, REG_UNUSED, operands[0]))
    return \"remu\\t%3,%1,%2\";

  return \"divu\\t%0,%1,%2\;mfhi\\t%3\";
}"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")
   (set_attr "length"	"13")])		;; various tests for dividing by 0 and such

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(div:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "!optimize"
  "div\\t%0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")
   (set_attr "length"	"13")])		;; various tests for dividing by 0 and such

(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mod:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "!optimize"
  "rem\\t%0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")
   (set_attr "length"	"14")])		;; various tests for dividing by 0 and such

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(udiv:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "!optimize"
  "divu\\t%0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")
   (set_attr "length"	"14")])		;; various tests for dividing by 0 and such

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(umod:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))
   (clobber (reg:SI 64))
   (clobber (reg:SI 65))]
  "!optimize"
  "remu\\t%0,%1,%2"
  [(set_attr "type"	"idiv")
   (set_attr "mode"	"SI")
   (set_attr "length"	"14")])		;; various tests for dividing by 0 and such


;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && HAVE_SQRT_P()"
  "sqrt.d\\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT && HAVE_SQRT_P()"
  "sqrt.s\\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])


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
  ""
  "*
{
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  return (REGNO (operands[0]) == REGNO (operands[1]))
		? \"bgez\\t%1,1f%#\\n\\tsubu\\t%0,%z2,%0\\n1:\"
		: \"%(bgez\\t%1,1f\\n\\tmove\\t%0,%1\\n\\tsubu\\t%0,%z2,%0\\n1:%)\";
}"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"3")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs.d\\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs.s\\t%0,%1"
  [(set_attr "type"	"fabs")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])


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
  ""
  "*
{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return \"%(\\
move\\t%0,%z4\\n\\
\\tbeq\\t%1,%z4,2f\\n\\
1:\\tand\\t%2,%1,0x0001\\n\\
\\taddu\\t%0,%0,1\\n\\
\\tbeq\\t%2,%z4,1b\\n\\
\\tsrl\\t%1,%1,1\\n\\
2:%)\";

  return \"%(\\
move\\t%0,%z4\\n\\
\\tmove\\t%3,%1\\n\\
\\tbeq\\t%3,%z4,2f\\n\\
1:\\tand\\t%2,%3,0x0001\\n\\
\\taddu\\t%0,%0,1\\n\\
\\tbeq\\t%2,%z4,1b\\n\\
\\tsrl\\t%3,%3,1\\n\\
2:%)\";
}"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"SI")
   (set_attr "length"	"6")])


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
  operands[2] = const0_rtx;
  return \"subu\\t%0,%z2,%1\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "negdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=d")
		   (neg:DI (match_operand:DI 1 "register_operand" "d")))
	      (clobber (match_dup 2))])]
  "!TARGET_DEBUG_G_MODE"
  "operands[2] = gen_reg_rtx (SImode);")

(define_insn "negdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE"
  "*
{
  operands[3] = const0_rtx;
  return \"subu\\t%L0,%z3,%L1\;subu\\t%M0,%z3,%M1\;sltu\\t%2,%z3,%L0\;subu\\t%M0,%M0,%2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "neg.d\\t%0,%1"
  [(set_attr "type"	"fneg")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "neg.s\\t%0,%1"
  [(set_attr "type"	"fneg")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(not:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "*
{
  operands[2] = const0_rtx;
  return \"nor\\t%0,%z2,%1\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:SI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "*
{
  operands[2] = const0_rtx;
  return \"nor\\t%M0,%z2,%M1\;nor\\t%L0,%z2,%L1\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (match_operand:DI 1 "register_operand" "")))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"

  [(set (subreg:SI (match_dup 0) 0) (not:SI (subreg:SI (match_dup 1) 0)))
   (set (subreg:SI (match_dup 0) 1) (not:SI (subreg:SI (match_dup 1) 1)))]
  "")

;; Simple hack to recognize the "nor" instruction on the MIPS
;; This must appear before the normal or patterns, so that the
;; combiner will correctly fold things.

(define_insn "norsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(not:SI (ior:SI (match_operand:SI 1 "reg_or_0_operand" "dJ")
			(match_operand:SI 2 "reg_or_0_operand" "dJ"))))]
  ""
  "nor\\t%0,%z1,%z2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "nordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:DI (ior:DI (match_operand:DI 1 "register_operand" "d")
			(match_operand:DI 2 "register_operand" "d"))))]
  ""
  "nor\\t%M0,%M1,%M2\;nor\\t%L0,%L1,%L2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (ior:DI (match_operand:DI 1 "register_operand" "")
			(match_operand:DI 2 "register_operand" ""))))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (not:SI (ior:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0))))
   (set (subreg:SI (match_dup 0) 1) (not:SI (ior:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1))))]
  "")


;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(and:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
  "@
   and\\t%0,%1,%2
   andi\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "register_operand" "d")))]
  "!TARGET_DEBUG_G_MODE"
  "and\\t%M0,%M1,%M2\;and\\t%L0,%L1,%L2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (and:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0)))
   (set (subreg:SI (match_dup 0) 1) (and:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1)))]
  "")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
  "@
   or\\t%0,%1,%2
   ori\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "register_operand" "d")))]
  "!TARGET_DEBUG_G_MODE"
  "or\\t%M0,%M1,%M2\;or\\t%L0,%L1,%L2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (ior:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0)))
   (set (subreg:SI (match_dup 0) 1) (ior:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1)))]
  "")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(xor:SI (match_operand:SI 1 "uns_arith_operand" "%d,d")
		(match_operand:SI 2 "uns_arith_operand" "d,K")))]
  ""
  "@
   xor\\t%0,%1,%2
   xori\\t%0,%1,%x2"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "register_operand" "d")
		(match_operand:DI 2 "register_operand" "d")))]
  "!TARGET_DEBUG_G_MODE"
  "xor\\t%M0,%M1,%M2\;xor\\t%L0,%L1,%L2"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))"

  [(set (subreg:SI (match_dup 0) 0) (xor:SI (subreg:SI (match_dup 1) 0) (subreg:SI (match_dup 2) 0)))
   (set (subreg:SI (match_dup 0) 1) (xor:SI (subreg:SI (match_dup 1) 1) (subreg:SI (match_dup 2) 1)))]
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
  "TARGET_HARD_FLOAT"
  "cvt.s.d\\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1")])


;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand
;; are ordered widest source type first.

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0xffff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1,1,2")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,d,d")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0x00ff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"HI")
   (set_attr "length"	"1,1,2")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,R,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"andi\\t%0,%1,0x00ff\";
  else
    return mips_move_1word (operands, insn, TRUE);
}"
  [(set_attr "type"	"arith,load,load")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1,1,2")])


;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand
;; are ordered widest source type first.

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the lh and lb insns.

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
      rtx shift = gen_rtx (CONST_INT, VOIDmode, 16);

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
   (set_attr "length"	"1,2")])

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
      rtx shift = gen_rtx (CONST_INT, VOIDmode, 24);

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
   (set_attr "length"	"1,2")])


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
      rtx shift = gen_rtx (CONST_INT, VOIDmode, 24);

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
   (set_attr "length"	"1,2")])


(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "cvt.d.s\\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1")])



;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=d,*f,R,o")
	(fix:SI (match_operand:DF 1 "register_operand" "f,*f,f,f")))
   (clobber (match_scratch:SI 2 "=d,*d,d,d"))
   (clobber (match_scratch:DF 3 "=f,*X,f,f"))]
  "TARGET_HARD_FLOAT"
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
   (set_attr "length"	"11,9,10,11")])


(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=d,*f,R,o")
	(fix:SI (match_operand:SF 1 "register_operand" "f,*f,f,f")))
   (clobber (match_scratch:SI 2 "=d,*d,d,d"))
   (clobber (match_scratch:SF 3 "=f,*X,f,f"))]
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
   (set_attr "length"	"11,9,10,11")])


(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f,f")
	(float:DF (match_operand:SI 1 "nonimmediate_operand" "d,R,m")))]
  "TARGET_HARD_FLOAT"
  "*
{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return \"l.s\\t%0,%1%#\;cvt.d.w\\t%0,%0\";

  return \"mtc1\\t%1,%0%#\;cvt.d.w\\t%0,%0\";
}"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "length"	"3,4,3")])


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
   (set_attr "length"	"3,4,3")])


(define_expand "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT"
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
      emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			       gen_rtx (LABEL_REF, VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx (MINUS, DFmode, operands[1], reg1));
      emit_move_insn (reg3, gen_rtx (CONST_INT, VOIDmode, 0x80000000));

      emit_insn (gen_fix_truncdfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
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
      emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			       gen_rtx (LABEL_REF, VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx (MINUS, SFmode, operands[1], reg1));
      emit_move_insn (reg3, gen_rtx (CONST_INT, VOIDmode, 0x80000000));

      emit_insn (gen_fix_truncsfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")


;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; unaligned word moves generated by the block moves.

(define_expand "movsi_unaligned"
  [(set (match_operand:SI 0 "general_operand" "")
	(unspec [(match_operand:SI 1 "general_operand" "")] 0))]
  ""
  "
{
  /* Handle loads.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      rtx reg = gen_reg_rtx (SImode);
      rtx insn = emit_insn (gen_movsi_ulw (reg, operands[1]));
      rtx addr = XEXP (operands[0], 0);
      if (CONSTANT_P (addr))
	REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_EQUIV, addr, REG_NOTES (insn));

      if (reg_or_0_operand (operands[1], SImode))
	DONE;

      operands[1] = reg;
    }

  /* Generate appropriate load, store.  If not a load or store,
     do a normal movsi.  */
  if (GET_CODE (operands[0]) != MEM && GET_CODE (operands[1]) != MEM)
    {
      emit_insn (gen_movsi (operands[0], operands[1]));
      DONE;
    }

  /* Fall through and generate normal code.  */
}")

(define_insn "movsi_ulw"
  [(set (match_operand:SI 0 "register_operand" "=&d,&d,d,d")
	(unspec [(match_operand:SI 1 "general_operand" "R,o,dIKL,M")] 0))]
  ""
  "*
{
  enum rtx_code code;
  char *ret;
  rtx offset;
  rtx addr;
  rtx mem_addr;

  if (which_alternative != 0)
    return mips_move_1word (operands, insn, FALSE);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  offset = const0_rtx;
  addr = XEXP (operands[1], 0);
  mem_addr = eliminate_constant_term (addr, &offset);

  if ((INTVAL (offset) & (UNITS_PER_WORD-1)) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = \"lw\\t%0,%1\";

  else
    {
      ret = \"ulw\\t%0,%1\";
      if (TARGET_GAS)
	{
	  enum rtx_code code = GET_CODE (addr);

	  if (code == CONST || code == SYMBOL_REF || code == LABEL_REF)
	    {
	      operands[2] = gen_rtx (REG, SImode, GP_REG_FIRST + 1);
	      ret = \"%[la\\t%2,%1\;ulw\\t%0,0(%2)%]\";
	    }
	}
    }

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}"
  [(set_attr "type"	"load,load,move,arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2,4,1,2")])

(define_insn "movsi_usw"
  [(set (match_operand:SI 0 "memory_operand" "=R,o")
	(unspec [(match_operand:SI 1 "reg_or_0_operand" "dJ,dJ")] 0))]
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

  if ((INTVAL (offset) & (UNITS_PER_WORD-1)) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return \"sw\\t%1,%0\";


  if (TARGET_GAS)
    {
      enum rtx_code code = GET_CODE (XEXP (operands[0], 0));

      if (code == CONST || code == SYMBOL_REF || code == LABEL_REF)
	{
	  operands[2] = gen_rtx (REG, SImode, GP_REG_FIRST + 1);
	  return \"%[la\\t%2,%0\;usw\\t%z1,0(%2)%]\";
	}
    }

  return \"usw\\t%z1,%0\";
}"
  [(set_attr "type"	"store")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2,4")])


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
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], DImode)
      && !register_operand (operands[1], DImode)
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
      && operands[1] != CONST0_RTX (DImode))
    {
      rtx temp = force_reg (DImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

(define_insn "movdi_internal"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,d,R,o,*d,*x")
	(match_operand:DI 1 "general_operand" "d,iF,R,o,d,d,*x,*d"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)
   || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
   || operands[1] == CONST0_RTX (DImode)"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,arith,load,load,store,store,hilo,hilo")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2,4,2,4,2,4,2,2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operand:DI 1 "register_operand" ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"

  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 1) (subreg:SI (match_dup 1) 1))]
  "")


;; 32-bit Integer moves

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "large_int" ""))]
  "!TARGET_DEBUG_D_MODE"
  [(set (match_dup 0)
	(match_dup 2))
   (set (match_dup 0)
     	(ior:SI (match_dup 0)
		(match_dup 3)))]
  "
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[1]) & 0xffff0000);
  operands[3] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[1]) & 0x0000ffff);
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
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SImode)
      && !register_operand (operands[1], SImode)
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0))
    {
      rtx temp = force_reg (SImode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "movsi_internal1"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d,d,d,d,R,m,*d,*f*z,*f,*f,*f,*R,*m,*x,*d")
	(match_operand:SI 1 "general_operand" "d,S,IKL,Mnis,R,m,dJ,dJ,*f*z,*d,*f,*R,*m,*f,*f,*d,*x"))]
  "TARGET_DEBUG_H_MODE
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,load,arith,arith,load,load,store,store,xfer,xfer,move,load,load,store,store,hilo,hilo")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1,2,1,2,1,2,1,2,1,1,1,1,2,1,2,1,1")])

(define_insn "movsi_internal2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d,d,d,d,R,m,*d,*z,*d,*x")
	(match_operand:SI 1 "general_operand" "d,S,IKL,Mnis,R,m,dJ,dJ,*z,*d,*x,*d"))]
  "!TARGET_DEBUG_H_MODE
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,load,arith,arith,load,load,store,store,xfer,xfer,hilo,hilo")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1,2,1,2,1,2,1,2,1,1,1,1")])


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
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0))
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
  "TARGET_DEBUG_H_MODE
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,move,hilo,hilo")
   (set_attr "mode"	"HI")
   (set_attr "length"	"1,1,1,2,1,2,1,1,1,1,1")])

(define_insn "movhi_internal2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,d,R,m,*d,*z,*x,*d")
	(match_operand:HI 1 "general_operand"       "d,IK,R,m,dJ,dJ,*z,*d,*d,*x"))]
  "!TARGET_DEBUG_H_MODE
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,hilo,hilo")
   (set_attr "mode"	"HI")
   (set_attr "length"	"1,1,1,2,1,2,1,1,1,1")])


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
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0))
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
  "TARGET_DEBUG_H_MODE
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,move,hilo,hilo")
   (set_attr "mode"	"QI")
   (set_attr "length"	"1,1,1,2,1,2,1,1,1,1,1")])

(define_insn "movqi_internal2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,d,R,m,*d,*z,*x,*d")
	(match_operand:QI 1 "general_operand"       "d,IK,R,m,dJ,dJ,*z,*d,*d,*x"))]
  "!TARGET_DEBUG_H_MODE
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0))"
  "* return mips_move_1word (operands, insn, TRUE);"
  [(set_attr "type"	"move,arith,load,load,store,store,xfer,xfer,hilo,hilo")
   (set_attr "mode"	"QI")
   (set_attr "length"	"1,1,1,2,1,2,1,1,1,1")])


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
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
      && operands[1] != CONST0_RTX (SFmode))
    {
      rtx temp = force_reg (SFmode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

(define_insn "movsf_internal1"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,f,R,m,*f,*d,*d,*d,*d,*R,*m")
	(match_operand:SF 1 "general_operand" "f,G,R,Em,fG,fG,*d,*f,*G*d,*R,*E*m,*d,*d"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (SFmode))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,xfer,load,load,store,store,xfer,xfer,move,load,load,store,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1,1,1,2,1,2,1,1,1,1,2,1,2")])


(define_insn "movsf_internal2"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,d,R,m")
	(match_operand:SF 1 "general_operand" "      Gd,R,Em,d,d"))]
  "TARGET_SOFT_FLOAT
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (SFmode))"
  "* return mips_move_1word (operands, insn, FALSE);"
  [(set_attr "type"	"move,load,load,store,store")
   (set_attr "mode"	"SF")
   (set_attr "length"	"1,1,2,1,2")])


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
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0)
      && operands[1] != CONST0_RTX (DFmode))
    {
      rtx temp = force_reg (DFmode, operands[1]);
      emit_move_insn (operands[0], temp);
      DONE;
    }
}")

(define_insn "movdf_internal1"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,R,o,f,*f,*d,*d,*d,*d,*R,*o")
	(match_operand:DF 1 "general_operand" "f,R,o,fG,fG,E,*d,*f,*d*G,*R,*o*E,*d,*d"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DFmode))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,load,load,store,store,load,xfer,xfer,move,load,load,store,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"1,2,4,2,4,4,2,2,2,2,4,2,4")])

(define_insn "movdf_internal2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,d,R,o")
	(match_operand:DF 1 "general_operand" "dG,R,oE,d,d"))]
  "TARGET_SOFT_FLOAT
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DFmode))"
  "* return mips_move_2words (operands, insn); "
  [(set_attr "type"	"move,load,load,store,store")
   (set_attr "mode"	"DF")
   (set_attr "length"	"2,2,4,2,4")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(match_operand:DF 1 "register_operand" ""))]
  "reload_completed && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))"

  [(set (subreg:SI (match_dup 0) 0) (subreg:SI (match_dup 1) 0))
   (set (subreg:SI (match_dup 0) 1) (subreg:SI (match_dup 1) 1))]
  "")


;; Block moves, see mips.c for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	      (use (match_operand:SI 2 "arith32_operand" ""))
	      (use (match_operand:SI 3 "immediate_operand" ""))])]
  ""
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
  [(set (match_operand:BLK 0 "memory_operand" "=Ro")	;; destination
	(match_operand:BLK 1 "memory_operand" "Ro"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 0))]					;; normal block move
  ""
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"none")
   (set_attr "length"	"20")])

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
  [(set (match_operand:BLK 0 "memory_operand" "=Ro")	;; destination
	(match_operand:BLK 1 "memory_operand" "Ro"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 1))]					;; all but last store
  ""
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);"
  [(set_attr "type"	"multi")
   (set_attr "mode"	"none")
   (set_attr "length"	"20")])

(define_insn "movstrsi_internal3"
  [(set (match_operand:BLK 0 "memory_operand" "=Ro")	;; destination
	(match_operand:BLK 1 "memory_operand" "Ro"))	;; source
   (clobber (match_scratch:SI 4 "=&d"))			;; temp 1
   (clobber (match_scratch:SI 5 "=&d"))			;; temp 2
   (clobber (match_scratch:SI 6 "=&d"))			;; temp 3
   (clobber (match_scratch:SI 7 "=&d"))			;; temp 4
   (use (match_operand:SI 2 "small_int" "I"))		;; # bytes to move
   (use (match_operand:SI 3 "small_int" "I"))		;; alignment
   (use (const_int 2))]					;; just last store of block mvoe
  ""
  "* return output_block_move (insn, operands, 4, BLOCK_MOVE_LAST);"
  [(set_attr "type"	"store")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])


;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);

  return \"sll\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])


(define_expand "ashldi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashift:DI (match_operand:DI 1 "register_operand" "")
			      (match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "!TARGET_DEBUG_G_MODE"
  "operands[3] = gen_reg_rtx (SImode);")


(define_insn "ashldi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE"
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
1:\\n\\
\\t%(beq\\t%3,%z4,2f\\n\\
\\tsll\\t%M0,%M1,%2%)\\n\\
\\n\\
\\tsubu\\t%3,%z4,%2\\n\\
\\tsrl\\t%3,%L1,%3\\n\\
\\tor\\t%M0,%M0,%3\\n\\
2:\\n\\
\\tsll\\t%L0,%L1,%2\\n\\
3:\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"12")])


(define_insn "ashldi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0"
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);
  operands[4] = const0_rtx;
  return \"sll\\t%M0,%L1,%2\;move\\t%L0,%z4\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 1) (ashift:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 0) (const_int 0))]

  "operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (ashift:SI (subreg:SI (match_dup 1) 1) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 1) (const_int 0))]

  "operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);")


(define_insn "ashldi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashift:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
  "*
{
  int amount = INTVAL (operands[2]);

  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = const0_rtx;
  operands[5] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));

  return \"sll\\t%M0,%M1,%2\;srl\\t%3,%L1,%5\;or\\t%M0,%M0,%3\;sll\\t%L0,%L1,%2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));
}")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));
}")


(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);

  return \"sra\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])


(define_expand "ashrdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashiftrt:DI (match_operand:DI 1 "register_operand" "")
				(match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "!TARGET_DEBUG_G_MODE"
  "operands[3] = gen_reg_rtx (SImode);")


(define_insn "ashrdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE"
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
1:\\n\\
\\t%(beq\\t%3,%z4,2f\\n\\
\\tsrl\\t%L0,%L1,%2%)\\n\\
\\n\\
\\tsubu\\t%3,%z4,%2\\n\\
\\tsll\\t%3,%M1,%3\\n\\
\\tor\\t%L0,%L0,%3\\n\\
2:\\n\\
\\tsra\\t%M0,%M1,%2\\n\\
3:\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12")])


(define_insn "ashrdi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0"
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);
  return \"sra\\t%L0,%M1,%2\;sra\\t%M0,%M1,31\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (ashiftrt:SI (subreg:SI (match_dup 1) 1) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 1) (ashiftrt:SI (subreg:SI (match_dup 1) 1) (const_int 31)))]

  "operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 1) (ashiftrt:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 0) (ashiftrt:SI (subreg:SI (match_dup 1) 0) (const_int 31)))]

  "operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);")


(define_insn "ashrdi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
  "*
{
  int amount = INTVAL (operands[2]);

  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));

  return \"srl\\t%L0,%L1,%2\;sll\\t%3,%M1,%4\;or\\t%L0,%L0,%3\;sra\\t%M0,%M1,%2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));
}")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));
}")


(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "arith_operand" "dI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);

  return \"srl\\t%0,%1,%2\";
}"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])


(define_expand "lshrdi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (lshiftrt:DI (match_operand:DI 1 "register_operand" "")
				(match_operand:SI 2 "arith_operand" "")))
	      (clobber (match_dup  3))])]
  "!TARGET_DEBUG_G_MODE"
  "operands[3] = gen_reg_rtx (SImode);")


(define_insn "lshrdi3_internal"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE"
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
1:\\n\\
\\t%(beq\\t%3,%z4,2f\\n\\
\\tsrl\\t%L0,%L1,%2%)\\n\\
\\n\\
\\tsubu\\t%3,%z4,%2\\n\\
\\tsll\\t%3,%M1,%3\\n\\
\\tor\\t%L0,%L0,%3\\n\\
2:\\n\\
\\tsrl\\t%M0,%M1,%2\\n\\
3:\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"12")])


(define_insn "lshrdi3_internal2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		     (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0"
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);
  operands[4] = const0_rtx;
  return \"srl\\t%L0,%M1,%2\;move\\t%M0,%z4\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"2")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 0) (lshiftrt:SI (subreg:SI (match_dup 1) 1) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 1) (const_int 0))]

  "operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0"

  [(set (subreg:SI (match_dup 0) 1) (lshiftrt:SI (subreg:SI (match_dup 1) 0) (match_dup 2)))
   (set (subreg:SI (match_dup 0) 0) (const_int 0))]

  "operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);")


(define_insn "lshrdi3_internal3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:SI 2 "small_int" "IJK")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  "!TARGET_DEBUG_G_MODE
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0"
  "*
{
  int amount = INTVAL (operands[2]);

  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));

  return \"srl\\t%L0,%L1,%2\;sll\\t%3,%M1,%4\;or\\t%L0,%L0,%3\;srl\\t%M0,%M1,%2\";
}"
  [(set_attr "type"	"darith")
   (set_attr "mode"	"DI")
   (set_attr "length"	"4")])


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && !WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));
}")


(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:SI 2 "small_int" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "reload_completed && WORDS_BIG_ENDIAN && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
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
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (amount & 31));
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ((-amount) & 31));
}")


;;
;;  ....................
;;
;;	COMPARISONS
;;
;;  ....................

;; Flow here is rather complex:
;;
;;  1)	The cmp{si,sf,df} routine is called.  It deposits the
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

(define_expand "cmpdf"
  [(set (cc0)
	(compare:CC_FP (match_operand:DF 0 "register_operand" "")
		       (match_operand:DF 1 "register_operand" "")))]
  "TARGET_HARD_FLOAT"
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
	(compare:CC_FP (match_operand:SF 0 "register_operand" "")
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

(define_insn "branch_fp_ne"
  [(set (pc)
	(if_then_else (ne:CC_FP (reg:CC_FP 66)
				(const_int 0))
		      (match_operand 0 "pc_or_label_operand" "")
		      (match_operand 1 "pc_or_label_operand" "")))]
  ""
  "*
{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[0] != pc_rtx) ? \"%*bc1t%?\\t%0\" : \"%*bc1f%?\\t%1\";
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_insn "branch_fp_ne_rev"
  [(set (pc)
	(if_then_else (ne:CC_REV_FP (reg:CC_REV_FP 66)
				    (const_int 0))
		      (match_operand 0 "pc_or_label_operand" "")
		      (match_operand 1 "pc_or_label_operand" "")))]
  ""
  "*
{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[0] != pc_rtx) ? \"%*bc1f%?\\t%0\" : \"%*bc1t%?\\t%1\";
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_insn "branch_fp_eq"
  [(set (pc)
	(if_then_else (eq:CC_FP (reg:CC_FP 66)
				(const_int 0))
		      (match_operand 0 "pc_or_label_operand" "")
		      (match_operand 1 "pc_or_label_operand" "")))]
  ""
  "*
{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[0] != pc_rtx) ? \"%*bc1f%?\\t%0\" : \"%*bc1t%?\\t%1\";
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_insn "branch_fp_eq_rev"
  [(set (pc)
	(if_then_else (eq:CC_REV_FP (reg:CC_REV_FP 66)
				    (const_int 0))
		      (match_operand 0 "pc_or_label_operand" "")
		      (match_operand 1 "pc_or_label_operand" "")))]
  ""
  "*
{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[0] != pc_rtx) ? \"%*bc1t%?\\t%0\" : \"%*bc1f%?\\t%1\";
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])


(define_insn "branch_zero"
  [(set (pc)
	(if_then_else (match_operator:SI 0 "cmp_op"
					 [(match_operand:SI 1 "register_operand" "d")
					  (const_int 0)])
	(match_operand 2 "pc_or_label_operand" "")
	(match_operand 3 "pc_or_label_operand" "")))]
  ""
  "*
{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  if (operands[2] != pc_rtx)
    {				/* normal jump */
      switch (GET_CODE (operands[0]))
	{
	case EQ:  return \"%*beq%?\\t%z1,%.,%2\";
	case NE:  return \"%*bne%?\\t%z1,%.,%2\";
	case GTU: return \"%*bne%?\\t%z1,%.,%2\";
	case LEU: return \"%*beq%?\\t%z1,%.,%2\";
	case GEU: return \"%*j\\t%2\";
	case LTU: return \"%*bne%?\\t%.,%.,%2\";
	}

      return \"%*b%C0z%?\\t%z1,%2\";
    }
  else
    {				/* inverted jump */
      switch (GET_CODE (operands[0]))
	{
	case EQ:  return \"%*bne%?\\t%z1,%.,%3\";
	case NE:  return \"%*beq%?\\t%z1,%.,%3\";
	case GTU: return \"%*beq%?\\t%z1,%.,%3\";
	case LEU: return \"%*bne%?\\t%z1,%.,%3\";
	case GEU: return \"%*beq%?\\t%.,%.,%3\";
	case LTU: return \"%*j\\t%3\";
	}

      return \"%*b%N0z%?\\t%z1,%3\";
    }
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])


(define_insn "branch_equality"
  [(set (pc)
	(if_then_else (match_operator:SI 0 "equality_op"
					 [(match_operand:SI 1 "register_operand" "d")
					  (match_operand:SI 2 "register_operand" "d")])
	(match_operand 3 "pc_or_label_operand" "")
	(match_operand 4 "pc_or_label_operand" "")))]
  ""
  "*
{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[3] != pc_rtx)
	? \"%*b%C0%?\\t%z1,%z2,%3\"
	: \"%*b%N0%?\\t%z1,%z2,%4\";
}"
  [(set_attr "type"	"branch")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])


(define_expand "beq"
  [(set (pc)
	(if_then_else (eq:CC_EQ (cc0)
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
	(if_then_else (ne:CC_EQ (cc0)
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
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  ""
  "sltu\\t%0,%1,1"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "seq_si"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(eq:SI (match_operand:SI 1 "register_operand" "%d,d")
	       (match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "TARGET_DEBUG_C_MODE"
  "@
   xor\\t%0,%1,%2\;sltu\\t%0,%0,1
   xori\\t%0,%1,%2\;sltu\\t%0,%0,1"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "uns_arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)"
  [(set (match_dup 0)
	(xor:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(ltu:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ne:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  ""
  "sltu\\t%0,%.,%1"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "sne_si"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ne:SI (match_operand:SI 1 "register_operand" "%d,d")
	       (match_operand:SI 2 "uns_arith_operand" "d,K")))]
  "TARGET_DEBUG_C_MODE"
  "@
    xor\\t%0,%1,%2\;sltu\\t%0,%.,%0
    xori\\t%0,%1,%x2\;sltu\\t%0,%.,%0"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "uns_arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0)"
  [(set (match_dup 0)
	(xor:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(gtu:SI (match_dup 0)
		(const_int 0)))]
  "")

(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  ""
  "slt\\t%0,%z2,%1"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ge:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  "TARGET_DEBUG_C_MODE"
  "slt\\t%0,%1,%2\;xori\\t%0,%0,0x0001"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ge:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE"
  [(set (match_dup 0)
	(lt:SI (match_dup 1)
	       (match_dup 2)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  ""
  "slt\\t%0,%1,%2"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(le:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  "INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2])+1);
  return \"slt\\t%0,%1,%2\";
}"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "sle_si_reg"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(le:SI (match_operand:SI 1 "register_operand" "d")
	       (match_operand:SI 2 "register_operand" "d")))]
  "TARGET_DEBUG_C_MODE"
  "slt\\t%0,%z2,%1\;xori\\t%0,%0,0x0001"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(le:SI (match_operand:SI 1 "register_operand" "")
	       (match_operand:SI 2 "register_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE"
  [(set (match_dup 0)
	(lt:SI (match_dup 2)
	       (match_dup 1)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(gtu:SI (match_dup 1)
		(match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (geu:SI (match_dup 1)
                (match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  "TARGET_DEBUG_C_MODE"
  "sltu\\t%0,%1,%2\;xori\\t%0,%0,0x0001"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(geu:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "arith_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE"
  [(set (match_dup 0)
	(ltu:SI (match_dup 1)
		(match_dup 2)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")

(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ltu:SI (match_dup 1)
		(match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  ""
  "sltu\\t%0,%1,%2"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(leu:SI (match_dup 1)
		(match_dup 2)))]
  ""
  "
{
  if (branch_type != CMP_SI)
    FAIL;

  /* set up operands from compare.  */
  operands[1] = branch_cmp[0];
  operands[2] = branch_cmp[1];

  if (!TARGET_DEBUG_C_MODE)
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
  "INTVAL (operands[2]) < 32767"
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2])+1);
  return \"sltu\\t%0,%1,%2\";
}"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"1")])

(define_insn "sleu_si_reg"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(leu:SI (match_operand:SI 1 "register_operand" "d")
		(match_operand:SI 2 "register_operand" "d")))]
  "TARGET_DEBUG_C_MODE"
  "sltu\\t%0,%z2,%1\;xori\\t%0,%0,0x0001"
 [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")
   (set_attr "length"	"2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(leu:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))]
  "TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE"
  [(set (match_dup 0)
	(ltu:SI (match_dup 2)
		(match_dup 1)))
   (set (match_dup 0)
	(xor:SI (match_dup 0)
		(const_int 1)))]
  "")


;;
;;  ....................
;;
;;	FLOATING POINT COMPARISONS
;;
;;  ....................

(define_insn "seq_df"
  [(set (reg:CC_FP 66)
	(eq:CC_FP (match_operand:DF 0 "register_operand" "f")
		  (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.eq.d\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sne_df"
  [(set (reg:CC_REV_FP 66)
	(ne:CC_REV_FP (match_operand:DF 0 "register_operand" "f")
		      (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.eq.d\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "slt_df"
  [(set (reg:CC_FP 66)
	(lt:CC_FP (match_operand:DF 0 "register_operand" "f")
		  (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.lt.d\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sle_df"
  [(set (reg:CC_FP 66)
	(le:CC_FP (match_operand:DF 0 "register_operand" "f")
		  (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.le.d\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sgt_df"
  [(set (reg:CC_FP 66)
	(gt:CC_FP (match_operand:DF 0 "register_operand" "f")
		  (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.lt.d\\t%1,%0\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sge_df"
  [(set (reg:CC_FP 66)
	(ge:CC_FP (match_operand:DF 0 "register_operand" "f")
		  (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.le.d\\t%1,%0\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "seq_sf"
  [(set (reg:CC_FP 66)
	(eq:CC_FP (match_operand:SF 0 "register_operand" "f")
		  (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.eq.s\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sne_sf"
  [(set (reg:CC_REV_FP 66)
	(ne:CC_REV_FP (match_operand:SF 0 "register_operand" "f")
		      (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.eq.s\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "slt_sf"
  [(set (reg:CC_FP 66)
	(lt:CC_FP (match_operand:SF 0 "register_operand" "f")
		  (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.lt.s\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sle_sf"
  [(set (reg:CC_FP 66)
	(le:CC_FP (match_operand:SF 0 "register_operand" "f")
		  (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.le.s\\t%0,%1\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sgt_sf"
  [(set (reg:CC_FP 66)
	(gt:CC_FP (match_operand:SF 0 "register_operand" "f")
		  (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.lt.s\\t%1,%0\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])

(define_insn "sge_sf"
  [(set (reg:CC_FP 66)
	(ge:CC_FP (match_operand:SF 0 "register_operand" "f")
		  (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
{
  rtx xoperands[10];
  xoperands[0] = gen_rtx (REG, CC_FPmode, FPSW_REGNUM);
  xoperands[1] = operands[0];
  xoperands[2] = operands[1];

  return mips_fill_delay_slot (\"c.le.s\\t%1,%0\", DELAY_FCMP, xoperands, insn);
}"
 [(set_attr "type"	"fcmp")
  (set_attr "mode"	"FPSW")
  (set_attr "length"	"1")])


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
  ""
  "*
{
  if (GET_CODE (operands[0]) == REG)
    return \"%*j\\t%0\";
  else
    return \"%*j\\t%l0\";
}"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "d"))]
  ""
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

;; Function return, only allow after optimization, so that we can
;; eliminate jumps to jumps if no stack space is used.

;; (define_expand "return"
;;   [(set (pc) (reg:SI 31))]
;;   "simple_epilogue_p ()"
;;   "")

(define_expand "return"
  [(parallel [(return)
	      (use (reg:SI 31))])]
  "simple_epilogue_p ()"
  "")

(define_insn "return_internal"
  [(parallel [(return)
              (use (match_operand:SI 0 "register_operand" "d"))])]
  ""
  "%*j\\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])


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

;; At present, don't expand the epilogue, reorg.c will clobber the
;; return register in compiling gen_lowpart (emit-rtl.c).
;; 
;; (define_expand "epilogue"
;;   [(const_int 2)]
;;   ""
;;   "
;; {
;;   if (mips_isa >= 0)            /* avoid unused code warnings */
;;     {
;;       mips_expand_epilogue ();
;;       DONE;
;;     }
;; }")


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
      if (GET_CODE (addr) != REG && !CONSTANT_ADDRESS_P (addr))
	XEXP (operands[0], 0) = force_reg (FUNCTION_MODE, addr);

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

      emit_call_insn (gen_call_internal (operands[0], operands[1],
					 gen_rtx (REG, Pmode, GP_REG_FIRST + 31)));
      DONE;
    }
}")

(define_insn "call_internal"
  [(call (match_operand 0 "memory_operand" "m")
	 (match_operand 1 "" "i"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  ""
  "*
{
  register rtx target = XEXP (operands[0], 0);

  if (GET_CODE (target) == SYMBOL_REF)
    return \"%*jal\\t%0\";

  else if (GET_CODE (target) == CONST_INT)
    {
      operands[0] = target;
      return \"%*%[li\\t%@,%0\\n\\tjal\\t%2,%@%]\";
    }

  else
    {
      operands[0] = target;
      return \"%*jal\\t%2,%0\";
    }
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

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
      if (GET_CODE (addr) != REG && !CONSTANT_ADDRESS_P (addr))
	XEXP (operands[1], 0) = force_reg (FUNCTION_MODE, addr);

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

      emit_call_insn (gen_call_value_internal (operands[0], operands[1], operands[2],
					       gen_rtx (REG, Pmode, GP_REG_FIRST + 31)));

      DONE;
    }

}")

(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "=df")
        (call (match_operand 1 "memory_operand" "m")
              (match_operand 2 "" "i")))
   (clobber (match_operand:SI 3 "register_operand" "=d"))]
  ""
  "*
{
  register rtx target = XEXP (operands[1], 0);

  if (GET_CODE (target) == SYMBOL_REF)
    return \"%*jal\\t%1\";

  else if (GET_CODE (target) == CONST_INT)
    {
      operands[1] = target;
      return \"%*%[li\\t%@,%1\\n\\tjal\\t%3,%@%]\";
    }

  else
    {
      operands[1] = target;
      return \"%*jal\\t%3,%1\";
    }
}"
  [(set_attr "type"	"call")
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])


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
   (set_attr "mode"	"none")
   (set_attr "length"	"1")])

(define_expand "probe"
  [(set (match_dup 0)
	(match_dup 1))]
  ""
  "
{
  operands[0] = gen_reg_rtx (SImode);
  operands[1] = gen_rtx (MEM, SImode, stack_pointer_rtx);
  MEM_VOLATILE_P (operands[1]) = TRUE;

  /* fall through and generate default code */
}")


;;
;; Local variables:
;; mode:emacs-lisp
;; comment-start: ";; "
;; eval: (set-syntax-table (copy-sequence (syntax-table)))
;; eval: (modify-syntax-entry ?[ "(]")
;; eval: (modify-syntax-entry ?] ")[")
;; eval: (modify-syntax-entry ?{ "(}")
;; eval: (modify-syntax-entry ?} "){")
;; End:
