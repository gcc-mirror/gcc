;;- Machine description Acorn RISC Machine for GNU compiler
;;  Copyright (C) 1991 Free Software Foundation, Inc.
;;  Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
;;             and Martin Simmons (@harleqn.co.uk).

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

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Every template must be output by arm_output_asm_insn, since this keeps
;; track of the offset of labels within the text segment.  This is needed to
;; to be able to (correctly) output instructions for loading a value from a
;; function's constant pool, since different instructions are needed when the
;; constant pool is more than 4095 bytes away from the PC.

;; Addition insns.

(define_insn "adddi3"
  [(set (match_operand:DI 0 "di_operand" "=&r")
	(plus:DI (match_operand:DI 1 "di_operand" "%r")
		 (match_operand:DI 2 "di_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"adds\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"adc\\t%R0, %R1, %R2\", operands));
")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r")
		 (match_operand:SI 2 "general_operand" "r,n")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"add\\t%0, %1, %2\", operands));
    case 1:
      return (output_add_immediate (operands));
    }
")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"adfs\\t%0, %1, %2\", operands));
")

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"adfd\\t%0, %1, %2\", operands));
")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "di_operand" "=&r")
	(minus:DI (match_operand:DI 1 "di_operand" "%r")
		  (match_operand:DI 2 "di_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"sbc\\t%R0, %R1, %R2\", operands));
")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(minus:SI (match_operand:SI 1 "arm_rhs_operand" "r,r,I")
		  (match_operand:SI 2 "general_operand" "r,n,r")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sub\\t%0, %1, %2\", operands));
    case 1:
      operands[2] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[2]));
      return (output_add_immediate (operands));
    case 2:
      return (arm_output_asm_insn (\"rsb\\t%0, %2, %1\", operands));
    }
")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(minus:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sufs\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rsfs\\t%0, %2, %1\", operands));
    }
")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
      (match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands));
    case 2:
      return (arm_output_asm_insn (\"rsfd\\t%0, %2, %1\", operands));
    }
")

;; Multiplication insns

;; The `&' is too strict, but at least generates correct code.
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return (arm_output_asm_insn (\"mul\\t%0, %2, %1\", operands));
  else
    return (arm_output_asm_insn (\"mul\\t%0, %1, %2\", operands));
")

;; Unnamed templates to match MLA instruction.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (mult:SI (match_operand:SI 1 "register_operand" "%r")
		   (match_operand:SI 2 "register_operand" "r"))
	  (match_operand:SI 3 "register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return (arm_output_asm_insn (\"mla\\t%0, %2, %1, %3\", operands));
  else
    return (arm_output_asm_insn (\"mla\\t%0, %1, %2, %3\", operands));
")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (match_operand:SI 3 "register_operand" "r")
	  (mult:SI (match_operand:SI 1 "register_operand" "%r")
		   (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "*
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return (arm_output_asm_insn (\"mla\\t%0, %2, %1, %3\", operands));
  else
    return (arm_output_asm_insn (\"mla\\t%0, %1, %2, %3\", operands));
")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*return (arm_output_asm_insn (\"mufs\\t%0, %1, %2\", operands));")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"mufd\\t%0, %1, %2\", operands));
")

;; Division insns

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(div:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		(match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"dvfs\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rdfs\\t%0, %2, %1\", operands));
    }
")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		(match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"dvfd\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rdfd\\t%0, %2, %1\", operands));
    }
")

;; Modulo insns

(define_insn "modsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mod:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfs\\t%0, %1, %2\", operands));
")

(define_insn "moddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mod:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfd\\t%0, %1, %2\", operands));
")

;; Boolean and,ior,xor insns

(define_insn "anddi3"
  [(set (match_operand:DI 0 "di_operand" "=&r")
	(and:DI (match_operand:DI 1 "di_operand" "%r")
		(match_operand:DI 2 "di_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"and\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"and\\t%R0, %R1, %R2\", operands));
")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arm_rhs_operand" "rI")))]
  ""
  "*
  return (arm_output_asm_insn (\"and\\t%0, %1, %2\", operands));
")

(define_insn "andcbsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "r")
		(not:SI (match_operand:SI 2 "arm_rhs_operand" "rI"))))]
  ""
  "*
  return (arm_output_asm_insn (\"bic\\t%0, %1, %2\", operands));
")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "di_operand" "=&r")
	(ior:DI (match_operand:DI 1 "di_operand" "%r")
		(match_operand:DI 2 "di_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"orr\\t%R0, %R1, %R2\", operands));
")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands));
    case 1:
      return (output_multi_immediate (operands,
				      \"orr\\t%0, %1, %2\", \"orr\\t%0, %0, %2\",
   				      2, INTVAL (operands[2])));
    }
")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "nonmemory_operand" "r,n")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"eor\\t%0, %1, %2\", operands));
    case 1:
      return (output_multi_immediate (operands,
				      \"eor\\t%0, %1, %2\", \"eor\\t%0, %0, %2\",
				      2, INTVAL (operands[2])));
    }
")

;; Shift and rotation insns

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (ASHIFT, operands));
")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (ASHIFTRT, operands));
")

;; lshlsi3 is not defined because shift counts cannot be negative
;; An unnamed pattern is needed for expansion of zero_extend.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (LSHIFT, operands));
")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "general_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (LSHIFTRT, operands));
")

;; rotlsi3 is not defined yet to see what happens

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "general_operand" "r,n")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"mov\\t%0, %1,ror %2\", operands));
    case 1:
      if (INTVAL(operands[2]) > 31)
	operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) % 32);
      return (arm_output_asm_insn (\"mov\\t%0, %1,ror%2\", operands));
    }
")

;; Unary arithmetic insns

(define_insn "negdi2"
  [(set (match_operand:DI 0 "di_operand" "=&r")
	(neg:DI (match_operand:DI 1 "di_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"rsb\\t%0, %1, #0\", operands);
  return (arm_output_asm_insn (\"rsc\\t%R0, %R1, #0\", operands));
")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"rsb\\t%0, %1, #0\", operands));
")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mnfs\\t%0, %1\", operands));
")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mnfd\\t%0, %1\", operands));
")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	 (abs:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"abss\\t%0, %1\", operands));
")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"absd\\t%0, %1\", operands));
")

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"sqts\\t%0, %1\", operands));
")

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"sqtd\\t%0, %1\", operands));
")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"mvn\\t%0, %1\", operands));
")

;; Fixed <--> Floating conversion insns

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"flts\\t%0, %1\", operands));
")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"fltd\\t%0, %1\", operands));
")

;; Truncation insns

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mvfs\\t%0, %1\", operands));
")

;; Zero extension instructions.

(define_expand "zero_extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "register_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "register_operand" "")
	(lshiftrt:SI (match_dup 2)
		     (const_int 16)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI
	 (match_operand:QI 1 "register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"and\\t%0, %1, #255\\t@ zero_extendqihi2\", operands));
")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"and\\t%0, %1, #255\\t@ zero_extendqisi2\", operands));
    case 1:
      return (arm_output_asm_insn (\"ldrb\\t%0, %1\\t@ zero_extendqisi2\", operands));
    }
")

(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "register_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "register_operand" "")
		   (const_int 24)))
   (set (match_operand:HI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{ operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "register_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mvfd\\t%0, %1\", operands));
")

;; Move insns (including loads and stores)

;; XXX Just some ideas about movti.

;;(define_expand "loadti"
;;  [(set (match_operand:TI 0 "register_operand" "")
;;	(mem:TI (match_operand:SI 1 "address_operand" "")))]
;;  "" "")

;;(define_expand "storeti"
;;  [(set (mem:TI (match_operand:TI 0 "address_operand" ""))
;;	(match_operand:TI 1 "register_operand" ""))]
;;  "" "")

;;(define_expand "movti"
;;  [(set (match_operand:TI 0 "general_operand" "")
;;	(match_operand:TI 1 "general_operand" ""))]
;;  ""
;;  "
;;{
;;  rtx insn;
;;
;;  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
;;    operands[1] = copy_to_reg (operands[1]);
;;  if (GET_CODE (operands[0]) == MEM)
;;    insn = gen_storeti (XEXP (operands[0], 0), operands[1]);
;;  else if (GET_CODE (operands[1]) == MEM)
;;    insn = gen_loadti (operands[0], XEXP (operands[1], 0));
;;  else
;;    FAIL;
;;
;;  emit_insn (insn);
;;  DONE;
;;}")

;; Recognise garbage generated above.

;;(define_insn ""
;;  [(set (match_operand:TI 0 "general_operand" "=r,r,r,<,>,m")
;;	(match_operand:TI 1 "general_operand" "<,>,m,r,r,r"))]
;;  ""
;;  "*
;;  {
;;    register mem = (which_alternative < 3);
;;    register char *template;
;;
;;    operands[mem] = XEXP (operands[mem], 0);
;;    switch (which_alternative)
;;      {
;;      case 0: template = \"ldmdb\\t%1!, %M0\"; break;
;;      case 1: template = \"ldmia\\t%1!, %M0\"; break;
;;      case 2: template = \"ldmia\\t%1, %M0\"; break;
;;      case 3: template = \"stmdb\\t%0!, %M1\"; break;
;;      case 4: template = \"stmia\\t%0!, %M1\"; break;
;;      case 5: template = \"stmia\\t%0, %M1\"; break;
;;      }
;;    return (arm_output_asm_insn (template, operands));
;;  }")


(define_insn "movdi"
  [(set (match_operand:DI 0 "di_operand" "=r,r,r,o,r")
	(match_operand:DI 1 "di_operand" "r,n,o,r,F"))]
  ""
  "*
  return (output_move_double (operands));
")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,m")
	(match_operand:SI 1 "general_operand"  "r,n,m,r"))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"mov\\t%0, %1\", operands));
    case 1:
      return (output_mov_immediate (operands));
    case 2:
      if (GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	  &&  CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0)))
	return (arm_output_llc (operands));
      else
	return (arm_output_asm_insn (\"ldr\\t%0, %1\", operands));
    case 3:
      return (arm_output_asm_insn (\"str\\t%1, %0\", operands));
    }
")

;; XXX The movhi stuff isn't as correct or as nice as it could be...

;; Subroutine to load a half word into a register from memory.
;; Operand 0 is the destination register (HImode).
;; Operand 1 is the source address (SImode).
;; Operand 2 is a temporary (SImode).

;;(define_expand "loadhi"
;;  [;; load the whole word (ARM realigns it if not on word boundary)
;;   (set (match_operand:SI 2 "register_operand" "")
;;        (mem:SI (match_operand:SI 1 "address_operand" "")))
;;   ;; quietly forget the upper 16 bits
;;   (set (match_operand:HI 0 "register_operand" "")
;;        (subreg:HI (match_dup 2) 0))]
;;  ""
;;  ""
;;)

;; Load op0 from mem:op1.  Subroutine in case we're reloading and the normal
;; loadhi is not allowed.

;;(define_expand "reloadhi"
;;  [(set (reg:SI 10)
;;	(mem:SI (match_operand:SI 1 "address_operand" "")))
;;   (set (match_operand:HI 0 "register_operand" "")
;;	(subreg:HI (reg:SI 10) 0))]
;;  "" "")

;; Store op0 into mem:op1.  Subroutine in case we're reloading and the normal
;; storehi is not allowed.

(define_expand "restorehi"
  [(set (mem:QI (match_operand 1 "" ""))
	(match_dup 2))
   (set (reg:SI 10)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   (set (mem:QI (plus:SI (match_dup 1) (const_int 1)))
	(reg:QI 10))]
  ""
  "
{
  operands[2] = gen_lowpart (QImode, operands[0]);
  operands[0] = gen_lowpart (SImode, operands[0]);
}")

;; Subroutine to store a half word from a register into memory.
;; Operand 0 is the source register (HImode)
;; Operand 1 is the destination address in a register (SImode)

(define_expand "storehi"
  [;; store the low byte
   (set (mem:QI (match_operand 1 "" "")) (match_dup 3))
   ;; extract the high byte
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   ;; store the high byte
   (set (mem:QI (plus (match_dup 1) (const_int 1)))
	(subreg:QI (match_dup 2) 0))]	;explicit subreg safe
  ""
  "
{ operands[3] = gen_lowpart (QImode, operands[0]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[2] = gen_reg_rtx (SImode); }")

;; Subroutine to store a half word integer constant into memory.
;; Operand 0 is the constant
;; Operand 1 is the destination address in a register (SImode)

(define_expand "storeinthi"
  [;; store the low byte
   (set (mem:QI (match_operand 1 "" "")) (match_operand 0 "" ""))
   ;; store the high byte
   (set (mem:QI (plus (match_dup 1) (const_int 1)))
	(match_dup 2))]
  ""
  "
    {
      int value = INTVAL (operands[0]);

      operands[0] = force_reg (QImode, gen_rtx (CONST_INT, VOIDmode, value & 255));
      operands[2] = force_reg (QImode, gen_rtx (CONST_INT, VOIDmode,(value>>8) & 255));
    }
")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  rtx insn;

  if (reload_in_progress || reload_completed)
    {
      if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == REG)
	insn = gen_restorehi (operands[1], XEXP (operands[0], 0));
      else
	insn = gen_rtx (SET, VOIDmode, operands[0], operands[1]);
    }
  else
    {
      if (GET_CODE (operands[0]) == MEM)
	{
	  if (GET_CODE (operands[1]) == CONST_INT)
	    {
	      insn = gen_storeinthi (operands[1], force_reg (SImode, XEXP (operands[0], 0)));
	    }
	  else
	    {
	      if (GET_CODE (operands[1]) == MEM)
		operands[1] = copy_to_reg (operands[1]);
	      insn = gen_storehi (operands[1], force_reg (SImode, XEXP (operands[0], 0)));
	    }
	}
#if 0
      else if (GET_CODE (operands[1]) == MEM)
	{
	  insn = gen_loadhi (operands[0], XEXP (operands[1], 0),
			     gen_reg_rtx (SImode));
	}
#endif
      else
	insn = gen_rtx (SET, VOIDmode, operands[0], operands[1]);
    }

  emit_insn (insn);
  DONE;
}")

;; Pattern to recognise insn generated default case above

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=r,r,r,m")
	(match_operand:HI 1 "general_operand"  "r,n,m,r"))]
  ""
  "*
  switch (which_alternative)
    {
      case 0: return (arm_output_asm_insn (\"mov\\t%0, %1\\t@movhi\", operands));
      case 1: return (output_mov_immediate (operands));
      case 2: return (arm_output_asm_insn (\"ldr\\t%0, %1\\t@movhi\", operands));
      case 3: return (arm_output_asm_insn (\"str\\t%1, %0\\t@movhi\", operands));
    }
")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=r,r,r,m")
	(match_operand:QI 1 "general_operand" "r,n,m,r"))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"mov\\t%0, %1\", operands));
    case 1:
      return (output_mov_immediate (operands));
    case 2:
      return (arm_output_asm_insn (\"ldrb\\t%0, %1\", operands));
    case 3:
      return (arm_output_asm_insn (\"strb\\t%1, %0\", operands));
    }
")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=f,f,m,f,r,r")
	(match_operand:SF 1 "general_operand" "fG,m,f,r,f,r"))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"mvfs\\t%0, %1\", operands));
    case 1:
      return (arm_output_asm_insn (\"ldfs\\t%0, %1\", operands));
    case 2:
      return (arm_output_asm_insn (\"stfs\\t%1, %0\", operands));
    case 3:
      arm_output_asm_insn(\"stmfd\\tsp!, {%1}\", operands);
      return (arm_output_asm_insn (\"ldfs\\t%0, [sp],#4\", operands));
    case 4:
      arm_output_asm_insn(\"stfs\\t%1, [sp,#-4]!\", operands);
      return (arm_output_asm_insn (\"ldmfd\\tsp!, {%0}\", operands));
    case 5:
      return (arm_output_asm_insn (\"mov\\t%0, %1\", operands));
  }
")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=f,f,m,f,r,r")
	(match_operand:DF 1 "general_operand" "fG,m,f,r,f,r"))]
  ""
  "*
  switch (which_alternative)
    {
      case 0: return (arm_output_asm_insn (\"mvfd\\t%0, %1\", operands));
      case 1: return (arm_output_asm_insn (\"ldfd\\t%0, %1\", operands));
      case 2: return (arm_output_asm_insn (\"stfd\\t%1, %0\", operands));
      case 3: return (output_mov_double_fpu_from_arm (operands));
      case 4: return (output_mov_double_arm_from_fpu (operands));
      case 5: return (output_move_double (operands));
    }
")

;; Comparison and test insns

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "r")
		 (match_operand:SI 1 "arm_rhs_operand" "rI")))]
  ""
  "*
  return (arm_output_asm_insn (\"cmp\\t%0, %1\", operands));
")

(define_insn "tstsi"
  [(set (cc0) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "*
  return (arm_output_asm_insn (\"cmp\\t%0, #0\", operands));
")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "r")
		 (neg:SI (match_operand:SI 1 "arm_rhs_operand" "rI"))))]
  ""
  "*
  return (arm_output_asm_insn (\"cmn\\t%0, %1\", operands));
")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "register_operand" "f")
		 (match_operand:SF 1 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"cmf\\t%0, %1\", operands));
")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "f")
		 (match_operand:DF 1 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"cmf\\t%0, %1\", operands));
")

;; Conditional branch insns

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"beq\\t%l0\", operands));
")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"bne\\t%l0\", operands));
")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"bgt\\t%l0\", operands));
")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"ble\\t%l0\", operands));
")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"bge\\t%l0\", operands));
")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"blt\\t%l0\", operands));
")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"bhi\\t%l0\", operands));
")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"bls\\t%l0\", operands));
")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"bhs\\t%l0\", operands));
")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  return (arm_output_asm_insn (\"blo\\t%l0\", operands));
")

;; Inverted conditional branch insns

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"bne\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"beq\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"ble\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"bgt\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"blt\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"bge\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"bls\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"bhi\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"blo\\t%l0\", operands));
")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0) (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  return (arm_output_asm_insn (\"bhs\\t%l0\", operands));
")

;; Jump and linkage insns
;; `return' is still a jump-to-epilogue...

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
  return (arm_output_asm_insn (\"b\\t%l0\", operands));
")

(define_insn "call"
  [(call (match_operand 0 "memory_operand" "m")
	 (match_operand 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  ""
  "*
  return (output_call (operands));
")

(define_insn "call_value"
  [(set (match_operand 0 "" "=rf")
	(call (match_operand 1 "memory_operand" "m")
	(match_operand 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  ""
  "*
  return (output_call (&operands[1]));
")

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.

(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "" "i"))
	 (match_operand:SI 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  "GET_CODE (operands[0]) == SYMBOL_REF"
  "*
  return (arm_output_asm_insn (\"bl\\t%a0\", operands));
")

(define_insn ""
  [(set (match_operand 0 "register_operand" "=rf")
	(call (mem:SI (match_operand:SI 1 "" "i"))
	(match_operand:SI 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  "GET_CODE(operands[1]) == SYMBOL_REF"
  "*
  return (arm_output_asm_insn (\"bl\\t%a1\", operands));
")

(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "*
  return (arm_output_asm_insn (\"mov\\tpc, %0\\t@ table jump, label %l1\", operands));
")

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "*
  return (arm_output_asm_insn (\"mov\\tpc, %0\\t@ indirect jump\", operands));
")

;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  ""
  "*
  return (arm_output_asm_insn (\"mov\\tr0, r0\\t@ nop\", operands));
")

;; Patterns to allow combination of arithmetic, cond code and shifts

;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=r")
;        (match_operator:SI 1 "shiftable_operator"
;          [(match_operand:SI 2 "register_operand" "r")
;           (match_operator:SI 3 "shift_operator"
;             [(match_operand:SI 4 "register_operand" "r")
;	      (match_operand:SI 5 "nonmemory_operand" "rn")])]))]
;  ""
;  "*
;  return (output_arithmetic_with_shift (operands, FALSE, FALSE));
;  "
;)

;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=r")
;        (match_operator:SI 1 "shiftable_operator"
;          [(match_operator:SI 3 "shift_operator"
;             [(match_operand:SI 4 "register_operand" "r")
;              (match_operand:SI 5 "nonmemory_operand" "rI")])
;           (match_operand:SI 2 "register_operand" "r")]))]
;  ""
;  "*
;  return (output_arithmetic_with_shift (operands, TRUE, FALSE));
;")

;; Patterns to allow combination of arithmetic and multiplication

;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=r")
;        (match_operator:SI 1 "shiftable_operator"
;          [(match_operand:SI 2 "register_operand" "r")
;             (mult:SI
;               (match_operand:SI 3 "register_operand" "r")
;               (match_operand:SI 4 "power_of_two_operand" "n"))]))]
;  ""
;  "*
;  return (output_arithmetic_with_immediate_multiply (operands, FALSE));
;")

; Uncomment this to show combiner problem (see ../COMBINER-PROBLEM).
;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=r")
;        (match_operator:SI 1 "shiftable_operator"
;	  [(mult:SI
;	    (match_operand:SI 3 "register_operand" "r")
;	    (match_operand:SI 4 "power_of_two_operand" "n"))
;	   (match_operand:SI 2 "register_operand" "r")]))]
;  ""
;  "*
;  return (output_arithmetic_with_immediate_multiply (operands, TRUE));
;")

;; Peephole optimizations.

;; When testing a bitset smaller than 9 bits for (un)equality, a
;; shift/and/cmp/b{eq,ne} sequence can be replaced by one tst and the same
;; branch sequence.

;;(define_peephole
;;  [(set (match_operand:SI 0 "register_operand" "=r")
;;	(lshiftrt:SI (match_dup 0)
;;		     (match_operand 1 "immediate_operand" "")))
;;   (set (match_dup 0)
;;	(and:SI (match_dup 0)
;;		(match_operand 2 "immediate_operand" "")))
;;   (set (cc0) (match_dup 0))
;;   (set (pc)
;;	(if_then_else (ne (cc0) (const_int 0))
;;		      (label_ref (match_operand 3 "" ""))
;;		      (pc)))]
;;  "dead_or_set_p (PREV_INSN (insn), operands[0])
;;   && GET_CODE (operands[2]) == CONST_INT && GET_CODE (operands[1]) == CONST_INT
;;   && const_ok_for_arm (INTVAL (operands[2]) << INTVAL (operands[1]))"
;;  "*
;;  operands[2] = gen_rtx (CONST_INT, VOIDmode,
;;                         INTVAL (operands[2]) << INTVAL (operands[1]));
;;  arm_output_asm_insn (\"tst\\t%0, %2\\t\\t@ ph test bitfield\", operands);
;;  return (arm_output_asm_insn (\"bne\\t%l3\", operands));
;;")

;;(define_peephole
;;  [(set (match_operand:SI 0 "register_operand" "=r")
;;	(lshiftrt:SI (match_dup 0)
;;		     (match_operand 1 "immediate_operand" "")))
;;   (set (match_dup 0)
;;	(and:SI (match_dup 0)
;;		(match_operand 2 "immediate_operand" "")))
;;   (set (cc0) (match_dup 0))
;;   (set (pc)
;;	(if_then_else (ne (cc0) (const_int 0))
;;		      (pc)
;;		      (label_ref (match_operand 3 "" ""))))]
;;  "dead_or_set_p (prev_real_insn (insn), operands[0])
;;   && GET_CODE (operands[2]) == CONST_INT && GET_CODE (operands[1]) == CONST_INT
;;   && const_ok_for_arm (INTVAL (operands[2]) << INTVAL (operands[1]))"
;;  "*
;;  operands[2] = gen_rtx (CONST_INT, VOIDmode,
;;                         INTVAL (operands[2]) << INTVAL (operands[1]));
;;  arm_output_asm_insn (\"tst\\t%0, %2\\t\\t@ ph test bitfield\", operands);
;;  return (arm_output_asm_insn (\"beq\\t%l3\", operands));
;;")

;; This allows negative constants to be compared since GCC appears not to try
;; converting them with a NEG.

;;(define_peephole
;;  [(set (match_operand:SI 2 "register_operand" "=r")
;;        (match_operand:SI 1 "immediate_operand" "n"))
;;   (set (cc0)
;;        (compare (match_operand:SI 0 "register_operand" "r")
;;                 (match_dup 1)))]
;;  "const_ok_for_arm (-INTVAL (operands[1]))
;;   && dead_or_set_p (prev_real_insn (insn), operands[0])"
;;  "*
;;  operands[1] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[1]));
;;  return (arm_output_asm_insn (\"cmn\\t%0, %1\\t\\t@ ph negate comparison\", operands));
;;")

;; Local variables:
;; mode:emacs-lisp
;; eval: (setq comment-start ";; ")
;; eval: (setq comment-end "")
;; eval: (setq comment-start-skip ";;+ *")
;; eval: (set-syntax-table (copy-sequence (syntax-table)))
;; eval: (modify-syntax-entry ?[ "(]")
;; eval: (modify-syntax-entry ?] ")[")
;; eval: (modify-syntax-entry ?{ "(}")
;; eval: (modify-syntax-entry ?} "){")
;; End:
