;;- Machine description for GNU compiler, Fujitsu Gmicro Version
;;  Copyright (C) 1990, 1994, 1996 Free Software Foundation, Inc.
;;  Contributed by M.Yuhara, Fujitsu Laboratories LTD.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; Among other things, the copyright
;; notice and this notice must be preserved on all copies.


;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;- instruction definitions

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;;- When naming insn's (operand 0 of define_insn) be careful about using
;;- names from other targets machine descriptions.

;;- cpp macro #define NOTICE_UPDATE_CC is essentially a no-op for the 
;;- gmicro; no compares are eliminated.

;;- The original structure of this file is m68k.md.

;; ??? Work to be done:
;; Add patterns for ACB and SCB instructions.
;; Add define_insn patterns to recognize the insns that extend a byte
;; to a word and add it into a word, etc.

;;- Some of these insn's are composites of several Gmicro op codes.
;;- The assembler (or final @@??) insures that the appropriate one is
;;- selected.

(define_insn ""
  [(set (match_operand:DF 0 "push_operand" "=m")
	(match_operand:DF 1 "general_operand" "rmfF"))]
  ""
  "*
{
  if (FPU_REG_P (operands[1]))
    return \"fmov.d %f1,%0\";
  return output_move_double (operands);
}")

(define_insn ""
  [(set (match_operand:DI 0 "push_operand" "=m")
	(match_operand:DF 1 "general_operand" "rmF"))]
  ""
  "*
{
  return output_move_double (operands);
}")

;; We don't want to allow a constant operand for test insns because
;; (set (cc0) (const_int foo)) has no mode information.  Such insns will
;; be folded while optimizing anyway.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "nonimmediate_operand" "rm"))]
  ""
  "cmp:z.w #0,%0")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "nonimmediate_operand" "rm"))]
  ""
  "cmp:z.h #0,%0")

(define_insn "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "nonimmediate_operand" "rm"))]
  ""
  "cmp:z.b #0,%0")
  

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "general_operand" "fmF"))]
  "TARGET_FPU"
  "*
{
  cc_status.flags = CC_IN_FPU;
  return \"ftst.s %0\";
}")


(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "general_operand" "fmF"))]
  "TARGET_FPU"
  "*
{
  cc_status.flags = CC_IN_FPU;
  return \"ftst.d %0\";
}")

;; compare instructions.

;; (operand0 - operand1)
(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "nonimmediate_operand" "ri,rm")
		 (match_operand:SI 1 "general_operand" "rm,rmi")))]
  ""
  "*
{
  int signed_flag = my_signed_comp (insn);

  if (which_alternative == 0)
    {
      cc_status.flags |= CC_REVERSED;
      if (signed_flag && GET_CODE (operands[0]) == CONST_INT)
	{
	  register rtx xfoo;
	  xfoo = operands[1];
	  operands[0] = operands[1];
	  operands[1] = xfoo;
	  return cmp_imm_word (INTVAL (operands[1]), operands[0]);
	}
      if (signed_flag)
	return \"cmp.w %0,%1\"; 
      return \"cmpu.w %0,%1\"; 
    }
  if (signed_flag)
    {
      if (GET_CODE (operands[1]) == CONST_INT)
	return cmp_imm_word (INTVAL (operands[1]), operands[0]);
      return \"cmp.w %1,%0\"; 
    }
  else
    return \"cmpu.w %1,%0\"; 
}")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "nonimmediate_operand" "ri,rm")
		 (match_operand:HI 1 "general_operand" "rm,rmi")))]
  ""
  "*
{
  int signed_flag = my_signed_comp (insn);

  if (which_alternative == 0)
    {
      cc_status.flags |= CC_REVERSED;
      if (signed_flag)
	return \"cmp.h %0,%1\"; 
      return \"cmpu.h %0,%1\"; 
    }
  if (signed_flag)
    return \"cmp.h %1,%0\"; 
  return \"cmpu.h %1,%0\"; 
}")

(define_insn "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "nonimmediate_operand" "ri,rm")
		 (match_operand:QI 1 "general_operand" "rm,rmi")))]
  ""
  "*
{
  int signed_flag = my_signed_comp (insn);

  if (which_alternative == 0)
    {
      cc_status.flags |= CC_REVERSED;
      if (signed_flag)
	return \"cmp.b %0,%1\"; 
      return \"cmpu.b %0,%1\"; 
    }
  if (signed_flag)
    return \"cmp.b %1,%0\"; 
  return \"cmpu.b %1,%0\"; 
}")


(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "general_operand" "f,mG")
		 (match_operand:DF 1 "general_operand" "fmG,f")))]
  "TARGET_FPU"
  "*
{
  cc_status.flags = CC_IN_FPU;

  if (FPU_REG_P (operands[0]))
    return \"fcmp.d %f1,%f0\";
  cc_status.flags |= CC_REVERSED;
  return \"fcmp.d %f0,%f1\";
}")


(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "general_operand" "f,mG")
		 (match_operand:SF 1 "general_operand" "fmG,f")))]
  "TARGET_FPU"
  "*
{
  cc_status.flags = CC_IN_FPU;
  if (FPU_REG_P (operands[0]))
    return \"fcmp.s %f1,%0\";
  cc_status.flags |= CC_REVERSED;
  return \"fcmp.s %f0,%1\";
}")

;; Recognizers for btst instructions.

(define_insn ""
  [(set (cc0) (zero_extract (match_operand:QI 0 "memory_operand" "m")
			    (const_int 1)
			    (match_operand:SI 1 "general_operand" "rmi")))]
  ""
  "btst %1.w,%0.b")

(define_insn ""
  [(set (cc0) (zero_extract (match_operand:SI 0 "register_operand" "rm")
			    (const_int 1)
			    (match_operand:SI 1 "general_operand" "rmi")))]
  ""
  "btst %1.w,%0.w")

;; The following two patterns are like the previous two
;; except that they use the fact that bit-number operands (offset)
;; are automatically masked to 3 or 5 bits when the base is a register.

(define_insn ""
  [(set (cc0) (zero_extract (match_operand:QI 0 "memory_operand" "m")
			    (const_int 1)
			    (and:SI
			       (match_operand:SI 1 "general_operand" "rmi")
			       (const_int 7))))]
  ""
  "btst %1.w,%0.b")

(define_insn ""
  [(set (cc0) (zero_extract (match_operand:SI 0 "register_operand" "r")
			    (const_int 1)
			    (and:SI
			       (match_operand:SI 1 "general_operand" "rmi")
			       (const_int 31))))]
  ""
  "btst %1.w,%0.w")

; More various size-patterns are allowed for btst, but not
; included yet.  M.Yuhara


(define_insn ""
  [(set (cc0) (and:SI (sign_extend:SI
		       (sign_extend:HI
			(match_operand:QI 0 "nonimmediate_operand" "rm")))
		      (match_operand:SI 1 "general_operand" "i")))]
  "(GET_CODE (operands[1]) == CONST_INT
    && (unsigned) INTVAL (operands[1]) < 0x100
    && exact_log2 (INTVAL (operands[1])) >= 0)"
  "*
{
  register int log = exact_log2 (INTVAL (operands[1]));
  operands[1] = GEN_INT (log);
  return \"btst %1,%0.b\";
}")

; I can add more patterns like above. But not yet.  M.Yuhara


; mtst is supported only by G/300.

(define_insn ""
  [(set (cc0) 
	(and:SI (match_operand:SI 0 "general_operand" "%rmi")
		(match_operand:SI 1 "general_operand" "rm")))]
  "TARGET_G300"
  "*
{
  if (GET_CODE (operands[0]) == CONST_INT)
    return \"mtst.w %0,%1\";
  return \"mtst.w %1,%0\";
}")

(define_insn ""
  [(set (cc0) 
	(and:HI (match_operand:HI 0 "general_operand" "%rmi")
		(match_operand:HI 1 "general_operand" "rm")))]
  "TARGET_G300"
  "*
{
  if (GET_CODE (operands[0]) == CONST_INT)
    return \"mtst.h %0,%1\";
  return \"mtst.h %1,%0\";
}")

(define_insn ""
  [(set (cc0) 
	(and:QI (match_operand:QI 0 "general_operand" "%rmi")
		(match_operand:QI 1 "general_operand" "rm")))]
  "TARGET_G300"
  "*
{
  if (GET_CODE (operands[0]) == CONST_INT)
    return \"mtst.b %0,%1\";
  return \"mtst.b %1,%0\";
}")



;; move instructions

/* added by M.Yuhara */
;; 1.35.04 89.08.28 modification start
;; register_operand -> general_operand
;; ashift -> mult 

(define_insn ""
  [(set (mem:SI (plus:SI
		  (match_operand:SI 0 "general_operand" "r")
		  (ashift:SI
		      (match_operand:SI 1 "general_operand" "r")
		      (const_int 2))))
	(match_operand:SI 2 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %2,@(%0:b,%1*4)\";
}")

(define_insn ""
  [(set (mem:SI (plus:SI
		  (ashift:SI
		      (match_operand:SI 0 "general_operand" "r")
		      (const_int 2))
		  (match_operand:SI 1 "general_operand" "r")))
	(match_operand:SI 2 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %2,@(%1:b,%0*4)\";
}")


(define_insn ""
  [(set (mem:SI (plus:SI
		  (match_operand:SI 0 "register_operand" "r")
		  (mult:SI
		      (match_operand:SI 1 "register_operand" "r")
		      (const_int 4))))
	(match_operand:SI 2 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %2,@(%0:b,%1*4)\";
}")

(define_insn ""
  [(set (mem:SI (plus:SI
		  (mult:SI
		      (match_operand:SI 0 "register_operand" "r")
		      (const_int 4))
		  (match_operand:SI 1 "register_operand" "r")))
	(match_operand:SI 2 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %2,@(%1:b,%0*4)\";
}")


(define_insn ""
  [(set (mem:SI (plus:SI
		  (match_operand:SI 0 "general_operand" "r")
		  (plus:SI
		      (match_operand:SI 1 "register_operand" "r")
		      (match_operand:SI 2 "register_operand" "i"))))
	(match_operand:SI 3 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %3,@(%c2,%0,%1)\";
}")

(define_insn ""
  [(set (mem:SI (plus:SI
		  (plus:SI
		      (match_operand:SI 0 "register_operand" "r")
		      (match_operand:SI 1 "register_operand" "r"))
		  (match_operand:SI 2 "general_operand" "i")))
	(match_operand:SI 3 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %3,@(%c2,%0,%1)\";
}")


(define_insn ""
  [(set (mem:SI (plus:SI
		  (match_operand:SI 0 "general_operand" "i")
		  (plus:SI
		      (match_operand:SI 1 "register_operand" "r")
		      (mult:SI
			  (match_operand:SI 2 "register_operand" "r")
			  (const_int 4)))))
	(match_operand:SI 3 "general_operand" "rmi"))]
  ""
  "*
{
  return \"mov.w %3,@(%1:b,%0,%2*4)\";
}")

;; 89.08.28 1.35.04 modification end

;; Should add "!" to op2 ??

;; General move-address-to-operand should handle these.
;; If that does not work, please figure out why.

;(define_insn ""
;  [(set (match_operand:SI 0 "push_operand" "=m")
;	(plus:SI
;	    (match_operand:SI 1 "immediate_operand" "i")
;	    (match_operand:SI 2 "general_operand" "r")))]
;  ""
;  "mova.w @(%c1,%2),%-")

;(define_insn ""
;  [(set (match_operand:SI 0 "push_operand" "=m")
;	(plus:SI
;	    (match_operand:SI 1 "general_operand" "r")
;	    (match_operand:SI 2 "immediate_operand" "i")))]
;  ""
;  "mova.w @(%c2,%1),%-")


(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=m")
	(minus:SI
	    (match_operand:SI 1 "general_operand" "r")
	    (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "mova.w @(%n2,%1),%-")



;; General case of fullword move.

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(match_operand:SI 1 "general_operand" "rmi"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return mov_imm_word (INTVAL (operands[1]), operands[0]);
  /* if (address_operand (operands[1], SImode))
     return \"mova.w %1,%0\"; */
  if (push_operand (operands[0], SImode))
    return \"mov.w %1,%-\";
  return \"mov.w %1,%0\";
}")

/* pushsi 89.08.10 for test M.Yuhara */
/*
(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=m")
	(match_operand:SI 1 "general_operand" "rmi"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return mov_imm_word (INTVAL (operands[1]), operands[0]);
  if (push_operand (operands[0], SImode))
    return \"mov.w %1,%-\";
  return \"mov.w %1,%0\";
}")
*/


(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(match_operand:HI 1 "general_operand" "rmi"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"mov.h %1,%-\";
  return \"mov.h %1,%0\";
}")

;; Is the operand constraint "+" necessary ????
;; Should I check push_operand ????

(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "general_operand" "+rm"))
	(match_operand:HI 1 "general_operand" "rmi"))]
  ""
  "mov.h %1,%0");

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(match_operand:QI 1 "general_operand" "rmi"))]
  ""
  "*
{
  if (GREG_P (operands[0]))
    {
      if (CONSTANT_P (operands[1]))
	return \"mov:l %1,%0.w\";
      else
	return \"mov:l %1.b,%0.w\";
    }
  if (GREG_P (operands[1]))
    return \"mov:s %1.w,%0.b\";
  return \"mov.b %1,%0\";
}")

(define_insn "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "general_operand" "+rm"))
	(match_operand:QI 1 "general_operand" "rmi"))]
  ""
  "mov.b %1,%0")


(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=f,mf,rm,fr")
	(match_operand:SF 1 "general_operand" "mfF,f,rmF,fr"))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	return output_move_const_single (operands);
      return \"fmov.s %1,%0\";
    case 1:
      return \"fmov.s %1,%0\";
    case 2:
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	return output_move_const_single (operands);
      return \"mov.w %1,%0\";
    case 3:
      if (FPU_REG_P (operands[0]))
	return \"mov.w %1,%-\\n\\tfmov.s %+,%0\";
      return \"fmov.s %1,%-\\n\\tmov.w %+,%0\";
    }
}")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=f,mf,rm,fr")
	(match_operand:DF 1 "general_operand" "mfF,f,rmF,fr"))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	return output_move_const_double (operands);
      return \"fmov.d %1,%0\";
    case 1:
      return \"fmov.d %1,%0\";
    case 2:
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	return output_move_const_double (operands);
      return output_move_double (operands);
    case 3:
      if (FPU_REG_P (operands[0]))
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
	  output_asm_insn (\"mov.w %1,%-\", xoperands);
	  output_asm_insn (\"mov.w %1,%-\", operands);
	  return \"fmov.d %+,%0\";
	}
      else
	{
	  output_asm_insn (\"fmov.d %f1,%-\", operands);
	  output_asm_insn (\"mov.w %+,%0\", operands);
	  operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  return \"mov.w %+,%0\";
	}
    }
}")


;; movdi can apply to fp regs in some cases
;; Must check again.  you can use fsti/fldi, etc.
;; FPU reg should be included ??
;; 89.12.13 for test

(define_insn "movdi"
  ;; Let's see if it really still needs to handle fp regs, and, if so, why.
  [(set (match_operand:DI 0 "general_operand" "=rm,&r,&ro")
	(match_operand:DI 1 "general_operand" "rF,m,roiF"))]
  ""
  "*
{
  if (FPU_REG_P (operands[0]))
    {
      if (FPU_REG_P (operands[1]))
	return \"fmov.d %1,%0\";
      if (REG_P (operands[1]))
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
	  output_asm_insn (\"mov.w %1,%-\", xoperands);
	  output_asm_insn (\"mov.w %1,%-\", operands);
	  return \"fmov.d %+,%0\";
	}
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	return output_move_const_double (operands);
      return \"fmov.d %f1,%0\";
    }
  else if (FPU_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	{
	  output_asm_insn (\"fmov.d %f1,%-\;mov.w %+,%0\", operands);
	  operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  return \"mov.w %+,%0\";
	}
      else
        return \"fmov.d %f1,%0\";
    }
  return output_move_double (operands);
}
")


;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it won't successfully combine with anything.

;; This is dangerous when %0 and %1 overlapped !!!!!
;; Ugly code...

(define_insn "movstrhi"
  [(set (match_operand:BLK 0 "general_operand" "=m")
	(match_operand:BLK 1 "general_operand" "m"))
   (use (match_operand:HI 2 "general_operand" "rmi"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))]
  ""
  "*
{
  int op2const;
  rtx tmpx;

  if (CONSTANT_P (operands[1]))
    {
      fprintf (stderr, \"smov 1 const err \");
      abort ();
    }
  else if (GET_CODE (operands[1]) == REG)
    {
      fprintf (stderr, \"smov 1 reg err \");
      abort ();
    }
  else if (GET_CODE (operands[1]) == MEM)
    {
      tmpx = XEXP (operands[1], 0);
      if (CONSTANT_ADDRESS_P (tmpx) || GREG_P (tmpx))
	{
	  operands[1] = tmpx;
	  output_asm_insn (\"mov.w %1,r0\", operands);
	}
      else
	{
	  output_asm_insn (\"mova %1,r0\", operands);
	}
    }
  else
    {
      fprintf (stderr, \"smov 1 else err \");
      abort ();
      output_asm_insn (\"mova.w %p1,r0\", operands);
    }
    
  if (CONSTANT_P (operands[0]))
    {
      fprintf (stderr, \"smov 0 const err \");
      abort ();
    }
  else if (GET_CODE (operands[0]) == REG)
    {
      fprintf (stderr, \"smov 0 reg err \");
      abort ();
    }
  else if (GET_CODE (operands[0]) == MEM)
    {
      tmpx = XEXP (operands[0], 0);
      if (CONSTANT_ADDRESS_P (tmpx) || GREG_P (tmpx))
	{
	  operands[0] = tmpx;
	  output_asm_insn (\"mov.w %0,r1\", operands);
	}
      else
	{
	  output_asm_insn (\"mova %0,r1\", operands);
	}
    }
  else
    {
      fprintf (stderr, \"smov 0 else err \");
      abort ();
    }
    
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      op2const = INTVAL (operands[2]);
      if (op2const % 4 != 0)
	{
	  output_asm_insn (\"mov.w %2,r2\", operands);
	  return \"smov/n/f.b\";
	}
      op2const = op2const / 4;
      if (op2const <= 4)
	{
	  if (op2const == 0)
	    abort (0);
	  if (op2const == 1)
	    return \"mov.w @r0,@r1\";
	  output_asm_insn (\"mov.w @r0,@r1\", operands);
	  if (op2const == 2)
	    return \"mov.w @(4,r0),@(4,r1)\";
	  output_asm_insn (\"mov.w @(4,r0),@(4,r1)\", operands);
	  if (op2const == 3)
	    return \"mov.w @(8,r0),@(8,r1)\";
	  output_asm_insn (\"mov.w @(8,r0),@(8,r1)\", operands);
	  return \"mov.w @(12,r0),@(12,r1)\";
	}
	    
      operands[2] = GEN_INT (op2const);
      output_asm_insn (\"mov.w %2,r2\", operands);
      return \"smov/n/f.w\";
    }
  else
    {
      fprintf (stderr, \"smov 0 else err \");
      abort ();
      output_asm_insn (\"mov %2.h,r2.w\", operands);
      return \"smov/n/f.b\";
    }

}")

;; M.Yuhara 89.08.24
;; experiment on the built-in strcpy (__builtin_smov)
;;
;; len = 0 means unknown string length.
;;
;; mem:SI is dummy. Necessary so as not to be deleted by optimization.
;; Use of BLKmode would be better...
;;
;;
(define_insn "smovsi"
  [(set (mem:SI (match_operand:SI 0 "general_operand" "=rm"))
	(mem:SI (match_operand:SI 1 "general_operand" "rm")))
   (use (match_operand:SI 2 "general_operand" "i"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))]
  ""
  "*
{
  int len, wlen, blen, offset;
  char tmpstr[128];
  rtx xoperands[1];

  len = INTVAL (operands[2]);
  output_asm_insn (\"mov.w %1,r0\\t; begin built-in strcpy\", operands);
  output_asm_insn (\"mov.w %0,r1\", operands);

  if (len == 0)
    {
      output_asm_insn (\"mov:z.w #0,r2\", operands);
      output_asm_insn (\"mov:z.w #0,r3\", operands);
      return \"smov/eq/f.b\\t; end built-in strcpy\";
    }

  wlen = len / 4;
  blen = len - wlen * 4;

  if (wlen > 0)
    {
      if (len <= 40 && !TARGET_FORCE_SMOV)
	{
	  output_asm_insn (\"mov.w @r0,@r1\", operands);
	  offset = 4;
	  while ( (blen = len - offset) > 0)
	    {
	      if (blen >= 4)
		{
		  sprintf (tmpstr, \"mov.w @(%d,r0),@(%d,r1)\",
			   offset, offset);
		  output_asm_insn (tmpstr, operands);
		  offset += 4;
		}
	      else if (blen >= 2)
		{
		  sprintf (tmpstr, \"mov.h @(%d,r0),@(%d,r1)\",
			   offset, offset);
		  output_asm_insn (tmpstr, operands);
		  offset += 2;
		}
	      else
		{
		  sprintf (tmpstr, \"mov.b @(%d,r0),@(%d,r1)\",
			   offset, offset);
		  output_asm_insn (tmpstr, operands);
		  offset++;
		}
	    }
	  return \"\\t\\t; end built-in strcpy\";
	}
      else
	{
	  xoperands[0] = GEN_INT (wlen);
	  output_asm_insn (\"mov.w %0,r2\", xoperands);
	  output_asm_insn (\"smov/n/f.w\", operands);
	}
    }

  if (blen >= 2)
    {
      output_asm_insn (\"mov.h @r0,@r1\", operands);
      if (blen == 3)
	output_asm_insn (\"mov.b @(2,r0),@(2,r1)\", operands);
    }
  else if (blen == 1)
    {
      output_asm_insn (\"mov.b @r0,@r1\", operands);
    }

  return \"\\t\\t; end built-in strcpy\";
}")

;; truncation instructions
(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(truncate:QI
	 (match_operand:SI 1 "general_operand" "rmi")))]
  ""
  "mov %1.w,%0.b")
;  "*
;{
;  if (GET_CODE (operands[0]) == REG)
;    return \"mov.w %1,%0\";
;  if (GET_CODE (operands[1]) == MEM)
;    operands[1] = adj_offsettable_operand (operands[1], 3);
;  return \"mov.b %1,%0\";
;}")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(truncate:QI
	 (match_operand:HI 1 "general_operand" "rmi")))]
  ""
  "mov %1.h,%0.b")
;  "*
;{
;  if (GET_CODE (operands[0]) == REG)
;    return \"mov.h %1,%0\";
;  if (GET_CODE (operands[1]) == MEM)
;    operands[1] = adj_offsettable_operand (operands[1], 1);
;  return \"mov.b %1,%0\";
;}")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(truncate:HI
	 (match_operand:SI 1 "general_operand" "rmi")))]
  ""
  "mov %1.w,%0.h")
;  "*
;{
;  if (GET_CODE (operands[0]) == REG)
;    return \"mov.w %1,%0\";
;  if (GET_CODE (operands[1]) == MEM)
;    operands[1] = adj_offsettable_operand (operands[1], 2);
;  return \"mov.h %1,%0\";
;}")

;; zero extension instructions
;; define_expand (68k) -> define_insn (Gmicro)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "rm")))]
  ""
  "movu %1.h,%0.w")


(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
        (zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "movu %1.b,%0.h")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "movu %1.b,%0.w")


;; sign extension instructions

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "rm")))]
  ""
  "mov %1.h,%0.w")


(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
        (sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "mov %1.b,%0.h")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "rm")))]
  ""
  "mov %1.b,%0.w")



;; Conversions between float and double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "general_operand" "=*frm,f")
	(float_extend:DF
	  (match_operand:SF 1 "general_operand" "f,rmF")))]
  "TARGET_FPU"
  "*
{
  if (FPU_REG_P (operands[0]))
    {
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	return output_move_const_double (operands);
      if (GREG_P (operands[1]))
	{
	  output_asm_insn (\"mov.w %1,%-\", operands);
	  return \"fmov %+.s,%0.d\";
	}
      return \"fmov %1.s,%0.d\";
    }
  else
    {
      if (GREG_P (operands[0]))
	{
	  output_asm_insn (\"fmov %1.s,%-.d\", operands);
	  output_asm_insn (\"mov.w %+,%0\", operands);
	  operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  return \"mov.w %+,%0\";
	}
      return \"fmov %1.s,%0.d\";
    }
}")


(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "general_operand" "=rfm")
	(float_truncate:SF
	  (match_operand:DF 1 "general_operand" "f")))]
  "TARGET_FPU"
  "*
{
  if (GREG_P (operands[0]))
    {
      output_asm_insn (\"fmov %1.d,%-.s\", operands);
      return \"mov.w %+,%0\";
    }
  return \"fmov %1.d,%0.s\";
}")

;; Conversion between fixed point and floating point.
;; Note that among the fix-to-float insns
;; the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(float:SF (match_operand:SI 1 "general_operand" "rmi")))]
  "TARGET_FPU"
  "fldi %1.w,%0.s")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(float:DF (match_operand:SI 1 "general_operand" "rmi")))]
  "TARGET_FPU"
  "fldi %1.w,%0.d")

(define_insn "floathisf2"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(float:SF (match_operand:HI 1 "general_operand" "rmi")))]
  "TARGET_FPU"
  "fldi %1.h,%0.s")

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(float:DF (match_operand:HI 1 "general_operand" "rmi")))]
  "TARGET_FPU"
  "fldi %1.h,%0.d")

(define_insn "floatqisf2"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(float:SF (match_operand:QI 1 "general_operand" "rmi")))]
  "TARGET_FPU"
  "fldi %1.b,%0.s")

(define_insn "floatqidf2"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(float:DF (match_operand:QI 1 "general_operand" "rmi")))]
  "TARGET_FPU"
  "fldi %1.b,%0.d")

;;; Convert a float to a float whose value is an integer.
;;; This is the first stage of converting it to an integer type.
;
;(define_insn "ftruncdf2"
;  [(set (match_operand:DF 0 "general_operand" "=f")
;	(fix:DF (match_operand:DF 1 "general_operand" "fFm")))]
;  "TARGET_FPU"
;  "*
;{
;  return \"fintrz.d %f1,%0\";
;}")
;
;(define_insn "ftruncsf2"
;  [(set (match_operand:SF 0 "general_operand" "=f")
;	(fix:SF (match_operand:SF 1 "general_operand" "fFm")))]
;  "TARGET_FPU"
;  "*
;{
;  return \"fintrz.s %f1,%0\";
;}")

;; Convert a float to an integer.

(define_insn "fix_truncsfqi2"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(fix:QI (fix:SF (match_operand:SF 1 "general_operand" "f"))))]
  "TARGET_FPU"
  "fsti %1.s,%0.b")

(define_insn "fix_truncsfhi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(fix:HI (fix:SF (match_operand:SF 1 "general_operand" "f"))))]
  "TARGET_FPU"
  "fsti %1.s,%0.h")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(fix:SI (fix:SF (match_operand:SF 1 "general_operand" "f"))))]
  "TARGET_FPU"
  "fsti %1.s,%0.w")

(define_insn "fix_truncdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(fix:QI (fix:DF (match_operand:DF 1 "general_operand" "f"))))]
  "TARGET_FPU"
  "fsti %1.d,%0.b")

(define_insn "fix_truncdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(fix:HI (fix:DF (match_operand:DF 1 "general_operand" "f"))))]
  "TARGET_FPU"
  "fsti %1.d,%0.h")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(fix:SI (fix:DF (match_operand:DF 1 "general_operand" "f"))))]
  "TARGET_FPU"
  "fsti %1.d,%0.w")


;;; Special add patterns
;;; 89.09.28

;; This should be redundant; please find out why regular addsi3
;; fails to match this case.

;(define_insn ""
;  [(set (mem:SI (plus:SI
;		    (plus:SI (match_operand 0 "general_operand" "r")
;			     (match_operand 1 "general_operand" "r"))
;		    (match_operand 2 "general_operand" "i")))
;	(plus:SI
;	    (mem:SI (plus:SI
;			(plus:SI (match_dup 0)
;				 (match_dup 1))
;			(match_dup 2)))
;	    (match_operand 3 "general_operand" "rmi")))]
;  ""
;  "add.w %3,@(%c2,%0,%1)")


;; add instructions

;; Note that the last two alternatives are near-duplicates
;; in order to handle insns generated by reload.
;; This is needed since they are not themselves reloaded,
;; so commutativity won't apply to them.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm,!r,!r")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,r,ri")
		 (match_operand:SI 2 "general_operand" "rmi,ri,r")))]
  ""
  "*
{
  if (which_alternative == 0)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  operands[1] = operands[2];
	  return add_imm_word (INTVAL (operands[1]), operands[0], &operands[1]);
	}
      else
	return \"add.w %2,%0\";
    }
  else
    {
      if (GET_CODE (operands[1]) == REG
	  && REGNO (operands[0]) == REGNO (operands[1]))
	return \"add.w %2,%0\";
      if (GET_CODE (operands[2]) == REG
	  && REGNO (operands[0]) == REGNO (operands[2]))
	return \"add.w %1,%0\";

      if (GET_CODE (operands[1]) == REG)
	{
	  if (GET_CODE (operands[2]) == REG)
	    return \"mova.w @(%1,%2),%0\";
	  else
	    return \"mova.w @(%c2,%1),%0\";
	}
      else
	return \"mova.w @(%c1,%2),%0\";
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(plus:SI (match_operand:SI 1 "general_operand" "0")
		 (sign_extend:SI (match_operand:HI 2 "nonimmediate_operand" "rmi"))))]
  ""
  "*
{
  if (CONSTANT_P (operands[2]))
    {
      operands[1] = operands[2];
      return add_imm_word (INTVAL (operands[1]), operands[0], &operands[1]);
    }
  else
    return \"add %2.h,%0.w\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(plus:HI (match_operand:HI 1 "general_operand" "%0")
		 (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) < 0)
    return \"sub.h #%n2,%0\";
  if (GREG_P (operands[0]))
    {
      if (CONSTANT_P (operands[2]))
	return \"add:l %2,%0.w\";
      else
	return \"add:l %2.h,%0.w\";
    }
  return \"add.h %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:HI 0 "general_operand" "+rm"))
	(plus:HI (match_dup 0)
		 (match_operand:HI 1 "general_operand" "rmi")))]
  ""
  "add.h %1,%0")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(plus:QI (match_operand:QI 1 "general_operand" "%0")
		 (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) < 0)
    return \"sub.b #%n2,%0\";
  if (GREG_P (operands[0]))
    {
      if (CONSTANT_P (operands[2]))
	return \"add:l %2,%0.w\";
      else
	return \"add:l %2.b,%0.w\";
    }
  return \"add.b %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:QI 0 "general_operand" "+rm"))
	(plus:QI (match_dup 0)
		 (match_operand:QI 1 "general_operand" "rmi")))]
  ""
  "add.b %1,%0")

(define_insn "adddf3"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(plus:DF (match_operand:DF 1 "general_operand" "%0")
		 (match_operand:DF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fadd.d %f2,%0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(plus:SF (match_operand:SF 1 "general_operand" "%0")
		 (match_operand:SF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fadd.s %f2,%0")

;; subtract instructions

(define_insn "subsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm,!r")
	(minus:SI (match_operand:SI 1 "general_operand" "0,r")
		  (match_operand:SI 2 "general_operand" "rmi,i")))]
  ""
  "*
{
  if (which_alternative == 0
      || (GET_CODE (operands[1]) == REG
	  && REGNO (operands[0]) == REGNO (operands[1])))
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  operands[1] = operands[2];
	  return sub_imm_word (INTVAL (operands[1]),
			       operands[0], &operands[1]);
	}
      else
	return \"sub.w %2,%0\";
    }
  else
    return \"mova.w @(%n2,%1),%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(minus:SI (match_operand:SI 1 "general_operand" "0")
		  (sign_extend:SI (match_operand:HI 2 "nonimmediate_operand" "rmi"))))]
  ""
  "sub %2.h,%0.w")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(minus:HI (match_operand:HI 1 "general_operand" "0")
		  (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) < 0
      && INTVAL (operands[2]) != 0x8000)
    return \"add.h #%n2,%0\";
  return \"sub.h %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:HI 0 "general_operand" "+rm"))
	(minus:HI (match_dup 0)
		  (match_operand:HI 1 "general_operand" "rmi")))]
  ""
  "sub.h %1,%0")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(minus:QI (match_operand:QI 1 "general_operand" "0")
		  (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) < 0
      && INTVAL (operands[2]) != 0x80)
    return \"add.b #%n2,%0\";
  return \"sub.b %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:QI 0 "general_operand" "+rm"))
	(minus:QI (match_dup 0)
		  (match_operand:QI 1 "general_operand" "rmi")))]
  ""
  "sub.b %1,%0")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(minus:DF (match_operand:DF 1 "general_operand" "0")
		  (match_operand:DF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fsub.d %f2,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(minus:SF (match_operand:SF 1 "general_operand" "0")
		  (match_operand:SF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fsub.s %f2,%0")


;; multiply instructions

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(mult:QI (match_operand:QI 1 "general_operand" "%0")
		 (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "mul.b %2,%0")


(define_insn "mulhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(mult:HI (match_operand:HI 1 "general_operand" "%0")
		 (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "mul.h %2,%0")

;; define_insn "mulhisi3"

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(mult:SI (match_operand:SI 1 "general_operand" "%0")
		 (match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "mul.w %2,%0")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(mult:DF (match_operand:DF 1 "general_operand" "%0")
		 (match_operand:DF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fmul.d %f2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(mult:SF (match_operand:SF 1 "general_operand" "%0")
		 (match_operand:SF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fmul.s %f2,%0")


;; divide instructions

(define_insn "divqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(div:QI (match_operand:QI 1 "general_operand" "0")
		(match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "div.b %2,%0")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(div:HI (match_operand:HI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "div.h %2,%0")

(define_insn "divhisi3"
  [(set (match_operand:HI 0 "general_operand" "=r")
	(div:HI (match_operand:SI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "div %2.h,%0.w")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(div:SI (match_operand:SI 1 "general_operand" "0")
		(match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "div.w %2,%0")

(define_insn "udivqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(udiv:QI (match_operand:QI 1 "general_operand" "0")
		 (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "divu.b %2,%0")

(define_insn "udivhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(udiv:HI (match_operand:HI 1 "general_operand" "0")
		 (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "divu.h %2,%0")

(define_insn "udivhisi3"
  [(set (match_operand:HI 0 "general_operand" "=r")
	(udiv:HI (match_operand:SI 1 "general_operand" "0")
		 (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "divu %2.h,%0.w")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(udiv:SI (match_operand:SI 1 "general_operand" "0")
		 (match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "divu.w %2,%0")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(div:DF (match_operand:DF 1 "general_operand" "0")
		(match_operand:DF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fdiv.d %f2,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(div:SF (match_operand:SF 1 "general_operand" "0")
		(match_operand:SF 2 "general_operand" "fmG")))]
  "TARGET_FPU"
  "fdiv.s %f2,%0")

;; Remainder instructions.

(define_insn "modqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(mod:QI (match_operand:QI 1 "general_operand" "0")
		(match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "rem.b %2,%0")

(define_insn "modhisi3"
  [(set (match_operand:HI 0 "general_operand" "=r")
	(mod:HI (match_operand:SI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "rem.h %2,%0")

(define_insn "umodqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(umod:QI (match_operand:QI 1 "general_operand" "0")
		 (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "remu.b %2,%0")

(define_insn "umodhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(umod:HI (match_operand:HI 1 "general_operand" "0")
		 (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "remu.h %2,%0")

(define_insn "umodhisi3"
  [(set (match_operand:HI 0 "general_operand" "=r")
	(umod:HI (match_operand:SI 1 "general_operand" "0")
		 (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "remu %2.h,%0.w")

;; define_insn "divmodsi4"

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(udiv:SI (match_operand:SI 1 "general_operand" "0")
		 (match_operand:SI 2 "general_operand" "rmi")))
   (set (match_operand:SI 3 "general_operand" "=r")
	(umod:SI (match_dup 1) (match_dup 2)))]
  ""
  "mov.w #0,%3;divx.w %2,%0,%3")

;; logical-and instructions

(define_insn "andsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(and:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (INTVAL (operands[2]) | 0xffff) == 0xffffffff
      && (GREG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
   
    { 
      if (GET_CODE (operands[0]) != REG)
        operands[0] = adj_offsettable_operand (operands[0], 2);
      operands[2] = GEN_INT (INTVAL (operands[2]) & 0xffff);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      return \"and.h %2,%0\";
    }
  return \"and.w %2,%0\";
}")

(define_insn "andhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(and:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "and.h %2,%0")

(define_insn "andqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(and:QI (match_operand:QI 1 "general_operand" "%0")
		(match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "and.b %2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(and:SI (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "rm"))
		(match_operand:SI 2 "general_operand" "0")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return \"and %1,%0.w\";
  return \"and %1.h,%0.w\";
}")


(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(and:SI (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "rm"))
		(match_operand:SI 2 "general_operand" "0")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return \"and %1,%0.w\";
  return \"and %1.b,%0.w\";
}")

;; inclusive-or instructions

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(ior:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "*
{
  register int logval;
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >> 16 == 0
      && (GREG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
    { 
      if (GET_CODE (operands[0]) != REG)
        operands[0] = adj_offsettable_operand (operands[0], 2);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      return \"or.h %2,%0\";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (logval = exact_log2 (INTVAL (operands[2]))) >= 0
      && (GREG_P (operands[0])
	  || offsettable_memref_p (operands[0])))
    { 
      if (GREG_P (operands[0]))
	{
	  if (logval < 7)
	    {
	      operands[1] = GEN_INT (7 - logval);
	      return \"bset.b %1,%0\";
	    }
	  operands[1] = GEN_INT (31 - logval);
	  return \"bset.w %1,%0\";
	}
      else
        {
	  operands[0] = adj_offsettable_operand (operands[0], 3 - (logval / 8));
	  operands[1] = GEN_INT (7 - (logval % 8));
	}
      return \"bset.b %1,%0\";
    }
  return \"or.w %2,%0\";
}")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(ior:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "or.h %2,%0")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(ior:QI (match_operand:QI 1 "general_operand" "%0")
		(match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "or.b %2,%0")

;; xor instructions

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(xor:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >> 16 == 0
      && (offsettable_memref_p (operands[0]) || GREG_P (operands[0])))
    { 
      if (! GREG_P (operands[0]))
	operands[0] = adj_offsettable_operand (operands[0], 2);
      /* Do not delete a following tstl %0 insn; that would be incorrect.  */
      CC_STATUS_INIT;
      return \"xor.h %2,%0\";
    }
  return \"xor.w %2,%0\";
}")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(xor:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "xor.h %2,%0")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(xor:QI (match_operand:QI 1 "general_operand" "%0")
		(match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "xor.b %2,%0")

;; negation instructions

(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(neg:SI (match_operand:SI 1 "general_operand" "0")))]
  ""
  "neg.w %0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(neg:HI (match_operand:HI 1 "general_operand" "0")))]
  ""
  "neg.h %0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(neg:QI (match_operand:QI 1 "general_operand" "0")))]
  ""
  "neg.b %0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(neg:SF (match_operand:SF 1 "general_operand" "fmF")))]
  "TARGET_FPU"
  "fneg.s %f1,%0")


(define_insn "negdf2"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(neg:DF (match_operand:DF 1 "general_operand" "fmF")))]
  "TARGET_FPU"
  "fneg.d %f1,%0")


;; Absolute value instructions

(define_insn "abssf2"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(abs:SF (match_operand:SF 1 "general_operand" "fmF")))]
  "TARGET_FPU"
  "fabs.s %f1,%0")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(abs:DF (match_operand:DF 1 "general_operand" "fmF")))]
  "TARGET_FPU"
  "fabs.d %f1,%0")


;; one complement instructions

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(not:SI (match_operand:SI 1 "general_operand" "0")))]
  ""
  "not.w %0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(not:HI (match_operand:HI 1 "general_operand" "0")))]
  ""
  "not.h %0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(not:QI (match_operand:QI 1 "general_operand" "0")))]
  ""
  "not.b %0")

;; Optimized special case of shifting.
;; Must precede the general case.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 24)))]
  "GET_CODE (XEXP (operands[1], 0)) != POST_INC
   && GET_CODE (XEXP (operands[1], 0)) != PRE_DEC"
  "mov:l %1.b,%0.w")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 24)))]
  "GET_CODE (XEXP (operands[1], 0)) != POST_INC
   && GET_CODE (XEXP (operands[1], 0)) != PRE_DEC"
  "movu %1.b,%0.w")

(define_insn ""
  [(set (cc0) (compare (match_operand:QI 0 "general_operand" "i")
		       (lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
				    (const_int 24))))]
  "(GET_CODE (operands[0]) == CONST_INT
    && (INTVAL (operands[0]) & ~0xff) == 0)"
  "*
{
  cc_status.flags |= CC_REVERSED;
  if (my_signed_comp (insn))
    return \"cmp.b %0,%1\";
  return \"cmpu.b %0,%1\";
}")

(define_insn ""
  [(set (cc0) (compare (lshiftrt:SI (match_operand:SI 0 "memory_operand" "m")
				    (const_int 24))
		       (match_operand:QI 1 "general_operand" "i")))]
  "(GET_CODE (operands[1]) == CONST_INT
    && (INTVAL (operands[1]) & ~0xff) == 0)"
  "*
  if (my_signed_comp (insn))
	return \"cmp.b %1,%0\";
  return \"cmpu.b %1,%0\";
")

(define_insn ""
  [(set (cc0) (compare (match_operand:QI 0 "general_operand" "i")
		       (ashiftrt:SI (match_operand:SI 1 "memory_operand" "m")
				    (const_int 24))))]
  "(GET_CODE (operands[0]) == CONST_INT
    && ((INTVAL (operands[0]) + 0x80) & ~0xff) == 0)"
  "*
  cc_status.flags |= CC_REVERSED;
  if (my_signed_comp (insn))
	return \"cmp.b %0,%1\";
  return \"cmpu.b %0,%1\";
")

(define_insn ""
  [(set (cc0) (compare (ashiftrt:SI (match_operand:SI 0 "memory_operand" "m")
				    (const_int 24))
		       (match_operand:QI 1 "general_operand" "i")))]
  "(GET_CODE (operands[1]) == CONST_INT
    && ((INTVAL (operands[1]) + 0x80) & ~0xff) == 0)"
  "*
  if (my_signed_comp (insn))
	return \"cmp.b %1,%0\";
  return \"cmpu.b %1,%0\";
")

;; arithmetic shift instructions
;; We don't need the shift memory by 1 bit instruction

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(ashift:SI (match_operand:SI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "sha.w %2,%0")

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(ashift:HI (match_operand:HI 1 "general_operand" "0")
		   (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "sha.h %2,%0")

(define_insn "ashlqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(ashift:QI (match_operand:QI 1 "general_operand" "0")
		   (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "sha.b %2,%0")

;; Arithmetic right shift on the Gmicro works by negating the shift count

;; ashiftrt -> ashift
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(ashift:SI (match_operand:SI 1 "general_operand" "0")
		     (match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "{ operands[2] = negate_rtx (SImode, operands[2]); }")

;; ashiftrt -> ashift
(define_expand "ashrhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(ashift:HI (match_operand:HI 1 "general_operand" "0")
		     (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  " { operands[2] = negate_rtx (HImode, operands[2]); }")

;; ashiftrt -> ashift
(define_expand "ashrqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(ashift:QI (match_operand:QI 1 "general_operand" "0")
		     (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  " { operands[2] = negate_rtx (QImode, operands[2]); }")

;; logical shift instructions

;; Logical right shift on the gmicro works by negating the shift count,
;; then emitting a right shift with the shift count negated.  This means
;; that all actual shift counts in the RTL will be positive.  This 
;; prevents converting shifts to ZERO_EXTRACTs with negative positions,
;; which isn't valid.

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "0")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "shl.w %n2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "rm"))))]
  ""
  "shl.w %2,%0")

(define_expand "lshrhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "g")
		     (match_operand:HI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, HImode, negate_rtx (HImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "0")
		     (match_operand:HI 2 "const_int_operand" "n")))]
  ""
  "shl.h %n2,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "0")
		     (neg:HI (match_operand:HI 2 "general_operand" "rm"))))]
  ""
  "shl.h %2,%0")

(define_expand "lshrqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(lshiftrt:QI (match_operand:QI 1 "general_operand" "g")
		     (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx (NEG, QImode, negate_rtx (QImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(lshiftrt:QI (match_operand:QI 1 "general_operand" "0")
		     (match_operand:QI 2 "const_int_operand" "n")))]
  ""
  "shl.b %n2,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(lshiftrt:QI (match_operand:QI 1 "general_operand" "0")
		     (neg:QI (match_operand:QI 2 "general_operand" "rm"))))]
  ""
  "shl.b %2,%0")

;; rotate instructions

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(rotate:SI (match_operand:SI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "rmi")))]
  ""
  "rol.w %2,%0")

(define_insn "rotlhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(rotate:HI (match_operand:HI 1 "general_operand" "0")
		   (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  "rol.h %2,%0")

(define_insn "rotlqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(rotate:QI (match_operand:QI 1 "general_operand" "0")
		   (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  "rol.b %2,%0")

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(rotatert:SI (match_operand:SI 1 "general_operand" "0")
		     (match_operand:SI 2 "general_operand" "rmi")))]
  ""
  " { operands[2] = negate_rtx (SImode, operands[2]); }")

(define_expand "rotrhi3"
  [(set (match_operand:HI 0 "general_operand" "=rm")
	(rotatert:HI (match_operand:HI 1 "general_operand" "0")
		     (match_operand:HI 2 "general_operand" "rmi")))]
  ""
  " { operands[2] = negate_rtx (HImode, operands[2]); }")

(define_expand "rotrqi3"
  [(set (match_operand:QI 0 "general_operand" "=rm")
	(rotatert:QI (match_operand:QI 1 "general_operand" "0")
		     (match_operand:QI 2 "general_operand" "rmi")))]
  ""
  " { operands[2] = negate_rtx (QImode, operands[2]); }")

;; Special cases of bit-field insns which we should
;; recognize in preference to the general case.
;; These handle aligned 8-bit and 16-bit fields,
;; which can usually be done with move instructions.

;; Should I add  mode_dependent_address_p ????

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+rm")
			 (match_operand:SI 1 "immediate_operand" "i")
			 (match_operand:SI 2 "immediate_operand" "i"))
	(match_operand:SI 3 "general_operand" "rm"))]
  "TARGET_BITFIELD
   && GET_CODE (operands[1]) == CONST_INT
   && (INTVAL (operands[1]) == 8 || INTVAL (operands[1]) == 16)
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) % INTVAL (operands[1]) == 0
   && (GET_CODE (operands[0]) != REG
       || ( INTVAL (operands[1]) + INTVAL (operands[2]) == 32))"
  "*
{
  if (GET_CODE (operands[3]) == MEM)
    operands[3] = adj_offsettable_operand (operands[3],
					   (32 - INTVAL (operands[1])) / 8);

  if (GET_CODE (operands[0]) == REG)
    {
      if (INTVAL (operands[1]) == 8)
	return \"movu %3.b,%0.w\";
      return \"movu %3.h,%0.w\";
    }
  else
    {
      operands[0]
	= adj_offsettable_operand (operands[0], INTVAL (operands[2]) / 8);
      if (INTVAL (operands[1]) == 8)
	return \"mov.b %3,%0\";
      return \"mov.h %3,%0\";
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=&r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "rm")
			 (match_operand:SI 2 "immediate_operand" "i")
			 (match_operand:SI 3 "immediate_operand" "i")))]
  "TARGET_BITFIELD
   && GET_CODE (operands[2]) == CONST_INT
   && (INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && GET_CODE (operands[3]) == CONST_INT
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0"
  "*
{
  if (!REG_P (operands[1]))
    operands[1]
      = adj_offsettable_operand (operands[1], INTVAL (operands[3]) / 8);

  if (REG_P (operands[0]))
    {
      if (REG_P (operands[1]))
	{
	  if (INTVAL (operands[2]) == 8)
	    {			/* width == 8 */
	      switch (INTVAL (operands[3]))
		{
		case 0:
		  return \"mov.w %1,%0;shl.w #-24,%0\";
		  break;
		case 8:
		  return \"mov.w %1,%0;shl.w #8,%0;shl.w #-24,%0\";
		  break;
		case 16:
		  return \"mov.w %1,%0;shl.w #16,%0;shl.w #-24,%0\";
		  break;
		case 24:
		  return \"movu %1.b,%0.w\";
		  break;
		default:
		  myabort (2);
		}
	    }
	  else
	    {
	      switch (INTVAL (operands[3]))
		{
		case 0:
		  return \"mov.w %1,%0;shl.w #-16,%0\";
		  break;
		case 16:
		  return \"movu %1.h,%0.w\";
		  break;
		default:
		  myabort (3);
		}
	    }
	}
      else
	{
	  if (INTVAL (operands[2]) == 8)
	    return \"movu %1.h,%0.w\";
	  else
	    return \"movu %1.b,%0.w\";
	}
    }
  else
    {				/* op[0] == MEM */
      if (INTVAL (operands[2]) == 8)
	return \"movu %1.b,%0.w\";
      return \"movu %1.h,%0.w\";
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "ro")
			 (match_operand:SI 2 "immediate_operand" "i")
			 (match_operand:SI 3 "immediate_operand" "i")))]
  "TARGET_BITFIELD
   && GET_CODE (operands[2]) == CONST_INT
   && (INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && GET_CODE (operands[3]) == CONST_INT
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0"
  "*
{
  if (!REG_P (operands[1]))
    operands[1]
      = adj_offsettable_operand (operands[1], INTVAL (operands[3]) / 8);

  if (REG_P (operands[0]))
    {
      if (REG_P (operands[1]))
	{
	  if (INTVAL (operands[2]) == 8)
	    {			/* width == 8 */
	      switch (INTVAL (operands[3]))
		{
		case 0:
		  return \"mov.w %1,%0;sha.w #-24,%0\";
		  break;
		case 8:
		  return \"mov.w %1,%0;shl.w #8,%0;sha.w #-24,%0\";
		  break;
		case 16:
		  return \"mov.w %1,%0;shl.w #16,%0;sha.w #-24,%0\";
		  break;
		case 24:
		  return \"mov %1.b,%0.w\";
		  break;
		default:
		  myabort (4);
		}
	    }
	  else
	    {
	      switch (INTVAL (operands[3]))
		{
		case 0:
		  return \"mov.w %1,%0;sha.w #-16,%0\";
		  break;
		case 16:
		  return \"mov %1.h,%0.w\";
		  break;
		default:
		  myabort (5);
		}
	    }
	}
      else
	{
	  if (INTVAL (operands[2]) == 8)
	    return \"mov %1.h,%0.w\";
	  else
	    return \"mov %1.b,%0.w\";
	}
    }
  else
    {				/* op[0] == MEM */
      if (INTVAL (operands[2]) == 8)
	return \"mov %1.b,%0.w\";
      return \"mov %1.h,%0.w\";
    }
}")

;; Bit field instructions, general cases.
;; "o,d" constraint causes a nonoffsettable memref to match the "o"
;; so that its address is reloaded.

;; extv dest:SI src(:QI/:SI) width:SI pos:SI
;;        r.w    m            r.w/#    rmi  
;;        %0     %1           %2       %3

(define_expand "extv"
  [(set (match_operand:SI 0 "general_operand" "")
	(sign_extract:SI (match_operand:SI 1 "general_operand" "")
			 (match_operand:SI 2 "general_operand" "")
			 (match_operand:SI 3 "general_operand" "")))]
  "TARGET_BITFIELD"
  "")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(sign_extract:SI (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:SI 2 "general_operand" "ri")
			 (match_operand:SI 3 "general_operand" "rmi")))]
  "TARGET_BITFIELD"
  "bfext %3,%2,%1,%0")


(define_expand "extzv"
  [(set (match_operand:SI 0 "general_operand" "")
	(zero_extract:SI (match_operand:SI 1 "general_operand" "")
			 (match_operand:SI 2 "general_operand" "")
			 (match_operand:SI 3 "general_operand" "")))]
  "TARGET_BITFIELD"
  "")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(zero_extract:SI (match_operand:QI 1 "memory_operand" "m")
			 (match_operand:SI 2 "general_operand" "ri")
			 (match_operand:SI 3 "general_operand" "rmi")))]
  "TARGET_BITFIELD"
  "bfextu %3,%2,%1,%0")

;; There is no insn on the Gmicro to NOT/SET/CLR bitfield.


;; insv dest(BF):QI/SI  width:SI  pos:SI  src:SI
;;        m                r.w      rmi     r.w/i
;;        0                1        2       3


(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "general_operand" "")
			 (match_operand:SI 1 "general_operand" "")
			 (match_operand:SI 2 "general_operand" ""))
	(match_operand:SI 3 "general_operand" ""))]
  "TARGET_BITFIELD"
  "")

(define_insn ""
  [(set (zero_extract:SI (match_operand:QI 0 "memory_operand" "+m,m")
			 (match_operand:SI 1 "general_operand" "r,i")
			 (match_operand:SI 2 "general_operand" "rmi,i"))
	(match_operand:SI 3 "general_operand" "ri,ri"))]
  "TARGET_BITFIELD"
  "bfinsu %3,%2,%1,%0")

;;; bfins/bfinsu ????????

;; == == == == == == == == == == == == == 

;; Now recognize bit field insns that operate on registers
;; (or at least were intended to do so).

;; On the Gmicro/300,
;; bitfield instructions are not applicable to registers ;-<
;; But I write the register cases, because without them the gcc
;; seems to use "and" instruction with some other instructions
;; instead of using a shift instruction.
;; It is because on many processors shift instructions are slower.
;; On the Gmicro/300 which has a barrel shifter,
;; it is faster to use a shift instruction.
;;
;; Restricts width and offset to be immediates.
;;
(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "immediate_operand" "i")
			 (match_operand:SI 3 "immediate_operand" "i")))]
  "TARGET_BITFIELD"
  "*
{
  if (REGNO (operands[0]) != REGNO (operands[1]))
    output_asm_insn (\"mov.w %1,%0\", operands);
  if (INTVAL (operands[3]) != 0)
    output_asm_insn (\"shl.w %3,%0\", operands);
  operands[2] = GEN_INT (-(32 - INTVAL (operands[2])));
  return \"sha.w %3,%0\";
}")
    

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "immediate_operand" "i")
			 (match_operand:SI 3 "immediate_operand" "i")))]
  "TARGET_BITFIELD"
  "*
{
  if (REGNO (operands[0]) != REGNO (operands[1]))
    output_asm_insn (\"mov.w %1,%0\", operands);
  if (INTVAL (operands[3]) != 0)
    output_asm_insn (\"shl.w %3,%0\", operands);
  operands[2] = GEN_INT (-(32 - INTVAL (operands[2])));
  return \"shl.w %3,%0\";
}")


;; There are more descriptions for m68k, but not yet for the Gmicro.
;;

;; Basic conditional jump instructions.


(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"beq %b0\", \"fbeq %b0\", \"beq %b0\");
}")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"bne %b0\", \"fbne %b0\", \"bne %b0\");
}")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  OUTPUT_JUMP (\"bgt %b0\", \"fbgt %b0\", 0);
")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bgt %b0")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  OUTPUT_JUMP (\"blt %b0\", \"fblt %b0\", \"bms %b0\");
")

;; bms ?????
;; 

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "blt %b0")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
  OUTPUT_JUMP (\"bge %b0\", \"fbge %b0\", \"bmc %b0\");
")

;; bmc ??

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bge %b0")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "ble %b0")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "ble %b0")

;; Negated conditional jump instructions.

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  OUTPUT_JUMP (\"bne %b0\", \"fbne %b0\", \"bne %b0\");
}")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  OUTPUT_JUMP (\"beq %b0\", \"fbeq %b0\", \"beq %b0\");
}")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  OUTPUT_JUMP (\"ble %b0\", \"fbngt %b0\", 0);
")
;; fbngt ???

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "ble %b0")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  OUTPUT_JUMP (\"bge %b0\", \"fbnlt %b0\", \"jbmc %b0\");
")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "blt %b0")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  OUTPUT_JUMP (\"blt %b0\", \"fbnge %b0\", \"jbms %b0\");
")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "blt %b0")
;; ????

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
  OUTPUT_JUMP (\"bgt %b0\", \"fbnle %b0\", 0);
")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bgt %b0")

;; Unconditional and other jump instructions
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "bra %b0")

(define_insn "tablejump"
  [(set (pc)
	(plus:SI (pc) (match_operand:SI 0 "general_operand" "r")))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp @(pc:b,4:4,%0)")

;;
;; Should Add code for "ACB", "SCB". !!! ????
;; See m68k.h (dbra)
;;

;; Call subroutine with no return value.
(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "m")
	 (match_operand:SI 1 "general_operand" "rmi"))]
  ;; Operand 1 not really used on the Gmicro.

  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (XEXP (operands[0],0)) == SYMBOL_REF)
    return \"bsr %b0\";
  return \"jsr %0\";
}")

;; Call subroutine, returning value in operand 0
;; (which must be a hard register).
(define_insn "call_value"
  [(set (match_operand 0 "" "=rf")
	(call (match_operand:QI 1 "general_operand" "m")
	      (match_operand:SI 2 "general_operand" "rmi")))]
  ;; Operand 2 not really used on the Gmicro.
  ""
  "*
{
  if (GET_CODE (operands[1]) == MEM
      && GET_CODE (XEXP (operands[1],0)) == SYMBOL_REF)
    return \"bsr %b1\";
  return \"jsr %1\";
}")

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "")

(define_insn "nop"
  [(const_int 0)]
    ""
    "nop")

;; Turned off because the general move-an-address pattern handles it.
;; 
;; Thus goes after the move instructions
;; because the move instructions are better (require no spilling)
;; when they can apply. 
;; After add/sub now !!

;(define_insn "pushasi"
;  [(set (match_operand:SI 0 "push_operand" "=m")
;	(match_operand:SI 1 "address_operand" "p"))]
;  ""
;  "*
;{
;  if (GET_CODE (operands[1]) == CONST_INT)
;    return push_imm_word (INTVAL (operands[1]), operands[0]);
;  if (CONSTANT_P (operands[1]))
;    return \"mov.w %1,%-\";
;  if (GET_CODE (operands[1]) == REG)
;    return \"mov.w %1,%-\";
;  else if (GET_CODE (operands[1]) == MEM)
;    {
;      return \"mov.w %1,%-\";
;    }
;  else
;    return \"mova.w %p1,%-\";
;}")

;; This should not be used unless the add/sub insns can't be.

/* mova.[whq] 89.08.11 for test M.Yuhara */
;(define_insn ""
;  [(set (match_operand:SI 0 "general_operand" "=rm")
;	(address (match_operand:SI 1 "address_operand" "p")))]
;  ""
;  "*
;{
;    if (GET_CODE (operands[1]) == CONST_INT)
;        return mov_imm_word (INTVAL (operands[1]), operands[0]);
;    if (CONSTANT_P (operands[1]))
;        return \"mov.w %1,%0\";
;    if (GET_CODE (operands[1]) == REG)
;        return \"mov.w %1,%0\";
;    else  if (GET_CODE (operands[1]) == MEM) {
;	operands[1] = XEXP (operands[1],0);
;        return \"mov.w %1,%0\";
;    }
;    else
;        return \"mova.w %p1,%0\";
;}")


(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(address (match_operand:HI 1 "address_operand" "")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    return mov_imm_word (INTVAL (operands[1]), operands[0]);
  if (CONSTANT_P (operands[1]))
    return \"mov.w %1,%0\";
  if (GET_CODE (operands[1]) == REG)
    return \"mov.w %1,%0\";
  else  if (GET_CODE (operands[1]) == MEM)
    {
      operands[1] = XEXP (operands[1],0);
      return \"mov.w %1,%0\";	/* OK ? */
    }
  else
    return \"mova.w %p1,%0\";
}")

;(define_insn ""
;  [(set (match_operand:SI 0 "general_operand" "=rm")
;	(match_operand:QI 1 "address_operand" "p"))]
;  ""
;  "*
;{
;  if (push_operand (operands[0], SImode))
;    return \"mova %1,%-\";
;  return \"mova %1,%0\";
;}")

;(define_insn ""
;  [(set (match_operand:SI 0 "general_operand" "=rm")
;	(match_operand:QI 1 "address_operand" "p"))]
;  ""
;  "*
;{
;  if (CONSTANT_P (operands[1]))
;    return \"mov.w %1,%0\";
;  else if (GET_CODE (operands[1]) == REG)
;    return \"mov.w %1,%0\";
;  else if (GET_CODE (operands[1]) == MEM)
;    {
;      operands[1] = XEXP (operands[1],0);
;      return \"mov.w %1,%0 ; OK?\";
;    }
;  else if (GET_CODE (operands[0]) == REG
;	   && GET_CODE (operands[1]) == PLUS)
;    {
;      rtx xreg, xdisp;
;
;      if (GET_CODE (XEXP (operands[1], 0)) == REG 
;	  && REGNO (XEXP (operands[1], 0)) == REGNO (operands[0]))
;	{
;	  xreg = XEXP (operands[1], 0);
;	  xdisp = XEXP (operands[1],1);
;	}
;      else
;	{
;	  xreg = XEXP (operands[1], 1);
;	  xdisp = XEXP (operands[1],0);
;	}
;
;      if (GET_CODE (xreg) == REG
;	  && REGNO (xreg) == REGNO (operands[0])
;	  && (CONSTANT_P (xdisp) || GET_CODE (xdisp) == REG))
;	{
;	  operands[1] = xdisp;
;	  if (CONSTANT_P (xdisp))
;	    return add_imm_word (INTVAL (xdisp), xreg, &operands[1]);
;	  else
;	    return \"add.w %1,%0\";
;	}
;    }
;  return \"mova.w %p1,%0\";
;}")

;; This is the first machine-dependent peephole optimization.
;; It is useful when a floating value is returned from a function call
;; and then is moved into an FP register.
;; But it is mainly intended to test the support for these optimizations.

(define_peephole
  [(set (reg:SI 15) (plus:SI (reg:SI 15) (const_int 4)))
   (set (match_operand:DF 0 "register_operand" "=f")
	(match_operand:DF 1 "register_operand" "r"))]
  "FPU_REG_P (operands[0]) && ! FPU_REG_P (operands[1])"
  "*
{
  rtx xoperands[2];
  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  output_asm_insn (\"mov.w %1,@sp\", xoperands);
  output_asm_insn (\"mov.w %1,%-\", operands);
  return \"fmov.d %+,%0\";
}
")
