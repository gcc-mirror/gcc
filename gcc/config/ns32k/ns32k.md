;;- Machine description for GNU compiler, ns32000 Version
;;  Copyright (C) 1988, 1994, 1996, 1998-99, 2000 Free Software Foundation, Inc.
;;  Contributed by Michael Tiemann (tiemann@cygnus.com)

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


; BUGS:
;; Insert no-op between an insn with memory read-write operands
;;   following by a scale-indexing operation.
;; The Sequent assembler does not allow addresses to be used
;;   except in insns which explicitly compute an effective address.
;;   I.e., one cannot say "cmpd _p,@_x"
;; Implement unsigned multiplication??

;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;; We don't want to allow a constant operand for test insns because
;; (set (cc0) (const_int foo)) has no mode information.  Such insns will
;; be folded while optimizing anyway.
;;
;; In order for pic mode to work we cannot generate, for example
;;
;;   addd _x+5,r1
;;
;; instead we must force gcc to generate something like
;;
;;   addr 5(_x(sb)),r0
;;   addd r0,r1
;;
;; This was done through operand constraints (using "rmn" in place of "g"),
;; but with the proper definition of LEGITIMATE_PIC_OPERAND (ns32k.h)
;; this is unnecessary.
;;
(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "nonimmediate_operand" "rm"))]
  ""
  "*
{ cc_status.flags |= CC_REVERSED;
  operands[1] = const0_rtx;
  return \"cmpqd %1,%0\"; }")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "nonimmediate_operand" "g"))]
  ""
  "*
{ cc_status.flags |= CC_REVERSED;
  operands[1] = const0_rtx;
  return \"cmpqw %1,%0\"; }")

(define_insn "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "nonimmediate_operand" "g"))]
  ""
  "*
{ cc_status.flags |= CC_REVERSED;
  operands[1] = const0_rtx;
  return \"cmpqb %1,%0\"; }")

(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "general_operand" "lmF"))]
  "TARGET_32081"
  "*
{ cc_status.flags |= CC_REVERSED;
  operands[1] = CONST0_RTX (DFmode);
  return \"cmpl %1,%0\"; }")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "general_operand" "fmF"))]
  "TARGET_32081"
  "*
{ cc_status.flags |= CC_REVERSED;
  operands[1] = CONST0_RTX (SFmode);
  return \"cmpf %1,%0\"; }")

;; See note 1
(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "general_operand" "g")
		 (match_operand:SI 1 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = INTVAL (operands[1]);
      if (i <= 7 && i >= -8)
	{
	  cc_status.flags |= CC_REVERSED;
	  return \"cmpqd %1,%0\";
	}
    }
  cc_status.flags &= ~CC_REVERSED;
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      int i = INTVAL (operands[0]);
      if (i <= 7 && i >= -8)
	return \"cmpqd %0,%1\";
    }
  return \"cmpd %0,%1\";
}")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "general_operand" "g")
		 (match_operand:HI 1 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      short i = INTVAL (operands[1]);
    if (i <= 7 && i >= -8)
      {
	cc_status.flags |= CC_REVERSED;
	if (INTVAL (operands[1]) > 7)
	  operands[1] = GEN_INT (i);
	return \"cmpqw %1,%0\";
      }
    }
  cc_status.flags &= ~CC_REVERSED;
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      short i = INTVAL (operands[0]);
      if (i <= 7 && i >= -8)
	{
	  if (INTVAL (operands[0]) > 7)
	    operands[0] = GEN_INT (i);
	  return \"cmpqw %0,%1\";
	}
    }
  return \"cmpw %0,%1\";
}")

(define_insn "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "general_operand" "g")
		 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      char i = INTVAL (operands[1]);
      if (i <= 7 && i >= -8)
	{
	  cc_status.flags |= CC_REVERSED;
	  if (INTVAL (operands[1]) > 7)
	    operands[1] = GEN_INT (i);
	  return \"cmpqb %1,%0\";
	}
    }
  cc_status.flags &= ~CC_REVERSED;
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      char i = INTVAL (operands[0]);
      if (i <= 7 && i >= -8)
	{
	  if (INTVAL (operands[0]) > 7)
	    operands[0] = GEN_INT (i);
	  return \"cmpqb %0,%1\";
	}
    }
  return \"cmpb %0,%1\";
}")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "general_operand" "lmF")
		 (match_operand:DF 1 "general_operand" "lmF")))]
  "TARGET_32081"
  "cmpl %0,%1")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "general_operand" "fmF")
		 (match_operand:SF 1 "general_operand" "fmF")))]
  "TARGET_32081"
  "cmpf %0,%1")

;; movdf and movsf copy between general and floating registers using
;; the stack. In principle, we could get better code not allowing
;; that case in the constraints and defining SECONDARY_MEMORY_NEEDED
;; in practice, though the stack slots used are not available for
;; optimization.
(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=lg<")
	(match_operand:DF 1 "general_operand" "lFg"))]
  ""
  "*
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]) || GET_CODE (operands[1]) == CONST_DOUBLE)
	return \"movl %1,%0\";
      if (REG_P (operands[1]))
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
	  output_asm_insn (\"movd %1,tos\", xoperands);
	  output_asm_insn (\"movd %1,tos\", operands);
	  return \"movl tos,%0\";
	}
      return \"movl %1,%0\";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	{
	  output_asm_insn (\"movl %1,tos\;movd tos,%0\", operands);
	  operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
	  return \"movd tos,%0\";
	}
      else
        return \"movl %1,%0\";
    }
  return output_move_double (operands);
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=fg<")
	(match_operand:SF 1 "general_operand" "fFg"))]
  ""
  "*
{
  if (FP_REG_P (operands[0]))
    {
      if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) < F0_REGNUM)
	return \"movd %1,tos\;movf tos,%0\";
      else
	return \"movf %1,%0\";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	return \"movf %1,tos\;movd tos,%0\";
      return \"movf %1,%0\";
    }
#if 0 /* Someone suggested this for the Sequent.  Is it needed?  */
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    return \"movf %1,%0\";
#endif
/* There was a #if 0 around this, but that was erroneous
   for many machines -- rms.  */
#ifndef MOVD_FLOAT_OK
  /* GAS understands floating constants in ordinary movd instructions
     but other assemblers might object.  */
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      union {int i[2]; float f; double d;} convrt;
      convrt.i[0] = CONST_DOUBLE_LOW (operands[1]);
      convrt.i[1] = CONST_DOUBLE_HIGH (operands[1]);
      convrt.f = convrt.d;

      /* Is there a better machine-independent way to to this?  */
      operands[1] = GEN_INT (convrt.i[0]);
      return \"movd %1,%0\";
    }
#endif
  else return \"movd %1,%0\";
}")

(define_insn ""
  [(set (match_operand:TI 0 "memory_operand" "=m")
	(match_operand:TI 1 "memory_operand" "m"))]
  ""
  "movmd %1,%0,4")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g<,*f,g")
	(match_operand:DI 1 "general_operand" "gF,g,*f"))]
  ""
  "*
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]) || GET_CODE (operands[1]) == CONST_DOUBLE)
	return \"movl %1,%0\";
      if (REG_P (operands[1]))
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
	  output_asm_insn (\"movd %1,tos\", xoperands);
	  output_asm_insn (\"movd %1,tos\", operands);
	  return \"movl tos,%0\";
	}
      return \"movl %1,%0\";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	{
	  output_asm_insn (\"movl %1,tos\;movd tos,%0\", operands);
	  operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
	  return \"movd tos,%0\";
	}
      else
        return \"movl %1,%0\";
    }
  return output_move_double (operands);
}")

;; This special case must precede movsi.
(define_insn ""
  [(set (reg:SI 25)
	(match_operand:SI 0 "general_operand" "g"))]
  ""
  "lprd sp,%0")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=g<,g<,*f,g,x")
	(match_operand:SI 1 "general_operand" "g,?xy,g,*f,rmn"))]
  ""
  "*
{
  extern int flag_pic;						\

  if (FP_REG_P (operands[0]))
    {
      if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) < F0_REGNUM)
	return \"movd %1,tos\;movf tos,%0\";
      else
	return \"movf %1,%0\";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	return \"movf %1,tos\;movd tos,%0\";
      return \"movf %1,%0\";
    }
  if (GET_CODE (operands[0]) == REG
      && REGNO (operands[0]) == FRAME_POINTER_REGNUM)
    return \"lprd fp,%1\";
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = INTVAL (operands[1]);
      if (! TARGET_32532)
	{
	  if (i <= 7 && i >= -8)
	    return \"movqd %1,%0\";
	  if (NS32K_DISPLACEMENT_P (i))
#if defined (GNX_V3) || defined (UTEK_ASM)
	    return \"addr %c1,%0\";
#else
	    return \"addr @%c1,%0\";
#endif
	  return \"movd %1,%0\";
	}
      else
        return output_move_dconst(i, \"%1,%0\");
    }
  else if (GET_CODE (operands[1]) == CONST && ! flag_pic)
    {
	/* Must contain symbols so we don`t know how big it is. In
	 * that case addr might lead to overflow. For PIC symbolic
	 * address loads always have to be done with addr.
	 */
	return \"movd %1,%0\";
    }
  else if (GET_CODE (operands[1]) == REG)
    {
      if (REGNO (operands[1]) < F0_REGNUM)
        return \"movd %1,%0\";
      else if (REGNO (operands[1]) == FRAME_POINTER_REGNUM)
	{
	  if (GET_CODE(operands[0]) == REG)
	    return \"sprd fp,%0\";
	  else
	    return \"addr 0(fp),%0\" ;
	}
      else if (REGNO (operands[1]) == STACK_POINTER_REGNUM)
	{
	  if (GET_CODE(operands[0]) == REG)
	    return \"sprd sp,%0\";
	  else
	    return \"addr 0(sp),%0\" ;
	}
      else abort ();
    }
  else if (GET_CODE (operands[1]) == MEM)
    return \"movd %1,%0\";

  /* Check if this effective address can be
     calculated faster by pulling it apart.  */
  if (REG_P (operands[0])
      && GET_CODE (operands[1]) == MULT
      && GET_CODE (XEXP (operands[1], 1)) == CONST_INT
      && (INTVAL (XEXP (operands[1], 1)) == 2
	  || INTVAL (XEXP (operands[1], 1)) == 4))
    {
      rtx xoperands[3];
      xoperands[0] = operands[0];
      xoperands[1] = XEXP (operands[1], 0);
      xoperands[2] = GEN_INT (INTVAL (XEXP (operands[1], 1)) >> 1);
      return output_shift_insn (xoperands);
    }
  return \"addr %a1,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=g<,*f,g")
	(match_operand:HI 1 "general_operand" "g,g,*f"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      short i = INTVAL (operands[1]);
      if (i <= 7 && i >= -8)
	{
	  if (INTVAL (operands[1]) > 7)
	    operands[1] = GEN_INT (i);
	  return \"movqw %1,%0\";
	}
	return \"movw %1,%0\";
    }
  else if (FP_REG_P (operands[0]))
    {
      if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) < F0_REGNUM)
	return \"movwf %1,tos\;movf tos,%0\";
      else
	return \"movwf %1,%0\";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	return \"movf %1,tos\;movd tos,%0\";
      return \"movf %1,%0\";
    }
  else
     return \"movw %1,%0\";
}")

(define_insn "movstricthi"
  [(set (strict_low_part (match_operand:HI 0 "general_operand" "+r"))
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL(operands[1]) <= 7 && INTVAL(operands[1]) >= -8)
    return \"movqw %1,%0\";
  return \"movw %1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=g<,*f,g")
	(match_operand:QI 1 "general_operand" "g,g,*f"))]
  ""
  "*
{ if (GET_CODE (operands[1]) == CONST_INT)
    {
      char char_val = (char)INTVAL (operands[1]);
      if (char_val <= 7 && char_val >= -8)
	{
	  if (INTVAL (operands[1]) > 7)
	    operands[1] = GEN_INT (char_val);
	  return \"movqb %1,%0\";
	}
	return \"movb %1,%0\";
    }
  else if (FP_REG_P (operands[0]))
    {
      if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) < F0_REGNUM)
	return \"movbf %1,tos\;movf tos,%0\";
      else
	return \"movbf %1,%0\";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (REG_P (operands[0]))
	return \"movf %1,tos\;movd tos,%0\";
      return \"movf %1,%0\";
    }
  else
     return \"movb %1,%0\";
}")

(define_insn "movstrictqi"
  [(set (strict_low_part (match_operand:QI 0 "general_operand" "+r"))
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL(operands[1]) < 8 && INTVAL(operands[1]) > -9)
    return \"movqb %1,%0\";
  return \"movb %1,%0\";
}")

;; Block moves
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment
;;
;; Strategy: Use define_expand to
;; either emit insns directly if it can be done simply or
;; emit rtl to match movstrsi1 which has extra scratch registers
;; which can be used to generate more complex code.

(define_expand "movstrsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand" "")
		   (match_operand:BLK 1 "general_operand" ""))
	      (use (match_operand:SI 2 "general_operand" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))])]
  ""
  "
{
  if (operands[0])		/* avoid unused code messages */
    {
      expand_block_move (operands);
      DONE;
    }
}")

;; Special Registers:
;; r0  count
;; r1  from 
;; r2  to   
;; r3  match


(define_insn "movstrsi1"
  [(set (mem:BLK (reg:SI 2))
	(mem:BLK (reg:SI 1)))
   (use (reg:SI 0))
   (set (reg:SI 2) (plus:SI (reg:SI 2) (mult:SI (reg:SI 0) (match_operand:SI 0 "const_int_operand" ""))))
   (set (reg:SI 1) (plus:SI (reg:SI 1) (mult:SI (reg:SI 0) (match_dup 0))))
   (set (reg:SI 0) (const_int 0))]
  ""
  "*
  {
     int align = INTVAL(operands[0]);
     if (align == 4)
       return \"movsd\";
     else
       return \"movsb\";
  }")

(define_insn "movstrsi2"
  [(set (mem:BLK (match_operand:SI 0 "address_operand" "g"))
	(mem:BLK (match_operand:SI 1 "address_operand" "g")))
   (use (match_operand 2 "immediate_operand" "i"))]
  ""
  "movmd %a1,%a0,%2")


;; Extension and truncation insns.
;; Those for integer source operand
;; are ordered widest source type first.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(truncate:QI (match_operand:SI 1 "nonimmediate_operand" "g")))]
  ""
  "movb %1,%0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(truncate:HI (match_operand:SI 1 "nonimmediate_operand" "g")))]
  ""
  "movw %1,%0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(truncate:QI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "movb %1,%0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "movxwd %1,%0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "movxbw %1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "movxbd %1,%0")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "general_operand" "=lm<")
	(float_extend:DF (match_operand:SF 1 "general_operand" "fmF")))]
  "TARGET_32081"
  "movfl %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "general_operand" "=fm<")
	(float_truncate:SF (match_operand:DF 1 "general_operand" "lmF")))]
  "TARGET_32081"
  "movlf %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "g")))]
  ""
  "movzwd %1,%0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "movzbw %1,%0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "movzbd %1,%0")

;; Fix-to-float conversion insns.
;; Note that the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

;; Rumor has it that the National part does not correctly convert
;; constant ints to floats.  This conversion is therefore disabled.
;; A register must be used to perform the conversion.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=fm<")
	(float:SF (match_operand:SI 1 "general_operand" "rm")))]
  "TARGET_32081"
  "movdf %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=lm<")
	(float:DF (match_operand:SI 1 "general_operand" "rm")))]
  "TARGET_32081"
  "movdl %1,%0")

(define_insn "floathisf2"
  [(set (match_operand:SF 0 "general_operand" "=fm<")
	(float:SF (match_operand:HI 1 "general_operand" "rm")))]
  "TARGET_32081"
  "movwf %1,%0")

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "general_operand" "=lm<")
	(float:DF (match_operand:HI 1 "general_operand" "rm")))]
  "TARGET_32081"
  "movwl %1,%0")

(define_insn "floatqisf2"
  [(set (match_operand:SF 0 "general_operand" "=fm<")
	(float:SF (match_operand:QI 1 "general_operand" "rm")))]
  "TARGET_32081"
  "movbf %1,%0")

; Some assemblers warn that this insn doesn't work.
; Maybe they know something we don't.
;(define_insn "floatqidf2"
;  [(set (match_operand:DF 0 "general_operand" "=lm<")
;	(float:DF (match_operand:QI 1 "general_operand" "rm")))]
;  "TARGET_32081"
;  "movbl %1,%0")

;; Float-to-fix conversion insns.
;; The sequent compiler always generates "trunc" insns.

(define_insn "fixsfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(fix:QI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  "TARGET_32081"
  "truncfb %1,%0")

(define_insn "fixsfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(fix:HI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  "TARGET_32081"
  "truncfw %1,%0")

(define_insn "fixsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(fix:SI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  "TARGET_32081"
  "truncfd %1,%0")

(define_insn "fixdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(fix:QI (fix:DF (match_operand:DF 1 "general_operand" "lm"))))]
  "TARGET_32081"
  "trunclb %1,%0")

(define_insn "fixdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(fix:HI (fix:DF (match_operand:DF 1 "general_operand" "lm"))))]
  "TARGET_32081"
  "trunclw %1,%0")

(define_insn "fixdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(fix:SI (fix:DF (match_operand:DF 1 "general_operand" "lm"))))]
  "TARGET_32081"
  "truncld %1,%0")

;; Unsigned

(define_insn "fixunssfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(unsigned_fix:QI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  "TARGET_32081"
  "truncfb %1,%0")

(define_insn "fixunssfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(unsigned_fix:HI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  "TARGET_32081"
  "truncfw %1,%0")

(define_insn "fixunssfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(unsigned_fix:SI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  "TARGET_32081"
  "truncfd %1,%0")

(define_insn "fixunsdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(unsigned_fix:QI (fix:DF (match_operand:DF 1 "general_operand" "lm"))))]
  "TARGET_32081"
  "trunclb %1,%0")

(define_insn "fixunsdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(unsigned_fix:HI (fix:DF (match_operand:DF 1 "general_operand" "lm"))))]
  "TARGET_32081"
  "trunclw %1,%0")

(define_insn "fixunsdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(unsigned_fix:SI (fix:DF (match_operand:DF 1 "general_operand" "lm"))))]
  "TARGET_32081"
  "truncld %1,%0")

;;; These are not yet used by GCC
(define_insn "fix_truncsfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(fix:QI (match_operand:SF 1 "general_operand" "fm")))]
  "TARGET_32081"
  "truncfb %1,%0")

(define_insn "fix_truncsfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(fix:HI (match_operand:SF 1 "general_operand" "fm")))]
  "TARGET_32081"
  "truncfw %1,%0")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(fix:SI (match_operand:SF 1 "general_operand" "fm")))]
  "TARGET_32081"
  "truncfd %1,%0")

(define_insn "fix_truncdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(fix:QI (match_operand:DF 1 "general_operand" "lm")))]
  "TARGET_32081"
  "trunclb %1,%0")

(define_insn "fix_truncdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(fix:HI (match_operand:DF 1 "general_operand" "lm")))]
  "TARGET_32081"
  "trunclw %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(fix:SI (match_operand:DF 1 "general_operand" "lm")))]
  "TARGET_32081"
  "truncld %1,%0")

;; Multiply-add instructions
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=v,v")
	(plus:DF (mult:DF (match_operand:DF 1 "general_operand" "%lmF,0")
		          (match_operand:DF 2 "general_operand" "lmF,lmF"))
                 (match_operand:DF 3 "general_operand" "0,lmF")))]
  "TARGET_MULT_ADD"
  "@
   dotl %1,%2
   polyl %2,%3")

(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=u,u")
	(plus:SF (mult:SF (match_operand:SF 1 "general_operand" "%fmF,0")
		          (match_operand:SF 2 "general_operand" "fmF,fmF"))
                 (match_operand:SF 3 "general_operand" "0,fmF")))]
  "TARGET_MULT_ADD"
  "@
   dotf %1,%2
   polyf %2,%3")


;; Multiply-sub instructions
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=v")
	(minus:DF (mult:DF (match_operand:DF 1 "general_operand" "%lmF")
		          (match_operand:DF 2 "general_operand" "lmF"))
                 (match_operand:DF 3 "general_operand" "0")))]
  "TARGET_MULT_ADD"
  "@
   negl %0,%0\;dotl %1,%2")

(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=u")
	(minus:SF (mult:SF (match_operand:SF 1 "general_operand" "%fmF")
		          (match_operand:SF 2 "general_operand" "fmF"))
                 (match_operand:SF 3 "general_operand" "0")))]
  "TARGET_MULT_ADD"
  "@
   negf %0,%0\;dotf %1,%2")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "general_operand" "=lm")
	(plus:DF (match_operand:DF 1 "general_operand" "%0")
		 (match_operand:DF 2 "general_operand" "lmF")))]
  "TARGET_32081"
  "addl %2,%0")


(define_insn "addsf3"
  [(set (match_operand:SF 0 "general_operand" "=fm")
	(plus:SF (match_operand:SF 1 "general_operand" "%0")
		 (match_operand:SF 2 "general_operand" "fmF")))]
  "TARGET_32081"
  "addf %2,%0")

(define_insn ""
  [(set (reg:SI 25)
	(plus:SI (reg:SI 25)
		 (match_operand:SI 0 "immediate_operand" "i")))]
  "GET_CODE (operands[0]) == CONST_INT"
  "*
{
#ifndef SEQUENT_ADJUST_STACK
  if (TARGET_32532)
    if (INTVAL (operands[0]) == 8)
      return \"cmpd tos,tos\";
  if (TARGET_32532 || TARGET_32332)
    if (INTVAL (operands[0]) == 4)
      return \"cmpqd %$0,tos\";
#endif
  if (! TARGET_32532)
    {
      if (INTVAL (operands[0]) < 64 && INTVAL (operands[0]) > -64)
        return \"adjspb %n0\";
      else if (INTVAL (operands[0]) < 8192 && INTVAL (operands[0]) >= -8192)
        return \"adjspw %n0\";
    }
  return \"adjspd %n0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(plus:SI (reg:SI 24)
		 (match_operand:SI 1 "immediate_operand" "i")))]
  "GET_CODE (operands[1]) == CONST_INT"
  "addr %c1(fp),%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(plus:SI (reg:SI 25)
		 (match_operand:SI 1 "immediate_operand" "i")))]
  "GET_CODE (operands[1]) == CONST_INT"
  "addr %c1(sp),%0")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "general_operand" "=ro")
	(plus:DI (match_operand:DI 1 "general_operand" "%0")
		 (match_operand:DI 2 "general_operand" "ron")))]
  ""
  "*
{
  rtx low[3], high[3], xops[4];
  split_di (operands, 3, low, high);
  xops[0] = low[0];
  xops[1] = high[0];
  xops[2] = low[2];
  xops[3] = high[2];

  if (GET_CODE (xops[2]) == CONST_INT)
    {
      int i = INTVAL (xops[2]);

      if (i <= 7 && i >= -8) 
        {
          if (i == 0)
	    {
	      i = INTVAL (xops[3]);
	      if (i <= 7 && i >= -8)
                output_asm_insn (\"addqd %3,%1\", xops);
	      else
                output_asm_insn (\"addd %3,%1\", xops);
	    }
	  else
	    {
              output_asm_insn (\"addqd %2,%0\", xops);
              output_asm_insn (\"addcd %3,%1\", xops);
	    }
	  return \"\";
	}
    }
  output_asm_insn (\"addd %2,%0\", xops);
  output_asm_insn (\"addcd %3,%1\", xops);
  return \"\";
}")

;; See Note 1
(define_insn "addsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,=g&<")
	(plus:SI (match_operand:SI 1 "general_operand" "%0,r")
		 (match_operand:SI 2 "general_operand" "g,i")))]
  ""
  "*
{
  if (which_alternative == 1)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        {
	  int i = INTVAL (operands[2]);
	  if (NS32K_DISPLACEMENT_P (i))
	    return \"addr %c2(%1),%0\";
	  else
	    return \"movd %1,%0\;addd %2,%0\";
        }
      else
        {
          if (flag_pic) 
            return \"addr %a2[%1:b],%0\";
	  else
	    return \"addr %c2(%1),%0\";
        }
    }
  else if (GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);

      if (i <= 7 && i >= -8)
	return \"addqd %2,%0\";
      else if (! TARGET_32532 && GET_CODE (operands[0]) == REG
	       && NS32K_DISPLACEMENT_P (i))
	return \"addr %c2(%0),%0\";
    }
  return \"addd %2,%0\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(plus:HI (match_operand:HI 1 "general_operand" "%0")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);
      if (i <= 7 && i >= -8)
	return \"addqw %2,%0\";
    }
  return \"addw %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:HI 0 "general_operand" "=r"))
	(plus:HI (match_operand:HI 1 "general_operand" "0")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) >-9 && INTVAL(operands[1]) < 8)
    return \"addqw %2,%0\";
  return \"addw %2,%0\";
}")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(plus:QI (match_operand:QI 1 "general_operand" "%0")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);
      if (i <= 7 && i >= -8)
	return \"addqb %2,%0\";
    }
  return \"addb %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:QI 0 "general_operand" "=r"))
	(plus:QI (match_operand:QI 1 "general_operand" "0")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) >-9 && INTVAL(operands[1]) < 8)
    return \"addqb %2,%0\";
  return \"addb %2,%0\";
}")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "general_operand" "=lm")
	(minus:DF (match_operand:DF 1 "general_operand" "0")
		  (match_operand:DF 2 "general_operand" "lmF")))]
  "TARGET_32081"
  "subl %2,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "general_operand" "=fm")
	(minus:SF (match_operand:SF 1 "general_operand" "0")
		  (match_operand:SF 2 "general_operand" "fmF")))]
  "TARGET_32081"
  "subf %2,%0")

(define_insn ""
  [(set (reg:SI 25)
	(minus:SI (reg:SI 25)
		  (match_operand:SI 0 "immediate_operand" "i")))]
  "GET_CODE (operands[0]) == CONST_INT"
  "*
{
  if (! TARGET_32532 && GET_CODE(operands[0]) == CONST_INT 
      && INTVAL(operands[0]) < 64 && INTVAL(operands[0]) > -64)
    return \"adjspb %0\";
  return \"adjspd %0\";
}")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "general_operand" "=ro")
	(minus:DI (match_operand:DI 1 "general_operand" "0")
		  (match_operand:DI 2 "general_operand" "ron")))]
  ""
  "*
{
  rtx low[3], high[3], xops[4];
  split_di (operands, 3, low, high);
  xops[0] = low[0];
  xops[1] = high[0];
  xops[2] = low[2];
  xops[3] = high[2];

  if (GET_CODE (xops[2]) == CONST_INT)
    {
      int i = INTVAL (xops[2]);

      if (i <= 8 && i >= -7)
        {
          if (i == 0)
	    {
	      i = INTVAL (xops[3]);
	      if (i <= 8 && i >= -7)
                output_asm_insn (\"addqd %n3,%1\", xops);
	      else
                output_asm_insn (\"subd %3,%1\", xops);
	    }
	  else
	    {
              output_asm_insn (\"addqd %n2,%0\", xops);
              output_asm_insn (\"subcd %3,%1\", xops);
	    }
	  return \"\";
	}
    }
  output_asm_insn (\"subd %2,%0\", xops);
  output_asm_insn (\"subcd %3,%1\", xops);
  return \"\";
}")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(minus:SI (match_operand:SI 1 "general_operand" "0")
		  (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);

      if (i <= 8 && i >= -7)
        return \"addqd %n2,%0\";
    }
  return \"subd %2,%0\";
}")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(minus:HI (match_operand:HI 1 "general_operand" "0")
		  (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);

      if (i <= 8 && i >= -7)
        return \"addqw %n2,%0\";
    }
  return \"subw %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:HI 0 "general_operand" "=r"))
	(minus:HI (match_operand:HI 1 "general_operand" "0")
		  (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) >-8 && INTVAL(operands[1]) < 9)
    return \"addqw %n2,%0\";
  return \"subw %2,%0\";
}")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(minus:QI (match_operand:QI 1 "general_operand" "0")
		  (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);

      if (i <= 8 && i >= -7)
	return \"addqb %n2,%0\";
    }
  return \"subb %2,%0\";
}")

(define_insn ""
  [(set (strict_low_part (match_operand:QI 0 "general_operand" "=r"))
	(minus:QI (match_operand:QI 1 "general_operand" "0")
		  (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) >-8 && INTVAL(operands[1]) < 9)
    return \"addqb %n2,%0\";
  return \"subb %2,%0\";
}")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "general_operand" "=lm")
	(mult:DF (match_operand:DF 1 "general_operand" "%0")
		 (match_operand:DF 2 "general_operand" "lmF")))]
  "TARGET_32081"
  "mull %2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "general_operand" "=fm")
	(mult:SF (match_operand:SF 1 "general_operand" "%0")
		 (match_operand:SF 2 "general_operand" "fmF")))]
  "TARGET_32081"
  "mulf %2,%0")

;; See note 1
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(mult:SI (match_operand:SI 1 "general_operand" "%0")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "muld %2,%0")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(mult:HI (match_operand:HI 1 "general_operand" "%0")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "mulw %2,%0")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(mult:QI (match_operand:QI 1 "general_operand" "%0")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "mulb %2,%0")

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(mult:DI (zero_extend:DI
		  (match_operand:SI 1 "nonimmediate_operand" "0"))
		 (zero_extend:DI
		  (match_operand:SI 2 "nonimmediate_operand" "g"))))]
  ""
  "meid %2,%0")

;; divmod insns: We can only do the unsigned case.
(define_expand "udivmodsi4"
  [(parallel
  [(set (match_operand:SI 0 "reg_or_mem_operand" "")
	(udiv:SI (match_operand:SI 1 "general_operand" "")
		     (match_operand:SI 2 "general_operand" "")))
   (set (match_operand:SI 3 "reg_or_mem_operand" "")
	(umod:SI (match_dup 1) (match_dup 2)))])]
  ""
  "
{
  rtx temp = gen_reg_rtx(DImode);
  rtx insn, first, last;
  first = emit_move_insn(gen_lowpart(SImode, temp), operands[1]);
  emit_move_insn(gen_highpart(SImode, temp), const0_rtx);
  emit_insn(gen_udivmoddisi4_internal(temp, temp, operands[2]));
  last = emit_move_insn(temp, temp);
  {
    rtx divdi, moddi, divsi, modsi;
    divsi = gen_rtx (UDIV, SImode, operands[1], operands[2]);
    modsi = gen_rtx (UMOD, SImode, operands[1], operands[2]);
    divdi = gen_rtx (ZERO_EXTEND, DImode, divsi);
    moddi = gen_rtx (ZERO_EXTEND, DImode, modsi);
    REG_NOTES (first) = gen_rtx (INSN_LIST, REG_LIBCALL, last,
			         REG_NOTES (first));
    REG_NOTES (last) = gen_rtx (INSN_LIST, REG_RETVAL, first,
                                gen_rtx (EXPR_LIST, REG_EQUAL,
                       gen_rtx (IOR, DImode, moddi,
                               gen_rtx (ASHIFT, DImode, divdi, GEN_INT(32))),
                       REG_NOTES (last)));
  }

  insn = emit_move_insn(operands[0], gen_highpart(SImode, temp));
  insn = emit_move_insn(operands[3], gen_lowpart(SImode, temp));
  DONE;
}")

;; If we try and describe what this does, we have to zero-expand an
;; operand, which prevents it being a constant (VOIDmode) (see udivmoddisi4
;; below. This udivmoddisi4_internal never matches anything and is only
;; ever used when explicitly emitted by a define_expand.
(define_insn "udivmoddisi4_internal"
  [(set (match_operand:DI 0 "reg_or_mem_operand" "=rm")
        (unspec:DI [(match_operand:DI 1 "reg_or_mem_operand" "0")
                    (match_operand:SI 2 "general_operand" "g")] 0))]
  ""
  "deid %2,%0")

;; Retain this insn which *does* have a pattern indicating what it does,
;; just in case the compiler is smart enough to recognize a substitution.
(define_insn "udivmoddisi4"
  [(set (subreg:SI (match_operand:DI 0 "register_operand" "=rm") 1)
	(truncate:SI (udiv:DI (match_operand:DI 1 "reg_or_mem_operand" "0")
		 (zero_extend:DI (match_operand:SI 2 "nonimmediate_operand" "g")))))
   (set (subreg:SI (match_operand:DI 3 "register_operand" "=0") 0)
	(truncate:SI (umod:DI (match_dup 1) (zero_extend:DI (match_dup 2)))))]
  ""
  "deid %2,%0")

;; Part word variants. These seem to never be used at the moment (gcc
;; 2.7.2.2). The code generation prefers to zero extend hi's and qi's
;; and use signed div and mod. Keep these insns incase that changes.
;; divmod should have an advantage when both div and mod are needed. However,
;; divmod uses two registers, so maybe the compiler knows best.

(define_expand "udivmodhi4"
  [(parallel
  [(set (match_operand:HI 0 "reg_or_mem_operand" "")
	(udiv:HI (match_operand:HI 1 "general_operand" "")
		     (match_operand:HI 2 "general_operand" "")))
   (set (match_operand:HI 3 "reg_or_mem_operand" "")
	(umod:HI (match_dup 1) (match_dup 2)))])]
  ""
  "
{
  rtx temp = gen_reg_rtx(DImode);
  rtx insn, first, last;
  first = emit_move_insn(gen_lowpart(HImode, temp), operands[1]);
  emit_move_insn(gen_highpart (HImode, temp), const0_rtx);
  operands[2] = force_reg(HImode, operands[2]);
  emit_insn(gen_udivmoddihi4_internal(temp, temp, operands[2]));
  last = emit_move_insn(temp, temp);
  {
    rtx divdi, moddi, divhi, modhi;
    divhi = gen_rtx (UDIV, HImode, operands[1], operands[2]);
    modhi = gen_rtx (UMOD, HImode, operands[1], operands[2]);
    divdi = gen_rtx (ZERO_EXTEND, DImode, divhi);
    moddi = gen_rtx (ZERO_EXTEND, DImode, modhi);
    REG_NOTES (first) = gen_rtx (INSN_LIST, REG_LIBCALL, last,
			         REG_NOTES (first));
    REG_NOTES (last) = gen_rtx (INSN_LIST, REG_RETVAL, first,
                                gen_rtx (EXPR_LIST, REG_EQUAL,
                       gen_rtx(IOR, DImode, moddi,
                               gen_rtx(ASHIFT, DImode, divdi, GEN_INT(32))),
                       REG_NOTES (last)));
  }

  insn = emit_move_insn(operands[0], gen_highpart(HImode, temp));
  insn = emit_move_insn(operands[3], gen_lowpart(HImode, temp));
  DONE;
}")

;; deiw wants two hi's in separate registers or else they can be adjacent
;; in memory. DI mode will ensure two registers are available, but if we
;; want to allow memory as an operand we would need SI mode. There is no
;; way to do this, so just restrict operand 0 and 1 to be in registers.
(define_insn "udivmoddihi4_internal"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "register_operand" "0")
                    (match_operand:HI 2 "general_operand" "g")] 0))]
  ""
  "deiw %2,%0")

(define_insn "udivmoddihi4"
  [(set (subreg:HI (match_operand:DI 0 "register_operand" "=r") 1)
	(truncate:HI (udiv:DI (match_operand:DI 1 "reg_or_mem_operand" "0")
		 (zero_extend:DI (match_operand:HI 2 "nonimmediate_operand" "g")))))
   (set (subreg:HI (match_operand:DI 3 "register_operand" "=0") 0)
	(truncate:HI (umod:DI (match_dup 1) (zero_extend:DI (match_dup 2)))))]
  ""
  "deiw %2,%0")

(define_expand "udivmodqi4"
  [(parallel
  [(set (match_operand:QI 0 "reg_or_mem_operand" "")
	(udiv:QI (match_operand:QI 1 "general_operand" "")
		     (match_operand:QI 2 "general_operand" "")))
   (set (match_operand:QI 3 "reg_or_mem_operand" "")
	(umod:QI (match_dup 1) (match_dup 2)))])]
  ""
  "
{
  rtx temp = gen_reg_rtx(DImode);
  rtx insn, first, last;
  first = emit_move_insn(gen_lowpart(QImode, temp), operands[1]);
  emit_move_insn(gen_highpart(QImode, temp), const0_rtx);
  operands[2] = force_reg(QImode, operands[2]);
  emit_insn(gen_udivmoddiqi4_internal(temp, temp, operands[2]));
  last = emit_move_insn(temp, temp);
  {
    rtx divdi, moddi, divqi, modqi;
    divqi = gen_rtx (UDIV, QImode, operands[1], operands[2]);
    modqi = gen_rtx (UMOD, QImode, operands[1], operands[2]);
    divdi = gen_rtx (ZERO_EXTEND, DImode, divqi);
    moddi = gen_rtx (ZERO_EXTEND, DImode, modqi);
    REG_NOTES (first) = gen_rtx (INSN_LIST, REG_LIBCALL, last,
			         REG_NOTES (first));
    REG_NOTES (last) = gen_rtx (INSN_LIST, REG_RETVAL, first,
                                gen_rtx (EXPR_LIST, REG_EQUAL,
                       gen_rtx(IOR, DImode, moddi,
                               gen_rtx(ASHIFT, DImode, divdi, GEN_INT(32))),
                       REG_NOTES (last)));
  }

  insn = emit_move_insn(operands[0], gen_highpart(QImode, temp));
  insn = emit_move_insn(operands[3], gen_lowpart(QImode, temp));
  DONE;
}")

;; deib wants two qi's in separate registers or else they can be adjacent
;; in memory. DI mode will ensure two registers are available, but if we
;; want to allow memory as an operand we would need HI mode. There is no
;; way to do this, so just restrict operand 0 and 1 to be in registers.
(define_insn "udivmoddiqi4_internal"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "reg_or_mem_operand" "0")
                    (match_operand:QI 2 "general_operand" "g")] 0))]
  ""
  "deib %2,%0")

(define_insn "udivmoddiqi4"
  [(set (subreg:QI (match_operand:DI 0 "register_operand" "=r") 1)
	(truncate:QI (udiv:DI (match_operand:DI 1 "reg_or_mem_operand" "0")
		 (zero_extend:DI (match_operand:QI 2 "nonimmediate_operand" "g")))))
   (set (subreg:QI (match_operand:DI 3 "register_operand" "=0") 0)
	(truncate:QI (umod:DI (match_dup 1) (zero_extend:DI (match_dup 2)))))]
  ""
  "deib %2,%0")

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "general_operand" "=lm")
	(div:DF (match_operand:DF 1 "general_operand" "0")
		(match_operand:DF 2 "general_operand" "lmF")))]
  "TARGET_32081"
  "divl %2,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "general_operand" "=fm")
	(div:SF (match_operand:SF 1 "general_operand" "0")
		(match_operand:SF 2 "general_operand" "fmF")))]
  "TARGET_32081"
  "divf %2,%0")

;; See note 1
(define_insn "divsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(div:SI (match_operand:SI 1 "general_operand" "0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "quod %2,%0")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(div:HI (match_operand:HI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "quow %2,%0")

(define_insn "divqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(div:QI (match_operand:QI 1 "general_operand" "0")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "quob %2,%0")

;; Remainder instructions.

;; See note 1
(define_insn "modsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(mod:SI (match_operand:SI 1 "general_operand" "0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "remd %2,%0")

(define_insn "modhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(mod:HI (match_operand:HI 1 "general_operand" "0")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "remw %2,%0")

(define_insn "modqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(mod:QI (match_operand:QI 1 "general_operand" "0")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "remb %2,%0")


;;- Logical Instructions: AND

;; See note 1
(define_insn "andsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if ((INTVAL (operands[2]) | 0xff) == 0xffffffff)
	{
	  if (INTVAL (operands[2]) == 0xffffff00)
	    return \"movqb %$0,%0\";
	  else
	    {
	      operands[2] = GEN_INT (INTVAL (operands[2]) & 0xff);
	      return \"andb %2,%0\";
	    }
	}
      if ((INTVAL (operands[2]) | 0xffff) == 0xffffffff)
        {
	  if (INTVAL (operands[2]) == 0xffff0000)
	    return \"movqw %$0,%0\";
	  else
	    {
	      operands[2] = GEN_INT (INTVAL (operands[2]) & 0xffff);
	      return \"andw %2,%0\";
	    }
	}
    }
  return \"andd %2,%0\";
}")

(define_insn "andhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (INTVAL (operands[2]) | 0xff) == 0xffffffff)
    {
      if (INTVAL (operands[2]) == 0xffffff00)
	return \"movqb %$0,%0\";
      else
	{
	  operands[2] = GEN_INT (INTVAL (operands[2]) & 0xff);
	  return \"andb %2,%0\";
	}
    }
  return \"andw %2,%0\";
}")

(define_insn "andqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "%0")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "andb %2,%0")

;; See note 1
(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (not:SI (match_operand:SI 1 "general_operand" "g"))
		(match_operand:SI 2 "general_operand" "0")))]
  ""
  "bicd %1,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (not:HI (match_operand:HI 1 "general_operand" "g"))
		(match_operand:HI 2 "general_operand" "0")))]
  ""
  "bicw %1,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (not:QI (match_operand:QI 1 "general_operand" "g"))
		(match_operand:QI 2 "general_operand" "0")))]
  ""
  "bicb %1,%0")

;;- Bit set instructions.

;; See note 1
(define_insn "iorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ior:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT) {
    if ((INTVAL (operands[2]) & 0xffffff00) == 0)
      return \"orb %2,%0\";
    if ((INTVAL (operands[2]) & 0xffff0000) == 0)
      return \"orw %2,%0\";
  }
  return \"ord %2,%0\";
}")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ior:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE(operands[2]) == CONST_INT &&
      (INTVAL(operands[2]) & 0xffffff00) == 0)
    return \"orb %2,%0\";
  return \"orw %2,%0\";
}")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ior:QI (match_operand:QI 1 "general_operand" "%0")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "orb %2,%0")

;;- xor instructions.

;; See note 1
(define_insn "xorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(xor:SI (match_operand:SI 1 "general_operand" "%0")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT) {
    if ((INTVAL (operands[2]) & 0xffffff00) == 0)
      return \"xorb %2,%0\";
    if ((INTVAL (operands[2]) & 0xffff0000) == 0)
      return \"xorw %2,%0\";
  }
  return \"xord %2,%0\";
}")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(xor:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (GET_CODE(operands[2]) == CONST_INT &&
      (INTVAL(operands[2]) & 0xffffff00) == 0)
    return \"xorb %2,%0\";
  return \"xorw %2,%0\";
}")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(xor:QI (match_operand:QI 1 "general_operand" "%0")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "xorb %2,%0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "general_operand" "=lm<")
	(neg:DF (match_operand:DF 1 "general_operand" "lmF")))]
  "TARGET_32081"
  "negl %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "general_operand" "=fm<")
	(neg:SF (match_operand:SF 1 "general_operand" "fmF")))]
  "TARGET_32081"
  "negf %1,%0")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "general_operand" "=ro")
	(neg:DI (match_operand:DI 1 "general_operand" "ro")))]
  ""
  "*
{
  rtx low[2], high[2], xops[4];
  split_di (operands, 2, low, high);
  xops[0] = low[0];
  xops[1] = high[0];
  xops[2] = low[1];
  xops[3] = high[1];

  if (rtx_equal_p (operands[0], operands[1]))
    {
      output_asm_insn (\"negd %3,%1\", xops);
      output_asm_insn (\"negd %2,%0\", xops);
      output_asm_insn (\"subcd %$0,%1\", xops);
    }
  else
    {
      output_asm_insn (\"negd %2,%0\", xops);
      output_asm_insn (\"movqd %$0,%1\", xops);
      output_asm_insn (\"subcd %3,%1\", xops);
    }
  return \"\"; 
}")

;; See note 1
(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(neg:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "negd %1,%0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(neg:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "negw %1,%0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(neg:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "negb %1,%0")

;; See note 1
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(not:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "comd %1,%0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(not:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "comw %1,%0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(not:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "comb %1,%0")

;; arithmetic left and right shift operations
;; on the 32532 we will always use lshd for arithmetic left shifts,
;; because it is three times faster.  Broken programs which
;; use negative shift counts are probably broken differently
;; than elsewhere.

;; alternative 0 never matches on the 32532
;; See note 1
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g,g")
	(ashift:SI (match_operand:SI 1 "general_operand" "r,0")
		   (match_operand:SI 2 "general_operand" "I,g")))]
  ""
  "*
{ if (TARGET_32532)
    return \"lshd %2,%0\";
  else
    return output_shift_insn (operands);
}")

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ashift:HI (match_operand:HI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) == 1)
	return \"addw %0,%0\";
      else if (! TARGET_32532 && INTVAL (operands[2]) == 2)
	return \"addw %0,%0\;addw %0,%0\";
    }
  if (TARGET_32532)
    return \"lshw %2,%0\";
  else
    return \"ashw %2,%0\";
}")

(define_insn "ashlqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ashift:QI (match_operand:QI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) == 1)
	return \"addb %0,%0\";
      else if (! TARGET_32532 && INTVAL (operands[2]) == 2)
	return \"addb %0,%0\;addb %0,%0\";
    }
  if (TARGET_32532)
    return \"lshb %2,%0\";
  else
    return \"ashb %2,%0\";
}")

;; Arithmetic right shift on the 32k works by negating the shift count.
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "ashd %n2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "ashd %2,%0")

(define_expand "ashrhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ashiftrt:HI (match_operand:HI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ashiftrt:HI (match_operand:HI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "ashw %n2,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ashiftrt:HI (match_operand:HI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "ashw %2,%0")

(define_expand "ashrqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ashiftrt:QI (match_operand:QI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ashiftrt:QI (match_operand:QI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "ashb %n2,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ashiftrt:QI (match_operand:QI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "ashb %2,%0")

;; logical shift instructions

;; Logical right shift on the 32k works by negating the shift count.
(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "lshd %n2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "lshd %2,%0")

(define_expand "lshrhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "lshw %n2,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(lshiftrt:HI (match_operand:HI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "lshw %2,%0")

(define_expand "lshrqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(lshiftrt:QI (match_operand:QI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(lshiftrt:QI (match_operand:QI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "lshb %n2,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(lshiftrt:QI (match_operand:QI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "lshb %2,%0")

;; Rotate instructions

;; See note 1
(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "g")))]
  ""
  "rotd %2,%0")

(define_insn "rotlhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(rotate:HI (match_operand:HI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "g")))]
  ""
  "rotw %2,%0")

(define_insn "rotlqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(rotate:QI (match_operand:QI 1 "general_operand" "0")
		   (match_operand:SI 2 "general_operand" "g")))]
  ""
  "rotb %2,%0")

;; Right rotate on the 32k works by negating the shift count.
(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "rotd %n2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotatert:SI (match_operand:SI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "rotd %2,%0")

(define_expand "rotrhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(rotatert:HI (match_operand:HI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(rotatert:HI (match_operand:HI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "rotw %n2,%0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(rotatert:HI (match_operand:HI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "rotw %2,%0")

(define_expand "rotrqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(rotatert:QI (match_operand:QI 1 "general_operand" "g")
		     (match_operand:SI 2 "general_operand" "g")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    operands[2] = gen_rtx_NEG (SImode, negate_rtx (SImode, operands[2]));
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(rotatert:QI (match_operand:QI 1 "general_operand" "0")
		     (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "rotb %n2,%0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(rotatert:QI (match_operand:QI 1 "general_operand" "0")
		     (neg:SI (match_operand:SI 2 "general_operand" "r"))))]
  ""
  "rotb %2,%0")

;;- load or push effective address 
;; These come after the move, add, and multiply patterns
;; because we don't want pushl $1 turned into pushad 1.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(match_operand:QI 1 "address_operand" "p"))]
  ""
  "*
{
  if (REG_P (operands[0])
      && GET_CODE (operands[1]) == MULT
      && GET_CODE (XEXP (operands[1], 1)) == CONST_INT
      && (INTVAL (XEXP (operands[1], 1)) == 2
	  || INTVAL (XEXP (operands[1], 1)) == 4))
    {
      rtx xoperands[3];
      xoperands[0] = operands[0];
      xoperands[1] = XEXP (operands[1], 0);
      xoperands[2] = GEN_INT (INTVAL (XEXP (operands[1], 1)) >> 1);
      return output_shift_insn (xoperands);
    }
  return \"addr %a1,%0\";
}")

;;; Index insns.  These are about the same speed as multiply-add counterparts.
;;; but slower then using power-of-2 shifts if we can use them
;
;;; See note 1
;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=r")
;	(plus:SI (match_operand:SI 1 "general_operand" "g")
;		 (mult:SI (match_operand:SI 2 "register_operand" "0")
;			  (plus:SI (match_operand:SI 3 "general_operand" "g") (const_int 1)))))]
;  "GET_CODE (operands[3]) != CONST_INT || INTVAL (operands[3]) > 8"
;  "indexd %0,%3,%1")
;
;(define_insn ""
;  [(set (match_operand:SI 0 "register_operand" "=r")
;	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "0")
;			  (plus:SI (match_operand:SI 2 "general_operand" "g") (const_int 1)))
;		 (match_operand:SI 3 "general_operand" "g")))]
;  "GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) > 8"
;  "indexd %0,%2,%3")

;; Set, Clear, and Invert bit

;; See note 1
(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "general_operand" "+g")
			 (const_int 1)
			 (match_operand:SI 1 "general_operand" "g"))
	(const_int 1))]
  ""
  "sbitd %1,%0")

;; See note 1
(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "general_operand" "+g")
			 (const_int 1)
			 (match_operand:SI 1 "general_operand" "g"))
	(const_int 0))]
  ""
  "cbitd %1,%0")

;; See note 1
(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "+g")
	(xor:SI (ashift:SI (const_int 1)
			   (match_operand:SI 1 "general_operand" "g"))
		(match_dup 0)))]
  ""
  "ibitd %1,%0")

;; See note 1
(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(xor:QI (subreg:QI
		 (ashift:SI (const_int 1)
			    (match_operand:QI 1 "general_operand" "g")) 0)
		(match_dup 0)))]
  ""
  "ibitb %1,%0")

;; Recognize jbs and jbc instructions.

(define_insn ""
  [(set (cc0)
	(zero_extract (match_operand:SI 0 "general_operand" "rm")
		      (const_int 1)
		      (match_operand:SI 1 "general_operand" "g")))]
  ""
  "*
{ cc_status.flags = CC_Z_IN_F;
  return \"tbitd %1,%0\";
}")

;; extract(base, width, offset)
;; Signed bitfield extraction is not supported in hardware on the
;; NS 32032.  It is therefore better to let GCC figure out a
;; good strategy for generating the proper instruction sequence
;; and represent it as rtl.

;; Optimize the case of extracting a byte or word from a register.
;; Otherwise we must load a register with the offset of the
;; chunk we want, and perform an extract insn (each of which
;; is very expensive).  Since we use the stack to do our bit-twiddling
;; we cannot use it for a destination.  Perhaps things are fast
;; enough on the 32532 that such hacks are not needed.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=ro")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "const_int_operand" "i")
			 (match_operand:SI 3 "const_int_operand" "i")))]
  "(INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && (INTVAL (operands[3]) == 8 || INTVAL (operands[3]) == 16 || INTVAL (operands[3]) == 24)"
  "*
{
  output_asm_insn (\"movd %1,tos\", operands);
  if (INTVAL (operands[2]) == 16)
    {
      if (INTVAL (operands[3]) == 8)
	output_asm_insn (\"movzwd 1(sp),%0\", operands);
      else
	output_asm_insn (\"movzwd 2(sp),%0\", operands);
    }
  else
    {
      if (INTVAL (operands[3]) == 8)
	output_asm_insn (\"movzbd 1(sp),%0\", operands);
      else if (INTVAL (operands[3]) == 16)
	output_asm_insn (\"movzbd 2(sp),%0\", operands);
      else
	output_asm_insn (\"movzbd 3(sp),%0\", operands);
    }
  if (TARGET_32532 || TARGET_32332)
    return \"cmpqd %$0,tos\";
  else
    return \"adjspb %$-4\";
}")

;; The exts/ext instructions have the problem that they always access
;; 32 bits even if the bitfield is smaller. For example the instruction
;; 	extsd 7(r1),r0,2,5
;; would read not only at address 7(r1) but also at 8(r1) to 10(r1).
;; If these addresses are in a different (unmapped) page a memory fault
;; is the result.
;;
;; Timing considerations:
;;	movd	0(r1),r0	3 bytes
;;	lshd	-26,r0		4
;;	andd	0x1f,r0		5
;; takes about 13 cycles on the 532 while
;;	extsd	7(r1),r0,2,5	5 bytes
;; takes about 21 cycles.
;;
;; The inss/ins instructions suffer from the same problem.
;;
;; A machine specific option (-mbitfield/-mnobitfield) is used
;; to allow/disallow the use of these instructions.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "g")
			 (match_operand:SI 2 "const_int_operand" "i")
			 (match_operand:SI 3 "general_operand" "rK")))]
  "TARGET_BITFIELD"
  "*
{ if (GET_CODE (operands[3]) == CONST_INT)
    return \"extsd %1,%0,%3,%2\";
  else return \"extd %3,%1,%0,%2\";
}")

(define_insn "extzv"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(zero_extract:SI (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "const_int_operand" "i")
			 (match_operand:SI 3 "general_operand" "rK")))]
  "TARGET_BITFIELD"
  "*
{ if (GET_CODE (operands[3]) == CONST_INT)
    return \"extsd %1,%0,%3,%2\";
  else return \"extd %3,%1,%0,%2\";
}")

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "memory_operand" "+o")
			 (match_operand:SI 1 "const_int_operand" "i")
			 (match_operand:SI 2 "general_operand" "rn"))
	(match_operand:SI 3 "general_operand" "rm"))]
  "TARGET_BITFIELD"
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) >= 8)
	{
	  operands[0] = adj_offsettable_operand (operands[0],
					        INTVAL (operands[2]) / 8);
          operands[2] = GEN_INT (INTVAL (operands[2]) % 8);
	}
      if (INTVAL (operands[1]) <= 8)
        return \"inssb %3,%0,%2,%1\";
      else if (INTVAL (operands[1]) <= 16)
	return \"inssw %3,%0,%2,%1\";
      else
	return \"inssd %3,%0,%2,%1\";
    }
  return \"insd %2,%3,%0,%1\";
}")

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (match_operand:SI 1 "const_int_operand" "i")
			 (match_operand:SI 2 "general_operand" "rK"))
	(match_operand:SI 3 "general_operand" "rm"))]
  "TARGET_BITFIELD"
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    if (INTVAL (operands[1]) <= 8)
      return \"inssb %3,%0,%2,%1\";
    else if (INTVAL (operands[1]) <= 16)
      return \"inssw %3,%0,%2,%1\";
    else
      return \"inssd %3,%0,%2,%1\";
  return \"insd %2,%3,%0,%1\";
}")

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:QI 0 "general_operand" "+g")
			 (match_operand:SI 1 "const_int_operand" "i")
			 (match_operand:SI 2 "general_operand" "rK"))
	(match_operand:SI 3 "general_operand" "rm"))]
  "TARGET_BITFIELD"
  "*
{ if (GET_CODE (operands[2]) == CONST_INT)
    if (INTVAL (operands[1]) <= 8)
      return \"inssb %3,%0,%2,%1\";
    else if (INTVAL (operands[1]) <= 16)
      return \"inssw %3,%0,%2,%1\";
    else
      return \"inssd %3,%0,%2,%1\";
  return \"insd %2,%3,%0,%1\";
}")


(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "br %l0")

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"bfc %l0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"bfs %l0\";
  else return \"beq %l0\";
}")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"bfs %l0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"bfc %l0\";
  else return \"bne %l0\";
}")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bgt %l0")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bhi %l0")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "blt %l0")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "blo %l0")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bge %l0")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bhs %l0")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "ble %l0")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bls %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"bfs %l0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"bfc %l0\";
  else return \"bne %l0\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"bfc %l0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"bfs %l0\";
  else return \"beq %l0\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "ble %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bls %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bge %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bhs %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "blt %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "blo %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bgt %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bhi %l0")

;; Subtract-and-jump and Add-and-jump insns.
;; These can actually be used for adding numbers in the range -8 to 7

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (match_operand:SI 0 "general_operand" "+g")
	     (match_operand:SI 1 "const_int_operand" "i"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
  (set (match_dup 0)
       (minus:SI (match_dup 0)
		 (match_dup 1)))]
  "INTVAL (operands[1]) > -8 && INTVAL (operands[1]) <= 8"
  "acbd %n1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (match_operand:SI 0 "general_operand" "+g")
	     (match_operand:SI 1 "const_int_operand" "i"))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
  (set (match_dup 0)
       (plus:SI (match_dup 0)
		(match_operand:SI 3 "const_int_operand" "i")))]
  "INTVAL (operands[1]) == - INTVAL (operands[3])
   && INTVAL (operands[3]) >= -8 && INTVAL (operands[3]) < 8"
  "acbd %3,%0,%l2")

(define_insn "call"
  [(call (match_operand:QI 0 "memory_operand" "m")
	 (match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
#ifndef JSR_ALWAYS
  if (GET_CODE (operands[0]) == MEM)
    {
      rtx temp = XEXP (operands[0], 0);
      if (CONSTANT_ADDRESS_P (temp))
	{
#ifdef ENCORE_ASM
	  return \"bsr %?%0\";
#else
#ifdef CALL_MEMREF_IMPLICIT
	  operands[0] = temp;
	  return \"bsr %0\";
#else
#ifdef GNX_V3
	  return \"bsr %0\";
#else
	  return \"bsr %?%a0\";
#endif
#endif
#endif
	}
      if (GET_CODE (XEXP (operands[0], 0)) == REG)
#if defined (GNX_V3) || defined (CALL_MEMREF_IMPLICIT)
	return \"jsr %0\";
#else
        return \"jsr %a0\";
#endif
    }
#endif /* not JSR_ALWAYS */
  return \"jsr %0\";
}")

(define_insn "call_value"
  [(set (match_operand 0 "" "=rf")
	(call (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
#ifndef JSR_ALWAYS
  if (GET_CODE (operands[1]) == MEM)
    {
      rtx temp = XEXP (operands[1], 0);
      if (CONSTANT_ADDRESS_P (temp))
	{
#ifdef ENCORE_ASM
	  return \"bsr %?%1\";
#else
#ifdef CALL_MEMREF_IMPLICIT
	  operands[1] = temp;
	  return \"bsr %1\";
#else
#ifdef GNX_V3
	  return \"bsr %1\";
#else
	  return \"bsr %?%a1\";
#endif
#endif
#endif
	}
      if (GET_CODE (XEXP (operands[1], 0)) == REG)
#if defined (GNX_V3) || defined (CALL_MEMREF_IMPLICIT)
	return \"jsr %1\";
#else
        return \"jsr %a1\";
#endif
    }
#endif /* not JSR_ALWAYS */
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

(define_insn "return"
  [(return)]
  "0"
  "ret 0")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "general_operand" "=fm<")
	(abs:SF (match_operand:SF 1 "general_operand" "fmF")))]
  "TARGET_32081"
  "absf %1,%0")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "general_operand" "=lm<")
	(abs:DF (match_operand:DF 1 "general_operand" "lmF")))]
  "TARGET_32081"
  "absl %1,%0")

;; See note 1
(define_insn "abssi2"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(abs:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "absd %1,%0")

(define_insn "abshi2"
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(abs:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "absw %1,%0")

(define_insn "absqi2"
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(abs:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "absb %1,%0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jump %0")

(define_insn "tablejump"
  [(set (pc)
	(plus:SI (pc) (match_operand:SI 0 "general_operand" "g")))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "*
{
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"LI\",
			     CODE_LABEL_NUMBER (operands[1]));
  return \"cased %0\";
}")

;; Scondi instructions
(define_insn "seq"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(eq:SI (cc0) (const_int 0)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"sfcd %0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"sfsd %0\";
  else return \"seqd %0\";
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(eq:HI (cc0) (const_int 0)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"sfcw %0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"sfsw %0\";
  else return \"seqw %0\";
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(eq:QI (cc0) (const_int 0)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"sfcb %0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"sfsb %0\";
  else return \"seqb %0\";
}")

(define_insn "sne"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(ne:SI (cc0) (const_int 0)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"sfsd %0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"sfcd %0\";
  else return \"sned %0\";
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(ne:HI (cc0) (const_int 0)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"sfsw %0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"sfcw %0\";
  else return \"snew %0\";
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(ne:QI (cc0) (const_int 0)))]
  ""
  "*
{ if (cc_prev_status.flags & CC_Z_IN_F)
    return \"sfsb %0\";
  else if (cc_prev_status.flags & CC_Z_IN_NOT_F)
    return \"sfcb %0\";
  else return \"sneb %0\";
}")

(define_insn "sgt"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(gt:SI (cc0) (const_int 0)))]
  ""
  "sgtd %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(gt:HI (cc0) (const_int 0)))]
  ""
  "sgtw %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(gt:QI (cc0) (const_int 0)))]
  ""
  "sgtb %0")

(define_insn "sgtu"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(gtu:SI (cc0) (const_int 0)))]
  ""
  "shid %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(gtu:HI (cc0) (const_int 0)))]
  ""
  "shiw %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(gtu:QI (cc0) (const_int 0)))]
  ""
  "shib %0")

(define_insn "slt"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(lt:SI (cc0) (const_int 0)))]
  ""
  "sltd %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(lt:HI (cc0) (const_int 0)))]
  ""
  "sltw %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(lt:QI (cc0) (const_int 0)))]
  ""
  "sltb %0")

(define_insn "sltu"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(ltu:SI (cc0) (const_int 0)))]
  ""
  "slod %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(ltu:HI (cc0) (const_int 0)))]
  ""
  "slow %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(ltu:QI (cc0) (const_int 0)))]
  ""
  "slob %0")

(define_insn "sge"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(ge:SI (cc0) (const_int 0)))]
  ""
  "sged %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(ge:HI (cc0) (const_int 0)))]
  ""
  "sgew %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(ge:QI (cc0) (const_int 0)))]
  ""
  "sgeb %0")

(define_insn "sgeu"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(geu:SI (cc0) (const_int 0)))]
  ""
  "shsd %0")  

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(geu:HI (cc0) (const_int 0)))]
  ""
  "shsw %0")  

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(geu:QI (cc0) (const_int 0)))]
  ""
  "shsb %0")  

(define_insn "sle"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(le:SI (cc0) (const_int 0)))]
  ""
  "sled %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(le:HI (cc0) (const_int 0)))]
  ""
  "slew %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(le:QI (cc0) (const_int 0)))]
  ""
  "sleb %0")

(define_insn "sleu"
  [(set (match_operand:SI 0 "general_operand" "=g<")
	(leu:SI (cc0) (const_int 0)))]
  ""
  "slsd %0")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g<")
	(leu:HI (cc0) (const_int 0)))]
  ""
  "slsw %0")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g<")
	(leu:QI (cc0) (const_int 0)))]
  ""
  "slsb %0")

;; ffs instructions

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=ro")
	(minus:SI 
		(plus:SI (ffs:SI (zero_extract:SI 
				(match_operand:SI 1 "general_operand" "g") 
				(minus:SI (const_int 32) (match_dup 0))
				(match_dup 0)))
			(match_dup 0)) 
		(const_int 1)))]
  ""
  "ffsd %1,%0; bfc 1f; addqd %$-1,%0; 1:")

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "general_operand" "=g") (const_int 0))
   (set (match_dup 0)
	(minus:SI 
		(plus:SI (ffs:SI (zero_extract:SI 
				(match_operand:SI 1 "general_operand" "g") 
				(minus:SI (const_int 32) (match_dup 0))
				(match_dup 0)))
			(match_dup 0)) 
		(const_int 1)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  ""
  "operands[1] = make_safe_from(operands[1], operands[0]);")

;; Speed up stack adjust followed by a HI fixedpoint push.

(define_peephole
  [(set (reg:SI 25) (plus:SI (reg:SI 25) (const_int -2)))
   (set (match_operand:HI 0 "push_operand" "=m")
	(match_operand:HI 1 "general_operand" "g"))]
  "! reg_mentioned_p (stack_pointer_rtx, operands[1])"
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[1]), \"%1,tos\"),
			 operands);
  else
	output_asm_insn (\"movzwd %1,tos\", operands);
  return \"\";
}")

;; Speed up stack adjust followed by a zero_extend:HI(QI) fixedpoint push.

(define_peephole
  [(set (reg:SI 25) (plus:SI (reg:SI 25) (const_int -2)))
   (set (match_operand:HI 0 "push_operand" "=m")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  "! reg_mentioned_p (stack_pointer_rtx, operands[1])"
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[1]), \"%1,tos\"),
			 operands);
  else
	output_asm_insn (\"movzbd %1,tos\", operands);
  return \"\";
}")

;; Speed up stack adjust followed by a sign_extend:HI(QI) fixedpoint push.

(define_peephole
  [(set (reg:SI 25) (plus:SI (reg:SI 25) (const_int -2)))
   (set (match_operand:HI 0 "push_operand" "=m")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  "! reg_mentioned_p (stack_pointer_rtx, operands[1])"
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[1]), \"%1,tos\"),
			 operands);
  else
	output_asm_insn (\"movxbd %1,tos\", operands);
  return \"\";
}")

;; Speed up stack adjust followed by a QI fixedpoint push.

(define_peephole
  [(set (reg:SI 25) (plus:SI (reg:SI 25) (const_int -3)))
   (set (match_operand:QI 0 "push_operand" "=m")
	(match_operand:QI 1 "general_operand" "g"))]
  "! reg_mentioned_p (stack_pointer_rtx, operands[1])"
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[1]), \"%1,tos\"),
			 operands);
  else
	output_asm_insn (\"movzbd %1,tos\", operands);
  return \"\";
}")

;; Speed up stack adjust followed by a SI fixedpoint push.

(define_peephole
  [(set (reg:SI 25) (plus:SI (reg:SI 25) (const_int 4)))
   (set (match_operand:SI 0 "push_operand" "=m")
	(match_operand:SI 1 "general_operand" "g"))]
  "! reg_mentioned_p (stack_pointer_rtx, operands[1])"
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[1]), \"%1,0(sp)\"),
			 operands);
  else if (GET_CODE (operands[1]) != REG
	   && GET_CODE (operands[1]) != MEM
	   && address_operand (operands[1], SImode))
	output_asm_insn (\"addr %a1,0(sp)\", operands);
  else
	output_asm_insn (\"movd %1,0(sp)\", operands);
  return \"\";
}")

;; Speed up stack adjust followed by two fullword fixedpoint pushes.

(define_peephole
  [(set (reg:SI 25) (plus:SI (reg:SI 25) (const_int 8)))
   (set (match_operand:SI 0 "push_operand" "=m")
	(match_operand:SI 1 "general_operand" "g"))
   (set (match_operand:SI 2 "push_operand" "=m")
	(match_operand:SI 3 "general_operand" "g"))]
  "! reg_mentioned_p (stack_pointer_rtx, operands[1])
   && ! reg_mentioned_p (stack_pointer_rtx, operands[3])"
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[1]), \"%1,4(sp)\"),
			 operands);
  else if (GET_CODE (operands[1]) != REG
	   && GET_CODE (operands[1]) != MEM
	   && address_operand (operands[1], SImode))
	output_asm_insn (\"addr %a1,4(sp)\", operands);
  else
	output_asm_insn (\"movd %1,4(sp)\", operands);

  if (GET_CODE (operands[3]) == CONST_INT)
	output_asm_insn (output_move_dconst (INTVAL (operands[3]), \"%3,0(sp)\"),
			 operands);
  else if (GET_CODE (operands[3]) != REG
	   && GET_CODE (operands[3]) != MEM
	   && address_operand (operands[3], SImode))
	output_asm_insn (\"addr %a3,0(sp)\", operands);
  else
	output_asm_insn (\"movd %3,0(sp)\", operands);
  return \"\";
}")
