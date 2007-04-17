;; Decimal Floating Point (DFP) patterns.
;; Copyright (C) 2007
;; Free Software Foundation, Inc.
;; Contributed by Ben Elliston (bje@au.ibm.com) and Peter Bergner
;; (bergner@vnet.ibm.com).

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to the
;; Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(define_expand "negdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(neg:DD (match_operand:DD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)"
  "")

(define_insn "*negdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=f")
	(neg:DD (match_operand:DD 1 "gpc_reg_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fneg %0,%1"
  [(set_attr "type" "fp")])

(define_expand "absdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(abs:DD (match_operand:DD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)"
  "")

(define_insn "*absdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=f")
	(abs:DD (match_operand:DD 1 "gpc_reg_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fabs %0,%1"
  [(set_attr "type" "fp")])

(define_insn "*nabsdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=f")
	(neg:DD (abs:DD (match_operand:DF 1 "gpc_reg_operand" "f"))))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fnabs %0,%1"
  [(set_attr "type" "fp")])

(define_expand "movdd"
  [(set (match_operand:DD 0 "nonimmediate_operand" "")
	(match_operand:DD 1 "any_operand" ""))]
  ""
  "{ rs6000_emit_move (operands[0], operands[1], DDmode); DONE; }")

(define_split
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(match_operand:DD 1 "const_int_operand" ""))]
  "! TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 1))]
  "
{
  int endian = (WORDS_BIG_ENDIAN == 0);
  HOST_WIDE_INT value = INTVAL (operands[1]);

  operands[2] = operand_subword (operands[0], endian, 0, DDmode);
  operands[3] = operand_subword (operands[0], 1 - endian, 0, DDmode);
#if HOST_BITS_PER_WIDE_INT == 32
  operands[4] = (value & 0x80000000) ? constm1_rtx : const0_rtx;
#else
  operands[4] = GEN_INT (value >> 32);
  operands[1] = GEN_INT (((value & 0xffffffff) ^ 0x80000000) - 0x80000000);
#endif
}")

(define_split
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(match_operand:DD 1 "const_double_operand" ""))]
  "! TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 5))]
  "
{
  int endian = (WORDS_BIG_ENDIAN == 0);
  long l[2];
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DECIMAL64 (rv, l);

  operands[2] = operand_subword (operands[0], endian, 0, DDmode);
  operands[3] = operand_subword (operands[0], 1 - endian, 0, DDmode);
  operands[4] = gen_int_mode (l[endian], SImode);
  operands[5] = gen_int_mode (l[1 - endian], SImode);
}")

(define_split
  [(set (match_operand:DD 0 "gpc_reg_operand" "")
	(match_operand:DD 1 "const_double_operand" ""))]
  "TARGET_POWERPC64 && reload_completed
   && ((GET_CODE (operands[0]) == REG && REGNO (operands[0]) <= 31)
       || (GET_CODE (operands[0]) == SUBREG
	   && GET_CODE (SUBREG_REG (operands[0])) == REG
	   && REGNO (SUBREG_REG (operands[0])) <= 31))"
  [(set (match_dup 2) (match_dup 3))]
  "
{
  int endian = (WORDS_BIG_ENDIAN == 0);
  long l[2];
  REAL_VALUE_TYPE rv;
#if HOST_BITS_PER_WIDE_INT >= 64
  HOST_WIDE_INT val;
#endif

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_DECIMAL64 (rv, l);

  operands[2] = gen_lowpart (DImode, operands[0]);
  /* HIGHPART is lower memory address when WORDS_BIG_ENDIAN.  */
#if HOST_BITS_PER_WIDE_INT >= 64
  val = ((HOST_WIDE_INT)(unsigned long)l[endian] << 32
	 | ((HOST_WIDE_INT)(unsigned long)l[1 - endian]));

  operands[3] = gen_int_mode (val, DImode);
#else
  operands[3] = immed_double_const (l[1 - endian], l[endian], DImode);
#endif
}")

;; Don't have reload use general registers to load a constant.  First,
;; it might not work if the output operand is the equivalent of
;; a non-offsettable memref, but also it is less efficient than loading
;; the constant into an FP register, since it will probably be used there.
;; The "??" is a kludge until we can figure out a more reasonable way
;; of handling these non-offsettable values.
(define_insn "*movdd_hardfloat32"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=!r,??r,m,f,f,m,!r,!r,!r")
	(match_operand:DD 1 "input_operand" "r,m,r,f,m,f,G,H,F"))]
  "! TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "*
{
  switch (which_alternative)
    {
    default:
      gcc_unreachable ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register
	 of operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return \"mr %L0,%L1\;mr %0,%1\";
      else
	return \"mr %0,%1\;mr %L0,%L1\";
    case 1:
      if (rs6000_offsettable_memref_p (operands[1])
	  || (GET_CODE (operands[1]) == MEM
	      && (GET_CODE (XEXP (operands[1], 0)) == LO_SUM
		  || GET_CODE (XEXP (operands[1], 0)) == PRE_INC
		  || GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)))
	{
	  /* If the low-address word is used in the address, we must load
	     it last.  Otherwise, load it first.  Note that we cannot have
	     auto-increment in that case since the address register is
	     known to be dead.  */
	  if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
				 operands[1], 0))
	    return \"{l|lwz} %L0,%L1\;{l|lwz} %0,%1\";
	  else
	    return \"{l%U1|lwz%U1} %0,%1\;{l|lwz} %L0,%L1\";
	}
      else
	{
	  rtx addreg;

	  addreg = find_addr_reg (XEXP (operands[1], 0));
	  if (refers_to_regno_p (REGNO (operands[0]),
				 REGNO (operands[0]) + 1,
				 operands[1], 0))
	    {
	      output_asm_insn (\"{cal|la} %0,4(%0)\", &addreg);
	      output_asm_insn (\"{lx|lwzx} %L0,%1\", operands);
	      output_asm_insn (\"{cal|la} %0,-4(%0)\", &addreg);
	      return \"{lx|lwzx} %0,%1\";
	    }
	  else
	    {
	      output_asm_insn (\"{lx|lwzx} %0,%1\", operands);
	      output_asm_insn (\"{cal|la} %0,4(%0)\", &addreg);
	      output_asm_insn (\"{lx|lwzx} %L0,%1\", operands);
	      output_asm_insn (\"{cal|la} %0,-4(%0)\", &addreg);
	      return \"\";
	    }
	}
    case 2:
      if (rs6000_offsettable_memref_p (operands[0])
	  || (GET_CODE (operands[0]) == MEM
	      && (GET_CODE (XEXP (operands[0], 0)) == LO_SUM
		  || GET_CODE (XEXP (operands[0], 0)) == PRE_INC
		  || GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)))
	return \"{st%U0|stw%U0} %1,%0\;{st|stw} %L1,%L0\";
      else
	{
	  rtx addreg;

	  addreg = find_addr_reg (XEXP (operands[0], 0));
	  output_asm_insn (\"{stx|stwx} %1,%0\", operands);
	  output_asm_insn (\"{cal|la} %0,4(%0)\", &addreg);
	  output_asm_insn (\"{stx|stwx} %L1,%0\", operands);
	  output_asm_insn (\"{cal|la} %0,-4(%0)\", &addreg);
	  return \"\";
	}
    case 3:
      return \"fmr %0,%1\";
    case 4:
      return \"lfd%U1%X1 %0,%1\";
    case 5:
      return \"stfd%U0%X0 %1,%0\";
    case 6:
    case 7:
    case 8:
      return \"#\";
    }
}"
  [(set_attr "type" "two,load,store,fp,fpload,fpstore,*,*,*")
   (set_attr "length" "8,16,16,4,4,4,8,12,16")])

(define_insn "*movdd_softfloat32"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=r,r,m,r,r,r")
	(match_operand:DD 1 "input_operand" "r,m,r,G,H,F"))]
  "! TARGET_POWERPC64 && TARGET_SOFT_FLOAT
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "*
{
  switch (which_alternative)
    {
    default:
      gcc_unreachable ();
    case 0:
      /* We normally copy the low-numbered register first.  However, if
	 the first register operand 0 is the same as the second register of
	 operand 1, we must copy in the opposite order.  */
      if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	return \"mr %L0,%L1\;mr %0,%1\";
      else
	return \"mr %0,%1\;mr %L0,%L1\";
    case 1:
      /* If the low-address word is used in the address, we must load
	 it last.  Otherwise, load it first.  Note that we cannot have
	 auto-increment in that case since the address register is
	 known to be dead.  */
      if (refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			     operands[1], 0))
	return \"{l|lwz} %L0,%L1\;{l|lwz} %0,%1\";
      else
	return \"{l%U1|lwz%U1} %0,%1\;{l|lwz} %L0,%L1\";
    case 2:
      return \"{st%U0|stw%U0} %1,%0\;{st|stw} %L1,%L0\";
    case 3:
    case 4:
    case 5:
      return \"#\";
    }
}"
  [(set_attr "type" "two,load,store,*,*,*")
   (set_attr "length" "8,8,8,8,12,16")])

; ld/std require word-aligned displacements -> 'Y' constraint.
; List Y->r and r->Y before r->r for reload.
(define_insn "*movdd_hardfloat64_mfpgpr"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=Y,r,!r,f,f,m,*c*l,!r,*h,!r,!r,!r,r,f")
	(match_operand:DD 1 "input_operand" "r,Y,r,f,m,f,r,h,0,G,H,F,f,r"))]
  "TARGET_POWERPC64 && TARGET_MFPGPR && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "@
   std%U0%X0 %1,%0
   ld%U1%X1 %0,%1
   mr %0,%1
   fmr %0,%1
   lfd%U1%X1 %0,%1
   stfd%U0%X0 %1,%0
   mt%0 %1
   mf%1 %0
   {cror 0,0,0|nop}
   #
   #
   #
   mftgpr %0,%1
   mffgpr %0,%1"
  [(set_attr "type" "store,load,*,fp,fpload,fpstore,mtjmpr,mfjmpr,*,*,*,*,mftgpr,mffgpr")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,8,12,16,4,4")])

; ld/std require word-aligned displacements -> 'Y' constraint.
; List Y->r and r->Y before r->r for reload.(define_insn "*movdd_hardfloat64"
(define_insn "*movdd_hardfloat64"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=Y,r,!r,f,f,m,*c*l,!r,*h,!r,!r,!r")
	(match_operand:DD 1 "input_operand" "r,Y,r,f,m,f,r,h,0,G,H,F"))]
  "TARGET_POWERPC64 && !TARGET_MFPGPR && TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "@
   std%U0%X0 %1,%0
   ld%U1%X1 %0,%1
   mr %0,%1
   fmr %0,%1
   lfd%U1%X1 %0,%1
   stfd%U0%X0 %1,%0
   mt%0 %1
   mf%1 %0
   {cror 0,0,0|nop}
   #
   #
   #"
  [(set_attr "type" "store,load,*,fp,fpload,fpstore,mtjmpr,mfjmpr,*,*,*,*")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,8,12,16")])

(define_insn "*movdd_softfloat64"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=r,Y,r,cl,r,r,r,r,*h")
	(match_operand:DD 1 "input_operand" "Y,r,r,r,h,G,H,F,0"))]
  "TARGET_POWERPC64 && (TARGET_SOFT_FLOAT || !TARGET_FPRS)
   && (gpc_reg_operand (operands[0], DDmode)
       || gpc_reg_operand (operands[1], DDmode))"
  "@
   ld%U1%X1 %0,%1
   std%U0%X0 %1,%0
   mr %0,%1
   mt%0 %1
   mf%1 %0
   #
   #
   #
   {cror 0,0,0|nop}"
  [(set_attr "type" "load,store,*,mtjmpr,mfjmpr,*,*,*,*")
   (set_attr "length" "4,4,4,4,4,8,12,16,4")])

(define_expand "negtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "")
	(neg:TD (match_operand:TD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)"
  "")

(define_insn "*negtd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=f")
	(neg:TD (match_operand:TD 1 "gpc_reg_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fneg %0,%1"
  [(set_attr "type" "fp")])

(define_expand "abstd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "")
	(abs:TD (match_operand:TD 1 "gpc_reg_operand" "")))]
  "TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)"
  "")

(define_insn "*abstd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=f")
	(abs:TD (match_operand:TD 1 "gpc_reg_operand" "f")))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fabs %0,%1"
  [(set_attr "type" "fp")])

(define_insn "*nabstd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=f")
	(neg:TD (abs:TD (match_operand:DF 1 "gpc_reg_operand" "f"))))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "fnabs %0,%1"
  [(set_attr "type" "fp")])

(define_expand "movtd"
  [(set (match_operand:TD 0 "general_operand" "")
	(match_operand:TD 1 "any_operand" ""))]
  "TARGET_HARD_FLOAT && TARGET_FPRS"
  "{ rs6000_emit_move (operands[0], operands[1], TDmode); DONE; }")

; It's important to list the o->f and f->o moves before f->f because
; otherwise reload, given m->f, will try to pick f->f and reload it,
; which doesn't make progress.  Likewise r->Y must be before r->r.
(define_insn_and_split "*movtd_internal"
  [(set (match_operand:TD 0 "nonimmediate_operand" "=o,f,f,r,Y,r")
	(match_operand:TD 1 "input_operand"         "f,o,f,YGHF,r,r"))]
  "TARGET_HARD_FLOAT && TARGET_FPRS
   && (gpc_reg_operand (operands[0], TDmode)
       || gpc_reg_operand (operands[1], TDmode))"
  "#"
  "&& reload_completed"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; }
  [(set_attr "length" "8,8,8,20,20,16")])

