/* Auxiliary functions for output asm template or expand rtl
   pattern of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2019 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------------ */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "recog.h"
#include "output.h"
#include "tm-constrs.h"
#include "expr.h"
#include "emit-rtl.h"
#include "explow.h"
#include "stringpool.h"
#include "attribs.h"


/* ------------------------------------------------------------------------ */

static int
nds32_regno_to_enable4 (unsigned regno)
{
  switch (regno)
    {
    case 28: /* $r28/fp */
      return 0x8;
    case 29: /* $r29/gp */
      return 0x4;
    case 30: /* $r30/lp */
      return 0x2;
    case 31: /* $r31/sp */
      return 0x1;
    default:
      gcc_unreachable ();
    }
}

/* A helper function to return character based on byte size.  */
static char
nds32_byte_to_size (int byte)
{
  switch (byte)
    {
    case 4:
      return 'w';
    case 2:
      return 'h';
    case 1:
      return 'b';
    default:
      /* Normally it should not be here.  */
      gcc_unreachable ();
    }
}

static int
nds32_inverse_cond_code (int code)
{
  switch (code)
    {
      case NE:
	return EQ;
      case EQ:
	return NE;
      case GT:
	return LE;
      case LE:
	return GT;
      case GE:
	return LT;
      case LT:
	return GE;
      default:
	gcc_unreachable ();
    }
}

static const char *
nds32_cond_code_str (int code)
{
  switch (code)
    {
      case NE:
	return "ne";
      case EQ:
	return "eq";
      case GT:
	return "gt";
      case LE:
	return "le";
      case GE:
	return "ge";
      case LT:
	return "lt";
      default:
	gcc_unreachable ();
    }
}

static void
output_cond_branch (int code, const char *suffix, bool r5_p,
		    bool long_jump_p, rtx *operands)
{
  char pattern[256];
  const char *cond_code;
  bool align_p = NDS32_ALIGN_P ();
  const char *align = align_p ? "\t.align\t2\n" : "";

  if (r5_p && REGNO (operands[2]) == 5 && TARGET_16_BIT)
    {
      /* This is special case for beqs38 and bnes38,
	 second operand 2 can't be $r5 and it's almost meanless,
	 however it may occur after copy propgation.  */
      if (code == EQ)
	{
	  /* $r5 == $r5 always taken! */
	  if (long_jump_p)
	    snprintf (pattern, sizeof (pattern),
		      "j\t%%3");
	  else
	    snprintf (pattern, sizeof (pattern),
		      "j8\t%%3");
	}
      else
	/* Don't output anything since $r5 != $r5 never taken! */
	pattern[0] = '\0';
    }
  else if (long_jump_p)
    {
      int inverse_code = nds32_inverse_cond_code (code);
      cond_code = nds32_cond_code_str (inverse_code);

      /*      b<cond><suffix>  $r0, $r1, .L0
	    =>
	      b<inverse_cond><suffix>  $r0, $r1, .LCB0
	      j  .L0
	    .LCB0:

	    or

	      b<cond><suffix>  $r0, $r1, .L0
	    =>
	      b<inverse_cond><suffix>  $r0, $r1, .LCB0
	      j  .L0
	    .LCB0:
      */
      if (r5_p && TARGET_16_BIT)
	{
	  snprintf (pattern, sizeof (pattern),
		    "b%ss38\t %%2, .LCB%%=\n\tj\t%%3\n%s.LCB%%=:",
		    cond_code, align);
	}
      else
	{
	  snprintf (pattern, sizeof (pattern),
		    "b%s%s\t%%1, %%2, .LCB%%=\n\tj\t%%3\n%s.LCB%%=:",
		    cond_code, suffix, align);
	}
    }
  else
    {
      cond_code = nds32_cond_code_str (code);
      if (r5_p && TARGET_16_BIT)
	{
	  /* b<cond>s38  $r1, .L0   */
	  snprintf (pattern, sizeof (pattern),
		    "b%ss38\t %%2, %%3", cond_code);
	}
      else
	{
	  /* b<cond><suffix>  $r0, $r1, .L0   */
	  snprintf (pattern, sizeof (pattern),
		    "b%s%s\t%%1, %%2, %%3", cond_code, suffix);
	}
    }

  output_asm_insn (pattern, operands);
}

static void
output_cond_branch_compare_zero (int code, const char *suffix,
				 bool long_jump_p, rtx *operands,
				 bool ta_implied_p)
{
  char pattern[256];
  const char *cond_code;
  bool align_p = NDS32_ALIGN_P ();
  const char *align = align_p ? "\t.align\t2\n" : "";
  if (long_jump_p)
    {
      int inverse_code = nds32_inverse_cond_code (code);
      cond_code = nds32_cond_code_str (inverse_code);

      if (ta_implied_p && TARGET_16_BIT)
	{
	  /*    b<cond>z<suffix>  .L0
	      =>
		b<inverse_cond>z<suffix>  .LCB0
		j  .L0
	      .LCB0:
	   */
	  snprintf (pattern, sizeof (pattern),
		    "b%sz%s\t.LCB%%=\n\tj\t%%2\n%s.LCB%%=:",
		    cond_code, suffix, align);
	}
      else
	{
	  /*      b<cond>z<suffix>  $r0, .L0
		=>
		  b<inverse_cond>z<suffix>  $r0, .LCB0
		  j  .L0
		.LCB0:
	   */
	  snprintf (pattern, sizeof (pattern),
		    "b%sz%s\t%%1, .LCB%%=\n\tj\t%%2\n%s.LCB%%=:",
		    cond_code, suffix, align);
	}
    }
  else
    {
      cond_code = nds32_cond_code_str (code);
      if (ta_implied_p && TARGET_16_BIT)
	{
	  /* b<cond>z<suffix>  .L0  */
	  snprintf (pattern, sizeof (pattern),
		    "b%sz%s\t%%2", cond_code, suffix);
	}
      else
	{
	  /* b<cond>z<suffix>  $r0, .L0  */
	  snprintf (pattern, sizeof (pattern),
		    "b%sz%s\t%%1, %%2", cond_code, suffix);
	}
    }

  output_asm_insn (pattern, operands);
}

static void
nds32_split_shiftrtdi3 (rtx dst, rtx src, rtx shiftamount, bool logic_shift_p)
{
  rtx src_high_part;
  rtx dst_high_part, dst_low_part;

  dst_high_part = nds32_di_high_part_subreg (dst);
  src_high_part = nds32_di_high_part_subreg (src);
  dst_low_part = nds32_di_low_part_subreg (dst);

  if (CONST_INT_P (shiftamount))
    {
      if (INTVAL (shiftamount) < 32)
	{
	  if (logic_shift_p)
	    {
	      emit_insn (gen_uwext (dst_low_part, src,
						  shiftamount));
	      emit_insn (gen_lshrsi3 (dst_high_part, src_high_part,
						     shiftamount));
	    }
	  else
	    {
	      emit_insn (gen_wext (dst_low_part, src,
						 shiftamount));
	      emit_insn (gen_ashrsi3 (dst_high_part, src_high_part,
						     shiftamount));
	    }
	}
      else
	{
	  rtx new_shift_amout = gen_int_mode(INTVAL (shiftamount) - 32, SImode);

	  if (logic_shift_p)
	    {
	      emit_insn (gen_lshrsi3 (dst_low_part, src_high_part,
						    new_shift_amout));
	      emit_move_insn (dst_high_part, const0_rtx);
	    }
	  else
	    {
	      emit_insn (gen_ashrsi3 (dst_low_part, src_high_part,
						    new_shift_amout));
	      emit_insn (gen_ashrsi3 (dst_high_part, src_high_part,
						     GEN_INT (31)));
	    }
	}
    }
  else
    {
      rtx dst_low_part_l32, dst_high_part_l32;
      rtx dst_low_part_g32, dst_high_part_g32;
      rtx new_shift_amout, select_reg;
      dst_low_part_l32 = gen_reg_rtx (SImode);
      dst_high_part_l32 = gen_reg_rtx (SImode);
      dst_low_part_g32 = gen_reg_rtx (SImode);
      dst_high_part_g32 = gen_reg_rtx (SImode);
      new_shift_amout = gen_reg_rtx (SImode);
      select_reg = gen_reg_rtx (SImode);

      emit_insn (gen_andsi3 (shiftamount, shiftamount, GEN_INT (0x3f)));

      if (logic_shift_p)
	{
	  /*
	     if (shiftamount < 32)
	       dst_low_part = wext (src, shiftamount)
	       dst_high_part = src_high_part >> shiftamount
	     else
	       dst_low_part = src_high_part >> (shiftamount & 0x1f)
	       dst_high_part = 0
	  */
	  emit_insn (gen_uwext (dst_low_part_l32, src, shiftamount));
	  emit_insn (gen_lshrsi3 (dst_high_part_l32, src_high_part,
						     shiftamount));

	  emit_insn (gen_andsi3 (new_shift_amout, shiftamount, GEN_INT (0x1f)));
	  emit_insn (gen_lshrsi3 (dst_low_part_g32, src_high_part,
						    new_shift_amout));
	  emit_move_insn (dst_high_part_g32, const0_rtx);
	}
      else
	{
	  /*
	     if (shiftamount < 32)
	       dst_low_part = wext (src, shiftamount)
	       dst_high_part = src_high_part >> shiftamount
	     else
	       dst_low_part = src_high_part >> (shiftamount & 0x1f)
	       # shift 31 for sign extend
	       dst_high_part = src_high_part >> 31
	  */
	  emit_insn (gen_wext (dst_low_part_l32, src, shiftamount));
	  emit_insn (gen_ashrsi3 (dst_high_part_l32, src_high_part,
						     shiftamount));

	  emit_insn (gen_andsi3 (new_shift_amout, shiftamount, GEN_INT (0x1f)));
	  emit_insn (gen_ashrsi3 (dst_low_part_g32, src_high_part,
						    new_shift_amout));
	  emit_insn (gen_ashrsi3 (dst_high_part_g32, src_high_part,
						     GEN_INT (31)));
	}

      emit_insn (gen_slt_compare (select_reg, shiftamount, GEN_INT (32)));

      emit_insn (gen_cmovnsi (dst_low_part, select_reg,
			      dst_low_part_l32, dst_low_part_g32));
      emit_insn (gen_cmovnsi (dst_high_part, select_reg,
			      dst_high_part_l32, dst_high_part_g32));
  }
}

/* ------------------------------------------------------------------------ */

/* Auxiliary function for expand RTL pattern.  */

enum nds32_expand_result_type
nds32_expand_cbranch (rtx *operands)
{
  rtx tmp_reg;
  enum rtx_code code;

  code = GET_CODE (operands[0]);

  /* If operands[2] is (const_int 0),
     we can use beqz,bnez,bgtz,bgez,bltz,or blez instructions.
     So we have gcc generate original template rtx.  */
  if (GET_CODE (operands[2]) == CONST_INT)
    if (INTVAL (operands[2]) == 0)
      if ((code != GTU)
	  && (code != GEU)
	  && (code != LTU)
	  && (code != LEU))
	return EXPAND_CREATE_TEMPLATE;

  /* For other comparison, NDS32 ISA only has slt (Set-on-Less-Than)
     behavior for the comparison, we might need to generate other
     rtx patterns to achieve same semantic.  */
  switch (code)
    {
    case GT:
    case GTU:
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  /* GT  reg_A, const_int  =>  !(LT  reg_A, const_int + 1) */
	  if (optimize_size || optimize == 0)
	    tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);
	  else
	    tmp_reg = gen_reg_rtx (SImode);

	  /* We want to plus 1 into the integer value
	     of operands[2] to create 'slt' instruction.
	     This caculation is performed on the host machine,
	     which may be 64-bit integer.
	     So the meaning of caculation result may be
	     different from the 32-bit nds32 target.

	     For example:
	       0x7fffffff + 0x1 -> 0x80000000,
	       this value is POSITIVE on 64-bit machine,
	       but the expected value on 32-bit nds32 target
	       should be NEGATIVE value.

	     Hence, instead of using GEN_INT(), we use gen_int_mode() to
	     explicitly create SImode constant rtx.  */
	  enum rtx_code cmp_code;

	  rtx plus1 = gen_int_mode (INTVAL (operands[2]) + 1, SImode);
	  if (satisfies_constraint_Is15 (plus1))
	    {
	      operands[2] = plus1;
	      cmp_code = EQ;
	      if (code == GT)
		{
		  /* GT, use slts instruction */
		  emit_insn (
		    gen_slts_compare (tmp_reg, operands[1], operands[2]));
		}
	      else
		{
		  /* GTU, use slt instruction */
		  emit_insn (
		    gen_slt_compare  (tmp_reg, operands[1], operands[2]));
		}
	    }
	  else
	    {
	      cmp_code = NE;
	      if (code == GT)
		{
		  /* GT, use slts instruction */
		  emit_insn (
		    gen_slts_compare (tmp_reg, operands[2], operands[1]));
		}
	      else
		{
		  /* GTU, use slt instruction */
		  emit_insn (
		    gen_slt_compare  (tmp_reg, operands[2], operands[1]));
		}
	    }

	  PUT_CODE (operands[0], cmp_code);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  return EXPAND_DONE;
	}
      else
	{
	  /* GT  reg_A, reg_B  =>  LT  reg_B, reg_A */
	  if (optimize_size || optimize == 0)
	    tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);
	  else
	    tmp_reg = gen_reg_rtx (SImode);

	  if (code == GT)
	    {
	      /* GT, use slts instruction */
	      emit_insn (gen_slts_compare (tmp_reg, operands[2], operands[1]));
	    }
	  else
	    {
	      /* GTU, use slt instruction */
	      emit_insn (gen_slt_compare  (tmp_reg, operands[2], operands[1]));
	    }

	  PUT_CODE (operands[0], NE);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  return EXPAND_DONE;
	}

    case GE:
    case GEU:
      /* GE  reg_A, reg_B      =>  !(LT  reg_A, reg_B) */
      /* GE  reg_A, const_int  =>  !(LT  reg_A, const_int) */
      if (optimize_size || optimize == 0)
	tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);
      else
	tmp_reg = gen_reg_rtx (SImode);

      if (code == GE)
	{
	  /* GE, use slts instruction */
	  emit_insn (gen_slts_compare (tmp_reg, operands[1], operands[2]));
	}
      else
	{
	  /* GEU, use slt instruction */
	  emit_insn (gen_slt_compare  (tmp_reg, operands[1], operands[2]));
	}

      PUT_CODE (operands[0], EQ);
      operands[1] = tmp_reg;
      operands[2] = const0_rtx;
      emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				 operands[2], operands[3]));

      return EXPAND_DONE;

    case LT:
    case LTU:
      /* LT  reg_A, reg_B      =>  LT  reg_A, reg_B */
      /* LT  reg_A, const_int  =>  LT  reg_A, const_int */
      if (optimize_size || optimize == 0)
	tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);
      else
	tmp_reg = gen_reg_rtx (SImode);

      if (code == LT)
	{
	  /* LT, use slts instruction */
	  emit_insn (gen_slts_compare (tmp_reg, operands[1], operands[2]));
	}
      else
	{
	  /* LTU, use slt instruction */
	  emit_insn (gen_slt_compare  (tmp_reg, operands[1], operands[2]));
	}

      PUT_CODE (operands[0], NE);
      operands[1] = tmp_reg;
      operands[2] = const0_rtx;
      emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				 operands[2], operands[3]));

      return EXPAND_DONE;

    case LE:
    case LEU:
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  /* LE  reg_A, const_int  =>  LT  reg_A, const_int + 1 */
	  if (optimize_size || optimize == 0)
	    tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);
	  else
	    tmp_reg = gen_reg_rtx (SImode);

	  enum rtx_code cmp_code;
	  /* Note that (le:SI X INT_MAX) is not the same as (lt:SI X INT_MIN).
	     We better have an assert here in case GCC does not properly
	     optimize it away.  The INT_MAX here is 0x7fffffff for target.  */
	  rtx plus1 = gen_int_mode (INTVAL (operands[2]) + 1, SImode);
	  if (satisfies_constraint_Is15 (plus1))
	    {
	      operands[2] = plus1;
	      cmp_code = NE;
	      if (code == LE)
		{
		  /* LE, use slts instruction */
		  emit_insn (
		    gen_slts_compare (tmp_reg, operands[1], operands[2]));
		}
	      else
		{
		  /* LEU, use slt instruction */
		  emit_insn (
		    gen_slt_compare  (tmp_reg, operands[1], operands[2]));
		}
	    }
	  else
	    {
	      cmp_code = EQ;
	      if (code == LE)
		{
		  /* LE, use slts instruction */
		  emit_insn (
		    gen_slts_compare (tmp_reg, operands[2], operands[1]));
		}
	      else
		{
		  /* LEU, use slt instruction */
		  emit_insn (
		    gen_slt_compare  (tmp_reg, operands[2], operands[1]));
		}
	    }

	  PUT_CODE (operands[0], cmp_code);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  return EXPAND_DONE;
	}
      else
	{
	  /* LE  reg_A, reg_B  =>  !(LT  reg_B, reg_A) */
	  if (optimize_size || optimize == 0)
	    tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);
	  else
	    tmp_reg = gen_reg_rtx (SImode);

	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (tmp_reg, operands[2], operands[1]));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (tmp_reg, operands[2], operands[1]));
	    }

	  PUT_CODE (operands[0], EQ);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  return EXPAND_DONE;
	}

    case EQ:
    case NE:
      /* NDS32 ISA has various form for eq/ne behavior no matter
	 what kind of the operand is.
	 So just generate original template rtx.  */

      /* Put operands[2] into register if operands[2] is a large
	 const_int or ISAv2.  */
      if (GET_CODE (operands[2]) == CONST_INT
	  && (!satisfies_constraint_Is11 (operands[2])
	      || TARGET_ISA_V2))
	operands[2] = force_reg (SImode, operands[2]);

      return EXPAND_CREATE_TEMPLATE;

    default:
      return EXPAND_FAIL;
    }
}

enum nds32_expand_result_type
nds32_expand_cstore (rtx *operands)
{
  rtx tmp_reg;
  enum rtx_code code;

  code = GET_CODE (operands[1]);

  switch (code)
    {
    case EQ:
    case NE:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A == const_int_B)
	     --> xori reg_C, reg_A, const_int_B
		 slti reg_R, reg_C, const_int_1
	     reg_R = (reg_A != const_int_B)
	     --> xori reg_C, reg_A, const_int_B
		 slti reg_R, const_int0, reg_C */
	  tmp_reg = gen_reg_rtx (SImode);

	  /* If the integer value is not in the range of imm15s,
	     we need to force register first because our addsi3 pattern
	     only accept nds32_rimm15s_operand predicate.  */
	  rtx new_imm = gen_int_mode (-INTVAL (operands[3]), SImode);
	  if (satisfies_constraint_Is15 (new_imm))
	    emit_insn (gen_addsi3 (tmp_reg, operands[2], new_imm));
	  else
	    {
	      if (!(satisfies_constraint_Iu15 (operands[3])
		    || (TARGET_EXT_PERF
			&& satisfies_constraint_It15 (operands[3]))))
		operands[3] = force_reg (SImode, operands[3]);
	      emit_insn (gen_xorsi3 (tmp_reg, operands[2], operands[3]));
	    }

	  if (code == EQ)
	    emit_insn (gen_slt_eq0 (operands[0], tmp_reg));
	  else
	    emit_insn (gen_slt_compare (operands[0], const0_rtx, tmp_reg));

	  return EXPAND_DONE;
	}
      else
	{
	  /* reg_R = (reg_A == reg_B)
	     --> xor  reg_C, reg_A, reg_B
		 slti reg_R, reg_C, const_int_1
	     reg_R = (reg_A != reg_B)
	     --> xor  reg_C, reg_A, reg_B
		 slti reg_R, const_int0, reg_C */
	  tmp_reg = gen_reg_rtx (SImode);
	  emit_insn (gen_xorsi3 (tmp_reg, operands[2], operands[3]));
	  if (code == EQ)
	    emit_insn (gen_slt_eq0 (operands[0], tmp_reg));
	  else
	    emit_insn (gen_slt_compare (operands[0], const0_rtx, tmp_reg));

	  return EXPAND_DONE;
	}
    case GT:
    case GTU:
      /* reg_R = (reg_A > reg_B)       --> slt reg_R, reg_B, reg_A */
      /* reg_R = (reg_A > const_int_B) --> slt reg_R, const_int_B, reg_A */
      if (code == GT)
	{
	  /* GT, use slts instruction */
	  emit_insn (gen_slts_compare (operands[0], operands[3], operands[2]));
	}
      else
	{
	  /* GTU, use slt instruction */
	  emit_insn (gen_slt_compare  (operands[0], operands[3], operands[2]));
	}

      return EXPAND_DONE;

    case GE:
    case GEU:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A >= const_int_B)
	     --> movi reg_C, const_int_B - 1
		 slt  reg_R, reg_C, reg_A */
	  tmp_reg = gen_reg_rtx (SImode);

	  emit_insn (gen_movsi (tmp_reg,
				gen_int_mode (INTVAL (operands[3]) - 1,
					      SImode)));
	  if (code == GE)
	    {
	      /* GE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0], tmp_reg, operands[2]));
	    }
	  else
	    {
	      /* GEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0], tmp_reg, operands[2]));
	    }

	  return EXPAND_DONE;
	}
      else
	{
	  /* reg_R = (reg_A >= reg_B)
	     --> slt  reg_R, reg_A, reg_B
		 xori reg_R, reg_R, const_int_1 */
	  if (code == GE)
	    {
	      /* GE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0],
					   operands[2], operands[3]));
	    }
	  else
	    {
	      /* GEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0],
					   operands[2], operands[3]));
	    }

	  /* perform 'not' behavior */
	  emit_insn (gen_xorsi3 (operands[0], operands[0], const1_rtx));

	  return EXPAND_DONE;
	}

    case LT:
    case LTU:
      /* reg_R = (reg_A < reg_B)       --> slt reg_R, reg_A, reg_B */
      /* reg_R = (reg_A < const_int_B) --> slt reg_R, reg_A, const_int_B */
      if (code == LT)
	{
	  /* LT, use slts instruction */
	  emit_insn (gen_slts_compare (operands[0], operands[2], operands[3]));
	}
      else
	{
	  /* LTU, use slt instruction */
	  emit_insn (gen_slt_compare  (operands[0], operands[2], operands[3]));
	}

      return EXPAND_DONE;

    case LE:
    case LEU:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A <= const_int_B)
	     --> movi reg_C, const_int_B + 1
		 slt  reg_R, reg_A, reg_C */
	  tmp_reg = gen_reg_rtx (SImode);

	  emit_insn (gen_movsi (tmp_reg,
				gen_int_mode (INTVAL (operands[3]) + 1,
						      SImode)));
	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0], operands[2], tmp_reg));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0], operands[2], tmp_reg));
	    }

	  return EXPAND_DONE;
	}
      else
	{
	  /* reg_R = (reg_A <= reg_B) --> slt  reg_R, reg_B, reg_A
					  xori reg_R, reg_R, const_int_1 */
	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0],
					   operands[3], operands[2]));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0],
					   operands[3], operands[2]));
	    }

	  /* perform 'not' behavior */
	  emit_insn (gen_xorsi3 (operands[0], operands[0], const1_rtx));

	  return EXPAND_DONE;
	}


    default:
      gcc_unreachable ();
    }
}

void
nds32_expand_float_cbranch (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[0]);
  enum rtx_code new_code = code;
  rtx cmp_op0 = operands[1];
  rtx cmp_op1 = operands[2];
  rtx tmp_reg;
  rtx tmp;

  int reverse = 0;

  /* Main Goal: Use compare instruction + branch instruction.

     For example:
     GT, GE: swap condition and swap operands and generate
     compare instruction(LT, LE) + branch not equal instruction.

     UNORDERED, LT, LE, EQ: no need to change and generate
     compare instruction(UNORDERED, LT, LE, EQ) + branch not equal instruction.

     ORDERED, NE: reverse condition and generate
     compare instruction(EQ) + branch equal instruction. */

  switch (code)
    {
    case GT:
    case GE:
      tmp = cmp_op0;
      cmp_op0 = cmp_op1;
      cmp_op1 = tmp;
      new_code = swap_condition (new_code);
      break;
    case UNORDERED:
    case LT:
    case LE:
    case EQ:
      break;
    case ORDERED:
    case NE:
      new_code = reverse_condition (new_code);
      reverse = 1;
      break;
    case UNGT:
    case UNGE:
      new_code = reverse_condition_maybe_unordered (new_code);
      reverse = 1;
      break;
    case UNLT:
    case UNLE:
      new_code = reverse_condition_maybe_unordered (new_code);
      tmp = cmp_op0;
      cmp_op0 = cmp_op1;
      cmp_op1 = tmp;
      new_code = swap_condition (new_code);
      reverse = 1;
      break;
    default:
      return;
    }

  tmp_reg = gen_reg_rtx (SImode);
  emit_insn (gen_rtx_SET (tmp_reg,
			  gen_rtx_fmt_ee (new_code, SImode,
					  cmp_op0, cmp_op1)));

  PUT_CODE (operands[0], reverse ? EQ : NE);
  emit_insn (gen_cbranchsi4 (operands[0], tmp_reg,
			     const0_rtx, operands[3]));
}

void
nds32_expand_float_cstore (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[1]);
  enum rtx_code new_code = code;
  machine_mode mode = GET_MODE (operands[2]);

  rtx cmp_op0 = operands[2];
  rtx cmp_op1 = operands[3];
  rtx tmp;

  /* Main Goal: Use compare instruction to store value.

     For example:
     GT, GE: swap condition and swap operands.
       reg_R = (reg_A >  reg_B) --> fcmplt reg_R, reg_B, reg_A
       reg_R = (reg_A >= reg_B) --> fcmple reg_R, reg_B, reg_A

     LT, LE, EQ: no need to change, it is already LT, LE, EQ.
       reg_R = (reg_A <  reg_B) --> fcmplt reg_R, reg_A, reg_B
       reg_R = (reg_A <= reg_B) --> fcmple reg_R, reg_A, reg_B
       reg_R = (reg_A == reg_B) --> fcmpeq reg_R, reg_A, reg_B

     ORDERED: reverse condition and using xor insturction to achieve 'ORDERED'.
       reg_R = (reg_A != reg_B) --> fcmpun reg_R, reg_A, reg_B
				       xor reg_R, reg_R, const1_rtx

     NE: reverse condition and using xor insturction to achieve 'NE'.
       reg_R = (reg_A != reg_B) --> fcmpeq reg_R, reg_A, reg_B
				       xor reg_R, reg_R, const1_rtx */
  switch (code)
    {
    case GT:
    case GE:
      tmp = cmp_op0;
      cmp_op0 = cmp_op1;
      cmp_op1 =tmp;
      new_code = swap_condition (new_code);
      break;
    case UNORDERED:
    case LT:
    case LE:
    case EQ:
      break;
    case ORDERED:
      if (mode == SFmode)
	emit_insn (gen_cmpsf_un (operands[0], cmp_op0, cmp_op1));
      else
	emit_insn (gen_cmpdf_un (operands[0], cmp_op0, cmp_op1));

      emit_insn (gen_xorsi3 (operands[0], operands[0], const1_rtx));
      return;
    case NE:
      if (mode == SFmode)
	emit_insn (gen_cmpsf_eq (operands[0], cmp_op0, cmp_op1));
      else
	emit_insn (gen_cmpdf_eq (operands[0], cmp_op0, cmp_op1));

      emit_insn (gen_xorsi3 (operands[0], operands[0], const1_rtx));
      return;
    default:
      return;
    }

  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_fmt_ee (new_code, SImode,
					  cmp_op0, cmp_op1)));
}

enum nds32_expand_result_type
nds32_expand_movcc (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[1]);
  enum rtx_code new_code = code;
  machine_mode cmp0_mode = GET_MODE (XEXP (operands[1], 0));
  rtx cmp_op0 = XEXP (operands[1], 0);
  rtx cmp_op1 = XEXP (operands[1], 1);
  rtx tmp;

  if ((GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)
      && XEXP (operands[1], 1) == const0_rtx)
    {
      /* If the operands[1] rtx is already (eq X 0) or (ne X 0),
	 we have gcc generate original template rtx.  */
      return EXPAND_CREATE_TEMPLATE;
    }
  else if ((TARGET_FPU_SINGLE && cmp0_mode == SFmode)
	   || (TARGET_FPU_DOUBLE && cmp0_mode == DFmode))
    {
      nds32_expand_float_movcc (operands);
    }
  else
    {
      /* Since there is only 'slt'(Set when Less Than) instruction for
	 comparison in Andes ISA, the major strategy we use here is to
	 convert conditional move into 'LT + EQ' or 'LT + NE' rtx combination.
	 We design constraints properly so that the reload phase will assist
	 to make one source operand to use same register as result operand.
	 Then we can use cmovz/cmovn to catch the other source operand
	 which has different register.  */
      int reverse = 0;

      /* Main Goal: Use 'LT + EQ' or 'LT + NE' to target "then" part
	 Strategy : Reverse condition and swap comparison operands

	 For example:

	     a <= b ? P : Q   (LE or LEU)
	 --> a >  b ? Q : P   (reverse condition)
	 --> b <  a ? Q : P   (swap comparison operands to achieve 'LT/LTU')

	     a >= b ? P : Q   (GE or GEU)
	 --> a <  b ? Q : P   (reverse condition to achieve 'LT/LTU')

	     a <  b ? P : Q   (LT or LTU)
	 --> (NO NEED TO CHANGE, it is already 'LT/LTU')

	     a >  b ? P : Q   (GT or GTU)
	 --> b <  a ? P : Q   (swap comparison operands to achieve 'LT/LTU') */
      switch (code)
	{
	case GE: case GEU: case LE: case LEU:
	  new_code = reverse_condition (code);
	  reverse = 1;
	  break;
	case EQ:
	case NE:
	  /* no need to reverse condition */
	  break;
	default:
	  return EXPAND_FAIL;
	}

      /* For '>' comparison operator, we swap operands
	 so that we can have 'LT/LTU' operator.  */
      if (new_code == GT || new_code == GTU)
	{
	  tmp     = cmp_op0;
	  cmp_op0 = cmp_op1;
	  cmp_op1 = tmp;

	  new_code = swap_condition (new_code);
	}

      /* Use a temporary register to store slt/slts result.  */
      tmp = gen_reg_rtx (SImode);

      if (new_code == EQ || new_code == NE)
	{
	  emit_insn (gen_xorsi3 (tmp, cmp_op0, cmp_op1));
	  /* tmp == 0 if cmp_op0 == cmp_op1.  */
	  operands[1] = gen_rtx_fmt_ee (new_code, VOIDmode, tmp, const0_rtx);
	}
      else
	{
	  /* This emit_insn will create corresponding 'slt/slts'
	      insturction.  */
	  if (new_code == LT)
	    emit_insn (gen_slts_compare (tmp, cmp_op0, cmp_op1));
	  else if (new_code == LTU)
	    emit_insn (gen_slt_compare (tmp, cmp_op0, cmp_op1));
	  else
	    gcc_unreachable ();

	  /* Change comparison semantic into (eq X 0) or (ne X 0) behavior
	     so that cmovz or cmovn will be matched later.

	     For reverse condition cases, we want to create a semantic that:
	       (eq X 0) --> pick up "else" part
	     For normal cases, we want to create a semantic that:
	       (ne X 0) --> pick up "then" part

	     Later we will have cmovz/cmovn instruction pattern to
	     match corresponding behavior and output instruction.  */
	  operands[1] = gen_rtx_fmt_ee (reverse ? EQ : NE,
					VOIDmode, tmp, const0_rtx);
	}
    }
  return EXPAND_CREATE_TEMPLATE;
}

void
nds32_expand_float_movcc (rtx *operands)
{
  if ((GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)
      && GET_MODE (XEXP (operands[1], 0)) == SImode
      && XEXP (operands[1], 1) == const0_rtx)
    {
      /* If the operands[1] rtx is already (eq X 0) or (ne X 0),
	 we have gcc generate original template rtx.  */
      return;
    }
  else
    {
      enum rtx_code code = GET_CODE (operands[1]);
      enum rtx_code new_code = code;
      machine_mode cmp0_mode = GET_MODE (XEXP (operands[1], 0));
      machine_mode cmp1_mode = GET_MODE (XEXP (operands[1], 1));
      rtx cmp_op0 = XEXP (operands[1], 0);
      rtx cmp_op1 = XEXP (operands[1], 1);
      rtx tmp;

      /* Compare instruction Operations: (cmp_op0 condition cmp_op1) ? 1 : 0,
	 when result is 1, and 'reverse' be set 1 for fcmovzs instructuin. */
      int reverse = 0;

      /* Main Goal: Use cmpare instruction + conditional move instruction.
	 Strategy : swap condition and swap comparison operands.

	 For example:
	     a > b ? P : Q   (GT)
	 --> a < b ? Q : P   (swap condition)
	 --> b < a ? Q : P   (swap comparison operands to achieve 'GT')

	     a >= b ? P : Q  (GE)
	 --> a <= b ? Q : P  (swap condition)
	 --> b <= a ? Q : P  (swap comparison operands to achieve 'GE')

	     a <  b ? P : Q  (LT)
	 --> (NO NEED TO CHANGE, it is already 'LT')

	     a >= b ? P : Q  (LE)
	 --> (NO NEED TO CHANGE, it is already 'LE')

	     a == b ? P : Q  (EQ)
	 --> (NO NEED TO CHANGE, it is already 'EQ') */

      switch (code)
	{
	case GT:
	case GE:
	  tmp = cmp_op0;
	  cmp_op0 = cmp_op1;
	  cmp_op1 =tmp;
	  new_code = swap_condition (new_code);
	  break;
	case UNORDERED:
	case LT:
	case LE:
	case EQ:
	  break;
	case ORDERED:
	case NE:
	  reverse = 1;
	  new_code = reverse_condition (new_code);
	  break;
	case UNGT:
	case UNGE:
	  new_code = reverse_condition_maybe_unordered (new_code);
	  reverse = 1;
	  break;
	case UNLT:
	case UNLE:
	  new_code = reverse_condition_maybe_unordered (new_code);
	  tmp = cmp_op0;
	  cmp_op0 = cmp_op1;
	  cmp_op1 = tmp;
	  new_code = swap_condition (new_code);
	  reverse = 1;
	  break;
	default:
	  return;
	}

      /* Use a temporary register to store fcmpxxs result.  */
      tmp = gen_reg_rtx (SImode);

      /* Create float compare instruction for SFmode and DFmode,
	 other MODE using cstoresi create compare instruction. */
      if ((cmp0_mode == DFmode || cmp0_mode == SFmode)
	  && (cmp1_mode == DFmode || cmp1_mode == SFmode))
	{
	  /* This emit_insn create corresponding float compare instruction */
	  emit_insn (gen_rtx_SET (tmp,
				  gen_rtx_fmt_ee (new_code, SImode,
						  cmp_op0, cmp_op1)));
	}
      else
	{
	  /* This emit_insn using cstoresi create corresponding
	     compare instruction */
	  PUT_CODE (operands[1], new_code);
	  emit_insn (gen_cstoresi4 (tmp, operands[1],
				    cmp_op0, cmp_op1));
	}
      /* operands[1] crete corresponding condition move instruction
	 for fcmovzs and fcmovns.  */
      operands[1] = gen_rtx_fmt_ee (reverse ? EQ : NE,
				    VOIDmode, tmp, const0_rtx);
    }
}

void
nds32_emit_push_fpr_callee_saved (int base_offset)
{
  rtx fpu_insn;
  rtx reg, mem;
  unsigned int regno = cfun->machine->callee_saved_first_fpr_regno;
  unsigned int last_fpr = cfun->machine->callee_saved_last_fpr_regno;

  while (regno <= last_fpr)
    {
      /* Handling two registers, using fsdi instruction.  */
      reg = gen_rtx_REG (DFmode, regno);
      mem = gen_frame_mem (DFmode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  base_offset));
      base_offset += 8;
      regno += 2;
      fpu_insn = emit_move_insn (mem, reg);
      RTX_FRAME_RELATED_P (fpu_insn) = 1;
    }
}

void
nds32_emit_pop_fpr_callee_saved (int gpr_padding_size)
{
  rtx fpu_insn;
  rtx reg, mem, addr;
  rtx dwarf, adjust_sp_rtx;
  unsigned int regno = cfun->machine->callee_saved_first_fpr_regno;
  unsigned int last_fpr = cfun->machine->callee_saved_last_fpr_regno;
  int padding = 0;

  while (regno <= last_fpr)
    {
      /* Handling two registers, using fldi.bi instruction.  */
      if ((regno + 1) >= last_fpr)
	padding = gpr_padding_size;

      reg = gen_rtx_REG (DFmode, (regno));
      addr = gen_rtx_POST_MODIFY (Pmode, stack_pointer_rtx,
				  gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						GEN_INT (8 + padding)));
      mem = gen_frame_mem (DFmode, addr);
      regno += 2;
      fpu_insn = emit_move_insn (reg, mem);

      adjust_sp_rtx =
	gen_rtx_SET (stack_pointer_rtx,
		     plus_constant (Pmode, stack_pointer_rtx,
				    8 + padding));

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, NULL_RTX);
      /* Tell gcc we adjust SP in this insn.  */
      dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, copy_rtx (adjust_sp_rtx),
			      dwarf);
      RTX_FRAME_RELATED_P (fpu_insn) = 1;
      REG_NOTES (fpu_insn) = dwarf;
    }
}

void
nds32_emit_v3pop_fpr_callee_saved (int base)
{
  int fpu_base_addr = base;
  int regno;
  rtx fpu_insn;
  rtx reg, mem;
  rtx dwarf;

  regno = cfun->machine->callee_saved_first_fpr_regno;
  while (regno <= cfun->machine->callee_saved_last_fpr_regno)
    {
      /* Handling two registers, using fldi instruction.  */
      reg = gen_rtx_REG (DFmode, regno);
      mem = gen_frame_mem (DFmode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  fpu_base_addr));
      fpu_base_addr += 8;
      regno += 2;
      fpu_insn = emit_move_insn (reg, mem);
      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, NULL_RTX);
      RTX_FRAME_RELATED_P (fpu_insn) = 1;
      REG_NOTES (fpu_insn) = dwarf;
    }
}

enum nds32_expand_result_type
nds32_expand_extv (rtx *operands)
{
  gcc_assert (CONST_INT_P (operands[2]) && CONST_INT_P (operands[3]));
  HOST_WIDE_INT width = INTVAL (operands[2]);
  HOST_WIDE_INT bitpos = INTVAL (operands[3]);
  rtx dst = operands[0];
  rtx src = operands[1];

  if (MEM_P (src)
      && width == 32
      && (bitpos % BITS_PER_UNIT)  == 0
      && GET_MODE_BITSIZE (GET_MODE (dst)) == width)
    {
      rtx newmem = adjust_address (src, GET_MODE (dst),
				   bitpos / BITS_PER_UNIT);

      rtx base_addr = force_reg (Pmode, XEXP (newmem, 0));

      emit_insn (gen_unaligned_loadsi (dst, base_addr));

      return EXPAND_DONE;
    }
  return EXPAND_FAIL;
}

enum nds32_expand_result_type
nds32_expand_insv (rtx *operands)
{
  gcc_assert (CONST_INT_P (operands[1]) && CONST_INT_P (operands[2]));
  HOST_WIDE_INT width = INTVAL (operands[1]);
  HOST_WIDE_INT bitpos = INTVAL (operands[2]);
  rtx dst = operands[0];
  rtx src = operands[3];

  if (MEM_P (dst)
      && width == 32
      && (bitpos % BITS_PER_UNIT)  == 0
      && GET_MODE_BITSIZE (GET_MODE (src)) == width)
    {
      rtx newmem = adjust_address (dst, GET_MODE (src),
				      bitpos / BITS_PER_UNIT);

      rtx base_addr = force_reg (Pmode, XEXP (newmem, 0));

      emit_insn (gen_unaligned_storesi (base_addr, src));

      return EXPAND_DONE;
    }
  return EXPAND_FAIL;
}

/* ------------------------------------------------------------------------ */

/* Function to generate PC relative jump table.
   Refer to nds32.md for more details.

   The following is the sample for the case that diff value
   can be presented in '.short' size.

     addi    $r1, $r1, -(case_lower_bound)
     slti    $ta, $r1, (case_number)
     beqz    $ta, .L_skip_label

     la      $ta, .L35             ! get jump table address
     lh      $r1, [$ta + $r1 << 1] ! load symbol diff from jump table entry
     addi    $ta, $r1, $ta
     jr5     $ta

     ! jump table entry
   L35:
     .short  .L25-.L35
     .short  .L26-.L35
     .short  .L27-.L35
     .short  .L28-.L35
     .short  .L29-.L35
     .short  .L30-.L35
     .short  .L31-.L35
     .short  .L32-.L35
     .short  .L33-.L35
     .short  .L34-.L35 */
const char *
nds32_output_casesi_pc_relative (rtx *operands)
{
  machine_mode mode;
  rtx diff_vec;

  diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[1])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  /* Step C: "t <-- operands[1]".  */
  if (flag_pic)
    {
      output_asm_insn ("sethi\t$ta, hi20(%l1@GOTOFF)", operands);
      output_asm_insn ("ori\t$ta, $ta, lo12(%l1@GOTOFF)", operands);
      output_asm_insn ("add\t$ta, $ta, $gp", operands);
    }
  else
    output_asm_insn ("la\t$ta, %l1", operands);

  /* Get the mode of each element in the difference vector.  */
  mode = GET_MODE (diff_vec);

  /* Step D: "z <-- (mem (plus (operands[0] << m) t))",
     where m is 0, 1, or 2 to load address-diff value from table.  */
  switch (mode)
    {
    case E_QImode:
      output_asm_insn ("lb\t%2, [$ta + %0 << 0]", operands);
      break;
    case E_HImode:
      output_asm_insn ("lh\t%2, [$ta + %0 << 1]", operands);
      break;
    case E_SImode:
      output_asm_insn ("lw\t%2, [$ta + %0 << 2]", operands);
      break;
    default:
      gcc_unreachable ();
    }

  /* Step E: "t <-- z + t".
     Add table label_ref with address-diff value to
     obtain target case address.  */
  output_asm_insn ("add\t$ta, %2, $ta", operands);

  /* Step F: jump to target with register t.  */
  if (TARGET_16_BIT)
    return "jr5\t$ta";
  else
    return "jr\t$ta";
}

/* Function to generate normal jump table.  */
const char *
nds32_output_casesi (rtx *operands)
{
  /* Step C: "t <-- operands[1]".  */
  if (flag_pic)
    {
      output_asm_insn ("sethi\t$ta, hi20(%l1@GOTOFF)", operands);
      output_asm_insn ("ori\t$ta, $ta, lo12(%l1@GOTOFF)", operands);
      output_asm_insn ("add\t$ta, $ta, $gp", operands);
    }
  else
    output_asm_insn ("la\t$ta, %l1", operands);

  /* Step D: "z <-- (mem (plus (operands[0] << 2) t))".  */
  output_asm_insn ("lw\t%2, [$ta + %0 << 2]", operands);

  /* No need to perform Step E, which is only used for
     pc relative jump table.  */

  /* Step F: jump to target with register z.  */
  if (TARGET_16_BIT)
    return "jr5\t%2";
  else
    return "jr\t%2";
}

/* Function to return memory format.  */
enum nds32_16bit_address_type
nds32_mem_format (rtx op)
{
  machine_mode mode_test;
  int val;
  int regno;

  if (!TARGET_16_BIT)
    return ADDRESS_NOT_16BIT_FORMAT;

  mode_test = GET_MODE (op);

  op = XEXP (op, 0);

  /* 45 format.  */
  if (GET_CODE (op) == REG
      && ((mode_test == SImode) || (mode_test == SFmode)))
    return ADDRESS_REG;

  /* 333 format for QI/HImode.  */
  if (GET_CODE (op) == REG && (REGNO (op) < R8_REGNUM))
    return ADDRESS_LO_REG_IMM3U;

  /* post_inc 333 format.  */
  if ((GET_CODE (op) == POST_INC)
      && ((mode_test == SImode) || (mode_test == SFmode)))
    {
      regno = REGNO(XEXP (op, 0));

      if (regno < 8)
	return ADDRESS_POST_INC_LO_REG_IMM3U;
    }

  /* post_inc 333 format.  */
  if ((GET_CODE (op) == POST_MODIFY)
      && ((mode_test == SImode) || (mode_test == SFmode))
      && (REG_P (XEXP (XEXP (op, 1), 0)))
      && (CONST_INT_P (XEXP (XEXP (op, 1), 1))))
    {
      regno = REGNO (XEXP (XEXP (op, 1), 0));
      val = INTVAL (XEXP (XEXP (op, 1), 1));
      if (regno < 8 && val > 0 && val < 32)
	return ADDRESS_POST_MODIFY_LO_REG_IMM3U;
    }

  if ((GET_CODE (op) == PLUS)
      && (GET_CODE (XEXP (op, 0)) == REG)
      && (GET_CODE (XEXP (op, 1)) == CONST_INT))
    {
      val = INTVAL (XEXP (op, 1));

      regno = REGNO(XEXP (op, 0));

      if (regno > 8
	  && regno != SP_REGNUM
	  && regno != FP_REGNUM)
	return ADDRESS_NOT_16BIT_FORMAT;

      switch (mode_test)
	{
	case E_QImode:
	  /* 333 format.  */
	  if (val >= 0 && val < 8 && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	case E_HImode:
	  /* 333 format.  */
	  if (val >= 0 && val < 16 && (val % 2 == 0) && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	case E_SImode:
	case E_SFmode:
	case E_DFmode:
	  /* r8 imply fe format.  */
	  if ((regno == 8) &&
	      (val >= -128 && val <= -4 && (val % 4 == 0)))
	    return ADDRESS_R8_IMM7U;
	  /* fp imply 37 format.  */
	  if ((regno == FP_REGNUM) &&
	      (val >= 0 && val < 512 && (val % 4 == 0)))
	    return ADDRESS_FP_IMM7U;
	  /* sp imply 37 format.  */
	  else if ((regno == SP_REGNUM) &&
		   (val >= 0 && val < 512 && (val % 4 == 0)))
	    return ADDRESS_SP_IMM7U;
	  /* 333 format.  */
	  else if (val >= 0 && val < 32 && (val % 4 == 0) && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	default:
	  break;
	}
    }

  return ADDRESS_NOT_16BIT_FORMAT;
}

/* Output 16-bit store.  */
const char *
nds32_output_16bit_store (rtx *operands, int byte)
{
  char pattern[100];
  char size;
  rtx code = XEXP (operands[0], 0);

  size = nds32_byte_to_size (byte);

  switch (nds32_mem_format (operands[0]))
    {
    case ADDRESS_REG:
      operands[0] = code;
      output_asm_insn ("swi450\t%1, [%0]", operands);
      break;
    case ADDRESS_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "s%ci333\t%%1, %%0", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_INC_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "swi333.bi\t%%1, %%0, 4");
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_MODIFY_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "swi333.bi\t%%1, %%0");
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_FP_IMM7U:
      output_asm_insn ("swi37\t%1, %0", operands);
      break;
    case ADDRESS_SP_IMM7U:
      /* Get immediate value and set back to operands[1].  */
      operands[0] = XEXP (code, 1);
      output_asm_insn ("swi37.sp\t%1, [ + (%0)]", operands);
      break;
    default:
      break;
    }

  return "";
}

/* Output 16-bit load.  */
const char *
nds32_output_16bit_load (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (nds32_mem_format (operands[1]))
    {
    case ADDRESS_REG:
      operands[1] = code;
      output_asm_insn ("lwi450\t%0, [%1]", operands);
      break;
    case ADDRESS_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "l%ci333\t%%0, %%1", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_INC_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "lwi333.bi\t%%0, %%1, 4");
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_MODIFY_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "lwi333.bi\t%%0, %%1");
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_R8_IMM7U:
      output_asm_insn ("lwi45.fe\t%0, %e1", operands);
      break;
    case ADDRESS_FP_IMM7U:
      output_asm_insn ("lwi37\t%0, %1", operands);
      break;
    case ADDRESS_SP_IMM7U:
      /* Get immediate value and set back to operands[0].  */
      operands[1] = XEXP (code, 1);
      output_asm_insn ("lwi37.sp\t%0, [ + (%1)]", operands);
      break;
    default:
      break;
    }

  return "";
}

/* Output 32-bit store.  */
const char *
nds32_output_32bit_store (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code = XEXP (operands[0], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "sbi / shi / swi" */
      snprintf (pattern, sizeof (pattern), "s%ci\t%%1, %%0", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "sbi.gp / shi.gp / swi.gp" */
      operands[0] = XEXP (operands[0], 0);
      snprintf (pattern, sizeof (pattern), "s%ci.gp\t%%1, [ + %%0]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "sbi.bi / shi.bi / swi.bi" */
      snprintf (pattern, sizeof (pattern),
		"s%ci.bi\t%%1, %%0, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "sbi.bi / shi.bi / swi.bi" */
      snprintf (pattern, sizeof (pattern),
		"s%ci.bi\t%%1, %%0, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "sb.bi/ sh.bi / sw.bi" */
	  snprintf (pattern, sizeof (pattern), "s%c.bi\t%%1, %%0", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "sbi.bi/ shi.bi / swi.bi" */
	  snprintf (pattern, sizeof (pattern), "s%ci.bi\t%%1, %%0", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     => access location by adding two registers,
	     use "sb / sh / sw" */
	  snprintf (pattern, sizeof (pattern), "s%c\t%%1, %%0", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "sbi / shi / swi" */
	  snprintf (pattern, sizeof (pattern), "s%ci\t%%1, %%0", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[0] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"s%ci\t%%1, [%%0 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Output 32-bit load.  */
const char *
nds32_output_32bit_load (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code;

  code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "lbi / lhi / lwi" */
      snprintf (pattern, sizeof (pattern), "l%ci\t%%0, %%1", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "lbi.gp / lhi.gp / lwi.gp" */
      operands[1] = XEXP (operands[1], 0);
      snprintf (pattern, sizeof (pattern), "l%ci.gp\t%%0, [ + %%1]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "lbi.bi / lhi.bi / lwi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%ci.bi\t%%0, %%1, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "lbi.bi / lhi.bi / lwi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%ci.bi\t%%0, %%1, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "lb.bi/ lh.bi / lw.bi" */
	  snprintf (pattern, sizeof (pattern), "l%c.bi\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "lbi.bi/ lhi.bi / lwi.bi" */
	  snprintf (pattern, sizeof (pattern), "l%ci.bi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     use "lb / lh / lw" */
	  snprintf (pattern, sizeof (pattern), "l%c\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "lbi / lhi / lwi" */
	  snprintf (pattern, sizeof (pattern), "l%ci\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[1] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"l%ci\t%%0, [%%1 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Output 32-bit load with signed extension.  */
const char *
nds32_output_32bit_load_s (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code;

  code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "lbsi / lhsi" */
      snprintf (pattern, sizeof (pattern), "l%csi\t%%0, %%1", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "lbsi.gp / lhsi.gp" */
      operands[1] = XEXP (operands[1], 0);
      snprintf (pattern, sizeof (pattern), "l%csi.gp\t%%0, [ + %%1]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "lbsi.bi / lhsi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%csi.bi\t%%0, %%1, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "lbsi.bi / lhsi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%csi.bi\t%%0, %%1, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "lbs.bi/ lhs.bi" */
	  snprintf (pattern, sizeof (pattern), "l%cs.bi\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "lbsi.bi/ lhsi.bi" */
	  snprintf (pattern, sizeof (pattern), "l%csi.bi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     use "lbs / lhs" */
	  snprintf (pattern, sizeof (pattern), "l%cs\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "lbsi / lhsi" */
	  snprintf (pattern, sizeof (pattern), "l%csi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[1] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"l%csi\t%%0, [%%1 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output stack push operation.
   We need to deal with normal stack push multiple or stack v3push.  */
const char *
nds32_output_stack_push (rtx par_rtx)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[3];
  /* Pick up varargs first regno and last regno for further use.  */
  int rb_va_args = cfun->machine->va_args_first_regno;
  int re_va_args = cfun->machine->va_args_last_regno;
  int last_argument_regno = NDS32_FIRST_GPR_REGNUM
			    + NDS32_MAX_GPR_REGS_FOR_ARGS
			    - 1;
  /* Pick up first and last eh data regno for further use.  */
  int rb_eh_data = cfun->machine->eh_return_data_first_regno;
  int re_eh_data = cfun->machine->eh_return_data_last_regno;
  int first_eh_data_regno = EH_RETURN_DATA_REGNO (0);
  /* Pick up callee-saved first regno and last regno for further use.  */
  int rb_callee_saved = cfun->machine->callee_saved_first_gpr_regno;
  int re_callee_saved = cfun->machine->callee_saved_last_gpr_regno;

  /* First we need to check if we are pushing argument registers not used
     for the named arguments.  If so, we have to create 'smw.adm' (push.s)
     instruction.  */
  if (reg_mentioned_p (gen_rtx_REG (SImode, last_argument_regno), par_rtx))
    {
      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_va_args);
      operands[1] = gen_rtx_REG (SImode, re_va_args);
      /* Create assembly code pattern: "Rb, Re, { }".  */
      snprintf (pattern, sizeof (pattern), "push.s\t%s", "%0, %1, { }");
      /* We use output_asm_insn() to output assembly code by ourself.  */
      output_asm_insn (pattern, operands);
      return "";
    }

  /* If last_argument_regno is not mentioned in par_rtx, we can confirm that
     we do not need to push argument registers for variadic function.
     But we still need to check if we need to push exception handling
     data registers.  */
  if (reg_mentioned_p (gen_rtx_REG (SImode, first_eh_data_regno), par_rtx))
    {
      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_eh_data);
      operands[1] = gen_rtx_REG (SImode, re_eh_data);
      /* Create assembly code pattern: "Rb, Re, { }".  */
      snprintf (pattern, sizeof (pattern), "push.s\t%s", "%0, %1, { }");
      /* We use output_asm_insn() to output assembly code by ourself.  */
      output_asm_insn (pattern, operands);
      return "";
    }

  /* If we step here, we are going to do v3push or multiple push operation.  */

  /* Refer to nds32.h, where we comment when push25/pop25 are available.  */
  if (NDS32_V3PUSH_AVAILABLE_P)
    {
      /* For stack v3push:
	   operands[0]: Re
	   operands[1]: imm8u */

      /* This variable is to check if 'push25 Re,imm8u' is available.  */
      int sp_adjust;

      /* Set operands[0].  */
      operands[0] = gen_rtx_REG (SImode, re_callee_saved);

      /* Check if we can generate 'push25 Re,imm8u',
	 otherwise, generate 'push25 Re,0'.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_gpr_padding_bytes
		  + cfun->machine->callee_saved_fpr_regs_size;
      if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
	  && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust))
	operands[1] = GEN_INT (sp_adjust);
      else
	{
	  /* Allocate callee saved fpr space.  */
	  if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	    {
	      sp_adjust = cfun->machine->callee_saved_area_gpr_padding_bytes
			  + cfun->machine->callee_saved_fpr_regs_size;
	      operands[1] = GEN_INT (sp_adjust);
	    }
	  else
	    {
	      operands[1] = GEN_INT (0);
	    }
	}

      /* Create assembly code pattern.  */
      snprintf (pattern, sizeof (pattern), "push25\t%%0, %%1");
    }
  else
    {
      /* For normal stack push multiple:
	 operands[0]: Rb
	 operands[1]: Re
	 operands[2]: En4 */

      /* This variable is used to check if we only need to generate En4 field.
	 As long as Rb==Re=SP_REGNUM, we set this variable to 1.  */
      int push_en4_only_p = 0;

      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_callee_saved);
      operands[1] = gen_rtx_REG (SImode, re_callee_saved);

      /* 'smw.adm $sp,[$sp],$sp,0' means push nothing.  */
      if (!cfun->machine->fp_size
	  && !cfun->machine->gp_size
	  && !cfun->machine->lp_size
	  && REGNO (operands[0]) == SP_REGNUM
	  && REGNO (operands[1]) == SP_REGNUM)
	{
	  /* No need to generate instruction.  */
	  return "";
	}
      else
	{
	  /* If Rb==Re=SP_REGNUM, we only need to generate En4 field.  */
	  if (REGNO (operands[0]) == SP_REGNUM
	      && REGNO (operands[1]) == SP_REGNUM)
	    push_en4_only_p = 1;

	  /* Create assembly code pattern.
	     We need to handle the form: "Rb, Re, { $fp $gp $lp }".  */
	  snprintf (pattern, sizeof (pattern),
		    "push.s\t%s{%s%s%s }",
		    push_en4_only_p ? "" : "%0, %1, ",
		    cfun->machine->fp_size ? " $fp" : "",
		    cfun->machine->gp_size ? " $gp" : "",
		    cfun->machine->lp_size ? " $lp" : "");
	}
    }

  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output stack pop operation.
   We need to deal with normal stack pop multiple or stack v3pop.  */
const char *
nds32_output_stack_pop (rtx par_rtx ATTRIBUTE_UNUSED)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[3];
  /* Pick up first and last eh data regno for further use.  */
  int rb_eh_data = cfun->machine->eh_return_data_first_regno;
  int re_eh_data = cfun->machine->eh_return_data_last_regno;
  int first_eh_data_regno = EH_RETURN_DATA_REGNO (0);
  /* Pick up callee-saved first regno and last regno for further use.  */
  int rb_callee_saved = cfun->machine->callee_saved_first_gpr_regno;
  int re_callee_saved = cfun->machine->callee_saved_last_gpr_regno;

  /* We need to check if we need to push exception handling
     data registers.  */
  if (reg_mentioned_p (gen_rtx_REG (SImode, first_eh_data_regno), par_rtx))
    {
      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_eh_data);
      operands[1] = gen_rtx_REG (SImode, re_eh_data);
      /* Create assembly code pattern: "Rb, Re, { }".  */
      snprintf (pattern, sizeof (pattern), "pop.s\t%s", "%0, %1, { }");
      /* We use output_asm_insn() to output assembly code by ourself.  */
      output_asm_insn (pattern, operands);
      return "";
    }

  /* If we step here, we are going to do v3pop or multiple pop operation.  */

  /* Refer to nds32.h, where we comment when push25/pop25 are available.  */
  if (NDS32_V3PUSH_AVAILABLE_P)
    {
      /* For stack v3pop:
	   operands[0]: Re
	   operands[1]: imm8u */

      /* This variable is to check if 'pop25 Re,imm8u' is available.  */
      int sp_adjust;

      /* Set operands[0].  */
      operands[0] = gen_rtx_REG (SImode, re_callee_saved);

      /* Check if we can generate 'pop25 Re,imm8u',
	 otherwise, generate 'pop25 Re,0'.
	 We have to consider alloca issue as well.
	 If the function does call alloca(), the stack pointer is not fixed.
	 In that case, we cannot use 'pop25 Re,imm8u' directly.
	 We have to caculate stack pointer from frame pointer
	 and then use 'pop25 Re,0'.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_gpr_padding_bytes
		  + cfun->machine->callee_saved_fpr_regs_size;
      if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
	  && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust)
	  && !cfun->calls_alloca)
	operands[1] = GEN_INT (sp_adjust);
      else
	{
	  if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	    {
	      /* If has fpr need to restore, the $sp on callee saved fpr
		 position, so we need to consider gpr pading bytes and
		 callee saved fpr size.  */
	      sp_adjust = cfun->machine->callee_saved_area_gpr_padding_bytes
			  + cfun->machine->callee_saved_fpr_regs_size;
	      operands[1] = GEN_INT (sp_adjust);
	    }
	  else
	    {
	      operands[1] = GEN_INT (0);
	    }
	}

      /* Create assembly code pattern.  */
      snprintf (pattern, sizeof (pattern), "pop25\t%%0, %%1");
    }
  else
    {
      /* For normal stack pop multiple:
	 operands[0]: Rb
	 operands[1]: Re
	 operands[2]: En4 */

      /* This variable is used to check if we only need to generate En4 field.
	 As long as Rb==Re=SP_REGNUM, we set this variable to 1.  */
      int pop_en4_only_p = 0;

      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_callee_saved);
      operands[1] = gen_rtx_REG (SImode, re_callee_saved);

      /* 'lmw.bim $sp,[$sp],$sp,0' means pop nothing.  */
      if (!cfun->machine->fp_size
	  && !cfun->machine->gp_size
	  && !cfun->machine->lp_size
	  && REGNO (operands[0]) == SP_REGNUM
	  && REGNO (operands[1]) == SP_REGNUM)
	{
	  /* No need to generate instruction.  */
	  return "";
	}
      else
	{
	  /* If Rb==Re=SP_REGNUM, we only need to generate En4 field.  */
	  if (REGNO (operands[0]) == SP_REGNUM
	      && REGNO (operands[1]) == SP_REGNUM)
	    pop_en4_only_p = 1;

	  /* Create assembly code pattern.
	     We need to handle the form: "Rb, Re, { $fp $gp $lp }".  */
	  snprintf (pattern, sizeof (pattern),
		    "pop.s\t%s{%s%s%s }",
		    pop_en4_only_p ? "" : "%0, %1, ",
		    cfun->machine->fp_size ? " $fp" : "",
		    cfun->machine->gp_size ? " $gp" : "",
		    cfun->machine->lp_size ? " $lp" : "");
	}
    }

  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output return operation.  */
const char *
nds32_output_return (void)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[2];
  /* For stack v3pop:
     operands[0]: Re
     operands[1]: imm8u */
  int re_callee_saved = cfun->machine->callee_saved_last_gpr_regno;
  int sp_adjust;

  /* Set operands[0].  */
  operands[0] = gen_rtx_REG (SImode, re_callee_saved);

  /* Check if we can generate 'pop25 Re,imm8u',
     otherwise, generate 'pop25 Re,0'.
     We have to consider alloca issue as well.
     If the function does call alloca(), the stack pointer is not fixed.
     In that case, we cannot use 'pop25 Re,imm8u' directly.
     We have to caculate stack pointer from frame pointer
     and then use 'pop25 Re,0'.  */
  sp_adjust = cfun->machine->local_size
    + cfun->machine->out_args_size
    + cfun->machine->callee_saved_area_gpr_padding_bytes
    + cfun->machine->callee_saved_fpr_regs_size;
  if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
      && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust)
      && !cfun->calls_alloca)
    operands[1] = GEN_INT (sp_adjust);
  else
    operands[1] = GEN_INT (0);

  /* Create assembly code pattern.  */
  snprintf (pattern, sizeof (pattern), "pop25\t%%0, %%1");
  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}


/* output a float load instruction */
const char *
nds32_output_float_load (rtx *operands)
{
  char buff[100];
  const char *pattern;
  rtx addr, addr_op0, addr_op1;
  int dp = GET_MODE_SIZE (GET_MODE (operands[0])) == 8;
  addr = XEXP (operands[1], 0);
  switch (GET_CODE (addr))
    {
    case REG:
      pattern = "fl%ci\t%%0, %%1";
      break;

    case PLUS:
      addr_op0 = XEXP (addr, 0);
      addr_op1 = XEXP (addr, 1);

      if (REG_P (addr_op0) && REG_P (addr_op1))
	pattern = "fl%c\t%%0, %%1";
      else if (REG_P (addr_op0) && CONST_INT_P (addr_op1))
	pattern = "fl%ci\t%%0, %%1";
      else if (GET_CODE (addr_op0) == MULT && REG_P (addr_op1)
	       && REG_P (XEXP (addr_op0, 0))
	       && CONST_INT_P (XEXP (addr_op0, 1)))
	pattern = "fl%c\t%%0, %%1";
      else
	gcc_unreachable ();
      break;

    case POST_MODIFY:
      addr_op0 = XEXP (addr, 0);
      addr_op1 = XEXP (addr, 1);

      if (REG_P (addr_op0) && GET_CODE (addr_op1) == PLUS
	  && REG_P (XEXP (addr_op1, 1)))
	pattern = "fl%c.bi\t%%0, %%1";
      else if (REG_P (addr_op0) && GET_CODE (addr_op1) == PLUS
	       && CONST_INT_P (XEXP (addr_op1, 1)))
	pattern = "fl%ci.bi\t%%0, %%1";
      else
	gcc_unreachable ();
      break;

    case POST_INC:
      if (REG_P (XEXP (addr, 0)))
	{
	  if (dp)
	    pattern = "fl%ci.bi\t%%0, %%1, 8";
	  else
	    pattern = "fl%ci.bi\t%%0, %%1, 4";
	}
      else
	gcc_unreachable ();
      break;

    case POST_DEC:
      if (REG_P (XEXP (addr, 0)))
	{
	  if (dp)
	    pattern = "fl%ci.bi\t%%0, %%1, -8";
	  else
	    pattern = "fl%ci.bi\t%%0, %%1, -4";
	}
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }

  sprintf (buff, pattern, dp ? 'd' : 's');
  output_asm_insn (buff, operands);
  return "";
}

/* output a float store instruction */
const char *
nds32_output_float_store (rtx *operands)
{
  char buff[100];
  const char *pattern;
  rtx addr, addr_op0, addr_op1;
  int dp = GET_MODE_SIZE (GET_MODE (operands[0])) == 8;
  addr = XEXP (operands[0], 0);
  switch (GET_CODE (addr))
    {
    case REG:
      pattern = "fs%ci\t%%1, %%0";
      break;

    case PLUS:
      addr_op0 = XEXP (addr, 0);
      addr_op1 = XEXP (addr, 1);

      if (REG_P (addr_op0) && REG_P (addr_op1))
	pattern = "fs%c\t%%1, %%0";
      else if (REG_P (addr_op0) && CONST_INT_P (addr_op1))
	pattern = "fs%ci\t%%1, %%0";
      else if (GET_CODE (addr_op0) == MULT && REG_P (addr_op1)
	       && REG_P (XEXP (addr_op0, 0))
	       && CONST_INT_P (XEXP (addr_op0, 1)))
	pattern = "fs%c\t%%1, %%0";
      else
	gcc_unreachable ();
      break;

    case POST_MODIFY:
      addr_op0 = XEXP (addr, 0);
      addr_op1 = XEXP (addr, 1);

      if (REG_P (addr_op0) && GET_CODE (addr_op1) == PLUS
	  && REG_P (XEXP (addr_op1, 1)))
	pattern = "fs%c.bi\t%%1, %%0";
      else if (REG_P (addr_op0) && GET_CODE (addr_op1) == PLUS
	       && CONST_INT_P (XEXP (addr_op1, 1)))
	pattern = "fs%ci.bi\t%%1, %%0";
      else
	gcc_unreachable ();
      break;

    case POST_INC:
      if (REG_P (XEXP (addr, 0)))
	{
	  if (dp)
	    pattern = "fs%ci.bi\t%%1, %%0, 8";
	  else
	    pattern = "fs%ci.bi\t%%1, %%0, 4";
	}
      else
	gcc_unreachable ();
      break;

    case POST_DEC:
      if (REG_P (XEXP (addr, 0)))
	{
	  if (dp)
	    pattern = "fs%ci.bi\t%%1, %%0, -8";
	  else
	    pattern = "fs%ci.bi\t%%1, %%0, -4";
	}
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }

  sprintf (buff, pattern, dp ? 'd' : 's');
  output_asm_insn (buff, operands);
  return "";
}

const char *
nds32_output_smw_single_word (rtx *operands)
{
  char buff[100];
  unsigned regno;
  int enable4;
  bool update_base_p;
  rtx base_addr = operands[0];
  rtx base_reg;
  rtx otherops[2];

  if (REG_P (XEXP (base_addr, 0)))
    {
      update_base_p = false;
      base_reg = XEXP (base_addr, 0);
    }
  else
    {
      update_base_p = true;
      base_reg = XEXP (XEXP (base_addr, 0), 0);
    }

  const char *update_base = update_base_p ? "m" : "";

  regno = REGNO (operands[1]);

  otherops[0] = base_reg;
  otherops[1] = operands[1];

  if (regno >= 28)
    {
      enable4 = nds32_regno_to_enable4 (regno);
      sprintf (buff, "smw.bi%s\t$sp, [%%0], $sp, %x", update_base, enable4);
    }
  else
    {
      sprintf (buff, "smw.bi%s\t%%1, [%%0], %%1", update_base);
    }
  output_asm_insn (buff, otherops);
  return "";
}

/* ------------------------------------------------------------------------ */
const char *
nds32_output_smw_double_word (rtx *operands)
{
  char buff[100];
  unsigned regno;
  int enable4;
  bool update_base_p;
  rtx base_addr = operands[0];
  rtx base_reg;
  rtx otherops[3];

  if (REG_P (XEXP (base_addr, 0)))
    {
      update_base_p = false;
      base_reg = XEXP (base_addr, 0);
    }
  else
    {
      update_base_p = true;
      base_reg = XEXP (XEXP (base_addr, 0), 0);
    }

  const char *update_base = update_base_p ? "m" : "";

  regno = REGNO (operands[1]);

  otherops[0] = base_reg;
  otherops[1] = operands[1];
  otherops[2] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);;

  if (regno >= 28)
    {
      enable4 = nds32_regno_to_enable4 (regno)
		| nds32_regno_to_enable4 (regno + 1);
      sprintf (buff, "smw.bi%s\t$sp, [%%0], $sp, %x", update_base, enable4);
    }
  else if (regno == 27)
    {
      enable4 = nds32_regno_to_enable4 (regno + 1);
      sprintf (buff, "smw.bi%s\t%%1, [%%0], %%1, %x", update_base, enable4);
    }
  else
    {
      sprintf (buff, "smw.bi%s\t%%1, [%%0], %%2", update_base);
    }
  output_asm_insn (buff, otherops);
  return "";
}

const char *
nds32_output_lmw_single_word (rtx *operands)
{
  char buff[100];
  unsigned regno;
  bool update_base_p;
  int enable4;
  rtx base_addr = operands[1];
  rtx base_reg;
  rtx otherops[2];

  if (REG_P (XEXP (base_addr, 0)))
    {
      update_base_p = false;
      base_reg = XEXP (base_addr, 0);
    }
  else
    {
      update_base_p = true;
      base_reg = XEXP (XEXP (base_addr, 0), 0);
    }

  const char *update_base = update_base_p ? "m" : "";

  regno = REGNO (operands[0]);

  otherops[0] = operands[0];
  otherops[1] = base_reg;

  if (regno >= 28)
    {
      enable4 = nds32_regno_to_enable4 (regno);
      sprintf (buff, "lmw.bi%s\t$sp, [%%1], $sp, %x", update_base, enable4);
    }
  else
    {
      sprintf (buff, "lmw.bi%s\t%%0, [%%1], %%0", update_base);
    }
  output_asm_insn (buff, otherops);
  return "";
}

void
nds32_expand_unaligned_load (rtx *operands, enum machine_mode mode)
{
  /* Initial memory offset.  */
  int offset = WORDS_BIG_ENDIAN ? GET_MODE_SIZE (mode) - 1 : 0;
  int offset_adj = WORDS_BIG_ENDIAN ? -1 : 1;
  /* Initial register shift byte.  */
  int shift = 0;
  /* The first load byte instruction is not the same. */
  int width = GET_MODE_SIZE (mode) - 1;
  rtx mem[2];
  rtx reg[2];
  rtx sub_reg;
  rtx temp_reg, temp_sub_reg;
  int num_reg;

  /* Generating a series of load byte instructions.
     The first load byte instructions and other
     load byte instructions are not the same. like:
     First:
       lbi reg0, [mem]
       zeh reg0, reg0
     Second:
       lbi temp_reg, [mem + offset]
       sll temp_reg, (8 * shift)
       ior reg0, temp_reg

       lbi temp_reg, [mem + (offset + 1)]
       sll temp_reg, (8 * (shift + 1))
       ior reg0, temp_reg  */

  temp_reg = gen_reg_rtx (SImode);
  temp_sub_reg = gen_lowpart (QImode, temp_reg);

  if (mode == DImode)
    {
      /* Load doubleword, we need two registers to access.  */
      reg[0] = nds32_di_low_part_subreg (operands[0]);
      reg[1] = nds32_di_high_part_subreg (operands[0]);
      /* A register only store 4 byte.  */
      width = GET_MODE_SIZE (SImode) - 1;
    }
  else
    {
      if (VECTOR_MODE_P (mode))
	reg[0] = gen_reg_rtx (SImode);
      else
	reg[0] = operands[0];
    }

  for (num_reg = (mode == DImode) ? 2 : 1; num_reg > 0; num_reg--)
    {
      sub_reg = gen_lowpart (QImode, reg[0]);
      mem[0] = gen_rtx_MEM (QImode, plus_constant (Pmode, operands[1], offset));

      /* Generating the first part instructions.
	   lbi reg0, [mem]
	   zeh reg0, reg0 */
      emit_move_insn (sub_reg, mem[0]);
      emit_insn (gen_zero_extendqisi2 (reg[0], sub_reg));

      while (width > 0)
	{
	  offset = offset + offset_adj;
	  shift++;
	  width--;

	  mem[1] = gen_rtx_MEM (QImode, plus_constant (Pmode,
						       operands[1],
						       offset));
	  /* Generating the second part instructions.
	       lbi temp_reg, [mem + offset]
	       sll temp_reg, (8 * shift)
	       ior reg0, temp_reg  */
	  emit_move_insn (temp_sub_reg, mem[1]);
	  emit_insn (gen_ashlsi3 (temp_reg, temp_reg,
				  GEN_INT (shift * 8)));
	  emit_insn (gen_iorsi3 (reg[0], reg[0], temp_reg));
	}

      if (mode == DImode)
	{
	  /* Using the second register to load memory information. */
	  reg[0] = reg[1];
	  shift = 0;
	  width = GET_MODE_SIZE (SImode) - 1;
	  offset = offset + offset_adj;
	}
    }
    if (VECTOR_MODE_P (mode))
      convert_move (operands[0], reg[0], false);
}

void
nds32_expand_unaligned_store (rtx *operands, enum machine_mode mode)
{
  /* Initial memory offset.  */
  int offset = WORDS_BIG_ENDIAN ? GET_MODE_SIZE (mode) - 1 : 0;
  int offset_adj = WORDS_BIG_ENDIAN ? -1 : 1;
  /* Initial register shift byte.  */
  int shift = 0;
  /* The first load byte instruction is not the same. */
  int width = GET_MODE_SIZE (mode) - 1;
  rtx mem[2];
  rtx reg[2];
  rtx sub_reg;
  rtx temp_reg, temp_sub_reg;
  int num_reg;

  /* Generating a series of store byte instructions.
     The first store byte instructions and other
     load byte instructions are not the same. like:
     First:
	sbi  reg0, [mem + 0]
     Second:
	srli    temp_reg, reg0, (8 * shift)
	sbi	temp_reg, [mem + offset]  */

  temp_reg = gen_reg_rtx (SImode);
  temp_sub_reg = gen_lowpart (QImode, temp_reg);

  if (mode == DImode)
    {
      /* Load doubleword, we need two registers to access.  */
      reg[0] = nds32_di_low_part_subreg (operands[1]);
      reg[1] = nds32_di_high_part_subreg (operands[1]);
      /* A register only store 4 byte.  */
      width = GET_MODE_SIZE (SImode) - 1;
    }
  else
    {
      if (VECTOR_MODE_P (mode))
	{
	  reg[0] = gen_reg_rtx (SImode);
	  convert_move (reg[0], operands[1], false);
	}
      else
	reg[0] = operands[1];
    }

  for (num_reg = (mode == DImode) ? 2 : 1; num_reg > 0; num_reg--)
    {
      sub_reg = gen_lowpart (QImode, reg[0]);
      mem[0] = gen_rtx_MEM (QImode, plus_constant (Pmode, operands[0], offset));

      /* Generating the first part instructions.
	   sbi reg0, [mem + 0] */
      emit_move_insn (mem[0], sub_reg);

      while (width > 0)
	{
	  offset = offset + offset_adj;
	  shift++;
	  width--;

	  mem[1] = gen_rtx_MEM (QImode, plus_constant (Pmode,
						       operands[0],
						       offset));
	  /* Generating the second part instructions.
	       srli  temp_reg, reg0, (8 * shift)
	       sbi   temp_reg, [mem + offset]  */
	  emit_insn (gen_lshrsi3 (temp_reg, reg[0],
				  GEN_INT (shift * 8)));
	  emit_move_insn (mem[1], temp_sub_reg);
	}

      if (mode == DImode)
	{
	  /* Using the second register to load memory information. */
	  reg[0] = reg[1];
	  shift = 0;
	  width = GET_MODE_SIZE (SImode) - 1;
	  offset = offset + offset_adj;
	}
    }
}

/* Using multiple load/store instruction to output doubleword instruction.  */
const char *
nds32_output_double (rtx *operands, bool load_p)
{
  char pattern[100];
  int reg = load_p ? 0 : 1;
  int mem = load_p ? 1 : 0;
  rtx otherops[3];
  rtx addr = XEXP (operands[mem], 0);

  otherops[0] = gen_rtx_REG (SImode, REGNO (operands[reg]));
  otherops[1] = gen_rtx_REG (SImode, REGNO (operands[reg]) + 1);

  if (GET_CODE (addr)  == POST_INC)
    {
      /* (mem (post_inc (reg))) */
      otherops[2] = XEXP (addr, 0);
      snprintf (pattern, sizeof (pattern),
		"%cmw.bim\t%%0, [%%2], %%1, 0", load_p ? 'l' : 's');
    }
  else
    {
      /* (mem (reg)) */
      otherops[2] = addr;
      snprintf (pattern, sizeof (pattern),
		"%cmw.bi\t%%0, [%%2], %%1, 0", load_p ? 'l' : 's');

    }

  output_asm_insn (pattern, otherops);
  return "";
}

const char *
nds32_output_cbranchsi4_equality_zero (rtx_insn *insn, rtx *operands)
{
  enum rtx_code code;
  bool long_jump_p = false;

  code = GET_CODE (operands[0]);

  /* This zero-comparison conditional branch has two forms:
       32-bit instruction =>          beqz/bnez           imm16s << 1
       16-bit instruction => beqzs8/bnezs8/beqz38/bnez38  imm8s << 1

     For 32-bit case,
     we assume it is always reachable. (but check range -65500 ~ 65500)

     For 16-bit case,
     it must satisfy { 255 >= (label - pc) >= -256 } condition.
     However, since the $pc for nds32 is at the beginning of the instruction,
     we should leave some length space for current insn.
     So we use range -250 ~ 250.  */

  switch (get_attr_length (insn))
    {
    case 8:
      long_jump_p = true;
      /* fall through  */
    case 2:
      if (which_alternative == 0)
	{
	  /* constraint: t */
	  /*    b<cond>zs8  .L0
	      or
		b<inverse_cond>zs8  .LCB0
		j  .L0
	      .LCB0:
	   */
	  output_cond_branch_compare_zero (code, "s8", long_jump_p,
					   operands, true);
	  return "";
	}
      else if (which_alternative == 1)
	{
	  /* constraint: l */
	  /*    b<cond>z38  $r0, .L0
	      or
		b<inverse_cond>z38  $r0, .LCB0
		j  .L0
	      .LCB0:
	   */
	  output_cond_branch_compare_zero (code, "38", long_jump_p,
					   operands, false);
	  return "";
	}
      else
	{
	  /* constraint: r */
	  /* For which_alternative==2, it should not be here.  */
	  gcc_unreachable ();
	}
    case 10:
      /* including constraints: t, l, and r */
      long_jump_p = true;
      /* fall through  */
    case 4:
      /* including constraints: t, l, and r */
      output_cond_branch_compare_zero (code, "", long_jump_p, operands, false);
      return "";

    default:
      gcc_unreachable ();
    }
}

const char *
nds32_output_cbranchsi4_equality_reg (rtx_insn *insn, rtx *operands)
{
  enum rtx_code code;
  bool long_jump_p, r5_p;
  int insn_length;

  insn_length = get_attr_length (insn);

  long_jump_p = (insn_length == 10 || insn_length == 8) ? true : false;
  r5_p = (insn_length == 2 || insn_length == 8) ? true : false;

  code = GET_CODE (operands[0]);

  /* This register-comparison conditional branch has one form:
       32-bit instruction =>          beq/bne           imm14s << 1

     For 32-bit case,
     we assume it is always reachable. (but check range -16350 ~ 16350).  */

  switch (code)
    {
    case EQ:
    case NE:
      output_cond_branch (code, "", r5_p, long_jump_p, operands);
      return "";

    default:
      gcc_unreachable ();
    }
}

const char *
nds32_output_cbranchsi4_equality_reg_or_const_int (rtx_insn *insn,
						   rtx *operands)
{
  enum rtx_code code;
  bool long_jump_p, r5_p;
  int insn_length;

  insn_length = get_attr_length (insn);

  long_jump_p = (insn_length == 10 || insn_length == 8) ? true : false;
  r5_p = (insn_length == 2 || insn_length == 8) ? true : false;

  code = GET_CODE (operands[0]);

  /* This register-comparison conditional branch has one form:
       32-bit instruction =>          beq/bne           imm14s << 1
       32-bit instruction =>         beqc/bnec          imm8s << 1

     For 32-bit case, we assume it is always reachable.
     (but check range -16350 ~ 16350 and -250 ~ 250).  */

  switch (code)
    {
    case EQ:
    case NE:
      if (which_alternative == 2)
	{
	  /* r, Is11 */
	  /* b<cond>c */
	  output_cond_branch (code, "c", r5_p, long_jump_p, operands);
	}
      else
	{
	  /* r, r */
	  /* v, r */
	  output_cond_branch (code, "", r5_p, long_jump_p, operands);
	}
      return "";
    default:
      gcc_unreachable ();
    }
}

const char *
nds32_output_cbranchsi4_greater_less_zero (rtx_insn *insn, rtx *operands)
{
  enum rtx_code code;
  bool long_jump_p;
  int insn_length;

  insn_length = get_attr_length (insn);

  gcc_assert (insn_length == 4 || insn_length == 10);

  long_jump_p = (insn_length == 10) ? true : false;

  code = GET_CODE (operands[0]);

  /* This zero-greater-less-comparison conditional branch has one form:
       32-bit instruction =>      bgtz/bgez/bltz/blez     imm16s << 1

     For 32-bit case, we assume it is always reachable.
     (but check range -65500 ~ 65500).  */

  switch (code)
    {
    case GT:
    case GE:
    case LT:
    case LE:
      output_cond_branch_compare_zero (code, "", long_jump_p, operands, false);
      break;
    default:
      gcc_unreachable ();
    }
  return "";
}

const char *
nds32_output_unpkd8 (rtx output, rtx input,
		     rtx high_idx_rtx, rtx low_idx_rtx,
		     bool signed_p)
{
  char pattern[100];
  rtx output_operands[2];
  HOST_WIDE_INT high_idx, low_idx;
  high_idx = INTVAL (high_idx_rtx);
  low_idx = INTVAL (low_idx_rtx);

  gcc_assert (high_idx >= 0 && high_idx <= 3);
  gcc_assert (low_idx >= 0 && low_idx <= 3);

  /* We only have 10, 20, 30 and 31.  */
  if ((low_idx != 0 || high_idx == 0) &&
      !(low_idx == 1 && high_idx == 3))
    return "#";

  char sign_char = signed_p ? 's' : 'z';

  sprintf (pattern,
	   "%cunpkd8" HOST_WIDE_INT_PRINT_DEC HOST_WIDE_INT_PRINT_DEC "\t%%0, %%1",
	   sign_char, high_idx, low_idx);
  output_operands[0] = output;
  output_operands[1] = input;
  output_asm_insn (pattern, output_operands);
  return "";
}

/* Return true if SYMBOL_REF X binds locally.  */

static bool
nds32_symbol_binds_local_p (const_rtx x)
{
  return (SYMBOL_REF_DECL (x)
	  ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	  : SYMBOL_REF_LOCAL_P (x));
}

const char *
nds32_output_call (rtx insn, rtx *operands, rtx symbol, const char *long_call,
		   const char *call, bool align_p)
{
  char pattern[100];
  bool noreturn_p;

  if (nds32_long_call_p (symbol))
    strcpy (pattern, long_call);
  else
    strcpy (pattern, call);

  if (flag_pic && CONSTANT_P (symbol)
      && !nds32_symbol_binds_local_p (symbol))
    strcat (pattern, "@PLT");

  if (align_p)
    strcat (pattern, "\n\t.align 2");

  noreturn_p = find_reg_note (insn, REG_NORETURN, NULL_RTX) != NULL_RTX;

  if (noreturn_p)
    {
      if (TARGET_16_BIT)
	strcat (pattern, "\n\tnop16");
      else
	strcat (pattern, "\n\tnop");
    }

  output_asm_insn (pattern, operands);
  return "";
}

bool
nds32_need_split_sms_p (rtx in0_idx0, rtx in1_idx0,
			rtx in0_idx1, rtx in1_idx1)
{
  /* smds or smdrs.  */
  if (INTVAL (in0_idx0) == INTVAL (in1_idx0)
      && INTVAL (in0_idx1) == INTVAL (in1_idx1)
      && INTVAL (in0_idx0) != INTVAL (in0_idx1))
    return false;

  /* smxds.  */
  if (INTVAL (in0_idx0) != INTVAL (in0_idx1)
      && INTVAL (in1_idx0) != INTVAL (in1_idx1))
    return false;

  return true;
}

const char *
nds32_output_sms (rtx in0_idx0, rtx in1_idx0,
		  rtx in0_idx1, rtx in1_idx1)
{
  if (nds32_need_split_sms_p (in0_idx0, in1_idx0,
			      in0_idx1, in1_idx1))
    return "#";
  /* out = in0[in0_idx0] * in1[in1_idx0] - in0[in0_idx1] * in1[in1_idx1] */

  /* smds or smdrs.  */
  if (INTVAL (in0_idx0) == INTVAL (in1_idx0)
      && INTVAL (in0_idx1) == INTVAL (in1_idx1)
      && INTVAL (in0_idx0) != INTVAL (in0_idx1))
    {
      if (INTVAL (in0_idx0) == 0)
	{
	  if (TARGET_BIG_ENDIAN)
	    return "smds\t%0, %1, %2";
	  else
	    return "smdrs\t%0, %1, %2";
	}
      else
	{
	  if (TARGET_BIG_ENDIAN)
	    return "smdrs\t%0, %1, %2";
	  else
	    return "smds\t%0, %1, %2";
	}
    }

  if (INTVAL (in0_idx0) != INTVAL (in0_idx1)
      && INTVAL (in1_idx0) != INTVAL (in1_idx1))
    {
      if (INTVAL (in0_idx0) == 1)
	{
	  if (TARGET_BIG_ENDIAN)
	    return "smxds\t%0, %2, %1";
	  else
	    return "smxds\t%0, %1, %2";
	}
      else
	{
	  if (TARGET_BIG_ENDIAN)
	    return "smxds\t%0, %1, %2";
	  else
	    return "smxds\t%0, %2, %1";
	}
    }

  gcc_unreachable ();
  return "";
}

void
nds32_split_sms (rtx out, rtx in0, rtx in1,
		 rtx in0_idx0, rtx in1_idx0,
		 rtx in0_idx1, rtx in1_idx1)
{
  rtx result0 = gen_reg_rtx (SImode);
  rtx result1 = gen_reg_rtx (SImode);
  emit_insn (gen_mulhisi3v (result0, in0, in1,
			    in0_idx0, in1_idx0));
  emit_insn (gen_mulhisi3v (result1, in0, in1,
			    in0_idx1, in1_idx1));
  emit_insn (gen_subsi3 (out, result0, result1));
}

/* Spilt a doubleword instrucion to two single word instructions.  */
void
nds32_spilt_doubleword (rtx *operands, bool load_p)
{
  int reg = load_p ? 0 : 1;
  int mem = load_p ? 1 : 0;
  rtx reg_rtx = load_p ? operands[0] : operands[1];
  rtx mem_rtx = load_p ? operands[1] : operands[0];
  rtx low_part[2], high_part[2];
  rtx sub_mem = XEXP (mem_rtx, 0);

  /* Generate low_part and high_part register pattern.
     i.e. register pattern like:
     (reg:DI) -> (subreg:SI (reg:DI))
		 (subreg:SI (reg:DI)) */
  low_part[reg] = simplify_gen_subreg (SImode, reg_rtx, GET_MODE (reg_rtx), 0);
  high_part[reg] = simplify_gen_subreg (SImode, reg_rtx, GET_MODE (reg_rtx), 4);

  /* Generate low_part and high_part memory pattern.
     Memory format is (post_dec) will generate:
       low_part:  lwi.bi reg, [mem], 4
       high_part: lwi.bi reg, [mem], -12 */
  if (GET_CODE (sub_mem) == POST_DEC)
    {
      /* memory format is (post_dec (reg)),
	 so that extract (reg) from the (post_dec (reg)) pattern.  */
      sub_mem = XEXP (sub_mem, 0);

      /* generate low_part and high_part memory format:
	   low_part:  (post_modify ((reg) (plus (reg) (const 4)))
	   high_part: (post_modify ((reg) (plus (reg) (const -12))) */
      low_part[mem] = gen_rtx_MEM (SImode,
				   gen_rtx_POST_MODIFY (Pmode, sub_mem,
							gen_rtx_PLUS (Pmode,
							sub_mem,
							GEN_INT (4))));
      high_part[mem] = gen_rtx_MEM (SImode,
				    gen_rtx_POST_MODIFY (Pmode, sub_mem,
							 gen_rtx_PLUS (Pmode,
							 sub_mem,
							 GEN_INT (-12))));
    }
  else if (GET_CODE (sub_mem) == POST_INC)
    {
      /* memory format is (post_inc (reg)),
	 so that extract (reg) from the (post_inc (reg)) pattern.  */
      sub_mem = XEXP (sub_mem, 0);

      /* generate low_part and high_part memory format:
	   low_part:  (post_inc (reg))
	   high_part: (post_inc (reg)) */
      low_part[mem] = gen_rtx_MEM (SImode,
				   gen_rtx_POST_INC (Pmode, sub_mem));
      high_part[mem] = gen_rtx_MEM (SImode,
				    gen_rtx_POST_INC (Pmode, sub_mem));
    }
  else if (GET_CODE (sub_mem) == POST_MODIFY)
    {
      /* Memory format is (post_modify (reg) (plus (reg) (const))),
	 so that extract (reg) from the post_modify pattern.  */
      rtx post_mem = XEXP (sub_mem, 0);

      /* Extract (const) from the (post_modify (reg) (plus (reg) (const)))
	 pattern.  */

      rtx plus_op = XEXP (sub_mem, 1);
      rtx post_val = XEXP (plus_op, 1);

      /* Generate low_part and high_part memory format:
	   low_part:  (post_modify ((reg) (plus (reg) (const)))
	   high_part: ((plus (reg) (const 4))) */
      low_part[mem] = gen_rtx_MEM (SImode,
				   gen_rtx_POST_MODIFY (Pmode, post_mem,
							gen_rtx_PLUS (Pmode,
							post_mem,
							post_val)));
      high_part[mem] = gen_rtx_MEM (SImode, plus_constant (Pmode,
							   post_mem,
							   4));
    }
  else
    {
      /* memory format: (symbol_ref), (const), (reg + const_int).  */
      low_part[mem] = adjust_address (mem_rtx, SImode, 0);
      high_part[mem] = adjust_address (mem_rtx, SImode, 4);
    }

  /* After reload completed, we have dependent issue by low part register and
     higt part memory. i.e. we cannot split a sequence
     like:
	load $r0, [%r1]
     spilt to
	lw  $r0, [%r0]
	lwi $r1, [%r0 + 4]
     swap position
	lwi $r1, [%r0 + 4]
	lw  $r0, [%r0]
     For store instruction we don't have a problem.

     When memory format is [post_modify], we need to emit high part instruction,
     before low part instruction.
     expamle:
       load $r0, [%r2], post_val
     spilt to
       load $r1, [%r2 + 4]
       load $r0, [$r2], post_val.  */
  if ((load_p && reg_overlap_mentioned_p (low_part[0], high_part[1]))
      || GET_CODE (sub_mem) == POST_MODIFY)
    {
      operands[2] = high_part[0];
      operands[3] = high_part[1];
      operands[4] = low_part[0];
      operands[5] = low_part[1];
    }
  else
    {
      operands[2] = low_part[0];
      operands[3] = low_part[1];
      operands[4] = high_part[0];
      operands[5] = high_part[1];
    }
}

void
nds32_split_ashiftdi3 (rtx dst, rtx src, rtx shiftamount)
{
  rtx src_high_part, src_low_part;
  rtx dst_high_part, dst_low_part;

  dst_high_part = nds32_di_high_part_subreg (dst);
  dst_low_part = nds32_di_low_part_subreg (dst);

  src_high_part = nds32_di_high_part_subreg (src);
  src_low_part = nds32_di_low_part_subreg (src);

  /* We need to handle shift more than 32 bit!!!! */
  if (CONST_INT_P (shiftamount))
    {
      if (INTVAL (shiftamount) < 32)
	{
	  rtx ext_start;
	  ext_start = gen_int_mode(32 - INTVAL (shiftamount), SImode);

	  emit_insn (gen_wext (dst_high_part, src, ext_start));
	  emit_insn (gen_ashlsi3 (dst_low_part, src_low_part, shiftamount));
	}
      else
	{
	  rtx new_shift_amout = gen_int_mode(INTVAL (shiftamount) - 32, SImode);

	  emit_insn (gen_ashlsi3 (dst_high_part, src_low_part,
						 new_shift_amout));

	  emit_move_insn (dst_low_part, GEN_INT (0));
	}
    }
  else
    {
      rtx dst_low_part_l32, dst_high_part_l32;
      rtx dst_low_part_g32, dst_high_part_g32;
      rtx new_shift_amout, select_reg;
      dst_low_part_l32 = gen_reg_rtx (SImode);
      dst_high_part_l32 = gen_reg_rtx (SImode);
      dst_low_part_g32 = gen_reg_rtx (SImode);
      dst_high_part_g32 = gen_reg_rtx (SImode);
      new_shift_amout = gen_reg_rtx (SImode);
      select_reg = gen_reg_rtx (SImode);

      rtx ext_start;
      ext_start = gen_reg_rtx (SImode);

      /*
	 # In fact, we want to check shift amonut is great than or equal 32,
	 # but in some corner case, the shift amount might be very large value,
	 # however we've defined SHIFT_COUNT_TRUNCATED, so GCC think we've
	 # handle that correctly without any truncate.
	 # so check the the condition of (shiftamount & 32) is most
	 # safe way to do.
	 if (shiftamount & 32)
	   dst_low_part = 0
	   dst_high_part = src_low_part << shiftamount & 0x1f
	 else
	   dst_low_part = src_low_part << shiftamout
	   dst_high_part = wext (src, 32 - shiftamount)
	   # wext can't handle wext (src, 32) since it's only take rb[0:4]
	   # for extract.
	   dst_high_part = shiftamount == 0 ? src_high_part : dst_high_part

      */

      emit_insn (gen_subsi3 (ext_start,
			     gen_int_mode (32, SImode),
			     shiftamount));
      emit_insn (gen_wext (dst_high_part_l32, src, ext_start));

      /* Handle for shiftamout == 0.  */
      emit_insn (gen_cmovzsi (dst_high_part_l32, shiftamount,
			      src_high_part, dst_high_part_l32));

      emit_insn (gen_ashlsi3 (dst_low_part_l32, src_low_part, shiftamount));

      emit_move_insn (dst_low_part_g32, const0_rtx);
      emit_insn (gen_andsi3 (new_shift_amout, shiftamount, GEN_INT (0x1f)));
      emit_insn (gen_ashlsi3 (dst_high_part_g32, src_low_part,
						 new_shift_amout));

      emit_insn (gen_andsi3 (select_reg, shiftamount, GEN_INT (32)));

      emit_insn (gen_cmovzsi (dst_low_part, select_reg,
			      dst_low_part_l32, dst_low_part_g32));
      emit_insn (gen_cmovzsi (dst_high_part, select_reg,
			      dst_high_part_l32, dst_high_part_g32));
    }
}

void
nds32_split_ashiftrtdi3 (rtx dst, rtx src, rtx shiftamount)
{
  nds32_split_shiftrtdi3 (dst, src, shiftamount, false);
}

void
nds32_split_lshiftrtdi3 (rtx dst, rtx src, rtx shiftamount)
{
  nds32_split_shiftrtdi3 (dst, src, shiftamount, true);
}

void
nds32_split_rotatertdi3 (rtx dst, rtx src, rtx shiftamount)
{
  rtx dst_low_part_l32, dst_high_part_l32;
  rtx dst_low_part_g32, dst_high_part_g32;
  rtx select_reg, low5bit, low5bit_inv, minus32sa;
  rtx dst_low_part_g32_tmph;
  rtx dst_low_part_g32_tmpl;
  rtx dst_high_part_l32_tmph;
  rtx dst_high_part_l32_tmpl;

  rtx src_low_part, src_high_part;
  rtx dst_high_part, dst_low_part;

  shiftamount = force_reg (SImode, shiftamount);

  emit_insn (gen_andsi3 (shiftamount,
			 shiftamount,
			 gen_int_mode (0x3f, SImode)));

  dst_high_part = nds32_di_high_part_subreg (dst);
  dst_low_part = nds32_di_low_part_subreg (dst);

  src_high_part = nds32_di_high_part_subreg (src);
  src_low_part = nds32_di_low_part_subreg (src);

  dst_low_part_l32 = gen_reg_rtx (SImode);
  dst_high_part_l32 = gen_reg_rtx (SImode);
  dst_low_part_g32 = gen_reg_rtx (SImode);
  dst_high_part_g32 = gen_reg_rtx (SImode);
  low5bit = gen_reg_rtx (SImode);
  low5bit_inv = gen_reg_rtx (SImode);
  minus32sa = gen_reg_rtx (SImode);
  select_reg = gen_reg_rtx (SImode);

  dst_low_part_g32_tmph = gen_reg_rtx (SImode);
  dst_low_part_g32_tmpl = gen_reg_rtx (SImode);

  dst_high_part_l32_tmph = gen_reg_rtx (SImode);
  dst_high_part_l32_tmpl = gen_reg_rtx (SImode);

  emit_insn (gen_slt_compare (select_reg, shiftamount, GEN_INT (32)));

  /* if shiftamount < 32
       dst_low_part = wext(src, shiftamount)
     else
       dst_low_part = ((src_high_part >> (shiftamount & 0x1f))
		       | (src_low_part << (32 - (shiftamount & 0x1f))))
  */
  emit_insn (gen_andsi3 (low5bit, shiftamount, gen_int_mode (0x1f, SImode)));
  emit_insn (gen_subsi3 (low5bit_inv, gen_int_mode (32, SImode), low5bit));

  emit_insn (gen_wext (dst_low_part_l32, src, shiftamount));

  emit_insn (gen_lshrsi3 (dst_low_part_g32_tmpl, src_high_part, low5bit));
  emit_insn (gen_ashlsi3 (dst_low_part_g32_tmph, src_low_part, low5bit_inv));

  emit_insn (gen_iorsi3 (dst_low_part_g32,
			 dst_low_part_g32_tmpl,
			 dst_low_part_g32_tmph));

  emit_insn (gen_cmovnsi (dst_low_part, select_reg,
			  dst_low_part_l32, dst_low_part_g32));

  /* if shiftamount < 32
       dst_high_part = ((src_high_part >> shiftamount)
			| (src_low_part << (32 - shiftamount)))
       dst_high_part = shiftamount == 0 ? src_high_part : dst_high_part
     else
       dst_high_part = wext(src, shiftamount & 0x1f)
  */

  emit_insn (gen_subsi3 (minus32sa, gen_int_mode (32, SImode), shiftamount));

  emit_insn (gen_lshrsi3 (dst_high_part_l32_tmpl, src_high_part, shiftamount));
  emit_insn (gen_ashlsi3 (dst_high_part_l32_tmph, src_low_part, minus32sa));

  emit_insn (gen_iorsi3 (dst_high_part_l32,
			 dst_high_part_l32_tmpl,
			 dst_high_part_l32_tmph));

  emit_insn (gen_cmovzsi (dst_high_part_l32, shiftamount,
			  src_high_part, dst_high_part_l32));

  emit_insn (gen_wext (dst_high_part_g32, src, low5bit));

  emit_insn (gen_cmovnsi (dst_high_part, select_reg,
			  dst_high_part_l32, dst_high_part_g32));
}

/* Return true if OP contains a symbol reference.  */
bool
symbolic_reference_mentioned_p (rtx op)
{
  const char *fmt;
  int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return true;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return true;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return true;
    }

  return false;
}

/* Expand PIC code for @GOTOFF and @GOT.

  Example for @GOTOFF:

    la $r0, symbol@GOTOFF
      -> sethi $ta, hi20(symbol@GOTOFF)
	 ori $ta, $ta, lo12(symbol@GOTOFF)
	 add $r0, $ta, $gp

  Example for @GOT:

    la $r0, symbol@GOT
      -> sethi $ta, hi20(symbol@GOT)
	 ori $ta, $ta, lo12(symbol@GOT)
	 lw  $r0, [$ta + $gp]
*/
rtx
nds32_legitimize_pic_address (rtx x)
{
  rtx addr = x;
  rtx reg = gen_reg_rtx (Pmode);
  rtx pat;
  int relax_group_id = nds32_alloc_relax_group_id ();

  if (GET_CODE (x) == LABEL_REF
      || (GET_CODE (x) == SYMBOL_REF
	  && (CONSTANT_POOL_ADDRESS_P (x)
	      || SYMBOL_REF_LOCAL_P (x))))
    {
      addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, x), UNSPEC_GOTOFF);
      addr = gen_rtx_CONST (SImode, addr);
      emit_insn (gen_sym_got (reg, addr, GEN_INT (relax_group_id)));
      x = gen_rtx_PLUS (Pmode, reg, pic_offset_table_rtx);
    }
  else if (GET_CODE (x) == SYMBOL_REF)
    {
      addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, x), UNSPEC_GOT);
      addr = gen_rtx_CONST (SImode, addr);
      emit_insn (gen_sym_got (reg, addr, GEN_INT (relax_group_id)));

      x = gen_const_mem (SImode, gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
					       reg));
    }
  else if (GET_CODE (x) == CONST)
    {
      /* We don't split constant in expand_pic_move because GOTOFF can combine
	 the addend with the symbol.  */
      addr = XEXP (x, 0);
      gcc_assert (GET_CODE (addr) == PLUS);

      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);

      if ((GET_CODE (op0) == LABEL_REF
	   || (GET_CODE (op0) == SYMBOL_REF
	       && (CONSTANT_POOL_ADDRESS_P (op0)
		   || SYMBOL_REF_LOCAL_P (op0))))
	  && GET_CODE (op1) == CONST_INT)
	{
	  pat = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0), UNSPEC_GOTOFF);
	  pat = gen_rtx_PLUS (Pmode, pat, op1);
	  pat = gen_rtx_CONST (Pmode, pat);
	  emit_insn (gen_sym_got (reg, pat, GEN_INT (relax_group_id)));
	  x = gen_rtx_PLUS (Pmode, reg, pic_offset_table_rtx);
	}
      else if (GET_CODE (op0) == SYMBOL_REF
	       && GET_CODE (op1) == CONST_INT)
	{
	  /* This is a constant offset from a @GOT symbol reference.  */
	  addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, op0), UNSPEC_GOT);
	  addr = gen_rtx_CONST (SImode, addr);
	  emit_insn (gen_sym_got (reg, addr, GEN_INT (relax_group_id)));

	  addr = gen_const_mem (SImode, gen_rtx_PLUS (Pmode,
						      pic_offset_table_rtx,
						      reg));
	  emit_move_insn (reg, addr);
	  if (satisfies_constraint_Is15 (op1))
	    x = gen_rtx_PLUS (Pmode, reg, op1);
	  else
	    {
	      rtx tmp_reg = gen_reg_rtx (SImode);
	      emit_insn (gen_movsi (tmp_reg, op1));
	      x = gen_rtx_PLUS (Pmode, reg, tmp_reg);
	    }
	}
      else
	{
	  /* Don't handle this pattern.  */
	  debug_rtx (x);
	  gcc_unreachable ();
	}
    }
  return x;
}

void
nds32_expand_pic_move (rtx *operands)
{
  rtx src;

  src = nds32_legitimize_pic_address (operands[1]);
  emit_move_insn (operands[0], src);
}

/* Expand ICT symbol.
    Example for @ICT and ICT model=large:

    la $r0, symbol@ICT
      -> sethi $rt, hi20(symbol@ICT)
	 lwi $r0, [$rt + lo12(symbol@ICT)]

*/
rtx
nds32_legitimize_ict_address (rtx x)
{
  rtx symbol = x;
  rtx addr = x;
  rtx reg = gen_reg_rtx (Pmode);
  gcc_assert (GET_CODE (x) == SYMBOL_REF
	      && nds32_indirect_call_referenced_p (x));

  addr = gen_rtx_UNSPEC (SImode, gen_rtvec (1, symbol), UNSPEC_ICT);
  addr = gen_rtx_CONST (SImode, addr);
  emit_insn (gen_sethi (reg, addr));

  x = gen_const_mem (SImode, gen_rtx_LO_SUM (Pmode, reg, addr));

  return x;
}

void
nds32_expand_ict_move (rtx *operands)
{
  rtx src = operands[1];

  src = nds32_legitimize_ict_address (src);

  emit_move_insn (operands[0], src);
}

/* Return true X is a indirect call symbol.  */
bool
nds32_indirect_call_referenced_p (rtx x)
{
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_ICT)
    x = XVECEXP (x, 0, 0);

  if (GET_CODE (x) == SYMBOL_REF)
    {
      tree decl = SYMBOL_REF_DECL (x);

      return decl
	     && (lookup_attribute("indirect_call",
				  DECL_ATTRIBUTES(decl))
		 != NULL);
    }

  return false;
}

/* Return true X is need use long call.  */
bool
nds32_long_call_p (rtx symbol)
{
  if (nds32_indirect_call_referenced_p (symbol))
    return TARGET_ICT_MODEL_LARGE;
  else
    return TARGET_CMODEL_LARGE;
}

/* Return true if X contains a thread-local symbol.  */
bool
nds32_tls_referenced_p (rtx x)
{
  if (!targetm.have_tls)
   return false;

  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
    x = XEXP (XEXP (x, 0), 0);

  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x))
    return true;

  return false;
}

/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  */
rtx
nds32_legitimize_tls_address (rtx x)
{
  rtx tmp_reg;
  rtx tp_reg = gen_rtx_REG (Pmode, TP_REGNUM);
  rtx pat, insns, reg0;
  int relax_group_id = nds32_alloc_relax_group_id ();

  if (GET_CODE (x) == SYMBOL_REF)
    switch (SYMBOL_REF_TLS_MODEL (x))
      {
      case TLS_MODEL_GLOBAL_DYNAMIC:
      case TLS_MODEL_LOCAL_DYNAMIC:
	/* Emit UNSPEC_TLS_DESC rather than expand rtl directly because spill
	   may destroy the define-use chain anylysis to insert relax_hint.  */
	if (SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_GLOBAL_DYNAMIC)
	  pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, x), UNSPEC_TLSGD);
	else
	  pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, x), UNSPEC_TLSLD);

	pat = gen_rtx_CONST (SImode, pat);
	reg0 = gen_rtx_REG (Pmode, 0);
	/* If we can confirm all clobber reigsters, it doesn't have to use call
	   instruction.  */
	insns = emit_call_insn (gen_tls_desc (pat, GEN_INT (relax_group_id)));
	use_reg (&CALL_INSN_FUNCTION_USAGE (insns), pic_offset_table_rtx);
	RTL_CONST_CALL_P (insns) = 1;
	tmp_reg = gen_reg_rtx (SImode);
	emit_move_insn (tmp_reg, reg0);
	x = tmp_reg;
	break;

      case TLS_MODEL_INITIAL_EXEC:
	pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, x), UNSPEC_TLSIE);
	tmp_reg  = gen_reg_rtx (SImode);
	pat = gen_rtx_CONST (SImode, pat);
	emit_insn (gen_tls_ie (tmp_reg, pat, GEN_INT (relax_group_id)));
	if (flag_pic)
	  emit_use (pic_offset_table_rtx);
	x = gen_rtx_PLUS (Pmode, tmp_reg, tp_reg);
	break;

      case TLS_MODEL_LOCAL_EXEC:
	/* Expand symbol_ref@TPOFF':
	     sethi $ta, hi20(symbol_ref@TPOFF)
	     ori   $ta, $ta, lo12(symbol_ref@TPOFF)
	     add   $r0, $ta, $tp */
	tmp_reg  = gen_reg_rtx (SImode);
	pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, x), UNSPEC_TLSLE);
	pat = gen_rtx_CONST (SImode, pat);
	emit_insn (gen_tls_le (tmp_reg, pat, GEN_INT (relax_group_id)));
	x = gen_rtx_PLUS (Pmode, tmp_reg, tp_reg);
	break;

      default:
	gcc_unreachable ();
      }
  else if (GET_CODE (x) == CONST)
    {
      rtx base, addend;
      split_const (x, &base, &addend);

      if (SYMBOL_REF_TLS_MODEL (base) == TLS_MODEL_LOCAL_EXEC)
	{
	  /* Expand symbol_ref@TPOFF':
	     sethi $ta, hi20(symbol_ref@TPOFF + addend)
	     ori   $ta, $ta, lo12(symbol_ref@TPOFF + addend)
	     add   $r0, $ta, $tp */
	  tmp_reg  = gen_reg_rtx (SImode);
	  pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, base), UNSPEC_TLSLE);
	  pat = gen_rtx_PLUS (SImode, pat, addend);
	  pat = gen_rtx_CONST (SImode, pat);
	  emit_insn (gen_tls_le (tmp_reg, pat, GEN_INT (relax_group_id)));
	  x = gen_rtx_PLUS (Pmode, tmp_reg, tp_reg);
	}
    }

  return x;
}

void
nds32_expand_tls_move (rtx *operands)
{
  rtx src = operands[1];
  rtx base, addend;

  if (CONSTANT_P (src))
    split_const (src, &base, &addend);

  if (SYMBOL_REF_TLS_MODEL (base) == TLS_MODEL_LOCAL_EXEC)
    src = nds32_legitimize_tls_address (src);
  else
    {
      src = nds32_legitimize_tls_address (base);
      if (addend != const0_rtx)
	{
	  src = gen_rtx_PLUS (SImode, src, addend);
	  src = force_operand (src, operands[0]);
	}
    }

  emit_move_insn (operands[0], src);
}

void
nds32_expand_constant (machine_mode mode, HOST_WIDE_INT val,
		       rtx target, rtx source)
{
  rtx temp = gen_reg_rtx (mode);
  int clear_sign_bit_copies = 0;
  int clear_zero_bit_copies = 0;
  unsigned HOST_WIDE_INT remainder = val & 0xffffffffUL;

  /* Count number of leading zeros.  */
  clear_sign_bit_copies =  __builtin_clz (remainder);
  /* Count number of trailing zeros.  */
  clear_zero_bit_copies = __builtin_ctz (remainder);

  HOST_WIDE_INT sign_shift_mask = ((0xffffffffUL
				    << (32 - clear_sign_bit_copies))
				   & 0xffffffffUL);
  HOST_WIDE_INT zero_shift_mask = (1 << clear_zero_bit_copies) - 1;

  if (clear_sign_bit_copies > 0 && clear_sign_bit_copies < 17
      && (remainder | sign_shift_mask) == 0xffffffffUL)
    {
      /* Transfer AND to two shifts, example:
	 a = b & 0x7fffffff => (b << 1) >> 1 */
      rtx shift = GEN_INT (clear_sign_bit_copies);

      emit_insn (gen_ashlsi3 (temp, source, shift));
      emit_insn (gen_lshrsi3 (target, temp, shift));
    }
  else if (clear_zero_bit_copies > 0 && clear_sign_bit_copies < 17
	   && (remainder | zero_shift_mask) == 0xffffffffUL)
    {
      /* Transfer AND to two shifts, example:
	 a = b & 0xfff00000 => (b >> 20) << 20 */
      rtx shift = GEN_INT (clear_zero_bit_copies);

      emit_insn (gen_lshrsi3 (temp, source, shift));
      emit_insn (gen_ashlsi3 (target, temp, shift));
    }
  else
    {
      emit_move_insn (temp, GEN_INT (val));
      emit_move_insn (target, gen_rtx_fmt_ee (AND, mode, source, temp));
    }
}

/* Auxiliary functions for lwm/smw.  */
bool
nds32_valid_smw_lwm_base_p (rtx op)
{
  rtx base_addr;

  if (!MEM_P (op))
    return false;

  base_addr = XEXP (op, 0);

  if (REG_P (base_addr))
    return true;
  else
    {
      if (GET_CODE (base_addr) == POST_INC
	  && REG_P (XEXP (base_addr, 0)))
	return true;
    }

  return false;
}

/* Auxiliary functions for manipulation DI mode.  */
rtx nds32_di_high_part_subreg(rtx reg)
{
  unsigned high_part_offset = subreg_highpart_offset (SImode, DImode);

  return simplify_gen_subreg (
	   SImode, reg,
	   DImode, high_part_offset);
}

rtx nds32_di_low_part_subreg(rtx reg)
{
  unsigned low_part_offset = subreg_lowpart_offset (SImode, DImode);

  return simplify_gen_subreg (
	   SImode, reg,
	   DImode, low_part_offset);
}

/* ------------------------------------------------------------------------ */

/* Auxiliary function for output TLS patterns.  */

const char *
nds32_output_tls_desc (rtx *operands)
{
  char pattern[1000];

  if (TARGET_RELAX_HINT)
    snprintf (pattern, sizeof (pattern),
	      ".relax_hint %%1\n\tsethi $r0, hi20(%%0)\n\t"
	      ".relax_hint %%1\n\tori $r0, $r0, lo12(%%0)\n\t"
	      ".relax_hint %%1\n\tlw $r15, [$r0 + $gp]\n\t"
	      ".relax_hint %%1\n\tadd $r0, $r0, $gp\n\t"
	      ".relax_hint %%1\n\tjral $r15");
  else
    snprintf (pattern, sizeof (pattern),
	      "sethi $r0, hi20(%%0)\n\t"
	      "ori $r0, $r0, lo12(%%0)\n\t"
	      "lw $r15, [$r0 + $gp]\n\t"
	      "add $r0, $r0, $gp\n\t"
	      "jral $r15");
  output_asm_insn (pattern, operands);
  return "";
}

const char *
nds32_output_tls_ie (rtx *operands)
{
  char pattern[1000];

  if (flag_pic)
  {
      if (TARGET_RELAX_HINT)
	snprintf (pattern, sizeof (pattern),
		  ".relax_hint %%2\n\tsethi %%0, hi20(%%1)\n\t"
		  ".relax_hint %%2\n\tori %%0, %%0, lo12(%%1)\n\t"
		  ".relax_hint %%2\n\tlw %%0, [%%0 + $gp]");
      else
	snprintf (pattern, sizeof (pattern),
		  "sethi %%0, hi20(%%1)\n\t"
		  "ori %%0, %%0, lo12(%%1)\n\t"
		  "lw %%0, [%%0 + $gp]");
  }
  else
    {
      if (TARGET_RELAX_HINT)
	snprintf (pattern, sizeof (pattern),
		  ".relax_hint %%2\n\tsethi %%0, hi20(%%1)\n\t"
		  ".relax_hint %%2\n\tlwi %%0, [%%0 + lo12(%%1)]");
      else
	snprintf (pattern, sizeof (pattern),
		  "sethi %%0, hi20(%%1)\n\t"
		  "lwi %%0, [%%0 + lo12(%%1)]");
    }
  output_asm_insn (pattern, operands);
  return "";
}

const char *
nds32_output_symrel (rtx *operands)
{
  char pattern[1000];

  if (TARGET_RELAX_HINT)
    snprintf (pattern, sizeof (pattern),
	      ".relax_hint %%2\n\tsethi %%0, hi20(%%1)\n\t"
	      ".relax_hint %%2\n\tori %%0, %%0, lo12(%%1)");
  else
    snprintf (pattern, sizeof (pattern),
	      "sethi %%0, hi20(%%1)\n\t"
	      "ori %%0, %%0, lo12(%%1)");

  output_asm_insn (pattern, operands);
  return "";
}
