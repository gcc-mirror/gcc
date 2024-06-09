/* Subroutines used for calculate rtx costs of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
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
#include "tm-constrs.h"
#include "tree-pass.h"

/* ------------------------------------------------------------------------ */

typedef bool (*rtx_cost_func) (rtx, int, int, int, int*);

struct rtx_cost_model_t {
  rtx_cost_func speed_prefer;
  rtx_cost_func size_prefer;
};

static rtx_cost_model_t rtx_cost_model;

static int insn_size_16bit; /* Initial at nds32_init_rtx_costs.  */
static const int insn_size_32bit = 4;

static bool
nds32_rtx_costs_speed_prefer (rtx x ATTRIBUTE_UNUSED,
			      int code,
			      int outer_code ATTRIBUTE_UNUSED,
			      int opno ATTRIBUTE_UNUSED,
			      int *total)
{
  rtx op0;
  rtx op1;
  machine_mode mode = GET_MODE (x);
  /* Scale cost by mode size.  */
  int cost = COSTS_N_INSNS (GET_MODE_SIZE (mode) / GET_MODE_SIZE (SImode));

  switch (code)
    {
    case USE:
      /* Used in combine.cc as a marker.  */
      *total = 0;
      return true;

    case CONST_INT:
      /* When not optimizing for size, we care more about the cost
	 of hot code, and hot code is often in a loop.  If a constant
	 operand needs to be forced into a register, we will often be
	 able to hoist the constant load out of the loop, so the load
	 should not contribute to the cost.  */
      if (outer_code == SET || outer_code == PLUS)
	*total = satisfies_constraint_Is20 (x) ? 0 : 4;
      else if (outer_code == AND || outer_code == IOR || outer_code == XOR
	       || outer_code == MINUS)
	*total = satisfies_constraint_Iu15 (x) ? 0 : 4;
      else if (outer_code == ASHIFT || outer_code == ASHIFTRT
	       || outer_code == LSHIFTRT)
	*total = satisfies_constraint_Iu05 (x) ? 0 : 4;
      else if (GET_RTX_CLASS (outer_code) == RTX_COMPARE
	       || GET_RTX_CLASS (outer_code) == RTX_COMM_COMPARE)
	*total = satisfies_constraint_Is16 (x) ? 0 : 4;
      else
	*total = COSTS_N_INSNS (1);
      return true;

    case CONST:
    case LO_SUM:
    case HIGH:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (1);
      return true;

    case MEM:
      *total = COSTS_N_INSNS (1);
      return true;

    case SET:
      op0 = SET_DEST (x);
      op1 = SET_SRC (x);
      mode = GET_MODE (op0);
      /* Scale cost by mode size.  */
      cost = COSTS_N_INSNS (GET_MODE_SIZE (mode) / GET_MODE_SIZE (SImode));

      switch (GET_CODE (op1))
	{
	case REG:
	case SUBREG:
	  /* Register move and Store instructions.  */
	  if ((REG_P (op0) || MEM_P (op0))
	      && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (DImode))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = cost;
	  return true;

	case MEM:
	  /* Load instructions.  */
	  if (REG_P (op0) && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (DImode))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = cost;
	  return true;

	case CONST_INT:
	  /* movi instruction.  */
	  if (REG_P (op0) && GET_MODE_SIZE (mode) < GET_MODE_SIZE (DImode))
	    {
	      if (satisfies_constraint_Is20 (op1))
		*total = COSTS_N_INSNS (1) - 1;
	      else
		*total = COSTS_N_INSNS (2);
	    }
	  else
	    *total = cost;
	  return true;

	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  /* la instruction.  */
	  if (REG_P (op0) && GET_MODE_SIZE (mode) < GET_MODE_SIZE (DImode))
	    *total = COSTS_N_INSNS (1) - 1;
	  else
	    *total = cost;
	  return true;
	case VEC_SELECT:
	  *total = cost;
	  return true;

	default:
	  *total = cost;
	  return true;
	}

    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if (GET_CODE (op0) == MULT || GET_CODE (op0) == LSHIFTRT
	       || GET_CODE (op1) == MULT || GET_CODE (op1) == LSHIFTRT)
	/* ALU_SHIFT */
	*total = COSTS_N_INSNS (2);

      else if ((GET_CODE (op1) == CONST_INT
		&& satisfies_constraint_Is15 (op1))
		|| REG_P (op1))
	/* ADD instructions */
	*total = COSTS_N_INSNS (1);
      else
	/* ADD instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);
      return true;

    case MINUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if (GET_CODE (op0) == MULT || GET_CODE (op0) == LSHIFTRT
	       || GET_CODE (op1) == MULT || GET_CODE (op1) == LSHIFTRT)
	/* ALU_SHIFT */
	*total = COSTS_N_INSNS (2);
      else if ((GET_CODE (op0) == CONST_INT
		&& satisfies_constraint_Is15 (op0))
		|| REG_P (op0))
	/* SUB instructions */
	*total = COSTS_N_INSNS (1);
      else
	/* SUB instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);
      return true;

    case TRUNCATE:
      /* TRUNCATE and AND behavior is same. */
      *total = COSTS_N_INSNS (1);
      return true;

    case AND:
    case IOR:
    case XOR:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (NDS32_EXT_DSP_P ())
	{
	  /* We prefer (and (ior) (ior)) than (ior (and) (and)) for
	     synthetize pk** and insb instruction.  */
	  if (code == AND && GET_CODE (op0) == IOR && GET_CODE (op1) == IOR)
	    return COSTS_N_INSNS (1);

	  if (code == IOR && GET_CODE (op0) == AND && GET_CODE (op1) == AND)
	    return COSTS_N_INSNS (10);
	}

      if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if (GET_CODE (op0) == ASHIFT || GET_CODE (op0) == LSHIFTRT)
	*total = COSTS_N_INSNS (2);
      else if ((GET_CODE (op1) == CONST_INT
	       && satisfies_constraint_Iu15 (op1))
	       || REG_P (op1))
	/* AND, OR, XOR instructions */
	*total = COSTS_N_INSNS (1);
      else if (code == AND || GET_CODE (op0) == NOT)
	/* BITC instruction */
	*total = COSTS_N_INSNS (1);
      else
	/* AND, OR, XOR instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);
      return true;

    case MULT:
      if (GET_MODE (x) == DImode
	  || GET_CODE (XEXP (x, 1)) == SIGN_EXTEND
	  || GET_CODE (XEXP (x, 1)) == ZERO_EXTEND)
	/* MUL instructions */
	*total = COSTS_N_INSNS (1);
      else if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if (outer_code == PLUS || outer_code == MINUS)
	*total = COSTS_N_INSNS (2);
      else if ((GET_CODE (XEXP (x, 1)) == CONST_INT
	       && satisfies_constraint_Iu05 (XEXP (x, 1)))
	       || REG_P (XEXP (x, 1)))
	/* MUL instructions */
	*total = COSTS_N_INSNS (1);
      else
	/* MUL instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);

      if (TARGET_MUL_SLOW)
	*total += COSTS_N_INSNS (4);

      return true;

    case LSHIFTRT:
      if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if (outer_code == PLUS || outer_code == MINUS
	       || outer_code == AND || outer_code == IOR
	       || outer_code == XOR)
	*total = COSTS_N_INSNS (2);
      else if ((GET_CODE (XEXP (x, 1)) == CONST_INT
	       && satisfies_constraint_Iu05 (XEXP (x, 1)))
	       || REG_P (XEXP (x, 1)))
	/* SRL instructions */
	*total = COSTS_N_INSNS (1);
      else
	/* SRL instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);
      return true;

    case ASHIFT:
      if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if (outer_code == AND || outer_code == IOR
	       || outer_code == XOR)
	*total = COSTS_N_INSNS (2);
      else if ((GET_CODE (XEXP (x, 1)) == CONST_INT
	       && satisfies_constraint_Iu05 (XEXP (x, 1)))
	       || REG_P (XEXP (x, 1)))
	/* SLL instructions */
	*total = COSTS_N_INSNS (1);
      else
	/* SLL instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);
      return true;

    case ASHIFTRT:
    case ROTATERT:
      if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (DImode))
	*total = cost;
      else if ((GET_CODE (XEXP (x, 1)) == CONST_INT
	       && satisfies_constraint_Iu05 (XEXP (x, 1)))
	       || REG_P (XEXP (x, 1)))
	/* ROTR, SLL instructions */
	*total = COSTS_N_INSNS (1);
      else
	/* ROTR, SLL instructions: IMM out of range.  */
	*total = COSTS_N_INSNS (2);
      return true;

    case LT:
    case LTU:
      if (outer_code == SET)
	{
	  if ((GET_CODE (XEXP (x, 1)) == CONST_INT
	      && satisfies_constraint_Iu15 (XEXP (x, 1)))
	      || REG_P (XEXP (x, 1)))
	    /* SLT, SLTI instructions */
	    *total = COSTS_N_INSNS (1);
	  else
	    /* SLT, SLT instructions: IMM out of range.  */
	    *total = COSTS_N_INSNS (2);
	}
      else
	/* branch */
	*total = COSTS_N_INSNS (2);
      return true;

    case EQ:
    case NE:
    case GE:
    case LE:
    case GT:
      /* branch */
      *total = COSTS_N_INSNS (2);
      return true;

    case IF_THEN_ELSE:
      if (GET_CODE (XEXP (x, 1)) == LABEL_REF)
	/* branch */
	*total = COSTS_N_INSNS (2);
      else
	/* cmovz, cmovn instructions */
	*total = COSTS_N_INSNS (1);
      return true;

    case LABEL_REF:
      if (outer_code == IF_THEN_ELSE)
	/* branch */
	*total = COSTS_N_INSNS (2);
      else
	*total = COSTS_N_INSNS (1);
      return true;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	/* Using memory access. */
	*total = COSTS_N_INSNS (1);
      else
	/* Zero extend and sign extend instructions.  */
	*total = COSTS_N_INSNS (1);
      return true;

    case NEG:
    case NOT:
      *total = COSTS_N_INSNS (1);
      return true;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (20);
      return true;

    case CALL:
      *total = COSTS_N_INSNS (2);
      return true;

    case CLZ:
    case SMIN:
    case SMAX:
    case ZERO_EXTRACT:
      if (TARGET_EXT_PERF)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (3);
      return true;
    case VEC_SELECT:
      *total = COSTS_N_INSNS (1);
      return true;

    default:
      *total = COSTS_N_INSNS (3);
      return true;
    }
}

static bool
nds32_rtx_costs_size_prefer (rtx x,
			     int code,
			     int outer_code,
			     int opno ATTRIBUTE_UNUSED,
			     int *total)
{
  /* In gcc/rtl.h, the default value of COSTS_N_INSNS(N) is N*4.
     We treat it as 4-byte cost for each instruction
     under code size consideration.  */
  switch (code)
    {
    case SET:
      /* For 'SET' rtx, we need to return false
	 so that it can recursively calculate costs.  */
      return false;

    case USE:
      /* Used in combine.cc as a marker.  */
      *total = 0;
      break;

    case CONST_INT:
      /* All instructions involving constant operation
	 need to be considered for cost evaluation.  */
      if (outer_code == SET)
	{
	  /* (set X imm5s), use movi55, 2-byte cost.
	     (set X imm20s), use movi, 4-byte cost.
	     (set X BIG_INT), use sethi/ori, 8-byte cost.  */
	  if (satisfies_constraint_Is05 (x))
	    *total = insn_size_16bit;
	  else if (satisfies_constraint_Is20 (x))
	    *total = insn_size_32bit;
	  else
	    *total = insn_size_32bit * 2;
	}
      else if (outer_code == PLUS || outer_code == MINUS)
	{
	  /* Possible addi333/subi333 or subi45/addi45, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu05 (x))
	    *total = insn_size_16bit;
	  else
	    *total = insn_size_32bit;
	}
      else if (outer_code == ASHIFT)
	{
	  /* Possible slli333, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu03 (x))
	    *total = insn_size_16bit;
	  else
	    *total = insn_size_32bit;
	}
      else if (outer_code == ASHIFTRT || outer_code == LSHIFTRT)
	{
	  /* Possible srai45 or srli45, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu05 (x))
	    *total = insn_size_16bit;
	  else
	    *total = insn_size_32bit;
	}
      else
	{
	  /* For other cases, simply set it 4-byte cost.  */
	  *total = insn_size_32bit;
	}
      break;

    case CONST_DOUBLE:
      /* It requires high part and low part processing, set it 8-byte cost.  */
      *total = insn_size_32bit * 2;
      break;

    case CONST:
    case SYMBOL_REF:
      *total = insn_size_32bit * 2;
      break;

    default:
      /* For other cases, generally we set it 4-byte cost
	 and stop resurively traversing.  */
      *total = insn_size_32bit;
      break;
    }

  return true;
}

void
nds32_init_rtx_costs (void)
{
  rtx_cost_model.speed_prefer = nds32_rtx_costs_speed_prefer;
  rtx_cost_model.size_prefer  = nds32_rtx_costs_size_prefer;

  if (TARGET_16_BIT)
    insn_size_16bit = 2;
  else
    insn_size_16bit = 4;
}

/* This target hook describes the relative costs of RTL expressions.
   Return 'true' when all subexpressions of x have been processed.
   Return 'false' to sum the costs of sub-rtx, plus cost of this operation.
   Refer to gcc/rtlanal.cc for more information.  */
bool
nds32_rtx_costs_impl (rtx x,
		      machine_mode mode ATTRIBUTE_UNUSED,
		      int outer_code,
		      int opno,
		      int *total,
		      bool speed)
{
  int code = GET_CODE (x);

  /* According to 'speed', use suitable cost model section.  */
  if (speed)
    return rtx_cost_model.speed_prefer(x, code, outer_code, opno, total);
  else
    return rtx_cost_model.size_prefer(x, code, outer_code, opno, total);
}


int nds32_address_cost_speed_prefer (rtx address)
{
  rtx plus0, plus1;
  enum rtx_code code;

  code = GET_CODE (address);

  switch (code)
    {
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      /* We encourage that rtx contains
	 POST_MODIFY/POST_INC/POST_DEC behavior.  */
      return COSTS_N_INSNS (1) - 2;

    case SYMBOL_REF:
      /* We can have gp-relative load/store for symbol_ref.
	Have it 4-byte cost.  */
      return COSTS_N_INSNS (2);

    case CONST:
      /* It is supposed to be the pattern (const (plus symbol_ref const_int)).
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (2);

    case REG:
      /* Simply return 4-byte costs.  */
      return COSTS_N_INSNS (1) - 2;

    case PLUS:
      /* We do not need to check if the address is a legitimate address,
	 because this hook is never called with an invalid address.
	 But we better check the range of
	 const_int value for cost, if it exists.  */
      plus0 = XEXP (address, 0);
      plus1 = XEXP (address, 1);

      if (REG_P (plus0) && CONST_INT_P (plus1))
	return COSTS_N_INSNS (1) - 2;
      else if (ARITHMETIC_P (plus0) || ARITHMETIC_P (plus1))
	return COSTS_N_INSNS (1) - 1;
      else if (REG_P (plus0) && REG_P (plus1))
	return COSTS_N_INSNS (1);

      /* For other 'plus' situation, make it cost 4-byte.  */
      return COSTS_N_INSNS (1);

    default:
      break;
    }

  return COSTS_N_INSNS (4);

}

int nds32_address_cost_speed_fwprop (rtx address)
{
  rtx plus0, plus1;
  enum rtx_code code;

  code = GET_CODE (address);

  switch (code)
    {
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      /* We encourage that rtx contains
	 POST_MODIFY/POST_INC/POST_DEC behavior.  */
      return 0;

    case SYMBOL_REF:
      /* We can have gp-relative load/store for symbol_ref.
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (2);

    case CONST:
      /* It is supposed to be the pattern (const (plus symbol_ref const_int)).
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (2);

    case REG:
      /* Simply return 4-byte costs.  */
      return COSTS_N_INSNS (1);

    case PLUS:
      /* We do not need to check if the address is a legitimate address,
	 because this hook is never called with an invalid address.
	 But we better check the range of
	 const_int value for cost, if it exists.  */
      plus0 = XEXP (address, 0);
      plus1 = XEXP (address, 1);

      if (REG_P (plus0) && CONST_INT_P (plus1))
	{
	  /* If it is possible to be lwi333/swi333 form,
	     make it 2-byte cost.  */
	  if (satisfies_constraint_Iu03 (plus1))
	    return (COSTS_N_INSNS (1) - 2);
	  else
	    return COSTS_N_INSNS (1);
	}
      if (ARITHMETIC_P (plus0) || ARITHMETIC_P (plus1))
	return COSTS_N_INSNS (1) - 2;
      else if (REG_P (plus0) && REG_P (plus1))
	return COSTS_N_INSNS (1);

      /* For other 'plus' situation, make it cost 4-byte.  */
      return COSTS_N_INSNS (1);

    default:
      break;
    }

  return COSTS_N_INSNS (4);
}


int nds32_address_cost_size_prefer (rtx address)
{
  rtx plus0, plus1;
  enum rtx_code code;

  code = GET_CODE (address);

  switch (code)
    {
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      /* We encourage that rtx contains
	 POST_MODIFY/POST_INC/POST_DEC behavior.  */
      return 0;

    case SYMBOL_REF:
      /* We can have gp-relative load/store for symbol_ref.
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (2);

    case CONST:
      /* It is supposed to be the pattern (const (plus symbol_ref const_int)).
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (2);

    case REG:
      /* Simply return 4-byte costs.  */
      return COSTS_N_INSNS (1) - 1;

    case PLUS:
      /* We do not need to check if the address is a legitimate address,
	 because this hook is never called with an invalid address.
	 But we better check the range of
	 const_int value for cost, if it exists.  */
      plus0 = XEXP (address, 0);
      plus1 = XEXP (address, 1);

      if (REG_P (plus0) && CONST_INT_P (plus1))
	{
	  /* If it is possible to be lwi333/swi333 form,
	     make it 2-byte cost.  */
	  if (satisfies_constraint_Iu03 (plus1))
	    return (COSTS_N_INSNS (1) - 2);
	  else
	    return COSTS_N_INSNS (1) - 1;
	}

      /* (plus (reg) (mult (reg) (const))) */
      if (ARITHMETIC_P (plus0) || ARITHMETIC_P (plus1))
	return (COSTS_N_INSNS (1) - 1);

      /* For other 'plus' situation, make it cost 4-byte.  */
      return COSTS_N_INSNS (1);

    default:
      break;
    }

  return COSTS_N_INSNS (4);

}

int nds32_address_cost_impl (rtx address,
			     machine_mode mode ATTRIBUTE_UNUSED,
			     addr_space_t as ATTRIBUTE_UNUSED,
			     bool speed_p)
{
  if (speed_p)
    {
      if (current_pass->tv_id == TV_FWPROP)
	return nds32_address_cost_speed_fwprop (address);
      else
	return nds32_address_cost_speed_prefer (address);
    }
  else
    return nds32_address_cost_size_prefer (address);
}

/* ------------------------------------------------------------------------ */
