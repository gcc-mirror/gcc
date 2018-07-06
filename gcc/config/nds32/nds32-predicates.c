/* Predicate functions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
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
#include "emit-rtl.h"
#include "recog.h"
#include "tm-constrs.h"
#include "insn-attr.h"

/* ------------------------------------------------------------------------ */

/* A subroutine that checks multiple load and store
   using consecutive registers.
     OP is a parallel rtx we would like to check.
     LOAD_P indicates whether we are checking load operation.
     PAR_INDEX is starting element of parallel rtx.
     FIRST_ELT_REGNO is used to tell starting register number.
     COUNT helps us to check consecutive register numbers.  */
static bool
nds32_consecutive_registers_load_store_p (rtx op,
					  bool load_p,
					  int par_index,
					  int first_elt_regno,
					  int count)
{
  int i;
  int check_regno;
  rtx elt;
  rtx elt_reg;
  rtx elt_mem;

  for (i = 0; i < count; i++)
    {
      /* Pick up each element from parallel rtx.  */
      elt = XVECEXP (op, 0, i + par_index);

      /* If this element is not a 'set' rtx, return false immediately.  */
      if (GET_CODE (elt) != SET)
	return false;

      /* Pick up reg and mem of this element.  */
      elt_reg = load_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_mem = load_p ? SET_SRC (elt) : SET_DEST (elt);

      /* If elt_reg is not a expected reg rtx, return false.  */
      if (GET_CODE (elt_reg) != REG || GET_MODE (elt_reg) != SImode)
	return false;
      /* If elt_mem is not a expected mem rtx, return false.  */
      if (GET_CODE (elt_mem) != MEM || GET_MODE (elt_mem) != SImode)
	return false;

      /* The consecutive registers should be in (Rb,Rb+1...Re) order.  */
      check_regno = first_elt_regno + i;

      /* If the register number is not continuous, return false.  */
      if (REGNO (elt_reg) != (unsigned int) check_regno)
	return false;
    }

  return true;
}

/* Function to check whether the OP is a valid load/store operation.
   This is a helper function for the predicates:
   'nds32_load_multiple_operation' and 'nds32_store_multiple_operation'
   in predicates.md file.

   The OP is supposed to be a parallel rtx.
   For each element within this parallel rtx:
     (set (reg) (mem addr)) is the form for load operation.
     (set (mem addr) (reg)) is the form for store operation.
   We have to extract reg and mem of every element and
   check if the information is valid for multiple load/store operation.  */
bool
nds32_valid_multiple_load_store_p (rtx op, bool load_p, bool bim_p)
{
  int count;
  int first_elt_regno;
  int update_base_elt_idx;
  int offset;
  rtx elt;
  rtx update_base;

  /* Get the counts of elements in the parallel rtx.
     Last one is update base register if bim_p.
     and pick up the first element.  */
  if (bim_p)
    {
      count = XVECLEN (op, 0) - 1;
      elt = XVECEXP (op, 0, 1);
    }
  else
    {
      count = XVECLEN (op, 0);
      elt = XVECEXP (op, 0, 0);
    }

  /* Perform some quick check for the first element in the parallel rtx.  */
  if (GET_CODE (elt) != SET
      || count <= 1
      || count > 25)
    return false;

  /* Pick up regno of first element for further detail checking.
     Note that the form is different between load and store operation.  */
  if (load_p)
    {
      if (GET_CODE (SET_DEST (elt)) != REG
	  || GET_CODE (SET_SRC (elt)) != MEM)
	return false;

      first_elt_regno = REGNO (SET_DEST (elt));
    }
  else
    {
      if (GET_CODE (SET_SRC (elt)) != REG
	  || GET_CODE (SET_DEST (elt)) != MEM)
	return false;

      first_elt_regno = REGNO (SET_SRC (elt));
    }

  /* Perform detail check for each element.
     Refer to nds32-multiple.md for more information
     about following checking.
     The starting element of parallel rtx is index 0.  */
  if (!nds32_consecutive_registers_load_store_p (op, load_p, bim_p ? 1 : 0,
						 first_elt_regno,
						 count))
    return false;

  if (bim_p)
    {
      update_base_elt_idx = 0;
      update_base = XVECEXP (op, 0, update_base_elt_idx);
      if (!REG_P (SET_DEST (update_base)))
	return false;
      if (GET_CODE (SET_SRC (update_base)) != PLUS)
	return false;
      else
	{
	  offset = count * UNITS_PER_WORD;
	  elt = XEXP (SET_SRC (update_base), 1);
	  if (GET_CODE (elt) != CONST_INT
	      || (INTVAL (elt) != offset))
	    return false;
	}
    }

  /* Pass all test, this is a valid rtx.  */
  return true;
}

/* Function to check whether the OP is a valid stack push/pop operation.
   For a valid stack operation, it must satisfy following conditions:
     1. Consecutive registers push/pop operations.
     2. Valid $fp/$gp/$lp push/pop operations.
     3. The last element must be stack adjustment rtx.
   See the prologue/epilogue implementation for details.  */
bool
nds32_valid_stack_push_pop_p (rtx op, bool push_p)
{
  int index;
  int total_count;
  int rest_count;
  int first_regno;
  int save_fp, save_gp, save_lp;
  rtx elt;
  rtx elt_reg;
  rtx elt_mem;
  rtx elt_plus;

  /* Get the counts of elements in the parallel rtx.  */
  total_count = XVECLEN (op, 0);

  /* Perform some quick check for that every element should be 'set'.  */
  for (index = 0; index < total_count; index++)
    {
      elt = XVECEXP (op, 0, index);
      if (GET_CODE (elt) != SET)
	return false;
    }

  /* For push operation, the parallel rtx looks like:
     (parallel [(set (mem (plus (reg:SI SP_REGNUM) (const_int -32)))
		     (reg:SI Rb))
		(set (mem (plus (reg:SI SP_REGNUM) (const_int -28)))
		     (reg:SI Rb+1))
		...
		(set (mem (plus (reg:SI SP_REGNUM) (const_int -16)))
		     (reg:SI Re))
		(set (mem (plus (reg:SI SP_REGNUM) (const_int -12)))
		     (reg:SI FP_REGNUM))
		(set (mem (plus (reg:SI SP_REGNUM) (const_int -8)))
		     (reg:SI GP_REGNUM))
		(set (mem (plus (reg:SI SP_REGNUM) (const_int -4)))
		     (reg:SI LP_REGNUM))
		(set (reg:SI SP_REGNUM)
		     (plus (reg:SI SP_REGNUM) (const_int -32)))])

     For pop operation, the parallel rtx looks like:
     (parallel [(set (reg:SI Rb)
		     (mem (reg:SI SP_REGNUM)))
		(set (reg:SI Rb+1)
		     (mem (plus (reg:SI SP_REGNUM) (const_int 4))))
		...
		(set (reg:SI Re)
		     (mem (plus (reg:SI SP_REGNUM) (const_int 16))))
		(set (reg:SI FP_REGNUM)
		     (mem (plus (reg:SI SP_REGNUM) (const_int 20))))
		(set (reg:SI GP_REGNUM)
		     (mem (plus (reg:SI SP_REGNUM) (const_int 24))))
		(set (reg:SI LP_REGNUM)
		     (mem (plus (reg:SI SP_REGNUM) (const_int 28))))
		(set (reg:SI SP_REGNUM)
		     (plus (reg:SI SP_REGNUM) (const_int 32)))]) */

  /* 1. Consecutive registers push/pop operations.
	We need to calculate how many registers should be consecutive.
	The $sp adjustment rtx, $fp push rtx, $gp push rtx,
	and $lp push rtx are excluded.  */

  /* Detect whether we have $fp, $gp, or $lp in the parallel rtx.  */
  save_fp = reg_mentioned_p (gen_rtx_REG (SImode, FP_REGNUM), op);
  save_gp = reg_mentioned_p (gen_rtx_REG (SImode, GP_REGNUM), op);
  save_lp = reg_mentioned_p (gen_rtx_REG (SImode, LP_REGNUM), op);
  /* Exclude last $sp adjustment rtx.  */
  rest_count = total_count - 1;
  /* Exclude $fp, $gp, and $lp if they are in the parallel rtx.  */
  if (save_fp)
    rest_count--;
  if (save_gp)
    rest_count--;
  if (save_lp)
    rest_count--;

  if (rest_count > 0)
    {
      elt = XVECEXP (op, 0, 0);
      /* Pick up register element.  */
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      first_regno = REGNO (elt_reg);

      /* The 'push' operation is a kind of store operation.
	 The 'pop' operation is a kind of load operation.
	 Pass corresponding false/true as second argument (bool load_p).
	 The par_index is supposed to start with index 0.  */
      if (!nds32_consecutive_registers_load_store_p (op,
						     !push_p ? true : false,
						     0,
						     first_regno,
						     rest_count))
	return false;
    }

  /* 2. Valid $fp/$gp/$lp push/pop operations.
	Remember to set start index for checking them.  */

  /* The rest_count is the start index for checking $fp/$gp/$lp.  */
  index = rest_count;
  /* If index < 0, this parallel rtx is definitely
     not a valid stack push/pop operation.  */
  if (index < 0)
    return false;

  /* Check $fp/$gp/$lp one by one.
     We use 'push_p' to pick up reg rtx and mem rtx.  */
  if (save_fp)
    {
      elt = XVECEXP (op, 0, index);
      elt_mem = push_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      index++;

      if (GET_CODE (elt_mem) != MEM
	  || GET_CODE (elt_reg) != REG
	  || REGNO (elt_reg) != FP_REGNUM)
	return false;
    }
  if (save_gp)
    {
      elt = XVECEXP (op, 0, index);
      elt_mem = push_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      index++;

      if (GET_CODE (elt_mem) != MEM
	  || GET_CODE (elt_reg) != REG
	  || REGNO (elt_reg) != GP_REGNUM)
	return false;
    }
  if (save_lp)
    {
      elt = XVECEXP (op, 0, index);
      elt_mem = push_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      index++;

      if (GET_CODE (elt_mem) != MEM
	  || GET_CODE (elt_reg) != REG
	  || REGNO (elt_reg) != LP_REGNUM)
	return false;
    }

  /* 3. The last element must be stack adjustment rtx.
	Its form of rtx should be:
	  (set (reg:SI SP_REGNUM)
	       (plus (reg:SI SP_REGNUM) (const_int X)))
	The X could be positive or negative value.  */

  /* Pick up the last element.  */
  elt = XVECEXP (op, 0, total_count - 1);

  /* Extract its destination and source rtx.  */
  elt_reg  = SET_DEST (elt);
  elt_plus = SET_SRC (elt);

  /* Check this is (set (stack_reg) (plus stack_reg const)) pattern.  */
  if (GET_CODE (elt_reg) != REG
      || GET_CODE (elt_plus) != PLUS
      || REGNO (elt_reg) != SP_REGNUM)
    return false;

  /* Pass all test, this is a valid rtx.  */
  return true;
}

/* Function to check if 'bclr' instruction can be used with IVAL.  */
int
nds32_can_use_bclr_p (int ival)
{
  int one_bit_count;
  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (SImode);

  /* Calculate the number of 1-bit of (~ival), if there is only one 1-bit,
     it means the original ival has only one 0-bit,
     So it is ok to perform 'bclr' operation.  */

  one_bit_count = popcount_hwi ((unsigned HOST_WIDE_INT) (~ival) & mask);

  /* 'bclr' is a performance extension instruction.  */
  return (TARGET_EXT_PERF && (one_bit_count == 1));
}

/* Function to check if 'bset' instruction can be used with IVAL.  */
int
nds32_can_use_bset_p (int ival)
{
  int one_bit_count;
  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (SImode);

  /* Caculate the number of 1-bit of ival, if there is only one 1-bit,
     it is ok to perform 'bset' operation.  */

  one_bit_count = popcount_hwi ((unsigned HOST_WIDE_INT) (ival) & mask);

  /* 'bset' is a performance extension instruction.  */
  return (TARGET_EXT_PERF && (one_bit_count == 1));
}

/* Function to check if 'btgl' instruction can be used with IVAL.  */
int
nds32_can_use_btgl_p (int ival)
{
  int one_bit_count;
  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (SImode);

  /* Caculate the number of 1-bit of ival, if there is only one 1-bit,
     it is ok to perform 'btgl' operation.  */

  one_bit_count = popcount_hwi ((unsigned HOST_WIDE_INT) (ival) & mask);

  /* 'btgl' is a performance extension instruction.  */
  return (TARGET_EXT_PERF && (one_bit_count == 1));
}

/* Function to check if 'bitci' instruction can be used with IVAL.  */
int
nds32_can_use_bitci_p (int ival)
{
  /* If we are using V3 ISA, we have 'bitci' instruction.
     Try to see if we can present 'andi' semantic with
     such 'bit-clear-immediate' operation.
     For example, 'andi $r0,$r0,0xfffffffc' can be
     presented with 'bitci $r0,$r0,3'.  */
  return (TARGET_ISA_V3
	  && (ival < 0)
	  && satisfies_constraint_Iu15 (gen_int_mode (~ival, SImode)));
}

/* Return true if is load/store with SYMBOL_REF addressing mode
   and memory mode is SImode.  */
bool
nds32_symbol_load_store_p (rtx_insn *insn)
{
  rtx mem_src = NULL_RTX;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
      mem_src = SET_SRC (PATTERN (insn));
      break;
    case TYPE_STORE:
      mem_src = SET_DEST (PATTERN (insn));
      break;
    default:
      break;
    }

  /* Find load/store insn with addressing mode is SYMBOL_REF.  */
  if (mem_src != NULL_RTX)
    {
      if ((GET_CODE (mem_src) == ZERO_EXTEND)
	  || (GET_CODE (mem_src) == SIGN_EXTEND))
	mem_src = XEXP (mem_src, 0);

      if ((GET_CODE (XEXP (mem_src, 0)) == SYMBOL_REF)
	   || (GET_CODE (XEXP (mem_src, 0)) == LO_SUM))
	return true;
    }

  return false;
}

/* Vaild memory operand for floating-point loads and stores */
bool
nds32_float_mem_operand_p (rtx op)
{
  machine_mode mode = GET_MODE (op);
  rtx addr = XEXP (op, 0);

  /* Not support [symbol] [const] memory */
  if (GET_CODE (addr) == SYMBOL_REF
      || GET_CODE (addr) == CONST
      || GET_CODE (addr) == LO_SUM)
    return false;

  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == SYMBOL_REF)
	return false;

      /* Restrict const range: (imm12s << 2) */
      if (GET_CODE (XEXP (addr, 1)) == CONST_INT)
	{
	  if ((mode == SImode || mode == SFmode)
	      && NDS32_SINGLE_WORD_ALIGN_P (INTVAL (XEXP (addr, 1)))
	      && !satisfies_constraint_Is14 ( XEXP(addr, 1)))
	    return false;

	  if ((mode == DImode || mode == DFmode)
	      && NDS32_DOUBLE_WORD_ALIGN_P (INTVAL (XEXP (addr, 1)))
	      && !satisfies_constraint_Is14 (XEXP (addr, 1)))
	    return false;
	}
    }

  return true;
}

int
nds32_cond_move_p (rtx cmp_rtx)
{
  machine_mode cmp0_mode = GET_MODE (XEXP (cmp_rtx, 0));
  machine_mode cmp1_mode = GET_MODE (XEXP (cmp_rtx, 1));
  enum rtx_code cond = GET_CODE (cmp_rtx);

  if ((cmp0_mode == DFmode || cmp0_mode == SFmode)
      && (cmp1_mode == DFmode || cmp1_mode == SFmode)
      && (cond == ORDERED || cond == UNORDERED))
    return true;
  return false;
}

bool
nds32_const_double_range_ok_p (rtx op, machine_mode mode,
			       HOST_WIDE_INT lower, HOST_WIDE_INT upper)
{
  if (GET_CODE (op) != CONST_DOUBLE
      || GET_MODE (op) != mode)
    return false;

  const REAL_VALUE_TYPE *rv;
  long val;

  rv = CONST_DOUBLE_REAL_VALUE (op);
  REAL_VALUE_TO_TARGET_SINGLE (*rv, val);

  return val >= lower && val < upper;
}

bool
nds32_const_unspec_p (rtx x)
{
  if (GET_CODE (x) == CONST)
    {
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	x = XEXP (x, 0);

      if (GET_CODE (x) == UNSPEC)
	{
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_GOTINIT:
	    case UNSPEC_GOT:
	    case UNSPEC_GOTOFF:
	    case UNSPEC_PLT:
	    case UNSPEC_TLSGD:
	    case UNSPEC_TLSLD:
	    case UNSPEC_TLSIE:
	    case UNSPEC_TLSLE:
	      return false;
	    default:
	      return true;
	    }
	}
    }

  if (GET_CODE (x) == SYMBOL_REF
      && SYMBOL_REF_TLS_MODEL (x))
    return false;

  return true;
}

HOST_WIDE_INT
const_vector_to_hwint (rtx op)
{
  HOST_WIDE_INT hwint = 0;
  HOST_WIDE_INT mask;
  int i;
  int shift_adv;
  int shift = 0;
  int nelem;

  switch (GET_MODE (op))
    {
      case E_V2HImode:
	mask = 0xffff;
	shift_adv = 16;
	nelem = 2;
	break;
      case E_V4QImode:
	mask = 0xff;
	shift_adv = 8;
	nelem = 4;
	break;
      default:
	gcc_unreachable ();
    }

  if (TARGET_BIG_ENDIAN)
    {
      for (i = 0; i < nelem; ++i)
	{
	  HOST_WIDE_INT val = XINT (XVECEXP (op, 0, nelem - i - 1), 0);
	  hwint |= (val & mask) << shift;
	  shift = shift + shift_adv;
	}
    }
  else
    {
      for (i = 0; i < nelem; ++i)
	{
	  HOST_WIDE_INT val = XINT (XVECEXP (op, 0, i), 0);
	  hwint |= (val & mask) << shift;
	  shift = shift + shift_adv;
	}
    }

  return hwint;
}

bool
nds32_valid_CVp5_p (rtx op)
{
  HOST_WIDE_INT ival = const_vector_to_hwint (op);
  return (ival < ((1 << 5) + 16)) && (ival >= (0 + 16));
}

bool
nds32_valid_CVs5_p (rtx op)
{
  HOST_WIDE_INT ival = const_vector_to_hwint (op);
  return (ival < (1 << 4)) && (ival >= -(1 << 4));
}

bool
nds32_valid_CVs2_p (rtx op)
{
  HOST_WIDE_INT ival = const_vector_to_hwint (op);
  return (ival < (1 << 19)) && (ival >= -(1 << 19));
}

bool
nds32_valid_CVhi_p (rtx op)
{
  HOST_WIDE_INT ival = const_vector_to_hwint (op);
  return (ival != 0) && ((ival & 0xfff) == 0);
}

/* ------------------------------------------------------------------------ */
