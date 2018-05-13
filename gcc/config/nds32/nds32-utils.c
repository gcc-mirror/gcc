/* Auxiliary functions for pipeline descriptions pattern of Andes
   NDS32 cpu for GNU compiler
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
#include "recog.h"
#include "tm-constrs.h"
#include "insn-attr.h"


namespace nds32 {

/* Get the rtx in the PATTERN field of an insn.  If INSN is not an insn,
   the funciton doesn't change anything and returns it directly.  */
rtx
extract_pattern_from_insn (rtx insn)
{
  if (INSN_P (insn))
    return PATTERN (insn);

  return insn;
}

/* Get the number of elements in a parallel rtx.  */
size_t
parallel_elements (rtx parallel_rtx)
{
  parallel_rtx = extract_pattern_from_insn (parallel_rtx);
  gcc_assert (GET_CODE (parallel_rtx) == PARALLEL);

  return XVECLEN (parallel_rtx, 0);
}

/* Extract an rtx from a parallel rtx with index NTH.  If NTH is a negative
   value, the function returns the last NTH rtx.  */
rtx
parallel_element (rtx parallel_rtx, int nth)
{
  parallel_rtx = extract_pattern_from_insn (parallel_rtx);
  gcc_assert (GET_CODE (parallel_rtx) == PARALLEL);

  int len = parallel_elements (parallel_rtx);

  if (nth >= 0)
    {
      if (nth >= len)
	return NULL_RTX;

      return XVECEXP (parallel_rtx, 0, nth);
    }
  else
    {
      if (len + nth < 0)
	return NULL_RTX;

      return XVECEXP (parallel_rtx, 0, len + nth);
    }
}

/* Functions to determine whether INSN is single-word, double-word
   or partial-word load/store insn.  */

bool
load_single_p (rtx_insn *insn)
{
  if (get_attr_type (insn) != TYPE_LOAD)
    return false;

  if (INSN_CODE (insn) == CODE_FOR_move_di ||
      INSN_CODE (insn) == CODE_FOR_move_df)
    return false;

  return true;
}

bool
store_single_p (rtx_insn *insn)
{
  if (get_attr_type (insn) != TYPE_STORE)
    return false;

  if (INSN_CODE (insn) == CODE_FOR_move_di ||
      INSN_CODE (insn) == CODE_FOR_move_df)
    return false;

  return true;
}

bool
load_double_p (rtx_insn *insn)
{
  if (get_attr_type (insn) != TYPE_LOAD)
    return false;

  if (INSN_CODE (insn) != CODE_FOR_move_di &&
      INSN_CODE (insn) != CODE_FOR_move_df)
    return false;

  return true;
}

bool
store_double_p (rtx_insn *insn)
{
  if (get_attr_type (insn) != TYPE_STORE)
    return false;

  if (INSN_CODE (insn) != CODE_FOR_move_di &&
      INSN_CODE (insn) != CODE_FOR_move_df)
    return false;

  return true;
}

bool
store_offset_reg_p (rtx_insn *insn)
{
  if (get_attr_type (insn) != TYPE_STORE)
    return false;

  rtx offset_rtx = extract_offset_rtx (insn);

  if (offset_rtx == NULL_RTX)
    return false;

  if (REG_P (offset_rtx))
    return true;

  return false;
}

/* Determine if INSN is a post update insn.  */
bool
post_update_insn_p (rtx_insn *insn)
{
  if (find_post_update_rtx (insn) == -1)
    return false;
  else
    return true;
}

/* Check if the address of MEM_RTX consists of a base register and an
   immediate offset.  */
bool
immed_offset_p (rtx mem_rtx)
{
  gcc_assert (MEM_P (mem_rtx));

  rtx addr_rtx = XEXP (mem_rtx, 0);

  /* (mem (reg)) is equivalent to (mem (plus (reg) (const_int 0))) */
  if (REG_P (addr_rtx))
    return true;

  /* (mem (plus (reg) (const_int))) */
  if (GET_CODE (addr_rtx) == PLUS
      && GET_CODE (XEXP (addr_rtx, 1)) == CONST_INT)
    return true;

  return false;
}

/* Find the post update rtx in INSN.  If INSN is a load/store multiple insn,
   the function returns the vector index of its parallel part.  If INSN is a
   single load/store insn, the function returns 0.  If INSN is not a post-
   update insn, the function returns -1.  */
int
find_post_update_rtx (rtx_insn *insn)
{
  rtx mem_rtx;
  int i, len;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD_MULTIPLE:
    case TYPE_STORE_MULTIPLE:
      /* Find a pattern in a parallel rtx:
	 (set (reg) (plus (reg) (const_int)))  */
      len = parallel_elements (insn);
      for (i = 0; i < len; ++i)
	{
	  rtx curr_insn = parallel_element (insn, i);

	  if (GET_CODE (curr_insn) == SET
	      && REG_P (SET_DEST (curr_insn))
	      && GET_CODE (SET_SRC (curr_insn)) == PLUS)
		return i;
	}
      return -1;

    case TYPE_LOAD:
    case TYPE_FLOAD:
    case TYPE_STORE:
    case TYPE_FSTORE:
      mem_rtx = extract_mem_rtx (insn);
      /* (mem (post_inc (reg)))  */
      switch (GET_CODE (XEXP (mem_rtx, 0)))
	{
	case POST_INC:
	case POST_DEC:
	case POST_MODIFY:
	  return 0;

	default:
	  return -1;
	}

    default:
      gcc_unreachable ();
    }
}

/* Extract the MEM rtx from a load/store insn.  */
rtx
extract_mem_rtx (rtx_insn *insn)
{
  rtx body = PATTERN (insn);

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
    case TYPE_FLOAD:
      if (MEM_P (SET_SRC (body)))
	return SET_SRC (body);

      /* unaligned address: (unspec [(mem)])  */
      if (GET_CODE (SET_SRC (body)) == UNSPEC)
	{
	  gcc_assert (MEM_P (XVECEXP (SET_SRC (body), 0, 0)));
	  return XVECEXP (SET_SRC (body), 0, 0);
	}

      /* (sign_extend (mem)) */
      gcc_assert (MEM_P (XEXP (SET_SRC (body), 0)));
      return XEXP (SET_SRC (body), 0);

    case TYPE_STORE:
    case TYPE_FSTORE:
      if (MEM_P (SET_DEST (body)))
	return SET_DEST (body);

      /* unaligned address: (unspec [(mem)])  */
      if (GET_CODE (SET_DEST (body)) == UNSPEC)
	{
	  gcc_assert (MEM_P (XVECEXP (SET_DEST (body), 0, 0)));
	  return XVECEXP (SET_DEST (body), 0, 0);
	}

      /* (sign_extend (mem)) */
      gcc_assert (MEM_P (XEXP (SET_DEST (body), 0)));
      return XEXP (SET_DEST (body), 0);

    default:
      gcc_unreachable ();
    }
}

/* Extract the base register from load/store insns.  The function returns
   NULL_RTX if the address is not consist of any registers.  */
rtx
extract_base_reg (rtx_insn *insn)
{
  int post_update_rtx_index;
  rtx mem_rtx;
  rtx plus_rtx;

  /* Find the MEM rtx.  If we can find an insn updating the base register,
     the base register will be returned directly.  */
  switch (get_attr_type (insn))
    {
    case TYPE_LOAD_MULTIPLE:
      post_update_rtx_index = find_post_update_rtx (insn);

      if (post_update_rtx_index != -1)
        return SET_DEST (parallel_element (insn, post_update_rtx_index));

      mem_rtx = SET_SRC (parallel_element (insn, 0));
      break;

    case TYPE_STORE_MULTIPLE:
      post_update_rtx_index = find_post_update_rtx (insn);

      if (post_update_rtx_index != -1)
        return SET_DEST (parallel_element (insn, post_update_rtx_index));

      mem_rtx = SET_DEST (parallel_element (insn, 0));
      break;

    case TYPE_LOAD:
    case TYPE_FLOAD:
    case TYPE_STORE:
    case TYPE_FSTORE:
      mem_rtx = extract_mem_rtx (insn);
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (MEM_P (mem_rtx));

  /* (mem (reg))  */
  if (REG_P (XEXP (mem_rtx, 0)))
    return XEXP (mem_rtx, 0);

  /* (mem (lo_sum (reg) (symbol_ref)) */
  if (GET_CODE (XEXP (mem_rtx, 0)) == LO_SUM)
    return XEXP (XEXP (mem_rtx, 0), 0);

  plus_rtx = XEXP (mem_rtx, 0);

  if (GET_CODE (plus_rtx) == SYMBOL_REF
      || GET_CODE (plus_rtx) == CONST)
    return NULL_RTX;

  /* (mem (plus (reg) (const_int))) or
     (mem (plus (mult (reg) (const_int 4)) (reg))) or
     (mem (post_inc (reg))) or
     (mem (post_dec (reg))) or
     (mem (post_modify (reg) (plus (reg) (reg))))  */
  gcc_assert (GET_CODE (plus_rtx) == PLUS
	      || GET_CODE (plus_rtx) == POST_INC
	      || GET_CODE (plus_rtx) == POST_DEC
	      || GET_CODE (plus_rtx) == POST_MODIFY);

  if (REG_P (XEXP (plus_rtx, 0)))
    return XEXP (plus_rtx, 0);

  gcc_assert (REG_P (XEXP (plus_rtx, 1)));
  return XEXP (plus_rtx, 1);
}

/* Extract the offset rtx from load/store insns.  The function returns
   NULL_RTX if offset is absent.  */
rtx
extract_offset_rtx (rtx_insn *insn)
{
  rtx mem_rtx;
  rtx plus_rtx;
  rtx offset_rtx;

  /* Find the MEM rtx.  The multiple load/store insns doens't have
     the offset field so we can return NULL_RTX here.  */
  switch (get_attr_type (insn))
    {
    case TYPE_LOAD_MULTIPLE:
    case TYPE_STORE_MULTIPLE:
      return NULL_RTX;

    case TYPE_LOAD:
    case TYPE_FLOAD:
    case TYPE_STORE:
    case TYPE_FSTORE:
      mem_rtx = extract_mem_rtx (insn);
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (MEM_P (mem_rtx));

  /* (mem (reg))  */
  if (REG_P (XEXP (mem_rtx, 0)))
    return NULL_RTX;

  plus_rtx = XEXP (mem_rtx, 0);

  switch (GET_CODE (plus_rtx))
    {
    case SYMBOL_REF:
    case CONST:
    case POST_INC:
    case POST_DEC:
      return NULL_RTX;

    case PLUS:
      /* (mem (plus (reg) (const_int))) or
         (mem (plus (mult (reg) (const_int 4)) (reg))) */
      if (REG_P (XEXP (plus_rtx, 0)))
        offset_rtx = XEXP (plus_rtx, 1);
      else
	{
	  gcc_assert (REG_P (XEXP (plus_rtx, 1)));
	  offset_rtx = XEXP (plus_rtx, 0);
	}

      if (ARITHMETIC_P (offset_rtx))
	{
	  gcc_assert (GET_CODE (offset_rtx) == MULT);
	  gcc_assert (REG_P (XEXP (offset_rtx, 0)));
	  offset_rtx = XEXP (offset_rtx, 0);
	}
      break;

    case LO_SUM:
      /* (mem (lo_sum (reg) (symbol_ref)) */
      offset_rtx = XEXP (plus_rtx, 1);
      break;

    case POST_MODIFY:
      /* (mem (post_modify (reg) (plus (reg) (reg / const_int)))) */
      gcc_assert (REG_P (XEXP (plus_rtx, 0)));
      plus_rtx = XEXP (plus_rtx, 1);
      gcc_assert (GET_CODE (plus_rtx) == PLUS);
      offset_rtx = XEXP (plus_rtx, 0);
      break;

    default:
      gcc_unreachable ();
    }

  return offset_rtx;
}

/* Extract the register of the shift operand from an ALU_SHIFT rtx.  */
rtx
extract_shift_reg (rtx alu_shift_rtx)
{
  alu_shift_rtx = extract_pattern_from_insn (alu_shift_rtx);

  rtx alu_rtx = SET_SRC (alu_shift_rtx);
  rtx shift_rtx;

  /* Various forms of ALU_SHIFT can be made by the combiner.
     See the difference between add_slli and sub_slli in nds32.md.  */
  if (REG_P (XEXP (alu_rtx, 0)))
    shift_rtx = XEXP (alu_rtx, 1);
  else
    shift_rtx = XEXP (alu_rtx, 0);

  return XEXP (shift_rtx, 0);
}

/* Check if INSN is a movd44 insn.  */
bool
movd44_insn_p (rtx_insn *insn)
{
  if (get_attr_type (insn) == TYPE_ALU
      && (INSN_CODE (insn) == CODE_FOR_move_di
	  || INSN_CODE (insn) == CODE_FOR_move_df))
    {
      rtx body = PATTERN (insn);
      gcc_assert (GET_CODE (body) == SET);

      rtx src = SET_SRC (body);
      rtx dest = SET_DEST (body);

      if ((REG_P (src) || GET_CODE (src) == SUBREG)
	  && (REG_P (dest) || GET_CODE (dest) == SUBREG))
	return true;

      return false;
    }

  return false;
}

/* Extract the second result (odd reg) of a movd44 insn.  */
rtx
extract_movd44_odd_reg (rtx_insn *insn)
{
  gcc_assert (movd44_insn_p (insn));

  rtx def_reg = SET_DEST (PATTERN (insn));
  machine_mode mode;

  gcc_assert (REG_P (def_reg) || GET_CODE (def_reg) == SUBREG);
  switch (GET_MODE (def_reg))
    {
    case E_DImode:
      mode = SImode;
      break;

    case E_DFmode:
      mode = SFmode;
      break;

    default:
      gcc_unreachable ();
    }

  return gen_highpart (mode, def_reg);
}

/* Extract the rtx representing non-accumulation operands of a MAC insn.  */
rtx
extract_mac_non_acc_rtx (rtx_insn *insn)
{
  rtx exp = SET_SRC (PATTERN (insn));

  switch (get_attr_type (insn))
    {
    case TYPE_MAC:
    case TYPE_DMAC:
      if (REG_P (XEXP (exp, 0)))
	return XEXP (exp, 1);
      else
	return XEXP (exp, 0);

    default:
      gcc_unreachable ();
    }
}

/* Check if the DIV insn needs two write ports.  */
bool
divmod_p (rtx_insn *insn)
{
  gcc_assert (get_attr_type (insn) == TYPE_DIV);

  if (INSN_CODE (insn) == CODE_FOR_divmodsi4
      || INSN_CODE (insn) == CODE_FOR_udivmodsi4)
    return true;

  return false;
}

/* Extract the rtx representing the branch target to help recognize
   data hazards.  */
rtx
extract_branch_target_rtx (rtx_insn *insn)
{
  gcc_assert (CALL_P (insn) || JUMP_P (insn));

  rtx body = PATTERN (insn);

  if (GET_CODE (body) == SET)
    {
      /* RTXs in IF_THEN_ELSE are branch conditions.  */
      if (GET_CODE (SET_SRC (body)) == IF_THEN_ELSE)
        return NULL_RTX;

      return SET_SRC (body);
    }

  if (GET_CODE (body) == CALL)
    return XEXP (body, 0);

  if (GET_CODE (body) == PARALLEL)
    {
      rtx first_rtx = parallel_element (body, 0);

      if (GET_CODE (first_rtx) == SET)
	return SET_SRC (first_rtx);

      if (GET_CODE (first_rtx) == CALL)
	return XEXP (first_rtx, 0);
    }

  /* Handle special cases of bltzal, bgezal and jralnez.  */
  if (GET_CODE (body) == COND_EXEC)
    {
      rtx addr_rtx = XEXP (body, 1);

      if (GET_CODE (addr_rtx) == SET)
	return SET_SRC (addr_rtx);

      if (GET_CODE (addr_rtx) == PARALLEL)
	{
	  rtx first_rtx = parallel_element (addr_rtx, 0);

	  if (GET_CODE (first_rtx) == SET)
	    {
	      rtx call_rtx = SET_SRC (first_rtx);
	      gcc_assert (GET_CODE (call_rtx) == CALL);

	      return XEXP (call_rtx, 0);
	    }

	  if (GET_CODE (first_rtx) == CALL)
	    return XEXP (first_rtx, 0);
	}
    }

  gcc_unreachable ();
}

/* Extract the rtx representing the branch condition to help recognize
   data hazards.  */
rtx
extract_branch_condition_rtx (rtx_insn *insn)
{
  gcc_assert (CALL_P (insn) || JUMP_P (insn));

  rtx body = PATTERN (insn);

  if (GET_CODE (body) == SET)
    {
      rtx if_then_else_rtx = SET_SRC (body);

      if (GET_CODE (if_then_else_rtx) == IF_THEN_ELSE)
        return XEXP (if_then_else_rtx, 0);

      return NULL_RTX;
    }

  if (GET_CODE (body) == COND_EXEC)
    return XEXP (body, 0);

  return NULL_RTX;
}

} // namespace nds32
