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
#include "rtl.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "target.h"

#include "nds32-protos.h"

/* ------------------------------------------------------------------------ */

namespace nds32 {
namespace scheduling {

/* Classify the memory access direction.  It's unknown if the offset register
   is not a constant value.  */
enum memory_access_direction
{
  MEM_ACCESS_DIR_POS,
  MEM_ACCESS_DIR_NEG,
  MEM_ACCESS_DIR_UNKNOWN
};

/* A safe wrapper to the function reg_overlap_mentioned_p ().  */
bool
reg_overlap_p (rtx x, rtx in)
{
  if (x == NULL_RTX || in == NULL_RTX)
    return false;

  return static_cast <bool> (reg_overlap_mentioned_p (x, in));
}


/* Determine the memory access direction of a load/store insn.  */
memory_access_direction
determine_access_direction (rtx_insn *insn)
{
  int post_update_rtx_index;
  rtx plus_rtx;
  rtx mem_rtx;
  rtx offset_rtx;

  switch (get_attr_type (insn))
  {
  case TYPE_LOAD_MULTIPLE:
    gcc_assert (parallel_elements (insn) >= 2);

    post_update_rtx_index = find_post_update_rtx (insn);
    if (post_update_rtx_index != -1)
      plus_rtx = SET_SRC (parallel_element (insn, post_update_rtx_index));
    else
      {
	/* (parallel
	     [(set (reg) (mem (reg)))              : index 0
	      (set (reg) (mem (plus (reg) (...)))) : index 1
	      ...])  */
	mem_rtx = SET_SRC (parallel_element (insn, 1));
	if (GET_CODE (mem_rtx) == UNSPEC)
	  mem_rtx = XVECEXP (mem_rtx, 0, 0);
	gcc_assert (MEM_P (mem_rtx));
	plus_rtx = XEXP (mem_rtx, 0);
      }
    break;

  case TYPE_STORE_MULTIPLE:
    gcc_assert (parallel_elements (insn) >= 2);

    post_update_rtx_index = find_post_update_rtx (insn);
    if (post_update_rtx_index != -1)
      plus_rtx = SET_SRC (parallel_element (insn, post_update_rtx_index));
    else
      {
	/* (parallel
	     [(set (mem (reg))              (reg)) : index 0
	      (set (mem (plus (reg) (...))) (reg)) : index 1
	      ...])  */
	mem_rtx = SET_DEST (parallel_element (insn, 1));
	if (GET_CODE (mem_rtx) == UNSPEC)
	  mem_rtx = XVECEXP (mem_rtx, 0, 0);
	gcc_assert (MEM_P (mem_rtx));
	plus_rtx = XEXP (mem_rtx, 0);
      }
    break;

  case TYPE_LOAD:
  case TYPE_STORE:
    mem_rtx = extract_mem_rtx (insn);

    switch (GET_CODE (XEXP (mem_rtx, 0)))
      {
      case POST_INC:
	/* (mem (post_inc (...)))  */
	return MEM_ACCESS_DIR_POS;

      case POST_DEC:
	/* (mem (post_dec (...)))  */
	return MEM_ACCESS_DIR_NEG;

      case PLUS:
	/* (mem (plus (reg) (...)))  */
	plus_rtx = XEXP (mem_rtx, 0);
	break;

      case POST_MODIFY:
	/* (mem (post_modify (reg) (plus (reg) (...))))  */
	plus_rtx = XEXP (XEXP (mem_rtx, 0), 1);
	break;

      default:
	gcc_unreachable ();
      }
    break;

  default:
    gcc_unreachable ();
  }

  gcc_assert (GET_CODE (plus_rtx) == PLUS);

  offset_rtx = XEXP (plus_rtx, 1);
  if (GET_CODE (offset_rtx) == CONST_INT)
    {
      if (INTVAL (offset_rtx) < 0)
	return MEM_ACCESS_DIR_NEG;
      else
	return MEM_ACCESS_DIR_POS;
    }

  return MEM_ACCESS_DIR_UNKNOWN;
}

/* Return the nth load/store operation in the real micro-operation
   accessing order.  */
rtx
extract_nth_access_rtx (rtx_insn *insn, int n)
{
  int n_elems = parallel_elements (insn);
  int post_update_rtx_index = find_post_update_rtx (insn);
  memory_access_direction direction = determine_access_direction (insn);

  gcc_assert (direction != MEM_ACCESS_DIR_UNKNOWN);

  /* Reverse the order if the direction negative.  */
  if (direction == MEM_ACCESS_DIR_NEG)
    n = -1 * n - 1;

  if (post_update_rtx_index != -1)
    {
      if (n >= 0 && post_update_rtx_index <= n)
	++n;
      else if (n < 0 && post_update_rtx_index >= n + n_elems)
	--n;
    }

  return parallel_element (insn, n);
}

/* Returns the register operated by the nth load/store operation in the real
   micro-operation accessing order.  This function assumes INSN must be a
   multiple-word load/store insn.  */
rtx
extract_nth_lmsw_access_reg (rtx_insn *insn, int n)
{
  rtx nth_rtx = extract_nth_access_rtx (insn, n);

  if (nth_rtx == NULL_RTX)
    return NULL_RTX;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD_MULTIPLE:
      return SET_DEST (nth_rtx);

    case TYPE_STORE_MULTIPLE:
      return SET_SRC (nth_rtx);

    default:
      gcc_unreachable ();
    }
}

/* Returns the register operated by the nth load/store operation in the real
   micro-operation accessing order.  This function assumes INSN must be a
   double-word load/store insn.  */
rtx
extract_nth_ls2_access_reg (rtx_insn *insn, int n)
{
  rtx reg;
  machine_mode mode;

  if (post_update_insn_p (insn))
    {
      memory_access_direction direction = determine_access_direction (insn);
      gcc_assert (direction != MEM_ACCESS_DIR_UNKNOWN);

      /* Reverse the order if the direction negative.  */
      if (direction == MEM_ACCESS_DIR_NEG)
	n = -1 * n - 1;
    }

  /* Handle the out-of-range case.  */
  if (n < -2 || n > 1)
    return NULL_RTX;

  /* Convert the index to a positive one.  */
  if (n < 0)
    n = 2 + n;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
      reg = SET_DEST (PATTERN (insn));
      break;

    case TYPE_STORE:
      reg = SET_SRC (PATTERN (insn));
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (REG_P (reg) || GET_CODE (reg) == SUBREG);

  switch (GET_MODE (reg))
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

  if (n == 0)
    return gen_lowpart (mode, reg);
  else
    return gen_highpart (mode, reg);
}

/* Returns the register operated by the nth load/store operation in the real
   micro-operation accessing order.  */
rtx
extract_nth_access_reg (rtx_insn *insn, int index)
{
  switch (GET_CODE (PATTERN (insn)))
    {
    case PARALLEL:
      return extract_nth_lmsw_access_reg (insn, index);

    case SET:
      return extract_nth_ls2_access_reg (insn, index);

    default:
      gcc_unreachable ();
    }
}

/* Determine if the latency is occured when the consumer PBSADA_INSN uses the
   value of DEF_REG in its Ra or Rb fields.  */
bool
pbsada_insn_ra_rb_dep_reg_p (rtx pbsada_insn, rtx def_reg)
{
  rtx unspec_rtx = SET_SRC (PATTERN (pbsada_insn));
  gcc_assert (GET_CODE (unspec_rtx) == UNSPEC);

  rtx pbsada_ra = XVECEXP (unspec_rtx, 0, 0);
  rtx pbsada_rb = XVECEXP (unspec_rtx, 0, 1);

  if (rtx_equal_p (def_reg, pbsada_ra)
      || rtx_equal_p (def_reg, pbsada_rb))
    return true;

  return false;
}

/* Check if INSN is a movd44 insn consuming DEF_REG.  */
bool
movd44_even_dep_p (rtx_insn *insn, rtx def_reg)
{
  if (!movd44_insn_p (insn))
    return false;

  rtx use_rtx = SET_SRC (PATTERN (insn));

  if (REG_P (def_reg))
    {
      return rtx_equal_p (def_reg, use_rtx);
    }
  else if (GET_CODE (def_reg) == SUBREG
	   && GET_MODE (def_reg) == SImode
	   && rtx_equal_p (SUBREG_REG (def_reg), use_rtx))
    {
      if (TARGET_BIG_ENDIAN && SUBREG_BYTE (def_reg) == 4)
	return true;

      if (!TARGET_BIG_ENDIAN && SUBREG_BYTE (def_reg) == 0)
	return true;

      return false;
    }

  return false;
}

} // namespace scheduling
} // namespace nds32

/* ------------------------------------------------------------------------ */

using namespace nds32;
using namespace nds32::scheduling;

namespace { // anonymous namespace

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at II.  */
bool
n7_consumed_by_ii_dep_p (rtx_insn *consumer, rtx def_reg)
{
  rtx use_rtx;

  switch (get_attr_type (consumer))
    {
    /* MOVD44_E */
    case TYPE_ALU:
      if (movd44_even_dep_p (consumer, def_reg))
	return true;

      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MUL:
      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MAC:
      use_rtx = extract_mac_non_acc_rtx (consumer);
      break;

   /* Some special instructions, divmodsi4 and udivmodsi4, produce two
      results, the quotient and the remainder.  It requires two micro-
      operations in order to write two registers. We have to check the
      dependency from the producer to the first micro-operation.  */
    case TYPE_DIV:
      if (INSN_CODE (consumer) == CODE_FOR_divmodsi4
	  || INSN_CODE (consumer) == CODE_FOR_udivmodsi4)
	use_rtx = SET_SRC (parallel_element (consumer, 0));
      else
	use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_LOAD:
      /* ADDR_IN_bi_Ra, ADDR_IN_!bi */
      if (post_update_insn_p (consumer))
	use_rtx = extract_base_reg (consumer);
      else
	use_rtx = extract_mem_rtx (consumer);
      break;

    case TYPE_STORE:
      /* ADDR_IN_bi_Ra, ADDR_IN_!bi */
      if (post_update_insn_p (consumer))
	use_rtx = extract_base_reg (consumer);
      else
	use_rtx = extract_mem_rtx (consumer);

      if (reg_overlap_p (def_reg, use_rtx))
	return true;

      /* ST_bi, ST_!bi_RI */
      if (!post_update_insn_p (consumer)
	  && !immed_offset_p (extract_mem_rtx (consumer)))
	return false;

      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_LOAD_MULTIPLE:
      use_rtx = extract_base_reg (consumer);
      break;

    case TYPE_STORE_MULTIPLE:
      /* ADDR_IN */
      use_rtx = extract_base_reg (consumer);
      if (reg_overlap_p (def_reg, use_rtx))
	return true;

      /* SMW (N, 1) */
      use_rtx = extract_nth_access_rtx (consumer, 0);
      break;

    case TYPE_BRANCH:
      use_rtx = PATTERN (consumer);
      break;

    default:
      gcc_unreachable ();
    }

  if (reg_overlap_p (def_reg, use_rtx))
    return true;

  return false;
}

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at AG (II).  */
bool
n8_consumed_by_addr_in_p (rtx_insn *consumer, rtx def_reg)
{
  rtx use_rtx;

  switch (get_attr_type (consumer))
    {
    case TYPE_BRANCH:
      use_rtx = extract_branch_target_rtx (consumer);
      break;

    case TYPE_LOAD:
      if (load_single_p (consumer))
	use_rtx = extract_mem_rtx (consumer);
      else
	use_rtx = extract_base_reg (consumer);
      break;

    case TYPE_STORE:
      if (store_single_p (consumer)
	  && (!post_update_insn_p (consumer)
	      || immed_offset_p (extract_mem_rtx (consumer))))
	use_rtx = extract_mem_rtx (consumer);
      else
	use_rtx = extract_base_reg (consumer);
      break;

    case TYPE_LOAD_MULTIPLE:
    case TYPE_STORE_MULTIPLE:
      use_rtx = extract_base_reg (consumer);
      break;

    default:
      gcc_unreachable ();
    }

  return reg_overlap_p (def_reg, use_rtx);
}

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at EX.  */
bool
n8_consumed_by_ex_p (rtx_insn *consumer, rtx def_reg)
{
  rtx use_rtx;

  switch (get_attr_type (consumer))
    {
    case TYPE_ALU:
      if (movd44_even_dep_p (consumer, def_reg))
	return true;

      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MUL:
      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MAC:
      use_rtx = extract_mac_non_acc_rtx (consumer);
      break;

   /* Some special instructions, divmodsi4 and udivmodsi4, produce two
      results, the quotient and the remainder.  It requires two micro-
      operations in order to write two registers. We have to check the
      dependency from the producer to the first micro-operation.  */
    case TYPE_DIV:
      if (INSN_CODE (consumer) == CODE_FOR_divmodsi4
	  || INSN_CODE (consumer) == CODE_FOR_udivmodsi4)
	use_rtx = SET_SRC (parallel_element (consumer, 0));
      else
	use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_BRANCH:
      use_rtx = extract_branch_condition_rtx (consumer);
      break;

    case TYPE_STORE:
      /* exclude ST_!bi_RR */
      if (!post_update_insn_p (consumer)
	  && !immed_offset_p (extract_mem_rtx (consumer)))
	return false;

      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_STORE_MULTIPLE:
      use_rtx = extract_nth_access_rtx (consumer, 0);
      break;

    default:
      gcc_unreachable ();
    }

  return reg_overlap_p (def_reg, use_rtx);
}

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at AG (II).  */
bool
e8_consumed_by_addr_in_p (rtx_insn *consumer, rtx def_reg)
{
  return n8_consumed_by_addr_in_p (consumer, def_reg);
}

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at EX.  */
bool
e8_consumed_by_ex_p (rtx_insn *consumer, rtx def_reg)
{
  rtx use_rtx;

  switch (get_attr_type (consumer))
    {
    case TYPE_ALU:
    case TYPE_STORE:
      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MUL:
    case TYPE_MAC:
    case TYPE_DIV:
    case TYPE_BRANCH:
    case TYPE_STORE_MULTIPLE:
      return n8_consumed_by_ex_p (consumer, def_reg);

    default:
      gcc_unreachable ();
    }

  return reg_overlap_p (def_reg, use_rtx);
}

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at EX.  */
bool
n9_2r1w_consumed_by_ex_dep_p (rtx_insn *consumer, rtx def_reg)
{
  rtx use_rtx;

  switch (get_attr_type (consumer))
    {
    case TYPE_ALU:
      if (movd44_even_dep_p (consumer, def_reg))
	return true;

      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_PBSAD:
    case TYPE_MUL:
      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_ALU_SHIFT:
      use_rtx = extract_shift_reg (consumer);
      break;

    case TYPE_PBSADA:
      return pbsada_insn_ra_rb_dep_reg_p (consumer, def_reg);

    case TYPE_MAC:
      use_rtx = PATTERN (consumer);
      break;

    case TYPE_DIV:
      if (INSN_CODE (consumer) == CODE_FOR_divmodsi4
	  || INSN_CODE (consumer) == CODE_FOR_udivmodsi4)
	use_rtx = SET_SRC (parallel_element (consumer, 0));
      else
	use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MMU:
      if (GET_CODE (PATTERN (consumer)) == SET)
	use_rtx = SET_SRC (PATTERN (consumer));
      else
	return true;
      break;

    case TYPE_LOAD:
      /* ADDR_IN_bi_Ra, ADDR_IN_!bi */
      if (post_update_insn_p (consumer))
	use_rtx = extract_base_reg (consumer);
      else
	use_rtx = extract_mem_rtx (consumer);
      break;

    case TYPE_STORE:
      /* ADDR_IN_bi_Ra, ADDR_IN_!bi */
      if (post_update_insn_p (consumer))
	use_rtx = extract_base_reg (consumer);
      else
	use_rtx = extract_mem_rtx (consumer);

      if (reg_overlap_p (def_reg, use_rtx))
	return true;

      /* exclude ST_!bi_RR */
      if (!post_update_insn_p (consumer)
	  && !immed_offset_p (extract_mem_rtx (consumer)))
	return false;

      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_LOAD_MULTIPLE:
      use_rtx = extract_base_reg (consumer);
      break;

    case TYPE_STORE_MULTIPLE:
      /* ADDR_IN */
      use_rtx = extract_base_reg (consumer);
      if (reg_overlap_p (def_reg, use_rtx))
	return true;

      /* SMW (N, 1) */
      use_rtx = extract_nth_access_rtx (consumer, 0);
      break;

    case TYPE_BRANCH:
      use_rtx = PATTERN (consumer);
      break;

    default:
      gcc_unreachable ();
    }

  if (reg_overlap_p (def_reg, use_rtx))
    return true;

  return false;
}

/* Check the dependency between the producer defining DEF_REG and CONSUMER
   requiring input operand at EX.  */
bool
n9_3r2w_consumed_by_ex_dep_p (rtx_insn *consumer, rtx def_reg)
{
  rtx use_rtx;

  switch (get_attr_type (consumer))
    {
    case TYPE_ALU:
    case TYPE_PBSAD:
    case TYPE_MUL:
      use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_ALU_SHIFT:
      use_rtx = extract_shift_reg (consumer);
      break;

    case TYPE_PBSADA:
      return pbsada_insn_ra_rb_dep_reg_p (consumer, def_reg);

    case TYPE_MAC:
      use_rtx = extract_mac_non_acc_rtx (consumer);
      break;

   /* Some special instructions, divmodsi4 and udivmodsi4, produce two
      results, the quotient and the remainder.  In 2R1W configuration,
      it requires two micro-operations in order to write two registers.
      We have to check the dependency from the producer to the first
      micro-operation.  */
    case TYPE_DIV:
      if (INSN_CODE (consumer) == CODE_FOR_divmodsi4
	  || INSN_CODE (consumer) == CODE_FOR_udivmodsi4)
	use_rtx = SET_SRC (parallel_element (consumer, 0));
      else
	use_rtx = SET_SRC (PATTERN (consumer));
      break;

    case TYPE_MMU:
      if (GET_CODE (PATTERN (consumer)) == SET)
	use_rtx = SET_SRC (PATTERN (consumer));
      else
	return true;
      break;

    case TYPE_LOAD:
    case TYPE_STORE:
      use_rtx = extract_mem_rtx (consumer);
      break;

    case TYPE_LOAD_MULTIPLE:
    case TYPE_STORE_MULTIPLE:
      use_rtx = extract_base_reg (consumer);
      break;

    case TYPE_BRANCH:
      use_rtx = PATTERN (consumer);
      break;

    default:
      gcc_unreachable ();
    }

  if (reg_overlap_p (def_reg, use_rtx))
    return true;

  return false;
}


} // anonymous namespace

/* ------------------------------------------------------------------------ */

/* Guard functions for N7 core.  */

bool
nds32_n7_load_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  if (post_update_insn_p (producer))
    return false;

  rtx def_reg = SET_DEST (PATTERN (producer));

  return n7_consumed_by_ii_dep_p (consumer, def_reg);
}

bool
nds32_n7_last_load_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  /* If PRODUCER is a post-update LMW insn, the last micro-operation updates
     the base register and the result is ready in II stage, so we don't need
     to handle that case in this guard function and the corresponding bypass
     rule.  */
  if (post_update_insn_p (producer))
    return false;

  rtx last_def_reg = extract_nth_access_reg (producer, -1);

  if (last_def_reg == NULL_RTX)
    return false;

  gcc_assert (REG_P (last_def_reg) || GET_CODE (last_def_reg) == SUBREG);

  return n7_consumed_by_ii_dep_p (consumer, last_def_reg);
}

/* Guard functions for N8 core.  */

bool
nds32_n8_load_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  if (post_update_insn_p (producer))
    return false;

  rtx def_reg = SET_DEST (PATTERN (producer));

  return n8_consumed_by_addr_in_p (consumer, def_reg);
}

bool
nds32_n8_load_bi_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  if (!post_update_insn_p (producer))
    return false;

  rtx def_reg = SET_DEST (PATTERN (producer));

  return n8_consumed_by_addr_in_p (consumer, def_reg);
}

bool
nds32_n8_load_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  if (post_update_insn_p (producer))
    return false;

  rtx def_reg = SET_DEST (PATTERN (producer));

  return n8_consumed_by_ex_p (consumer, def_reg);
}

bool
nds32_n8_ex_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx def_reg;

  switch (get_attr_type (producer))
    {
    case TYPE_ALU:
      if (movd44_insn_p (producer))
	def_reg = extract_movd44_odd_reg (producer);
      else
	def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_MUL:
    case TYPE_MAC:
      def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_DIV:
      if (INSN_CODE (producer) == CODE_FOR_divmodsi4
	  || INSN_CODE (producer) == CODE_FOR_udivmodsi4)
	def_reg = SET_DEST (parallel_element (producer, 1));
      else
	def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_LOAD:
    case TYPE_STORE:
    case TYPE_LOAD_MULTIPLE:
    case TYPE_STORE_MULTIPLE:
      if (!post_update_insn_p (producer))
	return false;

      def_reg = extract_base_reg (producer);
      break;

    default:
      gcc_unreachable ();
    }

  return n8_consumed_by_addr_in_p (consumer, def_reg);
}

bool
nds32_n8_last_load_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  /* If PRODUCER is a post-update LMW insn, the last micro-operation updates
     the base register and the result is ready in EX stage, so we don't need
     to handle that case in this guard function and the corresponding bypass
     rule.  */
  if (post_update_insn_p (producer))
    return false;

  rtx last_def_reg = extract_nth_access_reg (producer, -1);

  if (last_def_reg == NULL_RTX)
    return false;

  gcc_assert (REG_P (last_def_reg) || GET_CODE (last_def_reg) == SUBREG);

  return n8_consumed_by_addr_in_p (consumer, last_def_reg);
}

bool
nds32_n8_last_load_two_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  int index = -2;

  /* If PRODUCER is a post-update insn, there is an additional one micro-
     operation inserted in the end, so the last memory access operation should
     be handled by this guard function and the corresponding bypass rule.  */
  if (post_update_insn_p (producer))
    index = -1;

  rtx last_two_def_reg = extract_nth_access_reg (producer, index);

  if (last_two_def_reg == NULL_RTX)
    return false;

  gcc_assert (REG_P (last_two_def_reg)
	      || GET_CODE (last_two_def_reg) == SUBREG);

  return n8_consumed_by_addr_in_p (consumer, last_two_def_reg);
}

bool
nds32_n8_last_load_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  /* If PRODUCER is a post-update LMW insn, the last micro-operation updates
     the base register and the result is ready in EX stage, so we don't need
     to handle that case in this guard function and the corresponding bypass
     rule.  */
  if (post_update_insn_p (producer))
    return false;

  rtx last_def_reg = extract_nth_access_reg (producer, -1);

  if (last_def_reg == NULL_RTX)
    return false;

  gcc_assert (REG_P (last_def_reg) || GET_CODE (last_def_reg) == SUBREG);

  return n8_consumed_by_ex_p (consumer, last_def_reg);
}

/* Guard functions for E8 cores.  */

bool
nds32_e8_load_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx def_reg = SET_DEST (PATTERN (producer));

  return e8_consumed_by_addr_in_p (consumer, def_reg);
}

bool
nds32_e8_load_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx def_reg = SET_DEST (PATTERN (producer));

  return e8_consumed_by_ex_p (consumer, def_reg);
}

bool
nds32_e8_ex_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx def_reg;

  switch (get_attr_type (producer))
    {
    case TYPE_ALU:
      /* No data hazards if AGEN's input is produced by MOVI or SETHI.  */
      if (GET_CODE (PATTERN (producer)) == SET)
	{
	  rtx dest = SET_DEST (PATTERN (producer));
	  rtx src = SET_SRC (PATTERN (producer));

	  if ((REG_P (dest) || GET_CODE (dest) == SUBREG)
	      && (GET_CODE (src) == CONST_INT || GET_CODE (src) == HIGH))
	    return false;
	}

      def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_MUL:
    case TYPE_MAC:
      def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_DIV:
      if (INSN_CODE (producer) == CODE_FOR_divmodsi4
	  || INSN_CODE (producer) == CODE_FOR_udivmodsi4)
	{
	  rtx def_reg1 = SET_DEST (parallel_element (producer, 0));
	  rtx def_reg2 = SET_DEST (parallel_element (producer, 1));

	  return (e8_consumed_by_addr_in_p (consumer, def_reg1)
		  || e8_consumed_by_addr_in_p (consumer, def_reg2));
	}

      def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_LOAD:
    case TYPE_STORE:
    case TYPE_LOAD_MULTIPLE:
    case TYPE_STORE_MULTIPLE:
      if (!post_update_insn_p (producer))
	return false;

      def_reg = extract_base_reg (producer);
      break;

    default:
      gcc_unreachable ();
    }

  return e8_consumed_by_addr_in_p (consumer, def_reg);
}

bool
nds32_e8_last_load_to_ii_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx last_def_reg = extract_nth_access_reg (producer, -1);

  if (last_def_reg == NULL_RTX)
    return false;

  gcc_assert (REG_P (last_def_reg) || GET_CODE (last_def_reg) == SUBREG);

  return e8_consumed_by_addr_in_p (consumer, last_def_reg);
}

bool
nds32_e8_last_load_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx last_def_reg = extract_nth_access_reg (producer, -1);

  if (last_def_reg == NULL_RTX)
    return false;

  gcc_assert (REG_P (last_def_reg) || GET_CODE (last_def_reg) == SUBREG);

  return e8_consumed_by_ex_p (consumer, last_def_reg);
}

/* Guard functions for N9 cores.  */

/* Check dependencies from MM to EX.  */
bool
nds32_n9_2r1w_mm_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx def_reg;

  switch (get_attr_type (producer))
    {
    /* LD_!bi */
    case TYPE_LOAD:
      if (post_update_insn_p (producer))
	return false;

      def_reg = SET_DEST (PATTERN (producer));
      break;

    case TYPE_MUL:
    case TYPE_MAC:
      def_reg = SET_DEST (PATTERN (producer));
      break;

    default:
      gcc_unreachable ();
    }

    return n9_2r1w_consumed_by_ex_dep_p (consumer, def_reg);
}

/* Check dependencies from MM to EX.  */
bool
nds32_n9_3r2w_mm_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx def_reg;

  switch (get_attr_type (producer))
    {
    case TYPE_LOAD:
    case TYPE_MUL:
    case TYPE_MAC:
      def_reg = SET_DEST (PATTERN (producer));
      break;

   /* Some special instructions, divmodsi4 and udivmodsi4, produce two
      results, the quotient and the remainder.  We have to handle them
      individually.  */
    case TYPE_DIV:
      if (INSN_CODE (producer) == CODE_FOR_divmodsi4
	  || INSN_CODE (producer) == CODE_FOR_udivmodsi4)
	{
	  rtx def_reg1 = SET_DEST (parallel_element (producer, 0));
	  rtx def_reg2 = SET_DEST (parallel_element (producer, 1));

	  return (n9_3r2w_consumed_by_ex_dep_p (consumer, def_reg1)
		  || n9_3r2w_consumed_by_ex_dep_p (consumer, def_reg2));
	}

      def_reg = SET_DEST (PATTERN (producer));
      break;

    default:
      gcc_unreachable ();
    }

    return n9_3r2w_consumed_by_ex_dep_p (consumer, def_reg);
}

/* Check dependencies from LMW(N, N) to EX.  */
bool
nds32_n9_last_load_to_ex_p (rtx_insn *producer, rtx_insn *consumer)
{
  rtx last_def_reg = extract_nth_access_reg (producer, -1);

  if (nds32_register_ports_config == REG_PORT_2R1W)
    {
      /* The base-update micro operation occupies the last cycle.  */
      if (post_update_insn_p (producer))
	return false;

      /* When the base register is in the list of a load multiple insn and the
	 access order of the base register is not the last one, we need an
	 additional micro operation to commit the load result to the base
	 register -- we can treat the base register as the last defined
	 register.  */
      size_t i;
      size_t n_elems = parallel_elements (producer);
      rtx base_reg = extract_base_reg (producer);

      for (i = 0; i < n_elems; ++i)
	{
	  rtx load_rtx = extract_nth_access_rtx (producer, i);
	  rtx list_element = SET_DEST (load_rtx);

	  if (rtx_equal_p (base_reg, list_element) && i != n_elems - 1)
	    {
	      last_def_reg = base_reg;
	      break;
	    }
	}

      return n9_2r1w_consumed_by_ex_dep_p (consumer, last_def_reg);
    }
  else
    return n9_3r2w_consumed_by_ex_dep_p (consumer, last_def_reg);
}

/* ------------------------------------------------------------------------ */
