/* Information about fuunction binary interfaces.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.

This file is part of GCC

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "function-abi.h"
#include "varasm.h"
#include "cgraph.h"

target_function_abi_info default_target_function_abi_info;
#if SWITCHABLE_TARGET
target_function_abi_info *this_target_function_abi_info
  = &default_target_function_abi_info;
#endif

/* Initialize a predefined function ABI with the given values of
   ID and FULL_REG_CLOBBERS.  */

void
predefined_function_abi::initialize (unsigned int id,
				     const_hard_reg_set full_reg_clobbers)
{
  m_id = id;
  m_initialized = true;
  m_full_reg_clobbers = full_reg_clobbers;

  /* Set up the value of m_full_and_partial_reg_clobbers.

     If the ABI specifies that part of a hard register R is call-clobbered,
     we should be able to find a single-register mode M for which
     targetm.hard_regno_call_part_clobbered (m_id, R, M) is true.
     In other words, it shouldn't be the case that R can hold all
     single-register modes across a call, but can't hold part of
     a multi-register mode.

     If that assumption doesn't hold for a future target, we would need
     to change the interface of TARGET_HARD_REGNO_CALL_PART_CLOBBERED so
     that it tells us which registers in a multi-register value are
     actually clobbered.  */
  m_full_and_partial_reg_clobbers = full_reg_clobbers;
  for (unsigned int i = 0; i < NUM_MACHINE_MODES; ++i)
    {
      machine_mode mode = (machine_mode) i;
      for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	if (targetm.hard_regno_mode_ok (regno, mode)
	    && hard_regno_nregs (regno, mode) == 1
	    && targetm.hard_regno_call_part_clobbered (m_id, regno, mode))
	  SET_HARD_REG_BIT (m_full_and_partial_reg_clobbers, regno);
    }

  /* For each mode MODE, work out which registers are unable to hold
     any part of a MODE value across a call, i.e. those for which no
     overlapping call-preserved (reg:MODE REGNO) exists.

     We assume that this can be flipped around to say that a call
     preserves (reg:MODE REGNO) unless the register overlaps this set.
     The usual reason for this being true is that if (reg:MODE REGNO)
     contains a part-clobbered register, that register would be
     part-clobbered regardless of which part of MODE it holds.
     For example, if (reg:M 2) occupies two registers and if the
     register 3 portion of it is part-clobbered, (reg:M 3) is usually
     either invalid or also part-clobbered.  */
  for (unsigned int i = 0; i < NUM_MACHINE_MODES; ++i)
    {
      machine_mode mode = (machine_mode) i;
      m_mode_clobbers[i] = m_full_and_partial_reg_clobbers;
      for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	if (targetm.hard_regno_mode_ok (regno, mode)
	    && !overlaps_hard_reg_set_p (m_full_reg_clobbers, mode, regno)
	    && !targetm.hard_regno_call_part_clobbered (m_id, regno, mode))
	  remove_from_hard_reg_set (&m_mode_clobbers[i], mode, regno);
    }

  /* Check that the assumptions above actually hold, i.e. that testing
     for single-register modes makes sense, and that overlap tests for
     mode_clobbers work as expected.  */
  if (flag_checking)
    for (unsigned int i = 0; i < NUM_MACHINE_MODES; ++i)
      {
	machine_mode mode = (machine_mode) i;
	const_hard_reg_set all_clobbers = m_full_and_partial_reg_clobbers;
	for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	  if (targetm.hard_regno_mode_ok (regno, mode)
	      && !overlaps_hard_reg_set_p (m_full_reg_clobbers, mode, regno)
	      && targetm.hard_regno_call_part_clobbered (m_id, regno, mode))
	    gcc_assert (overlaps_hard_reg_set_p (all_clobbers, mode, regno)
			&& overlaps_hard_reg_set_p (m_mode_clobbers[i],
						    mode, regno));
      }
}

/* If the ABI has been initialized, add REGNO to the set of registers
   that can be completely altered by a call.  */

void
predefined_function_abi::add_full_reg_clobber (unsigned int regno)
{
  if (!m_initialized)
    return;

  SET_HARD_REG_BIT (m_full_reg_clobbers, regno);
  SET_HARD_REG_BIT (m_full_and_partial_reg_clobbers, regno);
  for (unsigned int i = 0; i < NUM_MACHINE_MODES; ++i)
    SET_HARD_REG_BIT (m_mode_clobbers[i], regno);
}

/* Return the set of registers that the caller of the recorded functions must
   save in order to honor the requirements of CALLER_ABI.  */

HARD_REG_SET
function_abi_aggregator::
caller_save_regs (const function_abi &caller_abi) const
{
  HARD_REG_SET result;
  CLEAR_HARD_REG_SET (result);
  for (unsigned int abi_id = 0; abi_id < NUM_ABI_IDS; ++abi_id)
    {
      const predefined_function_abi &callee_abi = function_abis[abi_id];

      /* Skip cases that clearly aren't problematic.  */
      if (abi_id == caller_abi.id ()
	  || hard_reg_set_empty_p (m_abi_clobbers[abi_id]))
	continue;

      /* Collect the set of registers that can be "more clobbered" by
	 CALLEE_ABI than by CALLER_ABI.  */
      HARD_REG_SET extra_clobbers;
      CLEAR_HARD_REG_SET (extra_clobbers);
      for (unsigned int i = 0; i < NUM_MACHINE_MODES; ++i)
	{
	  machine_mode mode = (machine_mode) i;
	  extra_clobbers |= (callee_abi.mode_clobbers (mode)
			     & ~caller_abi.mode_clobbers (mode));
	}

      /* Restrict it to the set of registers that we actually saw
	 clobbers for (e.g. taking -fipa-ra into account).  */
      result |= (extra_clobbers & m_abi_clobbers[abi_id]);
    }
  return result;
}

/* Return the set of registers that cannot be used to hold a value of
   mode MODE across the calls in a region described by ABIS and MASK, where:

   * Bit ID of ABIS is set if the region contains a call with
     function_abi identifier ID.

   * MASK contains all the registers that are fully or partially
     clobbered by calls in the region.

   This is not quite as accurate as testing each individual call,
   but it's a close and conservatively-correct approximation.
   It's much better for some targets than just using MASK.  */

HARD_REG_SET
call_clobbers_in_region (unsigned int abis, const_hard_reg_set mask,
			 machine_mode mode)
{
  HARD_REG_SET result;
  CLEAR_HARD_REG_SET (result);
  for (unsigned int id = 0; abis; abis >>= 1, ++id)
    if (abis & 1)
      result |= function_abis[id].mode_clobbers (mode);
  return result & mask;
}

/* Return the predefined ABI used by functions with type TYPE.  */

const predefined_function_abi &
fntype_abi (const_tree type)
{
  gcc_assert (FUNC_OR_METHOD_TYPE_P (type));
  if (targetm.calls.fntype_abi)
    return targetm.calls.fntype_abi (type);
  return default_function_abi;
}

/* Return the ABI of function decl FNDECL.  */

function_abi
fndecl_abi (const_tree fndecl)
{
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);
  const predefined_function_abi &base_abi = fntype_abi (TREE_TYPE (fndecl));

  if (flag_ipa_ra && decl_binds_to_current_def_p (fndecl))
    if (cgraph_rtl_info *info = cgraph_node::rtl_info (fndecl))
      return function_abi (base_abi, info->function_used_regs);

  return base_abi;
}

/* Return the ABI of the function called by INSN.  */

function_abi
insn_callee_abi (const rtx_insn *insn)
{
  gcc_assert (insn && CALL_P (insn));

  if (flag_ipa_ra)
    if (tree fndecl = get_call_fndecl (insn))
      return fndecl_abi (fndecl);

  if (targetm.calls.insn_callee_abi)
    return targetm.calls.insn_callee_abi (insn);

  return default_function_abi;
}

/* Return the ABI of the function called by CALL_EXPR EXP.  Return the
   default ABI for erroneous calls.  */

function_abi
expr_callee_abi (const_tree exp)
{
  gcc_assert (TREE_CODE (exp) == CALL_EXPR);

  if (tree fndecl = get_callee_fndecl (exp))
    return fndecl_abi (fndecl);

  tree callee = CALL_EXPR_FN (exp);
  if (callee == error_mark_node)
    return default_function_abi;

  tree type = TREE_TYPE (callee);
  if (type == error_mark_node)
    return default_function_abi;

  gcc_assert (POINTER_TYPE_P (type));
  return fntype_abi (TREE_TYPE (type));
}
