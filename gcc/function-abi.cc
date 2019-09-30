/* Information about fuunction binary interfaces.
   Copyright (C) 2019 Free Software Foundation, Inc.

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
     targetm.hard_regno_call_part_clobbered (NULL, R, M) is true.
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
	    && targetm.hard_regno_call_part_clobbered (NULL, regno, mode))
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
	    && !targetm.hard_regno_call_part_clobbered (NULL, regno, mode))
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
	      && targetm.hard_regno_call_part_clobbered (NULL, regno, mode))
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

/* Return the predefined ABI used by functions with type TYPE.  */

const predefined_function_abi &
fntype_abi (const_tree type)
{
  gcc_assert (FUNC_OR_METHOD_TYPE_P (type));
  return default_function_abi;
}

/* Return the ABI of function decl FNDECL.  */

function_abi
fndecl_abi (const_tree fndecl)
{
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);
  return fntype_abi (TREE_TYPE (fndecl));
}
