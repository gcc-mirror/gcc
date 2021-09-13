// RTL SSA utility functions for changing instructions              -*- C++ -*-
// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

namespace rtl_ssa {

// Return true if INSN is one of the instructions being changed by CHANGES.
inline bool
insn_is_changing (array_slice<insn_change *const> changes,
		  const insn_info *insn)
{
  for (const insn_change *change : changes)
    if (change->insn () == insn)
      return true;
  return false;
}

// Return a closure of insn_is_changing, for use as a predicate.
// This could be done using local lambdas instead, but the predicate is
// used often enough that having a class should be more convenient and allow
// reuse of template instantiations.
//
// We don't use std::bind because it would involve an indirect function call,
// whereas this function is used in relatively performance-critical code.
inline insn_is_changing_closure
insn_is_changing (array_slice<insn_change *const> changes)
{
  return insn_is_changing_closure (changes);
}

// Restrict CHANGE.move_range so that the changed instruction can perform
// all its definitions and uses.  Assume that if:
//
// - CHANGE contains an access A1 of resource R;
// - an instruction I2 contains another access A2 to R; and
// - IGNORE (I2) is true
//
// then either:
//
// - A2 will be removed; or
// - something will ensure that A1 and A2 maintain their current order,
//   without this having to be enforced by CHANGE's move range.
//
// IGNORE should return true for CHANGE.insn ().
//
// Return true on success, otherwise leave CHANGE.move_range in an invalid
// state.
//
// This function only works correctly for instructions that remain within
// the same extended basic block.
template<typename IgnorePredicate>
bool
restrict_movement_ignoring (insn_change &change, IgnorePredicate ignore)
{
  // Uses generally lead to failure quicker, so test those first.
  return (restrict_movement_for_uses_ignoring (change.move_range,
					       change.new_uses, ignore)
	  && restrict_movement_for_defs_ignoring (change.move_range,
						  change.new_defs, ignore)
	  && canonicalize_move_range (change.move_range, change.insn ()));
}

// Like restrict_movement_ignoring, but ignore only the instruction
// that is being changed.
inline bool
restrict_movement (insn_change &change)
{
  return restrict_movement_ignoring (change, insn_is (change.insn ()));
}

using add_regno_clobber_fn = std::function<bool (insn_change &,
						 unsigned int)>;
bool recog_internal (insn_change &, add_regno_clobber_fn);

// Try to recognize the new instruction pattern for CHANGE, potentially
// tweaking the pattern or adding extra clobbers in order to make it match.
//
// When adding an extra clobber for register R, restrict CHANGE.move_range
// to a range of instructions for which R is not live.  When determining
// whether R is live, ignore accesses made by an instruction I if
// IGNORE (I) is true.  The caller then assumes the responsibility
// of ensuring that CHANGE and I are placed in a valid order.
//
// IGNORE should return true for CHANGE.insn ().
//
// Return true on success.  Leave CHANGE unmodified on failure.
template<typename IgnorePredicate>
inline bool
recog_ignoring (obstack_watermark &watermark, insn_change &change,
		IgnorePredicate ignore)
{
  auto add_regno_clobber = [&](insn_change &change, unsigned int regno)
    {
      return crtl->ssa->add_regno_clobber (watermark, change, regno, ignore);
    };
  return recog_internal (change, add_regno_clobber);
}

// As for recog_ignoring, but ignore only the instruction that is being
// changed.
inline bool
recog (obstack_watermark &watermark, insn_change &change)
{
  return recog_ignoring (watermark, change, insn_is (change.insn ()));
}

// Check whether insn costs indicate that the net effect of the changes
// in CHANGES is worthwhile.  Require a strict improvement if STRICT_P,
// otherwise allow the new instructions to be the same cost as the old
// instructions.
bool changes_are_worthwhile (array_slice<insn_change *const> changes,
			     bool strict_p = false);

// Like changes_are_worthwhile, but for a single change.
inline bool
change_is_worthwhile (insn_change &change, bool strict_p = false)
{
  insn_change *changes[] = { &change };
  return changes_are_worthwhile (changes, strict_p);
}

}
