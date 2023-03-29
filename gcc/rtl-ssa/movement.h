// RTL SSA utilities relating to instruction movement               -*- C++ -*-
// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// Restrict movement range RANGE so that the instruction is placed later
// than INSN.  (The movement range is the range of instructions after which
// an instruction can be placed.)
inline insn_range_info
move_later_than (insn_range_info range, insn_info *insn)
{
  return { later_insn (range.first, insn), range.last };
}

// Restrict movement range RANGE so that the instruction is placed no earlier
// than INSN.  (The movement range is the range of instructions after which
// an instruction can be placed.)
inline insn_range_info
move_no_earlier_than (insn_range_info range, insn_info *insn)
{
  insn_info *first = later_insn (range.first, insn->prev_nondebug_insn ());
  return { first, range.last };
}

// Restrict movement range RANGE so that the instruction is placed no later
// than INSN.  (The movement range is the range of instructions after which
// an instruction can be placed.)
inline insn_range_info
move_no_later_than (insn_range_info range, insn_info *insn)
{
  return { range.first, earlier_insn (range.last, insn) };
}

// Restrict movement range RANGE so that the instruction is placed earlier
// than INSN.  (The movement range is the range of instructions after which
// an instruction can be placed.)
inline insn_range_info
move_earlier_than (insn_range_info range, insn_info *insn)
{
  insn_info *last = earlier_insn (range.last, insn->prev_nondebug_insn ());
  return { range.first, last };
}

// Return true if it is possible to insert a new instruction after INSN.
inline bool
can_insert_after (insn_info *insn)
{
  return insn->is_bb_head () || (insn->is_real () && !insn->is_jump ());
}

// Try to restrict move range MOVE_RANGE so that it is possible to
// insert INSN after both of the end points.  Return true on success,
// otherwise leave MOVE_RANGE in an invalid state.
inline bool
canonicalize_move_range (insn_range_info &move_range, insn_info *insn)
{
  while (move_range.first != insn && !can_insert_after (move_range.first))
    move_range.first = move_range.first->next_nondebug_insn ();
  while (move_range.last != insn && !can_insert_after (move_range.last))
    move_range.last = move_range.last->prev_nondebug_insn ();
  return bool (move_range);
}

// Try to restrict movement range MOVE_RANGE of INSN so that it can set
// or clobber REGNO.  Assume that if:
//
// - an instruction I2 contains another access A to REGNO; and
// - IGNORE (I2) is true
//
// then either:
//
// - A will be removed; or
// - something will ensure that the new definition of REGNO does not
//   interfere with A, without this having to be enforced by I1's move range.
//
// Return true on success, otherwise leave MOVE_RANGE in an invalid state.
//
// This function only works correctly for instructions that remain within
// the same extended basic block.
template<typename IgnorePredicate>
bool
restrict_movement_for_dead_range (insn_range_info &move_range,
				  unsigned int regno, insn_info *insn,
				  IgnorePredicate ignore)
{
  // Find a definition at or neighboring INSN.
  resource_info resource = full_register (regno);
  def_lookup dl = crtl->ssa->find_def (resource, insn);

  def_info *prev = dl.last_def_of_prev_group ();
  ebb_info *ebb = insn->ebb ();
  if (!prev || prev->ebb () != ebb)
    {
      // REGNO is not defined or used in EBB before INSN, but it
      // might be live on entry.  To keep complexity under control,
      // handle only these cases:
      //
      // - If the register is not live on entry to EBB, the register is
      //   free from the start of EBB to the first definition in EBB.
      //
      // - Otherwise, if the register is live on entry to BB, refuse
      //   to allocate the register.  We could in principle try to move
      //   the instruction to later blocks in the EBB, but it's rarely
      //   worth the effort, and could lead to linear complexity.
      //
      // - Otherwise, don't allow INSN to move earlier than its current
      //   block.  Again, we could in principle look backwards to find where
      //   REGNO dies, but it's rarely worth the effort.
      bb_info *bb = insn->bb ();
      insn_info *limit;
      if (!bitmap_bit_p (DF_LR_IN (ebb->first_bb ()->cfg_bb ()), regno))
	limit = ebb->phi_insn ();
      else if (bitmap_bit_p (DF_LR_IN (bb->cfg_bb ()), regno))
	return false;
      else
	limit = bb->head_insn ();
      move_range = move_later_than (move_range, limit);
    }
  else
    {
      // Stop the instruction moving beyond the previous relevant access
      // to REGNO.
      access_info *prev_access
	= last_access_ignoring (prev, ignore_clobbers::YES, ignore);
      if (prev_access)
	move_range = move_later_than (move_range, access_insn (prev_access));
    }

  // Stop the instruction moving beyond the next relevant definition of REGNO.
  def_info *next = dl.matching_set_or_first_def_of_next_group ();
  next = first_def_ignoring (next, ignore_clobbers::YES, ignore);
  if (next)
    move_range = move_earlier_than (move_range, next->insn ());

  return canonicalize_move_range (move_range, insn);
}

// Try to restrict movement range MOVE_RANGE so that it is possible for the
// instruction being moved ("instruction I1") to perform all the definitions
// in DEFS while still preserving dependencies between those definitions
// and surrounding instructions.  Assume that if:
//
// - DEFS contains a definition D of resource R;
// - an instruction I2 contains another access A to R; and
// - IGNORE (I2) is true
//
// then either:
//
// - A will be removed; or
// - something will ensure that D and A maintain their current order,
//   without this having to be enforced by I1's move range.
//
// Return true on success, otherwise leave MOVE_RANGE in an invalid state.
//
// This function only works correctly for instructions that remain within
// the same extended basic block.
template<typename IgnorePredicate>
bool
restrict_movement_for_defs_ignoring (insn_range_info &move_range,
				     def_array defs, IgnorePredicate ignore)
{
  for (def_info *def : defs)
    {
      // If the definition is a clobber, we can move it with respect
      // to other clobbers.
      //
      // ??? We could also do this if a definition and all its uses
      // are being moved at once.
      bool is_clobber = is_a<clobber_info *> (def);

      // Search back for the first unfiltered use or definition of the
      // same resource.
      access_info *access;
      access = prev_access_ignoring (def, ignore_clobbers (is_clobber),
				     ignore);
      if (access)
	move_range = move_later_than (move_range, access_insn (access));

      // Search forward for the first unfiltered use of DEF,
      // or the first unfiltered definition that follows DEF.
      //
      // We don't need to consider uses of following definitions, since
      // if IGNORE (D->insn ()) is true for some definition D, the caller
      // is guarantees that either
      //
      // - D will be removed, and thus its uses will be removed; or
      // - D will occur after DEF, and thus D's uses will also occur
      //   after DEF.
      //
      // This is purely a simplification: we could also process D's uses,
      // but we don't need to.
      access = next_access_ignoring (def, ignore_clobbers (is_clobber),
				     ignore);
      if (access)
	move_range = move_earlier_than (move_range, access_insn (access));

      // If DEF sets a hard register, take any call clobbers
      // into account.
      unsigned int regno = def->regno ();
      if (!HARD_REGISTER_NUM_P (regno) || is_clobber)
	continue;

      ebb_info *ebb = def->ebb ();
      for (ebb_call_clobbers_info *call_group : ebb->call_clobbers ())
	{
	  if (!call_group->clobbers (def->resource ()))
	    continue;

	  // Exit now if we've already failed, and if the splay accesses
	  // below would be wasted work.
	  if (!move_range)
	    return false;

	  insn_info *insn;
	  insn = prev_call_clobbers_ignoring (*call_group, def->insn (),
					      ignore);
	  if (insn)
	    move_range = move_later_than (move_range, insn);

	  insn = next_call_clobbers_ignoring (*call_group, def->insn (),
					      ignore);
	  if (insn)
	    move_range = move_earlier_than (move_range, insn);
	}
    }

  // Make sure that we don't move stores between basic blocks, since we
  // don't have enough information to tell whether it's safe.
  if (def_info *def = memory_access (defs))
    {
      move_range = move_later_than (move_range, def->bb ()->head_insn ());
      move_range = move_earlier_than (move_range, def->bb ()->end_insn ());
    }

  return bool (move_range);
}

// Like restrict_movement_for_defs_ignoring, but for the uses in USES.
template<typename IgnorePredicate>
bool
restrict_movement_for_uses_ignoring (insn_range_info &move_range,
				     use_array uses, IgnorePredicate ignore)
{
  for (const use_info *use : uses)
    {
      // Ignore uses of undefined values.
      set_info *set = use->def ();
      if (!set)
	continue;

      // Ignore uses by debug instructions.  Debug instructions are
      // never supposed to move, and uses by debug instructions are
      // never supposed to be transferred elsewhere, so we know that
      // the caller must be changing the uses on the debug instruction
      // and checking whether all new uses are available at the debug
      // instruction's original location.
      if (use->is_in_debug_insn ())
	continue;

      // If the used value is defined by an instruction I2 for which
      // IGNORE (I2) is true, the caller guarantees that I2 will occur
      // before change.insn ().  Otherwise, make sure that the use occurs
      // after the definition.
      insn_info *insn = set->insn ();
      if (!ignore (insn))
	move_range = move_later_than (move_range, insn);

      // Search forward for the first unfiltered definition that follows SET.
      //
      // We don't need to consider the uses of these definitions, since
      // if IGNORE (D->insn ()) is true for some definition D, the caller
      // is guarantees that either
      //
      // - D will be removed, and thus its uses will be removed; or
      // - D will occur after USE, and thus D's uses will also occur
      //   after USE.
      //
      // This is purely a simplification: we could also process D's uses,
      // but we don't need to.
      def_info *def;
      def = first_def_ignoring (set->next_def (), ignore_clobbers::NO,
				ignore);
      if (def)
	move_range = move_earlier_than (move_range, def->insn ());

      // If USE uses a hard register, take any call clobbers into account too.
      // SET will necessarily occur after any previous call clobber, so we
      // only need to check for later clobbers.
      unsigned int regno = use->regno ();
      if (!HARD_REGISTER_NUM_P (regno))
	continue;

      ebb_info *ebb = use->ebb ();
      for (ebb_call_clobbers_info *call_group : ebb->call_clobbers ())
	{
	  if (!call_group->clobbers (use->resource ()))
	    continue;

	  if (!move_range)
	    return false;

	  insn_info *insn = next_call_clobbers_ignoring (*call_group,
							 use->insn (), ignore);
	  if (insn)
	    move_range = move_earlier_than (move_range, insn);
	}
    }

  // Make sure that we don't move loads into an earlier basic block.
  //
  // ??? It would be good to relax this for loads that can be safely
  // speculated.
  if (use_info *use = memory_access (uses))
    move_range = move_later_than (move_range, use->bb ()->head_insn ());

  return bool (move_range);
}

}
