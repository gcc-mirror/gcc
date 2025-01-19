// RTL SSA utilities relating to instruction movement               -*- C++ -*-
// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// Return true if INSN can in principle be moved around, and if RTL-SSA
// has enough information to do that.
bool can_move_insn_p (insn_info *);

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
  return (insn->is_bb_head ()
	  || (insn->is_real () && !control_flow_insn_p (insn->rtl ())));
}

// Try to restrict move range MOVE_RANGE so that it is possible to
// insert INSN after both of the end points.  Return true on success,
// otherwise leave MOVE_RANGE in an invalid state.
inline bool
canonicalize_move_range (insn_range_info &move_range, insn_info *insn)
{
  while (move_range.first != insn && !can_insert_after (move_range.first))
    if (auto *next = move_range.first->next_nondebug_insn ())
      move_range.first = next;
    else
      {
	// Invalidate the range.  prev_nondebug_insn is always nonnull
	// if next_nondebug_insn is null.
	move_range.last = move_range.first->prev_nondebug_insn ();
	return false;
      }
  while (move_range.last != insn && !can_insert_after (move_range.last))
    if (auto *prev = move_range.last->prev_nondebug_insn ())
      move_range.last = prev;
    else
      {
	// Invalidate the range.  next_nondebug_insn is always nonnull
	// if prev_nondebug_insn is null.
	move_range.first = move_range.last->next_nondebug_insn ();
	return false;
      }
  return bool (move_range);
}

// Try to restrict movement range MOVE_RANGE of INSN so that it can set
// or clobber REGNO.
//
// IGNORE is an object that provides the same interface as ignore_nothing.
// Assume that if:
//
// - an instruction I2 contains another access A to REGNO; and
// - IGNORE says that I2 should be ignored
//
// then either:
//
// - A will be removed; or
// - something will ensure that the new definition of REGNO does not
//   interfere with A, without this having to be enforced by I1's move range.
//
// If IGNORE says that a definition D of REGNO should be ignored, assume that
// the new definition of REGNO will not conflict with D.
//
// Return true on success, otherwise leave MOVE_RANGE in an invalid state.
//
// This function only works correctly for instructions that remain within
// the same extended basic block.
template<typename IgnorePredicates>
bool
restrict_movement_for_dead_range (insn_range_info &move_range,
				  unsigned int regno, insn_info *insn,
				  IgnorePredicates ignore)
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
      access_info *prev_access = last_access (prev, ignore_clobbers::YES,
					      ignore);
      if (prev_access)
	move_range = move_later_than (move_range, access_insn (prev_access));
    }

  // Stop the instruction moving beyond the next relevant use or definition
  // of REGNO.
  def_info *next = dl.matching_set_or_first_def_of_next_group ();
  access_info *next_access = first_access (next, ignore_clobbers::YES, ignore);
  if (next_access)
    move_range = move_earlier_than (move_range, access_insn (next_access));

  return canonicalize_move_range (move_range, insn);
}

// Try to restrict movement range MOVE_RANGE so that it is possible for the
// instruction being moved ("instruction I1") to perform all the definitions
// in DEFS while still preserving dependencies between those definitions
// and surrounding instructions.
//
// IGNORE is an object that provides the same interface as ignore_nothing.
// Assume that if:
//
// - DEFS contains a definition D of resource R;
// - an instruction I2 contains another access A to R; and
// - IGNORE says that I2 should be ignored
//
// then either:
//
// - A will be removed; or
// - something will ensure that D and A maintain their current order,
//   without this having to be enforced by I1's move range.
//
// Assume the same thing about a definition D and all uses of D if IGNORE
// says that D should be ignored.
//
// Return true on success, otherwise leave MOVE_RANGE in an invalid state.
//
// This function only works correctly for instructions that remain within
// the same extended basic block.
template<typename IgnorePredicates>
bool
restrict_movement_for_defs (insn_range_info &move_range, def_array defs,
			    IgnorePredicates ignore)
{
  for (def_info *def : defs)
    {
      // Skip fresh defs that are being inserted, as these shouldn't
      // constrain movement.
      if (def->is_temporary ())
	continue;

      // If the definition is a clobber, we can move it with respect
      // to other clobbers.
      //
      // ??? We could also do this if a definition and all its uses
      // are being moved at once.
      bool is_clobber = is_a<clobber_info *> (def);

      // Search back for the first relevant use or definition of the
      // same resource.
      access_info *access;
      access = prev_access (def, ignore_clobbers (is_clobber), ignore);
      if (access)
	move_range = move_later_than (move_range, access_insn (access));

      // Search forward for the next relevant use or definition of the
      // same resource.
      access = next_access (def, ignore_clobbers (is_clobber), ignore);
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
	  insn = prev_call_clobbers (*call_group, def->insn (), ignore);
	  if (insn)
	    move_range = move_later_than (move_range, insn);

	  insn = next_call_clobbers (*call_group, def->insn (), ignore);
	  if (insn)
	    move_range = move_earlier_than (move_range, insn);
	}
    }

  // Make sure that we don't move stores between basic blocks, since we
  // don't have enough information to tell whether it's safe.
  def_info *def = memory_access (defs);
  if (def && !def->is_temporary ())
    {
      move_range = move_later_than (move_range, def->bb ()->head_insn ());
      move_range = move_earlier_than (move_range, def->bb ()->end_insn ());
    }

  return bool (move_range);
}

// Like restrict_movement_for_defs, but for the uses in USES.
template<typename IgnorePredicates>
bool
restrict_movement_for_uses (insn_range_info &move_range, use_array uses,
			    IgnorePredicates ignore)
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

      // If the used value is defined by an ignored instruction I2,
      // the caller guarantees that I2 will occur before change.insn ()
      // and that its value will still be available at change.insn ().
      // Otherwise, make sure that the use occurs after the definition.
      insn_info *insn = set->insn ();
      if (!ignore.should_ignore_def (set)
	  && !ignore.should_ignore_insn (insn))
	move_range = move_later_than (move_range, insn);

      // Search forward after SET's live range for the first relevant
      // use or definition of the same resource.
      access_info *access;
      access = first_access (set->next_def (), ignore_clobbers::NO, ignore);
      if (access)
	move_range = move_earlier_than (move_range, access_insn (access));

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

	  insn_info *insn = next_call_clobbers (*call_group, use->insn (),
						ignore);
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
