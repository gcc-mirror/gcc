// RTL SSA routines for changing instructions                       -*- C++ -*-
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

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_ARRAY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "rtl-ssa/internals.h"
#include "rtl-ssa/internals.inl"
#include "target.h"
#include "predict.h"
#include "memmodel.h" // Needed by emit-rtl.h
#include "emit-rtl.h"
#include "cfghooks.h"
#include "cfgrtl.h"
#include "sreal.h"

using namespace rtl_ssa;

// See the comment above the declaration.
void
insn_change::print (pretty_printer *pp) const
{
  if (m_is_deletion)
    {
      pp_string (pp, "deletion of ");
      pp_insn (pp, m_insn);
    }
  else
    {
      pp_string (pp, "change to ");
      pp_insn (pp, m_insn);
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "~~~~~~~");

      pp_newline_and_indent (pp, 0);
      pp_string (pp, "new cost: ");
      pp_decimal_int (pp, new_cost);

      pp_newline_and_indent (pp, 0);
      pp_string (pp, "new uses:");
      pp_newline_and_indent (pp, 2);
      pp_accesses (pp, new_uses);
      pp_indentation (pp) -= 2;

      pp_newline_and_indent (pp, 0);
      pp_string (pp, "new defs:");
      pp_newline_and_indent (pp, 2);
      pp_accesses (pp, new_defs);
      pp_indentation (pp) -= 2;

      pp_newline_and_indent (pp, 0);
      pp_string (pp, "first insert-after candidate: ");
      move_range.first->print_identifier_and_location (pp);

      pp_newline_and_indent (pp, 0);
      pp_string (pp, "last insert-after candidate: ");
      move_range.last->print_identifier_and_location (pp);
    }
}

// Return a copy of access_array ACCESSES, allocating it on the
// temporary obstack.
access_array
function_info::temp_access_array (access_array accesses)
{
  if (accesses.empty ())
    return accesses;

  gcc_assert (obstack_object_size (&m_temp_obstack) == 0);
  obstack_grow (&m_temp_obstack, accesses.begin (), accesses.size_bytes ());
  return { static_cast<access_info **> (obstack_finish (&m_temp_obstack)),
	   accesses.size () };
}

// See the comment above the declaration.
bool
function_info::verify_insn_changes (array_slice<insn_change *const> changes)
{
  HARD_REG_SET defined_hard_regs, clobbered_hard_regs;
  CLEAR_HARD_REG_SET (defined_hard_regs);
  CLEAR_HARD_REG_SET (clobbered_hard_regs);

  insn_info *min_insn = m_first_insn;
  for (insn_change *change : changes)
    if (!change->is_deletion ())
      {
	// Make sure that the changes can be kept in their current order
	// while honoring all of the move ranges.
	min_insn = later_insn (min_insn, change->move_range.first);
	while (min_insn != change->insn () && !can_insert_after (min_insn))
	  min_insn = min_insn->next_nondebug_insn ();
	if (*min_insn > *change->move_range.last)
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "no viable insn position assignment\n");
	    return false;
	  }

	// If recog introduced new clobbers of a register as part of
	// the matching process, make sure that they don't conflict
	// with any other new definitions or uses of the register.
	// (We have already checked that they don't conflict with
	// unchanging definitions and uses.)
	for (use_info *use : change->new_uses)
	  {
	    unsigned int regno = use->regno ();
	    if (HARD_REGISTER_NUM_P (regno)
		&& TEST_HARD_REG_BIT (clobbered_hard_regs, regno))
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "register %d would be clobbered"
			   " while it is still live\n", regno);
		return false;
	      }
	  }
	for (def_info *def : change->new_defs)
	  {
	    unsigned int regno = def->regno ();
	    if (HARD_REGISTER_NUM_P (regno))
	      {
		if (def->m_is_temp)
		  {
		    // This is a clobber introduced by recog.
		    gcc_checking_assert (is_a<clobber_info *> (def));
		    if (TEST_HARD_REG_BIT (defined_hard_regs, regno))
		      {
			if (dump_file && (dump_flags & TDF_DETAILS))
			  fprintf (dump_file, "conflicting definitions of"
				   " register %d\n", regno);
			return false;
		      }
		    SET_HARD_REG_BIT (clobbered_hard_regs, regno);
		  }
		else if (is_a<set_info *> (def))
		  {
		    // REGNO now has a defined value.
		    SET_HARD_REG_BIT (defined_hard_regs, regno);
		    CLEAR_HARD_REG_BIT (clobbered_hard_regs, regno);
		  }
	      }
	  }
      }
  return true;
}

// See the comment above the declaration.
bool
rtl_ssa::changes_are_worthwhile (array_slice<insn_change *const> changes,
				 bool strict_p)
{
  unsigned int old_cost = 0;
  sreal weighted_old_cost = 0;
  auto entry_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
  {
    undo_recog_changes undo (0);
    for (insn_change *change : changes)
      {
	// Count zero for the old cost if the old instruction was a no-op
	// move or had an unknown cost.  This should reduce the chances of
	// making an unprofitable change.
	old_cost += change->old_cost ();
	basic_block cfg_bb = change->bb ()->cfg_bb ();
	if (optimize_bb_for_speed_p (cfg_bb))
	  weighted_old_cost += (cfg_bb->count.to_sreal_scale (entry_count)
				* change->old_cost ());

      }
  }
  unsigned int new_cost = 0;
  sreal weighted_new_cost = 0;
  for (insn_change *change : changes)
    {
      basic_block cfg_bb = change->bb ()->cfg_bb ();
      bool for_speed = optimize_bb_for_speed_p (cfg_bb);
      if (!change->is_deletion ()
	  && INSN_CODE (change->rtl ()) != NOOP_MOVE_INSN_CODE)
	{
	  change->new_cost = insn_cost (change->rtl (), for_speed);
	  /* If the cost is unknown, replacement is not worthwhile.  */
	  if (!change->new_cost)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "Reject replacement due to unknown insn cost.\n");
	      return false;
	    }
	  new_cost += change->new_cost;
	  if (for_speed)
	    weighted_new_cost += (cfg_bb->count.to_sreal_scale (entry_count)
				  * change->new_cost);
	}
    }
  bool ok_p;
  if (weighted_new_cost != weighted_old_cost)
    ok_p = weighted_new_cost < weighted_old_cost;
  else if (strict_p)
    ok_p = new_cost < old_cost;
  else
    ok_p = new_cost <= old_cost;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "original cost");
      char sep = '=';
      for (const insn_change *change : changes)
	{
	  fprintf (dump_file, " %c %d", sep, change->old_cost ());
	  sep = '+';
	}
      if (weighted_old_cost != 0)
	fprintf (dump_file, " (weighted: %f)", weighted_old_cost.to_double ());
      fprintf (dump_file, ", replacement cost");
      sep = '=';
      for (const insn_change *change : changes)
	if (!change->is_deletion ())
	  {
	    if (INSN_CODE (change->rtl ()) == NOOP_MOVE_INSN_CODE)
	      fprintf (dump_file, " %c nop", sep);
	    else
	      fprintf (dump_file, " %c %d", sep, change->new_cost);
	    sep = '+';
	  }
      if (weighted_new_cost != 0)
	fprintf (dump_file, " (weighted: %f)", weighted_new_cost.to_double ());
      fprintf (dump_file, "; %s\n",
	       ok_p ? "keeping replacement" : "rejecting replacement");
    }
  if (!ok_p)
    return false;

  return true;
}

// SET has been deleted.  Clean up all remaining uses.  Such uses are
// either dead phis or now-redundant live-out uses.
void
function_info::process_uses_of_deleted_def (set_info *set)
{
  if (!set->has_any_uses ())
    return;

  auto *use = *set->all_uses ().begin ();
  do
    {
      auto *next_use = use->next_use ();
      if (use->is_in_phi ())
	{
	  // This call will not recurse.
	  process_uses_of_deleted_def (use->phi ());
	  delete_phi (use->phi ());
	}
      else
	{
	  gcc_assert (use->is_live_out_use ());
	  remove_use (use);
	}
      use = next_use;
    }
  while (use);
  gcc_assert (!set->has_any_uses ());
}

// Update the REG_NOTES of INSN, whose pattern has just been changed.
static void
update_notes (rtx_insn *insn)
{
  for (rtx *note_ptr = &REG_NOTES (insn); *note_ptr; )
    {
      rtx note = *note_ptr;
      bool keep_p = true;
      switch (REG_NOTE_KIND (note))
	{
	case REG_EQUAL:
	case REG_EQUIV:
	case REG_NOALIAS:
	  keep_p = (single_set (insn) != nullptr);
	  break;

	case REG_UNUSED:
	case REG_DEAD:
	  // These notes are stale.  We'll recompute REG_UNUSED notes
	  // after the update.
	  keep_p = false;
	  break;

	default:
	  break;
	}
      if (keep_p)
	note_ptr = &XEXP (*note_ptr, 1);
      else
	{
	  *note_ptr = XEXP (*note_ptr, 1);
	  free_EXPR_LIST_node (note);
	}
    }
}

// Pick a location for CHANGE's instruction and return the instruction
// after which it should be placed.
static insn_info *
choose_insn_placement (insn_change &change)
{
  gcc_checking_assert (change.move_range);

  insn_info *insn = change.insn ();
  insn_info *first = change.move_range.first;
  insn_info *last = change.move_range.last;

  // Quick(ish) exit if there is only one possible choice.
  if (first == last)
    return first;
  if (first == insn->prev_nondebug_insn () && last == insn)
    return insn;

  // For now just use the closest valid choice to the original instruction.
  // If the register usage has changed significantly, it might instead be
  // better to try to take register pressure into account.
  insn_info *closest = change.move_range.clamp_insn_to_range (insn);
  while (closest != insn && !can_insert_after (closest))
    closest = closest->next_nondebug_insn ();
  return closest;
}

// Record any changes related to CHANGE that need to be queued for later.
void
function_info::possibly_queue_changes (insn_change &change)
{
  insn_info *insn = change.insn ();
  rtx_insn *rtl = insn->rtl ();

  // If the instruction could previously throw, we eventually need to call
  // purge_dead_edges to check whether things have changed.
  if (find_reg_note (rtl, REG_EH_REGION, nullptr))
    bitmap_set_bit (m_need_to_purge_dead_edges, insn->bb ()->index ());

  auto needs_pending_update = [&]()
    {
      // If an instruction became a no-op without the pass explicitly
      // deleting it, queue the deletion for later.  Removing the
      // instruction on the fly would require an update to all instructions
      // that use the result of the move, which would be a potential source
      // of quadraticness.  Also, definitions shouldn't disappear under
      // the pass's feet.
      if (INSN_CODE (rtl) == NOOP_MOVE_INSN_CODE)
	return true;

      // If any jumps got turned into unconditional jumps or nops, we need
      // to update the CFG accordingly.
      if (JUMP_P (rtl)
	  && (returnjump_p (rtl) || any_uncondjump_p (rtl))
	  && !single_succ_p (insn->bb ()->cfg_bb ()))
	return true;

      // If a previously conditional trap now always fires, execution
      // terminates at that point.
      rtx pattern = PATTERN (rtl);
      if (GET_CODE (pattern) == TRAP_IF
	  && XEXP (pattern, 0) == const1_rtx)
	return true;

      return false;
    };

  if (needs_pending_update ()
      && bitmap_set_bit (m_queued_insn_update_uids, insn->uid ()))
    {
      gcc_assert (!change.is_deletion ());
      m_queued_insn_updates.safe_push (insn);
    }
}

// Remove the instruction described by CHANGE from the underlying RTL
// and from the insn_info list.
static void
delete_insn (insn_change &change)
{
  insn_info *insn = change.insn ();
  rtx_insn *rtl = change.rtl ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "deleting insn %d\n", insn->uid ());
  set_insn_deleted (rtl);
}

// Move the RTL instruction associated with CHANGE so that it comes
// immediately after AFTER.
static void
move_insn (insn_change &change, insn_info *after)
{
  rtx_insn *rtl = change.rtl ();
  rtx_insn *after_rtl = after->rtl ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "moving insn %d after insn %d\n",
	     INSN_UID (rtl), INSN_UID (after_rtl));

  // At the moment we don't support moving instructions between EBBs,
  // but this would be worth adding if it's useful.
  insn_info *insn = change.insn ();

  bb_info *bb = after->bb ();
  basic_block cfg_bb = bb->cfg_bb ();

  if (!insn->is_temporary ())
    {
      gcc_assert (after->ebb () == insn->ebb ());

      if (insn->bb () != bb)
	// Force DF to mark the old block as dirty.
	df_insn_delete (rtl);
      ::remove_insn (rtl);
    }

  ::add_insn_after (rtl, after_rtl, cfg_bb);
}

// The instruction associated with CHANGE is being changed in-place.
// Update the DF information for its new pattern.
static void
update_insn_in_place (insn_change &change)
{
  insn_info *insn = change.insn ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "updating insn %d in-place\n", insn->uid ());
  df_insn_rescan (change.rtl ());
}

// Finalize the new list of definitions and uses in CHANGE, removing
// any uses and definitions that are no longer needed, and converting
// pending clobbers into actual definitions.
//
// POS gives the final position of INSN, which hasn't yet been moved into
// place.  Keep track of any newly-created set_infos being added with this
// change by adding them to NEW_SETS.
void
function_info::finalize_new_accesses (insn_change &change, insn_info *pos,
				      hash_set<def_info *> &new_sets)
{
  insn_info *insn = change.insn ();

  // Get a list of all the things that the instruction now references.
  vec_rtx_properties properties;
  properties.add_insn (insn->rtl (), true);

  // Build up the new list of definitions.
  for (rtx_obj_reference ref : properties.refs ())
    if (ref.is_write ())
      {
	def_info *def = find_access (change.new_defs, ref.regno);
	gcc_assert (def);

	if (def->m_is_temp && is_a<set_info *> (def) && def->last_def ())
	  {
	    // For temporary sets being added with this change, we keep track of
	    // the corresponding permanent def using the last_def link.
	    //
	    // So if we have one of these, follow it to get the permanent def.
	    def = def->last_def ();
	    gcc_assert (!def->m_is_temp && !def->m_has_been_superceded);
	  }

	if (def->m_is_temp)
	  {
	    if (is_a<clobber_info *> (def))
	      def = allocate<clobber_info> (change.insn (), ref.regno);
	    else if (is_a<set_info *> (def))
	      {
		// Install the permanent set in the last_def link of the
		// temporary def.  This allows us to find the permanent def
		// later in case we see a second write to the same resource.
		def_info *perm_def = allocate<set_info> (change.insn (),
							 def->resource ());

		// Keep track of the new set so we remember to add it to the
		// def chain later.
		if (new_sets.add (perm_def))
		  gcc_unreachable (); // We shouldn't see duplicates here.

		def->set_last_def (perm_def);
		def = perm_def;
	      }
	    else
	      gcc_unreachable ();
	  }
	else if (!def->m_has_been_superceded)
	  {
	    // This is a second or subsequent definition.
	    // See function_info::record_def for a discussion of when
	    // this can happen.
	    def->record_reference (ref, false);
	    continue;
	  }
	else
	  {
	    def->m_has_been_superceded = false;

	    // Clobbers can move around, so remove them from their current
	    // position and them back in their final position.
	    //
	    // At the moment, we don't allow sets to move relative to other
	    // definitions of the same resource, so we can leave those where
	    // they are.  It might be useful to relax this in future.
	    // The main complication is that removing a set would potentially
	    // fuse two adjoining clobber_groups, and adding the set back
	    // would require the group to be split again.
	    if (is_a<clobber_info *> (def))
	      remove_def (def);
	    else if (ref.is_reg ())
	      def->set_mode (ref.mode);
	    def->set_insn (insn);
	  }
	def->record_reference (ref, true);
	m_temp_defs.safe_push (def);
      }

  // Also keep any explicitly-recorded call clobbers, which are deliberately
  // excluded from the vec_rtx_properties.  Calls shouldn't move, so we can
  // keep the definitions in their current position.
  //
  // If the change describes a set of memory, but the pattern doesn't
  // reference memory, keep the set anyway.  This can happen if the
  // old pattern was a parallel that contained a memory clobber, and if
  // the new pattern was recognized without that clobber.  Keeping the
  // set avoids a linear-complexity update to the set's users.
  //
  // ??? We could queue an update so that these bogus clobbers are
  // removed later.
  for (def_info *def : change.new_defs)
    if (def->m_has_been_superceded
	&& (def->is_call_clobber () || def->is_mem ()))
      {
	def->m_has_been_superceded = false;
	def->set_insn (insn);
	m_temp_defs.safe_push (def);
      }

  // Install the new list of definitions in CHANGE.
  sort_accesses (m_temp_defs);
  access_array accesses = temp_access_array (m_temp_defs);
  change.new_defs = def_array (accesses);
  m_temp_defs.truncate (0);

  // Create temporary copies of use_infos that are already attached to
  // other insns, which could happen if the uses come from unchanging
  // insns or if they have been used by earlier changes.  Doing this
  // makes it easier to detect multiple reads below.
  auto *unshared_uses_base = XOBNEWVEC (&m_temp_obstack, access_info *,
					change.new_uses.size ());
  unsigned int i = 0;
  for (use_info *use : change.new_uses)
    {
      if (!use->m_has_been_superceded)
	{
	  use = allocate_temp<use_info> (insn, use->resource (), use->def ());
	  use->m_has_been_superceded = true;
	  use->m_is_temp = true;
	}
      unshared_uses_base[i++] = use;
    }
  auto unshared_uses = use_array (unshared_uses_base, change.new_uses.size ());

  // Add (possibly temporary) uses to m_temp_uses for each resource.
  // If there are multiple references to the same resource, aggregate
  // information in the modes and flags.
  use_info *mem_use = nullptr;
  for (rtx_obj_reference ref : properties.refs ())
    if (ref.is_read ())
      {
	unsigned int regno = ref.regno;
	machine_mode mode = ref.is_reg () ? ref.mode : BLKmode;
	use_info *use = find_access (unshared_uses, ref.regno);
	if (!use)
	  {
	    // For now, we only support inferring uses of mem.
	    gcc_assert (regno == MEM_REGNO);

	    if (mem_use)
	      {
		mem_use->record_reference (ref, false);
		continue;
	      }

	    resource_info resource { mode, regno };
	    auto def = find_def (resource, pos).prev_def (pos);
	    auto set = safe_dyn_cast <set_info *> (def);
	    gcc_assert (set);
	    mem_use = allocate<use_info> (insn, resource, set);
	    mem_use->record_reference (ref, true);
	    m_temp_uses.safe_push (mem_use);
	    continue;
	  }

	if (use->m_has_been_superceded)
	  {
	    // This is the first reference to the resource.
	    bool is_temp = use->m_is_temp;
	    *use = use_info (insn, resource_info { mode, regno }, use->def ());
	    use->m_is_temp = is_temp;
	    use->record_reference (ref, true);
	    m_temp_uses.safe_push (use);
	  }
	else
	  {
	    // Record the mode of the largest use.  The choice is arbitrary if
	    // the instruction (unusually) references the same register in two
	    // different but equal-sized modes.
	    if (HARD_REGISTER_NUM_P (regno)
		&& partial_subreg_p (use->mode (), mode))
	      use->set_mode (mode);
	    use->record_reference (ref, false);
	  }
      }

  // Replace any temporary uses and definitions with real ones.
  for (unsigned int i = 0; i < m_temp_uses.length (); ++i)
    {
      auto *use = as_a<use_info *> (m_temp_uses[i]);
      if (use->m_is_temp)
	{
	  m_temp_uses[i] = use = allocate<use_info> (*use);
	  use->m_is_temp = false;
	  set_info *def = use->def ();
	  if (!def || !def->m_is_temp)
	    continue;

	  if (auto phi = dyn_cast<phi_info *> (def))
	    {
	      // Handle cases in which the value was previously not used
	      // within the block.
	      gcc_assert (phi->is_degenerate ());
	      phi = create_degenerate_phi (phi->ebb (), phi->input_value (0));
	      use->set_def (phi);
	    }
	  else
	    {
	      // The temporary def may also be a set added with this change, in
	      // which case the permanent set is stored in the last_def link,
	      // and we need to update the use to refer to the permanent set.
	      gcc_assert (is_a<set_info *> (def));
	      auto perm_set = as_a<set_info *> (def->last_def ());
	      gcc_assert (!perm_set->is_temporary ());
	      use->set_def (perm_set);
	    }
	}
    }

  // Install the new list of uses in CHANGE.
  sort_accesses (m_temp_uses);
  change.new_uses = use_array (temp_access_array (m_temp_uses));
  m_temp_uses.truncate (0);

  // Record the new instruction-wide properties.
  insn->set_properties (properties);
}

// Copy information from CHANGE to its underlying insn_info, given that
// the insn_info has already been placed appropriately.  NEW_SETS contains the
// new set_infos that are being added as part of this change (as opposed to
// being moved or repurposed from existing instructions).
void
function_info::apply_changes_to_insn (insn_change &change,
				      hash_set<def_info *> &new_sets)
{
  insn_info *insn = change.insn ();
  if (change.is_deletion ())
    {
      insn->set_accesses (nullptr, 0, 0);
      return;
    }

  // Copy the cost.
  insn->set_cost (change.new_cost);

  // Add all clobbers and newly-created sets.  Existing sets and call
  // clobbers never move relative to other definitions, so are OK as-is.
  for (def_info *def : change.new_defs)
    if ((is_a<clobber_info *> (def) && !def->is_call_clobber ())
	|| (is_a<set_info *> (def) && new_sets.contains (def)))
      add_def (def);

  // Add all uses, now that their position is final.
  for (use_info *use : change.new_uses)
    add_use (use);

  // Copy the uses and definitions.
  unsigned int num_defs = change.new_defs.size ();
  unsigned int num_uses = change.new_uses.size ();
  if (num_defs + num_uses <= insn->num_defs () + insn->num_uses ())
    insn->copy_accesses (change.new_defs, change.new_uses);
  else
    {
      access_array_builder builder (&m_obstack);
      builder.reserve (num_defs + num_uses);

      for (def_info *def : change.new_defs)
	builder.quick_push (def);
      for (use_info *use : change.new_uses)
	builder.quick_push (use);

      insn->set_accesses (builder.finish ().begin (), num_defs, num_uses);
    }

  insn->m_is_temp = false;
}

// Add a temporary placeholder instruction after AFTER.
insn_info *
function_info::add_placeholder_after (insn_info *after)
{
  insn_info *insn = allocate_temp<insn_info> (after->bb (), nullptr, -1);
  add_insn_after (insn, after);
  return insn;
}

// See the comment above the declaration.
void
function_info::change_insns (array_slice<insn_change *> changes)
{
  auto watermark = temp_watermark ();

  insn_info *min_insn = m_first_insn;
  for (insn_change *change : changes)
    {
      // Tentatively mark all the old uses and definitions for deletion.
      for (use_info *use : change->old_uses ())
	{
	  use->m_has_been_superceded = true;
	  remove_use (use);
	}
      for (def_info *def : change->old_defs ())
	def->m_has_been_superceded = true;

      if (!change->is_deletion ())
	{
	  // Remove any notes that are no longer relevant.
	  if (!change->insn ()->m_is_temp)
	    update_notes (change->rtl ());

	  // Make sure that the placement of this instruction would still
	  // leave room for previous instructions.
	  change->move_range = move_later_than (change->move_range, min_insn);
	  if (!canonicalize_move_range (change->move_range, change->insn ()))
	    // verify_insn_changes is supposed to make sure that this holds.
	    gcc_unreachable ();
	  min_insn = later_insn (min_insn, change->move_range.first);

	  if (change->insn ()->m_is_temp)
	    {
	      change->m_insn = allocate<insn_info> (change->insn ()->bb (),
						    change->rtl (),
						    change->insn_uid ());

	      // Set the flag again so subsequent logic is aware.
	      // It will be cleared later on.
	      change->m_insn->m_is_temp = true;
	    }
	}
    }

  // Walk backwards through the changes, allocating specific positions
  // to each one.  Update the underlying RTL and its associated DF
  // information.
  insn_info *following_insn = nullptr;
  auto_vec<insn_info *, 16> placeholders;
  placeholders.safe_grow_cleared (changes.size ());
  for (unsigned int i = changes.size (); i-- > 0;)
    {
      insn_change &change = *changes[i];
      insn_info *placeholder = nullptr;
      possibly_queue_changes (change);
      if (change.is_deletion ())
	delete_insn (change);
      else
	{
	  // Make sure that this instruction comes before later ones.
	  if (following_insn)
	    {
	      change.move_range = move_earlier_than (change.move_range,
						     following_insn);
	      if (!canonicalize_move_range (change.move_range,
					    change.insn ()))
		// verify_insn_changes is supposed to make sure that this
		// holds.
		gcc_unreachable ();
	    }

	  // Decide which instruction INSN should go after.
	  insn_info *after = choose_insn_placement (change);

	  // If INSN is moving, insert a placeholder insn_info at the
	  // new location.  We can't move INSN itself yet because it
	  // might still be referenced by earlier move ranges.
	  insn_info *insn = change.insn ();
	  if (after == insn || after == insn->prev_nondebug_insn ())
	    {
	      update_insn_in_place (change);
	      following_insn = insn;
	    }
	  else
	    {
	      move_insn (change, after);
	      placeholder = add_placeholder_after (after);
	      following_insn = placeholder;
	    }
	}
      placeholders[i] = placeholder;
    }

  // We need to keep track of newly-added sets as these need adding to
  // the def chain later.
  hash_set<def_info *> new_sets;

  // Finalize the new list of accesses for each change.  Don't install them yet,
  // so that we still have access to the old lists below.
  //
  // Note that we do this forwards instead of in the backwards loop above so
  // that any new defs being inserted are processed before new uses of those
  // defs, so that the (initially) temporary uses referring to temporary defs
  // can be easily updated to become permanent uses referring to permanent defs.
  for (unsigned i = 0; i < changes.size (); i++)
    {
      insn_change &change = *changes[i];
      insn_info *placeholder = placeholders[i];
      if (!change.is_deletion ())
	finalize_new_accesses (change,
			       placeholder ? placeholder : change.insn (),
			       new_sets);
    }

  // Remove all definitions that are no longer needed.  After the above,
  // the only uses of such definitions should be dead phis and now-redundant
  // live-out uses.
  //
  // In particular, this means that consumers must handle debug
  // instructions before removing a set.
  for (insn_change *change : changes)
    for (def_info *def : change->old_defs ())
      if (def->m_has_been_superceded)
	{
	  auto *set = dyn_cast<set_info *> (def);
	  if (set && set->has_any_uses ())
	    process_uses_of_deleted_def (set);
	  remove_def (def);
	}

  // Move the insn_infos to their new locations.
  for (unsigned int i = 0; i < changes.size (); ++i)
    {
      insn_change &change = *changes[i];
      insn_info *insn = change.insn ();
      if (change.is_deletion ())
	{
	  if (rtx_insn *rtl = insn->rtl ())
	    ::remove_insn (rtl); // Remove the underlying RTL insn.
	  remove_insn (insn);
	}
      else if (insn_info *placeholder = placeholders[i])
	{
	  // Check if earlier movements turned a move into a no-op.
	  if (placeholder->prev_nondebug_insn () == insn
	      || placeholder->next_nondebug_insn () == insn)
	    {
	      remove_insn (placeholder);
	      placeholders[i] = nullptr;
	    }
	  else
	    {
	      insn_info *after = placeholder->prev_any_insn ();
	      if (!insn->is_temporary ())
		remove_insn (insn);
	      replace_nondebug_insn (placeholder, insn);
	      insn->set_bb (after->bb ());
	    }
	}
    }

  // Apply the changes to the underlying insn_infos.
  for (insn_change *change : changes)
    apply_changes_to_insn (*change, new_sets);

  // Now that the insns and accesses are up to date, add any REG_UNUSED notes.
  for (insn_change *change : changes)
    if (!change->is_deletion ())
      add_reg_unused_notes (change->insn ());
}

// See the comment above the declaration.
void
function_info::change_insn (insn_change &change)
{
  insn_change *changes[] = { &change };
  return change_insns (changes);
}

// Try to adjust CHANGE so that its pattern can include clobber rtx CLOBBER.
// Return true on success.
//
// ADD_REGNO_CLOBBER is a specialization of function_info::add_regno_clobber
// for a specific caller-provided predicate.
static bool
add_clobber (insn_change &change, add_regno_clobber_fn add_regno_clobber,
	     rtx clobber)
{
  rtx pat = PATTERN (change.rtl ());
  gcc_assert (GET_CODE (clobber) == CLOBBER);
  rtx dest = XEXP (clobber, 0);
  if (GET_CODE (dest) == SCRATCH)
    {
      if (reload_completed)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      // ??? Maybe we could try to do some RA here?
	      fprintf (dump_file, "instruction requires a scratch"
		       " after reload:\n");
	      print_rtl_single (dump_file, pat);
	    }
	  return false;
	}
      return true;
    }

  gcc_assert (REG_P (dest));
  for (unsigned int regno = REGNO (dest); regno != END_REGNO (dest); ++regno)
    if (!add_regno_clobber (change, regno))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "cannot clobber live register %d in:\n",
		     regno);
	    print_rtl_single (dump_file, pat);
	  }
	return false;
      }
  return true;
}

// See if PARALLEL pattern PAT clobbers any of the registers in ACCESSES.
// Return one such access if so, otherwise return null.
static access_info *
find_clobbered_access (access_array accesses, rtx pat)
{
  rtx subpat;
  for (int i = 0; i < XVECLEN (pat, 0); ++i)
    if (GET_CODE (subpat = XVECEXP (pat, 0, i)) == CLOBBER)
      {
	rtx x = XEXP (subpat, 0);
	if (REG_P (x))
	  for (auto *access : accesses)
	    if (access->regno () >= REGNO (x)
		&& access->regno () < END_REGNO (x))
	      return access;
      }
  return nullptr;
}

// Try to recognize the new form of the insn associated with CHANGE,
// adding any clobbers that are necessary to make the instruction match
// an .md pattern.  Return true on success.
//
// ADD_REGNO_CLOBBER is a specialization of function_info::add_regno_clobber
// for a specific caller-provided predicate.
static bool
recog_level2 (insn_change &change, add_regno_clobber_fn add_regno_clobber)
{
  insn_change_watermark insn_watermark;
  rtx_insn *rtl = change.rtl ();
  rtx pat = PATTERN (rtl);
  int num_clobbers = 0;
  int icode = -1;
  bool asm_p = asm_noperands (pat) >= 0;
  if (asm_p)
    {
      if (!check_asm_operands (pat))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "failed to match this asm instruction:\n");
	      print_rtl_single (dump_file, pat);
	    }
	  return false;
	}
    }
  else if (noop_move_p (rtl))
    {
      INSN_CODE (rtl) = NOOP_MOVE_INSN_CODE;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "instruction becomes a no-op:\n");
	  print_rtl_single (dump_file, pat);
	}
      insn_watermark.keep ();
      return true;
    }
  else
    {
      icode = ::recog (pat, rtl, &num_clobbers);
      if (icode < 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "failed to match this instruction:\n");
	      print_rtl_single (dump_file, pat);
	    }
	  return false;
	}
    }

  auto prev_new_defs = change.new_defs;
  auto prev_move_range = change.move_range;
  if (num_clobbers > 0)
    {
      // ??? It would be good to have a way of recycling the rtxes on failure,
      // but any attempt to cache old PARALLELs would at best be a half
      // measure, since add_clobbers would still generate fresh clobbers
      // each time.  It would be better to have a more general recycling
      // mechanism that all rtx passes can use.
      rtvec newvec;
      int oldlen;
      if (GET_CODE (pat) == PARALLEL)
	{
	  oldlen = XVECLEN (pat, 0);
	  newvec = rtvec_alloc (num_clobbers + oldlen);
	  for (int i = 0; i < oldlen; ++i)
	    RTVEC_ELT (newvec, i) = XVECEXP (pat, 0, i);
	}
      else
	{
	  oldlen = 1;
	  newvec = rtvec_alloc (num_clobbers + oldlen);
	  RTVEC_ELT (newvec, 0) = pat;
	}
      rtx newpat = gen_rtx_PARALLEL (VOIDmode, newvec);
      add_clobbers (newpat, icode);
      validate_change (rtl, &PATTERN (rtl), newpat, true);
      for (int i = 0; i < num_clobbers; ++i)
	if (!add_clobber (change, add_regno_clobber,
			  XVECEXP (newpat, 0, oldlen + i)))
	  {
	    change.new_defs = prev_new_defs;
	    change.move_range = prev_move_range;
	    return false;
	  }

      pat = newpat;
    }

  INSN_CODE (rtl) = icode;
  if (recog_data.insn == rtl)
    recog_data.insn = nullptr;

  // See if the pattern contains any hard-coded clobbers of registers
  // that are also inputs to the instruction.  The standard rtl semantics
  // treat such clobbers as earlyclobbers, since there is no way of proving
  // which clobbers conflict with the inputs and which don't.
  //
  // (Non-hard-coded clobbers are handled by constraint satisfaction instead.)
  rtx subpat;
  if (GET_CODE (pat) == PARALLEL)
    for (int i = 0; i < XVECLEN (pat, 0); ++i)
      if (GET_CODE (subpat = XVECEXP (pat, 0, i)) == CLOBBER
	  && REG_P (XEXP (subpat, 0)))
	{
	  // Stub out all operands, so that we can tell which registers
	  // are hard-coded.
	  extract_insn (rtl);
	  for (int j = 0; j < recog_data.n_operands; ++j)
	    *recog_data.operand_loc[j] = pc_rtx;

	  auto *use = find_clobbered_access (change.new_uses, pat);

	  // Restore the operands.
	  for (int j = 0; j < recog_data.n_operands; ++j)
	    *recog_data.operand_loc[j] = recog_data.operand[j];

	  if (use)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "register %d is both clobbered"
			   " and used as an input:\n", use->regno ());
		  print_rtl_single (dump_file, pat);
		}
	      return false;
	    }
	}

  // Per rtl.texi, registers that are modified using RTX_AUTOINC operations
  // cannot also appear outside an address.
  vec_rtx_properties properties;
  properties.add_pattern (pat);
  for (rtx_obj_reference def : properties.refs ())
    if (def.is_pre_post_modify ())
      for (rtx_obj_reference use : properties.refs ())
	if (def.regno == use.regno && !use.in_address ())
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "register %d is both auto-modified"
			 " and used outside an address:\n", def.regno);
		print_rtl_single (dump_file, pat);
	      }
	    return false;
	  }

  // check_asm_operands checks the constraints after RA, so we don't
  // need to do it again.
  if (reload_completed && !asm_p)
    {
      extract_insn (rtl);
      if (!constrain_operands (1, get_preferred_alternatives (rtl)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      if (asm_p)
		fprintf (dump_file, "asm does not match its constraints:\n");
	      else if (const char *name = get_insn_name (icode))
		fprintf (dump_file, "instruction does not match the"
			 " constraints for %s:\n", name);
	      else
		fprintf (dump_file, "instruction does not match its"
			 " constraints:\n");
	      print_rtl_single (dump_file, pat);
	    }
	  change.new_defs = prev_new_defs;
	  change.move_range = prev_move_range;
	  return false;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      const char *name;
      if (!asm_p && (name = get_insn_name (icode)))
	fprintf (dump_file, "successfully matched this instruction "
		 "to %s:\n", name);
      else
	fprintf (dump_file, "successfully matched this instruction:\n");
      print_rtl_single (dump_file, pat);
    }

  insn_watermark.keep ();
  return true;
}

// Try to recognize the new form of the insn associated with CHANGE,
// adding and removing clobbers as necessary to make the instruction
// match an .md pattern.  Return true on success, otherwise leave
// CHANGE as it was on entry.
//
// ADD_REGNO_CLOBBER is a specialization of function_info::add_regno_clobber
// for a specific caller-provided predicate.
bool
rtl_ssa::recog_internal (insn_change &change,
			 add_regno_clobber_fn add_regno_clobber)
{
  // Accept all changes to debug instructions.
  insn_info *insn = change.insn ();
  if (insn->is_debug_insn ())
    return true;

  rtx_insn *rtl = insn->rtl ();
  rtx pat = PATTERN (rtl);
  if (GET_CODE (pat) == PARALLEL && asm_noperands (pat) < 0)
    {
      // Try to remove trailing (clobber (scratch)) rtxes, since the new form
      // of the instruction might not need those scratches.  recog will add
      // back any that are needed.
      int len = XVECLEN (pat, 0);
      int new_len = len;
      while (new_len > 0
	     && GET_CODE (XVECEXP (pat, 0, new_len - 1)) == CLOBBER
	     && GET_CODE (XEXP (XVECEXP (pat, 0, new_len - 1), 0)) == SCRATCH)
	new_len -= 1;

      int old_num_changes = num_validated_changes ();
      validate_change_xveclen (rtl, &PATTERN (rtl), new_len, true);
      if (recog_level2 (change, add_regno_clobber))
	return true;
      cancel_changes (old_num_changes);

      // Try to remove all trailing clobbers.  For example, a pattern that
      // used to clobber the flags might no longer need to do so.
      int prev_len = new_len;
      while (new_len > 0
	     && GET_CODE (XVECEXP (pat, 0, new_len - 1)) == CLOBBER)
	new_len -= 1;
      if (new_len != prev_len)
	{
	  validate_change_xveclen (rtl, &PATTERN (rtl), new_len, true);
	  if (recog_level2 (change, add_regno_clobber))
	    return true;
	  cancel_changes (old_num_changes);
	}
      return false;
    }

  return recog_level2 (change, add_regno_clobber);
}

// See the comment above the declaration.
bool
function_info::perform_pending_updates ()
{
  bool changed_cfg = false;
  bool changed_jumps = false;
  for (insn_info *insn : m_queued_insn_updates)
    {
      rtx_insn *rtl = insn->rtl ();
      if (NOTE_P (rtl))
	// The insn was later optimized away, typically to a NOTE_INSN_DELETED.
	;
      else if (JUMP_P (rtl))
	{
	  if (INSN_CODE (rtl) == NOOP_MOVE_INSN_CODE)
	    {
	      ::delete_insn (rtl);
	      bitmap_set_bit (m_need_to_purge_dead_edges,
			      insn->bb ()->index ());
	    }
	  else if (returnjump_p (rtl) || any_uncondjump_p (rtl))
	    {
	      mark_jump_label (PATTERN (rtl), rtl, 0);
	      update_cfg_for_uncondjump (rtl);
	      changed_cfg = true;
	      changed_jumps = true;
	    }
	}
      else if (INSN_CODE (rtl) == NOOP_MOVE_INSN_CODE)
	::delete_insn (rtl);
      else
	{
	  rtx pattern = PATTERN (rtl);
	  if (GET_CODE (pattern) == TRAP_IF
	      && XEXP (pattern, 0) == const1_rtx)
	    {
	      remove_edge (split_block (BLOCK_FOR_INSN (rtl), rtl));
	      emit_barrier_after_bb (BLOCK_FOR_INSN (rtl));
	      changed_cfg = true;
	    }
	}
    }

  unsigned int index;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (m_need_to_purge_dead_edges, 0, index, bi)
    if (purge_dead_edges (BASIC_BLOCK_FOR_FN (m_fn, index)))
      changed_cfg = true;

  if (changed_jumps)
    // This uses its own timevar internally, so we don't need to push
    // one ourselves.
    rebuild_jump_labels (get_insns ());

  bitmap_clear (m_need_to_purge_dead_edges);
  bitmap_clear (m_queued_insn_update_uids);
  m_queued_insn_updates.truncate (0);

  if (changed_cfg)
    {
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
    }

  return changed_cfg;
}

insn_info *
function_info::create_insn (obstack_watermark &watermark,
			    rtx_code insn_code,
			    rtx pat)
{
  rtx_insn *rti = nullptr;

  // TODO: extend, move in to emit-rtl.cc.
  switch (insn_code)
    {
    case INSN:
      rti = make_insn_raw (pat);
      break;
    default:
      gcc_unreachable ();
    }

  auto insn = change_alloc<insn_info> (watermark, nullptr, rti, INSN_UID (rti));
  insn->m_is_temp = true;
  return insn;
}

// Print a description of CHANGE to PP.
void
rtl_ssa::pp_insn_change (pretty_printer *pp, const insn_change &change)
{
  change.print (pp);
}

// Print a description of CHANGE to FILE.
void
dump (FILE *file, const insn_change &change)
{
  dump_using (file, pp_insn_change, change);
}

// Debug interface to the dump routine above.
void debug (const insn_change &x) { dump (stderr, x); }
