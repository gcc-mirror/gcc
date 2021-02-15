// Implementation of instruction-related RTL SSA functions          -*- C++ -*-
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

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "rtl-ssa/internals.h"
#include "rtl-ssa/internals.inl"
#include "predict.h"
#include "print-rtl.h"
#include "rtl-iter.h"

using namespace rtl_ssa;

// The gap to leave between program points when building up the list
// of instructions for the first time.  Using 2 allows an instruction
// to be inserted between two others without resorting to splay tree
// ordering.  Using 0 is useful as a debugging aid to stress the
// splay tree code.
static const unsigned int POINT_INCREASE = 2;

// Calculate and record the cost of the instruction, based on the
// form it had before any in-progress changes were made.
void
insn_info::calculate_cost () const
{
  basic_block cfg_bb = BLOCK_FOR_INSN (m_rtl);
  temporarily_undo_changes (0);
  m_cost_or_uid = insn_cost (m_rtl, optimize_bb_for_speed_p (cfg_bb));
  redo_changes (0);
}

// Add NOTE to the instruction's notes.
void
insn_info::add_note (insn_note *note)
{
  insn_note **ptr = &m_first_note;
  // Always put the order node first, since it's the one that's likely
  // to be used most often.
  if (*ptr && (*ptr)->kind () == insn_note_kind::ORDER_NODE)
    ptr = &(*ptr)->m_next_note;
  note->m_next_note = *ptr;
  *ptr = note;
}

// Implement compare_with for the case in which this insn and OTHER
// have the same program point.
int
insn_info::slow_compare_with (const insn_info &other) const
{
  return order_splay_tree::compare_nodes (get_known_order_node (),
					  other.get_known_order_node ());
}

// Print insn uid UID to PP, where UID has the same form as insn_info::uid.
void
insn_info::print_uid (pretty_printer *pp, int uid)
{
  char tmp[3 * sizeof (uid) + 2];
  if (uid < 0)
    // An artificial instruction.
    snprintf (tmp, sizeof (tmp), "a%d", -uid);
  else
    // A real RTL instruction.
    snprintf (tmp, sizeof (tmp), "i%d", uid);
  pp_string (pp, tmp);
}

// See comment above declaration.
void
insn_info::print_identifier (pretty_printer *pp) const
{
  print_uid (pp, uid ());
}

// See comment above declaration.
void
insn_info::print_location (pretty_printer *pp) const
{
  if (bb_info *bb = this->bb ())
    {
      ebb_info *ebb = bb->ebb ();
      if (ebb && is_phi ())
	ebb->print_identifier (pp);
      else
	bb->print_identifier (pp);
      pp_string (pp, " at point ");
      pp_decimal_int (pp, m_point);
    }
  else
    pp_string (pp, "<unknown location>");
}

// See comment above declaration.
void
insn_info::print_identifier_and_location (pretty_printer *pp) const
{
  if (m_is_asm)
    pp_string (pp, "asm ");
  if (m_is_debug_insn)
    pp_string (pp, "debug ");
  pp_string (pp, "insn ");
  print_identifier (pp);
  pp_string (pp, " in ");
  print_location (pp);
}

// See comment above declaration.
void
insn_info::print_full (pretty_printer *pp) const
{
  print_identifier_and_location (pp);
  pp_colon (pp);
  if (is_real ())
    {
      pp_newline_and_indent (pp, 2);
      if (has_been_deleted ())
	pp_string (pp, "deleted");
      else
	{
	  // Print the insn pattern to a temporary printer.
	  pretty_printer sub_pp;
	  print_insn_with_notes (&sub_pp, rtl ());
	  const char *text = pp_formatted_text (&sub_pp);

	  // Calculate the length of the maximum line in the pattern.
	  unsigned int max_len = 0;
	  const char *start = text;
	  while (const char *end = strchr (start, '\n'))
	    {
	      max_len = MAX (max_len, (unsigned int) (end - start));
	      start = end + 1;
	    }

	  // Print a separator before or after the pattern.
	  auto print_top_bottom = [&]()
	    {
	      pp_character (pp, '+');
	      for (unsigned int i = 0; i < max_len + 2; ++i)
		pp_character (pp, '-');
	    };

	  print_top_bottom ();
	  start = text;
	  while (const char *end = strchr (start, '\n'))
	    {
	      pp_newline_and_indent (pp, 0);
	      pp_character (pp, '|');
	      // Each line of the pattern already starts with a space.
	      // so we don't need to add another one here.
	      pp_append_text (pp, start, end);
	      start = end + 1;
	    }
	  pp_newline_and_indent (pp, 0);
	  print_top_bottom ();

	  if (m_cost_or_uid != UNKNOWN_COST)
	    {
	      pp_newline_and_indent (pp, 0);
	      pp_string (pp, "cost: ");
	      pp_decimal_int (pp, m_cost_or_uid);
	    }
	  if (m_has_pre_post_modify)
	    {
	      pp_newline_and_indent (pp, 0);
	      pp_string (pp, "has pre/post-modify operations");
	    }
	  if (m_has_volatile_refs)
	    {
	      pp_newline_and_indent (pp, 0);
	      pp_string (pp, "has volatile refs");
	    }
	}
      pp_indentation (pp) -= 2;
    }

  auto print_accesses = [&](const char *heading, access_array accesses,
			    unsigned int flags)
    {
      if (!accesses.empty ())
	{
	  pp_newline_and_indent (pp, 2);
	  pp_string (pp, heading);
	  pp_newline_and_indent (pp, 2);
	  pp_accesses (pp, accesses, flags);
	  pp_indentation (pp) -= 4;
	}
    };

  print_accesses ("uses:", uses (), PP_ACCESS_USER);
  auto *call_clobbers_note = find_note<insn_call_clobbers_note> ();
  if (call_clobbers_note)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "has call clobbers for ABI ");
      pp_decimal_int (pp, call_clobbers_note->abi_id ());
      pp_indentation (pp) -= 2;
    }
  print_accesses ("defines:", defs (), PP_ACCESS_SETTER);
  if (num_uses () == 0 && !call_clobbers_note && num_defs () == 0)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "has no uses or defs");
      pp_indentation (pp) -= 2;
    }

  if (order_node *node = get_order_node ())
    {
      while (node->m_parent)
	node = node->m_parent;

      pp_newline_and_indent (pp, 2);
      pp_string (pp, "insn order: ");
      pp_newline_and_indent (pp, 2);
      auto print_order = [](pretty_printer *pp, order_node *node)
	{
	  print_uid (pp, node->uid ());
	};
      order_splay_tree::print (pp, node, print_order);
      pp_indentation (pp) -= 4;
    }
}

// Return an insn_info::order_node for INSN, creating one if necessary.
insn_info::order_node *
function_info::need_order_node (insn_info *insn)
{
  insn_info::order_node *order = insn->get_order_node ();
  if (!order)
    {
      order = allocate<insn_info::order_node> (insn->uid ());
      insn->add_note (order);
    }
  return order;
}

// Add instruction INSN immediately after AFTER in the reverse postorder list.
// INSN is not currently in the list.
void
function_info::add_insn_after (insn_info *insn, insn_info *after)
{
  gcc_checking_assert (!insn->has_insn_links ());

  insn->copy_next_from (after);
  after->set_next_any_insn (insn);

  // The prev link is easy if AFTER and INSN are the same type.
  // Handle the other cases below.
  if (after->is_debug_insn () == insn->is_debug_insn ())
    insn->set_prev_sametype_insn (after);

  if (insn_info *next = insn->next_any_insn ())
    {
      if (insn->is_debug_insn () == next->is_debug_insn ())
	{
	  // INSN might now be the start of the subsequence of debug insns,
	  // and so its prev pointer might point to the end of the subsequence
	  // instead of AFTER.
	  insn->copy_prev_from (next);
	  next->set_prev_sametype_insn (insn);
	}
      else if (insn->is_debug_insn ()) // && !next->is_debug_insn ()
	{
	  // INSN ends a subsequence of debug instructions.  Find the
	  // first debug instruction in the subsequence, which might
	  // be INSN itself.  (If it isn't, then AFTER is also a debug
	  // instruction and we updated INSN's prev link above.)
	  insn_info *first = next->prev_nondebug_insn ()->next_any_insn ();
	  first->set_last_debug_insn (insn);
	}
      else // !insn->is_debug_insn () && next->is_debug_insn ()
	// At present we don't (need to) support inserting a nondebug
	// instruction between two existing debug instructions.
	gcc_assert (!after->is_debug_insn ());

      // If AFTER and NEXT are separated by at least two points, we can
      // use a unique point number for INSN.  Otherwise INSN will have
      // the same point number as AFTER.
      insn->set_point ((next->point () + after->point ()) / 2);
    }
  else
    {
      if (!insn->is_debug_insn ())
	{
	  insn->set_prev_sametype_insn (m_last_nondebug_insn);
	  m_last_nondebug_insn = insn;
	}
      else
	// There is now at least one debug instruction after
	// m_last_nondebug_insn: either INSN itself, or the start of
	// a longer subsequence of debug insns that now ends with AFTER
	// followed by INSN.
	m_last_nondebug_insn->next_any_insn ()->set_last_debug_insn (insn);
      m_last_insn = insn;

      insn->set_point (after->point () + POINT_INCREASE);
    }

  // If INSN's program point is the same as AFTER's, we need to use the
  // splay tree to record their relative order.
  if (insn->point () == after->point ())
    {
      insn_info::order_node *after_node = need_order_node (after);
      insn_info::order_node *insn_node = need_order_node (insn);
      insn_info::order_splay_tree::insert_child (after_node, 1, insn_node);
    }
}

// Remove INSN from the function's list of instructions.
void
function_info::remove_insn (insn_info *insn)
{
  if (insn_info::order_node *order = insn->get_order_node ())
    insn_info::order_splay_tree::remove_node (order);

  if (auto *note = insn->find_note<insn_call_clobbers_note> ())
    {
      ebb_call_clobbers_info *ecc = insn->ebb ()->first_call_clobbers ();
      while (ecc->abi ()->id () != note->abi_id ())
	ecc = ecc->next ();
      int comparison = lookup_call_clobbers (*ecc, insn);
      gcc_assert (comparison == 0);
      ecc->remove_root ();
    }

  insn_info *prev = insn->prev_any_insn ();
  insn_info *next = insn->next_any_insn ();
  insn_info *prev_nondebug = insn->prev_nondebug_insn ();
  insn_info *next_nondebug = insn->next_nondebug_insn ();

  // We should never remove the entry or exit block's instructions.
  // At present we also don't remove entire blocks, so should never
  // remove debug instructions.
  gcc_checking_assert (prev_nondebug
		       && next_nondebug
		       && !insn->is_debug_insn ());

  if (prev->is_debug_insn () && next->is_debug_insn ())
    {
      // We need to stitch together two subsequences of debug insns.
      insn_info *last = next->last_debug_insn ();
      next->set_prev_sametype_insn (prev);
      prev_nondebug->next_any_insn ()->set_last_debug_insn (last);
    }
  prev->set_next_any_insn (next);
  next_nondebug->set_prev_sametype_insn (prev_nondebug);

  insn->clear_insn_links ();
}

// Create an artificial instruction for BB, associating it with RTL (which can
// be null).  Add the new instruction to the end of the function's list and
// return the new instruction.
insn_info *
function_info::append_artificial_insn (bb_info *bb, rtx_insn *rtl)
{
  insn_info *insn = allocate<insn_info> (bb, rtl, m_next_artificial_uid);
  m_next_artificial_uid -= 1;
  append_insn (insn);
  return insn;
}

// Finish building a new list of uses and definitions for instruction INSN.
void
function_info::finish_insn_accesses (insn_info *insn)
{
  unsigned int num_defs = m_temp_defs.length ();
  unsigned int num_uses = m_temp_uses.length ();
  obstack_make_room (&m_obstack, num_defs + num_uses);
  if (num_defs)
    {
      sort_accesses (m_temp_defs);
      obstack_grow (&m_obstack, m_temp_defs.address (),
		    num_defs * sizeof (access_info *));
      m_temp_defs.truncate (0);
    }
  if (num_uses)
    {
      sort_accesses (m_temp_uses);
      obstack_grow (&m_obstack, m_temp_uses.address (),
		    num_uses * sizeof (access_info *));
      m_temp_uses.truncate (0);
    }
  void *addr = obstack_finish (&m_obstack);
  insn->set_accesses (static_cast<access_info **> (addr), num_defs, num_uses);
}

// Called while building SSA form using BI.  Create and return a use of
// register RESOURCE in INSN.  Create a degenerate phi where necessary.
use_info *
function_info::create_reg_use (build_info &bi, insn_info *insn,
			       resource_info resource)
{
  set_info *value = bi.current_reg_value (resource.regno);
  if (value && value->ebb () != bi.current_ebb)
    {
      if (insn->is_debug_insn ())
	value = look_through_degenerate_phi (value);
      else if (bitmap_bit_p (bi.potential_phi_regs, resource.regno))
	{
	  // VALUE is defined by a previous EBB and RESOURCE has multiple
	  // definitions.  Create a degenerate phi in the current EBB
	  // so that all definitions and uses follow a linear RPO view;
	  // see rtl.texi for details.
	  access_info *inputs[] = { look_through_degenerate_phi (value) };
	  value = create_phi (bi.current_ebb, value->resource (), inputs, 1);
	  bi.record_reg_def (value);
	}
    }
  auto *use = allocate<use_info> (insn, resource, value);
  add_use (use);
  return use;
}

// Called while building SSA form using BI.  Record that INSN contains
// read reference REF.  If this requires new entries to be added to
// INSN->uses (), add those entries to the list we're building in
// m_temp_uses.
void
function_info::record_use (build_info &bi, insn_info *insn,
			   rtx_obj_reference ref)
{
  unsigned int regno = ref.regno;
  machine_mode mode = ref.is_reg () ? ref.mode : BLKmode;
  access_info *access = bi.last_access[ref.regno + 1];
  use_info *use = safe_dyn_cast<use_info *> (access);
  if (!use)
    {
      set_info *value = safe_dyn_cast<set_info *> (access);
      // In order to ensure that -g doesn't affect codegen, uses in debug
      // instructions do not affect liveness, either in DF or here.
      // This means that there might be no correct definition of the resource
      // available (e.g. if it would require a phi node that the nondebug
      // code doesn't need).  Perhaps we could have "debug phi nodes" as
      // well as "debug instructions", but that would require a method
      // of building phi nodes that didn't depend on DF liveness information,
      // and so might be significantly more expensive.
      //
      // Therefore, the only value we try to attach to a use by a debug
      // instruction is VALUE itself (as we would for nondebug instructions).
      // We then need to make a conservative check for whether VALUE is
      // actually correct.
      auto value_is_valid = [&]()
	{
	  // Memmory always has a valid definition.
	  if (ref.is_mem ())
	    return true;

	  // If VALUE would lead to an uninitialized use anyway, there's
	  // nothing to check.
	  if (!value)
	    return false;

	  // If the previous definition occurs in the same EBB then it
	  // is certainly correct.
	  if (value->ebb () == bi.current_ebb)
	    return true;

	  // Check if VALUE is the function's only definition of REGNO.
	  // (We already know that it dominates the use.)
	  if (!bitmap_bit_p (bi.potential_phi_regs, regno))
	    return true;

	  // If the register is live on entry to the EBB but not used
	  // within it, VALUE is the correct live-in value.
	  if (!bi.ebb_live_in_for_debug)
	    calculate_ebb_live_in_for_debug (bi);
	  if (bitmap_bit_p (bi.ebb_live_in_for_debug, regno))
	    return true;

	  // Punt for other cases.
	  return false;
	};
      if (insn->is_debug_insn () && !value_is_valid ())
	value = nullptr;

      use = create_reg_use (bi, insn, { mode, regno });
      m_temp_uses.safe_push (use);
      bi.last_access[ref.regno + 1] = use;
      use->record_reference (ref, true);
    }
  else
    {
      // Record the mode of the largest use.  The choice is arbitrary if
      // the instruction (unusually) references the same register in two
      // different but equal-sized modes.
      gcc_checking_assert (use->insn () == insn);
      if (HARD_REGISTER_NUM_P (regno)
	  && partial_subreg_p (use->mode (), mode))
	use->set_mode (mode);
      use->record_reference (ref, false);
    }
}

// Called while building SSA form for INSN using BI.  Record the effect
// of call clobbers in RTL.  We have already added the explicit sets and
// clobbers for RTL, which have priority over any call clobbers.
void
function_info::record_call_clobbers (build_info &bi, insn_info *insn,
				     rtx_call_insn *rtl)
{
  // See whether we should record this call in the EBB's list of
  // call clobbers.  Three things affect this choice:
  //
  // (1) The list is the only way we have of recording partial clobbers.
  //     All calls that only partially clobber registers must therefore
  //     be in the list.
  //
  // (2) Adding calls to the list is much more memory-efficient than
  //     creating a long list of clobber_infos.
  //
  // (3) Adding calls to the list limits the ability to move definitions
  //     of registers that are normally fully or partially clobbered
  //     by the associated predefined ABI.  So adding calls to the list
  //     can hamper optimization if (thanks to -fipa-ra) the number of
  //     clobbers is much smaller than the usual set.
  //
  // The trade-off that we currently take is to use the list if there
  // are some registers that the call only partially clobbers or if
  // the set of clobbers is the standard set.
  function_abi abi = insn_callee_abi (rtl);
  if (abi.base_abi ().full_reg_clobbers () == abi.full_reg_clobbers ()
      || abi.full_and_partial_reg_clobbers () != abi.full_reg_clobbers ())
    {
      // Find an entry for this predefined ABI, creating one if necessary.
      ebb_call_clobbers_info *ecc = bi.current_ebb->first_call_clobbers ();
      while (ecc && ecc->abi () != &abi.base_abi ())
	ecc = ecc->next ();
      if (!ecc)
	{
	  ecc = allocate<ebb_call_clobbers_info> (&abi.base_abi ());
	  ecc->m_next = bi.current_ebb->first_call_clobbers ();
	  bi.current_ebb->set_first_call_clobbers (ecc);
	}

      auto abi_id = abi.base_abi ().id ();
      auto *insn_clobbers = allocate<insn_call_clobbers_note> (abi_id, insn);
      insn->add_note (insn_clobbers);

      ecc->insert_max_node (insn_clobbers);
    }
  else
    for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
      if (TEST_HARD_REG_BIT (abi.full_reg_clobbers (), regno))
	{
	  def_info *def = m_defs[regno + 1];
	  if (!def || def->last_def ()->insn () != insn)
	    {
	      def = allocate<clobber_info> (insn, regno);
	      def->m_is_call_clobber = true;
	      append_def (def);
	      m_temp_defs.safe_push (def);
	      bi.record_reg_def (def);
	    }
	}
}

// Called while building SSA form using BI.  Record that INSN contains
// write reference REF.  Add associated def_infos to the list of accesses
// that we're building in m_temp_defs.  Record the register's new live
// value in BI.
void
function_info::record_def (build_info &bi, insn_info *insn,
			   rtx_obj_reference ref)
{
  // Punt if we see multiple definitions of the same resource.
  // This can happen for several reasons:
  //
  // - An instruction might store two values to memory at once, giving two
  //   distinct memory references.
  //
  // - An instruction might assign to multiple pieces of a wide pseudo
  //   register.  For example, on 32-bit targets, an instruction might
  //   assign to both the upper and lower halves of a 64-bit pseudo register.
  //
  // - It's possible for the same register to be clobbered by the
  //   CALL_INSN_FUNCTION_USAGE and to be set by the main instruction
  //   pattern as well.  In that case, the clobber conceptually happens
  //   before the set and can essentially be ignored.
  //
  // - Similarly, global registers are implicitly set by a call but can
  //   be explicitly set or clobbered as well.  In that situation, the sets
  //   are listed first and should win over a clobber.
  unsigned int regno = ref.regno;
  machine_mode mode = ref.is_reg () ? ref.mode : BLKmode;
  def_info *def = safe_dyn_cast<def_info *> (bi.last_access[ref.regno + 1]);
  if (def && def->insn () == insn)
    {
      if (!ref.is_clobber ())
	{
	  gcc_checking_assert (!is_a<clobber_info *> (def));
	  def->record_reference (ref, false);
	}
      return;
    }

  // Memory is always well-defined, so only use clobber_infos for registers.
  if (ref.is_reg () && ref.is_clobber ())
    def = allocate<clobber_info> (insn, regno);
  else
    def = allocate<set_info> (insn, resource_info { mode, regno });
  def->record_reference (ref, true);
  append_def (def);
  m_temp_defs.safe_push (def);
  bi.record_reg_def (def);
}

// Called while building SSA form using BI.  Add an insn_info for RTL
// to the block that we're current building.
void
function_info::add_insn_to_block (build_info &bi, rtx_insn *rtl)
{
  insn_info *insn = allocate<insn_info> (bi.current_bb, rtl, UNKNOWN_COST);
  append_insn (insn);

  vec_rtx_properties properties;
  properties.add_insn (rtl, true);
  insn->set_properties (properties);

  start_insn_accesses ();

  // Record the uses.
  for (rtx_obj_reference ref : properties.refs ())
    if (ref.is_read ())
      record_use (bi, insn, ref);

  // Restore the contents of bi.last_access, which we used as a cache
  // when assembling the uses.
  for (access_info *access : m_temp_uses)
    {
      unsigned int regno = access->regno ();
      gcc_checking_assert (bi.last_access[regno + 1] == access);
      bi.last_access[regno + 1] = as_a<use_info *> (access)->def ();
    }

  // Record the definitions.
  for (rtx_obj_reference ref : properties.refs ())
    if (ref.is_write ())
      record_def (bi, insn, ref);

  // Logically these happen before the explicit definitions, but if the
  // explicit definitions and call clobbers reference the same register,
  // the explicit definition should win.
  if (auto *call_rtl = dyn_cast<rtx_call_insn *> (rtl))
    record_call_clobbers (bi, insn, call_rtl);

  finish_insn_accesses (insn);
}

// Check whether INSN sets any registers that are never subsequently used.
// If so, add REG_UNUSED notes for them.  The caller has already removed
// any previous REG_UNUSED notes.
void
function_info::add_reg_unused_notes (insn_info *insn)
{
  rtx_insn *rtl = insn->rtl ();

  auto handle_potential_set = [&](rtx pattern)
    {
      if (GET_CODE (pattern) != SET)
	return;

      rtx dest = SET_DEST (pattern);
      if (!REG_P (dest))
	return;

      def_array defs = insn->defs ();
      unsigned int index = find_access_index (defs, REGNO (dest));
      for (unsigned int i = 0; i < REG_NREGS (dest); ++i)
	{
	  def_info *def = defs[index + i];
	  gcc_checking_assert (def->regno () == REGNO (dest) + i);
	  set_info *set = dyn_cast<set_info *> (def);
	  if (set && set->has_nondebug_uses ())
	    return;
	}
      add_reg_note (rtl, REG_UNUSED, dest);
    };

  rtx pattern = PATTERN (rtl);
  if (GET_CODE (pattern) == PARALLEL)
    for (int i = 0; i < XVECLEN (pattern, 0); ++i)
      handle_potential_set (XVECEXP (pattern, 0, i));
  else
    handle_potential_set (pattern);
}

// Search TREE for call clobbers at INSN.  Return:
//
// - less than zero if INSN occurs before the root of TREE
// - 0 if INSN is the root of TREE
// - greater than zero if INSN occurs after the root of TREE
int
rtl_ssa::lookup_call_clobbers (insn_call_clobbers_tree &tree, insn_info *insn)
{
  auto compare = [&](insn_call_clobbers_note *clobbers)
    {
      return insn->compare_with (clobbers->insn ());
    };
  return tree.lookup (compare);
}

// Print a description of INSN to PP.
void
rtl_ssa::pp_insn (pretty_printer *pp, const insn_info *insn)
{
  if (!insn)
    pp_string (pp, "<null>");
  else
    insn->print_full (pp);
}

// Print a description of INSN to FILE.
void
dump (FILE *file, const insn_info *insn)
{
  dump_using (file, pp_insn, insn);
}

// Debug interface to the dump routine above.
void debug (const insn_info *x) { dump (stderr, x); }
