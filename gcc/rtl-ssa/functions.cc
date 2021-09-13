// Implementation of function-related RTL SSA functions             -*- C++ -*-
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

using namespace rtl_ssa;

function_info::function_info (function *fn)
  : m_fn (fn)
{
  // Force the alignment to be obstack_alignment.  Everything else is normal.
  obstack_specify_allocation (&m_obstack, OBSTACK_CHUNK_SIZE,
			      obstack_alignment, obstack_chunk_alloc,
			      obstack_chunk_free);
  obstack_specify_allocation (&m_temp_obstack, OBSTACK_CHUNK_SIZE,
			      obstack_alignment, obstack_chunk_alloc,
			      obstack_chunk_free);

  // Record the start of the obstacks.
  m_obstack_start = XOBNEWVAR (&m_obstack, char, 0);
  m_temp_obstack_start = XOBNEWVAR (&m_temp_obstack, char, 0);

  init_function_data ();
  process_all_blocks ();
  simplify_phis ();
}

function_info::~function_info ()
{
  // Anything using the temporary obstack should free it afterwards,
  // preferably via temp_watermark ().
  gcc_assert (XOBNEWVAR (&m_temp_obstack, char, 0) == m_temp_obstack_start);

  obstack_free (&m_temp_obstack, nullptr);
  obstack_free (&m_obstack, nullptr);
}

// See the comment above the declaration.
void
function_info::print (pretty_printer *pp) const
{
  pp_string (pp, "Function: ");
  pp_string (pp, function_name (m_fn));
  for (ebb_info *ebb : ebbs ())
    {
      pp_newline (pp);
      pp_newline_and_indent (pp, 0);
      pp_ebb (pp, ebb);
    }
}

// Initialize all member variables in preparation for (re)building
// SSA form from scratch.
void
function_info::init_function_data ()
{
  m_next_artificial_uid = -1;
  m_next_phi_uid = 0;
  m_num_regs = max_reg_num ();
  m_defs.safe_grow_cleared (m_num_regs + 1);
  m_bbs.safe_grow_cleared (last_basic_block_for_fn (m_fn));
  m_first_bb = nullptr;
  m_last_bb = nullptr;
  m_first_insn = nullptr;
  m_last_insn = nullptr;
  m_last_nondebug_insn = nullptr;
  m_free_phis = nullptr;
}

// The initial phase of the phi simplification process.  The cumulative
// effect of the initial phase is to set up ASSUMED_VALUES such that,
// for a phi P with uid ID:
//
// - if we think all inputs to P have the same value, ASSUMED_VALUES[ID]
//   is that value
//
// - otherwise, ASSUMED_VALUES[ID] is P.
//
// This has already been done for phis with a lower uid than PHI,
// initially making optimistic assumptions about backedge inputs.
// Now do the same for PHI.  If this might invalidate any assumptions
// made for earlier phis, add the uids of those phis to WORKLIST.
void
function_info::simplify_phi_setup (phi_info *phi, set_info **assumed_values,
				   bitmap worklist)
{
  // If all non-backedge inputs have the same value, set NEW_VALUE
  // to that value.  Otherwise set NEW_VALUE to PHI, to indicate
  // that PHI cannot be simplified.
  unsigned int phi_uid = phi->uid ();
  bool is_first_input = true;
  set_info *new_value = nullptr;
  machine_mode phi_mode = phi->mode ();
  for (use_info *input : phi->inputs ())
    {
      set_info *def = input->def ();

      if (auto *input_phi = safe_dyn_cast<phi_info *> (def))
	{
	  // Ignore backedges for now.
	  unsigned int input_phi_uid = input_phi->uid ();
	  if (phi_uid <= input_phi_uid)
	    continue;

	  def = assumed_values[input_phi_uid];
	}

      // Compare this definition with previous ones.
      if (is_first_input)
	{
	  new_value = def;
	  is_first_input = false;
	}
      else if (new_value != def)
	new_value = phi;

      // If the input has a known mode (i.e. not BLKmode), make sure
      // that the phi's mode is at least as large.
      if (def)
	phi_mode = combine_modes (phi_mode, def->mode ());
    }
  if (phi->mode () != phi_mode)
    phi->set_mode (phi_mode);

  // Since we use a reverse postorder traversal, no phi can consist
  // entirely of backedges.
  gcc_checking_assert (!is_first_input);
  assumed_values[phi_uid] = new_value;

  // See whether any assumptions for earlier phis are now invalid.
  simplify_phi_propagate (phi, assumed_values, nullptr, worklist);
}

// The propagation phase of the phi simplification process, with
// ASSUMED_VALUES as described above simplify_phi_setup.  Iteratively
// update the phis that use PHI based on PHI's entry in ASSUMED_VALUES.
// If CURR_WORKLIST is null, consider only phi uses with a lower uid
// than PHI, otherwise consider all phi uses.
//
// If a phi with a higher uid than PHI needs updating, add its uid to
// CURR_WORKLIST; if a phi with a lower uid than PHI needs updating,
// add its uid to NEXT_WORKLIST.
void
function_info::simplify_phi_propagate (phi_info *phi,
				       set_info **assumed_values,
				       bitmap curr_worklist,
				       bitmap next_worklist)
{
  // Go through each phi user of PHI to see whether it needs updating.
  unsigned int phi_uid = phi->uid ();
  machine_mode phi_mode = phi->mode ();
  set_info *phi_value = assumed_values[phi_uid];
  for (use_info *use : phi->phi_uses ())
    {
      phi_info *user_phi = use->phi ();

      // Propagate the phi's new mode to all phi users.  Insn uses should
      // not be updated, since their modes reflect a property of the insns
      // rather than the phi.
      if (use->mode () != phi_mode)
	use->set_mode (phi_mode);

      if (user_phi == phi)
	continue;

      // If this is a phi we should be looking at, see whether it needs
      // an update.
      unsigned int user_phi_uid = user_phi->uid ();
      if (user_phi_uid < phi_uid || curr_worklist)
	{
	  bool needs_update = false;

	  // Make sure that USER_PHI's mode is at least as big as PHI_MODE.
	  machine_mode user_phi_mode = user_phi->mode ();
	  machine_mode new_mode = combine_modes (user_phi_mode, phi_mode);
	  if (user_phi_mode != new_mode)
	    {
	      user_phi->set_mode (new_mode);
	      needs_update = true;
	    }

	  // If USER_PHI optimistically assumed an incorrect value,
	  // adjust it now.
	  if (assumed_values[user_phi_uid] != user_phi
	      && assumed_values[user_phi_uid] != phi_value)
	    {
	      assumed_values[user_phi_uid] = user_phi;
	      needs_update = true;
	    }

	  if (needs_update)
	    {
	      if (user_phi_uid < phi_uid)
		bitmap_set_bit (next_worklist, user_phi_uid);
	      else
		bitmap_set_bit (curr_worklist, user_phi_uid);
	    }
	}
    }
}

// Update the modes of all phis so that they are at least as big as
// all inputs.  Remove any non-degenerate phis whose inputs are all equal.
void
function_info::simplify_phis ()
{
  auto temps = temp_watermark ();

  // See the comment above simplify_phi_setup for details about this array.
  auto *assumed_values = XOBNEWVEC (&m_temp_obstack, set_info *,
				    m_next_phi_uid);

  // An array of all phis, indexed by uid.
  auto *phis = XOBNEWVEC (&m_temp_obstack, phi_info *, m_next_phi_uid);

  // Which phi uids are actually in use.
  auto_sbitmap valid_phi_uids (m_next_phi_uid);
  bitmap_clear (valid_phi_uids);

  // Bitmaps used for the main double-queue propagation phase.
  auto_bitmap worklist1;
  auto_bitmap worklist2;
  bitmap curr_worklist = worklist1;
  bitmap next_worklist = worklist2;

  // Perform the set-up phase; see simplify_phi_setup for details.
  for (ebb_info *ebb : ebbs ())
    for (phi_info *phi : ebb->phis ())
      {
	bitmap_set_bit (valid_phi_uids, phi->uid ());
	phis[phi->uid ()] = phi;
	simplify_phi_setup (phi, assumed_values, curr_worklist);
      }

  // Iteratively process any phis that need updating; see
  // simplify_phi_propagate for details.  Using a double queue
  // should reduce the number of times that any given phi node
  // needs to be revisited.
  while (!bitmap_empty_p (curr_worklist))
    {
      do
	{
	  unsigned int uid = bitmap_first_set_bit (curr_worklist);
	  bitmap_clear_bit (curr_worklist, uid);
	  simplify_phi_propagate (phis[uid], assumed_values,
				  curr_worklist, next_worklist);
	}
      while (!bitmap_empty_p (curr_worklist));
      std::swap (next_worklist, curr_worklist);
    }

  // Make sure that assumed_values is a transitive closure.  This ensures
  // that each use_info is only updated once.
  if (flag_checking)
    for (unsigned int i = 0; i < m_next_phi_uid; ++i)
      if (bitmap_bit_p (valid_phi_uids, i))
	if (auto *new_phi = safe_dyn_cast<phi_info *> (assumed_values[i]))
	  gcc_assert (assumed_values[new_phi->uid ()] == new_phi);

  // Update any phis that turned out to be equivalent to a single input.
  for (unsigned int i = 0; i < m_next_phi_uid; ++i)
    if (bitmap_bit_p (valid_phi_uids, i) && phis[i] != assumed_values[i])
      replace_phi (phis[i], assumed_values[i]);
}

// Print a description of FUNCTION to PP.
void
rtl_ssa::pp_function (pretty_printer *pp, const function_info *function)
{
  function->print (pp);
}

// Print a description of FUNCTION to FILE.
void
dump (FILE *file, const function_info *function)
{
  dump_using (file, pp_function, function);
}

// Debug interface to the dump routine above.
void debug (const function_info *x) { dump (stderr, x); }
