// Implementation of basic-block-related functions for RTL SSA      -*- C++ -*-
// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
#include "cfganal.h"
#include "cfgrtl.h"
#include "predict.h"
#include "domwalk.h"

using namespace rtl_ssa;

// Prepare to build information for a function in which all register numbers
// are less than NUM_REGS and all basic block indices are less than
// NUM_BB_INDICES
function_info::build_info::build_info (unsigned int num_regs,
				       unsigned int num_bb_indices)
  : current_bb (nullptr),
    current_ebb (nullptr),
    last_access (num_regs + 1),
    ebb_live_in_for_debug (nullptr),
    potential_phi_regs (num_regs),
    bb_phis (num_bb_indices),
    bb_mem_live_out (num_bb_indices),
    bb_to_rpo (num_bb_indices),
    exit_block_dominator (nullptr)
{
  last_access.safe_grow_cleared (num_regs + 1);

  bitmap_clear (potential_phi_regs);

  // These arrays shouldn't need to be initialized, since we'll always
  // write to an entry before reading from it.  But poison the contents
  // when checking, just to make sure we don't accidentally use an
  // uninitialized value.
  bb_phis.quick_grow_cleared (num_bb_indices);
  bb_mem_live_out.quick_grow (num_bb_indices);
  bb_to_rpo.quick_grow (num_bb_indices);
  if (flag_checking)
    {
      // Can't do this for bb_phis because it has a constructor.
      memset (bb_mem_live_out.address (), 0xaf,
	      num_bb_indices * sizeof (bb_mem_live_out[0]));
      memset (bb_to_rpo.address (), 0xaf,
	      num_bb_indices * sizeof (bb_to_rpo[0]));
    }

  // Start off with an empty set of phi nodes for each block.
  for (bb_phi_info &info : bb_phis)
    bitmap_initialize (&info.regs, &bitmap_default_obstack);
}

function_info::build_info::~build_info ()
{
  for (bb_phi_info &info : bb_phis)
    bitmap_release (&info.regs);
}

// A dom_walker for populating the basic blocks.
class function_info::bb_walker : public dom_walker
{
public:
  bb_walker (function_info *, build_info &);
  edge before_dom_children (basic_block) final override;
  void after_dom_children (basic_block) final override;

private:
  // Information about the function we're building.
  function_info *m_function;
  build_info &m_bi;

  // We should treat the exit block as being the last child of this one.
  // See the comment in the constructor for more information.
  basic_block m_exit_block_dominator;
};

// Prepare to walk the blocks in FUNCTION using BI.
function_info::bb_walker::bb_walker (function_info *function, build_info &bi)
  : dom_walker (CDI_DOMINATORS, ALL_BLOCKS, bi.bb_to_rpo.address ()),
    m_function (function),
    m_bi (bi),
    m_exit_block_dominator (bi.exit_block_dominator)
{
  // If the exit block is unreachable, process it last.
  if (!m_exit_block_dominator)
    m_exit_block_dominator = ENTRY_BLOCK_PTR_FOR_FN (m_function->m_fn);
}

edge
function_info::bb_walker::before_dom_children (basic_block bb)
{
  m_function->start_block (m_bi, m_function->bb (bb));
  return nullptr;
}

void
function_info::bb_walker::after_dom_children (basic_block bb)
{
  // See the comment in the constructor for details.
  if (bb == m_exit_block_dominator)
    {
      before_dom_children (EXIT_BLOCK_PTR_FOR_FN (m_function->m_fn));
      after_dom_children (EXIT_BLOCK_PTR_FOR_FN (m_function->m_fn));
    }
  m_function->end_block (m_bi, m_function->bb (bb));
}

// See the comment above the declaration.
void
bb_info::print_identifier (pretty_printer *pp) const
{
  char tmp[3 * sizeof (index ()) + 3];
  snprintf (tmp, sizeof (tmp), "bb%d", index ());
  pp_string (pp, tmp);
  if (ebb_info *ebb = this->ebb ())
    {
      pp_space (pp);
      pp_left_bracket (pp);
      ebb->print_identifier (pp);
      pp_right_bracket (pp);
    }
}

// See the comment above the declaration.
void
bb_info::print_full (pretty_printer *pp) const
{
  pp_string (pp, "basic block ");
  print_identifier (pp);
  pp_colon (pp);

  auto print_insn = [pp](const char *header, const insn_info *insn)
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, header);
      pp_newline_and_indent (pp, 2);
      if (insn)
	pp_insn (pp, insn);
      else
	pp_string (pp, "<uninitialized>");
      pp_indentation (pp) -= 4;
    };

  print_insn ("head:", head_insn ());

  pp_newline (pp);
  pp_newline_and_indent (pp, 2);
  pp_string (pp, "contents:");
  if (!head_insn ())
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "<uninitialized>");
      pp_indentation (pp) -= 2;
    }
  else if (auto insns = real_insns ())
    {
      bool is_first = true;
      for (const insn_info *insn : insns)
	{
	  if (is_first)
	    is_first = false;
	  else
	    pp_newline (pp);
	  pp_newline_and_indent (pp, 2);
	  pp_insn (pp, insn);
	  pp_indentation (pp) -= 2;
	}
    }
  else
    {
      pp_newline_and_indent (pp, 2);
      pp_string (pp, "none");
      pp_indentation (pp) -= 2;
    }
  pp_indentation (pp) -= 2;

  pp_newline (pp);
  print_insn ("end:", end_insn ());
}

// See the comment above the declaration.
void
ebb_call_clobbers_info::print_summary (pretty_printer *pp) const
{
  pp_string (pp, "call clobbers for ABI ");
  if (m_abi)
    pp_decimal_int (pp, m_abi->id ());
  else
    pp_string (pp, "<null>");
}

// See the comment above the declaration.
void
ebb_call_clobbers_info::print_full (pretty_printer *pp) const
{
  print_summary (pp);
  pp_colon (pp);
  pp_newline_and_indent (pp, 2);
  auto print_node = [](pretty_printer *pp,
		       const insn_call_clobbers_note *note)
    {
      if (insn_info *insn = note->insn ())
	insn->print_identifier_and_location (pp);
      else
	pp_string (pp, "<null>");
    };
  print (pp, root (), print_node);
  pp_indentation (pp) -= 2;
}

// See the comment above the declaration.
void
ebb_info::print_identifier (pretty_printer *pp) const
{
  // first_bb is populated by the constructor and so should always
  // be nonnull.
  auto index = first_bb ()->index ();
  char tmp[3 * sizeof (index) + 4];
  snprintf (tmp, sizeof (tmp), "ebb%d", index);
  pp_string (pp, tmp);
}

// See the comment above the declaration.
void
ebb_info::print_full (pretty_printer *pp) const
{
  pp_string (pp, "extended basic block ");
  print_identifier (pp);
  pp_colon (pp);

  pp_newline_and_indent (pp, 2);
  if (insn_info *phi_insn = this->phi_insn ())
    {
      phi_insn->print_identifier_and_location (pp);
      pp_colon (pp);
      if (auto phis = this->phis ())
	{
	  bool is_first = true;
	  for (const phi_info *phi : phis)
	    {
	      if (is_first)
		is_first = false;
	      else
		pp_newline (pp);
	      pp_newline_and_indent (pp, 2);
	      pp_access (pp, phi, PP_ACCESS_SETTER);
	      pp_indentation (pp) -= 2;
	    }
	}
      else
	{
	  pp_newline_and_indent (pp, 2);
	  pp_string (pp, "no phi nodes");
	  pp_indentation (pp) -= 2;
	}
    }
  else
    pp_string (pp, "no phi insn");
  pp_indentation (pp) -= 2;

  for (const bb_info *bb : bbs ())
    {
      pp_newline (pp);
      pp_newline_and_indent (pp, 2);
      pp_bb (pp, bb);
      pp_indentation (pp) -= 2;
    }

  for (ebb_call_clobbers_info *ecc : call_clobbers ())
    {
      pp_newline (pp);
      pp_newline_and_indent (pp, 2);
      pp_ebb_call_clobbers (pp, ecc);
      pp_indentation (pp) -= 2;
    }
}

// Add a dummy use to mark that DEF is live out of BB's EBB at the end of BB.
void
function_info::add_live_out_use (bb_info *bb, set_info *def)
{
  // There is nothing to do if DEF is an artificial definition at the end
  // of BB.  In that case the definitino is rooted at the end of the block
  // and we wouldn't gain anything by inserting a use immediately after it.
  // If we did want to insert a use, we'd need to associate it with a new
  // instruction that comes after bb->end_insn ().
  if (def->insn () == bb->end_insn ())
    return;

  // If the end of the block already has an artificial use, that use
  // acts to make DEF live at the appropriate point.
  use_info *use = def->last_nondebug_insn_use ();
  if (use && use->insn () == bb->end_insn ())
    return;

  // Currently there is no need to maintain a backward link from the end
  // instruction to the list of live-out uses.  Such a list would be
  // expensive to update if it was represented using the usual insn_info
  // access arrays.
  use = allocate<use_info> (bb->end_insn (), def->resource (), def);
  use->set_is_live_out_use (true);
  add_use (use);
}

// Return true if all nondebug uses of DEF are live-out uses.
static bool
all_uses_are_live_out_uses (set_info *def)
{
  for (use_info *use : def->all_uses ())
    if (!use->is_in_debug_insn () && !use->is_live_out_use ())
      return false;
  return true;
}

// SET, if nonnull, is a definition of something that is live out from BB.
// Return the live-out value itself.
set_info *
function_info::live_out_value (bb_info *bb, set_info *set)
{
  // Degenerate phis only exist to provide a definition for uses in the
  // same EBB.  The live-out value is the same as the live-in value.
  if (auto *phi = safe_dyn_cast<phi_info *> (set))
    if (phi->is_degenerate ())
      {
	set = phi->input_value (0);

	// Remove the phi if it turned out to be useless.  This is
	// mainly useful for memory, because we don't know ahead of time
	// whether a block will use memory or not.
	if (bb == bb->ebb ()->last_bb () && all_uses_are_live_out_uses (phi))
	  replace_phi (phi, set);
      }

  return set;
}

// Add PHI to EBB and enter it into the function's hash table.
void
function_info::append_phi (ebb_info *ebb, phi_info *phi)
{
  phi_info *first_phi = ebb->first_phi ();
  if (first_phi)
    first_phi->set_prev_phi (phi);
  phi->set_next_phi (first_phi);
  ebb->set_first_phi (phi);
  add_def (phi);
}

// Remove PHI from its current position in the SSA graph.
void
function_info::remove_phi (phi_info *phi)
{
  phi_info *next = phi->next_phi ();
  phi_info *prev = phi->prev_phi ();

  if (next)
    next->set_prev_phi (prev);

  if (prev)
    prev->set_next_phi (next);
  else
    phi->ebb ()->set_first_phi (next);

  remove_def (phi);
  phi->clear_phi_links ();
}

// Remove PHI from the SSA graph and free its memory.
void
function_info::delete_phi (phi_info *phi)
{
  gcc_assert (!phi->has_any_uses ());

  // Remove the inputs to the phi.
  for (use_info *input : phi->inputs ())
    remove_use (input);

  remove_phi (phi);

  phi->set_next_phi (m_free_phis);
  m_free_phis = phi;
}

// If possible, remove PHI and replace all uses with NEW_VALUE.
void
function_info::replace_phi (phi_info *phi, set_info *new_value)
{
  auto update_use = [&](use_info *use)
    {
      remove_use (use);
      use->set_def (new_value);
      add_use (use);
    };

  if (new_value)
    for (use_info *use : phi->nondebug_insn_uses ())
      if (!use->is_live_out_use ())
	{
	  // We need to keep the phi around for its local uses.
	  // Turn it into a degenerate phi, if it isn't already.
	  use_info *use = phi->input_use (0);
	  if (use->def () != new_value)
	    update_use (use);

	  if (phi->is_degenerate ())
	    return;

	  phi->make_degenerate (use);

	  // Redirect all phi users to NEW_VALUE.
	  while (use_info *phi_use = phi->last_phi_use ())
	    update_use (phi_use);

	  return;
	}

  // Replace the uses.  We can discard uses that only existed for the
  // sake of marking live-out values, since the resource is now transparent
  // in the phi's EBB.
  while (use_info *use = phi->last_use ())
    if (use->is_live_out_use ())
      remove_use (use);
    else
      update_use (use);

  delete_phi (phi);
}

// Create and return a phi node for EBB.  RESOURCE is the resource that
// the phi node sets (and thus that all the inputs set too).  NUM_INPUTS
// is the number of inputs, which is 1 for a degenerate phi.  INPUTS[I]
// is a set_info that gives the value of input I, or null if the value
// is either unknown or uninitialized.  If NUM_INPUTS > 1, this array
// is allocated on the main obstack and can be reused for the use array.
//
// Add the created phi node to its basic block and enter it into the
// function's hash table.
phi_info *
function_info::create_phi (ebb_info *ebb, resource_info resource,
			   access_info **inputs, unsigned int num_inputs)
{
  phi_info *phi = m_free_phis;
  if (phi)
    {
      m_free_phis = phi->next_phi ();
      *phi = phi_info (ebb->phi_insn (), resource, phi->uid ());
    }
  else
    {
      phi = allocate<phi_info> (ebb->phi_insn (), resource, m_next_phi_uid);
      m_next_phi_uid += 1;
    }

  // Convert the array of set_infos into an array of use_infos.  Also work
  // out what mode the phi should have.
  machine_mode new_mode = resource.mode;
  for (unsigned int i = 0; i < num_inputs; ++i)
    {
      auto *input = safe_as_a<set_info *> (inputs[i]);
      auto *use = allocate<use_info> (phi, resource, input);
      add_use (use);
      inputs[i] = use;
      if (input)
	new_mode = combine_modes (new_mode, input->mode ());
    }

  phi->set_inputs (use_array (inputs, num_inputs));
  phi->set_mode (new_mode);

  append_phi (ebb, phi);

  return phi;
}

// Create and return a degenerate phi for EBB whose input comes from DEF.
// This is used in cases where DEF is known to be available on entry to
// EBB but was not previously used within it.  If DEF is for a register,
// there are two cases:
//
// (1) DEF was already live on entry to EBB but was previously transparent
//     within it.
//
// (2) DEF was not previously live on entry to EBB and is being made live
//     by this update.
//
// At the moment, this function only handles the case in which EBB has a
// single predecessor block and DEF is defined in that block's EBB.
phi_info *
function_info::create_degenerate_phi (ebb_info *ebb, set_info *def)
{
  // Allow the function to be called twice in succession for the same def.
  def_lookup dl = find_def (def->resource (), ebb->phi_insn ());
  if (set_info *set = dl.matching_set ())
    return as_a<phi_info *> (set);

  access_info *input = def;
  phi_info *phi = create_phi (ebb, def->resource (), &input, 1);
  if (def->is_reg ())
    {
      unsigned int regno = def->regno ();

      // Find the single predecessor mentioned above.
      basic_block pred_cfg_bb = single_pred (ebb->first_bb ()->cfg_bb ());
      bb_info *pred_bb = this->bb (pred_cfg_bb);

      if (!bitmap_set_bit (DF_LR_IN (ebb->first_bb ()->cfg_bb ()), regno))
	{
	  // The register was not previously live on entry to EBB and
	  // might not have been live on exit from PRED_BB either.
	  if (bitmap_set_bit (DF_LR_OUT (pred_cfg_bb), regno))
	    add_live_out_use (pred_bb, def);
	}
      else
	{
	  // The register was previously live in to EBB.  Add live-out uses
	  // at the appropriate points.
	  insn_info *next_insn = nullptr;
	  if (def_info *next_def = phi->next_def ())
	    next_insn = next_def->insn ();
	  for (bb_info *bb : ebb->bbs ())
	    {
	      if ((next_insn && *next_insn <= *bb->end_insn ())
		  || !bitmap_bit_p (DF_LR_OUT (bb->cfg_bb ()), regno))
		break;
	      add_live_out_use (bb, def);
	    }
	}
    }
  return phi;
}

// Create a bb_info for CFG_BB, given that no such structure currently exists.
bb_info *
function_info::create_bb_info (basic_block cfg_bb)
{
  bb_info *bb = allocate<bb_info> (cfg_bb);
  gcc_checking_assert (!m_bbs[cfg_bb->index]);
  m_bbs[cfg_bb->index] = bb;
  return bb;
}

// Add BB to the end of the list of blocks.
void
function_info::append_bb (bb_info *bb)
{
  if (m_last_bb)
    m_last_bb->set_next_bb (bb);
  else
    m_first_bb = bb;
  bb->set_prev_bb (m_last_bb);
  m_last_bb = bb;
}

// Calculate BI.potential_phi_regs and BI.potential_phi_regs_for_debug.
void
function_info::calculate_potential_phi_regs (build_info &bi)
{
  auto *lr_info = DF_LR_BB_INFO (ENTRY_BLOCK_PTR_FOR_FN (m_fn));
  bool is_debug = MAY_HAVE_DEBUG_INSNS;
  for (unsigned int regno = 0; regno < m_num_regs; ++regno)
    if (regno >= DF_REG_SIZE (DF)
	// Exclude registers that have a single definition that dominates
	// all uses.  If the definition does not dominate all uses,
	// the register will be exposed upwards to the entry block but
	// will not be defined by the entry block.
	|| DF_REG_DEF_COUNT (regno) > 1
	|| (!bitmap_bit_p (&lr_info->def, regno)
	    && bitmap_bit_p (&lr_info->out, regno)))
      {
	bitmap_set_bit (bi.potential_phi_regs, regno);
	if (is_debug)
	  bitmap_set_bit (bi.potential_phi_regs_for_debug, regno);
      }
}

// Called while building SSA form using BI.  Decide where phi nodes
// should be placed for each register and initialize BI.bb_phis accordingly.
void
function_info::place_phis (build_info &bi)
{
  unsigned int num_bb_indices = last_basic_block_for_fn (m_fn);

  // Calculate dominance frontiers.
  auto_vec<bitmap_head> frontiers;
  frontiers.safe_grow_cleared (num_bb_indices);
  for (unsigned int i = 0; i < num_bb_indices; ++i)
    bitmap_initialize (&frontiers[i], &bitmap_default_obstack);
  compute_dominance_frontiers (frontiers.address ());

  // The normal dominance information doesn't calculate dominators for
  // the exit block, so we don't get dominance frontiers for them either.
  // Calculate them by hand.
  for (edge e : EXIT_BLOCK_PTR_FOR_FN (m_fn)->preds)
    {
      basic_block bb = e->src;
      while (bb != bi.exit_block_dominator)
	{
	  bitmap_set_bit (&frontiers[bb->index], EXIT_BLOCK);
	  bb = get_immediate_dominator (CDI_DOMINATORS, bb);
	}
    }

  // In extreme cases, the number of live-in registers can be much
  // greater than the number of phi nodes needed in a block (see PR98863).
  // Try to reduce the number of operations involving live-in sets by using
  // PENDING as a staging area: registers in PENDING need phi nodes if
  // they are live on entry to the corresponding block, but do not need
  // phi nodes otherwise.
  auto_vec<bitmap_head> unfiltered;
  unfiltered.safe_grow_cleared (num_bb_indices);
  for (unsigned int i = 0; i < num_bb_indices; ++i)
    bitmap_initialize (&unfiltered[i], &bitmap_default_obstack);

  // If block B1 defines R and if B2 is in the dominance frontier of B1,
  // queue a possible phi node for R in B2.
  auto_bitmap worklist;
  for (unsigned int b1 = 0; b1 < num_bb_indices; ++b1)
    {
      // Only access DF information for blocks that are known to exist.
      if (bitmap_empty_p (&frontiers[b1]))
	continue;

      // Defs in B1 that are possibly in LR_IN in the dominance frontier
      // blocks.
      auto_bitmap b1_def;
      bitmap_and (b1_def, &DF_LR_BB_INFO (BASIC_BLOCK_FOR_FN (m_fn, b1))->def,
		  DF_LR_OUT (BASIC_BLOCK_FOR_FN (m_fn, b1)));

      bitmap_iterator bmi;
      unsigned int b2;
      EXECUTE_IF_SET_IN_BITMAP (&frontiers[b1], 0, b2, bmi)
	if (bitmap_ior_into (&unfiltered[b2], b1_def)
	    && !bitmap_empty_p (&frontiers[b2]))
	  // Propagate the (potential) new phi node definitions in B2.
	  bitmap_set_bit (worklist, b2);
    }

  while (!bitmap_empty_p (worklist))
    {
      unsigned int b1 = bitmap_first_set_bit (worklist);
      bitmap_clear_bit (worklist, b1);

      // Restrict the phi nodes to registers that are live on entry to
      // the block.
      bitmap b1_in = DF_LR_IN (BASIC_BLOCK_FOR_FN (m_fn, b1));
      bitmap b1_phis = &bi.bb_phis[b1].regs;
      if (!bitmap_ior_and_into (b1_phis, &unfiltered[b1], b1_in))
	continue;

      // If block B1 has a phi node for R and if B2 is in the dominance
      // frontier of B1, queue a possible phi node for R in B2.
      bitmap_iterator bmi;
      unsigned int b2;
      EXECUTE_IF_SET_IN_BITMAP (&frontiers[b1], 0, b2, bmi)
	if (bitmap_ior_into (&unfiltered[b2], b1_phis)
	    && !bitmap_empty_p (&frontiers[b2]))
	  bitmap_set_bit (worklist, b2);
    }

  basic_block cfg_bb;
  FOR_ALL_BB_FN (cfg_bb, m_fn)
    {
      // Calculate the set of phi nodes for blocks that don't have any
      // dominance frontiers.  We only need to do this once per block.
      unsigned int i = cfg_bb->index;
      bb_phi_info &phis = bi.bb_phis[i];
      if (bitmap_empty_p (&frontiers[i]))
	bitmap_and (&phis.regs, &unfiltered[i], DF_LR_IN (cfg_bb));

      // Create an array that contains all phi inputs for this block.
      // See the comment above the member variables for more information.
      phis.num_phis = bitmap_count_bits (&phis.regs);
      phis.num_preds = EDGE_COUNT (cfg_bb->preds);
      unsigned int num_inputs = phis.num_phis * phis.num_preds;
      if (num_inputs != 0)
	{
	  phis.inputs = XOBNEWVEC (&m_temp_obstack, set_info *, num_inputs);
	  memset (phis.inputs, 0, num_inputs * sizeof (phis.inputs[0]));
	}
    }

  // Free the temporary bitmaps.
  for (unsigned int i = 0; i < num_bb_indices; ++i)
    {
      bitmap_release (&frontiers[i]);
      bitmap_release (&unfiltered[i]);
    }
}

// Called while building SSA form using BI, with BI.current_bb being
// the entry block.
//
// Create the entry block instructions and their definitions.  The only
// useful instruction is the end instruction, which carries definitions
// for the values that are live on entry to the function.  However, it
// seems simpler to create a head instruction too, rather than force all
// users of the block information to treat the entry block as a special case.
void
function_info::add_entry_block_defs (build_info &bi)
{
  bb_info *bb = bi.current_bb;
  basic_block cfg_bb = bi.current_bb->cfg_bb ();
  auto *lr_info = DF_LR_BB_INFO (cfg_bb);

  bb->set_head_insn (append_artificial_insn (bb));
  insn_info *insn = append_artificial_insn (bb);
  bb->set_end_insn (insn);

  start_insn_accesses ();

  // Using LR to derive the liveness information means that we create an
  // entry block definition for upwards exposed registers.  These registers
  // are sometimes genuinely uninitialized.  However, some targets also
  // create a pseudo PIC base register and only initialize it later.
  // Handling that case correctly seems more important than optimizing
  // uninitialized uses.
  unsigned int regno;
  bitmap_iterator in_bi;
  EXECUTE_IF_SET_IN_BITMAP (&lr_info->out, 0, regno, in_bi)
    {
      auto *set = allocate<set_info> (insn, full_register (regno));
      append_def (set);
      m_temp_defs.safe_push (set);
      bi.record_reg_def (set);
    }

  // Create a definition that reflects the state of memory on entry to
  // the function.
  auto *set = allocate<set_info> (insn, memory);
  append_def (set);
  m_temp_defs.safe_push (set);
  bi.record_mem_def (set);

  finish_insn_accesses (insn);
}

// Lazily calculate the value of BI.ebb_live_in_for_debug for BI.current_ebb.
void
function_info::calculate_ebb_live_in_for_debug (build_info &bi)
{
  gcc_checking_assert (bitmap_empty_p (bi.tmp_ebb_live_in_for_debug));
  bi.ebb_live_in_for_debug = bi.tmp_ebb_live_in_for_debug;
  bitmap_and (bi.ebb_live_in_for_debug, bi.potential_phi_regs_for_debug,
	      DF_LR_IN (bi.current_ebb->first_bb ()->cfg_bb ()));
  bitmap_tree_view (bi.ebb_live_in_for_debug);
}

// Called while building SSA form using BI.  Create phi nodes for the
// current EBB.
void
function_info::add_phi_nodes (build_info &bi)
{
  ebb_info *ebb = bi.current_ebb;
  basic_block cfg_bb = ebb->first_bb ()->cfg_bb ();

  // Create the register phis for this EBB.
  bb_phi_info &phis = bi.bb_phis[cfg_bb->index];
  unsigned int num_preds = phis.num_preds;
  unsigned int regno;
  bitmap_iterator in_bi;
  EXECUTE_IF_SET_IN_BITMAP (&phis.regs, 0, regno, in_bi)
    {
      gcc_checking_assert (bitmap_bit_p (bi.potential_phi_regs, regno));

      // Create an array of phi inputs, to be filled in later.
      auto *inputs = XOBNEWVEC (&m_obstack, access_info *, num_preds);
      memset (inputs, 0, sizeof (access_info *) * num_preds);

      // Later code works out the correct mode of the phi.  Use BLKmode
      // as a placeholder for now.
      phi_info *phi = create_phi (ebb, { E_BLKmode, regno },
				  inputs, num_preds);
      bi.record_reg_def (phi);
    }

  bitmap_copy (bi.ebb_def_regs, &phis.regs);

  // Collect the live-in memory definitions and record whether they're
  // all the same.
  m_temp_defs.reserve (num_preds);
  set_info *mem_value = nullptr;
  bool mem_phi_is_degenerate = true;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
    {
      bb_info *pred_bb = this->bb (e->src);
      if (pred_bb && pred_bb->head_insn ())
	{
	  mem_value = bi.bb_mem_live_out[pred_bb->index ()];
	  m_temp_defs.quick_push (mem_value);
	  if (mem_value != m_temp_defs[0])
	    mem_phi_is_degenerate = false;
	}
      else
	{
	  m_temp_defs.quick_push (nullptr);
	  mem_phi_is_degenerate = false;
	}
    }

  // Create a phi for memory, on the assumption that something in the
  // EBB will need it.
  if (mem_phi_is_degenerate)
    {
      access_info *input[] = { mem_value };
      mem_value = create_phi (ebb, memory, input, 1);
    }
  else
    {
      obstack_grow (&m_obstack, m_temp_defs.address (),
		    num_preds * sizeof (access_info *));
      auto *inputs = static_cast<access_info **> (obstack_finish (&m_obstack));
      mem_value = create_phi (ebb, memory, inputs, num_preds);
    }
  bi.record_mem_def (mem_value);
  m_temp_defs.truncate (0);
}

// Called while building SSA form using BI.
//
// If FLAGS is DF_REF_AT_TOP, create the head insn for BI.current_bb
// and populate its uses and definitions.  If FLAGS is 0, do the same
// for the end insn.
void
function_info::add_artificial_accesses (build_info &bi, df_ref_flags flags)
{
  bb_info *bb = bi.current_bb;
  basic_block cfg_bb = bb->cfg_bb ();
  auto *lr_info = DF_LR_BB_INFO (cfg_bb);
  df_ref ref;

  insn_info *insn;
  if (flags == DF_REF_AT_TOP)
    {
      if (cfg_bb->index == EXIT_BLOCK)
	insn = append_artificial_insn (bb);
      else
	insn = append_artificial_insn (bb, bb_note (cfg_bb));
      bb->set_head_insn (insn);
    }
  else
    {
      insn = append_artificial_insn (bb);
      bb->set_end_insn (insn);
    }

  start_insn_accesses ();

  HARD_REG_SET added_regs = {};
  FOR_EACH_ARTIFICIAL_USE (ref, cfg_bb->index)
    if ((DF_REF_FLAGS (ref) & DF_REF_AT_TOP) == flags)
      {
	unsigned int regno = DF_REF_REGNO (ref);
	machine_mode mode = GET_MODE (DF_REF_REAL_REG (ref));
	if (HARD_REGISTER_NUM_P (regno))
	  SET_HARD_REG_BIT (added_regs, regno);

	// A definition must be available.
	gcc_checking_assert (bitmap_bit_p (&lr_info->in, regno)
			     || (flags != DF_REF_AT_TOP
				 && bitmap_bit_p (&lr_info->def, regno)));
	m_temp_uses.safe_push (create_reg_use (bi, insn, { mode, regno }));
      }

  // Ensure that global registers and memory are live at the end of any
  // block that has no successors, such as the exit block and non-local gotos.
  // Global registers have to be singled out because they are not part of
  // the DF artifical use list (they are instead treated as used within
  // every block).
  if (flags == 0 && EDGE_COUNT (cfg_bb->succs) == 0)
    {
      for (unsigned int i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
	if (global_regs[i] && !TEST_HARD_REG_BIT (added_regs, i))
	  {
	    auto mode = reg_raw_mode[i];
	    m_temp_uses.safe_push (create_reg_use (bi, insn, { mode, i }));
	  }

      auto *use = allocate<use_info> (insn, memory, bi.current_mem_value ());
      add_use (use);
      m_temp_uses.safe_push (use);
    }

  FOR_EACH_ARTIFICIAL_DEF (ref, cfg_bb->index)
    if ((DF_REF_FLAGS (ref) & DF_REF_AT_TOP) == flags)
      {
	unsigned int regno = DF_REF_REGNO (ref);
	machine_mode mode = GET_MODE (DF_REF_REAL_REG (ref));
	resource_info resource { mode, regno };

	// We rely on the def set being correct.
	gcc_checking_assert (bitmap_bit_p (&lr_info->def, regno));

	// If the value isn't used later in the block and isn't live
	// on exit, we could instead represent the definition as a
	// clobber_info.  However, that case should be relatively
	// rare and set_info is any case more compact than clobber_info.
	set_info *def = allocate<set_info> (insn, resource);
	append_def (def);
	m_temp_defs.safe_push (def);
	bi.record_reg_def (def);
      }

  // Model the effect of a memory clobber on an incoming edge by adding
  // a fake definition of memory at the start of the block.  We don't need
  // to add a use of the phi node because memory is implicitly always live.
  if (flags == DF_REF_AT_TOP && has_abnormal_call_or_eh_pred_edge_p (cfg_bb))
    {
      set_info *def = allocate<set_info> (insn, memory);
      append_def (def);
      m_temp_defs.safe_push (def);
      bi.record_mem_def (def);
    }

  finish_insn_accesses (insn);
}

// Called while building SSA form using BI.  Create insn_infos for all
// relevant instructions in BI.current_bb.
void
function_info::add_block_contents (build_info &bi)
{
  basic_block cfg_bb = bi.current_bb->cfg_bb ();
  rtx_insn *insn;
  FOR_BB_INSNS (cfg_bb, insn)
    if (INSN_P (insn))
      add_insn_to_block (bi, insn);
}

// Called while building SSA form using BI.  Record live-out register values
// in the phi inputs of successor blocks and create live-out uses where
// appropriate.  Record the live-out memory value in BI.bb_mem_live_out.
void
function_info::record_block_live_out (build_info &bi)
{
  bb_info *bb = bi.current_bb;
  ebb_info *ebb = bi.current_ebb;
  basic_block cfg_bb = bb->cfg_bb ();

  // Record the live-out register values in the phi inputs of
  // successor blocks.
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->succs)
    {
      bb_phi_info &phis = bi.bb_phis[e->dest->index];
      unsigned int input_i = e->dest_idx * phis.num_phis;
      unsigned int regno;
      bitmap_iterator out_bi;
      EXECUTE_IF_SET_IN_BITMAP (&phis.regs, 0, regno, out_bi)
	{
	  phis.inputs[input_i]
	    = live_out_value (bb, bi.current_reg_value (regno));
	  input_i += 1;
	}
    }

  // Add the set of registers that were defined in this BB to the set
  // of potentially-live registers defined in the EBB.
  bitmap_ior_into (bi.ebb_def_regs, &DF_LR_BB_INFO (cfg_bb)->def);

  // Iterate through the registers in LIVE_OUT and see whether we need
  // to add a live-out use for them.
  auto record_live_out_regs = [&](bitmap live_out)
    {
      unsigned int regno;
      bitmap_iterator out_bi;
      EXECUTE_IF_AND_IN_BITMAP (bi.ebb_def_regs, live_out, 0, regno, out_bi)
	{
	  set_info *value = live_out_value (bb, bi.current_reg_value (regno));
	  if (value && value->ebb () == ebb)
	    add_live_out_use (bb, value);
	}
    };

  if (bb == ebb->last_bb ())
    // All live-out registers might need live-out uses.
    record_live_out_regs (DF_LR_OUT (cfg_bb));
  else
    // Registers might need live-out uses if they are live on entry
    // to a successor block in a different EBB.
    FOR_EACH_EDGE (e, ei, cfg_bb->succs)
      {
	bb_info *dest_bb = this->bb (e->dest);
	if (dest_bb->ebb () != ebb || dest_bb == ebb->first_bb ())
	  record_live_out_regs (DF_LR_IN (e->dest));
      }

  // Record the live-out memory value.
  bi.bb_mem_live_out[cfg_bb->index]
    = live_out_value (bb, bi.current_mem_value ());
}

// Add BB and its contents to the SSA information.
void
function_info::start_block (build_info &bi, bb_info *bb)
{
  ebb_info *ebb = bb->ebb ();

  // We (need to) add all blocks from one EBB before moving on to the next.
  bi.current_bb = bb;
  if (bb == ebb->first_bb ())
    bi.current_ebb = ebb;
  else
    gcc_assert (bi.current_ebb == ebb);

  // Record the start of this block's definitions in the definitions stack.
  bi.old_def_stack_limit.safe_push (bi.def_stack.length ());

  // Add the block itself.
  append_bb (bb);

  // If the block starts an EBB, create the phi insn.  This insn should exist
  // for all EBBs, even if they don't (yet) need phis.
  if (bb == ebb->first_bb ())
    ebb->set_phi_insn (append_artificial_insn (bb));

  if (bb->index () == ENTRY_BLOCK)
    {
      add_entry_block_defs (bi);
      record_block_live_out (bi);
      return;
    }

  if (EDGE_COUNT (bb->cfg_bb ()->preds) == 0)
    {
      // Leave unreachable blocks empty, since there is no useful
      // liveness information for them, and anything they do will
      // be wasted work.  In a cleaned-up cfg, the only unreachable
      // block we should see is the exit block of a noreturn function.
      bb->set_head_insn (append_artificial_insn (bb));
      bb->set_end_insn (append_artificial_insn (bb));
      return;
    }

  // If the block starts an EBB, create the phi nodes.
  if (bb == ebb->first_bb ())
    add_phi_nodes (bi);

  // Process the contents of the block.
  add_artificial_accesses (bi, DF_REF_AT_TOP);
  if (bb->index () != EXIT_BLOCK)
    add_block_contents (bi);
  add_artificial_accesses (bi, df_ref_flags ());
  record_block_live_out (bi);

  // If we needed to calculate a live-in set for debug purposes,
  // reset it to null at the end of the EBB.  Convert the underlying
  // bitmap to an empty list view, ready for the next calculation.
  if (bi.ebb_live_in_for_debug && bb == ebb->last_bb ())
    {
      bitmap_clear (bi.tmp_ebb_live_in_for_debug);
      bitmap_list_view (bi.tmp_ebb_live_in_for_debug);
      bi.ebb_live_in_for_debug = nullptr;
    }
}

// Finish adding BB and the blocks that it dominates to the SSA information.
void
function_info::end_block (build_info &bi, bb_info *bb)
{
  // Restore the register last_access information to the state it was
  // in before we started processing BB.
  unsigned int old_limit = bi.old_def_stack_limit.pop ();
  while (bi.def_stack.length () > old_limit)
    {
      // We pushed a definition in BB if it was the first dominating
      // definition (and so the previous entry was null).  In other
      // cases we pushed the previous dominating definition.
      def_info *def = bi.def_stack.pop ();
      unsigned int regno = def->regno ();
      if (def->bb () == bb)
	def = nullptr;
      bi.last_access[regno + 1] = def;
    }
}

// Finish setting up the phi nodes for each block, now that we've added
// the contents of all blocks.
void
function_info::populate_phi_inputs (build_info &bi)
{
  auto_vec<phi_info *, 32> sorted_phis;
  for (ebb_info *ebb : ebbs ())
    {
      if (!ebb->first_phi ())
	continue;

      // Get a sorted array of EBB's phi nodes.
      basic_block cfg_bb = ebb->first_bb ()->cfg_bb ();
      bb_phi_info &phis = bi.bb_phis[cfg_bb->index];
      sorted_phis.truncate (0);
      for (phi_info *phi : ebb->phis ())
	sorted_phis.safe_push (phi);
      std::sort (sorted_phis.address (),
		 sorted_phis.address () + sorted_phis.length (),
		 compare_access_infos);

      // Set the inputs of the non-degenerate register phis.  All inputs
      // for one edge come before all inputs for the next edge.
      set_info **inputs = phis.inputs;
      unsigned int phi_i = 0;
      bitmap_iterator bmi;
      unsigned int regno;
      EXECUTE_IF_SET_IN_BITMAP (&phis.regs, 0, regno, bmi)
	{
	  // Skip intervening degenerate phis.
	  while (sorted_phis[phi_i]->regno () < regno)
	    phi_i += 1;
	  phi_info *phi = sorted_phis[phi_i];
	  gcc_assert (phi->regno () == regno);
	  for (unsigned int input_i = 0; input_i < phis.num_preds; ++input_i)
	    if (set_info *input = inputs[input_i * phis.num_phis])
	      {
		use_info *use = phi->input_use (input_i);
		gcc_assert (!use->def ());
		use->set_def (input);
		add_use (use);
	      }
	  phi_i += 1;
	  inputs += 1;
	}

      // Fill in the backedge inputs to any memory phi.
      phi_info *mem_phi = sorted_phis.last ();
      if (mem_phi->is_mem () && !mem_phi->is_degenerate ())
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
	    {
	      use_info *use = mem_phi->input_use (e->dest_idx);
	      if (!use->def ())
		{
		  use->set_def (bi.bb_mem_live_out[e->src->index]);
		  add_use (use);
		}
	    }
	}
    }
}

// Return true if it would be better to continue an EBB across NEW_EDGE
// rather than across OLD_EDGE, given that both edges are viable candidates.
// This is not a total ordering.
static bool
better_ebb_edge_p (edge new_edge, edge old_edge)
{
  // Prefer the likeliest edge.
  if (new_edge->probability.initialized_p ()
      && old_edge->probability.initialized_p ()
      && !(old_edge->probability == new_edge->probability))
    return old_edge->probability < new_edge->probability;

  // If both edges are equally likely, prefer a fallthru edge.
  if (new_edge->flags & EDGE_FALLTHRU)
    return true;
  if (old_edge->flags & EDGE_FALLTHRU)
    return false;

  // Otherwise just stick with OLD_EDGE.
  return false;
}

// Pick and return the next basic block in an EBB that currently ends with BB.
// Return null if the EBB must end with BB.
static basic_block
choose_next_block_in_ebb (basic_block bb)
{
  // Although there's nothing in principle wrong with having an EBB that
  // starts with the entry block and includes later blocks, there's not
  // really much point either.  Keeping the entry block separate means
  // that uses of arguments consistently occur through phi nodes, rather
  // than the arguments sometimes appearing to come from an EBB-local
  // definition instead.
  if (bb->index == ENTRY_BLOCK)
    return nullptr;

  bool optimize_for_speed_p = optimize_bb_for_speed_p (bb);
  edge best_edge = nullptr;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_COMPLEX)
	&& e->dest->index != EXIT_BLOCK
	&& single_pred_p (e->dest)
	&& optimize_for_speed_p == optimize_bb_for_speed_p (e->dest)
	&& (!best_edge || better_ebb_edge_p (e, best_edge)))
      best_edge = e;

  return best_edge ? best_edge->dest : nullptr;
}

// Partition the function into extended basic blocks.  Create the
// associated ebb_infos and bb_infos, but don't add the bb_infos
// to the function list yet.
void
function_info::create_ebbs (build_info &bi)
{
  // Compute the starting reverse postorder.  We tweak this later to try
  // to get better EBB assignments.
  auto *postorder = new int[n_basic_blocks_for_fn (m_fn)];
  unsigned int postorder_num
    = pre_and_rev_post_order_compute (nullptr, postorder, true);
  gcc_assert (int (postorder_num) <= n_basic_blocks_for_fn (m_fn));

  // Iterate over the blocks in reverse postorder.  In cases where
  // multiple possible orders exist, prefer orders that chain blocks
  // together into EBBs.  If multiple possible EBBs exist, try to pick
  // the ones that are most likely to be profitable.
  auto_vec<bb_info *, 16> bbs;
  unsigned int next_bb_index = 0;
  for (unsigned int i = 0; i < postorder_num; ++i)
    if (!m_bbs[postorder[i]])
      {
	// Choose and create the blocks that should form the next EBB.
	basic_block cfg_bb = BASIC_BLOCK_FOR_FN (m_fn, postorder[i]);
	do
	  {
	    // Record the chosen block order in a new RPO.
	    bi.bb_to_rpo[cfg_bb->index] = next_bb_index++;
	    bbs.safe_push (create_bb_info (cfg_bb));
	    cfg_bb = choose_next_block_in_ebb (cfg_bb);
	  }
	while (cfg_bb);

	// Create the EBB itself.
	auto *ebb = allocate<ebb_info> (bbs[0], bbs.last ());
	for (bb_info *bb : bbs)
	  bb->set_ebb (ebb);
	bbs.truncate (0);
      }

  delete[] postorder;
}

// Partition the function's blocks into EBBs and build SSA form for all
// EBBs in the function.
void
function_info::process_all_blocks ()
{
  auto temps = temp_watermark ();
  unsigned int num_bb_indices = last_basic_block_for_fn (m_fn);

  build_info bi (m_num_regs, num_bb_indices);

  // ??? There is no dominance information associated with the exit block,
  // so work out its immediate dominator using predecessor blocks.
  for (edge e : EXIT_BLOCK_PTR_FOR_FN (m_fn)->preds)
    if (bi.exit_block_dominator)
      bi.exit_block_dominator
	= nearest_common_dominator (CDI_DOMINATORS,
				    bi.exit_block_dominator, e->src);
    else
      bi.exit_block_dominator = e->src;

  calculate_potential_phi_regs (bi);
  create_ebbs (bi);
  place_phis (bi);
  bb_walker (this, bi).walk (ENTRY_BLOCK_PTR_FOR_FN (m_fn));
  populate_phi_inputs (bi);

  if (flag_checking)
    {
      // The definition stack should be empty and all register definitions
      // should be back in their original undefined state.
      gcc_assert (bi.def_stack.is_empty ()
		  && bi.old_def_stack_limit.is_empty ());
      for (unsigned int regno = 0; regno < m_num_regs; ++regno)
	gcc_assert (!bi.last_access[regno + 1]);
    }
}

// Print a description of CALL_CLOBBERS to PP.
void
rtl_ssa::pp_ebb_call_clobbers (pretty_printer *pp,
			       const ebb_call_clobbers_info *call_clobbers)
{
  if (!call_clobbers)
    pp_string (pp, "<null>");
  else
    call_clobbers->print_full (pp);
}

// Print a description of BB to PP.
void
rtl_ssa::pp_bb (pretty_printer *pp, const bb_info *bb)
{
  if (!bb)
    pp_string (pp, "<null>");
  else
    bb->print_full (pp);
}

// Print a description of EBB to PP
void
rtl_ssa::pp_ebb (pretty_printer *pp, const ebb_info *ebb)
{
  if (!ebb)
    pp_string (pp, "<null>");
  else
    ebb->print_full (pp);
}

// Print a description of CALL_CLOBBERS to FILE.
void
dump (FILE *file, const ebb_call_clobbers_info *call_clobbers)
{
  dump_using (file, pp_ebb_call_clobbers, call_clobbers);
}

// Print a description of BB to FILE.
void
dump (FILE *file, const bb_info *bb)
{
  dump_using (file, pp_bb, bb);
}

// Print a description of EBB to FILE.
void
dump (FILE *file, const ebb_info *ebb)
{
  dump_using (file, pp_ebb, ebb);
}

// Debug interfaces to the dump routines above.
void debug (const ebb_call_clobbers_info *x) { dump (stderr, x); }
void debug (const bb_info *x) { dump (stderr, x); }
void debug (const ebb_info *x) { dump (stderr, x); }
