// Implementation of basic-block-related functions for RTL SSA      -*- C++ -*-
// Copyright (C) 2020 Free Software Foundation, Inc.
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
#include "rtl-ssa/internals.inl"
#include "cfganal.h"
#include "cfgrtl.h"
#include "predict.h"

using namespace rtl_ssa;

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
  unsigned int regno = def->regno ();
  if (find_access (bb->end_insn ()->uses (), regno))
    return;

  // Currently there is no need to maintain a backward link from the end
  // instruction to the list of live-out uses.  Such a list would be
  // expensive to update if it was represented using the usual insn_info
  // access arrays.
  use_info *use = allocate<use_info> (bb->end_insn (), def->resource (), def);
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
      bi.record_reg_def (regno, set);
    }

  // Create a definition that reflects the state of memory on entry to
  // the function.
  auto *set = allocate<set_info> (insn, memory);
  append_def (set);
  m_temp_defs.safe_push (set);
  bi.record_mem_def (set);

  finish_insn_accesses (insn);
}

// Called while building SSA form using BI.  Create phi nodes for the
// current EBB, leaving backedge inputs to be filled in later.  Set
// bi.last_access to the values that are live on entry to the EBB,
// regardless of whether or not they are phi nodes.
void
function_info::add_phi_nodes (build_info &bi)
{
  ebb_info *ebb = bi.current_ebb;
  basic_block cfg_bb = ebb->first_bb ()->cfg_bb ();
  auto *lr_info = DF_LR_BB_INFO (cfg_bb);

  // Get a local cache of the predecessor blocks' live out values.
  unsigned int num_preds = EDGE_COUNT (cfg_bb->preds);
  auto_vec<const bb_live_out_info *, 16> pred_live_outs (num_preds);
  bool has_backedge = false;
  bool has_eh_edge = false;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
    {
      bb_info *pred_bb = this->bb (e->src);
      const bb_live_out_info *live_out = &bi.bb_live_out[e->src->index];

      // In LR (but not LIVE), the registers live on entry to a block must
      // normally be a subset of the registers live on exit from any
      // given predecessor block.  The exceptions are EH edges, which
      // implicitly clobber all registers in eh_edge_abi.full_reg_clobbers ().
      // Thus if a register is upwards exposed in an EH handler, it won't
      // be propagated across the EH edge.
      //
      // Excluding that special case, all registers live on entry to
      // EBB are also live on exit from PRED_BB and were (or will be)
      // considered when creating LIVE_OUT.
      gcc_checking_assert ((e->flags & EDGE_EH)
			   || !bitmap_intersect_compl_p (&lr_info->in,
							 DF_LR_OUT (e->src)));
      if (!pred_bb || !pred_bb->head_insn ())
	{
	  has_backedge = true;
	  live_out = nullptr;
	}
      has_eh_edge |= (e->flags & EDGE_EH);
      pred_live_outs.quick_push (live_out);
    }

  // PRED_REG_INDICES[I] tracks the index into PRED_LIVE_OUTS[I]->reg_values
  // of the first unused entry.
  auto_vec<unsigned int, 16> pred_reg_indices (num_preds);
  pred_reg_indices.quick_grow_cleared (num_preds);

  // Use this array to build up the list of inputs to each phi.
  m_temp_defs.safe_grow (num_preds);

  // Return true if the current phi is degenerate, i.e. if all its inputs
  // are the same.
  auto is_degenerate_phi = [&]()
    {
      if (has_backedge)
	return false;

      for (unsigned int i = 1; i < num_preds; ++i)
	if (m_temp_defs[i] != m_temp_defs[0])
	  return false;

      return true;
    };

  // Finish calculating the live-in value for RESOURCE.  Decide how to
  // represent the value of RESOURCE on entry to EBB and return its definition.
  auto finish_phi = [&](resource_info resource) -> set_info *
    {
      access_info **inputs;
      unsigned int num_inputs;
      if (is_degenerate_phi ())
	{
	  auto *input = safe_as_a<set_info *> (m_temp_defs[0]);
	  if (!input)
	    // The live-in value is completely uninitialized.
	    return nullptr;

	  unsigned int regno = input->regno ();
	  if (input->is_reg () && !bitmap_bit_p (bi.ebb_use, regno))
	    // The live-in value comes from a single source and there
	    // are no uses of it within the EBB itself.  We therefore
	    // don't need a phi node.
	    return input;

	  // The live-in value comes from a single source and might be
	  // used by the EBB itself.  Create a degenerate phi for it.
	  inputs = m_temp_defs.begin ();
	  num_inputs = 1;
	}
      else
	{
	  obstack_grow (&m_obstack, m_temp_defs.address (),
			num_preds * sizeof (access_info *));
	  inputs = static_cast<access_info **> (obstack_finish (&m_obstack));
	  num_inputs = num_preds;
	}
      return create_phi (ebb, resource, inputs, num_inputs);
    };

  if (bi.ebb_live_in_for_debug)
    bitmap_clear (bi.ebb_live_in_for_debug);

  // Get the definition of each live input register, excluding registers
  // that are known to have a single definition that dominates all uses.
  unsigned int regno;
  bitmap_iterator in_bi;
  EXECUTE_IF_AND_IN_BITMAP (&lr_info->in, m_potential_phi_regs,
			    0, regno, in_bi)
    {
      for (unsigned int pred_i = 0; pred_i < num_preds; ++pred_i)
	{
	  set_info *input = nullptr;
	  if (const bb_live_out_info *pred_live_out = pred_live_outs[pred_i])
	    {
	      // Skip over registers that aren't live on entry to this block.
	      unsigned int reg_i = pred_reg_indices[pred_i];
	      while (reg_i < pred_live_out->num_reg_values
		     && pred_live_out->reg_values[reg_i]->regno () < regno)
		reg_i += 1;

	      // As we asserted above, REGNO is live out from the predecessor
	      // block, at least by the LR reckoning.  But there are three
	      // cases:
	      //
	      // (1) The live-out value is well-defined (the normal case),
	      //     with the definition coming either from the block itself
	      //     or from a predecessor block.  In this case reg_values
	      //     has a set_info entry for the register.
	      //
	      // (2) The live-out value was not modified by the predecessor
	      //     EBB and did not have a defined value on input to that
	      //     EBB either.  In this case reg_values has no entry for
	      //     the register.
	      //
	      // (3) The live-out value was modified by the predecessor EBB,
	      //     but the final modification was a clobber rather than
	      //     a set.  In this case reg_values again has no entry for
	      //     the register.
	      //
	      // The phi input for (2) and (3) is undefined, which we
	      // represent as a null set_info.
	      if (reg_i < pred_live_out->num_reg_values)
		{
		  set_info *set = pred_live_out->reg_values[reg_i];
		  if (set->regno () == regno)
		    {
		      input = set;
		      reg_i += 1;
		    }
		}

	      // Fully call-clobbered values do not survive across EH edges.
	      // In particular, if a call that normally sets a result register
	      // throws an exception, the set of the result register should
	      // not be treated as live on entry to the EH handler.
	      if (has_eh_edge
		  && HARD_REGISTER_NUM_P (regno)
		  && eh_edge_abi.clobbers_full_reg_p (regno)
		  && (EDGE_PRED (cfg_bb, pred_i)->flags & EDGE_EH))
		input = nullptr;

	      pred_reg_indices[pred_i] = reg_i;
	    }
	  m_temp_defs[pred_i] = input;
	}
      // Later code works out the correct mode of the phi.  Use BLKmode
      // as a placeholder for now.
      bi.record_reg_def (regno, finish_phi ({ E_BLKmode, regno }));
      if (bi.ebb_live_in_for_debug)
	bitmap_set_bit (bi.ebb_live_in_for_debug, regno);
    }

  // Repeat the process above for memory.
  for (unsigned int pred_i = 0; pred_i < num_preds; ++pred_i)
    {
      set_info *input = nullptr;
      if (const bb_live_out_info *pred_live_out = pred_live_outs[pred_i])
	input = pred_live_out->mem_value;
      m_temp_defs[pred_i] = input;
    }
  bi.record_mem_def (finish_phi (memory));

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

  FOR_EACH_ARTIFICIAL_USE (ref, cfg_bb->index)
    if ((DF_REF_FLAGS (ref) & DF_REF_AT_TOP) == flags)
      {
	unsigned int regno = DF_REF_REGNO (ref);
	machine_mode mode = GET_MODE (DF_REF_REAL_REG (ref));
	resource_info resource { mode, regno };

	// A definition must be available.
	gcc_checking_assert (bitmap_bit_p (&lr_info->in, regno)
			     || (flags != DF_REF_AT_TOP
				 && bitmap_bit_p (&lr_info->def, regno)));
	set_info *def = bi.current_reg_value (regno);
	auto *use = allocate<use_info> (insn, resource, def);
	add_use (use);
	m_temp_uses.safe_push (use);
      }

  // Track the return value of memory by adding an artificial use of
  // memory at the end of the exit block.
  if (flags == 0 && cfg_bb->index == EXIT_BLOCK)
    {
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

	// If the value isn't used later in the block and isn't live
	// on exit, we could instead represent the definition as a
	// clobber_info.  However, that case should be relatively
	// rare and set_info is any case more compact than clobber_info.
	set_info *def = allocate<set_info> (insn, resource);
	append_def (def);
	m_temp_defs.safe_push (def);
	bi.record_reg_def (regno, def);
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

// Called while building SSA form using BI.  Use BI.bb_live_out to record
// the values that are live out from BI.current_bb.
void
function_info::record_block_live_out (build_info &bi)
{
  bb_info *bb = bi.current_bb;
  ebb_info *ebb = bi.current_ebb;
  basic_block cfg_bb = bb->cfg_bb ();
  bb_live_out_info *live_out = &bi.bb_live_out[bb->index ()];
  auto *lr_info = DF_LR_BB_INFO (bb->cfg_bb ());

  // Calculate which subset of m_potential_phi_regs is live out from EBB
  // at the end of BB.
  auto_bitmap live_out_from_ebb;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->succs)
    {
      bb_info *dest_bb = this->bb (e->dest);
      if (!dest_bb || dest_bb->ebb () != ebb)
	bitmap_ior_and_into (live_out_from_ebb, DF_LR_IN (e->dest),
			     m_potential_phi_regs);
    }

  // Record the live-out register values.
  unsigned int regno;
  bitmap_iterator out_bi;
  EXECUTE_IF_AND_IN_BITMAP (&lr_info->out, m_potential_phi_regs,
			    0, regno, out_bi)
    if (set_info *value = live_out_value (bb, bi.current_reg_value (regno)))
      {
	if (value->ebb () == ebb && bitmap_bit_p (live_out_from_ebb, regno))
	  add_live_out_use (bb, value);
	obstack_ptr_grow (&m_temp_obstack, value);
      }

  live_out->num_reg_values = (obstack_object_size (&m_temp_obstack)
			      / sizeof (set_info *));
  auto *data = obstack_finish (&m_temp_obstack);
  live_out->reg_values = static_cast<set_info **> (data);

  live_out->mem_value = live_out_value (bb, bi.current_mem_value ());
}

// Called while building SSA form using BI.  Check if BI.current_bb has
// any outgoing backedges.  If so, use the up-to-date contents of
// BI.bb_live_out to populate the associated inputs of any phi nodes.
void
function_info::populate_backedge_phis (build_info &bi)
{
  bb_info *bb = bi.current_bb;
  basic_block cfg_bb = bb->cfg_bb ();
  const bb_live_out_info *live_out = &bi.bb_live_out[bb->index ()];

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->succs)
    {
      // Check if this edge counts as a backedge in the current traversal.
      bb_info *succ_bb = this->bb (e->dest);
      if (!succ_bb || !succ_bb->head_insn ())
	continue;

      // Although the phis do not keep a defined order long-term, they are
      // still in reverse regno order at this point.  We can therefore use
      // a merge operation on the phis and the live-out values.
      unsigned int input_i = e->dest_idx;
      int reg_i = live_out->num_reg_values - 1;
      for (phi_info *phi : succ_bb->ebb ()->phis ())
	{
	  set_info *input = nullptr;
	  if (phi->is_mem ())
	    input = live_out->mem_value;
	  else
	    {
	      // Skip over any intervening live-out values.
	      unsigned int regno = phi->regno ();
	      while (reg_i >= 0)
		{
		  set_info *reg_value = live_out->reg_values[reg_i];
		  if (reg_value->regno () < regno)
		    break;
		  reg_i -= 1;
		  if (reg_value->regno () == regno)
		    {
		      input = reg_value;
		      break;
		    }
		}
	    }
	  if (input)
	    {
	      use_info *use = phi->input_use (input_i);
	      gcc_assert (!use->def ());
	      use->set_def (input);
	      add_use (use);
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

// Partition the function's blocks into EBBs and build SSA form for all
// EBBs in the function.
void
function_info::process_all_blocks ()
{
  auto temps = temp_watermark ();
  unsigned int num_bb_indices = last_basic_block_for_fn (m_fn);

  // Compute the starting reverse postorder.  We tweak this later to try
  // to get better EBB assignments.
  auto *postorder = new int[n_basic_blocks_for_fn (m_fn)];
  unsigned int postorder_num
    = pre_and_rev_post_order_compute (nullptr, postorder, true);
  gcc_assert (int (postorder_num) <= n_basic_blocks_for_fn (m_fn));

  // Construct the working state for this function and its subroutines.
  build_info bi;
  bi.last_access = XOBNEWVEC (&m_temp_obstack, access_info *, m_num_regs + 1);
  memset (bi.last_access, 0, (m_num_regs + 1) * sizeof (set_info *));

  // The bb_live_out array shouldn't need to be initialized, since we'll
  // always write to an entry before reading from it.  But poison the
  // contents when checking, just to make sure we don't accidentally use
  // an uninitialized value.
  bi.bb_live_out = XOBNEWVEC (&m_temp_obstack, bb_live_out_info,
			      num_bb_indices);
  if (flag_checking)
    memset (bi.bb_live_out, 0xaf,
	    num_bb_indices * sizeof (bb_live_out_info));

  // Only pay the overhead of recording a separate live-in bitmap if
  // there are debug instructions that might need it.
  auto_bitmap ebb_live_in;
  if (MAY_HAVE_DEBUG_INSNS)
    {
      bi.ebb_live_in_for_debug = ebb_live_in;
      // The bitmap is tested using individual bit operations, so optimize
      // for that case.
      bitmap_tree_view (ebb_live_in);
    }
  else
    bi.ebb_live_in_for_debug = nullptr;

  // Iterate over the blocks in reverse postorder.  In cases where
  // multiple possible orders exist, prefer orders that chain blocks
  // together into EBBs.  If multiple possible EBBs exist, try to pick
  // the ones that are most likely to be profitable.
  auto_vec<bb_info *, 16> ebb;
  auto_bitmap ebb_use_tmp;
  auto_bitmap ebb_def_tmp;
  for (unsigned int i = 0; i < postorder_num; ++i)
    if (!m_bbs[postorder[i]])
      {
	// Choose and create the blocks that should form the next EBB,
	// and calculate the set of registers that the EBB uses and defines
	// Only do actual bitmap operations if the EBB contains multiple
	// blocks.
	basic_block cfg_bb = BASIC_BLOCK_FOR_FN (m_fn, postorder[i]);
	bi.ebb_use = &DF_LR_BB_INFO (cfg_bb)->use;
	bi.ebb_def = &DF_LR_BB_INFO (cfg_bb)->def;
	ebb.safe_push (create_bb_info (cfg_bb));
	cfg_bb = choose_next_block_in_ebb (cfg_bb);
	if (cfg_bb)
	  {
	    // An EBB with two blocks.
	    bitmap_ior (ebb_use_tmp, bi.ebb_use, &DF_LR_BB_INFO (cfg_bb)->use);
	    bitmap_ior (ebb_def_tmp, bi.ebb_def, &DF_LR_BB_INFO (cfg_bb)->def);
	    bi.ebb_use = ebb_use_tmp;
	    bi.ebb_def = ebb_def_tmp;
	    ebb.safe_push (create_bb_info (cfg_bb));
	    cfg_bb = choose_next_block_in_ebb (cfg_bb);
	    while (cfg_bb)
	      {
		// An EBB with three or more blocks.
		bitmap_ior_into (bi.ebb_use, &DF_LR_BB_INFO (cfg_bb)->use);
		bitmap_ior_into (bi.ebb_def, &DF_LR_BB_INFO (cfg_bb)->def);
		ebb.safe_push (create_bb_info (cfg_bb));
		cfg_bb = choose_next_block_in_ebb (cfg_bb);
	      }
	  }

	// Create the EBB itself.
	bi.current_ebb = allocate<ebb_info> (ebb[0], ebb.last ());
	for (bb_info *bb : ebb)
	  {
	    bb->set_ebb (bi.current_ebb);
	    append_bb (bb);
	  }

	// Populate the contents of the EBB.
	bi.current_ebb->set_phi_insn (append_artificial_insn (ebb[0]));
	if (ebb[0]->index () == ENTRY_BLOCK)
	  {
	    gcc_assert (ebb.length () == 1);
	    bi.current_bb = ebb[0];
	    add_entry_block_defs (bi);
	    record_block_live_out (bi);
	  }
	else if (EDGE_COUNT (ebb[0]->cfg_bb ()->preds) == 0)
	  // Leave unreachable blocks empty, since there is no useful
	  // liveness information for them, and anything they do will
	  // be wasted work.  In a cleaned-up cfg, the only unreachable
	  // block we should see is the exit block of a noreturn function.
	  for (bb_info *bb : ebb)
	    {
	      bb->set_head_insn (append_artificial_insn (bb));
	      bb->set_end_insn (append_artificial_insn (bb));
	    }
	else
	  {
	    add_phi_nodes (bi);
	    for (bb_info *bb : ebb)
	      {
		bi.current_bb = bb;
		add_artificial_accesses (bi, DF_REF_AT_TOP);
		if (bb->index () != EXIT_BLOCK)
		  add_block_contents (bi);
		add_artificial_accesses (bi, df_ref_flags ());
		record_block_live_out (bi);
		populate_backedge_phis (bi);
	      }
	  }
	ebb.truncate (0);
      }

  delete[] postorder;
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
