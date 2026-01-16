/* Copyright The GNU Toolchain Authors

This file is part of GCC.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "hierarchical_discriminator.h"
#include "cfghooks.h"
#include "diagnostic-core.h"
#include "function.h"
#include "rtl.h"
#include "basic-block.h"

/* Helper to update a location with new discriminator components.  If
   multiplicity_factor is non-zero, multiply existing multiplicity.
   If copyid is non-zero, set it (otherwise preserve existing).  */

static location_t
update_location_discriminator (location_t loc,
			       unsigned int multiplicity_factor,
			       unsigned int copyid)
{
  if (loc == UNKNOWN_LOCATION)
    return loc;

  discriminator_components comp = get_discriminator_components_from_loc (loc);

  /* Multiply existing multiplicity if requested.  */
  if (multiplicity_factor > 0)
    {
      unsigned int new_mult = (comp.multiplicity == 0)
	? multiplicity_factor
	: comp.multiplicity * multiplicity_factor;
      if (new_mult > DISCR_MULTIPLICITY_MAX)
	new_mult = DISCR_MULTIPLICITY_MAX;
      comp.multiplicity = new_mult;
    }

  /* Update copyid if requested.  */
  if (copyid > 0)
    comp.copyid = copyid;

  return location_with_discriminator_components (loc, comp);
}

/* Assign discriminators to a statement
   Updates the multiplicity and/or copyid discriminator components for
   all statements in the given basic block, while preserving the base
   discriminator.

   If multiplicity_factor > 0, multiply existing multiplicity by this factor.
   If copyid > 0, set it to this value.  */

void
assign_discriminators_to_stmt (gimple* stmt,
			       unsigned int multiplicity_factor,
			       unsigned int copyid)
{
  location_t loc = gimple_location (stmt);

  if (loc != UNKNOWN_LOCATION)
    {
      location_t new_loc
	= update_location_discriminator (loc,
					 multiplicity_factor,
					 copyid);
      gimple_set_location (stmt, new_loc);
    }
}


/* Assign discriminators to all statements in a basic block.  This
   function updates the multiplicity and/or copyid discriminator components for
   all statements in the given basic block, while preserving the base
   discriminator.

   If multiplicity_factor > 0, multiply existing multiplicity by this factor.
   If copyid > 0, set it to this value.  */

void
assign_discriminators_to_bb (basic_block bb,
			     unsigned int multiplicity_factor,
			     unsigned int copyid)
{
  gimple_stmt_iterator gsi;
  gphi_iterator phi_gsi;
  edge e;
  edge_iterator ei;

  /* Update PHI statements.  */
  for (phi_gsi = gsi_start_phis (bb); !gsi_end_p (phi_gsi);
       gsi_next (&phi_gsi))
    {
      gphi *phi = phi_gsi.phi ();
      location_t loc = gimple_location (phi);

      if (loc != UNKNOWN_LOCATION)
	{
	  location_t new_loc
	    = update_location_discriminator (loc,
					     multiplicity_factor,
					     copyid);
	  gimple_set_location (phi, new_loc);
	}
    }

  /* Update regular statements.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (is_gimple_debug (stmt))
	continue;

      location_t loc = gimple_location (stmt);
      if (loc != UNKNOWN_LOCATION)
	{
	  location_t new_loc
	    = update_location_discriminator (loc,
					     multiplicity_factor,
					     copyid);
	  gimple_set_location (stmt, new_loc);
	}
    }

  /* Update goto/edge locations.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      location_t loc = e->goto_locus;
      if (loc != UNKNOWN_LOCATION)
	{
	  location_t new_loc
	    = update_location_discriminator (loc,
					     multiplicity_factor,
					     copyid);
	  e->goto_locus = new_loc;
	}
    }
}

/* Assign discriminators to all basic blocks in a loop.  This function is
   used by loop versioning passes to assign version IDs and vectorization
   factors to all statements in a loop version.  */

void
assign_discriminators_to_loop (class loop *loop,
			       unsigned int multiplicity_factor,
			       unsigned int copyid)
{
  basic_block *bbs;
  unsigned int i;

  /* Get all basic blocks in the loop.  */
  bbs = get_loop_body (loop);

  /* Assign discriminators to all blocks in the loop.  */
  for (i = 0; i < loop->num_nodes; i++)
    assign_discriminators_to_bb (bbs[i], multiplicity_factor, copyid);

  free (bbs);
}


/* Helper to update the copyid allocator map with a location's existing copyid.
   If the location has a non-zero copyid, record that we need to start
   allocating from copyid+1 for this location.  */

static void
record_existing_copyid (location_t loc, struct function *fn)
{
  if (loc == UNKNOWN_LOCATION)
    return;

  location_t pure_loc = get_pure_location (loc);
  unsigned int discr = get_discriminator_from_loc (loc);
  if (discr == 0)
    return;

  /* Extract copyid from discriminator.  */
  unsigned int copyid = (discr >> DISCR_COPYID_SHIFT) & DISCR_COPYID_MASK;
  if (copyid == 0)
    return;

  /* Update max copyid for this location.  */
  unsigned int next_copyid = copyid + 1;
  if (next_copyid > DISCR_COPYID_MAX)
    next_copyid = DISCR_COPYID_MAX;

  unsigned int *existing = fn->copyid_alloc->location_map->get (pure_loc);
  if (!existing || *existing <= copyid)
    fn->copyid_alloc->location_map->put (pure_loc, next_copyid);
}

/* Initialize the per-function copyid allocator.  Walks the function
   body to find existing max copyids per location.  */

static void
init_copyid_allocator (struct function *fn)
{
  if (!fn)
    return;

  if (fn->copyid_alloc && fn->copyid_alloc->initialized)
    return;  /* Already initialized.  */

  if (!fn->copyid_alloc)
    {
      fn->copyid_alloc = XNEW (struct copyid_allocator);
      fn->copyid_alloc->location_map
	= new hash_map<int_hash<location_t, UNKNOWN_LOCATION,
			       UNKNOWN_LOCATION>, unsigned int>;
      fn->copyid_alloc->initialized = false;
    }

  /* Only walk the body if not yet initialized.  */
  if (fn->copyid_alloc->initialized)
    return;

  /* Walk the function body to find existing max copyids per location.
     This ensures we don't reuse copyids that were allocated in previous
     passes, during LTO, or brought in by inlining.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      if (current_ir_type () == IR_GIMPLE)
	{
	  /* Process PHI nodes.  */
	  gphi_iterator phi_gsi;
	  for (phi_gsi = gsi_start_phis (bb); !gsi_end_p (phi_gsi);
	       gsi_next (&phi_gsi))
	    record_existing_copyid (gimple_location (phi_gsi.phi ()), fn);

	  /* Process regular statements.  */
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    record_existing_copyid (gimple_location (gsi_stmt (gsi)), fn);

	  /* Process edge goto_locus locations.  */
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    record_existing_copyid (e->goto_locus, fn);
	}
      else
	{
	  /* For RTL mode.  */
	  rtx_insn *insn;
	  FOR_BB_INSNS (bb, insn)
	    {
	      if (INSN_P (insn))
		record_existing_copyid (INSN_LOCATION (insn), fn);
	    }
	}
    }

  fn->copyid_alloc->initialized = true;
}

/* Allocate a unique copy_id base for the given location.
   STRIDE indicates how many copy_ids to reserve (for unrolling N times,
   use stride=N).  Returns the base copy_id.  */

unsigned int
allocate_copyid_base (location_t loc, unsigned int stride)
{
  /* Need current function context.  */
  if (!cfun)
    return 1;  /* No function context, return default.  */

  init_copyid_allocator (cfun);

  loc = get_pure_location (loc);

  /* Cannot allocate copyid for unknown location.  */
  if (loc == UNKNOWN_LOCATION)
    return 1;  /* Return default base copyid.  */

  /* Check if this location has been seen before.  */
  unsigned int *existing = cfun->copyid_alloc->location_map->get (loc);
  if (existing)
    {
      /* Location already duplicated before.  Allocate next copy_id for it.  */
      unsigned int base = *existing;
      *existing += stride;  /* Update for next duplication.  */

      /* Clamp to maximum.  */
      if (*existing > DISCR_COPYID_MAX)
	*existing = DISCR_COPYID_MAX;
      return base;
    }
  else
    {
      /* First duplication at this location in this function.
	 Start at 1 (not 0, which means "no copyid").  */
      unsigned int base = 1;
      unsigned int next = base + stride;
      if (next > DISCR_COPYID_MAX)
	next = DISCR_COPYID_MAX;
      /* Record this location for future duplications.  */
      cfun->copyid_alloc->location_map->put (loc, next);

      return base;
    }
}

/* Free the copy_id allocator for a function.  Called when the function
   is being destroyed.  */

void
free_copyid_allocator (struct function *fn)
{
  if (fn && fn->copyid_alloc)
    {
      delete fn->copyid_alloc->location_map;
      XDELETE (fn->copyid_alloc);
      fn->copyid_alloc = NULL;
    }
}
