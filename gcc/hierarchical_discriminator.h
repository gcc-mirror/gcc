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

#ifndef GCC_HIERARCHICAL_DISCRIMINATOR_H
#define GCC_HIERARCHICAL_DISCRIMINATOR_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gimple.h"
#include "tree.h"
#include "basic-block.h"
#include "input.h"

/* Hierarchical discriminator layout (32 bits total):
   Discriminator format: [Base:8][Multiplicity:7][CopyID:11][Unused:6]
   - Base: bits 0-7 (8 bits, 0-255)
   - Multiplicity: bits 8-14 (7 bits, 0-127)
   - CopyID: bits 15-25 (11 bits, 0-2047)
   - Unused: bits 26-31 (6 bits, reserved)

   Base discriminator: Used by front-end and early passes to distinguish
		       different statements on the same source line.

   Multiplicity: Represents when a single IR statement corresponds to
		 multiple scalar iterations or executions

   CopyID: Unique identifier for distinct code copies to distinguish
 */


/* Helper function to assign discriminators to a statement.
   This preserves the base discriminator and updates multiplicity
   and/or copyid.  */
extern void assign_discriminators_to_stmt (gimple* stmt,
					   unsigned int multiplicity_factor,
					   unsigned int copyid);

/* Helper function to assign discriminators to all statements in a basic
   block.  This preserves the base discriminator and updates multiplicity
   and/or copyid.  PHI statements, PHI arguments, and edge locations are
   also updated.  */
extern void assign_discriminators_to_bb (basic_block bb,
					 unsigned int multiplicity_factor,
					 unsigned int copyid);

/* Helper function to assign discriminators to all basic blocks in a loop.
   This is used by loop versioning passes to distinguish different versions
   of the same loop and to indicate vectorization factors.  */
extern void assign_discriminators_to_loop (class loop *loop,
					   unsigned int multiplicity_factor,
					   unsigned int copyid);

/* Copy ID allocator for tracking unique copy_id assignments per location.
   This ensures that nested code duplication (e.g., unroll + vectorize) gets
   unique copy_ids even when the same location is duplicated
   multiple times.  */

struct copyid_allocator
{
  /* Hash map from location to the next available copy_id base.
     Key: location_t, Value: unsigned int (next available base).  */
  hash_map<int_hash<location_t, UNKNOWN_LOCATION, UNKNOWN_LOCATION>,
	   unsigned int> *location_map;

  /* Whether the allocator has been initialized.  */
  bool initialized;
};

/* Allocate a unique copy_id base for the given location.
   STRIDE indicates how many copy_ids to reserve (for unrolling N times,
   use stride=N).  Returns the base copy_id.  */
extern unsigned int allocate_copyid_base (location_t loc, unsigned int stride);

/* Free the copy_id allocator for a function.  */
extern void free_copyid_allocator (struct function *fn);

#endif /* GCC_HIERARCHICAL_DISCRIMINATOR_H.  */
