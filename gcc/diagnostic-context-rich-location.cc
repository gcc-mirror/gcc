/* A rich_location subclass that lazily populates a diagnostic_path
   with diagnostic context events, but only if the path is actually to be
   used.

   Copyright (C) 2025-2026 Free Software Foundation, Inc.
   Contributed by Qing Zhao<qing.zhao@oracle.com>

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "cfganal.h"
#include "tree-cfg.h"
#include "simple-diagnostic-path.h"
#include "diagnostic-context-rich-location.h"

/* Implemenation of the method make_inner_path of the class
   lazy_diagnostic_context_path.  */

std::unique_ptr<diagnostics::paths::path>
lazy_diagnostic_context_path::make_inner_path () const
{
  auto path = std::make_unique<simple_diagnostic_path>
		(m_logical_loc_mgr,
		 global_dc->get_reference_printer ());
  if (!flag_diagnostics_show_context)
    return path;
  if (!m_stmt)
    return path;

  /* For the following more complicated code:
  if (i < 10)				// B2
    {
      if (is_day)			// B3
	__builtin_printf ("day");	// B4
      else
	__builtin_printf ("night");	// B5

      if (i == -1)			// B6
	{
	  if (is_dollar)		// B7
	    __builtin_printf ("dollar");// B8
	  else
	    __builtin_printf ("euro");	// B9
	  a[i] = -1;    		// B10 (warning here)
	}
      else
	a[i] = i;			// B11
    }
  else
    a[i] = i + 1;			// B12

  it has the following CFG:

	    B2
	   / \
	  V   \
	 B3    \
	/ \     \
       V   V     \
      B4  B5      V
	\ /      B12
	 V
	 B6
	/ \
       V   V
      B7  B11
     / \
    V   V
   B8   B9
     \ /
      V
     B10 (warning here)

  If the STMT that has warning is in B10, the interesting conditions for
  the diagnostic are:
  depth=1: the condition stmt at B6, the edge from B6->B7;
  depth=2: the condition stmt at B2, the edge from B2->B3;

  In order to get the interesting conditions, the key to the heuristic
  is to backtrace the immediate dominator block chain of the current
  block B10, such as, B7, then B6, B3, B2.

  In this basic block dominator chain, identify the single predecessor
  edges, such as B6->B7, and B2->B3.

  For each of the single predecessor edge, the interesting condition is
  embedded in the single predecessor block, and the TRUE/FALSE of this
  condition is embedded in the edge.

  FIXME: We currently only handle GIMPLE_COND, might extend to GIMPLE_SWITCH
  later.  */

  basic_block cur_bb = gimple_bb (m_stmt);
  if (!cur_bb)
    return path;
  basic_block cond_bb = NULL;
  int depth = 0;
  do
    {
      /* Step 1. locate the immediate dominator of cur_bb.  */
      if (dom_info_available_p (cfun, CDI_DOMINATORS))
	cond_bb = get_immediate_dominator (CDI_DOMINATORS, cur_bb);

      if (!cond_bb)
	return path;

      /* Step 2. identify the single predecessor edge to locate the
	 conditional statement.  */
      if (single_pred_p (cur_bb))
	{
	  gcc_assert (cond_bb == single_pred (cur_bb));
	  gimple *cond_stmt = NULL;
	  gimple_stmt_iterator gsi = gsi_last_bb (cond_bb);

	  /* Currently, we only hanlde GIMPLE_COND.
	     FIXME, will handle GIMPLE_SWITCH and other ctrl stmt later.  */
	  if (gsi_stmt (gsi) && stmt_ends_bb_p (gsi_stmt (gsi)))
	    if (gimple_code (gsi_stmt (gsi)) == GIMPLE_COND)
	      cond_stmt = gsi_stmt (gsi);

	  /* If there is no conditional statement in the cond_bb, continue.  */
	  if (!cond_stmt)
	    {
	      cur_bb = cond_bb;
	      continue;
	    }

	  depth++;

	  /* if there is no location information for the cond_stmt, we should
	     not add this event to confuse end user.  */
	  if (cond_stmt
	      && LOCATION_LOCUS (gimple_location (cond_stmt))
		 != UNKNOWN_LOCATION)
	    {
	      /* Get the edge from the cond_bb to cur_bb, to determine whether
		 the stmt is on the taken path of the conditional statement.  */
	      edge e = find_edge (cond_bb, cur_bb);
	      bool is_branch_taken = e->flags & EDGE_TRUE_VALUE;
	      path->add_event (gimple_location (cond_stmt), cfun->decl, 1,
			       "when the condition is evaluated to %s",
			       is_branch_taken ? "true" : "false");
	    }
	}
	cur_bb = cond_bb;
    }
  while (cond_bb != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	 && depth < flag_diagnostics_show_context);

  /* Add an end of path warning event in the end of the path.  */
  if (path->num_events () > 0)
    path->add_event (m_location, cfun->decl, 1,
		     "warning happens here");
  return path;
}
