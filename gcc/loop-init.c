/* Loop optimizer initialization routines.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "cfglayout.h"

/* Initialize loop optimizer.  */

struct loops *
loop_optimizer_init (FILE *dumpfile)
{
  struct loops *loops = xcalloc (1, sizeof (struct loops));
  edge e;

  /* Initialize structures for layout changes.  */
  cfg_layout_initialize ();

  /* Avoid annoying special cases of edges going to exit
     block.  */
  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
    if ((e->flags & EDGE_FALLTHRU) && e->src->succ->succ_next)
      split_edge (e);

  /* Find the loops.  */

  if (flow_loops_find (loops, LOOP_TREE) <= 1)
    {
      basic_block bb;

      /* No loops.  */
      flow_loops_free (loops);
      free_dominance_info (CDI_DOMINATORS);
      free (loops);

      /* Make chain.  */
      FOR_EACH_BB (bb)
	if (bb->next_bb != EXIT_BLOCK_PTR)
	  bb->rbi->next = bb->next_bb;
	  cfg_layout_finalize ();
      return NULL;
    }

  /* Not going to update these.  */
  free (loops->cfg.rc_order);
  loops->cfg.rc_order = NULL;
  free (loops->cfg.dfs_order);
  loops->cfg.dfs_order = NULL;

  /* Create pre-headers.  */
  create_preheaders (loops, CP_SIMPLE_PREHEADERS);

  /* Force all latches to have only single successor.  */
  force_single_succ_latches (loops);

  /* Mark irreducible loops.  */
  mark_irreducible_loops (loops);

  /* Dump loops.  */
  flow_loops_dump (loops, dumpfile, NULL, 1);

#ifdef ENABLE_CHECKING
  verify_dominators (CDI_DOMINATORS);
  verify_loop_structure (loops);
#endif

  return loops;
}

/* Finalize loop optimizer.  */
void
loop_optimizer_finalize (struct loops *loops, FILE *dumpfile)
{
  basic_block bb;

  /* Finalize layout changes.  */
  /* Make chain.  */
  FOR_EACH_BB (bb)
    if (bb->next_bb != EXIT_BLOCK_PTR)
      bb->rbi->next = bb->next_bb;

  /* Another dump.  */
  flow_loops_dump (loops, dumpfile, NULL, 1);

  /* Clean up.  */
  flow_loops_free (loops);
  free_dominance_info (CDI_DOMINATORS);
  free (loops);

  /* Finalize changes.  */
  cfg_layout_finalize ();

  /* Checking.  */
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}
