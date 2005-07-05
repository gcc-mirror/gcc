/* Loop optimizer initialization routines.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "tree-pass.h"
#include "timevar.h"
#include "flags.h"

/* Initialize loop optimizer.  */

struct loops *
loop_optimizer_init (FILE *dumpfile)
{
  struct loops *loops = xcalloc (1, sizeof (struct loops));
  edge e;
  edge_iterator ei;
  static bool first_time = true;

  if (first_time)
    {
      first_time = false;
      init_set_costs ();
    }

  /* Avoid annoying special cases of edges going to exit
     block.  */

  for (ei = ei_start (EXIT_BLOCK_PTR->preds); (e = ei_safe_edge (ei)); )
    if ((e->flags & EDGE_FALLTHRU) && !single_succ_p (e->src))
      split_edge (e);
    else
      ei_next (&ei);

  /* Find the loops.  */

  if (flow_loops_find (loops) <= 1)
    {
      /* No loops.  */
      flow_loops_free (loops);
      free (loops);

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
  unsigned i;

  if (!loops)
    return;

  for (i = 1; i < loops->num; i++)
    if (loops->parray[i])
      free_simple_loop_desc (loops->parray[i]);

  /* Another dump.  */
  flow_loops_dump (loops, dumpfile, NULL, 1);

  /* Clean up.  */
  flow_loops_free (loops);
  free (loops);

  /* Checking.  */
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

static bool
gate_handle_loop2 (void)
{
  return (optimize > 0 && flag_loop_optimize2
  	  && (flag_move_loop_invariants
              || flag_unswitch_loops
              || flag_peel_loops
              || flag_unroll_loops
              || flag_branch_on_count_reg));
}

/* Perform loop optimizations.  It might be better to do them a bit
   sooner, but we want the profile feedback to work more
   efficiently.  */
static void
rest_of_handle_loop2 (void)
{
  struct loops *loops;
  basic_block bb;

  if (dump_file)
    dump_flow_info (dump_file);

  /* Initialize structures for layout changes.  */
  cfg_layout_initialize (0);

  loops = loop_optimizer_init (dump_file);

  if (loops)
    {
      /* The optimizations:  */
      if (flag_move_loop_invariants)
        move_loop_invariants (loops);

      if (flag_unswitch_loops)
        unswitch_loops (loops);

      if (flag_peel_loops || flag_unroll_loops)
        unroll_and_peel_loops (loops,
                               (flag_peel_loops ? UAP_PEEL : 0) |
                               (flag_unroll_loops ? UAP_UNROLL : 0) |
                               (flag_unroll_all_loops ? UAP_UNROLL_ALL : 0));

#ifdef HAVE_doloop_end
      if (flag_branch_on_count_reg && HAVE_doloop_end)
        doloop_optimize_loops (loops);
#endif /* HAVE_doloop_end */

      loop_optimizer_finalize (loops, dump_file);
    }

  free_dominance_info (CDI_DOMINATORS);

  /* Finalize layout changes.  */
  FOR_EACH_BB (bb)
    if (bb->next_bb != EXIT_BLOCK_PTR)
      bb->aux = bb->next_bb;
  cfg_layout_finalize ();

  cleanup_cfg (CLEANUP_EXPENSIVE);
  delete_trivially_dead_insns (get_insns (), max_reg_num ());
  reg_scan (get_insns (), max_reg_num ());
  if (dump_file)
    dump_flow_info (dump_file);
}

struct tree_opt_pass pass_loop2 =
{
  "loop2",                              /* name */
  gate_handle_loop2, 		        /* gate */
  rest_of_handle_loop2,      		/* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_ggc_collect,                     /* todo_flags_finish */
  'L'                                   /* letter */
};

