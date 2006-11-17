/* Loop optimizer initialization routines and RTL loop optimization passes.
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


/* Initialize loop structures.  This is used by the tree and RTL loop
   optimizers.  FLAGS specify what properties to compute and/or ensure for
   loops.  */

void
loop_optimizer_init (unsigned flags)
{
  edge e;
  edge_iterator ei;
  static bool first_time = true;
  struct loops *loops;

  if (first_time)
    {
      first_time = false;
      init_set_costs ();
    }

  gcc_assert (!current_loops);
  loops = XCNEW (struct loops);

  /* Avoid annoying special cases of edges going to exit
     block.  */

  for (ei = ei_start (EXIT_BLOCK_PTR->preds); (e = ei_safe_edge (ei)); )
    if ((e->flags & EDGE_FALLTHRU) && !single_succ_p (e->src))
      split_edge (e);
    else
      ei_next (&ei);

  /* Find the loops.  */

  flow_loops_find (loops);
  current_loops = loops;

  if (current_loops->num <= 1)
    {
      /* No loops.  */
      loop_optimizer_finalize ();
      return;
    }

  /* Create pre-headers.  */
  if (flags & LOOPS_HAVE_PREHEADERS)
    create_preheaders (current_loops, CP_SIMPLE_PREHEADERS);

  /* Force all latches to have only single successor.  */
  if (flags & LOOPS_HAVE_SIMPLE_LATCHES)
    force_single_succ_latches (current_loops);

  /* Mark irreducible loops.  */
  if (flags & LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS)
    mark_irreducible_loops (current_loops);

  if (flags & LOOPS_HAVE_MARKED_SINGLE_EXITS)
    mark_single_exit_loops (current_loops);

  /* Dump loops.  */
  flow_loops_dump (current_loops, dump_file, NULL, 1);

#ifdef ENABLE_CHECKING
  verify_dominators (CDI_DOMINATORS);
  verify_loop_structure (current_loops);
#endif
}

/* Finalize loop structures.  */

void
loop_optimizer_finalize (void)
{
  unsigned i;
  basic_block bb;

  if (!current_loops)
    return;

  for (i = 1; i < current_loops->num; i++)
    if (current_loops->parray[i])
      free_simple_loop_desc (current_loops->parray[i]);

  /* Clean up.  */
  flow_loops_free (current_loops);
  free (current_loops);
  current_loops = NULL;

  FOR_ALL_BB (bb)
    {
      bb->loop_father = NULL;
    }

  /* Checking.  */
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}


/* Gate for the RTL loop superpass.  The actual passes are subpasses.
   See passes.c for more on that.  */

static bool
gate_handle_loop2 (void)
{
  return (optimize > 0
  	  && (flag_move_loop_invariants
              || flag_unswitch_loops
              || flag_peel_loops
              || flag_unroll_loops
#ifdef HAVE_doloop_end
	      || (flag_branch_on_count_reg && HAVE_doloop_end)
#endif
	      ));
}

struct tree_opt_pass pass_loop2 =
{
  "loop2",                              /* name */
  gate_handle_loop2, 		        /* gate */
  NULL,                                 /* execute */
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


/* Initialization of the RTL loop passes.  */
static unsigned int
rtl_loop_init (void)
{
  if (dump_file)
    dump_flow_info (dump_file, dump_flags);

  /* Initialize structures for layout changes.  */
  cfg_layout_initialize (0);

  loop_optimizer_init (LOOPS_NORMAL);
  return 0;
}

struct tree_opt_pass pass_rtl_loop_init =
{
  "loop2_init",                           /* name */
  NULL,                                 /* gate */
  rtl_loop_init,                        /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
  'L'                                   /* letter */
};


/* Finalization of the RTL loop passes.  */

static unsigned int
rtl_loop_done (void)
{
  basic_block bb;

  loop_optimizer_finalize ();
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
    dump_flow_info (dump_file, dump_flags);

  return 0;
}

struct tree_opt_pass pass_rtl_loop_done =
{
  "loop2_done",                          /* name */
  NULL,                                 /* gate */
  rtl_loop_done,                        /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
  'L'                                   /* letter */
};


/* Loop invariant code motion.  */
static bool
gate_rtl_move_loop_invariants (void)
{
  return flag_move_loop_invariants;
}

static unsigned int
rtl_move_loop_invariants (void)
{
  if (current_loops)
    move_loop_invariants (current_loops);
  return 0;
}

struct tree_opt_pass pass_rtl_move_loop_invariants =
{
  "loop2_invariant",                     /* name */
  gate_rtl_move_loop_invariants,        /* gate */
  rtl_move_loop_invariants,             /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
  'L'                                   /* letter */
};


/* Loop unswitching for RTL.  */
static bool
gate_rtl_unswitch (void)
{
  return flag_unswitch_loops;
}

static unsigned int
rtl_unswitch (void)
{
  if (current_loops)
    unswitch_loops (current_loops);
  return 0;
}

struct tree_opt_pass pass_rtl_unswitch =
{
  "loop2_unswitch",                      /* name */
  gate_rtl_unswitch,                    /* gate */
  rtl_unswitch,                         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
  'L'                                   /* letter */
};


/* Loop unswitching for RTL.  */
static bool
gate_rtl_unroll_and_peel_loops (void)
{
  return (flag_peel_loops || flag_unroll_loops || flag_unroll_all_loops);
}

static unsigned int
rtl_unroll_and_peel_loops (void)
{
  if (current_loops)
    {
      int flags = 0;

      if (flag_peel_loops)
	flags |= UAP_PEEL;
      if (flag_unroll_loops)
	flags |= UAP_UNROLL;
      if (flag_unroll_all_loops)
	flags |= UAP_UNROLL_ALL;

      unroll_and_peel_loops (current_loops, flags);
    }
  return 0;
}

struct tree_opt_pass pass_rtl_unroll_and_peel_loops =
{
  "loop2_unroll",                        /* name */
  gate_rtl_unroll_and_peel_loops,       /* gate */
  rtl_unroll_and_peel_loops,            /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
  'L'                                   /* letter */
};


/* The doloop optimization.  */
static bool
gate_rtl_doloop (void)
{
#ifdef HAVE_doloop_end
  return (flag_branch_on_count_reg && HAVE_doloop_end);
#else
  return 0;
#endif
}

static unsigned int
rtl_doloop (void)
{
#ifdef HAVE_doloop_end
  if (current_loops)
    doloop_optimize_loops (current_loops);
#endif
  return 0;
}

struct tree_opt_pass pass_rtl_doloop =
{
  "loop2_doloop",                        /* name */
  gate_rtl_doloop,                      /* gate */
  rtl_doloop,                           /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_LOOP,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
  'L'                                   /* letter */
};

