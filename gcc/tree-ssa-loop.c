/* Loop optimizations over tree-ssa.
   Copyright (C) 2003 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "cfgloop.h"
#include "flags.h"
#include "tree-inline.h"

/* The loop tree currently optimized.  */

struct loops *current_loops;

/* Initializes the loop structures.  DUMP is the file to that the details
   about the analysis should be dumped.  */

static struct loops *
tree_loop_optimizer_init (FILE *dump)
{
  struct loops *loops = loop_optimizer_init (dump);

  if (!loops)
    return NULL;

  /* Creation of preheaders may create redundant phi nodes if the loop is
     entered by more than one edge, but the initial value of the induction
     variable is the same on all of them.  */
  kill_redundant_phi_nodes ();
  rewrite_into_ssa (false);
  bitmap_clear (vars_to_rename);

  return loops;
}

/* The loop superpass.  */

static bool
gate_loop (void)
{
  return flag_tree_loop_optimize != 0;
}

struct tree_opt_pass pass_loop = 
{
  "loop",				/* name */
  gate_loop,				/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  TODO_ggc_collect,			/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa | TODO_ggc_collect	/* todo_flags_finish */
};

/* Loop optimizer initialization.  */

static void
tree_ssa_loop_init (void)
{
  current_loops = tree_loop_optimizer_init (dump_file);
}
  
struct tree_opt_pass pass_loop_init = 
{
  "loopinit",				/* name */
  NULL,					/* gate */
  tree_ssa_loop_init,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};

/* Loop optimizer finalization.  */

static void
tree_ssa_loop_done (void)
{
  if (!current_loops)
    return;

  loop_optimizer_finalize (current_loops,
			   (dump_flags & TDF_DETAILS ? dump_file : NULL));
  current_loops = NULL;
  cleanup_tree_cfg ();
}
  
struct tree_opt_pass pass_loop_done = 
{
  "loopdone",				/* name */
  NULL,					/* gate */
  tree_ssa_loop_done,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};

