/* Loop optimizer initialization routines and RTL loop optimization passes.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "obstack.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "flags.h"
#include "df.h"
#include "ggc.h"
#include "tree-ssa-loop-niter.h"


/* Apply FLAGS to the loop state.  */

static void
apply_loop_flags (unsigned flags)
{
  if (flags & LOOPS_MAY_HAVE_MULTIPLE_LATCHES)
    {
      /* If the loops may have multiple latches, we cannot canonicalize
	 them further (and most of the loop manipulation functions will
	 not work).  However, we avoid modifying cfg, which some
	 passes may want.  */
      gcc_assert ((flags & ~(LOOPS_MAY_HAVE_MULTIPLE_LATCHES
			     | LOOPS_HAVE_RECORDED_EXITS)) == 0);
      loops_state_set (LOOPS_MAY_HAVE_MULTIPLE_LATCHES);
    }
  else
    disambiguate_loops_with_multiple_latches ();

  /* Create pre-headers.  */
  if (flags & LOOPS_HAVE_PREHEADERS)
    {
      int cp_flags = CP_SIMPLE_PREHEADERS;

      if (flags & LOOPS_HAVE_FALLTHRU_PREHEADERS)
        cp_flags |= CP_FALLTHRU_PREHEADERS;

      create_preheaders (cp_flags);
    }

  /* Force all latches to have only single successor.  */
  if (flags & LOOPS_HAVE_SIMPLE_LATCHES)
    force_single_succ_latches ();

  /* Mark irreducible loops.  */
  if (flags & LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS)
    mark_irreducible_loops ();

  if (flags & LOOPS_HAVE_RECORDED_EXITS)
    record_loop_exits ();
}

/* Initialize loop structures.  This is used by the tree and RTL loop
   optimizers.  FLAGS specify what properties to compute and/or ensure for
   loops.  */

void
loop_optimizer_init (unsigned flags)
{
  timevar_push (TV_LOOP_INIT);

  if (!current_loops)
    {
      gcc_assert (!(cfun->curr_properties & PROP_loops));

      /* Find the loops.  */
      current_loops = flow_loops_find (NULL);
    }
  else
    {
      bool recorded_exits = loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS);

      gcc_assert (cfun->curr_properties & PROP_loops);

      /* Ensure that the dominators are computed, like flow_loops_find does.  */
      calculate_dominance_info (CDI_DOMINATORS);

      if (loops_state_satisfies_p (LOOPS_NEED_FIXUP))
	{
	  loops_state_clear (~0U);
	  fix_loop_structure (NULL);
	}

#ifdef ENABLE_CHECKING
      else
	verify_loop_structure ();
#endif

      /* Clear all flags.  */
      if (recorded_exits)
	release_recorded_exits ();
      loops_state_clear (~0U);
    }

  /* Apply flags to loops.  */
  apply_loop_flags (flags);

  /* Dump loops.  */
  flow_loops_dump (dump_file, NULL, 1);

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
#endif

  timevar_pop (TV_LOOP_INIT);
}

/* Finalize loop structures.  */

void
loop_optimizer_finalize (void)
{
  struct loop *loop;
  basic_block bb;

  timevar_push (TV_LOOP_FINI);

  if (loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    release_recorded_exits ();

  free_numbers_of_iterations_estimates ();

  /* If we should preserve loop structure, do not free it but clear
     flags that advanced properties are there as we are not preserving
     that in full.  */
  if (cfun->curr_properties & PROP_loops)
    {
      loops_state_clear (LOOP_CLOSED_SSA
			 | LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS
			 | LOOPS_HAVE_PREHEADERS
			 | LOOPS_HAVE_SIMPLE_LATCHES
			 | LOOPS_HAVE_FALLTHRU_PREHEADERS);
      loops_state_set (LOOPS_MAY_HAVE_MULTIPLE_LATCHES);
      goto loop_fini_done;
    }

  gcc_assert (current_loops != NULL);

  FOR_EACH_LOOP (loop, 0)
    free_simple_loop_desc (loop);

  /* Clean up.  */
  flow_loops_free (current_loops);
  ggc_free (current_loops);
  current_loops = NULL;

  FOR_ALL_BB_FN (bb, cfun)
    {
      bb->loop_father = NULL;
    }

loop_fini_done:
  timevar_pop (TV_LOOP_FINI);
}

/* The structure of loops might have changed.  Some loops might get removed
   (and their headers and latches were set to NULL), loop exists might get
   removed (thus the loop nesting may be wrong), and some blocks and edges
   were changed (so the information about bb --> loop mapping does not have
   to be correct).  But still for the remaining loops the header dominates
   the latch, and loops did not get new subloops (new loops might possibly
   get created, but we are not interested in them).  Fix up the mess.

   If CHANGED_BBS is not NULL, basic blocks whose loop depth has changed are
   marked in it.

   Returns the number of new discovered loops.  */

unsigned
fix_loop_structure (bitmap changed_bbs)
{
  basic_block bb;
  int record_exits = 0;
  struct loop *loop;
  unsigned old_nloops, i;

  timevar_push (TV_LOOP_INIT);

  /* We need exact and fast dominance info to be available.  */
  gcc_assert (dom_info_state (CDI_DOMINATORS) == DOM_OK);

  if (loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    {
      release_recorded_exits ();
      record_exits = LOOPS_HAVE_RECORDED_EXITS;
    }

  /* Remember the depth of the blocks in the loop hierarchy, so that we can
     recognize blocks whose loop nesting relationship has changed.  */
  if (changed_bbs)
    FOR_EACH_BB_FN (bb, cfun)
      bb->aux = (void *) (size_t) loop_depth (bb->loop_father);

  /* Remove the dead loops from structures.  We start from the innermost
     loops, so that when we remove the loops, we know that the loops inside
     are preserved, and do not waste time relinking loops that will be
     removed later.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      /* Detect the case that the loop is no longer present even though
         it wasn't marked for removal.
	 ???  If we do that we can get away with not marking loops for
	 removal at all.  And possibly avoid some spurious removals.  */
      if (loop->header
	  && bb_loop_header_p (loop->header))
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "fix_loop_structure: removing loop %d\n",
		 loop->num);

      while (loop->inner)
	{
	  struct loop *ploop = loop->inner;
	  flow_loop_tree_node_remove (ploop);
	  flow_loop_tree_node_add (loop_outer (loop), ploop);
	}

      /* Remove the loop.  */
      loop->header = NULL;
      flow_loop_tree_node_remove (loop);
    }

  /* Remember the number of loops so we can return how many new loops
     flow_loops_find discovered.  */
  old_nloops = number_of_loops (cfun);

  /* Re-compute loop structure in-place.  */
  flow_loops_find (current_loops);

  /* Mark the blocks whose loop has changed.  */
  if (changed_bbs)
    {
      FOR_EACH_BB_FN (bb, cfun)
	{
	  if ((void *) (size_t) loop_depth (bb->loop_father) != bb->aux)
	    bitmap_set_bit (changed_bbs, bb->index);

    	  bb->aux = NULL;
	}
    }

  /* Finally free deleted loops.  */
  FOR_EACH_VEC_ELT (*get_loops (cfun), i, loop)
    if (loop && loop->header == NULL)
      {
	(*get_loops (cfun))[i] = NULL;
	flow_loop_free (loop);
      }

  loops_state_clear (LOOPS_NEED_FIXUP);

  /* Apply flags to loops.  */
  apply_loop_flags (current_loops->state | record_exits);

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
#endif

  timevar_pop (TV_LOOP_INIT);

  return number_of_loops (cfun) - old_nloops;
}

/* Gate for the RTL loop superpass.  The actual passes are subpasses.
   See passes.c for more on that.  */

static bool
gate_handle_loop2 (void)
{
  if (optimize > 0
      && (flag_move_loop_invariants
	  || flag_unswitch_loops
	  || flag_peel_loops
	  || flag_unroll_loops
#ifdef HAVE_doloop_end
	  || (flag_branch_on_count_reg && HAVE_doloop_end)
#endif
	 ))
    return true;
  else
    {
      /* No longer preserve loops, remove them now.  */
      cfun->curr_properties &= ~PROP_loops;
      if (current_loops)
	loop_optimizer_finalize ();
      return false;
    } 
}

namespace {

const pass_data pass_data_loop2 =
{
  RTL_PASS, /* type */
  "loop2", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  false, /* has_execute */
  TV_LOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop2 : public rtl_opt_pass
{
public:
  pass_loop2 (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_loop2, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_handle_loop2 (); }

}; // class pass_loop2

} // anon namespace

rtl_opt_pass *
make_pass_loop2 (gcc::context *ctxt)
{
  return new pass_loop2 (ctxt);
}


/* Initialization of the RTL loop passes.  */
static unsigned int
rtl_loop_init (void)
{
  gcc_assert (current_ir_type () == IR_RTL_CFGLAYOUT);

  if (dump_file)
    {
      dump_reg_info (dump_file);
      dump_flow_info (dump_file, dump_flags);
    }

  loop_optimizer_init (LOOPS_NORMAL);
  return 0;
}

namespace {

const pass_data pass_data_rtl_loop_init =
{
  RTL_PASS, /* type */
  "loop2_init", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_LOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_rtl_sharing, /* todo_flags_finish */
};

class pass_rtl_loop_init : public rtl_opt_pass
{
public:
  pass_rtl_loop_init (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_loop_init, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return rtl_loop_init (); }

}; // class pass_rtl_loop_init

} // anon namespace

rtl_opt_pass *
make_pass_rtl_loop_init (gcc::context *ctxt)
{
  return new pass_rtl_loop_init (ctxt);
}


/* Finalization of the RTL loop passes.  */

static unsigned int
rtl_loop_done (void)
{
  /* No longer preserve loops, remove them now.  */
  cfun->curr_properties &= ~PROP_loops;
  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);

  cleanup_cfg (0);
  if (dump_file)
    {
      dump_reg_info (dump_file);
      dump_flow_info (dump_file, dump_flags);
    }

  return 0;
}

namespace {

const pass_data pass_data_rtl_loop_done =
{
  RTL_PASS, /* type */
  "loop2_done", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_LOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  PROP_loops, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_flow | TODO_verify_rtl_sharing ), /* todo_flags_finish */
};

class pass_rtl_loop_done : public rtl_opt_pass
{
public:
  pass_rtl_loop_done (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_loop_done, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return rtl_loop_done (); }

}; // class pass_rtl_loop_done

} // anon namespace

rtl_opt_pass *
make_pass_rtl_loop_done (gcc::context *ctxt)
{
  return new pass_rtl_loop_done (ctxt);
}


/* Loop invariant code motion.  */
static bool
gate_rtl_move_loop_invariants (void)
{
  return flag_move_loop_invariants;
}

static unsigned int
rtl_move_loop_invariants (void)
{
  if (number_of_loops (cfun) > 1)
    move_loop_invariants ();
  return 0;
}

namespace {

const pass_data pass_data_rtl_move_loop_invariants =
{
  RTL_PASS, /* type */
  "loop2_invariant", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_LOOP_MOVE_INVARIANTS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_df_verify | TODO_df_finish
    | TODO_verify_rtl_sharing ), /* todo_flags_finish */
};

class pass_rtl_move_loop_invariants : public rtl_opt_pass
{
public:
  pass_rtl_move_loop_invariants (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_move_loop_invariants, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_rtl_move_loop_invariants (); }
  unsigned int execute () { return rtl_move_loop_invariants (); }

}; // class pass_rtl_move_loop_invariants

} // anon namespace

rtl_opt_pass *
make_pass_rtl_move_loop_invariants (gcc::context *ctxt)
{
  return new pass_rtl_move_loop_invariants (ctxt);
}


/* Loop unswitching for RTL.  */
static bool
gate_rtl_unswitch (void)
{
  return flag_unswitch_loops;
}

static unsigned int
rtl_unswitch (void)
{
  if (number_of_loops (cfun) > 1)
    unswitch_loops ();
  return 0;
}

namespace {

const pass_data pass_data_rtl_unswitch =
{
  RTL_PASS, /* type */
  "loop2_unswitch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_LOOP_UNSWITCH, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_rtl_sharing, /* todo_flags_finish */
};

class pass_rtl_unswitch : public rtl_opt_pass
{
public:
  pass_rtl_unswitch (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_unswitch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_rtl_unswitch (); }
  unsigned int execute () { return rtl_unswitch (); }

}; // class pass_rtl_unswitch

} // anon namespace

rtl_opt_pass *
make_pass_rtl_unswitch (gcc::context *ctxt)
{
  return new pass_rtl_unswitch (ctxt);
}


/* Loop unswitching for RTL.  */
static bool
gate_rtl_unroll_and_peel_loops (void)
{
  return (flag_peel_loops || flag_unroll_loops || flag_unroll_all_loops);
}

static unsigned int
rtl_unroll_and_peel_loops (void)
{
  if (number_of_loops (cfun) > 1)
    {
      int flags = 0;
      if (dump_file)
	df_dump (dump_file);

      if (flag_peel_loops)
	flags |= UAP_PEEL;
      if (flag_unroll_loops)
	flags |= UAP_UNROLL;
      if (flag_unroll_all_loops)
	flags |= UAP_UNROLL_ALL;

      unroll_and_peel_loops (flags);
    }
  return 0;
}

namespace {

const pass_data pass_data_rtl_unroll_and_peel_loops =
{
  RTL_PASS, /* type */
  "loop2_unroll", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_LOOP_UNROLL, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_rtl_sharing, /* todo_flags_finish */
};

class pass_rtl_unroll_and_peel_loops : public rtl_opt_pass
{
public:
  pass_rtl_unroll_and_peel_loops (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_unroll_and_peel_loops, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_rtl_unroll_and_peel_loops (); }
  unsigned int execute () { return rtl_unroll_and_peel_loops (); }

}; // class pass_rtl_unroll_and_peel_loops

} // anon namespace

rtl_opt_pass *
make_pass_rtl_unroll_and_peel_loops (gcc::context *ctxt)
{
  return new pass_rtl_unroll_and_peel_loops (ctxt);
}


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
  if (number_of_loops (cfun) > 1)
    doloop_optimize_loops ();
#endif
  return 0;
}

namespace {

const pass_data pass_data_rtl_doloop =
{
  RTL_PASS, /* type */
  "loop2_doloop", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_LOOP_DOLOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_rtl_sharing, /* todo_flags_finish */
};

class pass_rtl_doloop : public rtl_opt_pass
{
public:
  pass_rtl_doloop (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_doloop, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_rtl_doloop (); }
  unsigned int execute () { return rtl_doloop (); }

}; // class pass_rtl_doloop

} // anon namespace

rtl_opt_pass *
make_pass_rtl_doloop (gcc::context *ctxt)
{
  return new pass_rtl_doloop (ctxt);
}
