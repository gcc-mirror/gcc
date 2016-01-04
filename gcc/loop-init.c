/* Loop optimizer initialization routines and RTL loop optimization passes.
   Copyright (C) 2002-2016 Free Software Foundation, Inc.

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
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "regs.h"
#include "cfgcleanup.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "tree-ssa-loop-niter.h"
#include "loop-unroll.h"
#include "tree-scalar-evolution.h"


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
      bool needs_fixup = loops_state_satisfies_p (LOOPS_NEED_FIXUP);

      gcc_assert (cfun->curr_properties & PROP_loops);

      /* Ensure that the dominators are computed, like flow_loops_find does.  */
      calculate_dominance_info (CDI_DOMINATORS);

      if (!needs_fixup)
	checking_verify_loop_structure ();

      /* Clear all flags.  */
      if (recorded_exits)
	release_recorded_exits (cfun);
      loops_state_clear (~0U);

      if (needs_fixup)
	{
	  /* Apply LOOPS_MAY_HAVE_MULTIPLE_LATCHES early as fix_loop_structure
	     re-applies flags.  */
	  loops_state_set (flags & LOOPS_MAY_HAVE_MULTIPLE_LATCHES);
	  fix_loop_structure (NULL);
	}
    }

  /* Apply flags to loops.  */
  apply_loop_flags (flags);

  /* Dump loops.  */
  flow_loops_dump (dump_file, NULL, 1);

  checking_verify_loop_structure ();

  timevar_pop (TV_LOOP_INIT);
}

/* Finalize loop structures.  */

void
loop_optimizer_finalize (struct function *fn)
{
  struct loop *loop;
  basic_block bb;

  timevar_push (TV_LOOP_FINI);

  if (loops_state_satisfies_p (fn, LOOPS_HAVE_RECORDED_EXITS))
    release_recorded_exits (fn);

  free_numbers_of_iterations_estimates (fn);

  /* If we should preserve loop structure, do not free it but clear
     flags that advanced properties are there as we are not preserving
     that in full.  */
  if (fn->curr_properties & PROP_loops)
    {
      loops_state_clear (fn, LOOP_CLOSED_SSA
			 | LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS
			 | LOOPS_HAVE_PREHEADERS
			 | LOOPS_HAVE_SIMPLE_LATCHES
			 | LOOPS_HAVE_FALLTHRU_PREHEADERS);
      loops_state_set (fn, LOOPS_MAY_HAVE_MULTIPLE_LATCHES);
      goto loop_fini_done;
    }

  FOR_EACH_LOOP_FN (fn, loop, 0)
    free_simple_loop_desc (loop);

  /* Clean up.  */
  flow_loops_free (loops_for_fn (fn));
  ggc_free (loops_for_fn (fn));
  set_loops_for_fn (fn, NULL);

  FOR_ALL_BB_FN (bb, fn)
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

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "fix_loop_structure: fixing up loops for function\n");

  /* We need exact and fast dominance info to be available.  */
  gcc_assert (dom_info_state (CDI_DOMINATORS) == DOM_OK);

  if (loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    {
      release_recorded_exits (cfun);
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
      if (loop->header)
	loop->former_header = loop->header;
      else
	gcc_assert (loop->former_header != NULL);
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
  bool any_deleted = false;
  FOR_EACH_VEC_ELT (*get_loops (cfun), i, loop)
    if (loop && loop->header == NULL)
      {
	if (dump_file
	    && ((unsigned) loop->former_header->index
		< basic_block_info_for_fn (cfun)->length ()))
	  {
	    basic_block former_header
	      = BASIC_BLOCK_FOR_FN (cfun, loop->former_header->index);
	    /* If the old header still exists we want to check if the
	       original loop is re-discovered or the old header is now
	       part of a newly discovered loop.
	       In both cases we should have avoided removing the loop.  */
	    if (former_header == loop->former_header)
	      {
		if (former_header->loop_father->header == former_header)
		  fprintf (dump_file, "fix_loop_structure: rediscovered "
			   "removed loop %d as loop %d with old header %d\n",
			   loop->num, former_header->loop_father->num,
			   former_header->index);
		else if ((unsigned) former_header->loop_father->num
			 >= old_nloops)
		  fprintf (dump_file, "fix_loop_structure: header %d of "
			   "removed loop %d is part of the newly "
			   "discovered loop %d with header %d\n",
			   former_header->index, loop->num,
			   former_header->loop_father->num,
			   former_header->loop_father->header->index);
	      }
	  }
	(*get_loops (cfun))[i] = NULL;
	flow_loop_free (loop);
	any_deleted = true;
      }

  /* If we deleted loops then the cached scalar evolutions refering to
     those loops become invalid.  */
  if (any_deleted && scev_initialized_p ())
    scev_reset_htab ();

  loops_state_clear (LOOPS_NEED_FIXUP);

  /* Apply flags to loops.  */
  apply_loop_flags (current_loops->state | record_exits);

  checking_verify_loop_structure ();

  timevar_pop (TV_LOOP_INIT);

  return number_of_loops (cfun) - old_nloops;
}

/* The RTL loop superpass.  The actual passes are subpasses.  See passes.c for
   more on that.  */

namespace {

const pass_data pass_data_loop2 =
{
  RTL_PASS, /* type */
  "loop2", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
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
  virtual bool gate (function *);

}; // class pass_loop2

bool
pass_loop2::gate (function *fun)
{
  if (optimize > 0
      && (flag_move_loop_invariants
	  || flag_unswitch_loops
	  || flag_unroll_loops
	  || (flag_branch_on_count_reg
	      && targetm.have_doloop_end ())))
    return true;
  else
    {
      /* No longer preserve loops, remove them now.  */
      fun->curr_properties &= ~PROP_loops;
      if (current_loops)
	loop_optimizer_finalize ();
      return false;
    } 
}

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

  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  return 0;
}

namespace {

const pass_data pass_data_rtl_loop_init =
{
  RTL_PASS, /* type */
  "loop2_init", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rtl_loop_init : public rtl_opt_pass
{
public:
  pass_rtl_loop_init (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_loop_init, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return rtl_loop_init (); }

}; // class pass_rtl_loop_init

} // anon namespace

rtl_opt_pass *
make_pass_rtl_loop_init (gcc::context *ctxt)
{
  return new pass_rtl_loop_init (ctxt);
}


/* Finalization of the RTL loop passes.  */

namespace {

const pass_data pass_data_rtl_loop_done =
{
  RTL_PASS, /* type */
  "loop2_done", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  PROP_loops, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rtl_loop_done : public rtl_opt_pass
{
public:
  pass_rtl_loop_done (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_loop_done, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_rtl_loop_done

unsigned int
pass_rtl_loop_done::execute (function *fun)
{
  /* No longer preserve loops, remove them now.  */
  fun->curr_properties &= ~PROP_loops;
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

} // anon namespace

rtl_opt_pass *
make_pass_rtl_loop_done (gcc::context *ctxt)
{
  return new pass_rtl_loop_done (ctxt);
}


/* Loop invariant code motion.  */

namespace {

const pass_data pass_data_rtl_move_loop_invariants =
{
  RTL_PASS, /* type */
  "loop2_invariant", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_MOVE_INVARIANTS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_df_verify | TODO_df_finish ), /* todo_flags_finish */
};

class pass_rtl_move_loop_invariants : public rtl_opt_pass
{
public:
  pass_rtl_move_loop_invariants (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_move_loop_invariants, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_move_loop_invariants; }
  virtual unsigned int execute (function *fun)
    {
      if (number_of_loops (fun) > 1)
	move_loop_invariants ();
      return 0;
    }

}; // class pass_rtl_move_loop_invariants

} // anon namespace

rtl_opt_pass *
make_pass_rtl_move_loop_invariants (gcc::context *ctxt)
{
  return new pass_rtl_move_loop_invariants (ctxt);
}


namespace {

const pass_data pass_data_rtl_unroll_loops =
{
  RTL_PASS, /* type */
  "loop2_unroll", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_UNROLL, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rtl_unroll_loops : public rtl_opt_pass
{
public:
  pass_rtl_unroll_loops (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_unroll_loops, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (flag_peel_loops || flag_unroll_loops || flag_unroll_all_loops);
    }

  virtual unsigned int execute (function *);

}; // class pass_rtl_unroll_loops

unsigned int
pass_rtl_unroll_loops::execute (function *fun)
{
  if (number_of_loops (fun) > 1)
    {
      int flags = 0;
      if (dump_file)
	df_dump (dump_file);

      if (flag_unroll_loops)
	flags |= UAP_UNROLL;
      if (flag_unroll_all_loops)
	flags |= UAP_UNROLL_ALL;

      unroll_loops (flags);
    }
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_rtl_unroll_loops (gcc::context *ctxt)
{
  return new pass_rtl_unroll_loops (ctxt);
}


namespace {

const pass_data pass_data_rtl_doloop =
{
  RTL_PASS, /* type */
  "loop2_doloop", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_DOLOOP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rtl_doloop : public rtl_opt_pass
{
public:
  pass_rtl_doloop (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_doloop, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

}; // class pass_rtl_doloop

bool
pass_rtl_doloop::gate (function *)
{
  return (flag_branch_on_count_reg && targetm.have_doloop_end ());
}

unsigned int
pass_rtl_doloop::execute (function *fun)
{
  if (number_of_loops (fun) > 1)
    doloop_optimize_loops ();
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_rtl_doloop (gcc::context *ctxt)
{
  return new pass_rtl_doloop (ctxt);
}
