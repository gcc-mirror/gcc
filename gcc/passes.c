/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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

/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include <signal.h>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#include "line-map.h"
#include "input.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "insn-attr.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "output.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "expr.h"
#include "basic-block.h"
#include "intl.h"
#include "ggc.h"
#include "graph.h"
#include "regs.h"
#include "timevar.h"
#include "diagnostic.h"
#include "params.h"
#include "reload.h"
#include "dwarf2asm.h"
#include "integrate.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "cfgloop.h"
#include "hosthooks.h"
#include "cgraph.h"
#include "opts.h"
#include "coverage.h"
#include "value-prof.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "tree-dump.h"

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
#include "dwarf2out.h"
#endif

#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
#include "dbxout.h"
#endif

#ifdef SDB_DEBUGGING_INFO
#include "sdbout.h"
#endif

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data
				   declarations for e.g. AIX 4.x.  */
#endif

#ifndef HAVE_conditional_execution
#define HAVE_conditional_execution 0
#endif

/* Format to use to print dumpfile index value */
#ifndef DUMPFILE_FORMAT
#define DUMPFILE_FORMAT ".%02d."
#endif

static int initializing_dump = 0;

/* Routine to open a dump file.  Return true if the dump file is enabled.  */

static int
open_dump_file (enum tree_dump_index index, tree decl)
{
  if (! dump_enabled_p (index))
    return 0;

  timevar_push (TV_DUMP);

  if (dump_file != NULL || dump_file_name != NULL)
    abort ();

  dump_file_name = get_dump_file_name (index);
  initializing_dump = !dump_initialized_p (index);
  dump_file = dump_begin (index, NULL);

  if (dump_file == NULL)
    fatal_error ("can't open %s: %m", dump_file_name);

  if (decl)
    fprintf (dump_file, "\n;; Function %s%s\n\n",
	     lang_hooks.decl_printable_name (decl, 2),
	     cfun->function_frequency == FUNCTION_FREQUENCY_HOT
	     ? " (hot)"
	     : cfun->function_frequency == FUNCTION_FREQUENCY_UNLIKELY_EXECUTED
	     ? " (unlikely executed)"
	     : "");

  timevar_pop (TV_DUMP);
  return 1;
}

/* Routine to close a dump file.  */

static void
close_dump_file (enum tree_dump_index index,
		 void (*func) (FILE *, rtx),
		 rtx insns)
{
  if (! dump_file)
    return;

  timevar_push (TV_DUMP);
  if (insns
      && graph_dump_format != no_graph)
    {
      /* If we've not initialized the files, do so now.  */
      if (initializing_dump)
	clean_graph_dump_file (dump_file_name);

      print_rtl_graph_with_bb (dump_file_name, insns);
    }

  if (func && insns)
    func (dump_file, insns);

  dump_end (index, dump_file);
  free ((char *) dump_file_name);

  dump_file = NULL;
  dump_file_name = NULL;
  timevar_pop (TV_DUMP);
}

/* This is called from various places for FUNCTION_DECL, VAR_DECL,
   and TYPE_DECL nodes.

   This does nothing for local (non-static) variables, unless the
   variable is a register variable with DECL_ASSEMBLER_NAME set.  In
   that case, or if the variable is not an automatic, it sets up the
   RTL and outputs any assembler code (label definition, storage
   allocation and initialization).

   DECL is the declaration.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (tree decl,
			  int top_level,
			  int at_end)
{
  /* We deferred calling assemble_alias so that we could collect
     other attributes such as visibility.  Emit the alias now.  */
  {
    tree alias;
    alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl));
    if (alias)
      {
	alias = TREE_VALUE (TREE_VALUE (alias));
	alias = get_identifier (TREE_STRING_POINTER (alias));
	assemble_alias (decl, alias);
      }
  }

  /* Can't defer this, because it needs to happen before any
     later function definitions are processed.  */
  if (DECL_REGISTER (decl) && DECL_ASSEMBLER_NAME_SET_P (decl))
    make_decl_rtl (decl);

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    {
      timevar_push (TV_VARCONST);

      /* Don't output anything when a tentative file-scope definition
	 is seen.  But at end of compilation, do output code for them.

	 We do output all variables when unit-at-a-time is active and rely on
	 callgraph code to defer them except for forward declarations
	 (see gcc.c-torture/compile/920624-1.c) */
      if ((at_end
	   || !DECL_DEFER_OUTPUT (decl)
	   || (flag_unit_at_a_time && DECL_INITIAL (decl)))
	  && !DECL_EXTERNAL (decl))
	{
	  if (flag_unit_at_a_time && !cgraph_global_info_ready
	      && TREE_CODE (decl) != FUNCTION_DECL && top_level)
	    cgraph_varpool_finalize_decl (decl);
	  else
	    assemble_variable (decl, top_level, at_end, 0);
	}

#ifdef ASM_FINISH_DECLARE_OBJECT
      if (decl == last_assemble_variable_decl)
	{
	  ASM_FINISH_DECLARE_OBJECT (asm_out_file, decl,
				     top_level, at_end);
	}
#endif

      timevar_pop (TV_VARCONST);
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      timevar_push (TV_SYMOUT);
      debug_hooks->type_decl (decl, !top_level);
      timevar_pop (TV_SYMOUT);
    }

  /* Let cgraph know about the existance of variables.  */
  if (TREE_CODE (decl) == VAR_DECL && !DECL_EXTERNAL (decl))
    cgraph_varpool_node (decl);
}

/* Called after finishing a record, union or enumeral type.  */

void
rest_of_type_compilation (tree type, int toplev)
{
  /* Avoid confusing the debug information machinery when there are
     errors.  */
  if (errorcount != 0 || sorrycount != 0)
    return;

  timevar_push (TV_SYMOUT);
  debug_hooks->type_decl (TYPE_STUB_DECL (type), !toplev);
  timevar_pop (TV_SYMOUT);
}

/* Turn the RTL into assembly.  */
static void
rest_of_handle_final (void)
{
  timevar_push (TV_FINAL);
  {
    rtx x;
    const char *fnname;

    /* Get the function's name, as described by its RTL.  This may be
       different from the DECL_NAME name used in the source file.  */

    x = DECL_RTL (current_function_decl);
    if (!MEM_P (x))
      abort ();
    x = XEXP (x, 0);
    if (GET_CODE (x) != SYMBOL_REF)
      abort ();
    fnname = XSTR (x, 0);

    assemble_start_function (current_function_decl, fnname);
    final_start_function (get_insns (), asm_out_file, optimize);
    final (get_insns (), asm_out_file, optimize, 0);
    final_end_function ();

#ifdef TARGET_UNWIND_INFO
    /* ??? The IA-64 ".handlerdata" directive must be issued before
       the ".endp" directive that closes the procedure descriptor.  */
    output_function_exception_table ();
#endif

    assemble_end_function (current_function_decl, fnname);

#ifndef TARGET_UNWIND_INFO
    /* Otherwise, it feels unclean to switch sections in the middle.  */
    output_function_exception_table ();
#endif

    user_defined_section_attribute = false;

    if (! quiet_flag)
      fflush (asm_out_file);

    /* Release all memory allocated by flow.  */
    free_basic_block_vars ();
  }

  /* Write DBX symbols if requested.  */

  /* Note that for those inline functions where we don't initially
     know for certain that we will be generating an out-of-line copy,
     the first invocation of this routine (rest_of_compilation) will
     skip over this code by doing a `goto exit_rest_of_compilation;'.
     Later on, wrapup_global_declarations will (indirectly) call
     rest_of_compilation again for those inline functions that need
     to have out-of-line copies generated.  During that call, we
     *will* be routed past here.  */

  timevar_push (TV_SYMOUT);
  (*debug_hooks->function_decl) (current_function_decl);
  timevar_pop (TV_SYMOUT);

  ggc_collect ();
  timevar_pop (TV_FINAL);
}

#ifdef DELAY_SLOTS
/* Run delay slot optimization.  */
static void
rest_of_handle_delay_slots (void)
{
  timevar_push (TV_DBR_SCHED);
  open_dump_file (DFI_dbr, current_function_decl);

  dbr_schedule (get_insns (), dump_file);

  close_dump_file (DFI_dbr, print_rtl, get_insns ());

  ggc_collect ();

  timevar_pop (TV_DBR_SCHED);
}
#endif

#ifdef STACK_REGS
/* Convert register usage from flat register file usage to a stack
   register file.  */
static void
rest_of_handle_stack_regs (void)
{
#if defined (HAVE_ATTR_length)
  /* If flow2 creates new instructions which need splitting
     and scheduling after reload is not done, they might not be
     split until final which doesn't allow splitting
     if HAVE_ATTR_length.  */
#ifdef INSN_SCHEDULING
  if (optimize && !flag_schedule_insns_after_reload)
#else
  if (optimize)
#endif
    {
      timevar_push (TV_SHORTEN_BRANCH);
      split_all_insns (1);
      timevar_pop (TV_SHORTEN_BRANCH);
    }
#endif

  timevar_push (TV_REG_STACK);
  open_dump_file (DFI_stack, current_function_decl);

  if (reg_to_stack (dump_file) && optimize)
    {
      if (cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK
		       | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0))
	  && (flag_reorder_blocks || flag_reorder_blocks_and_partition))
	{
	  reorder_basic_blocks (0);
	  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK);
	}
    }

  close_dump_file (DFI_stack, print_rtl_with_bb, get_insns ());

  ggc_collect ();
  timevar_pop (TV_REG_STACK);
}
#endif

/* Track the variables, i.e. compute where the variable is stored at each position in function.  */
static void
rest_of_handle_variable_tracking (void)
{
  timevar_push (TV_VAR_TRACKING);
  open_dump_file (DFI_vartrack, current_function_decl);

  variable_tracking_main ();

  close_dump_file (DFI_vartrack, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_VAR_TRACKING);
}

/* Machine dependent reorg pass.  */
static void
rest_of_handle_machine_reorg (void)
{
  timevar_push (TV_MACH_DEP);
  open_dump_file (DFI_mach, current_function_decl);

  targetm.machine_dependent_reorg ();

  close_dump_file (DFI_mach, print_rtl, get_insns ());

  ggc_collect ();
  timevar_pop (TV_MACH_DEP);
}


/* Run old register allocator.  Return TRUE if we must exit
   rest_of_compilation upon return.  */
static bool
rest_of_handle_old_regalloc (void)
{
  int failure;
  int rebuild_notes;

  timevar_push (TV_LOCAL_ALLOC);
  open_dump_file (DFI_lreg, current_function_decl);

  /* Allocate the reg_renumber array.  */
  allocate_reg_info (max_regno, FALSE, TRUE);

  /* And the reg_equiv_memory_loc array.  */
  VARRAY_GROW (reg_equiv_memory_loc_varray, max_regno);
  reg_equiv_memory_loc = &VARRAY_RTX (reg_equiv_memory_loc_varray, 0);

  allocate_initial_values (reg_equiv_memory_loc);

  regclass (get_insns (), max_reg_num (), dump_file);
  rebuild_notes = local_alloc ();

  timevar_pop (TV_LOCAL_ALLOC);

  /* Local allocation may have turned an indirect jump into a direct
     jump.  If so, we must rebuild the JUMP_LABEL fields of jumping
     instructions.  */
  if (rebuild_notes)
    {
      timevar_push (TV_JUMP);

      rebuild_jump_labels (get_insns ());
      purge_all_dead_edges (0);
      delete_unreachable_blocks ();

      timevar_pop (TV_JUMP);
    }

  if (dump_enabled_p (DFI_lreg))
    {
      timevar_push (TV_DUMP);
      dump_flow_info (dump_file);
      dump_local_alloc (dump_file);
      timevar_pop (TV_DUMP);
    }

  close_dump_file (DFI_lreg, print_rtl_with_bb, get_insns ());

  ggc_collect ();

  timevar_push (TV_GLOBAL_ALLOC);
  open_dump_file (DFI_greg, current_function_decl);

  /* If optimizing, allocate remaining pseudo-regs.  Do the reload
     pass fixing up any insns that are invalid.  */

  if (optimize)
    failure = global_alloc (dump_file);
  else
    {
      build_insn_chain (get_insns ());
      failure = reload (get_insns (), 0);
    }

  if (dump_enabled_p (DFI_greg))
    {
      timevar_push (TV_DUMP);
      dump_global_regs (dump_file);
      timevar_pop (TV_DUMP);

      close_dump_file (DFI_greg, print_rtl_with_bb, get_insns ());
    }

  ggc_collect ();

  timevar_pop (TV_GLOBAL_ALLOC);

  return failure;
}

/* Run the regrename and cprop passes.  */
static void
rest_of_handle_regrename (void)
{
  timevar_push (TV_RENAME_REGISTERS);
  open_dump_file (DFI_rnreg, current_function_decl);

  if (flag_rename_registers)
    regrename_optimize ();
  if (flag_cprop_registers)
    copyprop_hardreg_forward ();

  close_dump_file (DFI_rnreg, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_RENAME_REGISTERS);
}

/* Reorder basic blocks.  */
static void
rest_of_handle_reorder_blocks (void)
{
  bool changed;
  unsigned int liveness_flags;

  open_dump_file (DFI_bbro, current_function_decl);

  /* Last attempt to optimize CFG, as scheduling, peepholing and insn
     splitting possibly introduced more crossjumping opportunities.  */
  liveness_flags = (!HAVE_conditional_execution ? CLEANUP_UPDATE_LIFE : 0);
  changed = cleanup_cfg (CLEANUP_EXPENSIVE | liveness_flags);

  if (flag_sched2_use_traces && flag_schedule_insns_after_reload)
    tracer (liveness_flags);
  if (flag_reorder_blocks || flag_reorder_blocks_and_partition)
    reorder_basic_blocks (liveness_flags);
  if (flag_reorder_blocks || flag_reorder_blocks_and_partition
      || (flag_sched2_use_traces && flag_schedule_insns_after_reload))
    changed |= cleanup_cfg (CLEANUP_EXPENSIVE | liveness_flags);

  /* On conditional execution targets we can not update the life cheaply, so
     we deffer the updating to after both cleanups.  This may lose some cases
     but should not be terribly bad.  */
  if (changed && HAVE_conditional_execution)
    update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES,
		      PROP_DEATH_NOTES);
  close_dump_file (DFI_bbro, print_rtl_with_bb, get_insns ());
}

/* Partition hot and cold basic blocks.  */
static void
rest_of_handle_partition_blocks (void)
{
  no_new_pseudos = 0;
  partition_hot_cold_basic_blocks ();
  allocate_reg_life_data ();
  update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES, 
		    PROP_LOG_LINKS | PROP_REG_INFO | PROP_DEATH_NOTES);
  no_new_pseudos = 1;
}

#ifdef INSN_SCHEDULING
/* Run instruction scheduler.  */
/* Perform SMS module scheduling.  */
static void
rest_of_handle_sms (void)
{
  sbitmap blocks;

  timevar_push (TV_SMS);
  open_dump_file (DFI_sms, current_function_decl);

  /* We want to be able to create new pseudos.  */
  no_new_pseudos = 0;
  sms_schedule (dump_file);
  close_dump_file (DFI_sms, print_rtl, get_insns ());


  /* Update the life information, because we add pseudos.  */
  max_regno = max_reg_num ();
  allocate_reg_info (max_regno, FALSE, FALSE);
  blocks = sbitmap_alloc (last_basic_block);
  sbitmap_ones (blocks);
  update_life_info (blocks, UPDATE_LIFE_GLOBAL_RM_NOTES,
		    (PROP_DEATH_NOTES
		     | PROP_REG_INFO
		     | PROP_KILL_DEAD_CODE
		     | PROP_SCAN_DEAD_CODE));

  no_new_pseudos = 1;

  ggc_collect ();
  timevar_pop (TV_SMS);
}

/* Run instruction scheduler.  */
static void
rest_of_handle_sched (void)
{
  timevar_push (TV_SCHED);

  /* Print function header into sched dump now
     because doing the sched analysis makes some of the dump.  */
  open_dump_file (DFI_sched, current_function_decl);

  /* Do control and data sched analysis,
     and write some of the results to dump file.  */

  schedule_insns (dump_file);

  close_dump_file (DFI_sched, print_rtl_with_bb, get_insns ());

  ggc_collect ();
  timevar_pop (TV_SCHED);
}

/* Run second scheduling pass after reload.  */
static void
rest_of_handle_sched2 (void)
{
  timevar_push (TV_SCHED2);
  open_dump_file (DFI_sched2, current_function_decl);

  /* Do control and data sched analysis again,
     and write some more of the results to dump file.  */

  split_all_insns (1);

  if (flag_sched2_use_superblocks || flag_sched2_use_traces)
    {
      schedule_ebbs (dump_file);
      /* No liveness updating code yet, but it should be easy to do.
	 reg-stack recomputes the liveness when needed for now.  */
      count_or_remove_death_notes (NULL, 1);
      cleanup_cfg (CLEANUP_EXPENSIVE);
    }
  else
    schedule_insns (dump_file);

  close_dump_file (DFI_sched2, print_rtl_with_bb, get_insns ());

  ggc_collect ();

  timevar_pop (TV_SCHED2);
}
#endif

static void
rest_of_handle_gcse2 (void)
{
  timevar_push (TV_GCSE_AFTER_RELOAD);
  open_dump_file (DFI_gcse2, current_function_decl);

  gcse_after_reload_main (get_insns ());
  rebuild_jump_labels (get_insns ());
  delete_trivially_dead_insns (get_insns (), max_reg_num ());
  close_dump_file (DFI_gcse2, print_rtl_with_bb, get_insns ());

  ggc_collect ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  timevar_pop (TV_GCSE_AFTER_RELOAD);
}

/* Register allocation pre-pass, to reduce number of moves necessary
   for two-address machines.  */
static void
rest_of_handle_regmove (void)
{
  timevar_push (TV_REGMOVE);
  open_dump_file (DFI_regmove, current_function_decl);

  regmove_optimize (get_insns (), max_reg_num (), dump_file);

  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
  close_dump_file (DFI_regmove, print_rtl_with_bb, get_insns ());

  ggc_collect ();
  timevar_pop (TV_REGMOVE);
}

/* Run tracer.  */
static void
rest_of_handle_tracer (void)
{
  open_dump_file (DFI_tracer, current_function_decl);
  if (dump_file)
    dump_flow_info (dump_file);
  tracer (0);
  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (get_insns (), max_reg_num ());
  close_dump_file (DFI_tracer, print_rtl_with_bb, get_insns ());
}

/* If-conversion and CFG cleanup.  */
static void
rest_of_handle_if_conversion (void)
{
  timevar_push (TV_IFCVT);
  open_dump_file (DFI_ce1, current_function_decl);

  if (flag_if_conversion)
    {
      if (dump_file)
	dump_flow_info (dump_file);
      cleanup_cfg (CLEANUP_EXPENSIVE);
      reg_scan (get_insns (), max_reg_num ());
      if_convert (0);
    }

  timevar_push (TV_JUMP);
  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (get_insns (), max_reg_num ());
  timevar_pop (TV_JUMP);

  close_dump_file (DFI_ce1, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_IFCVT);
}

/* Rerun if-conversion, as combine may have simplified things enough
   to now meet sequence length restrictions.  */
static void
rest_of_handle_if_after_combine (void)
{
  timevar_push (TV_IFCVT);
  open_dump_file (DFI_ce2, current_function_decl);

  no_new_pseudos = 0;
  if_convert (1);
  no_new_pseudos = 1;

  close_dump_file (DFI_ce2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_IFCVT);
}

static void
rest_of_handle_if_after_reload (void)
{
  timevar_push (TV_IFCVT2);
  open_dump_file (DFI_ce3, current_function_decl);

  /* Last attempt to optimize CFG, as scheduling, peepholing and insn
     splitting possibly introduced more crossjumping opportunities.  */
  cleanup_cfg (CLEANUP_EXPENSIVE
	       | CLEANUP_UPDATE_LIFE 
	       | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0));
  if (flag_if_conversion2)
    if_convert (1);
  close_dump_file (DFI_ce3, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_IFCVT2);
}

static void
rest_of_handle_web (void)
{
  open_dump_file (DFI_web, current_function_decl);
  timevar_push (TV_WEB);
  web_main ();
  delete_trivially_dead_insns (get_insns (), max_reg_num ());
  cleanup_cfg (CLEANUP_EXPENSIVE);

  timevar_pop (TV_WEB);
  close_dump_file (DFI_web, print_rtl_with_bb, get_insns ());
  reg_scan (get_insns (), max_reg_num ());
}

/* Do branch profiling and static profile estimation passes.  */
static void
rest_of_handle_branch_prob (void)
{
  struct loops loops;

  timevar_push (TV_BRANCH_PROB);
  open_dump_file (DFI_bp, current_function_decl);

  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    branch_prob ();

  /* Discover and record the loop depth at the head of each basic
     block.  The loop infrastructure does the real job for us.  */
  flow_loops_find (&loops, LOOP_TREE);

  if (dump_file)
    flow_loops_dump (&loops, dump_file, NULL, 0);

  /* Estimate using heuristics if no profiling info is available.  */
  if (flag_guess_branch_prob)
    estimate_probability (&loops);

  flow_loops_free (&loops);
  free_dominance_info (CDI_DOMINATORS);
  close_dump_file (DFI_bp, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_BRANCH_PROB);
}

/* Do optimizations based on expression value profiles.  */
static void
rest_of_handle_value_profile_transformations (void)
{
  open_dump_file (DFI_vpt, current_function_decl);
  timevar_push (TV_VPT);

  if (value_profile_transformations ())
    cleanup_cfg (CLEANUP_EXPENSIVE);

  timevar_pop (TV_VPT);
  close_dump_file (DFI_vpt, print_rtl_with_bb, get_insns ());
}

/* Do control and data flow analysis; write some of the results to the
   dump file.  */
static void
rest_of_handle_cfg (void)
{
  open_dump_file (DFI_cfg, current_function_decl);
  if (dump_file)
    dump_flow_info (dump_file);
  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  /* It may make more sense to mark constant functions after dead code is
     eliminated by life_analysis, but we need to do it early, as -fprofile-arcs
     may insert code making function non-constant, but we still must consider
     it as constant, otherwise -fbranch-probabilities will not read data back.

     life_analysis rarely eliminates modification of external memory.

     FIXME: now with tree based profiling we are in the trap described above
     again.  It seems to be easiest to disable the optimization for time
     being before the problem is either solved by moving the transformation
     to the IPA level (we need the CFG for this) or the very early optimization
     passes are made to ignore the const/pure flags so code does not change.  */
  if (optimize
      && (!flag_tree_based_profiling
	  || (!profile_arc_flag && !flag_branch_probabilities)))
    {
      /* Alias analysis depends on this information and mark_constant_function
       depends on alias analysis.  */
      reg_scan (get_insns (), max_reg_num ());
      mark_constant_function ();
    }

  close_dump_file (DFI_cfg, print_rtl_with_bb, get_insns ());
}

/* Perform jump bypassing and control flow optimizations.  */
static void
rest_of_handle_jump_bypass (void)
{
  timevar_push (TV_BYPASS);
  open_dump_file (DFI_bypass, current_function_decl);

  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (get_insns (), max_reg_num ());

  if (bypass_jumps (dump_file))
    {
      rebuild_jump_labels (get_insns ());
      cleanup_cfg (CLEANUP_EXPENSIVE);
      delete_trivially_dead_insns (get_insns (), max_reg_num ());
    }

  close_dump_file (DFI_bypass, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_BYPASS);

  ggc_collect ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Try combining insns through substitution.  */
static void
rest_of_handle_combine (void)
{
  int rebuild_jump_labels_after_combine = 0;

  timevar_push (TV_COMBINE);
  open_dump_file (DFI_combine, current_function_decl);

  rebuild_jump_labels_after_combine
    = combine_instructions (get_insns (), max_reg_num ());

  /* Combining insns may have turned an indirect jump into a
     direct jump.  Rebuild the JUMP_LABEL fields of jumping
     instructions.  */
  if (rebuild_jump_labels_after_combine)
    {
      timevar_push (TV_JUMP);
      rebuild_jump_labels (get_insns ());
      timevar_pop (TV_JUMP);

      delete_dead_jumptables ();
      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
    }

  close_dump_file (DFI_combine, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_COMBINE);

  ggc_collect ();
}

/* Perform life analysis.  */
static void
rest_of_handle_life (void)
{
  open_dump_file (DFI_life, current_function_decl);
  regclass_init ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  life_analysis (dump_file, PROP_FINAL);
  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE | CLEANUP_LOG_LINKS
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  if (extra_warnings)
    {
      setjmp_vars_warning (DECL_INITIAL (current_function_decl));
      setjmp_args_warning ();
    }

  if (optimize)
    {
      if (initialize_uninitialized_subregs ())
	{
	  /* Insns were inserted, and possibly pseudos created, so
	     things might look a bit different.  */
	  allocate_reg_life_data ();
	  update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES,
			    PROP_LOG_LINKS | PROP_REG_INFO | PROP_DEATH_NOTES);
	}
    }

  no_new_pseudos = 1;

  close_dump_file (DFI_life, print_rtl_with_bb, get_insns ());

  ggc_collect ();
}

/* Perform common subexpression elimination.  Nonzero value from
   `cse_main' means that jumps were simplified and some code may now
   be unreachable, so do jump optimization again.  */
static void
rest_of_handle_cse (void)
{
  int tem;

  open_dump_file (DFI_cse, current_function_decl);
  if (dump_file)
    dump_flow_info (dump_file);
  timevar_push (TV_CSE);

  reg_scan (get_insns (), max_reg_num ());

  tem = cse_main (get_insns (), max_reg_num (), dump_file);
  if (tem)
    rebuild_jump_labels (get_insns ());
  if (purge_all_dead_edges (0))
    delete_unreachable_blocks ();

  delete_trivially_dead_insns (get_insns (), max_reg_num ());

  /* If we are not running more CSE passes, then we are no longer
     expecting CSE to be run.  But always rerun it in a cheap mode.  */
  cse_not_expected = !flag_rerun_cse_after_loop && !flag_gcse;

  if (tem)
    delete_dead_jumptables ();

  if (tem || optimize > 1)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  timevar_pop (TV_CSE);
  close_dump_file (DFI_cse, print_rtl_with_bb, get_insns ());

  ggc_collect ();
}

/* Run second CSE pass after loop optimizations.  */
static void
rest_of_handle_cse2 (void)
{
  int tem;

  timevar_push (TV_CSE2);
  open_dump_file (DFI_cse2, current_function_decl);
  if (dump_file)
    dump_flow_info (dump_file);
  /* CFG is no longer maintained up-to-date.  */
  tem = cse_main (get_insns (), max_reg_num (), dump_file);

  /* Run a pass to eliminate duplicated assignments to condition code
     registers.  We have to run this after bypass_jumps, because it
     makes it harder for that pass to determine whether a jump can be
     bypassed safely.  */
  cse_condition_code_reg ();

  purge_all_dead_edges (0);
  delete_trivially_dead_insns (get_insns (), max_reg_num ());

  if (tem)
    {
      timevar_push (TV_JUMP);
      rebuild_jump_labels (get_insns ());
      delete_dead_jumptables ();
      cleanup_cfg (CLEANUP_EXPENSIVE);
      timevar_pop (TV_JUMP);
    }
  reg_scan (get_insns (), max_reg_num ());
  close_dump_file (DFI_cse2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_CSE2);

  ggc_collect ();
}

/* Perform global cse.  */
static void
rest_of_handle_gcse (void)
{
  int save_csb, save_cfj;
  int tem2 = 0, tem;

  timevar_push (TV_GCSE);
  open_dump_file (DFI_gcse, current_function_decl);

  tem = gcse_main (get_insns (), dump_file);
  rebuild_jump_labels (get_insns ());
  delete_trivially_dead_insns (get_insns (), max_reg_num ());

  save_csb = flag_cse_skip_blocks;
  save_cfj = flag_cse_follow_jumps;
  flag_cse_skip_blocks = flag_cse_follow_jumps = 0;

  /* If -fexpensive-optimizations, re-run CSE to clean up things done
     by gcse.  */
  if (flag_expensive_optimizations)
    {
      timevar_push (TV_CSE);
      reg_scan (get_insns (), max_reg_num ());
      tem2 = cse_main (get_insns (), max_reg_num (), dump_file);
      purge_all_dead_edges (0);
      delete_trivially_dead_insns (get_insns (), max_reg_num ());
      timevar_pop (TV_CSE);
      cse_not_expected = !flag_rerun_cse_after_loop;
    }

  /* If gcse or cse altered any jumps, rerun jump optimizations to clean
     things up.  */
  if (tem || tem2)
    {
      timevar_push (TV_JUMP);
      rebuild_jump_labels (get_insns ());
      delete_dead_jumptables ();
      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
      timevar_pop (TV_JUMP);
    }

  close_dump_file (DFI_gcse, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_GCSE);

  ggc_collect ();
  flag_cse_skip_blocks = save_csb;
  flag_cse_follow_jumps = save_cfj;
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Move constant computations out of loops.  */
static void
rest_of_handle_loop_optimize (void)
{
  int do_prefetch;

  timevar_push (TV_LOOP);
  open_dump_file (DFI_loop, current_function_decl);

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();
  profile_status = PROFILE_ABSENT;

  do_prefetch = flag_prefetch_loop_arrays ? LOOP_PREFETCH : 0;

  if (flag_rerun_loop_opt)
    {
      cleanup_barriers ();

      /* We only want to perform unrolling once.  */
      loop_optimize (get_insns (), dump_file, 0);

      /* The first call to loop_optimize makes some instructions
	 trivially dead.  We delete those instructions now in the
	 hope that doing so will make the heuristics in loop work
	 better and possibly speed up compilation.  */
      delete_trivially_dead_insns (get_insns (), max_reg_num ());

      /* The regscan pass is currently necessary as the alias
	 analysis code depends on this information.  */
      reg_scan (get_insns (), max_reg_num ());
    }
  cleanup_barriers ();
  loop_optimize (get_insns (), dump_file, do_prefetch);

  /* Loop can create trivially dead instructions.  */
  delete_trivially_dead_insns (get_insns (), max_reg_num ());
  find_basic_blocks (get_insns ());
  close_dump_file (DFI_loop, print_rtl, get_insns ());
  timevar_pop (TV_LOOP);

  ggc_collect ();
}

/* Perform loop optimizations.  It might be better to do them a bit
   sooner, but we want the profile feedback to work more
   efficiently.  */
static void
rest_of_handle_loop2 (void)
{
  struct loops *loops;
  basic_block bb;

  if (!flag_move_loop_invariants
      && !flag_unswitch_loops
      && !flag_peel_loops
      && !flag_unroll_loops
      && !flag_branch_on_count_reg)
    return;

  timevar_push (TV_LOOP);
  open_dump_file (DFI_loop2, current_function_decl);
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
      bb->rbi->next = bb->next_bb;
  cfg_layout_finalize ();

  cleanup_cfg (CLEANUP_EXPENSIVE);
  delete_trivially_dead_insns (get_insns (), max_reg_num ());
  reg_scan (get_insns (), max_reg_num ());
  if (dump_file)
    dump_flow_info (dump_file);
  close_dump_file (DFI_loop2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_LOOP);
  ggc_collect ();
}

static void
rest_of_handle_branch_target_load_optimize (void)
{
  static int warned = 0;

  /* Leave this a warning for now so that it is possible to experiment
     with running this pass twice.  In 3.6, we should either make this
     an error, or use separate dump files.  */
  if (flag_branch_target_load_optimize
      && flag_branch_target_load_optimize2
      && !warned)
    {
      warning ("branch target register load optimization is not intended "
	       "to be run twice");

      warned = 1;
    }

  open_dump_file (DFI_branch_target_load, current_function_decl);
  branch_target_load_optimize (epilogue_completed);
  close_dump_file (DFI_branch_target_load, print_rtl_with_bb, get_insns ());
  ggc_collect ();
}

#ifdef OPTIMIZE_MODE_SWITCHING
static void
rest_of_handle_mode_switching (void)
{
  timevar_push (TV_MODE_SWITCH);

  no_new_pseudos = 0;
  optimize_mode_switching (NULL);
  no_new_pseudos = 1;

  timevar_pop (TV_MODE_SWITCH);
}
#endif

static void
rest_of_handle_jump (void)
{
  ggc_collect ();

  timevar_push (TV_JUMP);
  open_dump_file (DFI_sibling, current_function_decl);

  delete_unreachable_blocks ();
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  if (cfun->tail_call_emit)
    fixup_tail_calls ();

  close_dump_file (DFI_sibling, print_rtl, get_insns ());
  timevar_pop (TV_JUMP);
}

static void
rest_of_handle_eh (void)
{
  insn_locators_initialize ();
  /* Complete generation of exception handling code.  */
  if (doing_eh (0))
    {
      timevar_push (TV_JUMP);
      open_dump_file (DFI_eh, current_function_decl);

      cleanup_cfg (CLEANUP_PRE_LOOP | CLEANUP_NO_INSN_DEL);

      finish_eh_generation ();

      cleanup_cfg (CLEANUP_PRE_LOOP | CLEANUP_NO_INSN_DEL);

      close_dump_file (DFI_eh, print_rtl, get_insns ());
      timevar_pop (TV_JUMP);
    }
}

static void
rest_of_handle_stack_adjustments (void)
{
  life_analysis (dump_file, PROP_POSTRELOAD);
  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE
	       | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0));
    
  /* This is kind of a heuristic.  We need to run combine_stack_adjustments
     even for machines with possibly nonzero RETURN_POPS_ARGS
     and ACCUMULATE_OUTGOING_ARGS.  We expect that only ports having
     push instructions will have popping returns.  */
#ifndef PUSH_ROUNDING
  if (!ACCUMULATE_OUTGOING_ARGS)
#endif
    combine_stack_adjustments ();
}

static void
rest_of_handle_flow2 (void)
{
  timevar_push (TV_FLOW2);
  open_dump_file (DFI_flow2, current_function_decl);

  /* Re-create the death notes which were deleted during reload.  */
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  /* If optimizing, then go ahead and split insns now.  */
#ifndef STACK_REGS
  if (optimize > 0)
#endif
    split_all_insns (0);

  if (flag_branch_target_load_optimize)
    {
      close_dump_file (DFI_flow2, print_rtl_with_bb, get_insns ());
      rest_of_handle_branch_target_load_optimize ();
      open_dump_file (DFI_flow2, current_function_decl);
    }

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE);

  /* On some machines, the prologue and epilogue code, or parts thereof,
     can be represented as RTL.  Doing so lets us schedule insns between
     it and the rest of the code and also allows delayed branch
     scheduling to operate in the epilogue.  */
  thread_prologue_and_epilogue_insns (get_insns ());
  epilogue_completed = 1;

  if (optimize)
    rest_of_handle_stack_adjustments ();

  flow2_completed = 1;

  close_dump_file (DFI_flow2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_FLOW2);

  ggc_collect ();
}


static void
rest_of_handle_jump2 (void)
{
  open_dump_file (DFI_jump, current_function_decl);

  /* Always do one jump optimization pass to ensure that JUMP_LABEL fields
     are initialized and to compute whether control can drop off the end
     of the function.  */

  timevar_push (TV_JUMP);
  /* Turn NOTE_INSN_EXPECTED_VALUE into REG_BR_PROB.  Do this
     before jump optimization switches branch directions.  */
  if (flag_guess_branch_prob)
    expected_value_to_br_prob ();

  delete_trivially_dead_insns (get_insns (), max_reg_num ());
  reg_scan (get_insns (), max_reg_num ());
  if (dump_file)
    dump_flow_info (dump_file);
  cleanup_cfg ((optimize ? CLEANUP_EXPENSIVE : 0) | CLEANUP_PRE_LOOP
	       | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  create_loop_notes ();

  purge_line_number_notes (get_insns ());

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  /* Jump optimization, and the removal of NULL pointer checks, may
     have reduced the number of instructions substantially.  CSE, and
     future passes, allocate arrays whose dimensions involve the
     maximum instruction UID, so if we can reduce the maximum UID
     we'll save big on memory.  */
  renumber_insns (dump_file);

  close_dump_file (DFI_jump, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_JUMP);

  ggc_collect ();
}

#ifdef HAVE_peephole2
static void
rest_of_handle_peephole2 (void)
{
  timevar_push (TV_PEEPHOLE2);
  open_dump_file (DFI_peephole2, current_function_decl);

  peephole2_optimize (dump_file);

  close_dump_file (DFI_peephole2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_PEEPHOLE2);
}
#endif

static void
rest_of_handle_postreload (void)
{
  timevar_push (TV_RELOAD_CSE_REGS);
  open_dump_file (DFI_postreload, current_function_decl);

  /* Do a very simple CSE pass over just the hard registers.  */
  reload_cse_regs (get_insns ());
  /* reload_cse_regs can eliminate potentially-trapping MEMs.
     Remove any EH edges associated with them.  */
  if (flag_non_call_exceptions)
    purge_all_dead_edges (0);

  close_dump_file (DFI_postreload, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_RELOAD_CSE_REGS);
}

static void
rest_of_handle_shorten_branches (void)
{
  /* Shorten branches.  */
  timevar_push (TV_SHORTEN_BRANCH);
  shorten_branches (get_insns ());
  timevar_pop (TV_SHORTEN_BRANCH);
}

static void
rest_of_clean_state (void)
{
  rtx insn, next;
  coverage_end_function ();

  /* It is very important to decompose the RTL instruction chain here:
     debug information keeps pointing into CODE_LABEL insns inside the function
     body.  If these remain pointing to the other insns, we end up preserving
     whole RTL chain and attached detailed debug info in memory.  */
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      NEXT_INSN (insn) = NULL;
      PREV_INSN (insn) = NULL;
    }

  /* In case the function was not output,
     don't leave any temporary anonymous types
     queued up for sdb output.  */
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_types (NULL_TREE);
#endif

  reload_completed = 0;
  epilogue_completed = 0;
  flow2_completed = 0;
  no_new_pseudos = 0;

  timevar_push (TV_FINAL);

  /* Clear out the insn_length contents now that they are no
     longer valid.  */
  init_insn_lengths ();

  /* Show no temporary slots allocated.  */
  init_temp_slots ();

  free_basic_block_vars ();
  free_bb_for_insn ();

  timevar_pop (TV_FINAL);

  if (targetm.binds_local_p (current_function_decl))
    {
      int pref = cfun->preferred_stack_boundary;
      if (cfun->stack_alignment_needed > cfun->preferred_stack_boundary)
	pref = cfun->stack_alignment_needed;
      cgraph_rtl_info (current_function_decl)->preferred_incoming_stack_boundary
        = pref;
    }

  /* Make sure volatile mem refs aren't considered valid operands for
     arithmetic insns.  We must call this here if this is a nested inline
     function, since the above code leaves us in the init_recog state
     (from final.c), and the function context push/pop code does not
     save/restore volatile_ok.

     ??? Maybe it isn't necessary for expand_start_function to call this
     anymore if we do it here?  */

  init_recog_no_volatile ();

  /* We're done with this function.  Free up memory if we can.  */
  free_after_parsing (cfun);
  free_after_compilation (cfun);
}


/* This function is called from the pass manager in tree-optimize.c
   after all tree passes have finished for a single function, and we
   have expanded the function body from trees to RTL.
   Once we are here, we have decided that we're supposed to output
   that function, i.e. that we should write assembler code for it.

   We run a series of low-level passes here on the function's RTL
   representation.  Each pass is called via a rest_of_* function.  */

static void
rest_of_compilation (void)
{
  /* If we're emitting a nested function, make sure its parent gets
     emitted as well.  Doing otherwise confuses debug info.  */
  {
    tree parent;
    for (parent = DECL_CONTEXT (current_function_decl);
	 parent != NULL_TREE;
	 parent = get_containing_scope (parent))
      if (TREE_CODE (parent) == FUNCTION_DECL)
	TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (parent)) = 1;
  }

  /* We are now committed to emitting code for this function.  Do any
     preparation, such as emitting abstract debug info for the inline
     before it gets mangled by optimization.  */
  if (cgraph_function_possibly_inlined_p (current_function_decl))
    (*debug_hooks->outlining_inline_function) (current_function_decl);

  /* Remove any notes we don't need.  That will make iterating
     over the instruction sequence faster, and allow the garbage
     collector to reclaim the memory used by the notes.  */
  remove_unnecessary_notes ();

  /* Initialize some variables used by the optimizers.  */
  init_function_for_compilation ();

  TREE_ASM_WRITTEN (current_function_decl) = 1;

  /* Early return if there were errors.  We can run afoul of our
     consistency checks, and there's not really much point in fixing them.  */
  if (rtl_dump_and_exit || flag_syntax_only || errorcount || sorrycount)
    goto exit_rest_of_compilation;

  rest_of_handle_jump ();

  rest_of_handle_eh ();

  /* Delay emitting hard_reg_initial_value sets until after EH landing pad
     generation, which might create new sets.  */
  emit_initial_value_sets ();

#ifdef FINALIZE_PIC
  /* If we are doing position-independent code generation, now
     is the time to output special prologues and epilogues.
     We do not want to do this earlier, because it just clutters
     up inline functions with meaningless insns.  */
  if (flag_pic)
    FINALIZE_PIC;
#endif

  /* Copy any shared structure that should not be shared.  */
  unshare_all_rtl ();

#ifdef SETJMP_VIA_SAVE_AREA
  /* This must be performed before virtual register instantiation.
     Please be aware that everything in the compiler that can look
     at the RTL up to this point must understand that REG_SAVE_AREA
     is just like a use of the REG contained inside.  */
  if (current_function_calls_alloca)
    optimize_save_area_alloca ();
#endif

  /* Instantiate all virtual registers.  */
  instantiate_virtual_regs ();

  rest_of_handle_jump2 ();

  if (optimize > 0)
    rest_of_handle_cse ();

  if (optimize > 0)
    {
      if (flag_gcse)
	rest_of_handle_gcse ();

      if (flag_loop_optimize)
	rest_of_handle_loop_optimize ();

      if (flag_gcse)
	rest_of_handle_jump_bypass ();
    }

  timevar_push (TV_FLOW);
  rest_of_handle_cfg ();

  if (!flag_tree_based_profiling
      && (optimize > 0 || profile_arc_flag
	  || flag_test_coverage || flag_branch_probabilities))
    {
      rtl_register_profile_hooks ();
      rtl_register_value_prof_hooks ();
      rest_of_handle_branch_prob ();

      if (flag_branch_probabilities
	  && flag_profile_values
	  && (flag_value_profile_transformations
	      || flag_speculative_prefetching))
	rest_of_handle_value_profile_transformations ();

      /* Remove the death notes created for vpt.  */
      if (flag_profile_values)
	count_or_remove_death_notes (NULL, 1);
    }

  if (optimize > 0)
    rest_of_handle_if_conversion ();

  if (optimize > 0 && flag_tracer)
    rest_of_handle_tracer ();

  if (optimize > 0
      && flag_loop_optimize2)
    rest_of_handle_loop2 ();

  if (optimize > 0 && flag_web)
    rest_of_handle_web ();

  if (optimize > 0 && flag_rerun_cse_after_loop)
    rest_of_handle_cse2 ();

  cse_not_expected = 1;

  rest_of_handle_life ();
  timevar_pop (TV_FLOW);

  if (optimize > 0)
    rest_of_handle_combine ();

  if (optimize > 0 && flag_if_conversion)
    rest_of_handle_if_after_combine ();

  /* The optimization to partition hot/cold basic blocks into separate
     sections of the .o file does not work well with linkonce or with
     user defined section attributes.  Don't call it if either case
     arises.  */

  if (flag_reorder_blocks_and_partition 
      && !DECL_ONE_ONLY (current_function_decl)
      && !user_defined_section_attribute)
    rest_of_handle_partition_blocks ();

  if (optimize > 0 && flag_regmove)
    rest_of_handle_regmove ();

  /* Do unconditional splitting before register allocation to allow machine
     description to add extra information not needed previously.  */
  split_all_insns (1);

#ifdef OPTIMIZE_MODE_SWITCHING
  rest_of_handle_mode_switching ();
#endif

  /* Any of the several passes since flow1 will have munged register
     lifetime data a bit.  We need it to be up to date for scheduling
     (see handling of reg_known_equiv in init_alias_analysis).  */
  recompute_reg_usage ();

#ifdef INSN_SCHEDULING
  if (optimize > 0 && flag_modulo_sched)
    rest_of_handle_sms ();

  if (flag_schedule_insns)
    rest_of_handle_sched ();
#endif

  /* Determine if the current function is a leaf before running reload
     since this can impact optimizations done by the prologue and
     epilogue thus changing register elimination offsets.  */
  current_function_is_leaf = leaf_function_p ();

  if (rest_of_handle_old_regalloc ())
    goto exit_rest_of_compilation;

  if (optimize > 0)
    rest_of_handle_postreload ();

  if (optimize > 0 && flag_gcse_after_reload)
    rest_of_handle_gcse2 ();

  rest_of_handle_flow2 ();

#ifdef HAVE_peephole2
  if (optimize > 0 && flag_peephole2)
    rest_of_handle_peephole2 ();
#endif

  if (optimize > 0)
    rest_of_handle_if_after_reload ();

  if (optimize > 0)
    {
      if (flag_rename_registers || flag_cprop_registers)
	rest_of_handle_regrename ();

      rest_of_handle_reorder_blocks ();
    }

  if (flag_branch_target_load_optimize2)
    rest_of_handle_branch_target_load_optimize ();

#ifdef LEAF_REGISTERS
  current_function_uses_only_leaf_regs
    = optimize > 0 && only_leaf_regs_used () && leaf_function_p ();
#endif

#ifdef INSN_SCHEDULING
  if (optimize > 0 && flag_schedule_insns_after_reload)
    rest_of_handle_sched2 ();
#endif

#ifdef STACK_REGS
  rest_of_handle_stack_regs ();
#endif

  compute_alignments ();

  /* Aggressively duplicate basic blocks ending in computed gotos to the
     tails of their predecessors, unless we are optimizing for size.  */
  if (flag_expensive_optimizations && !optimize_size)
    duplicate_computed_gotos ();

  if (flag_var_tracking)
    rest_of_handle_variable_tracking ();

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();

  if (targetm.machine_dependent_reorg != 0)
    rest_of_handle_machine_reorg ();

  purge_line_number_notes (get_insns ());
  cleanup_barriers ();

#ifdef DELAY_SLOTS
  if (flag_delayed_branch)
    rest_of_handle_delay_slots ();
#endif

#if defined (HAVE_ATTR_length) && !defined (STACK_REGS)
  timevar_push (TV_SHORTEN_BRANCH);
  split_all_insns_noflow ();
  timevar_pop (TV_SHORTEN_BRANCH);
#endif

  convert_to_eh_region_ranges ();

  rest_of_handle_shorten_branches ();

  set_nothrow_function_flags ();

  rest_of_handle_final ();

 exit_rest_of_compilation:

  rest_of_clean_state ();
}

void
finish_optimization_passes (void)
{
  enum tree_dump_index i;
  struct dump_file_info *dfi;
  char *name;

  timevar_push (TV_DUMP);
  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      open_dump_file (DFI_bp, NULL);
      end_branch_prob ();
      close_dump_file (DFI_bp, NULL, NULL_RTX);
    }

  if (optimize > 0 && open_dump_file (DFI_combine, NULL))
    {
      dump_combine_total_stats (dump_file);
      close_dump_file (DFI_combine, NULL, NULL_RTX);
    }

  /* Do whatever is necessary to finish printing the graphs.  */
  if (graph_dump_format != no_graph)
    for (i = DFI_MIN; (dfi = get_dump_file_info (i)) != NULL; ++i)
      if (dump_initialized_p (i)
	  && (dfi->flags & TDF_RTL) != 0
	  && (name = get_dump_file_name (i)) != NULL)
        {
          finish_graph_dump_file (name);
          free (name);
        }

  timevar_pop (TV_DUMP);
}

struct tree_opt_pass pass_rest_of_compilation =
{
  NULL,			                /* name */
  NULL,		                        /* gate */
  rest_of_compilation,                  /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_REST_OF_COMPILATION,               /* tv_id */
  PROP_rtl,		                /* properties_required */
  0,                                    /* properties_provided */
  PROP_rtl,                             /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_ggc_collect,			/* todo_flags_finish */
  0					/* letter */
};


