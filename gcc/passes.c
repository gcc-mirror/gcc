/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
#include "loop.h"
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

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
#include "dwarf2out.h"
#endif

#if defined(DBX_DEBUGGING_INFO) || defined(XCOFF_DEBUGGING_INFO)
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

/* Describes a dump file.  */

struct dump_file_info
{
  /* The unique extension to apply, e.g. ".jump".  */
  const char *const extension;

  /* The -d<c> character that enables this dump file.  */
  char const debug_switch;

  /* True if there is a corresponding graph dump file.  */
  char const graph_dump_p;

  /* True if the user selected this dump.  */
  char enabled;

  /* True if the files have been initialized (ie truncated).  */
  char initialized;
};

/* Enumerate the extant dump files.  */

enum dump_file_index
{
  DFI_cgraph,
  DFI_rtl,
  DFI_sibling,
  DFI_eh,
  DFI_jump,
  DFI_null,
  DFI_cse,
  DFI_addressof,
  DFI_gcse,
  DFI_loop,
  DFI_bypass,
  DFI_cfg,
  DFI_bp,
  DFI_vpt,
  DFI_ce1,
  DFI_tracer,
  DFI_loop2,
  DFI_web,
  DFI_cse2,
  DFI_life,
  DFI_combine,
  DFI_ce2,
  DFI_regmove,
  DFI_sched,
  DFI_lreg,
  DFI_greg,
  DFI_postreload,
  DFI_gcse2,
  DFI_flow2,
  DFI_peephole2,
  DFI_ce3,
  DFI_rnreg,
  DFI_bbro,
  DFI_branch_target_load,
  DFI_sched2,
  DFI_stack,
  DFI_vartrack,
  DFI_mach,
  DFI_dbr,
  DFI_MAX
};

/* Describes all the dump files.  Should be kept in order of the
   pass and in sync with dump_file_index above.

   Remaining -d letters:

	"   e        m   q         "
	"          K   O Q     WXY "
*/

static struct dump_file_info dump_file_tbl[DFI_MAX] =
{
  { "cgraph",	'U', 0, 0, 0 },
  { "rtl",	'r', 0, 0, 0 },
  { "sibling",  'i', 0, 0, 0 },
  { "eh",	'h', 0, 0, 0 },
  { "jump",	'j', 0, 0, 0 },
  { "null",	'u', 0, 0, 0 },
  { "cse",	's', 0, 0, 0 },
  { "addressof", 'F', 0, 0, 0 },
  { "gcse",	'G', 1, 0, 0 },
  { "loop",	'L', 1, 0, 0 },
  { "bypass",   'G', 1, 0, 0 }, /* Yes, duplicate enable switch.  */
  { "cfg",	'f', 1, 0, 0 },
  { "bp",	'b', 1, 0, 0 },
  { "vpt",	'V', 1, 0, 0 },
  { "ce1",	'C', 1, 0, 0 },
  { "tracer",	'T', 1, 0, 0 },
  { "loop2",	'L', 1, 0, 0 },
  { "web",      'Z', 0, 0, 0 },
  { "cse2",	't', 1, 0, 0 },
  { "life",	'f', 1, 0, 0 },	/* Yes, duplicate enable switch.  */
  { "combine",	'c', 1, 0, 0 },
  { "ce2",	'C', 1, 0, 0 },
  { "regmove",	'N', 1, 0, 0 },
  { "sched",	'S', 1, 0, 0 },
  { "lreg",	'l', 1, 0, 0 },
  { "greg",	'g', 1, 0, 0 },
  { "postreload", 'o', 1, 0, 0 },
  { "gcse2",    'J', 0, 0, 0 },
  { "flow2",	'w', 1, 0, 0 },
  { "peephole2", 'z', 1, 0, 0 },
  { "ce3",	'E', 1, 0, 0 },
  { "rnreg",	'n', 1, 0, 0 },
  { "bbro",	'B', 1, 0, 0 },
  { "btl",	'd', 1, 0, 0 }, /* Yes, duplicate enable switch.  */
  { "sched2",	'R', 1, 0, 0 },
  { "stack",	'k', 1, 0, 0 },
  { "vartrack",	'V', 1, 0, 0 }, /* Yes, duplicate enable switch.  */
  { "mach",	'M', 1, 0, 0 },
  { "dbr",	'd', 0, 0, 0 },
};

/* Routine to open a dump file.  Return true if the dump file is enabled.  */

static int
open_dump_file (enum dump_file_index index, tree decl)
{
  char *dump_name;
  const char *open_arg;
  char seq[16];

  if (! dump_file_tbl[index].enabled)
    return 0;

  timevar_push (TV_DUMP);
  if (dump_file != NULL)
    fclose (dump_file);

  sprintf (seq, DUMPFILE_FORMAT, index);

  if (! dump_file_tbl[index].initialized)
    {
      /* If we've not initialized the files, do so now.  */
      if (graph_dump_format != no_graph
	  && dump_file_tbl[index].graph_dump_p)
	{
	  dump_name = concat (seq, dump_file_tbl[index].extension, NULL);
	  clean_graph_dump_file (dump_base_name, dump_name);
	  free (dump_name);
	}
      dump_file_tbl[index].initialized = 1;
      open_arg = "w";
    }
  else
    open_arg = "a";

  dump_name = concat (dump_base_name, seq,
		      dump_file_tbl[index].extension, NULL);

  dump_file = fopen (dump_name, open_arg);
  if (dump_file == NULL)
    fatal_error ("can't open %s: %m", dump_name);

  free (dump_name);

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
close_dump_file (enum dump_file_index index,
		 void (*func) (FILE *, rtx),
		 rtx insns)
{
  if (! dump_file)
    return;

  timevar_push (TV_DUMP);
  if (insns
      && graph_dump_format != no_graph
      && dump_file_tbl[index].graph_dump_p)
    {
      char seq[16];
      char *suffix;

      sprintf (seq, DUMPFILE_FORMAT, index);
      suffix = concat (seq, dump_file_tbl[index].extension, NULL);
      print_rtl_graph_with_bb (dump_base_name, suffix, insns);
      free (suffix);
    }

  if (func && insns)
    func (dump_file, insns);

  fflush (dump_file);
  fclose (dump_file);

  dump_file = NULL;
  timevar_pop (TV_DUMP);
}

/* This is called from various places for FUNCTION_DECL, VAR_DECL,
   and TYPE_DECL nodes.

   This does nothing for local (non-static) variables, unless the
   variable is a register variable with an ASMSPEC.  In that case, or
   if the variable is not an automatic, it sets up the RTL and
   outputs any assembler code (label definition, storage allocation
   and initialization).

   DECL is the declaration.  If ASMSPEC is nonzero, it specifies
   the assembler symbol name to be used.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (tree decl,
			  const char *asmspec,
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

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    {
      timevar_push (TV_VARCONST);

      if (asmspec)
	make_decl_rtl (decl, asmspec);

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
  else if (DECL_REGISTER (decl) && asmspec != 0)
    {
      if (decode_reg_name (asmspec) >= 0)
	{
	  SET_DECL_RTL (decl, NULL_RTX);
	  make_decl_rtl (decl, asmspec);
	}
      else
	{
	  error ("%Hinvalid register name `%s' for register variable",
		 &DECL_SOURCE_LOCATION (decl), asmspec);
	  DECL_REGISTER (decl) = 0;
	  if (!top_level)
	    expand_decl (decl);
	}
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      timevar_push (TV_SYMOUT);
      debug_hooks->type_decl (decl, !top_level);
      timevar_pop (TV_SYMOUT);
    }
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
rest_of_handle_final (tree decl, rtx insns)
{
  timevar_push (TV_FINAL);
  {
    rtx x;
    const char *fnname;

    /* Get the function's name, as described by its RTL.  This may be
       different from the DECL_NAME name used in the source file.  */

    x = DECL_RTL (decl);
    if (GET_CODE (x) != MEM)
      abort ();
    x = XEXP (x, 0);
    if (GET_CODE (x) != SYMBOL_REF)
      abort ();
    fnname = XSTR (x, 0);

    assemble_start_function (decl, fnname);
    final_start_function (insns, asm_out_file, optimize);
    final (insns, asm_out_file, optimize, 0);
    final_end_function ();

#ifdef IA64_UNWIND_INFO
    /* ??? The IA-64 ".handlerdata" directive must be issued before
       the ".endp" directive that closes the procedure descriptor.  */
    output_function_exception_table ();
#endif

    assemble_end_function (decl, fnname);

#ifndef IA64_UNWIND_INFO
    /* Otherwise, it feels unclean to switch sections in the middle.  */
    output_function_exception_table ();
#endif

    if (! quiet_flag)
      fflush (asm_out_file);

    /* Release all memory allocated by flow.  */
    free_basic_block_vars ();

    /* Release all memory held by regsets now.  */
    regset_release_memory ();
  }
  timevar_pop (TV_FINAL);

  ggc_collect ();
}

#ifdef DELAY_SLOTS
/* Run delay slot optimization.  */
static void
rest_of_handle_delay_slots (tree decl, rtx insns)
{
  timevar_push (TV_DBR_SCHED);
  open_dump_file (DFI_dbr, decl);

  dbr_schedule (insns, dump_file);

  close_dump_file (DFI_dbr, print_rtl, insns);
  timevar_pop (TV_DBR_SCHED);

  ggc_collect ();
}
#endif

#ifdef STACK_REGS
/* Convert register usage from flat register file usage to a stack
   register file.  */
static void
rest_of_handle_stack_regs (tree decl, rtx insns)
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
  open_dump_file (DFI_stack, decl);

  if (reg_to_stack (dump_file) && optimize)
    {
      if (cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK
		       | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0))
	  && (flag_reorder_blocks || flag_reorder_blocks_and_partition))
	{
	  reorder_basic_blocks ();
	  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK);
	}
    }

  close_dump_file (DFI_stack, print_rtl_with_bb, insns);
  timevar_pop (TV_REG_STACK);

  ggc_collect ();
}
#endif

/* Track the variables, ie. compute where the variable is stored at each position in function.  */
static void
rest_of_handle_variable_tracking (tree decl, rtx insns)
{
  timevar_push (TV_VAR_TRACKING);
  open_dump_file (DFI_vartrack, decl);

  variable_tracking_main ();

  close_dump_file (DFI_vartrack, print_rtl_with_bb, insns);
  timevar_pop (TV_VAR_TRACKING);
}

/* Machine independent reorg pass.  */
static void
rest_of_handle_machine_reorg (tree decl, rtx insns)
{
  timevar_push (TV_MACH_DEP);
  open_dump_file (DFI_mach, decl);

  targetm.machine_dependent_reorg ();

  close_dump_file (DFI_mach, print_rtl, insns);
  timevar_pop (TV_MACH_DEP);

  ggc_collect ();
}


/* Run new register allocator.  Return TRUE if we must exit
   rest_of_compilation upon return.  */
static bool
rest_of_handle_new_regalloc (tree decl, rtx insns)
{
  int failure;

  delete_trivially_dead_insns (insns, max_reg_num ());
  reg_alloc ();

  timevar_pop (TV_LOCAL_ALLOC);
  if (dump_file_tbl[DFI_lreg].enabled)
    {
      timevar_push (TV_DUMP);

      close_dump_file (DFI_lreg, NULL, NULL);
      timevar_pop (TV_DUMP);
    }

  /* XXX clean up the whole mess to bring live info in shape again.  */
  timevar_push (TV_GLOBAL_ALLOC);
  open_dump_file (DFI_greg, decl);

  build_insn_chain (insns);
  failure = reload (insns, 0);

  timevar_pop (TV_GLOBAL_ALLOC);

  if (dump_file_tbl[DFI_greg].enabled)
    {
      timevar_push (TV_DUMP);

      dump_global_regs (dump_file);

      close_dump_file (DFI_greg, print_rtl_with_bb, insns);
      timevar_pop (TV_DUMP);
    }

  if (failure)
    return true;

  reload_completed = 1;

  return false;
}

/* Run old register allocator.  Return TRUE if we must exit
   rest_of_compilation upon return.  */
static bool
rest_of_handle_old_regalloc (tree decl, rtx insns)
{
  int failure;
  int rebuild_notes;

  /* Allocate the reg_renumber array.  */
  allocate_reg_info (max_regno, FALSE, TRUE);

  /* And the reg_equiv_memory_loc array.  */
  VARRAY_GROW (reg_equiv_memory_loc_varray, max_regno);
  reg_equiv_memory_loc = &VARRAY_RTX (reg_equiv_memory_loc_varray, 0);

  allocate_initial_values (reg_equiv_memory_loc);

  regclass (insns, max_reg_num (), dump_file);
  rebuild_notes = local_alloc ();

  timevar_pop (TV_LOCAL_ALLOC);

  /* Local allocation may have turned an indirect jump into a direct
     jump.  If so, we must rebuild the JUMP_LABEL fields of jumping
     instructions.  */
  if (rebuild_notes)
    {
      timevar_push (TV_JUMP);

      rebuild_jump_labels (insns);
      purge_all_dead_edges (0);

      timevar_pop (TV_JUMP);
    }

  if (dump_file_tbl[DFI_lreg].enabled)
    {
      timevar_push (TV_DUMP);

      dump_flow_info (dump_file);
      dump_local_alloc (dump_file);

      close_dump_file (DFI_lreg, print_rtl_with_bb, insns);
      timevar_pop (TV_DUMP);
    }

  ggc_collect ();

  timevar_push (TV_GLOBAL_ALLOC);
  open_dump_file (DFI_greg, decl);

  /* If optimizing, allocate remaining pseudo-regs.  Do the reload
     pass fixing up any insns that are invalid.  */

  if (optimize)
    failure = global_alloc (dump_file);
  else
    {
      build_insn_chain (insns);
      failure = reload (insns, 0);
    }

  timevar_pop (TV_GLOBAL_ALLOC);

  if (dump_file_tbl[DFI_greg].enabled)
    {
      timevar_push (TV_DUMP);

      dump_global_regs (dump_file);

      close_dump_file (DFI_greg, print_rtl_with_bb, insns);
      timevar_pop (TV_DUMP);
    }

  return failure;
}

/* Run the regrename and cprop passes.  */
static void
rest_of_handle_regrename (tree decl, rtx insns)
{
  timevar_push (TV_RENAME_REGISTERS);
  open_dump_file (DFI_rnreg, decl);

  if (flag_rename_registers)
    regrename_optimize ();
  if (flag_cprop_registers)
    copyprop_hardreg_forward ();

  close_dump_file (DFI_rnreg, print_rtl_with_bb, insns);
  timevar_pop (TV_RENAME_REGISTERS);
}

/* Reorder basic blocks.  */
static void
rest_of_handle_reorder_blocks (tree decl, rtx insns)
{
  bool changed;
  open_dump_file (DFI_bbro, decl);

  /* Last attempt to optimize CFG, as scheduling, peepholing and insn
     splitting possibly introduced more crossjumping opportunities.  */
  changed = cleanup_cfg (CLEANUP_EXPENSIVE
			 | (!HAVE_conditional_execution
			    ? CLEANUP_UPDATE_LIFE : 0));

  if (flag_sched2_use_traces && flag_schedule_insns_after_reload)
    tracer ();
  if (flag_reorder_blocks || flag_reorder_blocks_and_partition)
    reorder_basic_blocks ();
  if (flag_reorder_blocks || flag_reorder_blocks_and_partition
      || (flag_sched2_use_traces && flag_schedule_insns_after_reload))
    changed |= cleanup_cfg (CLEANUP_EXPENSIVE
			    | (!HAVE_conditional_execution
			       ? CLEANUP_UPDATE_LIFE : 0));

  /* On conditional execution targets we can not update the life cheaply, so
     we deffer the updating to after both cleanups.  This may lose some cases
     but should not be terribly bad.  */
  if (changed && HAVE_conditional_execution)
    update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES,
		      PROP_DEATH_NOTES);
  close_dump_file (DFI_bbro, print_rtl_with_bb, insns);
}

#ifdef INSN_SCHEDULING
/* Run instruction scheduler.  */
static void
rest_of_handle_sched (tree decl, rtx insns)
{
  timevar_push (TV_SCHED);

  /* Print function header into sched dump now
     because doing the sched analysis makes some of the dump.  */
  if (optimize > 0 && flag_schedule_insns)
    {
      open_dump_file (DFI_sched, decl);

      /* Do control and data sched analysis,
	 and write some of the results to dump file.  */

      schedule_insns (dump_file);

      close_dump_file (DFI_sched, print_rtl_with_bb, insns);
    }
  timevar_pop (TV_SCHED);

  ggc_collect ();
}

/* Run second scheduling pass after reload.  */
static void
rest_of_handle_sched2 (tree decl, rtx insns)
{
  timevar_push (TV_SCHED2);
  open_dump_file (DFI_sched2, decl);

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

  close_dump_file (DFI_sched2, print_rtl_with_bb, insns);
  timevar_pop (TV_SCHED2);

  ggc_collect ();
}
#endif

static void
rest_of_handle_gcse2 (tree decl, rtx insns)
{
  open_dump_file (DFI_gcse2, decl);

  gcse_after_reload_main (insns, dump_file);
  rebuild_jump_labels (insns);
  delete_trivially_dead_insns (insns, max_reg_num ());
  close_dump_file (DFI_gcse2, print_rtl_with_bb, insns);

  ggc_collect ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Register allocation pre-pass, to reduce number of moves necessary
   for two-address machines.  */
static void
rest_of_handle_regmove (tree decl, rtx insns)
{
  timevar_push (TV_REGMOVE);
  open_dump_file (DFI_regmove, decl);

  regmove_optimize (insns, max_reg_num (), dump_file);

  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
  close_dump_file (DFI_regmove, print_rtl_with_bb, insns);
  timevar_pop (TV_REGMOVE);

  ggc_collect ();
}

/* Run tracer.  */
static void
rest_of_handle_tracer (tree decl, rtx insns)
{
  open_dump_file (DFI_tracer, decl);
  if (dump_file)
    dump_flow_info (dump_file);
  tracer ();
  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (insns, max_reg_num (), 0);
  close_dump_file (DFI_tracer, print_rtl_with_bb, get_insns ());
}

/* If-conversion and CFG cleanup.  */
static void
rest_of_handle_if_conversion (tree decl, rtx insns)
{
  open_dump_file (DFI_ce1, decl);
  if (flag_if_conversion)
    {
      timevar_push (TV_IFCVT);
      if (dump_file)
	dump_flow_info (dump_file);
      cleanup_cfg (CLEANUP_EXPENSIVE);
      reg_scan (insns, max_reg_num (), 0);
      if_convert (0);
      timevar_pop (TV_IFCVT);
    }
  timevar_push (TV_JUMP);
  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (insns, max_reg_num (), 0);
  timevar_pop (TV_JUMP);
  close_dump_file (DFI_ce1, print_rtl_with_bb, get_insns ());
}

/* Rerun if-conversion, as combine may have simplified things enough
   to now meet sequence length restrictions.  */
static void
rest_of_handle_if_after_combine (tree decl, rtx insns)
{
  timevar_push (TV_IFCVT);
  open_dump_file (DFI_ce2, decl);

  no_new_pseudos = 0;
  if_convert (1);
  no_new_pseudos = 1;

  close_dump_file (DFI_ce2, print_rtl_with_bb, insns);
  timevar_pop (TV_IFCVT);
}

static void
rest_of_handle_web (tree decl, rtx insns)
{
  open_dump_file (DFI_web, decl);
  timevar_push (TV_WEB);
  web_main ();
  delete_trivially_dead_insns (insns, max_reg_num ());
  cleanup_cfg (CLEANUP_EXPENSIVE);

  timevar_pop (TV_WEB);
  close_dump_file (DFI_web, print_rtl_with_bb, insns);
  reg_scan (get_insns (), max_reg_num (), 0);
}

/* Do branch profiling and static profile estimation passes.  */
static void
rest_of_handle_branch_prob (tree decl, rtx insns)
{
  struct loops loops;

  timevar_push (TV_BRANCH_PROB);
  open_dump_file (DFI_bp, decl);

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
  close_dump_file (DFI_bp, print_rtl_with_bb, insns);
  timevar_pop (TV_BRANCH_PROB);
}

/* Do optimizations based on expression value profiles.  */
static void
rest_of_handle_value_profile_transformations (tree decl, rtx insns)
{
  open_dump_file (DFI_vpt, decl);
  timevar_push (TV_VPT);

  if (value_profile_transformations ())
    cleanup_cfg (CLEANUP_EXPENSIVE);

  timevar_pop (TV_VPT);
  close_dump_file (DFI_vpt, print_rtl_with_bb, insns);
}

/* Do control and data flow analysis; write some of the results to the
   dump file.  */
static void
rest_of_handle_cfg (tree decl, rtx insns)
{
  open_dump_file (DFI_cfg, decl);
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
   */
  if (optimize)
    {
      /* Alias analysis depends on this information and mark_constant_function
       depends on alias analysis.  */
      reg_scan (insns, max_reg_num (), 1);
      mark_constant_function ();
    }

  close_dump_file (DFI_cfg, print_rtl_with_bb, insns);
}

/* Purge addressofs.  */
static void
rest_of_handle_addressof (tree decl, rtx insns)
{
  open_dump_file (DFI_addressof, decl);

  purge_addressof (insns);
  if (optimize && purge_all_dead_edges (0))
    delete_unreachable_blocks ();
  reg_scan (insns, max_reg_num (), 1);

  close_dump_file (DFI_addressof, print_rtl, insns);
}

/* Perform jump bypassing and control flow optimizations.  */
static void
rest_of_handle_jump_bypass (tree decl, rtx insns)
{
  timevar_push (TV_BYPASS);
  open_dump_file (DFI_bypass, decl);

  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (insns, max_reg_num (), 1);

  if (bypass_jumps (dump_file))
    {
      rebuild_jump_labels (insns);
      cleanup_cfg (CLEANUP_EXPENSIVE);
      delete_trivially_dead_insns (insns, max_reg_num ());
    }

  close_dump_file (DFI_bypass, print_rtl_with_bb, insns);
  timevar_pop (TV_BYPASS);

  ggc_collect ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Try combining insns through substitution.  */
static void
rest_of_handle_combine (tree decl, rtx insns)
{
  int rebuild_jump_labels_after_combine = 0;

  timevar_push (TV_COMBINE);
  open_dump_file (DFI_combine, decl);

  rebuild_jump_labels_after_combine
    = combine_instructions (insns, max_reg_num ());

  /* Combining insns may have turned an indirect jump into a
     direct jump.  Rebuild the JUMP_LABEL fields of jumping
     instructions.  */
  if (rebuild_jump_labels_after_combine)
    {
      timevar_push (TV_JUMP);
      rebuild_jump_labels (insns);
      timevar_pop (TV_JUMP);

      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
    }

  close_dump_file (DFI_combine, print_rtl_with_bb, insns);
  timevar_pop (TV_COMBINE);

  ggc_collect ();
}

/* Perform life analysis.  */
static void
rest_of_handle_life (tree decl, rtx insns)
{
  open_dump_file (DFI_life, decl);
  regclass_init ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  life_analysis (dump_file, PROP_FINAL);
  if (optimize)
    cleanup_cfg ((optimize ? CLEANUP_EXPENSIVE : 0) | CLEANUP_UPDATE_LIFE
		 | CLEANUP_LOG_LINKS
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));
  timevar_pop (TV_FLOW);

  if (extra_warnings)
    {
      setjmp_vars_warning (DECL_INITIAL (decl));
      setjmp_args_warning ();
    }

  if (optimize)
    {
      if (!flag_new_regalloc && initialize_uninitialized_subregs ())
	{
	  /* Insns were inserted, and possibly pseudos created, so
	     things might look a bit different.  */
	  insns = get_insns ();
	  allocate_reg_life_data ();
	  update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES,
			    PROP_LOG_LINKS | PROP_REG_INFO | PROP_DEATH_NOTES);
	}
    }

  no_new_pseudos = 1;

  close_dump_file (DFI_life, print_rtl_with_bb, insns);

  ggc_collect ();
}

/* Perform common subexpression elimination.  Nonzero value from
   `cse_main' means that jumps were simplified and some code may now
   be unreachable, so do jump optimization again.  */
static void
rest_of_handle_cse (tree decl, rtx insns)
{
  int tem;

  open_dump_file (DFI_cse, decl);
  if (dump_file)
    dump_flow_info (dump_file);
  timevar_push (TV_CSE);

  reg_scan (insns, max_reg_num (), 1);

  tem = cse_main (insns, max_reg_num (), 0, dump_file);
  if (tem)
    rebuild_jump_labels (insns);
  if (purge_all_dead_edges (0))
    delete_unreachable_blocks ();

  delete_trivially_dead_insns (insns, max_reg_num ());

  /* If we are not running more CSE passes, then we are no longer
     expecting CSE to be run.  But always rerun it in a cheap mode.  */
  cse_not_expected = !flag_rerun_cse_after_loop && !flag_gcse;

  if (tem || optimize > 1)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  timevar_pop (TV_CSE);
  close_dump_file (DFI_cse, print_rtl_with_bb, insns);
}

/* Run second CSE pass after loop optimizations.  */
static void
rest_of_handle_cse2 (tree decl, rtx insns)
{
  int tem;

  timevar_push (TV_CSE2);
  open_dump_file (DFI_cse2, decl);
  if (dump_file)
    dump_flow_info (dump_file);
  /* CFG is no longer maintained up-to-date.  */
  tem = cse_main (insns, max_reg_num (), 1, dump_file);

  /* Run a pass to eliminate duplicated assignments to condition code
     registers.  We have to run this after bypass_jumps, because it
     makes it harder for that pass to determine whether a jump can be
     bypassed safely.  */
  cse_condition_code_reg ();

  purge_all_dead_edges (0);
  delete_trivially_dead_insns (insns, max_reg_num ());

  if (tem)
    {
      timevar_push (TV_JUMP);
      rebuild_jump_labels (insns);
      cleanup_cfg (CLEANUP_EXPENSIVE);
      timevar_pop (TV_JUMP);
    }
  reg_scan (insns, max_reg_num (), 0);
  close_dump_file (DFI_cse2, print_rtl_with_bb, insns);
  ggc_collect ();
  timevar_pop (TV_CSE2);
}

/* Perform global cse.  */
static void
rest_of_handle_gcse (tree decl, rtx insns)
{
  int save_csb, save_cfj;
  int tem2 = 0, tem;

  timevar_push (TV_GCSE);
  open_dump_file (DFI_gcse, decl);

  tem = gcse_main (insns, dump_file);
  rebuild_jump_labels (insns);
  delete_trivially_dead_insns (insns, max_reg_num ());

  save_csb = flag_cse_skip_blocks;
  save_cfj = flag_cse_follow_jumps;
  flag_cse_skip_blocks = flag_cse_follow_jumps = 0;

  /* If -fexpensive-optimizations, re-run CSE to clean up things done
     by gcse.  */
  if (flag_expensive_optimizations)
    {
      timevar_push (TV_CSE);
      reg_scan (insns, max_reg_num (), 1);
      tem2 = cse_main (insns, max_reg_num (), 0, dump_file);
      purge_all_dead_edges (0);
      delete_trivially_dead_insns (insns, max_reg_num ());
      timevar_pop (TV_CSE);
      cse_not_expected = !flag_rerun_cse_after_loop;
    }

  /* If gcse or cse altered any jumps, rerun jump optimizations to clean
     things up.  Then possibly re-run CSE again.  */
  while (tem || tem2)
    {
      tem = tem2 = 0;
      timevar_push (TV_JUMP);
      rebuild_jump_labels (insns);
      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
      timevar_pop (TV_JUMP);

      if (flag_expensive_optimizations)
	{
	  timevar_push (TV_CSE);
	  reg_scan (insns, max_reg_num (), 1);
	  tem2 = cse_main (insns, max_reg_num (), 0, dump_file);
	  purge_all_dead_edges (0);
	  delete_trivially_dead_insns (insns, max_reg_num ());
	  timevar_pop (TV_CSE);
	}
    }

  close_dump_file (DFI_gcse, print_rtl_with_bb, insns);
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
rest_of_handle_loop_optimize (tree decl, rtx insns)
{
  int do_unroll, do_prefetch;

  timevar_push (TV_LOOP);
  delete_dead_jumptables ();
  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
  open_dump_file (DFI_loop, decl);

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();

  if (flag_unroll_loops)
    do_unroll = LOOP_AUTO_UNROLL;	/* Having two unrollers is useless.  */
  else
    do_unroll = flag_old_unroll_loops ? LOOP_UNROLL : LOOP_AUTO_UNROLL;
  do_prefetch = flag_prefetch_loop_arrays ? LOOP_PREFETCH : 0;

  if (flag_rerun_loop_opt)
    {
      cleanup_barriers ();

      /* We only want to perform unrolling once.  */
      loop_optimize (insns, dump_file, do_unroll);
      do_unroll = 0;

      /* The first call to loop_optimize makes some instructions
	 trivially dead.  We delete those instructions now in the
	 hope that doing so will make the heuristics in loop work
	 better and possibly speed up compilation.  */
      delete_trivially_dead_insns (insns, max_reg_num ());

      /* The regscan pass is currently necessary as the alias
	 analysis code depends on this information.  */
      reg_scan (insns, max_reg_num (), 1);
    }
  cleanup_barriers ();
  loop_optimize (insns, dump_file, do_unroll | do_prefetch);

  /* Loop can create trivially dead instructions.  */
  delete_trivially_dead_insns (insns, max_reg_num ());
  close_dump_file (DFI_loop, print_rtl, insns);
  timevar_pop (TV_LOOP);
  find_basic_blocks (insns, max_reg_num (), dump_file);

  ggc_collect ();
}

/* Perform loop optimizations.  It might be better to do them a bit
   sooner, but we want the profile feedback to work more
   efficiently.  */
static void
rest_of_handle_loop2 (tree decl, rtx insns)
{
  struct loops *loops;
  basic_block bb;

  if (!flag_unswitch_loops
      && !flag_peel_loops
      && !flag_unroll_loops
      && !flag_branch_on_count_reg)
    return;

  timevar_push (TV_LOOP);
  open_dump_file (DFI_loop2, decl);
  if (dump_file)
    dump_flow_info (dump_file);

  /* Initialize structures for layout changes.  */
  cfg_layout_initialize ();

  loops = loop_optimizer_init (dump_file);

  if (loops)
    {
      /* The optimizations:  */
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

  /* Finalize layout changes.  */
  FOR_EACH_BB (bb)
    if (bb->next_bb != EXIT_BLOCK_PTR)
      bb->rbi->next = bb->next_bb;
  cfg_layout_finalize ();

  cleanup_cfg (CLEANUP_EXPENSIVE);
  delete_trivially_dead_insns (insns, max_reg_num ());
  reg_scan (insns, max_reg_num (), 0);
  if (dump_file)
    dump_flow_info (dump_file);
  close_dump_file (DFI_loop2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_LOOP);
  ggc_collect ();
}

/* This is called from finish_function (within langhooks.parse_file)
   after each top-level definition is parsed.
   It is supposed to compile that function or variable
   and output the assembler code for it.
   After we return, the tree storage is freed.  */

void
rest_of_compilation (tree decl)
{
  rtx insns;

  timevar_push (TV_REST_OF_COMPILATION);

  /* Register rtl specific functions for cfg.  */
  rtl_register_cfg_hooks ();

  /* Now that we're out of the frontend, we shouldn't have any more
     CONCATs anywhere.  */
  generating_concat_p = 0;

  /* When processing delayed functions, prepare_function_start() won't
     have been run to re-initialize it.  */
  cse_not_expected = ! optimize;

  if (!cfun->dont_emit_block_notes)
    {
      /* First, make sure that NOTE_BLOCK is set correctly for each
	 NOTE_INSN_BLOCK_BEG/NOTE_INSN_BLOCK_END note.  */
      if (!cfun->x_whole_function_mode_p)
	identify_blocks ();

      /* In function-at-a-time mode, we do not attempt to keep the BLOCK
	 tree in sensible shape.  So, we just recalculate it here.  */
      if (cfun->x_whole_function_mode_p)
	reorder_blocks ();
    }
  else
    finalize_block_changes ();

  init_flow ();

  /* Dump the rtl code if we are dumping rtl.  */
  if (open_dump_file (DFI_rtl, decl))
    close_dump_file (DFI_rtl, print_rtl, get_insns ());

  /* Convert from NOTE_INSN_EH_REGION style notes, and do other
     sorts of eh initialization.  Delay this until after the
     initial rtl dump so that we can see the original nesting.  */
  convert_from_eh_region_ranges ();

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
  if (cgraph_function_possibly_inlined_p (decl))
    (*debug_hooks->outlining_inline_function) (decl);

  /* Remove any notes we don't need.  That will make iterating
     over the instruction sequence faster, and allow the garbage
     collector to reclaim the memory used by the notes.  */
  remove_unnecessary_notes ();
  if (!cfun->dont_emit_block_notes)
    reorder_blocks ();

  ggc_collect ();

  /* Initialize some variables used by the optimizers.  */
  init_function_for_compilation ();

  if (! DECL_DEFER_OUTPUT (decl))
    TREE_ASM_WRITTEN (decl) = 1;

  /* Now that integrate will no longer see our rtl, we need not
     distinguish between the return value of this function and the
     return value of called functions.  Also, we can remove all SETs
     of subregs of hard registers; they are only here because of
     integrate.  Also, we can now initialize pseudos intended to
     carry magic hard reg data throughout the function.  */
  rtx_equal_function_value_matters = 0;
  purge_hard_subreg_sets (get_insns ());

  /* Early return if there were errors.  We can run afoul of our
     consistency checks, and there's not really much point in fixing them.
     Don't return yet if -Wreturn-type; we need to do cleanup_cfg.  */
  if (((rtl_dump_and_exit || flag_syntax_only) && !warn_return_type)
      || errorcount || sorrycount)
    goto exit_rest_of_compilation;

  timevar_push (TV_JUMP);
  open_dump_file (DFI_sibling, decl);
  insns = get_insns ();
  rebuild_jump_labels (insns);
  find_exception_handler_labels ();
  find_basic_blocks (insns, max_reg_num (), dump_file);

  delete_unreachable_blocks ();

  /* Turn NOTE_INSN_PREDICTIONs into branch predictions.  */
  if (flag_guess_branch_prob)
    {
      timevar_push (TV_BRANCH_PROB);
      note_prediction_to_br_prob ();
      timevar_pop (TV_BRANCH_PROB);
    }

  timevar_pop (TV_JUMP);

  if (cfun->tail_call_emit)
    fixup_tail_calls ();

  insn_locators_initialize ();
  /* Complete generation of exception handling code.  */
  if (doing_eh (0))
    {
      timevar_push (TV_JUMP);
      open_dump_file (DFI_eh, decl);

      finish_eh_generation ();

      close_dump_file (DFI_eh, print_rtl, get_insns ());
      timevar_pop (TV_JUMP);
    }

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

  insns = get_insns ();

  /* Copy any shared structure that should not be shared.  */
  unshare_all_rtl (current_function_decl, insns);

#ifdef SETJMP_VIA_SAVE_AREA
  /* This must be performed before virtual register instantiation.
     Please be aware that everything in the compiler that can look
     at the RTL up to this point must understand that REG_SAVE_AREA
     is just like a use of the REG contained inside.  */
  if (current_function_calls_alloca)
    optimize_save_area_alloca (insns);
#endif

  /* Instantiate all virtual registers.  */
  instantiate_virtual_regs (current_function_decl, insns);

  open_dump_file (DFI_jump, decl);

  /* Always do one jump optimization pass to ensure that JUMP_LABEL fields
     are initialized and to compute whether control can drop off the end
     of the function.  */

  timevar_push (TV_JUMP);
  /* Turn NOTE_INSN_EXPECTED_VALUE into REG_BR_PROB.  Do this
     before jump optimization switches branch directions.  */
  if (flag_guess_branch_prob)
    expected_value_to_br_prob ();

  reg_scan (insns, max_reg_num (), 0);
  rebuild_jump_labels (insns);
  find_basic_blocks (insns, max_reg_num (), dump_file);
  delete_trivially_dead_insns (insns, max_reg_num ());
  if (dump_file)
    dump_flow_info (dump_file);
  cleanup_cfg ((optimize ? CLEANUP_EXPENSIVE : 0) | CLEANUP_PRE_LOOP
	       | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  create_loop_notes ();

  purge_line_number_notes (insns);

  timevar_pop (TV_JUMP);
  close_dump_file (DFI_jump, print_rtl, insns);

  /* Now is when we stop if -fsyntax-only and -Wreturn-type.  */
  if (rtl_dump_and_exit || flag_syntax_only || DECL_DEFER_OUTPUT (decl))
    goto exit_rest_of_compilation;

  timevar_push (TV_JUMP);

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  /* Jump optimization, and the removal of NULL pointer checks, may
     have reduced the number of instructions substantially.  CSE, and
     future passes, allocate arrays whose dimensions involve the
     maximum instruction UID, so if we can reduce the maximum UID
     we'll save big on memory.  */
  renumber_insns (dump_file);
  timevar_pop (TV_JUMP);

  close_dump_file (DFI_jump, print_rtl_with_bb, insns);

  ggc_collect ();

  if (optimize > 0)
    rest_of_handle_cse (decl, insns);

  rest_of_handle_addressof (decl, insns);

  ggc_collect ();

  if (optimize > 0)
    {
      if (flag_gcse)
	rest_of_handle_gcse (decl, insns);

      if (flag_loop_optimize)
	rest_of_handle_loop_optimize (decl, insns);

      if (flag_gcse)
	rest_of_handle_jump_bypass (decl, insns);
    }

  timevar_push (TV_FLOW);

  rest_of_handle_cfg (decl, insns);

  if (!flag_tree_based_profiling
      && (optimize > 0 || profile_arc_flag
	  || flag_test_coverage || flag_branch_probabilities))
    {
      rtl_register_profile_hooks ();
      rtl_register_value_prof_hooks ();
      rest_of_handle_branch_prob (decl, insns);

      if (flag_branch_probabilities
	  && flag_profile_values
	  && flag_value_profile_transformations)
	rest_of_handle_value_profile_transformations (decl, insns);

      /* Remove the death notes created for vpt.  */
      if (flag_profile_values)
	count_or_remove_death_notes (NULL, 1);
    }

  if (optimize > 0)
    rest_of_handle_if_conversion (decl, insns);

  if (flag_tracer)
    rest_of_handle_tracer (decl, insns);

  if (optimize > 0)
    rest_of_handle_loop2 (decl, insns);

  if (flag_web)
    rest_of_handle_web (decl, insns);

  if (flag_rerun_cse_after_loop)
    rest_of_handle_cse2 (decl, insns);

  cse_not_expected = 1;

  rest_of_handle_life (decl, insns);

  if (optimize > 0)
    rest_of_handle_combine (decl, insns);

  if (flag_if_conversion)
    rest_of_handle_if_after_combine (decl, insns);

  /* The optimization to partition hot/cold basic blocks into separate
     sections of the .o file does not work well with exception handling.
     Don't call it if there are exceptions. */

  if (flag_reorder_blocks_and_partition && !flag_exceptions)
    {
      no_new_pseudos = 0;
      partition_hot_cold_basic_blocks ();
      allocate_reg_life_data ();
      update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES, 
			PROP_LOG_LINKS | PROP_REG_INFO | PROP_DEATH_NOTES);
      no_new_pseudos = 1;
    }

  if (optimize > 0 && (flag_regmove || flag_expensive_optimizations))
    rest_of_handle_regmove (decl, insns);

  /* Do unconditional splitting before register allocation to allow machine
     description to add extra information not needed previously.  */
  split_all_insns (1);

#ifdef OPTIMIZE_MODE_SWITCHING
  timevar_push (TV_MODE_SWITCH);

  no_new_pseudos = 0;
  optimize_mode_switching (NULL);
  no_new_pseudos = 1;

  timevar_pop (TV_MODE_SWITCH);
#endif

  /* Any of the several passes since flow1 will have munged register
     lifetime data a bit.  We need it to be up to date for scheduling
     (see handling of reg_known_equiv in init_alias_analysis).  */
  recompute_reg_usage (insns, !optimize_size);

#ifdef INSN_SCHEDULING
  rest_of_handle_sched (decl, insns);
#endif

  /* Determine if the current function is a leaf before running reload
     since this can impact optimizations done by the prologue and
     epilogue thus changing register elimination offsets.  */
  current_function_is_leaf = leaf_function_p ();

  timevar_push (TV_LOCAL_ALLOC);
  open_dump_file (DFI_lreg, decl);

  if (flag_new_regalloc)
    {
      if (rest_of_handle_new_regalloc (decl, insns))
	goto exit_rest_of_compilation;
    }
  else
    {
      if (rest_of_handle_old_regalloc (decl, insns))
	goto exit_rest_of_compilation;
    }

  ggc_collect ();

  open_dump_file (DFI_postreload, decl);

  /* Do a very simple CSE pass over just the hard registers.  */
  if (optimize > 0)
    {
      timevar_push (TV_RELOAD_CSE_REGS);
      reload_cse_regs (insns);
      /* reload_cse_regs can eliminate potentially-trapping MEMs.
	 Remove any EH edges associated with them.  */
      if (flag_non_call_exceptions)
	purge_all_dead_edges (0);
      timevar_pop (TV_RELOAD_CSE_REGS);
    }

  close_dump_file (DFI_postreload, print_rtl_with_bb, insns);

  if (optimize > 0 && flag_gcse_after_reload)
    rest_of_handle_gcse2 (decl, insns);

  /* Re-create the death notes which were deleted during reload.  */
  timevar_push (TV_FLOW2);
  open_dump_file (DFI_flow2, decl);

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
	open_dump_file (DFI_branch_target_load, decl);

	branch_target_load_optimize (/*after_prologue_epilogue_gen=*/false);

	close_dump_file (DFI_branch_target_load, print_rtl_with_bb, insns);

	ggc_collect ();
      }

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE);

  /* On some machines, the prologue and epilogue code, or parts thereof,
     can be represented as RTL.  Doing so lets us schedule insns between
     it and the rest of the code and also allows delayed branch
     scheduling to operate in the epilogue.  */
  thread_prologue_and_epilogue_insns (insns);
  epilogue_completed = 1;

  if (optimize)
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

      ggc_collect ();
    }

  flow2_completed = 1;

  close_dump_file (DFI_flow2, print_rtl_with_bb, insns);
  timevar_pop (TV_FLOW2);

#ifdef HAVE_peephole2
  if (optimize > 0 && flag_peephole2)
    {
      timevar_push (TV_PEEPHOLE2);
      open_dump_file (DFI_peephole2, decl);

      peephole2_optimize (dump_file);

      close_dump_file (DFI_peephole2, print_rtl_with_bb, insns);
      timevar_pop (TV_PEEPHOLE2);
    }
#endif

  open_dump_file (DFI_ce3, decl);
  if (optimize)
    /* Last attempt to optimize CFG, as scheduling, peepholing and insn
       splitting possibly introduced more crossjumping opportunities.  */
    cleanup_cfg (CLEANUP_EXPENSIVE
		 | CLEANUP_UPDATE_LIFE 
		 | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0));
  if (flag_if_conversion2)
    {
      timevar_push (TV_IFCVT2);

      if_convert (1);

      timevar_pop (TV_IFCVT2);
    }
  close_dump_file (DFI_ce3, print_rtl_with_bb, insns);

  if (optimize > 0)
    {
      if (flag_rename_registers || flag_cprop_registers)
	rest_of_handle_regrename (decl, insns);

      rest_of_handle_reorder_blocks (decl, insns);
    }

  if (flag_branch_target_load_optimize2)
    {
      /* Leave this a warning for now so that it is possible to experiment
	 with running this pass twice.  In 3.6, we should either make this
	 an error, or use separate dump files.  */
      if (flag_branch_target_load_optimize)
	warning ("branch target register load optimization is not intended "
		 "to be run twice");

      open_dump_file (DFI_branch_target_load, decl);

      branch_target_load_optimize (/*after_prologue_epilogue_gen=*/true);

      close_dump_file (DFI_branch_target_load, print_rtl_with_bb, insns);

      ggc_collect ();
    }

#ifdef INSN_SCHEDULING
  if (optimize > 0 && flag_schedule_insns_after_reload)
    rest_of_handle_sched2 (decl, insns);
#endif

#ifdef LEAF_REGISTERS
  current_function_uses_only_leaf_regs
    = optimize > 0 && only_leaf_regs_used () && leaf_function_p ();
#endif

#ifdef STACK_REGS
  rest_of_handle_stack_regs (decl, insns);
#endif

  compute_alignments ();

  if (flag_var_tracking)
    rest_of_handle_variable_tracking (decl, insns);

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();

  if (targetm.machine_dependent_reorg != 0)
    rest_of_handle_machine_reorg (decl, insns);

  purge_line_number_notes (insns);
  cleanup_barriers ();

#ifdef DELAY_SLOTS
  if (optimize > 0 && flag_delayed_branch)
    rest_of_handle_delay_slots (decl, insns);
#endif

#if defined (HAVE_ATTR_length) && !defined (STACK_REGS)
  timevar_push (TV_SHORTEN_BRANCH);
  split_all_insns_noflow ();
  timevar_pop (TV_SHORTEN_BRANCH);
#endif

  convert_to_eh_region_ranges ();

  /* Shorten branches.  */
  timevar_push (TV_SHORTEN_BRANCH);
  shorten_branches (get_insns ());
  timevar_pop (TV_SHORTEN_BRANCH);

  set_nothrow_function_flags ();
  if (current_function_nothrow)
    /* Now we know that this can't throw; set the flag for the benefit
       of other functions later in this translation unit.  */
    TREE_NOTHROW (current_function_decl) = 1;

  rest_of_handle_final (decl, insns);

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
  (*debug_hooks->function_decl) (decl);
  timevar_pop (TV_SYMOUT);

 exit_rest_of_compilation:

  coverage_end_function ();

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
      if (cfun->recursive_call_emit
          && cfun->stack_alignment_needed > cfun->preferred_stack_boundary)
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

  ggc_collect ();

  timevar_pop (TV_REST_OF_COMPILATION);
}

void
init_optimization_passes (void)
{
  open_dump_file (DFI_cgraph, NULL);
  cgraph_dump_file = dump_file;
  dump_file = NULL;
}

void
finish_optimization_passes (void)
{
  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      timevar_push (TV_DUMP);
      open_dump_file (DFI_bp, NULL);

      end_branch_prob ();

      close_dump_file (DFI_bp, NULL, NULL_RTX);
      timevar_pop (TV_DUMP);
    }

  if (optimize > 0 && open_dump_file (DFI_combine, NULL))
    {
      timevar_push (TV_DUMP);
      dump_combine_total_stats (dump_file);
      close_dump_file (DFI_combine, NULL, NULL_RTX);
      timevar_pop (TV_DUMP);
    }

  dump_file = cgraph_dump_file;
  cgraph_dump_file = NULL;
  close_dump_file (DFI_cgraph, NULL, NULL_RTX);

  /* Do whatever is necessary to finish printing the graphs.  */
  if (graph_dump_format != no_graph)
    {
      int i;

      for (i = 0; i < (int) DFI_MAX; ++i)
	if (dump_file_tbl[i].initialized && dump_file_tbl[i].graph_dump_p)
	  {
	    char seq[16];
	    char *suffix;

	    sprintf (seq, DUMPFILE_FORMAT, i);
	    suffix = concat (seq, dump_file_tbl[i].extension, NULL);
	    finish_graph_dump_file (dump_base_name, suffix);
	    free (suffix);
	  }
    }

}

bool
enable_rtl_dump_file (int letter)
{
  bool matched = false;
  int i;

  if (letter == 'a')
    {
      for (i = 0; i < (int) DFI_MAX; ++i)
	dump_file_tbl[i].enabled = 1;
      matched = true;
    }
  else
    {
      for (i = 0; i < (int) DFI_MAX; ++i)
	if (letter == dump_file_tbl[i].debug_switch)
	  {
	    dump_file_tbl[i].enabled = 1;
	    matched = true;
	  }
    }

  return matched;
}
