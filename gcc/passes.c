/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "df.h"
#include "predict.h"

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

/* This is used for debugging.  It allows the current pass to printed
   from anywhere in compilation.  */
struct opt_pass *current_pass;

/* Call from anywhere to find out what pass this is.  Useful for
   printing out debugging information deep inside an service
   routine.  */
void
print_current_pass (FILE *file)
{
  if (current_pass)
    fprintf (file, "current pass = %s (%d)\n", 
	     current_pass->name, current_pass->static_pass_number);
  else
    fprintf (file, "no current pass.\n");
}


/* Call from the debugger to get the current pass name.  */
void
debug_pass (void)
{
  print_current_pass (stderr);
} 



/* Global variables used to communicate with passes.  */
int dump_flags;
bool in_gimple_form;
bool first_pass_instance;


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
  if (DECL_ASSEMBLER_NAME_SET_P (decl) && DECL_REGISTER (decl))
    make_decl_rtl (decl);

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    {
      timevar_push (TV_VARCONST);

      /* Don't output anything when a tentative file-scope definition
	 is seen.  But at end of compilation, do output code for them.

	 We do output all variables and rely on
	 callgraph code to defer them except for forward declarations
	 (see gcc.c-torture/compile/920624-1.c) */
      if ((at_end
	   || !DECL_DEFER_OUTPUT (decl)
	   || DECL_INITIAL (decl))
	  && !DECL_EXTERNAL (decl))
	{
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    varpool_finalize_decl (decl);
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
  else if (TREE_CODE (decl) == TYPE_DECL
	   /* Like in rest_of_type_compilation, avoid confusing the debug
	      information machinery when there are errors.  */
	   && !(sorrycount || errorcount))
    {
      timevar_push (TV_SYMOUT);
      debug_hooks->type_decl (decl, !top_level);
      timevar_pop (TV_SYMOUT);
    }

  /* Let cgraph know about the existence of variables.  */
  if (TREE_CODE (decl) == VAR_DECL && !DECL_EXTERNAL (decl))
    varpool_node (decl);
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



void
finish_optimization_passes (void)
{
  int i;
  struct dump_file_info *dfi;
  char *name;

  timevar_push (TV_DUMP);
  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      dump_file = dump_begin (pass_profile.pass.static_pass_number, NULL);
      end_branch_prob ();
      if (dump_file)
	dump_end (pass_profile.pass.static_pass_number, dump_file);
    }

  if (optimize > 0)
    {
      dump_file = dump_begin (pass_combine.pass.static_pass_number, NULL);
      if (dump_file)
	{
	  dump_combine_total_stats (dump_file);
          dump_end (pass_combine.pass.static_pass_number, dump_file);
	}
    }

  /* Do whatever is necessary to finish printing the graphs.  */
  if (graph_dump_format != no_graph)
    for (i = TDI_end; (dfi = get_dump_file_info (i)) != NULL; ++i)
      if (dump_initialized_p (i)
	  && (dfi->flags & TDF_GRAPH) != 0
	  && (name = get_dump_file_name (i)) != NULL)
	{
	  finish_graph_dump_file (name);
	  free (name);
	}

  timevar_pop (TV_DUMP);
}

static bool
gate_rest_of_compilation (void)
{
  /* Early return if there were errors.  We can run afoul of our
     consistency checks, and there's not really much point in fixing them.  */
  return !(rtl_dump_and_exit || flag_syntax_only || errorcount || sorrycount);
}

struct gimple_opt_pass pass_rest_of_compilation =
{
 {
  GIMPLE_PASS,
  "*rest_of_compilation",               /* name */
  gate_rest_of_compilation,             /* gate */
  NULL,                                 /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_REST_OF_COMPILATION,               /* tv_id */
  PROP_rtl,                             /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_ggc_collect                      /* todo_flags_finish */
 }
};

static bool
gate_postreload (void)
{
  return reload_completed;
}

struct rtl_opt_pass pass_postreload =
{
 {
  RTL_PASS,
  NULL,                                 /* name */
  gate_postreload,                      /* gate */
  NULL,                                 /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  PROP_rtl,                             /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_ggc_collect | TODO_verify_rtl_sharing /* todo_flags_finish */
 }
};



/* The root of the compilation pass tree, once constructed.  */
struct opt_pass *all_passes, *all_ipa_passes, *all_lowering_passes;

/* A map from static pass id to optimization pass.  */
struct opt_pass **passes_by_id;
int passes_by_id_size;

/* Set the static pass number of pass PASS to ID and record that
   in the mapping from static pass number to pass.  */

static void
set_pass_for_id (int id, struct opt_pass *pass)
{
  pass->static_pass_number = id;
  if (passes_by_id_size <= id)
    {
      passes_by_id = XRESIZEVEC (struct opt_pass *, passes_by_id, id + 1);
      memset (passes_by_id + passes_by_id_size, 0,
	      (id + 1 - passes_by_id_size) * sizeof (void *));
      passes_by_id_size = id + 1;
    }
  passes_by_id[id] = pass;
}

/* Return the pass with the static pass number ID.  */

struct opt_pass *
get_pass_for_id (int id)
{
  if (id >= passes_by_id_size)
    return NULL;
  return passes_by_id[id];
}

/* Iterate over the pass tree allocating dump file numbers.  We want
   to do this depth first, and independent of whether the pass is
   enabled or not.  */

void
register_one_dump_file (struct opt_pass *pass)
{
  char *dot_name, *flag_name, *glob_name;
  const char *prefix;
  char num[10];
  int flags, id;

  /* See below in next_pass_1.  */
  num[0] = '\0';
  if (pass->static_pass_number != -1)
    sprintf (num, "%d", ((int) pass->static_pass_number < 0
			 ? 1 : pass->static_pass_number));

  dot_name = concat (".", pass->name, num, NULL);
  if (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS)
    prefix = "ipa-", flags = TDF_IPA;
  else if (pass->type == GIMPLE_PASS)
    prefix = "tree-", flags = TDF_TREE;
  else
    prefix = "rtl-", flags = TDF_RTL;

  flag_name = concat (prefix, pass->name, num, NULL);
  glob_name = concat (prefix, pass->name, NULL);
  id = dump_register (dot_name, flag_name, glob_name, flags);
  set_pass_for_id (id, pass);
}

/* Recursive worker function for register_dump_files.  */

static int 
register_dump_files_1 (struct opt_pass *pass, int properties)
{
  do
    {
      int new_properties = (properties | pass->properties_provided)
			   & ~pass->properties_destroyed;

      if (pass->name && pass->name[0] != '*')
        register_one_dump_file (pass);

      if (pass->sub)
        new_properties = register_dump_files_1 (pass->sub, new_properties);

      /* If we have a gate, combine the properties that we could have with
         and without the pass being examined.  */
      if (pass->gate)
        properties &= new_properties;
      else
        properties = new_properties;

      pass = pass->next;
    }
  while (pass);

  return properties;
}

/* Register the dump files for the pipeline starting at PASS. 
   PROPERTIES reflects the properties that are guaranteed to be available at
   the beginning of the pipeline.  */

static void 
register_dump_files (struct opt_pass *pass,int properties)
{
  pass->properties_required |= properties;
  register_dump_files_1 (pass, properties);
}

/* Look at the static_pass_number and duplicate the pass
   if it is already added to a list. */

static struct opt_pass *
make_pass_instance (struct opt_pass *pass, bool track_duplicates)
{
  /* A nonzero static_pass_number indicates that the
     pass is already in the list.  */
  if (pass->static_pass_number)
    {
      struct opt_pass *new_pass;

      new_pass = XNEW (struct opt_pass);
      memcpy (new_pass, pass, sizeof (*new_pass));
      new_pass->next = NULL;

      new_pass->todo_flags_start &= ~TODO_mark_first_instance;

      /* Indicate to register_dump_files that this pass has duplicates,
         and so it should rename the dump file.  The first instance will
         be -1, and be number of duplicates = -static_pass_number - 1.
         Subsequent instances will be > 0 and just the duplicate number.  */
      if (pass->name || track_duplicates)
        {
          pass->static_pass_number -= 1;
          new_pass->static_pass_number = -pass->static_pass_number;
	}
      return new_pass;
    }
  else
    {
      pass->todo_flags_start |= TODO_mark_first_instance;
      pass->static_pass_number = -1;
    } 
  return pass; 
}

/* Add a pass to the pass list. Duplicate the pass if it's already
   in the list.  */

static struct opt_pass **
next_pass_1 (struct opt_pass **list, struct opt_pass *pass)
{
  *list = make_pass_instance (pass, false);
  
  return &(*list)->next;
}

/* List node for an inserted pass instance. We need to keep track of all
   the newly-added pass instances (with 'added_pass_nodes' defined below)
   so that we can register their dump files after pass-positioning is finished.
   Registering dumping files needs to be post-processed or the
   static_pass_number of the opt_pass object would be modified and mess up
   the dump file names of future pass instances to be added.  */

struct pass_list_node
{
  struct opt_pass *pass;
  struct pass_list_node *next;
};

static struct pass_list_node *added_pass_nodes = NULL;
static struct pass_list_node *prev_added_pass_node;

/* Insert the pass at the proper position. Return true if the pass 
   is successfully added.

   NEW_PASS_INFO - new pass to be inserted
   PASS_LIST - root of the pass list to insert the new pass to  */

static bool
position_pass (struct register_pass_info *new_pass_info,
               struct opt_pass **pass_list)
{
  struct opt_pass *pass = *pass_list, *prev_pass = NULL;
  bool success = false;

  for ( ; pass; prev_pass = pass, pass = pass->next)
    {
      /* Check if the current pass is of the same type as the new pass and
         matches the name and the instance number of the reference pass.  */
      if (pass->type == new_pass_info->pass->type
          && pass->name
          && !strcmp (pass->name, new_pass_info->reference_pass_name)
          && ((new_pass_info->ref_pass_instance_number == 0)
              || (new_pass_info->ref_pass_instance_number ==
                  pass->static_pass_number)
              || (new_pass_info->ref_pass_instance_number == 1
                  && pass->todo_flags_start & TODO_mark_first_instance)))
        {
          struct opt_pass *new_pass;
          struct pass_list_node *new_pass_node;

	  new_pass = make_pass_instance (new_pass_info->pass, true);
  
          /* Insert the new pass instance based on the positioning op.  */
          switch (new_pass_info->pos_op)
            {
              case PASS_POS_INSERT_AFTER:
                new_pass->next = pass->next;
                pass->next = new_pass;

		/* Skip newly inserted pass to avoid repeated
		   insertions in the case where the new pass and the
		   existing one have the same name.  */
                pass = new_pass; 
                break;
              case PASS_POS_INSERT_BEFORE:
                new_pass->next = pass;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                break;
              case PASS_POS_REPLACE:
                new_pass->next = pass->next;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                new_pass->sub = pass->sub;
                new_pass->tv_id = pass->tv_id;
                pass = new_pass;
                break;
              default:
                error ("Invalid pass positioning operation");
                return false;
            }

          /* Save the newly added pass (instance) in the added_pass_nodes
             list so that we can register its dump file later. Note that
             we cannot register the dump file now because doing so will modify
             the static_pass_number of the opt_pass object and therefore
             mess up the dump file name of future instances.  */
          new_pass_node = XCNEW (struct pass_list_node);
          new_pass_node->pass = new_pass;
          if (!added_pass_nodes)
            added_pass_nodes = new_pass_node;
          else
            prev_added_pass_node->next = new_pass_node;
          prev_added_pass_node = new_pass_node;

          success = true;
        }

      if (pass->sub && position_pass (new_pass_info, &pass->sub))
        success = true;
    }

  return success;
}

/* Hooks a new pass into the pass lists.

   PASS_INFO   - pass information that specifies the opt_pass object,
                 reference pass, instance number, and how to position
                 the pass  */

void
register_pass (struct register_pass_info *pass_info)
{
  if (!pass_info->pass)
    {
      gcc_unreachable ();
    } 

  if (!pass_info->reference_pass_name)
    {
      gcc_unreachable ();
    }

  /* Try to insert the new pass to the pass lists. We need to check all
     three lists as the reference pass could be in one (or all) of them.  */
  if (!position_pass (pass_info, &all_lowering_passes)
      && !position_pass (pass_info, &all_ipa_passes)
      && !position_pass (pass_info, &all_passes))
    gcc_unreachable ();
  else
    {
      /* OK, we have successfully inserted the new pass. We need to register
         the dump files for the newly added pass and its duplicates (if any).
         Because the registration of plugin/backend passes happens after the
         command-line options are parsed, the options that specify single
         pass dumping (e.g. -fdump-tree-PASSNAME) cannot be used for new
         passes. Therefore we currently can only enable dumping of
         new passes when the 'dump-all' flags (e.g. -fdump-tree-all)
         are specified. While doing so, we also delete the pass_list_node
         objects created during pass positioning.  */
      while (added_pass_nodes)
        {
          struct pass_list_node *next_node = added_pass_nodes->next;
          enum tree_dump_index tdi;
          register_one_dump_file (added_pass_nodes->pass);
          if (added_pass_nodes->pass->type == SIMPLE_IPA_PASS
              || added_pass_nodes->pass->type == IPA_PASS)
            tdi = TDI_ipa_all;
          else if (added_pass_nodes->pass->type == GIMPLE_PASS)
            tdi = TDI_tree_all;
          else
            tdi = TDI_rtl_all;
          /* Check if dump-all flag is specified.  */
          if (get_dump_file_info (tdi)->state)
            get_dump_file_info (added_pass_nodes->pass->static_pass_number)
                ->state = get_dump_file_info (tdi)->state;
          XDELETE (added_pass_nodes);
          added_pass_nodes = next_node;
        }
    }
}

/* Construct the pass tree.  The sequencing of passes is driven by
   the cgraph routines:

   cgraph_finalize_compilation_unit ()
       for each node N in the cgraph
	   cgraph_analyze_function (N)
	       cgraph_lower_function (N) -> all_lowering_passes

   If we are optimizing, cgraph_optimize is then invoked:

   cgraph_optimize ()
       ipa_passes () 			-> all_ipa_passes
       cgraph_expand_all_functions ()
           for each node N in the cgraph
	       cgraph_expand_function (N)
		  tree_rest_of_compilation (DECL (N))  -> all_passes
*/

void
init_optimization_passes (void)
{
  struct opt_pass **p;

#define NEXT_PASS(PASS)  (p = next_pass_1 (p, &((PASS).pass)))

 /* All passes needed to lower the function into shape optimizers can
    operate on.  These passes are always run first on the function, but
    backend might produce already lowered functions that are not processed
    by these passes.  */
  p = &all_lowering_passes;
  NEXT_PASS (pass_warn_unused_result);
  NEXT_PASS (pass_diagnose_omp_blocks);
  NEXT_PASS (pass_remove_useless_stmts);
  NEXT_PASS (pass_mudflap_1);
  NEXT_PASS (pass_lower_omp);
  NEXT_PASS (pass_lower_cf);
  NEXT_PASS (pass_refactor_eh);
  NEXT_PASS (pass_lower_eh);
  NEXT_PASS (pass_build_cfg);
  NEXT_PASS (pass_lower_complex_O0);
  NEXT_PASS (pass_lower_vector);
  NEXT_PASS (pass_warn_function_return);
  NEXT_PASS (pass_build_cgraph_edges);
  NEXT_PASS (pass_inline_parameters);
  *p = NULL;

  /* Interprocedural optimization passes.  */
  p = &all_ipa_passes;
  NEXT_PASS (pass_ipa_function_and_variable_visibility);
  NEXT_PASS (pass_ipa_early_inline);
    {
      struct opt_pass **p = &pass_ipa_early_inline.pass.sub;
      NEXT_PASS (pass_early_inline);
      NEXT_PASS (pass_inline_parameters);
      NEXT_PASS (pass_rebuild_cgraph_edges);
    }
  NEXT_PASS (pass_ipa_free_lang_data);
  NEXT_PASS (pass_early_local_passes);
    {
      struct opt_pass **p = &pass_early_local_passes.pass.sub;
      NEXT_PASS (pass_fixup_cfg);
      NEXT_PASS (pass_tree_profile);
      NEXT_PASS (pass_cleanup_cfg);
      NEXT_PASS (pass_init_datastructures);
      NEXT_PASS (pass_expand_omp);

      NEXT_PASS (pass_referenced_vars);
      NEXT_PASS (pass_build_ssa);
      NEXT_PASS (pass_early_warn_uninitialized);
      NEXT_PASS (pass_all_early_optimizations);
	{
	  struct opt_pass **p = &pass_all_early_optimizations.pass.sub;
	  NEXT_PASS (pass_rebuild_cgraph_edges);
	  NEXT_PASS (pass_early_inline);
	  NEXT_PASS (pass_remove_cgraph_callee_edges);
	  NEXT_PASS (pass_rename_ssa_copies);
	  NEXT_PASS (pass_ccp);
	  NEXT_PASS (pass_forwprop);
	  /* pass_build_ealias is a dummy pass that ensures that we
	     execute TODO_rebuild_alias at this point.  Re-building
	     alias information also rewrites no longer addressed
	     locals into SSA form if possible.  */
	  NEXT_PASS (pass_build_ealias);
	  NEXT_PASS (pass_sra_early);
	  NEXT_PASS (pass_copy_prop);
	  NEXT_PASS (pass_merge_phi);
	  NEXT_PASS (pass_cd_dce);
	  NEXT_PASS (pass_early_ipa_sra);
	  NEXT_PASS (pass_tail_recursion);
	  NEXT_PASS (pass_convert_switch);
          NEXT_PASS (pass_cleanup_eh);
          NEXT_PASS (pass_profile);
          NEXT_PASS (pass_local_pure_const);
	}
      NEXT_PASS (pass_release_ssa_names);
      NEXT_PASS (pass_rebuild_cgraph_edges);
      NEXT_PASS (pass_inline_parameters);
    }
  NEXT_PASS (pass_ipa_increase_alignment);
  NEXT_PASS (pass_ipa_matrix_reorg);
  NEXT_PASS (pass_ipa_cp);
  NEXT_PASS (pass_ipa_inline);
  NEXT_PASS (pass_ipa_reference);
  NEXT_PASS (pass_ipa_pure_const); 
  NEXT_PASS (pass_ipa_type_escape);
  NEXT_PASS (pass_ipa_pta);
  NEXT_PASS (pass_ipa_struct_reorg);  
  *p = NULL;

  /* These passes are run after IPA passes on every function that is being
     output to the assembler file.  */
  p = &all_passes;
  NEXT_PASS (pass_lower_eh_dispatch);
  NEXT_PASS (pass_all_optimizations);
    {
      struct opt_pass **p = &pass_all_optimizations.pass.sub;
      NEXT_PASS (pass_remove_cgraph_callee_edges);
      /* Initial scalar cleanups before alias computation.
	 They ensure memory accesses are not indirect wherever possible.  */
      NEXT_PASS (pass_strip_predict_hints);
      NEXT_PASS (pass_update_address_taken);
      NEXT_PASS (pass_rename_ssa_copies);
      NEXT_PASS (pass_complete_unrolli);
      NEXT_PASS (pass_ccp);
      NEXT_PASS (pass_forwprop);
      NEXT_PASS (pass_call_cdce);
      /* pass_build_alias is a dummy pass that ensures that we
	 execute TODO_rebuild_alias at this point.  Re-building
	 alias information also rewrites no longer addressed
	 locals into SSA form if possible.  */
      NEXT_PASS (pass_build_alias);
      NEXT_PASS (pass_return_slot);
      NEXT_PASS (pass_phiprop);
      NEXT_PASS (pass_fre);
      NEXT_PASS (pass_copy_prop);
      NEXT_PASS (pass_merge_phi);
      NEXT_PASS (pass_vrp);
      NEXT_PASS (pass_dce);
      NEXT_PASS (pass_cselim);
      NEXT_PASS (pass_tree_ifcombine);
      NEXT_PASS (pass_phiopt);
      NEXT_PASS (pass_tail_recursion);
      NEXT_PASS (pass_ch);
      NEXT_PASS (pass_stdarg);
      NEXT_PASS (pass_lower_complex);
      NEXT_PASS (pass_sra);
      NEXT_PASS (pass_rename_ssa_copies);
      /* The dom pass will also resolve all __builtin_constant_p calls
         that are still there to 0.  This has to be done after some
	 propagations have already run, but before some more dead code
	 is removed, and this place fits nicely.  Remember this when
	 trying to move or duplicate pass_dominator somewhere earlier.  */
      NEXT_PASS (pass_dominator);
      /* The only const/copy propagation opportunities left after
	 DOM should be due to degenerate PHI nodes.  So rather than
	 run the full propagators, run a specialized pass which
	 only examines PHIs to discover const/copy propagation
	 opportunities.  */
      NEXT_PASS (pass_phi_only_cprop);
      NEXT_PASS (pass_dse);
      NEXT_PASS (pass_reassoc);
      NEXT_PASS (pass_dce);
      NEXT_PASS (pass_forwprop);
      NEXT_PASS (pass_phiopt);
      NEXT_PASS (pass_object_sizes);
      NEXT_PASS (pass_ccp);
      NEXT_PASS (pass_copy_prop);
      NEXT_PASS (pass_cse_sincos);
      NEXT_PASS (pass_optimize_bswap);
      NEXT_PASS (pass_split_crit_edges);
      NEXT_PASS (pass_pre);
      NEXT_PASS (pass_sink_code);
      NEXT_PASS (pass_tree_loop);
	{
	  struct opt_pass **p = &pass_tree_loop.pass.sub;
	  NEXT_PASS (pass_tree_loop_init);
	  NEXT_PASS (pass_copy_prop);
	  NEXT_PASS (pass_dce_loop);
	  NEXT_PASS (pass_lim);
	  NEXT_PASS (pass_tree_unswitch);
	  NEXT_PASS (pass_scev_cprop);
	  NEXT_PASS (pass_record_bounds);
	  NEXT_PASS (pass_check_data_deps);
	  NEXT_PASS (pass_loop_distribution);
	  NEXT_PASS (pass_linear_transform);
	  NEXT_PASS (pass_graphite_transforms);
	    {
	      struct opt_pass **p = &pass_graphite_transforms.pass.sub;
	      NEXT_PASS (pass_dce_loop);
	      NEXT_PASS (pass_lim);
	    }
	  NEXT_PASS (pass_iv_canon);
	  NEXT_PASS (pass_if_conversion);
	  NEXT_PASS (pass_vectorize);
	    {
	      struct opt_pass **p = &pass_vectorize.pass.sub;
	      NEXT_PASS (pass_lower_vector_ssa);
	      NEXT_PASS (pass_dce_loop);
	    }
          NEXT_PASS (pass_predcom);
	  NEXT_PASS (pass_complete_unroll);
	  NEXT_PASS (pass_slp_vectorize);
	  NEXT_PASS (pass_parallelize_loops);
	  NEXT_PASS (pass_loop_prefetch);
	  NEXT_PASS (pass_iv_optimize);
	  NEXT_PASS (pass_tree_loop_done);
	}
      NEXT_PASS (pass_cse_reciprocals);
      NEXT_PASS (pass_convert_to_rsqrt);
      NEXT_PASS (pass_reassoc);
      NEXT_PASS (pass_vrp);
      NEXT_PASS (pass_dominator);
      /* The only const/copy propagation opportunities left after
	 DOM should be due to degenerate PHI nodes.  So rather than
	 run the full propagators, run a specialized pass which
	 only examines PHIs to discover const/copy propagation
	 opportunities.  */
      NEXT_PASS (pass_phi_only_cprop);
      NEXT_PASS (pass_cd_dce);
      NEXT_PASS (pass_tracer);

      /* FIXME: If DCE is not run before checking for uninitialized uses,
	 we may get false warnings (e.g., testsuite/gcc.dg/uninit-5.c).
	 However, this also causes us to misdiagnose cases that should be
	 real warnings (e.g., testsuite/gcc.dg/pr18501.c).
	 
	 To fix the false positives in uninit-5.c, we would have to
	 account for the predicates protecting the set and the use of each
	 variable.  Using a representation like Gated Single Assignment
	 may help.  */
      NEXT_PASS (pass_late_warn_uninitialized);
      NEXT_PASS (pass_dse);
      NEXT_PASS (pass_forwprop);
      NEXT_PASS (pass_phiopt);
      NEXT_PASS (pass_fold_builtins);
      NEXT_PASS (pass_tail_calls);
      NEXT_PASS (pass_rename_ssa_copies);
      NEXT_PASS (pass_uncprop);
      NEXT_PASS (pass_local_pure_const);
    }
  NEXT_PASS (pass_cleanup_eh);
  NEXT_PASS (pass_lower_resx);
  NEXT_PASS (pass_nrv);
  NEXT_PASS (pass_mudflap_2);
  NEXT_PASS (pass_cleanup_cfg_post_optimizing);
  NEXT_PASS (pass_warn_function_noreturn);

  NEXT_PASS (pass_expand);

  NEXT_PASS (pass_rest_of_compilation);
    {
      struct opt_pass **p = &pass_rest_of_compilation.pass.sub;
      NEXT_PASS (pass_init_function);
      NEXT_PASS (pass_jump);
      NEXT_PASS (pass_rtl_eh);
      NEXT_PASS (pass_initial_value_sets);
      NEXT_PASS (pass_unshare_all_rtl);
      NEXT_PASS (pass_instantiate_virtual_regs);
      NEXT_PASS (pass_into_cfg_layout_mode);
      NEXT_PASS (pass_jump2);
      NEXT_PASS (pass_lower_subreg);
      NEXT_PASS (pass_df_initialize_opt);
      NEXT_PASS (pass_cse);
      NEXT_PASS (pass_rtl_fwprop);
      NEXT_PASS (pass_rtl_cprop);
      NEXT_PASS (pass_rtl_pre);
      NEXT_PASS (pass_rtl_hoist);
      NEXT_PASS (pass_rtl_cprop);
      NEXT_PASS (pass_rtl_store_motion);
      NEXT_PASS (pass_cse_after_global_opts);
      NEXT_PASS (pass_rtl_ifcvt);
      /* Perform loop optimizations.  It might be better to do them a bit
	 sooner, but we want the profile feedback to work more
	 efficiently.  */
      NEXT_PASS (pass_loop2);
	{
	  struct opt_pass **p = &pass_loop2.pass.sub;
	  NEXT_PASS (pass_rtl_loop_init);
	  NEXT_PASS (pass_rtl_move_loop_invariants);
	  NEXT_PASS (pass_rtl_unswitch);
	  NEXT_PASS (pass_rtl_unroll_and_peel_loops);
	  NEXT_PASS (pass_rtl_doloop);
	  NEXT_PASS (pass_rtl_loop_done);
	  *p = NULL;
	}
      NEXT_PASS (pass_web);
      NEXT_PASS (pass_rtl_cprop);
      NEXT_PASS (pass_cse2);
      NEXT_PASS (pass_rtl_dse1);
      NEXT_PASS (pass_rtl_fwprop_addr);
      NEXT_PASS (pass_reginfo_init);
      NEXT_PASS (pass_inc_dec);
      NEXT_PASS (pass_initialize_regs);
      NEXT_PASS (pass_ud_rtl_dce);
      NEXT_PASS (pass_combine);
      NEXT_PASS (pass_if_after_combine);
      NEXT_PASS (pass_partition_blocks);
      NEXT_PASS (pass_regmove);
      NEXT_PASS (pass_outof_cfg_layout_mode);
      NEXT_PASS (pass_split_all_insns);
      NEXT_PASS (pass_lower_subreg2);
      NEXT_PASS (pass_df_initialize_no_opt);
      NEXT_PASS (pass_stack_ptr_mod);
      NEXT_PASS (pass_mode_switching);
      NEXT_PASS (pass_match_asm_constraints);
      NEXT_PASS (pass_sms);
      NEXT_PASS (pass_subregs_of_mode_init);
      NEXT_PASS (pass_sched);
      NEXT_PASS (pass_ira);
      NEXT_PASS (pass_subregs_of_mode_finish);
      NEXT_PASS (pass_postreload);
	{
	  struct opt_pass **p = &pass_postreload.pass.sub;
	  NEXT_PASS (pass_postreload_cse);
	  NEXT_PASS (pass_gcse2);
	  NEXT_PASS (pass_split_after_reload);
	  NEXT_PASS (pass_branch_target_load_optimize1);
	  NEXT_PASS (pass_thread_prologue_and_epilogue);
	  NEXT_PASS (pass_rtl_dse2);
	  NEXT_PASS (pass_stack_adjustments);
	  NEXT_PASS (pass_peephole2);
	  NEXT_PASS (pass_if_after_reload);
	  NEXT_PASS (pass_regrename);
	  NEXT_PASS (pass_cprop_hardreg);
	  NEXT_PASS (pass_fast_rtl_dce);
	  NEXT_PASS (pass_reorder_blocks);
	  NEXT_PASS (pass_branch_target_load_optimize2);
	  NEXT_PASS (pass_leaf_regs);
	  NEXT_PASS (pass_split_before_sched2);
	  NEXT_PASS (pass_sched2);
	  NEXT_PASS (pass_stack_regs);
	    {
	      struct opt_pass **p = &pass_stack_regs.pass.sub;
	      NEXT_PASS (pass_split_before_regstack);
	      NEXT_PASS (pass_stack_regs_run);
	    }
	  NEXT_PASS (pass_compute_alignments);
	  NEXT_PASS (pass_duplicate_computed_gotos);
	  NEXT_PASS (pass_variable_tracking);
	  NEXT_PASS (pass_free_cfg);
	  NEXT_PASS (pass_machine_reorg);
	  NEXT_PASS (pass_cleanup_barriers);
	  NEXT_PASS (pass_delay_slots);
	  NEXT_PASS (pass_split_for_shorten_branches);
	  NEXT_PASS (pass_convert_to_eh_region_ranges);
	  NEXT_PASS (pass_shorten_branches);
	  NEXT_PASS (pass_set_nothrow_function_flags);
	  NEXT_PASS (pass_final);
	}
      NEXT_PASS (pass_df_finish);
    }
  NEXT_PASS (pass_clean_state);
  *p = NULL;

#undef NEXT_PASS

  /* Register the passes with the tree dump code.  */
  register_dump_files (all_lowering_passes, PROP_gimple_any);
  register_dump_files (all_ipa_passes, 
		       PROP_gimple_any | PROP_gimple_lcf | PROP_gimple_leh
		       | PROP_cfg);
  register_dump_files (all_passes, 
		       PROP_gimple_any | PROP_gimple_lcf | PROP_gimple_leh
		       | PROP_cfg);
}

/* If we are in IPA mode (i.e., current_function_decl is NULL), call
   function CALLBACK for every function in the call graph.  Otherwise,
   call CALLBACK on the current function.  */ 

static void
do_per_function (void (*callback) (void *data), void *data)
{
  if (current_function_decl)
    callback (data);
  else
    {
      struct cgraph_node *node;
      for (node = cgraph_nodes; node; node = node->next)
	if (node->analyzed && gimple_has_body_p (node->decl)
	    && (!node->clone_of || node->decl != node->clone_of->decl))
	  {
	    push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	    current_function_decl = node->decl;
	    callback (data);
	    free_dominance_info (CDI_DOMINATORS);
	    free_dominance_info (CDI_POST_DOMINATORS);
	    current_function_decl = NULL;
	    pop_cfun ();
	    ggc_collect ();
	  }
    }
}

/* Because inlining might remove no-longer reachable nodes, we need to
   keep the array visible to garbage collector to avoid reading collected
   out nodes.  */
static int nnodes;
static GTY ((length ("nnodes"))) struct cgraph_node **order;

/* If we are in IPA mode (i.e., current_function_decl is NULL), call
   function CALLBACK for every function in the call graph.  Otherwise,
   call CALLBACK on the current function.  */ 

static void
do_per_function_toporder (void (*callback) (void *data), void *data)
{
  int i;

  if (current_function_decl)
    callback (data);
  else
    {
      gcc_assert (!order);
      order = GGC_NEWVEC (struct cgraph_node *, cgraph_n_nodes);
      nnodes = cgraph_postorder (order);
      for (i = nnodes - 1; i >= 0; i--)
        order[i]->process = 1;
      for (i = nnodes - 1; i >= 0; i--)
	{
	  struct cgraph_node *node = order[i];

	  /* Allow possibly removed nodes to be garbage collected.  */
	  order[i] = NULL;
	  node->process = 0;
	  if (node->analyzed && (node->needed || node->reachable))
	    {
	      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	      current_function_decl = node->decl;
	      callback (data);
	      free_dominance_info (CDI_DOMINATORS);
	      free_dominance_info (CDI_POST_DOMINATORS);
	      current_function_decl = NULL;
	      pop_cfun ();
	      ggc_collect ();
	    }
	}
    }
  ggc_free (order);
  order = NULL;
  nnodes = 0;
}

/* Perform all TODO actions that ought to be done on each function.  */

static void
execute_function_todo (void *data)
{
  unsigned int flags = (size_t)data;
  if (cfun->curr_properties & PROP_ssa)
    flags |= TODO_verify_ssa;
  flags &= ~cfun->last_verified;
  if (!flags)
    return;

  statistics_fini_pass ();

  /* Always cleanup the CFG before trying to update SSA.  */
  if (flags & TODO_cleanup_cfg)
    {
      bool cleanup = cleanup_tree_cfg ();

      if (cleanup && (cfun->curr_properties & PROP_ssa))
	flags |= TODO_remove_unused_locals;
	
      /* When cleanup_tree_cfg merges consecutive blocks, it may
	 perform some simplistic propagation when removing single
	 valued PHI nodes.  This propagation may, in turn, cause the
	 SSA form to become out-of-date (see PR 22037).  So, even
	 if the parent pass had not scheduled an SSA update, we may
	 still need to do one.  */
      if (!(flags & TODO_update_ssa_any) && need_ssa_update_p (cfun))
	flags |= TODO_update_ssa;
    }

  if (flags & TODO_update_ssa_any)
    {
      unsigned update_flags = flags & TODO_update_ssa_any;
      update_ssa (update_flags);
      cfun->last_verified &= ~TODO_verify_ssa;
    }
  
  if (flags & TODO_update_address_taken)
    execute_update_addresses_taken (true);

  if (flags & TODO_rebuild_alias)
    {
      if (!(flags & TODO_update_address_taken))
	execute_update_addresses_taken (true);
      compute_may_aliases ();
    }
  
  if (flags & TODO_remove_unused_locals)
    remove_unused_locals ();

  if ((flags & TODO_dump_func) && dump_file && current_function_decl)
    {
      if (cfun->curr_properties & PROP_trees)
        dump_function_to_file (current_function_decl, dump_file, dump_flags);
      else
	{
	  if (dump_flags & TDF_SLIM)
	    print_rtl_slim_with_bb (dump_file, get_insns (), dump_flags);
	  else if ((cfun->curr_properties & PROP_cfg)
		   && (dump_flags & TDF_BLOCKS))
	    print_rtl_with_bb (dump_file, get_insns ());
          else
	    print_rtl (dump_file, get_insns ());

	  if ((cfun->curr_properties & PROP_cfg)
	      && graph_dump_format != no_graph
	      && (dump_flags & TDF_GRAPH))
	    print_rtl_graph_with_bb (dump_file_name, get_insns ());
	}

      /* Flush the file.  If verification fails, we won't be able to
	 close the file before aborting.  */
      fflush (dump_file);
    }

  if (flags & TODO_rebuild_frequencies)
    {
      if (profile_status == PROFILE_GUESSED)
	{
	  loop_optimizer_init (0);
	  add_noreturn_fake_exit_edges ();
	  mark_irreducible_loops ();
	  connect_infinite_loops_to_exit ();
	  estimate_bb_frequencies ();
	  remove_fake_exit_edges ();
	  loop_optimizer_finalize ();
	}
      else if (profile_status == PROFILE_READ)
	counts_to_freqs ();
      else
	gcc_unreachable ();
    }

#if defined ENABLE_CHECKING
  if (flags & TODO_verify_ssa)
    verify_ssa (true);
  if (flags & TODO_verify_flow)
    verify_flow_info ();
  if (flags & TODO_verify_stmts)
    verify_stmts ();
  if (flags & TODO_verify_loops)
    verify_loop_closed_ssa ();
  if (flags & TODO_verify_rtl_sharing)
    verify_rtl_sharing ();
#endif

  cfun->last_verified = flags & TODO_verify_all;
}

/* Perform all TODO actions.  */
static void
execute_todo (unsigned int flags)
{
#if defined ENABLE_CHECKING
  if (cfun
      && need_ssa_update_p (cfun))
    gcc_assert (flags & TODO_update_ssa_any);
#endif

  /* Inform the pass whether it is the first time it is run.  */
  first_pass_instance = (flags & TODO_mark_first_instance) != 0;

  do_per_function (execute_function_todo, (void *)(size_t) flags);

  /* Always remove functions just as before inlining: IPA passes might be
     interested to see bodies of extern inline functions that are not inlined
     to analyze side effects.  The full removal is done just at the end
     of IPA pass queue.  */
  if (flags & TODO_remove_functions)
    {
      gcc_assert (!cfun);
      cgraph_remove_unreachable_nodes (true, dump_file);
    }

  if ((flags & TODO_dump_cgraph) && dump_file && !current_function_decl)
    {
      gcc_assert (!cfun);
      dump_cgraph (dump_file);
      /* Flush the file.  If verification fails, we won't be able to
	 close the file before aborting.  */
      fflush (dump_file);
    }

  if (flags & TODO_ggc_collect)
    ggc_collect ();

  /* Now that the dumping has been done, we can get rid of the optional 
     df problems.  */
  if (flags & TODO_df_finish)
    df_finish_pass ((flags & TODO_df_verify) != 0);
}

/* Verify invariants that should hold between passes.  This is a place
   to put simple sanity checks.  */

static void
verify_interpass_invariants (void)
{
#ifdef ENABLE_CHECKING
  gcc_assert (!fold_deferring_overflow_warnings_p ());
#endif
}

/* Clear the last verified flag.  */

static void
clear_last_verified (void *data ATTRIBUTE_UNUSED)
{
  cfun->last_verified = 0;
}

/* Helper function. Verify that the properties has been turn into the
   properties expected by the pass.  */

#ifdef ENABLE_CHECKING
static void
verify_curr_properties (void *data)
{
  unsigned int props = (size_t)data;
  gcc_assert ((cfun->curr_properties & props) == props);
}
#endif

/* Initialize pass dump file.  */

static bool
pass_init_dump_file (struct opt_pass *pass)
{
  /* If a dump file name is present, open it if enabled.  */
  if (pass->static_pass_number != -1)
    {
      bool initializing_dump = !dump_initialized_p (pass->static_pass_number);
      dump_file_name = get_dump_file_name (pass->static_pass_number);
      dump_file = dump_begin (pass->static_pass_number, &dump_flags);
      if (dump_file && current_function_decl)
	{
	  const char *dname, *aname;
	  dname = lang_hooks.decl_printable_name (current_function_decl, 2);
	  aname = (IDENTIFIER_POINTER
		   (DECL_ASSEMBLER_NAME (current_function_decl)));
	  fprintf (dump_file, "\n;; Function %s (%s)%s\n\n", dname, aname,
	     cfun->function_frequency == FUNCTION_FREQUENCY_HOT
	     ? " (hot)"
	     : cfun->function_frequency == FUNCTION_FREQUENCY_UNLIKELY_EXECUTED
	     ? " (unlikely executed)"
	     : "");
	}
      return initializing_dump;
    }
  else
    return false;
}

/* Flush PASS dump file.  */

static void
pass_fini_dump_file (struct opt_pass *pass)
{
  /* Flush and close dump file.  */
  if (dump_file_name)
    {
      free (CONST_CAST (char *, dump_file_name));
      dump_file_name = NULL;
    }

  if (dump_file)
    {
      dump_end (pass->static_pass_number, dump_file);
      dump_file = NULL;
    }
}

/* After executing the pass, apply expected changes to the function
   properties. */

static void
update_properties_after_pass (void *data)
{
  struct opt_pass *pass = (struct opt_pass *) data;
  cfun->curr_properties = (cfun->curr_properties | pass->properties_provided)
		           & ~pass->properties_destroyed;
}

/* Schedule IPA transform pass DATA for CFUN.  */

static void
add_ipa_transform_pass (void *data)
{
  struct ipa_opt_pass_d *ipa_pass = (struct ipa_opt_pass_d *) data;
  VEC_safe_push (ipa_opt_pass, heap, cfun->ipa_transforms_to_apply, ipa_pass);
}

/* Execute summary generation for all of the passes in IPA_PASS.  */

static void
execute_ipa_summary_passes (struct ipa_opt_pass_d *ipa_pass)
{
  while (ipa_pass)
    {
      struct opt_pass *pass = &ipa_pass->pass;

      /* Execute all of the IPA_PASSes in the list.  */
      if (ipa_pass->pass.type == IPA_PASS 
	  && (!pass->gate || pass->gate ()))
	{
	  pass_init_dump_file (pass);
	  ipa_pass->generate_summary ();
	  pass_fini_dump_file (pass);
	}
      ipa_pass = (struct ipa_opt_pass_d *)ipa_pass->pass.next;
    }
}

/* Execute IPA_PASS function transform on NODE.  */

static void
execute_one_ipa_transform_pass (struct cgraph_node *node,
				struct ipa_opt_pass_d *ipa_pass)
{
  struct opt_pass *pass = &ipa_pass->pass;
  unsigned int todo_after = 0;

  current_pass = pass;
  if (!ipa_pass->function_transform)
    return;

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (cfun && (cfun->curr_properties & PROP_trees)) != 0;

  pass_init_dump_file (pass);

  /* Run pre-pass verification.  */
  execute_todo (ipa_pass->function_transform_todo_flags_start);

  /* If a timevar is present, start it.  */
  if (pass->tv_id != TV_NONE)
    timevar_push (pass->tv_id);

  /* Do it!  */
  todo_after = ipa_pass->function_transform (node);

  /* Stop timevar.  */
  if (pass->tv_id != TV_NONE)
    timevar_pop (pass->tv_id);

  /* Run post-pass cleanup and verification.  */
  execute_todo (todo_after);
  verify_interpass_invariants ();

  pass_fini_dump_file (pass);

  current_pass = NULL;
}

static bool
execute_one_pass (struct opt_pass *pass)
{
  bool initializing_dump;
  unsigned int todo_after = 0;

  /* IPA passes are executed on whole program, so cfun should be NULL.
     Other passes need function context set.  */
  if (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS)
    gcc_assert (!cfun && !current_function_decl);
  else
    gcc_assert (cfun && current_function_decl);

  if (cfun && cfun->ipa_transforms_to_apply)
    {
      unsigned int i;
      struct cgraph_node *node = cgraph_node (current_function_decl);

      for (i = 0; i < VEC_length (ipa_opt_pass, cfun->ipa_transforms_to_apply);
	   i++)
	execute_one_ipa_transform_pass (node,
				        VEC_index (ipa_opt_pass,
						   cfun->ipa_transforms_to_apply,
						   i));
      VEC_free (ipa_opt_pass, heap, cfun->ipa_transforms_to_apply);
      cfun->ipa_transforms_to_apply = NULL;
    }

  current_pass = pass;

  /* See if we're supposed to run this pass.  */
  if (pass->gate && !pass->gate ())
    return false;

  if (!quiet_flag && !cfun)
    fprintf (stderr, " <%s>", pass->name ? pass->name : "");

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (cfun && (cfun->curr_properties & PROP_trees)) != 0;

  initializing_dump = pass_init_dump_file (pass);

  /* Run pre-pass verification.  */
  execute_todo (pass->todo_flags_start);

#ifdef ENABLE_CHECKING
  do_per_function (verify_curr_properties,
		   (void *)(size_t)pass->properties_required);
#endif

  /* If a timevar is present, start it.  */
  if (pass->tv_id != TV_NONE)
    timevar_push (pass->tv_id);

  /* Do it!  */
  if (pass->execute)
    {
      todo_after = pass->execute ();
      do_per_function (clear_last_verified, NULL);
    }

  /* Stop timevar.  */
  if (pass->tv_id != TV_NONE)
    timevar_pop (pass->tv_id);

  do_per_function (update_properties_after_pass, pass);

  if (initializing_dump
      && dump_file
      && graph_dump_format != no_graph
      && cfun
      && (cfun->curr_properties & (PROP_cfg | PROP_rtl))
	  == (PROP_cfg | PROP_rtl))
    {
      get_dump_file_info (pass->static_pass_number)->flags |= TDF_GRAPH;
      dump_flags |= TDF_GRAPH;
      clean_graph_dump_file (dump_file_name);
    }

  /* Run post-pass cleanup and verification.  */
  execute_todo (todo_after | pass->todo_flags_finish);
  verify_interpass_invariants ();
  if (pass->type == IPA_PASS)
    do_per_function (add_ipa_transform_pass, pass);

  if (!current_function_decl)
    cgraph_process_new_functions ();

  pass_fini_dump_file (pass);

  if (pass->type != SIMPLE_IPA_PASS && pass->type != IPA_PASS)
    gcc_assert (!(cfun->curr_properties & PROP_trees)
		|| pass->type != RTL_PASS);

  current_pass = NULL;

  return true;
}

void
execute_pass_list (struct opt_pass *pass)
{
  do
    {
      gcc_assert (pass->type == GIMPLE_PASS
		  || pass->type == RTL_PASS);
      if (execute_one_pass (pass) && pass->sub)
        execute_pass_list (pass->sub);
      pass = pass->next;
    }
  while (pass);
}

/* Same as execute_pass_list but assume that subpasses of IPA passes
   are local passes.  */
void
execute_ipa_pass_list (struct opt_pass *pass)
{
  bool summaries_generated = false;
  do
    {
      gcc_assert (!current_function_decl);
      gcc_assert (!cfun);
      gcc_assert (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS);
      if (pass->type == IPA_PASS && (!pass->gate || pass->gate ()))
	{
	  if (!summaries_generated)
	    {
	      if (!quiet_flag && !cfun)
		fprintf (stderr, " <summary generate>");
	      execute_ipa_summary_passes ((struct ipa_opt_pass_d *) pass);
	    }
	  summaries_generated = true;
	}
      if (execute_one_pass (pass) && pass->sub)
	{
	  if (pass->sub->type == GIMPLE_PASS)
	    do_per_function_toporder ((void (*)(void *))execute_pass_list,
				      pass->sub);
	  else if (pass->sub->type == SIMPLE_IPA_PASS
		   || pass->sub->type == IPA_PASS)
	    execute_ipa_pass_list (pass->sub);
	  else
	    gcc_unreachable ();
	}
      if (!current_function_decl)
	cgraph_process_new_functions ();
      pass = pass->next;
    }
  while (pass);
}

/* Called by local passes to see if function is called by already processed nodes.
   Because we process nodes in topological order, this means that function is
   in recursive cycle or we introduced new direct calls.  */
bool
function_called_by_processed_nodes_p (void)
{
  struct cgraph_edge *e;
  for (e = cgraph_node (current_function_decl)->callers; e; e = e->next_caller)
    {
      if (e->caller->decl == current_function_decl)
        continue;
      if (!e->caller->analyzed || (!e->caller->needed && !e->caller->reachable))
        continue;
      if (TREE_ASM_WRITTEN (e->caller->decl))
        continue;
      if (!e->caller->process && !e->caller->global.inlined_to)
      	break;
    }
  if (dump_file && e)
    {
      fprintf (dump_file, "Already processed call to:\n");
      dump_cgraph_node (dump_file, e->caller);
    }
  return e != NULL;
}

#include "gt-passes.h"
