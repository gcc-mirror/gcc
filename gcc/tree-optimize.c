/* Top-level control of tree optimizations.
   Copyright 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
#include "expr.h"
#include "diagnostic.h"
#include "basic-block.h"
#include "flags.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "function.h"
#include "langhooks.h"
#include "toplev.h"
#include "flags.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "ggc.h"
#include "cgraph.h"
#include "graph.h"


/* Global variables used to communicate with passes.  */
int dump_flags;
bitmap vars_to_rename;
bool in_gimple_form;

/* The root of the compilation pass tree, once constructed.  */
static struct tree_opt_pass *all_passes;

/* Pass: dump the gimplified, inlined, functions.  */

static struct tree_opt_pass pass_gimple = 
{
  "gimple",				/* name */
  NULL,					/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  0,					/* properties_required */
  PROP_gimple_any,			/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func,			/* todo_flags_finish */
  0					/* letter */
};

/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_optimizations (void)
{
  return (optimize >= 1
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

static struct tree_opt_pass pass_all_optimizations =
{
  NULL,					/* name */
  gate_all_optimizations,		/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};

/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static void 
execute_cleanup_cfg_post_optimizing (void)
{
  cleanup_tree_cfg ();
  cleanup_dead_labels ();
  group_case_labels ();
}

static struct tree_opt_pass pass_cleanup_cfg_post_optimizing =
{
  "final_cleanup",			/* name */
  NULL,					/* gate */
  execute_cleanup_cfg_post_optimizing,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func,					/* todo_flags_finish */
  0					/* letter */
};

/* Pass: do the actions required to finish with tree-ssa optimization
   passes.  */

static void
execute_free_datastructures (void)
{
  tree *chain;

  /* ??? This isn't the right place for this.  Worse, it got computed
     more or less at random in various passes.  */
  free_dominance_info (CDI_DOMINATORS);

  /* Emit gotos for implicit jumps.  */
  disband_implicit_edges ();

  /* Remove the ssa structures.  Do it here since this includes statement
     annotations that need to be intact during disband_implicit_edges.  */
  delete_tree_ssa ();

  /* Re-chain the statements from the blocks.  */
  chain = &DECL_SAVED_TREE (current_function_decl);
  *chain = alloc_stmt_list ();

  /* And get rid of annotations we no longer need.  */
  delete_tree_cfg_annotations ();
}

static struct tree_opt_pass pass_free_datastructures =
{
  NULL,					/* name */
  NULL,					/* gate */
  execute_free_datastructures,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};


/* Do the actions required to initialize internal data structures used
   in tree-ssa optimization passes.  */

static void
execute_init_datastructures (void)
{
  /* Allocate hash tables, arrays and other structures.  */
  init_tree_ssa ();
}

static struct tree_opt_pass pass_init_datastructures =
{
  NULL,					/* name */
  NULL,					/* gate */
  execute_init_datastructures,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};

/* Iterate over the pass tree allocating dump file numbers.  We want
   to do this depth first, and independent of whether the pass is
   enabled or not.  */

static void
register_one_dump_file (struct tree_opt_pass *pass, int n)
{
  char *dot_name, *flag_name, *glob_name;
  char num[10];

  /* See below in next_pass_1.  */
  num[0] = '\0';
  if (pass->static_pass_number != -1)
    sprintf (num, "%d", ((int) pass->static_pass_number < 0
			 ? 1 : pass->static_pass_number));

  dot_name = concat (".", pass->name, num, NULL);
  if (pass->properties_provided & PROP_trees)
    {
      flag_name = concat ("tree-", pass->name, num, NULL);
      glob_name = concat ("tree-", pass->name, NULL);
      pass->static_pass_number = dump_register (dot_name, flag_name, glob_name,
                                                TDF_TREE, n + TDI_tree_all, 0);
    }
  else
    {
      flag_name = concat ("rtl-", pass->name, num, NULL);
      glob_name = concat ("rtl-", pass->name, NULL);
      pass->static_pass_number = dump_register (dot_name, flag_name, glob_name,
                                                TDF_RTL, n, pass->letter);
    }
}

static int 
register_dump_files (struct tree_opt_pass *pass, int properties)
{
  static int n = 0;
  do
    {
      int new_properties;
      int pass_number;

      pass->properties_required = properties;
      new_properties =
        (properties | pass->properties_provided) & ~pass->properties_destroyed;

      /* Reset the counter when we reach RTL-based passes.  */
      if ((pass->properties_provided ^ pass->properties_required) & PROP_rtl)
        n = 0;

      pass_number = n;
      if (pass->name)
        n++;

      if (pass->sub)
        new_properties = register_dump_files (pass->sub, new_properties);

      /* If we have a gate, combine the properties that we could have with
         and without the pass being examined.  */
      if (pass->gate)
        properties &= new_properties;
      else
        properties = new_properties;

      pass->properties_provided = properties;
      if (pass->name)
        register_one_dump_file (pass, pass_number);

      pass = pass->next;
    }
  while (pass);

  return properties;
}

/* Add a pass to the pass list. Duplicate the pass if it's already
   in the list.  */

static struct tree_opt_pass **
next_pass_1 (struct tree_opt_pass **list, struct tree_opt_pass *pass)
{

  /* A nonzero static_pass_number indicates that the
     pass is already in the list.  */
  if (pass->static_pass_number)
    {
      struct tree_opt_pass *new;

      new = xmalloc (sizeof (*new));
      memcpy (new, pass, sizeof (*new));

      /* Indicate to register_dump_files that this pass has duplicates,
         and so it should rename the dump file.  The first instance will
         be -1, and be number of duplicates = -static_pass_number - 1.
         Subsequent instances will be > 0 and just the duplicate number.  */
      if (pass->name)
        {
          pass->static_pass_number -= 1;
          new->static_pass_number = -pass->static_pass_number;
	}
      
      *list = new;
    }
  else
    {
      pass->static_pass_number = -1;
      *list = pass;
    }  
  
  return &(*list)->next;
          
}

/* Construct the pass tree.  */

void
init_tree_optimization_passes (void)
{
  struct tree_opt_pass **p;

#define NEXT_PASS(PASS)  (p = next_pass_1 (p, &PASS))

  p = &all_passes;
  NEXT_PASS (pass_gimple);
  NEXT_PASS (pass_remove_useless_stmts);
  NEXT_PASS (pass_mudflap_1);
  NEXT_PASS (pass_lower_cf);
  NEXT_PASS (pass_lower_eh);
  NEXT_PASS (pass_build_cfg);
  NEXT_PASS (pass_pre_expand);
  NEXT_PASS (pass_tree_profile);
  NEXT_PASS (pass_init_datastructures);
  NEXT_PASS (pass_all_optimizations);
  NEXT_PASS (pass_warn_function_return);
  NEXT_PASS (pass_mudflap_2);
  NEXT_PASS (pass_free_datastructures);
  NEXT_PASS (pass_expand);
  NEXT_PASS (pass_rest_of_compilation);
  *p = NULL;

  p = &pass_all_optimizations.sub;
  NEXT_PASS (pass_referenced_vars);
  NEXT_PASS (pass_build_ssa);
  NEXT_PASS (pass_may_alias);
  NEXT_PASS (pass_rename_ssa_copies);
  NEXT_PASS (pass_early_warn_uninitialized);
  NEXT_PASS (pass_dce);
  NEXT_PASS (pass_dominator);
  NEXT_PASS (pass_redundant_phi);
  NEXT_PASS (pass_dce);
  NEXT_PASS (pass_merge_phi);
  NEXT_PASS (pass_forwprop);
  NEXT_PASS (pass_phiopt);
  NEXT_PASS (pass_may_alias);
  NEXT_PASS (pass_tail_recursion);
  NEXT_PASS (pass_ch);
  NEXT_PASS (pass_profile);
  NEXT_PASS (pass_sra);
  /* FIXME: SRA may generate arbitrary gimple code, exposing new
     aliased and call-clobbered variables.  As mentioned below,
     pass_may_alias should be a TODO item.  */
  NEXT_PASS (pass_may_alias);
  NEXT_PASS (pass_rename_ssa_copies);
  NEXT_PASS (pass_dominator);
  NEXT_PASS (pass_redundant_phi);
  NEXT_PASS (pass_dce);
  NEXT_PASS (pass_dse);
  NEXT_PASS (pass_may_alias);
  NEXT_PASS (pass_forwprop);
  NEXT_PASS (pass_phiopt);
  NEXT_PASS (pass_ccp);
  NEXT_PASS (pass_redundant_phi);
  NEXT_PASS (pass_fold_builtins);
  /* FIXME: May alias should a TODO but for 4.0.0,
     we add may_alias right after fold builtins
     which can create arbitrary GIMPLE.  */
  NEXT_PASS (pass_may_alias);
  NEXT_PASS (pass_split_crit_edges);
  NEXT_PASS (pass_pre);
  NEXT_PASS (pass_loop);
  NEXT_PASS (pass_dominator);
  NEXT_PASS (pass_redundant_phi);
  /* FIXME: If DCE is not run before checking for uninitialized uses,
     we may get false warnings (e.g., testsuite/gcc.dg/uninit-5.c).
     However, this also causes us to misdiagnose cases that should be
     real warnings (e.g., testsuite/gcc.dg/pr18501.c).
     
     To fix the false positives in uninit-5.c, we would have to
     account for the predicates protecting the set and the use of each
     variable.  Using a representation like Gated Single Assignment
     may help.  */
  NEXT_PASS (pass_late_warn_uninitialized);
  NEXT_PASS (pass_cd_dce);
  NEXT_PASS (pass_dse);
  NEXT_PASS (pass_forwprop);
  NEXT_PASS (pass_phiopt);
  NEXT_PASS (pass_tail_calls);
  NEXT_PASS (pass_rename_ssa_copies);
  NEXT_PASS (pass_del_ssa);
  NEXT_PASS (pass_nrv);
  NEXT_PASS (pass_remove_useless_vars);
  NEXT_PASS (pass_mark_used_blocks);
  NEXT_PASS (pass_cleanup_cfg_post_optimizing);
  *p = NULL;

  p = &pass_loop.sub;
  NEXT_PASS (pass_loop_init);
  NEXT_PASS (pass_lim);
  NEXT_PASS (pass_unswitch);
  NEXT_PASS (pass_record_bounds);
  NEXT_PASS (pass_linear_transform);
  NEXT_PASS (pass_iv_canon);
  NEXT_PASS (pass_if_conversion);
  NEXT_PASS (pass_vectorize);
  NEXT_PASS (pass_complete_unroll);
  NEXT_PASS (pass_iv_optimize);
  NEXT_PASS (pass_loop_done);
  *p = NULL;

#undef NEXT_PASS

  /* Register the passes with the tree dump code.  */
  register_dump_files (all_passes, 0);
}

static void execute_pass_list (struct tree_opt_pass *);

static unsigned int last_verified;

static void
execute_todo (int properties, unsigned int flags)
{
  if (flags & TODO_rename_vars)
    {
      rewrite_into_ssa (false);
      bitmap_clear (vars_to_rename);
    }
  if (flags & TODO_fix_def_def_chains)
    {
      rewrite_def_def_chains ();
      bitmap_clear (vars_to_rename);
    }

  if (flags & TODO_cleanup_cfg)
    cleanup_tree_cfg ();

  if ((flags & TODO_dump_func) && dump_file)
    {
      if (properties & PROP_trees)
        dump_function_to_file (current_function_decl,
                               dump_file, dump_flags);
      else if (properties & PROP_cfg)
        print_rtl_with_bb (dump_file, get_insns ());
      else
        print_rtl (dump_file, get_insns ());

      /* Flush the file.  If verification fails, we won't be able to
	 close the file before aborting.  */
      fflush (dump_file);
    }

  if (flags & TODO_ggc_collect)
    ggc_collect ();

#ifdef ENABLE_CHECKING
  if (flags & TODO_verify_ssa)
    verify_ssa ();
  if (flags & TODO_verify_flow)
    verify_flow_info ();
  if (flags & TODO_verify_stmts)
    verify_stmts ();
#endif
}

static bool
execute_one_pass (struct tree_opt_pass *pass)
{
  unsigned int todo; 

  /* See if we're supposed to run this pass.  */
  if (pass->gate && !pass->gate ())
    return false;

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (pass->properties_provided & PROP_trees) != 0;

  /* Run pre-pass verification.  */
  todo = pass->todo_flags_start & ~last_verified;
  if (todo)
    execute_todo (pass->properties_required, todo);

  /* If a dump file name is present, open it if enabled.  */
  if (pass->static_pass_number != -1)
    {
      bool initializing_dump = !dump_initialized_p (pass->static_pass_number);
      dump_file_name = get_dump_file_name (pass->static_pass_number);
      dump_file = dump_begin (pass->static_pass_number, &dump_flags);
      if (dump_file)
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

      if (initializing_dump
          && graph_dump_format != no_graph
	  && (pass->properties_provided & (PROP_cfg | PROP_rtl))
	      == (PROP_cfg | PROP_rtl))
        clean_graph_dump_file (dump_file_name);
    }

  /* If a timevar is present, start it.  */
  if (pass->tv_id)
    timevar_push (pass->tv_id);

  /* Do it!  */
  if (pass->execute)
    pass->execute ();

  /* Stop timevar.  */
  if (pass->tv_id)
    timevar_pop (pass->tv_id);

  if (dump_file
      && (pass->properties_provided & (PROP_cfg | PROP_rtl))
	  == (PROP_cfg | PROP_rtl))
    print_rtl_with_bb (dump_file, get_insns ());

  /* Run post-pass cleanup and verification.  */
  todo = pass->todo_flags_finish;
  last_verified = todo & TODO_verify_all;
  if (todo)
    execute_todo (pass->properties_provided, todo);

  /* Flush and close dump file.  */
  if (dump_file_name)
    {
      free ((char *) dump_file_name);
      dump_file_name = NULL;
    }
  if (dump_file)
    {
      dump_end (pass->static_pass_number, dump_file);
      dump_file = NULL;
    }

  return true;
}

static void
execute_pass_list (struct tree_opt_pass *pass)
{
  do
    {
      if (execute_one_pass (pass) && pass->sub)
	execute_pass_list (pass->sub);
      pass = pass->next;
    }
  while (pass);
}


/* Update recursively all inlined_to pointers of functions
   inlined into NODE to INLINED_TO.  */
static void
update_inlined_to_pointers (struct cgraph_node *node,
			    struct cgraph_node *inlined_to)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      if (e->callee->global.inlined_to)
	{
	  e->callee->global.inlined_to = inlined_to;
	  update_inlined_to_pointers (e->callee, inlined_to);
	}
    }
}


/* For functions-as-trees languages, this performs all optimization and
   compilation for FNDECL.  */

void
tree_rest_of_compilation (tree fndecl)
{
  location_t saved_loc;
  struct cgraph_node *saved_node = NULL, *node;

  timevar_push (TV_EXPAND);

  gcc_assert (!flag_unit_at_a_time || cgraph_global_info_ready);

  /* Initialize the RTL code for the function.  */
  current_function_decl = fndecl;
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fndecl);
  init_function_start (fndecl);

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  cfun->x_dont_save_pending_sizes_p = 1;

  node = cgraph_node (fndecl);

  /* We might need the body of this function so that we can expand
     it inline somewhere else.  This means not lowering some constructs
     such as exception handling.  */
  if (cgraph_preserve_function_body_p (fndecl))
    {
      if (!flag_unit_at_a_time)
	{
	  struct cgraph_edge *e;

	  saved_node = cgraph_clone_node (node);
	  for (e = saved_node->callees; e; e = e->next_callee)
	    if (!e->inline_failed)
	      cgraph_clone_inlined_nodes (e, true);
	}
      cfun->saved_static_chain_decl = cfun->static_chain_decl;
      cfun->saved_tree = save_body (fndecl, &cfun->saved_args,
				    &cfun->saved_static_chain_decl);
    }

  if (flag_inline_trees)
    {
      struct cgraph_edge *e;
      for (e = node->callees; e; e = e->next_callee)
	if (!e->inline_failed || warn_inline)
	  break;
      if (e)
	{
	  timevar_push (TV_INTEGRATION);
	  optimize_inline_calls (fndecl);
	  timevar_pop (TV_INTEGRATION);
	}
    }

  /* We are not going to maintain the cgraph edges up to date.
     Kill it so it won't confuse us.  */
  cgraph_node_remove_callees (node);


  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);
  bitmap_obstack_initialize (&reg_obstack); /* FIXME, only at RTL generation*/
  
  vars_to_rename = BITMAP_ALLOC (NULL);
  
  /* Perform all tree transforms and optimizations.  */
  execute_pass_list (all_passes);
  
  bitmap_obstack_release (&reg_obstack);

  /* Release the default bitmap obstack.  */
  bitmap_obstack_release (NULL);
  
  /* Restore original body if still needed.  */
  if (cfun->saved_tree)
    {
      DECL_SAVED_TREE (fndecl) = cfun->saved_tree;
      DECL_ARGUMENTS (fndecl) = cfun->saved_args;
      cfun->static_chain_decl = cfun->saved_static_chain_decl;

      /* When not in unit-at-a-time mode, we must preserve out of line copy
	 representing node before inlining.  Restore original outgoing edges
	 using clone we created earlier.  */
      if (!flag_unit_at_a_time)
	{
	  struct cgraph_edge *e;

	  cgraph_node_remove_callees (node);
	  node->callees = saved_node->callees;
	  saved_node->callees = NULL;
	  update_inlined_to_pointers (node, node);
	  for (e = node->callees; e; e = e->next_callee)
	    e->caller = node;
	  cgraph_remove_node (saved_node);
	}
    }
  else
    DECL_SAVED_TREE (fndecl) = NULL;
  cfun = 0;

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */
  if (warn_larger_than && !DECL_EXTERNAL (fndecl) && TREE_TYPE (fndecl))
    {
      tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));

      if (ret_type && TYPE_SIZE_UNIT (ret_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (ret_type)) == INTEGER_CST
	  && 0 < compare_tree_int (TYPE_SIZE_UNIT (ret_type),
				   larger_than_size))
	{
	  unsigned int size_as_int
	    = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ret_type));

	  if (compare_tree_int (TYPE_SIZE_UNIT (ret_type), size_as_int) == 0)
	    warning ("%Jsize of return value of %qD is %u bytes",
                     fndecl, fndecl, size_as_int);
	  else
	    warning ("%Jsize of return value of %qD is larger than %wd bytes",
                     fndecl, fndecl, larger_than_size);
	}
    }

  if (!flag_inline_trees)
    {
      DECL_SAVED_TREE (fndecl) = NULL;
      if (DECL_STRUCT_FUNCTION (fndecl) == 0
	  && !cgraph_node (fndecl)->origin)
	{
	  /* Stop pointing to the local nodes about to be freed.
	     But DECL_INITIAL must remain nonzero so we know this
	     was an actual function definition.
	     For a nested function, this is done in c_pop_function_context.
	     If rest_of_compilation set this to 0, leave it 0.  */
	  if (DECL_INITIAL (fndecl) != 0)
	    DECL_INITIAL (fndecl) = error_mark_node;
	}
    }

  input_location = saved_loc;

  ggc_collect ();
  timevar_pop (TV_EXPAND);
}
