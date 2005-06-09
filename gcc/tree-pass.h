/* Definitions for describing one tree-ssa optimization pass.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>

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


#ifndef GCC_TREE_PASS_H
#define GCC_TREE_PASS_H 1

/* Global variables used to communicate with passes.  */
extern FILE *dump_file;
extern int dump_flags;
extern const char *dump_file_name;

/* Return the dump_file_info for the given phase.  */
extern struct dump_file_info *get_dump_file_info (enum tree_dump_index);

/* Describe one pass.  */
struct tree_opt_pass
{
  /* Terse name of the pass used as a fragment of the dump file name.  */
  const char *name;

  /* If non-null, this pass and all sub-passes are executed only if
     the function returns true.  */
  bool (*gate) (void);

  /* This is the code to run.  If null, then there should be sub-passes
     otherwise this pass does nothing.  */
  void (*execute) (void);

  /* A list of sub-passes to run, dependent on gate predicate.  */
  struct tree_opt_pass *sub;

  /* Next in the list of passes to run, independent of gate predicate.  */
  struct tree_opt_pass *next;

  /* Static pass number, used as a fragment of the dump file name.  */
  int static_pass_number;

  /* The timevar id associated with this pass.  */
  /* ??? Ideally would be dynamically assigned.  */
  unsigned int tv_id;

  /* Sets of properties input and output from this pass.  */
  unsigned int properties_required;
  unsigned int properties_provided;
  unsigned int properties_destroyed;

  /* Flags indicating common sets things to do before and after.  */
  unsigned int todo_flags_start;
  unsigned int todo_flags_finish;

  /* Letter for RTL dumps.  */
  char letter;
};

/* Define a tree dump switch.  */
struct dump_file_info
{
  const char *suffix;           /* suffix to give output file.  */
  const char *swtch;            /* command line switch */
  const char *glob;             /* command line glob  */
  int flags;                    /* user flags */
  int state;                    /* state of play */
  int num;                      /* dump file number */
  int letter;                   /* enabling letter for RTL dumps */
};

/* Pass properties.  */
#define PROP_gimple_any		(1 << 0)	/* entire gimple grammar */
#define PROP_gimple_lcf		(1 << 1)	/* lowered control flow */
#define PROP_gimple_leh		(1 << 2)	/* lowered eh */
#define PROP_cfg		(1 << 3)
#define PROP_referenced_vars	(1 << 4)
#define PROP_pta		(1 << 5)
#define PROP_ssa		(1 << 6)
#define PROP_no_crit_edges      (1 << 7)
#define PROP_rtl		(1 << 8)
#define PROP_alias		(1 << 9)

#define PROP_trees \
  (PROP_gimple_any | PROP_gimple_lcf | PROP_gimple_leh)

/* To-do flags.  */
#define TODO_dump_func			(1 << 0)
#define TODO_ggc_collect		(1 << 1)
#define TODO_verify_ssa			(1 << 2) 
#define TODO_verify_flow		(1 << 3)
#define TODO_verify_stmts		(1 << 4)
#define TODO_cleanup_cfg        	(1 << 5)
#define TODO_verify_loops		(1 << 6)
#define TODO_dump_cgraph		(1 << 7)

/* To-do flags for calls to update_ssa.  */

/* Update the SSA form inserting PHI nodes for newly exposed symbols
   and virtual names marked for updating.  When updating real names,
   only insert PHI nodes for a real name O_j in blocks reached by all
   the new and old definitions for O_j.  If the iterated dominance
   frontier for O_j is not pruned, we may end up inserting PHI nodes
   in blocks that have one or more edges with no incoming definition
   for O_j.  This would lead to uninitialized warnings for O_j's
   symbol.  */
#define TODO_update_ssa			(1 << 7)

/* Update the SSA form without inserting any new PHI nodes at all.
   This is used by passes that have either inserted all the PHI nodes
   themselves or passes that need only to patch use-def and def-def
   chains for virtuals (e.g., DCE).  */
#define TODO_update_ssa_no_phi		(1 << 8)

/* Insert PHI nodes everywhere they are needed.  No pruning of the
   IDF is done.  This is used by passes that need the PHI nodes for
   O_j even if it means that some arguments will come from the default
   definition of O_j's symbol (e.g., pass_linear_transform).
   
   WARNING: If you need to use this flag, chances are that your pass
   may be doing something wrong.  Inserting PHI nodes for an old name
   where not all edges carry a new replacement may lead to silent
   codegen errors or spurious uninitialized warnings.  */
#define TODO_update_ssa_full_phi	(1 << 9)

/* Passes that update the SSA form on their own may want to delegate
   the updating of virtual names to the generic updater.  Since FUD
   chains are easier to maintain, this simplifies the work they need
   to do.  NOTE: If this flag is used, any OLD->NEW mappings for real
   names are explicitly destroyed and only the symbols marked for
   renaming are processed.  */
#define TODO_update_ssa_only_virtuals	(1 << 10)

#define TODO_update_ssa_any		\
    (TODO_update_ssa			\
     | TODO_update_ssa_no_phi		\
     | TODO_update_ssa_full_phi		\
     | TODO_update_ssa_only_virtuals)

#define TODO_verify_all \
  (TODO_verify_ssa | TODO_verify_flow | TODO_verify_stmts)

extern void ipa_passes (void);
extern void tree_lowering_passes (tree decl);

extern struct tree_opt_pass pass_mudflap_1;
extern struct tree_opt_pass pass_mudflap_2;
extern struct tree_opt_pass pass_remove_useless_stmts;
extern struct tree_opt_pass pass_lower_cf;
extern struct tree_opt_pass pass_lower_eh;
extern struct tree_opt_pass pass_build_cfg;
extern struct tree_opt_pass pass_tree_profile;
extern struct tree_opt_pass pass_referenced_vars;
extern struct tree_opt_pass pass_sra;
extern struct tree_opt_pass pass_tail_recursion;
extern struct tree_opt_pass pass_tail_calls;
extern struct tree_opt_pass pass_loop;
extern struct tree_opt_pass pass_loop_init;
extern struct tree_opt_pass pass_lim;
extern struct tree_opt_pass pass_unswitch;
extern struct tree_opt_pass pass_iv_canon;
extern struct tree_opt_pass pass_scev_cprop;
extern struct tree_opt_pass pass_record_bounds;
extern struct tree_opt_pass pass_if_conversion;
extern struct tree_opt_pass pass_vectorize;
extern struct tree_opt_pass pass_complete_unroll;
extern struct tree_opt_pass pass_iv_optimize;
extern struct tree_opt_pass pass_loop_done;
extern struct tree_opt_pass pass_ch;
extern struct tree_opt_pass pass_ccp;
extern struct tree_opt_pass pass_build_ssa;
extern struct tree_opt_pass pass_del_ssa;
extern struct tree_opt_pass pass_dominator;
extern struct tree_opt_pass pass_dce;
extern struct tree_opt_pass pass_cd_dce;
extern struct tree_opt_pass pass_merge_phi;
extern struct tree_opt_pass pass_may_alias;
extern struct tree_opt_pass pass_split_crit_edges;
extern struct tree_opt_pass pass_pre;
extern struct tree_opt_pass pass_profile;
extern struct tree_opt_pass pass_lower_complex_O0;
extern struct tree_opt_pass pass_lower_complex;
extern struct tree_opt_pass pass_lower_vector;
extern struct tree_opt_pass pass_lower_vector_ssa;
extern struct tree_opt_pass pass_fold_builtins;
extern struct tree_opt_pass pass_stdarg;
extern struct tree_opt_pass pass_early_warn_uninitialized;
extern struct tree_opt_pass pass_late_warn_uninitialized;
extern struct tree_opt_pass pass_cse_reciprocals;
extern struct tree_opt_pass pass_warn_function_return;
extern struct tree_opt_pass pass_warn_function_noreturn;
extern struct tree_opt_pass pass_phiopt;
extern struct tree_opt_pass pass_forwprop;
extern struct tree_opt_pass pass_redundant_phi;
extern struct tree_opt_pass pass_dse;
extern struct tree_opt_pass pass_nrv;
extern struct tree_opt_pass pass_remove_useless_vars;
extern struct tree_opt_pass pass_mark_used_blocks;
extern struct tree_opt_pass pass_rename_ssa_copies;
extern struct tree_opt_pass pass_expand;
extern struct tree_opt_pass pass_rest_of_compilation;
extern struct tree_opt_pass pass_sink_code;
extern struct tree_opt_pass pass_fre;
extern struct tree_opt_pass pass_linear_transform;
extern struct tree_opt_pass pass_copy_prop;
extern struct tree_opt_pass pass_store_ccp;
extern struct tree_opt_pass pass_store_copy_prop;
extern struct tree_opt_pass pass_vrp;
extern struct tree_opt_pass pass_create_structure_vars;
extern struct tree_opt_pass pass_build_pta;
extern struct tree_opt_pass pass_del_pta;
extern struct tree_opt_pass pass_uncprop;
extern struct tree_opt_pass pass_reassoc;

/* IPA Passes */
extern struct tree_opt_pass pass_ipa_inline;

#endif /* GCC_TREE_PASS_H */
