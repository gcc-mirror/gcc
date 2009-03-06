/* Data flow functions for trees.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hashtab.h"
#include "pointer-set.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "timevar.h"
#include "expr.h"
#include "ggc.h"
#include "langhooks.h"
#include "flags.h"
#include "function.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "gimple.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "convert.h"
#include "params.h"
#include "cgraph.h"

/* Build and maintain data flow information for trees.  */

/* Counters used to display DFA and SSA statistics.  */
struct dfa_stats_d
{
  long num_var_anns;
  long num_defs;
  long num_uses;
  long num_phis;
  long num_phi_args;
  size_t max_num_phi_args;
  long num_vdefs;
  long num_vuses;
};


/* Local functions.  */
static void collect_dfa_stats (struct dfa_stats_d *);
static tree find_vars_r (tree *, int *, void *);


/*---------------------------------------------------------------------------
			Dataflow analysis (DFA) routines
---------------------------------------------------------------------------*/
/* Find all the variables referenced in the function.  This function
   builds the global arrays REFERENCED_VARS and CALL_CLOBBERED_VARS.

   Note that this function does not look for statement operands, it simply
   determines what variables are referenced in the program and detects
   various attributes for each variable used by alias analysis and the
   optimizer.  */

static unsigned int
find_referenced_vars (void)
{
  basic_block bb;
  gimple_stmt_iterator si;

  FOR_EACH_BB (bb)
    {
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  size_t i;
	  gimple stmt = gsi_stmt (si);
	  for (i = 0; i < gimple_num_ops (stmt); i++)
	    walk_tree (gimple_op_ptr (stmt, i), find_vars_r, NULL, NULL);
	}

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple phi = gsi_stmt (si);
	  size_t i, len = gimple_phi_num_args (phi);

	  walk_tree (gimple_phi_result_ptr (phi), find_vars_r, NULL, NULL);

	  for (i = 0; i < len; i++)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      walk_tree (&arg, find_vars_r, NULL, NULL);
	    }
	}
    }

  return 0;
}

struct gimple_opt_pass pass_referenced_vars =
{
 {
  GIMPLE_PASS,
  NULL,					/* name */
  NULL,					/* gate */
  find_referenced_vars,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_FIND_REFERENCED_VARS,		/* tv_id */
  PROP_gimple_leh | PROP_cfg,		/* properties_required */
  PROP_referenced_vars,			/* properties_provided */
  0,					/* properties_destroyed */
  TODO_dump_func,			/* todo_flags_start */
  TODO_dump_func                        /* todo_flags_finish */
 }
};


/*---------------------------------------------------------------------------
			    Manage annotations
---------------------------------------------------------------------------*/
/* Create a new annotation for a _DECL node T.  */

var_ann_t
create_var_ann (tree t)
{
  var_ann_t ann;

  gcc_assert (t);
  gcc_assert (DECL_P (t));
  gcc_assert (!t->base.ann || t->base.ann->common.type == VAR_ANN);

  ann = GGC_CNEW (struct var_ann_d);
  ann->common.type = VAR_ANN;
  t->base.ann = (tree_ann_t) ann;

  return ann;
}

/* Create a new annotation for a FUNCTION_DECL node T.  */

function_ann_t
create_function_ann (tree t)
{
  function_ann_t ann;

  gcc_assert (t);
  gcc_assert (TREE_CODE (t) == FUNCTION_DECL);
  gcc_assert (!t->base.ann || t->base.ann->common.type == FUNCTION_ANN);

  ann = (function_ann_t) ggc_alloc (sizeof (*ann));
  memset ((void *) ann, 0, sizeof (*ann));

  ann->common.type = FUNCTION_ANN;

  t->base.ann = (tree_ann_t) ann;

  return ann;
}

/* Renumber all of the gimple stmt uids.  */

void 
renumber_gimple_stmt_uids (void)
{
  basic_block bb;

  set_gimple_stmt_max_uid (cfun, 0);
  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator bsi;
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	}
    }
}

/* Create a new annotation for a tree T.  */

tree_ann_common_t
create_tree_common_ann (tree t)
{
  tree_ann_common_t ann;

  gcc_assert (t);
  gcc_assert (!t->base.ann || t->base.ann->common.type == TREE_ANN_COMMON);

  ann = GGC_CNEW (struct tree_ann_common_d);

  ann->type = TREE_ANN_COMMON;
  ann->rn = -1;
  t->base.ann = (tree_ann_t) ann;

  return ann;
}

/* Build a temporary.  Make sure and register it to be renamed.  */

tree
make_rename_temp (tree type, const char *prefix)
{
  tree t = create_tmp_var (type, prefix);

  if (TREE_CODE (TREE_TYPE (t)) == COMPLEX_TYPE
      || TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (t) = 1;

  if (gimple_referenced_vars (cfun))
    {
      add_referenced_var (t);
      mark_sym_for_renaming (t);
    }

  return t;
}



/*---------------------------------------------------------------------------
			      Debugging functions
---------------------------------------------------------------------------*/
/* Dump the list of all the referenced variables in the current function to
   FILE.  */

void
dump_referenced_vars (FILE *file)
{
  tree var;
  referenced_var_iterator rvi;
  
  fprintf (file, "\nReferenced variables in %s: %u\n\n",
	   get_name (current_function_decl), (unsigned) num_referenced_vars);
  
  FOR_EACH_REFERENCED_VAR (var, rvi)
    {
      fprintf (file, "Variable: ");
      dump_variable (file, var);
      fprintf (file, "\n");
    }
}


/* Dump the list of all the referenced variables to stderr.  */

void
debug_referenced_vars (void)
{
  dump_referenced_vars (stderr);
}


/* Dump variable VAR and its may-aliases to FILE.  */

void
dump_variable (FILE *file, tree var)
{
  var_ann_t ann;

  if (TREE_CODE (var) == SSA_NAME)
    {
      if (POINTER_TYPE_P (TREE_TYPE (var)))
	dump_points_to_info_for (file, var);
      var = SSA_NAME_VAR (var);
    }

  if (var == NULL_TREE)
    {
      fprintf (file, "<nil>");
      return;
    }

  print_generic_expr (file, var, dump_flags);

  ann = var_ann (var);

  fprintf (file, ", UID D.%u", (unsigned) DECL_UID (var));

  fprintf (file, ", ");
  print_generic_expr (file, TREE_TYPE (var), dump_flags);

  if (ann && ann->symbol_mem_tag)
    {
      fprintf (file, ", symbol memory tag: ");
      print_generic_expr (file, ann->symbol_mem_tag, dump_flags);
    }

  if (TREE_ADDRESSABLE (var))
    fprintf (file, ", is addressable");
  
  if (is_global_var (var))
    fprintf (file, ", is global");

  if (TREE_THIS_VOLATILE (var))
    fprintf (file, ", is volatile");

  dump_mem_sym_stats_for_var (file, var);

  if (is_call_clobbered (var))
    {
      const char *s = "";
      var_ann_t va = var_ann (var);
      unsigned int escape_mask = va->escape_mask;

      fprintf (file, ", call clobbered");
      fprintf (file, " (");
      if (escape_mask & ESCAPE_STORED_IN_GLOBAL)
	{ fprintf (file, "%sstored in global", s); s = ", "; }
      if (escape_mask & ESCAPE_TO_ASM)
	{ fprintf (file, "%sgoes through ASM", s); s = ", "; }
      if (escape_mask & ESCAPE_TO_CALL)
	{ fprintf (file, "%spassed to call", s); s = ", "; }
      if (escape_mask & ESCAPE_BAD_CAST)
	{ fprintf (file, "%sbad cast", s); s = ", "; }
      if (escape_mask & ESCAPE_TO_RETURN)
	{ fprintf (file, "%sreturned from func", s); s = ", "; }
      if (escape_mask & ESCAPE_TO_PURE_CONST)
	{ fprintf (file, "%spassed to pure/const", s); s = ", "; }
      if (escape_mask & ESCAPE_IS_GLOBAL)
	{ fprintf (file, "%sis global var", s); s = ", "; }
      if (escape_mask & ESCAPE_IS_PARM)
	{ fprintf (file, "%sis incoming pointer", s); s = ", "; }
      if (escape_mask & ESCAPE_UNKNOWN)
	{ fprintf (file, "%sunknown escape", s); s = ", "; }
      fprintf (file, ")");
    }

  if (ann->noalias_state == NO_ALIAS)
    fprintf (file, ", NO_ALIAS (does not alias other NO_ALIAS symbols)");
  else if (ann->noalias_state == NO_ALIAS_GLOBAL)
    fprintf (file, ", NO_ALIAS_GLOBAL (does not alias other NO_ALIAS symbols"
	           " and global vars)");
  else if (ann->noalias_state == NO_ALIAS_ANYTHING)
    fprintf (file, ", NO_ALIAS_ANYTHING (does not alias any other symbols)");

  if (gimple_default_def (cfun, var))
    {
      fprintf (file, ", default def: ");
      print_generic_expr (file, gimple_default_def (cfun, var), dump_flags);
    }

  if (MTAG_P (var) && may_aliases (var))
    {
      fprintf (file, ", may aliases: ");
      dump_may_aliases_for (file, var);
    }

  if (!is_gimple_reg (var))
    {
      if (memory_partition (var))
	{
	  fprintf (file, ", belongs to partition: ");
	  print_generic_expr (file, memory_partition (var), dump_flags);
	}

      if (TREE_CODE (var) == MEMORY_PARTITION_TAG)
	{
	  fprintf (file, ", partition symbols: ");
	  dump_decl_set (file, MPT_SYMBOLS (var));
	}
    }

  fprintf (file, "\n");
}


/* Dump variable VAR and its may-aliases to stderr.  */

void
debug_variable (tree var)
{
  dump_variable (stderr, var);
}


/* Dump various DFA statistics to FILE.  */

void
dump_dfa_stats (FILE *file)
{
  struct dfa_stats_d dfa_stats;

  unsigned long size, total = 0;
  const char * const fmt_str   = "%-30s%-13s%12s\n";
  const char * const fmt_str_1 = "%-30s%13lu%11lu%c\n";
  const char * const fmt_str_3 = "%-43s%11lu%c\n";
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);

  collect_dfa_stats (&dfa_stats);

  fprintf (file, "\nDFA Statistics for %s\n\n", funcname);

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str, "", "  Number of  ", "Memory");
  fprintf (file, fmt_str, "", "  instances  ", "used ");
  fprintf (file, "---------------------------------------------------------\n");

  size = num_referenced_vars * sizeof (tree);
  total += size;
  fprintf (file, fmt_str_1, "Referenced variables", (unsigned long)num_referenced_vars,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_var_anns * sizeof (struct var_ann_d);
  total += size;
  fprintf (file, fmt_str_1, "Variables annotated", dfa_stats.num_var_anns,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_uses * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "USE operands", dfa_stats.num_uses,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_defs * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "DEF operands", dfa_stats.num_defs,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_vuses * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "VUSE operands", dfa_stats.num_vuses,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_vdefs * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "VDEF operands", dfa_stats.num_vdefs,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_phis * sizeof (struct gimple_statement_phi);
  total += size;
  fprintf (file, fmt_str_1, "PHI nodes", dfa_stats.num_phis,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_phi_args * sizeof (struct phi_arg_d);
  total += size;
  fprintf (file, fmt_str_1, "PHI arguments", dfa_stats.num_phi_args,
 	   SCALE (size), LABEL (size));

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str_3, "Total memory used by DFA/SSA data", SCALE (total),
	   LABEL (total));
  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, "\n");

  if (dfa_stats.num_phis)
    fprintf (file, "Average number of arguments per PHI node: %.1f (max: %ld)\n",
	     (float) dfa_stats.num_phi_args / (float) dfa_stats.num_phis,
	     (long) dfa_stats.max_num_phi_args);

  fprintf (file, "\n");
}


/* Dump DFA statistics on stderr.  */

void
debug_dfa_stats (void)
{
  dump_dfa_stats (stderr);
}


/* Collect DFA statistics and store them in the structure pointed to by
   DFA_STATS_P.  */

static void
collect_dfa_stats (struct dfa_stats_d *dfa_stats_p ATTRIBUTE_UNUSED)
{
  basic_block bb;
  referenced_var_iterator vi;
  tree var;

  gcc_assert (dfa_stats_p);

  memset ((void *)dfa_stats_p, 0, sizeof (struct dfa_stats_d));

  /* Count all the variable annotations.  */
  FOR_EACH_REFERENCED_VAR (var, vi)
    if (var_ann (var))
      dfa_stats_p->num_var_anns++;

  /* Walk all the statements in the function counting references.  */
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator si;

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple phi = gsi_stmt (si);
	  dfa_stats_p->num_phis++;
	  dfa_stats_p->num_phi_args += gimple_phi_num_args (phi);
	  if (gimple_phi_num_args (phi) > dfa_stats_p->max_num_phi_args)
	    dfa_stats_p->max_num_phi_args = gimple_phi_num_args (phi);
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple stmt = gsi_stmt (si);
	  dfa_stats_p->num_defs += NUM_SSA_OPERANDS (stmt, SSA_OP_DEF);
	  dfa_stats_p->num_uses += NUM_SSA_OPERANDS (stmt, SSA_OP_USE);
	  dfa_stats_p->num_vdefs += NUM_SSA_OPERANDS (stmt, SSA_OP_VDEF);
	  dfa_stats_p->num_vuses += NUM_SSA_OPERANDS (stmt, SSA_OP_VUSE);
	}
    }
}


/*---------------------------------------------------------------------------
			     Miscellaneous helpers
---------------------------------------------------------------------------*/
/* Callback for walk_tree.  Used to collect variables referenced in
   the function.  */

static tree
find_vars_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  /* If we are reading the lto info back in, we need to rescan the
     referenced vars.  */
  if (TREE_CODE (*tp) == SSA_NAME)
    add_referenced_var (SSA_NAME_VAR (*tp));

  /* If T is a regular variable that the optimizers are interested
     in, add it to the list of variables.  */
  else if (SSA_VAR_P (*tp))
    add_referenced_var (*tp);

  /* Type, _DECL and constant nodes have no interesting children.
     Ignore them.  */
  else if (IS_TYPE_OR_DECL_P (*tp) || CONSTANT_CLASS_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Lookup UID in the referenced_vars hashtable and return the associated
   variable.  */

tree 
referenced_var_lookup (unsigned int uid)
{
  tree h;
  struct tree_decl_minimal in;
  in.uid = uid;
  h = (tree) htab_find_with_hash (gimple_referenced_vars (cfun), &in, uid);
  gcc_assert (h || uid == 0);
  return h;
}

/* Check if TO is in the referenced_vars hash table and insert it if not.  
   Return true if it required insertion.  */

bool
referenced_var_check_and_insert (tree to)
{ 
  tree h, *loc;
  struct tree_decl_minimal in;
  unsigned int uid = DECL_UID (to);

  in.uid = uid;
  h = (tree) htab_find_with_hash (gimple_referenced_vars (cfun), &in, uid);
  if (h)
    {
      /* DECL_UID has already been entered in the table.  Verify that it is
	 the same entry as TO.  See PR 27793.  */
      gcc_assert (h == to);
      return false;
    }

  loc = (tree *) htab_find_slot_with_hash (gimple_referenced_vars (cfun),
					   &in, uid, INSERT);
  *loc = to;
  return true;
}

/* Lookup VAR UID in the default_defs hashtable and return the associated
   variable.  */

tree 
gimple_default_def (struct function *fn, tree var)
{
  struct tree_decl_minimal ind;
  struct tree_ssa_name in;
  gcc_assert (SSA_VAR_P (var));
  in.var = (tree)&ind;
  ind.uid = DECL_UID (var);
  return (tree) htab_find_with_hash (DEFAULT_DEFS (fn), &in, DECL_UID (var));
}

/* Insert the pair VAR's UID, DEF into the default_defs hashtable.  */

void
set_default_def (tree var, tree def)
{ 
  struct tree_decl_minimal ind;
  struct tree_ssa_name in;
  void **loc;

  gcc_assert (SSA_VAR_P (var));
  in.var = (tree)&ind;
  ind.uid = DECL_UID (var);
  if (!def)
    {
      loc = htab_find_slot_with_hash (DEFAULT_DEFS (cfun), &in,
            DECL_UID (var), INSERT);
      gcc_assert (*loc);
      htab_remove_elt (DEFAULT_DEFS (cfun), *loc);
      return;
    }
  gcc_assert (TREE_CODE (def) == SSA_NAME && SSA_NAME_VAR (def) == var);
  loc = htab_find_slot_with_hash (DEFAULT_DEFS (cfun), &in,
                                  DECL_UID (var), INSERT);

  /* Default definition might be changed by tail call optimization.  */
  if (*loc)
    SSA_NAME_IS_DEFAULT_DEF (*(tree *) loc) = false;
  *(tree *) loc = def;

   /* Mark DEF as the default definition for VAR.  */
   SSA_NAME_IS_DEFAULT_DEF (def) = true;
}

/* Add VAR to the list of referenced variables if it isn't already there.  */

bool
add_referenced_var (tree var)
{
  var_ann_t v_ann;

  v_ann = get_var_ann (var);
  gcc_assert (DECL_P (var));
  
  /* Insert VAR into the referenced_vars has table if it isn't present.  */
  if (referenced_var_check_and_insert (var))
    {
      /* This is the first time we found this variable, annotate it with
	 attributes that are intrinsic to the variable.  */
      
      /* Tag's don't have DECL_INITIAL.  */
      if (MTAG_P (var))
	return true;

      /* Scan DECL_INITIAL for pointer variables as they may contain
	 address arithmetic referencing the address of other
	 variables.  
	 Even non-constant initializers need to be walked, because
	 IPA passes might prove that their are invariant later on.  */
      if (DECL_INITIAL (var)
	  /* Initializers of external variables are not useful to the
	     optimizers.  */
          && !DECL_EXTERNAL (var))
      	walk_tree (&DECL_INITIAL (var), find_vars_r, NULL, 0);

      return true;
    }

  return false;
}

/* Remove VAR from the list.  */

void
remove_referenced_var (tree var)
{
  var_ann_t v_ann;
  struct tree_decl_minimal in;
  void **loc;
  unsigned int uid = DECL_UID (var);

  clear_call_clobbered (var);
  bitmap_clear_bit (gimple_call_used_vars (cfun), uid);
  if ((v_ann = var_ann (var)))
    {
      /* Preserve var_anns of globals, but clear their alias info.  */
      if (MTAG_P (var)
	  || (!TREE_STATIC (var) && !DECL_EXTERNAL (var)))
	{
	  ggc_free (v_ann);
	  var->base.ann = NULL;
	}
      else
	{
	  v_ann->mpt = NULL_TREE;
	  v_ann->symbol_mem_tag = NULL_TREE;
	}
    }
  gcc_assert (DECL_P (var));
  in.uid = uid;
  loc = htab_find_slot_with_hash (gimple_referenced_vars (cfun), &in, uid,
				  NO_INSERT);
  htab_clear_slot (gimple_referenced_vars (cfun), loc);
}


/* Return the virtual variable associated to the non-scalar variable VAR.  */

tree
get_virtual_var (tree var)
{
  STRIP_NOPS (var);

  if (TREE_CODE (var) == SSA_NAME)
    var = SSA_NAME_VAR (var);

  while (TREE_CODE (var) == REALPART_EXPR || TREE_CODE (var) == IMAGPART_EXPR
	 || handled_component_p (var))
    var = TREE_OPERAND (var, 0);

  /* Treating GIMPLE registers as virtual variables makes no sense.
     Also complain if we couldn't extract a _DECL out of the original
     expression.  */
  gcc_assert (SSA_VAR_P (var));
  gcc_assert (!is_gimple_reg (var));

  return var;
}

/* Mark all the naked symbols in STMT for SSA renaming.
   
   NOTE: This function should only be used for brand new statements.
   If the caller is modifying an existing statement, it should use the
   combination push_stmt_changes/pop_stmt_changes.  */

void
mark_symbols_for_renaming (gimple stmt)
{
  tree op;
  ssa_op_iter iter;

  update_stmt (stmt);

  /* Mark all the operands for renaming.  */
  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_ALL_OPERANDS)
    if (DECL_P (op))
      mark_sym_for_renaming (op);
}


/* Find all variables within the gimplified statement that were not
   previously visible to the function and add them to the referenced
   variables list.  */

static tree
find_new_referenced_vars_1 (tree *tp, int *walk_subtrees,
			    void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (TREE_CODE (t) == VAR_DECL && !var_ann (t))
    {
      add_referenced_var (t);
      mark_sym_for_renaming (t);
    }

  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

  return NULL;
}


/* Find any new referenced variables in STMT.  */

void
find_new_referenced_vars (gimple stmt)
{
  walk_gimple_op (stmt, find_new_referenced_vars_1, NULL);
}


/* If EXP is a handled component reference for a structure, return the
   base variable.  The access range is delimited by bit positions *POFFSET and
   *POFFSET + *PMAX_SIZE.  The access size is *PSIZE bits.  If either
   *PSIZE or *PMAX_SIZE is -1, they could not be determined.  If *PSIZE
   and *PMAX_SIZE are equal, the access is non-variable.  */

tree
get_ref_base_and_extent (tree exp, HOST_WIDE_INT *poffset,
			 HOST_WIDE_INT *psize,
			 HOST_WIDE_INT *pmax_size)
{
  HOST_WIDE_INT bitsize = -1;
  HOST_WIDE_INT maxsize = -1;
  tree size_tree = NULL_TREE;
  HOST_WIDE_INT bit_offset = 0;
  bool seen_variable_array_ref = false;

  gcc_assert (!SSA_VAR_P (exp));

  /* First get the final access size from just the outermost expression.  */
  if (TREE_CODE (exp) == COMPONENT_REF)
    size_tree = DECL_SIZE (TREE_OPERAND (exp, 1));
  else if (TREE_CODE (exp) == BIT_FIELD_REF)
    size_tree = TREE_OPERAND (exp, 1);
  else
    {
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
      if (mode == BLKmode)
	size_tree = TYPE_SIZE (TREE_TYPE (exp));
      else
	bitsize = GET_MODE_BITSIZE (mode);
    }
  if (size_tree != NULL_TREE)
    {
      if (! host_integerp (size_tree, 1))
	bitsize = -1;
      else
	bitsize = TREE_INT_CST_LOW (size_tree);
    }

  /* Initially, maxsize is the same as the accessed element size.
     In the following it will only grow (or become -1).  */
  maxsize = bitsize;

  /* Compute cumulative bit-offset for nested component-refs and array-refs,
     and find the ultimate containing object.  */
  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case BIT_FIELD_REF:
	  bit_offset += tree_low_cst (TREE_OPERAND (exp, 2), 0);
	  break;

	case COMPONENT_REF:
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    tree this_offset = component_ref_field_offset (exp);

	    if (this_offset && TREE_CODE (this_offset) == INTEGER_CST)
	      {
		HOST_WIDE_INT hthis_offset = tree_low_cst (this_offset, 0);

		hthis_offset *= BITS_PER_UNIT;
		bit_offset += hthis_offset;
		bit_offset += tree_low_cst (DECL_FIELD_BIT_OFFSET (field), 0);
	      }
	    else
	      {
		tree csize = TYPE_SIZE (TREE_TYPE (TREE_OPERAND (exp, 0)));
		/* We need to adjust maxsize to the whole structure bitsize.
		   But we can subtract any constant offset seen so far,
		   because that would get us out of the structure otherwise.  */
		if (maxsize != -1 && csize && host_integerp (csize, 1))
		  maxsize = TREE_INT_CST_LOW (csize) - bit_offset;
		else
		  maxsize = -1;
	      }
	  }
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  {
	    tree index = TREE_OPERAND (exp, 1);
	    tree low_bound = array_ref_low_bound (exp);
	    tree unit_size = array_ref_element_size (exp);

	    /* If the resulting bit-offset is constant, track it.  */
	    if (host_integerp (index, 0)
		&& host_integerp (low_bound, 0)
		&& host_integerp (unit_size, 1))
	      {
		HOST_WIDE_INT hindex = tree_low_cst (index, 0);

		hindex -= tree_low_cst (low_bound, 0);
		hindex *= tree_low_cst (unit_size, 1);
		hindex *= BITS_PER_UNIT;
		bit_offset += hindex;

		/* An array ref with a constant index up in the structure
		   hierarchy will constrain the size of any variable array ref
		   lower in the access hierarchy.  */
		seen_variable_array_ref = false;
	      }
	    else
	      {
		tree asize = TYPE_SIZE (TREE_TYPE (TREE_OPERAND (exp, 0)));
		/* We need to adjust maxsize to the whole array bitsize.
		   But we can subtract any constant offset seen so far,
		   because that would get us outside of the array otherwise.  */
		if (maxsize != -1 && asize && host_integerp (asize, 1))
		  maxsize = TREE_INT_CST_LOW (asize) - bit_offset;
		else
		  maxsize = -1;

		/* Remember that we have seen an array ref with a variable
		   index.  */
		seen_variable_array_ref = true;
	      }
	  }
	  break;

	case REALPART_EXPR:
	  break;

	case IMAGPART_EXPR:
	  bit_offset += bitsize;
	  break;

	case VIEW_CONVERT_EXPR:
	  /* ???  We probably should give up here and bail out.  */
	  break;

	default:
	  goto done;
	}

      exp = TREE_OPERAND (exp, 0);
    }
 done:

  /* We need to deal with variable arrays ending structures such as
       struct { int length; int a[1]; } x;           x.a[d]
       struct { struct { int a; int b; } a[1]; } x;  x.a[d].a
       struct { struct { int a[1]; } a[1]; } x;      x.a[0][d], x.a[d][0]
     where we do not know maxsize for variable index accesses to
     the array.  The simplest way to conservatively deal with this
     is to punt in the case that offset + maxsize reaches the
     base type boundary.  */
  if (seen_variable_array_ref
      && maxsize != -1
      && host_integerp (TYPE_SIZE (TREE_TYPE (exp)), 1)
      && bit_offset + maxsize
	   == (signed)TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (exp))))
    maxsize = -1;

  /* ???  Due to negative offsets in ARRAY_REF we can end up with
     negative bit_offset here.  We might want to store a zero offset
     in this case.  */
  *poffset = bit_offset;
  *psize = bitsize;
  *pmax_size = maxsize;

  return exp;
}

/* Returns true if STMT references an SSA_NAME that has
   SSA_NAME_OCCURS_IN_ABNORMAL_PHI set, otherwise false.  */

bool
stmt_references_abnormal_ssa_name (gimple stmt)
{
  ssa_op_iter oi;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, oi, SSA_OP_USE)
    {
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (use_p)))
	return true;
    }

  return false;
}

/* Return true, if the two memory references REF1 and REF2 may alias.  */

bool
refs_may_alias_p (tree ref1, tree ref2)
{
  tree base1, base2;
  HOST_WIDE_INT offset1 = 0, offset2 = 0;
  HOST_WIDE_INT size1 = -1, size2 = -1;
  HOST_WIDE_INT max_size1 = -1, max_size2 = -1;
  bool strict_aliasing_applies;

  gcc_assert ((SSA_VAR_P (ref1)
	       || handled_component_p (ref1)
	       || INDIRECT_REF_P (ref1)
	       || TREE_CODE (ref1) == TARGET_MEM_REF)
	      && (SSA_VAR_P (ref2)
		  || handled_component_p (ref2)
		  || INDIRECT_REF_P (ref2)
		  || TREE_CODE (ref2) == TARGET_MEM_REF));

  /* Defer to TBAA if possible.  */
  if (flag_strict_aliasing
      && !alias_sets_conflict_p (get_alias_set (ref1), get_alias_set (ref2)))
    return false;

  /* Decompose the references into their base objects and the access.  */
  base1 = ref1;
  if (handled_component_p (ref1))
    base1 = get_ref_base_and_extent (ref1, &offset1, &size1, &max_size1);
  base2 = ref2;
  if (handled_component_p (ref2))
    base2 = get_ref_base_and_extent (ref2, &offset2, &size2, &max_size2);

  /* If both references are based on different variables, they cannot alias.
     If both references are based on the same variable, they cannot alias if
     the accesses do not overlap.  */
  if (SSA_VAR_P (base1)
      && SSA_VAR_P (base2))
    {
      if (!operand_equal_p (base1, base2, 0))
	return false;
      return ranges_overlap_p (offset1, max_size1, offset2, max_size2);
    }

  /* If one base is a ref-all pointer weird things are allowed.  */
  strict_aliasing_applies = (flag_strict_aliasing
			     && (!INDIRECT_REF_P (base1)
				 || get_alias_set (base1) != 0)
			     && (!INDIRECT_REF_P (base2)
				 || get_alias_set (base2) != 0));

  /* If strict aliasing applies the only way to access a scalar variable
     is through a pointer dereference or through a union (gcc extension).  */
  if (strict_aliasing_applies
      && ((SSA_VAR_P (ref2)
	   && !AGGREGATE_TYPE_P (TREE_TYPE (ref2))
	   && !INDIRECT_REF_P (ref1)
	   && TREE_CODE (TREE_TYPE (base1)) != UNION_TYPE)
	  || (SSA_VAR_P (ref1)
	      && !AGGREGATE_TYPE_P (TREE_TYPE (ref1))
	      && !INDIRECT_REF_P (ref2)
	      && TREE_CODE (TREE_TYPE (base2)) != UNION_TYPE)))
    return false;

  /* If both references are through the same type, or if strict aliasing
     doesn't apply they are through two same pointers, they do not alias
     if the accesses do not overlap.  */
  if ((strict_aliasing_applies
       && (TYPE_MAIN_VARIANT (TREE_TYPE (base1))
	   == TYPE_MAIN_VARIANT (TREE_TYPE (base2))))
      || (TREE_CODE (base1) == INDIRECT_REF
	  && TREE_CODE (base2) == INDIRECT_REF
	  && operand_equal_p (TREE_OPERAND (base1, 0),
			      TREE_OPERAND (base2, 0), 0)))
    return ranges_overlap_p (offset1, max_size1, offset2, max_size2);

  /* If both are component references through pointers try to find a
     common base and apply offset based disambiguation.  This handles
     for example
       struct A { int i; int j; } *q;
       struct B { struct A a; int k; } *p;
     disambiguating q->i and p->a.j.  */
  if (strict_aliasing_applies
      && (TREE_CODE (base1) == INDIRECT_REF
	  || TREE_CODE (base2) == INDIRECT_REF)
      && handled_component_p (ref1)
      && handled_component_p (ref2))
    {
      tree *refp;
      /* Now search for the type of base1 in the access path of ref2.  This
	 would be a common base for doing offset based disambiguation on.  */
      refp = &ref2;
      while (handled_component_p (*refp)
	     /* Note that the following is only conservative if there are
		never copies of types appearing as sub-structures.  */
	     && (TYPE_MAIN_VARIANT (TREE_TYPE (*refp))
		 != TYPE_MAIN_VARIANT (TREE_TYPE (base1))))
	refp = &TREE_OPERAND (*refp, 0);
      if (TYPE_MAIN_VARIANT (TREE_TYPE (*refp))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (base1)))
	{
	  HOST_WIDE_INT offadj, sztmp, msztmp;
	  get_ref_base_and_extent (*refp, &offadj, &sztmp, &msztmp);
	  offset2 -= offadj;
	  return ranges_overlap_p (offset1, max_size1, offset2, max_size2);
	}
      /* The other way around.  */
      refp = &ref1;
      while (handled_component_p (*refp)
	     && (TYPE_MAIN_VARIANT (TREE_TYPE (*refp))
		 != TYPE_MAIN_VARIANT (TREE_TYPE (base2))))
	refp = &TREE_OPERAND (*refp, 0);
      if (TYPE_MAIN_VARIANT (TREE_TYPE (*refp))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (base2)))
	{
	  HOST_WIDE_INT offadj, sztmp, msztmp;
	  get_ref_base_and_extent (*refp, &offadj, &sztmp, &msztmp);
	  offset1 -= offadj;
	  return ranges_overlap_p (offset1, max_size1, offset2, max_size2);
	}
      /* If we can be sure to catch all equivalent types in the search
	 for the common base then we could return false here.  In that
	 case we would be able to disambiguate q->i and p->k.  */
    }

  return true;
}

/* Given a stmt STMT that references memory, return the single stmt
   that is reached by following the VUSE -> VDEF link.  Returns
   NULL_TREE, if there is no single stmt that defines all VUSEs of
   STMT.
   Note that for a stmt with a single virtual operand this may return
   a PHI node as well.  Note that if all VUSEs are default definitions
   this function will return an empty statement.  */

gimple
get_single_def_stmt (gimple stmt)
{
  gimple def_stmt = NULL;
  tree use;
  ssa_op_iter iter;

  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_VIRTUAL_USES)
    {
      gimple tmp = SSA_NAME_DEF_STMT (use);

      /* ???  This is too simplistic for multiple virtual operands
	 reaching different PHI nodes of the same basic blocks or for
	 reaching all default definitions.  */
      if (def_stmt
	  && def_stmt != tmp
	  && !(gimple_nop_p (def_stmt)
	       && gimple_nop_p (tmp)))
	return NULL;

      def_stmt = tmp;
    }

  return def_stmt;
}

/* Given a PHI node of virtual operands, tries to eliminate cyclic
   reached definitions if they do not alias REF and returns the
   defining statement of the single virtual operand that flows in
   from a non-backedge.  Returns NULL_TREE if such statement within
   the above conditions cannot be found.  */

gimple
get_single_def_stmt_from_phi (tree ref, gimple phi)
{
  tree def_arg = NULL_TREE;
  unsigned i;

  /* Find the single PHI argument that is not flowing in from a
     back edge and verify that the loop-carried definitions do
     not alias the reference we look for.  */
  for (i = 0; i < gimple_phi_num_args (phi); ++i)
    {
      tree arg = PHI_ARG_DEF (phi, i);
      gimple def_stmt;

      if (!(gimple_phi_arg_edge (phi, i)->flags & EDGE_DFS_BACK))
	{
	  /* Multiple non-back edges?  Do not try to handle this.  */
	  if (def_arg)
	    return NULL;
	  def_arg = arg;
	  continue;
	}

      /* Follow the definitions back to the original PHI node.  Bail
	 out once a definition is found that may alias REF.  */
      def_stmt = SSA_NAME_DEF_STMT (arg);
      do
	{
	  if (!is_gimple_assign (def_stmt)
	      || refs_may_alias_p (ref, gimple_assign_lhs (def_stmt)))
	    return NULL;
	  /* ???  This will only work, reaching the PHI node again if
	     there is a single virtual operand on def_stmt.  */
	  def_stmt = get_single_def_stmt (def_stmt);
	  if (!def_stmt)
	    return NULL;
	}
      while (def_stmt != phi);
    }

  return SSA_NAME_DEF_STMT (def_arg);
}

/* Return the single reference statement defining all virtual uses
   on STMT or NULL_TREE, if there are multiple defining statements.
   Take into account only definitions that alias REF if following
   back-edges when looking through a loop PHI node.  */

gimple
get_single_def_stmt_with_phi (tree ref, gimple stmt)
{
  switch (NUM_SSA_OPERANDS (stmt, SSA_OP_VIRTUAL_USES))
    {
    case 0:
      gcc_unreachable ();

    case 1:
      {
	gimple def_stmt = SSA_NAME_DEF_STMT (SINGLE_SSA_TREE_OPERAND
					     (stmt, SSA_OP_VIRTUAL_USES));
	/* We can handle lookups over PHI nodes only for a single
	   virtual operand.  */
	if (gimple_code (def_stmt) == GIMPLE_PHI)
	  return get_single_def_stmt_from_phi (ref, def_stmt);
	return def_stmt;
      }

    default:
      return get_single_def_stmt (stmt);
    }
}
