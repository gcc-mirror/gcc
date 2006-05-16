/* Data flow functions for trees.
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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
#include "tree-gimple.h"
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
  long num_stmt_anns;
  long num_var_anns;
  long num_defs;
  long num_uses;
  long num_phis;
  long num_phi_args;
  int max_num_phi_args;
  long num_v_may_defs;
  long num_vuses;
  long num_v_must_defs;
};


/* Local functions.  */
static void collect_dfa_stats (struct dfa_stats_d *);
static tree collect_dfa_stats_r (tree *, int *, void *);
static tree find_vars_r (tree *, int *, void *);
static void add_referenced_var (tree, bool);


/* Global declarations.  */

/* Array of all variables referenced in the function.  */
htab_t referenced_vars;


/*---------------------------------------------------------------------------
			Dataflow analysis (DFA) routines
---------------------------------------------------------------------------*/
/* Find all the variables referenced in the function.  This function
   builds the global arrays REFERENCED_VARS and CALL_CLOBBERED_VARS.

   Note that this function does not look for statement operands, it simply
   determines what variables are referenced in the program and detects
   various attributes for each variable used by alias analysis and the
   optimizer.  */

static void
find_referenced_vars (void)
{
  basic_block bb;
  block_stmt_iterator si;

  FOR_EACH_BB (bb)
    for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
      {
	tree *stmt_p = bsi_stmt_ptr (si);
	walk_tree (stmt_p, find_vars_r, NULL, NULL);
      }

}

struct tree_opt_pass pass_referenced_vars =
{
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
  0,					/* todo_flags_start */
  0,                                    /* todo_flags_finish */
  0				        /* letter */
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
  gcc_assert (!t->common.ann || t->common.ann->common.type == VAR_ANN);

  ann = ggc_alloc (sizeof (*ann));
  memset ((void *) ann, 0, sizeof (*ann));

  ann->common.type = VAR_ANN;

  t->common.ann = (tree_ann_t) ann;

  return ann;
}


/* Create a new annotation for a statement node T.  */

stmt_ann_t
create_stmt_ann (tree t)
{
  stmt_ann_t ann;

  gcc_assert (is_gimple_stmt (t));
  gcc_assert (!t->common.ann || t->common.ann->common.type == STMT_ANN);

  ann = ggc_alloc (sizeof (*ann));
  memset ((void *) ann, 0, sizeof (*ann));

  ann->common.type = STMT_ANN;

  /* Since we just created the annotation, mark the statement modified.  */
  ann->modified = true;

  t->common.ann = (tree_ann_t) ann;

  return ann;
}

/* Create a new annotation for a tree T.  */

tree_ann_t
create_tree_ann (tree t)
{
  tree_ann_t ann;

  gcc_assert (t);
  gcc_assert (!t->common.ann || t->common.ann->common.type == TREE_ANN_COMMON);

  ann = ggc_alloc (sizeof (*ann));
  memset ((void *) ann, 0, sizeof (*ann));

  ann->common.type = TREE_ANN_COMMON;
  t->common.ann = ann;

  return ann;
}

/* Build a temporary.  Make sure and register it to be renamed.  */

tree
make_rename_temp (tree type, const char *prefix)
{
  tree t = create_tmp_var (type, prefix);

  if (TREE_CODE (type) == COMPLEX_TYPE)
    DECL_COMPLEX_GIMPLE_REG_P (t) = 1;

  if (referenced_vars)
    {
      add_referenced_tmp_var (t);
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


/* Dump sub-variables for VAR to FILE.  */

void
dump_subvars_for (FILE *file, tree var)
{
  subvar_t sv = get_subvars_for_var (var);

  if (!sv)
    return;

  fprintf (file, "{ ");

  for (; sv; sv = sv->next)
    {
      print_generic_expr (file, sv->var, dump_flags);
      fprintf (file, " ");
    }

  fprintf (file, "}");
}


/* Dumb sub-variables for VAR to stderr.  */

void
debug_subvars_for (tree var)
{
  dump_subvars_for (stderr, var);
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

  fprintf (file, ", UID %u", (unsigned) DECL_UID (var));

  fprintf (file, ", ");
  print_generic_expr (file, TREE_TYPE (var), dump_flags);

  if (ann && ann->type_mem_tag)
    {
      fprintf (file, ", type memory tag: ");
      print_generic_expr (file, ann->type_mem_tag, dump_flags);
    }

  if (ann && ann->is_alias_tag)
    fprintf (file, ", is an alias tag");

  if (TREE_ADDRESSABLE (var))
    fprintf (file, ", is addressable");
  
  if (is_global_var (var))
    fprintf (file, ", is global");

  if (TREE_THIS_VOLATILE (var))
    fprintf (file, ", is volatile");

  if (is_call_clobbered (var))
    fprintf (file, ", call clobbered");

  if (default_def (var))
    {
      fprintf (file, ", default def: ");
      print_generic_expr (file, default_def (var), dump_flags);
    }

  if (may_aliases (var))
    {
      fprintf (file, ", may aliases: ");
      dump_may_aliases_for (file, var);
    }

  if (get_subvars_for_var (var))
    {
      fprintf (file, ", sub-vars: ");
      dump_subvars_for (file, var);
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

  size = dfa_stats.num_stmt_anns * sizeof (struct stmt_ann_d);
  total += size;
  fprintf (file, fmt_str_1, "Statements annotated", dfa_stats.num_stmt_anns,
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

  size = dfa_stats.num_v_may_defs * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "V_MAY_DEF operands", dfa_stats.num_v_may_defs,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_v_must_defs * sizeof (tree *);
  total += size;
  fprintf (file, fmt_str_1, "V_MUST_DEF operands", dfa_stats.num_v_must_defs,
	   SCALE (size), LABEL (size));

  size = dfa_stats.num_phis * sizeof (struct tree_phi_node);
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
    fprintf (file, "Average number of arguments per PHI node: %.1f (max: %d)\n",
	     (float) dfa_stats.num_phi_args / (float) dfa_stats.num_phis,
	     dfa_stats.max_num_phi_args);

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
collect_dfa_stats (struct dfa_stats_d *dfa_stats_p)
{
  struct pointer_set_t *pset;
  basic_block bb;
  block_stmt_iterator i;

  gcc_assert (dfa_stats_p);

  memset ((void *)dfa_stats_p, 0, sizeof (struct dfa_stats_d));

  /* Walk all the trees in the function counting references.  Start at
     basic block 0, but don't stop at block boundaries.  */
  pset = pointer_set_create ();

  for (i = bsi_start (BASIC_BLOCK (0)); !bsi_end_p (i); bsi_next (&i))
    walk_tree (bsi_stmt_ptr (i), collect_dfa_stats_r, (void *) dfa_stats_p,
	       pset);

  pointer_set_destroy (pset);

  FOR_EACH_BB (bb)
    {
      tree phi;
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  dfa_stats_p->num_phis++;
	  dfa_stats_p->num_phi_args += PHI_NUM_ARGS (phi);
	  if (PHI_NUM_ARGS (phi) > dfa_stats_p->max_num_phi_args)
	    dfa_stats_p->max_num_phi_args = PHI_NUM_ARGS (phi);
	}
    }
}


/* Callback for walk_tree to collect DFA statistics for a tree and its
   children.  */

static tree
collect_dfa_stats_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
		     void *data)
{
  tree t = *tp;
  struct dfa_stats_d *dfa_stats_p = (struct dfa_stats_d *)data;

  if (t->common.ann)
    {
      switch (ann_type (t->common.ann))
	{
	case STMT_ANN:
	  {
	    dfa_stats_p->num_stmt_anns++;
	    dfa_stats_p->num_defs += NUM_SSA_OPERANDS (t, SSA_OP_DEF);
	    dfa_stats_p->num_uses += NUM_SSA_OPERANDS (t, SSA_OP_USE);
	    dfa_stats_p->num_v_may_defs += NUM_SSA_OPERANDS (t, SSA_OP_VMAYDEF);
	    dfa_stats_p->num_vuses += NUM_SSA_OPERANDS (t, SSA_OP_VUSE);
	    dfa_stats_p->num_v_must_defs += 
				  NUM_SSA_OPERANDS (t, SSA_OP_VMUSTDEF);
	    break;
	  }

	case VAR_ANN:
	  dfa_stats_p->num_var_anns++;
	  break;

	default:
	  break;
	}
    }

  return NULL;
}


/*---------------------------------------------------------------------------
			     Miscellaneous helpers
---------------------------------------------------------------------------*/
/* Callback for walk_tree.  Used to collect variables referenced in
   the function.  */

static tree
find_vars_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  /* If T is a regular variable that the optimizers are interested
     in, add it to the list of variables.  */
  if (SSA_VAR_P (*tp))
    add_referenced_var (*tp, false);

  /* Type, _DECL and constant nodes have no interesting children.
     Ignore them.  */
  else if (IS_TYPE_OR_DECL_P (*tp) || CONSTANT_CLASS_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}


/* Lookup UID in the referenced_vars hashtable and return the associated
   variable or NULL if it is not there.  */

tree 
referenced_var_lookup_if_exists (unsigned int uid)
{
  struct int_tree_map *h, in;
  in.uid = uid;
  h = htab_find_with_hash (referenced_vars, &in, uid);
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Lookup UID in the referenced_vars hashtable and return the associated
   variable.  */

tree 
referenced_var_lookup (unsigned int uid)
{
  struct int_tree_map *h, in;
  in.uid = uid;
  h = htab_find_with_hash (referenced_vars, &in, uid);
  gcc_assert (h || uid == 0);
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert the pair UID, TO into the referenced_vars hashtable.  */

static void
referenced_var_insert (unsigned int uid, tree to)
{ 
  struct int_tree_map *h;
  void **loc;

  h = ggc_alloc (sizeof (struct int_tree_map));
  h->uid = uid;
  h->to = to;
  loc = htab_find_slot_with_hash (referenced_vars, h, uid, INSERT);
  /* This assert can only trigger if a variable with the same UID has been 
     inserted already.  */
  gcc_assert ((*(struct int_tree_map **)loc) == NULL);
  *(struct int_tree_map **)  loc = h;
}

/* Add VAR to the list of dereferenced variables.

   WALK_STATE contains a hash table used to avoid adding the same
      variable more than once. Note that this function assumes that
      VAR is a valid SSA variable.  If WALK_STATE is NULL, no
      duplicate checking is done.  */

static void
add_referenced_var (tree var, bool always)
{
  var_ann_t v_ann;

  v_ann = get_var_ann (var);
  gcc_assert (DECL_P (var));
  
  if (always || referenced_var_lookup_if_exists (DECL_UID (var)) == NULL_TREE)
    {
      /* This is the first time we find this variable, add it to the
         REFERENCED_VARS array and annotate it with attributes that are
	 intrinsic to the variable.  */
      
      referenced_var_insert (DECL_UID (var), var);

      /* Global variables are always call-clobbered.  */
      if (is_global_var (var))
	mark_call_clobbered (var);

      /* Scan DECL_INITIAL for pointer variables as they may contain
	 address arithmetic referencing the address of other
	 variables.  */
      if (DECL_INITIAL (var)
	  /* Initializers of external variables are not useful to the
	     optimizers.  */
          && !DECL_EXTERNAL (var)
	  /* It's not necessary to walk the initial value of non-constant
	     variables because it cannot be propagated by the
	     optimizers.  */
	  && (TREE_CONSTANT (var) || TREE_READONLY (var)))
      	walk_tree (&DECL_INITIAL (var), find_vars_r, NULL, 0);
    }
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

/* Add a temporary variable to REFERENCED_VARS.  This is similar to
   add_referenced_var, but is used by passes that need to add new temps to
   the REFERENCED_VARS array after the program has been scanned for
   variables.  The variable will just receive a new UID and be added
   to the REFERENCED_VARS array without checking for duplicates.  */

void
add_referenced_tmp_var (tree var)
{
  add_referenced_var (var, true);
}


/* Mark all the non-SSA variables found in STMT's operands to be
   processed by update_ssa.  */

void
mark_new_vars_to_rename (tree stmt)
{
  ssa_op_iter iter;
  tree val;
  bitmap vars_in_vops_to_rename;
  bool found_exposed_symbol = false;
  int v_may_defs_before, v_may_defs_after;
  int v_must_defs_before, v_must_defs_after;

  if (TREE_CODE (stmt) == PHI_NODE)
    return;

  vars_in_vops_to_rename = BITMAP_ALLOC (NULL);

  /* Before re-scanning the statement for operands, mark the existing
     virtual operands to be renamed again.  We do this because when new
     symbols are exposed, the virtual operands that were here before due to
     aliasing will probably be removed by the call to get_stmt_operand.
     Therefore, we need to flag them to be renamed beforehand.

     We flag them in a separate bitmap because we don't really want to
     rename them if there are not any newly exposed symbols in the
     statement operands.  */
  v_may_defs_before = NUM_SSA_OPERANDS (stmt, SSA_OP_VMAYDEF);
  v_must_defs_before = NUM_SSA_OPERANDS (stmt, SSA_OP_VMUSTDEF);

  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, 
			     SSA_OP_VMAYDEF | SSA_OP_VUSE | SSA_OP_VMUSTDEF)
    {
      if (!DECL_P (val))
	val = SSA_NAME_VAR (val);
      bitmap_set_bit (vars_in_vops_to_rename, DECL_UID (val));
    }

  /* Now force an operand re-scan on the statement and mark any newly
     exposed variables.  */
  update_stmt (stmt);

  v_may_defs_after = NUM_SSA_OPERANDS (stmt, SSA_OP_VMAYDEF);
  v_must_defs_after = NUM_SSA_OPERANDS (stmt, SSA_OP_VMUSTDEF);

  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_ALL_OPERANDS)
    if (DECL_P (val))
      {
	found_exposed_symbol = true;
	mark_sym_for_renaming (val);
      }

  /* If we found any newly exposed symbols, or if there are fewer VDEF
     operands in the statement, add the variables we had set in
     VARS_IN_VOPS_TO_RENAME to VARS_TO_RENAME.  We need to check for
     vanishing VDEFs because in those cases, the names that were formerly
     generated by this statement are not going to be available anymore.  */
  if (found_exposed_symbol
      || v_may_defs_before > v_may_defs_after
      || v_must_defs_before > v_must_defs_after)
    mark_set_for_renaming (vars_in_vops_to_rename);

  BITMAP_FREE (vars_in_vops_to_rename);
}

/* Find all variables within the gimplified statement that were not previously
   visible to the function and add them to the referenced variables list.  */

static tree
find_new_referenced_vars_1 (tree *tp, int *walk_subtrees,
			    void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (TREE_CODE (t) == VAR_DECL && !var_ann (t))
    {
      add_referenced_tmp_var (t);
      mark_sym_for_renaming (t);
    }

  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

  return NULL;
}

void
find_new_referenced_vars (tree *stmt_p)
{
  walk_tree (stmt_p, find_new_referenced_vars_1, NULL, NULL);
}


/* If REF is a COMPONENT_REF for a structure that can have sub-variables, and
   we know where REF is accessing, return the variable in REF that has the
   sub-variables.  If the return value is not NULL, POFFSET will be the
   offset, in bits, of REF inside the return value, and PSIZE will be the
   size, in bits, of REF inside the return value.  */

tree
okay_component_ref_for_subvars (tree ref, unsigned HOST_WIDE_INT *poffset,
				unsigned HOST_WIDE_INT *psize)
{
  tree result = NULL;
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  tree offset;
  enum machine_mode mode;
  int unsignedp;
  int volatilep;

  gcc_assert (!SSA_VAR_P (ref));
  *poffset = 0;  
  *psize = (unsigned int) -1;
  
  if (ref_contains_array_ref (ref))
    return result;
  ref = get_inner_reference (ref, &bitsize, &bitpos, &offset, &mode,
			     &unsignedp, &volatilep, false);
  if (TREE_CODE (ref) == INDIRECT_REF)
    return result;
  else if (offset == NULL && bitsize != -1 && SSA_VAR_P (ref))
    {
      *poffset = bitpos;      
      *psize = bitsize;
      if (get_subvars_for_var (ref) != NULL)
	return ref;
    }
  else if (SSA_VAR_P (ref))
    {
      if (get_subvars_for_var (ref) != NULL)
	return ref;
    }
  return NULL_TREE;
}
