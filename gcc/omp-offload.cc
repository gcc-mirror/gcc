/* Bits of OpenMP and OpenACC handling that is specific to device offloading
   and a lowering pass for OpenACC device directives.

   Copyright (C) 2005-2022 Free Software Foundation, Inc.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "langhooks.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-nested.h"
#include "stor-layout.h"
#include "common/common-target.h"
#include "omp-general.h"
#include "omp-offload.h"
#include "lto-section-names.h"
#include "gomp-constants.h"
#include "gimple-pretty-print.h"
#include "intl.h"
#include "stringpool.h"
#include "attribs.h"
#include "cfgloop.h"
#include "context.h"
#include "convert.h"
#include "opts.h"

/* Describe the OpenACC looping structure of a function.  The entire
   function is held in a 'NULL' loop.  */

struct oacc_loop
{
  oacc_loop *parent; /* Containing loop.  */

  oacc_loop *child; /* First inner loop.  */

  oacc_loop *sibling; /* Next loop within same parent.  */

  location_t loc; /* Location of the loop start.  */

  gcall *marker; /* Initial head marker.  */

  gcall *heads[GOMP_DIM_MAX];  /* Head marker functions.  */
  gcall *tails[GOMP_DIM_MAX];  /* Tail marker functions.  */

  tree routine;  /* Pseudo-loop enclosing a routine.  */

  unsigned mask;   /* Partitioning mask.  */
  unsigned e_mask; /* Partitioning of element loops (when tiling).  */
  unsigned inner;  /* Partitioning of inner loops.  */
  unsigned flags;  /* Partitioning flags.  */
  vec<gcall *> ifns;  /* Contained loop abstraction functions.  */
  tree chunk_size; /* Chunk size.  */
  gcall *head_end; /* Final marker of head sequence.  */
};

/* Holds offload tables with decls.  */
vec<tree, va_gc> *offload_funcs, *offload_vars;

/* Return level at which oacc routine may spawn a partitioned loop, or
   -1 if it is not a routine (i.e. is an offload fn).  */

int
oacc_fn_attrib_level (tree attr)
{
  tree pos = TREE_VALUE (attr);

  if (!TREE_PURPOSE (pos))
    return -1;

  int ix = 0;
  for (ix = 0; ix != GOMP_DIM_MAX;
       ix++, pos = TREE_CHAIN (pos))
    if (!integer_zerop (TREE_PURPOSE (pos)))
      break;

  return ix;
}

/* Helper function for omp_finish_file routine.  Takes decls from V_DECLS and
   adds their addresses and sizes to constructor-vector V_CTOR.  */

static void
add_decls_addresses_to_decl_constructor (vec<tree, va_gc> *v_decls,
					 vec<constructor_elt, va_gc> *v_ctor)
{
  unsigned len = vec_safe_length (v_decls);
  for (unsigned i = 0; i < len; i++)
    {
      tree it = (*v_decls)[i];
      bool is_var = VAR_P (it);
      bool is_link_var
	= is_var
#ifdef ACCEL_COMPILER
	  && DECL_HAS_VALUE_EXPR_P (it)
#endif
	  && lookup_attribute ("omp declare target link", DECL_ATTRIBUTES (it));

      /* See also omp_finish_file and output_offload_tables in lto-cgraph.cc.  */
      if (!in_lto_p && !symtab_node::get (it))
	continue;

      tree size = NULL_TREE;
      if (is_var)
	size = fold_convert (const_ptr_type_node, DECL_SIZE_UNIT (it));

      tree addr;
      if (!is_link_var)
	addr = build_fold_addr_expr (it);
      else
	{
#ifdef ACCEL_COMPILER
	  /* For "omp declare target link" vars add address of the pointer to
	     the target table, instead of address of the var.  */
	  tree value_expr = DECL_VALUE_EXPR (it);
	  tree link_ptr_decl = TREE_OPERAND (value_expr, 0);
	  varpool_node::finalize_decl (link_ptr_decl);
	  addr = build_fold_addr_expr (link_ptr_decl);
#else
	  addr = build_fold_addr_expr (it);
#endif

	  /* Most significant bit of the size marks "omp declare target link"
	     vars in host and target tables.  */
	  unsigned HOST_WIDE_INT isize = tree_to_uhwi (size);
	  isize |= 1ULL << (int_size_in_bytes (const_ptr_type_node)
			    * BITS_PER_UNIT - 1);
	  size = wide_int_to_tree (const_ptr_type_node, isize);
	}

      CONSTRUCTOR_APPEND_ELT (v_ctor, NULL_TREE, addr);
      if (is_var)
	CONSTRUCTOR_APPEND_ELT (v_ctor, NULL_TREE, size);
    }
}

/* Return true if DECL is a function for which its references should be
   analyzed.  */

static bool
omp_declare_target_fn_p (tree decl)
{
  return (TREE_CODE (decl) == FUNCTION_DECL
	  && lookup_attribute ("omp declare target", DECL_ATTRIBUTES (decl))
	  && !lookup_attribute ("omp declare target host",
				DECL_ATTRIBUTES (decl))
	  && (!flag_openacc
	      || oacc_get_fn_attrib (decl) == NULL_TREE));
}

/* Return true if DECL Is a variable for which its initializer references
   should be analyzed.  */

static bool
omp_declare_target_var_p (tree decl)
{
  return (VAR_P (decl)
	  && lookup_attribute ("omp declare target", DECL_ATTRIBUTES (decl))
	  && !lookup_attribute ("omp declare target link",
				DECL_ATTRIBUTES (decl)));
}

/* Helper function for omp_discover_implicit_declare_target, called through
   walk_tree.  Mark referenced FUNCTION_DECLs implicitly as
   declare target to.  */

static tree
omp_discover_declare_target_tgt_fn_r (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == CALL_EXPR
      && CALL_EXPR_FN (*tp)
      && TREE_CODE (CALL_EXPR_FN (*tp)) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (CALL_EXPR_FN (*tp), 0)) == FUNCTION_DECL
      && lookup_attribute ("omp declare variant base",
			   DECL_ATTRIBUTES (TREE_OPERAND (CALL_EXPR_FN (*tp),
							  0))))
    {
      tree fn = TREE_OPERAND (CALL_EXPR_FN (*tp), 0);
      for (tree attr = DECL_ATTRIBUTES (fn); attr; attr = TREE_CHAIN (attr))
	{
	  attr = lookup_attribute ("omp declare variant base", attr);
	  if (attr == NULL_TREE)
	    break;
	  tree purpose = TREE_PURPOSE (TREE_VALUE (attr));
	  if (TREE_CODE (purpose) == FUNCTION_DECL)
	    omp_discover_declare_target_tgt_fn_r (&purpose, walk_subtrees, data);
	}
    }
  else if (TREE_CODE (*tp) == FUNCTION_DECL)
    {
      tree decl = *tp;
      tree id = get_identifier ("omp declare target");
      symtab_node *node = symtab_node::get (*tp);
      if (node != NULL)
	{
	  while (node->alias_target
		 && TREE_CODE (node->alias_target) == FUNCTION_DECL)
	    {
	      if (!omp_declare_target_fn_p (node->decl)
		  && !lookup_attribute ("omp declare target host",
					DECL_ATTRIBUTES (node->decl)))
		{
		  node->offloadable = 1;
		  DECL_ATTRIBUTES (node->decl)
		    = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (node->decl));
		}
	      node = symtab_node::get (node->alias_target);
	    }
	  symtab_node *new_node = node->ultimate_alias_target ();
	  decl = new_node->decl;
	  while (node != new_node)
	    {
	      if (!omp_declare_target_fn_p (node->decl)
		  && !lookup_attribute ("omp declare target host",
					DECL_ATTRIBUTES (node->decl)))
		{
		  node->offloadable = 1;
		  DECL_ATTRIBUTES (node->decl)
		    = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (node->decl));
		}
	      gcc_assert (node->alias && node->analyzed);
	      node = node->get_alias_target ();
	    }
	  node->offloadable = 1;
	  if (ENABLE_OFFLOADING)
	    g->have_offload = true;
	}
      if (omp_declare_target_fn_p (decl)
	  || lookup_attribute ("omp declare target host",
			       DECL_ATTRIBUTES (decl)))
	return NULL_TREE;

      if (!DECL_EXTERNAL (decl) && DECL_SAVED_TREE (decl))
	((vec<tree> *) data)->safe_push (decl);
      DECL_ATTRIBUTES (decl) = tree_cons (id, NULL_TREE,
					  DECL_ATTRIBUTES (decl));
    }
  else if (TYPE_P (*tp))
    *walk_subtrees = 0;
  else if (TREE_CODE (*tp) == OMP_TARGET)
    {
      tree c = omp_find_clause (OMP_CLAUSES (*tp), OMP_CLAUSE_DEVICE);
      if (c && OMP_CLAUSE_DEVICE_ANCESTOR (c))
	*walk_subtrees = 0;
    }
  return NULL_TREE;
}

/* Similarly, but ignore references outside of OMP_TARGET regions.  */

static tree
omp_discover_declare_target_fn_r (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == OMP_TARGET)
    {
      tree c = omp_find_clause (OMP_CLAUSES (*tp), OMP_CLAUSE_DEVICE);
      if (!c || !OMP_CLAUSE_DEVICE_ANCESTOR (c))
	walk_tree_without_duplicates (&OMP_TARGET_BODY (*tp),
				      omp_discover_declare_target_tgt_fn_r,
				      data);
      *walk_subtrees = 0;
    }
  else if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Helper function for omp_discover_implicit_declare_target, called through
   walk_tree.  Mark referenced FUNCTION_DECLs implicitly as
   declare target to.  */

static tree
omp_discover_declare_target_var_r (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == FUNCTION_DECL)
    return omp_discover_declare_target_tgt_fn_r (tp, walk_subtrees, data);
  else if (VAR_P (*tp)
	   && is_global_var (*tp)
	   && !omp_declare_target_var_p (*tp))
    {
      tree id = get_identifier ("omp declare target");
      if (lookup_attribute ("omp declare target link", DECL_ATTRIBUTES (*tp)))
	{
	  error_at (DECL_SOURCE_LOCATION (*tp),
		    "%qD specified both in declare target %<link%> and "
		    "implicitly in %<to%> clauses", *tp);
	  DECL_ATTRIBUTES (*tp)
	    = remove_attribute ("omp declare target link", DECL_ATTRIBUTES (*tp));
	}
      if (TREE_STATIC (*tp) && lang_hooks.decls.omp_get_decl_init (*tp))
	((vec<tree> *) data)->safe_push (*tp);
      DECL_ATTRIBUTES (*tp) = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (*tp));
      symtab_node *node = symtab_node::get (*tp);
      if (node != NULL && !node->offloadable)
	{
	  node->offloadable = 1;
	  if (ENABLE_OFFLOADING)
	    {
	      g->have_offload = true;
	      if (is_a <varpool_node *> (node))
		vec_safe_push (offload_vars, node->decl);
	    }
	}
    }
  else if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Perform the OpenMP implicit declare target to discovery.  */

void
omp_discover_implicit_declare_target (void)
{
  cgraph_node *node;
  varpool_node *vnode;
  auto_vec<tree> worklist;

  FOR_EACH_DEFINED_FUNCTION (node)
    if (DECL_SAVED_TREE (node->decl))
      {
	struct cgraph_node *cgn;
        if (omp_declare_target_fn_p (node->decl))
	  worklist.safe_push (node->decl);
	else if (DECL_STRUCT_FUNCTION (node->decl)
		 && DECL_STRUCT_FUNCTION (node->decl)->has_omp_target)
	  worklist.safe_push (node->decl);
	for (cgn = first_nested_function (node);
	     cgn; cgn = next_nested_function (cgn))
	  if (omp_declare_target_fn_p (cgn->decl))
	    worklist.safe_push (cgn->decl);
	  else if (DECL_STRUCT_FUNCTION (cgn->decl)
		   && DECL_STRUCT_FUNCTION (cgn->decl)->has_omp_target)
	    worklist.safe_push (cgn->decl);
      }
  FOR_EACH_VARIABLE (vnode)
    if (lang_hooks.decls.omp_get_decl_init (vnode->decl)
	&& omp_declare_target_var_p (vnode->decl))
      worklist.safe_push (vnode->decl);
  while (!worklist.is_empty ())
    {
      tree decl = worklist.pop ();
      if (VAR_P (decl))
	walk_tree_without_duplicates (lang_hooks.decls.omp_get_decl_init (decl),
				      omp_discover_declare_target_var_r,
				      &worklist);
      else if (omp_declare_target_fn_p (decl))
	walk_tree_without_duplicates (&DECL_SAVED_TREE (decl),
				      omp_discover_declare_target_tgt_fn_r,
				      &worklist);
      else
	walk_tree_without_duplicates (&DECL_SAVED_TREE (decl),
				      omp_discover_declare_target_fn_r,
				      &worklist);
    }

  lang_hooks.decls.omp_finish_decl_inits ();
}

static bool ompacc_supported_clauses_p (tree clauses)
{
  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    switch (OMP_CLAUSE_CODE (c))
      {
      case OMP_CLAUSE_COLLAPSE:
      case OMP_CLAUSE_NOWAIT:
	continue;
      default:
	return false;
      }
  return true;
}

struct target_region_data
{
  tree func_decl;
  bool has_omp_for;
  bool has_omp_parallel;
  bool ompacc_invalid;
  auto_vec<const char *> warning_msgs;
  auto_vec<location_t> warning_locs;
  target_region_data (void)
    : func_decl (NULL_TREE),
      has_omp_for (false), has_omp_parallel (false), ompacc_invalid (false),
      warning_msgs (), warning_locs () {}
};

static tree scan_omp_target_region_r (tree *, int *, void *);

static void
scan_fndecl_for_ompacc (tree decl, target_region_data *tgtdata)
{
  target_region_data td;
  td.func_decl = decl;
  walk_tree_without_duplicates (&DECL_SAVED_TREE (decl),
				scan_omp_target_region_r, &td);
  tree v;
  if ((v = lookup_attribute ("omp declare variant base",
			     DECL_ATTRIBUTES (decl)))
      || (v = lookup_attribute ("omp declare variant variant",
				DECL_ATTRIBUTES (decl))))
    {
      td.ompacc_invalid = true;
      td.warning_msgs.safe_push ("declare variant not supported for OMPACC");
      td.warning_locs.safe_push (EXPR_LOCATION (v));
    }
  if (tgtdata)
    {
      tgtdata->has_omp_for |= td.has_omp_for;
      tgtdata->has_omp_parallel |= td.has_omp_parallel;
      tgtdata->ompacc_invalid |= td.ompacc_invalid;
      for (unsigned i = 0; i < td.warning_msgs.length (); i++)
	tgtdata->warning_msgs.safe_push (td.warning_msgs[i]);
      for (unsigned i = 0; i < td.warning_locs.length (); i++)
	tgtdata->warning_locs.safe_push (td.warning_locs[i]);
    }

  if (!td.ompacc_invalid
      && !lookup_attribute ("ompacc", DECL_ATTRIBUTES (decl)))
    {
      DECL_ATTRIBUTES (decl)
	= tree_cons (get_identifier ("ompacc"), NULL_TREE,
		     DECL_ATTRIBUTES (decl));
      if (!td.has_omp_parallel)
	DECL_ATTRIBUTES (decl)
	  = tree_cons (get_identifier ("ompacc seq"), NULL_TREE,
		       DECL_ATTRIBUTES (decl));
    }
}

static tree
scan_omp_target_region_r (tree *tp, int *walk_subtrees, void *data)
{
  target_region_data *tgtdata = (target_region_data *) data;

  if (TREE_CODE (*tp) == FUNCTION_DECL
      && !(fndecl_built_in_p (*tp, BUILT_IN_OMP_GET_THREAD_NUM)
	   || fndecl_built_in_p (*tp, BUILT_IN_OMP_GET_NUM_THREADS)
	   || fndecl_built_in_p (*tp, BUILT_IN_OMP_GET_TEAM_NUM)
	   || fndecl_built_in_p (*tp, BUILT_IN_OMP_GET_NUM_TEAMS)
	   || id_equal (DECL_NAME (*tp), "omp_get_thread_num")
	   || id_equal (DECL_NAME (*tp), "omp_get_num_threads")
	   || id_equal (DECL_NAME (*tp), "omp_get_team_num")
	   || id_equal (DECL_NAME (*tp), "omp_get_num_teams"))
      && *tp != tgtdata->func_decl)
    {
      tree decl = *tp;
      symtab_node *node = symtab_node::get (*tp);
      if (node)
	{
	  node = node->ultimate_alias_target ();
	  decl = node->decl;
	}

      if (!DECL_EXTERNAL (decl) && DECL_SAVED_TREE (decl))
	{
	  scan_fndecl_for_ompacc (decl, tgtdata);
	}
      else
	{
	  tgtdata->warning_msgs.safe_push ("referencing external function");
	  tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
	  tgtdata->ompacc_invalid = true;
	}
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  switch (TREE_CODE (*tp))
    {
    case OMP_FOR:
      if (!ompacc_supported_clauses_p (OMP_CLAUSES (*tp)))
	{
	  tgtdata->ompacc_invalid = true;
	  tgtdata->warning_msgs.safe_push ("clauses not supported");
	  tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
	}
      else if (OMP_FOR_NON_RECTANGULAR (*tp))
	{
	  tgtdata->ompacc_invalid = true;
	  tgtdata->warning_msgs.safe_push ("non-rectangular loops not supported");
	  tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
	}
      else
	tgtdata->has_omp_for = true;
      break;

    case OMP_PARALLEL:
      if (!ompacc_supported_clauses_p (OMP_CLAUSES (*tp)))
	{
	  tgtdata->ompacc_invalid = true;
	  tgtdata->warning_msgs.safe_push ("clauses not supported");
	  tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
	}
      else
	tgtdata->has_omp_parallel = true;
      break;

    case OMP_DISTRIBUTE:
    case OMP_TEAMS:
      if (!ompacc_supported_clauses_p (OMP_CLAUSES (*tp)))
	{
	  tgtdata->ompacc_invalid = true;
	  tgtdata->warning_msgs.safe_push ("clauses not supported");
	  tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
	}
      /* Fallthru.  */

    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
      break;

    case OMP_SIMD:
    case OMP_TASK:
    case OMP_LOOP:
    case OMP_TASKLOOP:
    case OMP_TASKGROUP:
    case OMP_SECTION:
    case OMP_MASTER:
    case OMP_MASKED:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_SCAN:
    case OMP_METADIRECTIVE:
      tgtdata->ompacc_invalid = true;
      tgtdata->warning_msgs.safe_push ("construct not supported");
      tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
      *walk_subtrees = 0;
      break;

    case OMP_TARGET:
      tgtdata->ompacc_invalid = true;
      tgtdata->warning_msgs.safe_push ("nested target/reverse offload "
				       "not supported");
      tgtdata->warning_locs.safe_push (EXPR_LOCATION (*tp));
      *walk_subtrees = 0;
      break;

    default:
      break;
    }
  return NULL_TREE;
}

static tree
scan_omp_target_construct_r (tree *tp, int *walk_subtrees,
			     void *data)
{
  if (TREE_CODE (*tp) == OMP_TARGET)
    {
      target_region_data td;
      td.func_decl = (tree) data;
      walk_tree_without_duplicates (&OMP_TARGET_BODY (*tp),
				    scan_omp_target_region_r, &td);
      for (tree c = OMP_TARGET_CLAUSES (*tp); c; c = OMP_CLAUSE_CHAIN (c))
	{
	  switch (OMP_CLAUSE_CODE (c))
	    {
	    case OMP_CLAUSE_MAP:
	      continue;
	    default:
	      td.ompacc_invalid = true;
	      td.warning_msgs.safe_push ("clause not supported");
	      td.warning_locs.safe_push (EXPR_LOCATION (c));
	      break;
	    }
	  break;
	}
      if (!td.ompacc_invalid)
	{
	  tree c = build_omp_clause (EXPR_LOCATION (*tp), OMP_CLAUSE__OMPACC_);
	  if (!td.has_omp_parallel)
	    OMP_CLAUSE__OMPACC__SEQ (c) = 1;
	  OMP_CLAUSE_CHAIN (c) = OMP_TARGET_CLAUSES (*tp);
	  OMP_TARGET_CLAUSES (*tp) = c;
	}
      else
	{
	  warning_at (EXPR_LOCATION (*tp), 0, "Target region not suitable for "
		      "OMPACC mode");
	  for (unsigned i = 0; i < td.warning_locs.length (); i++)
	    warning_at (td.warning_locs[i], 0, td.warning_msgs[i]);
	}
      *walk_subtrees = 0;
    }
  return NULL_TREE;
}

void
omp_ompacc_attribute_tagging (void)
{
  cgraph_node *node;
  FOR_EACH_DEFINED_FUNCTION (node)
    if (DECL_SAVED_TREE (node->decl))
      {
	if (DECL_STRUCT_FUNCTION (node->decl)
	    && DECL_STRUCT_FUNCTION (node->decl)->has_omp_target)
	  walk_tree_without_duplicates (&DECL_SAVED_TREE (node->decl),
					scan_omp_target_construct_r,
					node->decl);

	for (cgraph_node *cgn = first_nested_function (node);
	     cgn; cgn = next_nested_function (cgn))
	  if (omp_declare_target_fn_p (cgn->decl))
	    {
	      scan_fndecl_for_ompacc (cgn->decl, NULL);

	      if (lookup_attribute ("ompacc", DECL_ATTRIBUTES (cgn->decl))
		  && !lookup_attribute ("noinline", DECL_ATTRIBUTES (cgn->decl)))
		{
		  DECL_ATTRIBUTES (cgn->decl)
		    = tree_cons (get_identifier ("noinline"),
				 NULL, DECL_ATTRIBUTES (cgn->decl));
		  DECL_ATTRIBUTES (cgn->decl)
		    = tree_cons (get_identifier ("noipa"),
				 NULL, DECL_ATTRIBUTES (cgn->decl));
		}
	    }
      }
}

/* Create new symbols containing (address, size) pairs for global variables,
   marked with "omp declare target" attribute, as well as addresses for the
   functions, which are outlined offloading regions.  */
void
omp_finish_file (void)
{
  unsigned num_funcs = vec_safe_length (offload_funcs);
  unsigned num_vars = vec_safe_length (offload_vars);

  if (num_funcs == 0 && num_vars == 0)
    return;

  if (targetm_common.have_named_sections)
    {
      vec<constructor_elt, va_gc> *v_f, *v_v;
      vec_alloc (v_f, num_funcs);
      vec_alloc (v_v, num_vars * 2);

      add_decls_addresses_to_decl_constructor (offload_funcs, v_f);
      add_decls_addresses_to_decl_constructor (offload_vars, v_v);

      tree vars_decl_type = build_array_type_nelts (pointer_sized_int_node,
						    vec_safe_length (v_v));
      tree funcs_decl_type = build_array_type_nelts (pointer_sized_int_node,
						     num_funcs);
      SET_TYPE_ALIGN (vars_decl_type, TYPE_ALIGN (pointer_sized_int_node));
      SET_TYPE_ALIGN (funcs_decl_type, TYPE_ALIGN (pointer_sized_int_node));
      tree ctor_v = build_constructor (vars_decl_type, v_v);
      tree ctor_f = build_constructor (funcs_decl_type, v_f);
      TREE_CONSTANT (ctor_v) = TREE_CONSTANT (ctor_f) = 1;
      TREE_STATIC (ctor_v) = TREE_STATIC (ctor_f) = 1;
      tree funcs_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				    get_identifier (".offload_func_table"),
				    funcs_decl_type);
      tree vars_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				   get_identifier (".offload_var_table"),
				   vars_decl_type);
      TREE_STATIC (funcs_decl) = TREE_STATIC (vars_decl) = 1;
      /* Do not align tables more than TYPE_ALIGN (pointer_sized_int_node),
	 otherwise a joint table in a binary will contain padding between
	 tables from multiple object files.  */
      DECL_USER_ALIGN (funcs_decl) = DECL_USER_ALIGN (vars_decl) = 1;
      SET_DECL_ALIGN (funcs_decl, TYPE_ALIGN (funcs_decl_type));
      SET_DECL_ALIGN (vars_decl, TYPE_ALIGN (vars_decl_type));
      DECL_INITIAL (funcs_decl) = ctor_f;
      DECL_INITIAL (vars_decl) = ctor_v;
      set_decl_section_name (funcs_decl, OFFLOAD_FUNC_TABLE_SECTION_NAME);
      set_decl_section_name (vars_decl, OFFLOAD_VAR_TABLE_SECTION_NAME);

      varpool_node::finalize_decl (vars_decl);
      varpool_node::finalize_decl (funcs_decl);
    }
  else
    {
      for (unsigned i = 0; i < num_funcs; i++)
	{
	  tree it = (*offload_funcs)[i];
	  /* See also add_decls_addresses_to_decl_constructor
	     and output_offload_tables in lto-cgraph.cc.  */
	  if (!in_lto_p && !symtab_node::get (it))
	    continue;
	  targetm.record_offload_symbol (it);
	}
      for (unsigned i = 0; i < num_vars; i++)
	{
	  tree it = (*offload_vars)[i];
	  if (!in_lto_p && !symtab_node::get (it))
	    continue;
#ifdef ACCEL_COMPILER
	  if (DECL_HAS_VALUE_EXPR_P (it)
	      && lookup_attribute ("omp declare target link",
				   DECL_ATTRIBUTES (it)))
	    {
	      tree value_expr = DECL_VALUE_EXPR (it);
	      tree link_ptr_decl = TREE_OPERAND (value_expr, 0);
	      targetm.record_offload_symbol (link_ptr_decl);
	      varpool_node::finalize_decl (link_ptr_decl);
	    }
	  else
#endif
	    targetm.record_offload_symbol (it);
	}
    }
}

/* Call dim_pos (POS == true) or dim_size (POS == false) builtins for
   axis DIM.  Return a tmp var holding the result.  */

static tree
oacc_dim_call (bool pos, int dim, gimple_seq *seq)
{
  if (flag_openmp && flag_openmp_target == OMP_TARGET_MODE_OMPACC)
    {
      enum built_in_function fn;
      if (dim == GOMP_DIM_VECTOR)
	fn = pos ? BUILT_IN_OMP_GET_THREAD_NUM : BUILT_IN_OMP_GET_NUM_THREADS;
      else if (dim == GOMP_DIM_GANG)
	fn = pos ? BUILT_IN_OMP_GET_TEAM_NUM : BUILT_IN_OMP_GET_NUM_TEAMS;
      else
	gcc_unreachable ();
      tree size = create_tmp_var (integer_type_node);
      gimple *call = gimple_build_call (builtin_decl_explicit (fn), 0);
      gimple_call_set_lhs (call, size);
      gimple_seq_add_stmt (seq, call);
      return size;
    }

  tree arg = build_int_cst (unsigned_type_node, dim);
  tree size = create_tmp_var (integer_type_node);
  enum internal_fn fn = pos ? IFN_GOACC_DIM_POS : IFN_GOACC_DIM_SIZE;
  gimple *call = gimple_build_call_internal (fn, 1, arg);

  gimple_call_set_lhs (call, size);
  gimple_seq_add_stmt (seq, call);

  return size;
}

/* Find the number of threads (POS = false), or thread number (POS =
   true) for an OpenACC region partitioned as MASK.  If VF_BY_VECTORIZER is
   true, use that as the vectorization factor for the auto-vectorized
   dimension size, instead of calling the builtin function.  Setup code
   required for the calculation is added to SEQ.  */

static tree
oacc_thread_numbers (bool pos, int mask, tree vf_by_vectorizer, gimple_seq *seq)
{
  tree res = pos ? NULL_TREE : build_int_cst (unsigned_type_node, 1);
  unsigned ix;

  /* Start at gang level, and examine relevant dimension indices.  */
  for (ix = GOMP_DIM_GANG; ix != GOMP_DIM_MAX; ix++)
    if (GOMP_DIM_MASK (ix) & mask)
      {
	if (res)
	  {
	    /* We had an outer index, so scale that by the size of
	       this dimension.  */
	    tree n = (ix == GOMP_DIM_VECTOR && vf_by_vectorizer)
		     ? vf_by_vectorizer : oacc_dim_call (false, ix, seq);
	    res = fold_build2 (MULT_EXPR, integer_type_node, res, n);
	  }
	if (pos)
	  {
	    /* Determine index in this dimension.  */
	    tree id = (ix == GOMP_DIM_VECTOR && vf_by_vectorizer)
		      ? integer_zero_node :  oacc_dim_call (true, ix, seq);
	    if (res)
	      res = fold_build2 (PLUS_EXPR, integer_type_node, res, id);
	    else
	      res = id;
	  }
      }

  if (res == NULL_TREE)
    res = integer_zero_node;

  return res;
}

static tree
oacc_thread_numbers (bool pos, int mask, gimple_seq *seq)
{
  return oacc_thread_numbers (pos, mask, NULL_TREE, seq);
}

/* Transform IFN_GOACC_LOOP calls to actual code.  See
   expand_oacc_for for where these are generated.  At the vector
   level, we stride loops, such that each member of a warp will
   operate on adjacent iterations.  At the worker and gang level,
   each gang/warp executes a set of contiguous iterations.  Chunking
   can override this such that each iteration engine executes a
   contiguous chunk, and then moves on to stride to the next chunk.  */

static void
oacc_xform_loop (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  enum ifn_goacc_loop_kind code
    = (enum ifn_goacc_loop_kind) TREE_INT_CST_LOW (gimple_call_arg (call, 0));
  tree dir = gimple_call_arg (call, 1);
  tree range = gimple_call_arg (call, 2);
  tree step = gimple_call_arg (call, 3);
  tree chunk_size = NULL_TREE;
  unsigned mask = (unsigned) TREE_INT_CST_LOW (gimple_call_arg (call, 5));
  tree lhs = gimple_call_lhs (call);
  tree type = NULL_TREE;
  tree diff_type = TREE_TYPE (range);
  tree r = NULL_TREE;
  gimple_seq seq = NULL;
  bool chunking = false, striding = true;
  unsigned outer_mask = mask & (~mask + 1); // Outermost partitioning
  unsigned inner_mask = mask & ~outer_mask; // Inner partitioning (if any)
  tree vf_by_vectorizer = NULL_TREE;
  tree noalias = NULL_TREE;

  /* Skip lowering if return value of IFN_GOACC_LOOP call is not used.  */
  if (!lhs)
    {
      gsi_replace_with_seq (&gsi, seq, true);
      return;
    }

  type = TREE_TYPE (lhs);

#ifdef ACCEL_COMPILER
  chunk_size = gimple_call_arg (call, 4);
  if (integer_minus_onep (chunk_size)  /* Force static allocation.  */
      || integer_zerop (chunk_size))   /* Default (also static).  */
    {
      /* If we're at the gang level, we want each to execute a
	 contiguous run of iterations.  Otherwise we want each element
	 to stride.  */
      striding = !(outer_mask & GOMP_DIM_MASK (GOMP_DIM_GANG));
      chunking = false;
    }
  else
    {
      /* Chunk of size 1 is striding.  */
      striding = integer_onep (chunk_size);
      chunking = !striding;
    }

  if (!chunking
      && !targetm.simt.vf
      && (mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR)))
    {
      poly_uint64 max_vf = omp_max_vf ();
      vf_by_vectorizer = build_int_cst (integer_type_node, max_vf);
    }

#endif

  /* For SIMT targets:

     striding=true, chunking=true
       -> invalid.
     striding=true, chunking=false
       -> chunks=1
     striding=false,chunking=true
       -> chunks=ceil (range/(chunksize*threads*step))
     striding=false,chunking=false
       -> chunk_size=ceil(range/(threads*step)),chunks=1

     For non-SIMT targets:

      striding=N/A, chunking=true
	-> as above, for now.
      striding=N/A, chunking=false
	-> chunks=1
	   threads=gangs*workers*vf
	   chunk_size=ceil(range/(threads*step))
	   inner chunking loop steps by "step", vf*chunk_size times.
  */

  push_gimplify_context (true);

  switch (code)
    {
    default:
      gcc_unreachable ();

    case IFN_GOACC_LOOP_CHUNKS:
      noalias = gimple_call_arg (call, 6);
      if (!chunking)
        r = build_int_cst (type, 1);
      else
        {
          /* chunk_max
             = (range - dir) / (chunks * step * num_threads) + dir  */
          tree per = oacc_thread_numbers (false, mask, &seq);
          per = fold_convert (type, per);
          noalias = fold_convert (type, noalias);
          per = fold_build2 (MULT_EXPR, type, per, noalias);
          per = fold_build2 (MAX_EXPR, type, per, fold_convert (type, integer_one_node));
          chunk_size = fold_convert (type, chunk_size);
          per = fold_build2 (MULT_EXPR, type, per, chunk_size);
          per = fold_build2 (MULT_EXPR, type, per, step);
          r = fold_build2 (MINUS_EXPR, type, range, dir);
          r = fold_build2 (PLUS_EXPR, type, r, per);
          r = build2 (TRUNC_DIV_EXPR, type, r, per);
        }
      break;

    case IFN_GOACC_LOOP_STEP:
      noalias = gimple_call_arg (call, 6);
      {
        if (vf_by_vectorizer)
          r = step;
        else
          {
            /* If striding, step by the entire compute volume, otherwise
               step by the inner volume.  */
            unsigned volume = striding ? mask : inner_mask;

            noalias = fold_convert (type, noalias);
            r = oacc_thread_numbers (false, volume, &seq);
            r = fold_convert (type, r);
            r = build2 (MULT_EXPR, type, r, noalias);
            r = build2 (MAX_EXPR, type, r, fold_convert (type, fold_convert (type, integer_one_node)));
            r = build2 (MULT_EXPR, type, fold_convert (type, r), step);
          }
        break;
      }

      case IFN_GOACC_LOOP_OFFSET:
	noalias = gimple_call_arg (call, 7);
        if (vf_by_vectorizer)
          {
            /* If not -fno-tree-loop-vectorize, hint that we want to vectorize
               the loop.  */
            if (flag_tree_loop_vectorize
                || !OPTION_SET_P (flag_tree_loop_vectorize))
              {
                /* Enable vectorization on non-SIMT targets.  */
                basic_block bb = gsi_bb (gsi);
                class loop *chunk_loop = bb->loop_father;
                class loop *inner_loop = chunk_loop->inner;

                /* Chunking isn't supported for VF_BY_VECTORIZER loops yet,
                   so we know that the outer chunking loop will be executed
                   just once and the inner loop is the one which must be
                   vectorized (unless it has been optimized out for some
                   reason).  */
                gcc_assert (!chunking);

                if (inner_loop)
                  {
                    inner_loop->force_vectorize = true;
                    inner_loop->safelen = INT_MAX;

                    cfun->has_force_vectorize_loops = true;
                  }
              }

            /* ...and expand the abstract loops such that the vectorizer can
               work on them more effectively.

               It might be nicer to merge this code with the "!striding" case
               below, particularly if chunking support is added.  */
            tree warppos
                = oacc_thread_numbers (true, mask, vf_by_vectorizer, &seq);
            warppos = fold_convert (diff_type, warppos);

            tree volume
                = oacc_thread_numbers (false, mask, vf_by_vectorizer, &seq);
            volume = fold_convert (diff_type, volume);

            tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);
            chunk_size = fold_build2 (PLUS_EXPR, diff_type, range, per);
            chunk_size = fold_build2 (MINUS_EXPR, diff_type, chunk_size, dir);
            chunk_size
                = fold_build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);

            warppos = fold_build2 (MULT_EXPR, diff_type, warppos, chunk_size);

            tree chunk = fold_convert (diff_type, gimple_call_arg (call, 6));
            chunk = fold_build2 (MULT_EXPR, diff_type, chunk, volume);
            r = fold_build2 (PLUS_EXPR, diff_type, chunk, warppos);
          }
        else if (striding)
          {
            r = oacc_thread_numbers (true, mask, &seq);
            r = fold_convert (diff_type, r);
            tree tmp1 = build2 (NE_EXPR, boolean_type_node, r,
                                fold_convert (diff_type, integer_zero_node));
            tree tmp2 = build2 (EQ_EXPR, boolean_type_node, noalias,
                                boolean_false_node);
            tree tmp3 = build2 (BIT_AND_EXPR, diff_type,
                                fold_convert (diff_type, tmp1),
                                fold_convert (diff_type, tmp2));
            tree tmp4 = build2 (MULT_EXPR, diff_type, tmp3, range);
            r = build2 (PLUS_EXPR, diff_type, r, tmp4);
          }
        else
          {
            tree inner_size = oacc_thread_numbers (false, inner_mask, &seq);
            tree outer_size = oacc_thread_numbers (false, outer_mask, &seq);
            tree volume = fold_build2 (MULT_EXPR, TREE_TYPE (inner_size),
                                       inner_size, outer_size);

            volume = fold_convert (diff_type, volume);
            if (chunking)
              chunk_size = fold_convert (diff_type, chunk_size);
            else
              {
                tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);
                /* chunk_size = (range + per - 1) / per.  */
                chunk_size = build2 (MINUS_EXPR, diff_type, range, dir);
                chunk_size = build2 (PLUS_EXPR, diff_type, chunk_size, per);
                chunk_size = build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);
              }

            /* Curtail the range in all but one thread when there may be
               aliasing to prevent parallelization.  */
            tree n = oacc_thread_numbers (true, mask, &seq);
            n = fold_convert (diff_type, n);
            tree tmp1 = build2 (NE_EXPR, boolean_type_node, n,
                                fold_convert (diff_type, integer_zero_node));
            tree tmp2 = build2 (EQ_EXPR, boolean_type_node, noalias,
                                boolean_false_node);
            tree tmp3 = build2 (BIT_AND_EXPR, diff_type,
                                fold_convert (diff_type, tmp1),
                                fold_convert (diff_type, tmp2));
            range = build2 (MULT_EXPR, diff_type, tmp3, range);

            tree span = build2 (MULT_EXPR, diff_type, chunk_size,
                                fold_convert (diff_type, inner_size));
            r = oacc_thread_numbers (true, outer_mask, &seq);
            r = fold_convert (diff_type, r);
            r = build2 (PLUS_EXPR, diff_type, r, range);
            r = build2 (MULT_EXPR, diff_type, r, span);

            tree inner = oacc_thread_numbers (true, inner_mask, &seq);

            inner = fold_convert (diff_type, inner);
            r = fold_build2 (PLUS_EXPR, diff_type, r, inner);

            if (chunking)
              {
                tree chunk
                    = fold_convert (diff_type, gimple_call_arg (call, 6));
                tree per
                    = fold_build2 (MULT_EXPR, diff_type, volume, chunk_size);
                per = build2 (MULT_EXPR, diff_type, per, chunk);

                r = build2 (PLUS_EXPR, diff_type, r, per);
              }
          }
        r = fold_build2 (MULT_EXPR, diff_type, r, step);
        if (type != diff_type)
          r = fold_convert (type, r);
        break;

      case IFN_GOACC_LOOP_BOUND:
        if (vf_by_vectorizer)
          {
            tree volume
                = oacc_thread_numbers (false, mask, vf_by_vectorizer, &seq);
            volume = fold_convert (diff_type, volume);

            tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);
            chunk_size = fold_build2 (PLUS_EXPR, diff_type, range, per);
            chunk_size = fold_build2 (MINUS_EXPR, diff_type, chunk_size, dir);
            chunk_size
                = fold_build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);

            vf_by_vectorizer = fold_convert (diff_type, vf_by_vectorizer);
            tree vecsize = fold_build2 (MULT_EXPR, diff_type, chunk_size,
                                        vf_by_vectorizer);
            vecsize = fold_build2 (MULT_EXPR, diff_type, vecsize, step);
            tree vecend = fold_convert (diff_type, gimple_call_arg (call, 6));
            vecend = fold_build2 (PLUS_EXPR, diff_type, vecend, vecsize);
            r = fold_build2 (integer_onep (dir) ? MIN_EXPR : MAX_EXPR,
                             diff_type, range, vecend);
          }
        else if (striding)
          r = range;
        else
          {
            noalias = fold_convert (diff_type, gimple_call_arg (call, 7));

            tree inner_size = oacc_thread_numbers (false, inner_mask, &seq);
            tree outer_size = oacc_thread_numbers (false, outer_mask, &seq);
            tree volume = fold_build2 (MULT_EXPR, TREE_TYPE (inner_size),
                                       inner_size, outer_size);

            volume = fold_convert (diff_type, volume);
            volume = fold_build2 (MULT_EXPR, diff_type, volume, noalias);
            volume
                = fold_build2 (MAX_EXPR, diff_type, volume, fold_convert (diff_type, integer_one_node));
            if (chunking)
              chunk_size = fold_convert (diff_type, chunk_size);
            else
              {
                tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);
                /* chunk_size = (range + per - 1) / per.  */
                chunk_size = build2 (MINUS_EXPR, diff_type, range, dir);
                chunk_size = build2 (PLUS_EXPR, diff_type, chunk_size, per);
                chunk_size
                    = build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);
              }

            tree span = build2 (MULT_EXPR, diff_type, chunk_size,
                                fold_convert (diff_type, inner_size));

            r = fold_build2 (MULT_EXPR, diff_type, span, step);

            tree offset = gimple_call_arg (call, 6);
            r = build2 (PLUS_EXPR, diff_type, r,
                        fold_convert (diff_type, offset));
            r = build2 (integer_onep (dir) ? MIN_EXPR : MAX_EXPR, diff_type, r,
                        range);
          }
        if (diff_type != type)
          r = fold_convert (type, r);
        break;
    }

  gimplify_assign (lhs, r, &seq);

  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* This is used for expanding the loop calls to "fake" values that mimic the
   values used for host execution during scalar evolution analysis in
   Graphite. The function has been derived from oacc_xform_loop which could not
   be used because it rewrites the code directly.

   TODO This function can either be simplified significantly (cf. the fixed
   values for number_of_threads, thread_index, chunking, striding) or unified
   with oacc_xform_loop. */

tree
oacc_extract_loop_call (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  enum ifn_goacc_loop_kind code
      = (enum ifn_goacc_loop_kind)TREE_INT_CST_LOW (gimple_call_arg (call, 0));
  tree dir = gimple_call_arg (call, 1);
  tree range = gimple_call_arg (call, 2);
  tree step = gimple_call_arg (call, 3);
  tree chunk_size = NULL_TREE;
  unsigned mask = (unsigned)TREE_INT_CST_LOW (gimple_call_arg (call, 5));
  tree lhs = gimple_call_lhs (call);
  tree type = NULL_TREE;
  tree diff_type = TREE_TYPE (range);
  tree r = NULL_TREE;
  bool chunking = false, striding = true;
  unsigned outer_mask = mask & (~mask + 1); // Outermost partitioning
  /* unsigned inner_mask = mask & ~outer_mask; // Inner partitioning (if any)
   */

  gcc_checking_assert (lhs);

  type = TREE_TYPE (lhs);

  tree number_of_threads = integer_one_node;
  tree thread_index = integer_zero_node;

  /* striding=true, chunking=true
       -> invalid.
     striding=true, chunking=false
       -> chunks=1
     striding=false,chunking=true
       -> chunks=ceil (range/(chunksize*threads*step))
     striding=false,chunking=false
       -> chunk_size=ceil(range/(threads*step)),chunks=1  */

  switch (code)
    {
    default:
      gcc_unreachable ();

    case IFN_GOACC_LOOP_CHUNKS:
      if (!chunking)
	r = build_int_cst (type, 1);
      else
	{
	  /* chunk_max
	     = (range - dir) / (chunks * step * num_threads) + dir  */
	  tree per = number_of_threads;
	  per = fold_convert (type, per);
	  chunk_size = fold_convert (type, chunk_size);
	  per = fold_build2 (MULT_EXPR, type, per, chunk_size);
	  per = fold_build2 (MULT_EXPR, type, per, step);
	  r = fold_build2 (MINUS_EXPR, type, range, dir);
	  r = fold_build2 (PLUS_EXPR, type, r, per);
	  r = fold_build2 (TRUNC_DIV_EXPR, type, r, per);
	}
      break;

    case IFN_GOACC_LOOP_STEP:
      {
	/* If striding, step by the entire compute volume, otherwise
	   step by the inner volume.  */
	/* unsigned volume = striding ? mask : inner_mask; */

	r = number_of_threads;
	r = fold_build2 (MULT_EXPR, type, fold_convert (type, r), step);
      }
      break;

    case IFN_GOACC_LOOP_OFFSET:
      /* Enable vectorization on non-SIMT targets.  */
      if (!targetm.simt.vf
	  && outer_mask == GOMP_DIM_MASK (GOMP_DIM_VECTOR)
	  /* If not -fno-tree-loop-vectorize, hint that we want to vectorize
	     the loop.  */
	  && (flag_tree_loop_vectorize
	      || !global_options_set.x_flag_tree_loop_vectorize))
	{
	  basic_block bb = gsi_bb (gsi);
	  class loop *parent = bb->loop_father;
	  class loop *body = parent->inner;

	  parent->force_vectorize = true;
	  parent->safelen = INT_MAX;

	  /* "Chunking loops" may have inner loops.  */
	  if (parent->inner)
	    {
	      body->force_vectorize = true;
	      body->safelen = INT_MAX;
	    }

	  cfun->has_force_vectorize_loops = true;
	}
      if (striding)
	{
	  r = thread_index;
	  r = fold_convert (diff_type, r);
	}
      else
	{
	  tree inner_size = number_of_threads;
	  tree outer_size = number_of_threads;
	  tree volume = fold_build2 (MULT_EXPR, TREE_TYPE (inner_size),
				     inner_size, outer_size);

	  volume = fold_convert (diff_type, volume);
	  if (chunking)
	    chunk_size = fold_convert (diff_type, chunk_size);
	  else
	    {
	      tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);

	      chunk_size = fold_build2 (MINUS_EXPR, diff_type, range, dir);
	      chunk_size = fold_build2 (PLUS_EXPR, diff_type, chunk_size, per);
	      chunk_size
		  = fold_build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);
	    }

	  tree span = fold_build2 (MULT_EXPR, diff_type, chunk_size,
				   fold_convert (diff_type, inner_size));
	  r = thread_index;
	  r = fold_convert (diff_type, r);
	  r = fold_build2 (MULT_EXPR, diff_type, r, span);

	  tree inner = thread_index;
	  inner = fold_convert (diff_type, inner);
	  r = fold_build2 (PLUS_EXPR, diff_type, r, inner);

	  if (chunking)
	    {
	      tree chunk = fold_convert (diff_type, gimple_call_arg (call, 6));
	      tree per
		  = fold_build2 (MULT_EXPR, diff_type, volume, chunk_size);
	      per = fold_build2 (MULT_EXPR, diff_type, per, chunk);

	      r = fold_build2 (PLUS_EXPR, diff_type, r, per);
	    }
	}
      r = fold_build2 (MULT_EXPR, diff_type, r, step);
      if (type != diff_type)
	r = fold_convert (type, r);
      break;

    case IFN_GOACC_LOOP_BOUND:
      if (striding)
	r = range;
      else
	{
	  tree inner_size = number_of_threads;
	  tree outer_size = number_of_threads;
	  tree volume = fold_build2 (MULT_EXPR, TREE_TYPE (inner_size),
				     inner_size, outer_size);

	  volume = fold_convert (diff_type, volume);
	  if (chunking)
	    chunk_size = fold_convert (diff_type, chunk_size);
	  else
	    {
	      tree per = fold_build2 (MULT_EXPR, diff_type, volume, step);

	      chunk_size = fold_build2 (MINUS_EXPR, diff_type, range, dir);
	      chunk_size = fold_build2 (PLUS_EXPR, diff_type, chunk_size, per);
	      chunk_size
		  = fold_build2 (TRUNC_DIV_EXPR, diff_type, chunk_size, per);
	    }

	  tree span = fold_build2 (MULT_EXPR, diff_type, chunk_size,
				   fold_convert (diff_type, inner_size));

	  r = fold_build2 (MULT_EXPR, diff_type, span, step);

	  tree offset = gimple_call_arg (call, 6);
	  r = fold_build2 (PLUS_EXPR, diff_type, r,
			   fold_convert (diff_type, offset));
	  r = fold_build2 (integer_onep (dir) ? MIN_EXPR : MAX_EXPR, diff_type,
			   r, range);
	}
      if (diff_type != type)
	r = fold_convert (type, r);
      break;
    }

  return r;
}

/* Transform a GOACC_TILE call.  Determines the element loop span for
   the specified loop of the nest.  This is 1 if we're not tiling.
   
   GOACC_TILE (collapse_count, loop_no, tile_arg, gwv_tile, gwv_element);  */

static void
oacc_xform_tile (gcall *call)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  unsigned collapse = tree_to_uhwi (gimple_call_arg (call, 0));
  /* Inner loops have higher loop_nos.  */
  unsigned loop_no = tree_to_uhwi (gimple_call_arg (call, 1));
  tree tile_size = gimple_call_arg (call, 2);
  unsigned e_mask = tree_to_uhwi (gimple_call_arg (call, 4));
  tree lhs = gimple_call_lhs (call);
  tree type = TREE_TYPE (lhs);
  gimple_seq seq = NULL;
  tree span = build_int_cst (type, 1);

  gcc_assert (!(e_mask
		& ~(GOMP_DIM_MASK (GOMP_DIM_VECTOR)
		    | GOMP_DIM_MASK (GOMP_DIM_WORKER))));
  push_gimplify_context (!seen_error ());

#ifndef ACCEL_COMPILER
  /* Partitioning disabled on host compilers.  */
  e_mask = 0;
#endif
  if (!e_mask)
    /* Not paritioning.  */
    span = integer_one_node;
  else if (!integer_zerop (tile_size))
    /* User explicitly specified size.  */
    span = tile_size;
  else
    {
      /* Pick a size based on the paritioning of the element loop and
	 the number of loop nests.  */
      tree first_size = NULL_TREE;
      tree second_size = NULL_TREE;

      if (e_mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR))
	first_size = oacc_dim_call (false, GOMP_DIM_VECTOR, &seq);
      if (e_mask & GOMP_DIM_MASK (GOMP_DIM_WORKER))
	second_size = oacc_dim_call (false, GOMP_DIM_WORKER, &seq);

      if (!first_size)
	{
	  first_size = second_size;
	  second_size = NULL_TREE;
	}

      if (loop_no + 1 == collapse)
	{
	  span = first_size;
	  if (!loop_no && second_size)
	    span = fold_build2 (MULT_EXPR, TREE_TYPE (span),
				span, second_size);
	}
      else if (loop_no + 2 == collapse)
	span = second_size;
      else
	span = NULL_TREE;

      if (!span)
	/* There's no obvious element size for this loop.  Options
	   are 1, first_size or some non-unity constant (32 is my
	   favourite).   We should gather some statistics.  */
	span = first_size;
    }

  span = fold_convert (type, span);
  gimplify_assign (lhs, span, &seq);

  pop_gimplify_context (NULL);

  gsi_replace_with_seq (&gsi, seq, true);
}

/* Default partitioned and minimum partitioned dimensions.  */

static int oacc_default_dims[GOMP_DIM_MAX];
static int oacc_min_dims[GOMP_DIM_MAX];

int
oacc_get_default_dim (int dim)
{
  gcc_assert (0 <= dim && dim < GOMP_DIM_MAX);
  return oacc_default_dims[dim];
}

int
oacc_get_min_dim (int dim)
{
  gcc_assert (0 <= dim && dim < GOMP_DIM_MAX);
  return oacc_min_dims[dim];
}

/* Parse the default dimension parameter.  This is a set of
   :-separated optional compute dimensions.  Each dimension is either
   a positive integer, or '-' for a dynamic value computed at
   runtime.  When device type support is added, it is
   planned to be a comma separated list of such compute dimensions,
   with all but the first prefixed by the colon-terminated device
   type.  */

static void
oacc_parse_default_dims (const char *dims)
{
  int ix;

  for (ix = GOMP_DIM_MAX; ix--;)
    {
      oacc_default_dims[ix] = -1;
      oacc_min_dims[ix] = 1;
    }

#ifndef ACCEL_COMPILER
  /* Cannot be overridden on the host.  */
  dims = NULL;
#endif
  if (dims)
    {
      const char *pos = dims;

      for (ix = 0; *pos && ix != GOMP_DIM_MAX; ix++)
	{
	  if (ix)
	    {
	      if (*pos != ':')
		goto malformed;
	      pos++;
	    }

	  if (*pos != ':')
	    {
	      long val = 0;

	      if (*pos == '-')
		pos++;
	      else
		{
		  const char *eptr;

		  errno = 0;
		  val = strtol (pos, CONST_CAST (char **, &eptr), 10);
		  if (errno || val <= 0 || (int) val != val)
		    goto malformed;
		  pos = eptr;
		}
	      oacc_default_dims[ix] = (int) val;
	    }
	}
      if (*pos)
	{
	malformed:
	  error_at (UNKNOWN_LOCATION,
		    "%<-fopenacc-dim%> operand is malformed at %qs", pos);
	}
    }

  /* Allow the backend to validate the dimensions.  */
  targetm.goacc.validate_dims (NULL_TREE, oacc_default_dims, -1, 0);
  targetm.goacc.validate_dims (NULL_TREE, oacc_min_dims, -2, 0);
}

/* Remove parallelism dimensions below LEVEL which are not set in USED
   from DIMS and emit a warning pointing to the location of FN. */

static void
oacc_remove_unused_partitioning (tree fn, int *dims, int level, unsigned used)
{

  bool host_compiler = true;
#ifdef ACCEL_COMPILER
  host_compiler = false;
#endif

  static char const *const axes[] =
      /* Must be kept in sync with GOMP_DIM enumeration.  */
      { "gang", "worker", "vector" };

  char removed_partitions[20] = "\0";
  for (int ix = level >= 0 ? level : 0; ix != GOMP_DIM_MAX; ix++)
    if (!(used & GOMP_DIM_MASK (ix)) && dims[ix] >= 0)
      {
        if (host_compiler)
          {
            strcat (removed_partitions, axes[ix]);
            strcat (removed_partitions, " ");
          }
        dims[ix] = -1;
      }
  if (removed_partitions[0] != '\0')
    warning_at (DECL_SOURCE_LOCATION (fn), OPT_Wopenacc_parallelism,
                "removed %spartitioning from %<kernels%> region",
                removed_partitions);
}

/* Validate and update the dimensions for offloaded FN.  ATTRS is the
   raw attribute.  DIMS is an array of dimensions, which is filled in.
   LEVEL is the partitioning level of a routine, or -1 for an offload
   region itself.  USED is the mask of partitioned execution in the
   function.  */

static void
oacc_validate_dims (tree fn, tree attrs, int *dims, int level, unsigned used)
{
  tree purpose[GOMP_DIM_MAX];
  unsigned ix;
  tree pos = TREE_VALUE (attrs);

  /* Make sure the attribute creator attached the dimension
     information.  */
  gcc_assert (pos);

  for (ix = 0; ix != GOMP_DIM_MAX; ix++)
    {
      purpose[ix] = TREE_PURPOSE (pos);

      tree val = TREE_VALUE (pos);
      dims[ix] = val ? TREE_INT_CST_LOW (val) : -1;
      pos = TREE_CHAIN (pos);
    }

  bool check = true;
#ifdef ACCEL_COMPILER
  check = false;
#endif

  static char const *const axes[] =
      /* Must be kept in sync with GOMP_DIM enumeration.  */
      { "gang", "worker", "vector" };

  if (check
      && warn_openacc_parallelism
      && !lookup_attribute ("oacc kernels", DECL_ATTRIBUTES (fn)))
    {
      for (ix = level >= 0 ? level : 0; ix != GOMP_DIM_MAX; ix++)
	if (dims[ix] < 0)
	  ; /* Defaulting axis.  */
	else if ((used & GOMP_DIM_MASK (ix)) && dims[ix] == 1)
	  /* There is partitioned execution, but the user requested a
	     dimension size of 1.  They're probably confused.  */
	  warning_at (DECL_SOURCE_LOCATION (fn), OPT_Wopenacc_parallelism,
		      "region contains %s partitioned code but"
		      " is not %s partitioned", axes[ix], axes[ix]);
	else if (!(used & GOMP_DIM_MASK (ix)) && dims[ix] != 1)
	  {
	  /* The dimension is explicitly partitioned to non-unity, but
	     no use is made within the region.  */
	  warning_at (DECL_SOURCE_LOCATION (fn), OPT_Wopenacc_parallelism,
		      "region is %s partitioned but"
		      " does not contain %s partitioned code",
		      axes[ix], axes[ix]);
          }
    }

  if (lookup_attribute ("oacc parallel_kernels_graphite",
                         DECL_ATTRIBUTES (fn)))
    oacc_remove_unused_partitioning  (fn, dims, level, used);

  bool changed = targetm.goacc.validate_dims (fn, dims, level, used);

  /* Default anything left to 1 or a partitioned default.  */
  for (ix = 0; ix != GOMP_DIM_MAX; ix++)
    if (dims[ix] < 0)
      {
	/* The OpenACC spec says 'If the [num_gangs] clause is not
	   specified, an implementation-defined default will be used;
	   the default may depend on the code within the construct.'
	   (2.5.6).  Thus an implementation is free to choose
	   non-unity default for a parallel region that doesn't have
	   any gang-partitioned loops.  However, it appears that there
	   is a sufficient body of user code that expects non-gang
	   partitioned regions to not execute in gang-redundant mode.
	   So we (a) don't warn about the non-portability and (b) pick
	   the minimum permissible dimension size when there is no
	   partitioned execution.  Otherwise we pick the global
	   default for the dimension, which the user can control.  The
	   same wording and logic applies to num_workers and
	   vector_length, however the worker- or vector- single
	   execution doesn't have the same impact as gang-redundant
	   execution.  (If the minimum gang-level partioning is not 1,
	   the target is probably too confusing.)  */
	dims[ix] = (used & GOMP_DIM_MASK (ix)
		    ? oacc_default_dims[ix] : oacc_min_dims[ix]);
	changed = true;
      }

  if (changed)
    {
      /* Replace the attribute with new values.  */
      pos = NULL_TREE;
      for (ix = GOMP_DIM_MAX; ix--;)
	pos = tree_cons (purpose[ix],
			 build_int_cst (integer_type_node, dims[ix]), pos);
      oacc_replace_fn_attrib (fn, pos);
    }
}

/* Create an empty OpenACC loop structure at LOC.  */

static oacc_loop *
new_oacc_loop_raw (oacc_loop *parent, location_t loc)
{
  oacc_loop *loop = XCNEW (oacc_loop);

  loop->parent = parent;

  if (parent)
    {
      loop->sibling = parent->child;
      parent->child = loop;
    }

  loop->loc = loc;
  return loop;
}

/* Create an outermost, dummy OpenACC loop for offloaded function
   DECL.  */

static oacc_loop *
new_oacc_loop_outer (tree decl)
{
  return new_oacc_loop_raw (NULL, DECL_SOURCE_LOCATION (decl));
}

/* Start a new OpenACC loop  structure beginning at head marker HEAD.
   Link into PARENT loop.  Return the new loop.  */

static oacc_loop *
new_oacc_loop (oacc_loop *parent, gcall *marker)
{
  oacc_loop *loop = new_oacc_loop_raw (parent, gimple_location (marker));

  loop->marker = marker;

  /* TODO: This is where device_type flattening would occur for the loop
     flags.  */

  loop->flags = TREE_INT_CST_LOW (gimple_call_arg (marker, 3));

  tree chunk_size = integer_zero_node;
  if (loop->flags & OLF_GANG_STATIC)
    chunk_size = gimple_call_arg (marker, 4);
  loop->chunk_size = chunk_size;

  return loop;
}

/* Create a dummy loop encompassing a call to a openACC routine.
   Extract the routine's partitioning requirements.  */

static void
new_oacc_loop_routine (oacc_loop *parent, gcall *call, tree decl, tree attrs)
{
  oacc_loop *loop = new_oacc_loop_raw (parent, gimple_location (call));
  int level = oacc_fn_attrib_level (attrs);

  gcc_assert (level >= 0);

  loop->marker = call;
  loop->routine = decl;
  loop->mask = ((GOMP_DIM_MASK (GOMP_DIM_MAX) - 1)
		^ (GOMP_DIM_MASK (level) - 1));
}

/* Finish off the current OpenACC loop ending at tail marker TAIL.
   Return the parent loop.  */

static oacc_loop *
finish_oacc_loop (oacc_loop *loop)
{
  /* If the loop has been collapsed, don't partition it.  */
  if (loop->ifns.is_empty ())
    loop->mask = loop->flags = 0;
  return loop->parent;
}

/* Free all OpenACC loop structures within LOOP (inclusive).  */

static void
free_oacc_loop (oacc_loop *loop)
{
  if (loop->sibling)
    free_oacc_loop (loop->sibling);
  if (loop->child)
    free_oacc_loop (loop->child);

  loop->ifns.release ();
  free (loop);
}

/* Dump out the OpenACC loop head or tail beginning at FROM.  */

static void
dump_oacc_loop_part (FILE *file, gcall *from, int depth,
		     const char *title, int level)
{
  enum ifn_unique_kind kind
    = (enum ifn_unique_kind) TREE_INT_CST_LOW (gimple_call_arg (from, 0));

  fprintf (file, "%*s%s-%d:\n", depth * 2, "", title, level);
  for (gimple_stmt_iterator gsi = gsi_for_stmt (from);;)
    {
      gimple *stmt = gsi_stmt (gsi);

      if (gimple_call_internal_p (stmt, IFN_UNIQUE))
	{
	  enum ifn_unique_kind k
	    = ((enum ifn_unique_kind) TREE_INT_CST_LOW
	       (gimple_call_arg (stmt, 0)));

	  if (k == kind && stmt != from)
	    break;
	}
      print_gimple_stmt (file, stmt, depth * 2 + 2);

      gsi_next (&gsi);
      while (gsi_end_p (gsi))
	gsi = gsi_start_bb (single_succ (gsi_bb (gsi)));
    }
}

/* Dump OpenACC loop LOOP, its children, and its siblings.  */

static void
dump_oacc_loop (FILE *file, oacc_loop *loop, int depth)
{
  int ix;

  fprintf (file, "%*sLoop %x(%x) %s:%u\n", depth * 2, "",
	   loop->flags, loop->mask,
	   LOCATION_FILE (loop->loc), LOCATION_LINE (loop->loc));

  if (loop->marker)
    print_gimple_stmt (file, loop->marker, depth * 2);

  if (loop->routine)
    fprintf (file, "%*sRoutine %s:%u:%s\n",
	     depth * 2, "", DECL_SOURCE_FILE (loop->routine),
	     DECL_SOURCE_LINE (loop->routine),
	     IDENTIFIER_POINTER (DECL_NAME (loop->routine)));

  for (ix = GOMP_DIM_GANG; ix != GOMP_DIM_MAX; ix++)
    if (loop->heads[ix])
      dump_oacc_loop_part (file, loop->heads[ix], depth, "Head", ix);
  for (ix = GOMP_DIM_MAX; ix--;)
    if (loop->tails[ix])
      dump_oacc_loop_part (file, loop->tails[ix], depth, "Tail", ix);

  if (loop->child)
    dump_oacc_loop (file, loop->child, depth + 1);
  if (loop->sibling)
    dump_oacc_loop (file, loop->sibling, depth);
}

void debug_oacc_loop (oacc_loop *);

/* Dump loops to stderr.  */

DEBUG_FUNCTION void
debug_oacc_loop (oacc_loop *loop)
{
  dump_oacc_loop (stderr, loop, 0);
}

/* Provide diagnostics on OpenACC loop LOOP, its children, and its
   siblings.  */

static void
inform_oacc_loop (const oacc_loop *loop)
{
  const char *gang
    = loop->mask & GOMP_DIM_MASK (GOMP_DIM_GANG) ? " gang" : "";
  const char *worker
    = loop->mask & GOMP_DIM_MASK (GOMP_DIM_WORKER) ? " worker" : "";
  const char *vector
    = loop->mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR) ? " vector" : "";
  const char *seq = loop->mask == 0 ? " seq" : "";
  const dump_user_location_t loc
    = dump_user_location_t::from_location_t (loop->loc);
  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
		   "assigned OpenACC%s%s%s%s loop parallelism\n", gang, worker,
		   vector, seq);

  if (loop->child)
    inform_oacc_loop (loop->child);
  if (loop->sibling)
    inform_oacc_loop (loop->sibling);
}

/* DFS walk of basic blocks BB onwards, creating OpenACC loop
   structures as we go.  By construction these loops are properly
   nested.  */

static void
oacc_loop_discover_walk (oacc_loop *loop, basic_block bb)
{
  int marker = 0;
  int remaining = 0;

  if (bb->flags & BB_VISITED)
    return;

 follow:
  bb->flags |= BB_VISITED;

  /* Scan for loop markers.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      if (!is_gimple_call (stmt))
	continue;

      gcall *call = as_a <gcall *> (stmt);

      /* If this is a routine, make a dummy loop for it.  */
      if (tree decl = gimple_call_fndecl (call))
	if (tree attrs = oacc_get_fn_attrib (decl))
	  {
	    gcc_assert (!marker);
	    new_oacc_loop_routine (loop, call, decl, attrs);
	  }

      if (!gimple_call_internal_p (call))
	continue;

      switch (gimple_call_internal_fn (call))
	{
	default:
	  break;

	case IFN_GOACC_LOOP:
	case IFN_GOACC_TILE:
	  /* Record the abstraction function, so we can manipulate it
	     later.  */
	  loop->ifns.safe_push (call);
	  break;

	case IFN_UNIQUE:
	  enum ifn_unique_kind kind
	    = (enum ifn_unique_kind) (TREE_INT_CST_LOW
				      (gimple_call_arg (call, 0)));
	  if (kind == IFN_UNIQUE_OACC_HEAD_MARK
	      || kind == IFN_UNIQUE_OACC_TAIL_MARK)
	    {
	      if (gimple_call_num_args (call) == 2)
		{
		  gcc_assert (marker && !remaining);
		  marker = 0;
		  if (kind == IFN_UNIQUE_OACC_TAIL_MARK)
		    loop = finish_oacc_loop (loop);
		  else
		    loop->head_end = call;
		}
	      else
		{
		  int count = TREE_INT_CST_LOW (gimple_call_arg (call, 2));

		  if (!marker)
		    {
		      if (kind == IFN_UNIQUE_OACC_HEAD_MARK)
			loop = new_oacc_loop (loop, call);
		      remaining = count;
		    }
		  gcc_assert (count == remaining);
		  if (remaining)
		    {
		      remaining--;
		      if (kind == IFN_UNIQUE_OACC_HEAD_MARK)
			loop->heads[marker] = call;
		      else
			loop->tails[remaining] = call;
		    }
		  marker++;
		}
	    }
	}
    }
  if (remaining || marker)
    {
      bb = single_succ (bb);
      gcc_assert (single_pred_p (bb) && !(bb->flags & BB_VISITED));
      goto follow;
    }

  /* Walk successor blocks.  */
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    oacc_loop_discover_walk (loop, e->dest);
}

/* LOOP is the first sibling.  Reverse the order in place and return
   the new first sibling.  Recurse to child loops.  */

static oacc_loop *
oacc_loop_sibling_nreverse (oacc_loop *loop)
{
  oacc_loop *last = NULL;
  do
    {
      if (loop->child)
	loop->child = oacc_loop_sibling_nreverse (loop->child);

      oacc_loop *next = loop->sibling;
      loop->sibling = last;
      last = loop;
      loop = next;
    }
  while (loop);

  return last;
}

/* Discover the OpenACC loops marked up by HEAD and TAIL markers for
   the current function.  */

static oacc_loop *
oacc_loop_discovery ()
{
  /* Clear basic block flags, in particular BB_VISITED which we're going to use
     in the following.  */
  clear_bb_flags ();

  oacc_loop *top = new_oacc_loop_outer (current_function_decl);
  oacc_loop_discover_walk (top, ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* The siblings were constructed in reverse order, reverse them so
     that diagnostics come out in an unsurprising order.  */
  top = oacc_loop_sibling_nreverse (top);

  return top;
}

/* Transform the abstract internal function markers starting at FROM
   to be for partitioning level LEVEL.  Stop when we meet another HEAD
   or TAIL  marker.  */

static void
oacc_loop_xform_head_tail (gcall *from, int level)
{
  enum ifn_unique_kind kind
    = (enum ifn_unique_kind) TREE_INT_CST_LOW (gimple_call_arg (from, 0));
  tree replacement = build_int_cst (unsigned_type_node, level);

  for (gimple_stmt_iterator gsi = gsi_for_stmt (from);;)
    {
      gimple *stmt = gsi_stmt (gsi);

      if (gimple_call_internal_p (stmt, IFN_UNIQUE))
	{
	  enum ifn_unique_kind k
	    = ((enum ifn_unique_kind)
	       TREE_INT_CST_LOW (gimple_call_arg (stmt, 0)));

	  if (k == IFN_UNIQUE_OACC_FORK
	      || k == IFN_UNIQUE_OACC_JOIN
	      || k == IFN_UNIQUE_OACC_PRIVATE)
	    *gimple_call_arg_ptr (stmt, 2) = replacement;
	  else if (k == kind && stmt != from)
	    break;
	}
      else if (gimple_call_internal_p (stmt, IFN_GOACC_REDUCTION))
	*gimple_call_arg_ptr (stmt, 3) = replacement;
      update_stmt (stmt);

      gsi_next (&gsi);
      while (gsi_end_p (gsi))
	gsi = gsi_start_bb (single_succ (gsi_bb (gsi)));
    }
}

/* Process the discovered OpenACC loops, setting the correct
   partitioning level etc.  */

static void
oacc_loop_process (oacc_loop *loop, int fn_level)
{
  if (loop->child)
    oacc_loop_process (loop->child, fn_level);

  if (loop->mask && !loop->routine)
    {
      int ix;
      tree mask_arg = build_int_cst (unsigned_type_node, loop->mask);
      tree e_mask_arg = build_int_cst (unsigned_type_node, loop->e_mask);
      tree chunk_arg = loop->chunk_size;
      gcall *call;
      
      for (ix = 0; loop->ifns.iterate (ix, &call); ix++)
	{
	  switch (gimple_call_internal_fn (call))
	    {
	    case IFN_GOACC_LOOP:
	      {
		bool is_e = gimple_call_arg (call, 5) == integer_minus_one_node;
		gimple_call_set_arg (call, 5, is_e ? e_mask_arg : mask_arg);
		if (!is_e)
		  gimple_call_set_arg (call, 4, chunk_arg);
	      }
	      break;

	    case IFN_GOACC_TILE:
	      gimple_call_set_arg (call, 3, mask_arg);
	      gimple_call_set_arg (call, 4, e_mask_arg);
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  update_stmt (call);
	}

      unsigned dim = GOMP_DIM_GANG;
      unsigned mask = loop->mask | loop->e_mask;
      for (ix = 0; ix != GOMP_DIM_MAX && mask; ix++)
	{
	  while (!(GOMP_DIM_MASK (dim) & mask))
	    dim++;

	  oacc_loop_xform_head_tail (loop->heads[ix], dim);
	  oacc_loop_xform_head_tail (loop->tails[ix], dim);

	  mask ^= GOMP_DIM_MASK (dim);
	}
    }

  if (loop->sibling)
    oacc_loop_process (loop->sibling, fn_level);


  /* OpenACC 2.6, 2.9.11. "reduction clause" places a restriction such that
     "The 'reduction' clause may not be specified on an orphaned 'loop'
     construct with the 'gang' clause, or on an orphaned 'loop' construct that
     will generate gang parallelism in a procedure that is compiled with the
     'routine gang' clause."  */
  if (fn_level == GOMP_DIM_GANG
      && (loop->mask & GOMP_DIM_MASK (GOMP_DIM_GANG))
      && (loop->flags & OLF_REDUCTION))
    error_at (loop->loc,
	      "gang reduction on an orphan loop");
}

/* Return the outermost CFG loop that is enclosed between the head and
   tail mark calls for LOOP, or NULL if there is no such CFG loop.

   The outermost CFG loop is a loop that is used for "chunking" the
   original loop from the user's code.  The lower_omp_for function
   in omp-low.c which creates the head and tail mark sequence and
   the expand_oacc_for function in omp-expand.c are relevant for
   understanding the structure that we expect to find here. But note
   that the passes implemented in those files do not operate on CFG
   loops and hence the correspondence to the CFG loop structure is
   not directly visible there and has to be inferred. */

static loop_p
oacc_loop_get_cfg_loop (oacc_loop *loop)
{
  loop_p enclosed_cfg_loop = NULL;
  for (unsigned dim = 0; dim < GOMP_DIM_MAX; ++dim)
    {
      gcall *tail_mark = loop->tails[dim];
      gimple *head_mark = loop->heads[dim];
      if (!tail_mark)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	dump_printf (MSG_OPTIMIZED_LOCATIONS | MSG_PRIORITY_INTERNALS, "%G",
		     (gimple *) tail_mark);

      loop_p mark_cfg_loop = tail_mark->bb->loop_father;
      loop_p current_cfg_loop = mark_cfg_loop;

      /* Ascend from TAIL_MARK until a different CFG loop is reached.

	 From the way that OpenACC loops are treated in omp-low.c, we
	 could expect the tail marker to be immediately preceded by a
	 loop exit. But loop optimizations (e.g. store-motion in
	 pass_lim) can change this. */
      basic_block bb = tail_mark->bb;
      bool empty_loop = false;
      while (current_cfg_loop == mark_cfg_loop)
	{
	  /* If the OpenACC loop becomes empty due to optimizations,
	     there is no CFG loop at all enclosed between head and
	     tail mark */
	  if (bb == head_mark->bb)
	    {
	      empty_loop = true;
	      break;
	    }

	  bb = get_immediate_dominator (CDI_DOMINATORS, bb);
	  current_cfg_loop = bb->loop_father;
	}

      if (empty_loop)
	continue;

      /* We expect to find the same CFG loop enclosed between all head
	 and tail mark pairs. Hence we actually need to look at only
	 the first available pair. But we consider all for
	 verification purposes. */
      if (enclosed_cfg_loop)
	{
	  gcc_assert (current_cfg_loop == enclosed_cfg_loop);
	  continue;
	}

      enclosed_cfg_loop = current_cfg_loop;

      gcc_checking_assert (dominated_by_p (
	  CDI_DOMINATORS, enclosed_cfg_loop->header, head_mark->bb));
    }

  return enclosed_cfg_loop;
}

static const char*
can_be_parallel_str (loop_p loop)
{
  if (!loop->can_be_parallel_valid_p)
    return "not analyzed";

  return loop->can_be_parallel ? "can be parallel" : "cannot be parallel";
}

/* Returns true if LOOP is known to be parallelizable and false
   otherwise.  The decision is based on the the dependence analysis
   that must have been previously performed by Graphite on the CFG
   loops contained in the OpenACC loop LOOP.  The value of ANALYZED is
   set to true if all relevant CFG loops have been analyzed. */

static bool
oacc_loop_can_be_parallel_p (oacc_loop *loop, bool& analyzed)
{
  /* Graphite will not run without enabled optimizations, so we cannot
     expect to find any parallelizability information on the CFG loops. */
  if (!optimize)
    return false;

  const dump_user_location_t loc
      = dump_user_location_t::from_location_t (loop->loc);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS | MSG_PRIORITY_INTERNALS, loc,
		     "Inspecting CFG-loops for OpenACC loop.\n");

  /* Search for the CFG loops that are enclosed between the head and
     tail mark calls for LOOP. The two outer CFG loops are considered
     to belong to the OpenACC loop and hence the CAN_BE_PARALLEL flags
     on those loops will be used to determine the return value. */
  bool can_be_parallel = false;
  loop_p enclosed_cfg_loop = oacc_loop_get_cfg_loop (loop);

  if (enclosed_cfg_loop
      /* The inner loop may have been removed in degenerate cases, e.g.
	 if an infinite "for (; ;)" gets optimized in an OpenACC loop nest. */
      && enclosed_cfg_loop->inner)
    {
      gcc_assert (enclosed_cfg_loop->inner != NULL);
      gcc_assert (enclosed_cfg_loop->inner->next == NULL);

      can_be_parallel = enclosed_cfg_loop->can_be_parallel
			&& enclosed_cfg_loop->inner->can_be_parallel;

      analyzed = enclosed_cfg_loop->can_be_parallel_valid_p
		 && enclosed_cfg_loop->inner->can_be_parallel_valid_p;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  dump_printf (MSG_OPTIMIZED_LOCATIONS | MSG_PRIORITY_INTERNALS,
		       "\tOuter loop <%d> preceeding tail mark %s.\n"
		       "\tInner loop <%d> %s.\n",
		       enclosed_cfg_loop->num,
		       can_be_parallel_str (enclosed_cfg_loop),
		       enclosed_cfg_loop->inner->num,
		       can_be_parallel_str (enclosed_cfg_loop->inner));
	}
    }
  else if (dump_file && (dump_flags & TDF_DETAILS))
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS | MSG_PRIORITY_INTERNALS, loc,
		     "Empty OpenACC loop.\n");

  return can_be_parallel;
}

static bool
oacc_parallel_kernels_graphite_fun_p ()
{
  return lookup_attribute ("oacc parallel_kernels_graphite",
			   DECL_ATTRIBUTES (cfun->decl));
}

static bool
oacc_parallel_fun_p ()
{
  return lookup_attribute ("oacc parallel",
			   DECL_ATTRIBUTES (cfun->decl));
}

/* If LOOP is an "auto" loop for which dependence analysis has determined that
   it can be parallelized, make it "independent" by adjusting its FLAGS field
   and return true. Otherwise, return false. */

static bool
oacc_loop_transform_auto_into_independent (oacc_loop *loop)
{
  if (!optimize)
    return false;

  /* This function is only relevant on "kernels"
     regions that have been explicitly designated
     to be analyzed by Graphite and on "auto"
     loops in "parallel" regions. */
  if (!oacc_parallel_kernels_graphite_fun_p () &&
      !oacc_parallel_fun_p ())
    return false;

  if (loop->routine)
    return false;

  if (!(loop->flags & OLF_AUTO))
    return false;

  bool analyzed = false;
  bool can_be_parallel = oacc_loop_can_be_parallel_p (loop, analyzed);
  dump_user_location_t loc = dump_user_location_t::from_location_t (loop->loc);

  if (dump_enabled_p ())
    {
      if (!analyzed)
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "'auto' loop has not been analyzed (cf. 'graphite' "
			 "dumps for more information).\n");
    }
  if (!can_be_parallel)
    return false;

  loop->flags |= OLF_INDEPENDENT;

  /* We need to keep the OLF_AUTO flag for now.
     oacc_loop_fixed_partitions and oacc_loop_auto_partitions
     interpret "independent auto" as "this loop can be parallel,
     please determine the dimensions" which seems to correspond to the
     meaning of those clauses in an old OpenACC version.  We rely on
     this behaviour to assign the dimensions for this loop.

     TODO Use a different flag to indicate that the dimensions must be assigned. */

  // loop->flags &= ~OLF_AUTO;

  return true;
}

/* Emit a warning if LOOP has an "independent" clause but Graphite's
   analysis shows that it has data dependences. Note that we respect
   the user's explicit decision to parallelize the loop but we
   nevertheless warn that this decision could be wrong. */

static void
oacc_loop_warn_if_false_independent (oacc_loop *loop)
{
  if (!optimize)
    return;

  if (loop->routine)
    return;

  /* TODO Warn about "auto" & "independent" in "parallel" regions? */
  if (!oacc_parallel_kernels_graphite_fun_p ())
    return;

  if (!(loop->flags & OLF_INDEPENDENT))
    return;

  bool analyzed = false;
  bool can_be_parallel = oacc_loop_can_be_parallel_p (loop, analyzed);
  loop_p cfg_loop = oacc_loop_get_cfg_loop (loop);

  if (cfg_loop && cfg_loop->inner && !analyzed)
    {
      if (dump_enabled_p ())
	{
	  const dump_user_location_t loc
	    = dump_user_location_t::from_location_t (loop->loc);
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			   "'independent' loop in 'kernels' region has not been "
			   "analyzed (cf. 'graphite' "
			   "dumps for more information).\n");
	}
      return;
    }

  if (!can_be_parallel)
    warning_at (loop->loc, 0,
                "loop has \"independent\" clause but data dependences were "
		"found");
}

/* Walk the OpenACC loop hierarchy checking and assigning the
   programmer-specified partitionings.  OUTER_MASK is the partitioning
   this loop is contained within.  Return mask of partitioning
   encountered.  If any auto loops are discovered, set GOMP_DIM_MAX
   bit.  */

static unsigned
oacc_loop_fixed_partitions (oacc_loop *loop, unsigned outer_mask)
{
  unsigned this_mask = loop->mask;
  unsigned mask_all = 0;
  bool noisy = true;

#ifdef ACCEL_COMPILER
  /* When device_type is supported, we want the device compiler to be
     noisy, if the loop parameters are device_type-specific.  */
  noisy = false;
#endif

  if (!loop->routine)
    {
      bool auto_par = (loop->flags & OLF_AUTO) != 0;
      bool seq_par = (loop->flags & OLF_SEQ) != 0;
      bool tiling = (loop->flags & OLF_TILE) != 0;
      
      this_mask = ((loop->flags >> OLF_DIM_BASE)
		   & (GOMP_DIM_MASK (GOMP_DIM_MAX) - 1));

      /* Apply auto partitioning if this is a non-partitioned regular
	 loop, or (no more than) single axis tiled loop.  */
      bool maybe_auto
	= !seq_par && this_mask == (tiling ? this_mask & -this_mask : 0);

      if ((this_mask != 0) + auto_par + seq_par > 1)
	{
	  if (noisy)
	    error_at (loop->loc,
		      seq_par
		      ? G_("%<seq%> overrides other OpenACC loop specifiers")
		      : G_("%<auto%> conflicts with other OpenACC loop "
			   "specifiers"));
	  maybe_auto = false;
	  loop->flags &= ~OLF_AUTO;
	  if (seq_par)
	    {
	      loop->flags
		&= ~((GOMP_DIM_MASK (GOMP_DIM_MAX) - 1) << OLF_DIM_BASE);
	      this_mask = 0;
	    }
	}

      /* TODO Is this flag needed? Perhaps use -Wopenacc-parallelism? */
      if (warn_openacc_false_independent)
        oacc_loop_warn_if_false_independent (loop);

      if (maybe_auto && (loop->flags & OLF_INDEPENDENT))
	{
	  loop->flags |= OLF_AUTO;
	  mask_all |= GOMP_DIM_MASK (GOMP_DIM_MAX);
	}

      if (oacc_loop_transform_auto_into_independent (loop))
	  mask_all |= GOMP_DIM_MASK (GOMP_DIM_MAX);
    }

  if (this_mask & outer_mask)
    {
      const oacc_loop *outer;
      for (outer = loop->parent; outer; outer = outer->parent)
	if ((outer->mask | outer->e_mask) & this_mask)
	  break;

      if (noisy)
	{
	  if (outer)
	    {
	      error_at (loop->loc,
			loop->routine
			? G_("routine call uses same OpenACC parallelism"
			     " as containing loop")
			: G_("inner loop uses same OpenACC parallelism"
			     " as containing loop"));
	      inform (outer->loc, "containing loop here");
	    }
	  else
	    error_at (loop->loc,
		      loop->routine
		      ? G_("routine call uses OpenACC parallelism disallowed"
			   " by containing routine")
		      : G_("loop uses OpenACC parallelism disallowed"
			   " by containing routine"));

	  if (loop->routine)
	    inform (DECL_SOURCE_LOCATION (loop->routine),
		    "routine %qD declared here", loop->routine);
	}
      this_mask &= ~outer_mask;
    }
  else
    {
      unsigned outermost = least_bit_hwi (this_mask);

      if (outermost && outermost <= outer_mask)
	{
	  if (noisy)
	    {
	      error_at (loop->loc,
			"incorrectly nested OpenACC loop parallelism");

	      const oacc_loop *outer;
	      for (outer = loop->parent;
		   outer->flags && outer->flags < outermost;
		   outer = outer->parent)
		continue;
	      inform (outer->loc, "containing loop here");
	    }

	  this_mask &= ~outermost;
	}
    }

  mask_all |= this_mask;

  if (loop->flags & OLF_TILE)
    {
      /* When tiling, vector goes to the element loop, and failing
	 that we put worker there.  The std doesn't contemplate
	 specifying all three.  We choose to put worker and vector on
	 the element loops in that case.  */
      unsigned this_e_mask = this_mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR);
      if (!this_e_mask || this_mask & GOMP_DIM_MASK (GOMP_DIM_GANG))
	this_e_mask |= this_mask & GOMP_DIM_MASK (GOMP_DIM_WORKER);

      loop->e_mask = this_e_mask;
      this_mask ^= this_e_mask;
    }

  loop->mask = this_mask;

  if (dump_file)
    fprintf (dump_file, "Loop %s:%d user specified %d & %d\n",
	     LOCATION_FILE (loop->loc), LOCATION_LINE (loop->loc),
	     loop->mask, loop->e_mask);

  if (loop->child)
    {
      unsigned tmp_mask = outer_mask | this_mask | loop->e_mask;
      loop->inner = oacc_loop_fixed_partitions (loop->child, tmp_mask);
      mask_all |= loop->inner;
    }

  if (loop->sibling)
    mask_all |= oacc_loop_fixed_partitions (loop->sibling, outer_mask);

  return mask_all;
}

/* Walk the OpenACC loop heirarchy to assign auto-partitioned loops.
   OUTER_MASK is the partitioning this loop is contained within.
   OUTER_ASSIGN is true if an outer loop is being auto-partitioned.
   Return the cumulative partitioning used by this loop, siblings and
   children.  */

static unsigned
oacc_loop_auto_partitions (oacc_loop *loop, unsigned outer_mask,
			   bool outer_assign)
{
  bool assign = (loop->flags & OLF_AUTO) && (loop->flags & OLF_INDEPENDENT);
  bool noisy = true;
  bool tiling = loop->flags & OLF_TILE;

#ifdef ACCEL_COMPILER
  /* When device_type is supported, we want the device compiler to be
     noisy, if the loop parameters are device_type-specific.  */
  noisy = false;
#endif

  if (assign && (!outer_assign || loop->inner))
    {
      /* Allocate outermost and non-innermost loops at the outermost
	 non-innermost available level.  */
      unsigned this_mask = GOMP_DIM_MASK (GOMP_DIM_GANG);

      /* Find the first outermost available partition. */
      while (this_mask <= outer_mask)
	this_mask <<= 1;
      
      /* Grab two axes if tiling, and we've not assigned anything  */
      if (tiling && !(loop->mask | loop->e_mask))
	this_mask |= this_mask << 1;

      /* Prohibit the innermost partitioning at the moment.  */
      this_mask &= GOMP_DIM_MASK (GOMP_DIM_MAX - 1) - 1;

      /* Don't use any dimension explicitly claimed by an inner loop. */
      this_mask &= ~loop->inner;

      if (tiling && !loop->e_mask)
	{
	  /* If we got two axes, allocate the inner one to the element
	     loop.  */
	  loop->e_mask = this_mask & (this_mask << 1);
	  this_mask ^= loop->e_mask;
	}

      loop->mask |= this_mask;
    }

  if (loop->child)
    {
      unsigned tmp_mask = outer_mask | loop->mask | loop->e_mask;
      loop->inner = oacc_loop_auto_partitions (loop->child, tmp_mask,
					       outer_assign | assign);
    }

  if (assign && (!loop->mask || (tiling && !loop->e_mask) || !outer_assign))
    {
      /* Allocate the loop at the innermost available level.  Note
	 that we do this even if we already assigned this loop the
	 outermost available level above.  That way we'll partition
	 this along 2 axes, if they are available.  */
      unsigned this_mask = 0;

      /* Determine the outermost partitioning used within this loop.  */
      this_mask = loop->inner | GOMP_DIM_MASK (GOMP_DIM_MAX);
      this_mask = least_bit_hwi (this_mask);

      /* Pick the partitioning just inside that one.  */
      this_mask >>= 1;

      /* And avoid picking one use by an outer loop.  */
      this_mask &= ~outer_mask;

      /* If tiling and we failed completely above, grab the next one
	 too.  Making sure it doesn't hit an outer loop.  */
      if (tiling)
	{
	  this_mask &= ~(loop->e_mask | loop->mask);
	  unsigned tile_mask = ((this_mask >> 1)
				& ~(outer_mask | loop->e_mask | loop->mask));

	  if (tile_mask || loop->mask)
	    {
	      loop->e_mask |= this_mask;
	      this_mask = tile_mask;
	    }
	  if (!loop->e_mask && noisy)
	    warning_at (loop->loc, 0,
			"insufficient partitioning available"
			" to parallelize element loop");
	}

      loop->mask |= this_mask;
      if (!loop->mask && noisy)
	warning_at (loop->loc, 0,
		    tiling
		    ? G_("insufficient partitioning available"
			 " to parallelize tile loop")
		    : G_("insufficient partitioning available"
			 " to parallelize loop"));
    }

  if (assign && dump_file)
    fprintf (dump_file, "Auto loop %s:%d assigned %d & %d\n",
	     LOCATION_FILE (loop->loc), LOCATION_LINE (loop->loc),
	     loop->mask, loop->e_mask);

  unsigned inner_mask = 0;

  if (loop->sibling)
    inner_mask |= oacc_loop_auto_partitions (loop->sibling,
					     outer_mask, outer_assign);

  inner_mask |= loop->inner | loop->mask | loop->e_mask;

  return inner_mask;
}

/* Walk the OpenACC loop heirarchy to check and assign partitioning
   axes.  Return mask of partitioning.  */

static unsigned
oacc_loop_partition (oacc_loop *loop, unsigned outer_mask)
{
  unsigned mask_all = oacc_loop_fixed_partitions (loop, outer_mask);

  if (mask_all & GOMP_DIM_MASK (GOMP_DIM_MAX))
    {
      mask_all ^= GOMP_DIM_MASK (GOMP_DIM_MAX);
      mask_all |= oacc_loop_auto_partitions (loop, outer_mask, false);
    }
  return mask_all;
}

/* Default fork/join early expander.  Delete the function calls if
   there is no RTL expander.  */

bool
default_goacc_fork_join (gcall *ARG_UNUSED (call),
			 const int *ARG_UNUSED (dims), bool is_fork)
{
  if (is_fork)
    return targetm.have_oacc_fork ();
  else
    return targetm.have_oacc_join ();
}

/* Default goacc.reduction early expander.

   LHS-opt = IFN_REDUCTION (KIND, RES_PTR, VAR, LEVEL, OP, OFFSET)
   If RES_PTR is not integer-zerop:
       SETUP - emit 'LHS = *RES_PTR', LHS = NULL
       TEARDOWN - emit '*RES_PTR = VAR'
   If LHS is not NULL
       emit 'LHS = VAR'   */

void
default_goacc_reduction (gcall *call)
{
  unsigned code = (unsigned)TREE_INT_CST_LOW (gimple_call_arg (call, 0));
  gimple_stmt_iterator gsi = gsi_for_stmt (call);
  tree lhs = gimple_call_lhs (call);
  tree var = gimple_call_arg (call, 2);
  gimple_seq seq = NULL;

  if (code == IFN_GOACC_REDUCTION_SETUP
      || code == IFN_GOACC_REDUCTION_TEARDOWN)
    {
      /* Setup and Teardown need to copy from/to the receiver object,
	 if there is one.  */
      tree ref_to_res = gimple_call_arg (call, 1);

      if (!integer_zerop (ref_to_res))
	{
	  /* Dummy reduction vars that have GOMP_MAP_FIRSTPRIVATE_POINTER data
	     mappings gets retyped to (void *).  Adjust the type of ref_to_res
	     as appropriate.  */
	  if (TREE_TYPE (TREE_TYPE (ref_to_res)) != TREE_TYPE (var))
	    {
	      tree ptype = build_pointer_type (TREE_TYPE (var));
	      tree t = make_ssa_name (ptype);
	      tree expr = fold_build1 (NOP_EXPR, ptype, ref_to_res);
	      gimple_seq_add_stmt (&seq, gimple_build_assign (t, expr));
	      ref_to_res = t;
	    }
	  tree dst = build_simple_mem_ref (ref_to_res);
	  tree src = var;

	  if (code == IFN_GOACC_REDUCTION_SETUP)
	    {
	      src = dst;
	      dst = lhs;
	      lhs = NULL;
	    }
	  gimple_seq_add_stmt (&seq, gimple_build_assign (dst, src));
	}
    }

  /* Copy VAR to LHS, if there is an LHS.  */
  if (lhs)
    gimple_seq_add_stmt (&seq, gimple_build_assign (lhs, var));

  gsi_replace_with_seq (&gsi, seq, true);
}

struct var_decl_rewrite_info
{
  gimple *stmt;
  hash_map<tree, tree> *adjusted_vars;
  bool avoid_pointer_conversion;
  bool modified;
};

/* Helper function for execute_oacc_device_lower.  Rewrite VAR_DECLs (by
   themselves or wrapped in various other nodes) according to ADJUSTED_VARS in
   the var_decl_rewrite_info pointed to via DATA.  Used as part of coercing
   gang-private variables in OpenACC offload regions to reside in GPU shared
   memory.  */

static tree
oacc_rewrite_var_decl (tree *tp, int *walk_subtrees, void *data)
{
  walk_stmt_info *wi = (walk_stmt_info *) data;
  var_decl_rewrite_info *info = (var_decl_rewrite_info *) wi->info;

  if (TREE_CODE (*tp) == ADDR_EXPR)
    {
      tree arg = TREE_OPERAND (*tp, 0);
      tree *new_arg = info->adjusted_vars->get (arg);

      if (new_arg)
	{
	  if (info->avoid_pointer_conversion)
	    {
	      *tp = build_fold_addr_expr (*new_arg);
	      info->modified = true;
	      *walk_subtrees = 0;
	    }
	  else
	    {
	      gimple_stmt_iterator gsi = gsi_for_stmt (info->stmt);
	      tree repl = build_fold_addr_expr (*new_arg);
	      gimple *stmt1
		= gimple_build_assign (make_ssa_name (TREE_TYPE (repl)), repl);
	      tree conv = convert_to_pointer (TREE_TYPE (*tp),
					      gimple_assign_lhs (stmt1));
	      gimple *stmt2
		= gimple_build_assign (make_ssa_name (TREE_TYPE (*tp)), conv);
	      gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
	      gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
	      *tp = gimple_assign_lhs (stmt2);
	      info->modified = true;
	      *walk_subtrees = 0;
	    }
	}
    }
  else if (TREE_CODE (*tp) == COMPONENT_REF || TREE_CODE (*tp) == ARRAY_REF)
    {
      tree *base = &TREE_OPERAND (*tp, 0);

      while (TREE_CODE (*base) == COMPONENT_REF
	     || TREE_CODE (*base) == ARRAY_REF)
	base = &TREE_OPERAND (*base, 0);

      if (TREE_CODE (*base) != VAR_DECL)
	return NULL;

      tree *new_decl = info->adjusted_vars->get (*base);
      if (!new_decl)
	return NULL;

      int base_quals = TYPE_QUALS (TREE_TYPE (*new_decl));
      tree field = TREE_OPERAND (*tp, 1);

      /* Adjust the type of the field.  */
      int field_quals = TYPE_QUALS (TREE_TYPE (field));
      if (TREE_CODE (field) == FIELD_DECL && field_quals != base_quals)
	{
	  tree *field_type = &TREE_TYPE (field);
	  while (TREE_CODE (*field_type) == ARRAY_TYPE)
	    field_type = &TREE_TYPE (*field_type);
	  field_quals |= base_quals;
	  *field_type = build_qualified_type (*field_type, field_quals);
	}

      /* Adjust the type of the component ref itself.  */
      tree comp_type = TREE_TYPE (*tp);
      int comp_quals = TYPE_QUALS (comp_type);
      if (TREE_CODE (*tp) == COMPONENT_REF && comp_quals != base_quals)
	{
	  comp_quals |= base_quals;
	  TREE_TYPE (*tp)
	    = build_qualified_type (comp_type, comp_quals);
	}

      *base = *new_decl;
      info->modified = true;
    }
  else if (TREE_CODE (*tp) == VAR_DECL)
    {
      tree *new_decl = info->adjusted_vars->get (*tp);
      if (new_decl)
	{
	  *tp = *new_decl;
	  info->modified = true;
	}
    }

  return NULL_TREE;
}

/* Return TRUE if CALL is a call to a builtin atomic/sync operation.  */

static bool
is_sync_builtin_call (gcall *call)
{
  tree callee = gimple_call_fndecl (call);

  if (callee != NULL_TREE
      && gimple_call_builtin_p (call, BUILT_IN_NORMAL))
    switch (DECL_FUNCTION_CODE (callee))
      {
#undef DEF_SYNC_BUILTIN
#define DEF_SYNC_BUILTIN(ENUM, NAME, TYPE, ATTRS) case ENUM:
#include "sync-builtins.def"
#undef DEF_SYNC_BUILTIN
	return true;

      default:
	;
      }

  return false;
}

/* Main entry point for oacc transformations which run on the device
   compiler after LTO, so we know what the target device is at this
   point (including the host fallback).  */

static unsigned int
execute_oacc_loop_designation ()
{
  tree attrs = oacc_get_fn_attrib (current_function_decl);

  if (!attrs)
    /* Not an offloaded function.  */
    return 0;

  /* Parse the default dim argument exactly once.  */
  if ((const void *)flag_openacc_dims != &flag_openacc_dims)
    {
      oacc_parse_default_dims (flag_openacc_dims);
      flag_openacc_dims = (char *)&flag_openacc_dims;
    }

  bool is_oacc_kernels
    = (lookup_attribute ("oacc kernels",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  bool is_oacc_parallel
    = (lookup_attribute ("oacc parallel",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  bool is_oacc_serial
    = (lookup_attribute ("oacc serial",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  bool is_oacc_kernels_parallelized
    = (lookup_attribute ("oacc kernels parallelized",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  if (is_oacc_kernels_parallelized)
    gcc_checking_assert (is_oacc_kernels);
  bool is_oacc_parallel_kernels_parallelized
    = (lookup_attribute ("oacc parallel_kernels_parallelized",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  if (is_oacc_parallel_kernels_parallelized)
    gcc_checking_assert (!is_oacc_kernels);
  bool is_oacc_parallel_kernels_gang_single
    = (lookup_attribute ("oacc parallel_kernels_gang_single",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  if (is_oacc_parallel_kernels_gang_single)
    gcc_checking_assert (!is_oacc_kernels);
  gcc_checking_assert (!(is_oacc_parallel_kernels_parallelized
			 && is_oacc_parallel_kernels_gang_single));
  bool is_oacc_parallel_kernels_graphite
    = (lookup_attribute ("oacc parallel_kernels_graphite",
			 DECL_ATTRIBUTES (current_function_decl)) != NULL);
  if (is_oacc_parallel_kernels_graphite)
      gcc_checking_assert (!is_oacc_kernels
			   && !is_oacc_parallel_kernels_gang_single);

  /* Unparallelized OpenACC kernels constructs must get launched as 1 x 1 x 1
     kernels, so remove the parallelism dimensions function attributes
     potentially set earlier on.  */
  if (is_oacc_kernels && !is_oacc_kernels_parallelized)
    {
      gcc_checking_assert (!is_oacc_parallel_kernels_graphite);
      oacc_set_fn_attrib (current_function_decl, NULL, NULL);
      attrs = oacc_get_fn_attrib (current_function_decl);
    }

  /* Discover, partition and process the loops.  */
  oacc_loop *loops = oacc_loop_discovery ();
  int fn_level = oacc_fn_attrib_level (attrs);
  bool is_oacc_routine = (fn_level >= 0);

  if (dump_file)
    {
      if (fn_level >= 0)
	fprintf (dump_file, "Function is OpenACC routine level %d\n",
		 fn_level);
      else if (is_oacc_kernels)
	fprintf (dump_file, "Function is %s OpenACC kernels offload\n",
		 (is_oacc_kernels_parallelized
		  ? "parallelized" : "unparallelized"));
      else if (is_oacc_parallel_kernels_parallelized)
	fprintf (dump_file, "Function is %s OpenACC kernels offload\n",
		 "parallel_kernels_parallelized");
      else if (is_oacc_parallel_kernels_gang_single)
	fprintf (dump_file, "Function is %s OpenACC kernels offload\n",
		 "parallel_kernels_gang_single");
      else if (is_oacc_parallel_kernels_graphite)
	fprintf (dump_file, "Function is %s OpenACC kernels offload\n",
		 "parallel_kernels_graphite");
      else if (is_oacc_serial)
	fprintf (dump_file, "Function is OpenACC serial offload\n");
      else if (is_oacc_parallel)
	fprintf (dump_file, "Function is OpenACC parallel offload\n");
      else
	gcc_unreachable ();
    }

  /* This doesn't belong into 'pass_oacc_loop_designation' conceptually, but
     it's a convenient place, so...  */
  if (is_oacc_routine)
    {
      tree attr = lookup_attribute ("omp declare target",
				    DECL_ATTRIBUTES (current_function_decl));
      gcc_checking_assert (attr);
      tree clauses = TREE_VALUE (attr);
      gcc_checking_assert (clauses);

      /* Should this OpenACC routine be discarded?  */
      bool discard = false;

      tree clause_nohost = omp_find_clause (clauses, OMP_CLAUSE_NOHOST);
      if (dump_file)
	fprintf (dump_file,
		 "OpenACC routine '%s' %s '%s' clause.\n",
		 lang_hooks.decl_printable_name (current_function_decl, 2),
		 clause_nohost ? "has" : "doesn't have",
		 omp_clause_code_name[OMP_CLAUSE_NOHOST]);
      /* Host compiler, 'nohost' clause?  */
#ifndef ACCEL_COMPILER
      if (clause_nohost)
	discard = true;
#endif

      if (dump_file)
	fprintf (dump_file,
		 "OpenACC routine '%s' %sdiscarded.\n",
		 lang_hooks.decl_printable_name (current_function_decl, 2),
		 discard ? "" : "not ");
      if (discard)
	{
	  TREE_ASM_WRITTEN (current_function_decl) = 1;
	  return TODO_discard_function;
	}
    }

  unsigned outer_mask = fn_level >= 0 ? GOMP_DIM_MASK (fn_level) - 1 : 0;
  unsigned used_mask = oacc_loop_partition (loops, outer_mask);
  /* OpenACC kernels constructs are special: they currently don't use the
     generic oacc_loop infrastructure and attribute/dimension processing.  */
  if (is_oacc_kernels && is_oacc_kernels_parallelized)
    {
      gcc_checking_assert (!is_oacc_parallel_kernels_graphite);

      /* Parallelized OpenACC kernels constructs use gang parallelism.  See
	 also tree-parloops.cc:create_parallel_loop.  */
      used_mask |= GOMP_DIM_MASK (GOMP_DIM_GANG);
    }

  int dims[GOMP_DIM_MAX];
  oacc_validate_dims (current_function_decl, attrs, dims, fn_level, used_mask);

  if (dump_file)
    {
      const char *comma = "Compute dimensions [";
      for (int ix = 0; ix != GOMP_DIM_MAX; ix++, comma = ", ")
	fprintf (dump_file, "%s%d", comma, dims[ix]);
      fprintf (dump_file, "]\n");
    }

  /* Verify that for OpenACC 'kernels' decomposed "gang-single" parts we launch
     a single gang only.  */
  if (is_oacc_parallel_kernels_gang_single)
    gcc_checking_assert (dims[GOMP_DIM_GANG] == 1);

  oacc_loop_process (loops, fn_level);
  if (dump_file)
    {
      fprintf (dump_file, "OpenACC loops\n");
      dump_oacc_loop (dump_file, loops, 0);
      fprintf (dump_file, "\n");
    }
  if (dump_enabled_p ())
    {
      oacc_loop *l = loops;
      /* OpenACC kernels constructs are special: they currently don't use the
	 generic oacc_loop infrastructure.  */
      if (is_oacc_kernels)
	{
	  /* Create a fake oacc_loop for diagnostic purposes.  */
	  l = new_oacc_loop_raw (NULL,
				 DECL_SOURCE_LOCATION (current_function_decl));
	  l->mask = used_mask;
	}
      else
	{
	  /* Skip the outermost, dummy OpenACC loop  */
	  l = l->child;
	}
      if (l)
	inform_oacc_loop (l);
      if (is_oacc_kernels)
	free_oacc_loop (l);
    }

  free_oacc_loop (loops);

  return 0;
}

static unsigned int
execute_oacc_device_lower ()
{
  tree attrs;
  int dims[GOMP_DIM_MAX];

  if (flag_openacc)
    {
      attrs = oacc_get_fn_attrib (current_function_decl);
      if (!attrs)
	/* Not an offloaded function.  */
	return 0;

      for (unsigned i = 0; i < GOMP_DIM_MAX; i++)
	dims[i] = oacc_get_fn_dim_size (current_function_decl, i);
    }

  hash_map<tree, tree> adjusted_vars;

  /* Now lower internal loop functions to target-specific code
     sequences.  */
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
      {
	gimple *stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt))
	  {
	    gsi_next (&gsi);
	    continue;
	  }

	gcall *call = as_a <gcall *> (stmt);
	if (!gimple_call_internal_p (call))
	  {
	    gsi_next (&gsi);
	    continue;
	  }

	/* Rewind to allow rescan.  */
	gsi_prev (&gsi);
	bool rescan = false, remove = false;
	enum  internal_fn ifn_code = gimple_call_internal_fn (call);

	switch (ifn_code)
	  {
	  default: break;

	  case IFN_GOACC_TILE:
	    oacc_xform_tile (call);
	    rescan = true;
	    break;
	    
	  case IFN_GOACC_LOOP:
	    oacc_xform_loop (call);
	    rescan = true;
	    break;

	  case IFN_GOACC_REDUCTION:
	    /* Mark the function for SSA renaming.  */
	    mark_virtual_operands_for_renaming (cfun);

	    /* If the level is -1, this ended up being an unused
	       axis.  Handle as a default.  */
	    if (integer_minus_onep (gimple_call_arg (call, 3)))
	      default_goacc_reduction (call);
	    else
	      targetm.goacc.reduction (call);
	    rescan = true;
	    break;

	  case IFN_UNIQUE:
	    {
	      enum ifn_unique_kind kind
		= ((enum ifn_unique_kind)
		   TREE_INT_CST_LOW (gimple_call_arg (call, 0)));

	      switch (kind)
		{
		default:
		  break;

		case IFN_UNIQUE_OACC_FORK:
		case IFN_UNIQUE_OACC_JOIN:
		  if (flag_openacc
		      && integer_minus_onep (gimple_call_arg (call, 2)))
		    remove = true;
		  else if (!targetm.goacc.fork_join
			   (call, dims, kind == IFN_UNIQUE_OACC_FORK))
		    remove = true;
		  break;

		case IFN_UNIQUE_OACC_HEAD_MARK:
		case IFN_UNIQUE_OACC_TAIL_MARK:
		  remove = true;
		  break;

		case IFN_UNIQUE_OACC_PRIVATE_SCALAR:
		case IFN_UNIQUE_OACC_FIRSTPRIVATE:
		  remove = true;
		  break;

		case IFN_UNIQUE_OACC_PRIVATE:
		  {
		    dump_flags_t l_dump_flags
		      = get_openacc_privatization_dump_flags ();

		    location_t loc = gimple_location (stmt);
		    if (LOCATION_LOCUS (loc) == UNKNOWN_LOCATION)
		      loc = DECL_SOURCE_LOCATION (current_function_decl);
		    const dump_user_location_t d_u_loc
		      = dump_user_location_t::from_location_t (loc);

		    HOST_WIDE_INT level
		      = TREE_INT_CST_LOW (gimple_call_arg (call, 2));
		    gcc_checking_assert (level == -1
					 || (level >= 0
					     && level < GOMP_DIM_MAX));
		    for (unsigned i = 3;
			 i < gimple_call_num_args (call);
			 i++)
		      {
			static char const *const axes[] =
			/* Must be kept in sync with GOMP_DIM enumeration.  */
			  { "gang", "worker", "vector" };

			tree arg = gimple_call_arg (call, i);
			gcc_checking_assert (TREE_CODE (arg) == ADDR_EXPR);
			tree decl = TREE_OPERAND (arg, 0);
			if (dump_enabled_p ())
/* PR100695 "Format decoder, quoting in 'dump_printf' etc." */
#if __GNUC__ >= 10
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wformat"
#endif
			  dump_printf_loc (l_dump_flags, d_u_loc,
					   "variable %<%T%> ought to be"
					   " adjusted for OpenACC"
					   " privatization level: %qs\n",
					   decl,
					   (level == -1
					    ? "UNKNOWN" : axes[level]));
#if __GNUC__ >= 10
# pragma GCC diagnostic pop
#endif
			bool adjusted;
			if (level == -1)
			  adjusted = false;
			else if (!targetm.goacc.adjust_private_decl)
			  adjusted = false;
			else if (level == GOMP_DIM_VECTOR)
			  {
			    /* That's the default behavior.  */
			    adjusted = true;
			  }
			else
			  {
			    tree oldtype = TREE_TYPE (decl);
			    tree newdecl
			      = targetm.goacc.adjust_private_decl (loc, decl,
								   level);
			    adjusted = (TREE_TYPE (newdecl) != oldtype
					|| newdecl != decl);
			    if (adjusted)
			      adjusted_vars.put (decl, newdecl);
			  }
			if (adjusted
			    && dump_enabled_p ())
/* PR100695 "Format decoder, quoting in 'dump_printf' etc." */
#if __GNUC__ >= 10
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wformat"
#endif
			  dump_printf_loc (l_dump_flags, d_u_loc,
					   "variable %<%T%> adjusted for"
					   " OpenACC privatization level:"
					   " %qs\n",
					   decl, axes[level]);
#if __GNUC__ >= 10
# pragma GCC diagnostic pop
#endif
		      }
		    remove = true;
		  }
		  break;
		}
	      break;
	    }
	  }

	if (gsi_end_p (gsi))
	  /* We rewound past the beginning of the BB.  */
	  gsi = gsi_start_bb (bb);
	else
	  /* Undo the rewind.  */
	  gsi_next (&gsi);

	if (remove)
	  {
	    if (gimple_vdef (call))
	      replace_uses_by (gimple_vdef (call), gimple_vuse (call));
	    if (gimple_call_lhs (call))
	      {
		/* Propagate the data dependency var.  */
		gimple *ass = gimple_build_assign (gimple_call_lhs (call),
						   gimple_call_arg (call, 1));
		gsi_replace (&gsi, ass,  false);
	      }
	    else
	      gsi_remove (&gsi, true);
	  }
	else if (!rescan)
	  /* If not rescanning, advance over the call.  */
	  gsi_next (&gsi);
      }

  /* Regarding the OpenACC privatization level, we're currently only looking at
     making the gang-private level work.  Regarding that, we have the following
     configurations:

       - GCN offloading: 'targetm.goacc.adjust_private_decl' does the work (in
	 particular, change 'TREE_TYPE', etc.) and there is no
	 'targetm.goacc.expand_var_decl'.

       - nvptx offloading: 'targetm.goacc.adjust_private_decl' only sets a
	 marker and then 'targetm.goacc.expand_var_decl' does the work.

     Eventually (in particular, for worker-private level?), both
     'targetm.goacc.adjust_private_decl' and 'targetm.goacc.expand_var_decl'
     may need to do things, but that's currently not meant to be addressed, and
     thus not fully worked out and implemented, and thus untested.  Hence,
     'assert' what currently is implemented/tested, only.  */

  if (targetm.goacc.expand_var_decl)
    gcc_assert (adjusted_vars.is_empty ());

  /* Make adjustments to gang-private local variables if required by the
     target, e.g. forcing them into a particular address space.  Afterwards,
     ADDR_EXPR nodes which have adjusted variables as their argument need to
     be modified in one of two ways:

       1. They can be recreated, making a pointer to the variable in the new
	  address space, or

       2. The address of the variable in the new address space can be taken,
	  converted to the default (original) address space, and the result of
	  that conversion subsituted in place of the original ADDR_EXPR node.

     Which of these is done depends on the gimple statement being processed.
     At present atomic operations and inline asms use (1), and everything else
     uses (2).  At least on AMD GCN, there are atomic operations that work
     directly in the LDS address space.

     COMPONENT_REFS, ARRAY_REFS and plain VAR_DECLs are also rewritten to use
     the new decl, adjusting types of appropriate tree nodes as necessary.  */

  if (targetm.goacc.adjust_private_decl
      && !adjusted_vars.is_empty ())
    {
      FOR_ALL_BB_FN (bb, cfun)
	for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	     !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  {
	    gimple *stmt = gsi_stmt (gsi);
	    walk_stmt_info wi;
	    var_decl_rewrite_info info;

	    info.avoid_pointer_conversion
	      = (is_gimple_call (stmt)
		 && is_sync_builtin_call (as_a <gcall *> (stmt)))
		|| gimple_code (stmt) == GIMPLE_ASM;
	    info.stmt = stmt;
	    info.modified = false;
	    info.adjusted_vars = &adjusted_vars;

	    memset (&wi, 0, sizeof (wi));
	    wi.info = &info;

	    walk_gimple_op (stmt, oacc_rewrite_var_decl, &wi);

	    if (info.modified)
	      update_stmt (stmt);
	  }
    }

  return 0;
}

/* Default launch dimension validator.  Force everything to 1.  A
   backend that wants to provide larger dimensions must override this
   hook.  */

bool
default_goacc_validate_dims (tree ARG_UNUSED (decl), int *dims,
			     int ARG_UNUSED (fn_level),
			     unsigned ARG_UNUSED (used))
{
  bool changed = false;

  for (unsigned ix = 0; ix != GOMP_DIM_MAX; ix++)
    {
      if (dims[ix] != 1)
	{
	  dims[ix] = 1;
	  changed = true;
	}
    }

  return changed;
}

/* Default dimension bound is unknown on accelerator and 1 on host.  */

int
default_goacc_dim_limit (int ARG_UNUSED (axis))
{
#ifdef ACCEL_COMPILER
  return 0;
#else
  return 1;
#endif
}

namespace {

const pass_data pass_data_oacc_loop_designation =
{
  GIMPLE_PASS, /* type */
  "oaccloops", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0 /* Possibly PROP_gimple_eomp.  */, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_oacc_loop_designation : public gimple_opt_pass
{
public:
  pass_oacc_loop_designation (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_oacc_loop_designation, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_openacc; };

  virtual unsigned int execute (function *)
    {
      return execute_oacc_loop_designation ();
    }

  opt_pass * clone () { return new pass_oacc_loop_designation (m_ctxt); }

}; // class pass_oacc_loop_designation

const pass_data pass_data_oacc_device_lower =
{
  GIMPLE_PASS, /* type */
  "oaccdevlow", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0 /* Possibly PROP_gimple_eomp.  */, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_oacc_device_lower : public gimple_opt_pass
{
public:
  pass_oacc_device_lower (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_oacc_device_lower, ctxt)
  {}

  /* opt_pass methods: */
  /* TODO If this were gated on something like '!(fun->curr_properties &
     PROP_gimple_oaccdevlow)', then we could easily have several instances
     in the pass pipeline? */
  virtual bool gate (function *)
  { return flag_openacc || (flag_openmp && flag_openmp_target == OMP_TARGET_MODE_OMPACC); };

  virtual unsigned int execute (function *)
    {
      return execute_oacc_device_lower ();
    }
  opt_pass * clone () { return new pass_oacc_device_lower (m_ctxt); }

}; // class pass_oacc_device_lower

} // anon namespace

gimple_opt_pass *
make_pass_oacc_loop_designation (gcc::context *ctxt)
{
  return new pass_oacc_loop_designation (ctxt);
}

gimple_opt_pass *
make_pass_oacc_device_lower (gcc::context *ctxt)
{
  return new pass_oacc_device_lower (ctxt);
}


/* Rewrite GOMP_SIMT_ENTER_ALLOC call given by GSI and remove the preceding
   GOMP_SIMT_ENTER call identifying the privatized variables, which are
   turned to structure fields and receive a DECL_VALUE_EXPR accordingly.
   Set *REGIMPLIFY to true, except if no privatized variables were seen.  */

static void
ompdevlow_adjust_simt_enter (gimple_stmt_iterator *gsi, bool *regimplify)
{
  gimple *alloc_stmt = gsi_stmt (*gsi);
  tree simtrec = gimple_call_lhs (alloc_stmt);
  tree simduid = gimple_call_arg (alloc_stmt, 0);
  gimple *enter_stmt = SSA_NAME_DEF_STMT (simduid);
  gcc_assert (gimple_call_internal_p (enter_stmt, IFN_GOMP_SIMT_ENTER));
  tree rectype = lang_hooks.types.make_type (RECORD_TYPE);
  TYPE_ARTIFICIAL (rectype) = TYPE_NAMELESS (rectype) = 1;
  TREE_ADDRESSABLE (rectype) = 1;
  TREE_TYPE (simtrec) = build_pointer_type (rectype);
  for (unsigned i = 1; i < gimple_call_num_args (enter_stmt); i++)
    {
      tree *argp = gimple_call_arg_ptr (enter_stmt, i);
      if (*argp == null_pointer_node)
	continue;
      gcc_assert (TREE_CODE (*argp) == ADDR_EXPR
		  && VAR_P (TREE_OPERAND (*argp, 0)));
      tree var = TREE_OPERAND (*argp, 0);

      tree field = build_decl (DECL_SOURCE_LOCATION (var), FIELD_DECL,
			       DECL_NAME (var), TREE_TYPE (var));
      SET_DECL_ALIGN (field, DECL_ALIGN (var));
      DECL_USER_ALIGN (field) = DECL_USER_ALIGN (var);
      TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (var);

      insert_field_into_struct (rectype, field);

      tree t = build_simple_mem_ref (simtrec);
      t = build3 (COMPONENT_REF, TREE_TYPE (var), t, field, NULL);
      TREE_THIS_VOLATILE (t) = TREE_THIS_VOLATILE (var);
      SET_DECL_VALUE_EXPR (var, t);
      DECL_HAS_VALUE_EXPR_P (var) = 1;
      *regimplify = true;
    }
  layout_type (rectype);
  tree size = TYPE_SIZE_UNIT (rectype);
  tree align = build_int_cst (TREE_TYPE (size), TYPE_ALIGN_UNIT (rectype));

  alloc_stmt
    = gimple_build_call_internal (IFN_GOMP_SIMT_ENTER_ALLOC, 2, size, align);
  gimple_call_set_lhs (alloc_stmt, simtrec);
  gsi_replace (gsi, alloc_stmt, false);
  gimple_stmt_iterator enter_gsi = gsi_for_stmt (enter_stmt);
  enter_stmt = gimple_build_assign (simduid, gimple_call_arg (enter_stmt, 0));
  gsi_replace (&enter_gsi, enter_stmt, false);

  use_operand_p use;
  gimple *exit_stmt;
  if (single_imm_use (simtrec, &use, &exit_stmt))
    {
      gcc_assert (gimple_call_internal_p (exit_stmt, IFN_GOMP_SIMT_EXIT));
      gimple_stmt_iterator exit_gsi = gsi_for_stmt (exit_stmt);
      tree clobber = build_clobber (rectype);
      exit_stmt = gimple_build_assign (build_simple_mem_ref (simtrec), clobber);
      gsi_insert_before (&exit_gsi, exit_stmt, GSI_SAME_STMT);
    }
  else
    gcc_checking_assert (has_zero_uses (simtrec));
}

/* Callback for walk_gimple_stmt used to scan for SIMT-privatized variables.  */

static tree
find_simtpriv_var_op (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;

  if (VAR_P (t)
      && DECL_HAS_VALUE_EXPR_P (t)
      && lookup_attribute ("omp simt private", DECL_ATTRIBUTES (t)))
    {
      *walk_subtrees = 0;
      return t;
    }
  return NULL_TREE;
}

/* Cleanup uses of SIMT placeholder internal functions: on non-SIMT targets,
   VF is 1 and LANE is 0; on SIMT targets, VF is folded to a constant, and
   LANE is kept to be expanded to RTL later on.  Also cleanup all other SIMT
   internal functions on non-SIMT targets, and likewise some SIMD internal
   functions on SIMT targets.  */

static unsigned int
execute_omp_device_lower ()
{
  int vf = targetm.simt.vf ? targetm.simt.vf () : 1;
  bool regimplify = false;
  basic_block bb;
  gimple_stmt_iterator gsi;
  bool calls_declare_variant_alt
    = cgraph_node::get (cfun->decl)->calls_declare_variant_alt;
  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt))
	  continue;
	if (!gimple_call_internal_p (stmt))
	  {
	    if (calls_declare_variant_alt)
	      if (tree fndecl = gimple_call_fndecl (stmt))
		{
		  tree new_fndecl = omp_resolve_declare_variant (fndecl);
		  if (new_fndecl != fndecl)
		    {
		      gimple_call_set_fndecl (stmt, new_fndecl);
		      update_stmt (stmt);
		    }
		}
	    continue;
	  }
	tree lhs = gimple_call_lhs (stmt), rhs = NULL_TREE;
	tree type = lhs ? TREE_TYPE (lhs) : integer_type_node;
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_GOMP_TARGET_REV:
	    {
#ifndef ACCEL_COMPILER
	      gimple_stmt_iterator gsi2 = gsi;
	      gsi_next (&gsi2);
	      gcc_assert (!gsi_end_p (gsi2));
	      gcc_assert (gimple_call_builtin_p (gsi_stmt (gsi2),
						 BUILT_IN_GOMP_TARGET));
	      tree old_decl
		= TREE_OPERAND (gimple_call_arg (gsi_stmt (gsi2), 1), 0);
	      tree new_decl = gimple_call_arg (gsi_stmt (gsi), 0);
	      gimple_call_set_arg (gsi_stmt (gsi2), 1, new_decl);
	      update_stmt (gsi_stmt (gsi2));
	      new_decl = TREE_OPERAND (new_decl, 0);
	      unsigned i;
	      unsigned num_funcs = vec_safe_length (offload_funcs);
	      for (i = 0; i < num_funcs; i++)
		{
		  if ((*offload_funcs)[i] == old_decl)
		    {
		      (*offload_funcs)[i] = new_decl;
		      break;
		    }
		  else if ((*offload_funcs)[i] == new_decl)
		    break;  /* This can happen due to inlining.  */
		}
	      gcc_assert (i < num_funcs);
#else
	      tree old_decl = TREE_OPERAND (gimple_call_arg (gsi_stmt (gsi), 0),
					    0);
#endif
	      /* FIXME: Find a way to actually prevent outputting the empty-body
		 old_decl as debug symbol + function in the assembly file.  */
	      cgraph_node *node = cgraph_node::get (old_decl);
	      node->address_taken = false;
	      node->need_lto_streaming = false;
	      node->offloadable = false;

	      unlink_stmt_vdef (stmt);
	    }
	    break;
	  case IFN_GOMP_USE_SIMT:
	    rhs = vf == 1 ? integer_zero_node : integer_one_node;
	    break;
	  case IFN_GOMP_SIMT_ENTER:
	    rhs = vf == 1 ? gimple_call_arg (stmt, 0) : NULL_TREE;
	    goto simtreg_enter_exit;
	  case IFN_GOMP_SIMT_ENTER_ALLOC:
	    if (vf != 1)
	      ompdevlow_adjust_simt_enter (&gsi, &regimplify);
	    rhs = vf == 1 ? null_pointer_node : NULL_TREE;
	    goto simtreg_enter_exit;
	  case IFN_GOMP_SIMT_EXIT:
	  simtreg_enter_exit:
	    if (vf != 1)
	      continue;
	    unlink_stmt_vdef (stmt);
	    break;
	  case IFN_GOMP_SIMT_LANE:
	  case IFN_GOMP_SIMT_LAST_LANE:
	    rhs = vf == 1 ? build_zero_cst (type) : NULL_TREE;
	    break;
	  case IFN_GOMP_SIMT_VF:
	    rhs = build_int_cst (type, vf);
	    break;
	  case IFN_GOMP_SIMT_ORDERED_PRED:
	    rhs = vf == 1 ? integer_zero_node : NULL_TREE;
	    if (rhs || !lhs)
	      unlink_stmt_vdef (stmt);
	    break;
	  case IFN_GOMP_SIMT_VOTE_ANY:
	  case IFN_GOMP_SIMT_XCHG_BFLY:
	  case IFN_GOMP_SIMT_XCHG_IDX:
	    rhs = vf == 1 ? gimple_call_arg (stmt, 0) : NULL_TREE;
	    break;
	  case IFN_GOMP_SIMD_LANE:
	  case IFN_GOMP_SIMD_LAST_LANE:
	    rhs = vf != 1 ? build_zero_cst (type) : NULL_TREE;
	    break;
	  case IFN_GOMP_SIMD_VF:
	    rhs = vf != 1 ? build_one_cst (type) : NULL_TREE;
	    break;
	  default:
	    continue;
	  }
	if (lhs && !rhs)
	  continue;
	stmt = lhs ? gimple_build_assign (lhs, rhs) : gimple_build_nop ();
	gsi_replace (&gsi, stmt, false);
      }
  if (regimplify)
    FOR_EACH_BB_REVERSE_FN (bb, cfun)
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	if (walk_gimple_stmt (&gsi, NULL, find_simtpriv_var_op, NULL))
	  {
	    if (gimple_clobber_p (gsi_stmt (gsi)))
	      gsi_remove (&gsi, true);
	    else
	      gimple_regimplify_operands (gsi_stmt (gsi), &gsi);
	  }
  if (vf != 1)
    cfun->has_force_vectorize_loops = false;
  return 0;
}

namespace {

const pass_data pass_data_omp_device_lower =
{
  GIMPLE_PASS, /* type */
  "ompdevlow", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_gimple_lomp_dev, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_omp_device_lower : public gimple_opt_pass
{
public:
  pass_omp_device_lower (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_omp_device_lower, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fun)
    {
      return (!(fun->curr_properties & PROP_gimple_lomp_dev)
	      || (flag_openmp
		  && cgraph_node::get (fun->decl)->calls_declare_variant_alt));
    }
  virtual unsigned int execute (function *)
    {
      return execute_omp_device_lower ();
    }

}; // class pass_expand_omp_ssa

} // anon namespace

gimple_opt_pass *
make_pass_omp_device_lower (gcc::context *ctxt)
{
  return new pass_omp_device_lower (ctxt);
}

/* "omp declare target link" handling pass.  */

namespace {

const pass_data pass_data_omp_target_link =
{
  GIMPLE_PASS,			/* type */
  "omptargetlink",		/* name */
  OPTGROUP_OMP,			/* optinfo_flags */
  TV_NONE,			/* tv_id */
  PROP_ssa,			/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_update_ssa,		/* todo_flags_finish */
};

class pass_omp_target_link : public gimple_opt_pass
{
public:
  pass_omp_target_link (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_omp_target_link, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fun)
    {
#ifdef ACCEL_COMPILER
      return offloading_function_p (fun->decl);
#else
      (void) fun;
      return false;
#endif
    }

  virtual unsigned execute (function *);
};

/* Callback for walk_gimple_stmt used to scan for link var operands.  */

static tree
find_link_var_op (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;

  if (VAR_P (t)
      && DECL_HAS_VALUE_EXPR_P (t)
      && is_global_var (t)
      && lookup_attribute ("omp declare target link", DECL_ATTRIBUTES (t)))
    {
      *walk_subtrees = 0;
      return t;
    }

  return NULL_TREE;
}

unsigned
pass_omp_target_link::execute (function *fun)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  if (gimple_call_builtin_p (gsi_stmt (gsi), BUILT_IN_GOMP_TARGET))
	    {
	      tree dev = gimple_call_arg (gsi_stmt (gsi), 0);
	      tree fn = gimple_call_arg (gsi_stmt (gsi), 1);
	      if (POINTER_TYPE_P (TREE_TYPE (fn)))
		fn = TREE_OPERAND (fn, 0);
	      if (TREE_CODE (dev) == INTEGER_CST
		  && wi::to_wide (dev) == GOMP_DEVICE_HOST_FALLBACK
		  && lookup_attribute ("omp target device_ancestor_nohost",
				       DECL_ATTRIBUTES (fn)) != NULL_TREE)
		continue;  /* ancestor:1  */
	      /* Nullify the second argument of __builtin_GOMP_target_ext.  */
	      gimple_call_set_arg (gsi_stmt (gsi), 1, null_pointer_node);
	      update_stmt (gsi_stmt (gsi));
	    }
	  if (walk_gimple_stmt (&gsi, NULL, find_link_var_op, NULL))
	    gimple_regimplify_operands (gsi_stmt (gsi), &gsi);
	}
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_omp_target_link (gcc::context *ctxt)
{
  return new pass_omp_target_link (ctxt);
}
