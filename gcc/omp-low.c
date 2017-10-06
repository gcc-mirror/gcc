/* Lowering pass for OMP directives.  Converts OMP directives into explicit
   calls to the runtime library (libgomp), data marshalling to implement data
   sharing and copying clauses, offloading to accelerators, and more.

   Contributed by Diego Novillo <dnovillo@redhat.com>

   Copyright (C) 2005-2017 Free Software Foundation, Inc.

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
#include "stor-layout.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "splay-tree.h"
#include "omp-general.h"
#include "omp-low.h"
#include "omp-grid.h"
#include "gimple-low.h"
#include "symbol-summary.h"
#include "tree-nested.h"
#include "context.h"
#include "gomp-constants.h"
#include "gimple-pretty-print.h"
#include "hsa-common.h"
#include "stringpool.h"
#include "attribs.h"

/* Lowering of OMP parallel and workshare constructs proceeds in two
   phases.  The first phase scans the function looking for OMP statements
   and then for variables that must be replaced to satisfy data sharing
   clauses.  The second phase expands code for the constructs, as well as
   re-gimplifying things when variables have been replaced with complex
   expressions.

   Final code generation is done by pass_expand_omp.  The flowgraph is
   scanned for regions which are then moved to a new
   function, to be invoked by the thread library, or offloaded.  */

/* Context structure.  Used to store information about each parallel
   directive in the code.  */

struct omp_context
{
  /* This field must be at the beginning, as we do "inheritance": Some
     callback functions for tree-inline.c (e.g., omp_copy_decl)
     receive a copy_body_data pointer that is up-casted to an
     omp_context pointer.  */
  copy_body_data cb;

  /* The tree of contexts corresponding to the encountered constructs.  */
  struct omp_context *outer;
  gimple *stmt;

  /* Map variables to fields in a structure that allows communication
     between sending and receiving threads.  */
  splay_tree field_map;
  tree record_type;
  tree sender_decl;
  tree receiver_decl;

  /* These are used just by task contexts, if task firstprivate fn is
     needed.  srecord_type is used to communicate from the thread
     that encountered the task construct to task firstprivate fn,
     record_type is allocated by GOMP_task, initialized by task firstprivate
     fn and passed to the task body fn.  */
  splay_tree sfield_map;
  tree srecord_type;

  /* A chain of variables to add to the top-level block surrounding the
     construct.  In the case of a parallel, this is in the child function.  */
  tree block_vars;

  /* Label to which GOMP_cancel{,llation_point} and explicit and implicit
     barriers should jump to during omplower pass.  */
  tree cancel_label;

  /* The sibling GIMPLE_OMP_FOR simd with _simt_ clause or NULL
     otherwise.  */
  gimple *simt_stmt;

  /* Nesting depth of this context.  Used to beautify error messages re
     invalid gotos.  The outermost ctx is depth 1, with depth 0 being
     reserved for the main body of the function.  */
  int depth;

  /* True if this parallel directive is nested within another.  */
  bool is_nested;

  /* True if this construct can be cancelled.  */
  bool cancellable;
};

static splay_tree all_contexts;
static int taskreg_nesting_level;
static int target_nesting_level;
static bitmap task_shared_vars;
static vec<omp_context *> taskreg_contexts;

static void scan_omp (gimple_seq *, omp_context *);
static tree scan_omp_1_op (tree *, int *, void *);

#define WALK_SUBSTMTS  \
    case GIMPLE_BIND: \
    case GIMPLE_TRY: \
    case GIMPLE_CATCH: \
    case GIMPLE_EH_FILTER: \
    case GIMPLE_TRANSACTION: \
      /* The sub-statements for these should be walked.  */ \
      *handled_ops_p = false; \
      break;

/* Return true if CTX corresponds to an oacc parallel region.  */

static bool
is_oacc_parallel (omp_context *ctx)
{
  enum gimple_code outer_type = gimple_code (ctx->stmt);
  return ((outer_type == GIMPLE_OMP_TARGET)
	  && (gimple_omp_target_kind (ctx->stmt)
	      == GF_OMP_TARGET_KIND_OACC_PARALLEL));
}

/* Return true if CTX corresponds to an oacc kernels region.  */

static bool
is_oacc_kernels (omp_context *ctx)
{
  enum gimple_code outer_type = gimple_code (ctx->stmt);
  return ((outer_type == GIMPLE_OMP_TARGET)
	  && (gimple_omp_target_kind (ctx->stmt)
	      == GF_OMP_TARGET_KIND_OACC_KERNELS));
}

/* If DECL is the artificial dummy VAR_DECL created for non-static
   data member privatization, return the underlying "this" parameter,
   otherwise return NULL.  */

tree
omp_member_access_dummy_var (tree decl)
{
  if (!VAR_P (decl)
      || !DECL_ARTIFICIAL (decl)
      || !DECL_IGNORED_P (decl)
      || !DECL_HAS_VALUE_EXPR_P (decl)
      || !lang_hooks.decls.omp_disregard_value_expr (decl, false))
    return NULL_TREE;

  tree v = DECL_VALUE_EXPR (decl);
  if (TREE_CODE (v) != COMPONENT_REF)
    return NULL_TREE;

  while (1)
    switch (TREE_CODE (v))
      {
      case COMPONENT_REF:
      case MEM_REF:
      case INDIRECT_REF:
      CASE_CONVERT:
      case POINTER_PLUS_EXPR:
	v = TREE_OPERAND (v, 0);
	continue;
      case PARM_DECL:
	if (DECL_CONTEXT (v) == current_function_decl
	    && DECL_ARTIFICIAL (v)
	    && TREE_CODE (TREE_TYPE (v)) == POINTER_TYPE)
	  return v;
	return NULL_TREE;
      default:
	return NULL_TREE;
      }
}

/* Helper for unshare_and_remap, called through walk_tree.  */

static tree
unshare_and_remap_1 (tree *tp, int *walk_subtrees, void *data)
{
  tree *pair = (tree *) data;
  if (*tp == pair[0])
    {
      *tp = unshare_expr (pair[1]);
      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Return unshare_expr (X) with all occurrences of FROM
   replaced with TO.  */

static tree
unshare_and_remap (tree x, tree from, tree to)
{
  tree pair[2] = { from, to };
  x = unshare_expr (x);
  walk_tree (&x, unshare_and_remap_1, pair, NULL);
  return x;
}

/* Convenience function for calling scan_omp_1_op on tree operands.  */

static inline tree
scan_omp_op (tree *tp, omp_context *ctx)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.info = ctx;
  wi.want_locations = true;

  return walk_tree (tp, scan_omp_1_op, &wi, NULL);
}

static void lower_omp (gimple_seq *, omp_context *);
static tree lookup_decl_in_outer_ctx (tree, omp_context *);
static tree maybe_lookup_decl_in_outer_ctx (tree, omp_context *);

/* Return true if CTX is for an omp parallel.  */

static inline bool
is_parallel_ctx (omp_context *ctx)
{
  return gimple_code (ctx->stmt) == GIMPLE_OMP_PARALLEL;
}


/* Return true if CTX is for an omp task.  */

static inline bool
is_task_ctx (omp_context *ctx)
{
  return gimple_code (ctx->stmt) == GIMPLE_OMP_TASK;
}


/* Return true if CTX is for an omp taskloop.  */

static inline bool
is_taskloop_ctx (omp_context *ctx)
{
  return gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	 && gimple_omp_for_kind (ctx->stmt) == GF_OMP_FOR_KIND_TASKLOOP;
}


/* Return true if CTX is for an omp parallel or omp task.  */

static inline bool
is_taskreg_ctx (omp_context *ctx)
{
  return is_parallel_ctx (ctx) || is_task_ctx (ctx);
}

/* Return true if EXPR is variable sized.  */

static inline bool
is_variable_sized (const_tree expr)
{
  return !TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (expr)));
}

/* Lookup variables.  The "maybe" form
   allows for the variable form to not have been entered, otherwise we
   assert that the variable must have been entered.  */

static inline tree
lookup_decl (tree var, omp_context *ctx)
{
  tree *n = ctx->cb.decl_map->get (var);
  return *n;
}

static inline tree
maybe_lookup_decl (const_tree var, omp_context *ctx)
{
  tree *n = ctx->cb.decl_map->get (const_cast<tree> (var));
  return n ? *n : NULL_TREE;
}

static inline tree
lookup_field (tree var, omp_context *ctx)
{
  splay_tree_node n;
  n = splay_tree_lookup (ctx->field_map, (splay_tree_key) var);
  return (tree) n->value;
}

static inline tree
lookup_sfield (splay_tree_key key, omp_context *ctx)
{
  splay_tree_node n;
  n = splay_tree_lookup (ctx->sfield_map
			 ? ctx->sfield_map : ctx->field_map, key);
  return (tree) n->value;
}

static inline tree
lookup_sfield (tree var, omp_context *ctx)
{
  return lookup_sfield ((splay_tree_key) var, ctx);
}

static inline tree
maybe_lookup_field (splay_tree_key key, omp_context *ctx)
{
  splay_tree_node n;
  n = splay_tree_lookup (ctx->field_map, key);
  return n ? (tree) n->value : NULL_TREE;
}

static inline tree
maybe_lookup_field (tree var, omp_context *ctx)
{
  return maybe_lookup_field ((splay_tree_key) var, ctx);
}

/* Return true if DECL should be copied by pointer.  SHARED_CTX is
   the parallel context if DECL is to be shared.  */

static bool
use_pointer_for_field (tree decl, omp_context *shared_ctx)
{
  if (AGGREGATE_TYPE_P (TREE_TYPE (decl))
      || TYPE_ATOMIC (TREE_TYPE (decl)))
    return true;

  /* We can only use copy-in/copy-out semantics for shared variables
     when we know the value is not accessible from an outer scope.  */
  if (shared_ctx)
    {
      gcc_assert (!is_gimple_omp_oacc (shared_ctx->stmt));

      /* ??? Trivially accessible from anywhere.  But why would we even
	 be passing an address in this case?  Should we simply assert
	 this to be false, or should we have a cleanup pass that removes
	 these from the list of mappings?  */
      if (TREE_STATIC (decl) || DECL_EXTERNAL (decl))
	return true;

      /* For variables with DECL_HAS_VALUE_EXPR_P set, we cannot tell
	 without analyzing the expression whether or not its location
	 is accessible to anyone else.  In the case of nested parallel
	 regions it certainly may be.  */
      if (TREE_CODE (decl) != RESULT_DECL && DECL_HAS_VALUE_EXPR_P (decl))
	return true;

      /* Do not use copy-in/copy-out for variables that have their
	 address taken.  */
      if (TREE_ADDRESSABLE (decl))
	return true;

      /* lower_send_shared_vars only uses copy-in, but not copy-out
	 for these.  */
      if (TREE_READONLY (decl)
	  || ((TREE_CODE (decl) == RESULT_DECL
	       || TREE_CODE (decl) == PARM_DECL)
	      && DECL_BY_REFERENCE (decl)))
	return false;

      /* Disallow copy-in/out in nested parallel if
	 decl is shared in outer parallel, otherwise
	 each thread could store the shared variable
	 in its own copy-in location, making the
	 variable no longer really shared.  */
      if (shared_ctx->is_nested)
	{
	  omp_context *up;

	  for (up = shared_ctx->outer; up; up = up->outer)
	    if (is_taskreg_ctx (up) && maybe_lookup_decl (decl, up))
	      break;

	  if (up)
	    {
	      tree c;

	      for (c = gimple_omp_taskreg_clauses (up->stmt);
		   c; c = OMP_CLAUSE_CHAIN (c))
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
		    && OMP_CLAUSE_DECL (c) == decl)
		  break;

	      if (c)
		goto maybe_mark_addressable_and_ret;
	    }
	}

      /* For tasks avoid using copy-in/out.  As tasks can be
	 deferred or executed in different thread, when GOMP_task
	 returns, the task hasn't necessarily terminated.  */
      if (is_task_ctx (shared_ctx))
	{
	  tree outer;
	maybe_mark_addressable_and_ret:
	  outer = maybe_lookup_decl_in_outer_ctx (decl, shared_ctx);
	  if (is_gimple_reg (outer) && !omp_member_access_dummy_var (outer))
	    {
	      /* Taking address of OUTER in lower_send_shared_vars
		 might need regimplification of everything that uses the
		 variable.  */
	      if (!task_shared_vars)
		task_shared_vars = BITMAP_ALLOC (NULL);
	      bitmap_set_bit (task_shared_vars, DECL_UID (outer));
	      TREE_ADDRESSABLE (outer) = 1;
	    }
	  return true;
	}
    }

  return false;
}

/* Construct a new automatic decl similar to VAR.  */

static tree
omp_copy_decl_2 (tree var, tree name, tree type, omp_context *ctx)
{
  tree copy = copy_var_decl (var, name, type);

  DECL_CONTEXT (copy) = current_function_decl;
  DECL_CHAIN (copy) = ctx->block_vars;
  /* If VAR is listed in task_shared_vars, it means it wasn't
     originally addressable and is just because task needs to take
     it's address.  But we don't need to take address of privatizations
     from that var.  */
  if (TREE_ADDRESSABLE (var)
      && task_shared_vars
      && bitmap_bit_p (task_shared_vars, DECL_UID (var)))
    TREE_ADDRESSABLE (copy) = 0;
  ctx->block_vars = copy;

  return copy;
}

static tree
omp_copy_decl_1 (tree var, omp_context *ctx)
{
  return omp_copy_decl_2 (var, DECL_NAME (var), TREE_TYPE (var), ctx);
}

/* Build COMPONENT_REF and set TREE_THIS_VOLATILE and TREE_READONLY on it
   as appropriate.  */
static tree
omp_build_component_ref (tree obj, tree field)
{
  tree ret = build3 (COMPONENT_REF, TREE_TYPE (field), obj, field, NULL);
  if (TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ret) |= 1;
  if (TREE_READONLY (field))
    TREE_READONLY (ret) |= 1;
  return ret;
}

/* Build tree nodes to access the field for VAR on the receiver side.  */

static tree
build_receiver_ref (tree var, bool by_ref, omp_context *ctx)
{
  tree x, field = lookup_field (var, ctx);

  /* If the receiver record type was remapped in the child function,
     remap the field into the new record type.  */
  x = maybe_lookup_field (field, ctx);
  if (x != NULL)
    field = x;

  x = build_simple_mem_ref (ctx->receiver_decl);
  TREE_THIS_NOTRAP (x) = 1;
  x = omp_build_component_ref (x, field);
  if (by_ref)
    {
      x = build_simple_mem_ref (x);
      TREE_THIS_NOTRAP (x) = 1;
    }

  return x;
}

/* Build tree nodes to access VAR in the scope outer to CTX.  In the case
   of a parallel, this is a component reference; for workshare constructs
   this is some variable.  */

static tree
build_outer_var_ref (tree var, omp_context *ctx,
		     enum omp_clause_code code = OMP_CLAUSE_ERROR)
{
  tree x;

  if (is_global_var (maybe_lookup_decl_in_outer_ctx (var, ctx)))
    x = var;
  else if (is_variable_sized (var))
    {
      x = TREE_OPERAND (DECL_VALUE_EXPR (var), 0);
      x = build_outer_var_ref (x, ctx, code);
      x = build_simple_mem_ref (x);
    }
  else if (is_taskreg_ctx (ctx))
    {
      bool by_ref = use_pointer_for_field (var, NULL);
      x = build_receiver_ref (var, by_ref, ctx);
    }
  else if ((gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	    && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
	   || (code == OMP_CLAUSE_PRIVATE
	       && (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
		   || gimple_code (ctx->stmt) == GIMPLE_OMP_SECTIONS
		   || gimple_code (ctx->stmt) == GIMPLE_OMP_SINGLE)))
    {
      /* #pragma omp simd isn't a worksharing construct, and can reference
	 even private vars in its linear etc. clauses.
	 Similarly for OMP_CLAUSE_PRIVATE with outer ref, that can refer
	 to private vars in all worksharing constructs.  */
      x = NULL_TREE;
      if (ctx->outer && is_taskreg_ctx (ctx))
	x = lookup_decl (var, ctx->outer);
      else if (ctx->outer)
	x = maybe_lookup_decl_in_outer_ctx (var, ctx);
      if (x == NULL_TREE)
	x = var;
    }
  else if (code == OMP_CLAUSE_LASTPRIVATE && is_taskloop_ctx (ctx))
    {
      gcc_assert (ctx->outer);
      splay_tree_node n
	= splay_tree_lookup (ctx->outer->field_map,
			     (splay_tree_key) &DECL_UID (var));
      if (n == NULL)
	{
	  if (is_global_var (maybe_lookup_decl_in_outer_ctx (var, ctx->outer)))
	    x = var;
	  else
	    x = lookup_decl (var, ctx->outer);
	}
      else
	{
	  tree field = (tree) n->value;
	  /* If the receiver record type was remapped in the child function,
	     remap the field into the new record type.  */
	  x = maybe_lookup_field (field, ctx->outer);
	  if (x != NULL)
	    field = x;

	  x = build_simple_mem_ref (ctx->outer->receiver_decl);
	  x = omp_build_component_ref (x, field);
	  if (use_pointer_for_field (var, ctx->outer))
	    x = build_simple_mem_ref (x);
	}
    }
  else if (ctx->outer)
    {
      omp_context *outer = ctx->outer;
      if (gimple_code (outer->stmt) == GIMPLE_OMP_GRID_BODY)
	{
	  outer = outer->outer;
	  gcc_assert (outer
		      && gimple_code (outer->stmt) != GIMPLE_OMP_GRID_BODY);
	}
      x = lookup_decl (var, outer);
    }
  else if (omp_is_reference (var))
    /* This can happen with orphaned constructs.  If var is reference, it is
       possible it is shared and as such valid.  */
    x = var;
  else if (omp_member_access_dummy_var (var))
    x = var;
  else
    gcc_unreachable ();

  if (x == var)
    {
      tree t = omp_member_access_dummy_var (var);
      if (t)
	{
	  x = DECL_VALUE_EXPR (var);
	  tree o = maybe_lookup_decl_in_outer_ctx (t, ctx);
	  if (o != t)
	    x = unshare_and_remap (x, t, o);
	  else
	    x = unshare_expr (x);
	}
    }

  if (omp_is_reference (var))
    x = build_simple_mem_ref (x);

  return x;
}

/* Build tree nodes to access the field for VAR on the sender side.  */

static tree
build_sender_ref (splay_tree_key key, omp_context *ctx)
{
  tree field = lookup_sfield (key, ctx);
  return omp_build_component_ref (ctx->sender_decl, field);
}

static tree
build_sender_ref (tree var, omp_context *ctx)
{
  return build_sender_ref ((splay_tree_key) var, ctx);
}

/* Add a new field for VAR inside the structure CTX->SENDER_DECL.  If
   BASE_POINTERS_RESTRICT, declare the field with restrict.  */

static void
install_var_field (tree var, bool by_ref, int mask, omp_context *ctx,
		   bool base_pointers_restrict = false)
{
  tree field, type, sfield = NULL_TREE;
  splay_tree_key key = (splay_tree_key) var;

  if ((mask & 8) != 0)
    {
      key = (splay_tree_key) &DECL_UID (var);
      gcc_checking_assert (key != (splay_tree_key) var);
    }
  gcc_assert ((mask & 1) == 0
	      || !splay_tree_lookup (ctx->field_map, key));
  gcc_assert ((mask & 2) == 0 || !ctx->sfield_map
	      || !splay_tree_lookup (ctx->sfield_map, key));
  gcc_assert ((mask & 3) == 3
	      || !is_gimple_omp_oacc (ctx->stmt));

  type = TREE_TYPE (var);
  /* Prevent redeclaring the var in the split-off function with a restrict
     pointer type.  Note that we only clear type itself, restrict qualifiers in
     the pointed-to type will be ignored by points-to analysis.  */
  if (POINTER_TYPE_P (type)
      && TYPE_RESTRICT (type))
    type = build_qualified_type (type, TYPE_QUALS (type) & ~TYPE_QUAL_RESTRICT);

  if (mask & 4)
    {
      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
      type = build_pointer_type (build_pointer_type (type));
    }
  else if (by_ref)
    {
      type = build_pointer_type (type);
      if (base_pointers_restrict)
	type = build_qualified_type (type, TYPE_QUAL_RESTRICT);
    }
  else if ((mask & 3) == 1 && omp_is_reference (var))
    type = TREE_TYPE (type);

  field = build_decl (DECL_SOURCE_LOCATION (var),
		      FIELD_DECL, DECL_NAME (var), type);

  /* Remember what variable this field was created for.  This does have a
     side effect of making dwarf2out ignore this member, so for helpful
     debugging we clear it later in delete_omp_context.  */
  DECL_ABSTRACT_ORIGIN (field) = var;
  if (type == TREE_TYPE (var))
    {
      SET_DECL_ALIGN (field, DECL_ALIGN (var));
      DECL_USER_ALIGN (field) = DECL_USER_ALIGN (var);
      TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (var);
    }
  else
    SET_DECL_ALIGN (field, TYPE_ALIGN (type));

  if ((mask & 3) == 3)
    {
      insert_field_into_struct (ctx->record_type, field);
      if (ctx->srecord_type)
	{
	  sfield = build_decl (DECL_SOURCE_LOCATION (var),
			       FIELD_DECL, DECL_NAME (var), type);
	  DECL_ABSTRACT_ORIGIN (sfield) = var;
	  SET_DECL_ALIGN (sfield, DECL_ALIGN (field));
	  DECL_USER_ALIGN (sfield) = DECL_USER_ALIGN (field);
	  TREE_THIS_VOLATILE (sfield) = TREE_THIS_VOLATILE (field);
	  insert_field_into_struct (ctx->srecord_type, sfield);
	}
    }
  else
    {
      if (ctx->srecord_type == NULL_TREE)
	{
	  tree t;

	  ctx->srecord_type = lang_hooks.types.make_type (RECORD_TYPE);
	  ctx->sfield_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
	  for (t = TYPE_FIELDS (ctx->record_type); t ; t = TREE_CHAIN (t))
	    {
	      sfield = build_decl (DECL_SOURCE_LOCATION (t),
				   FIELD_DECL, DECL_NAME (t), TREE_TYPE (t));
	      DECL_ABSTRACT_ORIGIN (sfield) = DECL_ABSTRACT_ORIGIN (t);
	      insert_field_into_struct (ctx->srecord_type, sfield);
	      splay_tree_insert (ctx->sfield_map,
				 (splay_tree_key) DECL_ABSTRACT_ORIGIN (t),
				 (splay_tree_value) sfield);
	    }
	}
      sfield = field;
      insert_field_into_struct ((mask & 1) ? ctx->record_type
				: ctx->srecord_type, field);
    }

  if (mask & 1)
    splay_tree_insert (ctx->field_map, key, (splay_tree_value) field);
  if ((mask & 2) && ctx->sfield_map)
    splay_tree_insert (ctx->sfield_map, key, (splay_tree_value) sfield);
}

static tree
install_var_local (tree var, omp_context *ctx)
{
  tree new_var = omp_copy_decl_1 (var, ctx);
  insert_decl_map (&ctx->cb, var, new_var);
  return new_var;
}

/* Adjust the replacement for DECL in CTX for the new context.  This means
   copying the DECL_VALUE_EXPR, and fixing up the type.  */

static void
fixup_remapped_decl (tree decl, omp_context *ctx, bool private_debug)
{
  tree new_decl, size;

  new_decl = lookup_decl (decl, ctx);

  TREE_TYPE (new_decl) = remap_type (TREE_TYPE (decl), &ctx->cb);

  if ((!TREE_CONSTANT (DECL_SIZE (new_decl)) || private_debug)
      && DECL_HAS_VALUE_EXPR_P (decl))
    {
      tree ve = DECL_VALUE_EXPR (decl);
      walk_tree (&ve, copy_tree_body_r, &ctx->cb, NULL);
      SET_DECL_VALUE_EXPR (new_decl, ve);
      DECL_HAS_VALUE_EXPR_P (new_decl) = 1;
    }

  if (!TREE_CONSTANT (DECL_SIZE (new_decl)))
    {
      size = remap_decl (DECL_SIZE (decl), &ctx->cb);
      if (size == error_mark_node)
	size = TYPE_SIZE (TREE_TYPE (new_decl));
      DECL_SIZE (new_decl) = size;

      size = remap_decl (DECL_SIZE_UNIT (decl), &ctx->cb);
      if (size == error_mark_node)
	size = TYPE_SIZE_UNIT (TREE_TYPE (new_decl));
      DECL_SIZE_UNIT (new_decl) = size;
    }
}

/* The callback for remap_decl.  Search all containing contexts for a
   mapping of the variable; this avoids having to duplicate the splay
   tree ahead of time.  We know a mapping doesn't already exist in the
   given context.  Create new mappings to implement default semantics.  */

static tree
omp_copy_decl (tree var, copy_body_data *cb)
{
  omp_context *ctx = (omp_context *) cb;
  tree new_var;

  if (TREE_CODE (var) == LABEL_DECL)
    {
      if (FORCED_LABEL (var) || DECL_NONLOCAL (var))
	return var;
      new_var = create_artificial_label (DECL_SOURCE_LOCATION (var));
      DECL_CONTEXT (new_var) = current_function_decl;
      insert_decl_map (&ctx->cb, var, new_var);
      return new_var;
    }

  while (!is_taskreg_ctx (ctx))
    {
      ctx = ctx->outer;
      if (ctx == NULL)
	return var;
      new_var = maybe_lookup_decl (var, ctx);
      if (new_var)
	return new_var;
    }

  if (is_global_var (var) || decl_function_context (var) != ctx->cb.src_fn)
    return var;

  return error_mark_node;
}

/* Create a new context, with OUTER_CTX being the surrounding context.  */

static omp_context *
new_omp_context (gimple *stmt, omp_context *outer_ctx)
{
  omp_context *ctx = XCNEW (omp_context);

  splay_tree_insert (all_contexts, (splay_tree_key) stmt,
		     (splay_tree_value) ctx);
  ctx->stmt = stmt;

  if (outer_ctx)
    {
      ctx->outer = outer_ctx;
      ctx->cb = outer_ctx->cb;
      ctx->cb.block = NULL;
      ctx->depth = outer_ctx->depth + 1;
    }
  else
    {
      ctx->cb.src_fn = current_function_decl;
      ctx->cb.dst_fn = current_function_decl;
      ctx->cb.src_node = cgraph_node::get (current_function_decl);
      gcc_checking_assert (ctx->cb.src_node);
      ctx->cb.dst_node = ctx->cb.src_node;
      ctx->cb.src_cfun = cfun;
      ctx->cb.copy_decl = omp_copy_decl;
      ctx->cb.eh_lp_nr = 0;
      ctx->cb.transform_call_graph_edges = CB_CGE_MOVE;
      ctx->depth = 1;
    }

  ctx->cb.decl_map = new hash_map<tree, tree>;

  return ctx;
}

static gimple_seq maybe_catch_exception (gimple_seq);

/* Finalize task copyfn.  */

static void
finalize_task_copyfn (gomp_task *task_stmt)
{
  struct function *child_cfun;
  tree child_fn;
  gimple_seq seq = NULL, new_seq;
  gbind *bind;

  child_fn = gimple_omp_task_copy_fn (task_stmt);
  if (child_fn == NULL_TREE)
    return;

  child_cfun = DECL_STRUCT_FUNCTION (child_fn);
  DECL_STRUCT_FUNCTION (child_fn)->curr_properties = cfun->curr_properties;

  push_cfun (child_cfun);
  bind = gimplify_body (child_fn, false);
  gimple_seq_add_stmt (&seq, bind);
  new_seq = maybe_catch_exception (seq);
  if (new_seq != seq)
    {
      bind = gimple_build_bind (NULL, new_seq, NULL);
      seq = NULL;
      gimple_seq_add_stmt (&seq, bind);
    }
  gimple_set_body (child_fn, seq);
  pop_cfun ();

  /* Inform the callgraph about the new function.  */
  cgraph_node *node = cgraph_node::get_create (child_fn);
  node->parallelized_function = 1;
  cgraph_node::add_new_function (child_fn, false);
}

/* Destroy a omp_context data structures.  Called through the splay tree
   value delete callback.  */

static void
delete_omp_context (splay_tree_value value)
{
  omp_context *ctx = (omp_context *) value;

  delete ctx->cb.decl_map;

  if (ctx->field_map)
    splay_tree_delete (ctx->field_map);
  if (ctx->sfield_map)
    splay_tree_delete (ctx->sfield_map);

  /* We hijacked DECL_ABSTRACT_ORIGIN earlier.  We need to clear it before
     it produces corrupt debug information.  */
  if (ctx->record_type)
    {
      tree t;
      for (t = TYPE_FIELDS (ctx->record_type); t ; t = DECL_CHAIN (t))
	DECL_ABSTRACT_ORIGIN (t) = NULL;
    }
  if (ctx->srecord_type)
    {
      tree t;
      for (t = TYPE_FIELDS (ctx->srecord_type); t ; t = DECL_CHAIN (t))
	DECL_ABSTRACT_ORIGIN (t) = NULL;
    }

  if (is_task_ctx (ctx))
    finalize_task_copyfn (as_a <gomp_task *> (ctx->stmt));

  XDELETE (ctx);
}

/* Fix up RECEIVER_DECL with a type that has been remapped to the child
   context.  */

static void
fixup_child_record_type (omp_context *ctx)
{
  tree f, type = ctx->record_type;

  if (!ctx->receiver_decl)
    return;
  /* ??? It isn't sufficient to just call remap_type here, because
     variably_modified_type_p doesn't work the way we expect for
     record types.  Testing each field for whether it needs remapping
     and creating a new record by hand works, however.  */
  for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
    if (variably_modified_type_p (TREE_TYPE (f), ctx->cb.src_fn))
      break;
  if (f)
    {
      tree name, new_fields = NULL;

      type = lang_hooks.types.make_type (RECORD_TYPE);
      name = DECL_NAME (TYPE_NAME (ctx->record_type));
      name = build_decl (DECL_SOURCE_LOCATION (ctx->receiver_decl),
			 TYPE_DECL, name, type);
      TYPE_NAME (type) = name;

      for (f = TYPE_FIELDS (ctx->record_type); f ; f = DECL_CHAIN (f))
	{
	  tree new_f = copy_node (f);
	  DECL_CONTEXT (new_f) = type;
	  TREE_TYPE (new_f) = remap_type (TREE_TYPE (f), &ctx->cb);
	  DECL_CHAIN (new_f) = new_fields;
	  walk_tree (&DECL_SIZE (new_f), copy_tree_body_r, &ctx->cb, NULL);
	  walk_tree (&DECL_SIZE_UNIT (new_f), copy_tree_body_r,
		     &ctx->cb, NULL);
	  walk_tree (&DECL_FIELD_OFFSET (new_f), copy_tree_body_r,
		     &ctx->cb, NULL);
	  new_fields = new_f;

	  /* Arrange to be able to look up the receiver field
	     given the sender field.  */
	  splay_tree_insert (ctx->field_map, (splay_tree_key) f,
			     (splay_tree_value) new_f);
	}
      TYPE_FIELDS (type) = nreverse (new_fields);
      layout_type (type);
    }

  /* In a target region we never modify any of the pointers in *.omp_data_i,
     so attempt to help the optimizers.  */
  if (is_gimple_omp_offloaded (ctx->stmt))
    type = build_qualified_type (type, TYPE_QUAL_CONST);

  TREE_TYPE (ctx->receiver_decl)
    = build_qualified_type (build_reference_type (type), TYPE_QUAL_RESTRICT);
}

/* Instantiate decls as necessary in CTX to satisfy the data sharing
   specified by CLAUSES.  If BASE_POINTERS_RESTRICT, install var field with
   restrict.  */

static void
scan_sharing_clauses (tree clauses, omp_context *ctx,
		      bool base_pointers_restrict = false)
{
  tree c, decl;
  bool scan_array_reductions = false;

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      bool by_ref;

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_PRIVATE:
	  decl = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_PRIVATE_OUTER_REF (c))
	    goto do_private;
	  else if (!is_variable_sized (decl))
	    install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_SHARED:
	  decl = OMP_CLAUSE_DECL (c);
	  /* Ignore shared directives in teams construct.  */
	  if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
	    {
	      /* Global variables don't need to be copied,
		 the receiver side will use them directly.  */
	      tree odecl = maybe_lookup_decl_in_outer_ctx (decl, ctx);
	      if (is_global_var (odecl))
		break;
	      insert_decl_map (&ctx->cb, decl, odecl);
	      break;
	    }
	  gcc_assert (is_taskreg_ctx (ctx));
	  gcc_assert (!COMPLETE_TYPE_P (TREE_TYPE (decl))
		      || !is_variable_sized (decl));
	  /* Global variables don't need to be copied,
	     the receiver side will use them directly.  */
	  if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx)))
	    break;
	  if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	    {
	      use_pointer_for_field (decl, ctx);
	      break;
	    }
	  by_ref = use_pointer_for_field (decl, NULL);
	  if ((! TREE_READONLY (decl) && !OMP_CLAUSE_SHARED_READONLY (c))
	      || TREE_ADDRESSABLE (decl)
	      || by_ref
	      || omp_is_reference (decl))
	    {
	      by_ref = use_pointer_for_field (decl, ctx);
	      install_var_field (decl, by_ref, 3, ctx);
	      install_var_local (decl, ctx);
	      break;
	    }
	  /* We don't need to copy const scalar vars back.  */
	  OMP_CLAUSE_SET_CODE (c, OMP_CLAUSE_FIRSTPRIVATE);
	  goto do_private;

	case OMP_CLAUSE_REDUCTION:
	  decl = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	      && TREE_CODE (decl) == MEM_REF)
	    {
	      tree t = TREE_OPERAND (decl, 0);
	      if (TREE_CODE (t) == POINTER_PLUS_EXPR)
		t = TREE_OPERAND (t, 0);
	      if (TREE_CODE (t) == INDIRECT_REF
		  || TREE_CODE (t) == ADDR_EXPR)
		t = TREE_OPERAND (t, 0);
	      install_var_local (t, ctx);
	      if (is_taskreg_ctx (ctx)
		  && !is_global_var (maybe_lookup_decl_in_outer_ctx (t, ctx))
		  && !is_variable_sized (t))
		{
		  by_ref = use_pointer_for_field (t, ctx);
		  install_var_field (t, by_ref, 3, ctx);
		}
	      break;
	    }
	  goto do_private;

	case OMP_CLAUSE_LASTPRIVATE:
	  /* Let the corresponding firstprivate clause create
	     the variable.  */
	  if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
	    break;
	  /* FALLTHRU */

	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LINEAR:
	  decl = OMP_CLAUSE_DECL (c);
	do_private:
	  if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
	       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IS_DEVICE_PTR)
	      && is_gimple_omp_offloaded (ctx->stmt))
	    {
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
		install_var_field (decl, !omp_is_reference (decl), 3, ctx);
	      else if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
		install_var_field (decl, true, 3, ctx);
	      else
		install_var_field (decl, false, 3, ctx);
	    }
	  if (is_variable_sized (decl))
	    {
	      if (is_task_ctx (ctx))
		install_var_field (decl, false, 1, ctx);
	      break;
	    }
	  else if (is_taskreg_ctx (ctx))
	    {
	      bool global
		= is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx));
	      by_ref = use_pointer_for_field (decl, NULL);

	      if (is_task_ctx (ctx)
		  && (global || by_ref || omp_is_reference (decl)))
		{
		  install_var_field (decl, false, 1, ctx);
		  if (!global)
		    install_var_field (decl, by_ref, 2, ctx);
		}
	      else if (!global)
		install_var_field (decl, by_ref, 3, ctx);
	    }
	  install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_USE_DEVICE_PTR:
	  decl = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    install_var_field (decl, true, 3, ctx);
	  else
	    install_var_field (decl, false, 3, ctx);
	  if (DECL_SIZE (decl)
	      && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
	    {
	      tree decl2 = DECL_VALUE_EXPR (decl);
	      gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
	      decl2 = TREE_OPERAND (decl2, 0);
	      gcc_assert (DECL_P (decl2));
	      install_var_local (decl2, ctx);
	    }
	  install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_IS_DEVICE_PTR:
	  decl = OMP_CLAUSE_DECL (c);
	  goto do_private;

	case OMP_CLAUSE__LOOPTEMP_:
	  gcc_assert (is_taskreg_ctx (ctx));
	  decl = OMP_CLAUSE_DECL (c);
	  install_var_field (decl, false, 3, ctx);
	  install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_COPYIN:
	  decl = OMP_CLAUSE_DECL (c);
	  by_ref = use_pointer_for_field (decl, NULL);
	  install_var_field (decl, by_ref, 3, ctx);
	  break;

	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE__CILK_FOR_COUNT_:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	  if (ctx->outer)
	    scan_omp_op (&OMP_CLAUSE_OPERAND (c, 0), ctx->outer);
	  break;

	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_MAP:
	  if (ctx->outer)
	    scan_omp_op (&OMP_CLAUSE_SIZE (c), ctx->outer);
	  decl = OMP_CLAUSE_DECL (c);
	  /* Global variables with "omp declare target" attribute
	     don't need to be copied, the receiver side will use them
	     directly.  However, global variables with "omp declare target link"
	     attribute need to be copied.  */
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && DECL_P (decl)
	      && ((OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FIRSTPRIVATE_POINTER
		   && (OMP_CLAUSE_MAP_KIND (c)
		       != GOMP_MAP_FIRSTPRIVATE_REFERENCE))
		  || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	      && is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx))
	      && varpool_node::get_create (decl)->offloadable
	      && !lookup_attribute ("omp declare target link",
				    DECL_ATTRIBUTES (decl)))
	    break;
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER)
	    {
	      /* Ignore GOMP_MAP_POINTER kind for arrays in regions that are
		 not offloaded; there is nothing to map for those.  */
	      if (!is_gimple_omp_offloaded (ctx->stmt)
		  && !POINTER_TYPE_P (TREE_TYPE (decl))
		  && !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c))
		break;
	    }
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	      && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		  || (OMP_CLAUSE_MAP_KIND (c)
		      == GOMP_MAP_FIRSTPRIVATE_REFERENCE)))
	    {
	      if (TREE_CODE (decl) == COMPONENT_REF
		  || (TREE_CODE (decl) == INDIRECT_REF
		      && TREE_CODE (TREE_OPERAND (decl, 0)) == COMPONENT_REF
		      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (decl, 0)))
			  == REFERENCE_TYPE)))
		break;
	      if (DECL_SIZE (decl)
		  && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  install_var_local (decl2, ctx);
		}
	      install_var_local (decl, ctx);
	      break;
	    }
	  if (DECL_P (decl))
	    {
	      if (DECL_SIZE (decl)
		  && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  install_var_field (decl2, true, 3, ctx);
		  install_var_local (decl2, ctx);
		  install_var_local (decl, ctx);
		}
	      else
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		      && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		      && !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
		    install_var_field (decl, true, 7, ctx);
		  else
		    install_var_field (decl, true, 3, ctx,
				       base_pointers_restrict);
		  if (is_gimple_omp_offloaded (ctx->stmt)
		      && !OMP_CLAUSE_MAP_IN_REDUCTION (c))
		    install_var_local (decl, ctx);
		}
	    }
	  else
	    {
	      tree base = get_base_address (decl);
	      tree nc = OMP_CLAUSE_CHAIN (c);
	      if (DECL_P (base)
		  && nc != NULL_TREE
		  && OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
		  && OMP_CLAUSE_DECL (nc) == base
		  && OMP_CLAUSE_MAP_KIND (nc) == GOMP_MAP_POINTER
		  && integer_zerop (OMP_CLAUSE_SIZE (nc)))
		{
		  OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c) = 1;
		  OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (nc) = 1;
		}
	      else
		{
		  if (ctx->outer)
		    {
		      scan_omp_op (&OMP_CLAUSE_DECL (c), ctx->outer);
		      decl = OMP_CLAUSE_DECL (c);
		    }
		  gcc_assert (!splay_tree_lookup (ctx->field_map,
						  (splay_tree_key) decl));
		  tree field
		    = build_decl (OMP_CLAUSE_LOCATION (c),
				  FIELD_DECL, NULL_TREE, ptr_type_node);
		  SET_DECL_ALIGN (field, TYPE_ALIGN (ptr_type_node));
		  insert_field_into_struct (ctx->record_type, field);
		  splay_tree_insert (ctx->field_map, (splay_tree_key) decl,
				     (splay_tree_value) field);
		}
	    }
	  break;

	case OMP_CLAUSE__GRIDDIM_:
	  if (ctx->outer)
	    {
	      scan_omp_op (&OMP_CLAUSE__GRIDDIM__SIZE (c), ctx->outer);
	      scan_omp_op (&OMP_CLAUSE__GRIDDIM__GROUP (c), ctx->outer);
	    }
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE__SIMT_:
	case OMP_CLAUSE_DEFAULT:
	  break;

	case OMP_CLAUSE_ALIGNED:
	  decl = OMP_CLAUSE_DECL (c);
	  if (is_global_var (decl)
	      && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    install_var_local (decl, ctx);
	  break;

	case OMP_CLAUSE__CACHE_:
	default:
	  gcc_unreachable ();
	}
    }

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_LASTPRIVATE:
	  /* Let the corresponding firstprivate clause create
	     the variable.  */
	  if (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c))
	    scan_array_reductions = true;
	  if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
	    break;
	  /* FALLTHRU */

	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_LINEAR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	  decl = OMP_CLAUSE_DECL (c);
	  if (is_variable_sized (decl))
	    {
	      if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IS_DEVICE_PTR)
		  && is_gimple_omp_offloaded (ctx->stmt))
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  install_var_local (decl2, ctx);
		  fixup_remapped_decl (decl2, ctx, false);
		}
	      install_var_local (decl, ctx);
	    }
	  fixup_remapped_decl (decl, ctx,
			       OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
			       && OMP_CLAUSE_PRIVATE_DEBUG (c));
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	      && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c))
	    scan_array_reductions = true;
	  break;

	case OMP_CLAUSE_REDUCTION:
	  decl = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (decl) != MEM_REF)
	    {
	      if (is_variable_sized (decl))
		install_var_local (decl, ctx);
	      fixup_remapped_decl (decl, ctx, false);
	    }
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    scan_array_reductions = true;
	  break;

	case OMP_CLAUSE_SHARED:
	  /* Ignore shared directives in teams construct.  */
	  if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
	    break;
	  decl = OMP_CLAUSE_DECL (c);
	  if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx)))
	    break;
	  if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	    {
	      if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl,
								 ctx->outer)))
		break;
	      bool by_ref = use_pointer_for_field (decl, ctx);
	      install_var_field (decl, by_ref, 11, ctx);
	      break;
	    }
	  fixup_remapped_decl (decl, ctx, false);
	  break;

	case OMP_CLAUSE_MAP:
	  if (!is_gimple_omp_offloaded (ctx->stmt))
	    break;
	  decl = OMP_CLAUSE_DECL (c);
	  if (DECL_P (decl)
	      && ((OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FIRSTPRIVATE_POINTER
		   && (OMP_CLAUSE_MAP_KIND (c)
		       != GOMP_MAP_FIRSTPRIVATE_REFERENCE))
		  || TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	      && is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx))
	      && varpool_node::get_create (decl)->offloadable)
	    break;
	  if (DECL_P (decl))
	    {
	      if ((OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		   || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER)
		  && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
		  && !COMPLETE_TYPE_P (TREE_TYPE (decl)))
		{
		  tree new_decl = lookup_decl (decl, ctx);
		  TREE_TYPE (new_decl)
		    = remap_type (TREE_TYPE (decl), &ctx->cb);
		}
	      else if (DECL_SIZE (decl)
		       && TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST)
		{
		  tree decl2 = DECL_VALUE_EXPR (decl);
		  gcc_assert (TREE_CODE (decl2) == INDIRECT_REF);
		  decl2 = TREE_OPERAND (decl2, 0);
		  gcc_assert (DECL_P (decl2));
		  fixup_remapped_decl (decl2, ctx, false);
		  fixup_remapped_decl (decl, ctx, true);
		}
	      else
		fixup_remapped_decl (decl, ctx, false);
	    }
	  break;

	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_ALIGNED:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE__LOOPTEMP_:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE__CILK_FOR_COUNT_:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE__GRIDDIM_:
	case OMP_CLAUSE__SIMT_:
	  break;

	case OMP_CLAUSE__CACHE_:
	default:
	  gcc_unreachable ();
	}
    }

  gcc_checking_assert (!scan_array_reductions
		       || !is_gimple_omp_oacc (ctx->stmt));
  if (scan_array_reductions)
    {
      for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	    && OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	  {
	    scan_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c), ctx);
	    scan_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c), ctx);
	  }
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		 && OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c))
	  scan_omp (&OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c), ctx);
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		 && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c))
	  scan_omp (&OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c), ctx);
    }
}

/* Create a new name for omp child function.  Returns an identifier.  If
   IS_CILK_FOR is true then the suffix for the child function is
   "_cilk_for_fn."  */

static tree
create_omp_child_function_name (bool task_copy, bool is_cilk_for)
{
  if (is_cilk_for)
    return clone_function_name (current_function_decl, "_cilk_for_fn");
  return clone_function_name (current_function_decl,
			      task_copy ? "_omp_cpyfn" : "_omp_fn");
}

/* Returns the type of the induction variable for the child function for
   _Cilk_for and the types for _high and _low variables based on TYPE.  */

static tree
cilk_for_check_loop_diff_type (tree type)
{
  if (TYPE_PRECISION (type) <= TYPE_PRECISION (uint32_type_node))
    {
      if (TYPE_UNSIGNED (type))
	return uint32_type_node;
      else
	return integer_type_node;
    }
  else
    {
      if (TYPE_UNSIGNED (type))
	return uint64_type_node;
      else
	return long_long_integer_type_node;
    }
}

/* Return true if CTX may belong to offloaded code: either if current function
   is offloaded, or any enclosing context corresponds to a target region.  */

static bool
omp_maybe_offloaded_ctx (omp_context *ctx)
{
  if (cgraph_node::get (current_function_decl)->offloadable)
    return true;
  for (; ctx; ctx = ctx->outer)
    if (is_gimple_omp_offloaded (ctx->stmt))
      return true;
  return false;
}

/* Build a decl for the omp child function.  It'll not contain a body
   yet, just the bare decl.  */

static void
create_omp_child_function (omp_context *ctx, bool task_copy)
{
  tree decl, type, name, t;

  tree cilk_for_count
    = (flag_cilkplus && gimple_code (ctx->stmt) == GIMPLE_OMP_PARALLEL)
      ? omp_find_clause (gimple_omp_parallel_clauses (ctx->stmt),
			 OMP_CLAUSE__CILK_FOR_COUNT_) : NULL_TREE;
  tree cilk_var_type = NULL_TREE;

  name = create_omp_child_function_name (task_copy,
					 cilk_for_count != NULL_TREE);
  if (task_copy)
    type = build_function_type_list (void_type_node, ptr_type_node,
				     ptr_type_node, NULL_TREE);
  else if (cilk_for_count)
    {
      type = TREE_TYPE (OMP_CLAUSE_OPERAND (cilk_for_count, 0));
      cilk_var_type = cilk_for_check_loop_diff_type (type);
      type = build_function_type_list (void_type_node, ptr_type_node,
				       cilk_var_type, cilk_var_type, NULL_TREE);
    }
  else
    type = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);

  decl = build_decl (gimple_location (ctx->stmt), FUNCTION_DECL, name, type);

  gcc_checking_assert (!is_gimple_omp_oacc (ctx->stmt)
		       || !task_copy);
  if (!task_copy)
    ctx->cb.dst_fn = decl;
  else
    gimple_omp_task_set_copy_fn (ctx->stmt, decl);

  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (decl)) = decl;
  DECL_ATTRIBUTES (decl) = DECL_ATTRIBUTES (current_function_decl);
  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl)
    = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (current_function_decl);
  DECL_FUNCTION_SPECIFIC_TARGET (decl)
    = DECL_FUNCTION_SPECIFIC_TARGET (current_function_decl);
  DECL_FUNCTION_VERSIONED (decl)
    = DECL_FUNCTION_VERSIONED (current_function_decl);

  if (omp_maybe_offloaded_ctx (ctx))
    {
      cgraph_node::get_create (decl)->offloadable = 1;
      if (ENABLE_OFFLOADING)
	g->have_offload = true;
    }

  if (cgraph_node::get_create (decl)->offloadable
      && !lookup_attribute ("omp declare target",
                           DECL_ATTRIBUTES (current_function_decl)))
    {
      const char *target_attr = (is_gimple_omp_offloaded (ctx->stmt)
				 ? "omp target entrypoint"
				 : "omp declare target");
      DECL_ATTRIBUTES (decl)
	= tree_cons (get_identifier (target_attr),
		     NULL_TREE, DECL_ATTRIBUTES (decl));
    }

  t = build_decl (DECL_SOURCE_LOCATION (decl),
		  RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_CONTEXT (t) = decl;
  DECL_RESULT (decl) = t;

  /* _Cilk_for's child function requires two extra parameters called
     __low and __high that are set the by Cilk runtime when it calls this
     function.  */
  if (cilk_for_count)
    {
      t = build_decl (DECL_SOURCE_LOCATION (decl),
		      PARM_DECL, get_identifier ("__high"), cilk_var_type);
      DECL_ARTIFICIAL (t) = 1;
      DECL_NAMELESS (t) = 1;
      DECL_ARG_TYPE (t) = ptr_type_node;
      DECL_CONTEXT (t) = current_function_decl;
      TREE_USED (t) = 1;
      DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
      DECL_ARGUMENTS (decl) = t;

      t = build_decl (DECL_SOURCE_LOCATION (decl),
		      PARM_DECL, get_identifier ("__low"), cilk_var_type);
      DECL_ARTIFICIAL (t) = 1;
      DECL_NAMELESS (t) = 1;
      DECL_ARG_TYPE (t) = ptr_type_node;
      DECL_CONTEXT (t) = current_function_decl;
      TREE_USED (t) = 1;
      DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
      DECL_ARGUMENTS (decl) = t;
    }

  tree data_name = get_identifier (".omp_data_i");
  t = build_decl (DECL_SOURCE_LOCATION (decl), PARM_DECL, data_name,
		  ptr_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_NAMELESS (t) = 1;
  DECL_ARG_TYPE (t) = ptr_type_node;
  DECL_CONTEXT (t) = current_function_decl;
  TREE_USED (t) = 1;
  TREE_READONLY (t) = 1;
  if (cilk_for_count)
    DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
  DECL_ARGUMENTS (decl) = t;
  if (!task_copy)
    ctx->receiver_decl = t;
  else
    {
      t = build_decl (DECL_SOURCE_LOCATION (decl),
		      PARM_DECL, get_identifier (".omp_data_o"),
		      ptr_type_node);
      DECL_ARTIFICIAL (t) = 1;
      DECL_NAMELESS (t) = 1;
      DECL_ARG_TYPE (t) = ptr_type_node;
      DECL_CONTEXT (t) = current_function_decl;
      TREE_USED (t) = 1;
      TREE_ADDRESSABLE (t) = 1;
      DECL_CHAIN (t) = DECL_ARGUMENTS (decl);
      DECL_ARGUMENTS (decl) = t;
    }

  /* Allocate memory for the function structure.  The call to
     allocate_struct_function clobbers CFUN, so we need to restore
     it afterward.  */
  push_struct_function (decl);
  cfun->function_end_locus = gimple_location (ctx->stmt);
  init_tree_ssa (cfun);
  pop_cfun ();
}

/* Callback for walk_gimple_seq.  Check if combined parallel
   contains gimple_omp_for_combined_into_p OMP_FOR.  */

tree
omp_find_combined_for (gimple_stmt_iterator *gsi_p,
		       bool *handled_ops_p,
		       struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_FOR:
      if (gimple_omp_for_combined_into_p (stmt)
	  && gimple_omp_for_kind (stmt)
	     == *(const enum gf_mask *) (wi->info))
	{
	  wi->info = stmt;
	  return integer_zero_node;
	}
      break;
    default:
      break;
    }
  return NULL;
}

/* Add _LOOPTEMP_ clauses on OpenMP parallel or task.  */

static void
add_taskreg_looptemp_clauses (enum gf_mask msk, gimple *stmt,
			      omp_context *outer_ctx)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.val_only = true;
  wi.info = (void *) &msk;
  walk_gimple_seq (gimple_omp_body (stmt), omp_find_combined_for, NULL, &wi);
  if (wi.info != (void *) &msk)
    {
      gomp_for *for_stmt = as_a <gomp_for *> ((gimple *) wi.info);
      struct omp_for_data fd;
      omp_extract_for_data (for_stmt, &fd, NULL);
      /* We need two temporaries with fd.loop.v type (istart/iend)
	 and then (fd.collapse - 1) temporaries with the same
	 type for count2 ... countN-1 vars if not constant.  */
      size_t count = 2, i;
      tree type = fd.iter_type;
      if (fd.collapse > 1
	  && TREE_CODE (fd.loop.n2) != INTEGER_CST)
	{
	  count += fd.collapse - 1;
	  /* If there are lastprivate clauses on the inner
	     GIMPLE_OMP_FOR, add one more temporaries for the total number
	     of iterations (product of count1 ... countN-1).  */
	  if (omp_find_clause (gimple_omp_for_clauses (for_stmt),
			       OMP_CLAUSE_LASTPRIVATE))
	    count++;
	  else if (msk == GF_OMP_FOR_KIND_FOR
		   && omp_find_clause (gimple_omp_parallel_clauses (stmt),
				       OMP_CLAUSE_LASTPRIVATE))
	    count++;
	}
      for (i = 0; i < count; i++)
	{
	  tree temp = create_tmp_var (type);
	  tree c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__LOOPTEMP_);
	  insert_decl_map (&outer_ctx->cb, temp, temp);
	  OMP_CLAUSE_DECL (c) = temp;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	  gimple_omp_taskreg_set_clauses (stmt, c);
	}
    }
}

/* Scan an OpenMP parallel directive.  */

static void
scan_omp_parallel (gimple_stmt_iterator *gsi, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name;
  gomp_parallel *stmt = as_a <gomp_parallel *> (gsi_stmt (*gsi));

  /* Ignore parallel directives with empty bodies, unless there
     are copyin clauses.  */
  if (optimize > 0
      && empty_body_p (gimple_omp_body (stmt))
      && omp_find_clause (gimple_omp_parallel_clauses (stmt),
			  OMP_CLAUSE_COPYIN) == NULL)
    {
      gsi_replace (gsi, gimple_build_nop (), false);
      return;
    }

  if (gimple_omp_parallel_combined_p (stmt))
    add_taskreg_looptemp_clauses (GF_OMP_FOR_KIND_FOR, stmt, outer_ctx);

  ctx = new_omp_context (stmt, outer_ctx);
  taskreg_contexts.safe_push (ctx);
  if (taskreg_nesting_level > 1)
    ctx->is_nested = true;
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_data_s");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  DECL_ARTIFICIAL (name) = 1;
  DECL_NAMELESS (name) = 1;
  TYPE_NAME (ctx->record_type) = name;
  TYPE_ARTIFICIAL (ctx->record_type) = 1;
  if (!gimple_omp_parallel_grid_phony (stmt))
    {
      create_omp_child_function (ctx, false);
      gimple_omp_parallel_set_child_fn (stmt, ctx->cb.dst_fn);
    }

  scan_sharing_clauses (gimple_omp_parallel_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    ctx->record_type = ctx->receiver_decl = NULL;
}

/* Scan an OpenMP task directive.  */

static void
scan_omp_task (gimple_stmt_iterator *gsi, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name, t;
  gomp_task *stmt = as_a <gomp_task *> (gsi_stmt (*gsi));

  /* Ignore task directives with empty bodies, unless they have depend
     clause.  */
  if (optimize > 0
      && empty_body_p (gimple_omp_body (stmt))
      && !omp_find_clause (gimple_omp_task_clauses (stmt), OMP_CLAUSE_DEPEND))
    {
      gsi_replace (gsi, gimple_build_nop (), false);
      return;
    }

  if (gimple_omp_task_taskloop_p (stmt))
    add_taskreg_looptemp_clauses (GF_OMP_FOR_KIND_TASKLOOP, stmt, outer_ctx);

  ctx = new_omp_context (stmt, outer_ctx);
  taskreg_contexts.safe_push (ctx);
  if (taskreg_nesting_level > 1)
    ctx->is_nested = true;
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_data_s");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  DECL_ARTIFICIAL (name) = 1;
  DECL_NAMELESS (name) = 1;
  TYPE_NAME (ctx->record_type) = name;
  TYPE_ARTIFICIAL (ctx->record_type) = 1;
  create_omp_child_function (ctx, false);
  gimple_omp_task_set_child_fn (stmt, ctx->cb.dst_fn);

  scan_sharing_clauses (gimple_omp_task_clauses (stmt), ctx);

  if (ctx->srecord_type)
    {
      name = create_tmp_var_name (".omp_data_a");
      name = build_decl (gimple_location (stmt),
			 TYPE_DECL, name, ctx->srecord_type);
      DECL_ARTIFICIAL (name) = 1;
      DECL_NAMELESS (name) = 1;
      TYPE_NAME (ctx->srecord_type) = name;
      TYPE_ARTIFICIAL (ctx->srecord_type) = 1;
      create_omp_child_function (ctx, true);
    }

  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    {
      ctx->record_type = ctx->receiver_decl = NULL;
      t = build_int_cst (long_integer_type_node, 0);
      gimple_omp_task_set_arg_size (stmt, t);
      t = build_int_cst (long_integer_type_node, 1);
      gimple_omp_task_set_arg_align (stmt, t);
    }
}

/* Helper function for finish_taskreg_scan, called through walk_tree.
   If maybe_lookup_decl_in_outer_context returns non-NULL for some
   tree, replace it in the expression.  */

static tree
finish_taskreg_remap (tree *tp, int *walk_subtrees, void *data)
{
  if (VAR_P (*tp))
    {
      omp_context *ctx = (omp_context *) data;
      tree t = maybe_lookup_decl_in_outer_ctx (*tp, ctx);
      if (t != *tp)
	{
	  if (DECL_HAS_VALUE_EXPR_P (t))
	    t = unshare_expr (DECL_VALUE_EXPR (t));
	  *tp = t;
	}
      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* If any decls have been made addressable during scan_omp,
   adjust their fields if needed, and layout record types
   of parallel/task constructs.  */

static void
finish_taskreg_scan (omp_context *ctx)
{
  if (ctx->record_type == NULL_TREE)
    return;

  /* If any task_shared_vars were needed, verify all
     OMP_CLAUSE_SHARED clauses on GIMPLE_OMP_{PARALLEL,TASK}
     statements if use_pointer_for_field hasn't changed
     because of that.  If it did, update field types now.  */
  if (task_shared_vars)
    {
      tree c;

      for (c = gimple_omp_taskreg_clauses (ctx->stmt);
	   c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
	    && !OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	  {
	    tree decl = OMP_CLAUSE_DECL (c);

	    /* Global variables don't need to be copied,
	       the receiver side will use them directly.  */
	    if (is_global_var (maybe_lookup_decl_in_outer_ctx (decl, ctx)))
	      continue;
	    if (!bitmap_bit_p (task_shared_vars, DECL_UID (decl))
		|| !use_pointer_for_field (decl, ctx))
	      continue;
	    tree field = lookup_field (decl, ctx);
	    if (TREE_CODE (TREE_TYPE (field)) == POINTER_TYPE
		&& TREE_TYPE (TREE_TYPE (field)) == TREE_TYPE (decl))
	      continue;
	    TREE_TYPE (field) = build_pointer_type (TREE_TYPE (decl));
	    TREE_THIS_VOLATILE (field) = 0;
	    DECL_USER_ALIGN (field) = 0;
	    SET_DECL_ALIGN (field, TYPE_ALIGN (TREE_TYPE (field)));
	    if (TYPE_ALIGN (ctx->record_type) < DECL_ALIGN (field))
	      SET_TYPE_ALIGN (ctx->record_type, DECL_ALIGN (field));
	    if (ctx->srecord_type)
	      {
		tree sfield = lookup_sfield (decl, ctx);
		TREE_TYPE (sfield) = TREE_TYPE (field);
		TREE_THIS_VOLATILE (sfield) = 0;
		DECL_USER_ALIGN (sfield) = 0;
		SET_DECL_ALIGN (sfield, DECL_ALIGN (field));
		if (TYPE_ALIGN (ctx->srecord_type) < DECL_ALIGN (sfield))
		  SET_TYPE_ALIGN (ctx->srecord_type, DECL_ALIGN (sfield));
	      }
	  }
    }

  if (gimple_code (ctx->stmt) == GIMPLE_OMP_PARALLEL)
    {
      layout_type (ctx->record_type);
      fixup_child_record_type (ctx);
    }
  else
    {
      location_t loc = gimple_location (ctx->stmt);
      tree *p, vla_fields = NULL_TREE, *q = &vla_fields;
      /* Move VLA fields to the end.  */
      p = &TYPE_FIELDS (ctx->record_type);
      while (*p)
	if (!TYPE_SIZE_UNIT (TREE_TYPE (*p))
	    || ! TREE_CONSTANT (TYPE_SIZE_UNIT (TREE_TYPE (*p))))
	  {
	    *q = *p;
	    *p = TREE_CHAIN (*p);
	    TREE_CHAIN (*q) = NULL_TREE;
	    q = &TREE_CHAIN (*q);
	  }
	else
	  p = &DECL_CHAIN (*p);
      *p = vla_fields;
      if (gimple_omp_task_taskloop_p (ctx->stmt))
	{
	  /* Move fields corresponding to first and second _looptemp_
	     clause first.  There are filled by GOMP_taskloop
	     and thus need to be in specific positions.  */
	  tree c1 = gimple_omp_task_clauses (ctx->stmt);
	  c1 = omp_find_clause (c1, OMP_CLAUSE__LOOPTEMP_);
	  tree c2 = omp_find_clause (OMP_CLAUSE_CHAIN (c1),
				     OMP_CLAUSE__LOOPTEMP_);
	  tree f1 = lookup_field (OMP_CLAUSE_DECL (c1), ctx);
	  tree f2 = lookup_field (OMP_CLAUSE_DECL (c2), ctx);
	  p = &TYPE_FIELDS (ctx->record_type);
	  while (*p)
	    if (*p == f1 || *p == f2)
	      *p = DECL_CHAIN (*p);
	    else
	      p = &DECL_CHAIN (*p);
	  DECL_CHAIN (f1) = f2;
	  DECL_CHAIN (f2) = TYPE_FIELDS (ctx->record_type);
	  TYPE_FIELDS (ctx->record_type) = f1;
	  if (ctx->srecord_type)
	    {
	      f1 = lookup_sfield (OMP_CLAUSE_DECL (c1), ctx);
	      f2 = lookup_sfield (OMP_CLAUSE_DECL (c2), ctx);
	      p = &TYPE_FIELDS (ctx->srecord_type);
	      while (*p)
		if (*p == f1 || *p == f2)
		  *p = DECL_CHAIN (*p);
		else
		  p = &DECL_CHAIN (*p);
	      DECL_CHAIN (f1) = f2;
	      DECL_CHAIN (f2) = TYPE_FIELDS (ctx->srecord_type);
	      TYPE_FIELDS (ctx->srecord_type) = f1;
	    }
	}
      layout_type (ctx->record_type);
      fixup_child_record_type (ctx);
      if (ctx->srecord_type)
	layout_type (ctx->srecord_type);
      tree t = fold_convert_loc (loc, long_integer_type_node,
				 TYPE_SIZE_UNIT (ctx->record_type));
      if (TREE_CODE (t) != INTEGER_CST)
	{
	  t = unshare_expr (t);
	  walk_tree (&t, finish_taskreg_remap, ctx, NULL);
	}
      gimple_omp_task_set_arg_size (ctx->stmt, t);
      t = build_int_cst (long_integer_type_node,
			 TYPE_ALIGN_UNIT (ctx->record_type));
      gimple_omp_task_set_arg_align (ctx->stmt, t);
    }
}

/* Find the enclosing offload context.  */

static omp_context *
enclosing_target_ctx (omp_context *ctx)
{
  for (; ctx; ctx = ctx->outer)
    if (gimple_code (ctx->stmt) == GIMPLE_OMP_TARGET)
      break;

  return ctx;
}

/* Return true if ctx is part of an oacc kernels region.  */

static bool
ctx_in_oacc_kernels_region (omp_context *ctx)
{
  for (;ctx != NULL; ctx = ctx->outer)
    {
      gimple *stmt = ctx->stmt;
      if (gimple_code (stmt) == GIMPLE_OMP_TARGET
	  && gimple_omp_target_kind (stmt) == GF_OMP_TARGET_KIND_OACC_KERNELS)
	return true;
    }

  return false;
}

/* Check the parallelism clauses inside a kernels regions.
   Until kernels handling moves to use the same loop indirection
   scheme as parallel, we need to do this checking early.  */

static unsigned
check_oacc_kernel_gwv (gomp_for *stmt, omp_context *ctx)
{
  bool checking = true;
  unsigned outer_mask = 0;
  unsigned this_mask = 0;
  bool has_seq = false, has_auto = false;

  if (ctx->outer)
    outer_mask = check_oacc_kernel_gwv (NULL,  ctx->outer);
  if (!stmt)
    {
      checking = false;
      if (gimple_code (ctx->stmt) != GIMPLE_OMP_FOR)
	return outer_mask;
      stmt = as_a <gomp_for *> (ctx->stmt);
    }

  for (tree c = gimple_omp_for_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_GANG:
	  this_mask |= GOMP_DIM_MASK (GOMP_DIM_GANG);
	  break;
	case OMP_CLAUSE_WORKER:
	  this_mask |= GOMP_DIM_MASK (GOMP_DIM_WORKER);
	  break;
	case OMP_CLAUSE_VECTOR:
	  this_mask |= GOMP_DIM_MASK (GOMP_DIM_VECTOR);
	  break;
	case OMP_CLAUSE_SEQ:
	  has_seq = true;
	  break;
	case OMP_CLAUSE_AUTO:
	  has_auto = true;
	  break;
	default:
	  break;
	}
    }

  if (checking)
    {
      if (has_seq && (this_mask || has_auto))
	error_at (gimple_location (stmt), "%<seq%> overrides other"
		  " OpenACC loop specifiers");
      else if (has_auto && this_mask)
	error_at (gimple_location (stmt), "%<auto%> conflicts with other"
		  " OpenACC loop specifiers");

      if (this_mask & outer_mask)
	error_at (gimple_location (stmt), "inner loop uses same"
		  " OpenACC parallelism as containing loop");
    }

  return outer_mask | this_mask;
}

/* Scan a GIMPLE_OMP_FOR.  */

static omp_context *
scan_omp_for (gomp_for *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;
  size_t i;
  tree clauses = gimple_omp_for_clauses (stmt);

  ctx = new_omp_context (stmt, outer_ctx);

  if (is_gimple_omp_oacc (stmt))
    {
      omp_context *tgt = enclosing_target_ctx (outer_ctx);

      if (!tgt || is_oacc_parallel (tgt))
	for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	  {
	    char const *check = NULL;

	    switch (OMP_CLAUSE_CODE (c))
	      {
	      case OMP_CLAUSE_GANG:
		check = "gang";
		break;

	      case OMP_CLAUSE_WORKER:
		check = "worker";
		break;

	      case OMP_CLAUSE_VECTOR:
		check = "vector";
		break;

	      default:
		break;
	      }

	    if (check && OMP_CLAUSE_OPERAND (c, 0))
	      error_at (gimple_location (stmt),
			"argument not permitted on %qs clause in"
			" OpenACC %<parallel%>", check);
	  }

      if (tgt && is_oacc_kernels (tgt))
	{
	  /* Strip out reductions, as they are not  handled yet.  */
	  tree *prev_ptr = &clauses;

	  while (tree probe = *prev_ptr)
	    {
	      tree *next_ptr = &OMP_CLAUSE_CHAIN (probe);
	      
	      if (OMP_CLAUSE_CODE (probe) == OMP_CLAUSE_REDUCTION)
		*prev_ptr = *next_ptr;
	      else
		prev_ptr = next_ptr;
	    }

	  gimple_omp_for_set_clauses (stmt, clauses);
	  check_oacc_kernel_gwv (stmt, ctx);
	}
    }

  scan_sharing_clauses (clauses, ctx);

  scan_omp (gimple_omp_for_pre_body_ptr (stmt), ctx);
  for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
    {
      scan_omp_op (gimple_omp_for_index_ptr (stmt, i), ctx);
      scan_omp_op (gimple_omp_for_initial_ptr (stmt, i), ctx);
      scan_omp_op (gimple_omp_for_final_ptr (stmt, i), ctx);
      scan_omp_op (gimple_omp_for_incr_ptr (stmt, i), ctx);
    }
  scan_omp (gimple_omp_body_ptr (stmt), ctx);
  return ctx;
}

/* Duplicate #pragma omp simd, one for SIMT, another one for SIMD.  */

static void
scan_omp_simd (gimple_stmt_iterator *gsi, gomp_for *stmt,
	       omp_context *outer_ctx)
{
  gbind *bind = gimple_build_bind (NULL, NULL, NULL);
  gsi_replace (gsi, bind, false);
  gimple_seq seq = NULL;
  gimple *g = gimple_build_call_internal (IFN_GOMP_USE_SIMT, 0);
  tree cond = create_tmp_var_raw (integer_type_node);
  DECL_CONTEXT (cond) = current_function_decl;
  DECL_SEEN_IN_BIND_EXPR_P (cond) = 1;
  gimple_bind_set_vars (bind, cond);
  gimple_call_set_lhs (g, cond);
  gimple_seq_add_stmt (&seq, g);
  tree lab1 = create_artificial_label (UNKNOWN_LOCATION);
  tree lab2 = create_artificial_label (UNKNOWN_LOCATION);
  tree lab3 = create_artificial_label (UNKNOWN_LOCATION);
  g = gimple_build_cond (NE_EXPR, cond, integer_zero_node, lab1, lab2);
  gimple_seq_add_stmt (&seq, g);
  g = gimple_build_label (lab1);
  gimple_seq_add_stmt (&seq, g);
  gimple_seq new_seq = copy_gimple_seq_and_replace_locals (stmt);
  gomp_for *new_stmt = as_a <gomp_for *> (new_seq);
  tree clause = build_omp_clause (gimple_location (stmt), OMP_CLAUSE__SIMT_);
  OMP_CLAUSE_CHAIN (clause) = gimple_omp_for_clauses (new_stmt);
  gimple_omp_for_set_clauses (new_stmt, clause);
  gimple_seq_add_stmt (&seq, new_stmt);
  g = gimple_build_goto (lab3);
  gimple_seq_add_stmt (&seq, g);
  g = gimple_build_label (lab2);
  gimple_seq_add_stmt (&seq, g);
  gimple_seq_add_stmt (&seq, stmt);
  g = gimple_build_label (lab3);
  gimple_seq_add_stmt (&seq, g);
  gimple_bind_set_body (bind, seq);
  update_stmt (bind);
  scan_omp_for (new_stmt, outer_ctx);
  scan_omp_for (stmt, outer_ctx)->simt_stmt = new_stmt;
}

/* Scan an OpenMP sections directive.  */

static void
scan_omp_sections (gomp_sections *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;

  ctx = new_omp_context (stmt, outer_ctx);
  scan_sharing_clauses (gimple_omp_sections_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);
}

/* Scan an OpenMP single directive.  */

static void
scan_omp_single (gomp_single *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name;

  ctx = new_omp_context (stmt, outer_ctx);
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_copy_s");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  TYPE_NAME (ctx->record_type) = name;

  scan_sharing_clauses (gimple_omp_single_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    ctx->record_type = NULL;
  else
    layout_type (ctx->record_type);
}

/* Return true if the CLAUSES of an omp target guarantee that the base pointers
   used in the corresponding offloaded function are restrict.  */

static bool
omp_target_base_pointers_restrict_p (tree clauses)
{
  /* The analysis relies on the GOMP_MAP_FORCE_* mapping kinds, which are only
     used by OpenACC.  */
  if (flag_openacc == 0)
    return false;

  /* I.  Basic example:

       void foo (void)
       {
	 unsigned int a[2], b[2];

	 #pragma acc kernels \
	   copyout (a) \
	   copyout (b)
	 {
	   a[0] = 0;
	   b[0] = 1;
	 }
       }

     After gimplification, we have:

       #pragma omp target oacc_kernels \
	 map(force_from:a [len: 8]) \
	 map(force_from:b [len: 8])
       {
	 a[0] = 0;
	 b[0] = 1;
       }

     Because both mappings have the force prefix, we know that they will be
     allocated when calling the corresponding offloaded function, which means we
     can mark the base pointers for a and b in the offloaded function as
     restrict.  */

  tree c;
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
	return false;

      switch (OMP_CLAUSE_MAP_KIND (c))
	{
	case GOMP_MAP_FORCE_ALLOC:
	case GOMP_MAP_FORCE_TO:
	case GOMP_MAP_FORCE_FROM:
	case GOMP_MAP_FORCE_TOFROM:
	  break;
	default:
	  return false;
	}
    }

  return true;
}

/* Scan a GIMPLE_OMP_TARGET.  */

static void
scan_omp_target (gomp_target *stmt, omp_context *outer_ctx)
{
  omp_context *ctx;
  tree name;
  bool offloaded = is_gimple_omp_offloaded (stmt);
  tree clauses = gimple_omp_target_clauses (stmt);

  ctx = new_omp_context (stmt, outer_ctx);
  ctx->field_map = splay_tree_new (splay_tree_compare_pointers, 0, 0);
  ctx->record_type = lang_hooks.types.make_type (RECORD_TYPE);
  name = create_tmp_var_name (".omp_data_t");
  name = build_decl (gimple_location (stmt),
		     TYPE_DECL, name, ctx->record_type);
  DECL_ARTIFICIAL (name) = 1;
  DECL_NAMELESS (name) = 1;
  TYPE_NAME (ctx->record_type) = name;
  TYPE_ARTIFICIAL (ctx->record_type) = 1;

  bool base_pointers_restrict = false;
  if (offloaded)
    {
      create_omp_child_function (ctx, false);
      gimple_omp_target_set_child_fn (stmt, ctx->cb.dst_fn);

      base_pointers_restrict = omp_target_base_pointers_restrict_p (clauses);
      if (base_pointers_restrict
	  && dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Base pointers in offloaded function are restrict\n");
    }

  scan_sharing_clauses (clauses, ctx, base_pointers_restrict);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);

  if (TYPE_FIELDS (ctx->record_type) == NULL)
    ctx->record_type = ctx->receiver_decl = NULL;
  else
    {
      TYPE_FIELDS (ctx->record_type)
	= nreverse (TYPE_FIELDS (ctx->record_type));
      if (flag_checking)
	{
	  unsigned int align = DECL_ALIGN (TYPE_FIELDS (ctx->record_type));
	  for (tree field = TYPE_FIELDS (ctx->record_type);
	       field;
	       field = DECL_CHAIN (field))
	    gcc_assert (DECL_ALIGN (field) == align);
	}
      layout_type (ctx->record_type);
      if (offloaded)
	fixup_child_record_type (ctx);
    }
}

/* Scan an OpenMP teams directive.  */

static void
scan_omp_teams (gomp_teams *stmt, omp_context *outer_ctx)
{
  omp_context *ctx = new_omp_context (stmt, outer_ctx);
  scan_sharing_clauses (gimple_omp_teams_clauses (stmt), ctx);
  scan_omp (gimple_omp_body_ptr (stmt), ctx);
}

/* Check nesting restrictions.  */
static bool
check_omp_nesting_restrictions (gimple *stmt, omp_context *ctx)
{
  tree c;

  if (ctx && gimple_code (ctx->stmt) == GIMPLE_OMP_GRID_BODY)
    /* GRID_BODY is an artificial construct, nesting rules will be checked in
       the original copy of its contents.  */
    return true;

  /* No nesting of non-OpenACC STMT (that is, an OpenMP one, or a GOMP builtin)
     inside an OpenACC CTX.  */
  if (!(is_gimple_omp (stmt)
	&& is_gimple_omp_oacc (stmt))
      /* Except for atomic codes that we share with OpenMP.  */
      && !(gimple_code (stmt) == GIMPLE_OMP_ATOMIC_LOAD
	   || gimple_code (stmt) == GIMPLE_OMP_ATOMIC_STORE))
    {
      if (oacc_get_fn_attrib (cfun->decl) != NULL)
	{
	  error_at (gimple_location (stmt),
		    "non-OpenACC construct inside of OpenACC routine");
	  return false;
	}
      else
	for (omp_context *octx = ctx; octx != NULL; octx = octx->outer)
	  if (is_gimple_omp (octx->stmt)
	      && is_gimple_omp_oacc (octx->stmt))
	    {
	      error_at (gimple_location (stmt),
			"non-OpenACC construct inside of OpenACC region");
	      return false;
	    }
    }

  if (ctx != NULL)
    {
      if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	  && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
	{
	  c = NULL_TREE;
	  if (gimple_code (stmt) == GIMPLE_OMP_ORDERED)
	    {
	      c = gimple_omp_ordered_clauses (as_a <gomp_ordered *> (stmt));
	      if (omp_find_clause (c, OMP_CLAUSE_SIMD))
		{
		  if (omp_find_clause (c, OMP_CLAUSE_THREADS)
		      && (ctx->outer == NULL
			  || !gimple_omp_for_combined_into_p (ctx->stmt)
			  || gimple_code (ctx->outer->stmt) != GIMPLE_OMP_FOR
			  || (gimple_omp_for_kind (ctx->outer->stmt)
			      != GF_OMP_FOR_KIND_FOR)
			  || !gimple_omp_for_combined_p (ctx->outer->stmt)))
		    {
		      error_at (gimple_location (stmt),
				"%<ordered simd threads%> must be closely "
				"nested inside of %<for simd%> region");
		      return false;
		    }
		  return true;
		}
	    }
	  error_at (gimple_location (stmt),
		    "OpenMP constructs other than %<#pragma omp ordered simd%>"
		    " may not be nested inside %<simd%> region");
	  return false;
	}
      else if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
	{
	  if ((gimple_code (stmt) != GIMPLE_OMP_FOR
	       || ((gimple_omp_for_kind (stmt) != GF_OMP_FOR_KIND_DISTRIBUTE)
		   && (gimple_omp_for_kind (stmt) != GF_OMP_FOR_KIND_GRID_LOOP)))
	      && gimple_code (stmt) != GIMPLE_OMP_PARALLEL)
	    {
	      error_at (gimple_location (stmt),
			"only %<distribute%> or %<parallel%> regions are "
			"allowed to be strictly nested inside %<teams%> "
			"region");
	      return false;
	    }
	}
    }
  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_FOR:
      if (gimple_omp_for_kind (stmt) & GF_OMP_FOR_SIMD)
	return true;
      if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_DISTRIBUTE)
	{
	  if (ctx != NULL && gimple_code (ctx->stmt) != GIMPLE_OMP_TEAMS)
	    {
	      error_at (gimple_location (stmt),
			"%<distribute%> region must be strictly nested "
			"inside %<teams%> construct");
	      return false;
	    }
	  return true;
	}
      /* We split taskloop into task and nested taskloop in it.  */
      if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_TASKLOOP)
	return true;
      if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_OACC_LOOP)
	{
	  bool ok = false;

	  if (ctx)
	    switch (gimple_code (ctx->stmt))
	      {
	      case GIMPLE_OMP_FOR:
		ok = (gimple_omp_for_kind (ctx->stmt)
		      == GF_OMP_FOR_KIND_OACC_LOOP);
		break;

	      case GIMPLE_OMP_TARGET:
		switch (gimple_omp_target_kind (ctx->stmt))
		  {
		  case GF_OMP_TARGET_KIND_OACC_PARALLEL:
		  case GF_OMP_TARGET_KIND_OACC_KERNELS:
		    ok = true;
		    break;

		  default:
		    break;
		  }

	      default:
		break;
	      }
	  else if (oacc_get_fn_attrib (current_function_decl))
	    ok = true;
	  if (!ok)
	    {
	      error_at (gimple_location (stmt),
			"OpenACC loop directive must be associated with"
			" an OpenACC compute region");
	      return false;
	    }
	}
      /* FALLTHRU */
    case GIMPLE_CALL:
      if (is_gimple_call (stmt)
	  && (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
	      == BUILT_IN_GOMP_CANCEL
	      || DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		 == BUILT_IN_GOMP_CANCELLATION_POINT))
	{
	  const char *bad = NULL;
	  const char *kind = NULL;
	  const char *construct
	    = (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
	       == BUILT_IN_GOMP_CANCEL)
	      ? "#pragma omp cancel"
	      : "#pragma omp cancellation point";
	  if (ctx == NULL)
	    {
	      error_at (gimple_location (stmt), "orphaned %qs construct",
			construct);
	      return false;
	    }
	  switch (tree_fits_shwi_p (gimple_call_arg (stmt, 0))
		  ? tree_to_shwi (gimple_call_arg (stmt, 0))
		  : 0)
	    {
	    case 1:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_PARALLEL)
		bad = "#pragma omp parallel";
	      else if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		       == BUILT_IN_GOMP_CANCEL
		       && !integer_zerop (gimple_call_arg (stmt, 1)))
		ctx->cancellable = true;
	      kind = "parallel";
	      break;
	    case 2:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_FOR
		  || gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_FOR)
		bad = "#pragma omp for";
	      else if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		       == BUILT_IN_GOMP_CANCEL
		       && !integer_zerop (gimple_call_arg (stmt, 1)))
		{
		  ctx->cancellable = true;
		  if (omp_find_clause (gimple_omp_for_clauses (ctx->stmt),
				       OMP_CLAUSE_NOWAIT))
		    warning_at (gimple_location (stmt), 0,
				"%<#pragma omp cancel for%> inside "
				"%<nowait%> for construct");
		  if (omp_find_clause (gimple_omp_for_clauses (ctx->stmt),
				       OMP_CLAUSE_ORDERED))
		    warning_at (gimple_location (stmt), 0,
				"%<#pragma omp cancel for%> inside "
				"%<ordered%> for construct");
		}
	      kind = "for";
	      break;
	    case 4:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_SECTIONS
		  && gimple_code (ctx->stmt) != GIMPLE_OMP_SECTION)
		bad = "#pragma omp sections";
	      else if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		       == BUILT_IN_GOMP_CANCEL
		       && !integer_zerop (gimple_call_arg (stmt, 1)))
		{
		  if (gimple_code (ctx->stmt) == GIMPLE_OMP_SECTIONS)
		    {
		      ctx->cancellable = true;
		      if (omp_find_clause (gimple_omp_sections_clauses
								(ctx->stmt),
					   OMP_CLAUSE_NOWAIT))
			warning_at (gimple_location (stmt), 0,
				    "%<#pragma omp cancel sections%> inside "
				    "%<nowait%> sections construct");
		    }
		  else
		    {
		      gcc_assert (ctx->outer
				  && gimple_code (ctx->outer->stmt)
				     == GIMPLE_OMP_SECTIONS);
		      ctx->outer->cancellable = true;
		      if (omp_find_clause (gimple_omp_sections_clauses
							(ctx->outer->stmt),
					   OMP_CLAUSE_NOWAIT))
			warning_at (gimple_location (stmt), 0,
				    "%<#pragma omp cancel sections%> inside "
				    "%<nowait%> sections construct");
		    }
		}
	      kind = "sections";
	      break;
	    case 8:
	      if (gimple_code (ctx->stmt) != GIMPLE_OMP_TASK)
		bad = "#pragma omp task";
	      else
		{
		  for (omp_context *octx = ctx->outer;
		       octx; octx = octx->outer)
		    {
		      switch (gimple_code (octx->stmt))
			{
			case GIMPLE_OMP_TASKGROUP:
			  break;
			case GIMPLE_OMP_TARGET:
			  if (gimple_omp_target_kind (octx->stmt)
			      != GF_OMP_TARGET_KIND_REGION)
			    continue;
			  /* FALLTHRU */
			case GIMPLE_OMP_PARALLEL:
			case GIMPLE_OMP_TEAMS:
			  error_at (gimple_location (stmt),
				    "%<%s taskgroup%> construct not closely "
				    "nested inside of %<taskgroup%> region",
				    construct);
			  return false;
			default:
			  continue;
			}
		      break;
		    }
		  ctx->cancellable = true;
		}
	      kind = "taskgroup";
	      break;
	    default:
	      error_at (gimple_location (stmt), "invalid arguments");
	      return false;
	    }
	  if (bad)
	    {
	      error_at (gimple_location (stmt),
			"%<%s %s%> construct not closely nested inside of %qs",
			construct, kind, bad);
	      return false;
	    }
	}
      /* FALLTHRU */
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
      for (; ctx != NULL; ctx = ctx->outer)
	switch (gimple_code (ctx->stmt))
	  {
	  case GIMPLE_OMP_FOR:
	    if (gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_FOR
		&& gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_TASKLOOP)
	      break;
	    /* FALLTHRU */
	  case GIMPLE_OMP_SECTIONS:
	  case GIMPLE_OMP_SINGLE:
	  case GIMPLE_OMP_ORDERED:
	  case GIMPLE_OMP_MASTER:
	  case GIMPLE_OMP_TASK:
	  case GIMPLE_OMP_CRITICAL:
	    if (is_gimple_call (stmt))
	      {
		if (DECL_FUNCTION_CODE (gimple_call_fndecl (stmt))
		    != BUILT_IN_GOMP_BARRIER)
		  return true;
		error_at (gimple_location (stmt),
			  "barrier region may not be closely nested inside "
			  "of work-sharing, %<critical%>, %<ordered%>, "
			  "%<master%>, explicit %<task%> or %<taskloop%> "
			  "region");
		return false;
	      }
	    error_at (gimple_location (stmt),
		      "work-sharing region may not be closely nested inside "
		      "of work-sharing, %<critical%>, %<ordered%>, "
		      "%<master%>, explicit %<task%> or %<taskloop%> region");
	    return false;
	  case GIMPLE_OMP_PARALLEL:
	  case GIMPLE_OMP_TEAMS:
	    return true;
	  case GIMPLE_OMP_TARGET:
	    if (gimple_omp_target_kind (ctx->stmt)
		== GF_OMP_TARGET_KIND_REGION)
	      return true;
	    break;
	  default:
	    break;
	  }
      break;
    case GIMPLE_OMP_MASTER:
      for (; ctx != NULL; ctx = ctx->outer)
	switch (gimple_code (ctx->stmt))
	  {
	  case GIMPLE_OMP_FOR:
	    if (gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_FOR
		&& gimple_omp_for_kind (ctx->stmt) != GF_OMP_FOR_KIND_TASKLOOP)
	      break;
	    /* FALLTHRU */
	  case GIMPLE_OMP_SECTIONS:
	  case GIMPLE_OMP_SINGLE:
	  case GIMPLE_OMP_TASK:
	    error_at (gimple_location (stmt),
		      "%<master%> region may not be closely nested inside "
		      "of work-sharing, explicit %<task%> or %<taskloop%> "
		      "region");
	    return false;
	  case GIMPLE_OMP_PARALLEL:
	  case GIMPLE_OMP_TEAMS:
	    return true;
	  case GIMPLE_OMP_TARGET:
	    if (gimple_omp_target_kind (ctx->stmt)
		== GF_OMP_TARGET_KIND_REGION)
	      return true;
	    break;
	  default:
	    break;
	  }
      break;
    case GIMPLE_OMP_TASK:
      for (c = gimple_omp_task_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE
		|| OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK))
	  {
	    enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_KIND (c);
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<depend(%s)%> is only allowed in %<omp ordered%>",
		      kind == OMP_CLAUSE_DEPEND_SOURCE ? "source" : "sink");
	    return false;
	  }
      break;
    case GIMPLE_OMP_ORDERED:
      for (c = gimple_omp_ordered_clauses (as_a <gomp_ordered *> (stmt));
	   c; c = OMP_CLAUSE_CHAIN (c))
	{
	  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND)
	    {
	      gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_THREADS
			  || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SIMD);
	      continue;
	    }
	  enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_KIND (c);
	  if (kind == OMP_CLAUSE_DEPEND_SOURCE
	      || kind == OMP_CLAUSE_DEPEND_SINK)
	    {
	      tree oclause;
	      /* Look for containing ordered(N) loop.  */
	      if (ctx == NULL
		  || gimple_code (ctx->stmt) != GIMPLE_OMP_FOR
		  || (oclause
			= omp_find_clause (gimple_omp_for_clauses (ctx->stmt),
					   OMP_CLAUSE_ORDERED)) == NULL_TREE)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<ordered%> construct with %<depend%> clause "
			    "must be closely nested inside an %<ordered%> "
			    "loop");
		  return false;
		}
	      else if (OMP_CLAUSE_ORDERED_EXPR (oclause) == NULL_TREE)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<ordered%> construct with %<depend%> clause "
			    "must be closely nested inside a loop with "
			    "%<ordered%> clause with a parameter");
		  return false;
		}
	    }
	  else
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"invalid depend kind in omp %<ordered%> %<depend%>");
	      return false;
	    }
	}
      c = gimple_omp_ordered_clauses (as_a <gomp_ordered *> (stmt));
      if (omp_find_clause (c, OMP_CLAUSE_SIMD))
	{
	  /* ordered simd must be closely nested inside of simd region,
	     and simd region must not encounter constructs other than
	     ordered simd, therefore ordered simd may be either orphaned,
	     or ctx->stmt must be simd.  The latter case is handled already
	     earlier.  */
	  if (ctx != NULL)
	    {
	      error_at (gimple_location (stmt),
			"%<ordered%> %<simd%> must be closely nested inside "
			"%<simd%> region");
	      return false;
	    }
	}
      for (; ctx != NULL; ctx = ctx->outer)
	switch (gimple_code (ctx->stmt))
	  {
	  case GIMPLE_OMP_CRITICAL:
	  case GIMPLE_OMP_TASK:
	  case GIMPLE_OMP_ORDERED:
	  ordered_in_taskloop:
	    error_at (gimple_location (stmt),
		      "%<ordered%> region may not be closely nested inside "
		      "of %<critical%>, %<ordered%>, explicit %<task%> or "
		      "%<taskloop%> region");
	    return false;
	  case GIMPLE_OMP_FOR:
	    if (gimple_omp_for_kind (ctx->stmt) == GF_OMP_FOR_KIND_TASKLOOP)
	      goto ordered_in_taskloop;
	    if (omp_find_clause (gimple_omp_for_clauses (ctx->stmt),
				 OMP_CLAUSE_ORDERED) == NULL)
	      {
		error_at (gimple_location (stmt),
			  "%<ordered%> region must be closely nested inside "
			  "a loop region with an %<ordered%> clause");
		return false;
	      }
	    return true;
	  case GIMPLE_OMP_TARGET:
	    if (gimple_omp_target_kind (ctx->stmt)
		!= GF_OMP_TARGET_KIND_REGION)
	      break;
	    /* FALLTHRU */
	  case GIMPLE_OMP_PARALLEL:
	  case GIMPLE_OMP_TEAMS:
	    error_at (gimple_location (stmt),
		      "%<ordered%> region must be closely nested inside "
		      "a loop region with an %<ordered%> clause");
	    return false;
	  default:
	    break;
	  }
      break;
    case GIMPLE_OMP_CRITICAL:
      {
	tree this_stmt_name
	  = gimple_omp_critical_name (as_a <gomp_critical *> (stmt));
	for (; ctx != NULL; ctx = ctx->outer)
	  if (gomp_critical *other_crit
	        = dyn_cast <gomp_critical *> (ctx->stmt))
	    if (this_stmt_name == gimple_omp_critical_name (other_crit))
	      {
		error_at (gimple_location (stmt),
			  "%<critical%> region may not be nested inside "
			   "a %<critical%> region with the same name");
		return false;
	      }
      }
      break;
    case GIMPLE_OMP_TEAMS:
      if (ctx == NULL
	  || gimple_code (ctx->stmt) != GIMPLE_OMP_TARGET
	  || gimple_omp_target_kind (ctx->stmt) != GF_OMP_TARGET_KIND_REGION)
	{
	  error_at (gimple_location (stmt),
		    "%<teams%> construct not closely nested inside of "
		    "%<target%> construct");
	  return false;
	}
      break;
    case GIMPLE_OMP_TARGET:
      for (c = gimple_omp_target_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && (OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SOURCE
		|| OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK))
	  {
	    enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_KIND (c);
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<depend(%s)%> is only allowed in %<omp ordered%>",
		      kind == OMP_CLAUSE_DEPEND_SOURCE ? "source" : "sink");
	    return false;
	  }
      if (is_gimple_omp_offloaded (stmt)
	  && oacc_get_fn_attrib (cfun->decl) != NULL)
	{
	  error_at (gimple_location (stmt),
		    "OpenACC region inside of OpenACC routine, nested "
		    "parallelism not supported yet");
	  return false;
	}
      for (; ctx != NULL; ctx = ctx->outer)
	{
	  if (gimple_code (ctx->stmt) != GIMPLE_OMP_TARGET)
	    {
	      if (is_gimple_omp (stmt)
		  && is_gimple_omp_oacc (stmt)
		  && is_gimple_omp (ctx->stmt))
		{
		  error_at (gimple_location (stmt),
			    "OpenACC construct inside of non-OpenACC region");
		  return false;
		}
	      continue;
	    }

	  const char *stmt_name, *ctx_stmt_name;
	  switch (gimple_omp_target_kind (stmt))
	    {
	    case GF_OMP_TARGET_KIND_REGION: stmt_name = "target"; break;
	    case GF_OMP_TARGET_KIND_DATA: stmt_name = "target data"; break;
	    case GF_OMP_TARGET_KIND_UPDATE: stmt_name = "target update"; break;
	    case GF_OMP_TARGET_KIND_ENTER_DATA:
	      stmt_name = "target enter data"; break;
	    case GF_OMP_TARGET_KIND_EXIT_DATA:
	      stmt_name = "target exit data"; break;
	    case GF_OMP_TARGET_KIND_OACC_PARALLEL: stmt_name = "parallel"; break;
	    case GF_OMP_TARGET_KIND_OACC_KERNELS: stmt_name = "kernels"; break;
	    case GF_OMP_TARGET_KIND_OACC_DATA: stmt_name = "data"; break;
	    case GF_OMP_TARGET_KIND_OACC_UPDATE: stmt_name = "update"; break;
	    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
	      stmt_name = "enter/exit data"; break;
	    case GF_OMP_TARGET_KIND_OACC_HOST_DATA: stmt_name = "host_data";
	      break;
	    default: gcc_unreachable ();
	    }
	  switch (gimple_omp_target_kind (ctx->stmt))
	    {
	    case GF_OMP_TARGET_KIND_REGION: ctx_stmt_name = "target"; break;
	    case GF_OMP_TARGET_KIND_DATA: ctx_stmt_name = "target data"; break;
	    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
	      ctx_stmt_name = "parallel"; break;
	    case GF_OMP_TARGET_KIND_OACC_KERNELS:
	      ctx_stmt_name = "kernels"; break;
	    case GF_OMP_TARGET_KIND_OACC_DATA: ctx_stmt_name = "data"; break;
	    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
	      ctx_stmt_name = "host_data"; break;
	    default: gcc_unreachable ();
	    }

	  /* OpenACC/OpenMP mismatch?  */
	  if (is_gimple_omp_oacc (stmt)
	      != is_gimple_omp_oacc (ctx->stmt))
	    {
	      error_at (gimple_location (stmt),
			"%s %qs construct inside of %s %qs region",
			(is_gimple_omp_oacc (stmt)
			 ? "OpenACC" : "OpenMP"), stmt_name,
			(is_gimple_omp_oacc (ctx->stmt)
			 ? "OpenACC" : "OpenMP"), ctx_stmt_name);
	      return false;
	    }
	  if (is_gimple_omp_offloaded (ctx->stmt))
	    {
	      /* No GIMPLE_OMP_TARGET inside offloaded OpenACC CTX.  */
	      if (is_gimple_omp_oacc (ctx->stmt))
		{
		  error_at (gimple_location (stmt),
			    "%qs construct inside of %qs region",
			    stmt_name, ctx_stmt_name);
		  return false;
		}
	      else
		{
		  warning_at (gimple_location (stmt), 0,
			      "%qs construct inside of %qs region",
			      stmt_name, ctx_stmt_name);
		}
	    }
	}
      break;
    default:
      break;
    }
  return true;
}


/* Helper function scan_omp.

   Callback for walk_tree or operators in walk_gimple_stmt used to
   scan for OMP directives in TP.  */

static tree
scan_omp_1_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  omp_context *ctx = (omp_context *) wi->info;
  tree t = *tp;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case LABEL_DECL:
    case RESULT_DECL:
      if (ctx)
	{
	  tree repl = remap_decl (t, &ctx->cb);
	  gcc_checking_assert (TREE_CODE (repl) != ERROR_MARK);
	  *tp = repl;
	}
      break;

    default:
      if (ctx && TYPE_P (t))
	*tp = remap_type (t, &ctx->cb);
      else if (!DECL_P (t))
	{
	  *walk_subtrees = 1;
	  if (ctx)
	    {
	      tree tem = remap_type (TREE_TYPE (t), &ctx->cb);
	      if (tem != TREE_TYPE (t))
		{
		  if (TREE_CODE (t) == INTEGER_CST)
		    *tp = wide_int_to_tree (tem, t);
		  else
		    TREE_TYPE (t) = tem;
		}
	    }
	}
      break;
    }

  return NULL_TREE;
}

/* Return true if FNDECL is a setjmp or a longjmp.  */

static bool
setjmp_or_longjmp_p (const_tree fndecl)
{
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_SETJMP
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_LONGJMP))
    return true;

  tree declname = DECL_NAME (fndecl);
  if (!declname)
    return false;
  const char *name = IDENTIFIER_POINTER (declname);
  return !strcmp (name, "setjmp") || !strcmp (name, "longjmp");
}


/* Helper function for scan_omp.

   Callback for walk_gimple_stmt used to scan for OMP directives in
   the current statement in GSI.  */

static tree
scan_omp_1_stmt (gimple_stmt_iterator *gsi, bool *handled_ops_p,
		 struct walk_stmt_info *wi)
{
  gimple *stmt = gsi_stmt (*gsi);
  omp_context *ctx = (omp_context *) wi->info;

  if (gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  /* Check the nesting restrictions.  */
  bool remove = false;
  if (is_gimple_omp (stmt))
    remove = !check_omp_nesting_restrictions (stmt, ctx);
  else if (is_gimple_call (stmt))
    {
      tree fndecl = gimple_call_fndecl (stmt);
      if (fndecl)
	{
	  if (setjmp_or_longjmp_p (fndecl)
	      && ctx
	      && gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
	      && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
	    {
	      remove = true;
	      error_at (gimple_location (stmt),
			"setjmp/longjmp inside simd construct");
	    }
	  else if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	    switch (DECL_FUNCTION_CODE (fndecl))
	      {
	      case BUILT_IN_GOMP_BARRIER:
	      case BUILT_IN_GOMP_CANCEL:
	      case BUILT_IN_GOMP_CANCELLATION_POINT:
	      case BUILT_IN_GOMP_TASKYIELD:
	      case BUILT_IN_GOMP_TASKWAIT:
	      case BUILT_IN_GOMP_TASKGROUP_START:
	      case BUILT_IN_GOMP_TASKGROUP_END:
		remove = !check_omp_nesting_restrictions (stmt, ctx);
		break;
	      default:
		break;
	      }
	}
    }
  if (remove)
    {
      stmt = gimple_build_nop ();
      gsi_replace (gsi, stmt, false);
    }

  *handled_ops_p = true;

  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_PARALLEL:
      taskreg_nesting_level++;
      scan_omp_parallel (gsi, ctx);
      taskreg_nesting_level--;
      break;

    case GIMPLE_OMP_TASK:
      taskreg_nesting_level++;
      scan_omp_task (gsi, ctx);
      taskreg_nesting_level--;
      break;

    case GIMPLE_OMP_FOR:
      if (((gimple_omp_for_kind (as_a <gomp_for *> (stmt))
	    & GF_OMP_FOR_KIND_MASK) == GF_OMP_FOR_KIND_SIMD)
	  && omp_maybe_offloaded_ctx (ctx)
	  && omp_max_simt_vf ())
	scan_omp_simd (gsi, as_a <gomp_for *> (stmt), ctx);
      else
	scan_omp_for (as_a <gomp_for *> (stmt), ctx);
      break;

    case GIMPLE_OMP_SECTIONS:
      scan_omp_sections (as_a <gomp_sections *> (stmt), ctx);
      break;

    case GIMPLE_OMP_SINGLE:
      scan_omp_single (as_a <gomp_single *> (stmt), ctx);
      break;

    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_GRID_BODY:
      ctx = new_omp_context (stmt, ctx);
      scan_omp (gimple_omp_body_ptr (stmt), ctx);
      break;

    case GIMPLE_OMP_TARGET:
      scan_omp_target (as_a <gomp_target *> (stmt), ctx);
      break;

    case GIMPLE_OMP_TEAMS:
      scan_omp_teams (as_a <gomp_teams *> (stmt), ctx);
      break;

    case GIMPLE_BIND:
      {
	tree var;

	*handled_ops_p = false;
	if (ctx)
	  for (var = gimple_bind_vars (as_a <gbind *> (stmt));
	       var ;
	       var = DECL_CHAIN (var))
	    insert_decl_map (&ctx->cb, var, var);
      }
      break;
    default:
      *handled_ops_p = false;
      break;
    }

  return NULL_TREE;
}


/* Scan all the statements starting at the current statement.  CTX
   contains context information about the OMP directives and
   clauses found during the scan.  */

static void
scan_omp (gimple_seq *body_p, omp_context *ctx)
{
  location_t saved_location;
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.info = ctx;
  wi.want_locations = true;

  saved_location = input_location;
  walk_gimple_seq_mod (body_p, scan_omp_1_stmt, scan_omp_1_op, &wi);
  input_location = saved_location;
}

/* Re-gimplification and code generation routines.  */

/* If a context was created for STMT when it was scanned, return it.  */

static omp_context *
maybe_lookup_ctx (gimple *stmt)
{
  splay_tree_node n;
  n = splay_tree_lookup (all_contexts, (splay_tree_key) stmt);
  return n ? (omp_context *) n->value : NULL;
}


/* Find the mapping for DECL in CTX or the immediately enclosing
   context that has a mapping for DECL.

   If CTX is a nested parallel directive, we may have to use the decl
   mappings created in CTX's parent context.  Suppose that we have the
   following parallel nesting (variable UIDs showed for clarity):

	iD.1562 = 0;
     	#omp parallel shared(iD.1562)		-> outer parallel
	  iD.1562 = iD.1562 + 1;

	  #omp parallel shared (iD.1562)	-> inner parallel
	     iD.1562 = iD.1562 - 1;

   Each parallel structure will create a distinct .omp_data_s structure
   for copying iD.1562 in/out of the directive:

  	outer parallel		.omp_data_s.1.i -> iD.1562
	inner parallel		.omp_data_s.2.i -> iD.1562

   A shared variable mapping will produce a copy-out operation before
   the parallel directive and a copy-in operation after it.  So, in
   this case we would have:

  	iD.1562 = 0;
	.omp_data_o.1.i = iD.1562;
	#omp parallel shared(iD.1562)		-> outer parallel
	  .omp_data_i.1 = &.omp_data_o.1
	  .omp_data_i.1->i = .omp_data_i.1->i + 1;

	  .omp_data_o.2.i = iD.1562;		-> **
	  #omp parallel shared(iD.1562)		-> inner parallel
	    .omp_data_i.2 = &.omp_data_o.2
	    .omp_data_i.2->i = .omp_data_i.2->i - 1;


    ** This is a problem.  The symbol iD.1562 cannot be referenced
       inside the body of the outer parallel region.  But since we are
       emitting this copy operation while expanding the inner parallel
       directive, we need to access the CTX structure of the outer
       parallel directive to get the correct mapping:

	  .omp_data_o.2.i = .omp_data_i.1->i

    Since there may be other workshare or parallel directives enclosing
    the parallel directive, it may be necessary to walk up the context
    parent chain.  This is not a problem in general because nested
    parallelism happens only rarely.  */

static tree
lookup_decl_in_outer_ctx (tree decl, omp_context *ctx)
{
  tree t;
  omp_context *up;

  for (up = ctx->outer, t = NULL; up && t == NULL; up = up->outer)
    t = maybe_lookup_decl (decl, up);

  gcc_assert (!ctx->is_nested || t || is_global_var (decl));

  return t ? t : decl;
}


/* Similar to lookup_decl_in_outer_ctx, but return DECL if not found
   in outer contexts.  */

static tree
maybe_lookup_decl_in_outer_ctx (tree decl, omp_context *ctx)
{
  tree t = NULL;
  omp_context *up;

  for (up = ctx->outer, t = NULL; up && t == NULL; up = up->outer)
    t = maybe_lookup_decl (decl, up);

  return t ? t : decl;
}


/* Construct the initialization value for reduction operation OP.  */

tree
omp_reduction_init_op (location_t loc, enum tree_code op, tree type)
{
  switch (op)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_XOR_EXPR:
    case NE_EXPR:
      return build_zero_cst (type);

    case MULT_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case EQ_EXPR:
      return fold_convert_loc (loc, type, integer_one_node);

    case BIT_AND_EXPR:
      return fold_convert_loc (loc, type, integer_minus_one_node);

    case MAX_EXPR:
      if (SCALAR_FLOAT_TYPE_P (type))
	{
	  REAL_VALUE_TYPE max, min;
	  if (HONOR_INFINITIES (type))
	    {
	      real_inf (&max);
	      real_arithmetic (&min, NEGATE_EXPR, &max, NULL);
	    }
	  else
	    real_maxval (&min, 1, TYPE_MODE (type));
	  return build_real (type, min);
	}
      else if (POINTER_TYPE_P (type))
	{
	  wide_int min
	    = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
	  return wide_int_to_tree (type, min);
	}
      else
	{
	  gcc_assert (INTEGRAL_TYPE_P (type));
	  return TYPE_MIN_VALUE (type);
	}

    case MIN_EXPR:
      if (SCALAR_FLOAT_TYPE_P (type))
	{
	  REAL_VALUE_TYPE max;
	  if (HONOR_INFINITIES (type))
	    real_inf (&max);
	  else
	    real_maxval (&max, 0, TYPE_MODE (type));
	  return build_real (type, max);
	}
      else if (POINTER_TYPE_P (type))
	{
	  wide_int max
	    = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
	  return wide_int_to_tree (type, max);
	}
      else
	{
	  gcc_assert (INTEGRAL_TYPE_P (type));
	  return TYPE_MAX_VALUE (type);
	}

    default:
      gcc_unreachable ();
    }
}

/* Construct the initialization value for reduction CLAUSE.  */

tree
omp_reduction_init (tree clause, tree type)
{
  return omp_reduction_init_op (OMP_CLAUSE_LOCATION (clause),
				OMP_CLAUSE_REDUCTION_CODE (clause), type);
}

/* Return alignment to be assumed for var in CLAUSE, which should be
   OMP_CLAUSE_ALIGNED.  */

static tree
omp_clause_aligned_alignment (tree clause)
{
  if (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause))
    return OMP_CLAUSE_ALIGNED_ALIGNMENT (clause);

  /* Otherwise return implementation defined alignment.  */
  unsigned int al = 1;
  opt_scalar_mode mode_iter;
  int vs = targetm.vectorize.autovectorize_vector_sizes ();
  if (vs)
    vs = 1 << floor_log2 (vs);
  static enum mode_class classes[]
    = { MODE_INT, MODE_VECTOR_INT, MODE_FLOAT, MODE_VECTOR_FLOAT };
  for (int i = 0; i < 4; i += 2)
    /* The for loop above dictates that we only walk through scalar classes.  */
    FOR_EACH_MODE_IN_CLASS (mode_iter, classes[i])
      {
	scalar_mode mode = mode_iter.require ();
	machine_mode vmode = targetm.vectorize.preferred_simd_mode (mode);
	if (GET_MODE_CLASS (vmode) != classes[i + 1])
	  continue;
	while (vs
	       && GET_MODE_SIZE (vmode) < vs
	       && GET_MODE_2XWIDER_MODE (vmode).exists ())
	  vmode = GET_MODE_2XWIDER_MODE (vmode).require ();

	tree type = lang_hooks.types.type_for_mode (mode, 1);
	if (type == NULL_TREE || TYPE_MODE (type) != mode)
	  continue;
	type = build_vector_type (type, GET_MODE_SIZE (vmode)
					/ GET_MODE_SIZE (mode));
	if (TYPE_MODE (type) != vmode)
	  continue;
	if (TYPE_ALIGN_UNIT (type) > al)
	  al = TYPE_ALIGN_UNIT (type);
      }
  return build_int_cst (integer_type_node, al);
}


/* This structure is part of the interface between lower_rec_simd_input_clauses
   and lower_rec_input_clauses.  */

struct omplow_simd_context {
  tree idx;
  tree lane;
  vec<tree, va_heap> simt_eargs;
  gimple_seq simt_dlist;
  int max_vf;
  bool is_simt;
};

/* Helper function of lower_rec_input_clauses, used for #pragma omp simd
   privatization.  */

static bool
lower_rec_simd_input_clauses (tree new_var, omp_context *ctx,
			      omplow_simd_context *sctx, tree &ivar, tree &lvar)
{
  if (sctx->max_vf == 0)
    {
      sctx->max_vf = sctx->is_simt ? omp_max_simt_vf () : omp_max_vf ();
      if (sctx->max_vf > 1)
	{
	  tree c = omp_find_clause (gimple_omp_for_clauses (ctx->stmt),
				    OMP_CLAUSE_SAFELEN);
	  if (c
	      && (TREE_CODE (OMP_CLAUSE_SAFELEN_EXPR (c)) != INTEGER_CST
		  || tree_int_cst_sgn (OMP_CLAUSE_SAFELEN_EXPR (c)) != 1))
	    sctx->max_vf = 1;
	  else if (c && compare_tree_int (OMP_CLAUSE_SAFELEN_EXPR (c),
					  sctx->max_vf) == -1)
	    sctx->max_vf = tree_to_shwi (OMP_CLAUSE_SAFELEN_EXPR (c));
	}
      if (sctx->max_vf > 1)
	{
	  sctx->idx = create_tmp_var (unsigned_type_node);
	  sctx->lane = create_tmp_var (unsigned_type_node);
	}
    }
  if (sctx->max_vf == 1)
    return false;

  if (sctx->is_simt)
    {
      if (is_gimple_reg (new_var))
	{
	  ivar = lvar = new_var;
	  return true;
	}
      tree type = TREE_TYPE (new_var), ptype = build_pointer_type (type);
      ivar = lvar = create_tmp_var (type);
      TREE_ADDRESSABLE (ivar) = 1;
      DECL_ATTRIBUTES (ivar) = tree_cons (get_identifier ("omp simt private"),
					  NULL, DECL_ATTRIBUTES (ivar));
      sctx->simt_eargs.safe_push (build1 (ADDR_EXPR, ptype, ivar));
      tree clobber = build_constructor (type, NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gimple *g = gimple_build_assign (ivar, clobber);
      gimple_seq_add_stmt (&sctx->simt_dlist, g);
    }
  else
    {
      tree atype = build_array_type_nelts (TREE_TYPE (new_var), sctx->max_vf);
      tree avar = create_tmp_var_raw (atype);
      if (TREE_ADDRESSABLE (new_var))
	TREE_ADDRESSABLE (avar) = 1;
      DECL_ATTRIBUTES (avar)
	= tree_cons (get_identifier ("omp simd array"), NULL,
		     DECL_ATTRIBUTES (avar));
      gimple_add_tmp_var (avar);
      ivar = build4 (ARRAY_REF, TREE_TYPE (new_var), avar, sctx->idx,
		     NULL_TREE, NULL_TREE);
      lvar = build4 (ARRAY_REF, TREE_TYPE (new_var), avar, sctx->lane,
		     NULL_TREE, NULL_TREE);
    }
  if (DECL_P (new_var))
    {
      SET_DECL_VALUE_EXPR (new_var, lvar);
      DECL_HAS_VALUE_EXPR_P (new_var) = 1;
    }
  return true;
}

/* Helper function of lower_rec_input_clauses.  For a reference
   in simd reduction, add an underlying variable it will reference.  */

static void
handle_simd_reference (location_t loc, tree new_vard, gimple_seq *ilist)
{
  tree z = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_vard)));
  if (TREE_CONSTANT (z))
    {
      z = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (new_vard)),
			      get_name (new_vard));
      gimple_add_tmp_var (z);
      TREE_ADDRESSABLE (z) = 1;
      z = build_fold_addr_expr_loc (loc, z);
      gimplify_assign (new_vard, z, ilist);
    }
}

/* Generate code to implement the input clauses, FIRSTPRIVATE and COPYIN,
   from the receiver (aka child) side and initializers for REFERENCE_TYPE
   private variables.  Initialization statements go in ILIST, while calls
   to destructors go in DLIST.  */

static void
lower_rec_input_clauses (tree clauses, gimple_seq *ilist, gimple_seq *dlist,
			 omp_context *ctx, struct omp_for_data *fd)
{
  tree c, dtor, copyin_seq, x, ptr;
  bool copyin_by_ref = false;
  bool lastprivate_firstprivate = false;
  bool reduction_omp_orig_ref = false;
  int pass;
  bool is_simd = (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
		  && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD);
  omplow_simd_context sctx = omplow_simd_context ();
  tree simt_lane = NULL_TREE, simtrec = NULL_TREE;
  tree ivar = NULL_TREE, lvar = NULL_TREE, uid = NULL_TREE;
  gimple_seq llist[3] = { };

  copyin_seq = NULL;
  sctx.is_simt = is_simd && omp_find_clause (clauses, OMP_CLAUSE__SIMT_);

  /* Set max_vf=1 (which will later enforce safelen=1) in simd loops
     with data sharing clauses referencing variable sized vars.  That
     is unnecessarily hard to support and very unlikely to result in
     vectorized code anyway.  */
  if (is_simd)
    for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_LINEAR:
	  if (OMP_CLAUSE_LINEAR_ARRAY (c))
	    sctx.max_vf = 1;
	  /* FALLTHRU */
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LASTPRIVATE:
	  if (is_variable_sized (OMP_CLAUSE_DECL (c)))
	    sctx.max_vf = 1;
	  break;
	case OMP_CLAUSE_REDUCTION:
	  if (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF
	      || is_variable_sized (OMP_CLAUSE_DECL (c)))
	    sctx.max_vf = 1;
	  break;
	default:
	  continue;
	}

  /* Add a placeholder for simduid.  */
  if (sctx.is_simt && sctx.max_vf != 1)
    sctx.simt_eargs.safe_push (NULL_TREE);

  /* Do all the fixed sized types in the first pass, and the variable sized
     types in the second pass.  This makes sure that the scalar arguments to
     the variable sized types are processed before we use them in the
     variable sized operations.  */
  for (pass = 0; pass < 2; ++pass)
    {
      for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	{
	  enum omp_clause_code c_kind = OMP_CLAUSE_CODE (c);
	  tree var, new_var;
	  bool by_ref;
	  location_t clause_loc = OMP_CLAUSE_LOCATION (c);

	  switch (c_kind)
	    {
	    case OMP_CLAUSE_PRIVATE:
	      if (OMP_CLAUSE_PRIVATE_DEBUG (c))
		continue;
	      break;
	    case OMP_CLAUSE_SHARED:
	      /* Ignore shared directives in teams construct.  */
	      if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
		continue;
	      if (maybe_lookup_decl (OMP_CLAUSE_DECL (c), ctx) == NULL)
		{
		  gcc_assert (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c)
			      || is_global_var (OMP_CLAUSE_DECL (c)));
		  continue;
		}
	    case OMP_CLAUSE_FIRSTPRIVATE:
	    case OMP_CLAUSE_COPYIN:
	      break;
	    case OMP_CLAUSE_LINEAR:
	      if (!OMP_CLAUSE_LINEAR_NO_COPYIN (c)
		  && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		lastprivate_firstprivate = true;
	      break;
	    case OMP_CLAUSE_REDUCTION:
	      if (OMP_CLAUSE_REDUCTION_OMP_ORIG_REF (c))
		reduction_omp_orig_ref = true;
	      break;
	    case OMP_CLAUSE__LOOPTEMP_:
	      /* Handle _looptemp_ clauses only on parallel/task.  */
	      if (fd)
		continue;
	      break;
	    case OMP_CLAUSE_LASTPRIVATE:
	      if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
		{
		  lastprivate_firstprivate = true;
		  if (pass != 0 || is_taskloop_ctx (ctx))
		    continue;
		}
	      /* Even without corresponding firstprivate, if
		 decl is Fortran allocatable, it needs outer var
		 reference.  */
	      else if (pass == 0
		       && lang_hooks.decls.omp_private_outer_ref
							(OMP_CLAUSE_DECL (c)))
		lastprivate_firstprivate = true;
	      break;
	    case OMP_CLAUSE_ALIGNED:
	      if (pass == 0)
		continue;
	      var = OMP_CLAUSE_DECL (c);
	      if (TREE_CODE (TREE_TYPE (var)) == POINTER_TYPE
		  && !is_global_var (var))
		{
		  new_var = maybe_lookup_decl (var, ctx);
		  if (new_var == NULL_TREE)
		    new_var = maybe_lookup_decl_in_outer_ctx (var, ctx);
		  x = builtin_decl_explicit (BUILT_IN_ASSUME_ALIGNED);
		  tree alarg = omp_clause_aligned_alignment (c);
		  alarg = fold_convert_loc (clause_loc, size_type_node, alarg);
		  x = build_call_expr_loc (clause_loc, x, 2, new_var, alarg);
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		  x = build2 (MODIFY_EXPR, TREE_TYPE (new_var), new_var, x);
		  gimplify_and_add (x, ilist);
		}
	      else if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE
		       && is_global_var (var))
		{
		  tree ptype = build_pointer_type (TREE_TYPE (var)), t, t2;
		  new_var = lookup_decl (var, ctx);
		  t = maybe_lookup_decl_in_outer_ctx (var, ctx);
		  t = build_fold_addr_expr_loc (clause_loc, t);
		  t2 = builtin_decl_explicit (BUILT_IN_ASSUME_ALIGNED);
		  tree alarg = omp_clause_aligned_alignment (c);
		  alarg = fold_convert_loc (clause_loc, size_type_node, alarg);
		  t = build_call_expr_loc (clause_loc, t2, 2, t, alarg);
		  t = fold_convert_loc (clause_loc, ptype, t);
		  x = create_tmp_var (ptype);
		  t = build2 (MODIFY_EXPR, ptype, x, t);
		  gimplify_and_add (t, ilist);
		  t = build_simple_mem_ref_loc (clause_loc, x);
		  SET_DECL_VALUE_EXPR (new_var, t);
		  DECL_HAS_VALUE_EXPR_P (new_var) = 1;
		}
	      continue;
	    default:
	      continue;
	    }

	  new_var = var = OMP_CLAUSE_DECL (c);
	  if (c_kind == OMP_CLAUSE_REDUCTION && TREE_CODE (var) == MEM_REF)
	    {
	      var = TREE_OPERAND (var, 0);
	      if (TREE_CODE (var) == POINTER_PLUS_EXPR)
		var = TREE_OPERAND (var, 0);
	      if (TREE_CODE (var) == INDIRECT_REF
		  || TREE_CODE (var) == ADDR_EXPR)
		var = TREE_OPERAND (var, 0);
	      if (is_variable_sized (var))
		{
		  gcc_assert (DECL_HAS_VALUE_EXPR_P (var));
		  var = DECL_VALUE_EXPR (var);
		  gcc_assert (TREE_CODE (var) == INDIRECT_REF);
		  var = TREE_OPERAND (var, 0);
		  gcc_assert (DECL_P (var));
		}
	      new_var = var;
	    }
	  if (c_kind != OMP_CLAUSE_COPYIN)
	    new_var = lookup_decl (var, ctx);

	  if (c_kind == OMP_CLAUSE_SHARED || c_kind == OMP_CLAUSE_COPYIN)
	    {
	      if (pass != 0)
		continue;
	    }
	  /* C/C++ array section reductions.  */
	  else if (c_kind == OMP_CLAUSE_REDUCTION
		   && var != OMP_CLAUSE_DECL (c))
	    {
	      if (pass == 0)
		continue;

	      tree bias = TREE_OPERAND (OMP_CLAUSE_DECL (c), 1);
	      tree orig_var = TREE_OPERAND (OMP_CLAUSE_DECL (c), 0);
	      if (TREE_CODE (orig_var) == POINTER_PLUS_EXPR)
		{
		  tree b = TREE_OPERAND (orig_var, 1);
		  b = maybe_lookup_decl (b, ctx);
		  if (b == NULL)
		    {
		      b = TREE_OPERAND (orig_var, 1);
		      b = maybe_lookup_decl_in_outer_ctx (b, ctx);
		    }
		  if (integer_zerop (bias))
		    bias = b;
		  else
		    {
		      bias = fold_convert_loc (clause_loc,
					       TREE_TYPE (b), bias);
		      bias = fold_build2_loc (clause_loc, PLUS_EXPR,
					      TREE_TYPE (b), b, bias);
		    }
		  orig_var = TREE_OPERAND (orig_var, 0);
		}
	      if (TREE_CODE (orig_var) == INDIRECT_REF
		  || TREE_CODE (orig_var) == ADDR_EXPR)
		orig_var = TREE_OPERAND (orig_var, 0);
	      tree d = OMP_CLAUSE_DECL (c);
	      tree type = TREE_TYPE (d);
	      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
	      tree v = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	      const char *name = get_name (orig_var);
	      if (TREE_CONSTANT (v))
		{
		  x = create_tmp_var_raw (type, name);
		  gimple_add_tmp_var (x);
		  TREE_ADDRESSABLE (x) = 1;
		  x = build_fold_addr_expr_loc (clause_loc, x);
		}
	      else
		{
		  tree atmp
		    = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		  tree t = maybe_lookup_decl (v, ctx);
		  if (t)
		    v = t;
		  else
		    v = maybe_lookup_decl_in_outer_ctx (v, ctx);
		  gimplify_expr (&v, ilist, NULL, is_gimple_val, fb_rvalue);
		  t = fold_build2_loc (clause_loc, PLUS_EXPR,
				       TREE_TYPE (v), v,
				       build_int_cst (TREE_TYPE (v), 1));
		  t = fold_build2_loc (clause_loc, MULT_EXPR,
				       TREE_TYPE (v), t,
				       TYPE_SIZE_UNIT (TREE_TYPE (type)));
		  tree al = size_int (TYPE_ALIGN (TREE_TYPE (type)));
		  x = build_call_expr_loc (clause_loc, atmp, 2, t, al);
		}

	      tree ptype = build_pointer_type (TREE_TYPE (type));
	      x = fold_convert_loc (clause_loc, ptype, x);
	      tree y = create_tmp_var (ptype, name);
	      gimplify_assign (y, x, ilist);
	      x = y;
	      tree yb = y;

	      if (!integer_zerop (bias))
		{
		  bias = fold_convert_loc (clause_loc, pointer_sized_int_node,
					   bias);
		  yb = fold_convert_loc (clause_loc, pointer_sized_int_node,
					 x);
		  yb = fold_build2_loc (clause_loc, MINUS_EXPR,
					pointer_sized_int_node, yb, bias);
		  x = fold_convert_loc (clause_loc, TREE_TYPE (x), yb);
		  yb = create_tmp_var (ptype, name);
		  gimplify_assign (yb, x, ilist);
		  x = yb;
		}

	      d = TREE_OPERAND (d, 0);
	      if (TREE_CODE (d) == POINTER_PLUS_EXPR)
		d = TREE_OPERAND (d, 0);
	      if (TREE_CODE (d) == ADDR_EXPR)
		{
		  if (orig_var != var)
		    {
		      gcc_assert (is_variable_sized (orig_var));
		      x = fold_convert_loc (clause_loc, TREE_TYPE (new_var),
					    x);
		      gimplify_assign (new_var, x, ilist);
		      tree new_orig_var = lookup_decl (orig_var, ctx);
		      tree t = build_fold_indirect_ref (new_var);
		      DECL_IGNORED_P (new_var) = 0;
		      TREE_THIS_NOTRAP (t);
		      SET_DECL_VALUE_EXPR (new_orig_var, t);
		      DECL_HAS_VALUE_EXPR_P (new_orig_var) = 1;
		    }
		  else
		    {
		      x = build2 (MEM_REF, TREE_TYPE (new_var), x,
				  build_int_cst (ptype, 0));
		      SET_DECL_VALUE_EXPR (new_var, x);
		      DECL_HAS_VALUE_EXPR_P (new_var) = 1;
		    }
		}
	      else
		{
		  gcc_assert (orig_var == var);
		  if (TREE_CODE (d) == INDIRECT_REF)
		    {
		      x = create_tmp_var (ptype, name);
		      TREE_ADDRESSABLE (x) = 1;
		      gimplify_assign (x, yb, ilist);
		      x = build_fold_addr_expr_loc (clause_loc, x);
		    }
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		  gimplify_assign (new_var, x, ilist);
		}
	      tree y1 = create_tmp_var (ptype, NULL);
	      gimplify_assign (y1, y, ilist);
	      tree i2 = NULL_TREE, y2 = NULL_TREE;
	      tree body2 = NULL_TREE, end2 = NULL_TREE;
	      tree y3 = NULL_TREE, y4 = NULL_TREE;
	      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) || is_simd)
		{
		  y2 = create_tmp_var (ptype, NULL);
		  gimplify_assign (y2, y, ilist);
		  tree ref = build_outer_var_ref (var, ctx);
		  /* For ref build_outer_var_ref already performs this.  */
		  if (TREE_CODE (d) == INDIRECT_REF)
		    gcc_assert (omp_is_reference (var));
		  else if (TREE_CODE (d) == ADDR_EXPR)
		    ref = build_fold_addr_expr (ref);
		  else if (omp_is_reference (var))
		    ref = build_fold_addr_expr (ref);
		  ref = fold_convert_loc (clause_loc, ptype, ref);
		  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		      && OMP_CLAUSE_REDUCTION_OMP_ORIG_REF (c))
		    {
		      y3 = create_tmp_var (ptype, NULL);
		      gimplify_assign (y3, unshare_expr (ref), ilist);
		    }
		  if (is_simd)
		    {
		      y4 = create_tmp_var (ptype, NULL);
		      gimplify_assign (y4, ref, dlist);
		    }
		}
	      tree i = create_tmp_var (TREE_TYPE (v), NULL);
	      gimplify_assign (i, build_int_cst (TREE_TYPE (v), 0), ilist);
	      tree body = create_artificial_label (UNKNOWN_LOCATION);
	      tree end = create_artificial_label (UNKNOWN_LOCATION);
	      gimple_seq_add_stmt (ilist, gimple_build_label (body));
	      if (y2)
		{
		  i2 = create_tmp_var (TREE_TYPE (v), NULL);
		  gimplify_assign (i2, build_int_cst (TREE_TYPE (v), 0), dlist);
		  body2 = create_artificial_label (UNKNOWN_LOCATION);
		  end2 = create_artificial_label (UNKNOWN_LOCATION);
		  gimple_seq_add_stmt (dlist, gimple_build_label (body2));
		}
	      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
		{
		  tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);
		  tree decl_placeholder
		    = OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c);
		  SET_DECL_VALUE_EXPR (decl_placeholder,
				       build_simple_mem_ref (y1));
		  DECL_HAS_VALUE_EXPR_P (decl_placeholder) = 1;
		  SET_DECL_VALUE_EXPR (placeholder,
				       y3 ? build_simple_mem_ref (y3)
				       : error_mark_node);
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
		  x = lang_hooks.decls.omp_clause_default_ctor
				(c, build_simple_mem_ref (y1),
				 y3 ? build_simple_mem_ref (y3) : NULL_TREE);
		  if (x)
		    gimplify_and_add (x, ilist);
		  if (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c))
		    {
		      gimple_seq tseq = OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (ilist, tseq);
		    }
		  OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
		  if (is_simd)
		    {
		      SET_DECL_VALUE_EXPR (decl_placeholder,
					   build_simple_mem_ref (y2));
		      SET_DECL_VALUE_EXPR (placeholder,
					   build_simple_mem_ref (y4));
		      gimple_seq tseq = OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (dlist, tseq);
		      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
		    }
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 0;
		  DECL_HAS_VALUE_EXPR_P (decl_placeholder) = 0;
		  x = lang_hooks.decls.omp_clause_dtor
					(c, build_simple_mem_ref (y2));
		  if (x)
		    {
		      gimple_seq tseq = NULL;
		      dtor = x;
		      gimplify_stmt (&dtor, &tseq);
		      gimple_seq_add_seq (dlist, tseq);
		    }
		}
	      else
		{
		  x = omp_reduction_init (c, TREE_TYPE (type));
		  enum tree_code code = OMP_CLAUSE_REDUCTION_CODE (c);

		  /* reduction(-:var) sums up the partial results, so it
		     acts identically to reduction(+:var).  */
		  if (code == MINUS_EXPR)
		    code = PLUS_EXPR;

		  gimplify_assign (build_simple_mem_ref (y1), x, ilist);
		  if (is_simd)
		    {
		      x = build2 (code, TREE_TYPE (type),
				  build_simple_mem_ref (y4),
				  build_simple_mem_ref (y2));
		      gimplify_assign (build_simple_mem_ref (y4), x, dlist);
		    }
		}
	      gimple *g
		= gimple_build_assign (y1, POINTER_PLUS_EXPR, y1,
				       TYPE_SIZE_UNIT (TREE_TYPE (type)));
	      gimple_seq_add_stmt (ilist, g);
	      if (y3)
		{
		  g = gimple_build_assign (y3, POINTER_PLUS_EXPR, y3,
					   TYPE_SIZE_UNIT (TREE_TYPE (type)));
		  gimple_seq_add_stmt (ilist, g);
		}
	      g = gimple_build_assign (i, PLUS_EXPR, i,
				       build_int_cst (TREE_TYPE (i), 1));
	      gimple_seq_add_stmt (ilist, g);
	      g = gimple_build_cond (LE_EXPR, i, v, body, end);
	      gimple_seq_add_stmt (ilist, g);
	      gimple_seq_add_stmt (ilist, gimple_build_label (end));
	      if (y2)
		{
		  g = gimple_build_assign (y2, POINTER_PLUS_EXPR, y2,
					   TYPE_SIZE_UNIT (TREE_TYPE (type)));
		  gimple_seq_add_stmt (dlist, g);
		  if (y4)
		    {
		      g = gimple_build_assign
					(y4, POINTER_PLUS_EXPR, y4,
					 TYPE_SIZE_UNIT (TREE_TYPE (type)));
		      gimple_seq_add_stmt (dlist, g);
		    }
		  g = gimple_build_assign (i2, PLUS_EXPR, i2,
					   build_int_cst (TREE_TYPE (i2), 1));
		  gimple_seq_add_stmt (dlist, g);
		  g = gimple_build_cond (LE_EXPR, i2, v, body2, end2);
		  gimple_seq_add_stmt (dlist, g);
		  gimple_seq_add_stmt (dlist, gimple_build_label (end2));
		}
	      continue;
	    }
	  else if (is_variable_sized (var))
	    {
	      /* For variable sized types, we need to allocate the
		 actual storage here.  Call alloca and store the
		 result in the pointer decl that we created elsewhere.  */
	      if (pass == 0)
		continue;

	      if (c_kind != OMP_CLAUSE_FIRSTPRIVATE || !is_task_ctx (ctx))
		{
		  gcall *stmt;
		  tree tmp, atmp;

		  ptr = DECL_VALUE_EXPR (new_var);
		  gcc_assert (TREE_CODE (ptr) == INDIRECT_REF);
		  ptr = TREE_OPERAND (ptr, 0);
		  gcc_assert (DECL_P (ptr));
		  x = TYPE_SIZE_UNIT (TREE_TYPE (new_var));

		  /* void *tmp = __builtin_alloca */
		  atmp = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		  stmt = gimple_build_call (atmp, 2, x,
					    size_int (DECL_ALIGN (var)));
		  tmp = create_tmp_var_raw (ptr_type_node);
		  gimple_add_tmp_var (tmp);
		  gimple_call_set_lhs (stmt, tmp);

		  gimple_seq_add_stmt (ilist, stmt);

		  x = fold_convert_loc (clause_loc, TREE_TYPE (ptr), tmp);
		  gimplify_assign (ptr, x, ilist);
		}
	    }
	  else if (omp_is_reference (var))
	    {
	      /* For references that are being privatized for Fortran,
		 allocate new backing storage for the new pointer
		 variable.  This allows us to avoid changing all the
		 code that expects a pointer to something that expects
		 a direct variable.  */
	      if (pass == 0)
		continue;

	      x = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_var)));
	      if (c_kind == OMP_CLAUSE_FIRSTPRIVATE && is_task_ctx (ctx))
		{
		  x = build_receiver_ref (var, false, ctx);
		  x = build_fold_addr_expr_loc (clause_loc, x);
		}
	      else if (TREE_CONSTANT (x))
		{
		  /* For reduction in SIMD loop, defer adding the
		     initialization of the reference, because if we decide
		     to use SIMD array for it, the initilization could cause
		     expansion ICE.  */
		  if (c_kind == OMP_CLAUSE_REDUCTION && is_simd)
		    x = NULL_TREE;
		  else
		    {
		      x = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (new_var)),
					      get_name (var));
		      gimple_add_tmp_var (x);
		      TREE_ADDRESSABLE (x) = 1;
		      x = build_fold_addr_expr_loc (clause_loc, x);
		    }
		}
	      else
		{
		  tree atmp
		    = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		  tree rtype = TREE_TYPE (TREE_TYPE (new_var));
		  tree al = size_int (TYPE_ALIGN (rtype));
		  x = build_call_expr_loc (clause_loc, atmp, 2, x, al);
		}

	      if (x)
		{
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		  gimplify_assign (new_var, x, ilist);
		}

	      new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	    }
	  else if (c_kind == OMP_CLAUSE_REDUCTION
		   && OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    {
	      if (pass == 0)
		continue;
	    }
	  else if (pass != 0)
	    continue;

	  switch (OMP_CLAUSE_CODE (c))
	    {
	    case OMP_CLAUSE_SHARED:
	      /* Ignore shared directives in teams construct.  */
	      if (gimple_code (ctx->stmt) == GIMPLE_OMP_TEAMS)
		continue;
	      /* Shared global vars are just accessed directly.  */
	      if (is_global_var (new_var))
		break;
	      /* For taskloop firstprivate/lastprivate, represented
		 as firstprivate and shared clause on the task, new_var
		 is the firstprivate var.  */
	      if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
		break;
	      /* Set up the DECL_VALUE_EXPR for shared variables now.  This
		 needs to be delayed until after fixup_child_record_type so
		 that we get the correct type during the dereference.  */
	      by_ref = use_pointer_for_field (var, ctx);
	      x = build_receiver_ref (var, by_ref, ctx);
	      SET_DECL_VALUE_EXPR (new_var, x);
	      DECL_HAS_VALUE_EXPR_P (new_var) = 1;

	      /* ??? If VAR is not passed by reference, and the variable
		 hasn't been initialized yet, then we'll get a warning for
		 the store into the omp_data_s structure.  Ideally, we'd be
		 able to notice this and not store anything at all, but
		 we're generating code too early.  Suppress the warning.  */
	      if (!by_ref)
		TREE_NO_WARNING (var) = 1;
	      break;

	    case OMP_CLAUSE_LASTPRIVATE:
	      if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
		break;
	      /* FALLTHRU */

	    case OMP_CLAUSE_PRIVATE:
	      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_PRIVATE)
		x = build_outer_var_ref (var, ctx);
	      else if (OMP_CLAUSE_PRIVATE_OUTER_REF (c))
		{
		  if (is_task_ctx (ctx))
		    x = build_receiver_ref (var, false, ctx);
		  else
		    x = build_outer_var_ref (var, ctx, OMP_CLAUSE_PRIVATE);
		}
	      else
		x = NULL;
	    do_private:
	      tree nx;
	      nx = lang_hooks.decls.omp_clause_default_ctor
						(c, unshare_expr (new_var), x);
	      if (is_simd)
		{
		  tree y = lang_hooks.decls.omp_clause_dtor (c, new_var);
		  if ((TREE_ADDRESSABLE (new_var) || nx || y
		       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
		      && lower_rec_simd_input_clauses (new_var, ctx, &sctx,
						       ivar, lvar))
		    {
		      if (nx)
			x = lang_hooks.decls.omp_clause_default_ctor
						(c, unshare_expr (ivar), x);
		      if (nx && x)
			gimplify_and_add (x, &llist[0]);
		      if (y)
			{
			  y = lang_hooks.decls.omp_clause_dtor (c, ivar);
			  if (y)
			    {
			      gimple_seq tseq = NULL;

			      dtor = y;
			      gimplify_stmt (&dtor, &tseq);
			      gimple_seq_add_seq (&llist[1], tseq);
			    }
			}
		      break;
		    }
		}
	      if (nx)
		gimplify_and_add (nx, ilist);
	      /* FALLTHRU */

	    do_dtor:
	      x = lang_hooks.decls.omp_clause_dtor (c, new_var);
	      if (x)
		{
		  gimple_seq tseq = NULL;

		  dtor = x;
		  gimplify_stmt (&dtor, &tseq);
		  gimple_seq_add_seq (dlist, tseq);
		}
	      break;

	    case OMP_CLAUSE_LINEAR:
	      if (!OMP_CLAUSE_LINEAR_NO_COPYIN (c))
		goto do_firstprivate;
	      if (OMP_CLAUSE_LINEAR_NO_COPYOUT (c))
		x = NULL;
	      else
		x = build_outer_var_ref (var, ctx);
	      goto do_private;

	    case OMP_CLAUSE_FIRSTPRIVATE:
	      if (is_task_ctx (ctx))
		{
		  if (omp_is_reference (var) || is_variable_sized (var))
		    goto do_dtor;
		  else if (is_global_var (maybe_lookup_decl_in_outer_ctx (var,
									  ctx))
			   || use_pointer_for_field (var, NULL))
		    {
		      x = build_receiver_ref (var, false, ctx);
		      SET_DECL_VALUE_EXPR (new_var, x);
		      DECL_HAS_VALUE_EXPR_P (new_var) = 1;
		      goto do_dtor;
		    }
		}
	    do_firstprivate:
	      x = build_outer_var_ref (var, ctx);
	      if (is_simd)
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		      && gimple_omp_for_combined_into_p (ctx->stmt))
		    {
		      tree t = OMP_CLAUSE_LINEAR_STEP (c);
		      tree stept = TREE_TYPE (t);
		      tree ct = omp_find_clause (clauses,
						 OMP_CLAUSE__LOOPTEMP_);
		      gcc_assert (ct);
		      tree l = OMP_CLAUSE_DECL (ct);
		      tree n1 = fd->loop.n1;
		      tree step = fd->loop.step;
		      tree itype = TREE_TYPE (l);
		      if (POINTER_TYPE_P (itype))
			itype = signed_type_for (itype);
		      l = fold_build2 (MINUS_EXPR, itype, l, n1);
		      if (TYPE_UNSIGNED (itype)
			  && fd->loop.cond_code == GT_EXPR)
			l = fold_build2 (TRUNC_DIV_EXPR, itype,
					 fold_build1 (NEGATE_EXPR, itype, l),
					 fold_build1 (NEGATE_EXPR,
						      itype, step));
		      else
			l = fold_build2 (TRUNC_DIV_EXPR, itype, l, step);
		      t = fold_build2 (MULT_EXPR, stept,
				       fold_convert (stept, l), t);

		      if (OMP_CLAUSE_LINEAR_ARRAY (c))
			{
			  x = lang_hooks.decls.omp_clause_linear_ctor
							(c, new_var, x, t);
			  gimplify_and_add (x, ilist);
			  goto do_dtor;
			}

		      if (POINTER_TYPE_P (TREE_TYPE (x)))
			x = fold_build2 (POINTER_PLUS_EXPR,
					 TREE_TYPE (x), x, t);
		      else
			x = fold_build2 (PLUS_EXPR, TREE_TYPE (x), x, t);
		    }

		  if ((OMP_CLAUSE_CODE (c) != OMP_CLAUSE_LINEAR
		       || TREE_ADDRESSABLE (new_var))
		      && lower_rec_simd_input_clauses (new_var, ctx, &sctx,
						       ivar, lvar))
		    {
		      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR)
			{
			  tree iv = create_tmp_var (TREE_TYPE (new_var));
			  x = lang_hooks.decls.omp_clause_copy_ctor (c, iv, x);
			  gimplify_and_add (x, ilist);
			  gimple_stmt_iterator gsi
			    = gsi_start_1 (gimple_omp_body_ptr (ctx->stmt));
			  gassign *g
			    = gimple_build_assign (unshare_expr (lvar), iv);
			  gsi_insert_before_without_update (&gsi, g,
							    GSI_SAME_STMT);
			  tree t = OMP_CLAUSE_LINEAR_STEP (c);
			  enum tree_code code = PLUS_EXPR;
			  if (POINTER_TYPE_P (TREE_TYPE (new_var)))
			    code = POINTER_PLUS_EXPR;
			  g = gimple_build_assign (iv, code, iv, t);
			  gsi_insert_before_without_update (&gsi, g,
							    GSI_SAME_STMT);
			  break;
			}
		      x = lang_hooks.decls.omp_clause_copy_ctor
						(c, unshare_expr (ivar), x);
		      gimplify_and_add (x, &llist[0]);
		      x = lang_hooks.decls.omp_clause_dtor (c, ivar);
		      if (x)
			{
			  gimple_seq tseq = NULL;

			  dtor = x;
			  gimplify_stmt (&dtor, &tseq);
			  gimple_seq_add_seq (&llist[1], tseq);
			}
		      break;
		    }
		}
	      x = lang_hooks.decls.omp_clause_copy_ctor
						(c, unshare_expr (new_var), x);
	      gimplify_and_add (x, ilist);
	      goto do_dtor;

	    case OMP_CLAUSE__LOOPTEMP_:
	      gcc_assert (is_taskreg_ctx (ctx));
	      x = build_outer_var_ref (var, ctx);
	      x = build2 (MODIFY_EXPR, TREE_TYPE (new_var), new_var, x);
	      gimplify_and_add (x, ilist);
	      break;

	    case OMP_CLAUSE_COPYIN:
	      by_ref = use_pointer_for_field (var, NULL);
	      x = build_receiver_ref (var, by_ref, ctx);
	      x = lang_hooks.decls.omp_clause_assign_op (c, new_var, x);
	      append_to_statement_list (x, &copyin_seq);
	      copyin_by_ref |= by_ref;
	      break;

	    case OMP_CLAUSE_REDUCTION:
	      /* OpenACC reductions are initialized using the
		 GOACC_REDUCTION internal function.  */
	      if (is_gimple_omp_oacc (ctx->stmt))
		break;
	      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
		{
		  tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);
		  gimple *tseq;
		  x = build_outer_var_ref (var, ctx);

		  if (omp_is_reference (var)
		      && !useless_type_conversion_p (TREE_TYPE (placeholder),
						     TREE_TYPE (x)))
		    x = build_fold_addr_expr_loc (clause_loc, x);
		  SET_DECL_VALUE_EXPR (placeholder, x);
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
		  tree new_vard = new_var;
		  if (omp_is_reference (var))
		    {
		      gcc_assert (TREE_CODE (new_var) == MEM_REF);
		      new_vard = TREE_OPERAND (new_var, 0);
		      gcc_assert (DECL_P (new_vard));
		    }
		  if (is_simd
		      && lower_rec_simd_input_clauses (new_var, ctx, &sctx,
						       ivar, lvar))
		    {
		      if (new_vard == new_var)
			{
			  gcc_assert (DECL_VALUE_EXPR (new_var) == lvar);
			  SET_DECL_VALUE_EXPR (new_var, ivar);
			}
		      else
			{
			  SET_DECL_VALUE_EXPR (new_vard,
					       build_fold_addr_expr (ivar));
			  DECL_HAS_VALUE_EXPR_P (new_vard) = 1;
			}
		      x = lang_hooks.decls.omp_clause_default_ctor
				(c, unshare_expr (ivar),
				 build_outer_var_ref (var, ctx));
		      if (x)
			gimplify_and_add (x, &llist[0]);
		      if (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c))
			{
			  tseq = OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c);
			  lower_omp (&tseq, ctx);
			  gimple_seq_add_seq (&llist[0], tseq);
			}
		      OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
		      tseq = OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (&llist[1], tseq);
		      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
		      DECL_HAS_VALUE_EXPR_P (placeholder) = 0;
		      if (new_vard == new_var)
			SET_DECL_VALUE_EXPR (new_var, lvar);
		      else
			SET_DECL_VALUE_EXPR (new_vard,
					     build_fold_addr_expr (lvar));
		      x = lang_hooks.decls.omp_clause_dtor (c, ivar);
		      if (x)
			{
			  tseq = NULL;
			  dtor = x;
			  gimplify_stmt (&dtor, &tseq);
			  gimple_seq_add_seq (&llist[1], tseq);
			}
		      break;
		    }
		  /* If this is a reference to constant size reduction var
		     with placeholder, we haven't emitted the initializer
		     for it because it is undesirable if SIMD arrays are used.
		     But if they aren't used, we need to emit the deferred
		     initialization now.  */
		  else if (omp_is_reference (var) && is_simd)
		    handle_simd_reference (clause_loc, new_vard, ilist);
		  x = lang_hooks.decls.omp_clause_default_ctor
				(c, unshare_expr (new_var),
				 build_outer_var_ref (var, ctx));
		  if (x)
		    gimplify_and_add (x, ilist);
		  if (OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c))
		    {
		      tseq = OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (ilist, tseq);
		    }
		  OMP_CLAUSE_REDUCTION_GIMPLE_INIT (c) = NULL;
		  if (is_simd)
		    {
		      tseq = OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c);
		      lower_omp (&tseq, ctx);
		      gimple_seq_add_seq (dlist, tseq);
		      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
		    }
		  DECL_HAS_VALUE_EXPR_P (placeholder) = 0;
		  goto do_dtor;
		}
	      else
		{
		  x = omp_reduction_init (c, TREE_TYPE (new_var));
		  gcc_assert (TREE_CODE (TREE_TYPE (new_var)) != ARRAY_TYPE);
		  enum tree_code code = OMP_CLAUSE_REDUCTION_CODE (c);

		  /* reduction(-:var) sums up the partial results, so it
		     acts identically to reduction(+:var).  */
		  if (code == MINUS_EXPR)
		    code = PLUS_EXPR;

		  tree new_vard = new_var;
		  if (is_simd && omp_is_reference (var))
		    {
		      gcc_assert (TREE_CODE (new_var) == MEM_REF);
		      new_vard = TREE_OPERAND (new_var, 0);
		      gcc_assert (DECL_P (new_vard));
		    }
		  if (is_simd
		      && lower_rec_simd_input_clauses (new_var, ctx, &sctx,
						       ivar, lvar))
		    {
		      tree ref = build_outer_var_ref (var, ctx);

		      gimplify_assign (unshare_expr (ivar), x, &llist[0]);

		      if (sctx.is_simt)
			{
			  if (!simt_lane)
			    simt_lane = create_tmp_var (unsigned_type_node);
			  x = build_call_expr_internal_loc
			    (UNKNOWN_LOCATION, IFN_GOMP_SIMT_XCHG_BFLY,
			     TREE_TYPE (ivar), 2, ivar, simt_lane);
			  x = build2 (code, TREE_TYPE (ivar), ivar, x);
			  gimplify_assign (ivar, x, &llist[2]);
			}
		      x = build2 (code, TREE_TYPE (ref), ref, ivar);
		      ref = build_outer_var_ref (var, ctx);
		      gimplify_assign (ref, x, &llist[1]);

		      if (new_vard != new_var)
			{
			  SET_DECL_VALUE_EXPR (new_vard,
					       build_fold_addr_expr (lvar));
			  DECL_HAS_VALUE_EXPR_P (new_vard) = 1;
			}
		    }
		  else
		    {
		      if (omp_is_reference (var) && is_simd)
			handle_simd_reference (clause_loc, new_vard, ilist);
		      gimplify_assign (new_var, x, ilist);
		      if (is_simd)
			{
			  tree ref = build_outer_var_ref (var, ctx);

			  x = build2 (code, TREE_TYPE (ref), ref, new_var);
			  ref = build_outer_var_ref (var, ctx);
			  gimplify_assign (ref, x, dlist);
			}
		    }
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  if (sctx.max_vf == 1)
    sctx.is_simt = false;

  if (sctx.lane || sctx.is_simt)
    {
      uid = create_tmp_var (ptr_type_node, "simduid");
      /* Don't want uninit warnings on simduid, it is always uninitialized,
	 but we use it not for the value, but for the DECL_UID only.  */
      TREE_NO_WARNING (uid) = 1;
      c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__SIMDUID_);
      OMP_CLAUSE__SIMDUID__DECL (c) = uid;
      OMP_CLAUSE_CHAIN (c) = gimple_omp_for_clauses (ctx->stmt);
      gimple_omp_for_set_clauses (ctx->stmt, c);
    }
  /* Emit calls denoting privatized variables and initializing a pointer to
     structure that holds private variables as fields after ompdevlow pass.  */
  if (sctx.is_simt)
    {
      sctx.simt_eargs[0] = uid;
      gimple *g
	= gimple_build_call_internal_vec (IFN_GOMP_SIMT_ENTER, sctx.simt_eargs);
      gimple_call_set_lhs (g, uid);
      gimple_seq_add_stmt (ilist, g);
      sctx.simt_eargs.release ();

      simtrec = create_tmp_var (ptr_type_node, ".omp_simt");
      g = gimple_build_call_internal (IFN_GOMP_SIMT_ENTER_ALLOC, 1, uid);
      gimple_call_set_lhs (g, simtrec);
      gimple_seq_add_stmt (ilist, g);
    }
  if (sctx.lane)
    {
      gimple *g
	= gimple_build_call_internal (IFN_GOMP_SIMD_LANE, 1, uid);
      gimple_call_set_lhs (g, sctx.lane);
      gimple_stmt_iterator gsi = gsi_start_1 (gimple_omp_body_ptr (ctx->stmt));
      gsi_insert_before_without_update (&gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (sctx.lane, INTEGER_CST,
			       build_int_cst (unsigned_type_node, 0));
      gimple_seq_add_stmt (ilist, g);
      /* Emit reductions across SIMT lanes in log_2(simt_vf) steps.  */
      if (llist[2])
	{
	  tree simt_vf = create_tmp_var (unsigned_type_node);
	  g = gimple_build_call_internal (IFN_GOMP_SIMT_VF, 0);
	  gimple_call_set_lhs (g, simt_vf);
	  gimple_seq_add_stmt (dlist, g);

	  tree t = build_int_cst (unsigned_type_node, 1);
	  g = gimple_build_assign (simt_lane, INTEGER_CST, t);
	  gimple_seq_add_stmt (dlist, g);

	  t = build_int_cst (unsigned_type_node, 0);
	  g = gimple_build_assign (sctx.idx, INTEGER_CST, t);
	  gimple_seq_add_stmt (dlist, g);

	  tree body = create_artificial_label (UNKNOWN_LOCATION);
	  tree header = create_artificial_label (UNKNOWN_LOCATION);
	  tree end = create_artificial_label (UNKNOWN_LOCATION);
	  gimple_seq_add_stmt (dlist, gimple_build_goto (header));
	  gimple_seq_add_stmt (dlist, gimple_build_label (body));

	  gimple_seq_add_seq (dlist, llist[2]);

	  g = gimple_build_assign (simt_lane, LSHIFT_EXPR, simt_lane, integer_one_node);
	  gimple_seq_add_stmt (dlist, g);

	  gimple_seq_add_stmt (dlist, gimple_build_label (header));
	  g = gimple_build_cond (LT_EXPR, simt_lane, simt_vf, body, end);
	  gimple_seq_add_stmt (dlist, g);

	  gimple_seq_add_stmt (dlist, gimple_build_label (end));
	}
      for (int i = 0; i < 2; i++)
	if (llist[i])
	  {
	    tree vf = create_tmp_var (unsigned_type_node);
	    g = gimple_build_call_internal (IFN_GOMP_SIMD_VF, 1, uid);
	    gimple_call_set_lhs (g, vf);
	    gimple_seq *seq = i == 0 ? ilist : dlist;
	    gimple_seq_add_stmt (seq, g);
	    tree t = build_int_cst (unsigned_type_node, 0);
	    g = gimple_build_assign (sctx.idx, INTEGER_CST, t);
	    gimple_seq_add_stmt (seq, g);
	    tree body = create_artificial_label (UNKNOWN_LOCATION);
	    tree header = create_artificial_label (UNKNOWN_LOCATION);
	    tree end = create_artificial_label (UNKNOWN_LOCATION);
	    gimple_seq_add_stmt (seq, gimple_build_goto (header));
	    gimple_seq_add_stmt (seq, gimple_build_label (body));
	    gimple_seq_add_seq (seq, llist[i]);
	    t = build_int_cst (unsigned_type_node, 1);
	    g = gimple_build_assign (sctx.idx, PLUS_EXPR, sctx.idx, t);
	    gimple_seq_add_stmt (seq, g);
	    gimple_seq_add_stmt (seq, gimple_build_label (header));
	    g = gimple_build_cond (LT_EXPR, sctx.idx, vf, body, end);
	    gimple_seq_add_stmt (seq, g);
	    gimple_seq_add_stmt (seq, gimple_build_label (end));
	  }
    }
  if (sctx.is_simt)
    {
      gimple_seq_add_seq (dlist, sctx.simt_dlist);
      gimple *g
	= gimple_build_call_internal (IFN_GOMP_SIMT_EXIT, 1, simtrec);
      gimple_seq_add_stmt (dlist, g);
    }

  /* The copyin sequence is not to be executed by the main thread, since
     that would result in self-copies.  Perhaps not visible to scalars,
     but it certainly is to C++ operator=.  */
  if (copyin_seq)
    {
      x = build_call_expr (builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM),
			   0);
      x = build2 (NE_EXPR, boolean_type_node, x,
		  build_int_cst (TREE_TYPE (x), 0));
      x = build3 (COND_EXPR, void_type_node, x, copyin_seq, NULL);
      gimplify_and_add (x, ilist);
    }

  /* If any copyin variable is passed by reference, we must ensure the
     master thread doesn't modify it before it is copied over in all
     threads.  Similarly for variables in both firstprivate and
     lastprivate clauses we need to ensure the lastprivate copying
     happens after firstprivate copying in all threads.  And similarly
     for UDRs if initializer expression refers to omp_orig.  */
  if (copyin_by_ref || lastprivate_firstprivate || reduction_omp_orig_ref)
    {
      /* Don't add any barrier for #pragma omp simd or
	 #pragma omp distribute.  */
      if (gimple_code (ctx->stmt) != GIMPLE_OMP_FOR
	  || gimple_omp_for_kind (ctx->stmt) == GF_OMP_FOR_KIND_FOR)
	gimple_seq_add_stmt (ilist, omp_build_barrier (NULL_TREE));
    }

  /* If max_vf is non-zero, then we can use only a vectorization factor
     up to the max_vf we chose.  So stick it into the safelen clause.  */
  if (sctx.max_vf)
    {
      tree c = omp_find_clause (gimple_omp_for_clauses (ctx->stmt),
				OMP_CLAUSE_SAFELEN);
      if (c == NULL_TREE
	  || (TREE_CODE (OMP_CLAUSE_SAFELEN_EXPR (c)) == INTEGER_CST
	      && compare_tree_int (OMP_CLAUSE_SAFELEN_EXPR (c),
				   sctx.max_vf) == 1))
	{
	  c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_SAFELEN);
	  OMP_CLAUSE_SAFELEN_EXPR (c) = build_int_cst (integer_type_node,
						       sctx.max_vf);
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_for_clauses (ctx->stmt);
	  gimple_omp_for_set_clauses (ctx->stmt, c);
	}
    }
}


/* Generate code to implement the LASTPRIVATE clauses.  This is used for
   both parallel and workshare constructs.  PREDICATE may be NULL if it's
   always true.   */

static void
lower_lastprivate_clauses (tree clauses, tree predicate, gimple_seq *stmt_list,
			   omp_context *ctx)
{
  tree x, c, label = NULL, orig_clauses = clauses;
  bool par_clauses = false;
  tree simduid = NULL, lastlane = NULL, simtcond = NULL, simtlast = NULL;

  /* Early exit if there are no lastprivate or linear clauses.  */
  for (; clauses ; clauses = OMP_CLAUSE_CHAIN (clauses))
    if (OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_LASTPRIVATE
	|| (OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_LINEAR
	    && !OMP_CLAUSE_LINEAR_NO_COPYOUT (clauses)))
      break;
  if (clauses == NULL)
    {
      /* If this was a workshare clause, see if it had been combined
	 with its parallel.  In that case, look for the clauses on the
	 parallel statement itself.  */
      if (is_parallel_ctx (ctx))
	return;

      ctx = ctx->outer;
      if (ctx == NULL || !is_parallel_ctx (ctx))
	return;

      clauses = omp_find_clause (gimple_omp_parallel_clauses (ctx->stmt),
				 OMP_CLAUSE_LASTPRIVATE);
      if (clauses == NULL)
	return;
      par_clauses = true;
    }

  bool maybe_simt = false;
  if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
      && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
    {
      maybe_simt = omp_find_clause (orig_clauses, OMP_CLAUSE__SIMT_);
      simduid = omp_find_clause (orig_clauses, OMP_CLAUSE__SIMDUID_);
      if (simduid)
	simduid = OMP_CLAUSE__SIMDUID__DECL (simduid);
    }

  if (predicate)
    {
      gcond *stmt;
      tree label_true, arm1, arm2;
      enum tree_code pred_code = TREE_CODE (predicate);

      label = create_artificial_label (UNKNOWN_LOCATION);
      label_true = create_artificial_label (UNKNOWN_LOCATION);
      if (TREE_CODE_CLASS (pred_code) == tcc_comparison)
	{
	  arm1 = TREE_OPERAND (predicate, 0);
	  arm2 = TREE_OPERAND (predicate, 1);
	  gimplify_expr (&arm1, stmt_list, NULL, is_gimple_val, fb_rvalue);
	  gimplify_expr (&arm2, stmt_list, NULL, is_gimple_val, fb_rvalue);
	}
      else
	{
	  arm1 = predicate;
	  gimplify_expr (&arm1, stmt_list, NULL, is_gimple_val, fb_rvalue);
	  arm2 = boolean_false_node;
	  pred_code = NE_EXPR;
	}
      if (maybe_simt)
	{
	  c = build2 (pred_code, boolean_type_node, arm1, arm2);
	  c = fold_convert (integer_type_node, c);
	  simtcond = create_tmp_var (integer_type_node);
	  gimplify_assign (simtcond, c, stmt_list);
	  gcall *g = gimple_build_call_internal (IFN_GOMP_SIMT_VOTE_ANY,
						 1, simtcond);
	  c = create_tmp_var (integer_type_node);
	  gimple_call_set_lhs (g, c);
	  gimple_seq_add_stmt (stmt_list, g);
	  stmt = gimple_build_cond (NE_EXPR, c, integer_zero_node,
				    label_true, label);
	}
      else
	stmt = gimple_build_cond (pred_code, arm1, arm2, label_true, label);
      gimple_seq_add_stmt (stmt_list, stmt);
      gimple_seq_add_stmt (stmt_list, gimple_build_label (label_true));
    }

  for (c = clauses; c ;)
    {
      tree var, new_var;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	  || (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	      && !OMP_CLAUSE_LINEAR_NO_COPYOUT (c)))
	{
	  var = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c)
	      && is_taskloop_ctx (ctx))
	    {
	      gcc_checking_assert (ctx->outer && is_task_ctx (ctx->outer));
	      new_var = lookup_decl (var, ctx->outer);
	    }
	  else
	    {
	      new_var = lookup_decl (var, ctx);
	      /* Avoid uninitialized warnings for lastprivate and
		 for linear iterators.  */
	      if (predicate
		  && (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		      || OMP_CLAUSE_LINEAR_NO_COPYIN (c)))
		TREE_NO_WARNING (new_var) = 1;
	    }

	  if (!maybe_simt && simduid && DECL_HAS_VALUE_EXPR_P (new_var))
	    {
	      tree val = DECL_VALUE_EXPR (new_var);
	      if (TREE_CODE (val) == ARRAY_REF
		  && VAR_P (TREE_OPERAND (val, 0))
		  && lookup_attribute ("omp simd array",
				       DECL_ATTRIBUTES (TREE_OPERAND (val,
								      0))))
		{
		  if (lastlane == NULL)
		    {
		      lastlane = create_tmp_var (unsigned_type_node);
		      gcall *g
			= gimple_build_call_internal (IFN_GOMP_SIMD_LAST_LANE,
						      2, simduid,
						      TREE_OPERAND (val, 1));
		      gimple_call_set_lhs (g, lastlane);
		      gimple_seq_add_stmt (stmt_list, g);
		    }
		  new_var = build4 (ARRAY_REF, TREE_TYPE (val),
				    TREE_OPERAND (val, 0), lastlane,
				    NULL_TREE, NULL_TREE);
		}
	    }
	  else if (maybe_simt)
	    {
	      tree val = (DECL_HAS_VALUE_EXPR_P (new_var)
			  ? DECL_VALUE_EXPR (new_var)
			  : new_var);
	      if (simtlast == NULL)
		{
		  simtlast = create_tmp_var (unsigned_type_node);
		  gcall *g = gimple_build_call_internal
		    (IFN_GOMP_SIMT_LAST_LANE, 1, simtcond);
		  gimple_call_set_lhs (g, simtlast);
		  gimple_seq_add_stmt (stmt_list, g);
		}
	      x = build_call_expr_internal_loc
		(UNKNOWN_LOCATION, IFN_GOMP_SIMT_XCHG_IDX,
		 TREE_TYPE (val), 2, val, simtlast);
	      new_var = unshare_expr (new_var);
	      gimplify_assign (new_var, x, stmt_list);
	      new_var = unshare_expr (new_var);
	    }

	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c))
	    {
	      lower_omp (&OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c), ctx);
	      gimple_seq_add_seq (stmt_list,
				  OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c));
	      OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (c) = NULL;
	    }
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
		   && OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c))
	    {
	      lower_omp (&OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c), ctx);
	      gimple_seq_add_seq (stmt_list,
				  OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c));
	      OMP_CLAUSE_LINEAR_GIMPLE_SEQ (c) = NULL;
	    }

	  x = NULL_TREE;
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	      && OMP_CLAUSE_LASTPRIVATE_TASKLOOP_IV (c))
	    {
	      gcc_checking_assert (is_taskloop_ctx (ctx));
	      tree ovar = maybe_lookup_decl_in_outer_ctx (var,
							  ctx->outer->outer);
	      if (is_global_var (ovar))
		x = ovar;
	    }
	  if (!x)
	    x = build_outer_var_ref (var, ctx, OMP_CLAUSE_LASTPRIVATE);
	  if (omp_is_reference (var))
	    new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	  x = lang_hooks.decls.omp_clause_assign_op (c, x, new_var);
	  gimplify_and_add (x, stmt_list);
	}
      c = OMP_CLAUSE_CHAIN (c);
      if (c == NULL && !par_clauses)
	{
	  /* If this was a workshare clause, see if it had been combined
	     with its parallel.  In that case, continue looking for the
	     clauses also on the parallel statement itself.  */
	  if (is_parallel_ctx (ctx))
	    break;

	  ctx = ctx->outer;
	  if (ctx == NULL || !is_parallel_ctx (ctx))
	    break;

	  c = omp_find_clause (gimple_omp_parallel_clauses (ctx->stmt),
			       OMP_CLAUSE_LASTPRIVATE);
	  par_clauses = true;
	}
    }

  if (label)
    gimple_seq_add_stmt (stmt_list, gimple_build_label (label));
}

/* Lower the OpenACC reductions of CLAUSES for compute axis LEVEL
   (which might be a placeholder).  INNER is true if this is an inner
   axis of a multi-axis loop.  FORK and JOIN are (optional) fork and
   join markers.  Generate the before-loop forking sequence in
   FORK_SEQ and the after-loop joining sequence to JOIN_SEQ.  The
   general form of these sequences is

     GOACC_REDUCTION_SETUP
     GOACC_FORK
     GOACC_REDUCTION_INIT
     ...
     GOACC_REDUCTION_FINI
     GOACC_JOIN
     GOACC_REDUCTION_TEARDOWN.  */

static void
lower_oacc_reductions (location_t loc, tree clauses, tree level, bool inner,
		       gcall *fork, gcall *join, gimple_seq *fork_seq,
		       gimple_seq *join_seq, omp_context *ctx)
{
  gimple_seq before_fork = NULL;
  gimple_seq after_fork = NULL;
  gimple_seq before_join = NULL;
  gimple_seq after_join = NULL;
  tree init_code = NULL_TREE, fini_code = NULL_TREE,
    setup_code = NULL_TREE, teardown_code = NULL_TREE;
  unsigned offset = 0;

  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION)
      {
	tree orig = OMP_CLAUSE_DECL (c);
	tree var = maybe_lookup_decl (orig, ctx);
	tree ref_to_res = NULL_TREE;
	tree incoming, outgoing, v1, v2, v3;
	bool is_private = false;

	enum tree_code rcode = OMP_CLAUSE_REDUCTION_CODE (c);
	if (rcode == MINUS_EXPR)
	  rcode = PLUS_EXPR;
	else if (rcode == TRUTH_ANDIF_EXPR)
	  rcode = BIT_AND_EXPR;
	else if (rcode == TRUTH_ORIF_EXPR)
	  rcode = BIT_IOR_EXPR;
	tree op = build_int_cst (unsigned_type_node, rcode);

	if (!var)
	  var = orig;

	incoming = outgoing = var;

	if (!inner)
	  {
	    /* See if an outer construct also reduces this variable.  */
	    omp_context *outer = ctx;

	    while (omp_context *probe = outer->outer)
	      {
		enum gimple_code type = gimple_code (probe->stmt);
		tree cls;

		switch (type)
		  {
		  case GIMPLE_OMP_FOR:
		    cls = gimple_omp_for_clauses (probe->stmt);
		    break;

		  case GIMPLE_OMP_TARGET:
		    if (gimple_omp_target_kind (probe->stmt)
			!= GF_OMP_TARGET_KIND_OACC_PARALLEL)
		      goto do_lookup;

		    cls = gimple_omp_target_clauses (probe->stmt);
		    break;

		  default:
		    goto do_lookup;
		  }

		outer = probe;
		for (; cls;  cls = OMP_CLAUSE_CHAIN (cls))
		  if (OMP_CLAUSE_CODE (cls) == OMP_CLAUSE_REDUCTION
		      && orig == OMP_CLAUSE_DECL (cls))
		    {
		      incoming = outgoing = lookup_decl (orig, probe);
		      goto has_outer_reduction;
		    }
		  else if ((OMP_CLAUSE_CODE (cls) == OMP_CLAUSE_FIRSTPRIVATE
			    || OMP_CLAUSE_CODE (cls) == OMP_CLAUSE_PRIVATE)
			   && orig == OMP_CLAUSE_DECL (cls))
		    {
		      is_private = true;
		      goto do_lookup;
		    }
	      }

	  do_lookup:
	    /* This is the outermost construct with this reduction,
	       see if there's a mapping for it.  */
	    if (gimple_code (outer->stmt) == GIMPLE_OMP_TARGET
		&& maybe_lookup_field (orig, outer) && !is_private)
	      {
		ref_to_res = build_receiver_ref (orig, false, outer);
		if (omp_is_reference (orig))
		  ref_to_res = build_simple_mem_ref (ref_to_res);

		tree type = TREE_TYPE (var);
		if (POINTER_TYPE_P (type))
		  type = TREE_TYPE (type);

		outgoing = var;
		incoming = omp_reduction_init_op (loc, rcode, type);
	      }
	    else
	      {
		/* Try to look at enclosing contexts for reduction var,
		   use original if no mapping found.  */
		tree t = NULL_TREE;
		omp_context *c = ctx->outer;
		while (c && !t)
		  {
		    t = maybe_lookup_decl (orig, c);
		    c = c->outer;
		  }
		incoming = outgoing = (t ? t : orig);
	      }

	  has_outer_reduction:;
	  }

	if (!ref_to_res)
	  ref_to_res = integer_zero_node;

	if (omp_is_reference (orig))
	  {
	    tree type = TREE_TYPE (var);
	    const char *id = IDENTIFIER_POINTER (DECL_NAME (var));

	    if (!inner)
	      {
		tree x = create_tmp_var (TREE_TYPE (type), id);
		gimplify_assign (var, build_fold_addr_expr (x), fork_seq);
	      }

	    v1 = create_tmp_var (type, id);
	    v2 = create_tmp_var (type, id);
	    v3 = create_tmp_var (type, id);

	    gimplify_assign (v1, var, fork_seq);
	    gimplify_assign (v2, var, fork_seq);
	    gimplify_assign (v3, var, fork_seq);

	    var = build_simple_mem_ref (var);
	    v1 = build_simple_mem_ref (v1);
	    v2 = build_simple_mem_ref (v2);
	    v3 = build_simple_mem_ref (v3);
	    outgoing = build_simple_mem_ref (outgoing);

	    if (!TREE_CONSTANT (incoming))
	      incoming = build_simple_mem_ref (incoming);
	  }
	else
	  v1 = v2 = v3 = var;

	/* Determine position in reduction buffer, which may be used
	   by target.  */
	machine_mode mode = TYPE_MODE (TREE_TYPE (var));
	unsigned align = GET_MODE_ALIGNMENT (mode) /  BITS_PER_UNIT;
	offset = (offset + align - 1) & ~(align - 1);
	tree off = build_int_cst (sizetype, offset);
	offset += GET_MODE_SIZE (mode);

	if (!init_code)
	  {
	    init_code = build_int_cst (integer_type_node,
				       IFN_GOACC_REDUCTION_INIT);
	    fini_code = build_int_cst (integer_type_node,
				       IFN_GOACC_REDUCTION_FINI);
	    setup_code = build_int_cst (integer_type_node,
					IFN_GOACC_REDUCTION_SETUP);
	    teardown_code = build_int_cst (integer_type_node,
					   IFN_GOACC_REDUCTION_TEARDOWN);
	  }

	tree setup_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, setup_code,
					  unshare_expr (ref_to_res),
					  incoming, level, op, off);
	tree init_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, init_code,
					  unshare_expr (ref_to_res),
					  v1, level, op, off);
	tree fini_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, fini_code,
					  unshare_expr (ref_to_res),
					  v2, level, op, off);
	tree teardown_call
	  = build_call_expr_internal_loc (loc, IFN_GOACC_REDUCTION,
					  TREE_TYPE (var), 6, teardown_code,
					  ref_to_res, v3, level, op, off);

	gimplify_assign (v1, setup_call, &before_fork);
	gimplify_assign (v2, init_call, &after_fork);
	gimplify_assign (v3, fini_call, &before_join);
	gimplify_assign (outgoing, teardown_call, &after_join);
      }

  /* Now stitch things together.  */
  gimple_seq_add_seq (fork_seq, before_fork);
  if (fork)
    gimple_seq_add_stmt (fork_seq, fork);
  gimple_seq_add_seq (fork_seq, after_fork);

  gimple_seq_add_seq (join_seq, before_join);
  if (join)
    gimple_seq_add_stmt (join_seq, join);
  gimple_seq_add_seq (join_seq, after_join);
}

/* Generate code to implement the REDUCTION clauses.  */

static void
lower_reduction_clauses (tree clauses, gimple_seq *stmt_seqp, omp_context *ctx)
{
  gimple_seq sub_seq = NULL;
  gimple *stmt;
  tree x, c;
  int count = 0;

  /* OpenACC loop reductions are handled elsewhere.  */
  if (is_gimple_omp_oacc (ctx->stmt))
    return;

  /* SIMD reductions are handled in lower_rec_input_clauses.  */
  if (gimple_code (ctx->stmt) == GIMPLE_OMP_FOR
      && gimple_omp_for_kind (ctx->stmt) & GF_OMP_FOR_SIMD)
    return;

  /* First see if there is exactly one reduction clause.  Use OMP_ATOMIC
     update in that case, otherwise use a lock.  */
  for (c = clauses; c && count < 2; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION)
      {
	if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
	    || TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF)
	  {
	    /* Never use OMP_ATOMIC for array reductions or UDRs.  */
	    count = -1;
	    break;
	  }
	count++;
      }

  if (count == 0)
    return;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    {
      tree var, ref, new_var, orig_var;
      enum tree_code code;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION)
	continue;

      enum omp_clause_code ccode = OMP_CLAUSE_REDUCTION;
      orig_var = var = OMP_CLAUSE_DECL (c);
      if (TREE_CODE (var) == MEM_REF)
	{
	  var = TREE_OPERAND (var, 0);
	  if (TREE_CODE (var) == POINTER_PLUS_EXPR)
	    var = TREE_OPERAND (var, 0);
	  if (TREE_CODE (var) == ADDR_EXPR)
	    var = TREE_OPERAND (var, 0);
	  else
	    {
	      /* If this is a pointer or referenced based array
		 section, the var could be private in the outer
		 context e.g. on orphaned loop construct.  Pretend this
		 is private variable's outer reference.  */
	      ccode = OMP_CLAUSE_PRIVATE;
	      if (TREE_CODE (var) == INDIRECT_REF)
		var = TREE_OPERAND (var, 0);
	    }
	  orig_var = var;
	  if (is_variable_sized (var))
	    {
	      gcc_assert (DECL_HAS_VALUE_EXPR_P (var));
	      var = DECL_VALUE_EXPR (var);
	      gcc_assert (TREE_CODE (var) == INDIRECT_REF);
	      var = TREE_OPERAND (var, 0);
	      gcc_assert (DECL_P (var));
	    }
	}
      new_var = lookup_decl (var, ctx);
      if (var == OMP_CLAUSE_DECL (c) && omp_is_reference (var))
	new_var = build_simple_mem_ref_loc (clause_loc, new_var);
      ref = build_outer_var_ref (var, ctx, ccode);
      code = OMP_CLAUSE_REDUCTION_CODE (c);

      /* reduction(-:var) sums up the partial results, so it acts
	 identically to reduction(+:var).  */
      if (code == MINUS_EXPR)
        code = PLUS_EXPR;

      if (count == 1)
	{
	  tree addr = build_fold_addr_expr_loc (clause_loc, ref);

	  addr = save_expr (addr);
	  ref = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (addr)), addr);
	  x = fold_build2_loc (clause_loc, code, TREE_TYPE (ref), ref, new_var);
	  x = build2 (OMP_ATOMIC, void_type_node, addr, x);
	  gimplify_and_add (x, stmt_seqp);
	  return;
	}
      else if (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF)
	{
	  tree d = OMP_CLAUSE_DECL (c);
	  tree type = TREE_TYPE (d);
	  tree v = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	  tree i = create_tmp_var (TREE_TYPE (v), NULL);
	  tree ptype = build_pointer_type (TREE_TYPE (type));
	  tree bias = TREE_OPERAND (d, 1);
	  d = TREE_OPERAND (d, 0);
	  if (TREE_CODE (d) == POINTER_PLUS_EXPR)
	    {
	      tree b = TREE_OPERAND (d, 1);
	      b = maybe_lookup_decl (b, ctx);
	      if (b == NULL)
		{
		  b = TREE_OPERAND (d, 1);
		  b = maybe_lookup_decl_in_outer_ctx (b, ctx);
		}
	      if (integer_zerop (bias))
		bias = b;
	      else
		{
		  bias = fold_convert_loc (clause_loc, TREE_TYPE (b), bias);
		  bias = fold_build2_loc (clause_loc, PLUS_EXPR,
					  TREE_TYPE (b), b, bias);
		}
	      d = TREE_OPERAND (d, 0);
	    }
	  /* For ref build_outer_var_ref already performs this, so
	     only new_var needs a dereference.  */
	  if (TREE_CODE (d) == INDIRECT_REF)
	    {
	      new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	      gcc_assert (omp_is_reference (var) && var == orig_var);
	    }
	  else if (TREE_CODE (d) == ADDR_EXPR)
	    {
	      if (orig_var == var)
		{
		  new_var = build_fold_addr_expr (new_var);
		  ref = build_fold_addr_expr (ref);
		}
	    }
	  else
	    {
	      gcc_assert (orig_var == var);
	      if (omp_is_reference (var))
		ref = build_fold_addr_expr (ref);
	    }
	  if (DECL_P (v))
	    {
	      tree t = maybe_lookup_decl (v, ctx);
	      if (t)
		v = t;
	      else
		v = maybe_lookup_decl_in_outer_ctx (v, ctx);
	      gimplify_expr (&v, stmt_seqp, NULL, is_gimple_val, fb_rvalue);
	    }
	  if (!integer_zerop (bias))
	    {
	      bias = fold_convert_loc (clause_loc, sizetype, bias);
	      new_var = fold_build2_loc (clause_loc, POINTER_PLUS_EXPR,
					 TREE_TYPE (new_var), new_var,
					 unshare_expr (bias));
	      ref = fold_build2_loc (clause_loc, POINTER_PLUS_EXPR,
					 TREE_TYPE (ref), ref, bias);
	    }
	  new_var = fold_convert_loc (clause_loc, ptype, new_var);
	  ref = fold_convert_loc (clause_loc, ptype, ref);
	  tree m = create_tmp_var (ptype, NULL);
	  gimplify_assign (m, new_var, stmt_seqp);
	  new_var = m;
	  m = create_tmp_var (ptype, NULL);
	  gimplify_assign (m, ref, stmt_seqp);
	  ref = m;
	  gimplify_assign (i, build_int_cst (TREE_TYPE (v), 0), stmt_seqp);
	  tree body = create_artificial_label (UNKNOWN_LOCATION);
	  tree end = create_artificial_label (UNKNOWN_LOCATION);
	  gimple_seq_add_stmt (&sub_seq, gimple_build_label (body));
	  tree priv = build_simple_mem_ref_loc (clause_loc, new_var);
	  tree out = build_simple_mem_ref_loc (clause_loc, ref);
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	    {
	      tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);
	      tree decl_placeholder
		= OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c);
	      SET_DECL_VALUE_EXPR (placeholder, out);
	      DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
	      SET_DECL_VALUE_EXPR (decl_placeholder, priv);
	      DECL_HAS_VALUE_EXPR_P (decl_placeholder) = 1;
	      lower_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c), ctx);
	      gimple_seq_add_seq (&sub_seq,
				  OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c));
	      OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
	      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL;
	      OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c) = NULL;
	    }
	  else
	    {
	      x = build2 (code, TREE_TYPE (out), out, priv);
	      out = unshare_expr (out);
	      gimplify_assign (out, x, &sub_seq);
	    }
	  gimple *g = gimple_build_assign (new_var, POINTER_PLUS_EXPR, new_var,
					   TYPE_SIZE_UNIT (TREE_TYPE (type)));
	  gimple_seq_add_stmt (&sub_seq, g);
	  g = gimple_build_assign (ref, POINTER_PLUS_EXPR, ref,
				   TYPE_SIZE_UNIT (TREE_TYPE (type)));
	  gimple_seq_add_stmt (&sub_seq, g);
	  g = gimple_build_assign (i, PLUS_EXPR, i,
				   build_int_cst (TREE_TYPE (i), 1));
	  gimple_seq_add_stmt (&sub_seq, g);
	  g = gimple_build_cond (LE_EXPR, i, v, body, end);
	  gimple_seq_add_stmt (&sub_seq, g);
	  gimple_seq_add_stmt (&sub_seq, gimple_build_label (end));
	}
      else if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
	{
	  tree placeholder = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);

	  if (omp_is_reference (var)
	      && !useless_type_conversion_p (TREE_TYPE (placeholder),
					     TREE_TYPE (ref)))
	    ref = build_fold_addr_expr_loc (clause_loc, ref);
	  SET_DECL_VALUE_EXPR (placeholder, ref);
	  DECL_HAS_VALUE_EXPR_P (placeholder) = 1;
	  lower_omp (&OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c), ctx);
	  gimple_seq_add_seq (&sub_seq, OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c));
	  OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (c) = NULL;
	  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL;
	}
      else
	{
	  x = build2 (code, TREE_TYPE (ref), ref, new_var);
	  ref = build_outer_var_ref (var, ctx);
	  gimplify_assign (ref, x, &sub_seq);
	}
    }

  stmt = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ATOMIC_START),
			    0);
  gimple_seq_add_stmt (stmt_seqp, stmt);

  gimple_seq_add_seq (stmt_seqp, sub_seq);

  stmt = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ATOMIC_END),
			    0);
  gimple_seq_add_stmt (stmt_seqp, stmt);
}


/* Generate code to implement the COPYPRIVATE clauses.  */

static void
lower_copyprivate_clauses (tree clauses, gimple_seq *slist, gimple_seq *rlist,
			    omp_context *ctx)
{
  tree c;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    {
      tree var, new_var, ref, x;
      bool by_ref;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_COPYPRIVATE)
	continue;

      var = OMP_CLAUSE_DECL (c);
      by_ref = use_pointer_for_field (var, NULL);

      ref = build_sender_ref (var, ctx);
      x = new_var = lookup_decl_in_outer_ctx (var, ctx);
      if (by_ref)
	{
	  x = build_fold_addr_expr_loc (clause_loc, new_var);
	  x = fold_convert_loc (clause_loc, TREE_TYPE (ref), x);
	}
      gimplify_assign (ref, x, slist);

      ref = build_receiver_ref (var, false, ctx);
      if (by_ref)
	{
	  ref = fold_convert_loc (clause_loc,
				  build_pointer_type (TREE_TYPE (new_var)),
				  ref);
	  ref = build_fold_indirect_ref_loc (clause_loc, ref);
	}
      if (omp_is_reference (var))
	{
	  ref = fold_convert_loc (clause_loc, TREE_TYPE (new_var), ref);
	  ref = build_simple_mem_ref_loc (clause_loc, ref);
	  new_var = build_simple_mem_ref_loc (clause_loc, new_var);
	}
      x = lang_hooks.decls.omp_clause_assign_op (c, new_var, ref);
      gimplify_and_add (x, rlist);
    }
}


/* Generate code to implement the clauses, FIRSTPRIVATE, COPYIN, LASTPRIVATE,
   and REDUCTION from the sender (aka parent) side.  */

static void
lower_send_clauses (tree clauses, gimple_seq *ilist, gimple_seq *olist,
    		    omp_context *ctx)
{
  tree c, t;
  int ignored_looptemp = 0;
  bool is_taskloop = false;

  /* For taskloop, ignore first two _looptemp_ clauses, those are initialized
     by GOMP_taskloop.  */
  if (is_task_ctx (ctx) && gimple_omp_task_taskloop_p (ctx->stmt))
    {
      ignored_looptemp = 2;
      is_taskloop = true;
    }

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    {
      tree val, ref, x, var;
      bool by_ref, do_in = false, do_out = false;
      location_t clause_loc = OMP_CLAUSE_LOCATION (c);

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_PRIVATE:
	  if (OMP_CLAUSE_PRIVATE_OUTER_REF (c))
	    break;
	  continue;
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_LASTPRIVATE:
	case OMP_CLAUSE_REDUCTION:
	  break;
	case OMP_CLAUSE_SHARED:
	  if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	    break;
	  continue;
	case OMP_CLAUSE__LOOPTEMP_:
	  if (ignored_looptemp)
	    {
	      ignored_looptemp--;
	      continue;
	    }
	  break;
	default:
	  continue;
	}

      val = OMP_CLAUSE_DECL (c);
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	  && TREE_CODE (val) == MEM_REF)
	{
	  val = TREE_OPERAND (val, 0);
	  if (TREE_CODE (val) == POINTER_PLUS_EXPR)
	    val = TREE_OPERAND (val, 0);
	  if (TREE_CODE (val) == INDIRECT_REF
	      || TREE_CODE (val) == ADDR_EXPR)
	    val = TREE_OPERAND (val, 0);
	  if (is_variable_sized (val))
	    continue;
	}

      /* For OMP_CLAUSE_SHARED_FIRSTPRIVATE, look beyond the
	 outer taskloop region.  */
      omp_context *ctx_for_o = ctx;
      if (is_taskloop
	  && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
	  && OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	ctx_for_o = ctx->outer;

      var = lookup_decl_in_outer_ctx (val, ctx_for_o);

      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_COPYIN
	  && is_global_var (var))
	continue;

      t = omp_member_access_dummy_var (var);
      if (t)
	{
	  var = DECL_VALUE_EXPR (var);
	  tree o = maybe_lookup_decl_in_outer_ctx (t, ctx_for_o);
	  if (o != t)
	    var = unshare_and_remap (var, t, o);
	  else
	    var = unshare_expr (var);
	}

      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED)
	{
	  /* Handle taskloop firstprivate/lastprivate, where the
	     lastprivate on GIMPLE_OMP_TASK is represented as
	     OMP_CLAUSE_SHARED_FIRSTPRIVATE.  */
	  tree f = lookup_sfield ((splay_tree_key) &DECL_UID (val), ctx);
	  x = omp_build_component_ref (ctx->sender_decl, f);
	  if (use_pointer_for_field (val, ctx))
	    var = build_fold_addr_expr (var);
	  gimplify_assign (x, var, ilist);
	  DECL_ABSTRACT_ORIGIN (f) = NULL;
	  continue;
	}

      if ((OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION
	   || val == OMP_CLAUSE_DECL (c))
	  && is_variable_sized (val))
	continue;
      by_ref = use_pointer_for_field (val, NULL);

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_FIRSTPRIVATE:
	  if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c)
	      && !by_ref
	      && is_task_ctx (ctx))
	    TREE_NO_WARNING (var) = 1;
	  do_in = true;
	  break;

	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE__LOOPTEMP_:
	  do_in = true;
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (by_ref || omp_is_reference (val))
	    {
	      if (OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
		continue;
	      do_in = true;
	    }
	  else
	    {
	      do_out = true;
	      if (lang_hooks.decls.omp_private_outer_ref (val))
		do_in = true;
	    }
	  break;

	case OMP_CLAUSE_REDUCTION:
	  do_in = true;
	  if (val == OMP_CLAUSE_DECL (c))
	    do_out = !(by_ref || omp_is_reference (val));
	  else
	    by_ref = TREE_CODE (TREE_TYPE (val)) == ARRAY_TYPE;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (do_in)
	{
	  ref = build_sender_ref (val, ctx);
	  x = by_ref ? build_fold_addr_expr_loc (clause_loc, var) : var;
	  gimplify_assign (ref, x, ilist);
	  if (is_task_ctx (ctx))
	    DECL_ABSTRACT_ORIGIN (TREE_OPERAND (ref, 1)) = NULL;
	}

      if (do_out)
	{
	  ref = build_sender_ref (val, ctx);
	  gimplify_assign (var, ref, olist);
	}
    }
}

/* Generate code to implement SHARED from the sender (aka parent)
   side.  This is trickier, since GIMPLE_OMP_PARALLEL_CLAUSES doesn't
   list things that got automatically shared.  */

static void
lower_send_shared_vars (gimple_seq *ilist, gimple_seq *olist, omp_context *ctx)
{
  tree var, ovar, nvar, t, f, x, record_type;

  if (ctx->record_type == NULL)
    return;

  record_type = ctx->srecord_type ? ctx->srecord_type : ctx->record_type;
  for (f = TYPE_FIELDS (record_type); f ; f = DECL_CHAIN (f))
    {
      ovar = DECL_ABSTRACT_ORIGIN (f);
      if (!ovar || TREE_CODE (ovar) == FIELD_DECL)
	continue;

      nvar = maybe_lookup_decl (ovar, ctx);
      if (!nvar || !DECL_HAS_VALUE_EXPR_P (nvar))
	continue;

      /* If CTX is a nested parallel directive.  Find the immediately
	 enclosing parallel or workshare construct that contains a
	 mapping for OVAR.  */
      var = lookup_decl_in_outer_ctx (ovar, ctx);

      t = omp_member_access_dummy_var (var);
      if (t)
	{
	  var = DECL_VALUE_EXPR (var);
	  tree o = maybe_lookup_decl_in_outer_ctx (t, ctx);
	  if (o != t)
	    var = unshare_and_remap (var, t, o);
	  else
	    var = unshare_expr (var);
	}

      if (use_pointer_for_field (ovar, ctx))
	{
	  x = build_sender_ref (ovar, ctx);
	  var = build_fold_addr_expr (var);
	  gimplify_assign (x, var, ilist);
	}
      else
	{
	  x = build_sender_ref (ovar, ctx);
	  gimplify_assign (x, var, ilist);

	  if (!TREE_READONLY (var)
	      /* We don't need to receive a new reference to a result
	         or parm decl.  In fact we may not store to it as we will
		 invalidate any pending RSO and generate wrong gimple
		 during inlining.  */
	      && !((TREE_CODE (var) == RESULT_DECL
		    || TREE_CODE (var) == PARM_DECL)
		   && DECL_BY_REFERENCE (var)))
	    {
	      x = build_sender_ref (ovar, ctx);
	      gimplify_assign (var, x, olist);
	    }
	}
    }
}

/* Emit an OpenACC head marker call, encapulating the partitioning and
   other information that must be processed by the target compiler.
   Return the maximum number of dimensions the associated loop might
   be partitioned over.  */

static unsigned
lower_oacc_head_mark (location_t loc, tree ddvar, tree clauses,
		      gimple_seq *seq, omp_context *ctx)
{
  unsigned levels = 0;
  unsigned tag = 0;
  tree gang_static = NULL_TREE;
  auto_vec<tree, 5> args;

  args.quick_push (build_int_cst
		   (integer_type_node, IFN_UNIQUE_OACC_HEAD_MARK));
  args.quick_push (ddvar);
  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_GANG:
	  tag |= OLF_DIM_GANG;
	  gang_static = OMP_CLAUSE_GANG_STATIC_EXPR (c);
	  /* static:* is represented by -1, and we can ignore it, as
	     scheduling is always static.  */
	  if (gang_static && integer_minus_onep (gang_static))
	    gang_static = NULL_TREE;
	  levels++;
	  break;

	case OMP_CLAUSE_WORKER:
	  tag |= OLF_DIM_WORKER;
	  levels++;
	  break;

	case OMP_CLAUSE_VECTOR:
	  tag |= OLF_DIM_VECTOR;
	  levels++;
	  break;

	case OMP_CLAUSE_SEQ:
	  tag |= OLF_SEQ;
	  break;

	case OMP_CLAUSE_AUTO:
	  tag |= OLF_AUTO;
	  break;

	case OMP_CLAUSE_INDEPENDENT:
	  tag |= OLF_INDEPENDENT;
	  break;

	case OMP_CLAUSE_TILE:
	  tag |= OLF_TILE;
	  break;

	default:
	  continue;
	}
    }

  if (gang_static)
    {
      if (DECL_P (gang_static))
	gang_static = build_outer_var_ref (gang_static, ctx);
      tag |= OLF_GANG_STATIC;
    }

  /* In a parallel region, loops are implicitly INDEPENDENT.  */
  omp_context *tgt = enclosing_target_ctx (ctx);
  if (!tgt || is_oacc_parallel (tgt))
    tag |= OLF_INDEPENDENT;

  if (tag & OLF_TILE)
    /* Tiling could use all 3 levels.  */ 
    levels = 3;
  else
    {
      /* A loop lacking SEQ, GANG, WORKER and/or VECTOR could be AUTO.
	 Ensure at least one level, or 2 for possible auto
	 partitioning */
      bool maybe_auto = !(tag & (((GOMP_DIM_MASK (GOMP_DIM_MAX) - 1)
				  << OLF_DIM_BASE) | OLF_SEQ));

      if (levels < 1u + maybe_auto)
	levels = 1u + maybe_auto;
    }

  args.quick_push (build_int_cst (integer_type_node, levels));
  args.quick_push (build_int_cst (integer_type_node, tag));
  if (gang_static)
    args.quick_push (gang_static);

  gcall *call = gimple_build_call_internal_vec (IFN_UNIQUE, args);
  gimple_set_location (call, loc);
  gimple_set_lhs (call, ddvar);
  gimple_seq_add_stmt (seq, call);

  return levels;
}

/* Emit an OpenACC lopp head or tail marker to SEQ.  LEVEL is the
   partitioning level of the enclosed region.  */ 

static void
lower_oacc_loop_marker (location_t loc, tree ddvar, bool head,
			tree tofollow, gimple_seq *seq)
{
  int marker_kind = (head ? IFN_UNIQUE_OACC_HEAD_MARK
		     : IFN_UNIQUE_OACC_TAIL_MARK);
  tree marker = build_int_cst (integer_type_node, marker_kind);
  int nargs = 2 + (tofollow != NULL_TREE);
  gcall *call = gimple_build_call_internal (IFN_UNIQUE, nargs,
					    marker, ddvar, tofollow);
  gimple_set_location (call, loc);
  gimple_set_lhs (call, ddvar);
  gimple_seq_add_stmt (seq, call);
}

/* Generate the before and after OpenACC loop sequences.  CLAUSES are
   the loop clauses, from which we extract reductions.  Initialize
   HEAD and TAIL.  */

static void
lower_oacc_head_tail (location_t loc, tree clauses,
		      gimple_seq *head, gimple_seq *tail, omp_context *ctx)
{
  bool inner = false;
  tree ddvar = create_tmp_var (integer_type_node, ".data_dep");
  gimple_seq_add_stmt (head, gimple_build_assign (ddvar, integer_zero_node));

  unsigned count = lower_oacc_head_mark (loc, ddvar, clauses, head, ctx);
  tree fork_kind = build_int_cst (unsigned_type_node, IFN_UNIQUE_OACC_FORK);
  tree join_kind = build_int_cst (unsigned_type_node, IFN_UNIQUE_OACC_JOIN);

  gcc_assert (count);
  for (unsigned done = 1; count; count--, done++)
    {
      gimple_seq fork_seq = NULL;
      gimple_seq join_seq = NULL;

      tree place = build_int_cst (integer_type_node, -1);
      gcall *fork = gimple_build_call_internal (IFN_UNIQUE, 3,
						fork_kind, ddvar, place);
      gimple_set_location (fork, loc);
      gimple_set_lhs (fork, ddvar);

      gcall *join = gimple_build_call_internal (IFN_UNIQUE, 3,
						join_kind, ddvar, place);
      gimple_set_location (join, loc);
      gimple_set_lhs (join, ddvar);

      /* Mark the beginning of this level sequence.  */
      if (inner)
	lower_oacc_loop_marker (loc, ddvar, true,
				build_int_cst (integer_type_node, count),
				&fork_seq);
      lower_oacc_loop_marker (loc, ddvar, false,
			      build_int_cst (integer_type_node, done),
			      &join_seq);

      lower_oacc_reductions (loc, clauses, place, inner,
			     fork, join, &fork_seq, &join_seq,  ctx);

      /* Append this level to head. */
      gimple_seq_add_seq (head, fork_seq);
      /* Prepend it to tail.  */
      gimple_seq_add_seq (&join_seq, *tail);
      *tail = join_seq;

      inner = true;
    }

  /* Mark the end of the sequence.  */
  lower_oacc_loop_marker (loc, ddvar, true, NULL_TREE, head);
  lower_oacc_loop_marker (loc, ddvar, false, NULL_TREE, tail);
}

/* If exceptions are enabled, wrap the statements in BODY in a MUST_NOT_THROW
   catch handler and return it.  This prevents programs from violating the
   structured block semantics with throws.  */

static gimple_seq
maybe_catch_exception (gimple_seq body)
{
  gimple *g;
  tree decl;

  if (!flag_exceptions)
    return body;

  if (lang_hooks.eh_protect_cleanup_actions != NULL)
    decl = lang_hooks.eh_protect_cleanup_actions ();
  else
    decl = builtin_decl_explicit (BUILT_IN_TRAP);

  g = gimple_build_eh_must_not_throw (decl);
  g = gimple_build_try (body, gimple_seq_alloc_with_stmt (g),
      			GIMPLE_TRY_CATCH);

 return gimple_seq_alloc_with_stmt (g);
}


/* Routines to lower OMP directives into OMP-GIMPLE.  */

/* If ctx is a worksharing context inside of a cancellable parallel
   region and it isn't nowait, add lhs to its GIMPLE_OMP_RETURN
   and conditional branch to parallel's cancel_label to handle
   cancellation in the implicit barrier.  */

static void
maybe_add_implicit_barrier_cancel (omp_context *ctx, gimple_seq *body)
{
  gimple *omp_return = gimple_seq_last_stmt (*body);
  gcc_assert (gimple_code (omp_return) == GIMPLE_OMP_RETURN);
  if (gimple_omp_return_nowait_p (omp_return))
    return;
  if (ctx->outer
      && gimple_code (ctx->outer->stmt) == GIMPLE_OMP_PARALLEL
      && ctx->outer->cancellable)
    {
      tree fndecl = builtin_decl_explicit (BUILT_IN_GOMP_CANCEL);
      tree c_bool_type = TREE_TYPE (TREE_TYPE (fndecl));
      tree lhs = create_tmp_var (c_bool_type);
      gimple_omp_return_set_lhs (omp_return, lhs);
      tree fallthru_label = create_artificial_label (UNKNOWN_LOCATION);
      gimple *g = gimple_build_cond (NE_EXPR, lhs,
				    fold_convert (c_bool_type,
						  boolean_false_node),
				    ctx->outer->cancel_label, fallthru_label);
      gimple_seq_add_stmt (body, g);
      gimple_seq_add_stmt (body, gimple_build_label (fallthru_label));
    }
}

/* Lower the OpenMP sections directive in the current statement in GSI_P.
   CTX is the enclosing OMP context for the current statement.  */

static void
lower_omp_sections (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block, control;
  gimple_stmt_iterator tgsi;
  gomp_sections *stmt;
  gimple *t;
  gbind *new_stmt, *bind;
  gimple_seq ilist, dlist, olist, new_body;

  stmt = as_a <gomp_sections *> (gsi_stmt (*gsi_p));

  push_gimplify_context ();

  dlist = NULL;
  ilist = NULL;
  lower_rec_input_clauses (gimple_omp_sections_clauses (stmt),
      			   &ilist, &dlist, ctx, NULL);

  new_body = gimple_omp_body (stmt);
  gimple_omp_set_body (stmt, NULL);
  tgsi = gsi_start (new_body);
  for (; !gsi_end_p (tgsi); gsi_next (&tgsi))
    {
      omp_context *sctx;
      gimple *sec_start;

      sec_start = gsi_stmt (tgsi);
      sctx = maybe_lookup_ctx (sec_start);
      gcc_assert (sctx);

      lower_omp (gimple_omp_body_ptr (sec_start), sctx);
      gsi_insert_seq_after (&tgsi, gimple_omp_body (sec_start),
			    GSI_CONTINUE_LINKING);
      gimple_omp_set_body (sec_start, NULL);

      if (gsi_one_before_end_p (tgsi))
	{
	  gimple_seq l = NULL;
	  lower_lastprivate_clauses (gimple_omp_sections_clauses (stmt), NULL,
				     &l, ctx);
	  gsi_insert_seq_after (&tgsi, l, GSI_CONTINUE_LINKING);
	  gimple_omp_section_set_last (sec_start);
	}

      gsi_insert_after (&tgsi, gimple_build_omp_return (false),
			GSI_CONTINUE_LINKING);
    }

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, new_body, block);

  olist = NULL;
  lower_reduction_clauses (gimple_omp_sections_clauses (stmt), &olist, ctx);

  block = make_node (BLOCK);
  new_stmt = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, new_stmt, true);

  pop_gimplify_context (new_stmt);
  gimple_bind_append_vars (new_stmt, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (bind);
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;

  new_body = NULL;
  gimple_seq_add_seq (&new_body, ilist);
  gimple_seq_add_stmt (&new_body, stmt);
  gimple_seq_add_stmt (&new_body, gimple_build_omp_sections_switch ());
  gimple_seq_add_stmt (&new_body, bind);

  control = create_tmp_var (unsigned_type_node, ".section");
  t = gimple_build_omp_continue (control, control);
  gimple_omp_sections_set_control (stmt, control);
  gimple_seq_add_stmt (&new_body, t);

  gimple_seq_add_seq (&new_body, olist);
  if (ctx->cancellable)
    gimple_seq_add_stmt (&new_body, gimple_build_label (ctx->cancel_label));
  gimple_seq_add_seq (&new_body, dlist);

  new_body = maybe_catch_exception (new_body);

  bool nowait = omp_find_clause (gimple_omp_sections_clauses (stmt),
				 OMP_CLAUSE_NOWAIT) != NULL_TREE;
  t = gimple_build_omp_return (nowait);
  gimple_seq_add_stmt (&new_body, t);
  maybe_add_implicit_barrier_cancel (ctx, &new_body);

  gimple_bind_set_body (new_stmt, new_body);
}


/* A subroutine of lower_omp_single.  Expand the simple form of
   a GIMPLE_OMP_SINGLE, without a copyprivate clause:

     	if (GOMP_single_start ())
	  BODY;
	[ GOMP_barrier (); ]	-> unless 'nowait' is present.

  FIXME.  It may be better to delay expanding the logic of this until
  pass_expand_omp.  The expanded logic may make the job more difficult
  to a synchronization analysis pass.  */

static void
lower_omp_single_simple (gomp_single *single_stmt, gimple_seq *pre_p)
{
  location_t loc = gimple_location (single_stmt);
  tree tlabel = create_artificial_label (loc);
  tree flabel = create_artificial_label (loc);
  gimple *call, *cond;
  tree lhs, decl;

  decl = builtin_decl_explicit (BUILT_IN_GOMP_SINGLE_START);
  lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (decl)));
  call = gimple_build_call (decl, 0);
  gimple_call_set_lhs (call, lhs);
  gimple_seq_add_stmt (pre_p, call);

  cond = gimple_build_cond (EQ_EXPR, lhs,
			    fold_convert_loc (loc, TREE_TYPE (lhs),
					      boolean_true_node),
			    tlabel, flabel);
  gimple_seq_add_stmt (pre_p, cond);
  gimple_seq_add_stmt (pre_p, gimple_build_label (tlabel));
  gimple_seq_add_seq (pre_p, gimple_omp_body (single_stmt));
  gimple_seq_add_stmt (pre_p, gimple_build_label (flabel));
}


/* A subroutine of lower_omp_single.  Expand the simple form of
   a GIMPLE_OMP_SINGLE, with a copyprivate clause:

	#pragma omp single copyprivate (a, b, c)

   Create a new structure to hold copies of 'a', 'b' and 'c' and emit:

      {
	if ((copyout_p = GOMP_single_copy_start ()) == NULL)
	  {
	    BODY;
	    copyout.a = a;
	    copyout.b = b;
	    copyout.c = c;
	    GOMP_single_copy_end (&copyout);
	  }
	else
	  {
	    a = copyout_p->a;
	    b = copyout_p->b;
	    c = copyout_p->c;
	  }
	GOMP_barrier ();
      }

  FIXME.  It may be better to delay expanding the logic of this until
  pass_expand_omp.  The expanded logic may make the job more difficult
  to a synchronization analysis pass.  */

static void
lower_omp_single_copy (gomp_single *single_stmt, gimple_seq *pre_p,
		       omp_context *ctx)
{
  tree ptr_type, t, l0, l1, l2, bfn_decl;
  gimple_seq copyin_seq;
  location_t loc = gimple_location (single_stmt);

  ctx->sender_decl = create_tmp_var (ctx->record_type, ".omp_copy_o");

  ptr_type = build_pointer_type (ctx->record_type);
  ctx->receiver_decl = create_tmp_var (ptr_type, ".omp_copy_i");

  l0 = create_artificial_label (loc);
  l1 = create_artificial_label (loc);
  l2 = create_artificial_label (loc);

  bfn_decl = builtin_decl_explicit (BUILT_IN_GOMP_SINGLE_COPY_START);
  t = build_call_expr_loc (loc, bfn_decl, 0);
  t = fold_convert_loc (loc, ptr_type, t);
  gimplify_assign (ctx->receiver_decl, t, pre_p);

  t = build2 (EQ_EXPR, boolean_type_node, ctx->receiver_decl,
	      build_int_cst (ptr_type, 0));
  t = build3 (COND_EXPR, void_type_node, t,
	      build_and_jump (&l0), build_and_jump (&l1));
  gimplify_and_add (t, pre_p);

  gimple_seq_add_stmt (pre_p, gimple_build_label (l0));

  gimple_seq_add_seq (pre_p, gimple_omp_body (single_stmt));

  copyin_seq = NULL;
  lower_copyprivate_clauses (gimple_omp_single_clauses (single_stmt), pre_p,
			      &copyin_seq, ctx);

  t = build_fold_addr_expr_loc (loc, ctx->sender_decl);
  bfn_decl = builtin_decl_explicit (BUILT_IN_GOMP_SINGLE_COPY_END);
  t = build_call_expr_loc (loc, bfn_decl, 1, t);
  gimplify_and_add (t, pre_p);

  t = build_and_jump (&l2);
  gimplify_and_add (t, pre_p);

  gimple_seq_add_stmt (pre_p, gimple_build_label (l1));

  gimple_seq_add_seq (pre_p, copyin_seq);

  gimple_seq_add_stmt (pre_p, gimple_build_label (l2));
}


/* Expand code for an OpenMP single directive.  */

static void
lower_omp_single (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block;
  gomp_single *single_stmt = as_a <gomp_single *> (gsi_stmt (*gsi_p));
  gbind *bind;
  gimple_seq bind_body, bind_body_tail = NULL, dlist;

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  bind_body = NULL;
  dlist = NULL;
  lower_rec_input_clauses (gimple_omp_single_clauses (single_stmt),
			   &bind_body, &dlist, ctx, NULL);
  lower_omp (gimple_omp_body_ptr (single_stmt), ctx);

  gimple_seq_add_stmt (&bind_body, single_stmt);

  if (ctx->record_type)
    lower_omp_single_copy (single_stmt, &bind_body, ctx);
  else
    lower_omp_single_simple (single_stmt, &bind_body);

  gimple_omp_set_body (single_stmt, NULL);

  gimple_seq_add_seq (&bind_body, dlist);

  bind_body = maybe_catch_exception (bind_body);

  bool nowait = omp_find_clause (gimple_omp_single_clauses (single_stmt),
				 OMP_CLAUSE_NOWAIT) != NULL_TREE;
  gimple *g = gimple_build_omp_return (nowait);
  gimple_seq_add_stmt (&bind_body_tail, g);
  maybe_add_implicit_barrier_cancel (ctx, &bind_body_tail);
  if (ctx->record_type)
    {
      gimple_stmt_iterator gsi = gsi_start (bind_body_tail);
      tree clobber = build_constructor (ctx->record_type, NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gsi_insert_after (&gsi, gimple_build_assign (ctx->sender_decl,
						   clobber), GSI_SAME_STMT);
    }
  gimple_seq_add_seq (&bind_body, bind_body_tail);
  gimple_bind_set_body (bind, bind_body);

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;
}


/* Expand code for an OpenMP master directive.  */

static void
lower_omp_master (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block, lab = NULL, x, bfn_decl;
  gimple *stmt = gsi_stmt (*gsi_p);
  gbind *bind;
  location_t loc = gimple_location (stmt);
  gimple_seq tseq;

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  bfn_decl = builtin_decl_explicit (BUILT_IN_OMP_GET_THREAD_NUM);
  x = build_call_expr_loc (loc, bfn_decl, 0);
  x = build2 (EQ_EXPR, boolean_type_node, x, integer_zero_node);
  x = build3 (COND_EXPR, void_type_node, x, NULL, build_and_jump (&lab));
  tseq = NULL;
  gimplify_and_add (x, &tseq);
  gimple_bind_add_seq (bind, tseq);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_omp_set_body (stmt, maybe_catch_exception (gimple_omp_body (stmt)));
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  gimple_bind_add_stmt (bind, gimple_build_label (lab));

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
}


/* Expand code for an OpenMP taskgroup directive.  */

static void
lower_omp_taskgroup (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  gcall *x;
  gbind *bind;
  tree block = make_node (BLOCK);

  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  x = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_TASKGROUP_START),
			 0);
  gimple_bind_add_stmt (bind, x);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
}


/* Fold the OMP_ORDERED_CLAUSES for the OMP_ORDERED in STMT if possible.  */

static void
lower_omp_ordered_clauses (gimple_stmt_iterator *gsi_p, gomp_ordered *ord_stmt,
			   omp_context *ctx)
{
  struct omp_for_data fd;
  if (!ctx->outer || gimple_code (ctx->outer->stmt) != GIMPLE_OMP_FOR)
    return;

  unsigned int len = gimple_omp_for_collapse (ctx->outer->stmt);
  struct omp_for_data_loop *loops = XALLOCAVEC (struct omp_for_data_loop, len);
  omp_extract_for_data (as_a <gomp_for *> (ctx->outer->stmt), &fd, loops);
  if (!fd.ordered)
    return;

  tree *list_p = gimple_omp_ordered_clauses_ptr (ord_stmt);
  tree c = gimple_omp_ordered_clauses (ord_stmt);
  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
      && OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_SINK)
    {
      /* Merge depend clauses from multiple adjacent
	 #pragma omp ordered depend(sink:...) constructs
	 into one #pragma omp ordered depend(sink:...), so that
	 we can optimize them together.  */
      gimple_stmt_iterator gsi = *gsi_p;
      gsi_next (&gsi);
      while (!gsi_end_p (gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt)
	      || gimple_code (stmt) == GIMPLE_NOP)
	    {
	      gsi_next (&gsi);
	      continue;
	    }
	  if (gimple_code (stmt) != GIMPLE_OMP_ORDERED)
	    break;
	  gomp_ordered *ord_stmt2 = as_a <gomp_ordered *> (stmt);
	  c = gimple_omp_ordered_clauses (ord_stmt2);
	  if (c == NULL_TREE
	      || OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	      || OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_SINK)
	    break;
	  while (*list_p)
	    list_p = &OMP_CLAUSE_CHAIN (*list_p);
	  *list_p = c;
	  gsi_remove (&gsi, true);
	}
    }

  /* Canonicalize sink dependence clauses into one folded clause if
     possible.

     The basic algorithm is to create a sink vector whose first
     element is the GCD of all the first elements, and whose remaining
     elements are the minimum of the subsequent columns.

     We ignore dependence vectors whose first element is zero because
     such dependencies are known to be executed by the same thread.

     We take into account the direction of the loop, so a minimum
     becomes a maximum if the loop is iterating forwards.  We also
     ignore sink clauses where the loop direction is unknown, or where
     the offsets are clearly invalid because they are not a multiple
     of the loop increment.

     For example:

	#pragma omp for ordered(2)
	for (i=0; i < N; ++i)
	  for (j=0; j < M; ++j)
	    {
	      #pragma omp ordered \
		depend(sink:i-8,j-2) \
		depend(sink:i,j-1) \	// Completely ignored because i+0.
		depend(sink:i-4,j-3) \
		depend(sink:i-6,j-4)
	      #pragma omp ordered depend(source)
	    }

     Folded clause is:

	depend(sink:-gcd(8,4,6),-min(2,3,4))
	  -or-
	depend(sink:-2,-2)
  */

  /* FIXME: Computing GCD's where the first element is zero is
     non-trivial in the presence of collapsed loops.  Do this later.  */
  if (fd.collapse > 1)
    return;

  wide_int *folded_deps = XALLOCAVEC (wide_int, 2 * len - 1);

  /* wide_int is not a POD so it must be default-constructed.  */
  for (unsigned i = 0; i != 2 * len - 1; ++i)
    new (static_cast<void*>(folded_deps + i)) wide_int ();

  tree folded_dep = NULL_TREE;
  /* TRUE if the first dimension's offset is negative.  */
  bool neg_offset_p = false;

  list_p = gimple_omp_ordered_clauses_ptr (ord_stmt);
  unsigned int i;
  while ((c = *list_p) != NULL)
    {
      bool remove = false;

      gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND);
      if (OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_SINK)
	goto next_ordered_clause;

      tree vec;
      for (vec = OMP_CLAUSE_DECL (c), i = 0;
	   vec && TREE_CODE (vec) == TREE_LIST;
	   vec = TREE_CHAIN (vec), ++i)
	{
	  gcc_assert (i < len);

	  /* omp_extract_for_data has canonicalized the condition.  */
	  gcc_assert (fd.loops[i].cond_code == LT_EXPR
		      || fd.loops[i].cond_code == GT_EXPR);
	  bool forward = fd.loops[i].cond_code == LT_EXPR;
	  bool maybe_lexically_later = true;

	  /* While the committee makes up its mind, bail if we have any
	     non-constant steps.  */
	  if (TREE_CODE (fd.loops[i].step) != INTEGER_CST)
	    goto lower_omp_ordered_ret;

	  tree itype = TREE_TYPE (TREE_VALUE (vec));
	  if (POINTER_TYPE_P (itype))
	    itype = sizetype;
	  wide_int offset = wide_int::from (TREE_PURPOSE (vec),
					    TYPE_PRECISION (itype),
					    TYPE_SIGN (itype));

	  /* Ignore invalid offsets that are not multiples of the step.  */
	  if (!wi::multiple_of_p
	      (wi::abs (offset), wi::abs ((wide_int) fd.loops[i].step),
	       UNSIGNED))
	    {
	      warning_at (OMP_CLAUSE_LOCATION (c), 0,
			  "ignoring sink clause with offset that is not "
			  "a multiple of the loop step");
	      remove = true;
	      goto next_ordered_clause;
	    }

	  /* Calculate the first dimension.  The first dimension of
	     the folded dependency vector is the GCD of the first
	     elements, while ignoring any first elements whose offset
	     is 0.  */
	  if (i == 0)
	    {
	      /* Ignore dependence vectors whose first dimension is 0.  */
	      if (offset == 0)
		{
		  remove = true;
		  goto next_ordered_clause;
		}
	      else
		{
		  if (!TYPE_UNSIGNED (itype) && (forward ^ wi::neg_p (offset)))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"first offset must be in opposite direction "
				"of loop iterations");
		      goto lower_omp_ordered_ret;
		    }
		  if (forward)
		    offset = -offset;
		  neg_offset_p = forward;
		  /* Initialize the first time around.  */
		  if (folded_dep == NULL_TREE)
		    {
		      folded_dep = c;
		      folded_deps[0] = offset;
		    }
		  else
		    folded_deps[0] = wi::gcd (folded_deps[0],
					      offset, UNSIGNED);
		}
	    }
	  /* Calculate minimum for the remaining dimensions.  */
	  else
	    {
	      folded_deps[len + i - 1] = offset;
	      if (folded_dep == c)
		folded_deps[i] = offset;
	      else if (maybe_lexically_later
		       && !wi::eq_p (folded_deps[i], offset))
		{
		  if (forward ^ wi::gts_p (folded_deps[i], offset))
		    {
		      unsigned int j;
		      folded_dep = c;
		      for (j = 1; j <= i; j++)
			folded_deps[j] = folded_deps[len + j - 1];
		    }
		  else
		    maybe_lexically_later = false;
		}
	    }
	}
      gcc_assert (i == len);

      remove = true;

    next_ordered_clause:
      if (remove)
	*list_p = OMP_CLAUSE_CHAIN (c);
      else
	list_p = &OMP_CLAUSE_CHAIN (c);
    }

  if (folded_dep)
    {
      if (neg_offset_p)
	folded_deps[0] = -folded_deps[0];

      tree itype = TREE_TYPE (TREE_VALUE (OMP_CLAUSE_DECL (folded_dep)));
      if (POINTER_TYPE_P (itype))
	itype = sizetype;

      TREE_PURPOSE (OMP_CLAUSE_DECL (folded_dep))
	= wide_int_to_tree (itype, folded_deps[0]);
      OMP_CLAUSE_CHAIN (folded_dep) = gimple_omp_ordered_clauses (ord_stmt);
      *gimple_omp_ordered_clauses_ptr (ord_stmt) = folded_dep;
    }

 lower_omp_ordered_ret:

  /* Ordered without clauses is #pragma omp threads, while we want
     a nop instead if we remove all clauses.  */
  if (gimple_omp_ordered_clauses (ord_stmt) == NULL_TREE)
    gsi_replace (gsi_p, gimple_build_nop (), true);
}


/* Expand code for an OpenMP ordered directive.  */

static void
lower_omp_ordered (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block;
  gimple *stmt = gsi_stmt (*gsi_p), *g;
  gomp_ordered *ord_stmt = as_a <gomp_ordered *> (stmt);
  gcall *x;
  gbind *bind;
  bool simd = omp_find_clause (gimple_omp_ordered_clauses (ord_stmt),
			       OMP_CLAUSE_SIMD);
  /* FIXME: this should check presence of OMP_CLAUSE__SIMT_ on the enclosing
     loop.  */
  bool maybe_simt
    = simd && omp_maybe_offloaded_ctx (ctx) && omp_max_simt_vf () > 1;
  bool threads = omp_find_clause (gimple_omp_ordered_clauses (ord_stmt),
				  OMP_CLAUSE_THREADS);

  if (omp_find_clause (gimple_omp_ordered_clauses (ord_stmt),
		       OMP_CLAUSE_DEPEND))
    {
      /* FIXME: This is needs to be moved to the expansion to verify various
	 conditions only testable on cfg with dominators computed, and also
	 all the depend clauses to be merged still might need to be available
	 for the runtime checks.  */
      if (0)
	lower_omp_ordered_clauses (gsi_p, ord_stmt, ctx);
      return;
    }

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  if (simd)
    {
      x = gimple_build_call_internal (IFN_GOMP_SIMD_ORDERED_START, 1,
				      build_int_cst (NULL_TREE, threads));
      cfun->has_simduid_loops = true;
    }
  else
    x = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ORDERED_START),
			   0);
  gimple_bind_add_stmt (bind, x);

  tree counter = NULL_TREE, test = NULL_TREE, body = NULL_TREE;
  if (maybe_simt)
    {
      counter = create_tmp_var (integer_type_node);
      g = gimple_build_call_internal (IFN_GOMP_SIMT_LANE, 0);
      gimple_call_set_lhs (g, counter);
      gimple_bind_add_stmt (bind, g);

      body = create_artificial_label (UNKNOWN_LOCATION);
      test = create_artificial_label (UNKNOWN_LOCATION);
      gimple_bind_add_stmt (bind, gimple_build_label (body));

      tree simt_pred = create_tmp_var (integer_type_node);
      g = gimple_build_call_internal (IFN_GOMP_SIMT_ORDERED_PRED, 1, counter);
      gimple_call_set_lhs (g, simt_pred);
      gimple_bind_add_stmt (bind, g);

      tree t = create_artificial_label (UNKNOWN_LOCATION);
      g = gimple_build_cond (EQ_EXPR, simt_pred, integer_zero_node, t, test);
      gimple_bind_add_stmt (bind, g);

      gimple_bind_add_stmt (bind, gimple_build_label (t));
    }
  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_omp_set_body (stmt, maybe_catch_exception (gimple_omp_body (stmt)));
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  if (maybe_simt)
    {
      gimple_bind_add_stmt (bind, gimple_build_label (test));
      g = gimple_build_assign (counter, MINUS_EXPR, counter, integer_one_node);
      gimple_bind_add_stmt (bind, g);

      tree c = build2 (GE_EXPR, boolean_type_node, counter, integer_zero_node);
      tree nonneg = create_tmp_var (integer_type_node);
      gimple_seq tseq = NULL;
      gimplify_assign (nonneg, fold_convert (integer_type_node, c), &tseq);
      gimple_bind_add_seq (bind, tseq);

      g = gimple_build_call_internal (IFN_GOMP_SIMT_VOTE_ANY, 1, nonneg);
      gimple_call_set_lhs (g, nonneg);
      gimple_bind_add_stmt (bind, g);

      tree end = create_artificial_label (UNKNOWN_LOCATION);
      g = gimple_build_cond (NE_EXPR, nonneg, integer_zero_node, body, end);
      gimple_bind_add_stmt (bind, g);

      gimple_bind_add_stmt (bind, gimple_build_label (end));
    }
  if (simd)
    x = gimple_build_call_internal (IFN_GOMP_SIMD_ORDERED_END, 1,
				    build_int_cst (NULL_TREE, threads));
  else
    x = gimple_build_call (builtin_decl_explicit (BUILT_IN_GOMP_ORDERED_END),
			   0);
  gimple_bind_add_stmt (bind, x);

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (bind);
}


/* Gimplify a GIMPLE_OMP_CRITICAL statement.  This is a relatively simple
   substitution of a couple of function calls.  But in the NAMED case,
   requires that languages coordinate a symbol name.  It is therefore
   best put here in common code.  */

static GTY(()) hash_map<tree, tree> *critical_name_mutexes;

static void
lower_omp_critical (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree block;
  tree name, lock, unlock;
  gomp_critical *stmt = as_a <gomp_critical *> (gsi_stmt (*gsi_p));
  gbind *bind;
  location_t loc = gimple_location (stmt);
  gimple_seq tbody;

  name = gimple_omp_critical_name (stmt);
  if (name)
    {
      tree decl;

      if (!critical_name_mutexes)
	critical_name_mutexes = hash_map<tree, tree>::create_ggc (10);

      tree *n = critical_name_mutexes->get (name);
      if (n == NULL)
	{
	  char *new_str;

	  decl = create_tmp_var_raw (ptr_type_node);

	  new_str = ACONCAT ((".gomp_critical_user_",
			      IDENTIFIER_POINTER (name), NULL));
	  DECL_NAME (decl) = get_identifier (new_str);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_COMMON (decl) = 1;
	  DECL_ARTIFICIAL (decl) = 1;
	  DECL_IGNORED_P (decl) = 1;

	  varpool_node::finalize_decl (decl);

	  critical_name_mutexes->put (name, decl);
	}
      else
	decl = *n;

      /* If '#pragma omp critical' is inside offloaded region or
	 inside function marked as offloadable, the symbol must be
	 marked as offloadable too.  */
      omp_context *octx;
      if (cgraph_node::get (current_function_decl)->offloadable)
	varpool_node::get_create (decl)->offloadable = 1;
      else
	for (octx = ctx->outer; octx; octx = octx->outer)
	  if (is_gimple_omp_offloaded (octx->stmt))
	    {
	      varpool_node::get_create (decl)->offloadable = 1;
	      break;
	    }

      lock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_NAME_START);
      lock = build_call_expr_loc (loc, lock, 1,
				  build_fold_addr_expr_loc (loc, decl));

      unlock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_NAME_END);
      unlock = build_call_expr_loc (loc, unlock, 1,
				build_fold_addr_expr_loc (loc, decl));
    }
  else
    {
      lock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_START);
      lock = build_call_expr_loc (loc, lock, 0);

      unlock = builtin_decl_explicit (BUILT_IN_GOMP_CRITICAL_END);
      unlock = build_call_expr_loc (loc, unlock, 0);
    }

  push_gimplify_context ();

  block = make_node (BLOCK);
  bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_bind_add_stmt (bind, stmt);

  tbody = gimple_bind_body (bind);
  gimplify_and_add (lock, &tbody);
  gimple_bind_set_body (bind, tbody);

  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_omp_set_body (stmt, maybe_catch_exception (gimple_omp_body (stmt)));
  gimple_bind_add_seq (bind, gimple_omp_body (stmt));
  gimple_omp_set_body (stmt, NULL);

  tbody = gimple_bind_body (bind);
  gimplify_and_add (unlock, &tbody);
  gimple_bind_set_body (bind, tbody);

  gimple_bind_add_stmt (bind, gimple_build_omp_return (true));

  pop_gimplify_context (bind);
  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (bind);
}

/* A subroutine of lower_omp_for.  Generate code to emit the predicate
   for a lastprivate clause.  Given a loop control predicate of (V
   cond N2), we gate the clause on (!(V cond N2)).  The lowered form
   is appended to *DLIST, iterator initialization is appended to
   *BODY_P.  */

static void
lower_omp_for_lastprivate (struct omp_for_data *fd, gimple_seq *body_p,
			   gimple_seq *dlist, struct omp_context *ctx)
{
  tree clauses, cond, vinit;
  enum tree_code cond_code;
  gimple_seq stmts;

  cond_code = fd->loop.cond_code;
  cond_code = cond_code == LT_EXPR ? GE_EXPR : LE_EXPR;

  /* When possible, use a strict equality expression.  This can let VRP
     type optimizations deduce the value and remove a copy.  */
  if (tree_fits_shwi_p (fd->loop.step))
    {
      HOST_WIDE_INT step = tree_to_shwi (fd->loop.step);
      if (step == 1 || step == -1)
	cond_code = EQ_EXPR;
    }

  if (gimple_omp_for_kind (fd->for_stmt) == GF_OMP_FOR_KIND_GRID_LOOP
      || gimple_omp_for_grid_phony (fd->for_stmt))
    cond = omp_grid_lastprivate_predicate (fd);
  else
    {
      tree n2 = fd->loop.n2;
      if (fd->collapse > 1
	  && TREE_CODE (n2) != INTEGER_CST
	  && gimple_omp_for_combined_into_p (fd->for_stmt))
	{
	  struct omp_context *taskreg_ctx = NULL;
	  if (gimple_code (ctx->outer->stmt) == GIMPLE_OMP_FOR)
	    {
	      gomp_for *gfor = as_a <gomp_for *> (ctx->outer->stmt);
	      if (gimple_omp_for_kind (gfor) == GF_OMP_FOR_KIND_FOR
		  || gimple_omp_for_kind (gfor) == GF_OMP_FOR_KIND_DISTRIBUTE)
		{
		  if (gimple_omp_for_combined_into_p (gfor))
		    {
		      gcc_assert (ctx->outer->outer
				  && is_parallel_ctx (ctx->outer->outer));
		      taskreg_ctx = ctx->outer->outer;
		    }
		  else
		    {
		      struct omp_for_data outer_fd;
		      omp_extract_for_data (gfor, &outer_fd, NULL);
		      n2 = fold_convert (TREE_TYPE (n2), outer_fd.loop.n2);
		    }
		}
	      else if (gimple_omp_for_kind (gfor) == GF_OMP_FOR_KIND_TASKLOOP)
		taskreg_ctx = ctx->outer->outer;
	    }
	  else if (is_taskreg_ctx (ctx->outer))
	    taskreg_ctx = ctx->outer;
	  if (taskreg_ctx)
	    {
	      int i;
	      tree taskreg_clauses
		= gimple_omp_taskreg_clauses (taskreg_ctx->stmt);
	      tree innerc = omp_find_clause (taskreg_clauses,
					     OMP_CLAUSE__LOOPTEMP_);
	      gcc_assert (innerc);
	      for (i = 0; i < fd->collapse; i++)
		{
		  innerc = omp_find_clause (OMP_CLAUSE_CHAIN (innerc),
					    OMP_CLAUSE__LOOPTEMP_);
		  gcc_assert (innerc);
		}
	      innerc = omp_find_clause (OMP_CLAUSE_CHAIN (innerc),
					OMP_CLAUSE__LOOPTEMP_);
	      if (innerc)
		n2 = fold_convert (TREE_TYPE (n2),
				   lookup_decl (OMP_CLAUSE_DECL (innerc),
						taskreg_ctx));
	    }
	}
      cond = build2 (cond_code, boolean_type_node, fd->loop.v, n2);
    }

  clauses = gimple_omp_for_clauses (fd->for_stmt);
  stmts = NULL;
  lower_lastprivate_clauses (clauses, cond, &stmts, ctx);
  if (!gimple_seq_empty_p (stmts))
    {
      gimple_seq_add_seq (&stmts, *dlist);
      *dlist = stmts;

      /* Optimize: v = 0; is usually cheaper than v = some_other_constant.  */
      vinit = fd->loop.n1;
      if (cond_code == EQ_EXPR
	  && tree_fits_shwi_p (fd->loop.n2)
	  && ! integer_zerop (fd->loop.n2))
	vinit = build_int_cst (TREE_TYPE (fd->loop.v), 0);
      else
	vinit = unshare_expr (vinit);

      /* Initialize the iterator variable, so that threads that don't execute
	 any iterations don't execute the lastprivate clauses by accident.  */
      gimplify_assign (fd->loop.v, vinit, body_p);
    }
}


/* Lower code for an OMP loop directive.  */

static void
lower_omp_for (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree *rhs_p, block;
  struct omp_for_data fd, *fdp = NULL;
  gomp_for *stmt = as_a <gomp_for *> (gsi_stmt (*gsi_p));
  gbind *new_stmt;
  gimple_seq omp_for_body, body, dlist;
  gimple_seq oacc_head = NULL, oacc_tail = NULL;
  size_t i;

  push_gimplify_context ();

  lower_omp (gimple_omp_for_pre_body_ptr (stmt), ctx);

  block = make_node (BLOCK);
  new_stmt = gimple_build_bind (NULL, NULL, block);
  /* Replace at gsi right away, so that 'stmt' is no member
     of a sequence anymore as we're going to add to a different
     one below.  */
  gsi_replace (gsi_p, new_stmt, true);

  /* Move declaration of temporaries in the loop body before we make
     it go away.  */
  omp_for_body = gimple_omp_body (stmt);
  if (!gimple_seq_empty_p (omp_for_body)
      && gimple_code (gimple_seq_first_stmt (omp_for_body)) == GIMPLE_BIND)
    {
      gbind *inner_bind
	= as_a <gbind *> (gimple_seq_first_stmt (omp_for_body));
      tree vars = gimple_bind_vars (inner_bind);
      gimple_bind_append_vars (new_stmt, vars);
      /* bind_vars/BLOCK_VARS are being moved to new_stmt/block, don't
	 keep them on the inner_bind and it's block.  */
      gimple_bind_set_vars (inner_bind, NULL_TREE);
      if (gimple_bind_block (inner_bind))
	BLOCK_VARS (gimple_bind_block (inner_bind)) = NULL_TREE;
    }

  if (gimple_omp_for_combined_into_p (stmt))
    {
      omp_extract_for_data (stmt, &fd, NULL);
      fdp = &fd;

      /* We need two temporaries with fd.loop.v type (istart/iend)
	 and then (fd.collapse - 1) temporaries with the same
	 type for count2 ... countN-1 vars if not constant.  */
      size_t count = 2;
      tree type = fd.iter_type;
      if (fd.collapse > 1
	  && TREE_CODE (fd.loop.n2) != INTEGER_CST)
	count += fd.collapse - 1;
      bool taskreg_for
	= (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_FOR
	   || gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_TASKLOOP);
      tree outerc = NULL, *pc = gimple_omp_for_clauses_ptr (stmt);
      tree simtc = NULL;
      tree clauses = *pc;
      if (taskreg_for)
	outerc
	  = omp_find_clause (gimple_omp_taskreg_clauses (ctx->outer->stmt),
			     OMP_CLAUSE__LOOPTEMP_);
      if (ctx->simt_stmt)
	simtc = omp_find_clause (gimple_omp_for_clauses (ctx->simt_stmt),
				 OMP_CLAUSE__LOOPTEMP_);
      for (i = 0; i < count; i++)
	{
	  tree temp;
	  if (taskreg_for)
	    {
	      gcc_assert (outerc);
	      temp = lookup_decl (OMP_CLAUSE_DECL (outerc), ctx->outer);
	      outerc = omp_find_clause (OMP_CLAUSE_CHAIN (outerc),
					OMP_CLAUSE__LOOPTEMP_);
	    }
	  else
	    {
	      /* If there are 2 adjacent SIMD stmts, one with _simt_
		 clause, another without, make sure they have the same
		 decls in _looptemp_ clauses, because the outer stmt
		 they are combined into will look up just one inner_stmt.  */
	      if (ctx->simt_stmt)
		temp = OMP_CLAUSE_DECL (simtc);
	      else
		temp = create_tmp_var (type);
	      insert_decl_map (&ctx->outer->cb, temp, temp);
	    }
	  *pc = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE__LOOPTEMP_);
	  OMP_CLAUSE_DECL (*pc) = temp;
	  pc = &OMP_CLAUSE_CHAIN (*pc);
	  if (ctx->simt_stmt)
	    simtc = omp_find_clause (OMP_CLAUSE_CHAIN (simtc),
				     OMP_CLAUSE__LOOPTEMP_);
	}
      *pc = clauses;
    }

  /* The pre-body and input clauses go before the lowered GIMPLE_OMP_FOR.  */
  dlist = NULL;
  body = NULL;
  lower_rec_input_clauses (gimple_omp_for_clauses (stmt), &body, &dlist, ctx,
			   fdp);
  gimple_seq_add_seq (&body, gimple_omp_for_pre_body (stmt));

  lower_omp (gimple_omp_body_ptr (stmt), ctx);

  /* Lower the header expressions.  At this point, we can assume that
     the header is of the form:

     	#pragma omp for (V = VAL1; V {<|>|<=|>=} VAL2; V = V [+-] VAL3)

     We just need to make sure that VAL1, VAL2 and VAL3 are lowered
     using the .omp_data_s mapping, if needed.  */
  for (i = 0; i < gimple_omp_for_collapse (stmt); i++)
    {
      rhs_p = gimple_omp_for_initial_ptr (stmt, i);
      if (!is_gimple_min_invariant (*rhs_p))
	*rhs_p = get_formal_tmp_var (*rhs_p, &body);
      else if (TREE_CODE (*rhs_p) == ADDR_EXPR)
	recompute_tree_invariant_for_addr_expr (*rhs_p);

      rhs_p = gimple_omp_for_final_ptr (stmt, i);
      if (!is_gimple_min_invariant (*rhs_p))
	*rhs_p = get_formal_tmp_var (*rhs_p, &body);
      else if (TREE_CODE (*rhs_p) == ADDR_EXPR)
	recompute_tree_invariant_for_addr_expr (*rhs_p);

      rhs_p = &TREE_OPERAND (gimple_omp_for_incr (stmt, i), 1);
      if (!is_gimple_min_invariant (*rhs_p))
	*rhs_p = get_formal_tmp_var (*rhs_p, &body);
    }

  /* Once lowered, extract the bounds and clauses.  */
  omp_extract_for_data (stmt, &fd, NULL);

  if (is_gimple_omp_oacc (ctx->stmt)
      && !ctx_in_oacc_kernels_region (ctx))
    lower_oacc_head_tail (gimple_location (stmt),
			  gimple_omp_for_clauses (stmt),
			  &oacc_head, &oacc_tail, ctx);

  /* Add OpenACC partitioning and reduction markers just before the loop.  */
  if (oacc_head)
    gimple_seq_add_seq (&body, oacc_head);

  lower_omp_for_lastprivate (&fd, &body, &dlist, ctx);

  if (gimple_omp_for_kind (stmt) == GF_OMP_FOR_KIND_FOR)
    for (tree c = gimple_omp_for_clauses (stmt); c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	  && !OMP_CLAUSE_LINEAR_NO_COPYIN (c))
	{
	  OMP_CLAUSE_DECL (c) = lookup_decl (OMP_CLAUSE_DECL (c), ctx);
	  if (DECL_P (OMP_CLAUSE_LINEAR_STEP (c)))
	    OMP_CLAUSE_LINEAR_STEP (c)
	      = maybe_lookup_decl_in_outer_ctx (OMP_CLAUSE_LINEAR_STEP (c),
						ctx);
	}

  bool phony_loop = (gimple_omp_for_kind (stmt) != GF_OMP_FOR_KIND_GRID_LOOP
		     && gimple_omp_for_grid_phony (stmt));
  if (!phony_loop)
    gimple_seq_add_stmt (&body, stmt);
  gimple_seq_add_seq (&body, gimple_omp_body (stmt));

  if (!phony_loop)
    gimple_seq_add_stmt (&body, gimple_build_omp_continue (fd.loop.v,
							   fd.loop.v));

  /* After the loop, add exit clauses.  */
  lower_reduction_clauses (gimple_omp_for_clauses (stmt), &body, ctx);

  if (ctx->cancellable)
    gimple_seq_add_stmt (&body, gimple_build_label (ctx->cancel_label));

  gimple_seq_add_seq (&body, dlist);

  body = maybe_catch_exception (body);

  if (!phony_loop)
    {
      /* Region exit marker goes at the end of the loop body.  */
      gimple_seq_add_stmt (&body, gimple_build_omp_return (fd.have_nowait));
      maybe_add_implicit_barrier_cancel (ctx, &body);
    }

  /* Add OpenACC joining and reduction markers just after the loop.  */
  if (oacc_tail)
    gimple_seq_add_seq (&body, oacc_tail);

  pop_gimplify_context (new_stmt);

  gimple_bind_append_vars (new_stmt, ctx->block_vars);
  BLOCK_VARS (block) = gimple_bind_vars (new_stmt);
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;

  gimple_bind_set_body (new_stmt, body);
  gimple_omp_set_body (stmt, NULL);
  gimple_omp_for_set_pre_body (stmt, NULL);
}

/* Callback for walk_stmts.  Check if the current statement only contains
   GIMPLE_OMP_FOR or GIMPLE_OMP_SECTIONS.  */

static tree
check_combined_parallel (gimple_stmt_iterator *gsi_p,
    			 bool *handled_ops_p,
    			 struct walk_stmt_info *wi)
{
  int *info = (int *) wi->info;
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;
  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_FOR:
    case GIMPLE_OMP_SECTIONS:
      *info = *info == 0 ? 1 : -1;
      break;
    default:
      *info = -1;
      break;
    }
  return NULL;
}

struct omp_taskcopy_context
{
  /* This field must be at the beginning, as we do "inheritance": Some
     callback functions for tree-inline.c (e.g., omp_copy_decl)
     receive a copy_body_data pointer that is up-casted to an
     omp_context pointer.  */
  copy_body_data cb;
  omp_context *ctx;
};

static tree
task_copyfn_copy_decl (tree var, copy_body_data *cb)
{
  struct omp_taskcopy_context *tcctx = (struct omp_taskcopy_context *) cb;

  if (splay_tree_lookup (tcctx->ctx->sfield_map, (splay_tree_key) var))
    return create_tmp_var (TREE_TYPE (var));

  return var;
}

static tree
task_copyfn_remap_type (struct omp_taskcopy_context *tcctx, tree orig_type)
{
  tree name, new_fields = NULL, type, f;

  type = lang_hooks.types.make_type (RECORD_TYPE);
  name = DECL_NAME (TYPE_NAME (orig_type));
  name = build_decl (gimple_location (tcctx->ctx->stmt),
		     TYPE_DECL, name, type);
  TYPE_NAME (type) = name;

  for (f = TYPE_FIELDS (orig_type); f ; f = TREE_CHAIN (f))
    {
      tree new_f = copy_node (f);
      DECL_CONTEXT (new_f) = type;
      TREE_TYPE (new_f) = remap_type (TREE_TYPE (f), &tcctx->cb);
      TREE_CHAIN (new_f) = new_fields;
      walk_tree (&DECL_SIZE (new_f), copy_tree_body_r, &tcctx->cb, NULL);
      walk_tree (&DECL_SIZE_UNIT (new_f), copy_tree_body_r, &tcctx->cb, NULL);
      walk_tree (&DECL_FIELD_OFFSET (new_f), copy_tree_body_r,
		 &tcctx->cb, NULL);
      new_fields = new_f;
      tcctx->cb.decl_map->put (f, new_f);
    }
  TYPE_FIELDS (type) = nreverse (new_fields);
  layout_type (type);
  return type;
}

/* Create task copyfn.  */

static void
create_task_copyfn (gomp_task *task_stmt, omp_context *ctx)
{
  struct function *child_cfun;
  tree child_fn, t, c, src, dst, f, sf, arg, sarg, decl;
  tree record_type, srecord_type, bind, list;
  bool record_needs_remap = false, srecord_needs_remap = false;
  splay_tree_node n;
  struct omp_taskcopy_context tcctx;
  location_t loc = gimple_location (task_stmt);

  child_fn = gimple_omp_task_copy_fn (task_stmt);
  child_cfun = DECL_STRUCT_FUNCTION (child_fn);
  gcc_assert (child_cfun->cfg == NULL);
  DECL_SAVED_TREE (child_fn) = alloc_stmt_list ();

  /* Reset DECL_CONTEXT on function arguments.  */
  for (t = DECL_ARGUMENTS (child_fn); t; t = DECL_CHAIN (t))
    DECL_CONTEXT (t) = child_fn;

  /* Populate the function.  */
  push_gimplify_context ();
  push_cfun (child_cfun);

  bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  TREE_SIDE_EFFECTS (bind) = 1;
  list = NULL;
  DECL_SAVED_TREE (child_fn) = bind;
  DECL_SOURCE_LOCATION (child_fn) = gimple_location (task_stmt);

  /* Remap src and dst argument types if needed.  */
  record_type = ctx->record_type;
  srecord_type = ctx->srecord_type;
  for (f = TYPE_FIELDS (record_type); f ; f = DECL_CHAIN (f))
    if (variably_modified_type_p (TREE_TYPE (f), ctx->cb.src_fn))
      {
	record_needs_remap = true;
	break;
      }
  for (f = TYPE_FIELDS (srecord_type); f ; f = DECL_CHAIN (f))
    if (variably_modified_type_p (TREE_TYPE (f), ctx->cb.src_fn))
      {
	srecord_needs_remap = true;
	break;
      }

  if (record_needs_remap || srecord_needs_remap)
    {
      memset (&tcctx, '\0', sizeof (tcctx));
      tcctx.cb.src_fn = ctx->cb.src_fn;
      tcctx.cb.dst_fn = child_fn;
      tcctx.cb.src_node = cgraph_node::get (tcctx.cb.src_fn);
      gcc_checking_assert (tcctx.cb.src_node);
      tcctx.cb.dst_node = tcctx.cb.src_node;
      tcctx.cb.src_cfun = ctx->cb.src_cfun;
      tcctx.cb.copy_decl = task_copyfn_copy_decl;
      tcctx.cb.eh_lp_nr = 0;
      tcctx.cb.transform_call_graph_edges = CB_CGE_MOVE;
      tcctx.cb.decl_map = new hash_map<tree, tree>;
      tcctx.ctx = ctx;

      if (record_needs_remap)
	record_type = task_copyfn_remap_type (&tcctx, record_type);
      if (srecord_needs_remap)
	srecord_type = task_copyfn_remap_type (&tcctx, srecord_type);
    }
  else
    tcctx.cb.decl_map = NULL;

  arg = DECL_ARGUMENTS (child_fn);
  TREE_TYPE (arg) = build_pointer_type (record_type);
  sarg = DECL_CHAIN (arg);
  TREE_TYPE (sarg) = build_pointer_type (srecord_type);

  /* First pass: initialize temporaries used in record_type and srecord_type
     sizes and field offsets.  */
  if (tcctx.cb.decl_map)
    for (c = gimple_omp_task_clauses (task_stmt); c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	{
	  tree *p;

	  decl = OMP_CLAUSE_DECL (c);
	  p = tcctx.cb.decl_map->get (decl);
	  if (p == NULL)
	    continue;
	  n = splay_tree_lookup (ctx->sfield_map, (splay_tree_key) decl);
	  sf = (tree) n->value;
	  sf = *tcctx.cb.decl_map->get (sf);
	  src = build_simple_mem_ref_loc (loc, sarg);
	  src = omp_build_component_ref (src, sf);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (*p), *p, src);
	  append_to_statement_list (t, &list);
	}

  /* Second pass: copy shared var pointers and copy construct non-VLA
     firstprivate vars.  */
  for (c = gimple_omp_task_clauses (task_stmt); c; c = OMP_CLAUSE_CHAIN (c))
    switch (OMP_CLAUSE_CODE (c))
      {
	splay_tree_key key;
      case OMP_CLAUSE_SHARED:
	decl = OMP_CLAUSE_DECL (c);
	key = (splay_tree_key) decl;
	if (OMP_CLAUSE_SHARED_FIRSTPRIVATE (c))
	  key = (splay_tree_key) &DECL_UID (decl);
	n = splay_tree_lookup (ctx->field_map, key);
	if (n == NULL)
	  break;
	f = (tree) n->value;
	if (tcctx.cb.decl_map)
	  f = *tcctx.cb.decl_map->get (f);
	n = splay_tree_lookup (ctx->sfield_map, key);
	sf = (tree) n->value;
	if (tcctx.cb.decl_map)
	  sf = *tcctx.cb.decl_map->get (sf);
	src = build_simple_mem_ref_loc (loc, sarg);
	src = omp_build_component_ref (src, sf);
	dst = build_simple_mem_ref_loc (loc, arg);
	dst = omp_build_component_ref (dst, f);
	t = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
	append_to_statement_list (t, &list);
	break;
      case OMP_CLAUSE_FIRSTPRIVATE:
	decl = OMP_CLAUSE_DECL (c);
	if (is_variable_sized (decl))
	  break;
	n = splay_tree_lookup (ctx->field_map, (splay_tree_key) decl);
	if (n == NULL)
	  break;
	f = (tree) n->value;
	if (tcctx.cb.decl_map)
	  f = *tcctx.cb.decl_map->get (f);
	n = splay_tree_lookup (ctx->sfield_map, (splay_tree_key) decl);
	if (n != NULL)
	  {
	    sf = (tree) n->value;
	    if (tcctx.cb.decl_map)
	      sf = *tcctx.cb.decl_map->get (sf);
	    src = build_simple_mem_ref_loc (loc, sarg);
	    src = omp_build_component_ref (src, sf);
	    if (use_pointer_for_field (decl, NULL) || omp_is_reference (decl))
	      src = build_simple_mem_ref_loc (loc, src);
	  }
	else
	  src = decl;
	dst = build_simple_mem_ref_loc (loc, arg);
	dst = omp_build_component_ref (dst, f);
	t = lang_hooks.decls.omp_clause_copy_ctor (c, dst, src);
	append_to_statement_list (t, &list);
	break;
      case OMP_CLAUSE_PRIVATE:
	if (! OMP_CLAUSE_PRIVATE_OUTER_REF (c))
	  break;
	decl = OMP_CLAUSE_DECL (c);
	n = splay_tree_lookup (ctx->field_map, (splay_tree_key) decl);
	f = (tree) n->value;
	if (tcctx.cb.decl_map)
	  f = *tcctx.cb.decl_map->get (f);
	n = splay_tree_lookup (ctx->sfield_map, (splay_tree_key) decl);
	if (n != NULL)
	  {
	    sf = (tree) n->value;
	    if (tcctx.cb.decl_map)
	      sf = *tcctx.cb.decl_map->get (sf);
	    src = build_simple_mem_ref_loc (loc, sarg);
	    src = omp_build_component_ref (src, sf);
	    if (use_pointer_for_field (decl, NULL))
	      src = build_simple_mem_ref_loc (loc, src);
	  }
	else
	  src = decl;
	dst = build_simple_mem_ref_loc (loc, arg);
	dst = omp_build_component_ref (dst, f);
	t = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
	append_to_statement_list (t, &list);
	break;
      default:
	break;
      }

  /* Last pass: handle VLA firstprivates.  */
  if (tcctx.cb.decl_map)
    for (c = gimple_omp_task_clauses (task_stmt); c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	{
	  tree ind, ptr, df;

	  decl = OMP_CLAUSE_DECL (c);
	  if (!is_variable_sized (decl))
	    continue;
	  n = splay_tree_lookup (ctx->field_map, (splay_tree_key) decl);
	  if (n == NULL)
	    continue;
	  f = (tree) n->value;
	  f = *tcctx.cb.decl_map->get (f);
	  gcc_assert (DECL_HAS_VALUE_EXPR_P (decl));
	  ind = DECL_VALUE_EXPR (decl);
	  gcc_assert (TREE_CODE (ind) == INDIRECT_REF);
	  gcc_assert (DECL_P (TREE_OPERAND (ind, 0)));
	  n = splay_tree_lookup (ctx->sfield_map,
				 (splay_tree_key) TREE_OPERAND (ind, 0));
	  sf = (tree) n->value;
	  sf = *tcctx.cb.decl_map->get (sf);
	  src = build_simple_mem_ref_loc (loc, sarg);
	  src = omp_build_component_ref (src, sf);
	  src = build_simple_mem_ref_loc (loc, src);
	  dst = build_simple_mem_ref_loc (loc, arg);
	  dst = omp_build_component_ref (dst, f);
	  t = lang_hooks.decls.omp_clause_copy_ctor (c, dst, src);
	  append_to_statement_list (t, &list);
	  n = splay_tree_lookup (ctx->field_map,
				 (splay_tree_key) TREE_OPERAND (ind, 0));
	  df = (tree) n->value;
	  df = *tcctx.cb.decl_map->get (df);
	  ptr = build_simple_mem_ref_loc (loc, arg);
	  ptr = omp_build_component_ref (ptr, df);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (ptr), ptr,
		      build_fold_addr_expr_loc (loc, dst));
	  append_to_statement_list (t, &list);
	}

  t = build1 (RETURN_EXPR, void_type_node, NULL);
  append_to_statement_list (t, &list);

  if (tcctx.cb.decl_map)
    delete tcctx.cb.decl_map;
  pop_gimplify_context (NULL);
  BIND_EXPR_BODY (bind) = list;
  pop_cfun ();
}

static void
lower_depend_clauses (tree *pclauses, gimple_seq *iseq, gimple_seq *oseq)
{
  tree c, clauses;
  gimple *g;
  size_t n_in = 0, n_out = 0, idx = 2, i;

  clauses = omp_find_clause (*pclauses, OMP_CLAUSE_DEPEND);
  gcc_assert (clauses);
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
      switch (OMP_CLAUSE_DEPEND_KIND (c))
	{
	case OMP_CLAUSE_DEPEND_IN:
	  n_in++;
	  break;
	case OMP_CLAUSE_DEPEND_OUT:
	case OMP_CLAUSE_DEPEND_INOUT:
	  n_out++;
	  break;
	case OMP_CLAUSE_DEPEND_SOURCE:
	case OMP_CLAUSE_DEPEND_SINK:
	  /* FALLTHRU */
	default:
	  gcc_unreachable ();
	}
  tree type = build_array_type_nelts (ptr_type_node, n_in + n_out + 2);
  tree array = create_tmp_var (type);
  TREE_ADDRESSABLE (array) = 1;
  tree r = build4 (ARRAY_REF, ptr_type_node, array, size_int (0), NULL_TREE,
		   NULL_TREE);
  g = gimple_build_assign (r, build_int_cst (ptr_type_node, n_in + n_out));
  gimple_seq_add_stmt (iseq, g);
  r = build4 (ARRAY_REF, ptr_type_node, array, size_int (1), NULL_TREE,
	      NULL_TREE);
  g = gimple_build_assign (r, build_int_cst (ptr_type_node, n_out));
  gimple_seq_add_stmt (iseq, g);
  for (i = 0; i < 2; i++)
    {
      if ((i ? n_in : n_out) == 0)
	continue;
      for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	    && ((OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_IN) ^ i))
	  {
	    tree t = OMP_CLAUSE_DECL (c);
	    t = fold_convert (ptr_type_node, t);
	    gimplify_expr (&t, iseq, NULL, is_gimple_val, fb_rvalue);
	    r = build4 (ARRAY_REF, ptr_type_node, array, size_int (idx++),
			NULL_TREE, NULL_TREE);
	    g = gimple_build_assign (r, t);
	    gimple_seq_add_stmt (iseq, g);
	  }
    }
  c = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_DEPEND);
  OMP_CLAUSE_DECL (c) = build_fold_addr_expr (array);
  OMP_CLAUSE_CHAIN (c) = *pclauses;
  *pclauses = c;
  tree clobber = build_constructor (type, NULL);
  TREE_THIS_VOLATILE (clobber) = 1;
  g = gimple_build_assign (array, clobber);
  gimple_seq_add_stmt (oseq, g);
}

/* Lower the OpenMP parallel or task directive in the current statement
   in GSI_P.  CTX holds context information for the directive.  */

static void
lower_omp_taskreg (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree clauses;
  tree child_fn, t;
  gimple *stmt = gsi_stmt (*gsi_p);
  gbind *par_bind, *bind, *dep_bind = NULL;
  gimple_seq par_body, olist, ilist, par_olist, par_rlist, par_ilist, new_body;
  location_t loc = gimple_location (stmt);

  clauses = gimple_omp_taskreg_clauses (stmt);
  par_bind
    = as_a <gbind *> (gimple_seq_first_stmt (gimple_omp_body (stmt)));
  par_body = gimple_bind_body (par_bind);
  child_fn = ctx->cb.dst_fn;
  if (gimple_code (stmt) == GIMPLE_OMP_PARALLEL
      && !gimple_omp_parallel_combined_p (stmt))
    {
      struct walk_stmt_info wi;
      int ws_num = 0;

      memset (&wi, 0, sizeof (wi));
      wi.info = &ws_num;
      wi.val_only = true;
      walk_gimple_seq (par_body, check_combined_parallel, NULL, &wi);
      if (ws_num == 1)
	gimple_omp_parallel_set_combined_p (stmt, true);
    }
  gimple_seq dep_ilist = NULL;
  gimple_seq dep_olist = NULL;
  if (gimple_code (stmt) == GIMPLE_OMP_TASK
      && omp_find_clause (clauses, OMP_CLAUSE_DEPEND))
    {
      push_gimplify_context ();
      dep_bind = gimple_build_bind (NULL, NULL, make_node (BLOCK));
      lower_depend_clauses (gimple_omp_task_clauses_ptr (stmt),
			    &dep_ilist, &dep_olist);
    }

  if (ctx->srecord_type)
    create_task_copyfn (as_a <gomp_task *> (stmt), ctx);

  push_gimplify_context ();

  par_olist = NULL;
  par_ilist = NULL;
  par_rlist = NULL;
  bool phony_construct = gimple_code (stmt) == GIMPLE_OMP_PARALLEL
    && gimple_omp_parallel_grid_phony (as_a <gomp_parallel *> (stmt));
  if (phony_construct && ctx->record_type)
    {
      gcc_checking_assert (!ctx->receiver_decl);
      ctx->receiver_decl = create_tmp_var
	(build_reference_type (ctx->record_type), ".omp_rec");
    }
  lower_rec_input_clauses (clauses, &par_ilist, &par_olist, ctx, NULL);
  lower_omp (&par_body, ctx);
  if (gimple_code (stmt) == GIMPLE_OMP_PARALLEL)
    lower_reduction_clauses (clauses, &par_rlist, ctx);

  /* Declare all the variables created by mapping and the variables
     declared in the scope of the parallel body.  */
  record_vars_into (ctx->block_vars, child_fn);
  record_vars_into (gimple_bind_vars (par_bind), child_fn);

  if (ctx->record_type)
    {
      ctx->sender_decl
	= create_tmp_var (ctx->srecord_type ? ctx->srecord_type
			  : ctx->record_type, ".omp_data_o");
      DECL_NAMELESS (ctx->sender_decl) = 1;
      TREE_ADDRESSABLE (ctx->sender_decl) = 1;
      gimple_omp_taskreg_set_data_arg (stmt, ctx->sender_decl);
    }

  olist = NULL;
  ilist = NULL;
  lower_send_clauses (clauses, &ilist, &olist, ctx);
  lower_send_shared_vars (&ilist, &olist, ctx);

  if (ctx->record_type)
    {
      tree clobber = build_constructor (TREE_TYPE (ctx->sender_decl), NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gimple_seq_add_stmt (&olist, gimple_build_assign (ctx->sender_decl,
							clobber));
    }

  /* Once all the expansions are done, sequence all the different
     fragments inside gimple_omp_body.  */

  new_body = NULL;

  if (ctx->record_type)
    {
      t = build_fold_addr_expr_loc (loc, ctx->sender_decl);
      /* fixup_child_record_type might have changed receiver_decl's type.  */
      t = fold_convert_loc (loc, TREE_TYPE (ctx->receiver_decl), t);
      gimple_seq_add_stmt (&new_body,
	  		   gimple_build_assign (ctx->receiver_decl, t));
    }

  gimple_seq_add_seq (&new_body, par_ilist);
  gimple_seq_add_seq (&new_body, par_body);
  gimple_seq_add_seq (&new_body, par_rlist);
  if (ctx->cancellable)
    gimple_seq_add_stmt (&new_body, gimple_build_label (ctx->cancel_label));
  gimple_seq_add_seq (&new_body, par_olist);
  new_body = maybe_catch_exception (new_body);
  if (gimple_code (stmt) == GIMPLE_OMP_TASK)
    gimple_seq_add_stmt (&new_body,
			 gimple_build_omp_continue (integer_zero_node,
						    integer_zero_node));
  if (!phony_construct)
    {
      gimple_seq_add_stmt (&new_body, gimple_build_omp_return (false));
      gimple_omp_set_body (stmt, new_body);
    }

  bind = gimple_build_bind (NULL, NULL, gimple_bind_block (par_bind));
  gsi_replace (gsi_p, dep_bind ? dep_bind : bind, true);
  gimple_bind_add_seq (bind, ilist);
  if (!phony_construct)
    gimple_bind_add_stmt (bind, stmt);
  else
    gimple_bind_add_seq (bind, new_body);
  gimple_bind_add_seq (bind, olist);

  pop_gimplify_context (NULL);

  if (dep_bind)
    {
      gimple_bind_add_seq (dep_bind, dep_ilist);
      gimple_bind_add_stmt (dep_bind, bind);
      gimple_bind_add_seq (dep_bind, dep_olist);
      pop_gimplify_context (dep_bind);
    }
}

/* Lower the GIMPLE_OMP_TARGET in the current statement
   in GSI_P.  CTX holds context information for the directive.  */

static void
lower_omp_target (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  tree clauses;
  tree child_fn, t, c;
  gomp_target *stmt = as_a <gomp_target *> (gsi_stmt (*gsi_p));
  gbind *tgt_bind, *bind, *dep_bind = NULL;
  gimple_seq tgt_body, olist, ilist, fplist, new_body;
  location_t loc = gimple_location (stmt);
  bool offloaded, data_region;
  unsigned int map_cnt = 0;

  offloaded = is_gimple_omp_offloaded (stmt);
  switch (gimple_omp_target_kind (stmt))
    {
    case GF_OMP_TARGET_KIND_REGION:
    case GF_OMP_TARGET_KIND_UPDATE:
    case GF_OMP_TARGET_KIND_ENTER_DATA:
    case GF_OMP_TARGET_KIND_EXIT_DATA:
    case GF_OMP_TARGET_KIND_OACC_PARALLEL:
    case GF_OMP_TARGET_KIND_OACC_KERNELS:
    case GF_OMP_TARGET_KIND_OACC_UPDATE:
    case GF_OMP_TARGET_KIND_OACC_ENTER_EXIT_DATA:
    case GF_OMP_TARGET_KIND_OACC_DECLARE:
      data_region = false;
      break;
    case GF_OMP_TARGET_KIND_DATA:
    case GF_OMP_TARGET_KIND_OACC_DATA:
    case GF_OMP_TARGET_KIND_OACC_HOST_DATA:
      data_region = true;
      break;
    default:
      gcc_unreachable ();
    }

  clauses = gimple_omp_target_clauses (stmt);

  gimple_seq dep_ilist = NULL;
  gimple_seq dep_olist = NULL;
  if (omp_find_clause (clauses, OMP_CLAUSE_DEPEND))
    {
      push_gimplify_context ();
      dep_bind = gimple_build_bind (NULL, NULL, make_node (BLOCK));
      lower_depend_clauses (gimple_omp_target_clauses_ptr (stmt),
			    &dep_ilist, &dep_olist);
    }

  tgt_bind = NULL;
  tgt_body = NULL;
  if (offloaded)
    {
      tgt_bind = gimple_seq_first_stmt_as_a_bind (gimple_omp_body (stmt));
      tgt_body = gimple_bind_body (tgt_bind);
    }
  else if (data_region)
    tgt_body = gimple_omp_body (stmt);
  child_fn = ctx->cb.dst_fn;

  push_gimplify_context ();
  fplist = NULL;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    switch (OMP_CLAUSE_CODE (c))
      {
	tree var, x;

      default:
	break;
      case OMP_CLAUSE_MAP:
#if CHECKING_P
	/* First check what we're prepared to handle in the following.  */
	switch (OMP_CLAUSE_MAP_KIND (c))
	  {
	  case GOMP_MAP_ALLOC:
	  case GOMP_MAP_TO:
	  case GOMP_MAP_FROM:
	  case GOMP_MAP_TOFROM:
	  case GOMP_MAP_POINTER:
	  case GOMP_MAP_TO_PSET:
	  case GOMP_MAP_DELETE:
	  case GOMP_MAP_RELEASE:
	  case GOMP_MAP_ALWAYS_TO:
	  case GOMP_MAP_ALWAYS_FROM:
	  case GOMP_MAP_ALWAYS_TOFROM:
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  case GOMP_MAP_STRUCT:
	  case GOMP_MAP_ALWAYS_POINTER:
	    break;
	  case GOMP_MAP_FORCE_ALLOC:
	  case GOMP_MAP_FORCE_TO:
	  case GOMP_MAP_FORCE_FROM:
	  case GOMP_MAP_FORCE_TOFROM:
	  case GOMP_MAP_FORCE_PRESENT:
	  case GOMP_MAP_FORCE_DEVICEPTR:
	  case GOMP_MAP_DEVICE_RESIDENT:
	  case GOMP_MAP_LINK:
	    gcc_assert (is_gimple_omp_oacc (stmt));
	    break;
	  default:
	    gcc_unreachable ();
	  }
#endif
	  /* FALLTHRU */
      case OMP_CLAUSE_TO:
      case OMP_CLAUSE_FROM:
      oacc_firstprivate:
	var = OMP_CLAUSE_DECL (c);
	if (!DECL_P (var))
	  {
	    if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
		|| (!OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		    && (OMP_CLAUSE_MAP_KIND (c)
			!= GOMP_MAP_FIRSTPRIVATE_POINTER)))
	      map_cnt++;
	    continue;
	  }

	if (DECL_SIZE (var)
	    && TREE_CODE (DECL_SIZE (var)) != INTEGER_CST)
	  {
	    tree var2 = DECL_VALUE_EXPR (var);
	    gcc_assert (TREE_CODE (var2) == INDIRECT_REF);
	    var2 = TREE_OPERAND (var2, 0);
	    gcc_assert (DECL_P (var2));
	    var = var2;
	  }

	if (offloaded
	    && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	    && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE))
	  {
	    if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	      {
		if (is_global_var (maybe_lookup_decl_in_outer_ctx (var, ctx))
		    && varpool_node::get_create (var)->offloadable)
		  continue;

		tree type = build_pointer_type (TREE_TYPE (var));
		tree new_var = lookup_decl (var, ctx);
		x = create_tmp_var_raw (type, get_name (new_var));
		gimple_add_tmp_var (x);
		x = build_simple_mem_ref (x);
		SET_DECL_VALUE_EXPR (new_var, x);
		DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	      }
	    continue;
	  }

	if (!maybe_lookup_field (var, ctx))
	  continue;

	/* Don't remap oacc parallel reduction variables, because the
	   intermediate result must be local to each gang.  */
	if (offloaded && !(OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
			   && OMP_CLAUSE_MAP_IN_REDUCTION (c)))
	  {
	    x = build_receiver_ref (var, true, ctx);
	    tree new_var = lookup_decl (var, ctx);

	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		&& OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		&& !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		&& TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	      x = build_simple_mem_ref (x);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	      {
		gcc_assert (is_gimple_omp_oacc (ctx->stmt));
		if (omp_is_reference (new_var))
		  {
		    /* Create a local object to hold the instance
		       value.  */
		    tree type = TREE_TYPE (TREE_TYPE (new_var));
		    const char *id = IDENTIFIER_POINTER (DECL_NAME (new_var));
		    tree inst = create_tmp_var (type, id);
		    gimplify_assign (inst, fold_indirect_ref (x), &fplist);
		    x = build_fold_addr_expr (inst);
		  }
		gimplify_assign (new_var, x, &fplist);
	      }
	    else if (DECL_P (new_var))
	      {
		SET_DECL_VALUE_EXPR (new_var, x);
		DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	      }
	    else
	      gcc_unreachable ();
	  }
	map_cnt++;
	break;

      case OMP_CLAUSE_FIRSTPRIVATE:
	if (is_oacc_parallel (ctx))
	  goto oacc_firstprivate;
	map_cnt++;
	var = OMP_CLAUSE_DECL (c);
	if (!omp_is_reference (var)
	    && !is_gimple_reg_type (TREE_TYPE (var)))
	  {
	    tree new_var = lookup_decl (var, ctx);
	    if (is_variable_sized (var))
	      {
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_pvar = lookup_decl (pvar, ctx);
		x = build_fold_indirect_ref (new_pvar);
		TREE_THIS_NOTRAP (x) = 1;
	      }
	    else
	      x = build_receiver_ref (var, true, ctx);
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	break;

      case OMP_CLAUSE_PRIVATE:
	if (is_gimple_omp_oacc (ctx->stmt))
	  break;
	var = OMP_CLAUSE_DECL (c);
	if (is_variable_sized (var))
	  {
	    tree new_var = lookup_decl (var, ctx);
	    tree pvar = DECL_VALUE_EXPR (var);
	    gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
	    pvar = TREE_OPERAND (pvar, 0);
	    gcc_assert (DECL_P (pvar));
	    tree new_pvar = lookup_decl (pvar, ctx);
	    x = build_fold_indirect_ref (new_pvar);
	    TREE_THIS_NOTRAP (x) = 1;
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	break;

      case OMP_CLAUSE_USE_DEVICE_PTR:
      case OMP_CLAUSE_IS_DEVICE_PTR:
	var = OMP_CLAUSE_DECL (c);
	map_cnt++;
	if (is_variable_sized (var))
	  {
	    tree new_var = lookup_decl (var, ctx);
	    tree pvar = DECL_VALUE_EXPR (var);
	    gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
	    pvar = TREE_OPERAND (pvar, 0);
	    gcc_assert (DECL_P (pvar));
	    tree new_pvar = lookup_decl (pvar, ctx);
	    x = build_fold_indirect_ref (new_pvar);
	    TREE_THIS_NOTRAP (x) = 1;
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	else if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	  {
	    tree new_var = lookup_decl (var, ctx);
	    tree type = build_pointer_type (TREE_TYPE (var));
	    x = create_tmp_var_raw (type, get_name (new_var));
	    gimple_add_tmp_var (x);
	    x = build_simple_mem_ref (x);
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	else
	  {
	    tree new_var = lookup_decl (var, ctx);
	    x = create_tmp_var_raw (TREE_TYPE (new_var), get_name (new_var));
	    gimple_add_tmp_var (x);
	    SET_DECL_VALUE_EXPR (new_var, x);
	    DECL_HAS_VALUE_EXPR_P (new_var) = 1;
	  }
	break;
      }

  if (offloaded)
    {
      target_nesting_level++;
      lower_omp (&tgt_body, ctx);
      target_nesting_level--;
    }
  else if (data_region)
    lower_omp (&tgt_body, ctx);

  if (offloaded)
    {
      /* Declare all the variables created by mapping and the variables
	 declared in the scope of the target body.  */
      record_vars_into (ctx->block_vars, child_fn);
      record_vars_into (gimple_bind_vars (tgt_bind), child_fn);
    }

  olist = NULL;
  ilist = NULL;
  if (ctx->record_type)
    {
      ctx->sender_decl
	= create_tmp_var (ctx->record_type, ".omp_data_arr");
      DECL_NAMELESS (ctx->sender_decl) = 1;
      TREE_ADDRESSABLE (ctx->sender_decl) = 1;
      t = make_tree_vec (3);
      TREE_VEC_ELT (t, 0) = ctx->sender_decl;
      TREE_VEC_ELT (t, 1)
	= create_tmp_var (build_array_type_nelts (size_type_node, map_cnt),
			  ".omp_data_sizes");
      DECL_NAMELESS (TREE_VEC_ELT (t, 1)) = 1;
      TREE_ADDRESSABLE (TREE_VEC_ELT (t, 1)) = 1;
      TREE_STATIC (TREE_VEC_ELT (t, 1)) = 1;
      tree tkind_type = short_unsigned_type_node;
      int talign_shift = 8;
      TREE_VEC_ELT (t, 2)
	= create_tmp_var (build_array_type_nelts (tkind_type, map_cnt),
			  ".omp_data_kinds");
      DECL_NAMELESS (TREE_VEC_ELT (t, 2)) = 1;
      TREE_ADDRESSABLE (TREE_VEC_ELT (t, 2)) = 1;
      TREE_STATIC (TREE_VEC_ELT (t, 2)) = 1;
      gimple_omp_target_set_data_arg (stmt, t);

      vec<constructor_elt, va_gc> *vsize;
      vec<constructor_elt, va_gc> *vkind;
      vec_alloc (vsize, map_cnt);
      vec_alloc (vkind, map_cnt);
      unsigned int map_idx = 0;

      for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	    tree ovar, nc, s, purpose, var, x, type;
	    unsigned int talign;

	  default:
	    break;

	  case OMP_CLAUSE_MAP:
	  case OMP_CLAUSE_TO:
	  case OMP_CLAUSE_FROM:
	  oacc_firstprivate_map:
	    nc = c;
	    ovar = OMP_CLAUSE_DECL (c);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		&& (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		    || (OMP_CLAUSE_MAP_KIND (c)
			== GOMP_MAP_FIRSTPRIVATE_REFERENCE)))
	      break;
	    if (!DECL_P (ovar))
	      {
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		    && OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c))
		  {
		    gcc_checking_assert (OMP_CLAUSE_DECL (OMP_CLAUSE_CHAIN (c))
					 == get_base_address (ovar));
		    nc = OMP_CLAUSE_CHAIN (c);
		    ovar = OMP_CLAUSE_DECL (nc);
		  }
		else
		  {
		    tree x = build_sender_ref (ovar, ctx);
		    tree v
		      = build_fold_addr_expr_with_type (ovar, ptr_type_node);
		    gimplify_assign (x, v, &ilist);
		    nc = NULL_TREE;
		  }
	      }
	    else
	      {
		if (DECL_SIZE (ovar)
		    && TREE_CODE (DECL_SIZE (ovar)) != INTEGER_CST)
		  {
		    tree ovar2 = DECL_VALUE_EXPR (ovar);
		    gcc_assert (TREE_CODE (ovar2) == INDIRECT_REF);
		    ovar2 = TREE_OPERAND (ovar2, 0);
		    gcc_assert (DECL_P (ovar2));
		    ovar = ovar2;
		  }
		if (!maybe_lookup_field (ovar, ctx))
		  continue;
	      }

	    talign = TYPE_ALIGN_UNIT (TREE_TYPE (ovar));
	    if (DECL_P (ovar) && DECL_ALIGN_UNIT (ovar) > talign)
	      talign = DECL_ALIGN_UNIT (ovar);
	    if (nc)
	      {
		var = lookup_decl_in_outer_ctx (ovar, ctx);
		x = build_sender_ref (ovar, ctx);

		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		    && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
		    && !OMP_CLAUSE_MAP_ZERO_BIAS_ARRAY_SECTION (c)
		    && TREE_CODE (TREE_TYPE (ovar)) == ARRAY_TYPE)
		  {
		    gcc_assert (offloaded);
		    tree avar
		      = create_tmp_var (TREE_TYPE (TREE_TYPE (x)));
		    mark_addressable (avar);
		    gimplify_assign (avar, build_fold_addr_expr (var), &ilist);
		    talign = DECL_ALIGN_UNIT (avar);
		    avar = build_fold_addr_expr (avar);
		    gimplify_assign (x, avar, &ilist);
		  }
		else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
		  {
		    gcc_assert (is_gimple_omp_oacc (ctx->stmt));
		    if (!omp_is_reference (var))
		      {
			if (is_gimple_reg (var)
			    && OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
			  TREE_NO_WARNING (var) = 1;
			var = build_fold_addr_expr (var);
		      }
		    else
		      talign = TYPE_ALIGN_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
		    gimplify_assign (x, var, &ilist);
		  }
		else if (is_gimple_reg (var))
		  {
		    gcc_assert (offloaded);
		    tree avar = create_tmp_var (TREE_TYPE (var));
		    mark_addressable (avar);
		    enum gomp_map_kind map_kind = OMP_CLAUSE_MAP_KIND (c);
		    if (GOMP_MAP_COPY_TO_P (map_kind)
			|| map_kind == GOMP_MAP_POINTER
			|| map_kind == GOMP_MAP_TO_PSET
			|| map_kind == GOMP_MAP_FORCE_DEVICEPTR)
		      {
			/* If we need to initialize a temporary
			   with VAR because it is not addressable, and
			   the variable hasn't been initialized yet, then
			   we'll get a warning for the store to avar.
			   Don't warn in that case, the mapping might
			   be implicit.  */
			TREE_NO_WARNING (var) = 1;
			gimplify_assign (avar, var, &ilist);
		      }
		    avar = build_fold_addr_expr (avar);
		    gimplify_assign (x, avar, &ilist);
		    if ((GOMP_MAP_COPY_FROM_P (map_kind)
			 || map_kind == GOMP_MAP_FORCE_DEVICEPTR)
			&& !TYPE_READONLY (TREE_TYPE (var)))
		      {
			x = unshare_expr (x);
			x = build_simple_mem_ref (x);
			gimplify_assign (var, x, &olist);
		      }
		  }
		else
		  {
		    var = build_fold_addr_expr (var);
		    gimplify_assign (x, var, &ilist);
		  }
	      }
	    s = NULL_TREE;
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
	      {
		gcc_checking_assert (is_gimple_omp_oacc (ctx->stmt));
		s = TREE_TYPE (ovar);
		if (TREE_CODE (s) == REFERENCE_TYPE)
		  s = TREE_TYPE (s);
		s = TYPE_SIZE_UNIT (s);
	      }
	    else
	      s = OMP_CLAUSE_SIZE (c);
	    if (s == NULL_TREE)
	      s = TYPE_SIZE_UNIT (TREE_TYPE (ovar));
	    s = fold_convert (size_type_node, s);
	    purpose = size_int (map_idx++);
	    CONSTRUCTOR_APPEND_ELT (vsize, purpose, s);
	    if (TREE_CODE (s) != INTEGER_CST)
	      TREE_STATIC (TREE_VEC_ELT (t, 1)) = 0;

	    unsigned HOST_WIDE_INT tkind, tkind_zero;
	    switch (OMP_CLAUSE_CODE (c))
	      {
	      case OMP_CLAUSE_MAP:
		tkind = OMP_CLAUSE_MAP_KIND (c);
		tkind_zero = tkind;
		if (OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c))
		  switch (tkind)
		    {
		    case GOMP_MAP_ALLOC:
		    case GOMP_MAP_TO:
		    case GOMP_MAP_FROM:
		    case GOMP_MAP_TOFROM:
		    case GOMP_MAP_ALWAYS_TO:
		    case GOMP_MAP_ALWAYS_FROM:
		    case GOMP_MAP_ALWAYS_TOFROM:
		    case GOMP_MAP_RELEASE:
		    case GOMP_MAP_FORCE_TO:
		    case GOMP_MAP_FORCE_FROM:
		    case GOMP_MAP_FORCE_TOFROM:
		    case GOMP_MAP_FORCE_PRESENT:
		      tkind_zero = GOMP_MAP_ZERO_LEN_ARRAY_SECTION;
		      break;
		    case GOMP_MAP_DELETE:
		      tkind_zero = GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION;
		    default:
		      break;
		    }
		if (tkind_zero != tkind)
		  {
		    if (integer_zerop (s))
		      tkind = tkind_zero;
		    else if (integer_nonzerop (s))
		      tkind_zero = tkind;
		  }
		break;
	      case OMP_CLAUSE_FIRSTPRIVATE:
		gcc_checking_assert (is_gimple_omp_oacc (ctx->stmt));
		tkind = GOMP_MAP_TO;
		tkind_zero = tkind;
		break;
	      case OMP_CLAUSE_TO:
		tkind = GOMP_MAP_TO;
		tkind_zero = tkind;
		break;
	      case OMP_CLAUSE_FROM:
		tkind = GOMP_MAP_FROM;
		tkind_zero = tkind;
		break;
	      default:
		gcc_unreachable ();
	      }
	    gcc_checking_assert (tkind
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    gcc_checking_assert (tkind_zero
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    talign = ceil_log2 (talign);
	    tkind |= talign << talign_shift;
	    tkind_zero |= talign << talign_shift;
	    gcc_checking_assert (tkind
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    gcc_checking_assert (tkind_zero
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    if (tkind == tkind_zero)
	      x = build_int_cstu (tkind_type, tkind);
	    else
	      {
		TREE_STATIC (TREE_VEC_ELT (t, 2)) = 0;
		x = build3 (COND_EXPR, tkind_type,
			    fold_build2 (EQ_EXPR, boolean_type_node,
					 unshare_expr (s), size_zero_node),
			    build_int_cstu (tkind_type, tkind_zero),
			    build_int_cstu (tkind_type, tkind));
	      }
	    CONSTRUCTOR_APPEND_ELT (vkind, purpose, x);
	    if (nc && nc != c)
	      c = nc;
	    break;

	  case OMP_CLAUSE_FIRSTPRIVATE:
	    if (is_oacc_parallel (ctx))
	      goto oacc_firstprivate_map;
	    ovar = OMP_CLAUSE_DECL (c);
	    if (omp_is_reference (ovar))
	      talign = TYPE_ALIGN_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
	    else
	      talign = DECL_ALIGN_UNIT (ovar);
	    var = lookup_decl_in_outer_ctx (ovar, ctx);
	    x = build_sender_ref (ovar, ctx);
	    tkind = GOMP_MAP_FIRSTPRIVATE;
	    type = TREE_TYPE (ovar);
	    if (omp_is_reference (ovar))
	      type = TREE_TYPE (type);
	    if ((INTEGRAL_TYPE_P (type)
		 && TYPE_PRECISION (type) <= POINTER_SIZE)
		|| TREE_CODE (type) == POINTER_TYPE)
	      {
		tkind = GOMP_MAP_FIRSTPRIVATE_INT;
		tree t = var;
		if (omp_is_reference (var))
		  t = build_simple_mem_ref (var);
		else if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
		  TREE_NO_WARNING (var) = 1;
		if (TREE_CODE (type) != POINTER_TYPE)
		  t = fold_convert (pointer_sized_int_node, t);
		t = fold_convert (TREE_TYPE (x), t);
		gimplify_assign (x, t, &ilist);
	      }
	    else if (omp_is_reference (var))
	      gimplify_assign (x, var, &ilist);
	    else if (is_gimple_reg (var))
	      {
		tree avar = create_tmp_var (TREE_TYPE (var));
		mark_addressable (avar);
		if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c))
		  TREE_NO_WARNING (var) = 1;
		gimplify_assign (avar, var, &ilist);
		avar = build_fold_addr_expr (avar);
		gimplify_assign (x, avar, &ilist);
	      }
	    else
	      {
		var = build_fold_addr_expr (var);
		gimplify_assign (x, var, &ilist);
	      }
	    if (tkind == GOMP_MAP_FIRSTPRIVATE_INT)
	      s = size_int (0);
	    else if (omp_is_reference (ovar))
	      s = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
	    else
	      s = TYPE_SIZE_UNIT (TREE_TYPE (ovar));
	    s = fold_convert (size_type_node, s);
	    purpose = size_int (map_idx++);
	    CONSTRUCTOR_APPEND_ELT (vsize, purpose, s);
	    if (TREE_CODE (s) != INTEGER_CST)
	      TREE_STATIC (TREE_VEC_ELT (t, 1)) = 0;

	    gcc_checking_assert (tkind
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    talign = ceil_log2 (talign);
	    tkind |= talign << talign_shift;
	    gcc_checking_assert (tkind
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    CONSTRUCTOR_APPEND_ELT (vkind, purpose,
				    build_int_cstu (tkind_type, tkind));
	    break;

	  case OMP_CLAUSE_USE_DEVICE_PTR:
	  case OMP_CLAUSE_IS_DEVICE_PTR:
	    ovar = OMP_CLAUSE_DECL (c);
	    var = lookup_decl_in_outer_ctx (ovar, ctx);
	    x = build_sender_ref (ovar, ctx);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR)
	      tkind = GOMP_MAP_USE_DEVICE_PTR;
	    else
	      tkind = GOMP_MAP_FIRSTPRIVATE_INT;
	    type = TREE_TYPE (ovar);
	    if (TREE_CODE (type) == ARRAY_TYPE)
	      var = build_fold_addr_expr (var);
	    else
	      {
		if (omp_is_reference (ovar))
		  {
		    type = TREE_TYPE (type);
		    if (TREE_CODE (type) != ARRAY_TYPE)
		      var = build_simple_mem_ref (var);
		    var = fold_convert (TREE_TYPE (x), var);
		  }
	      }
	    gimplify_assign (x, var, &ilist);
	    s = size_int (0);
	    purpose = size_int (map_idx++);
	    CONSTRUCTOR_APPEND_ELT (vsize, purpose, s);
	    gcc_checking_assert (tkind
				 < (HOST_WIDE_INT_C (1U) << talign_shift));
	    gcc_checking_assert (tkind
				 <= tree_to_uhwi (TYPE_MAX_VALUE (tkind_type)));
	    CONSTRUCTOR_APPEND_ELT (vkind, purpose,
				    build_int_cstu (tkind_type, tkind));
	    break;
	  }

      gcc_assert (map_idx == map_cnt);

      DECL_INITIAL (TREE_VEC_ELT (t, 1))
	= build_constructor (TREE_TYPE (TREE_VEC_ELT (t, 1)), vsize);
      DECL_INITIAL (TREE_VEC_ELT (t, 2))
	= build_constructor (TREE_TYPE (TREE_VEC_ELT (t, 2)), vkind);
      for (int i = 1; i <= 2; i++)
	if (!TREE_STATIC (TREE_VEC_ELT (t, i)))
	  {
	    gimple_seq initlist = NULL;
	    force_gimple_operand (build1 (DECL_EXPR, void_type_node,
					  TREE_VEC_ELT (t, i)),
				  &initlist, true, NULL_TREE);
	    gimple_seq_add_seq (&ilist, initlist);

	    tree clobber = build_constructor (TREE_TYPE (TREE_VEC_ELT (t, i)),
					      NULL);
	    TREE_THIS_VOLATILE (clobber) = 1;
	    gimple_seq_add_stmt (&olist,
				 gimple_build_assign (TREE_VEC_ELT (t, i),
						      clobber));
	  }

      tree clobber = build_constructor (ctx->record_type, NULL);
      TREE_THIS_VOLATILE (clobber) = 1;
      gimple_seq_add_stmt (&olist, gimple_build_assign (ctx->sender_decl,
							clobber));
    }

  /* Once all the expansions are done, sequence all the different
     fragments inside gimple_omp_body.  */

  new_body = NULL;

  if (offloaded
      && ctx->record_type)
    {
      t = build_fold_addr_expr_loc (loc, ctx->sender_decl);
      /* fixup_child_record_type might have changed receiver_decl's type.  */
      t = fold_convert_loc (loc, TREE_TYPE (ctx->receiver_decl), t);
      gimple_seq_add_stmt (&new_body,
	  		   gimple_build_assign (ctx->receiver_decl, t));
    }
  gimple_seq_add_seq (&new_body, fplist);

  if (offloaded || data_region)
    {
      tree prev = NULL_TREE;
      for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	    tree var, x;
	  default:
	    break;
	  case OMP_CLAUSE_FIRSTPRIVATE:
	    if (is_gimple_omp_oacc (ctx->stmt))
	      break;
	    var = OMP_CLAUSE_DECL (c);
	    if (omp_is_reference (var)
		|| is_gimple_reg_type (TREE_TYPE (var)))
	      {
		tree new_var = lookup_decl (var, ctx);
		tree type;
		type = TREE_TYPE (var);
		if (omp_is_reference (var))
		  type = TREE_TYPE (type);
		if ((INTEGRAL_TYPE_P (type)
		     && TYPE_PRECISION (type) <= POINTER_SIZE)
		    || TREE_CODE (type) == POINTER_TYPE)
		  {
		    x = build_receiver_ref (var, false, ctx);
		    if (TREE_CODE (type) != POINTER_TYPE)
		      x = fold_convert (pointer_sized_int_node, x);
		    x = fold_convert (type, x);
		    gimplify_expr (&x, &new_body, NULL, is_gimple_val,
				   fb_rvalue);
		    if (omp_is_reference (var))
		      {
			tree v = create_tmp_var_raw (type, get_name (var));
			gimple_add_tmp_var (v);
			TREE_ADDRESSABLE (v) = 1;
			gimple_seq_add_stmt (&new_body,
					     gimple_build_assign (v, x));
			x = build_fold_addr_expr (v);
		      }
		    gimple_seq_add_stmt (&new_body,
					 gimple_build_assign (new_var, x));
		  }
		else
		  {
		    x = build_receiver_ref (var, !omp_is_reference (var), ctx);
		    gimplify_expr (&x, &new_body, NULL, is_gimple_val,
				   fb_rvalue);
		    gimple_seq_add_stmt (&new_body,
					 gimple_build_assign (new_var, x));
		  }
	      }
	    else if (is_variable_sized (var))
	      {
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_var = lookup_decl (pvar, ctx);
		x = build_receiver_ref (var, false, ctx);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  case OMP_CLAUSE_PRIVATE:
	    if (is_gimple_omp_oacc (ctx->stmt))
	      break;
	    var = OMP_CLAUSE_DECL (c);
	    if (omp_is_reference (var))
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		tree new_var = lookup_decl (var, ctx);
		x = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_var)));
		if (TREE_CONSTANT (x))
		  {
		    x = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (new_var)),
					    get_name (var));
		    gimple_add_tmp_var (x);
		    TREE_ADDRESSABLE (x) = 1;
		    x = build_fold_addr_expr_loc (clause_loc, x);
		  }
		else
		  break;

		x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  case OMP_CLAUSE_USE_DEVICE_PTR:
	  case OMP_CLAUSE_IS_DEVICE_PTR:
	    var = OMP_CLAUSE_DECL (c);
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR)
	      x = build_sender_ref (var, ctx);
	    else
	      x = build_receiver_ref (var, false, ctx);
	    if (is_variable_sized (var))
	      {
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_var = lookup_decl (pvar, ctx);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    else if (TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
	      {
		tree new_var = lookup_decl (var, ctx);
		new_var = DECL_VALUE_EXPR (new_var);
		gcc_assert (TREE_CODE (new_var) == MEM_REF);
		new_var = TREE_OPERAND (new_var, 0);
		gcc_assert (DECL_P (new_var));
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    else
	      {
		tree type = TREE_TYPE (var);
		tree new_var = lookup_decl (var, ctx);
		if (omp_is_reference (var))
		  {
		    type = TREE_TYPE (type);
		    if (TREE_CODE (type) != ARRAY_TYPE)
		      {
			tree v = create_tmp_var_raw (type, get_name (var));
			gimple_add_tmp_var (v);
			TREE_ADDRESSABLE (v) = 1;
			x = fold_convert (type, x);
			gimplify_expr (&x, &new_body, NULL, is_gimple_val,
				       fb_rvalue);
			gimple_seq_add_stmt (&new_body,
					     gimple_build_assign (v, x));
			x = build_fold_addr_expr (v);
		      }
		  }
		new_var = DECL_VALUE_EXPR (new_var);
		x = fold_convert (TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  }
      /* Handle GOMP_MAP_FIRSTPRIVATE_{POINTER,REFERENCE} in second pass,
	 so that firstprivate vars holding OMP_CLAUSE_SIZE if needed
	 are already handled.  Similarly OMP_CLAUSE_PRIVATE for VLAs
	 or references to VLAs.  */
      for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	    tree var;
	  default:
	    break;
	  case OMP_CLAUSE_MAP:
	    if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
		|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		HOST_WIDE_INT offset = 0;
		gcc_assert (prev);
		var = OMP_CLAUSE_DECL (c);
		if (DECL_P (var)
		    && TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE
		    && is_global_var (maybe_lookup_decl_in_outer_ctx (var,
								      ctx))
		    && varpool_node::get_create (var)->offloadable)
		  break;
		if (TREE_CODE (var) == INDIRECT_REF
		    && TREE_CODE (TREE_OPERAND (var, 0)) == COMPONENT_REF)
		  var = TREE_OPERAND (var, 0);
		if (TREE_CODE (var) == COMPONENT_REF)
		  {
		    var = get_addr_base_and_unit_offset (var, &offset);
		    gcc_assert (var != NULL_TREE && DECL_P (var));
		  }
		else if (DECL_SIZE (var)
			 && TREE_CODE (DECL_SIZE (var)) != INTEGER_CST)
		  {
		    tree var2 = DECL_VALUE_EXPR (var);
		    gcc_assert (TREE_CODE (var2) == INDIRECT_REF);
		    var2 = TREE_OPERAND (var2, 0);
		    gcc_assert (DECL_P (var2));
		    var = var2;
		  }
		tree new_var = lookup_decl (var, ctx), x;
		tree type = TREE_TYPE (new_var);
		bool is_ref;
		if (TREE_CODE (OMP_CLAUSE_DECL (c)) == INDIRECT_REF
		    && (TREE_CODE (TREE_OPERAND (OMP_CLAUSE_DECL (c), 0))
			== COMPONENT_REF))
		  {
		    type = TREE_TYPE (TREE_OPERAND (OMP_CLAUSE_DECL (c), 0));
		    is_ref = true;
		    new_var = build2 (MEM_REF, type,
				      build_fold_addr_expr (new_var),
				      build_int_cst (build_pointer_type (type),
						     offset));
		  }
		else if (TREE_CODE (OMP_CLAUSE_DECL (c)) == COMPONENT_REF)
		  {
		    type = TREE_TYPE (OMP_CLAUSE_DECL (c));
		    is_ref = TREE_CODE (type) == REFERENCE_TYPE;
		    new_var = build2 (MEM_REF, type,
				      build_fold_addr_expr (new_var),
				      build_int_cst (build_pointer_type (type),
						     offset));
		  }
		else
		  is_ref = omp_is_reference (var);
		if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_REFERENCE)
		  is_ref = false;
		bool ref_to_array = false;
		if (is_ref)
		  {
		    type = TREE_TYPE (type);
		    if (TREE_CODE (type) == ARRAY_TYPE)
		      {
			type = build_pointer_type (type);
			ref_to_array = true;
		      }
		  }
		else if (TREE_CODE (type) == ARRAY_TYPE)
		  {
		    tree decl2 = DECL_VALUE_EXPR (new_var);
		    gcc_assert (TREE_CODE (decl2) == MEM_REF);
		    decl2 = TREE_OPERAND (decl2, 0);
		    gcc_assert (DECL_P (decl2));
		    new_var = decl2;
		    type = TREE_TYPE (new_var);
		  }
		x = build_receiver_ref (OMP_CLAUSE_DECL (prev), false, ctx);
		x = fold_convert_loc (clause_loc, type, x);
		if (!integer_zerop (OMP_CLAUSE_SIZE (c)))
		  {
		    tree bias = OMP_CLAUSE_SIZE (c);
		    if (DECL_P (bias))
		      bias = lookup_decl (bias, ctx);
		    bias = fold_convert_loc (clause_loc, sizetype, bias);
		    bias = fold_build1_loc (clause_loc, NEGATE_EXPR, sizetype,
					    bias);
		    x = fold_build2_loc (clause_loc, POINTER_PLUS_EXPR,
					 TREE_TYPE (x), x, bias);
		  }
		if (ref_to_array)
		  x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		if (is_ref && !ref_to_array)
		  {
		    tree t = create_tmp_var_raw (type, get_name (var));
		    gimple_add_tmp_var (t);
		    TREE_ADDRESSABLE (t) = 1;
		    gimple_seq_add_stmt (&new_body,
					 gimple_build_assign (t, x));
		    x = build_fold_addr_expr_loc (clause_loc, t);
		  }
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
		prev = NULL_TREE;
	      }
	    else if (OMP_CLAUSE_CHAIN (c)
		     && OMP_CLAUSE_CODE (OMP_CLAUSE_CHAIN (c))
			== OMP_CLAUSE_MAP
		     && (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
			 == GOMP_MAP_FIRSTPRIVATE_POINTER
			 || (OMP_CLAUSE_MAP_KIND (OMP_CLAUSE_CHAIN (c))
			     == GOMP_MAP_FIRSTPRIVATE_REFERENCE)))
	      prev = c;
	    break;
	  case OMP_CLAUSE_PRIVATE:
	    var = OMP_CLAUSE_DECL (c);
	    if (is_variable_sized (var))
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		tree new_var = lookup_decl (var, ctx);
		tree pvar = DECL_VALUE_EXPR (var);
		gcc_assert (TREE_CODE (pvar) == INDIRECT_REF);
		pvar = TREE_OPERAND (pvar, 0);
		gcc_assert (DECL_P (pvar));
		tree new_pvar = lookup_decl (pvar, ctx);
		tree atmp = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		tree al = size_int (DECL_ALIGN (var));
		tree x = TYPE_SIZE_UNIT (TREE_TYPE (new_var));
		x = build_call_expr_loc (clause_loc, atmp, 2, x, al);
		x = fold_convert_loc (clause_loc, TREE_TYPE (new_pvar), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_pvar, x));
	      }
	    else if (omp_is_reference (var) && !is_gimple_omp_oacc (ctx->stmt))
	      {
		location_t clause_loc = OMP_CLAUSE_LOCATION (c);
		tree new_var = lookup_decl (var, ctx);
		tree x = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (new_var)));
		if (TREE_CONSTANT (x))
		  break;
		else
		  {
		    tree atmp
		      = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
		    tree rtype = TREE_TYPE (TREE_TYPE (new_var));
		    tree al = size_int (TYPE_ALIGN (rtype));
		    x = build_call_expr_loc (clause_loc, atmp, 2, x, al);
		  }

		x = fold_convert_loc (clause_loc, TREE_TYPE (new_var), x);
		gimplify_expr (&x, &new_body, NULL, is_gimple_val, fb_rvalue);
		gimple_seq_add_stmt (&new_body,
				     gimple_build_assign (new_var, x));
	      }
	    break;
	  }

      gimple_seq fork_seq = NULL;
      gimple_seq join_seq = NULL;

      if (is_oacc_parallel (ctx))
	{
	  /* If there are reductions on the offloaded region itself, treat
	     them as a dummy GANG loop.  */
	  tree level = build_int_cst (integer_type_node, GOMP_DIM_GANG);

	  lower_oacc_reductions (gimple_location (ctx->stmt), clauses, level,
				 false, NULL, NULL, &fork_seq, &join_seq, ctx);
	}

      gimple_seq_add_seq (&new_body, fork_seq);
      gimple_seq_add_seq (&new_body, tgt_body);
      gimple_seq_add_seq (&new_body, join_seq);

      if (offloaded)
	new_body = maybe_catch_exception (new_body);

      gimple_seq_add_stmt (&new_body, gimple_build_omp_return (false));
      gimple_omp_set_body (stmt, new_body);
    }

  bind = gimple_build_bind (NULL, NULL,
			    tgt_bind ? gimple_bind_block (tgt_bind)
				     : NULL_TREE);
  gsi_replace (gsi_p, dep_bind ? dep_bind : bind, true);
  gimple_bind_add_seq (bind, ilist);
  gimple_bind_add_stmt (bind, stmt);
  gimple_bind_add_seq (bind, olist);

  pop_gimplify_context (NULL);

  if (dep_bind)
    {
      gimple_bind_add_seq (dep_bind, dep_ilist);
      gimple_bind_add_stmt (dep_bind, bind);
      gimple_bind_add_seq (dep_bind, dep_olist);
      pop_gimplify_context (dep_bind);
    }
}

/* Expand code for an OpenMP teams directive.  */

static void
lower_omp_teams (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gomp_teams *teams_stmt = as_a <gomp_teams *> (gsi_stmt (*gsi_p));
  push_gimplify_context ();

  tree block = make_node (BLOCK);
  gbind *bind = gimple_build_bind (NULL, NULL, block);
  gsi_replace (gsi_p, bind, true);
  gimple_seq bind_body = NULL;
  gimple_seq dlist = NULL;
  gimple_seq olist = NULL;

  tree num_teams = omp_find_clause (gimple_omp_teams_clauses (teams_stmt),
				    OMP_CLAUSE_NUM_TEAMS);
  if (num_teams == NULL_TREE)
    num_teams = build_int_cst (unsigned_type_node, 0);
  else
    {
      num_teams = OMP_CLAUSE_NUM_TEAMS_EXPR (num_teams);
      num_teams = fold_convert (unsigned_type_node, num_teams);
      gimplify_expr (&num_teams, &bind_body, NULL, is_gimple_val, fb_rvalue);
    }
  tree thread_limit = omp_find_clause (gimple_omp_teams_clauses (teams_stmt),
				       OMP_CLAUSE_THREAD_LIMIT);
  if (thread_limit == NULL_TREE)
    thread_limit = build_int_cst (unsigned_type_node, 0);
  else
    {
      thread_limit = OMP_CLAUSE_THREAD_LIMIT_EXPR (thread_limit);
      thread_limit = fold_convert (unsigned_type_node, thread_limit);
      gimplify_expr (&thread_limit, &bind_body, NULL, is_gimple_val,
		     fb_rvalue);
    }

  lower_rec_input_clauses (gimple_omp_teams_clauses (teams_stmt),
			   &bind_body, &dlist, ctx, NULL);
  lower_omp (gimple_omp_body_ptr (teams_stmt), ctx);
  lower_reduction_clauses (gimple_omp_teams_clauses (teams_stmt), &olist, ctx);
  if (!gimple_omp_teams_grid_phony (teams_stmt))
    {
      gimple_seq_add_stmt (&bind_body, teams_stmt);
      location_t loc = gimple_location (teams_stmt);
      tree decl = builtin_decl_explicit (BUILT_IN_GOMP_TEAMS);
      gimple *call = gimple_build_call (decl, 2, num_teams, thread_limit);
      gimple_set_location (call, loc);
      gimple_seq_add_stmt (&bind_body, call);
    }

  gimple_seq_add_seq (&bind_body, gimple_omp_body (teams_stmt));
  gimple_omp_set_body (teams_stmt, NULL);
  gimple_seq_add_seq (&bind_body, olist);
  gimple_seq_add_seq (&bind_body, dlist);
  if (!gimple_omp_teams_grid_phony (teams_stmt))
    gimple_seq_add_stmt (&bind_body, gimple_build_omp_return (true));
  gimple_bind_set_body (bind, bind_body);

  pop_gimplify_context (bind);

  gimple_bind_append_vars (bind, ctx->block_vars);
  BLOCK_VARS (block) = ctx->block_vars;
  if (BLOCK_VARS (block))
    TREE_USED (block) = 1;
}

/* Expand code within an artificial GIMPLE_OMP_GRID_BODY OMP construct.  */

static void
lower_omp_grid_body (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  lower_omp (gimple_omp_body_ptr (stmt), ctx);
  gimple_seq_add_stmt (gimple_omp_body_ptr (stmt),
		       gimple_build_omp_return (false));
}


/* Callback for lower_omp_1.  Return non-NULL if *tp needs to be
   regimplified.  If DATA is non-NULL, lower_omp_1 is outside
   of OMP context, but with task_shared_vars set.  */

static tree
lower_omp_regimplify_p (tree *tp, int *walk_subtrees,
    			void *data)
{
  tree t = *tp;

  /* Any variable with DECL_VALUE_EXPR needs to be regimplified.  */
  if (VAR_P (t) && data == NULL && DECL_HAS_VALUE_EXPR_P (t))
    return t;

  if (task_shared_vars
      && DECL_P (t)
      && bitmap_bit_p (task_shared_vars, DECL_UID (t)))
    return t;

  /* If a global variable has been privatized, TREE_CONSTANT on
     ADDR_EXPR might be wrong.  */
  if (data == NULL && TREE_CODE (t) == ADDR_EXPR)
    recompute_tree_invariant_for_addr_expr (t);

  *walk_subtrees = !IS_TYPE_OR_DECL_P (t);
  return NULL_TREE;
}

/* Data to be communicated between lower_omp_regimplify_operands and
   lower_omp_regimplify_operands_p.  */

struct lower_omp_regimplify_operands_data
{
  omp_context *ctx;
  vec<tree> *decls;
};

/* Helper function for lower_omp_regimplify_operands.  Find
   omp_member_access_dummy_var vars and adjust temporarily their
   DECL_VALUE_EXPRs if needed.  */

static tree
lower_omp_regimplify_operands_p (tree *tp, int *walk_subtrees,
				 void *data)
{
  tree t = omp_member_access_dummy_var (*tp);
  if (t)
    {
      struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
      lower_omp_regimplify_operands_data *ldata
	= (lower_omp_regimplify_operands_data *) wi->info;
      tree o = maybe_lookup_decl (t, ldata->ctx);
      if (o != t)
	{
	  ldata->decls->safe_push (DECL_VALUE_EXPR (*tp));
	  ldata->decls->safe_push (*tp);
	  tree v = unshare_and_remap (DECL_VALUE_EXPR (*tp), t, o);
	  SET_DECL_VALUE_EXPR (*tp, v);
	}
    }
  *walk_subtrees = !IS_TYPE_OR_DECL_P (*tp);
  return NULL_TREE;
}

/* Wrapper around gimple_regimplify_operands that adjusts DECL_VALUE_EXPRs
   of omp_member_access_dummy_var vars during regimplification.  */

static void
lower_omp_regimplify_operands (omp_context *ctx, gimple *stmt,
			       gimple_stmt_iterator *gsi_p)
{
  auto_vec<tree, 10> decls;
  if (ctx)
    {
      struct walk_stmt_info wi;
      memset (&wi, '\0', sizeof (wi));
      struct lower_omp_regimplify_operands_data data;
      data.ctx = ctx;
      data.decls = &decls;
      wi.info = &data;
      walk_gimple_op (stmt, lower_omp_regimplify_operands_p, &wi);
    }
  gimple_regimplify_operands (stmt, gsi_p);
  while (!decls.is_empty ())
    {
      tree t = decls.pop ();
      tree v = decls.pop ();
      SET_DECL_VALUE_EXPR (t, v);
    }
}

static void
lower_omp_1 (gimple_stmt_iterator *gsi_p, omp_context *ctx)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  struct walk_stmt_info wi;
  gcall *call_stmt;

  if (gimple_has_location (stmt))
    input_location = gimple_location (stmt);

  if (task_shared_vars)
    memset (&wi, '\0', sizeof (wi));

  /* If we have issued syntax errors, avoid doing any heavy lifting.
     Just replace the OMP directives with a NOP to avoid
     confusing RTL expansion.  */
  if (seen_error () && is_gimple_omp (stmt))
    {
      gsi_replace (gsi_p, gimple_build_nop (), true);
      return;
    }

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      {
	gcond *cond_stmt = as_a <gcond *> (stmt);
	if ((ctx || task_shared_vars)
	    && (walk_tree (gimple_cond_lhs_ptr (cond_stmt),
			   lower_omp_regimplify_p,
			   ctx ? NULL : &wi, NULL)
		|| walk_tree (gimple_cond_rhs_ptr (cond_stmt),
			      lower_omp_regimplify_p,
			      ctx ? NULL : &wi, NULL)))
	  lower_omp_regimplify_operands (ctx, cond_stmt, gsi_p);
      }
      break;
    case GIMPLE_CATCH:
      lower_omp (gimple_catch_handler_ptr (as_a <gcatch *> (stmt)), ctx);
      break;
    case GIMPLE_EH_FILTER:
      lower_omp (gimple_eh_filter_failure_ptr (stmt), ctx);
      break;
    case GIMPLE_TRY:
      lower_omp (gimple_try_eval_ptr (stmt), ctx);
      lower_omp (gimple_try_cleanup_ptr (stmt), ctx);
      break;
    case GIMPLE_TRANSACTION:
      lower_omp (gimple_transaction_body_ptr (as_a <gtransaction *> (stmt)),
		 ctx);
      break;
    case GIMPLE_BIND:
      lower_omp (gimple_bind_body_ptr (as_a <gbind *> (stmt)), ctx);
      break;
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      if (ctx->cancellable)
	ctx->cancel_label = create_artificial_label (UNKNOWN_LOCATION);
      lower_omp_taskreg (gsi_p, ctx);
      break;
    case GIMPLE_OMP_FOR:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      if (ctx->cancellable)
	ctx->cancel_label = create_artificial_label (UNKNOWN_LOCATION);
      lower_omp_for (gsi_p, ctx);
      break;
    case GIMPLE_OMP_SECTIONS:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      if (ctx->cancellable)
	ctx->cancel_label = create_artificial_label (UNKNOWN_LOCATION);
      lower_omp_sections (gsi_p, ctx);
      break;
    case GIMPLE_OMP_SINGLE:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_single (gsi_p, ctx);
      break;
    case GIMPLE_OMP_MASTER:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_master (gsi_p, ctx);
      break;
    case GIMPLE_OMP_TASKGROUP:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_taskgroup (gsi_p, ctx);
      break;
    case GIMPLE_OMP_ORDERED:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_ordered (gsi_p, ctx);
      break;
    case GIMPLE_OMP_CRITICAL:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_critical (gsi_p, ctx);
      break;
    case GIMPLE_OMP_ATOMIC_LOAD:
      if ((ctx || task_shared_vars)
	  && walk_tree (gimple_omp_atomic_load_rhs_ptr (
			  as_a <gomp_atomic_load *> (stmt)),
			lower_omp_regimplify_p, ctx ? NULL : &wi, NULL))
	lower_omp_regimplify_operands (ctx, stmt, gsi_p);
      break;
    case GIMPLE_OMP_TARGET:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_target (gsi_p, ctx);
      break;
    case GIMPLE_OMP_TEAMS:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_teams (gsi_p, ctx);
      break;
    case GIMPLE_OMP_GRID_BODY:
      ctx = maybe_lookup_ctx (stmt);
      gcc_assert (ctx);
      lower_omp_grid_body (gsi_p, ctx);
      break;
    case GIMPLE_CALL:
      tree fndecl;
      call_stmt = as_a <gcall *> (stmt);
      fndecl = gimple_call_fndecl (call_stmt);
      if (fndecl
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (fndecl))
	  {
	  case BUILT_IN_GOMP_BARRIER:
	    if (ctx == NULL)
	      break;
	    /* FALLTHRU */
	  case BUILT_IN_GOMP_CANCEL:
	  case BUILT_IN_GOMP_CANCELLATION_POINT:
	    omp_context *cctx;
	    cctx = ctx;
	    if (gimple_code (cctx->stmt) == GIMPLE_OMP_SECTION)
	      cctx = cctx->outer;
	    gcc_assert (gimple_call_lhs (call_stmt) == NULL_TREE);
	    if (!cctx->cancellable)
	      {
		if (DECL_FUNCTION_CODE (fndecl)
		    == BUILT_IN_GOMP_CANCELLATION_POINT)
		  {
		    stmt = gimple_build_nop ();
		    gsi_replace (gsi_p, stmt, false);
		  }
		break;
	      }
	    if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_GOMP_BARRIER)
	      {
		fndecl = builtin_decl_explicit (BUILT_IN_GOMP_BARRIER_CANCEL);
		gimple_call_set_fndecl (call_stmt, fndecl);
		gimple_call_set_fntype (call_stmt, TREE_TYPE (fndecl));
	      }
	    tree lhs;
	    lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (fndecl)));
	    gimple_call_set_lhs (call_stmt, lhs);
	    tree fallthru_label;
	    fallthru_label = create_artificial_label (UNKNOWN_LOCATION);
	    gimple *g;
	    g = gimple_build_label (fallthru_label);
	    gsi_insert_after (gsi_p, g, GSI_SAME_STMT);
	    g = gimple_build_cond (NE_EXPR, lhs,
				   fold_convert (TREE_TYPE (lhs),
						 boolean_false_node),
				   cctx->cancel_label, fallthru_label);
	    gsi_insert_after (gsi_p, g, GSI_SAME_STMT);
	    break;
	  default:
	    break;
	  }
      /* FALLTHRU */
    default:
      if ((ctx || task_shared_vars)
	  && walk_gimple_op (stmt, lower_omp_regimplify_p,
			     ctx ? NULL : &wi))
	{
	  /* Just remove clobbers, this should happen only if we have
	     "privatized" local addressable variables in SIMD regions,
	     the clobber isn't needed in that case and gimplifying address
	     of the ARRAY_REF into a pointer and creating MEM_REF based
	     clobber would create worse code than we get with the clobber
	     dropped.  */
	  if (gimple_clobber_p (stmt))
	    {
	      gsi_replace (gsi_p, gimple_build_nop (), true);
	      break;
	    }
	  lower_omp_regimplify_operands (ctx, stmt, gsi_p);
	}
      break;
    }
}

static void
lower_omp (gimple_seq *body, omp_context *ctx)
{
  location_t saved_location = input_location;
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (*body); !gsi_end_p (gsi); gsi_next (&gsi))
    lower_omp_1 (&gsi, ctx);
  /* During gimplification, we haven't folded statments inside offloading
     or taskreg regions (gimplify.c:maybe_fold_stmt); do that now.  */
  if (target_nesting_level || taskreg_nesting_level)
    for (gsi = gsi_start (*body); !gsi_end_p (gsi); gsi_next (&gsi))
      fold_stmt (&gsi);
  input_location = saved_location;
}

/* Main entry point.  */

static unsigned int
execute_lower_omp (void)
{
  gimple_seq body;
  int i;
  omp_context *ctx;

  /* This pass always runs, to provide PROP_gimple_lomp.
     But often, there is nothing to do.  */
  if (flag_cilkplus == 0 && flag_openacc == 0 && flag_openmp == 0
      && flag_openmp_simd == 0)
    return 0;

  all_contexts = splay_tree_new (splay_tree_compare_pointers, 0,
				 delete_omp_context);

  body = gimple_body (current_function_decl);

  if (hsa_gen_requested_p ())
    omp_grid_gridify_all_targets (&body);

  scan_omp (&body, NULL);
  gcc_assert (taskreg_nesting_level == 0);
  FOR_EACH_VEC_ELT (taskreg_contexts, i, ctx)
    finish_taskreg_scan (ctx);
  taskreg_contexts.release ();

  if (all_contexts->root)
    {
      if (task_shared_vars)
	push_gimplify_context ();
      lower_omp (&body, NULL);
      if (task_shared_vars)
	pop_gimplify_context (NULL);
    }

  if (all_contexts)
    {
      splay_tree_delete (all_contexts);
      all_contexts = NULL;
    }
  BITMAP_FREE (task_shared_vars);
  return 0;
}

namespace {

const pass_data pass_data_lower_omp =
{
  GIMPLE_PASS, /* type */
  "omplower", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  PROP_gimple_lomp | PROP_gimple_lomp_dev, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_omp : public gimple_opt_pass
{
public:
  pass_lower_omp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_omp, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return execute_lower_omp (); }

}; // class pass_lower_omp

} // anon namespace

gimple_opt_pass *
make_pass_lower_omp (gcc::context *ctxt)
{
  return new pass_lower_omp (ctxt);
}

/* The following is a utility to diagnose structured block violations.
   It is not part of the "omplower" pass, as that's invoked too late.  It
   should be invoked by the respective front ends after gimplification.  */

static splay_tree all_labels;

/* Check for mismatched contexts and generate an error if needed.  Return
   true if an error is detected.  */

static bool
diagnose_sb_0 (gimple_stmt_iterator *gsi_p,
	       gimple *branch_ctx, gimple *label_ctx)
{
  gcc_checking_assert (!branch_ctx || is_gimple_omp (branch_ctx));
  gcc_checking_assert (!label_ctx || is_gimple_omp (label_ctx));

  if (label_ctx == branch_ctx)
    return false;

  const char* kind = NULL;

  if (flag_cilkplus)
    {
      if ((branch_ctx
	   && gimple_code (branch_ctx) == GIMPLE_OMP_FOR
	   && gimple_omp_for_kind (branch_ctx) == GF_OMP_FOR_KIND_CILKSIMD)
	  || (label_ctx
	      && gimple_code (label_ctx) == GIMPLE_OMP_FOR
	      && gimple_omp_for_kind (label_ctx) == GF_OMP_FOR_KIND_CILKSIMD))
	kind = "Cilk Plus";
    }
  if (flag_openacc)
    {
      if ((branch_ctx && is_gimple_omp_oacc (branch_ctx))
	  || (label_ctx && is_gimple_omp_oacc (label_ctx)))
	{
	  gcc_checking_assert (kind == NULL);
	  kind = "OpenACC";
	}
    }
  if (kind == NULL)
    {
      gcc_checking_assert (flag_openmp || flag_openmp_simd);
      kind = "OpenMP";
    }

  /* Previously we kept track of the label's entire context in diagnose_sb_[12]
     so we could traverse it and issue a correct "exit" or "enter" error
     message upon a structured block violation.

     We built the context by building a list with tree_cons'ing, but there is
     no easy counterpart in gimple tuples.  It seems like far too much work
     for issuing exit/enter error messages.  If someone really misses the
     distinct error message... patches welcome.  */

#if 0
  /* Try to avoid confusing the user by producing and error message
     with correct "exit" or "enter" verbiage.  We prefer "exit"
     unless we can show that LABEL_CTX is nested within BRANCH_CTX.  */
  if (branch_ctx == NULL)
    exit_p = false;
  else
    {
      while (label_ctx)
	{
	  if (TREE_VALUE (label_ctx) == branch_ctx)
	    {
	      exit_p = false;
	      break;
	    }
	  label_ctx = TREE_CHAIN (label_ctx);
	}
    }

  if (exit_p)
    error ("invalid exit from %s structured block", kind);
  else
    error ("invalid entry to %s structured block", kind);
#endif

  /* If it's obvious we have an invalid entry, be specific about the error.  */
  if (branch_ctx == NULL)
    error ("invalid entry to %s structured block", kind);
  else
    {
      /* Otherwise, be vague and lazy, but efficient.  */
      error ("invalid branch to/from %s structured block", kind);
    }

  gsi_replace (gsi_p, gimple_build_nop (), false);
  return true;
}

/* Pass 1: Create a minimal tree of structured blocks, and record
   where each label is found.  */

static tree
diagnose_sb_1 (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
    	       struct walk_stmt_info *wi)
{
  gimple *context = (gimple *) wi->info;
  gimple *inner_context;
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;

  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_TASKGROUP:
      /* The minimal context here is just the current OMP construct.  */
      inner_context = stmt;
      wi->info = inner_context;
      walk_gimple_seq (gimple_omp_body (stmt), diagnose_sb_1, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_OMP_FOR:
      inner_context = stmt;
      wi->info = inner_context;
      /* gimple_omp_for_{index,initial,final} are all DECLs; no need to
	 walk them.  */
      walk_gimple_seq (gimple_omp_for_pre_body (stmt),
	  	       diagnose_sb_1, NULL, wi);
      walk_gimple_seq (gimple_omp_body (stmt), diagnose_sb_1, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_LABEL:
      splay_tree_insert (all_labels,
			 (splay_tree_key) gimple_label_label (
					    as_a <glabel *> (stmt)),
			 (splay_tree_value) context);
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Pass 2: Check each branch and see if its context differs from that of
   the destination label's context.  */

static tree
diagnose_sb_2 (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
    	       struct walk_stmt_info *wi)
{
  gimple *context = (gimple *) wi->info;
  splay_tree_node n;
  gimple *stmt = gsi_stmt (*gsi_p);

  *handled_ops_p = true;

  switch (gimple_code (stmt))
    {
    WALK_SUBSTMTS;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_TASKGROUP:
      wi->info = stmt;
      walk_gimple_seq_mod (gimple_omp_body_ptr (stmt), diagnose_sb_2, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_OMP_FOR:
      wi->info = stmt;
      /* gimple_omp_for_{index,initial,final} are all DECLs; no need to
	 walk them.  */
      walk_gimple_seq_mod (gimple_omp_for_pre_body_ptr (stmt),
			   diagnose_sb_2, NULL, wi);
      walk_gimple_seq_mod (gimple_omp_body_ptr (stmt), diagnose_sb_2, NULL, wi);
      wi->info = context;
      break;

    case GIMPLE_COND:
	{
	  gcond *cond_stmt = as_a <gcond *> (stmt);
	  tree lab = gimple_cond_true_label (cond_stmt);
	  if (lab)
	    {
	      n = splay_tree_lookup (all_labels,
				     (splay_tree_key) lab);
	      diagnose_sb_0 (gsi_p, context,
			     n ? (gimple *) n->value : NULL);
	    }
	  lab = gimple_cond_false_label (cond_stmt);
	  if (lab)
	    {
	      n = splay_tree_lookup (all_labels,
				     (splay_tree_key) lab);
	      diagnose_sb_0 (gsi_p, context,
			     n ? (gimple *) n->value : NULL);
	    }
	}
      break;

    case GIMPLE_GOTO:
      {
	tree lab = gimple_goto_dest (stmt);
	if (TREE_CODE (lab) != LABEL_DECL)
	  break;

	n = splay_tree_lookup (all_labels, (splay_tree_key) lab);
	diagnose_sb_0 (gsi_p, context, n ? (gimple *) n->value : NULL);
      }
      break;

    case GIMPLE_SWITCH:
      {
	gswitch *switch_stmt = as_a <gswitch *> (stmt);
	unsigned int i;
	for (i = 0; i < gimple_switch_num_labels (switch_stmt); ++i)
	  {
	    tree lab = CASE_LABEL (gimple_switch_label (switch_stmt, i));
	    n = splay_tree_lookup (all_labels, (splay_tree_key) lab);
	    if (n && diagnose_sb_0 (gsi_p, context, (gimple *) n->value))
	      break;
	  }
      }
      break;

    case GIMPLE_RETURN:
      diagnose_sb_0 (gsi_p, context, NULL);
      break;

    default:
      break;
    }

  return NULL_TREE;
}

static unsigned int
diagnose_omp_structured_block_errors (void)
{
  struct walk_stmt_info wi;
  gimple_seq body = gimple_body (current_function_decl);

  all_labels = splay_tree_new (splay_tree_compare_pointers, 0, 0);

  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq (body, diagnose_sb_1, NULL, &wi);

  memset (&wi, 0, sizeof (wi));
  wi.want_locations = true;
  walk_gimple_seq_mod (&body, diagnose_sb_2, NULL, &wi);

  gimple_set_body (current_function_decl, body);

  splay_tree_delete (all_labels);
  all_labels = NULL;

  return 0;
}

namespace {

const pass_data pass_data_diagnose_omp_blocks =
{
  GIMPLE_PASS, /* type */
  "*diagnose_omp_blocks", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_diagnose_omp_blocks : public gimple_opt_pass
{
public:
  pass_diagnose_omp_blocks (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_diagnose_omp_blocks, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return flag_cilkplus || flag_openacc || flag_openmp || flag_openmp_simd;
  }
  virtual unsigned int execute (function *)
    {
      return diagnose_omp_structured_block_errors ();
    }

}; // class pass_diagnose_omp_blocks

} // anon namespace

gimple_opt_pass *
make_pass_diagnose_omp_blocks (gcc::context *ctxt)
{
  return new pass_diagnose_omp_blocks (ctxt);
}


#include "gt-omp-low.h"
