/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2014 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "tree.h"
#include "alloc-pool.h"
#include "output.h"
#include "toplev.h"
#include "ubsan.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-ubsan.h"
#include "asan.h"
#include "internal-fn.h"
#include "stor-layout.h"
#include "builtins.h"
#include "fold-const.h"
#include "stringpool.h"
#include "predict.h"
#include "tree-ssa-alias.h"
#include "basic-block.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "lto-streamer.h"
#include "cgraph.h"

/* Test if we should instrument vptr access.  */

static bool
cp_ubsan_instrument_vptr_p (tree type)
{
  if (!flag_rtti || flag_sanitize_undefined_trap_on_error)
    return false;

  if (current_function_decl
      && lookup_attribute ("no_sanitize_undefined",
			   DECL_ATTRIBUTES (current_function_decl)))
    return false;

  if (type)
    {
      type = TYPE_MAIN_VARIANT (type);
      if (!CLASS_TYPE_P (type) || !CLASSTYPE_VTABLES (type))
	return false;
    }

  return true;
}

/* Helper function for
   cp_ubsan_maybe_instrument_{member_{call,access},downcast}.
   Instrument vptr access.  */

static tree
cp_ubsan_instrument_vptr (location_t loc, tree op, tree type, bool is_addr,
			  enum ubsan_null_ckind ckind)
{
  type = TYPE_MAIN_VARIANT (type);
  const char *mangled = mangle_type_string (type);
  hashval_t str_hash1 = htab_hash_string (mangled);
  hashval_t str_hash2 = iterative_hash (mangled, strlen (mangled), 0);
  tree str_hash = wide_int_to_tree (uint64_type_node,
				    wi::uhwi (((uint64_t) str_hash1 << 32)
					      | str_hash2, 64));
  if (!is_addr)
    op = build_fold_addr_expr_loc (loc, op);
  op = save_expr (op);
  tree vptr = fold_build3_loc (loc, COMPONENT_REF,
			       TREE_TYPE (TYPE_VFIELD (type)),
			       build_fold_indirect_ref_loc (loc, op),
			       TYPE_VFIELD (type), NULL_TREE);
  vptr = fold_convert_loc (loc, pointer_sized_int_node, vptr);
  vptr = fold_convert_loc (loc, uint64_type_node, vptr);
  if (ckind == UBSAN_DOWNCAST_POINTER)
    vptr = fold_build3 (COND_EXPR, uint64_type_node,
			fold_build2 (NE_EXPR, boolean_type_node, op,
				     build_zero_cst (TREE_TYPE (op))),
			vptr, build_int_cst (uint64_type_node, 0));
  tree ti_decl = get_tinfo_decl (type);
  mark_used (ti_decl);
  tree ptype = build_pointer_type (type);
  tree call
    = build_call_expr_internal_loc (loc, IFN_UBSAN_VPTR,
				    void_type_node, 5, op, vptr, str_hash,
				    build_address (ti_decl),
				    build_int_cst (ptype, ckind));
  TREE_SIDE_EFFECTS (call) = 1;
  return fold_build2 (COMPOUND_EXPR, TREE_TYPE (op), call, op);
}

/* Helper function for
   cp_ubsan_maybe_instrument_{member_{call,access},downcast}.
   Instrument vptr access if it should be instrumented, otherwise return
   NULL_TREE.  */

static tree
cp_ubsan_maybe_instrument_vptr (location_t loc, tree op, tree type,
				bool is_addr, enum ubsan_null_ckind ckind)
{
  if (!cp_ubsan_instrument_vptr_p (type))
    return NULL_TREE;
  return cp_ubsan_instrument_vptr (loc, op, type, is_addr, ckind);
}

/* Instrument a member call (but not constructor call) if needed.  */

void
cp_ubsan_maybe_instrument_member_call (tree stmt)
{
  if (call_expr_nargs (stmt) == 0)
    return;
  tree *opp = &CALL_EXPR_ARG (stmt, 0);
  tree op = *opp;
  if (op == error_mark_node
      || !POINTER_TYPE_P (TREE_TYPE (op)))
    return;
  while (TREE_CODE (op) == COMPOUND_EXPR)
    {
      opp = &TREE_OPERAND (op, 1);
      op = *opp;
    }
  op = cp_ubsan_maybe_instrument_vptr (EXPR_LOCATION (stmt), op,
				       TREE_TYPE (TREE_TYPE (op)),
				       true, UBSAN_MEMBER_CALL);
  if (op)
    *opp = op;
}

/* Data passed to cp_ubsan_check_member_access_r.  */

struct cp_ubsan_check_member_access_data
{
  hash_set<tree> *pset;
  bool is_addr;
};

static tree cp_ubsan_check_member_access_r (tree *, int *, void *);

/* Instrument a member access.  */

static bool
cp_ubsan_maybe_instrument_member_access
     (tree stmt, cp_ubsan_check_member_access_data *ucmd)
{
  if (DECL_ARTIFICIAL (TREE_OPERAND (stmt, 1)))
    return false;

  tree base = TREE_OPERAND (stmt, 0);
  if (!cp_ubsan_instrument_vptr_p (TREE_TYPE (base)))
    return false;

  cp_walk_tree (&base, cp_ubsan_check_member_access_r, ucmd, ucmd->pset);

  base = cp_ubsan_instrument_vptr (EXPR_LOCATION (stmt), base,
				   TREE_TYPE (base), false,
				   UBSAN_MEMBER_ACCESS);
  TREE_OPERAND (stmt, 0)
    = build_fold_indirect_ref_loc (EXPR_LOCATION (stmt), base);
  return true;
}

/* Attempt to instrument member accesses inside of the function.
   cp_ubsan_maybe_instrument_member_access should be called on COMPONENT_REFs
   in the GENERIC IL, but only when the field is actually accessed, not
   merely when it's address is taken.  Therefore we track in is_addr field
   whether in the current context we are processing address taken
   handled components or not.  E.g. for &x->y[w->z] we want to call
   cp_ubsan_maybe_instrument_member_access on *w.z COMPONENT_REF, but
   not on *x.y.  */

static tree
cp_ubsan_check_member_access_r (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p, t;
  cp_ubsan_check_member_access_data *ucmd
    = (cp_ubsan_check_member_access_data *) data;
  switch (TREE_CODE (stmt))
    {
    case ADDR_EXPR:
      t = TREE_OPERAND (stmt, 0);
      while ((TREE_CODE (t) == MEM_REF || INDIRECT_REF_P (t))
	     && TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR)
	t = TREE_OPERAND (TREE_OPERAND (t, 0), 0);
      if (handled_component_p (t))
	{
	  *walk_subtrees = 0;
	  ucmd->is_addr = true;
	  cp_walk_tree (&t, cp_ubsan_check_member_access_r,
			data, ucmd->pset);
	  ucmd->is_addr = false;
	}
      break;
    case MEM_REF:
    case INDIRECT_REF:
      t = TREE_OPERAND (stmt, 0);
      if (TREE_CODE (t) == ADDR_EXPR)
	{
	  *walk_subtrees = 0;
	  t = TREE_OPERAND (stmt, 0);
	  cp_walk_tree (&t, cp_ubsan_check_member_access_r, data, ucmd->pset);
	}
      break;
    case COMPONENT_REF:
      if (!ucmd->is_addr && cp_ubsan_maybe_instrument_member_access (stmt, ucmd))
	{
	  *walk_subtrees = 0;
	  break;
	}
      /* FALLTHRU */
    default:
      if (ucmd->is_addr && handled_component_p (stmt))
	{
	  int i, len = TREE_OPERAND_LENGTH (stmt);
	  *walk_subtrees = 0;
	  if (!handled_component_p (TREE_OPERAND (stmt, 0)))
	    ucmd->is_addr = false;
	  for (i = 0; i < len; i++)
	    {
	      cp_walk_tree (&TREE_OPERAND (stmt, i),
			    cp_ubsan_check_member_access_r, data, ucmd->pset);
	      ucmd->is_addr = false;
	    }
	  ucmd->is_addr = true;
	}
      break;
    }
  return NULL_TREE;
}

/* Instrument all member accesses inside GENERIC *T_P.  */

void
cp_ubsan_instrument_member_accesses (tree *t_p)
{
  if (cp_ubsan_instrument_vptr_p (NULL_TREE))
    {
      hash_set<tree> pset;
      cp_ubsan_check_member_access_data ucmd;
      ucmd.pset = &pset;
      ucmd.is_addr = false;
      cp_walk_tree (t_p, cp_ubsan_check_member_access_r, &ucmd, &pset);
    }
}

/* Instrument downcast.  */

tree
cp_ubsan_maybe_instrument_downcast (location_t loc, tree type, tree op)
{
  if (!POINTER_TYPE_P (type)
      || !POINTER_TYPE_P (TREE_TYPE (op))
      || !CLASS_TYPE_P (TREE_TYPE (type))
      || !CLASS_TYPE_P (TREE_TYPE (TREE_TYPE (op)))
      || !DERIVED_FROM_P (TREE_TYPE (TREE_TYPE (op)), TREE_TYPE (type)))
    return NULL_TREE;

  return cp_ubsan_maybe_instrument_vptr (loc, op, TREE_TYPE (type), true,
					 TREE_CODE (type) == POINTER_TYPE
					 ? UBSAN_DOWNCAST_POINTER
					 : UBSAN_DOWNCAST_REFERENCE);
}

/* Instrument cast to virtual base.  */

tree
cp_ubsan_maybe_instrument_cast_to_vbase (location_t loc, tree type, tree op)
{
  return cp_ubsan_maybe_instrument_vptr (loc, op, type, true,
					 UBSAN_CAST_TO_VBASE);
}
