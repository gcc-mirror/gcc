/* This file contains routines to construct OpenACC and OpenMP constructs,
   called from parsing in the C and C++ front ends.

   Copyright (C) 2005-2020 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>,
		  Diego Novillo <dnovillo@redhat.com>.

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
#include "options.h"
#include "c-common.h"
#include "gimple-expr.h"
#include "c-pragma.h"
#include "stringpool.h"
#include "omp-general.h"
#include "gomp-constants.h"
#include "memmodel.h"
#include "attribs.h"
#include "gimplify.h"
#include "langhooks.h"


/* Complete a #pragma oacc wait construct.  LOC is the location of
   the #pragma.  */

tree
c_finish_oacc_wait (location_t loc, tree parms, tree clauses)
{
  const int nparms = list_length (parms);
  tree stmt, t;
  vec<tree, va_gc> *args;

  vec_alloc (args, nparms + 2);
  stmt = builtin_decl_explicit (BUILT_IN_GOACC_WAIT);

  if (omp_find_clause (clauses, OMP_CLAUSE_ASYNC))
    t = OMP_CLAUSE_ASYNC_EXPR (clauses);
  else
    t = build_int_cst (integer_type_node, GOMP_ASYNC_SYNC);

  args->quick_push (t);
  args->quick_push (build_int_cst (integer_type_node, nparms));

  for (t = parms; t; t = TREE_CHAIN (t))
    {
      if (TREE_CODE (OMP_CLAUSE_WAIT_EXPR (t)) == INTEGER_CST)
	args->quick_push (build_int_cst (integer_type_node,
			TREE_INT_CST_LOW (OMP_CLAUSE_WAIT_EXPR (t))));
      else
	args->quick_push (OMP_CLAUSE_WAIT_EXPR (t));
    }

  stmt = build_call_expr_loc_vec (loc, stmt, args);

  vec_free (args);

  return stmt;
}

/* Complete a #pragma omp master construct.  STMT is the structured-block
   that follows the pragma.  LOC is the location of the #pragma.  */

tree
c_finish_omp_master (location_t loc, tree stmt)
{
  tree t = add_stmt (build1 (OMP_MASTER, void_type_node, stmt));
  SET_EXPR_LOCATION (t, loc);
  return t;
}

/* Complete a #pragma omp taskgroup construct.  BODY is the structured-block
   that follows the pragma.  LOC is the location of the #pragma.  */

tree
c_finish_omp_taskgroup (location_t loc, tree body, tree clauses)
{
  tree stmt = make_node (OMP_TASKGROUP);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TASKGROUP_BODY (stmt) = body;
  OMP_TASKGROUP_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  return add_stmt (stmt);
}

/* Complete a #pragma omp critical construct.  BODY is the structured-block
   that follows the pragma, NAME is the identifier in the pragma, or null
   if it was omitted.  LOC is the location of the #pragma.  */

tree
c_finish_omp_critical (location_t loc, tree body, tree name, tree clauses)
{
  gcc_assert (!clauses || OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_HINT);
  if (name == NULL_TREE
      && clauses != NULL_TREE
      && integer_nonzerop (OMP_CLAUSE_HINT_EXPR (clauses)))
    {
      error_at (OMP_CLAUSE_LOCATION (clauses),
		"%<#pragma omp critical%> with %<hint%> clause requires "
		"a name, except when %<omp_sync_hint_none%> is used");
      return error_mark_node;
    }

  tree stmt = make_node (OMP_CRITICAL);
  TREE_TYPE (stmt) = void_type_node;
  OMP_CRITICAL_BODY (stmt) = body;
  OMP_CRITICAL_NAME (stmt) = name;
  OMP_CRITICAL_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  return add_stmt (stmt);
}

/* Complete a #pragma omp ordered construct.  STMT is the structured-block
   that follows the pragma.  LOC is the location of the #pragma.  */

tree
c_finish_omp_ordered (location_t loc, tree clauses, tree stmt)
{
  tree t = make_node (OMP_ORDERED);
  TREE_TYPE (t) = void_type_node;
  OMP_ORDERED_BODY (t) = stmt;
  if (!flag_openmp	/* flag_openmp_simd */
      && (OMP_CLAUSE_CODE (clauses) != OMP_CLAUSE_SIMD
	  || OMP_CLAUSE_CHAIN (clauses)))
    clauses = build_omp_clause (loc, OMP_CLAUSE_SIMD);
  OMP_ORDERED_CLAUSES (t) = clauses;
  SET_EXPR_LOCATION (t, loc);
  return add_stmt (t);
}


/* Complete a #pragma omp barrier construct.  LOC is the location of
   the #pragma.  */

void
c_finish_omp_barrier (location_t loc)
{
  tree x;

  x = builtin_decl_explicit (BUILT_IN_GOMP_BARRIER);
  x = build_call_expr_loc (loc, x, 0);
  add_stmt (x);
}


/* Complete a #pragma omp taskwait construct.  LOC is the location of the
   pragma.  */

void
c_finish_omp_taskwait (location_t loc)
{
  tree x;

  x = builtin_decl_explicit (BUILT_IN_GOMP_TASKWAIT);
  x = build_call_expr_loc (loc, x, 0);
  add_stmt (x);
}


/* Complete a #pragma omp taskyield construct.  LOC is the location of the
   pragma.  */

void
c_finish_omp_taskyield (location_t loc)
{
  tree x;

  x = builtin_decl_explicit (BUILT_IN_GOMP_TASKYIELD);
  x = build_call_expr_loc (loc, x, 0);
  add_stmt (x);
}


/* Complete a #pragma omp atomic construct.  For CODE OMP_ATOMIC
   the expression to be implemented atomically is LHS opcode= RHS. 
   For OMP_ATOMIC_READ V = LHS, for OMP_ATOMIC_CAPTURE_{NEW,OLD} LHS
   opcode= RHS with the new or old content of LHS returned.
   LOC is the location of the atomic statement.  The value returned
   is either error_mark_node (if the construct was erroneous) or an
   OMP_ATOMIC* node which should be added to the current statement
   tree with add_stmt.  If TEST is set, avoid calling save_expr
   or create_tmp_var*.  */

tree
c_finish_omp_atomic (location_t loc, enum tree_code code,
		     enum tree_code opcode, tree lhs, tree rhs,
		     tree v, tree lhs1, tree rhs1, bool swapped,
		     enum omp_memory_order memory_order, bool test)
{
  tree x, type, addr, pre = NULL_TREE;
  HOST_WIDE_INT bitpos = 0, bitsize = 0;

  if (lhs == error_mark_node || rhs == error_mark_node
      || v == error_mark_node || lhs1 == error_mark_node
      || rhs1 == error_mark_node)
    return error_mark_node;

  /* ??? According to one reading of the OpenMP spec, complex type are
     supported, but there are no atomic stores for any architecture.
     But at least icc 9.0 doesn't support complex types here either.
     And lets not even talk about vector types...  */
  type = TREE_TYPE (lhs);
  if (!INTEGRAL_TYPE_P (type)
      && !POINTER_TYPE_P (type)
      && !SCALAR_FLOAT_TYPE_P (type))
    {
      error_at (loc, "invalid expression type for %<#pragma omp atomic%>");
      return error_mark_node;
    }
  if (TYPE_ATOMIC (type))
    {
      error_at (loc, "%<_Atomic%> expression in %<#pragma omp atomic%>");
      return error_mark_node;
    }

  if (opcode == RDIV_EXPR)
    opcode = TRUNC_DIV_EXPR;

  /* ??? Validate that rhs does not overlap lhs.  */
  tree blhs = NULL;
  if (TREE_CODE (lhs) == COMPONENT_REF
      && TREE_CODE (TREE_OPERAND (lhs, 1)) == FIELD_DECL
      && DECL_C_BIT_FIELD (TREE_OPERAND (lhs, 1))
      && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (lhs, 1)))
    {
      tree field = TREE_OPERAND (lhs, 1);
      tree repr = DECL_BIT_FIELD_REPRESENTATIVE (field);
      if (tree_fits_uhwi_p (DECL_FIELD_OFFSET (field))
	  && tree_fits_uhwi_p (DECL_FIELD_OFFSET (repr)))
	bitpos = (tree_to_uhwi (DECL_FIELD_OFFSET (field))
		  - tree_to_uhwi (DECL_FIELD_OFFSET (repr))) * BITS_PER_UNIT;
      else
	bitpos = 0;
      bitpos += (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field))
		 - tree_to_uhwi (DECL_FIELD_BIT_OFFSET (repr)));
      gcc_assert (tree_fits_shwi_p (DECL_SIZE (field)));
      bitsize = tree_to_shwi (DECL_SIZE (field));
      blhs = lhs;
      type = TREE_TYPE (repr);
      lhs = build3 (COMPONENT_REF, TREE_TYPE (repr), TREE_OPERAND (lhs, 0),
		    repr, TREE_OPERAND (lhs, 2));
    }

  /* Take and save the address of the lhs.  From then on we'll reference it
     via indirection.  */
  addr = build_unary_op (loc, ADDR_EXPR, lhs, false);
  if (addr == error_mark_node)
    return error_mark_node;
  if (!test)
    addr = save_expr (addr);
  if (!test
      && TREE_CODE (addr) != SAVE_EXPR
      && (TREE_CODE (addr) != ADDR_EXPR
	  || !VAR_P (TREE_OPERAND (addr, 0))))
    {
      /* Make sure LHS is simple enough so that goa_lhs_expr_p can recognize
	 it even after unsharing function body.  */
      tree var = create_tmp_var_raw (TREE_TYPE (addr));
      DECL_CONTEXT (var) = current_function_decl;
      addr = build4 (TARGET_EXPR, TREE_TYPE (addr), var, addr, NULL, NULL);
    }
  tree orig_lhs = lhs;
  lhs = build_indirect_ref (loc, addr, RO_NULL);
  tree new_lhs = lhs;

  if (code == OMP_ATOMIC_READ)
    {
      x = build1 (OMP_ATOMIC_READ, type, addr);
      SET_EXPR_LOCATION (x, loc);
      OMP_ATOMIC_MEMORY_ORDER (x) = memory_order;
      if (blhs)
	x = build3_loc (loc, BIT_FIELD_REF, TREE_TYPE (blhs), x,
			bitsize_int (bitsize), bitsize_int (bitpos));
      return build_modify_expr (loc, v, NULL_TREE, NOP_EXPR,
				loc, x, NULL_TREE);
    }

  /* There are lots of warnings, errors, and conversions that need to happen
     in the course of interpreting a statement.  Use the normal mechanisms
     to do this, and then take it apart again.  */
  if (blhs)
    {
      lhs = build3_loc (loc, BIT_FIELD_REF, TREE_TYPE (blhs), lhs,
			bitsize_int (bitsize), bitsize_int (bitpos));
      if (swapped)
	rhs = build_binary_op (loc, opcode, rhs, lhs, true);
      else if (opcode != NOP_EXPR)
	rhs = build_binary_op (loc, opcode, lhs, rhs, true);
      opcode = NOP_EXPR;
    }
  else if (swapped)
    {
      rhs = build_binary_op (loc, opcode, rhs, lhs, true);
      opcode = NOP_EXPR;
    }
  bool save = in_late_binary_op;
  in_late_binary_op = true;
  x = build_modify_expr (loc, blhs ? blhs : lhs, NULL_TREE, opcode,
			 loc, rhs, NULL_TREE);
  in_late_binary_op = save;
  if (x == error_mark_node)
    return error_mark_node;
  if (TREE_CODE (x) == COMPOUND_EXPR)
    {
      pre = TREE_OPERAND (x, 0);
      gcc_assert (TREE_CODE (pre) == SAVE_EXPR || tree_invariant_p (pre));
      x = TREE_OPERAND (x, 1);
    }
  gcc_assert (TREE_CODE (x) == MODIFY_EXPR);
  rhs = TREE_OPERAND (x, 1);

  if (blhs)
    rhs = build3_loc (loc, BIT_INSERT_EXPR, type, new_lhs,
		      rhs, bitsize_int (bitpos));

  /* Punt the actual generation of atomic operations to common code.  */
  if (code == OMP_ATOMIC)
    type = void_type_node;
  x = build2 (code, type, addr, rhs);
  SET_EXPR_LOCATION (x, loc);
  OMP_ATOMIC_MEMORY_ORDER (x) = memory_order;

  /* Generally it is hard to prove lhs1 and lhs are the same memory
     location, just diagnose different variables.  */
  if (rhs1
      && VAR_P (rhs1)
      && VAR_P (orig_lhs)
      && rhs1 != orig_lhs
      && !test)
    {
      if (code == OMP_ATOMIC)
	error_at (loc, "%<#pragma omp atomic update%> uses two different "
		       "variables for memory");
      else
	error_at (loc, "%<#pragma omp atomic capture%> uses two different "
		       "variables for memory");
      return error_mark_node;
    }

  if (lhs1
      && lhs1 != orig_lhs
      && TREE_CODE (lhs1) == COMPONENT_REF
      && TREE_CODE (TREE_OPERAND (lhs1, 1)) == FIELD_DECL
      && DECL_C_BIT_FIELD (TREE_OPERAND (lhs1, 1))
      && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (lhs1, 1)))
    {
      tree field = TREE_OPERAND (lhs1, 1);
      tree repr = DECL_BIT_FIELD_REPRESENTATIVE (field);
      lhs1 = build3 (COMPONENT_REF, TREE_TYPE (repr), TREE_OPERAND (lhs1, 0),
		     repr, TREE_OPERAND (lhs1, 2));
    }
  if (rhs1
      && rhs1 != orig_lhs
      && TREE_CODE (rhs1) == COMPONENT_REF
      && TREE_CODE (TREE_OPERAND (rhs1, 1)) == FIELD_DECL
      && DECL_C_BIT_FIELD (TREE_OPERAND (rhs1, 1))
      && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (rhs1, 1)))
    {
      tree field = TREE_OPERAND (rhs1, 1);
      tree repr = DECL_BIT_FIELD_REPRESENTATIVE (field);
      rhs1 = build3 (COMPONENT_REF, TREE_TYPE (repr), TREE_OPERAND (rhs1, 0),
		     repr, TREE_OPERAND (rhs1, 2));
    }

  if (code != OMP_ATOMIC)
    {
      /* Generally it is hard to prove lhs1 and lhs are the same memory
	 location, just diagnose different variables.  */
      if (lhs1 && VAR_P (lhs1) && VAR_P (orig_lhs))
	{
	  if (lhs1 != orig_lhs && !test)
	    {
	      error_at (loc, "%<#pragma omp atomic capture%> uses two "
			     "different variables for memory");
	      return error_mark_node;
	    }
	}
      if (blhs)
	{
	  x = build3_loc (loc, BIT_FIELD_REF, TREE_TYPE (blhs), x,
			  bitsize_int (bitsize), bitsize_int (bitpos));
	  type = TREE_TYPE (blhs);
	}
      x = build_modify_expr (loc, v, NULL_TREE, NOP_EXPR,
			     loc, x, NULL_TREE);
      if (rhs1 && rhs1 != orig_lhs)
	{
	  tree rhs1addr = build_unary_op (loc, ADDR_EXPR, rhs1, false);
	  if (rhs1addr == error_mark_node)
	    return error_mark_node;
	  x = omit_one_operand_loc (loc, type, x, rhs1addr);
	}
      if (lhs1 && lhs1 != orig_lhs)
	{
	  tree lhs1addr = build_unary_op (loc, ADDR_EXPR, lhs1, false);
	  if (lhs1addr == error_mark_node)
	    return error_mark_node;
	  if (code == OMP_ATOMIC_CAPTURE_OLD)
	    x = omit_one_operand_loc (loc, type, x, lhs1addr);
	  else
	    {
	      if (!test)
		x = save_expr (x);
	      x = omit_two_operands_loc (loc, type, x, x, lhs1addr);
	    }
	}
    }
  else if (rhs1 && rhs1 != orig_lhs)
    {
      tree rhs1addr = build_unary_op (loc, ADDR_EXPR, rhs1, false);
      if (rhs1addr == error_mark_node)
	return error_mark_node;
      x = omit_one_operand_loc (loc, type, x, rhs1addr);
    }

  if (pre)
    x = omit_one_operand_loc (loc, type, x, pre);
  return x;
}


/* Return true if TYPE is the implementation's omp_depend_t.  */

bool
c_omp_depend_t_p (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  return (TREE_CODE (type) == RECORD_TYPE
	  && TYPE_NAME (type)
	  && ((TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	       ? DECL_NAME (TYPE_NAME (type)) : TYPE_NAME (type))
	      == get_identifier ("omp_depend_t"))
	  && (!TYPE_CONTEXT (type)
	      || TREE_CODE (TYPE_CONTEXT (type)) == TRANSLATION_UNIT_DECL)
	  && COMPLETE_TYPE_P (type)
	  && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	  && !compare_tree_int (TYPE_SIZE (type),
				2 * tree_to_uhwi (TYPE_SIZE (ptr_type_node))));
}


/* Complete a #pragma omp depobj construct.  LOC is the location of the
   #pragma.  */

void
c_finish_omp_depobj (location_t loc, tree depobj,
		     enum omp_clause_depend_kind kind, tree clause)
{
  tree t = NULL_TREE;
  if (!error_operand_p (depobj))
    {
      if (!c_omp_depend_t_p (TREE_TYPE (depobj)))
	{
	  error_at (EXPR_LOC_OR_LOC (depobj, loc),
		    "type of %<depobj%> expression is not %<omp_depend_t%>");
	  depobj = error_mark_node;
	}
      else if (TYPE_READONLY (TREE_TYPE (depobj)))
	{
	  error_at (EXPR_LOC_OR_LOC (depobj, loc),
		    "%<const%> qualified %<depobj%> expression");
	  depobj = error_mark_node;
	}
    }
  else
    depobj = error_mark_node;

  if (clause == error_mark_node)
    return;

  if (clause)
    {
      gcc_assert (TREE_CODE (clause) == OMP_CLAUSE
		  && OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_DEPEND);
      if (OMP_CLAUSE_CHAIN (clause))
	error_at (OMP_CLAUSE_LOCATION (clause),
		  "more than one locator in %<depend%> clause on %<depobj%> "
		  "construct");
      switch (OMP_CLAUSE_DEPEND_KIND (clause))
	{
	case OMP_CLAUSE_DEPEND_DEPOBJ:
	  error_at (OMP_CLAUSE_LOCATION (clause),
		    "%<depobj%> dependence type specified in %<depend%> "
		    "clause on %<depobj%> construct");
	  return;
	case OMP_CLAUSE_DEPEND_SOURCE:
	case OMP_CLAUSE_DEPEND_SINK:
	  error_at (OMP_CLAUSE_LOCATION (clause),
		    "%<depend(%s)%> is only allowed in %<omp ordered%>",
		    OMP_CLAUSE_DEPEND_KIND (clause) == OMP_CLAUSE_DEPEND_SOURCE
		    ? "source" : "sink");
	  return;
	case OMP_CLAUSE_DEPEND_IN:
	case OMP_CLAUSE_DEPEND_OUT:
	case OMP_CLAUSE_DEPEND_INOUT:
	case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
	  kind = OMP_CLAUSE_DEPEND_KIND (clause);
	  t = OMP_CLAUSE_DECL (clause);
	  gcc_assert (t);
	  if (TREE_CODE (t) == TREE_LIST
	      && TREE_PURPOSE (t)
	      && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	    {
	      error_at (OMP_CLAUSE_LOCATION (clause),
			"%<iterator%> modifier may not be specified on "
			"%<depobj%> construct");
	      return;
	    }
	  if (TREE_CODE (t) == COMPOUND_EXPR)
	    {
	      tree t1 = build_fold_addr_expr (TREE_OPERAND (t, 1));
	      t = build2 (COMPOUND_EXPR, TREE_TYPE (t1), TREE_OPERAND (t, 0),
			  t1);
	    }
	  else
	    t = build_fold_addr_expr (t);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    gcc_assert (kind != OMP_CLAUSE_DEPEND_SOURCE);

  if (depobj == error_mark_node)
    return;

  depobj = build_fold_addr_expr_loc (EXPR_LOC_OR_LOC (depobj, loc), depobj);
  tree dtype
    = build_pointer_type_for_mode (ptr_type_node, TYPE_MODE (ptr_type_node),
				   true);
  depobj = fold_convert (dtype, depobj);
  tree r;
  if (clause)
    {
      depobj = save_expr (depobj);
      r = build_indirect_ref (loc, depobj, RO_UNARY_STAR);
      add_stmt (build2 (MODIFY_EXPR, void_type_node, r, t));
    }
  int k;
  switch (kind)
    {
    case OMP_CLAUSE_DEPEND_IN:
      k = GOMP_DEPEND_IN;
      break;
    case OMP_CLAUSE_DEPEND_OUT:
      k = GOMP_DEPEND_OUT;
      break;
    case OMP_CLAUSE_DEPEND_INOUT:
      k = GOMP_DEPEND_INOUT;
      break;
    case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
      k = GOMP_DEPEND_MUTEXINOUTSET;
      break;
    case OMP_CLAUSE_DEPEND_LAST:
      k = -1;
      break;
    default:
      gcc_unreachable ();
    }
  t = build_int_cst (ptr_type_node, k);
  depobj = build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (depobj), depobj,
		       TYPE_SIZE_UNIT (ptr_type_node));
  r = build_indirect_ref (loc, depobj, RO_UNARY_STAR);
  add_stmt (build2 (MODIFY_EXPR, void_type_node, r, t));
}


/* Complete a #pragma omp flush construct.  We don't do anything with
   the variable list that the syntax allows.  LOC is the location of
   the #pragma.  */

void
c_finish_omp_flush (location_t loc, int mo)
{
  tree x;

  if (mo == MEMMODEL_LAST)
    {
      x = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
      x = build_call_expr_loc (loc, x, 0);
    }
  else
    {
      x = builtin_decl_explicit (BUILT_IN_ATOMIC_THREAD_FENCE);
      x = build_call_expr_loc (loc, x, 1,
			       build_int_cst (integer_type_node, mo));
    }
  add_stmt (x);
}


/* Check and canonicalize OMP_FOR increment expression.
   Helper function for c_finish_omp_for.  */

static tree
check_omp_for_incr_expr (location_t loc, tree exp, tree decl)
{
  tree t;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (exp))
      || TYPE_PRECISION (TREE_TYPE (exp)) < TYPE_PRECISION (TREE_TYPE (decl)))
    return error_mark_node;

  if (exp == decl)
    return build_int_cst (TREE_TYPE (exp), 0);

  switch (TREE_CODE (exp))
    {
    CASE_CONVERT:
      t = check_omp_for_incr_expr (loc, TREE_OPERAND (exp, 0), decl);
      if (t != error_mark_node)
        return fold_convert_loc (loc, TREE_TYPE (exp), t);
      break;
    case MINUS_EXPR:
      t = check_omp_for_incr_expr (loc, TREE_OPERAND (exp, 0), decl);
      if (t != error_mark_node)
        return fold_build2_loc (loc, MINUS_EXPR,
				TREE_TYPE (exp), t, TREE_OPERAND (exp, 1));
      break;
    case PLUS_EXPR:
      t = check_omp_for_incr_expr (loc, TREE_OPERAND (exp, 0), decl);
      if (t != error_mark_node)
        return fold_build2_loc (loc, PLUS_EXPR,
				TREE_TYPE (exp), t, TREE_OPERAND (exp, 1));
      t = check_omp_for_incr_expr (loc, TREE_OPERAND (exp, 1), decl);
      if (t != error_mark_node)
        return fold_build2_loc (loc, PLUS_EXPR,
				TREE_TYPE (exp), TREE_OPERAND (exp, 0), t);
      break;
    case COMPOUND_EXPR:
      {
	/* cp_build_modify_expr forces preevaluation of the RHS to make
	   sure that it is evaluated before the lvalue-rvalue conversion
	   is applied to the LHS.  Reconstruct the original expression.  */
	tree op0 = TREE_OPERAND (exp, 0);
	if (TREE_CODE (op0) == TARGET_EXPR
	    && !VOID_TYPE_P (TREE_TYPE (op0)))
	  {
	    tree op1 = TREE_OPERAND (exp, 1);
	    tree temp = TARGET_EXPR_SLOT (op0);
	    if (BINARY_CLASS_P (op1)
		&& TREE_OPERAND (op1, 1) == temp)
	      {
		op1 = copy_node (op1);
		TREE_OPERAND (op1, 1) = TARGET_EXPR_INITIAL (op0);
		return check_omp_for_incr_expr (loc, op1, decl);
	      }
	  }
	break;
      }
    default:
      break;
    }

  return error_mark_node;
}

/* If the OMP_FOR increment expression in INCR is of pointer type,
   canonicalize it into an expression handled by gimplify_omp_for()
   and return it.  DECL is the iteration variable.  */

static tree
c_omp_for_incr_canonicalize_ptr (location_t loc, tree decl, tree incr)
{
  if (POINTER_TYPE_P (TREE_TYPE (decl))
      && TREE_OPERAND (incr, 1))
    {
      tree t = fold_convert_loc (loc,
				 sizetype, TREE_OPERAND (incr, 1));

      if (TREE_CODE (incr) == POSTDECREMENT_EXPR
	  || TREE_CODE (incr) == PREDECREMENT_EXPR)
	t = fold_build1_loc (loc, NEGATE_EXPR, sizetype, t);
      t = fold_build_pointer_plus (decl, t);
      incr = build2 (MODIFY_EXPR, void_type_node, decl, t);
    }
  return incr;
}

/* Validate and generate OMP_FOR.
   DECLV is a vector of iteration variables, for each collapsed loop.

   ORIG_DECLV, if non-NULL, is a vector with the original iteration
   variables (prior to any transformations, by say, C++ iterators).

   INITV, CONDV and INCRV are vectors containing initialization
   expressions, controlling predicates and increment expressions.
   BODY is the body of the loop and PRE_BODY statements that go before
   the loop.  */

tree
c_finish_omp_for (location_t locus, enum tree_code code, tree declv,
		  tree orig_declv, tree initv, tree condv, tree incrv,
		  tree body, tree pre_body, bool final_p)
{
  location_t elocus;
  bool fail = false;
  int i;

  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (initv));
  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (condv));
  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (incrv));
  for (i = 0; i < TREE_VEC_LENGTH (declv); i++)
    {
      tree decl = TREE_VEC_ELT (declv, i);
      tree init = TREE_VEC_ELT (initv, i);
      tree cond = TREE_VEC_ELT (condv, i);
      tree incr = TREE_VEC_ELT (incrv, i);

      elocus = locus;
      if (EXPR_HAS_LOCATION (init))
	elocus = EXPR_LOCATION (init);

      /* Validate the iteration variable.  */
      if (!INTEGRAL_TYPE_P (TREE_TYPE (decl))
	  && TREE_CODE (TREE_TYPE (decl)) != POINTER_TYPE)
	{
	  error_at (elocus, "invalid type for iteration variable %qE", decl);
	  fail = true;
	}
      else if (TYPE_ATOMIC (TREE_TYPE (decl)))
	{
	  error_at (elocus, "%<_Atomic%> iteration variable %qE", decl);
	  fail = true;
	  /* _Atomic iterator confuses stuff too much, so we risk ICE
	     trying to diagnose it further.  */
	  continue;
	}

      /* In the case of "for (int i = 0...)", init will be a decl.  It should
	 have a DECL_INITIAL that we can turn into an assignment.  */
      if (init == decl)
	{
	  elocus = DECL_SOURCE_LOCATION (decl);

	  init = DECL_INITIAL (decl);
	  if (init == NULL)
	    {
	      error_at (elocus, "%qE is not initialized", decl);
	      init = integer_zero_node;
	      fail = true;
	    }
	  DECL_INITIAL (decl) = NULL_TREE;

	  init = build_modify_expr (elocus, decl, NULL_TREE, NOP_EXPR,
	      			    /* FIXME diagnostics: This should
				       be the location of the INIT.  */
	      			    elocus,
				    init,
				    NULL_TREE);
	}
      if (init != error_mark_node)
	{
	  gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
	  gcc_assert (TREE_OPERAND (init, 0) == decl);
	}

      if (cond == NULL_TREE)
	{
	  error_at (elocus, "missing controlling predicate");
	  fail = true;
	}
      else
	{
	  bool cond_ok = false;

	  /* E.g. C sizeof (vla) could add COMPOUND_EXPRs with
	     evaluation of the vla VAR_DECL.  We need to readd
	     them to the non-decl operand.  See PR45784.  */
	  while (TREE_CODE (cond) == COMPOUND_EXPR)
	    cond = TREE_OPERAND (cond, 1);

	  if (EXPR_HAS_LOCATION (cond))
	    elocus = EXPR_LOCATION (cond);

	  if (TREE_CODE (cond) == LT_EXPR
	      || TREE_CODE (cond) == LE_EXPR
	      || TREE_CODE (cond) == GT_EXPR
	      || TREE_CODE (cond) == GE_EXPR
	      || TREE_CODE (cond) == NE_EXPR
	      || TREE_CODE (cond) == EQ_EXPR)
	    {
	      tree op0 = TREE_OPERAND (cond, 0);
	      tree op1 = TREE_OPERAND (cond, 1);

	      /* 2.5.1.  The comparison in the condition is computed in
		 the type of DECL, otherwise the behavior is undefined.

		 For example:
		 long n; int i;
		 i < n;

		 according to ISO will be evaluated as:
		 (long)i < n;

		 We want to force:
		 i < (int)n;  */
	      if (TREE_CODE (op0) == NOP_EXPR
		  && decl == TREE_OPERAND (op0, 0))
		{
		  TREE_OPERAND (cond, 0) = TREE_OPERAND (op0, 0);
		  TREE_OPERAND (cond, 1)
		    = fold_build1_loc (elocus, NOP_EXPR, TREE_TYPE (decl),
				   TREE_OPERAND (cond, 1));
		}
	      else if (TREE_CODE (op1) == NOP_EXPR
		       && decl == TREE_OPERAND (op1, 0))
		{
		  TREE_OPERAND (cond, 1) = TREE_OPERAND (op1, 0);
		  TREE_OPERAND (cond, 0)
		    = fold_build1_loc (elocus, NOP_EXPR, TREE_TYPE (decl),
				   TREE_OPERAND (cond, 0));
		}

	      if (decl == TREE_OPERAND (cond, 0))
		cond_ok = true;
	      else if (decl == TREE_OPERAND (cond, 1))
		{
		  TREE_SET_CODE (cond,
				 swap_tree_comparison (TREE_CODE (cond)));
		  TREE_OPERAND (cond, 1) = TREE_OPERAND (cond, 0);
		  TREE_OPERAND (cond, 0) = decl;
		  cond_ok = true;
		}

	      if (TREE_CODE (cond) == NE_EXPR
		  || TREE_CODE (cond) == EQ_EXPR)
		{
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (decl)))
		    {
		      if (code == OACC_LOOP || TREE_CODE (cond) == EQ_EXPR)
			cond_ok = false;
		    }
		  else if (operand_equal_p (TREE_OPERAND (cond, 1),
					    TYPE_MIN_VALUE (TREE_TYPE (decl)),
					    0))
		    TREE_SET_CODE (cond, TREE_CODE (cond) == NE_EXPR
					 ? GT_EXPR : LE_EXPR);
		  else if (operand_equal_p (TREE_OPERAND (cond, 1),
					    TYPE_MAX_VALUE (TREE_TYPE (decl)),
					    0))
		    TREE_SET_CODE (cond, TREE_CODE (cond) == NE_EXPR
					 ? LT_EXPR : GE_EXPR);
		  else if (code == OACC_LOOP || TREE_CODE (cond) == EQ_EXPR)
		    cond_ok = false;
		}

	      if (cond_ok && TREE_VEC_ELT (condv, i) != cond)
		{
		  tree ce = NULL_TREE, *pce = &ce;
		  tree type = TREE_TYPE (TREE_OPERAND (cond, 1));
		  for (tree c = TREE_VEC_ELT (condv, i); c != cond;
		       c = TREE_OPERAND (c, 1))
		    {
		      *pce = build2 (COMPOUND_EXPR, type, TREE_OPERAND (c, 0),
				     TREE_OPERAND (cond, 1));
		      pce = &TREE_OPERAND (*pce, 1);
		    }
		  TREE_OPERAND (cond, 1) = ce;
		  TREE_VEC_ELT (condv, i) = cond;
		}
	    }

	  if (!cond_ok)
	    {
	      error_at (elocus, "invalid controlling predicate");
	      fail = true;
	    }
	}

      if (incr == NULL_TREE)
	{
	  error_at (elocus, "missing increment expression");
	  fail = true;
	}
      else
	{
	  bool incr_ok = false;

	  if (EXPR_HAS_LOCATION (incr))
	    elocus = EXPR_LOCATION (incr);

	  /* Check all the valid increment expressions: v++, v--, ++v, --v,
	     v = v + incr, v = incr + v and v = v - incr.  */
	  switch (TREE_CODE (incr))
	    {
	    case POSTINCREMENT_EXPR:
	    case PREINCREMENT_EXPR:
	    case POSTDECREMENT_EXPR:
	    case PREDECREMENT_EXPR:
	      if (TREE_OPERAND (incr, 0) != decl)
		break;

	      incr_ok = true;
	      if (!fail
		  && TREE_CODE (cond) == NE_EXPR
		  && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
		  && TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)))
		  && (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl))))
		      != INTEGER_CST))
		{
		  /* For pointer to VLA, transform != into < or >
		     depending on whether incr is increment or decrement.  */
		  if (TREE_CODE (incr) == PREINCREMENT_EXPR
		      || TREE_CODE (incr) == POSTINCREMENT_EXPR)
		    TREE_SET_CODE (cond, LT_EXPR);
		  else
		    TREE_SET_CODE (cond, GT_EXPR);
		}
	      incr = c_omp_for_incr_canonicalize_ptr (elocus, decl, incr);
	      break;

	    case COMPOUND_EXPR:
	      if (TREE_CODE (TREE_OPERAND (incr, 0)) != SAVE_EXPR
		  || TREE_CODE (TREE_OPERAND (incr, 1)) != MODIFY_EXPR)
		break;
	      incr = TREE_OPERAND (incr, 1);
	      /* FALLTHRU */
	    case MODIFY_EXPR:
	      if (TREE_OPERAND (incr, 0) != decl)
		break;
	      if (TREE_OPERAND (incr, 1) == decl)
		break;
	      if (TREE_CODE (TREE_OPERAND (incr, 1)) == PLUS_EXPR
		  && (TREE_OPERAND (TREE_OPERAND (incr, 1), 0) == decl
		      || TREE_OPERAND (TREE_OPERAND (incr, 1), 1) == decl))
		incr_ok = true;
	      else if ((TREE_CODE (TREE_OPERAND (incr, 1)) == MINUS_EXPR
			|| (TREE_CODE (TREE_OPERAND (incr, 1))
			    == POINTER_PLUS_EXPR))
		       && TREE_OPERAND (TREE_OPERAND (incr, 1), 0) == decl)
		incr_ok = true;
	      else
		{
		  tree t = check_omp_for_incr_expr (elocus,
						    TREE_OPERAND (incr, 1),
						    decl);
		  if (t != error_mark_node)
		    {
		      incr_ok = true;
		      t = build2 (PLUS_EXPR, TREE_TYPE (decl), decl, t);
		      incr = build2 (MODIFY_EXPR, void_type_node, decl, t);
		    }
		}
	      if (!fail
		  && incr_ok
		  && TREE_CODE (cond) == NE_EXPR)
		{
		  tree i = TREE_OPERAND (incr, 1);
		  i = TREE_OPERAND (i, TREE_OPERAND (i, 0) == decl);
		  i = c_fully_fold (i, false, NULL);
		  if (!final_p
		      && TREE_CODE (i) != INTEGER_CST)
		    ;
		  else if (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE)
		    {
		      tree unit
			= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)));
		      if (unit)
			{
			  enum tree_code ccode = GT_EXPR;
			  unit = c_fully_fold (unit, false, NULL);
			  i = fold_convert (TREE_TYPE (unit), i);
			  if (operand_equal_p (unit, i, 0))
			    ccode = LT_EXPR;
			  if (ccode == GT_EXPR)
			    {
			      i = fold_unary (NEGATE_EXPR, TREE_TYPE (i), i);
			      if (i == NULL_TREE
				  || !operand_equal_p (unit, i, 0))
				{
				  error_at (elocus,
					    "increment is not constant 1 or "
					    "-1 for %<!=%> condition");
				  fail = true;
				}
			    }
			  if (TREE_CODE (unit) != INTEGER_CST)
			    /* For pointer to VLA, transform != into < or >
			       depending on whether the pointer is
			       incremented or decremented in each
			       iteration.  */
			    TREE_SET_CODE (cond, ccode);
			}
		    }
		  else
		    {
		      if (!integer_onep (i) && !integer_minus_onep (i))
			{
			  error_at (elocus,
				    "increment is not constant 1 or -1 for"
				    " %<!=%> condition");
			  fail = true;
			}
		    }
		}
	      break;

	    default:
	      break;
	    }
	  if (!incr_ok)
	    {
	      error_at (elocus, "invalid increment expression");
	      fail = true;
	    }
	}

      TREE_VEC_ELT (initv, i) = init;
      TREE_VEC_ELT (incrv, i) = incr;
    }

  if (fail)
    return NULL;
  else
    {
      tree t = make_node (code);

      TREE_TYPE (t) = void_type_node;
      OMP_FOR_INIT (t) = initv;
      OMP_FOR_COND (t) = condv;
      OMP_FOR_INCR (t) = incrv;
      OMP_FOR_BODY (t) = body;
      OMP_FOR_PRE_BODY (t) = pre_body;
      OMP_FOR_ORIG_DECLS (t) = orig_declv;

      SET_EXPR_LOCATION (t, locus);
      return t;
    }
}

/* Type for passing data in between c_omp_check_loop_iv and
   c_omp_check_loop_iv_r.  */

struct c_omp_check_loop_iv_data
{
  tree declv;
  bool fail;
  bool maybe_nonrect;
  location_t stmt_loc;
  location_t expr_loc;
  int kind;
  int idx;
  walk_tree_lh lh;
  hash_set<tree> *ppset;
};

/* Return -1 if DECL is not a loop iterator in loop nest D, otherwise
   return the index of the loop in which it is an iterator.
   Return TREE_VEC_LENGTH (d->declv) if it is a C++ range for iterator.  */

static int
c_omp_is_loop_iterator (tree decl, struct c_omp_check_loop_iv_data *d)
{
  for (int i = 0; i < TREE_VEC_LENGTH (d->declv); i++)
    if (decl == TREE_VEC_ELT (d->declv, i)
	|| (TREE_CODE (TREE_VEC_ELT (d->declv, i)) == TREE_LIST
	    && decl == TREE_PURPOSE (TREE_VEC_ELT (d->declv, i))))
      return i;
    else if (TREE_CODE (TREE_VEC_ELT (d->declv, i)) == TREE_LIST
	     && TREE_CHAIN (TREE_VEC_ELT (d->declv, i))
	     && (TREE_CODE (TREE_CHAIN (TREE_VEC_ELT (d->declv, i)))
		 == TREE_VEC)
	     && decl == TREE_VEC_ELT (TREE_CHAIN (TREE_VEC_ELT (d->declv,
						  i)), 2))
      return TREE_VEC_LENGTH (d->declv);
  return -1;
}

/* Helper function called via walk_tree, to diagnose uses
   of associated loop IVs inside of lb, b and incr expressions
   of OpenMP loops.  */
   
static tree
c_omp_check_loop_iv_r (tree *tp, int *walk_subtrees, void *data)
{
  struct c_omp_check_loop_iv_data *d
    = (struct c_omp_check_loop_iv_data *) data;
  if (DECL_P (*tp))
    {
      int idx = c_omp_is_loop_iterator (*tp, d);
      if (idx == -1)
	return NULL_TREE;

      if ((d->kind & 4) && idx < d->idx)
	{
	  d->maybe_nonrect = true;
	  return NULL_TREE;
	}

      if (d->ppset->add (*tp))
	return NULL_TREE;

      location_t loc = d->expr_loc;
      if (loc == UNKNOWN_LOCATION)
	loc = d->stmt_loc;

      switch (d->kind & 3)
	{
	case 0:
	  error_at (loc, "initializer expression refers to "
			 "iteration variable %qD", *tp);
	  break;
	case 1:
	  error_at (loc, "condition expression refers to "
			 "iteration variable %qD", *tp);
	  break;
	case 2:
	  error_at (loc, "increment expression refers to "
			 "iteration variable %qD", *tp);
	  break;
	}
      d->fail = true;
    }
  else if (d->ppset->add (*tp))
    *walk_subtrees = 0;
  /* Don't walk dtors added by C++ wrap_cleanups_r.  */
  else if (TREE_CODE (*tp) == TRY_CATCH_EXPR
	   && TRY_CATCH_IS_CLEANUP (*tp))
    {
      *walk_subtrees = 0;
      return walk_tree_1 (&TREE_OPERAND (*tp, 0), c_omp_check_loop_iv_r, data,
			  NULL, d->lh);
    }

  return NULL_TREE;
}

/* Check the allowed expressions for non-rectangular loop nest lb and b
   expressions.  Return the outer var decl referenced in the expression.  */

static tree
c_omp_check_nonrect_loop_iv (tree *tp, struct c_omp_check_loop_iv_data *d,
			     walk_tree_lh lh)
{
  d->maybe_nonrect = false;
  if (d->fail)
    return NULL_TREE;

  hash_set<tree> pset;
  hash_set<tree> *ppset = d->ppset;
  d->ppset = &pset;

  tree t = *tp;
  if (TREE_CODE (t) == TREE_VEC
      && TREE_VEC_LENGTH (t) == 3
      && DECL_P (TREE_VEC_ELT (t, 0))
      && c_omp_is_loop_iterator (TREE_VEC_ELT (t, 0), d) >= 0)
    {
      d->kind &= 3;
      walk_tree_1 (&TREE_VEC_ELT (t, 1), c_omp_check_loop_iv_r, d, NULL, lh);
      walk_tree_1 (&TREE_VEC_ELT (t, 1), c_omp_check_loop_iv_r, d, NULL, lh);
      d->ppset = ppset;
      return d->fail ? NULL_TREE : TREE_VEC_ELT (t, 0);
    }

  while (CONVERT_EXPR_P (t))
    t = TREE_OPERAND (t, 0);

  tree a1 = t, a2 = integer_zero_node;
  bool neg_a1 = false, neg_a2 = false;
  switch (TREE_CODE (t))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      a1 = TREE_OPERAND (t, 0);
      a2 = TREE_OPERAND (t, 1);
      while (CONVERT_EXPR_P (a1))
	a1 = TREE_OPERAND (a1, 0);
      while (CONVERT_EXPR_P (a2))
	a2 = TREE_OPERAND (a2, 0);
      if (DECL_P (a1) && c_omp_is_loop_iterator (a1, d) >= 0)
	{
	  a2 = TREE_OPERAND (t, 1);
	  if (TREE_CODE (t) == MINUS_EXPR)
	    neg_a2 = true;
	  t = a1;
	  break;
	}
      if (DECL_P (a2) && c_omp_is_loop_iterator (a2, d) >= 0)
	{
	  a1 = TREE_OPERAND (t, 0);
	  if (TREE_CODE (t) == MINUS_EXPR)
	    neg_a1 = true;
	  t = a2;
	  a2 = a1;
	  break;
	}
      if (TREE_CODE (a1) == MULT_EXPR && TREE_CODE (a2) == MULT_EXPR)
	{
	  tree o1 = TREE_OPERAND (a1, 0);
	  tree o2 = TREE_OPERAND (a1, 1);
	  while (CONVERT_EXPR_P (o1))
	    o1 = TREE_OPERAND (o1, 0);
	  while (CONVERT_EXPR_P (o2))
	    o2 = TREE_OPERAND (o2, 0);
	  if ((DECL_P (o1) && c_omp_is_loop_iterator (o1, d) >= 0)
	      || (DECL_P (o2) && c_omp_is_loop_iterator (o2, d) >= 0))
	    {
	      a2 = TREE_OPERAND (t, 1);
	      if (TREE_CODE (t) == MINUS_EXPR)
		neg_a2 = true;
	      t = a1;
	      break;
	    }
	}
      if (TREE_CODE (a2) == MULT_EXPR)
	{
	  a1 = TREE_OPERAND (t, 0);
	  if (TREE_CODE (t) == MINUS_EXPR)
	    neg_a1 = true;
	  t = a2;
	  a2 = a1;
	  break;
	}
      if (TREE_CODE (a1) == MULT_EXPR)
	{
	  a2 = TREE_OPERAND (t, 1);
	  if (TREE_CODE (t) == MINUS_EXPR)
	    neg_a2 = true;
	  t = a1;
	  break;
	}
      a2 = integer_zero_node;
      break;
    default:
      break;
    }

  a1 = integer_one_node;
  if (TREE_CODE (t) == MULT_EXPR)
    {
      tree o1 = TREE_OPERAND (t, 0);
      tree o2 = TREE_OPERAND (t, 1);
      while (CONVERT_EXPR_P (o1))
	o1 = TREE_OPERAND (o1, 0);
      while (CONVERT_EXPR_P (o2))
	o2 = TREE_OPERAND (o2, 0);
      if (DECL_P (o1) && c_omp_is_loop_iterator (o1, d) >= 0)
	{
	  a1 = TREE_OPERAND (t, 1);
	  t = o1;
	}
      else if (DECL_P (o2) && c_omp_is_loop_iterator (o2, d) >= 0)
	{
	  a1 = TREE_OPERAND (t, 0);
	  t = o2;
	}
    }

  d->kind &= 3;
  tree ret = NULL_TREE;
  if (DECL_P (t) && c_omp_is_loop_iterator (t, d) >= 0)
    {
      location_t loc = d->expr_loc;
      if (loc == UNKNOWN_LOCATION)
	loc = d->stmt_loc;
      if (!lang_hooks.types_compatible_p (TREE_TYPE (*tp), TREE_TYPE (t)))
	{
	  if (d->kind == 0)
	    error_at (loc, "outer iteration variable %qD used in initializer"
			   " expression has type other than %qT",
		      t, TREE_TYPE (*tp));
	  else
	    error_at (loc, "outer iteration variable %qD used in condition"
			   " expression has type other than %qT",
		      t, TREE_TYPE (*tp));
	  d->fail = true;
	}
      else if (!INTEGRAL_TYPE_P (TREE_TYPE (a1)))
	{
	  error_at (loc, "outer iteration variable %qD multiplier expression"
			 " %qE is not integral", t, a1);
	  d->fail = true;
	}
      else if (!INTEGRAL_TYPE_P (TREE_TYPE (a2)))
	{
	  error_at (loc, "outer iteration variable %qD addend expression"
			 " %qE is not integral", t, a2);
	  d->fail = true;
	}
      else
	{
	  walk_tree_1 (&a1, c_omp_check_loop_iv_r, d, NULL, lh);
	  walk_tree_1 (&a2, c_omp_check_loop_iv_r, d, NULL, lh);
        }
      if (!d->fail)
	{
	  a1 = fold_convert (TREE_TYPE (*tp), a1);
	  a2 = fold_convert (TREE_TYPE (*tp), a2);
	  if (neg_a1)
	    a1 = fold_build1 (NEGATE_EXPR, TREE_TYPE (a1), a1);
	  if (neg_a2)
	    a2 = fold_build1 (NEGATE_EXPR, TREE_TYPE (a2), a2);
	  ret = t;
	  *tp = make_tree_vec (3);
	  TREE_VEC_ELT (*tp, 0) = t;
	  TREE_VEC_ELT (*tp, 1) = a1;
	  TREE_VEC_ELT (*tp, 2) = a2;
	}
    }
  else
    walk_tree_1 (&t, c_omp_check_loop_iv_r, d, NULL, lh);

  d->ppset = ppset;
  return ret;
}

/* Diagnose invalid references to loop iterators in lb, b and incr
   expressions.  */

bool
c_omp_check_loop_iv (tree stmt, tree declv, walk_tree_lh lh)
{
  hash_set<tree> pset;
  struct c_omp_check_loop_iv_data data;
  int i;

  data.declv = declv;
  data.fail = false;
  data.maybe_nonrect = false;
  data.stmt_loc = EXPR_LOCATION (stmt);
  data.lh = lh;
  data.ppset = &pset;
  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (stmt)); i++)
    {
      tree init = TREE_VEC_ELT (OMP_FOR_INIT (stmt), i);
      gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
      tree decl = TREE_OPERAND (init, 0);
      tree cond = TREE_VEC_ELT (OMP_FOR_COND (stmt), i);
      gcc_assert (COMPARISON_CLASS_P (cond));
      gcc_assert (TREE_OPERAND (cond, 0) == decl);
      tree incr = TREE_VEC_ELT (OMP_FOR_INCR (stmt), i);
      data.expr_loc = EXPR_LOCATION (TREE_OPERAND (init, 1));
      tree vec_outer1 = NULL_TREE, vec_outer2 = NULL_TREE;
      int kind = 0;
      if (i > 0
	  && (unsigned) c_omp_is_loop_iterator (decl, &data) < (unsigned) i)
	{
	  location_t loc = data.expr_loc;
	  if (loc == UNKNOWN_LOCATION)
	    loc = data.stmt_loc;
	  error_at (loc, "the same loop iteration variables %qD used in "
			 "multiple associated loops", decl);
	  data.fail = true;
	}
      /* Handle non-rectangular loop nests.  */
      if (TREE_CODE (stmt) != OACC_LOOP
	  && (TREE_CODE (TREE_OPERAND (init, 1)) == TREE_VEC
	      || INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (init, 1))))
	  && i > 0)
	kind = 4;
      data.kind = kind;
      data.idx = i;
      walk_tree_1 (&TREE_OPERAND (init, 1),
		   c_omp_check_loop_iv_r, &data, NULL, lh);
      if (data.maybe_nonrect)
	vec_outer1 = c_omp_check_nonrect_loop_iv (&TREE_OPERAND (init, 1),
						  &data, lh);
      /* Don't warn for C++ random access iterators here, the
	 expression then involves the subtraction and always refers
	 to the original value.  The C++ FE needs to warn on those
	 earlier.  */
      if (decl == TREE_VEC_ELT (declv, i)
	  || (TREE_CODE (TREE_VEC_ELT (declv, i)) == TREE_LIST
	      && decl == TREE_PURPOSE (TREE_VEC_ELT (declv, i))))
	{
	  data.expr_loc = EXPR_LOCATION (cond);
	  data.kind = kind | 1;
	  walk_tree_1 (&TREE_OPERAND (cond, 1),
		       c_omp_check_loop_iv_r, &data, NULL, lh);
	  if (data.maybe_nonrect)
	    vec_outer2 = c_omp_check_nonrect_loop_iv (&TREE_OPERAND (cond, 1),
						      &data, lh);
	}
      if (vec_outer1 && vec_outer2 && vec_outer1 != vec_outer2)
	{
	  location_t loc = data.expr_loc;
	  if (loc == UNKNOWN_LOCATION)
	    loc = data.stmt_loc;
	  error_at (loc, "two different outer iteration variables %qD and %qD"
			 " used in a single loop", vec_outer1, vec_outer2);
	  data.fail = true;
	}
      if (vec_outer1 || vec_outer2)
	OMP_FOR_NON_RECTANGULAR (stmt) = 1;
      if (TREE_CODE (incr) == MODIFY_EXPR)
	{
	  gcc_assert (TREE_OPERAND (incr, 0) == decl);
	  incr = TREE_OPERAND (incr, 1);
	  data.kind = 2;
	  if (TREE_CODE (incr) == PLUS_EXPR
	      && TREE_OPERAND (incr, 1) == decl)
	    {
	      data.expr_loc = EXPR_LOCATION (TREE_OPERAND (incr, 0));
	      walk_tree_1 (&TREE_OPERAND (incr, 0),
			   c_omp_check_loop_iv_r, &data, NULL, lh);
	    }
	  else
	    {
	      data.expr_loc = EXPR_LOCATION (TREE_OPERAND (incr, 1));
	      walk_tree_1 (&TREE_OPERAND (incr, 1),
			   c_omp_check_loop_iv_r, &data, NULL, lh);
	    }
	}
    }
  return !data.fail;
}

/* Similar, but allows to check the init or cond expressions individually.  */

bool
c_omp_check_loop_iv_exprs (location_t stmt_loc, tree declv, int i, tree decl,
			   tree init, tree cond, walk_tree_lh lh)
{
  hash_set<tree> pset;
  struct c_omp_check_loop_iv_data data;

  data.declv = declv;
  data.fail = false;
  data.maybe_nonrect = false;
  data.stmt_loc = stmt_loc;
  data.lh = lh;
  data.ppset = &pset;
  data.idx = i;
  if (i > 0
      && (unsigned) c_omp_is_loop_iterator (decl, &data) < (unsigned) i)
    {
      error_at (stmt_loc, "the same loop iteration variables %qD used in "
      			  "multiple associated loops", decl);
      data.fail = true;
    }
  if (init)
    {
      data.expr_loc = EXPR_LOCATION (init);
      data.kind = 0;
      walk_tree_1 (&init,
		   c_omp_check_loop_iv_r, &data, NULL, lh);
    }
  if (cond)
    {
      gcc_assert (COMPARISON_CLASS_P (cond));
      data.expr_loc = EXPR_LOCATION (init);
      data.kind = 1;
      if (TREE_OPERAND (cond, 0) == decl)
	walk_tree_1 (&TREE_OPERAND (cond, 1),
		     c_omp_check_loop_iv_r, &data, NULL, lh);
      else
	walk_tree_1 (&TREE_OPERAND (cond, 0),
		     c_omp_check_loop_iv_r, &data, NULL, lh);
    }
  return !data.fail;
}

/* This function splits clauses for OpenACC combined loop
   constructs.  OpenACC combined loop constructs are:
   #pragma acc kernels loop
   #pragma acc parallel loop  */

tree
c_oacc_split_loop_clauses (tree clauses, tree *not_loop_clauses,
			   bool is_parallel)
{
  tree next, loop_clauses, nc;

  loop_clauses = *not_loop_clauses = NULL_TREE;
  for (; clauses ; clauses = next)
    {
      next = OMP_CLAUSE_CHAIN (clauses);

      switch (OMP_CLAUSE_CODE (clauses))
        {
	  /* Loop clauses.  */
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_PRIVATE:
	  OMP_CLAUSE_CHAIN (clauses) = loop_clauses;
	  loop_clauses = clauses;
	  break;

	  /* Reductions must be duplicated on both constructs.  */
	case OMP_CLAUSE_REDUCTION:
	  if (is_parallel)
	    {
	      nc = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				     OMP_CLAUSE_REDUCTION);
	      OMP_CLAUSE_DECL (nc) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_REDUCTION_CODE (nc)
		= OMP_CLAUSE_REDUCTION_CODE (clauses);
	      OMP_CLAUSE_CHAIN (nc) = *not_loop_clauses;
	      *not_loop_clauses = nc;
	    }

	  OMP_CLAUSE_CHAIN (clauses) = loop_clauses;
	  loop_clauses = clauses;
	  break;

	  /* Parallel/kernels clauses.  */
	default:
	  OMP_CLAUSE_CHAIN (clauses) = *not_loop_clauses;
	  *not_loop_clauses = clauses;
	  break;
	}
    }

  return loop_clauses;
}

/* This function attempts to split or duplicate clauses for OpenMP
   combined/composite constructs.  Right now there are 30 different
   constructs.  CODE is the innermost construct in the combined construct,
   and MASK allows to determine which constructs are combined together,
   as every construct has at least one clause that no other construct
   has (except for OMP_SECTIONS, but that can be only combined with parallel,
   and OMP_MASTER, which doesn't have any clauses at all).
   OpenMP combined/composite constructs are:
   #pragma omp distribute parallel for
   #pragma omp distribute parallel for simd
   #pragma omp distribute simd
   #pragma omp for simd
   #pragma omp master taskloop
   #pragma omp master taskloop simd
   #pragma omp parallel for
   #pragma omp parallel for simd
   #pragma omp parallel loop
   #pragma omp parallel master
   #pragma omp parallel master taskloop
   #pragma omp parallel master taskloop simd
   #pragma omp parallel sections
   #pragma omp target parallel
   #pragma omp target parallel for
   #pragma omp target parallel for simd
   #pragma omp target parallel loop
   #pragma omp target teams
   #pragma omp target teams distribute
   #pragma omp target teams distribute parallel for
   #pragma omp target teams distribute parallel for simd
   #pragma omp target teams distribute simd
   #pragma omp target teams loop
   #pragma omp target simd
   #pragma omp taskloop simd
   #pragma omp teams distribute
   #pragma omp teams distribute parallel for
   #pragma omp teams distribute parallel for simd
   #pragma omp teams distribute simd
   #pragma omp teams loop  */

void
c_omp_split_clauses (location_t loc, enum tree_code code,
		     omp_clause_mask mask, tree clauses, tree *cclauses)
{
  tree next, c;
  enum c_omp_clause_split s;
  int i;

  for (i = 0; i < C_OMP_CLAUSE_SPLIT_COUNT; i++)
    cclauses[i] = NULL;
  /* Add implicit nowait clause on
     #pragma omp parallel {for,for simd,sections}.  */
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
    switch (code)
      {
      case OMP_FOR:
      case OMP_SIMD:
	if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	  cclauses[C_OMP_CLAUSE_SPLIT_FOR]
	    = build_omp_clause (loc, OMP_CLAUSE_NOWAIT);
	break;
      case OMP_SECTIONS:
	cclauses[C_OMP_CLAUSE_SPLIT_SECTIONS]
	  = build_omp_clause (loc, OMP_CLAUSE_NOWAIT);
	break;
      default:
	break;
      }

  for (; clauses ; clauses = next)
    {
      next = OMP_CLAUSE_CHAIN (clauses);

      switch (OMP_CLAUSE_CODE (clauses))
	{
	/* First the clauses that are unique to some constructs.  */
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_DEPEND:
	  s = C_OMP_CLAUSE_SPLIT_TARGET;
	  break;
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	  s = C_OMP_CLAUSE_SPLIT_TEAMS;
	  break;
	case OMP_CLAUSE_DIST_SCHEDULE:
	  s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
	  break;
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_PROC_BIND:
	  s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	  break;
	case OMP_CLAUSE_ORDERED:
	  s = C_OMP_CLAUSE_SPLIT_FOR;
	  break;
	case OMP_CLAUSE_SCHEDULE:
	  s = C_OMP_CLAUSE_SPLIT_FOR;
	  if (code != OMP_SIMD)
	    OMP_CLAUSE_SCHEDULE_SIMD (clauses) = 0;
	  break;
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_ALIGNED:
	case OMP_CLAUSE_NONTEMPORAL:
	  s = C_OMP_CLAUSE_SPLIT_SIMD;
	  break;
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_PRIORITY:
	  s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	  break;
	case OMP_CLAUSE_BIND:
	  s = C_OMP_CLAUSE_SPLIT_LOOP;
	  break;
	/* Duplicate this to all of taskloop, distribute, for, simd and
	   loop.  */
	case OMP_CLAUSE_COLLAPSE:
	  if (code == OMP_SIMD)
	    {
	      if ((mask & ((OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)
			   | (OMP_CLAUSE_MASK_1
			      << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)
			   | (OMP_CLAUSE_MASK_1
			      << PRAGMA_OMP_CLAUSE_NOGROUP))) != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_COLLAPSE);
		  OMP_CLAUSE_COLLAPSE_EXPR (c)
		    = OMP_CLAUSE_COLLAPSE_EXPR (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
		  cclauses[C_OMP_CLAUSE_SPLIT_SIMD] = c;
		}
	      else
		{
		  /* This must be #pragma omp target simd */
		  s = C_OMP_CLAUSE_SPLIT_SIMD;
		  break;
		}
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1
			   << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_COLLAPSE);
		  OMP_CLAUSE_COLLAPSE_EXPR (c)
		    = OMP_CLAUSE_COLLAPSE_EXPR (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
		  cclauses[C_OMP_CLAUSE_SPLIT_FOR] = c;
		  s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
		}
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP))
		   != 0)
	    s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	  else if (code == OMP_LOOP)
	    s = C_OMP_CLAUSE_SPLIT_LOOP;
	  else
	    s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
	  break;
	/* Private clause is supported on all constructs but master,
	   it is enough to put it on the innermost one other than master.  For
	   #pragma omp {for,sections} put it on parallel though,
	   as that's what we did for OpenMP 3.1.  */
	case OMP_CLAUSE_PRIVATE:
	  switch (code)
	    {
	    case OMP_SIMD: s = C_OMP_CLAUSE_SPLIT_SIMD; break;
	    case OMP_FOR: case OMP_SECTIONS:
	    case OMP_PARALLEL: s = C_OMP_CLAUSE_SPLIT_PARALLEL; break;
	    case OMP_DISTRIBUTE: s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE; break;
	    case OMP_TEAMS: s = C_OMP_CLAUSE_SPLIT_TEAMS; break;
	    case OMP_MASTER: s = C_OMP_CLAUSE_SPLIT_PARALLEL; break;
	    case OMP_TASKLOOP: s = C_OMP_CLAUSE_SPLIT_TASKLOOP; break;
	    case OMP_LOOP: s = C_OMP_CLAUSE_SPLIT_LOOP; break;
	    default: gcc_unreachable ();
	    }
	  break;
	/* Firstprivate clause is supported on all constructs but
	   simd, master and loop.  Put it on the outermost of those and
	   duplicate on teams and parallel.  */
	case OMP_CLAUSE_FIRSTPRIVATE:
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP))
	      != 0)
	    {
	      if (code == OMP_SIMD
		  && (mask & ((OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_NUM_THREADS)
			      | (OMP_CLAUSE_MASK_1
				 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))) == 0)
		{
		  /* This must be #pragma omp target simd.  */
		  s = C_OMP_CLAUSE_SPLIT_TARGET;
		  break;
		}
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_FIRSTPRIVATE);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
	      cclauses[C_OMP_CLAUSE_SPLIT_TARGET] = c;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
	      != 0)
	    {
	      if ((mask & ((OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS)
			   | (OMP_CLAUSE_MASK_1
			      << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE))) != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_FIRSTPRIVATE);
		  OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL];
		  cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL] = c;
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_NUM_TEAMS)) != 0)
		    s = C_OMP_CLAUSE_SPLIT_TEAMS;
		  else
		    s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
		}
	      else if ((mask & (OMP_CLAUSE_MASK_1
				<< PRAGMA_OMP_CLAUSE_NOGROUP)) != 0)
		/* This must be
		   #pragma omp parallel master taskloop{, simd}.  */
		s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	      else
		/* This must be
		   #pragma omp parallel{, for{, simd}, sections,loop}
		   or
		   #pragma omp target parallel.  */
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))
		   != 0)
	    {
	      /* This must be one of
		 #pragma omp {,target }teams {distribute,loop}
		 #pragma omp target teams
		 #pragma omp {,target }teams distribute simd.  */
	      gcc_assert (code == OMP_DISTRIBUTE
			  || code == OMP_LOOP
			  || code == OMP_TEAMS
			  || code == OMP_SIMD);
	      s = C_OMP_CLAUSE_SPLIT_TEAMS;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1
			    << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
	    {
	      /* This must be #pragma omp distribute simd.  */
	      gcc_assert (code == OMP_SIMD);
	      s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1
			    << PRAGMA_OMP_CLAUSE_NOGROUP)) != 0)
	    {
	      /* This must be #pragma omp {,{,parallel }master }taskloop simd
		 or
		 #pragma omp {,parallel }master taskloop.  */
	      gcc_assert (code == OMP_SIMD || code == OMP_TASKLOOP);
	      s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	    }
	  else
	    {
	      /* This must be #pragma omp for simd.  */
	      gcc_assert (code == OMP_SIMD);
	      s = C_OMP_CLAUSE_SPLIT_FOR;
	    }
	  break;
	/* Lastprivate is allowed on distribute, for, sections, taskloop, loop
	   and simd.  In parallel {for{, simd},sections} we actually want to
	   put it on parallel rather than for or sections.  */
	case OMP_CLAUSE_LASTPRIVATE:
	  if (code == OMP_DISTRIBUTE)
	    {
	      s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
	      break;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1
		       << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_LASTPRIVATE);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE];
	      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c)
		= OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (clauses);
	      cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE] = c;
	    }
	  if (code == OMP_FOR || code == OMP_SECTIONS)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
		  != 0)
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	      break;
	    }
	  if (code == OMP_TASKLOOP)
	    {
	      s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	      break;
	    }
	  if (code == OMP_LOOP)
	    {
	      s = C_OMP_CLAUSE_SPLIT_LOOP;
	      break;
	    }
	  gcc_assert (code == OMP_SIMD);
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_LASTPRIVATE);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c)
		= OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (clauses);
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
		  != 0)
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	      OMP_CLAUSE_CHAIN (c) = cclauses[s];
	      cclauses[s] = c;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP)) != 0)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_LASTPRIVATE);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c)
		= OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP];
	      cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP] = c;
	    }
	  s = C_OMP_CLAUSE_SPLIT_SIMD;
	  break;
	/* Shared and default clauses are allowed on parallel, teams and
	   taskloop.  */
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_DEFAULT:
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP))
	      != 0)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
		  != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_CODE (clauses));
		  if (OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_SHARED)
		    OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
		  else
		    OMP_CLAUSE_DEFAULT_KIND (c)
		      = OMP_CLAUSE_DEFAULT_KIND (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL];
		  cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL] = c;
		}
	      s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	      break;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))
	      != 0)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
		  == 0)
		{
		  s = C_OMP_CLAUSE_SPLIT_TEAMS;
		  break;
		}
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_CODE (clauses));
	      if (OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_SHARED)
		OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      else
		OMP_CLAUSE_DEFAULT_KIND (c)
		  = OMP_CLAUSE_DEFAULT_KIND (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
	      cclauses[C_OMP_CLAUSE_SPLIT_TEAMS] = c;
	    }
	  s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	  break;
	/* order clauses are allowed on for, simd and loop.  */
	case OMP_CLAUSE_ORDER:
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
	      if (code == OMP_SIMD)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_ORDER);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
		  cclauses[C_OMP_CLAUSE_SPLIT_FOR] = c;
		  s = C_OMP_CLAUSE_SPLIT_SIMD;
		}
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	    }
	  else if (code == OMP_LOOP)
	    s = C_OMP_CLAUSE_SPLIT_LOOP;
	  else
	    s = C_OMP_CLAUSE_SPLIT_SIMD;
	  break;
	/* Reduction is allowed on simd, for, parallel, sections, taskloop,
	   teams and loop.  Duplicate it on all of them, but omit on for or
	   sections if parallel is present (unless inscan, in that case
	   omit on parallel).  If taskloop or loop is combined with
	   parallel, omit it on parallel.  */
	case OMP_CLAUSE_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_TASK (clauses))
	    {
	      if (code == OMP_SIMD || code == OMP_LOOP)
		{
		  error_at (OMP_CLAUSE_LOCATION (clauses),
			    "invalid %<task%> reduction modifier on construct "
			    "combined with %<simd%> or %<loop%>");
		  OMP_CLAUSE_REDUCTION_TASK (clauses) = 0;
		}
	      else if (code != OMP_SECTIONS
		       && (mask & (OMP_CLAUSE_MASK_1
				   << PRAGMA_OMP_CLAUSE_NUM_THREADS)) == 0
		       && (mask & (OMP_CLAUSE_MASK_1
				   << PRAGMA_OMP_CLAUSE_SCHEDULE)) == 0)
		{
		  error_at (OMP_CLAUSE_LOCATION (clauses),
			    "invalid %<task%> reduction modifier on construct "
			    "not combined with %<parallel%>, %<for%> or "
			    "%<sections%>");
		  OMP_CLAUSE_REDUCTION_TASK (clauses) = 0;
		}
	    }
	  if (OMP_CLAUSE_REDUCTION_INSCAN (clauses)
	      && ((mask & ((OMP_CLAUSE_MASK_1
			    << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)
			   | (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)))
		  != 0))
	    {
	      error_at (OMP_CLAUSE_LOCATION (clauses),
			"%<inscan%> %<reduction%> clause on construct other "
			"than %<for%>, %<simd%>, %<for simd%>, "
			"%<parallel for%>, %<parallel for simd%>");
	      OMP_CLAUSE_REDUCTION_INSCAN (clauses) = 0;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
	      if (code == OMP_SIMD)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_REDUCTION);
		  OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
		  OMP_CLAUSE_REDUCTION_CODE (c)
		    = OMP_CLAUSE_REDUCTION_CODE (clauses);
		  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		    = OMP_CLAUSE_REDUCTION_PLACEHOLDER (clauses);
		  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c)
		    = OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clauses);
		  OMP_CLAUSE_REDUCTION_INSCAN (c)
		    = OMP_CLAUSE_REDUCTION_INSCAN (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
		  cclauses[C_OMP_CLAUSE_SPLIT_SIMD] = c;
		}
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))
		  != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_REDUCTION);
		  OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
		  OMP_CLAUSE_REDUCTION_CODE (c)
		    = OMP_CLAUSE_REDUCTION_CODE (clauses);
		  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		    = OMP_CLAUSE_REDUCTION_PLACEHOLDER (clauses);
		  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c)
		    = OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clauses);
		  OMP_CLAUSE_REDUCTION_INSCAN (c)
		    = OMP_CLAUSE_REDUCTION_INSCAN (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
		  cclauses[C_OMP_CLAUSE_SPLIT_TEAMS] = c;
		  s = C_OMP_CLAUSE_SPLIT_PARALLEL;
		}
	      else if ((mask & (OMP_CLAUSE_MASK_1
				<< PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0
		       && !OMP_CLAUSE_REDUCTION_INSCAN (clauses))
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	    }
	  else if (code == OMP_SECTIONS
		   || code == OMP_PARALLEL
		   || code == OMP_MASTER)
	    s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	  else if (code == OMP_TASKLOOP)
	    s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	  else if (code == OMP_LOOP)
	    s = C_OMP_CLAUSE_SPLIT_LOOP;
	  else if (code == OMP_SIMD)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP))
		  != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_REDUCTION);
		  OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
		  OMP_CLAUSE_REDUCTION_CODE (c)
		    = OMP_CLAUSE_REDUCTION_CODE (clauses);
		  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		    = OMP_CLAUSE_REDUCTION_PLACEHOLDER (clauses);
		  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c)
		    = OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clauses);
		  OMP_CLAUSE_REDUCTION_INSCAN (c)
		    = OMP_CLAUSE_REDUCTION_INSCAN (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP];
		  cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP] = c;
		}
	      s = C_OMP_CLAUSE_SPLIT_SIMD;
	    }
	  else
	    s = C_OMP_CLAUSE_SPLIT_TEAMS;
	  break;
	case OMP_CLAUSE_IN_REDUCTION:
	  /* in_reduction on taskloop simd becomes reduction on the simd
	     and keeps being in_reduction on taskloop.  */
	  if (code == OMP_SIMD)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_REDUCTION);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_REDUCTION_CODE (c)
		= OMP_CLAUSE_REDUCTION_CODE (clauses);
	      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		= OMP_CLAUSE_REDUCTION_PLACEHOLDER (clauses);
	      OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c)
		= OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
	      cclauses[C_OMP_CLAUSE_SPLIT_SIMD] = c;
	    }
	  s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	  break;
	case OMP_CLAUSE_IF:
	  if (OMP_CLAUSE_IF_MODIFIER (clauses) != ERROR_MARK)
	    {
	      s = C_OMP_CLAUSE_SPLIT_COUNT;
	      switch (OMP_CLAUSE_IF_MODIFIER (clauses))
		{
		case OMP_PARALLEL:
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
		    s = C_OMP_CLAUSE_SPLIT_PARALLEL;
		  break;
		case OMP_SIMD:
		  if (code == OMP_SIMD)
		    s = C_OMP_CLAUSE_SPLIT_SIMD;
		  break;
		case OMP_TASKLOOP:
		  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP))
		      != 0)
		    s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
		  break;
		case OMP_TARGET:
		  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP))
		      != 0)
		    s = C_OMP_CLAUSE_SPLIT_TARGET;
		  break;
		default:
		  break;
		}
	      if (s != C_OMP_CLAUSE_SPLIT_COUNT)
		break;
	      /* Error-recovery here, invalid if-modifier specified, add the
		 clause to just one construct.  */
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) != 0)
		s = C_OMP_CLAUSE_SPLIT_TARGET;
	      else if ((mask & (OMP_CLAUSE_MASK_1
				<< PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else if ((mask & (OMP_CLAUSE_MASK_1
				<< PRAGMA_OMP_CLAUSE_NOGROUP)) != 0)
		s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	      else if (code == OMP_SIMD)
		s = C_OMP_CLAUSE_SPLIT_SIMD;
	      else
		gcc_unreachable ();
	      break;
	    }
	  /* Otherwise, duplicate if clause to all constructs.  */
	  if (code == OMP_SIMD)
	    {
	      if ((mask & ((OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)
			   | (OMP_CLAUSE_MASK_1
			      << PRAGMA_OMP_CLAUSE_NUM_THREADS)
			   | (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP)))
		  != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_IF);
		  OMP_CLAUSE_IF_MODIFIER (c)
		    = OMP_CLAUSE_IF_MODIFIER (clauses);
		  OMP_CLAUSE_IF_EXPR (c) = OMP_CLAUSE_IF_EXPR (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
		  cclauses[C_OMP_CLAUSE_SPLIT_SIMD] = c;
		}
	      else
		{
		  s = C_OMP_CLAUSE_SPLIT_SIMD;
		  break;
		}
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP))
	      != 0)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1
			   << PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_IF);
		  OMP_CLAUSE_IF_MODIFIER (c)
		    = OMP_CLAUSE_IF_MODIFIER (clauses);
		  OMP_CLAUSE_IF_EXPR (c) = OMP_CLAUSE_IF_EXPR (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP];
		  cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP] = c;
		  s = C_OMP_CLAUSE_SPLIT_PARALLEL;
		}
	      else
		s = C_OMP_CLAUSE_SPLIT_TASKLOOP;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1
			    << PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1
			   << PRAGMA_OMP_CLAUSE_MAP)) != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_IF);
		  OMP_CLAUSE_IF_MODIFIER (c)
		    = OMP_CLAUSE_IF_MODIFIER (clauses);
		  OMP_CLAUSE_IF_EXPR (c) = OMP_CLAUSE_IF_EXPR (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
		  cclauses[C_OMP_CLAUSE_SPLIT_TARGET] = c;
		  s = C_OMP_CLAUSE_SPLIT_PARALLEL;
		}
	      else
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	    }
	  else
	    s = C_OMP_CLAUSE_SPLIT_TARGET;
	  break;
	case OMP_CLAUSE_LINEAR:
	  /* Linear clause is allowed on simd and for.  Put it on the
	     innermost construct.  */
	  if (code == OMP_SIMD)
	    s = C_OMP_CLAUSE_SPLIT_SIMD;
	  else
	    s = C_OMP_CLAUSE_SPLIT_FOR;
	  break;
	case OMP_CLAUSE_NOWAIT:
	  /* Nowait clause is allowed on target, for and sections, but
	     is not allowed on parallel for or parallel sections.  Therefore,
	     put it on target construct if present, because that can only
	     be combined with parallel for{, simd} and not with for{, simd},
	     otherwise to the worksharing construct.  */
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP))
	      != 0)
	    s = C_OMP_CLAUSE_SPLIT_TARGET;
	  else
	    s = C_OMP_CLAUSE_SPLIT_FOR;
	  break;
	default:
	  gcc_unreachable ();
	}
      OMP_CLAUSE_CHAIN (clauses) = cclauses[s];
      cclauses[s] = clauses;
    }

  if (!flag_checking)
    return;

  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) == 0)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_TARGET] == NULL_TREE);
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS)) == 0)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_TEAMS] == NULL_TREE);
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) == 0)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE] == NULL_TREE);
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS)) == 0)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL] == NULL_TREE);
  if ((mask & ((OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)
	       | (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP))) == 0
      && code != OMP_SECTIONS
      && code != OMP_LOOP)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_FOR] == NULL_TREE);
  if (code != OMP_SIMD)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_SIMD] == NULL_TREE);
}


/* qsort callback to compare #pragma omp declare simd clauses.  */

static int
c_omp_declare_simd_clause_cmp (const void *p, const void *q)
{
  tree a = *(const tree *) p;
  tree b = *(const tree *) q;
  if (OMP_CLAUSE_CODE (a) != OMP_CLAUSE_CODE (b))
    {
      if (OMP_CLAUSE_CODE (a) > OMP_CLAUSE_CODE (b))
	return -1;
      return 1;
    }
  if (OMP_CLAUSE_CODE (a) != OMP_CLAUSE_SIMDLEN
      && OMP_CLAUSE_CODE (a) != OMP_CLAUSE_INBRANCH
      && OMP_CLAUSE_CODE (a) != OMP_CLAUSE_NOTINBRANCH)
    {
      int c = tree_to_shwi (OMP_CLAUSE_DECL (a));
      int d = tree_to_shwi (OMP_CLAUSE_DECL (b));
      if (c < d)
	return 1;
      if (c > d)
	return -1;
    }
  return 0;
}

/* Change PARM_DECLs in OMP_CLAUSE_DECL of #pragma omp declare simd
   CLAUSES on FNDECL into argument indexes and sort them.  */

tree
c_omp_declare_simd_clauses_to_numbers (tree parms, tree clauses)
{
  tree c;
  vec<tree> clvec = vNULL;

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_SIMDLEN
	  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_INBRANCH
	  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_NOTINBRANCH)
	{
	  tree decl = OMP_CLAUSE_DECL (c);
	  tree arg;
	  int idx;
	  for (arg = parms, idx = 0; arg;
	       arg = TREE_CHAIN (arg), idx++)
	    if (arg == decl)
	      break;
	  if (arg == NULL_TREE)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD is not a function argument", decl);
	      continue;
	    }
	  OMP_CLAUSE_DECL (c) = build_int_cst (integer_type_node, idx);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	      && OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (c))
	    {
	      decl = OMP_CLAUSE_LINEAR_STEP (c);
	      for (arg = parms, idx = 0; arg;
		   arg = TREE_CHAIN (arg), idx++)
		if (arg == decl)
		  break;
	      if (arg == NULL_TREE)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD is not a function argument", decl);
		  continue;
		}
	      OMP_CLAUSE_LINEAR_STEP (c)
		= build_int_cst (integer_type_node, idx);
	    }
	}
      clvec.safe_push (c);
    }
  if (!clvec.is_empty ())
    {
      unsigned int len = clvec.length (), i;
      clvec.qsort (c_omp_declare_simd_clause_cmp);
      clauses = clvec[0];
      for (i = 0; i < len; i++)
	OMP_CLAUSE_CHAIN (clvec[i]) = (i < len - 1) ? clvec[i + 1] : NULL_TREE;
    }
  else
    clauses = NULL_TREE;
  clvec.release ();
  return clauses;
}

/* Change argument indexes in CLAUSES of FNDECL back to PARM_DECLs.  */

void
c_omp_declare_simd_clauses_to_decls (tree fndecl, tree clauses)
{
  tree c;

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_SIMDLEN
	&& OMP_CLAUSE_CODE (c) != OMP_CLAUSE_INBRANCH
	&& OMP_CLAUSE_CODE (c) != OMP_CLAUSE_NOTINBRANCH)
      {
	int idx = tree_to_shwi (OMP_CLAUSE_DECL (c)), i;
	tree arg;
	for (arg = DECL_ARGUMENTS (fndecl), i = 0; arg;
	     arg = TREE_CHAIN (arg), i++)
	  if (i == idx)
	    break;
	gcc_assert (arg);
	OMP_CLAUSE_DECL (c) = arg;
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	    && OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (c))
	  {
	    idx = tree_to_shwi (OMP_CLAUSE_LINEAR_STEP (c));
	    for (arg = DECL_ARGUMENTS (fndecl), i = 0; arg;
		 arg = TREE_CHAIN (arg), i++)
	      if (i == idx)
		break;
	    gcc_assert (arg);
	    OMP_CLAUSE_LINEAR_STEP (c) = arg;
	  }
      }
}

/* Return true for __func__ and similar function-local predefined
   variables (which are in OpenMP predetermined shared, allowed in
   shared/firstprivate clauses).  */

bool
c_omp_predefined_variable (tree decl)
{
  if (VAR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && TREE_READONLY (decl)
      && TREE_STATIC (decl)
      && DECL_NAME (decl)
      && (DECL_NAME (decl) == ridpointers[RID_C99_FUNCTION_NAME]
	  || DECL_NAME (decl) == ridpointers[RID_FUNCTION_NAME]
	  || DECL_NAME (decl) == ridpointers[RID_PRETTY_FUNCTION_NAME]))
    return true;
  return false;
}

/* OMP_CLAUSE_DEFAULT_UNSPECIFIED unless OpenMP sharing attribute of DECL
   is predetermined.  */

enum omp_clause_default_kind
c_omp_predetermined_sharing (tree decl)
{
  /* Predetermine artificial variables holding integral values, those
     are usually result of gimplify_one_sizepos or SAVE_EXPR
     gimplification.  */
  if (VAR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && INTEGRAL_TYPE_P (TREE_TYPE (decl)))
    return OMP_CLAUSE_DEFAULT_SHARED;

  if (c_omp_predefined_variable (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}

/* OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED unless OpenMP mapping attribute
   of DECL is predetermined.  */

enum omp_clause_defaultmap_kind
c_omp_predetermined_mapping (tree decl)
{
  /* Predetermine artificial variables holding integral values, those
     are usually result of gimplify_one_sizepos or SAVE_EXPR
     gimplification.  */
  if (VAR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && INTEGRAL_TYPE_P (TREE_TYPE (decl)))
    return OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE;

  if (c_omp_predefined_variable (decl))
    return OMP_CLAUSE_DEFAULTMAP_TO;

  return OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED;
}


/* Diagnose errors in an OpenMP context selector, return CTX if
   it is correct or error_mark_node otherwise.  */

tree
c_omp_check_context_selector (location_t loc, tree ctx)
{
  /* Each trait-set-selector-name can only be specified once.
     There are just 4 set names.  */
  for (tree t1 = ctx; t1; t1 = TREE_CHAIN (t1))
    for (tree t2 = TREE_CHAIN (t1); t2; t2 = TREE_CHAIN (t2))
      if (TREE_PURPOSE (t1) == TREE_PURPOSE (t2))
	{
	  error_at (loc, "selector set %qs specified more than once",
	  	    IDENTIFIER_POINTER (TREE_PURPOSE (t1)));
	  return error_mark_node;
	}
  for (tree t = ctx; t; t = TREE_CHAIN (t))
    {
      /* Each trait-selector-name can only be specified once.  */
      if (list_length (TREE_VALUE (t)) < 5)
	{
	  for (tree t1 = TREE_VALUE (t); t1; t1 = TREE_CHAIN (t1))
	    for (tree t2 = TREE_CHAIN (t1); t2; t2 = TREE_CHAIN (t2))
	      if (TREE_PURPOSE (t1) == TREE_PURPOSE (t2))
		{
		  error_at (loc,
			    "selector %qs specified more than once in set %qs",
			    IDENTIFIER_POINTER (TREE_PURPOSE (t1)),
			    IDENTIFIER_POINTER (TREE_PURPOSE (t)));
		  return error_mark_node;
		}
	}
      else
	{
	  hash_set<tree> pset;
	  for (tree t1 = TREE_VALUE (t); t1; t1 = TREE_CHAIN (t1))
	    if (pset.add (TREE_PURPOSE (t1)))
	      {
		error_at (loc,
			  "selector %qs specified more than once in set %qs",
			  IDENTIFIER_POINTER (TREE_PURPOSE (t1)),
			  IDENTIFIER_POINTER (TREE_PURPOSE (t)));
		return error_mark_node;
	      }
	}

      static const char *const kind[] = {
	"host", "nohost", "cpu", "gpu", "fpga", "any", NULL };
      static const char *const vendor[] = {
	"amd", "arm", "bsc", "cray", "fujitsu", "gnu", "ibm", "intel",
	"llvm", "nvidia", "pgi", "ti", "unknown", NULL };
      static const char *const extension[] = { NULL };
      static const char *const atomic_default_mem_order[] = {
	"seq_cst", "relaxed", "acq_rel", NULL };
      struct known_properties { const char *set; const char *selector;
				const char *const *props; };
      known_properties props[] = {
	{ "device", "kind", kind },
	{ "implementation", "vendor", vendor },
	{ "implementation", "extension", extension },
	{ "implementation", "atomic_default_mem_order",
	  atomic_default_mem_order } };
      for (tree t1 = TREE_VALUE (t); t1; t1 = TREE_CHAIN (t1))
	for (unsigned i = 0; i < ARRAY_SIZE (props); i++)
	  if (!strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t1)),
					   props[i].selector)
	      && !strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t)),
					      props[i].set))
	    for (tree t2 = TREE_VALUE (t1); t2; t2 = TREE_CHAIN (t2))
	      for (unsigned j = 0; ; j++)
		{
		  if (props[i].props[j] == NULL)
		    {
		      if (TREE_PURPOSE (t2)
			  && !strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t2)),
				      " score"))
			break;
		      if (props[i].props == atomic_default_mem_order)
			{
			  error_at (loc,
				    "incorrect property %qs of %qs selector",
				    IDENTIFIER_POINTER (TREE_PURPOSE (t2)),
				    "atomic_default_mem_order");
			  return error_mark_node;
			}
		      else if (TREE_PURPOSE (t2))
			warning_at (loc, 0,
				    "unknown property %qs of %qs selector",
				    IDENTIFIER_POINTER (TREE_PURPOSE (t2)),
				    props[i].selector);
		      else
			warning_at (loc, 0,
				    "unknown property %qE of %qs selector",
				    TREE_VALUE (t2), props[i].selector);
		      break;
		    }
		  else if (TREE_PURPOSE (t2) == NULL_TREE)
		    {
		      const char *str = TREE_STRING_POINTER (TREE_VALUE (t2));
		      if (!strcmp (str, props[i].props[j])
			  && ((size_t) TREE_STRING_LENGTH (TREE_VALUE (t2))
			      == strlen (str) + 1))
			break;
		    }
		  else if (!strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t2)),
				    props[i].props[j]))
		    break;
		}
    }
  return ctx;
}

/* Register VARIANT as variant of some base function marked with
   #pragma omp declare variant.  CONSTRUCT is corresponding construct
   selector set.  */

void
c_omp_mark_declare_variant (location_t loc, tree variant, tree construct)
{
  tree attr = lookup_attribute ("omp declare variant variant",
				DECL_ATTRIBUTES (variant));
  if (attr == NULL_TREE)
    {
      attr = tree_cons (get_identifier ("omp declare variant variant"),
			unshare_expr (construct),
			DECL_ATTRIBUTES (variant));
      DECL_ATTRIBUTES (variant) = attr;
      return;
    }
  if ((TREE_VALUE (attr) != NULL_TREE) != (construct != NULL_TREE)
      || (construct != NULL_TREE
	  && omp_context_selector_set_compare ("construct", TREE_VALUE (attr),
					       construct)))
    error_at (loc, "%qD used as a variant with incompatible %<construct%> "
		   "selector sets", variant);
}

/* For OpenACC, the OMP_CLAUSE_MAP_KIND of an OMP_CLAUSE_MAP is used internally
   to distinguish clauses as seen by the user.  Return the "friendly" clause
   name for error messages etc., where possible.  See also
   c/c-parser.c:c_parser_oacc_data_clause and
   cp/parser.c:cp_parser_oacc_data_clause.  */

const char *
c_omp_map_clause_name (tree clause, bool oacc)
{
  if (oacc && OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP)
    switch (OMP_CLAUSE_MAP_KIND (clause))
    {
    case GOMP_MAP_FORCE_ALLOC:
    case GOMP_MAP_ALLOC: return "create";
    case GOMP_MAP_FORCE_TO:
    case GOMP_MAP_TO: return "copyin";
    case GOMP_MAP_FORCE_FROM:
    case GOMP_MAP_FROM: return "copyout";
    case GOMP_MAP_FORCE_TOFROM:
    case GOMP_MAP_TOFROM: return "copy";
    case GOMP_MAP_RELEASE: return "delete";
    case GOMP_MAP_FORCE_PRESENT: return "present";
    case GOMP_MAP_ATTACH: return "attach";
    case GOMP_MAP_FORCE_DETACH:
    case GOMP_MAP_DETACH: return "detach";
    case GOMP_MAP_DEVICE_RESIDENT: return "device_resident";
    case GOMP_MAP_LINK: return "link";
    case GOMP_MAP_FORCE_DEVICEPTR: return "deviceptr";
    default: break;
    }
  return omp_clause_code_name[OMP_CLAUSE_CODE (clause)];
}
