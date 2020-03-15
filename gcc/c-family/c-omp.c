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
#include "tree-iterator.h"

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

/* State of annotation traversal for FOR loops in kernels regions,
   used to control processing and diagnostic messages that are deferred until
   the entire loop has been scanned.  */
enum annotation_state {
  as_outer,
  as_in_kernels_region,
  as_in_kernels_loop,
  /* The remaining state values represent conversion failures caught
     while in as_in_kernels_loop state.  To test whether the traversal is
     in the body of a kernels loop, use (state >= as_in_kernels_loop).  */
  as_invalid_variable_type,
  as_missing_initializer,
  as_invalid_initializer,
  as_missing_predicate,
  as_invalid_predicate,
  as_missing_increment,
  as_invalid_increment,
  as_explicit_annotation,
  as_invalid_control_flow,
  as_invalid_break,
  as_invalid_return,
  as_invalid_call,
  as_invalid_modification
};

/* Structure used to hold state for automatic annotation of FOR loops
   in kernels regions.  LOOP is the nearest enclosing loop, or
   NULL_TREE if outside of a loop context.  VARS is a tree_list
   containing the variables controlling LOOP's termination (the
   induction variable and a possible limit variable).  STATE keeps
   track of whether loop satisfies all criteria making it legal to
   parallelize.  Otherwise, REASON is a statement that blocks
   automatic parallelization, such as an unstructured jump or an
   assignment to a variable in VARS, used for printing diagnostics.

   These structures are chained through NEXT, which points to the
   next-closest enclosing loop's or the kernels region's annotation info, if
   any.  */

struct annotation_info
{
  tree loop;
  tree vars;
  bool break_ok;
  enum annotation_state state;
  tree reason;
  struct annotation_info *next;
};

/* Mark the current loop's INFO as not OK to annotate, recording STATE
   and REASON for producing diagnostics later.  */

static void
do_not_annotate_loop (struct annotation_info *info,
		      enum annotation_state state, tree reason)
{
  if (info->state == as_in_kernels_loop)
    {
      info->state = state;
      info->reason = reason;
    }
}

/* Mark the current loop identified by INFO and all of its ancestors (i.e.,
   enclosing loops) as not OK to annotate.  Arguments are the same as
   for do_not_annotate_loop.  */

static void
do_not_annotate_loop_nest (struct annotation_info *info,
			   enum annotation_state state, tree reason)
{
  while (info != NULL)
    {
      do_not_annotate_loop (info, state, reason);
      info = info->next;
    }
}

/* If INFO is non-null, call do_not_annotate_loop with STATE and REASON
   to record info for diagnosing an error later.  Otherwise emit an error now
   at ELOCUS with message MSG and the optional arguments.  */

static void annotation_error (struct annotation_info *,
			      enum annotation_state, tree, location_t,
			      const char *, ...) ATTRIBUTE_GCC_DIAG(5,6);
static
void annotation_error (struct annotation_info *info,
			      enum annotation_state state,
			      tree reason,
			      location_t elocus,
			      const char *msg, ...)
{
  if (info)
    do_not_annotate_loop (info, state, reason);
  else
    {
      auto_diagnostic_group d;
      va_list ap;
      va_start (ap, msg);
      emit_diagnostic_valist (DK_ERROR, elocus, -1, msg, &ap);
      va_end (ap);
    }
}

/* Validate and generate OMP_FOR.
   DECLV is a vector of iteration variables, for each collapsed loop.

   ORIG_DECLV, if non-NULL, is a vector with the original iteration
   variables (prior to any transformations, by say, C++ iterators).

   INITV, CONDV and INCRV are vectors containing initialization
   expressions, controlling predicates and increment expressions.
   BODY is the body of the loop and PRE_BODY statements that go before
   the loop.  FINAL_P is true if not inside a C++ template.

   INFO is null if called to parse an explicitly-annotated OMP for
   loop, otherwise it holds state information for automatically
   annotating a regular FOR loop in a kernels region.  In the former case,
   malformed loops are hard errors; otherwise we just record the annotation
   failure in INFO.  */

static tree
c_finish_omp_for_internal (location_t locus, enum tree_code code, tree declv,
			   tree orig_declv, tree initv, tree condv, tree incrv,
			   tree body, tree pre_body, bool final_p,
			   struct annotation_info *info)
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
	  annotation_error (info, as_invalid_variable_type, decl, elocus,
			    "invalid type for iteration variable %qE", decl);
	  fail = true;
	}
      else if (TYPE_ATOMIC (TREE_TYPE (decl)))
	{
	  annotation_error (info, as_invalid_variable_type, decl, elocus,
			    "%<_Atomic%> iteration variable %qE", decl);
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
	      annotation_error (info, as_missing_initializer, decl, elocus,
				"%qE is not initialized", decl);
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
	  annotation_error (info, as_missing_predicate, NULL_TREE, elocus,
			    "missing controlling predicate");
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

	  enum tree_code condcode = TREE_CODE (cond);

	  if (condcode == LT_EXPR
	      || condcode == LE_EXPR
	      || condcode == GT_EXPR
	      || condcode == GE_EXPR
	      || condcode == NE_EXPR
	      || condcode == EQ_EXPR)
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
		  op0 = TREE_OPERAND (op0, 0);
		  op1 = fold_build1_loc (elocus, NOP_EXPR, TREE_TYPE (decl),
					 op1);
		}
	      else if (TREE_CODE (op1) == NOP_EXPR
		       && decl == TREE_OPERAND (op1, 0))
		{
		  op1 = TREE_OPERAND (op1, 0);
		  op0 = fold_build1_loc (elocus, NOP_EXPR, TREE_TYPE (decl),
					 op0);
		}

	      if (decl == op0)
		cond_ok = true;
	      else if (decl == op1)
		{
		  condcode = swap_tree_comparison (condcode);
		  op1 = op0;
		  op0 = decl;
		  cond_ok = true;
		}

	      if (condcode == NE_EXPR || condcode == EQ_EXPR)
		{
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (decl)))
		    {
		      if (code == OACC_LOOP || condcode == EQ_EXPR)
			cond_ok = false;
		    }
		  else if (operand_equal_p (op1,
					    TYPE_MIN_VALUE (TREE_TYPE (decl)),
					    0))
		    condcode = (condcode == NE_EXPR ? GT_EXPR : LE_EXPR);
		  else if (operand_equal_p (op1,
					    TYPE_MAX_VALUE (TREE_TYPE (decl)),
					    0))
		    condcode = (condcode == NE_EXPR ? LT_EXPR : GE_EXPR);
		  else if (code == OACC_LOOP || condcode == EQ_EXPR)
		    cond_ok = false;
		}

	      if (cond_ok)
		{
		  /* We postponed destructive changes to canonicalize
		     cond until we're sure it is OK.  In the !error_p
		     case where we are trying to transform a regular FOR_STMT
		     to OMP_FOR, we don't want to destroy the original
		     condition if we aren't going to be able to do the
		     transformation anyway.  */
		  TREE_SET_CODE (cond, condcode);
		  TREE_OPERAND (cond, 0) = op0;
		  TREE_OPERAND (cond, 1) = op1;

		  if (TREE_VEC_ELT (condv, i) != cond)
		    {
		      tree ce = NULL_TREE, *pce = &ce;
		      tree type = TREE_TYPE (op1);
		      for (tree c = TREE_VEC_ELT (condv, i); c != cond;
			   c = TREE_OPERAND (c, 1))
			{
			  *pce = build2 (COMPOUND_EXPR, type,
					 TREE_OPERAND (c, 0), op1);
			  pce = &TREE_OPERAND (*pce, 1);
			}
		      op1 = ce;
		      TREE_VEC_ELT (condv, i) = cond;
		    }
		}
	    }

	  if (!cond_ok)
	    {
	      annotation_error (info, as_invalid_predicate, cond, elocus,
				"invalid controlling predicate");
	      fail = true;
	    }
	}

      if (incr == NULL_TREE)
	{
	  annotation_error (info, as_missing_increment, NULL_TREE, elocus,
			    "missing increment expression");
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
				  annotation_error (info,
						    as_invalid_increment,
						    incr, elocus,
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
			  annotation_error (info, as_invalid_increment,
					    incr, elocus,
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
	      annotation_error (info, as_invalid_increment, incr,
				elocus, "invalid increment expression");
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

/* External entry point to c_finish_omp_for_internal, called from the
   parsers.  See above for description of the arguments.  */

tree
c_finish_omp_for (location_t locus, enum tree_code code, tree declv,
		  tree orig_declv, tree initv, tree condv, tree incrv,
		  tree body, tree pre_body, bool final_p)
{
  return c_finish_omp_for_internal (locus, code, declv,
				    orig_declv, initv, condv, incrv,
				    body, pre_body, final_p, NULL);
}


/* Type for passing data in between c_omp_check_loop_iv and
   c_omp_check_loop_iv_r.  */

struct c_omp_check_loop_iv_data
{
  tree declv;
  bool fail;
  location_t stmt_loc;
  location_t expr_loc;
  int kind;
  walk_tree_lh lh;
  hash_set<tree> *ppset;
};

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
      int i;
      for (i = 0; i < TREE_VEC_LENGTH (d->declv); i++)
	if (*tp == TREE_VEC_ELT (d->declv, i)
	    || (TREE_CODE (TREE_VEC_ELT (d->declv, i)) == TREE_LIST
		&& *tp == TREE_PURPOSE (TREE_VEC_ELT (d->declv, i)))
	    || (TREE_CODE (TREE_VEC_ELT (d->declv, i)) == TREE_LIST
		&& TREE_CHAIN (TREE_VEC_ELT (d->declv, i))
		&& (TREE_CODE (TREE_CHAIN (TREE_VEC_ELT (d->declv, i)))
		    == TREE_VEC)
		&& *tp == TREE_VEC_ELT (TREE_CHAIN (TREE_VEC_ELT (d->declv,
								  i)), 2)))
	  {
	    location_t loc = d->expr_loc;
	    if (loc == UNKNOWN_LOCATION)
	      loc = d->stmt_loc;
	    switch (d->kind)
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
    }
  /* Don't walk dtors added by C++ wrap_cleanups_r.  */
  else if (TREE_CODE (*tp) == TRY_CATCH_EXPR
	   && TRY_CATCH_IS_CLEANUP (*tp))
    {
      *walk_subtrees = 0;
      return walk_tree_1 (&TREE_OPERAND (*tp, 0), c_omp_check_loop_iv_r, data,
			  d->ppset, d->lh);
    }

  return NULL_TREE;
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
      data.kind = 0;
      walk_tree_1 (&TREE_OPERAND (init, 1),
		   c_omp_check_loop_iv_r, &data, &pset, lh);
      /* Don't warn for C++ random access iterators here, the
	 expression then involves the subtraction and always refers
	 to the original value.  The C++ FE needs to warn on those
	 earlier.  */
      if (decl == TREE_VEC_ELT (declv, i)
	  || (TREE_CODE (TREE_VEC_ELT (declv, i)) == TREE_LIST
	      && decl == TREE_PURPOSE (TREE_VEC_ELT (declv, i))))
	{
	  data.expr_loc = EXPR_LOCATION (cond);
	  data.kind = 1;
	  walk_tree_1 (&TREE_OPERAND (cond, 1),
		       c_omp_check_loop_iv_r, &data, &pset, lh);
	}
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
			   c_omp_check_loop_iv_r, &data, &pset, lh);
	    }
	  else
	    {
	      data.expr_loc = EXPR_LOCATION (TREE_OPERAND (incr, 1));
	      walk_tree_1 (&TREE_OPERAND (incr, 1),
			   c_omp_check_loop_iv_r, &data, &pset, lh);
	    }
	}
    }
  return !data.fail;
}

/* Similar, but allows to check the init or cond expressions individually.  */

bool
c_omp_check_loop_iv_exprs (location_t stmt_loc, tree declv, tree decl,
			   tree init, tree cond, walk_tree_lh lh)
{
  hash_set<tree> pset;
  struct c_omp_check_loop_iv_data data;

  data.declv = declv;
  data.fail = false;
  data.stmt_loc = stmt_loc;
  data.lh = lh;
  data.ppset = &pset;
  if (init)
    {
      data.expr_loc = EXPR_LOCATION (init);
      data.kind = 0;
      walk_tree_1 (&init,
		   c_omp_check_loop_iv_r, &data, &pset, lh);
    }
  if (cond)
    {
      gcc_assert (COMPARISON_CLASS_P (cond));
      data.expr_loc = EXPR_LOCATION (init);
      data.kind = 1;
      if (TREE_OPERAND (cond, 0) == decl)
	walk_tree_1 (&TREE_OPERAND (cond, 1),
		     c_omp_check_loop_iv_r, &data, &pset, lh);
      else
	walk_tree_1 (&TREE_OPERAND (cond, 0),
		     c_omp_check_loop_iv_r, &data, &pset, lh);
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

/* True if OpenMP sharing attribute of DECL is predetermined.  */

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


/* The following functions implement automatic recognition and annotation of
   for loops in OpenACC kernels regions.  Inside a kernels region, a nest of
   for loops that does not contain any annotated OpenACC loops, nor break
   or goto statements or assignments to the variables controlling loop
   termination, is converted to an OMP_FOR node with an "acc loop auto"
   annotation on each loop.  This feature is controlled by
   flag_openacc_kernels_annotate_loops.  */

/* Check whether DECL is the declaration of a local variable (or function
   parameter) of integral type that does not have its address taken.  */

static bool
is_local_var (tree decl)
{
  return ((TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == PARM_DECL)
	  && DECL_CONTEXT (decl) != NULL
	  && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL
	  && INTEGRAL_TYPE_P (TREE_TYPE (decl))
	  && !TREE_ADDRESSABLE (decl));
}

/* The initializer for a FOR_STMT is sometimes wrapped in various other
   language-specific tree structures.  We need a hook to unwrap them.
   This function takes a tree argument and should return either a
   MODIFY_EXPR, VAR_DECL, or NULL_TREE.  */

static tree (*lang_specific_unwrap_initializer) (tree);

/* Try to annotate the given NODE, which must be a FOR_STMT, with a
   "#pragma acc loop auto" annotation.  In practice, this means
   building an OMP_FOR node for it.  PREV_STMT is the statement
   immediately before the loop, which may be used as the loop's
   initialization statement.  Annotating the loop may fail, in which
   case INFO is used to record the cause of the failure and the
   original loop remains unchanged.  This function returns the
   transformed loop if the transformation succeeded, the original node
   otherwise.  */

static tree
annotate_for_loop (tree node, tree_stmt_iterator *prev_tsi,
		   struct annotation_info *info)
{
  gcc_checking_assert (TREE_CODE (node) == FOR_STMT);

  location_t loc = EXPR_LOCATION (node);
  tree cond = FOR_COND (node);
  gcc_assert (cond);
  tree decl = TREE_OPERAND (cond, 0);
  gcc_assert (decl && TREE_CODE (decl) == VAR_DECL);
  tree init = FOR_INIT_STMT (node);
  tree prev_stmt = NULL_TREE;
  bool unlink_prev = false;
  bool fix_decl = false;


  /* Both the C and C++ front ends normally put the initializer in the
     statement list just before the FOR_STMT instead of in FOR_INIT_STMT.
     If FOR_INIT_STMT happens to exist but isn't a MODIFY_EXPR, bail out
     because the code below won't handle it.  */
  if (init != NULL_TREE && TREE_CODE (init) != MODIFY_EXPR)
    {
      do_not_annotate_loop (info, as_invalid_initializer, NULL_TREE);
      return node;
    }

  /* Examine the statement before the loop to see if it is a
     valid initializer.  It must be either a MODIFY_EXPR or VAR_DECL,
     possibly wrapped in language-specific structure.  */
  if (init == NULL_TREE && prev_tsi != NULL)
    {
      prev_stmt = tsi_stmt (*prev_tsi);

      /* Call the language-specific hook to unwrap prev_stmt.  */
      if (prev_stmt)
	prev_stmt = (*lang_specific_unwrap_initializer) (prev_stmt);

      /* See if we have a valid MODIFY_EXPR.  */
      if (prev_stmt
	  && TREE_CODE (prev_stmt) == MODIFY_EXPR
	  && TREE_OPERAND (prev_stmt, 0) == decl
	  && !TREE_SIDE_EFFECTS (TREE_OPERAND (prev_stmt, 1)))
	{
	  init = prev_stmt;
	  unlink_prev = true;
	}
      else if (prev_stmt == decl
	       && !TREE_SIDE_EFFECTS (DECL_INITIAL (decl)))
	{
	  /* If the preceding statement is the declaration of the loop
	     variable with its initialization, build an assignment
	     expression for the loop's initializer.  */
	  init = build2 (MODIFY_EXPR, TREE_TYPE (decl), decl,
			 DECL_INITIAL (decl));
	  /* We need to remove the initializer from the decl if we
	     end up using the init we just built instead.  */
	  fix_decl = true;
	}
    }

  if (init == NULL_TREE)
    /* There is nothing we can do to find the correct init statement for
       this loop, but c_finish_omp_for insists on having one and would fail
       otherwise.  In that case, we would just return node.  Do that
       directly, here.  */
    {
      do_not_annotate_loop (info, as_missing_initializer, NULL_TREE);
      return node;
    }

  tree incr = FOR_EXPR (node);

  /* The C++ frontend can wrap the increment two levels deep inside a
     cleanup expression, but c_finish_omp_for does not care about that.  */
  if (incr != NULL_TREE && TREE_CODE (incr) == CLEANUP_POINT_EXPR)
    incr = TREE_OPERAND (TREE_OPERAND (incr, 0), 0);
  tree body = FOR_BODY (node);

  tree declv = make_tree_vec (1);
  tree initv = make_tree_vec (1);
  tree condv = make_tree_vec (1);
  tree incrv = make_tree_vec (1);
  TREE_VEC_ELT (declv, 0) = decl;
  TREE_VEC_ELT (initv, 0) = init;
  TREE_VEC_ELT (condv, 0) = cond;
  TREE_VEC_ELT (incrv, 0) = incr;

  /* Do the actual transformation.  This can still fail because
     c_finish_omp_for has some stricter checks than we have performed up to
     this point.  */
  tree omp_for = c_finish_omp_for_internal (loc, OACC_LOOP, declv, NULL_TREE,
					    initv, condv, incrv, body,
					    NULL_TREE, false, info);
  if (omp_for != NULL_TREE)
    {
      if (unlink_prev)
	/* We don't need the previous statement that we consumed as an
	   initializer in the new OMP_FOR any more.  */
	tsi_delink (prev_tsi);

      if (fix_decl)
	/* We no longer need the initializer expression on the decl of
	   the loop variable and don't want to duplicate it.  The
	   kernels conversion pass would interpret it as a stray
	   assignment in a gang-single region.  */
	DECL_INITIAL (prev_stmt) = NULL_TREE;

      /* Add an auto clause, then return the new loop.  */
      tree auto_clause = build_omp_clause (loc, OMP_CLAUSE_AUTO);
      OMP_CLAUSE_CHAIN (auto_clause) = OMP_FOR_CLAUSES (omp_for);
      OMP_FOR_CLAUSES (omp_for) = auto_clause;
      return omp_for;
    }

  return node;
}

/* Forward declaration.  */
static tree annotate_loops_in_kernels_regions (tree *, int *, void *);

/* Given a FOR_STMT NODE that is a candidate for parallelization, check its
   body for validity, then try to annotate it with
   "#pragma oacc loop auto", possibly modifying the current node in place.
   The INFO argument contains the traversal state at the point the loop
   appears.  */

static void
check_and_annotate_for_loop (tree *nodeptr, tree_stmt_iterator *prev_tsi,
			     struct annotation_info *info)
{
  tree node = *nodeptr;
  gcc_assert (TREE_CODE (node) == FOR_STMT);

  /* This structure describes the current loop statement.  */
  struct annotation_info loop_info
    = { node, NULL_TREE, false, as_in_kernels_loop, NULL_TREE, info };
  tree cond = FOR_COND (node);

  /* If we are in the body of an explicitly-annotated loop, do not add
     annotations to this loop or any other nested loops.  */
  if (info->state == as_explicit_annotation)
    do_not_annotate_loop (&loop_info, as_explicit_annotation, info->reason);

  /* We need to find the controlling variable for the loop in order
     to detect whether it is modified in the body of the loop.
     That is why we are doing some checks on the loop condition
     that duplicate what c_finish_omp_for is doing.  */

  /* The loop condition must be a comparison.  */
  else if (cond == NULL_TREE)
    do_not_annotate_loop (&loop_info, as_missing_predicate, NULL_TREE);
  else if (TREE_CODE_CLASS (TREE_CODE (cond)) != tcc_comparison)
    do_not_annotate_loop (&loop_info, as_invalid_predicate, cond);
  else
    {
      /* The condition's LHS must be a local variable that does not
	 have its address taken.  Its RHS must also be such a local
	 variable or a constant.  */
      tree induction_var = TREE_OPERAND (cond, 0);
      tree limit_var = TREE_OPERAND (cond, 1);
      if (!is_local_var (induction_var)
	  || (!is_local_var (limit_var)
	      && (TREE_CODE_CLASS (TREE_CODE (limit_var))
		  != tcc_constant)))
	do_not_annotate_loop (&loop_info, as_invalid_predicate, cond);
      else
	{
	  /* These variables must not be assigned to in the loop.  */
	  loop_info.vars = tree_cons (NULL_TREE, induction_var,
				      loop_info.vars);
	  if (TREE_CODE_CLASS (TREE_CODE (limit_var)) != tcc_constant)
	    loop_info.vars = tree_cons (NULL_TREE, limit_var, loop_info.vars);
	}
    }

  /* Walk the body.  This will process any nested loops, so we have to do it
     even if we have already rejected this loop as a candidate for
     annotation.  */
  walk_tree (&FOR_BODY (node), annotate_loops_in_kernels_regions,
	     (void *) &loop_info, NULL);

  if (loop_info.state == as_in_kernels_loop)
    {
      /* If the traversal of the loop and all nested loops didn't hit
	 any problems, attempt the actual transformation.  If it
	 succeeds, replace this node with the annotated loop.  */
      tree result = annotate_for_loop (node, prev_tsi, &loop_info);
      if (result != node)
	{
	  /* Success!  */
	  *nodeptr = result;
	  return;
	}
    }

  /* If we got here, we have a FOR_STMT we could not convert to an
     OMP loop.  */

  if (loop_info.state == as_invalid_return)
    /* This is diagnosed elsewhere as a hard error, so no warning is
       needed here.  */
    return;

  /* Issue warnings about other problems.  */
  auto_diagnostic_group d;
  if (warning_at (EXPR_LOCATION (node),
		  OPT_Wopenacc_kernels_annotate_loops,
		  "loop cannot be annotated for OpenACC parallelization"))
    {
      location_t locus;
      if (loop_info.reason && EXPR_HAS_LOCATION (loop_info.reason))
	locus = EXPR_LOCATION (loop_info.reason);
      else
	locus = EXPR_LOCATION (node);
      switch (loop_info.state)
	{
	case as_invalid_variable_type:
	  inform (locus, "invalid type for iteration variable %qE",
		  loop_info.reason);
	  break;
	case as_missing_initializer:
	  inform (locus, "missing iteration variable initializer");
	  break;
	case as_invalid_initializer:
	  inform (locus, "unrecognized initializer");
	  break;
	case as_missing_predicate:
	  inform (locus, "missing controlling predicate");
	  break;
	case as_invalid_predicate:
	  inform (locus, "invalid controlling predicate");
	  break;
	case as_missing_increment:
	  inform (locus, "missing increment expression");
	  break;
	case as_invalid_increment:
	  inform (locus, "invalid increment expression");
	  break;
	case as_explicit_annotation:
	  inform (locus, "explicit OpenACC annotation in loop nest");
	  break;
	case as_invalid_control_flow:
	  inform (locus, "loop contains unstructured control flow");
	  break;
	case as_invalid_break:
	  inform (locus, "loop contains %<break%> statement");
	  break;
	case as_invalid_call:
	  inform (locus, "loop contains call to non-oacc function");
	  break;
	case as_invalid_modification:
	  inform (locus, "invalid modification of controlling variable");
	  break;
	default:
	  gcc_unreachable ();
	}
    }
}

/* Traversal function for walk_tree.  Visit the tree, finding OpenACC
   kernels regions.  DATA is NULL if we are outside of a kernels region,
   otherwise it is a pointer to the enclosing kernels region's
   annotation_info struct.  If the traversal encounters a for loop inside a
   kernels region that is a candidate for parallelization, annotate it
   with OpenACC loop directives.  */

static tree
annotate_loops_in_kernels_regions (tree *nodeptr, int *walk_subtrees,
				   void *data)
{
  tree node = *nodeptr;
  struct annotation_info *info = (struct annotation_info *) data;
  gcc_assert (info);

  switch (TREE_CODE (node))
    {
    case OACC_KERNELS:
      /* Recursively process the body of the kernels region in a new info
	 scope.  */
      if (info->state == as_outer)
	{
	  struct annotation_info nested_info
	    = { NULL_TREE, NULL_TREE, true,
		as_in_kernels_region, NULL_TREE, info };
	  walk_tree (&OMP_BODY (node), annotate_loops_in_kernels_regions,
		     (void *) &nested_info, NULL);
	  *walk_subtrees = 0;
	}
      break;

    case OACC_LOOP:
      /* Do not try to add automatic OpenACC annotations inside manually
	 annotated loops.  Presumably, the user avoided doing it on
	 purpose; for example, all available levels of parallelism may
	 have been used up.  */
      {
	struct annotation_info nested_info
	  = { NULL_TREE, NULL_TREE, false, as_explicit_annotation,
	      node, info };
	if (info->state >= as_in_kernels_region)
	  do_not_annotate_loop_nest (info, as_explicit_annotation,
				     node);
	walk_tree (&OMP_BODY (node), annotate_loops_in_kernels_regions,
		   (void *) &nested_info, NULL);
	*walk_subtrees = 0;
      }
      break;

    case FOR_STMT:
      /* Try to annotate the loop if we are in a kernels region.
	 This will do a recursive traversal of the loop body in a new
	 info scope.  */
      if (info->state >= as_in_kernels_region)
	{
	  check_and_annotate_for_loop (nodeptr, NULL, info);
	  *walk_subtrees = 0;
	}
      break;

    case LABEL_EXPR:
      /* Possibly unstructured control flow.  Unless we perform further
	 analyses, we must assume that such control flow may enter the
	 current loop.  In this case, we must not parallelize the loop.  */
      if (info->state >= as_in_kernels_loop
	  && TREE_USED (LABEL_EXPR_LABEL (node)))
	do_not_annotate_loop_nest (info, as_invalid_control_flow, node);
      break;

    case GOTO_EXPR:
      /* Possibly unstructured control flow.  Unless we perform further
	 analyses, we must assume that such control flow may leave the
	 current loop.  In this case, we must not parallelize the loop.  */
      if (info->state >= as_in_kernels_loop)
	do_not_annotate_loop_nest (info, as_invalid_control_flow, node);
      break;

    case BREAK_STMT:
      /* A break statement.  Whether or not this is valid depends on the
	 enclosing context.  */
      if (info->state >= as_in_kernels_loop && !info->break_ok)
	do_not_annotate_loop (info, as_invalid_break, node);
      break;

    case RETURN_EXPR:
      /* A return leaves the entire loop nest.  */
      if (info->state >= as_in_kernels_loop)
	do_not_annotate_loop_nest (info, as_invalid_return, node);
      break;

    case CALL_EXPR:
      /* Direct function calls to functions marked as OpenACC routines are
	 allowed.  Reject indirect calls or calls to non-routines.  */
      if (info->state >= as_in_kernels_loop)
	{
	  tree fn = CALL_EXPR_FN (node), fn_decl = NULL_TREE;
	  if (fn != NULL_TREE && TREE_CODE (fn) == FUNCTION_DECL)
	    fn_decl = fn;
	  else if (fn != NULL_TREE && TREE_CODE (fn) == ADDR_EXPR)
	    {
	      tree fn_op = TREE_OPERAND (fn, 0);
	      if (fn_op != NULL_TREE && TREE_CODE (fn_op) == FUNCTION_DECL)
		fn_decl = fn_op;
	    }
	  if (fn_decl == NULL_TREE)
	    do_not_annotate_loop_nest (info, as_invalid_call, node);
	  else if (!lookup_attribute ("oacc function",
				      DECL_ATTRIBUTES (fn_decl)))
	    do_not_annotate_loop_nest (info, as_invalid_call, node);
	}
      break;

    case MODIFY_EXPR:
      /* See if this assignment's LHS is one of the variables that must
	 not be modified in the loop body because they control termination
	 of the loop (or an enclosing loop in the nest).  */
      if (info->state >= as_in_kernels_loop)
	{
	  tree lhs = TREE_OPERAND (node, 0);
	  if (!is_local_var (lhs))
	    /* Early exit: This cannot be a variable we care about.  */
	    break;
	  /* Walk up the loop stack.  Invalidate the ones controlled by this
	     variable.  There may be several, if this variable is the common
	     iteration limit for several nested loops.  */
	  for (struct annotation_info *outer_loop = info; outer_loop != NULL;
	       outer_loop = outer_loop->next)
	    for (tree t = outer_loop->vars; t != NULL_TREE; t = TREE_CHAIN (t))
	      if (TREE_VALUE (t) == lhs)
		{
		  do_not_annotate_loop (outer_loop,
					as_invalid_modification,
					node);
		  break;
		}
	}
      break;

    case SWITCH_STMT:
      /* Needs special handling to allow break in the body.  */
      if (info->state >= as_in_kernels_loop)
	{
	  bool save_break_ok = info->break_ok;

	  walk_tree (&SWITCH_STMT_COND (node),
		     annotate_loops_in_kernels_regions,
		     (void *) info, NULL);
	  info->break_ok = true;
	  walk_tree (&SWITCH_STMT_BODY (node),
		     annotate_loops_in_kernels_regions,
		     (void *) info, NULL);
	  info->break_ok = save_break_ok;
	  *walk_subtrees = 0;
	}
      break;

    case WHILE_STMT:
      /* Needs special handling to allow break in the body.  */
      if (info->state >= as_in_kernels_loop)
	{
	  bool save_break_ok = info->break_ok;

	  walk_tree (&WHILE_COND (node), annotate_loops_in_kernels_regions,
		     (void *) info, NULL);
	  info->break_ok = true;
	  walk_tree (&WHILE_BODY (node), annotate_loops_in_kernels_regions,
		     (void *) info, NULL);
	  info->break_ok = save_break_ok;
	  *walk_subtrees = 0;
	}
      break;

    case DO_STMT:
      /* Needs special handling to allow break in the body.  */
      if (info->state >= as_in_kernels_loop)
	{
	  bool save_break_ok = info->break_ok;

	  walk_tree (&DO_COND (node), annotate_loops_in_kernels_regions,
		     (void *) info, NULL);
	  info->break_ok = true;
	  walk_tree (&DO_BODY (node), annotate_loops_in_kernels_regions,
		     (void *) info, NULL);
	  info->break_ok = save_break_ok;
	  *walk_subtrees = 0;
	}
      break;

    case STATEMENT_LIST:
      /* We iterate over these explicitly so that we can track the previous
	 statement in the chain.  It may be the initializer for a following
	 FOR_STMT node.  */
      if (info->state >= as_in_kernels_region)
	{
	  tree_stmt_iterator i = tsi_start (node);
	  tree_stmt_iterator prev, *prev_tsi = NULL;
	  while (!tsi_end_p (i))
	    {
	      tree *stmtptr = tsi_stmt_ptr (i);
	      if (TREE_CODE (*stmtptr) == FOR_STMT)
		{
		  check_and_annotate_for_loop (stmtptr, prev_tsi, info);
		  *walk_subtrees = 0;
		}
	      else
		walk_tree (stmtptr, annotate_loops_in_kernels_regions,
			   (void *) info, NULL);
	      prev = i;
	      prev_tsi = &prev;
	      tsi_next (&i);
	    }
	  *walk_subtrees = 0;
	}
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Find for loops in OpenACC kernels regions that do not have OpenACC
   annotations but look like they might benefit from automatic
   parallelization.  Convert them from FOR_STMT to OMP_FOR nodes and
   add the equivalent of "#pragma acc loop auto" annotations for them.
   Assumes flag_openacc_kernels_annotate_loops is set.  */

void
c_oacc_annotate_loops_in_kernels_regions (tree decl,
					  tree (*unwrap_fn) (tree))
{
  struct annotation_info info
    = { NULL_TREE, NULL_TREE, true, as_outer, NULL_TREE, NULL };
  lang_specific_unwrap_initializer = unwrap_fn;
  walk_tree (&DECL_SAVED_TREE (decl), annotate_loops_in_kernels_regions,
	     (void *) &info, NULL);
}
