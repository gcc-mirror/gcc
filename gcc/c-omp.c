/* This file contains routines to construct GNU OpenMP constructs, 
   called from parsing in the C and C++ front ends.

   Copyright (C) 2005, 2007, 2008, 2009 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "function.h"
#include "c-common.h"
#include "toplev.h"
#include "gimple.h"
#include "bitmap.h"
#include "langhooks.h"


/* Complete a #pragma omp master construct.  STMT is the structured-block
   that follows the pragma.  LOC is the l*/

tree
c_finish_omp_master (location_t loc, tree stmt)
{
  tree t = add_stmt (build1 (OMP_MASTER, void_type_node, stmt));
  SET_EXPR_LOCATION (t, loc);
  return t;
}

/* Complete a #pragma omp critical construct.  STMT is the structured-block
   that follows the pragma, NAME is the identifier in the pragma, or null
   if it was omitted.  LOC is the location of the #pragma.  */

tree
c_finish_omp_critical (location_t loc, tree body, tree name)
{
  tree stmt = make_node (OMP_CRITICAL);
  TREE_TYPE (stmt) = void_type_node;
  OMP_CRITICAL_BODY (stmt) = body;
  OMP_CRITICAL_NAME (stmt) = name;
  SET_EXPR_LOCATION (stmt, loc);
  return add_stmt (stmt);
}

/* Complete a #pragma omp ordered construct.  STMT is the structured-block
   that follows the pragma.  LOC is the location of the #pragma.  */

tree
c_finish_omp_ordered (location_t loc, tree stmt)
{
  tree t = build1 (OMP_ORDERED, void_type_node, stmt);
  SET_EXPR_LOCATION (t, loc);
  return add_stmt (t);
}


/* Complete a #pragma omp barrier construct.  LOC is the location of
   the #pragma.  */

void
c_finish_omp_barrier (location_t loc)
{
  tree x;

  x = built_in_decls[BUILT_IN_GOMP_BARRIER];
  x = build_call_expr_loc (loc, x, 0);
  add_stmt (x);
}


/* Complete a #pragma omp taskwait construct.  LOC is the location of the
   pragma.  */

void
c_finish_omp_taskwait (location_t loc)
{
  tree x;

  x = built_in_decls[BUILT_IN_GOMP_TASKWAIT];
  x = build_call_expr_loc (loc, x, 0);
  add_stmt (x);
}


/* Complete a #pragma omp atomic construct.  The expression to be
   implemented atomically is LHS code= RHS.  LOC is the location of
   the atomic statement.  The value returned is either error_mark_node
   (if the construct was erroneous) or an OMP_ATOMIC node which should
   be added to the current statement tree with add_stmt.*/

tree
c_finish_omp_atomic (location_t loc, enum tree_code code, tree lhs, tree rhs)
{
  tree x, type, addr;

  if (lhs == error_mark_node || rhs == error_mark_node)
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

  /* ??? Validate that rhs does not overlap lhs.  */

  /* Take and save the address of the lhs.  From then on we'll reference it
     via indirection.  */
  addr = build_unary_op (loc, ADDR_EXPR, lhs, 0);
  if (addr == error_mark_node)
    return error_mark_node;
  addr = save_expr (addr);
  if (TREE_CODE (addr) != SAVE_EXPR
      && (TREE_CODE (addr) != ADDR_EXPR
	  || TREE_CODE (TREE_OPERAND (addr, 0)) != VAR_DECL))
    {
      /* Make sure LHS is simple enough so that goa_lhs_expr_p can recognize
	 it even after unsharing function body.  */
      tree var = create_tmp_var_raw (TREE_TYPE (addr), NULL);
      DECL_CONTEXT (var) = current_function_decl;
      addr = build4 (TARGET_EXPR, TREE_TYPE (addr), var, addr, NULL, NULL);
    }
  lhs = build_indirect_ref (loc, addr, NULL);

  /* There are lots of warnings, errors, and conversions that need to happen
     in the course of interpreting a statement.  Use the normal mechanisms
     to do this, and then take it apart again.  */
  x = build_modify_expr (input_location, lhs, NULL_TREE, code,
      			 input_location, rhs, NULL_TREE);
  if (x == error_mark_node)
    return error_mark_node;
  gcc_assert (TREE_CODE (x) == MODIFY_EXPR);  
  rhs = TREE_OPERAND (x, 1);

  /* Punt the actual generation of atomic operations to common code.  */
  x = build2 (OMP_ATOMIC, void_type_node, addr, rhs);
  SET_EXPR_LOCATION (x, loc);
  return x;
}


/* Complete a #pragma omp flush construct.  We don't do anything with
   the variable list that the syntax allows.  LOC is the location of
   the #pragma.  */

void
c_finish_omp_flush (location_t loc)
{
  tree x;

  x = built_in_decls[BUILT_IN_SYNCHRONIZE];
  x = build_call_expr_loc (loc, x, 0);
  add_stmt (x);
}


/* Check and canonicalize #pragma omp for increment expression.
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
    default:
      break;
    }

  return error_mark_node;
}

/* Validate and emit code for the OpenMP directive #pragma omp for.
   DECLV is a vector of iteration variables, for each collapsed loop.
   INITV, CONDV and INCRV are vectors containing initialization
   expressions, controlling predicates and increment expressions.
   BODY is the body of the loop and PRE_BODY statements that go before
   the loop.  */

tree
c_finish_omp_for (location_t locus, tree declv, tree initv, tree condv,
		  tree incrv, tree body, tree pre_body)
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

	  init = build_modify_expr (elocus, decl, NULL_TREE, NOP_EXPR, 
	      			    /* FIXME diagnostics: This should
				       be the location of the INIT.  */
	      			    elocus,
				    init,
				    NULL_TREE);
	}
      gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
      gcc_assert (TREE_OPERAND (init, 0) == decl);

      if (cond == NULL_TREE)
	{
	  error_at (elocus, "missing controlling predicate");
	  fail = true;
	}
      else
	{
	  bool cond_ok = false;

	  if (EXPR_HAS_LOCATION (cond))
	    elocus = EXPR_LOCATION (cond);

	  if (TREE_CODE (cond) == LT_EXPR
	      || TREE_CODE (cond) == LE_EXPR
	      || TREE_CODE (cond) == GT_EXPR
	      || TREE_CODE (cond) == GE_EXPR
	      || TREE_CODE (cond) == NE_EXPR)
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

	      if (TREE_CODE (cond) == NE_EXPR)
		{
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (decl)))
		    cond_ok = false;
		  else if (operand_equal_p (TREE_OPERAND (cond, 1),
					    TYPE_MIN_VALUE (TREE_TYPE (decl)),
					    0))
		    TREE_SET_CODE (cond, GT_EXPR);
		  else if (operand_equal_p (TREE_OPERAND (cond, 1),
					    TYPE_MAX_VALUE (TREE_TYPE (decl)),
					    0))
		    TREE_SET_CODE (cond, LT_EXPR);
		  else
		    cond_ok = false;
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
	      if (POINTER_TYPE_P (TREE_TYPE (decl))
		  && TREE_OPERAND (incr, 1))
		{
		  tree t = fold_convert_loc (elocus,
					     sizetype, TREE_OPERAND (incr, 1));

		  if (TREE_CODE (incr) == POSTDECREMENT_EXPR
		      || TREE_CODE (incr) == PREDECREMENT_EXPR)
		    t = fold_build1_loc (elocus, NEGATE_EXPR, sizetype, t);
		  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (decl), decl, t);
		  incr = build2 (MODIFY_EXPR, void_type_node, decl, t);
		}
	      break;

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
      tree t = make_node (OMP_FOR);

      TREE_TYPE (t) = void_type_node;
      OMP_FOR_INIT (t) = initv;
      OMP_FOR_COND (t) = condv;
      OMP_FOR_INCR (t) = incrv;
      OMP_FOR_BODY (t) = body;
      OMP_FOR_PRE_BODY (t) = pre_body;

      SET_EXPR_LOCATION (t, locus);
      return add_stmt (t);
    }
}


/* Divide CLAUSES into two lists: those that apply to a parallel
   construct, and those that apply to a work-sharing construct.  Place
   the results in *PAR_CLAUSES and *WS_CLAUSES respectively.  In
   addition, add a nowait clause to the work-sharing list.  LOC is the
   location of the OMP_PARALLEL*.  */

void
c_split_parallel_clauses (location_t loc, tree clauses,
			  tree *par_clauses, tree *ws_clauses)
{
  tree next;

  *par_clauses = NULL;
  *ws_clauses = build_omp_clause (loc, OMP_CLAUSE_NOWAIT);

  for (; clauses ; clauses = next)
    {
      next = OMP_CLAUSE_CHAIN (clauses);

      switch (OMP_CLAUSE_CODE (clauses))
	{
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LASTPRIVATE:
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_DEFAULT:
	  OMP_CLAUSE_CHAIN (clauses) = *par_clauses;
	  *par_clauses = clauses;
	  break;

	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_COLLAPSE:
	  OMP_CLAUSE_CHAIN (clauses) = *ws_clauses;
	  *ws_clauses = clauses;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* True if OpenMP sharing attribute of DECL is predetermined.  */

enum omp_clause_default_kind
c_omp_predetermined_sharing (tree decl)
{
  /* Variables with const-qualified type having no mutable member
     are predetermined shared.  */
  if (TREE_READONLY (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}
