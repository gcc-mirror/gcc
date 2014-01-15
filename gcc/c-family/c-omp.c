/* This file contains routines to construct GNU OpenMP constructs,
   called from parsing in the C and C++ front ends.

   Copyright (C) 2005-2014 Free Software Foundation, Inc.
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
#include "tree.h"
#include "c-common.h"
#include "c-pragma.h"
#include "gimple-expr.h"
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

/* Complete a #pragma omp taskgroup construct.  STMT is the structured-block
   that follows the pragma.  LOC is the l*/

tree
c_finish_omp_taskgroup (location_t loc, tree stmt)
{
  tree t = add_stmt (build1 (OMP_TASKGROUP, void_type_node, stmt));
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
   tree with add_stmt.  */

tree
c_finish_omp_atomic (location_t loc, enum tree_code code,
		     enum tree_code opcode, tree lhs, tree rhs,
		     tree v, tree lhs1, tree rhs1, bool swapped, bool seq_cst)
{
  tree x, type, addr, pre = NULL_TREE;

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
  lhs = build_indirect_ref (loc, addr, RO_NULL);

  if (code == OMP_ATOMIC_READ)
    {
      x = build1 (OMP_ATOMIC_READ, type, addr);
      SET_EXPR_LOCATION (x, loc);
      OMP_ATOMIC_SEQ_CST (x) = seq_cst;
      return build_modify_expr (loc, v, NULL_TREE, NOP_EXPR,
				loc, x, NULL_TREE);
      return x;
    }

  /* There are lots of warnings, errors, and conversions that need to happen
     in the course of interpreting a statement.  Use the normal mechanisms
     to do this, and then take it apart again.  */
  if (swapped)
    {
      rhs = build2_loc (loc, opcode, TREE_TYPE (lhs), rhs, lhs);
      opcode = NOP_EXPR;
    }
  bool save = in_late_binary_op;
  in_late_binary_op = true;
  x = build_modify_expr (loc, lhs, NULL_TREE, opcode, loc, rhs, NULL_TREE);
  in_late_binary_op = save;
  if (x == error_mark_node)
    return error_mark_node;
  if (TREE_CODE (x) == COMPOUND_EXPR)
    {
      pre = TREE_OPERAND (x, 0);
      gcc_assert (TREE_CODE (pre) == SAVE_EXPR);
      x = TREE_OPERAND (x, 1);
    }
  gcc_assert (TREE_CODE (x) == MODIFY_EXPR);
  rhs = TREE_OPERAND (x, 1);

  /* Punt the actual generation of atomic operations to common code.  */
  if (code == OMP_ATOMIC)
    type = void_type_node;
  x = build2 (code, type, addr, rhs);
  SET_EXPR_LOCATION (x, loc);
  OMP_ATOMIC_SEQ_CST (x) = seq_cst;

  /* Generally it is hard to prove lhs1 and lhs are the same memory
     location, just diagnose different variables.  */
  if (rhs1
      && TREE_CODE (rhs1) == VAR_DECL
      && TREE_CODE (lhs) == VAR_DECL
      && rhs1 != lhs)
    {
      if (code == OMP_ATOMIC)
	error_at (loc, "%<#pragma omp atomic update%> uses two different variables for memory");
      else
	error_at (loc, "%<#pragma omp atomic capture%> uses two different variables for memory");
      return error_mark_node;
    }

  if (code != OMP_ATOMIC)
    {
      /* Generally it is hard to prove lhs1 and lhs are the same memory
	 location, just diagnose different variables.  */
      if (lhs1 && TREE_CODE (lhs1) == VAR_DECL && TREE_CODE (lhs) == VAR_DECL)
	{
	  if (lhs1 != lhs)
	    {
	      error_at (loc, "%<#pragma omp atomic capture%> uses two different variables for memory");
	      return error_mark_node;
	    }
	}
      x = build_modify_expr (loc, v, NULL_TREE, NOP_EXPR,
			     loc, x, NULL_TREE);
      if (rhs1 && rhs1 != lhs)
	{
	  tree rhs1addr = build_unary_op (loc, ADDR_EXPR, rhs1, 0);
	  if (rhs1addr == error_mark_node)
	    return error_mark_node;
	  x = omit_one_operand_loc (loc, type, x, rhs1addr);
	}
      if (lhs1 && lhs1 != lhs)
	{
	  tree lhs1addr = build_unary_op (loc, ADDR_EXPR, lhs1, 0);
	  if (lhs1addr == error_mark_node)
	    return error_mark_node;
	  if (code == OMP_ATOMIC_CAPTURE_OLD)
	    x = omit_one_operand_loc (loc, type, x, lhs1addr);
	  else
	    {
	      x = save_expr (x);
	      x = omit_two_operands_loc (loc, type, x, x, lhs1addr);
	    }
	}
    }
  else if (rhs1 && rhs1 != lhs)
    {
      tree rhs1addr = build_unary_op (loc, ADDR_EXPR, rhs1, 0);
      if (rhs1addr == error_mark_node)
	return error_mark_node;
      x = omit_one_operand_loc (loc, type, x, rhs1addr);
    }

  if (pre)
    x = omit_one_operand_loc (loc, type, x, pre);
  return x;
}


/* Complete a #pragma omp flush construct.  We don't do anything with
   the variable list that the syntax allows.  LOC is the location of
   the #pragma.  */

void
c_finish_omp_flush (location_t loc)
{
  tree x;

  x = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
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
	    if (TREE_CODE_CLASS (TREE_CODE (op1)) == tcc_binary
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

/* Validate and emit code for the OpenMP directive #pragma omp for.
   DECLV is a vector of iteration variables, for each collapsed loop.
   INITV, CONDV and INCRV are vectors containing initialization
   expressions, controlling predicates and increment expressions.
   BODY is the body of the loop and PRE_BODY statements that go before
   the loop.  */

tree
c_finish_omp_for (location_t locus, enum tree_code code, tree declv,
		  tree initv, tree condv, tree incrv, tree body, tree pre_body)
{
  location_t elocus;
  bool fail = false;
  int i;

  if (code == CILK_SIMD
      && !c_check_cilk_loop (locus, TREE_VEC_ELT (declv, 0)))
    fail = true;

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
		    cond_ok = false;
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
		  else if (code != CILK_SIMD)
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

      SET_EXPR_LOCATION (t, locus);
      return add_stmt (t);
    }
}

/* Right now we have 14 different combined constructs, this
   function attempts to split or duplicate clauses for combined
   constructs.  CODE is the innermost construct in the combined construct,
   and MASK allows to determine which constructs are combined together,
   as every construct has at least one clause that no other construct
   has (except for OMP_SECTIONS, but that can be only combined with parallel).
   Combined constructs are:
   #pragma omp parallel for
   #pragma omp parallel sections
   #pragma omp parallel for simd
   #pragma omp for simd
   #pragma omp distribute simd
   #pragma omp distribute parallel for
   #pragma omp distribute parallel for simd
   #pragma omp teams distribute
   #pragma omp teams distribute parallel for
   #pragma omp teams distribute parallel for simd
   #pragma omp target teams
   #pragma omp target teams distribute
   #pragma omp target teams distribute parallel for
   #pragma omp target teams distribute parallel for simd  */

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
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_NOWAIT:
	  s = C_OMP_CLAUSE_SPLIT_FOR;
	  break;
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_LINEAR:
	case OMP_CLAUSE_ALIGNED:
	  s = C_OMP_CLAUSE_SPLIT_SIMD;
	  break;
	/* Duplicate this to all of distribute, for and simd.  */
	case OMP_CLAUSE_COLLAPSE:
	  if (code == OMP_SIMD)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_COLLAPSE);
	      OMP_CLAUSE_COLLAPSE_EXPR (c)
		= OMP_CLAUSE_COLLAPSE_EXPR (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
	      cclauses[C_OMP_CLAUSE_SPLIT_SIMD] = c;
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
	  else
	    s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
	  break;
	/* Private clause is supported on all constructs but target,
	   it is enough to put it on the innermost one.  For
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
	    default: gcc_unreachable ();
	    }
	  break;
	/* Firstprivate clause is supported on all constructs but
	   target and simd.  Put it on the outermost of those and
	   duplicate on parallel.  */
	case OMP_CLAUSE_FIRSTPRIVATE:
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
	      else
		/* This must be
		   #pragma omp parallel{, for{, simd}, sections}.  */
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))
		   != 0)
	    {
	      /* This must be #pragma omp {,target }teams distribute.  */
	      gcc_assert (code == OMP_DISTRIBUTE);
	      s = C_OMP_CLAUSE_SPLIT_TEAMS;
	    }
	  else if ((mask & (OMP_CLAUSE_MASK_1
			    << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
	    {
	      /* This must be #pragma omp distribute simd.  */
	      gcc_assert (code == OMP_SIMD);
	      s = C_OMP_CLAUSE_SPLIT_TEAMS;
	    }
	  else
	    {
	      /* This must be #pragma omp for simd.  */
	      gcc_assert (code == OMP_SIMD);
	      s = C_OMP_CLAUSE_SPLIT_FOR;
	    }
	  break;
	/* Lastprivate is allowed on for, sections and simd.  In
	   parallel {for{, simd},sections} we actually want to put it on
	   parallel rather than for or sections.  */
	case OMP_CLAUSE_LASTPRIVATE:
	  if (code == OMP_FOR || code == OMP_SECTIONS)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
		  != 0)
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	      break;
	    }
	  gcc_assert (code == OMP_SIMD);
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_LASTPRIVATE);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
		  != 0)
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	      OMP_CLAUSE_CHAIN (c) = cclauses[s];
	      cclauses[s] = c;
	    }
	  s = C_OMP_CLAUSE_SPLIT_SIMD;
	  break;
	/* Shared and default clauses are allowed on private and teams.  */
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_DEFAULT:
	  if (code == OMP_TEAMS)
	    {
	      s = C_OMP_CLAUSE_SPLIT_TEAMS;
	      break;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))
	      != 0)
	    {
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
	/* Reduction is allowed on simd, for, parallel, sections and teams.
	   Duplicate it on all of them, but omit on for or sections if
	   parallel is present.  */
	case OMP_CLAUSE_REDUCTION:
	  if (code == OMP_SIMD)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_REDUCTION);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_REDUCTION_CODE (c)
		= OMP_CLAUSE_REDUCTION_CODE (clauses);
	      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		= OMP_CLAUSE_REDUCTION_PLACEHOLDER (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
	      cclauses[C_OMP_CLAUSE_SPLIT_SIMD] = c;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
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
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL];
		  cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL] = c;
		  s = C_OMP_CLAUSE_SPLIT_TEAMS;
		}
	      else if ((mask & (OMP_CLAUSE_MASK_1
				<< PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
		s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	      else
		s = C_OMP_CLAUSE_SPLIT_FOR;
	    }
	  else if (code == OMP_SECTIONS)
	    s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	  else
	    s = C_OMP_CLAUSE_SPLIT_TEAMS;
	  break;
	case OMP_CLAUSE_IF:
	  /* FIXME: This is currently being discussed.  */
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS))
	      != 0)
	    s = C_OMP_CLAUSE_SPLIT_PARALLEL;
	  else
	    s = C_OMP_CLAUSE_SPLIT_TARGET;
	  break;
	default:
	  gcc_unreachable ();
	}
      OMP_CLAUSE_CHAIN (clauses) = cclauses[s];
      cclauses[s] = clauses;
    }
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
			"%qD is not an function argument", decl);
	      continue;
	    }
	  OMP_CLAUSE_DECL (c) = build_int_cst (integer_type_node, idx);
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
