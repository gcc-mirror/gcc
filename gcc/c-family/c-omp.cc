/* This file contains routines to construct OpenACC and OpenMP constructs,
   called from parsing in the C and C++ front ends.

   Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
#include "bitmap.h"
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

/* Complete a #pragma omp masked construct.  BODY is the structured-block
   that follows the pragma.  LOC is the location of the #pragma.  */

tree
c_finish_omp_masked (location_t loc, tree body, tree clauses)
{
  tree stmt = make_node (OMP_MASKED);
  TREE_TYPE (stmt) = void_type_node;
  OMP_MASKED_BODY (stmt) = body;
  OMP_MASKED_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  return add_stmt (stmt);
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
		     tree v, tree lhs1, tree rhs1, tree r, bool swapped,
		     enum omp_memory_order memory_order, bool weak,
		     bool test)
{
  tree x, type, addr, pre = NULL_TREE, rtmp = NULL_TREE, vtmp = NULL_TREE;
  HOST_WIDE_INT bitpos = 0, bitsize = 0;
  enum tree_code orig_opcode = opcode;

  if (lhs == error_mark_node || rhs == error_mark_node
      || v == error_mark_node || lhs1 == error_mark_node
      || rhs1 == error_mark_node || r == error_mark_node)
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
  if (r && r != void_list_node && !INTEGRAL_TYPE_P (TREE_TYPE (r)))
    {
      error_at (loc, "%<#pragma omp atomic compare capture%> with non-integral "
		     "comparison result");
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
      gcc_assert (!weak);
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
      if (opcode == COND_EXPR)
	{
	  bool save = in_late_binary_op;
	  in_late_binary_op = true;
	  std::swap (rhs, rhs1);
	  rhs1 = build_binary_op (loc, EQ_EXPR, lhs, rhs1, true);
	  in_late_binary_op = save;
	}
      else if (swapped)
	rhs = build_binary_op (loc, opcode, rhs, lhs, true);
      else if (opcode != NOP_EXPR)
	rhs = build_binary_op (loc, opcode, lhs, rhs, true);
      opcode = NOP_EXPR;
    }
  else if (opcode == COND_EXPR)
    {
      bool save = in_late_binary_op;
      in_late_binary_op = true;
      std::swap (rhs, rhs1);
      rhs1 = build_binary_op (loc, EQ_EXPR, lhs, rhs1, true);
      in_late_binary_op = save;
      opcode = NOP_EXPR;
    }
  else if (swapped)
    {
      rhs = build_binary_op (loc, opcode, rhs, lhs, true);
      opcode = NOP_EXPR;
    }
  bool save = in_late_binary_op;
  in_late_binary_op = true;
  if ((opcode == MIN_EXPR || opcode == MAX_EXPR)
      && build_binary_op (loc, LT_EXPR, blhs ? blhs : lhs, rhs,
			  true) == error_mark_node)
    x = error_mark_node;
  else
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
  if (orig_opcode == COND_EXPR)
    {
      if (error_operand_p (rhs1))
	return error_mark_node;
      gcc_assert (TREE_CODE (rhs1) == EQ_EXPR);
      tree cmptype = TREE_TYPE (TREE_OPERAND (rhs1, 0));
      if (SCALAR_FLOAT_TYPE_P (cmptype) && !test)
	{
	  bool clear_padding = false;
	  HOST_WIDE_INT non_padding_start = 0;
	  HOST_WIDE_INT non_padding_end = 0;
	  if (BITS_PER_UNIT == 8
	      && CHAR_BIT == 8
	      && clear_padding_type_may_have_padding_p (cmptype))
	    {
	      HOST_WIDE_INT sz = int_size_in_bytes (cmptype), i;
	      gcc_assert (sz > 0);
	      unsigned char *buf = XALLOCAVEC (unsigned char, sz);
	      memset (buf, ~0, sz);
	      clear_type_padding_in_mask (cmptype, buf);
	      for (i = 0; i < sz; i++)
		if (buf[i] != (unsigned char) ~0)
		  {
		    clear_padding = true;
		    break;
		  }
	      if (clear_padding && buf[i] == 0)
		{
		  /* Try to optimize.  In the common case where
		     non-padding bits are all continuous and start
		     and end at a byte boundary, we can just adjust
		     the memcmp call arguments and don't need to
		     emit __builtin_clear_padding calls.  */
		  if (i == 0)
		    {
		      for (i = 0; i < sz; i++)
			if (buf[i] != 0)
			  break;
		      if (i < sz && buf[i] == (unsigned char) ~0)
			{
			  non_padding_start = i;
			  for (; i < sz; i++)
			    if (buf[i] != (unsigned char) ~0)
			      break;
			}
		      else
			i = 0;
		    }
		  if (i != 0)
		    {
		      non_padding_end = i;
		      for (; i < sz; i++)
			if (buf[i] != 0)
			  {
			    non_padding_start = 0;
			    non_padding_end = 0;
			    break;
			  }
		    }
		}
	    }
	  tree inttype = NULL_TREE;
	  if (!clear_padding && tree_fits_uhwi_p (TYPE_SIZE (cmptype)))
	    {
	      HOST_WIDE_INT prec = tree_to_uhwi (TYPE_SIZE (cmptype));
	      inttype = c_common_type_for_size (prec, 1);
	      if (inttype
		  && (!tree_int_cst_equal (TYPE_SIZE (cmptype),
					   TYPE_SIZE (inttype))
		      || TYPE_PRECISION (inttype) != prec))
		inttype = NULL_TREE;
	    }
	  if (inttype)
	    {
	      TREE_OPERAND (rhs1, 0)
		= build1_loc (loc, VIEW_CONVERT_EXPR, inttype,
			      TREE_OPERAND (rhs1, 0));
	      TREE_OPERAND (rhs1, 1)
		= build1_loc (loc, VIEW_CONVERT_EXPR, inttype,
			      TREE_OPERAND (rhs1, 1));
	    }
	  else
	    {
	      tree pcmptype = build_pointer_type (cmptype);
	      tree tmp1 = create_tmp_var_raw (cmptype);
	      TREE_ADDRESSABLE (tmp1) = 1;
	      DECL_CONTEXT (tmp1) = current_function_decl;
	      tmp1 = build4 (TARGET_EXPR, cmptype, tmp1,
			     TREE_OPERAND (rhs1, 0), NULL, NULL);
	      tmp1 = build1 (ADDR_EXPR, pcmptype, tmp1);
	      tree tmp2 = create_tmp_var_raw (cmptype);
	      TREE_ADDRESSABLE (tmp2) = 1;
	      DECL_CONTEXT (tmp2) = current_function_decl;
	      tmp2 = build4 (TARGET_EXPR, cmptype, tmp2,
			     TREE_OPERAND (rhs1, 1), NULL, NULL);
	      tmp2 = build1 (ADDR_EXPR, pcmptype, tmp2);
	      if (non_padding_start)
		{
		  tmp1 = build2 (POINTER_PLUS_EXPR, pcmptype, tmp1,
				 size_int (non_padding_start));
		  tmp2 = build2 (POINTER_PLUS_EXPR, pcmptype, tmp2,
				 size_int (non_padding_start));
		}
	      tree fndecl = builtin_decl_explicit (BUILT_IN_MEMCMP);
	      rhs1 = build_call_expr_loc (loc, fndecl, 3, tmp1, tmp2,
					  non_padding_end
					  ? size_int (non_padding_end
						      - non_padding_start)
					  : TYPE_SIZE_UNIT (cmptype));
	      rhs1 = build2 (EQ_EXPR, boolean_type_node, rhs1,
			     integer_zero_node);
	      if (clear_padding && non_padding_end == 0)
		{
		  fndecl = builtin_decl_explicit (BUILT_IN_CLEAR_PADDING);
		  tree cp1 = build_call_expr_loc (loc, fndecl, 1, tmp1);
		  tree cp2 = build_call_expr_loc (loc, fndecl, 1, tmp2);
		  rhs1 = omit_two_operands_loc (loc, boolean_type_node,
						rhs1, cp2, cp1);
		}
	    }
	}
      if (r && test)
	rtmp = rhs1;
      else if (r)
	{
	  tree var = create_tmp_var_raw (boolean_type_node);
	  DECL_CONTEXT (var) = current_function_decl;
	  rtmp = build4 (TARGET_EXPR, boolean_type_node, var,
			 boolean_false_node, NULL, NULL);
	  save = in_late_binary_op;
	  in_late_binary_op = true;
	  x = build_modify_expr (loc, var, NULL_TREE, NOP_EXPR,
				 loc, rhs1, NULL_TREE);
	  in_late_binary_op = save;
	  if (x == error_mark_node)
	    return error_mark_node;
	  gcc_assert (TREE_CODE (x) == MODIFY_EXPR
		      && TREE_OPERAND (x, 0) == var);
	  TREE_OPERAND (x, 0) = rtmp;
	  rhs1 = omit_one_operand_loc (loc, boolean_type_node, x, rtmp);
	}
      rhs = build3_loc (loc, COND_EXPR, type, rhs1, rhs, new_lhs);
      rhs1 = NULL_TREE;
    }

  /* Punt the actual generation of atomic operations to common code.  */
  if (code == OMP_ATOMIC)
    type = void_type_node;
  x = build2 (code, type, addr, rhs);
  SET_EXPR_LOCATION (x, loc);
  OMP_ATOMIC_MEMORY_ORDER (x) = memory_order;
  OMP_ATOMIC_WEAK (x) = weak;

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
	x = build3_loc (loc, BIT_FIELD_REF, TREE_TYPE (blhs), x,
			bitsize_int (bitsize), bitsize_int (bitpos));
      if (r && !test)
	{
	  vtmp = create_tmp_var_raw (TREE_TYPE (x));
	  DECL_CONTEXT (vtmp) = current_function_decl;
	}
      else
	vtmp = v;
      x = build_modify_expr (loc, vtmp, NULL_TREE, NOP_EXPR,
			     loc, x, NULL_TREE);
      if (x == error_mark_node)
	return error_mark_node;
      type = TREE_TYPE (x);
      if (r && !test)
	{
	  vtmp = build4 (TARGET_EXPR, TREE_TYPE (vtmp), vtmp,
			 build_zero_cst (TREE_TYPE (vtmp)), NULL, NULL);
	  gcc_assert (TREE_CODE (x) == MODIFY_EXPR
		      && TREE_OPERAND (x, 0) == TARGET_EXPR_SLOT (vtmp));
	  TREE_OPERAND (x, 0) = vtmp;
	}
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
  if (r && r != void_list_node)
    {
      in_late_binary_op = true;
      tree x2 = build_modify_expr (loc, r, NULL_TREE, NOP_EXPR,
				   loc, rtmp, NULL_TREE);
      in_late_binary_op = save;
      if (x2 == error_mark_node)
	return error_mark_node;
      x = omit_one_operand_loc (loc, TREE_TYPE (x2), x2, x);
    }
  if (v && vtmp != v)
    {
      in_late_binary_op = true;
      tree x2 = build_modify_expr (loc, v, NULL_TREE, NOP_EXPR,
				   loc, vtmp, NULL_TREE);
      in_late_binary_op = save;
      if (x2 == error_mark_node)
	return error_mark_node;
      x2 = build3_loc (loc, COND_EXPR, void_type_node, rtmp,
		       void_node, x2);
      x = omit_one_operand_loc (loc, TREE_TYPE (x2), x2, x);
    }
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
	  && TYPE_FILE_SCOPE_P (type)
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
      gcc_assert (TREE_CODE (clause) == OMP_CLAUSE);
      if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_DOACROSS)
	{
	  error_at (OMP_CLAUSE_LOCATION (clause),
		    "%<depend(%s)%> is only allowed in %<omp ordered%>",
		    OMP_CLAUSE_DOACROSS_KIND (clause)
		    == OMP_CLAUSE_DOACROSS_SOURCE
		    ? "source" : "sink");
	  return;
	}
      gcc_assert (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_DEPEND);
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
	case OMP_CLAUSE_DEPEND_IN:
	case OMP_CLAUSE_DEPEND_OUT:
	case OMP_CLAUSE_DEPEND_INOUT:
	case OMP_CLAUSE_DEPEND_MUTEXINOUTSET:
	case OMP_CLAUSE_DEPEND_INOUTSET:
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
	  else if (t != null_pointer_node)
	    t = build_fold_addr_expr (t);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    gcc_assert (kind != OMP_CLAUSE_DEPEND_INVALID);

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
    case OMP_CLAUSE_DEPEND_INOUTSET:
      k = GOMP_DEPEND_INOUTSET;
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

  if (mo == MEMMODEL_LAST || mo == MEMMODEL_SEQ_CST)
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

      if (init == NULL_TREE)
	{
	  gcc_assert (decl == NULL_TREE
		      && cond == NULL_TREE
		      && incr == NULL_TREE);
	  for (i++; i < TREE_VEC_LENGTH (declv); i++)
	    gcc_assert (TREE_VEC_ELT (declv, i) == NULL_TREE
			&& TREE_VEC_ELT (initv, i) == NULL_TREE
			&& TREE_VEC_ELT (condv, i) == NULL_TREE
			&& TREE_VEC_ELT (incrv, i) == NULL_TREE);
	  break;
	}

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
    if (TREE_VEC_ELT (d->declv, i) == NULL_TREE)
      continue;
    else if (decl == TREE_VEC_ELT (d->declv, i)
	     || (TREE_CODE (TREE_VEC_ELT (d->declv, i)) == TREE_LIST
		 && decl == TREE_PURPOSE (TREE_VEC_ELT (d->declv, i))))
      return i;
    else if (TREE_CODE (TREE_VEC_ELT (d->declv, i)) == TREE_LIST
	     && TREE_CHAIN (TREE_VEC_ELT (d->declv, i))
	     && (TREE_CODE (TREE_CHAIN (TREE_VEC_ELT (d->declv, i)))
		 == TREE_VEC))
      for (int j = 2;
	   j < TREE_VEC_LENGTH (TREE_CHAIN (TREE_VEC_ELT (d->declv, i))); j++)
	if (decl == TREE_VEC_ELT (TREE_CHAIN (TREE_VEC_ELT (d->declv, i)), j))
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
  else if ((d->kind & 4)
	   && TREE_CODE (*tp) != TREE_VEC
	   && TREE_CODE (*tp) != PLUS_EXPR
	   && TREE_CODE (*tp) != MINUS_EXPR
	   && TREE_CODE (*tp) != MULT_EXPR
	   && TREE_CODE (*tp) != POINTER_PLUS_EXPR
	   && !CONVERT_EXPR_P (*tp))
    {
      *walk_subtrees = 0;
      d->kind &= 3;
      walk_tree_1 (tp, c_omp_check_loop_iv_r, data, NULL, d->lh);
      d->kind |= 4;
      return NULL_TREE;
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
    case POINTER_PLUS_EXPR:
      a1 = TREE_OPERAND (t, 0);
      a2 = TREE_OPERAND (t, 1);
      while (CONVERT_EXPR_P (a1))
	a1 = TREE_OPERAND (a1, 0);
      if (DECL_P (a1) && c_omp_is_loop_iterator (a1, d) >= 0)
	{
	  a2 = TREE_OPERAND (t, 1);
	  t = a1;
	  break;
	}
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

/* Callback for walk_tree to find nested loop transforming construct.  */

static tree
c_find_nested_loop_xform_r (tree *tp, int *walk_subtrees, void *)
{
  *walk_subtrees = 0;
  switch (TREE_CODE (*tp))
    {
    case OMP_TILE:
    case OMP_UNROLL:
      return *tp;
    case BIND_EXPR:
      *walk_subtrees = 1;
      break;
    case STATEMENT_LIST:
      *walk_subtrees = 1;
      break;
    case TRY_FINALLY_EXPR:
    case CLEANUP_POINT_EXPR:
      *walk_subtrees = 1;
      break;
    default:
      break;
    }
  return NULL;
}

/* Find Jth loop among generated loops of STMT.  */

int
c_omp_find_generated_loop (tree &stmt, int j, walk_tree_lh lh)
{
  stmt = walk_tree_1 (&stmt, c_find_nested_loop_xform_r,
		      NULL, NULL, lh);
  gcc_assert (stmt);
  switch (TREE_CODE (stmt))
    {
    case OMP_UNROLL:
      gcc_assert (omp_find_clause (OMP_FOR_CLAUSES (stmt),
				   OMP_CLAUSE_PARTIAL));
      /* FALLTHRU */
    case OMP_TILE:
      int k;
      k = 0;
      for (int i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (stmt)); ++i)
	if (i == j)
	  {
	    if (TREE_VEC_ELT (OMP_FOR_INIT (stmt), i) == NULL_TREE)
	      {
		stmt = OMP_FOR_BODY (stmt);
		return c_omp_find_generated_loop (stmt, k, lh);
	      }
	    else
	      return i;
	  }
	else if (TREE_VEC_ELT (OMP_FOR_INIT (stmt), i) == NULL_TREE)
	  ++k;
      gcc_unreachable ();
    default:
      gcc_unreachable ();
    }
}

/* Diagnose invalid references to loop iterators in lb, b and incr
   expressions.  */

bool
c_omp_check_loop_iv (tree stmt, tree declv, walk_tree_lh lh)
{
  hash_set<tree> pset;
  struct c_omp_check_loop_iv_data data;
  int i, k = 0;

  data.declv = declv;
  data.fail = false;
  data.maybe_nonrect = false;
  data.stmt_loc = EXPR_LOCATION (stmt);
  data.lh = lh;
  data.ppset = &pset;
  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (stmt)); i++)
    {
      tree this_stmt = stmt;
      int j = i;
      tree init = TREE_VEC_ELT (OMP_FOR_INIT (stmt), i);
      if (init == NULL_TREE)
	{
	  if (k == 0)
	    data.declv = copy_node (declv);
	  this_stmt = OMP_FOR_BODY (stmt);
	  j = c_omp_find_generated_loop (this_stmt, k++, lh);
	  init = TREE_VEC_ELT (OMP_FOR_INIT (this_stmt), j);
	  TREE_VEC_ELT (data.declv, i) = TREE_OPERAND (init, 0);
	}
      gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
      tree decl = TREE_OPERAND (init, 0);
      tree cond = TREE_VEC_ELT (OMP_FOR_COND (this_stmt), j);
      gcc_assert (COMPARISON_CLASS_P (cond));
      gcc_assert (TREE_OPERAND (cond, 0) == decl);
      tree incr = TREE_VEC_ELT (OMP_FOR_INCR (this_stmt), j);
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
      if (TREE_CODE (stmt) != OACC_LOOP && i > 0)
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
      if (decl == TREE_VEC_ELT (data.declv, i)
	  || (TREE_CODE (TREE_VEC_ELT (data.declv, i)) == TREE_LIST
	      && decl == TREE_PURPOSE (TREE_VEC_ELT (data.declv, i))))
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
      else if ((vec_outer1 || vec_outer2) && this_stmt != stmt)
	{
	  location_t loc = data.expr_loc;
	  if (loc == UNKNOWN_LOCATION)
	    loc = data.stmt_loc;
	  sorry_at (loc, "non-rectangular loops from generated loops "
			 "unsupported");
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
c_omp_check_loop_iv_exprs (location_t stmt_loc, enum tree_code code,
			   tree declv, int i, tree decl, tree init, tree cond,
			   walk_tree_lh lh)
{
  hash_set<tree> pset;
  struct c_omp_check_loop_iv_data data;
  int kind = (code != OACC_LOOP && i > 0) ? 4 : 0;

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
      data.kind = kind;
      walk_tree_1 (&init,
		   c_omp_check_loop_iv_r, &data, NULL, lh);
    }
  if (cond)
    {
      gcc_assert (COMPARISON_CLASS_P (cond));
      data.expr_loc = EXPR_LOCATION (init);
      data.kind = kind | 1;
      if (TREE_OPERAND (cond, 0) == decl)
	walk_tree_1 (&TREE_OPERAND (cond, 1),
		     c_omp_check_loop_iv_r, &data, NULL, lh);
      else
	walk_tree_1 (&TREE_OPERAND (cond, 0),
		     c_omp_check_loop_iv_r, &data, NULL, lh);
    }
  return !data.fail;
}


/* Helper function for c_omp_check_loop_binding_exprs: look for a binding
   of DECL in BODY.  Only traverse things that might be containers for
   intervening code in an OMP loop.  Returns the BIND_EXPR or DECL_EXPR
   if found, otherwise null.  */

static tree
find_binding_in_body (tree decl, tree body)
{
  if (!body)
    return NULL_TREE;

  switch (TREE_CODE (body))
    {
    case BIND_EXPR:
      for (tree b = BIND_EXPR_VARS (body); b; b = DECL_CHAIN (b))
	if (b == decl)
	  return body;
      return find_binding_in_body (decl, BIND_EXPR_BODY (body));

    case DECL_EXPR:
      if (DECL_EXPR_DECL (body) == decl)
	return body;
      return NULL_TREE;

    case STATEMENT_LIST:
      for (tree_stmt_iterator si = tsi_start (body); !tsi_end_p (si);
	   tsi_next (&si))
	{
	  tree b = find_binding_in_body (decl, tsi_stmt (si));
	  if (b)
	    return b;
	}
      return NULL_TREE;

    case OMP_STRUCTURED_BLOCK:
      return find_binding_in_body (decl, OMP_BODY (body));

    default:
      return NULL_TREE;
    }
}

/* Traversal function for check_loop_binding_expr, to diagnose
   errors when a binding made in intervening code is referenced outside
   of the loop.  Returns non-null if such a reference is found.  DATA points
   to the tree containing the loop body.  */

static tree
check_loop_binding_expr_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
			   void *data)
{
  tree body = *(tree *)data;

  if (DECL_P (*tp) && find_binding_in_body (*tp, body))
    return *tp;
  return NULL_TREE;
}

/* Helper macro used below.  */

#define LOCATION_OR(loc1, loc2) \
  ((loc1) != UNKNOWN_LOCATION ? (loc1) : (loc2))

enum check_loop_binding_expr_ctx {
  CHECK_LOOP_BINDING_EXPR_CTX_LOOP_VAR,
  CHECK_LOOP_BINDING_EXPR_CTX_IN_INIT,
  CHECK_LOOP_BINDING_EXPR_CTX_END_TEST,
  CHECK_LOOP_BINDING_EXPR_CTX_INCR
};

/* Check a single expression EXPR for references to variables bound in
   intervening code in BODY.  Return true if ok, otherwise give an error
   referencing CONTEXT and return false.  Use LOC for the error message
   if EXPR doesn't have one.  */
static bool
check_loop_binding_expr (tree expr, tree body, location_t loc,
			 check_loop_binding_expr_ctx ctx)
{
  tree bad = walk_tree (&expr, check_loop_binding_expr_r, (void *)&body, NULL);

  if (bad)
    {
      location_t eloc = EXPR_LOCATION (expr);
      eloc = LOCATION_OR (eloc, loc);
      switch (ctx)
	{
	case CHECK_LOOP_BINDING_EXPR_CTX_LOOP_VAR:
	  error_at (eloc, "variable %qD used as loop variable is bound "
		    "in intervening code", bad);
	  break;
	case CHECK_LOOP_BINDING_EXPR_CTX_IN_INIT:
	  error_at (eloc, "variable %qD used in initializer is bound "
		    "in intervening code", bad);
	  break;
	case CHECK_LOOP_BINDING_EXPR_CTX_END_TEST:
	  error_at (eloc, "variable %qD used in end test is bound "
		    "in intervening code", bad);
	  break;
	case CHECK_LOOP_BINDING_EXPR_CTX_INCR:
	  error_at (eloc, "variable %qD used in increment expression is bound "
		    "in intervening code", bad);
	  break;
	}
      return false;
    }
  return true;
}

/* STMT is an OMP_FOR construct.  Check all of the iteration variable,
   initializer, end condition, and increment for bindings inside the
   loop body.  If ORIG_INITS is provided, check those elements too.
   Return true if OK, false otherwise.  */
bool
c_omp_check_loop_binding_exprs (tree stmt, vec<tree> *orig_inits)
{
  bool ok = true;
  location_t loc = EXPR_LOCATION (stmt);
  tree body = OMP_FOR_BODY (stmt);
  int orig_init_length = orig_inits ? orig_inits->length () : 0;

  for (int i = 1; i < TREE_VEC_LENGTH (OMP_FOR_INIT (stmt)); i++)
    {
      tree init = TREE_VEC_ELT (OMP_FOR_INIT (stmt), i);
      if (init == NULL_TREE)
	{
	  sorry_at (loc, "imperfectly nested loop using generated loops");
	  ok = false;
	  continue;
	}
      tree cond = TREE_VEC_ELT (OMP_FOR_COND (stmt), i);
      tree incr = TREE_VEC_ELT (OMP_FOR_INCR (stmt), i);
      gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
      tree decl = TREE_OPERAND (init, 0);
      tree orig_init = i < orig_init_length ? (*orig_inits)[i] : NULL_TREE;
      tree e;
      location_t eloc;

      e = TREE_OPERAND (init, 1);
      eloc = LOCATION_OR (EXPR_LOCATION (init), loc);
      if (!check_loop_binding_expr (decl, body, eloc,
				    CHECK_LOOP_BINDING_EXPR_CTX_LOOP_VAR))
	ok = false;
      if (!check_loop_binding_expr (e, body, eloc,
				    CHECK_LOOP_BINDING_EXPR_CTX_IN_INIT))
	ok = false;
      if (orig_init
	  && !check_loop_binding_expr (orig_init, body, eloc,
				       CHECK_LOOP_BINDING_EXPR_CTX_IN_INIT))
	ok = false;

      /* INCR and/or COND may be null if this is a template with a
	 class iterator.  */
      if (cond)
	{
	  eloc = LOCATION_OR (EXPR_LOCATION (cond), loc);
	  if (COMPARISON_CLASS_P (cond) && TREE_OPERAND (cond, 0) == decl)
	    e = TREE_OPERAND (cond, 1);
	  else if (COMPARISON_CLASS_P (cond) && TREE_OPERAND (cond, 1) == decl)
	    e = TREE_OPERAND (cond, 0);
	  else
	    e = cond;
	  if (!check_loop_binding_expr (e, body, eloc,
					CHECK_LOOP_BINDING_EXPR_CTX_END_TEST))
	    ok = false;
	}

      if (incr)
	{
	  eloc = LOCATION_OR (EXPR_LOCATION (incr), loc);
	  /* INCR should be either a MODIFY_EXPR or pre/post
	     increment/decrement.  We don't have to check the latter
	     since there are no operands besides the iteration variable.  */
	  if (TREE_CODE (incr) == MODIFY_EXPR
	      && !check_loop_binding_expr (TREE_OPERAND (incr, 1), body, eloc,
					   CHECK_LOOP_BINDING_EXPR_CTX_INCR))
	    ok = false;
	}
    }

  return ok;
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
   #pragma omp masked taskloop
   #pragma omp masked taskloop simd
   #pragma omp master taskloop
   #pragma omp master taskloop simd
   #pragma omp parallel for
   #pragma omp parallel for simd
   #pragma omp parallel loop
   #pragma omp parallel masked
   #pragma omp parallel masked taskloop
   #pragma omp parallel masked taskloop simd
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
  bool has_dup_allocate = false;

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
	case OMP_CLAUSE_HAS_DEVICE_ADDR:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_DEPEND:
	  s = C_OMP_CLAUSE_SPLIT_TARGET;
	  break;
	case OMP_CLAUSE_DOACROSS:
	  /* This can happen with invalid depend(source) or
	     depend(sink:vec) on target combined with other constructs.  */
	  gcc_assert (OMP_CLAUSE_DOACROSS_DEPEND (clauses));
	  s = C_OMP_CLAUSE_SPLIT_TARGET;
	  break;
	case OMP_CLAUSE_NUM_TEAMS:
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
	case OMP_CLAUSE_FILTER:
	  s = C_OMP_CLAUSE_SPLIT_MASKED;
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
	/* Private clause is supported on all constructs but master/masked,
	   it is enough to put it on the innermost one other than
	   master/masked.  For #pragma omp {for,sections} put it on parallel
	   though, as that's what we did for OpenMP 3.1.  */
	case OMP_CLAUSE_PRIVATE:
	  switch (code)
	    {
	    case OMP_SIMD: s = C_OMP_CLAUSE_SPLIT_SIMD; break;
	    case OMP_FOR: case OMP_SECTIONS:
	    case OMP_PARALLEL: s = C_OMP_CLAUSE_SPLIT_PARALLEL; break;
	    case OMP_DISTRIBUTE: s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE; break;
	    case OMP_TEAMS: s = C_OMP_CLAUSE_SPLIT_TEAMS; break;
	    case OMP_MASTER: s = C_OMP_CLAUSE_SPLIT_PARALLEL; break;
	    case OMP_MASKED: s = C_OMP_CLAUSE_SPLIT_PARALLEL; break;
	    case OMP_TASKLOOP: s = C_OMP_CLAUSE_SPLIT_TASKLOOP; break;
	    case OMP_LOOP: s = C_OMP_CLAUSE_SPLIT_LOOP; break;
	    default: gcc_unreachable ();
	    }
	  break;
	/* Firstprivate clause is supported on all constructs but
	   simd, master, masked and loop.  Put it on the outermost of those
	   and duplicate on teams and parallel.  */
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
		  OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (clauses) = 1;
		  OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT_TARGET (clauses) = 1;
		  break;
		}
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_FIRSTPRIVATE);
	      /* firstprivate should not be applied to target if it is
		 also lastprivate or on the combined/composite construct,
		 or if it is mentioned in map clause.  OMP_CLAUSE_DECLs
		 may need to go through FE handling though (instantiation,
		 C++ non-static data members, array section lowering), so
		 add the clause with OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT and
		 let *finish_omp_clauses and the gimplifier handle it
		 right.  */
	      OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c) = 1;
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
		   #pragma omp parallel mas{ked,ter} taskloop{, simd}.  */
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
	      /* This must be
		 #pragma omp {,{,parallel }mas{ked,ter} }taskloop simd
		 or
		 #pragma omp {,parallel }mas{ked,ter} taskloop.  */
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
	/* order clauses are allowed on distribute, for, simd and loop.  */
	case OMP_CLAUSE_ORDER:
	  if ((mask & (OMP_CLAUSE_MASK_1
		       << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
	    {
	      if (code == OMP_DISTRIBUTE)
		{
		  s = C_OMP_CLAUSE_SPLIT_DISTRIBUTE;
		  break;
		}
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_ORDER);
	      OMP_CLAUSE_ORDER_UNCONSTRAINED (c)
		= OMP_CLAUSE_ORDER_UNCONSTRAINED (clauses);
	      OMP_CLAUSE_ORDER_REPRODUCIBLE (c)
		= OMP_CLAUSE_ORDER_REPRODUCIBLE (clauses);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE];
	      cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE] = c;
	    }
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
	    {
	      if (code == OMP_SIMD)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_ORDER);
		  OMP_CLAUSE_ORDER_UNCONSTRAINED (c)
		    = OMP_CLAUSE_ORDER_UNCONSTRAINED (clauses);
		  OMP_CLAUSE_ORDER_REPRODUCIBLE (c)
		    = OMP_CLAUSE_ORDER_REPRODUCIBLE (clauses);
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
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) != 0)
	    {
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_MAP);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TOFROM);
	      OMP_CLAUSE_MAP_IMPLICIT (c) = 1;
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
	      cclauses[C_OMP_CLAUSE_SPLIT_TARGET] = c;
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
		   || code == OMP_MASTER
		   || code == OMP_MASKED)
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
	      else if ((mask & (OMP_CLAUSE_MASK_1
				<< PRAGMA_OMP_CLAUSE_NUM_TEAMS)) != 0)
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
		}
	      s = C_OMP_CLAUSE_SPLIT_SIMD;
	    }
	  else
	    s = C_OMP_CLAUSE_SPLIT_TEAMS;
	  break;
	case OMP_CLAUSE_IN_REDUCTION:
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) != 0)
	    {
	      /* When on target, map(always, tofrom: item) is added as
		 well.  For non-combined target it is added in the FEs.  */
	      c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
				    OMP_CLAUSE_MAP);
	      OMP_CLAUSE_DECL (c) = OMP_CLAUSE_DECL (clauses);
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_ALWAYS_TOFROM);
	      OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
	      cclauses[C_OMP_CLAUSE_SPLIT_TARGET] = c;
	      s = C_OMP_CLAUSE_SPLIT_TARGET;
	      break;
	    }
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
	  /* thread_limit is allowed on target and teams.  Distribute it
	     to all.  */
	case OMP_CLAUSE_THREAD_LIMIT:
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP))
	      != 0)
	    {
	      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS))
		  != 0)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_THREAD_LIMIT);
		  OMP_CLAUSE_THREAD_LIMIT_EXPR (c)
		    = OMP_CLAUSE_THREAD_LIMIT_EXPR (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
		  cclauses[C_OMP_CLAUSE_SPLIT_TARGET] = c;
		}
	      else
		{
		  s = C_OMP_CLAUSE_SPLIT_TARGET;
		  break;
		}
	    }
	  s = C_OMP_CLAUSE_SPLIT_TEAMS;
	  break;
	/* Allocate clause is allowed on target, teams, distribute, parallel,
	   for, sections and taskloop.  Distribute it to all.  */
	case OMP_CLAUSE_ALLOCATE:
	  s = C_OMP_CLAUSE_SPLIT_COUNT;
	  for (i = 0; i < C_OMP_CLAUSE_SPLIT_COUNT; i++)
	    {
	      switch (i)
		{
		case C_OMP_CLAUSE_SPLIT_TARGET:
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_MAP)) == 0)
		    continue;
		  break;
		case C_OMP_CLAUSE_SPLIT_TEAMS:
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_NUM_TEAMS)) == 0)
		    continue;
		  break;
		case C_OMP_CLAUSE_SPLIT_DISTRIBUTE:
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) == 0)
		    continue;
		  break;
		case C_OMP_CLAUSE_SPLIT_PARALLEL:
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_NUM_THREADS)) == 0)
		    continue;
		  break;
		case C_OMP_CLAUSE_SPLIT_FOR:
		  STATIC_ASSERT (C_OMP_CLAUSE_SPLIT_SECTIONS
				 == C_OMP_CLAUSE_SPLIT_FOR
				 && (C_OMP_CLAUSE_SPLIT_TASKLOOP
				     == C_OMP_CLAUSE_SPLIT_FOR)
				 && (C_OMP_CLAUSE_SPLIT_LOOP
				     == C_OMP_CLAUSE_SPLIT_FOR));
		  if (code == OMP_SECTIONS)
		    break;
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_SCHEDULE)) != 0)
		    break;
		  if ((mask & (OMP_CLAUSE_MASK_1
			       << PRAGMA_OMP_CLAUSE_NOGROUP)) != 0)
		    break;
		  continue;
		case C_OMP_CLAUSE_SPLIT_SIMD:
		  continue;
		default:
		  gcc_unreachable ();
		}
	      if (s != C_OMP_CLAUSE_SPLIT_COUNT)
		{
		  c = build_omp_clause (OMP_CLAUSE_LOCATION (clauses),
					OMP_CLAUSE_ALLOCATE);
		  OMP_CLAUSE_DECL (c)
		    = OMP_CLAUSE_DECL (clauses);
		  OMP_CLAUSE_ALLOCATE_ALLOCATOR (c)
		    = OMP_CLAUSE_ALLOCATE_ALLOCATOR (clauses);
		  OMP_CLAUSE_ALLOCATE_ALIGN (c)
		    = OMP_CLAUSE_ALLOCATE_ALIGN (clauses);
		  OMP_CLAUSE_CHAIN (c) = cclauses[s];
		  cclauses[s] = c;
		  has_dup_allocate = true;
		}
	      s = (enum c_omp_clause_split) i;
	    }
	  gcc_assert (s != C_OMP_CLAUSE_SPLIT_COUNT);
	  break;
	default:
	  gcc_unreachable ();
	}
      OMP_CLAUSE_CHAIN (clauses) = cclauses[s];
      cclauses[s] = clauses;
    }

  if (has_dup_allocate)
    {
      bool need_prune = false;
      bitmap_obstack_initialize (NULL);
      for (i = 0; i < C_OMP_CLAUSE_SPLIT_SIMD - (code == OMP_LOOP); i++)
	if (cclauses[i])
	  {
	    bitmap_head allocate_head;
	    bitmap_initialize (&allocate_head, &bitmap_default_obstack);
	    for (c = cclauses[i]; c; c = OMP_CLAUSE_CHAIN (c))
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ALLOCATE
		  && DECL_P (OMP_CLAUSE_DECL (c)))
		bitmap_set_bit (&allocate_head,
				DECL_UID (OMP_CLAUSE_DECL (c)));
	    for (c = cclauses[i]; c; c = OMP_CLAUSE_CHAIN (c))
	      switch (OMP_CLAUSE_CODE (c))
		{
		case OMP_CLAUSE_REDUCTION:
		case OMP_CLAUSE_IN_REDUCTION:
		case OMP_CLAUSE_TASK_REDUCTION:
		  if (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF)
		    {
		      tree t = TREE_OPERAND (OMP_CLAUSE_DECL (c), 0);
		      if (TREE_CODE (t) == POINTER_PLUS_EXPR)
			t = TREE_OPERAND (t, 0);
		      if (TREE_CODE (t) == ADDR_EXPR
			  || INDIRECT_REF_P (t))
			t = TREE_OPERAND (t, 0);
		      if (DECL_P (t))
			bitmap_clear_bit (&allocate_head, DECL_UID (t));
		      break;
		    }
		  else if (TREE_CODE (OMP_CLAUSE_DECL (c)) == TREE_LIST)
		    {
		      /* TODO: This can go away once we transition all uses of
			 TREE_LIST for representing OMP array sections to
			 OMP_ARRAY_SECTION.  */
		      tree t;
		      for (t = OMP_CLAUSE_DECL (c);
			   TREE_CODE (t) == TREE_LIST; t = TREE_CHAIN (t))
			;
		      if (DECL_P (t))
			bitmap_clear_bit (&allocate_head, DECL_UID (t));
		      break;
		    }
		  else if (TREE_CODE (OMP_CLAUSE_DECL (c)) == OMP_ARRAY_SECTION)
		    {
		      tree t;
		      for (t = OMP_CLAUSE_DECL (c);
			   TREE_CODE (t) == OMP_ARRAY_SECTION;
			   t = TREE_OPERAND (t, 0))
			;
		      if (DECL_P (t))
			bitmap_clear_bit (&allocate_head, DECL_UID (t));
		      break;
		    }
		  /* FALLTHRU */
		case OMP_CLAUSE_PRIVATE:
		case OMP_CLAUSE_FIRSTPRIVATE:
		case OMP_CLAUSE_LASTPRIVATE:
		case OMP_CLAUSE_LINEAR:
		  if (DECL_P (OMP_CLAUSE_DECL (c)))
		    bitmap_clear_bit (&allocate_head,
				      DECL_UID (OMP_CLAUSE_DECL (c)));
		  break;
		default:
		  break;
		}
	    for (c = cclauses[i]; c; c = OMP_CLAUSE_CHAIN (c))
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ALLOCATE
		  && DECL_P (OMP_CLAUSE_DECL (c))
		  && bitmap_bit_p (&allocate_head,
				   DECL_UID (OMP_CLAUSE_DECL (c))))
		{
		  /* Mark allocate clauses which don't have corresponding
		     explicit data sharing clause.  */
		  OMP_CLAUSE_ALLOCATE_COMBINED (c) = 1;
		  need_prune = true;
		}
	  }
      bitmap_obstack_release (NULL);
      if (need_prune)
	{
	  /* At least one allocate clause has been marked.  Walk all the
	     duplicated allocate clauses in sync.  If it is marked in all
	     constituent constructs, diagnose it as invalid and remove
	     them.  Otherwise, remove all marked inner clauses inside
	     a construct that doesn't have them marked.  Keep the outer
	     marked ones, because some clause duplication is done only
	     during gimplification.  */
	  tree *p[C_OMP_CLAUSE_SPLIT_COUNT];
	  for (i = 0; i < C_OMP_CLAUSE_SPLIT_COUNT; i++)
	    if (cclauses[i] == NULL_TREE
		|| i == C_OMP_CLAUSE_SPLIT_SIMD
		|| (i == C_OMP_CLAUSE_SPLIT_LOOP && code == OMP_LOOP))
	      p[i] = NULL;
	    else
	      p[i] = &cclauses[i];
	  do
	    {
	      int j = -1;
	      tree seen = NULL_TREE;
	      for (i = C_OMP_CLAUSE_SPLIT_COUNT - 1; i >= 0; i--)
		if (p[i])
		  {
		    while (*p[i]
			   && OMP_CLAUSE_CODE (*p[i]) != OMP_CLAUSE_ALLOCATE)
		      p[i] = &OMP_CLAUSE_CHAIN (*p[i]);
		    if (*p[i] == NULL_TREE)
		      {
			i = C_OMP_CLAUSE_SPLIT_COUNT;
			break;
		      }
		    if (!OMP_CLAUSE_ALLOCATE_COMBINED (*p[i]) && j == -1)
		      j = i;
		    seen = *p[i];
		  }
	      if (i == C_OMP_CLAUSE_SPLIT_COUNT)
		break;
	      if (j == -1)
		error_at (OMP_CLAUSE_LOCATION (seen),
			  "%qD specified in %<allocate%> clause but not in "
			  "an explicit privatization clause",
			  OMP_CLAUSE_DECL (seen));
	      for (i = 0; i < C_OMP_CLAUSE_SPLIT_COUNT; i++)
		if (p[i])
		  {
		    if (i > j)
		      /* Remove.  */
		      *p[i] = OMP_CLAUSE_CHAIN (*p[i]);
		    else
		      /* Keep.  */
		      p[i] = &OMP_CLAUSE_CHAIN (*p[i]);
		  }
	    }
	  while (1);
	}
    }

  if (!flag_checking)
    return;

  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) == 0)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_TARGET] == NULL_TREE);
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS)) == 0)
    gcc_assert (cclauses[C_OMP_CLAUSE_SPLIT_TEAMS] == NULL_TREE);
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) == 0
      && (mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FILTER)) == 0)
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
      && TREE_STATIC (decl)
      && DECL_NAME (decl))
    {
      if (TREE_READONLY (decl)
	  && (DECL_NAME (decl) == ridpointers[RID_C99_FUNCTION_NAME]
	      || DECL_NAME (decl) == ridpointers[RID_FUNCTION_NAME]
	      || DECL_NAME (decl) == ridpointers[RID_PRETTY_FUNCTION_NAME]))
	return true;
      /* For UBSan handle the same also ubsan_create_data created
	 variables.  There is no magic flag for those, but user variables
	 shouldn't be DECL_ARTIFICIAL or have TYPE_ARTIFICIAL type with
	 such names.  */
      if ((flag_sanitize & (SANITIZE_UNDEFINED
			    | SANITIZE_UNDEFINED_NONDEFAULT)) != 0
	  && DECL_IGNORED_P (decl)
	  && !TREE_READONLY (decl)
	  && TREE_CODE (DECL_NAME (decl)) == IDENTIFIER_NODE
	  && TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
	  && TYPE_ARTIFICIAL (TREE_TYPE (decl))
	  && TYPE_NAME (TREE_TYPE (decl))
	  && TREE_CODE (TYPE_NAME (TREE_TYPE (decl))) == TYPE_DECL
	  && DECL_NAME (TYPE_NAME (TREE_TYPE (decl)))
	  && (TREE_CODE (DECL_NAME (TYPE_NAME (TREE_TYPE (decl))))
	      == IDENTIFIER_NODE))
	{
	  tree id1 = DECL_NAME (decl);
	  tree id2 = DECL_NAME (TYPE_NAME (TREE_TYPE (decl)));
	  if (IDENTIFIER_LENGTH (id1) >= sizeof ("ubsan_data") - 1
	      && IDENTIFIER_LENGTH (id2) >= sizeof ("__ubsan__data")
	      && !memcmp (IDENTIFIER_POINTER (id2), "__ubsan_",
			  sizeof ("__ubsan_") - 1)
	      && !memcmp (IDENTIFIER_POINTER (id2) + IDENTIFIER_LENGTH (id2)
			  - sizeof ("_data") + 1, "_data",
			  sizeof ("_data") - 1)
	      && strstr (IDENTIFIER_POINTER (id1), "ubsan_data"))
	    return true;
	}
    }
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


/* Used to merge map clause information in c_omp_adjust_map_clauses.  */
struct map_clause
{
  tree clause;
  bool firstprivate_ptr_p;
  bool decl_mapped;
  bool omp_declare_target;
  map_clause (void) : clause (NULL_TREE), firstprivate_ptr_p (false),
    decl_mapped (false), omp_declare_target (false) { }
};

/* Adjust map clauses after normal clause parsing, mainly to mark specific
   base-pointer map cases addressable that may be turned into attach/detach
   operations during gimplification.  */
void
c_omp_adjust_map_clauses (tree clauses, bool is_target)
{
  if (!is_target)
    {
      /* If this is not a target construct, just turn firstprivate pointers
	 into attach/detach, the runtime will check and do the rest.  */

      for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	    && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
	    && DECL_P (OMP_CLAUSE_DECL (c))
	    && POINTER_TYPE_P (TREE_TYPE (OMP_CLAUSE_DECL (c))))
	  {
	    tree ptr = OMP_CLAUSE_DECL (c);
	    c_common_mark_addressable_vec (ptr);
	  }
      return;
    }

  hash_map<tree, map_clause> maps;

  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	&& DECL_P (OMP_CLAUSE_DECL (c)))
      {
	/* If this is for a target construct, the firstprivate pointer
	   is marked addressable if either is true:
	   (1) the base-pointer is mapped in this same construct, or
	   (2) the base-pointer is a variable place on the device by
	       "declare target" directives.

	   Here we iterate through all map clauses collecting these cases,
	   and merge them with a hash_map to process below.  */

	if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FIRSTPRIVATE_POINTER
	    && POINTER_TYPE_P (TREE_TYPE (OMP_CLAUSE_DECL (c))))
	  {
	    tree ptr = OMP_CLAUSE_DECL (c);
	    map_clause &mc = maps.get_or_insert (ptr);
	    if (mc.clause == NULL_TREE)
	      mc.clause = c;
	    mc.firstprivate_ptr_p = true;

	    if (is_global_var (ptr)
		&& lookup_attribute ("omp declare target",
				     DECL_ATTRIBUTES (ptr)))
	      mc.omp_declare_target = true;
	  }
	else if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALLOC
		 || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_TO
		 || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FROM
		 || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_TOFROM
		 || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_TO
		 || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_FROM
		 || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_TOFROM)
	  {
	    map_clause &mc = maps.get_or_insert (OMP_CLAUSE_DECL (c));
	    mc.decl_mapped = true;
	  }
      }

  for (hash_map<tree, map_clause>::iterator i = maps.begin ();
       i != maps.end (); ++i)
    {
      map_clause &mc = (*i).second;

      if (mc.firstprivate_ptr_p
	  && (mc.decl_mapped || mc.omp_declare_target))
	c_common_mark_addressable_vec (OMP_CLAUSE_DECL (mc.clause));
    }
}

/* Maybe strip off an indirection from a "converted" reference, then find the
   origin of a pointer (i.e. without any offset).  */

tree
c_omp_address_inspector::unconverted_ref_origin ()
{
  tree t = orig;

  /* We may have a reference-typed component access at the outermost level
     that has had convert_from_reference called on it.  Get the un-dereferenced
     reference itself.  */
  t = maybe_unconvert_ref (t);

  /* Find base pointer for POINTER_PLUS_EXPR, etc.  */
  t = get_origin (t);

  return t;
}

/* Return TRUE if the address is a component access.  */

bool
c_omp_address_inspector::component_access_p ()
{
  tree t = maybe_unconvert_ref (orig);

  t = get_origin (t);

  return TREE_CODE (t) == COMPONENT_REF;
}

/* Perform various checks on the address, as described by clause CLAUSE (we
   only use its code and location here).  */

bool
c_omp_address_inspector::check_clause (tree clause)
{
  tree t = unconverted_ref_origin ();

  if (TREE_CODE (t) != COMPONENT_REF)
    return true;

  if (TREE_CODE (TREE_OPERAND (t, 1)) == FIELD_DECL
      && DECL_BIT_FIELD (TREE_OPERAND (t, 1)))
    {
      error_at (OMP_CLAUSE_LOCATION (clause),
		"bit-field %qE in %qs clause",
		t, omp_clause_code_name[OMP_CLAUSE_CODE (clause)]);
      return false;
    }
  else if (!processing_template_decl_p ()
	   && !omp_mappable_type (TREE_TYPE (t)))
    {
      error_at (OMP_CLAUSE_LOCATION (clause),
		"%qE does not have a mappable type in %qs clause",
		t, omp_clause_code_name[OMP_CLAUSE_CODE (clause)]);
      emit_unmappable_type_notes (TREE_TYPE (t));
      return false;
    }
  else if (TREE_TYPE (t) && TYPE_ATOMIC (TREE_TYPE (t)))
    {
      error_at (OMP_CLAUSE_LOCATION (clause),
		"%<_Atomic%> %qE in %qs clause", t,
		omp_clause_code_name[OMP_CLAUSE_CODE (clause)]);
      return false;
    }

  return true;
}

/* Find the "root term" for the address.  This is the innermost decl, etc.
   of the access.  */

tree
c_omp_address_inspector::get_root_term (bool checking)
{
  if (root_term && !checking)
    return root_term;

  tree t = unconverted_ref_origin ();

  while (TREE_CODE (t) == COMPONENT_REF)
    {
      if (checking
	  && TREE_TYPE (TREE_OPERAND (t, 0))
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == UNION_TYPE)
	{
	  error_at (loc, "%qE is a member of a union", t);
	  return error_mark_node;
	}
      t = TREE_OPERAND (t, 0);
      while (TREE_CODE (t) == MEM_REF
	     || TREE_CODE (t) == INDIRECT_REF
	     || TREE_CODE (t) == ARRAY_REF)
	{
	  if (TREE_CODE (t) == MEM_REF
	      || TREE_CODE (t) == INDIRECT_REF)
	    indirections = true;
	  t = TREE_OPERAND (t, 0);
	  STRIP_NOPS (t);
	  if (TREE_CODE (t) == POINTER_PLUS_EXPR)
	    t = TREE_OPERAND (t, 0);
	}
    }

  root_term = t;

  return t;
}

/* Return TRUE if the address is supported in mapping clauses.  At present,
   this means that the innermost expression is a DECL_P, but could be extended
   to other types of expression in the future.  */

bool
c_omp_address_inspector::map_supported_p ()
{
  /* If we've already decided if the mapped address is supported, return
     that.  */
  if (map_supported != -1)
    return map_supported;

  tree t = unconverted_ref_origin ();

  STRIP_NOPS (t);

  while (TREE_CODE (t) == INDIRECT_REF
	 || TREE_CODE (t) == MEM_REF
	 || TREE_CODE (t) == ARRAY_REF
	 || TREE_CODE (t) == COMPONENT_REF
	 || TREE_CODE (t) == COMPOUND_EXPR
	 || TREE_CODE (t) == SAVE_EXPR
	 || TREE_CODE (t) == POINTER_PLUS_EXPR
	 || TREE_CODE (t) == NON_LVALUE_EXPR
	 || TREE_CODE (t) == OMP_ARRAY_SECTION
	 || TREE_CODE (t) == NOP_EXPR)
    if (TREE_CODE (t) == COMPOUND_EXPR)
      t = TREE_OPERAND (t, 1);
    else
      t = TREE_OPERAND (t, 0);

  STRIP_NOPS (t);

  map_supported = DECL_P (t);

  return map_supported;
}

/* Get the origin of an address T, stripping off offsets and some other
   bits.  */

tree
c_omp_address_inspector::get_origin (tree t)
{
  while (1)
    {
      if (TREE_CODE (t) == COMPOUND_EXPR)
	{
	  t = TREE_OPERAND (t, 1);
	  STRIP_NOPS (t);
	}
      else if (TREE_CODE (t) == POINTER_PLUS_EXPR
	       || TREE_CODE (t) == SAVE_EXPR)
	t = TREE_OPERAND (t, 0);
      else if (!processing_template_decl_p ()
	       && TREE_CODE (t) == INDIRECT_REF
	       && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == REFERENCE_TYPE)
	t = TREE_OPERAND (t, 0);
      else
	break;
    }
  STRIP_NOPS (t);
  return t;
}

/* For an address T that might be a reference that has had
   "convert_from_reference" called on it, return the actual reference without
   any indirection.  */

tree
c_omp_address_inspector::maybe_unconvert_ref (tree t)
{
  /* Be careful not to dereference the type if we're processing a
     template decl, else it might be NULL.  */
  if (!processing_template_decl_p ()
      && TREE_CODE (t) == INDIRECT_REF
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (t, 0))) == REFERENCE_TYPE)
    return TREE_OPERAND (t, 0);

  return t;
}

/* Return TRUE if CLAUSE might describe a zero-length array section.  */

bool
c_omp_address_inspector::maybe_zero_length_array_section (tree clause)
{
  switch (OMP_CLAUSE_MAP_KIND (clause))
    {
    case GOMP_MAP_ALLOC:
    case GOMP_MAP_IF_PRESENT:
    case GOMP_MAP_TO:
    case GOMP_MAP_FROM:
    case GOMP_MAP_TOFROM:
    case GOMP_MAP_ALWAYS_TO:
    case GOMP_MAP_ALWAYS_FROM:
    case GOMP_MAP_ALWAYS_TOFROM:
    case GOMP_MAP_PRESENT_ALLOC:
    case GOMP_MAP_PRESENT_TO:
    case GOMP_MAP_PRESENT_FROM:
    case GOMP_MAP_PRESENT_TOFROM:
    case GOMP_MAP_ALWAYS_PRESENT_TO:
    case GOMP_MAP_ALWAYS_PRESENT_FROM:
    case GOMP_MAP_ALWAYS_PRESENT_TOFROM:
    case GOMP_MAP_RELEASE:
    case GOMP_MAP_DELETE:
    case GOMP_MAP_FORCE_TO:
    case GOMP_MAP_FORCE_FROM:
    case GOMP_MAP_FORCE_TOFROM:
    case GOMP_MAP_FORCE_PRESENT:
      return true;
    default:
      return false;
    }
}

/* Expand a chained access.  We only expect to see a quite limited range of
   expression types here, because e.g. you can't have an array of
   references.  */

static tree
omp_expand_access_chain (tree c, tree expr, vec<omp_addr_token *> &addr_tokens,
			 unsigned *idx, c_omp_region_type ort)
{
  using namespace omp_addr_tokenizer;
  location_t loc = OMP_CLAUSE_LOCATION (c);
  unsigned i = *idx;
  tree c2 = NULL_TREE;
  gomp_map_kind kind;

  if ((ort & C_ORT_EXIT_DATA) != 0
      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FROM
      || (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
	  && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FROM
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DELETE
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_RELEASE
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_FROM
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FORCE_FROM
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_PRESENT_FROM
	      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_PRESENT_FROM)))
    kind = GOMP_MAP_DETACH;
  else
    kind = GOMP_MAP_ATTACH;

  switch (addr_tokens[i]->u.access_kind)
    {
    case ACCESS_POINTER:
    case ACCESS_POINTER_OFFSET:
      {
	tree virtual_origin
	  = fold_convert_loc (loc, ptrdiff_type_node, addr_tokens[i]->expr);
	tree data_addr = omp_accessed_addr (addr_tokens, i, expr);
	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	OMP_CLAUSE_SET_MAP_KIND (c2, kind);
	OMP_CLAUSE_DECL (c2) = addr_tokens[i]->expr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    case ACCESS_INDEXED_ARRAY:
      break;

    default:
      return error_mark_node;
    }

  if (c2)
    {
      OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (c);
      OMP_CLAUSE_CHAIN (c) = c2;
      c = c2;
    }

  *idx = ++i;

  if (i < addr_tokens.length ()
      && addr_tokens[i]->type == ACCESS_METHOD)
    return omp_expand_access_chain (c, expr, addr_tokens, idx, ort);

  return c;
}

/* Translate "array_base_decl access_method" to OMP mapping clauses.  */

tree
c_omp_address_inspector::expand_array_base (tree c,
					    vec<omp_addr_token *> &addr_tokens,
					    tree expr, unsigned *idx,
					    c_omp_region_type ort)
{
  using namespace omp_addr_tokenizer;
  location_t loc = OMP_CLAUSE_LOCATION (c);
  int i = *idx;
  tree decl = addr_tokens[i + 1]->expr;
  bool decl_p = DECL_P (decl);
  bool declare_target_p = (decl_p
			   && is_global_var (decl)
			   && lookup_attribute ("omp declare target",
						DECL_ATTRIBUTES (decl)));
  bool map_p = OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP;
  bool implicit_p = map_p && OMP_CLAUSE_MAP_IMPLICIT (c);
  bool chain_p = omp_access_chain_p (addr_tokens, i + 1);
  tree c2 = NULL_TREE, c3 = NULL_TREE;
  unsigned consume_tokens = 2;
  bool target_p = (ort & C_ORT_TARGET) != 0;
  bool openmp_p = (ort & C_ORT_OMP) != 0;

  gcc_assert (i == 0);

  if (!openmp_p
      && map_p
      && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH
	  || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DETACH))
    {
      i += 2;
      *idx = i;
      return c;
    }

  switch (addr_tokens[i + 1]->u.access_kind)
    {
    case ACCESS_DIRECT:
      if (decl_p && !target_p)
	c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
      break;

    case ACCESS_REF:
      {
	/* Copy the referenced object.  Note that we do this even for !MAP_P
	   clauses.  */
	tree obj = convert_from_reference (addr_tokens[i + 1]->expr);
	if (TREE_CODE (TREE_TYPE (obj)) == ARRAY_TYPE)
	  /* We have a ref to array: add a [0] element as the ME expects.  */
	  OMP_CLAUSE_DECL (c) = build_array_ref (loc, obj, integer_zero_node);
	else
	  OMP_CLAUSE_DECL (c) = obj;
	OMP_CLAUSE_SIZE (c) = TYPE_SIZE_UNIT (TREE_TYPE (obj));

	if (!map_p)
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    break;
	  }

	if (!target_p)
	  break;

	/* If we have a reference to a pointer, avoid using
	   FIRSTPRIVATE_REFERENCE here in case the pointer is modified in the
	   offload region (we can only do that if the pointer does not point
	   to a mapped block).  We could avoid doing this if we don't have a
	   FROM mapping...  */
	bool ref_to_ptr = TREE_CODE (TREE_TYPE (obj)) == POINTER_TYPE;

	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	if (!ref_to_ptr
	    && !declare_target_p
	    && decl_p)
	  OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_FIRSTPRIVATE_REFERENCE);
	else
	  {
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	  }
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2) = size_zero_node;

	if (ref_to_ptr)
	  {
	    c3 = c2;
	    c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ALLOC);
	    OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	    OMP_CLAUSE_SIZE (c2)
	      = TYPE_SIZE_UNIT (TREE_TYPE (OMP_CLAUSE_DECL (c2)));
	  }
      }
      break;

    case ACCESS_INDEXED_REF_TO_ARRAY:
      {
	if (!map_p)
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    break;
	  }

	if (!target_p)
	  break;

	tree virtual_origin
	  = convert_from_reference (addr_tokens[i + 1]->expr);
	virtual_origin = build_fold_addr_expr (virtual_origin);
	virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					   virtual_origin);
	tree data_addr = omp_accessed_addr (addr_tokens, i + 1, expr);
	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	if (decl_p && target_p && !declare_target_p)
	  {
	    /* It appears that omp-low.cc mishandles cases where we have a
	       [reference to an] array of pointers such as:

		 int *arr[N];   (or "int *(&arr)[N] = ...")
		 #pragma omp target map(arr[a][b:c])
		 { ... }

	       in such cases chain_p will be true.  For now, fall back to
	       GOMP_MAP_POINTER.  */
	    enum gomp_map_kind k = chain_p ? GOMP_MAP_POINTER
					   : GOMP_MAP_FIRSTPRIVATE_REFERENCE;
	    OMP_CLAUSE_SET_MAP_KIND (c2, k);
	  }
	else
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	  }
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    case ACCESS_INDEXED_ARRAY:
      {
	if (!map_p)
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    break;
	  }

	/* The code handling "firstprivatize_array_bases" in gimplify.cc is
	   relevant here.  What do we need to create for arrays at this
	   stage?  (This condition doesn't feel quite right.  FIXME?)  */
	if (!target_p
	    && (TREE_CODE (TREE_TYPE (addr_tokens[i + 1]->expr))
		== ARRAY_TYPE))
	  break;

	tree virtual_origin
	  = build_fold_addr_expr (addr_tokens[i + 1]->expr);
	virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					   virtual_origin);
	tree data_addr = omp_accessed_addr (addr_tokens, i + 1, expr);
	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	if (decl_p && target_p)
	  {
	    /* See comment for ACCESS_INDEXED_REF_TO_ARRAY above.  */
	    enum gomp_map_kind k = chain_p ? GOMP_MAP_POINTER
					   : GOMP_MAP_FIRSTPRIVATE_POINTER;
	    OMP_CLAUSE_SET_MAP_KIND (c2, k);
	  }
	else
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	  }
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    case ACCESS_POINTER:
    case ACCESS_POINTER_OFFSET:
      {
	if (!map_p)
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    break;
	  }

	unsigned last_access = i + 1;
	tree virtual_origin;

	if (chain_p
	    && addr_tokens[i + 2]->type == ACCESS_METHOD
	    && addr_tokens[i + 2]->u.access_kind == ACCESS_INDEXED_ARRAY)
	  {
	    /* !!! This seems wrong for ACCESS_POINTER_OFFSET.  */
	    consume_tokens = 3;
	    chain_p = omp_access_chain_p (addr_tokens, i + 2);
	    last_access = i + 2;
	    virtual_origin
	      = build_array_ref (loc, addr_tokens[last_access]->expr,
				 integer_zero_node);
	    virtual_origin = build_fold_addr_expr (virtual_origin);
	    virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					       virtual_origin);
	  }
	else
	  virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					     addr_tokens[last_access]->expr);
	tree data_addr = omp_accessed_addr (addr_tokens, last_access, expr);
	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	/* For OpenACC, use FIRSTPRIVATE_POINTER for decls even on non-compute
	   regions (e.g. "acc data" constructs).  It'll be removed anyway in
	   gimplify.cc, but doing it this way maintains diagnostic
	   behaviour.  */
	if (decl_p && (target_p || !openmp_p) && !chain_p && !declare_target_p)
	  OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_FIRSTPRIVATE_POINTER);
	else
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	  }
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    case ACCESS_REF_TO_POINTER:
    case ACCESS_REF_TO_POINTER_OFFSET:
      {
	if (!map_p)
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    break;
	  }

	unsigned last_access = i + 1;
	tree virtual_origin;

	if (chain_p
	    && addr_tokens[i + 2]->type == ACCESS_METHOD
	    && addr_tokens[i + 2]->u.access_kind == ACCESS_INDEXED_ARRAY)
	  {
	    /* !!! This seems wrong for ACCESS_POINTER_OFFSET.  */
	    consume_tokens = 3;
	    chain_p = omp_access_chain_p (addr_tokens, i + 2);
	    last_access = i + 2;
	    virtual_origin
	      = build_array_ref (loc, addr_tokens[last_access]->expr,
				 integer_zero_node);
	    virtual_origin = build_fold_addr_expr (virtual_origin);
	    virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					       virtual_origin);
	  }
	else
	  {
	    virtual_origin
	      = convert_from_reference (addr_tokens[last_access]->expr);
	    virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					       virtual_origin);
	  }

	tree data_addr = omp_accessed_addr (addr_tokens, last_access, expr);
	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	if (decl_p && target_p && !chain_p && !declare_target_p)
	  {
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_FIRSTPRIVATE_REFERENCE);
	    OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	  }
	else
	  {
	    if (decl_p)
	      c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	    OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	    OMP_CLAUSE_DECL (c2)
	      = convert_from_reference (addr_tokens[i + 1]->expr);
	  }
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    default:
      *idx = i + consume_tokens;
      return error_mark_node;
    }

  if (c3)
    {
      OMP_CLAUSE_CHAIN (c3) = OMP_CLAUSE_CHAIN (c);
      OMP_CLAUSE_CHAIN (c2) = c3;
      OMP_CLAUSE_CHAIN (c) = c2;
      if (implicit_p)
	{
	  OMP_CLAUSE_MAP_IMPLICIT (c2) = 1;
	  OMP_CLAUSE_MAP_IMPLICIT (c3) = 1;
	}
      c = c3;
    }
  else if (c2)
    {
      OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (c);
      OMP_CLAUSE_CHAIN (c) = c2;
      if (implicit_p)
	OMP_CLAUSE_MAP_IMPLICIT (c2) = 1;
      c = c2;
    }

  i += consume_tokens;
  *idx = i;

  if (chain_p && map_p)
    return omp_expand_access_chain (c, expr, addr_tokens, idx, ort);

  return c;
}

/* Translate "component_selector access_method" to OMP mapping clauses.  */

tree
c_omp_address_inspector::expand_component_selector (tree c,
						    vec<omp_addr_token *>
						      &addr_tokens,
						    tree expr, unsigned *idx,
						    c_omp_region_type ort)
{
  using namespace omp_addr_tokenizer;
  location_t loc = OMP_CLAUSE_LOCATION (c);
  unsigned i = *idx;
  tree c2 = NULL_TREE, c3 = NULL_TREE;
  bool chain_p = omp_access_chain_p (addr_tokens, i + 1);
  bool map_p = OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP;

  switch (addr_tokens[i + 1]->u.access_kind)
    {
    case ACCESS_DIRECT:
    case ACCESS_INDEXED_ARRAY:
      break;

    case ACCESS_REF:
      {
	/* Copy the referenced object.  Note that we also do this for !MAP_P
	   clauses.  */
	tree obj = convert_from_reference (addr_tokens[i + 1]->expr);
	OMP_CLAUSE_DECL (c) = obj;
	OMP_CLAUSE_SIZE (c) = TYPE_SIZE_UNIT (TREE_TYPE (obj));

	if (!map_p)
	  break;

	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2) = size_zero_node;
      }
      break;

    case ACCESS_INDEXED_REF_TO_ARRAY:
      {
	if (!map_p)
	  break;

	tree virtual_origin
	  = convert_from_reference (addr_tokens[i + 1]->expr);
	virtual_origin = build_fold_addr_expr (virtual_origin);
	virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
					   virtual_origin);
	tree data_addr = omp_accessed_addr (addr_tokens, i + 1, expr);

	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    case ACCESS_POINTER:
    case ACCESS_POINTER_OFFSET:
      {
	if (!map_p)
	  break;

	tree virtual_origin
	  = fold_convert_loc (loc, ptrdiff_type_node,
			      addr_tokens[i + 1]->expr);
	tree data_addr = omp_accessed_addr (addr_tokens, i + 1, expr);

	c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	OMP_CLAUSE_DECL (c2) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);
      }
      break;

    case ACCESS_REF_TO_POINTER:
    case ACCESS_REF_TO_POINTER_OFFSET:
      {
	if (!map_p)
	  break;

	tree ptr = convert_from_reference (addr_tokens[i + 1]->expr);
	tree virtual_origin = fold_convert_loc (loc, ptrdiff_type_node,
						ptr);
	tree data_addr = omp_accessed_addr (addr_tokens, i + 1, expr);

	/* Attach the pointer...  */
	c2 = build_omp_clause (OMP_CLAUSE_LOCATION (c), OMP_CLAUSE_MAP);
	OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_ATTACH_DETACH);
	OMP_CLAUSE_DECL (c2) = ptr;
	OMP_CLAUSE_SIZE (c2)
	  = fold_build2_loc (loc, MINUS_EXPR, ptrdiff_type_node,
			     fold_convert_loc (loc, ptrdiff_type_node,
					       data_addr),
			     virtual_origin);

	/* ...and also the reference.  */
	c3 = build_omp_clause (OMP_CLAUSE_LOCATION (c), OMP_CLAUSE_MAP);
	OMP_CLAUSE_SET_MAP_KIND (c3, GOMP_MAP_ATTACH_DETACH);
	OMP_CLAUSE_DECL (c3) = addr_tokens[i + 1]->expr;
	OMP_CLAUSE_SIZE (c3) = size_zero_node;
      }
      break;

    default:
      *idx = i + 2;
      return error_mark_node;
    }

  if (c3)
    {
      OMP_CLAUSE_CHAIN (c3) = OMP_CLAUSE_CHAIN (c);
      OMP_CLAUSE_CHAIN (c2) = c3;
      OMP_CLAUSE_CHAIN (c) = c2;
      c = c3;
    }
  else if (c2)
    {
      OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (c);
      OMP_CLAUSE_CHAIN (c) = c2;
      c = c2;
    }

  i += 2;
  *idx = i;

  if (chain_p && map_p)
    return omp_expand_access_chain (c, expr, addr_tokens, idx, ort);

  return c;
}

/* Expand a map clause into a group of mapping clauses, creating nodes to
   attach/detach pointers and so forth as necessary.  */

tree
c_omp_address_inspector::expand_map_clause (tree c, tree expr,
					    vec<omp_addr_token *> &addr_tokens,
					    c_omp_region_type ort)
{
  using namespace omp_addr_tokenizer;
  unsigned i, length = addr_tokens.length ();

  for (i = 0; i < length;)
    {
      int remaining = length - i;

      if (remaining >= 2
	  && addr_tokens[i]->type == ARRAY_BASE
	  && addr_tokens[i]->u.structure_base_kind == BASE_DECL
	  && addr_tokens[i + 1]->type == ACCESS_METHOD)
	{
	  c = expand_array_base (c, addr_tokens, expr, &i, ort);
	  if (c == error_mark_node)
	    return error_mark_node;
	}
      else if (remaining >= 2
	       && addr_tokens[i]->type == ARRAY_BASE
	       && addr_tokens[i]->u.structure_base_kind == BASE_ARBITRARY_EXPR
	       && addr_tokens[i + 1]->type == ACCESS_METHOD)
	{
	  c = expand_array_base (c, addr_tokens, expr, &i, ort);
	  if (c == error_mark_node)
	    return error_mark_node;
	}
      else if (remaining >= 2
	       && addr_tokens[i]->type == STRUCTURE_BASE
	       && addr_tokens[i]->u.structure_base_kind == BASE_DECL
	       && addr_tokens[i + 1]->type == ACCESS_METHOD)
	{
	  if (addr_tokens[i + 1]->u.access_kind == ACCESS_DIRECT)
	    c_common_mark_addressable_vec (addr_tokens[i + 1]->expr);
	  i += 2;
	  while (addr_tokens[i]->type == ACCESS_METHOD)
	    i++;
	}
      else if (remaining >= 2
	       && addr_tokens[i]->type == STRUCTURE_BASE
	       && addr_tokens[i]->u.structure_base_kind == BASE_ARBITRARY_EXPR
	       && addr_tokens[i + 1]->type == ACCESS_METHOD)
	{
	  switch (addr_tokens[i + 1]->u.access_kind)
	    {
	    case ACCESS_DIRECT:
	    case ACCESS_POINTER:
	      i += 2;
	      while (addr_tokens[i]->type == ACCESS_METHOD)
		i++;
	      break;
	    default:
	      return error_mark_node;
	    }
	}
      else if (remaining >= 2
	       && addr_tokens[i]->type == COMPONENT_SELECTOR
	       && addr_tokens[i + 1]->type == ACCESS_METHOD)
	{
	  c = expand_component_selector (c, addr_tokens, expr, &i, ort);
	  /* We used 'expr', so these must have been the last tokens.  */
	  gcc_assert (i == length);
	  if (c == error_mark_node)
	    return error_mark_node;
	}
      else if (remaining >= 3
	       && addr_tokens[i]->type == COMPONENT_SELECTOR
	       && addr_tokens[i + 1]->type == STRUCTURE_BASE
	       && (addr_tokens[i + 1]->u.structure_base_kind
		   == BASE_COMPONENT_EXPR)
	       && addr_tokens[i + 2]->type == ACCESS_METHOD)
	{
	  i += 3;
	  while (addr_tokens[i]->type == ACCESS_METHOD)
	    i++;
	}
      else
	break;
    }

  if (i == length)
    return c;

  return error_mark_node;
}

const struct c_omp_directive c_omp_directives[] = {
  /* Keep this alphabetically sorted by the first word.  Non-null second/third
     if any should precede null ones.  */
  { "allocate", nullptr, nullptr, PRAGMA_OMP_ALLOCATE,
    C_OMP_DIR_DECLARATIVE, false },
  { "assume", nullptr, nullptr, PRAGMA_OMP_ASSUME,
    C_OMP_DIR_INFORMATIONAL, false },
  { "assumes", nullptr, nullptr, PRAGMA_OMP_ASSUMES,
    C_OMP_DIR_INFORMATIONAL, false },
  { "atomic", nullptr, nullptr, PRAGMA_OMP_ATOMIC,
    C_OMP_DIR_CONSTRUCT, false },
  { "barrier", nullptr, nullptr, PRAGMA_OMP_BARRIER,
    C_OMP_DIR_STANDALONE, false },
  { "begin", "assumes", nullptr, PRAGMA_OMP_BEGIN,
    C_OMP_DIR_INFORMATIONAL, false },
  { "begin", "declare", "target", PRAGMA_OMP_BEGIN,
    C_OMP_DIR_DECLARATIVE, false },
  /* { "begin", "declare", "variant", PRAGMA_OMP_BEGIN,
    C_OMP_DIR_DECLARATIVE, false }, */
  /* { "begin", "metadirective", nullptr, PRAGMA_OMP_BEGIN,
    C_OMP_DIR_???, ??? },  */
  { "cancel", nullptr, nullptr, PRAGMA_OMP_CANCEL,
    C_OMP_DIR_STANDALONE, false },
  { "cancellation", "point", nullptr, PRAGMA_OMP_CANCELLATION_POINT,
    C_OMP_DIR_STANDALONE, false },
  { "critical", nullptr, nullptr, PRAGMA_OMP_CRITICAL,
    C_OMP_DIR_CONSTRUCT, false },
  /* { "declare", "mapper", nullptr, PRAGMA_OMP_DECLARE,
    C_OMP_DIR_DECLARATIVE, false },  */
  { "declare", "reduction", nullptr, PRAGMA_OMP_DECLARE,
    C_OMP_DIR_DECLARATIVE, true },
  { "declare", "simd", nullptr, PRAGMA_OMP_DECLARE,
    C_OMP_DIR_DECLARATIVE, true },
  { "declare", "target", nullptr, PRAGMA_OMP_DECLARE,
    C_OMP_DIR_DECLARATIVE, false },
  { "declare", "variant", nullptr, PRAGMA_OMP_DECLARE,
    C_OMP_DIR_DECLARATIVE, false },
  { "depobj", nullptr, nullptr, PRAGMA_OMP_DEPOBJ,
    C_OMP_DIR_STANDALONE, false },
  { "dispatch", nullptr, nullptr, PRAGMA_OMP_DISPATCH,
    C_OMP_DIR_DECLARATIVE, false },
  { "distribute", nullptr, nullptr, PRAGMA_OMP_DISTRIBUTE,
    C_OMP_DIR_CONSTRUCT, true },
  { "end", "assumes", nullptr, PRAGMA_OMP_END,
    C_OMP_DIR_INFORMATIONAL, false },
  { "end", "declare", "target", PRAGMA_OMP_END,
    C_OMP_DIR_DECLARATIVE, false },
  /* { "end", "declare", "variant", PRAGMA_OMP_END,
    C_OMP_DIR_DECLARATIVE, false }, */
  /* { "end", "metadirective", nullptr, PRAGMA_OMP_END,
    C_OMP_DIR_???, ??? },  */
  /* error with at(execution) is C_OMP_DIR_STANDALONE.  */
  { "error", nullptr, nullptr, PRAGMA_OMP_ERROR,
    C_OMP_DIR_UTILITY, false },
  { "flush", nullptr, nullptr, PRAGMA_OMP_FLUSH,
    C_OMP_DIR_STANDALONE, false },
  { "for", nullptr, nullptr, PRAGMA_OMP_FOR,
    C_OMP_DIR_CONSTRUCT, true },
  /* { "groupprivate", nullptr, nullptr, PRAGMA_OMP_GROUPPRIVATE,
    C_OMP_DIR_DECLARATIVE, false },  */
  /* { "interop", nullptr, nullptr, PRAGMA_OMP_INTEROP,
    C_OMP_DIR_STANDALONE, false },  */
  { "loop", nullptr, nullptr, PRAGMA_OMP_LOOP,
    C_OMP_DIR_CONSTRUCT, true },
  { "masked", nullptr, nullptr, PRAGMA_OMP_MASKED,
    C_OMP_DIR_CONSTRUCT, true },
  { "master", nullptr, nullptr, PRAGMA_OMP_MASTER,
    C_OMP_DIR_CONSTRUCT, true },
  /* { "metadirective", nullptr, nullptr, PRAGMA_OMP_METADIRECTIVE,
    C_OMP_DIR_???, ??? },  */
  { "nothing", nullptr, nullptr, PRAGMA_OMP_NOTHING,
    C_OMP_DIR_UTILITY, false },
  /* ordered with depend clause is C_OMP_DIR_STANDALONE.  */
  { "ordered", nullptr, nullptr, PRAGMA_OMP_ORDERED,
    C_OMP_DIR_CONSTRUCT, true },
  { "parallel", nullptr, nullptr, PRAGMA_OMP_PARALLEL,
    C_OMP_DIR_CONSTRUCT, true },
  { "requires", nullptr, nullptr, PRAGMA_OMP_REQUIRES,
    C_OMP_DIR_INFORMATIONAL, false },
  { "scan", nullptr, nullptr, PRAGMA_OMP_SCAN,
    C_OMP_DIR_CONSTRUCT, true },
  { "scope", nullptr, nullptr, PRAGMA_OMP_SCOPE,
    C_OMP_DIR_CONSTRUCT, false },
  { "section", nullptr, nullptr, PRAGMA_OMP_SECTION,
    C_OMP_DIR_CONSTRUCT, false },
  { "sections", nullptr, nullptr, PRAGMA_OMP_SECTIONS,
    C_OMP_DIR_CONSTRUCT, false },
  { "simd", nullptr, nullptr, PRAGMA_OMP_SIMD,
    C_OMP_DIR_CONSTRUCT, true },
  { "single", nullptr, nullptr, PRAGMA_OMP_SINGLE,
    C_OMP_DIR_CONSTRUCT, false },
  { "target", "data", nullptr, PRAGMA_OMP_TARGET,
    C_OMP_DIR_CONSTRUCT, false },
  { "target", "enter", "data", PRAGMA_OMP_TARGET,
    C_OMP_DIR_STANDALONE, false },
  { "target", "exit", "data", PRAGMA_OMP_TARGET,
    C_OMP_DIR_STANDALONE, false },
  { "target", "update", nullptr, PRAGMA_OMP_TARGET,
    C_OMP_DIR_STANDALONE, false },
  { "target", nullptr, nullptr, PRAGMA_OMP_TARGET,
    C_OMP_DIR_CONSTRUCT, true },
  { "task", nullptr, nullptr, PRAGMA_OMP_TASK,
    C_OMP_DIR_CONSTRUCT, false },
  { "taskgroup", nullptr, nullptr, PRAGMA_OMP_TASKGROUP,
    C_OMP_DIR_CONSTRUCT, false },
  { "taskloop", nullptr, nullptr, PRAGMA_OMP_TASKLOOP,
    C_OMP_DIR_CONSTRUCT, true },
  { "taskwait", nullptr, nullptr, PRAGMA_OMP_TASKWAIT,
    C_OMP_DIR_STANDALONE, false },
  { "taskyield", nullptr, nullptr, PRAGMA_OMP_TASKYIELD,
    C_OMP_DIR_STANDALONE, false },
  { "tile", nullptr, nullptr, PRAGMA_OMP_TILE,
    C_OMP_DIR_CONSTRUCT, false },
  { "teams", nullptr, nullptr, PRAGMA_OMP_TEAMS,
    C_OMP_DIR_CONSTRUCT, true },
  { "threadprivate", nullptr, nullptr, PRAGMA_OMP_THREADPRIVATE,
    C_OMP_DIR_DECLARATIVE, false },
  { "unroll", nullptr, nullptr, PRAGMA_OMP_UNROLL,
    C_OMP_DIR_CONSTRUCT, false },
};

/* Find (non-combined/composite) OpenMP directive (if any) which starts
   with FIRST keyword and for multi-word directives has SECOND and
   THIRD keyword after it.  */

const struct c_omp_directive *
c_omp_categorize_directive (const char *first, const char *second,
			    const char *third)
{
  const size_t n_omp_directives = ARRAY_SIZE (c_omp_directives);
  for (size_t i = 0; i < n_omp_directives; i++)
    {
      if ((unsigned char) c_omp_directives[i].first[0]
	  < (unsigned char) first[0])
	continue;
      if ((unsigned char) c_omp_directives[i].first[0]
	  > (unsigned char) first[0])
	break;
      if (strcmp (c_omp_directives[i].first, first))
	continue;
      if (!c_omp_directives[i].second)
	return &c_omp_directives[i];
      if (!second || strcmp (c_omp_directives[i].second, second))
	continue;
      if (!c_omp_directives[i].third)
	return &c_omp_directives[i];
      if (!third || strcmp (c_omp_directives[i].third, third))
	continue;
      return &c_omp_directives[i];
    }
  return NULL;
}
