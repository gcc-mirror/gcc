/* Statement simplification on GIMPLE.
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.
   Split out from tree-ssa-ccp.c.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "flags.h"
#include "function.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "target.h"
#include "gimple-fold.h"

/* Return true when DECL can be referenced from current unit.
   We can get declarations that are not possible to reference for
   various reasons:

     1) When analyzing C++ virtual tables.
	C++ virtual tables do have known constructors even
	when they are keyed to other compilation unit.
	Those tables can contain pointers to methods and vars
	in other units.  Those methods have both STATIC and EXTERNAL
	set.
     2) In WHOPR mode devirtualization might lead to reference
	to method that was partitioned elsehwere.
	In this case we have static VAR_DECL or FUNCTION_DECL
	that has no corresponding callgraph/varpool node
	declaring the body.  
     3) COMDAT functions referred by external vtables that
        we devirtualize only during final copmilation stage.
        At this time we already decided that we will not output
        the function body and thus we can't reference the symbol
        directly.  */

static bool
can_refer_decl_in_current_unit_p (tree decl)
{
  struct varpool_node *vnode;
  struct cgraph_node *node;

  if (!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
    return true;
  /* External flag is set, so we deal with C++ reference
     to static object from other file.  */
  if (DECL_EXTERNAL (decl) && TREE_STATIC (decl)
      && TREE_CODE (decl) == VAR_DECL)
    {
      /* Just be sure it is not big in frontend setting
	 flags incorrectly.  Those variables should never
	 be finalized.  */
      gcc_checking_assert (!(vnode = varpool_get_node (decl))
			   || !vnode->finalized);
      return false;
    }
  /* When function is public, we always can introduce new reference.
     Exception are the COMDAT functions where introducing a direct
     reference imply need to include function body in the curren tunit.  */
  if (TREE_PUBLIC (decl) && !DECL_COMDAT (decl))
    return true;
  /* We are not at ltrans stage; so don't worry about WHOPR.
     Also when still gimplifying all referred comdat functions will be
     produced.
     ??? as observed in PR20991 for already optimized out comdat virtual functions
     we may not neccesarily give up because the copy will be output elsewhere when
     corresponding vtable is output.  */
  if (!flag_ltrans && (!DECL_COMDAT (decl) || !cgraph_function_flags_ready))
    return true;
  /* If we already output the function body, we are safe.  */
  if (TREE_ASM_WRITTEN (decl))
    return true;
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      node = cgraph_get_node (decl);
      /* Check that we still have function body and that we didn't took
         the decision to eliminate offline copy of the function yet.
         The second is important when devirtualization happens during final
         compilation stage when making a new reference no longer makes callee
         to be compiled.  */
      if (!node || !node->analyzed || node->global.inlined_to)
	return false;
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      vnode = varpool_get_node (decl);
      if (!vnode || !vnode->finalized)
	return false;
    }
  return true;
}

/* CVAL is value taken from DECL_INITIAL of variable.  Try to transform it into
   acceptable form for is_gimple_min_invariant.   */

tree
canonicalize_constructor_val (tree cval)
{
  STRIP_NOPS (cval);
  if (TREE_CODE (cval) == POINTER_PLUS_EXPR
      && TREE_CODE (TREE_OPERAND (cval, 1)) == INTEGER_CST)
    {
      tree ptr = TREE_OPERAND (cval, 0);
      if (is_gimple_min_invariant (ptr))
	cval = build1_loc (EXPR_LOCATION (cval),
			   ADDR_EXPR, TREE_TYPE (ptr),
			   fold_build2 (MEM_REF, TREE_TYPE (TREE_TYPE (ptr)),
					ptr,
					fold_convert (ptr_type_node,
						      TREE_OPERAND (cval, 1))));
    }
  if (TREE_CODE (cval) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (cval, 0));

      if (base
	  && (TREE_CODE (base) == VAR_DECL
	      || TREE_CODE (base) == FUNCTION_DECL)
	  && !can_refer_decl_in_current_unit_p (base))
	return NULL_TREE;
      if (base && TREE_CODE (base) == VAR_DECL)
	{
	  TREE_ADDRESSABLE (base) = 1;
	  if (cfun && gimple_referenced_vars (cfun))
	    add_referenced_var (base);
	}
      /* Fixup types in global initializers.  */
      if (TREE_TYPE (TREE_TYPE (cval)) != TREE_TYPE (TREE_OPERAND (cval, 0)))
	cval = build_fold_addr_expr (TREE_OPERAND (cval, 0));
    }
  return cval;
}

/* If SYM is a constant variable with known value, return the value.
   NULL_TREE is returned otherwise.  */

tree
get_symbol_constant_value (tree sym)
{
  if (const_value_known_p (sym))
    {
      tree val = DECL_INITIAL (sym);
      if (val)
	{
	  val = canonicalize_constructor_val (val);
	  if (val && is_gimple_min_invariant (val))
	    return val;
	  else
	    return NULL_TREE;
	}
      /* Variables declared 'const' without an initializer
	 have zero as the initializer if they may not be
	 overridden at link or run time.  */
      if (!val
          && (INTEGRAL_TYPE_P (TREE_TYPE (sym))
	       || SCALAR_FLOAT_TYPE_P (TREE_TYPE (sym))))
	return build_zero_cst (TREE_TYPE (sym));
    }

  return NULL_TREE;
}



/* Subroutine of fold_stmt.  We perform several simplifications of the
   memory reference tree EXPR and make sure to re-gimplify them properly
   after propagation of constant addresses.  IS_LHS is true if the
   reference is supposed to be an lvalue.  */

static tree
maybe_fold_reference (tree expr, bool is_lhs)
{
  tree *t = &expr;
  tree result;

  if ((TREE_CODE (expr) == VIEW_CONVERT_EXPR
       || TREE_CODE (expr) == REALPART_EXPR
       || TREE_CODE (expr) == IMAGPART_EXPR)
      && CONSTANT_CLASS_P (TREE_OPERAND (expr, 0)))
    return fold_unary_loc (EXPR_LOCATION (expr),
			   TREE_CODE (expr),
			   TREE_TYPE (expr),
			   TREE_OPERAND (expr, 0));
  else if (TREE_CODE (expr) == BIT_FIELD_REF
	   && CONSTANT_CLASS_P (TREE_OPERAND (expr, 0)))
    return fold_ternary_loc (EXPR_LOCATION (expr),
			     TREE_CODE (expr),
			     TREE_TYPE (expr),
			     TREE_OPERAND (expr, 0),
			     TREE_OPERAND (expr, 1),
			     TREE_OPERAND (expr, 2));

  while (handled_component_p (*t))
    t = &TREE_OPERAND (*t, 0);

  /* Canonicalize MEM_REFs invariant address operand.  Do this first
     to avoid feeding non-canonical MEM_REFs elsewhere.  */
  if (TREE_CODE (*t) == MEM_REF
      && !is_gimple_mem_ref_addr (TREE_OPERAND (*t, 0)))
    {
      bool volatile_p = TREE_THIS_VOLATILE (*t);
      tree tem = fold_binary (MEM_REF, TREE_TYPE (*t),
			      TREE_OPERAND (*t, 0),
			      TREE_OPERAND (*t, 1));
      if (tem)
	{
	  TREE_THIS_VOLATILE (tem) = volatile_p;
	  *t = tem;
	  tem = maybe_fold_reference (expr, is_lhs);
	  if (tem)
	    return tem;
	  return expr;
	}
    }

  if (!is_lhs
      && (result = fold_const_aggregate_ref (expr))
      && is_gimple_min_invariant (result))
    return result;

  /* Fold back MEM_REFs to reference trees.  */
  if (TREE_CODE (*t) == MEM_REF
      && TREE_CODE (TREE_OPERAND (*t, 0)) == ADDR_EXPR
      && integer_zerop (TREE_OPERAND (*t, 1))
      && (TREE_THIS_VOLATILE (*t)
	  == TREE_THIS_VOLATILE (TREE_OPERAND (TREE_OPERAND (*t, 0), 0)))
      && !TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (*t, 1)))
      && (TYPE_MAIN_VARIANT (TREE_TYPE (*t))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (TREE_OPERAND (*t, 1)))))
      /* We have to look out here to not drop a required conversion
	 from the rhs to the lhs if is_lhs, but we don't have the
	 rhs here to verify that.  Thus require strict type
	 compatibility.  */
      && types_compatible_p (TREE_TYPE (*t),
			     TREE_TYPE (TREE_OPERAND
					(TREE_OPERAND (*t, 0), 0))))
    {
      tree tem;
      *t = TREE_OPERAND (TREE_OPERAND (*t, 0), 0);
      tem = maybe_fold_reference (expr, is_lhs);
      if (tem)
	return tem;
      return expr;
    }
  else if (TREE_CODE (*t) == TARGET_MEM_REF)
    {
      tree tem = maybe_fold_tmr (*t);
      if (tem)
	{
	  *t = tem;
	  tem = maybe_fold_reference (expr, is_lhs);
	  if (tem)
	    return tem;
	  return expr;
	}
    }

  return NULL_TREE;
}


/* Attempt to fold an assignment statement pointed-to by SI.  Returns a
   replacement rhs for the statement or NULL_TREE if no simplification
   could be made.  It is assumed that the operands have been previously
   folded.  */

static tree
fold_gimple_assign (gimple_stmt_iterator *si)
{
  gimple stmt = gsi_stmt (*si);
  enum tree_code subcode = gimple_assign_rhs_code (stmt);
  location_t loc = gimple_location (stmt);

  tree result = NULL_TREE;

  switch (get_gimple_rhs_class (subcode))
    {
    case GIMPLE_SINGLE_RHS:
      {
        tree rhs = gimple_assign_rhs1 (stmt);

	if (REFERENCE_CLASS_P (rhs))
	  return maybe_fold_reference (rhs, false);

	else if (TREE_CODE (rhs) == ADDR_EXPR)
	  {
	    tree ref = TREE_OPERAND (rhs, 0);
	    tree tem = maybe_fold_reference (ref, true);
	    if (tem
		&& TREE_CODE (tem) == MEM_REF
		&& integer_zerop (TREE_OPERAND (tem, 1)))
	      result = fold_convert (TREE_TYPE (rhs), TREE_OPERAND (tem, 0));
	    else if (tem)
	      result = fold_convert (TREE_TYPE (rhs),
				     build_fold_addr_expr_loc (loc, tem));
	    else if (TREE_CODE (ref) == MEM_REF
		     && integer_zerop (TREE_OPERAND (ref, 1)))
	      result = fold_convert (TREE_TYPE (rhs), TREE_OPERAND (ref, 0));
	  }

	else if (TREE_CODE (rhs) == CONSTRUCTOR
		 && TREE_CODE (TREE_TYPE (rhs)) == VECTOR_TYPE
		 && (CONSTRUCTOR_NELTS (rhs)
		     == TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs))))
	  {
	    /* Fold a constant vector CONSTRUCTOR to VECTOR_CST.  */
	    unsigned i;
	    tree val;

	    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), i, val)
	      if (TREE_CODE (val) != INTEGER_CST
		  && TREE_CODE (val) != REAL_CST
		  && TREE_CODE (val) != FIXED_CST)
		return NULL_TREE;

	    return build_vector_from_ctor (TREE_TYPE (rhs),
					   CONSTRUCTOR_ELTS (rhs));
	  }

	else if (DECL_P (rhs))
	  return unshare_expr (get_symbol_constant_value (rhs));

        /* If we couldn't fold the RHS, hand over to the generic
           fold routines.  */
        if (result == NULL_TREE)
          result = fold (rhs);

        /* Strip away useless type conversions.  Both the NON_LVALUE_EXPR
           that may have been added by fold, and "useless" type
           conversions that might now be apparent due to propagation.  */
        STRIP_USELESS_TYPE_CONVERSION (result);

        if (result != rhs && valid_gimple_rhs_p (result))
	  return result;

	return NULL_TREE;
      }
      break;

    case GIMPLE_UNARY_RHS:
      {
	tree rhs = gimple_assign_rhs1 (stmt);

	result = fold_unary_loc (loc, subcode, gimple_expr_type (stmt), rhs);
	if (result)
	  {
	    /* If the operation was a conversion do _not_ mark a
	       resulting constant with TREE_OVERFLOW if the original
	       constant was not.  These conversions have implementation
	       defined behavior and retaining the TREE_OVERFLOW flag
	       here would confuse later passes such as VRP.  */
	    if (CONVERT_EXPR_CODE_P (subcode)
		&& TREE_CODE (result) == INTEGER_CST
		&& TREE_CODE (rhs) == INTEGER_CST)
	      TREE_OVERFLOW (result) = TREE_OVERFLOW (rhs);

	    STRIP_USELESS_TYPE_CONVERSION (result);
	    if (valid_gimple_rhs_p (result))
	      return result;
	  }
      }
      break;

    case GIMPLE_BINARY_RHS:
      /* Try to canonicalize for boolean-typed X the comparisons
	 X == 0, X == 1, X != 0, and X != 1.  */
      if (gimple_assign_rhs_code (stmt) == EQ_EXPR
	  || gimple_assign_rhs_code (stmt) == NE_EXPR)
        {
	  tree lhs = gimple_assign_lhs (stmt);
	  tree op1 = gimple_assign_rhs1 (stmt);
	  tree op2 = gimple_assign_rhs2 (stmt);
	  tree type = TREE_TYPE (op1);

	  /* Check whether the comparison operands are of the same boolean
	     type as the result type is.
	     Check that second operand is an integer-constant with value
	     one or zero.  */
	  if (TREE_CODE (op2) == INTEGER_CST
	      && (integer_zerop (op2) || integer_onep (op2))
	      && useless_type_conversion_p (TREE_TYPE (lhs), type))
	    {
	      enum tree_code cmp_code = gimple_assign_rhs_code (stmt);
	      bool is_logical_not = false;

	      /* X == 0 and X != 1 is a logical-not.of X
	         X == 1 and X != 0 is X  */
	      if ((cmp_code == EQ_EXPR && integer_zerop (op2))
	          || (cmp_code == NE_EXPR && integer_onep (op2)))
	        is_logical_not = true;

	      if (is_logical_not == false)
	        result = op1;
	      /* Only for one-bit precision typed X the transformation
	         !X -> ~X is valied.  */
	      else if (TYPE_PRECISION (type) == 1)
		result = build1_loc (gimple_location (stmt), BIT_NOT_EXPR,
				     type, op1);
	      /* Otherwise we use !X -> X ^ 1.  */
	      else
	        result = build2_loc (gimple_location (stmt), BIT_XOR_EXPR,
				     type, op1, build_int_cst (type, 1));
	     
	    }
	}

      if (!result)
        result = fold_binary_loc (loc, subcode,
				  TREE_TYPE (gimple_assign_lhs (stmt)),
				  gimple_assign_rhs1 (stmt),
				  gimple_assign_rhs2 (stmt));

      if (result)
        {
          STRIP_USELESS_TYPE_CONVERSION (result);
          if (valid_gimple_rhs_p (result))
	    return result;
        }
      break;

    case GIMPLE_TERNARY_RHS:
      /* Try to fold a conditional expression.  */
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	{
	  tree op0 = gimple_assign_rhs1 (stmt);
	  tree tem;
	  bool set = false;
	  location_t cond_loc = gimple_location (stmt);

	  if (COMPARISON_CLASS_P (op0))
	    {
	      fold_defer_overflow_warnings ();
	      tem = fold_binary_loc (cond_loc,
				     TREE_CODE (op0), TREE_TYPE (op0),
				     TREE_OPERAND (op0, 0),
				     TREE_OPERAND (op0, 1));
	      /* This is actually a conditional expression, not a GIMPLE
		 conditional statement, however, the valid_gimple_rhs_p
		 test still applies.  */
	      set = (tem && is_gimple_condexpr (tem)
		     && valid_gimple_rhs_p (tem));
	      fold_undefer_overflow_warnings (set, stmt, 0);
	    }
	  else if (is_gimple_min_invariant (op0))
	    {
	      tem = op0;
	      set = true;
	    }
	  else
	    return NULL_TREE;

	  if (set)
	    result = fold_build3_loc (cond_loc, COND_EXPR,
				      TREE_TYPE (gimple_assign_lhs (stmt)), tem,
				      gimple_assign_rhs2 (stmt),
				      gimple_assign_rhs3 (stmt));
	}

      if (!result)
	result = fold_ternary_loc (loc, subcode,
				   TREE_TYPE (gimple_assign_lhs (stmt)),
				   gimple_assign_rhs1 (stmt),
				   gimple_assign_rhs2 (stmt),
				   gimple_assign_rhs3 (stmt));

      if (result)
        {
          STRIP_USELESS_TYPE_CONVERSION (result);
          if (valid_gimple_rhs_p (result))
	    return result;
        }
      break;

    case GIMPLE_INVALID_RHS:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Attempt to fold a conditional statement. Return true if any changes were
   made. We only attempt to fold the condition expression, and do not perform
   any transformation that would require alteration of the cfg.  It is
   assumed that the operands have been previously folded.  */

static bool
fold_gimple_cond (gimple stmt)
{
  tree result = fold_binary_loc (gimple_location (stmt),
			     gimple_cond_code (stmt),
                             boolean_type_node,
                             gimple_cond_lhs (stmt),
                             gimple_cond_rhs (stmt));

  if (result)
    {
      STRIP_USELESS_TYPE_CONVERSION (result);
      if (is_gimple_condexpr (result) && valid_gimple_rhs_p (result))
        {
          gimple_cond_set_condition_from_tree (stmt, result);
          return true;
        }
    }

  return false;
}

/* Convert EXPR into a GIMPLE value suitable for substitution on the
   RHS of an assignment.  Insert the necessary statements before
   iterator *SI_P.  The statement at *SI_P, which must be a GIMPLE_CALL
   is replaced.  If the call is expected to produces a result, then it
   is replaced by an assignment of the new RHS to the result variable.
   If the result is to be ignored, then the call is replaced by a
   GIMPLE_NOP.  A proper VDEF chain is retained by making the first
   VUSE and the last VDEF of the whole sequence be the same as the replaced
   statement and using new SSA names for stores in between.  */

void
gimplify_and_update_call_from_tree (gimple_stmt_iterator *si_p, tree expr)
{
  tree lhs;
  gimple stmt, new_stmt;
  gimple_stmt_iterator i;
  gimple_seq stmts = gimple_seq_alloc();
  struct gimplify_ctx gctx;
  gimple last;
  gimple laststore;
  tree reaching_vuse;

  stmt = gsi_stmt (*si_p);

  gcc_assert (is_gimple_call (stmt));

  push_gimplify_context (&gctx);
  gctx.into_ssa = gimple_in_ssa_p (cfun);

  lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    {
      gimplify_and_add (expr, &stmts);
      /* We can end up with folding a memcpy of an empty class assignment
	 which gets optimized away by C++ gimplification.  */
      if (gimple_seq_empty_p (stmts))
	{
	  pop_gimplify_context (NULL);
	  if (gimple_in_ssa_p (cfun))
	    {
	      unlink_stmt_vdef (stmt);
	      release_defs (stmt);
	    }
	  gsi_remove (si_p, true);
	  return;
	}
    }
  else
    {
      tree tmp = get_initialized_tmp_var (expr, &stmts, NULL);
      new_stmt = gimple_build_assign (lhs, tmp);
      i = gsi_last (stmts);
      gsi_insert_after_without_update (&i, new_stmt,
				       GSI_CONTINUE_LINKING);
    }

  pop_gimplify_context (NULL);

  if (gimple_has_location (stmt))
    annotate_all_with_location (stmts, gimple_location (stmt));

  /* First iterate over the replacement statements backward, assigning
     virtual operands to their defining statements.  */
  laststore = NULL;
  for (i = gsi_last (stmts); !gsi_end_p (i); gsi_prev (&i))
    {
      new_stmt = gsi_stmt (i);
      if (gimple_assign_single_p (new_stmt)
	  && !is_gimple_reg (gimple_assign_lhs (new_stmt)))
	{
	  tree vdef;
	  if (!laststore)
	    vdef = gimple_vdef (stmt);
	  else
	    vdef = make_ssa_name (gimple_vop (cfun), new_stmt);
	  gimple_set_vdef (new_stmt, vdef);
	  if (vdef && TREE_CODE (vdef) == SSA_NAME)
	    SSA_NAME_DEF_STMT (vdef) = new_stmt;
	  laststore = new_stmt;
	}
    }

  /* Second iterate over the statements forward, assigning virtual
     operands to their uses.  */
  last = NULL;
  reaching_vuse = gimple_vuse (stmt);
  for (i = gsi_start (stmts); !gsi_end_p (i); gsi_next (&i))
    {
      /* Do not insert the last stmt in this loop but remember it
         for replacing the original statement.  */
      if (last)
	{
	  gsi_insert_before (si_p, last, GSI_NEW_STMT);
	  gsi_next (si_p);
	}
      new_stmt = gsi_stmt (i);
      /* The replacement can expose previously unreferenced variables.  */
      if (gimple_in_ssa_p (cfun))
	find_new_referenced_vars (new_stmt);
      /* If the new statement possibly has a VUSE, update it with exact SSA
	 name we know will reach this one.  */
      if (gimple_has_mem_ops (new_stmt))
	gimple_set_vuse (new_stmt, reaching_vuse);
      gimple_set_modified (new_stmt, true);
      if (gimple_vdef (new_stmt))
	reaching_vuse = gimple_vdef (new_stmt);
      last = new_stmt;
    }

  /* If the new sequence does not do a store release the virtual
     definition of the original statement.  */
  if (reaching_vuse
      && reaching_vuse == gimple_vuse (stmt))
    {
      tree vdef = gimple_vdef (stmt);
      if (vdef
	  && TREE_CODE (vdef) == SSA_NAME)
	{
	  unlink_stmt_vdef (stmt);
	  release_ssa_name (vdef);
	}
    }

  /* Finally replace rhe original statement with the last.  */
  gsi_replace (si_p, last, false);
}

/* Return the string length, maximum string length or maximum value of
   ARG in LENGTH.
   If ARG is an SSA name variable, follow its use-def chains.  If LENGTH
   is not NULL and, for TYPE == 0, its value is not equal to the length
   we determine or if we are unable to determine the length or value,
   return false.  VISITED is a bitmap of visited variables.
   TYPE is 0 if string length should be returned, 1 for maximum string
   length and 2 for maximum value ARG can have.  */

static bool
get_maxval_strlen (tree arg, tree *length, bitmap visited, int type)
{
  tree var, val;
  gimple def_stmt;

  if (TREE_CODE (arg) != SSA_NAME)
    {
      if (TREE_CODE (arg) == COND_EXPR)
        return get_maxval_strlen (COND_EXPR_THEN (arg), length, visited, type)
               && get_maxval_strlen (COND_EXPR_ELSE (arg), length, visited, type);
      /* We can end up with &(*iftmp_1)[0] here as well, so handle it.  */
      else if (TREE_CODE (arg) == ADDR_EXPR
	       && TREE_CODE (TREE_OPERAND (arg, 0)) == ARRAY_REF
	       && integer_zerop (TREE_OPERAND (TREE_OPERAND (arg, 0), 1)))
	{
	  tree aop0 = TREE_OPERAND (TREE_OPERAND (arg, 0), 0);
	  if (TREE_CODE (aop0) == INDIRECT_REF
	      && TREE_CODE (TREE_OPERAND (aop0, 0)) == SSA_NAME)
	    return get_maxval_strlen (TREE_OPERAND (aop0, 0),
				      length, visited, type);
	}

      if (type == 2)
	{
	  val = arg;
	  if (TREE_CODE (val) != INTEGER_CST
	      || tree_int_cst_sgn (val) < 0)
	    return false;
	}
      else
	val = c_strlen (arg, 1);
      if (!val)
	return false;

      if (*length)
	{
	  if (type > 0)
	    {
	      if (TREE_CODE (*length) != INTEGER_CST
		  || TREE_CODE (val) != INTEGER_CST)
		return false;

	      if (tree_int_cst_lt (*length, val))
		*length = val;
	      return true;
	    }
	  else if (simple_cst_equal (val, *length) != 1)
	    return false;
	}

      *length = val;
      return true;
    }

  /* If we were already here, break the infinite cycle.  */
  if (!bitmap_set_bit (visited, SSA_NAME_VERSION (arg)))
    return true;

  var = arg;
  def_stmt = SSA_NAME_DEF_STMT (var);

  switch (gimple_code (def_stmt))
    {
      case GIMPLE_ASSIGN:
        /* The RHS of the statement defining VAR must either have a
           constant length or come from another SSA_NAME with a constant
           length.  */
        if (gimple_assign_single_p (def_stmt)
            || gimple_assign_unary_nop_p (def_stmt))
          {
            tree rhs = gimple_assign_rhs1 (def_stmt);
            return get_maxval_strlen (rhs, length, visited, type);
          }
        return false;

      case GIMPLE_PHI:
	{
	  /* All the arguments of the PHI node must have the same constant
	     length.  */
	  unsigned i;

	  for (i = 0; i < gimple_phi_num_args (def_stmt); i++)
          {
            tree arg = gimple_phi_arg (def_stmt, i)->def;

            /* If this PHI has itself as an argument, we cannot
               determine the string length of this argument.  However,
               if we can find a constant string length for the other
               PHI args then we can still be sure that this is a
               constant string length.  So be optimistic and just
               continue with the next argument.  */
            if (arg == gimple_phi_result (def_stmt))
              continue;

            if (!get_maxval_strlen (arg, length, visited, type))
              return false;
          }
        }
        return true;

      default:
        return false;
    }
}


/* Fold builtin call in statement STMT.  Returns a simplified tree.
   We may return a non-constant expression, including another call
   to a different function and with different arguments, e.g.,
   substituting memcpy for strcpy when the string length is known.
   Note that some builtins expand into inline code that may not
   be valid in GIMPLE.  Callers must take care.  */

tree
gimple_fold_builtin (gimple stmt)
{
  tree result, val[3];
  tree callee, a;
  int arg_idx, type;
  bitmap visited;
  bool ignore;
  int nargs;
  location_t loc = gimple_location (stmt);

  gcc_assert (is_gimple_call (stmt));

  ignore = (gimple_call_lhs (stmt) == NULL);

  /* First try the generic builtin folder.  If that succeeds, return the
     result directly.  */
  result = fold_call_stmt (stmt, ignore);
  if (result)
    {
      if (ignore)
	STRIP_NOPS (result);
      return result;
    }

  /* Ignore MD builtins.  */
  callee = gimple_call_fndecl (stmt);
  if (DECL_BUILT_IN_CLASS (callee) == BUILT_IN_MD)
    return NULL_TREE;

  /* Give up for always_inline inline builtins until they are
     inlined.  */
  if (avoid_folding_inline_builtin (callee))
    return NULL_TREE;

  /* If the builtin could not be folded, and it has no argument list,
     we're done.  */
  nargs = gimple_call_num_args (stmt);
  if (nargs == 0)
    return NULL_TREE;

  /* Limit the work only for builtins we know how to simplify.  */
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
    case BUILT_IN_FPUTS:
    case BUILT_IN_FPUTS_UNLOCKED:
      arg_idx = 0;
      type = 0;
      break;
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRNCPY:
      arg_idx = 1;
      type = 0;
      break;
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
    case BUILT_IN_STRNCPY_CHK:
      arg_idx = 2;
      type = 2;
      break;
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
      arg_idx = 1;
      type = 1;
      break;
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      arg_idx = 1;
      type = 2;
      break;
    default:
      return NULL_TREE;
    }

  if (arg_idx >= nargs)
    return NULL_TREE;

  /* Try to use the dataflow information gathered by the CCP process.  */
  visited = BITMAP_ALLOC (NULL);
  bitmap_clear (visited);

  memset (val, 0, sizeof (val));
  a = gimple_call_arg (stmt, arg_idx);
  if (!get_maxval_strlen (a, &val[arg_idx], visited, type))
    val[arg_idx] = NULL_TREE;

  BITMAP_FREE (visited);

  result = NULL_TREE;
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
      if (val[0] && nargs == 1)
	{
	  tree new_val =
              fold_convert (TREE_TYPE (gimple_call_lhs (stmt)), val[0]);

	  /* If the result is not a valid gimple value, or not a cast
	     of a valid gimple value, then we cannot use the result.  */
	  if (is_gimple_val (new_val)
	      || (CONVERT_EXPR_P (new_val)
		  && is_gimple_val (TREE_OPERAND (new_val, 0))))
	    return new_val;
	}
      break;

    case BUILT_IN_STRCPY:
      if (val[1] && is_gimple_val (val[1]) && nargs == 2)
	result = fold_builtin_strcpy (loc, callee,
                                      gimple_call_arg (stmt, 0),
                                      gimple_call_arg (stmt, 1),
				      val[1]);
      break;

    case BUILT_IN_STRNCPY:
      if (val[1] && is_gimple_val (val[1]) && nargs == 3)
	result = fold_builtin_strncpy (loc, callee,
                                       gimple_call_arg (stmt, 0),
                                       gimple_call_arg (stmt, 1),
                                       gimple_call_arg (stmt, 2),
				       val[1]);
      break;

    case BUILT_IN_FPUTS:
      if (nargs == 2)
	result = fold_builtin_fputs (loc, gimple_call_arg (stmt, 0),
				     gimple_call_arg (stmt, 1),
				     ignore, false, val[0]);
      break;

    case BUILT_IN_FPUTS_UNLOCKED:
      if (nargs == 2)
	result = fold_builtin_fputs (loc, gimple_call_arg (stmt, 0),
				     gimple_call_arg (stmt, 1),
				     ignore, true, val[0]);
      break;

    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
      if (val[2] && is_gimple_val (val[2]) && nargs == 4)
	result = fold_builtin_memory_chk (loc, callee,
                                          gimple_call_arg (stmt, 0),
                                          gimple_call_arg (stmt, 1),
                                          gimple_call_arg (stmt, 2),
                                          gimple_call_arg (stmt, 3),
					  val[2], ignore,
					  DECL_FUNCTION_CODE (callee));
      break;

    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
      if (val[1] && is_gimple_val (val[1]) && nargs == 3)
	result = fold_builtin_stxcpy_chk (loc, callee,
                                          gimple_call_arg (stmt, 0),
                                          gimple_call_arg (stmt, 1),
                                          gimple_call_arg (stmt, 2),
					  val[1], ignore,
					  DECL_FUNCTION_CODE (callee));
      break;

    case BUILT_IN_STRNCPY_CHK:
      if (val[2] && is_gimple_val (val[2]) && nargs == 4)
	result = fold_builtin_strncpy_chk (loc, gimple_call_arg (stmt, 0),
                                           gimple_call_arg (stmt, 1),
                                           gimple_call_arg (stmt, 2),
                                           gimple_call_arg (stmt, 3),
					   val[2]);
      break;

    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      if (val[1] && is_gimple_val (val[1]))
	result = gimple_fold_builtin_snprintf_chk (stmt, val[1],
                                                   DECL_FUNCTION_CODE (callee));
      break;

    default:
      gcc_unreachable ();
    }

  if (result && ignore)
    result = fold_ignored_result (result);
  return result;
}

/* Generate code adjusting the first parameter of a call statement determined
   by GSI by DELTA.  */

void
gimple_adjust_this_by_delta (gimple_stmt_iterator *gsi, tree delta)
{
  gimple call_stmt = gsi_stmt (*gsi);
  tree parm, tmp;
  gimple new_stmt;

  delta = convert_to_ptrofftype (delta);
  gcc_assert (gimple_call_num_args (call_stmt) >= 1);
  parm = gimple_call_arg (call_stmt, 0);
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (parm)));
  tmp = create_tmp_var (TREE_TYPE (parm), NULL);
  add_referenced_var (tmp);

  tmp = make_ssa_name (tmp, NULL);
  new_stmt = gimple_build_assign_with_ops (POINTER_PLUS_EXPR, tmp, parm, delta);
  SSA_NAME_DEF_STMT (tmp) = new_stmt;
  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
  gimple_call_set_arg (call_stmt, 0, tmp);
}

/* Return a binfo to be used for devirtualization of calls based on an object
   represented by a declaration (i.e. a global or automatically allocated one)
   or NULL if it cannot be found or is not safe.  CST is expected to be an
   ADDR_EXPR of such object or the function will return NULL.  Currently it is
   safe to use such binfo only if it has no base binfo (i.e. no ancestors).  */

tree
gimple_extract_devirt_binfo_from_cst (tree cst)
{
  HOST_WIDE_INT offset, size, max_size;
  tree base, type, expected_type, binfo;
  bool last_artificial = false;

  if (!flag_devirtualize
      || TREE_CODE (cst) != ADDR_EXPR
      || TREE_CODE (TREE_TYPE (TREE_TYPE (cst))) != RECORD_TYPE)
    return NULL_TREE;

  cst = TREE_OPERAND (cst, 0);
  expected_type = TREE_TYPE (cst);
  base = get_ref_base_and_extent (cst, &offset, &size, &max_size);
  type = TREE_TYPE (base);
  if (!DECL_P (base)
      || max_size == -1
      || max_size != size
      || TREE_CODE (type) != RECORD_TYPE)
    return NULL_TREE;

  /* Find the sub-object the constant actually refers to and mark whether it is
     an artificial one (as opposed to a user-defined one).  */
  while (true)
    {
      HOST_WIDE_INT pos, size;
      tree fld;

      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (expected_type))
	break;
      if (offset < 0)
	return NULL_TREE;

      for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	{
	  if (TREE_CODE (fld) != FIELD_DECL)
	    continue;

	  pos = int_bit_position (fld);
	  size = tree_low_cst (DECL_SIZE (fld), 1);
	  if (pos <= offset && (pos + size) > offset)
	    break;
	}
      if (!fld || TREE_CODE (TREE_TYPE (fld)) != RECORD_TYPE)
	return NULL_TREE;

      last_artificial = DECL_ARTIFICIAL (fld);
      type = TREE_TYPE (fld);
      offset -= pos;
    }
  /* Artifical sub-objects are ancestors, we do not want to use them for
     devirtualization, at least not here.  */
  if (last_artificial)
    return NULL_TREE;
  binfo = TYPE_BINFO (type);
  if (!binfo || BINFO_N_BASE_BINFOS (binfo) > 0)
    return NULL_TREE;
  else
    return binfo;
}

/* Attempt to fold a call statement referenced by the statement iterator GSI.
   The statement may be replaced by another statement, e.g., if the call
   simplifies to a constant value. Return true if any changes were made.
   It is assumed that the operands have been previously folded.  */

static bool
gimple_fold_call (gimple_stmt_iterator *gsi, bool inplace)
{
  gimple stmt = gsi_stmt (*gsi);
  tree callee;
  bool changed = false;
  unsigned i;

  /* Fold *& in call arguments.  */
  for (i = 0; i < gimple_call_num_args (stmt); ++i)
    if (REFERENCE_CLASS_P (gimple_call_arg (stmt, i)))
      {
	tree tmp = maybe_fold_reference (gimple_call_arg (stmt, i), false);
	if (tmp)
	  {
	    gimple_call_set_arg (stmt, i, tmp);
	    changed = true;
	  }
      }

  /* Check for virtual calls that became direct calls.  */
  callee = gimple_call_fn (stmt);
  if (callee && TREE_CODE (callee) == OBJ_TYPE_REF)
    {
      if (gimple_call_addr_fndecl (OBJ_TYPE_REF_EXPR (callee)) != NULL_TREE)
	{
	  gimple_call_set_fn (stmt, OBJ_TYPE_REF_EXPR (callee));
	  changed = true;
	}
      else
	{
	  tree obj = OBJ_TYPE_REF_OBJECT (callee);
	  tree binfo = gimple_extract_devirt_binfo_from_cst (obj);
	  if (binfo)
	    {
	      HOST_WIDE_INT token
		= TREE_INT_CST_LOW (OBJ_TYPE_REF_TOKEN (callee));
	      tree fndecl = gimple_get_virt_method_for_binfo (token, binfo);
	      if (fndecl)
		{
		  gimple_call_set_fndecl (stmt, fndecl);
		  changed = true;
		}
	    }
	}
    }

  if (inplace)
    return changed;

  /* Check for builtins that CCP can handle using information not
     available in the generic fold routines.  */
  callee = gimple_call_fndecl (stmt);
  if (callee && DECL_BUILT_IN (callee))
    {
      tree result = gimple_fold_builtin (stmt);
      if (result)
	{
          if (!update_call_from_tree (gsi, result))
	    gimplify_and_update_call_from_tree (gsi, result);
	  changed = true;
	}
    }

  return changed;
}

/* Worker for both fold_stmt and fold_stmt_inplace.  The INPLACE argument
   distinguishes both cases.  */

static bool
fold_stmt_1 (gimple_stmt_iterator *gsi, bool inplace)
{
  bool changed = false;
  gimple stmt = gsi_stmt (*gsi);
  unsigned i;
  gimple_stmt_iterator gsinext = *gsi;
  gimple next_stmt;

  gsi_next (&gsinext);
  next_stmt = gsi_end_p (gsinext) ? NULL : gsi_stmt (gsinext);

  /* Fold the main computation performed by the statement.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      {
	unsigned old_num_ops = gimple_num_ops (stmt);
	enum tree_code subcode = gimple_assign_rhs_code (stmt);
	tree lhs = gimple_assign_lhs (stmt);
	tree new_rhs;
	/* First canonicalize operand order.  This avoids building new
	   trees if this is the only thing fold would later do.  */
	if ((commutative_tree_code (subcode)
	     || commutative_ternary_tree_code (subcode))
	    && tree_swap_operands_p (gimple_assign_rhs1 (stmt),
				     gimple_assign_rhs2 (stmt), false))
	  {
	    tree tem = gimple_assign_rhs1 (stmt);
	    gimple_assign_set_rhs1 (stmt, gimple_assign_rhs2 (stmt));
	    gimple_assign_set_rhs2 (stmt, tem);
	    changed = true;
	  }
	new_rhs = fold_gimple_assign (gsi);
	if (new_rhs
	    && !useless_type_conversion_p (TREE_TYPE (lhs),
					   TREE_TYPE (new_rhs)))
	  new_rhs = fold_convert (TREE_TYPE (lhs), new_rhs);
	if (new_rhs
	    && (!inplace
		|| get_gimple_rhs_num_ops (TREE_CODE (new_rhs)) < old_num_ops))
	  {
	    gimple_assign_set_rhs_from_tree (gsi, new_rhs);
	    changed = true;
	  }
	break;
      }

    case GIMPLE_COND:
      changed |= fold_gimple_cond (stmt);
      break;

    case GIMPLE_CALL:
      changed |= gimple_fold_call (gsi, inplace);
      break;

    case GIMPLE_ASM:
      /* Fold *& in asm operands.  */
      {
	size_t noutputs;
	const char **oconstraints;
	const char *constraint;
	bool allows_mem, allows_reg;

	noutputs = gimple_asm_noutputs (stmt);
	oconstraints = XALLOCAVEC (const char *, noutputs);

	for (i = 0; i < gimple_asm_noutputs (stmt); ++i)
	  {
	    tree link = gimple_asm_output_op (stmt, i);
	    tree op = TREE_VALUE (link);
	    oconstraints[i]
	      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
	    if (REFERENCE_CLASS_P (op)
		&& (op = maybe_fold_reference (op, true)) != NULL_TREE)
	      {
		TREE_VALUE (link) = op;
		changed = true;
	      }
	  }
	for (i = 0; i < gimple_asm_ninputs (stmt); ++i)
	  {
	    tree link = gimple_asm_input_op (stmt, i);
	    tree op = TREE_VALUE (link);
	    constraint
	      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
	    parse_input_constraint (&constraint, 0, 0, noutputs, 0,
				    oconstraints, &allows_mem, &allows_reg);
	    if (REFERENCE_CLASS_P (op)
		&& (op = maybe_fold_reference (op, !allows_reg && allows_mem))
		   != NULL_TREE)
	      {
		TREE_VALUE (link) = op;
		changed = true;
	      }
	  }
      }
      break;

    case GIMPLE_DEBUG:
      if (gimple_debug_bind_p (stmt))
	{
	  tree val = gimple_debug_bind_get_value (stmt);
	  if (val
	      && REFERENCE_CLASS_P (val))
	    {
	      tree tem = maybe_fold_reference (val, false);
	      if (tem)
		{
		  gimple_debug_bind_set_value (stmt, tem);
		  changed = true;
		}
	    }
	}
      break;

    default:;
    }

  /* If stmt folds into nothing and it was the last stmt in a bb,
     don't call gsi_stmt.  */
  if (gsi_end_p (*gsi))
    {
      gcc_assert (next_stmt == NULL);
      return changed;
    }

  stmt = gsi_stmt (*gsi);

  /* Fold *& on the lhs.  Don't do this if stmt folded into nothing,
     as we'd changing the next stmt.  */
  if (gimple_has_lhs (stmt) && stmt != next_stmt)
    {
      tree lhs = gimple_get_lhs (stmt);
      if (lhs && REFERENCE_CLASS_P (lhs))
	{
	  tree new_lhs = maybe_fold_reference (lhs, true);
	  if (new_lhs)
	    {
	      gimple_set_lhs (stmt, new_lhs);
	      changed = true;
	    }
	}
    }

  return changed;
}

/* Fold the statement pointed to by GSI.  In some cases, this function may
   replace the whole statement with a new one.  Returns true iff folding
   makes any changes.
   The statement pointed to by GSI should be in valid gimple form but may
   be in unfolded state as resulting from for example constant propagation
   which can produce *&x = 0.  */

bool
fold_stmt (gimple_stmt_iterator *gsi)
{
  return fold_stmt_1 (gsi, false);
}

/* Perform the minimal folding on statement *GSI.  Only operations like
   *&x created by constant propagation are handled.  The statement cannot
   be replaced with a new one.  Return true if the statement was
   changed, false otherwise.
   The statement *GSI should be in valid gimple form but may
   be in unfolded state as resulting from for example constant propagation
   which can produce *&x = 0.  */

bool
fold_stmt_inplace (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  bool changed = fold_stmt_1 (gsi, true);
  gcc_assert (gsi_stmt (*gsi) == stmt);
  return changed;
}

/* Canonicalize and possibly invert the boolean EXPR; return NULL_TREE 
   if EXPR is null or we don't know how.
   If non-null, the result always has boolean type.  */

static tree
canonicalize_bool (tree expr, bool invert)
{
  if (!expr)
    return NULL_TREE;
  else if (invert)
    {
      if (integer_nonzerop (expr))
	return boolean_false_node;
      else if (integer_zerop (expr))
	return boolean_true_node;
      else if (TREE_CODE (expr) == SSA_NAME)
	return fold_build2 (EQ_EXPR, boolean_type_node, expr,
			    build_int_cst (TREE_TYPE (expr), 0));
      else if (TREE_CODE_CLASS (TREE_CODE (expr)) == tcc_comparison)
	return fold_build2 (invert_tree_comparison (TREE_CODE (expr), false),
			    boolean_type_node,
			    TREE_OPERAND (expr, 0),
			    TREE_OPERAND (expr, 1));
      else
	return NULL_TREE;
    }
  else
    {
      if (TREE_CODE (TREE_TYPE (expr)) == BOOLEAN_TYPE)
	return expr;
      if (integer_nonzerop (expr))
	return boolean_true_node;
      else if (integer_zerop (expr))
	return boolean_false_node;
      else if (TREE_CODE (expr) == SSA_NAME)
	return fold_build2 (NE_EXPR, boolean_type_node, expr,
			    build_int_cst (TREE_TYPE (expr), 0));
      else if (TREE_CODE_CLASS (TREE_CODE (expr)) == tcc_comparison)
	return fold_build2 (TREE_CODE (expr),
			    boolean_type_node,
			    TREE_OPERAND (expr, 0),
			    TREE_OPERAND (expr, 1));
      else
	return NULL_TREE;
    }
}

/* Check to see if a boolean expression EXPR is logically equivalent to the
   comparison (OP1 CODE OP2).  Check for various identities involving
   SSA_NAMEs.  */

static bool
same_bool_comparison_p (const_tree expr, enum tree_code code,
			const_tree op1, const_tree op2)
{
  gimple s;

  /* The obvious case.  */
  if (TREE_CODE (expr) == code
      && operand_equal_p (TREE_OPERAND (expr, 0), op1, 0)
      && operand_equal_p (TREE_OPERAND (expr, 1), op2, 0))
    return true;

  /* Check for comparing (name, name != 0) and the case where expr
     is an SSA_NAME with a definition matching the comparison.  */
  if (TREE_CODE (expr) == SSA_NAME
      && TREE_CODE (TREE_TYPE (expr)) == BOOLEAN_TYPE)
    {
      if (operand_equal_p (expr, op1, 0))
	return ((code == NE_EXPR && integer_zerop (op2))
		|| (code == EQ_EXPR && integer_nonzerop (op2)));
      s = SSA_NAME_DEF_STMT (expr);
      if (is_gimple_assign (s)
	  && gimple_assign_rhs_code (s) == code
	  && operand_equal_p (gimple_assign_rhs1 (s), op1, 0)
	  && operand_equal_p (gimple_assign_rhs2 (s), op2, 0))
	return true;
    }

  /* If op1 is of the form (name != 0) or (name == 0), and the definition
     of name is a comparison, recurse.  */
  if (TREE_CODE (op1) == SSA_NAME
      && TREE_CODE (TREE_TYPE (op1)) == BOOLEAN_TYPE)
    {
      s = SSA_NAME_DEF_STMT (op1);
      if (is_gimple_assign (s)
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison)
	{
	  enum tree_code c = gimple_assign_rhs_code (s);
	  if ((c == NE_EXPR && integer_zerop (op2))
	      || (c == EQ_EXPR && integer_nonzerop (op2)))
	    return same_bool_comparison_p (expr, c,
					   gimple_assign_rhs1 (s),
					   gimple_assign_rhs2 (s));
	  if ((c == EQ_EXPR && integer_zerop (op2))
	      || (c == NE_EXPR && integer_nonzerop (op2)))
	    return same_bool_comparison_p (expr,
					   invert_tree_comparison (c, false),
					   gimple_assign_rhs1 (s),
					   gimple_assign_rhs2 (s));
	}
    }
  return false;
}

/* Check to see if two boolean expressions OP1 and OP2 are logically
   equivalent.  */

static bool
same_bool_result_p (const_tree op1, const_tree op2)
{
  /* Simple cases first.  */
  if (operand_equal_p (op1, op2, 0))
    return true;

  /* Check the cases where at least one of the operands is a comparison.
     These are a bit smarter than operand_equal_p in that they apply some
     identifies on SSA_NAMEs.  */
  if (TREE_CODE_CLASS (TREE_CODE (op2)) == tcc_comparison
      && same_bool_comparison_p (op1, TREE_CODE (op2),
				 TREE_OPERAND (op2, 0),
				 TREE_OPERAND (op2, 1)))
    return true;
  if (TREE_CODE_CLASS (TREE_CODE (op1)) == tcc_comparison
      && same_bool_comparison_p (op2, TREE_CODE (op1),
				 TREE_OPERAND (op1, 0),
				 TREE_OPERAND (op1, 1)))
    return true;

  /* Default case.  */
  return false;
}

/* Forward declarations for some mutually recursive functions.  */

static tree
and_comparisons_1 (enum tree_code code1, tree op1a, tree op1b,
		   enum tree_code code2, tree op2a, tree op2b);
static tree
and_var_with_comparison (tree var, bool invert,
			 enum tree_code code2, tree op2a, tree op2b);
static tree
and_var_with_comparison_1 (gimple stmt, 
			   enum tree_code code2, tree op2a, tree op2b);
static tree
or_comparisons_1 (enum tree_code code1, tree op1a, tree op1b,
		  enum tree_code code2, tree op2a, tree op2b);
static tree
or_var_with_comparison (tree var, bool invert,
			enum tree_code code2, tree op2a, tree op2b);
static tree
or_var_with_comparison_1 (gimple stmt, 
			  enum tree_code code2, tree op2a, tree op2b);

/* Helper function for and_comparisons_1:  try to simplify the AND of the
   ssa variable VAR with the comparison specified by (OP2A CODE2 OP2B).
   If INVERT is true, invert the value of the VAR before doing the AND.
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
and_var_with_comparison (tree var, bool invert,
			 enum tree_code code2, tree op2a, tree op2b)
{
  tree t;
  gimple stmt = SSA_NAME_DEF_STMT (var);

  /* We can only deal with variables whose definitions are assignments.  */
  if (!is_gimple_assign (stmt))
    return NULL_TREE;
  
  /* If we have an inverted comparison, apply DeMorgan's law and rewrite
     !var AND (op2a code2 op2b) => !(var OR !(op2a code2 op2b))
     Then we only have to consider the simpler non-inverted cases.  */
  if (invert)
    t = or_var_with_comparison_1 (stmt, 
				  invert_tree_comparison (code2, false),
				  op2a, op2b);
  else
    t = and_var_with_comparison_1 (stmt, code2, op2a, op2b);
  return canonicalize_bool (t, invert);
}

/* Try to simplify the AND of the ssa variable defined by the assignment
   STMT with the comparison specified by (OP2A CODE2 OP2B).
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
and_var_with_comparison_1 (gimple stmt,
			   enum tree_code code2, tree op2a, tree op2b)
{
  tree var = gimple_assign_lhs (stmt);
  tree true_test_var = NULL_TREE;
  tree false_test_var = NULL_TREE;
  enum tree_code innercode = gimple_assign_rhs_code (stmt);

  /* Check for identities like (var AND (var == 0)) => false.  */
  if (TREE_CODE (op2a) == SSA_NAME
      && TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE)
    {
      if ((code2 == NE_EXPR && integer_zerop (op2b))
	  || (code2 == EQ_EXPR && integer_nonzerop (op2b)))
	{
	  true_test_var = op2a;
	  if (var == true_test_var)
	    return var;
	}
      else if ((code2 == EQ_EXPR && integer_zerop (op2b))
	       || (code2 == NE_EXPR && integer_nonzerop (op2b)))
	{
	  false_test_var = op2a;
	  if (var == false_test_var)
	    return boolean_false_node;
	}
    }

  /* If the definition is a comparison, recurse on it.  */
  if (TREE_CODE_CLASS (innercode) == tcc_comparison)
    {
      tree t = and_comparisons_1 (innercode,
				  gimple_assign_rhs1 (stmt),
				  gimple_assign_rhs2 (stmt),
				  code2,
				  op2a,
				  op2b);
      if (t)
	return t;
    }

  /* If the definition is an AND or OR expression, we may be able to
     simplify by reassociating.  */
  if (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE
      && (innercode == BIT_AND_EXPR || innercode == BIT_IOR_EXPR))
    {
      tree inner1 = gimple_assign_rhs1 (stmt);
      tree inner2 = gimple_assign_rhs2 (stmt);
      gimple s;
      tree t;
      tree partial = NULL_TREE;
      bool is_and = (innercode == BIT_AND_EXPR);
      
      /* Check for boolean identities that don't require recursive examination
	 of inner1/inner2:
	 inner1 AND (inner1 AND inner2) => inner1 AND inner2 => var
	 inner1 AND (inner1 OR inner2) => inner1
	 !inner1 AND (inner1 AND inner2) => false
	 !inner1 AND (inner1 OR inner2) => !inner1 AND inner2
         Likewise for similar cases involving inner2.  */
      if (inner1 == true_test_var)
	return (is_and ? var : inner1);
      else if (inner2 == true_test_var)
	return (is_and ? var : inner2);
      else if (inner1 == false_test_var)
	return (is_and
		? boolean_false_node
		: and_var_with_comparison (inner2, false, code2, op2a, op2b));
      else if (inner2 == false_test_var)
	return (is_and
		? boolean_false_node
		: and_var_with_comparison (inner1, false, code2, op2a, op2b));

      /* Next, redistribute/reassociate the AND across the inner tests.
	 Compute the first partial result, (inner1 AND (op2a code op2b))  */
      if (TREE_CODE (inner1) == SSA_NAME
	  && is_gimple_assign (s = SSA_NAME_DEF_STMT (inner1))
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison
	  && (t = maybe_fold_and_comparisons (gimple_assign_rhs_code (s),
					      gimple_assign_rhs1 (s),
					      gimple_assign_rhs2 (s),
					      code2, op2a, op2b)))
	{
	  /* Handle the AND case, where we are reassociating:
	     (inner1 AND inner2) AND (op2a code2 op2b)
	     => (t AND inner2)
	     If the partial result t is a constant, we win.  Otherwise
	     continue on to try reassociating with the other inner test.  */
	  if (is_and)
	    {
	      if (integer_onep (t))
		return inner2;
	      else if (integer_zerop (t))
		return boolean_false_node;
	    }

	  /* Handle the OR case, where we are redistributing:
	     (inner1 OR inner2) AND (op2a code2 op2b)
	     => (t OR (inner2 AND (op2a code2 op2b)))  */
	  else if (integer_onep (t))
	    return boolean_true_node;

	  /* Save partial result for later.  */
	  partial = t;
	}
      
      /* Compute the second partial result, (inner2 AND (op2a code op2b)) */
      if (TREE_CODE (inner2) == SSA_NAME
	  && is_gimple_assign (s = SSA_NAME_DEF_STMT (inner2))
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison
	  && (t = maybe_fold_and_comparisons (gimple_assign_rhs_code (s),
					      gimple_assign_rhs1 (s),
					      gimple_assign_rhs2 (s),
					      code2, op2a, op2b)))
	{
	  /* Handle the AND case, where we are reassociating:
	     (inner1 AND inner2) AND (op2a code2 op2b)
	     => (inner1 AND t)  */
	  if (is_and)
	    {
	      if (integer_onep (t))
		return inner1;
	      else if (integer_zerop (t))
		return boolean_false_node;
	      /* If both are the same, we can apply the identity
		 (x AND x) == x.  */
	      else if (partial && same_bool_result_p (t, partial))
		return t;
	    }

	  /* Handle the OR case. where we are redistributing:
	     (inner1 OR inner2) AND (op2a code2 op2b)
	     => (t OR (inner1 AND (op2a code2 op2b)))
	     => (t OR partial)  */
	  else
	    {
	      if (integer_onep (t))
		return boolean_true_node;
	      else if (partial)
		{
		  /* We already got a simplification for the other
		     operand to the redistributed OR expression.  The
		     interesting case is when at least one is false.
		     Or, if both are the same, we can apply the identity
		     (x OR x) == x.  */
		  if (integer_zerop (partial))
		    return t;
		  else if (integer_zerop (t))
		    return partial;
		  else if (same_bool_result_p (t, partial))
		    return t;
		}
	    }
	}
    }
  return NULL_TREE;
}

/* Try to simplify the AND of two comparisons defined by
   (OP1A CODE1 OP1B) and (OP2A CODE2 OP2B), respectively.
   If this can be done without constructing an intermediate value,
   return the resulting tree; otherwise NULL_TREE is returned.
   This function is deliberately asymmetric as it recurses on SSA_DEFs
   in the first comparison but not the second.  */

static tree
and_comparisons_1 (enum tree_code code1, tree op1a, tree op1b,
		   enum tree_code code2, tree op2a, tree op2b)
{
  /* First check for ((x CODE1 y) AND (x CODE2 y)).  */
  if (operand_equal_p (op1a, op2a, 0)
      && operand_equal_p (op1b, op2b, 0))
    {
      /* Result will be either NULL_TREE, or a combined comparison.  */
      tree t = combine_comparisons (UNKNOWN_LOCATION,
				    TRUTH_ANDIF_EXPR, code1, code2,
				    boolean_type_node, op1a, op1b);
      if (t)
	return t;
    }

  /* Likewise the swapped case of the above.  */
  if (operand_equal_p (op1a, op2b, 0)
      && operand_equal_p (op1b, op2a, 0))
    {
      /* Result will be either NULL_TREE, or a combined comparison.  */
      tree t = combine_comparisons (UNKNOWN_LOCATION,
				    TRUTH_ANDIF_EXPR, code1,
				    swap_tree_comparison (code2),
				    boolean_type_node, op1a, op1b);
      if (t)
	return t;
    }

  /* If both comparisons are of the same value against constants, we might
     be able to merge them.  */
  if (operand_equal_p (op1a, op2a, 0)
      && TREE_CODE (op1b) == INTEGER_CST
      && TREE_CODE (op2b) == INTEGER_CST)
    {
      int cmp = tree_int_cst_compare (op1b, op2b);

      /* If we have (op1a == op1b), we should either be able to
	 return that or FALSE, depending on whether the constant op1b
	 also satisfies the other comparison against op2b.  */
      if (code1 == EQ_EXPR)
	{
	  bool done = true;
	  bool val;
	  switch (code2)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp < 0); break;
	    case GT_EXPR: val = (cmp > 0); break;
	    case LE_EXPR: val = (cmp <= 0); break;
	    case GE_EXPR: val = (cmp >= 0); break;
	    default: done = false;
	    }
	  if (done)
	    {
	      if (val)
		return fold_build2 (code1, boolean_type_node, op1a, op1b);
	      else
		return boolean_false_node;
	    }
	}
      /* Likewise if the second comparison is an == comparison.  */
      else if (code2 == EQ_EXPR)
	{
	  bool done = true;
	  bool val;
	  switch (code1)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp > 0); break;
	    case GT_EXPR: val = (cmp < 0); break;
	    case LE_EXPR: val = (cmp >= 0); break;
	    case GE_EXPR: val = (cmp <= 0); break;
	    default: done = false;
	    }
	  if (done)
	    {
	      if (val)
		return fold_build2 (code2, boolean_type_node, op2a, op2b);
	      else
		return boolean_false_node;
	    }
	}

      /* Same business with inequality tests.  */
      else if (code1 == NE_EXPR)
	{
	  bool val;
	  switch (code2)
	    {
	    case EQ_EXPR: val = (cmp != 0); break;
	    case NE_EXPR: val = (cmp == 0); break;
	    case LT_EXPR: val = (cmp >= 0); break;
	    case GT_EXPR: val = (cmp <= 0); break;
	    case LE_EXPR: val = (cmp > 0); break;
	    case GE_EXPR: val = (cmp < 0); break;
	    default:
	      val = false;
	    }
	  if (val)
	    return fold_build2 (code2, boolean_type_node, op2a, op2b);
	}
      else if (code2 == NE_EXPR)
	{
	  bool val;
	  switch (code1)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp <= 0); break;
	    case GT_EXPR: val = (cmp >= 0); break;
	    case LE_EXPR: val = (cmp < 0); break;
	    case GE_EXPR: val = (cmp > 0); break;
	    default:
	      val = false;
	    }
	  if (val)
	    return fold_build2 (code1, boolean_type_node, op1a, op1b);
	}

      /* Chose the more restrictive of two < or <= comparisons.  */
      else if ((code1 == LT_EXPR || code1 == LE_EXPR)
	       && (code2 == LT_EXPR || code2 == LE_EXPR))
	{
	  if ((cmp < 0) || (cmp == 0 && code1 == LT_EXPR))
	    return fold_build2 (code1, boolean_type_node, op1a, op1b);
	  else
	    return fold_build2 (code2, boolean_type_node, op2a, op2b);
	}

      /* Likewise chose the more restrictive of two > or >= comparisons.  */
      else if ((code1 == GT_EXPR || code1 == GE_EXPR)
	       && (code2 == GT_EXPR || code2 == GE_EXPR))
	{
	  if ((cmp > 0) || (cmp == 0 && code1 == GT_EXPR))
	    return fold_build2 (code1, boolean_type_node, op1a, op1b);
	  else
	    return fold_build2 (code2, boolean_type_node, op2a, op2b);
	}

      /* Check for singleton ranges.  */
      else if (cmp == 0
	       && ((code1 == LE_EXPR && code2 == GE_EXPR)
		   || (code1 == GE_EXPR && code2 == LE_EXPR)))
	return fold_build2 (EQ_EXPR, boolean_type_node, op1a, op2b);

      /* Check for disjoint ranges. */
      else if (cmp <= 0
	       && (code1 == LT_EXPR || code1 == LE_EXPR)
	       && (code2 == GT_EXPR || code2 == GE_EXPR))
	return boolean_false_node;
      else if (cmp >= 0
	       && (code1 == GT_EXPR || code1 == GE_EXPR)
	       && (code2 == LT_EXPR || code2 == LE_EXPR))
	return boolean_false_node;
    }

  /* Perhaps the first comparison is (NAME != 0) or (NAME == 1) where
     NAME's definition is a truth value.  See if there are any simplifications
     that can be done against the NAME's definition.  */
  if (TREE_CODE (op1a) == SSA_NAME
      && (code1 == NE_EXPR || code1 == EQ_EXPR)
      && (integer_zerop (op1b) || integer_onep (op1b)))
    {
      bool invert = ((code1 == EQ_EXPR && integer_zerop (op1b))
		     || (code1 == NE_EXPR && integer_onep (op1b)));
      gimple stmt = SSA_NAME_DEF_STMT (op1a);
      switch (gimple_code (stmt))
	{
	case GIMPLE_ASSIGN:
	  /* Try to simplify by copy-propagating the definition.  */
	  return and_var_with_comparison (op1a, invert, code2, op2a, op2b);

	case GIMPLE_PHI:
	  /* If every argument to the PHI produces the same result when
	     ANDed with the second comparison, we win.
	     Do not do this unless the type is bool since we need a bool
	     result here anyway.  */
	  if (TREE_CODE (TREE_TYPE (op1a)) == BOOLEAN_TYPE)
	    {
	      tree result = NULL_TREE;
	      unsigned i;
	      for (i = 0; i < gimple_phi_num_args (stmt); i++)
		{
		  tree arg = gimple_phi_arg_def (stmt, i);
		  
		  /* If this PHI has itself as an argument, ignore it.
		     If all the other args produce the same result,
		     we're still OK.  */
		  if (arg == gimple_phi_result (stmt))
		    continue;
		  else if (TREE_CODE (arg) == INTEGER_CST)
		    {
		      if (invert ? integer_nonzerop (arg) : integer_zerop (arg))
			{
			  if (!result)
			    result = boolean_false_node;
			  else if (!integer_zerop (result))
			    return NULL_TREE;
			}
		      else if (!result)
			result = fold_build2 (code2, boolean_type_node,
					      op2a, op2b);
		      else if (!same_bool_comparison_p (result,
							code2, op2a, op2b))
			return NULL_TREE;
		    }
		  else if (TREE_CODE (arg) == SSA_NAME
			   && !SSA_NAME_IS_DEFAULT_DEF (arg))
		    {
		      tree temp;
		      gimple def_stmt = SSA_NAME_DEF_STMT (arg);
		      /* In simple cases we can look through PHI nodes,
			 but we have to be careful with loops.
			 See PR49073.  */
		      if (! dom_info_available_p (CDI_DOMINATORS)
			  || gimple_bb (def_stmt) == gimple_bb (stmt)
			  || dominated_by_p (CDI_DOMINATORS,
					     gimple_bb (def_stmt),
					     gimple_bb (stmt)))
			return NULL_TREE;
		      temp = and_var_with_comparison (arg, invert, code2,
						      op2a, op2b);
		      if (!temp)
			return NULL_TREE;
		      else if (!result)
			result = temp;
		      else if (!same_bool_result_p (result, temp))
			return NULL_TREE;
		    }
		  else
		    return NULL_TREE;
		}
	      return result;
	    }

	default:
	  break;
	}
    }
  return NULL_TREE;
}

/* Try to simplify the AND of two comparisons, specified by
   (OP1A CODE1 OP1B) and (OP2B CODE2 OP2B), respectively.
   If this can be simplified to a single expression (without requiring
   introducing more SSA variables to hold intermediate values),
   return the resulting tree.  Otherwise return NULL_TREE.
   If the result expression is non-null, it has boolean type.  */

tree
maybe_fold_and_comparisons (enum tree_code code1, tree op1a, tree op1b,
			    enum tree_code code2, tree op2a, tree op2b)
{
  tree t = and_comparisons_1 (code1, op1a, op1b, code2, op2a, op2b);
  if (t)
    return t;
  else
    return and_comparisons_1 (code2, op2a, op2b, code1, op1a, op1b);
}

/* Helper function for or_comparisons_1:  try to simplify the OR of the
   ssa variable VAR with the comparison specified by (OP2A CODE2 OP2B).
   If INVERT is true, invert the value of VAR before doing the OR.
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
or_var_with_comparison (tree var, bool invert,
			enum tree_code code2, tree op2a, tree op2b)
{
  tree t;
  gimple stmt = SSA_NAME_DEF_STMT (var);

  /* We can only deal with variables whose definitions are assignments.  */
  if (!is_gimple_assign (stmt))
    return NULL_TREE;
  
  /* If we have an inverted comparison, apply DeMorgan's law and rewrite
     !var OR (op2a code2 op2b) => !(var AND !(op2a code2 op2b))
     Then we only have to consider the simpler non-inverted cases.  */
  if (invert)
    t = and_var_with_comparison_1 (stmt, 
				   invert_tree_comparison (code2, false),
				   op2a, op2b);
  else
    t = or_var_with_comparison_1 (stmt, code2, op2a, op2b);
  return canonicalize_bool (t, invert);
}

/* Try to simplify the OR of the ssa variable defined by the assignment
   STMT with the comparison specified by (OP2A CODE2 OP2B).
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
or_var_with_comparison_1 (gimple stmt,
			  enum tree_code code2, tree op2a, tree op2b)
{
  tree var = gimple_assign_lhs (stmt);
  tree true_test_var = NULL_TREE;
  tree false_test_var = NULL_TREE;
  enum tree_code innercode = gimple_assign_rhs_code (stmt);

  /* Check for identities like (var OR (var != 0)) => true .  */
  if (TREE_CODE (op2a) == SSA_NAME
      && TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE)
    {
      if ((code2 == NE_EXPR && integer_zerop (op2b))
	  || (code2 == EQ_EXPR && integer_nonzerop (op2b)))
	{
	  true_test_var = op2a;
	  if (var == true_test_var)
	    return var;
	}
      else if ((code2 == EQ_EXPR && integer_zerop (op2b))
	       || (code2 == NE_EXPR && integer_nonzerop (op2b)))
	{
	  false_test_var = op2a;
	  if (var == false_test_var)
	    return boolean_true_node;
	}
    }

  /* If the definition is a comparison, recurse on it.  */
  if (TREE_CODE_CLASS (innercode) == tcc_comparison)
    {
      tree t = or_comparisons_1 (innercode,
				 gimple_assign_rhs1 (stmt),
				 gimple_assign_rhs2 (stmt),
				 code2,
				 op2a,
				 op2b);
      if (t)
	return t;
    }
  
  /* If the definition is an AND or OR expression, we may be able to
     simplify by reassociating.  */
  if (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE
      && (innercode == BIT_AND_EXPR || innercode == BIT_IOR_EXPR))
    {
      tree inner1 = gimple_assign_rhs1 (stmt);
      tree inner2 = gimple_assign_rhs2 (stmt);
      gimple s;
      tree t;
      tree partial = NULL_TREE;
      bool is_or = (innercode == BIT_IOR_EXPR);
      
      /* Check for boolean identities that don't require recursive examination
	 of inner1/inner2:
	 inner1 OR (inner1 OR inner2) => inner1 OR inner2 => var
	 inner1 OR (inner1 AND inner2) => inner1
	 !inner1 OR (inner1 OR inner2) => true
	 !inner1 OR (inner1 AND inner2) => !inner1 OR inner2
      */
      if (inner1 == true_test_var)
	return (is_or ? var : inner1);
      else if (inner2 == true_test_var)
	return (is_or ? var : inner2);
      else if (inner1 == false_test_var)
	return (is_or
		? boolean_true_node
		: or_var_with_comparison (inner2, false, code2, op2a, op2b));
      else if (inner2 == false_test_var)
	return (is_or
		? boolean_true_node
		: or_var_with_comparison (inner1, false, code2, op2a, op2b));
      
      /* Next, redistribute/reassociate the OR across the inner tests.
	 Compute the first partial result, (inner1 OR (op2a code op2b))  */
      if (TREE_CODE (inner1) == SSA_NAME
	  && is_gimple_assign (s = SSA_NAME_DEF_STMT (inner1))
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison
	  && (t = maybe_fold_or_comparisons (gimple_assign_rhs_code (s),
					     gimple_assign_rhs1 (s),
					     gimple_assign_rhs2 (s),
					     code2, op2a, op2b)))
	{
	  /* Handle the OR case, where we are reassociating:
	     (inner1 OR inner2) OR (op2a code2 op2b)
	     => (t OR inner2)
	     If the partial result t is a constant, we win.  Otherwise
	     continue on to try reassociating with the other inner test.  */
	  if (is_or)
	    {
	      if (integer_onep (t))
		return boolean_true_node;
	      else if (integer_zerop (t))
		return inner2;
	    }
	  
	  /* Handle the AND case, where we are redistributing:
	     (inner1 AND inner2) OR (op2a code2 op2b)
	     => (t AND (inner2 OR (op2a code op2b)))  */
	  else if (integer_zerop (t))
	    return boolean_false_node;

	  /* Save partial result for later.  */
	  partial = t;
	}
      
      /* Compute the second partial result, (inner2 OR (op2a code op2b)) */
      if (TREE_CODE (inner2) == SSA_NAME
	  && is_gimple_assign (s = SSA_NAME_DEF_STMT (inner2))
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison
	  && (t = maybe_fold_or_comparisons (gimple_assign_rhs_code (s),
					     gimple_assign_rhs1 (s),
					     gimple_assign_rhs2 (s),
					     code2, op2a, op2b)))
	{
	  /* Handle the OR case, where we are reassociating:
	     (inner1 OR inner2) OR (op2a code2 op2b)
	     => (inner1 OR t)
	     => (t OR partial)  */
	  if (is_or)
	    {
	      if (integer_zerop (t))
		return inner1;
	      else if (integer_onep (t))
		return boolean_true_node;
	      /* If both are the same, we can apply the identity
		 (x OR x) == x.  */
	      else if (partial && same_bool_result_p (t, partial))
		return t;
	    }
	  
	  /* Handle the AND case, where we are redistributing:
	     (inner1 AND inner2) OR (op2a code2 op2b)
	     => (t AND (inner1 OR (op2a code2 op2b)))
	     => (t AND partial)  */
	  else 
	    {
	      if (integer_zerop (t))
		return boolean_false_node;
	      else if (partial)
		{
		  /* We already got a simplification for the other
		     operand to the redistributed AND expression.  The
		     interesting case is when at least one is true.
		     Or, if both are the same, we can apply the identity
		     (x AND x) == x.  */
		  if (integer_onep (partial))
		    return t;
		  else if (integer_onep (t))
		    return partial;
		  else if (same_bool_result_p (t, partial))
		    return t;
		}
	    }
	}
    }
  return NULL_TREE;
}

/* Try to simplify the OR of two comparisons defined by
   (OP1A CODE1 OP1B) and (OP2A CODE2 OP2B), respectively.
   If this can be done without constructing an intermediate value,
   return the resulting tree; otherwise NULL_TREE is returned.
   This function is deliberately asymmetric as it recurses on SSA_DEFs
   in the first comparison but not the second.  */

static tree
or_comparisons_1 (enum tree_code code1, tree op1a, tree op1b,
		  enum tree_code code2, tree op2a, tree op2b)
{
  /* First check for ((x CODE1 y) OR (x CODE2 y)).  */
  if (operand_equal_p (op1a, op2a, 0)
      && operand_equal_p (op1b, op2b, 0))
    {
      /* Result will be either NULL_TREE, or a combined comparison.  */
      tree t = combine_comparisons (UNKNOWN_LOCATION,
				    TRUTH_ORIF_EXPR, code1, code2,
				    boolean_type_node, op1a, op1b);
      if (t)
	return t;
    }

  /* Likewise the swapped case of the above.  */
  if (operand_equal_p (op1a, op2b, 0)
      && operand_equal_p (op1b, op2a, 0))
    {
      /* Result will be either NULL_TREE, or a combined comparison.  */
      tree t = combine_comparisons (UNKNOWN_LOCATION,
				    TRUTH_ORIF_EXPR, code1,
				    swap_tree_comparison (code2),
				    boolean_type_node, op1a, op1b);
      if (t)
	return t;
    }

  /* If both comparisons are of the same value against constants, we might
     be able to merge them.  */
  if (operand_equal_p (op1a, op2a, 0)
      && TREE_CODE (op1b) == INTEGER_CST
      && TREE_CODE (op2b) == INTEGER_CST)
    {
      int cmp = tree_int_cst_compare (op1b, op2b);

      /* If we have (op1a != op1b), we should either be able to
	 return that or TRUE, depending on whether the constant op1b
	 also satisfies the other comparison against op2b.  */
      if (code1 == NE_EXPR)
	{
	  bool done = true;
	  bool val;
	  switch (code2)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp < 0); break;
	    case GT_EXPR: val = (cmp > 0); break;
	    case LE_EXPR: val = (cmp <= 0); break;
	    case GE_EXPR: val = (cmp >= 0); break;
	    default: done = false;
	    }
	  if (done)
	    {
	      if (val)
		return boolean_true_node;
	      else
		return fold_build2 (code1, boolean_type_node, op1a, op1b);
	    }
	}
      /* Likewise if the second comparison is a != comparison.  */
      else if (code2 == NE_EXPR)
	{
	  bool done = true;
	  bool val;
	  switch (code1)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp > 0); break;
	    case GT_EXPR: val = (cmp < 0); break;
	    case LE_EXPR: val = (cmp >= 0); break;
	    case GE_EXPR: val = (cmp <= 0); break;
	    default: done = false;
	    }
	  if (done)
	    {
	      if (val)
		return boolean_true_node;
	      else
		return fold_build2 (code2, boolean_type_node, op2a, op2b);
	    }
	}

      /* See if an equality test is redundant with the other comparison.  */
      else if (code1 == EQ_EXPR)
	{
	  bool val;
	  switch (code2)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp < 0); break;
	    case GT_EXPR: val = (cmp > 0); break;
	    case LE_EXPR: val = (cmp <= 0); break;
	    case GE_EXPR: val = (cmp >= 0); break;
	    default:
	      val = false;
	    }
	  if (val)
	    return fold_build2 (code2, boolean_type_node, op2a, op2b);
	}
      else if (code2 == EQ_EXPR)
	{
	  bool val;
	  switch (code1)
	    {
	    case EQ_EXPR: val = (cmp == 0); break;
	    case NE_EXPR: val = (cmp != 0); break;
	    case LT_EXPR: val = (cmp > 0); break;
	    case GT_EXPR: val = (cmp < 0); break;
	    case LE_EXPR: val = (cmp >= 0); break;
	    case GE_EXPR: val = (cmp <= 0); break;
	    default:
	      val = false;
	    }
	  if (val)
	    return fold_build2 (code1, boolean_type_node, op1a, op1b);
	}

      /* Chose the less restrictive of two < or <= comparisons.  */
      else if ((code1 == LT_EXPR || code1 == LE_EXPR)
	       && (code2 == LT_EXPR || code2 == LE_EXPR))
	{
	  if ((cmp < 0) || (cmp == 0 && code1 == LT_EXPR))
	    return fold_build2 (code2, boolean_type_node, op2a, op2b);
	  else
	    return fold_build2 (code1, boolean_type_node, op1a, op1b);
	}

      /* Likewise chose the less restrictive of two > or >= comparisons.  */
      else if ((code1 == GT_EXPR || code1 == GE_EXPR)
	       && (code2 == GT_EXPR || code2 == GE_EXPR))
	{
	  if ((cmp > 0) || (cmp == 0 && code1 == GT_EXPR))
	    return fold_build2 (code2, boolean_type_node, op2a, op2b);
	  else
	    return fold_build2 (code1, boolean_type_node, op1a, op1b);
	}

      /* Check for singleton ranges.  */
      else if (cmp == 0
	       && ((code1 == LT_EXPR && code2 == GT_EXPR)
		   || (code1 == GT_EXPR && code2 == LT_EXPR)))
	return fold_build2 (NE_EXPR, boolean_type_node, op1a, op2b);

      /* Check for less/greater pairs that don't restrict the range at all.  */
      else if (cmp >= 0
	       && (code1 == LT_EXPR || code1 == LE_EXPR)
	       && (code2 == GT_EXPR || code2 == GE_EXPR))
	return boolean_true_node;
      else if (cmp <= 0
	       && (code1 == GT_EXPR || code1 == GE_EXPR)
	       && (code2 == LT_EXPR || code2 == LE_EXPR))
	return boolean_true_node;
    }

  /* Perhaps the first comparison is (NAME != 0) or (NAME == 1) where
     NAME's definition is a truth value.  See if there are any simplifications
     that can be done against the NAME's definition.  */
  if (TREE_CODE (op1a) == SSA_NAME
      && (code1 == NE_EXPR || code1 == EQ_EXPR)
      && (integer_zerop (op1b) || integer_onep (op1b)))
    {
      bool invert = ((code1 == EQ_EXPR && integer_zerop (op1b))
		     || (code1 == NE_EXPR && integer_onep (op1b)));
      gimple stmt = SSA_NAME_DEF_STMT (op1a);
      switch (gimple_code (stmt))
	{
	case GIMPLE_ASSIGN:
	  /* Try to simplify by copy-propagating the definition.  */
	  return or_var_with_comparison (op1a, invert, code2, op2a, op2b);

	case GIMPLE_PHI:
	  /* If every argument to the PHI produces the same result when
	     ORed with the second comparison, we win.
	     Do not do this unless the type is bool since we need a bool
	     result here anyway.  */
	  if (TREE_CODE (TREE_TYPE (op1a)) == BOOLEAN_TYPE)
	    {
	      tree result = NULL_TREE;
	      unsigned i;
	      for (i = 0; i < gimple_phi_num_args (stmt); i++)
		{
		  tree arg = gimple_phi_arg_def (stmt, i);
		  
		  /* If this PHI has itself as an argument, ignore it.
		     If all the other args produce the same result,
		     we're still OK.  */
		  if (arg == gimple_phi_result (stmt))
		    continue;
		  else if (TREE_CODE (arg) == INTEGER_CST)
		    {
		      if (invert ? integer_zerop (arg) : integer_nonzerop (arg))
			{
			  if (!result)
			    result = boolean_true_node;
			  else if (!integer_onep (result))
			    return NULL_TREE;
			}
		      else if (!result)
			result = fold_build2 (code2, boolean_type_node,
					      op2a, op2b);
		      else if (!same_bool_comparison_p (result,
							code2, op2a, op2b))
			return NULL_TREE;
		    }
		  else if (TREE_CODE (arg) == SSA_NAME
			   && !SSA_NAME_IS_DEFAULT_DEF (arg))
		    {
		      tree temp;
		      gimple def_stmt = SSA_NAME_DEF_STMT (arg);
		      /* In simple cases we can look through PHI nodes,
			 but we have to be careful with loops.
			 See PR49073.  */
		      if (! dom_info_available_p (CDI_DOMINATORS)
			  || gimple_bb (def_stmt) == gimple_bb (stmt)
			  || dominated_by_p (CDI_DOMINATORS,
					     gimple_bb (def_stmt),
					     gimple_bb (stmt)))
			return NULL_TREE;
		      temp = or_var_with_comparison (arg, invert, code2,
						     op2a, op2b);
		      if (!temp)
			return NULL_TREE;
		      else if (!result)
			result = temp;
		      else if (!same_bool_result_p (result, temp))
			return NULL_TREE;
		    }
		  else
		    return NULL_TREE;
		}
	      return result;
	    }

	default:
	  break;
	}
    }
  return NULL_TREE;
}

/* Try to simplify the OR of two comparisons, specified by
   (OP1A CODE1 OP1B) and (OP2B CODE2 OP2B), respectively.
   If this can be simplified to a single expression (without requiring
   introducing more SSA variables to hold intermediate values),
   return the resulting tree.  Otherwise return NULL_TREE.
   If the result expression is non-null, it has boolean type.  */

tree
maybe_fold_or_comparisons (enum tree_code code1, tree op1a, tree op1b,
			   enum tree_code code2, tree op2a, tree op2b)
{
  tree t = or_comparisons_1 (code1, op1a, op1b, code2, op2a, op2b);
  if (t)
    return t;
  else
    return or_comparisons_1 (code2, op2a, op2b, code1, op1a, op1b);
}


/* Fold STMT to a constant using VALUEIZE to valueize SSA names.

   Either NULL_TREE, a simplified but non-constant or a constant
   is returned.

   ???  This should go into a gimple-fold-inline.h file to be eventually
   privatized with the single valueize function used in the various TUs
   to avoid the indirect function call overhead.  */

tree
gimple_fold_stmt_to_constant_1 (gimple stmt, tree (*valueize) (tree))
{
  location_t loc = gimple_location (stmt);
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      {
        enum tree_code subcode = gimple_assign_rhs_code (stmt);

        switch (get_gimple_rhs_class (subcode))
          {
          case GIMPLE_SINGLE_RHS:
            {
              tree rhs = gimple_assign_rhs1 (stmt);
              enum tree_code_class kind = TREE_CODE_CLASS (subcode);

              if (TREE_CODE (rhs) == SSA_NAME)
                {
                  /* If the RHS is an SSA_NAME, return its known constant value,
                     if any.  */
                  return (*valueize) (rhs);
                }
	      /* Handle propagating invariant addresses into address
		 operations.  */
	      else if (TREE_CODE (rhs) == ADDR_EXPR
		       && !is_gimple_min_invariant (rhs))
		{
		  HOST_WIDE_INT offset;
		  tree base;
		  base = get_addr_base_and_unit_offset_1 (TREE_OPERAND (rhs, 0),
							  &offset,
							  valueize);
		  if (base
		      && (CONSTANT_CLASS_P (base)
			  || decl_address_invariant_p (base)))
		    return build_invariant_address (TREE_TYPE (rhs),
						    base, offset);
		}
	      else if (TREE_CODE (rhs) == CONSTRUCTOR
		       && TREE_CODE (TREE_TYPE (rhs)) == VECTOR_TYPE
		       && (CONSTRUCTOR_NELTS (rhs)
			   == TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs))))
		{
		  unsigned i;
		  tree val, list;

		  list = NULL_TREE;
		  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), i, val)
		    {
		      val = (*valueize) (val);
		      if (TREE_CODE (val) == INTEGER_CST
			  || TREE_CODE (val) == REAL_CST
			  || TREE_CODE (val) == FIXED_CST)
			list = tree_cons (NULL_TREE, val, list);
		      else
			return NULL_TREE;
		    }

		  return build_vector (TREE_TYPE (rhs), nreverse (list));
		}

              if (kind == tcc_reference)
		{
		  if ((TREE_CODE (rhs) == VIEW_CONVERT_EXPR
		       || TREE_CODE (rhs) == REALPART_EXPR
		       || TREE_CODE (rhs) == IMAGPART_EXPR)
		      && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
		    {
		      tree val = (*valueize) (TREE_OPERAND (rhs, 0));
		      return fold_unary_loc (EXPR_LOCATION (rhs),
					     TREE_CODE (rhs),
					     TREE_TYPE (rhs), val);
		    }
		  else if (TREE_CODE (rhs) == BIT_FIELD_REF
			   && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
		    {
		      tree val = (*valueize) (TREE_OPERAND (rhs, 0));
		      return fold_ternary_loc (EXPR_LOCATION (rhs),
					       TREE_CODE (rhs),
					       TREE_TYPE (rhs), val,
					       TREE_OPERAND (rhs, 1),
					       TREE_OPERAND (rhs, 2));
		    }
		  else if (TREE_CODE (rhs) == MEM_REF
			   && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
		    {
		      tree val = (*valueize) (TREE_OPERAND (rhs, 0));
		      if (TREE_CODE (val) == ADDR_EXPR
			  && is_gimple_min_invariant (val))
			{
			  tree tem = fold_build2 (MEM_REF, TREE_TYPE (rhs),
						  unshare_expr (val),
						  TREE_OPERAND (rhs, 1));
			  if (tem)
			    rhs = tem;
			}
		    }
		  return fold_const_aggregate_ref_1 (rhs, valueize);
		}
              else if (kind == tcc_declaration)
                return get_symbol_constant_value (rhs);
              return rhs;
            }

          case GIMPLE_UNARY_RHS:
            {
              /* Handle unary operators that can appear in GIMPLE form.
                 Note that we know the single operand must be a constant,
                 so this should almost always return a simplified RHS.  */
	      tree lhs = gimple_assign_lhs (stmt);
              tree op0 = (*valueize) (gimple_assign_rhs1 (stmt));

	      /* Conversions are useless for CCP purposes if they are
		 value-preserving.  Thus the restrictions that
		 useless_type_conversion_p places for restrict qualification
		 of pointer types should not apply here.
		 Substitution later will only substitute to allowed places.  */
	      if (CONVERT_EXPR_CODE_P (subcode)
		  && POINTER_TYPE_P (TREE_TYPE (lhs))
		  && POINTER_TYPE_P (TREE_TYPE (op0))
		  && TYPE_ADDR_SPACE (TREE_TYPE (lhs))
		     == TYPE_ADDR_SPACE (TREE_TYPE (op0))
		  && TYPE_MODE (TREE_TYPE (lhs))
		     == TYPE_MODE (TREE_TYPE (op0)))
		return op0;

              return
		fold_unary_ignore_overflow_loc (loc, subcode,
						gimple_expr_type (stmt), op0);
            }

          case GIMPLE_BINARY_RHS:
            {
              /* Handle binary operators that can appear in GIMPLE form.  */
              tree op0 = (*valueize) (gimple_assign_rhs1 (stmt));
              tree op1 = (*valueize) (gimple_assign_rhs2 (stmt));

	      /* Translate &x + CST into an invariant form suitable for
	         further propagation.  */
	      if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
		  && TREE_CODE (op0) == ADDR_EXPR
		  && TREE_CODE (op1) == INTEGER_CST)
		{
		  tree off = fold_convert (ptr_type_node, op1);
		  return build_fold_addr_expr_loc
			   (loc,
			    fold_build2 (MEM_REF,
					 TREE_TYPE (TREE_TYPE (op0)),
					 unshare_expr (op0), off));
		}

              return fold_binary_loc (loc, subcode,
				      gimple_expr_type (stmt), op0, op1);
            }

          case GIMPLE_TERNARY_RHS:
            {
              /* Handle ternary operators that can appear in GIMPLE form.  */
              tree op0 = (*valueize) (gimple_assign_rhs1 (stmt));
              tree op1 = (*valueize) (gimple_assign_rhs2 (stmt));
              tree op2 = (*valueize) (gimple_assign_rhs3 (stmt));

	      /* Fold embedded expressions in ternary codes.  */
	      if ((subcode == COND_EXPR
		   || subcode == VEC_COND_EXPR)
		  && COMPARISON_CLASS_P (op0))
		{
		  tree op00 = (*valueize) (TREE_OPERAND (op0, 0));
		  tree op01 = (*valueize) (TREE_OPERAND (op0, 1));
		  tree tem = fold_binary_loc (loc, TREE_CODE (op0),
					      TREE_TYPE (op0), op00, op01);
		  if (tem)
		    op0 = tem;
		}

              return fold_ternary_loc (loc, subcode,
				       gimple_expr_type (stmt), op0, op1, op2);
            }

          default:
            gcc_unreachable ();
          }
      }

    case GIMPLE_CALL:
      {
	tree fn;

	if (gimple_call_internal_p (stmt))
	  /* No folding yet for these functions.  */
	  return NULL_TREE;

	fn = (*valueize) (gimple_call_fn (stmt));
	if (TREE_CODE (fn) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
	    && DECL_BUILT_IN (TREE_OPERAND (fn, 0)))
	  {
	    tree *args = XALLOCAVEC (tree, gimple_call_num_args (stmt));
	    tree call, retval;
	    unsigned i;
	    for (i = 0; i < gimple_call_num_args (stmt); ++i)
	      args[i] = (*valueize) (gimple_call_arg (stmt, i));
	    call = build_call_array_loc (loc,
					 gimple_call_return_type (stmt),
					 fn, gimple_call_num_args (stmt), args);
	    retval = fold_call_expr (EXPR_LOCATION (call), call, false);
	    if (retval)
	      /* fold_call_expr wraps the result inside a NOP_EXPR.  */
	      STRIP_NOPS (retval);
	    return retval;
	  }
	return NULL_TREE;
      }

    default:
      return NULL_TREE;
    }
}

/* Fold STMT to a constant using VALUEIZE to valueize SSA names.
   Returns NULL_TREE if folding to a constant is not possible, otherwise
   returns a constant according to is_gimple_min_invariant.  */

tree
gimple_fold_stmt_to_constant (gimple stmt, tree (*valueize) (tree))
{
  tree res = gimple_fold_stmt_to_constant_1 (stmt, valueize);
  if (res && is_gimple_min_invariant (res))
    return res;
  return NULL_TREE;
}


/* The following set of functions are supposed to fold references using
   their constant initializers.  */

static tree fold_ctor_reference (tree type, tree ctor,
				 unsigned HOST_WIDE_INT offset,
				 unsigned HOST_WIDE_INT size);

/* See if we can find constructor defining value of BASE.
   When we know the consructor with constant offset (such as
   base is array[40] and we do know constructor of array), then
   BIT_OFFSET is adjusted accordingly.

   As a special case, return error_mark_node when constructor
   is not explicitly available, but it is known to be zero
   such as 'static const int a;'.  */
static tree
get_base_constructor (tree base, HOST_WIDE_INT *bit_offset,
		      tree (*valueize)(tree))
{
  HOST_WIDE_INT bit_offset2, size, max_size;
  if (TREE_CODE (base) == MEM_REF)
    {
      if (!integer_zerop (TREE_OPERAND (base, 1)))
	{
	  if (!host_integerp (TREE_OPERAND (base, 1), 0))
	    return NULL_TREE;
	  *bit_offset += (mem_ref_offset (base).low
			  * BITS_PER_UNIT);
	}

      if (valueize
	  && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
	base = valueize (TREE_OPERAND (base, 0));
      if (!base || TREE_CODE (base) != ADDR_EXPR)
        return NULL_TREE;
      base = TREE_OPERAND (base, 0);
    }

  /* Get a CONSTRUCTOR.  If BASE is a VAR_DECL, get its
     DECL_INITIAL.  If BASE is a nested reference into another
     ARRAY_REF or COMPONENT_REF, make a recursive call to resolve
     the inner reference.  */
  switch (TREE_CODE (base))
    {
    case VAR_DECL:
      if (!const_value_known_p (base))
	return NULL_TREE;

      /* Fallthru.  */
    case CONST_DECL:
      if (!DECL_INITIAL (base)
	  && (TREE_STATIC (base) || DECL_EXTERNAL (base)))
        return error_mark_node;
      return DECL_INITIAL (base);

    case ARRAY_REF:
    case COMPONENT_REF:
      base = get_ref_base_and_extent (base, &bit_offset2, &size, &max_size);
      if (max_size == -1 || size != max_size)
	return NULL_TREE;
      *bit_offset +=  bit_offset2;
      return get_base_constructor (base, bit_offset, valueize);

    case STRING_CST:
    case CONSTRUCTOR:
      return base;

    default:
      return NULL_TREE;
    }
}

/* CTOR is STRING_CST.  Fold reference of type TYPE and size SIZE
   to the memory at bit OFFSET.

   We do only simple job of folding byte accesses.  */

static tree
fold_string_cst_ctor_reference (tree type, tree ctor,
				unsigned HOST_WIDE_INT offset,
				unsigned HOST_WIDE_INT size)
{
  if (INTEGRAL_TYPE_P (type)
      && (TYPE_MODE (type)
	  == TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
      && (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
	  == MODE_INT)
      && GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor)))) == 1
      && size == BITS_PER_UNIT
      && !(offset % BITS_PER_UNIT))
    {
      offset /= BITS_PER_UNIT;
      if (offset < (unsigned HOST_WIDE_INT) TREE_STRING_LENGTH (ctor))
	return build_int_cst_type (type, (TREE_STRING_POINTER (ctor)
				   [offset]));
      /* Folding
	 const char a[20]="hello";
	 return a[10];

	 might lead to offset greater than string length.  In this case we
	 know value is either initialized to 0 or out of bounds.  Return 0
	 in both cases.  */
      return build_zero_cst (type);
    }
  return NULL_TREE;
}

/* CTOR is CONSTRUCTOR of an array type.  Fold reference of type TYPE and size
   SIZE to the memory at bit OFFSET.  */

static tree
fold_array_ctor_reference (tree type, tree ctor,
			   unsigned HOST_WIDE_INT offset,
			   unsigned HOST_WIDE_INT size)
{
  unsigned HOST_WIDE_INT cnt;
  tree cfield, cval;
  double_int low_bound, elt_size;
  double_int index, max_index;
  double_int access_index;
  tree domain_type = NULL_TREE;
  HOST_WIDE_INT inner_offset;

  /* Compute low bound and elt size.  */
  if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE)
    domain_type = TYPE_DOMAIN (TREE_TYPE (ctor));
  if (domain_type && TYPE_MIN_VALUE (domain_type))
    {
      /* Static constructors for variably sized objects makes no sense.  */
      gcc_assert (TREE_CODE (TYPE_MIN_VALUE (domain_type)) == INTEGER_CST);
      low_bound = tree_to_double_int (TYPE_MIN_VALUE (domain_type));
    }
  else
    low_bound = double_int_zero;
  /* Static constructors for variably sized objects makes no sense.  */
  gcc_assert (TREE_CODE(TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ctor))))
	      == INTEGER_CST);
  elt_size =
    tree_to_double_int (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ctor))));


  /* We can handle only constantly sized accesses that are known to not
     be larger than size of array element.  */
  if (!TYPE_SIZE_UNIT (type)
      || TREE_CODE (TYPE_SIZE_UNIT (type)) != INTEGER_CST
      || double_int_cmp (elt_size,
			 tree_to_double_int (TYPE_SIZE_UNIT (type)), 0) < 0)
    return NULL_TREE;

  /* Compute the array index we look for.  */
  access_index = double_int_udiv (uhwi_to_double_int (offset / BITS_PER_UNIT),
				  elt_size, TRUNC_DIV_EXPR);
  access_index = double_int_add (access_index, low_bound);

  /* And offset within the access.  */
  inner_offset = offset % (double_int_to_uhwi (elt_size) * BITS_PER_UNIT);

  /* See if the array field is large enough to span whole access.  We do not
     care to fold accesses spanning multiple array indexes.  */
  if (inner_offset + size > double_int_to_uhwi (elt_size) * BITS_PER_UNIT)
    return NULL_TREE;

  index = double_int_sub (low_bound, double_int_one);
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield, cval)
    {
      /* Array constructor might explicitely set index, or specify range
	 or leave index NULL meaning that it is next index after previous
	 one.  */
      if (cfield)
	{
	  if (TREE_CODE (cfield) == INTEGER_CST)
	    max_index = index = tree_to_double_int (cfield);
	  else
	    {
	      gcc_assert (TREE_CODE (cfield) == RANGE_EXPR);
	      index = tree_to_double_int (TREE_OPERAND (cfield, 0));
	      max_index = tree_to_double_int (TREE_OPERAND (cfield, 1));
	    }
	}
      else
	max_index = index = double_int_add (index, double_int_one);

      /* Do we have match?  */
      if (double_int_cmp (access_index, index, 1) >= 0
	  && double_int_cmp (access_index, max_index, 1) <= 0)
	return fold_ctor_reference (type, cval, inner_offset, size);
    }
  /* When memory is not explicitely mentioned in constructor,
     it is 0 (or out of range).  */
  return build_zero_cst (type);
}

/* CTOR is CONSTRUCTOR of an aggregate or vector.
   Fold reference of type TYPE and size SIZE to the memory at bit OFFSET.  */

static tree
fold_nonarray_ctor_reference (tree type, tree ctor,
			      unsigned HOST_WIDE_INT offset,
			      unsigned HOST_WIDE_INT size)
{
  unsigned HOST_WIDE_INT cnt;
  tree cfield, cval;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield,
			    cval)
    {
      tree byte_offset = DECL_FIELD_OFFSET (cfield);
      tree field_offset = DECL_FIELD_BIT_OFFSET (cfield);
      tree field_size = DECL_SIZE (cfield);
      double_int bitoffset;
      double_int byte_offset_cst = tree_to_double_int (byte_offset);
      double_int bits_per_unit_cst = uhwi_to_double_int (BITS_PER_UNIT);
      double_int bitoffset_end, access_end;

      /* Variable sized objects in static constructors makes no sense,
	 but field_size can be NULL for flexible array members.  */
      gcc_assert (TREE_CODE (field_offset) == INTEGER_CST
		  && TREE_CODE (byte_offset) == INTEGER_CST
		  && (field_size != NULL_TREE
		      ? TREE_CODE (field_size) == INTEGER_CST
		      : TREE_CODE (TREE_TYPE (cfield)) == ARRAY_TYPE));

      /* Compute bit offset of the field.  */
      bitoffset = double_int_add (tree_to_double_int (field_offset),
				  double_int_mul (byte_offset_cst,
						  bits_per_unit_cst));
      /* Compute bit offset where the field ends.  */
      if (field_size != NULL_TREE)
	bitoffset_end = double_int_add (bitoffset,
					tree_to_double_int (field_size));
      else
	bitoffset_end = double_int_zero;

      access_end = double_int_add (uhwi_to_double_int (offset),
				   uhwi_to_double_int (size));

      /* Is there any overlap between [OFFSET, OFFSET+SIZE) and
	 [BITOFFSET, BITOFFSET_END)?  */
      if (double_int_cmp (access_end, bitoffset, 0) > 0
	  && (field_size == NULL_TREE
	      || double_int_cmp (uhwi_to_double_int (offset),
				 bitoffset_end, 0) < 0))
	{
	  double_int inner_offset = double_int_sub (uhwi_to_double_int (offset),
						    bitoffset);
	  /* We do have overlap.  Now see if field is large enough to
	     cover the access.  Give up for accesses spanning multiple
	     fields.  */
	  if (double_int_cmp (access_end, bitoffset_end, 0) > 0)
	    return NULL_TREE;
	  if (double_int_cmp (uhwi_to_double_int (offset), bitoffset, 0) < 0)
	    return NULL_TREE;
	  return fold_ctor_reference (type, cval,
				      double_int_to_uhwi (inner_offset), size);
	}
    }
  /* When memory is not explicitely mentioned in constructor, it is 0.  */
  return build_zero_cst (type);
}

/* CTOR is value initializing memory, fold reference of type TYPE and size SIZE
   to the memory at bit OFFSET.  */

static tree
fold_ctor_reference (tree type, tree ctor, unsigned HOST_WIDE_INT offset,
		     unsigned HOST_WIDE_INT size)
{
  tree ret;

  /* We found the field with exact match.  */
  if (useless_type_conversion_p (type, TREE_TYPE (ctor))
      && !offset)
    return canonicalize_constructor_val (ctor);

  /* We are at the end of walk, see if we can view convert the
     result.  */
  if (!AGGREGATE_TYPE_P (TREE_TYPE (ctor)) && !offset
      /* VIEW_CONVERT_EXPR is defined only for matching sizes.  */
      && operand_equal_p (TYPE_SIZE (type),
			  TYPE_SIZE (TREE_TYPE (ctor)), 0))
    {
      ret = canonicalize_constructor_val (ctor);
      ret = fold_unary (VIEW_CONVERT_EXPR, type, ret);
      if (ret)
	STRIP_NOPS (ret);
      return ret;
    }
  if (TREE_CODE (ctor) == STRING_CST)
    return fold_string_cst_ctor_reference (type, ctor, offset, size);
  if (TREE_CODE (ctor) == CONSTRUCTOR)
    {

      if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (ctor)) == VECTOR_TYPE)
	return fold_array_ctor_reference (type, ctor, offset, size);
      else
	return fold_nonarray_ctor_reference (type, ctor, offset, size);
    }

  return NULL_TREE;
}

/* Return the tree representing the element referenced by T if T is an
   ARRAY_REF or COMPONENT_REF into constant aggregates valuezing SSA
   names using VALUEIZE.  Return NULL_TREE otherwise.  */

tree
fold_const_aggregate_ref_1 (tree t, tree (*valueize) (tree))
{
  tree ctor, idx, base;
  HOST_WIDE_INT offset, size, max_size;
  tree tem;

  if (TREE_THIS_VOLATILE (t))
    return NULL_TREE;

  if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_declaration)
    return get_symbol_constant_value (t);

  tem = fold_read_from_constant_string (t);
  if (tem)
    return tem;

  switch (TREE_CODE (t))
    {
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      /* Constant indexes are handled well by get_base_constructor.
	 Only special case variable offsets.
	 FIXME: This code can't handle nested references with variable indexes
	 (they will be handled only by iteration of ccp).  Perhaps we can bring
	 get_ref_base_and_extent here and make it use a valueize callback.  */
      if (TREE_CODE (TREE_OPERAND (t, 1)) == SSA_NAME
	  && valueize
	  && (idx = (*valueize) (TREE_OPERAND (t, 1)))
	  && host_integerp (idx, 0))
	{
	  tree low_bound, unit_size;

	  /* If the resulting bit-offset is constant, track it.  */
	  if ((low_bound = array_ref_low_bound (t),
	       host_integerp (low_bound, 0))
	      && (unit_size = array_ref_element_size (t),
		  host_integerp (unit_size, 1)))
	    {
	      offset = TREE_INT_CST_LOW (idx);
	      offset -= TREE_INT_CST_LOW (low_bound);
	      offset *= TREE_INT_CST_LOW (unit_size);
	      offset *= BITS_PER_UNIT;

	      base = TREE_OPERAND (t, 0);
	      ctor = get_base_constructor (base, &offset, valueize);
	      /* Empty constructor.  Always fold to 0.  */
	      if (ctor == error_mark_node)
		return build_zero_cst (TREE_TYPE (t));
	      /* Out of bound array access.  Value is undefined,
		 but don't fold.  */
	      if (offset < 0)
		return NULL_TREE;
	      /* We can not determine ctor.  */
	      if (!ctor)
		return NULL_TREE;
	      return fold_ctor_reference (TREE_TYPE (t), ctor, offset,
					  TREE_INT_CST_LOW (unit_size)
					  * BITS_PER_UNIT);
	    }
	}
      /* Fallthru.  */

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case TARGET_MEM_REF:
    case MEM_REF:
      base = get_ref_base_and_extent (t, &offset, &size, &max_size);
      ctor = get_base_constructor (base, &offset, valueize);

      /* Empty constructor.  Always fold to 0.  */
      if (ctor == error_mark_node)
	return build_zero_cst (TREE_TYPE (t));
      /* We do not know precise address.  */
      if (max_size == -1 || max_size != size)
	return NULL_TREE;
      /* We can not determine ctor.  */
      if (!ctor)
	return NULL_TREE;

      /* Out of bound array access.  Value is undefined, but don't fold.  */
      if (offset < 0)
	return NULL_TREE;

      return fold_ctor_reference (TREE_TYPE (t), ctor, offset, size);

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
	tree c = fold_const_aggregate_ref_1 (TREE_OPERAND (t, 0), valueize);
	if (c && TREE_CODE (c) == COMPLEX_CST)
	  return fold_build1_loc (EXPR_LOCATION (t),
			      TREE_CODE (t), TREE_TYPE (t), c);
	break;
      }

    default:
      break;
    }

  return NULL_TREE;
}

tree
fold_const_aggregate_ref (tree t)
{
  return fold_const_aggregate_ref_1 (t, NULL);
}

/* Return a declaration of a function which an OBJ_TYPE_REF references. TOKEN
   is integer form of OBJ_TYPE_REF_TOKEN of the reference expression.
   KNOWN_BINFO carries the binfo describing the true type of
   OBJ_TYPE_REF_OBJECT(REF).  */

tree
gimple_get_virt_method_for_binfo (HOST_WIDE_INT token, tree known_binfo)
{
  unsigned HOST_WIDE_INT offset, size;
  tree v, fn;

  v = BINFO_VTABLE (known_binfo);
  /* If there is no virtual methods table, leave the OBJ_TYPE_REF alone.  */
  if (!v)
    return NULL_TREE;

  if (TREE_CODE (v) == POINTER_PLUS_EXPR)
    {
      offset = tree_low_cst (TREE_OPERAND (v, 1), 1) * BITS_PER_UNIT;
      v = TREE_OPERAND (v, 0);
    }
  else
    offset = 0;

  if (TREE_CODE (v) != ADDR_EXPR)
    return NULL_TREE;
  v = TREE_OPERAND (v, 0);

  if (TREE_CODE (v) != VAR_DECL
      || !DECL_VIRTUAL_P (v)
      || !DECL_INITIAL (v)
      || DECL_INITIAL (v) == error_mark_node)
    return NULL_TREE;
  gcc_checking_assert (TREE_CODE (TREE_TYPE (v)) == ARRAY_TYPE);
  size = tree_low_cst (TYPE_SIZE (TREE_TYPE (TREE_TYPE (v))), 1);
  offset += token * size;
  fn = fold_ctor_reference (TREE_TYPE (TREE_TYPE (v)), DECL_INITIAL (v),
			    offset, size);
  if (!fn)
    return NULL_TREE;
  gcc_assert (TREE_CODE (fn) == ADDR_EXPR
	      || TREE_CODE (fn) == FDESC_EXPR);
  fn = TREE_OPERAND (fn, 0);
  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);

  /* When cgraph node is missing and function is not public, we cannot
     devirtualize.  This can happen in WHOPR when the actual method
     ends up in other partition, because we found devirtualization
     possibility too late.  */
  if (!can_refer_decl_in_current_unit_p (fn))
    return NULL_TREE;

  return fn;
}

/* Return true iff VAL is a gimple expression that is known to be
   non-negative.  Restricted to floating-point inputs.  */

bool
gimple_val_nonnegative_real_p (tree val)
{
  gimple def_stmt;

  gcc_assert (val && SCALAR_FLOAT_TYPE_P (TREE_TYPE (val)));

  /* Use existing logic for non-gimple trees.  */
  if (tree_expr_nonnegative_p (val))
    return true;

  if (TREE_CODE (val) != SSA_NAME)
    return false;

  /* Currently we look only at the immediately defining statement
     to make this determination, since recursion on defining 
     statements of operands can lead to quadratic behavior in the
     worst case.  This is expected to catch almost all occurrences
     in practice.  It would be possible to implement limited-depth
     recursion if important cases are lost.  Alternatively, passes
     that need this information (such as the pow/powi lowering code
     in the cse_sincos pass) could be revised to provide it through
     dataflow propagation.  */

  def_stmt = SSA_NAME_DEF_STMT (val);

  if (is_gimple_assign (def_stmt))
    {
      tree op0, op1;

      /* See fold-const.c:tree_expr_nonnegative_p for additional
	 cases that could be handled with recursion.  */

      switch (gimple_assign_rhs_code (def_stmt))
	{
	case ABS_EXPR:
	  /* Always true for floating-point operands.  */
	  return true;

	case MULT_EXPR:
	  /* True if the two operands are identical (since we are
	     restricted to floating-point inputs).  */
	  op0 = gimple_assign_rhs1 (def_stmt);
	  op1 = gimple_assign_rhs2 (def_stmt);

	  if (op0 == op1
	      || operand_equal_p (op0, op1, 0))
	    return true;

	default:
	  return false;
	}
    }
  else if (is_gimple_call (def_stmt))
    {
      tree fndecl = gimple_call_fndecl (def_stmt);
      if (fndecl
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	{
	  tree arg1;

	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    CASE_FLT_FN (BUILT_IN_ACOS):
	    CASE_FLT_FN (BUILT_IN_ACOSH):
	    CASE_FLT_FN (BUILT_IN_CABS):
	    CASE_FLT_FN (BUILT_IN_COSH):
	    CASE_FLT_FN (BUILT_IN_ERFC):
	    CASE_FLT_FN (BUILT_IN_EXP):
	    CASE_FLT_FN (BUILT_IN_EXP10):
	    CASE_FLT_FN (BUILT_IN_EXP2):
	    CASE_FLT_FN (BUILT_IN_FABS):
	    CASE_FLT_FN (BUILT_IN_FDIM):
	    CASE_FLT_FN (BUILT_IN_HYPOT):
	    CASE_FLT_FN (BUILT_IN_POW10):
	      return true;

	    CASE_FLT_FN (BUILT_IN_SQRT):
	      /* sqrt(-0.0) is -0.0, and sqrt is not defined over other
		 nonnegative inputs.  */
	      if (!HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (val))))
		return true;

	      break;

	    CASE_FLT_FN (BUILT_IN_POWI):
	      /* True if the second argument is an even integer.  */
	      arg1 = gimple_call_arg (def_stmt, 1);

	      if (TREE_CODE (arg1) == INTEGER_CST
		  && (TREE_INT_CST_LOW (arg1) & 1) == 0)
		return true;

	      break;
	      
	    CASE_FLT_FN (BUILT_IN_POW):
	      /* True if the second argument is an even integer-valued
		 real.  */
	      arg1 = gimple_call_arg (def_stmt, 1);

	      if (TREE_CODE (arg1) == REAL_CST)
		{
		  REAL_VALUE_TYPE c;
		  HOST_WIDE_INT n;

		  c = TREE_REAL_CST (arg1);
		  n = real_to_integer (&c);

		  if ((n & 1) == 0)
		    {
		      REAL_VALUE_TYPE cint;
		      real_from_integer (&cint, VOIDmode, n, n < 0 ? -1 : 0, 0);
		      if (real_identical (&c, &cint))
			return true;
		    }
		}

	      break;

	    default:
	      return false;
	    }
	}
    }

  return false;
}
