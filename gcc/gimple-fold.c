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
     produced.  */
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

/* CVAL is value taken from DECL_INITIAL of variable.  Try to transorm it into
   acceptable form for is_gimple_min_invariant.   */

tree
canonicalize_constructor_val (tree cval)
{
  STRIP_NOPS (cval);
  if (TREE_CODE (cval) == POINTER_PLUS_EXPR)
    {
      tree t = maybe_fold_offset_to_address (EXPR_LOCATION (cval),
					     TREE_OPERAND (cval, 0),
					     TREE_OPERAND (cval, 1),
					     TREE_TYPE (cval));
      if (t)
	cval = t;
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
	add_referenced_var (base);
      /* We never have the chance to fixup types in global initializers
         during gimplification.  Do so here.  */
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


/* Return true if we may propagate the address expression ADDR into the
   dereference DEREF and cancel them.  */

bool
may_propagate_address_into_dereference (tree addr, tree deref)
{
  gcc_assert (TREE_CODE (deref) == MEM_REF
	      && TREE_CODE (addr) == ADDR_EXPR);

  /* Don't propagate if ADDR's operand has incomplete type.  */
  if (!COMPLETE_TYPE_P (TREE_TYPE (TREE_OPERAND (addr, 0))))
    return false;

  /* If the address is invariant then we do not need to preserve restrict
     qualifications.  But we do need to preserve volatile qualifiers until
     we can annotate the folded dereference itself properly.  */
  if (is_gimple_min_invariant (addr)
      && (!TREE_THIS_VOLATILE (deref)
	  || TYPE_VOLATILE (TREE_TYPE (addr))))
    return useless_type_conversion_p (TREE_TYPE (deref),
				      TREE_TYPE (TREE_OPERAND (addr, 0)));

  /* Else both the address substitution and the folding must result in
     a valid useless type conversion sequence.  */
  return (useless_type_conversion_p (TREE_TYPE (TREE_OPERAND (deref, 0)),
				     TREE_TYPE (addr))
	  && useless_type_conversion_p (TREE_TYPE (deref),
					TREE_TYPE (TREE_OPERAND (addr, 0))));
}


/* A subroutine of fold_stmt.  Attempts to fold *(A+O) to A[X].
   BASE is an array type.  OFFSET is a byte displacement.

   LOC is the location of the original expression.  */

static tree
maybe_fold_offset_to_array_ref (location_t loc, tree base, tree offset)
{
  tree min_idx, idx, idx_type, elt_offset = integer_zero_node;
  tree array_type, elt_type, elt_size;
  tree domain_type;

  /* If BASE is an ARRAY_REF, we can pick up another offset (this time
     measured in units of the size of elements type) from that ARRAY_REF).
     We can't do anything if either is variable.

     The case we handle here is *(&A[N]+O).  */
  if (TREE_CODE (base) == ARRAY_REF)
    {
      tree low_bound = array_ref_low_bound (base);

      elt_offset = TREE_OPERAND (base, 1);
      if (TREE_CODE (low_bound) != INTEGER_CST
	  || TREE_CODE (elt_offset) != INTEGER_CST)
	return NULL_TREE;

      elt_offset = int_const_binop (MINUS_EXPR, elt_offset, low_bound, 0);
      base = TREE_OPERAND (base, 0);
    }

  /* Ignore stupid user tricks of indexing non-array variables.  */
  array_type = TREE_TYPE (base);
  if (TREE_CODE (array_type) != ARRAY_TYPE)
    return NULL_TREE;
  elt_type = TREE_TYPE (array_type);

  /* Use signed size type for intermediate computation on the index.  */
  idx_type = ssizetype;

  /* If OFFSET and ELT_OFFSET are zero, we don't care about the size of the
     element type (so we can use the alignment if it's not constant).
     Otherwise, compute the offset as an index by using a division.  If the
     division isn't exact, then don't do anything.  */
  elt_size = TYPE_SIZE_UNIT (elt_type);
  if (!elt_size)
    return NULL;
  if (integer_zerop (offset))
    {
      if (TREE_CODE (elt_size) != INTEGER_CST)
	elt_size = size_int (TYPE_ALIGN (elt_type));

      idx = build_int_cst (idx_type, 0);
    }
  else
    {
      unsigned HOST_WIDE_INT lquo, lrem;
      HOST_WIDE_INT hquo, hrem;
      double_int soffset;

      /* The final array offset should be signed, so we need
	 to sign-extend the (possibly pointer) offset here
	 and use signed division.  */
      soffset = double_int_sext (tree_to_double_int (offset),
				 TYPE_PRECISION (TREE_TYPE (offset)));
      if (TREE_CODE (elt_size) != INTEGER_CST
	  || div_and_round_double (TRUNC_DIV_EXPR, 0,
				   soffset.low, soffset.high,
				   TREE_INT_CST_LOW (elt_size),
				   TREE_INT_CST_HIGH (elt_size),
				   &lquo, &hquo, &lrem, &hrem)
	  || lrem || hrem)
	return NULL_TREE;

      idx = build_int_cst_wide (idx_type, lquo, hquo);
    }

  /* Assume the low bound is zero.  If there is a domain type, get the
     low bound, if any, convert the index into that type, and add the
     low bound.  */
  min_idx = build_int_cst (idx_type, 0);
  domain_type = TYPE_DOMAIN (array_type);
  if (domain_type)
    {
      idx_type = domain_type;
      if (TYPE_MIN_VALUE (idx_type))
	min_idx = TYPE_MIN_VALUE (idx_type);
      else
	min_idx = fold_convert (idx_type, min_idx);

      if (TREE_CODE (min_idx) != INTEGER_CST)
	return NULL_TREE;

      elt_offset = fold_convert (idx_type, elt_offset);
    }

  if (!integer_zerop (min_idx))
    idx = int_const_binop (PLUS_EXPR, idx, min_idx, 0);
  if (!integer_zerop (elt_offset))
    idx = int_const_binop (PLUS_EXPR, idx, elt_offset, 0);

  /* Make sure to possibly truncate late after offsetting.  */
  idx = fold_convert (idx_type, idx);

  /* We don't want to construct access past array bounds. For example
       char *(c[4]);
       c[3][2];
     should not be simplified into (*c)[14] or tree-vrp will
     give false warnings.
     This is only an issue for multi-dimensional arrays.  */
  if (TREE_CODE (elt_type) == ARRAY_TYPE
      && domain_type)
    {
      if (TYPE_MAX_VALUE (domain_type)
	  && TREE_CODE (TYPE_MAX_VALUE (domain_type)) == INTEGER_CST
	  && tree_int_cst_lt (TYPE_MAX_VALUE (domain_type), idx))
	return NULL_TREE;
      else if (TYPE_MIN_VALUE (domain_type)
	       && TREE_CODE (TYPE_MIN_VALUE (domain_type)) == INTEGER_CST
	       && tree_int_cst_lt (idx, TYPE_MIN_VALUE (domain_type)))
	return NULL_TREE;
      else if (compare_tree_int (idx, 0) < 0)
	return NULL_TREE;
    }

  {
    tree t = build4 (ARRAY_REF, elt_type, base, idx, NULL_TREE, NULL_TREE);
    SET_EXPR_LOCATION (t, loc);
    return t;
  }
}


/* Attempt to express (ORIG_TYPE)BASE+OFFSET as BASE[index].
   LOC is the location of original expression.

   Before attempting the conversion strip off existing ADDR_EXPRs.  */

tree
maybe_fold_offset_to_reference (location_t loc, tree base, tree offset,
				tree orig_type)
{
  tree ret;

  STRIP_NOPS (base);
  if (TREE_CODE (base) != ADDR_EXPR)
    return NULL_TREE;

  base = TREE_OPERAND (base, 0);
  if (types_compatible_p (orig_type, TREE_TYPE (base))
      && integer_zerop (offset))
    return base;

  ret = maybe_fold_offset_to_array_ref (loc, base, offset);
  if (ret && types_compatible_p (orig_type, TREE_TYPE (ret)))
    return ret;
  return NULL_TREE;
}

/* Attempt to express (ORIG_TYPE)ADDR+OFFSET as (*ADDR)[index].
   LOC is the location of the original expression.  */

tree
maybe_fold_offset_to_address (location_t loc, tree addr, tree offset,
			      tree orig_type)
{
  tree base, ret;

  STRIP_NOPS (addr);
  if (TREE_CODE (addr) != ADDR_EXPR)
    return NULL_TREE;
  base = TREE_OPERAND (addr, 0);
  ret = maybe_fold_offset_to_array_ref (loc, base, offset);
  if (ret)
    {
      ret = build_fold_addr_expr (ret);
      if (!useless_type_conversion_p (orig_type, TREE_TYPE (ret)))
	return NULL_TREE;
      SET_EXPR_LOCATION (ret, loc);
    }

  return ret;
}


/* A quaint feature extant in our address arithmetic is that there
   can be hidden type changes here.  The type of the result need
   not be the same as the type of the input pointer.

   What we're after here is an expression of the form
	(T *)(&array + const)
   where array is OP0, const is OP1, RES_TYPE is T and
   the cast doesn't actually exist, but is implicit in the
   type of the POINTER_PLUS_EXPR.  We'd like to turn this into
	&array[x]
   which may be able to propagate further.  */

tree
maybe_fold_stmt_addition (location_t loc, tree res_type, tree op0, tree op1)
{
  tree ptd_type;
  tree t;

  /* The first operand should be an ADDR_EXPR.  */
  if (TREE_CODE (op0) != ADDR_EXPR)
    return NULL_TREE;
  op0 = TREE_OPERAND (op0, 0);

  /* It had better be a constant.  */
  if (TREE_CODE (op1) != INTEGER_CST)
    {
      /* Or op0 should now be A[0] and the non-constant offset defined
	 via a multiplication by the array element size.  */
      if (TREE_CODE (op0) == ARRAY_REF
	  /* As we will end up creating a variable index array access
	     in the outermost array dimension make sure there isn't
	     a more inner array that the index could overflow to.  */
	  && TREE_CODE (TREE_OPERAND (op0, 0)) != ARRAY_REF
	  && integer_zerop (TREE_OPERAND (op0, 1))
	  && TREE_CODE (op1) == SSA_NAME)
	{
	  gimple offset_def = SSA_NAME_DEF_STMT (op1);
	  tree elsz = TYPE_SIZE_UNIT (TREE_TYPE (op0));
	  if (!host_integerp (elsz, 1)
	      || !is_gimple_assign (offset_def))
	    return NULL_TREE;

	  /* Do not build array references of something that we can't
	     see the true number of array dimensions for.  */
	  if (!DECL_P (TREE_OPERAND (op0, 0))
	      && !handled_component_p (TREE_OPERAND (op0, 0)))
	    return NULL_TREE;

	  if (gimple_assign_rhs_code (offset_def) == MULT_EXPR
	      && TREE_CODE (gimple_assign_rhs2 (offset_def)) == INTEGER_CST
	      && tree_int_cst_equal (gimple_assign_rhs2 (offset_def), elsz))
	    return build_fold_addr_expr
			  (build4 (ARRAY_REF, TREE_TYPE (op0),
				   TREE_OPERAND (op0, 0),
				   gimple_assign_rhs1 (offset_def),
				   TREE_OPERAND (op0, 2),
				   TREE_OPERAND (op0, 3)));
	  else if (integer_onep (elsz)
		   && gimple_assign_rhs_code (offset_def) != MULT_EXPR)
	    return build_fold_addr_expr
			  (build4 (ARRAY_REF, TREE_TYPE (op0),
				   TREE_OPERAND (op0, 0),
				   op1,
				   TREE_OPERAND (op0, 2),
				   TREE_OPERAND (op0, 3)));
	}
      else if (TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE
	       /* Dto.  */
	       && TREE_CODE (TREE_TYPE (TREE_TYPE (op0))) != ARRAY_TYPE
	       && TREE_CODE (op1) == SSA_NAME)
	{
	  gimple offset_def = SSA_NAME_DEF_STMT (op1);
	  tree elsz = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (op0)));
	  if (!host_integerp (elsz, 1)
	      || !is_gimple_assign (offset_def))
	    return NULL_TREE;

	  /* Do not build array references of something that we can't
	     see the true number of array dimensions for.  */
	  if (!DECL_P (op0)
	      && !handled_component_p (op0))
	    return NULL_TREE;

	  if (gimple_assign_rhs_code (offset_def) == MULT_EXPR
	      && TREE_CODE (gimple_assign_rhs2 (offset_def)) == INTEGER_CST
	      && tree_int_cst_equal (gimple_assign_rhs2 (offset_def), elsz))
	    return build_fold_addr_expr
			  (build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (op0)),
				   op0, gimple_assign_rhs1 (offset_def),
				   integer_zero_node, NULL_TREE));
	  else if (integer_onep (elsz)
		   && gimple_assign_rhs_code (offset_def) != MULT_EXPR)
	    return build_fold_addr_expr
			  (build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (op0)),
				   op0, op1,
				   integer_zero_node, NULL_TREE));
	}

      return NULL_TREE;
    }

  /* If the first operand is an ARRAY_REF, expand it so that we can fold
     the offset into it.  */
  while (TREE_CODE (op0) == ARRAY_REF)
    {
      tree array_obj = TREE_OPERAND (op0, 0);
      tree array_idx = TREE_OPERAND (op0, 1);
      tree elt_type = TREE_TYPE (op0);
      tree elt_size = TYPE_SIZE_UNIT (elt_type);
      tree min_idx;

      if (TREE_CODE (array_idx) != INTEGER_CST)
	break;
      if (TREE_CODE (elt_size) != INTEGER_CST)
	break;

      /* Un-bias the index by the min index of the array type.  */
      min_idx = TYPE_DOMAIN (TREE_TYPE (array_obj));
      if (min_idx)
	{
	  min_idx = TYPE_MIN_VALUE (min_idx);
	  if (min_idx)
	    {
	      if (TREE_CODE (min_idx) != INTEGER_CST)
		break;

	      array_idx = fold_convert (TREE_TYPE (min_idx), array_idx);
	      if (!integer_zerop (min_idx))
		array_idx = int_const_binop (MINUS_EXPR, array_idx,
					     min_idx, 0);
	    }
	}

      /* Convert the index to a byte offset.  */
      array_idx = fold_convert (sizetype, array_idx);
      array_idx = int_const_binop (MULT_EXPR, array_idx, elt_size, 0);

      /* Update the operands for the next round, or for folding.  */
      op1 = int_const_binop (PLUS_EXPR,
			     array_idx, op1, 0);
      op0 = array_obj;
    }

  ptd_type = TREE_TYPE (res_type);
  /* If we want a pointer to void, reconstruct the reference from the
     array element type.  A pointer to that can be trivially converted
     to void *.  This happens as we fold (void *)(ptr p+ off).  */
  if (VOID_TYPE_P (ptd_type)
      && TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE)
    ptd_type = TREE_TYPE (TREE_TYPE (op0));

  /* At which point we can try some of the same things as for indirects.  */
  t = maybe_fold_offset_to_array_ref (loc, op0, op1);
  if (t)
    {
      t = build_fold_addr_expr (t);
      if (!useless_type_conversion_p (res_type, TREE_TYPE (t)))
	return NULL_TREE;
      SET_EXPR_LOCATION (t, loc);
    }

  return t;
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

  if (!is_lhs
      && (result = fold_const_aggregate_ref (expr))
      && is_gimple_min_invariant (result))
    return result;

  /* ???  We might want to open-code the relevant remaining cases
     to avoid using the generic fold.  */
  if (handled_component_p (*t)
      && CONSTANT_CLASS_P (TREE_OPERAND (*t, 0)))
    {
      tree tem = fold (*t);
      if (tem != *t)
	return tem;
    }

  while (handled_component_p (*t))
    t = &TREE_OPERAND (*t, 0);

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
  /* Canonicalize MEM_REFs invariant address operand.  */
  else if (TREE_CODE (*t) == MEM_REF
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
  else if (!is_lhs
	   && DECL_P (*t))
    {
      tree tem = get_symbol_constant_value (*t);
      if (tem
	  && useless_type_conversion_p (TREE_TYPE (*t), TREE_TYPE (tem)))
	{
	  *t = unshare_expr (tem);
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

        /* Try to fold a conditional expression.  */
        if (TREE_CODE (rhs) == COND_EXPR)
          {
	    tree op0 = COND_EXPR_COND (rhs);
	    tree tem;
	    bool set = false;
	    location_t cond_loc = EXPR_LOCATION (rhs);

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
	      result = fold_build3_loc (cond_loc, COND_EXPR, TREE_TYPE (rhs), tem,
				    COND_EXPR_THEN (rhs), COND_EXPR_ELSE (rhs));
          }

	else if (REFERENCE_CLASS_P (rhs))
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
	else if (CONVERT_EXPR_CODE_P (subcode)
		 && POINTER_TYPE_P (gimple_expr_type (stmt))
		 && POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (stmt))))
	  {
	    tree type = gimple_expr_type (stmt);
	    tree t = maybe_fold_offset_to_address (loc,
						   gimple_assign_rhs1 (stmt),
						   integer_zero_node, type);
	    if (t)
	      return t;
	  }
      }
      break;

    case GIMPLE_BINARY_RHS:
      /* Try to fold pointer addition.  */
      if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
	{
	  tree type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	  if (TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE)
	    {
	      type = build_pointer_type (TREE_TYPE (TREE_TYPE (type)));
	      if (!useless_type_conversion_p
		    (TREE_TYPE (gimple_assign_lhs (stmt)), type))
		type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	    }
	  result = maybe_fold_stmt_addition (gimple_location (stmt),
					     type,
					     gimple_assign_rhs1 (stmt),
					     gimple_assign_rhs2 (stmt));
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

	  /* Fold might have produced non-GIMPLE, so if we trust it blindly
	     we lose canonicalization opportunities.  Do not go again
	     through fold here though, or the same non-GIMPLE will be
	     produced.  */
          if (commutative_tree_code (subcode)
              && tree_swap_operands_p (gimple_assign_rhs1 (stmt),
                                       gimple_assign_rhs2 (stmt), false))
            return build2 (subcode, TREE_TYPE (gimple_assign_lhs (stmt)),
                           gimple_assign_rhs2 (stmt),
                           gimple_assign_rhs1 (stmt));
        }
      break;

    case GIMPLE_TERNARY_RHS:
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

	  /* Fold might have produced non-GIMPLE, so if we trust it blindly
	     we lose canonicalization opportunities.  Do not go again
	     through fold here though, or the same non-GIMPLE will be
	     produced.  */
          if (commutative_ternary_tree_code (subcode)
              && tree_swap_operands_p (gimple_assign_rhs1 (stmt),
                                       gimple_assign_rhs2 (stmt), false))
            return build3 (subcode, TREE_TYPE (gimple_assign_lhs (stmt)),
			   gimple_assign_rhs2 (stmt),
			   gimple_assign_rhs1 (stmt),
			   gimple_assign_rhs3 (stmt));
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
  tree tmp = NULL_TREE;  /* Silence warning.  */
  gimple stmt, new_stmt;
  gimple_stmt_iterator i;
  gimple_seq stmts = gimple_seq_alloc();
  struct gimplify_ctx gctx;
  gimple last = NULL;
  gimple laststore = NULL;
  tree reaching_vuse;

  stmt = gsi_stmt (*si_p);

  gcc_assert (is_gimple_call (stmt));

  lhs = gimple_call_lhs (stmt);
  reaching_vuse = gimple_vuse (stmt);

  push_gimplify_context (&gctx);

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
    tmp = get_initialized_tmp_var (expr, &stmts, NULL);

  pop_gimplify_context (NULL);

  if (gimple_has_location (stmt))
    annotate_all_with_location (stmts, gimple_location (stmt));

  /* The replacement can expose previously unreferenced variables.  */
  for (i = gsi_start (stmts); !gsi_end_p (i); gsi_next (&i))
    {
      if (last)
	{
	  gsi_insert_before (si_p, last, GSI_NEW_STMT);
	  gsi_next (si_p);
	}
      new_stmt = gsi_stmt (i);
      if (gimple_in_ssa_p (cfun))
	{
	  find_new_referenced_vars (new_stmt);
	  mark_symbols_for_renaming (new_stmt);
	}
      /* If the new statement has a VUSE, update it with exact SSA name we
         know will reach this one.  */
      if (gimple_vuse (new_stmt))
	{
	  /* If we've also seen a previous store create a new VDEF for
	     the latter one, and make that the new reaching VUSE.  */
	  if (laststore)
	    {
	      reaching_vuse = make_ssa_name (gimple_vop (cfun), laststore);
	      gimple_set_vdef (laststore, reaching_vuse);
	      update_stmt (laststore);
	      laststore = NULL;
	    }
	  gimple_set_vuse (new_stmt, reaching_vuse);
	  gimple_set_modified (new_stmt, true);
	}
      if (gimple_assign_single_p (new_stmt)
	  && !is_gimple_reg (gimple_assign_lhs (new_stmt)))
	{
	  laststore = new_stmt;
	}
      last = new_stmt;
    }

  if (lhs == NULL_TREE)
    {
      /* If we replace a call without LHS that has a VDEF and our new
         sequence ends with a store we must make that store have the same
	 vdef in order not to break the sequencing.  This can happen
	 for instance when folding memcpy calls into assignments.  */
      if (gimple_vdef (stmt) && laststore)
	{
	  gimple_set_vdef (laststore, gimple_vdef (stmt));
	  if (TREE_CODE (gimple_vdef (stmt)) == SSA_NAME)
	    SSA_NAME_DEF_STMT (gimple_vdef (stmt)) = laststore;
	  update_stmt (laststore);
	}
      else if (gimple_in_ssa_p (cfun))
	{
	  unlink_stmt_vdef (stmt);
	  release_defs (stmt);
	}
      new_stmt = last;
    }
  else
    {
      if (last)
	{
	  gsi_insert_before (si_p, last, GSI_NEW_STMT);
	  gsi_next (si_p);
	}
      if (laststore && is_gimple_reg (lhs))
	{
	  gimple_set_vdef (laststore, gimple_vdef (stmt));
	  update_stmt (laststore);
	  if (TREE_CODE (gimple_vdef (stmt)) == SSA_NAME)
	    SSA_NAME_DEF_STMT (gimple_vdef (stmt)) = laststore;
	  laststore = NULL;
	}
      else if (laststore)
	{
	  reaching_vuse = make_ssa_name (gimple_vop (cfun), laststore);
	  gimple_set_vdef (laststore, reaching_vuse);
	  update_stmt (laststore);
	  laststore = NULL;
	}
      new_stmt = gimple_build_assign (lhs, tmp);
      if (!is_gimple_reg (tmp))
	gimple_set_vuse (new_stmt, reaching_vuse);
      if (!is_gimple_reg (lhs))
	{
	  gimple_set_vdef (new_stmt, gimple_vdef (stmt));
	  if (TREE_CODE (gimple_vdef (stmt)) == SSA_NAME)
	    SSA_NAME_DEF_STMT (gimple_vdef (stmt)) = new_stmt;
	}
      else if (reaching_vuse == gimple_vuse (stmt))
	unlink_stmt_vdef (stmt);
    }

  gimple_set_location (new_stmt, gimple_location (stmt));
  gsi_replace (si_p, new_stmt, false);
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

/* Return a declaration of a function which an OBJ_TYPE_REF references. TOKEN
   is integer form of OBJ_TYPE_REF_TOKEN of the reference expression.
   KNOWN_BINFO carries the binfo describing the true type of
   OBJ_TYPE_REF_OBJECT(REF).  If a call to the function must be accompanied
   with a this adjustment, the constant which should be added to this pointer
   is stored to *DELTA.  If REFUSE_THUNKS is true, return NULL if the function
   is a thunk (other than a this adjustment which is dealt with by DELTA). */

tree
gimple_get_virt_method_for_binfo (HOST_WIDE_INT token, tree known_binfo,
				  tree *delta, bool refuse_thunks)
{
  HOST_WIDE_INT i;
  tree v, fndecl;
  struct cgraph_node *node;

  v = BINFO_VIRTUALS (known_binfo);
  /* If there is no virtual methods leave the OBJ_TYPE_REF alone.  */
  if (!v)
    return NULL_TREE;
  i = 0;
  while (i != token)
    {
      i += (TARGET_VTABLE_USES_DESCRIPTORS
	    ? TARGET_VTABLE_USES_DESCRIPTORS : 1);
      v = TREE_CHAIN (v);
    }

  /* If BV_VCALL_INDEX is non-NULL, give up.  */
  if (TREE_TYPE (v))
    return NULL_TREE;

  fndecl = TREE_VALUE (v);
  node = cgraph_get_node_or_alias (fndecl);
  if (refuse_thunks
      && (!node
    /* Bail out if it is a thunk declaration.  Since simple this_adjusting
       thunks are represented by a constant in TREE_PURPOSE of items in
       BINFO_VIRTUALS, this is a more complicate type which we cannot handle as
       yet.

       FIXME: Remove the following condition once we are able to represent
       thunk information on call graph edges.  */
	  || (node->same_body_alias && node->thunk.thunk_p)))
    return NULL_TREE;

  /* When cgraph node is missing and function is not public, we cannot
     devirtualize.  This can happen in WHOPR when the actual method
     ends up in other partition, because we found devirtualization
     possibility too late.  */
  if (!can_refer_decl_in_current_unit_p (TREE_VALUE (v)))
    return NULL_TREE;

  *delta = TREE_PURPOSE (v);
  gcc_checking_assert (host_integerp (*delta, 0));
  return fndecl;
}

/* Generate code adjusting the first parameter of a call statement determined
   by GSI by DELTA.  */

void
gimple_adjust_this_by_delta (gimple_stmt_iterator *gsi, tree delta)
{
  gimple call_stmt = gsi_stmt (*gsi);
  tree parm, tmp;
  gimple new_stmt;

  delta = fold_convert (sizetype, delta);
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

/* Attempt to fold a call statement referenced by the statement iterator GSI.
   The statement may be replaced by another statement, e.g., if the call
   simplifies to a constant value. Return true if any changes were made.
   It is assumed that the operands have been previously folded.  */

bool
gimple_fold_call (gimple_stmt_iterator *gsi, bool inplace)
{
  gimple stmt = gsi_stmt (*gsi);

  tree callee = gimple_call_fndecl (stmt);

  /* Check for builtins that CCP can handle using information not
     available in the generic fold routines.  */
  if (!inplace && callee && DECL_BUILT_IN (callee))
    {
      tree result = gimple_fold_builtin (stmt);

      if (result)
	{
          if (!update_call_from_tree (gsi, result))
	    gimplify_and_update_call_from_tree (gsi, result);
	  return true;
	}
    }
  return false;
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
	tree new_rhs = fold_gimple_assign (gsi);
	tree lhs = gimple_assign_lhs (stmt);
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

/* Perform the minimal folding on statement STMT.  Only operations like
   *&x created by constant propagation are handled.  The statement cannot
   be replaced with a new one.  Return true if the statement was
   changed, false otherwise.
   The statement STMT should be in valid gimple form but may
   be in unfolded state as resulting from for example constant propagation
   which can produce *&x = 0.  */

bool
fold_stmt_inplace (gimple stmt)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  bool changed = fold_stmt_1 (&gsi, true);
  gcc_assert (gsi_stmt (gsi) == stmt);
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
  if (innercode == TRUTH_AND_EXPR
      || innercode == TRUTH_OR_EXPR
      || (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE
	  && (innercode == BIT_AND_EXPR || innercode == BIT_IOR_EXPR)))
    {
      tree inner1 = gimple_assign_rhs1 (stmt);
      tree inner2 = gimple_assign_rhs2 (stmt);
      gimple s;
      tree t;
      tree partial = NULL_TREE;
      bool is_and = (innercode == TRUTH_AND_EXPR || innercode == BIT_AND_EXPR);
      
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
  if (innercode == TRUTH_AND_EXPR
      || innercode == TRUTH_OR_EXPR
      || (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE
	  && (innercode == BIT_AND_EXPR || innercode == BIT_IOR_EXPR)))
    {
      tree inner1 = gimple_assign_rhs1 (stmt);
      tree inner2 = gimple_assign_rhs2 (stmt);
      gimple s;
      tree t;
      tree partial = NULL_TREE;
      bool is_or = (innercode == TRUTH_OR_EXPR || innercode == BIT_IOR_EXPR);
      
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
