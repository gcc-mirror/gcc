/* Statement simplification on GIMPLE.
   Copyright (C) 2010-2023 Free Software Foundation, Inc.
   Split out from tree-ssa-ccp.cc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa-warn-access.h"
#include "gimple-ssa-warn-restrict.h"
#include "fold-const.h"
#include "stmt.h"
#include "expr.h"
#include "stor-layout.h"
#include "dumpfile.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-object-size.h"
#include "tree-ssa.h"
#include "tree-ssa-propagate.h"
#include "ipa-utils.h"
#include "tree-ssa-address.h"
#include "langhooks.h"
#include "gimplify-me.h"
#include "dbgcnt.h"
#include "builtins.h"
#include "tree-eh.h"
#include "gimple-match.h"
#include "gomp-constants.h"
#include "optabs-query.h"
#include "omp-general.h"
#include "tree-cfg.h"
#include "fold-const-call.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "calls.h"
#include "tree-vector-builder.h"
#include "tree-ssa-strlen.h"
#include "varasm.h"
#include "internal-fn.h"
#include "gimple-range.h"

enum strlen_range_kind {
  /* Compute the exact constant string length.  */
  SRK_STRLEN,
  /* Compute the maximum constant string length.  */
  SRK_STRLENMAX,
  /* Compute a range of string lengths bounded by object sizes.  When
     the length of a string cannot be determined, consider as the upper
     bound the size of the enclosing object the string may be a member
     or element of.  Also determine the size of the largest character
     array the string may refer to.  */
  SRK_LENRANGE,
  /* Determine the integer value of the argument (not string length).  */
  SRK_INT_VALUE
};

static bool
get_range_strlen (tree, bitmap, strlen_range_kind, c_strlen_data *, unsigned);

/* Return true when DECL can be referenced from current unit.
   FROM_DECL (if non-null) specify constructor of variable DECL was taken from.
   We can get declarations that are not possible to reference for various
   reasons:

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
        we devirtualize only during final compilation stage.
        At this time we already decided that we will not output
        the function body and thus we can't reference the symbol
        directly.  */

static bool
can_refer_decl_in_current_unit_p (tree decl, tree from_decl)
{
  varpool_node *vnode;
  struct cgraph_node *node;
  symtab_node *snode;

  if (DECL_ABSTRACT_P (decl))
    return false;

  /* We are concerned only about static/external vars and functions.  */
  if ((!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
      || !VAR_OR_FUNCTION_DECL_P (decl))
    return true;

  /* Static objects can be referred only if they are defined and not optimized
     out yet.  */
  if (!TREE_PUBLIC (decl))
    {
      if (DECL_EXTERNAL (decl))
	return false;
      /* Before we start optimizing unreachable code we can be sure all
	 static objects are defined.  */
      if (symtab->function_flags_ready)
	return true;
      snode = symtab_node::get (decl);
      if (!snode || !snode->definition)
	return false;
      node = dyn_cast <cgraph_node *> (snode);
      return !node || !node->inlined_to;
    }

  /* We will later output the initializer, so we can refer to it.
     So we are concerned only when DECL comes from initializer of
     external var or var that has been optimized out.  */
  if (!from_decl
      || !VAR_P (from_decl)
      || (!DECL_EXTERNAL (from_decl)
	  && (vnode = varpool_node::get (from_decl)) != NULL
	  && vnode->definition)
      || (flag_ltrans
	  && (vnode = varpool_node::get (from_decl)) != NULL
	  && vnode->in_other_partition))
    return true;
  /* We are folding reference from external vtable.  The vtable may reffer
     to a symbol keyed to other compilation unit.  The other compilation
     unit may be in separate DSO and the symbol may be hidden.  */
  if (DECL_VISIBILITY_SPECIFIED (decl)
      && DECL_EXTERNAL (decl)
      && DECL_VISIBILITY (decl) != VISIBILITY_DEFAULT
      && (!(snode = symtab_node::get (decl)) || !snode->in_other_partition))
    return false;
  /* When function is public, we always can introduce new reference.
     Exception are the COMDAT functions where introducing a direct
     reference imply need to include function body in the curren tunit.  */
  if (TREE_PUBLIC (decl) && !DECL_COMDAT (decl))
    return true;
  /* We have COMDAT.  We are going to check if we still have definition
     or if the definition is going to be output in other partition.
     Bypass this when gimplifying; all needed functions will be produced.

     As observed in PR20991 for already optimized out comdat virtual functions
     it may be tempting to not necessarily give up because the copy will be
     output elsewhere when corresponding vtable is output.  
     This is however not possible - ABI specify that COMDATs are output in
     units where they are used and when the other unit was compiled with LTO
     it is possible that vtable was kept public while the function itself
     was privatized. */
  if (!symtab->function_flags_ready)
    return true;

  snode = symtab_node::get (decl);
  if (!snode
      || ((!snode->definition || DECL_EXTERNAL (decl))
	  && (!snode->in_other_partition
	      || (!snode->forced_by_abi && !snode->force_output))))
    return false;
  node = dyn_cast <cgraph_node *> (snode);
  return !node || !node->inlined_to;
}

/* Create a temporary for TYPE for a statement STMT.  If the current function
   is in SSA form, a SSA name is created.  Otherwise a temporary register
   is made.  */

tree
create_tmp_reg_or_ssa_name (tree type, gimple *stmt)
{
  if (gimple_in_ssa_p (cfun))
    return make_ssa_name (type, stmt);
  else
    return create_tmp_reg (type);
}

/* CVAL is value taken from DECL_INITIAL of variable.  Try to transform it into
   acceptable form for is_gimple_min_invariant.
   FROM_DECL (if non-NULL) specify variable whose constructor contains CVAL.  */

tree
canonicalize_constructor_val (tree cval, tree from_decl)
{
  if (CONSTANT_CLASS_P (cval))
    return cval;

  tree orig_cval = cval;
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
      tree base = NULL_TREE;
      if (TREE_CODE (TREE_OPERAND (cval, 0)) == COMPOUND_LITERAL_EXPR)
	{
	  base = COMPOUND_LITERAL_EXPR_DECL (TREE_OPERAND (cval, 0));
	  if (base)
	    TREE_OPERAND (cval, 0) = base;
	}
      else
	base = get_base_address (TREE_OPERAND (cval, 0));
      if (!base)
	return NULL_TREE;

      if (VAR_OR_FUNCTION_DECL_P (base)
	  && !can_refer_decl_in_current_unit_p (base, from_decl))
	return NULL_TREE;
      if (TREE_TYPE (base) == error_mark_node)
	return NULL_TREE;
      if (VAR_P (base))
	/* ???  We should be able to assert that TREE_ADDRESSABLE is set,
	   but since the use can be in a debug stmt we can't.  */
	;
      else if (TREE_CODE (base) == FUNCTION_DECL)
	{
	  /* Make sure we create a cgraph node for functions we'll reference.
	     They can be non-existent if the reference comes from an entry
	     of an external vtable for example.  */
	  cgraph_node::get_create (base);
	}
      /* Fixup types in global initializers.  */
      if (TREE_TYPE (TREE_TYPE (cval)) != TREE_TYPE (TREE_OPERAND (cval, 0)))
	cval = build_fold_addr_expr (TREE_OPERAND (cval, 0));

      if (!useless_type_conversion_p (TREE_TYPE (orig_cval), TREE_TYPE (cval)))
	cval = fold_convert (TREE_TYPE (orig_cval), cval);
      return cval;
    }
  /* In CONSTRUCTORs we may see unfolded constants like (int (*) ()) 0.  */
  if (TREE_CODE (cval) == INTEGER_CST)
    {
      if (TREE_OVERFLOW_P (cval))
	cval = drop_tree_overflow (cval);
      if (!useless_type_conversion_p (TREE_TYPE (orig_cval), TREE_TYPE (cval)))
	cval = fold_convert (TREE_TYPE (orig_cval), cval);
      return cval;
    }
  return orig_cval;
}

/* If SYM is a constant variable with known value, return the value.
   NULL_TREE is returned otherwise.  */

tree
get_symbol_constant_value (tree sym)
{
  tree val = ctor_for_folding (sym);
  if (val != error_mark_node)
    {
      if (val)
	{
	  val = canonicalize_constructor_val (unshare_expr (val), sym);
	  if (val
	      && is_gimple_min_invariant (val)
	      && useless_type_conversion_p (TREE_TYPE (sym), TREE_TYPE (val)))
	    return val;
	  else
	    return NULL_TREE;
	}
      /* Variables declared 'const' without an initializer
	 have zero as the initializer if they may not be
	 overridden at link or run time.  */
      if (!val
          && is_gimple_reg_type (TREE_TYPE (sym)))
	return build_zero_cst (TREE_TYPE (sym));
    }

  return NULL_TREE;
}



/* Subroutine of fold_stmt.  We perform constant folding of the
   memory reference tree EXPR.  */

static tree
maybe_fold_reference (tree expr)
{
  tree result = NULL_TREE;

  if ((TREE_CODE (expr) == VIEW_CONVERT_EXPR
       || TREE_CODE (expr) == REALPART_EXPR
       || TREE_CODE (expr) == IMAGPART_EXPR)
      && CONSTANT_CLASS_P (TREE_OPERAND (expr, 0)))
    result = fold_unary_loc (EXPR_LOCATION (expr),
			     TREE_CODE (expr),
			     TREE_TYPE (expr),
			     TREE_OPERAND (expr, 0));
  else if (TREE_CODE (expr) == BIT_FIELD_REF
	   && CONSTANT_CLASS_P (TREE_OPERAND (expr, 0)))
    result = fold_ternary_loc (EXPR_LOCATION (expr),
			       TREE_CODE (expr),
			       TREE_TYPE (expr),
			       TREE_OPERAND (expr, 0),
			       TREE_OPERAND (expr, 1),
			       TREE_OPERAND (expr, 2));
  else
    result = fold_const_aggregate_ref (expr);

  if (result && is_gimple_min_invariant (result))
    return result;

  return NULL_TREE;
}

/* Return true if EXPR is an acceptable right-hand-side for a
   GIMPLE assignment.  We validate the entire tree, not just
   the root node, thus catching expressions that embed complex
   operands that are not permitted in GIMPLE.  This function
   is needed because the folding routines in fold-const.cc
   may return such expressions in some cases, e.g., an array
   access with an embedded index addition.  It may make more
   sense to have folding routines that are sensitive to the
   constraints on GIMPLE operands, rather than abandoning any
   any attempt to fold if the usual folding turns out to be too
   aggressive.  */

bool
valid_gimple_rhs_p (tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:
      if (!is_gimple_variable (expr))
	return false;
      break;

    case tcc_constant:
      /* All constants are ok.  */
      break;

    case tcc_comparison:
      /* GENERIC allows comparisons with non-boolean types, reject
	 those for GIMPLE.  Let vector-typed comparisons pass - rules
	 for GENERIC and GIMPLE are the same here.  */
      if (!(INTEGRAL_TYPE_P (TREE_TYPE (expr))
	    && (TREE_CODE (TREE_TYPE (expr)) == BOOLEAN_TYPE
		|| TYPE_PRECISION (TREE_TYPE (expr)) == 1))
	  && ! VECTOR_TYPE_P (TREE_TYPE (expr)))
	return false;

      /* Fallthru.  */
    case tcc_binary:
      if (!is_gimple_val (TREE_OPERAND (expr, 0))
	  || !is_gimple_val (TREE_OPERAND (expr, 1)))
	return false;
      break;

    case tcc_unary:
      if (!is_gimple_val (TREE_OPERAND (expr, 0)))
	return false;
      break;

    case tcc_expression:
      switch (code)
	{
	case ADDR_EXPR:
	  {
	    tree t;
	    if (is_gimple_min_invariant (expr))
	      return true;
	    t = TREE_OPERAND (expr, 0);
	    while (handled_component_p (t))
	      {
		/* ??? More checks needed, see the GIMPLE verifier.  */
		if ((TREE_CODE (t) == ARRAY_REF
		     || TREE_CODE (t) == ARRAY_RANGE_REF)
		    && !is_gimple_val (TREE_OPERAND (t, 1)))
		  return false;
		t = TREE_OPERAND (t, 0);
	      }
	    if (!is_gimple_id (t))
	      return false;
	  }
	  break;

	default:
	  if (get_gimple_rhs_class (code) == GIMPLE_TERNARY_RHS)
	    {
	      if (!is_gimple_val (TREE_OPERAND (expr, 0))
		  || !is_gimple_val (TREE_OPERAND (expr, 1))
		  || !is_gimple_val (TREE_OPERAND (expr, 2)))
		return false;
	      break;
	    }
	  return false;
	}
      break;

    case tcc_vl_exp:
      return false;

    case tcc_exceptional:
      if (code == CONSTRUCTOR)
	{
	  unsigned i;
	  tree elt;
	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expr), i, elt)
	    if (!is_gimple_val (elt))
	      return false;
	  return true;
	}
      if (code != SSA_NAME)
	return false;
      break;

    case tcc_reference:
      if (code == BIT_FIELD_REF)
	return is_gimple_val (TREE_OPERAND (expr, 0));
      return false;

    default:
      return false;
    }

  return true;
}


/* Attempt to fold an assignment statement pointed-to by SI.  Returns a
   replacement rhs for the statement or NULL_TREE if no simplification
   could be made.  It is assumed that the operands have been previously
   folded.  */

static tree
fold_gimple_assign (gimple_stmt_iterator *si)
{
  gimple *stmt = gsi_stmt (*si);
  enum tree_code subcode = gimple_assign_rhs_code (stmt);
  location_t loc = gimple_location (stmt);

  tree result = NULL_TREE;

  switch (get_gimple_rhs_class (subcode))
    {
    case GIMPLE_SINGLE_RHS:
      {
        tree rhs = gimple_assign_rhs1 (stmt);

	if (TREE_CLOBBER_P (rhs))
	  return NULL_TREE;

	if (REFERENCE_CLASS_P (rhs))
	  return maybe_fold_reference (rhs);

	else if (TREE_CODE (rhs) == OBJ_TYPE_REF)
	  {
	    tree val = OBJ_TYPE_REF_EXPR (rhs);
	    if (is_gimple_min_invariant (val))
	      return val;
	    else if (flag_devirtualize && virtual_method_call_p (rhs))
	      {
		bool final;
		vec <cgraph_node *>targets
		  = possible_polymorphic_call_targets (rhs, stmt, &final);
		if (final && targets.length () <= 1 && dbg_cnt (devirt))
		  {
		    if (dump_enabled_p ())
		      {
			dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
					 "resolving virtual function address "
					 "reference to function %s\n",
					 targets.length () == 1
					 ? targets[0]->name ()
					 : "NULL");
		      }
		    if (targets.length () == 1)
		      {
			val = fold_convert (TREE_TYPE (val),
					    build_fold_addr_expr_loc
					      (loc, targets[0]->decl));
			STRIP_USELESS_TYPE_CONVERSION (val);
		      }
		    else
		      /* We cannot use __builtin_unreachable here because it
			 cannot have address taken.  */
		      val = build_int_cst (TREE_TYPE (val), 0);
		    return val;
		  }
	      }
	  }

	else if (TREE_CODE (rhs) == ADDR_EXPR)
	  {
	    tree ref = TREE_OPERAND (rhs, 0);
	    if (TREE_CODE (ref) == MEM_REF
		&& integer_zerop (TREE_OPERAND (ref, 1)))
	      {
		result = TREE_OPERAND (ref, 0);
		if (!useless_type_conversion_p (TREE_TYPE (rhs),
						TREE_TYPE (result)))
		  result = build1 (NOP_EXPR, TREE_TYPE (rhs), result);
		return result;
	      }
	  }

	else if (TREE_CODE (rhs) == CONSTRUCTOR
		 && TREE_CODE (TREE_TYPE (rhs)) == VECTOR_TYPE)
	  {
	    /* Fold a constant vector CONSTRUCTOR to VECTOR_CST.  */
	    unsigned i;
	    tree val;

	    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), i, val)
	      if (! CONSTANT_CLASS_P (val))
		return NULL_TREE;

	    return build_vector_from_ctor (TREE_TYPE (rhs),
					   CONSTRUCTOR_ELTS (rhs));
	  }

	else if (DECL_P (rhs)
		 && is_gimple_reg_type (TREE_TYPE (rhs)))
	  return get_symbol_constant_value (rhs);
      }
      break;

    case GIMPLE_UNARY_RHS:
      break;

    case GIMPLE_BINARY_RHS:
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
        }
      break;

    case GIMPLE_INVALID_RHS:
      gcc_unreachable ();
    }

  return NULL_TREE;
}


/* Replace a statement at *SI_P with a sequence of statements in STMTS,
   adjusting the replacement stmts location and virtual operands.
   If the statement has a lhs the last stmt in the sequence is expected
   to assign to that lhs.  */

void
gsi_replace_with_seq_vops (gimple_stmt_iterator *si_p, gimple_seq stmts)
{
  gimple *stmt = gsi_stmt (*si_p);

  if (gimple_has_location (stmt))
    annotate_all_with_location (stmts, gimple_location (stmt));

  /* First iterate over the replacement statements backward, assigning
     virtual operands to their defining statements.  */
  gimple *laststore = NULL;
  for (gimple_stmt_iterator i = gsi_last (stmts);
       !gsi_end_p (i); gsi_prev (&i))
    {
      gimple *new_stmt = gsi_stmt (i);
      if ((gimple_assign_single_p (new_stmt)
	   && !is_gimple_reg (gimple_assign_lhs (new_stmt)))
	  || (is_gimple_call (new_stmt)
	      && (gimple_call_flags (new_stmt)
		  & (ECF_NOVOPS | ECF_PURE | ECF_CONST | ECF_NORETURN)) == 0))
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
  tree reaching_vuse = gimple_vuse (stmt);
  for (gimple_stmt_iterator i = gsi_start (stmts);
       !gsi_end_p (i); gsi_next (&i))
    {
      gimple *new_stmt = gsi_stmt (i);
      /* If the new statement possibly has a VUSE, update it with exact SSA
	 name we know will reach this one.  */
      if (gimple_has_mem_ops (new_stmt))
	gimple_set_vuse (new_stmt, reaching_vuse);
      gimple_set_modified (new_stmt, true);
      if (gimple_vdef (new_stmt))
	reaching_vuse = gimple_vdef (new_stmt);
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

  /* Finally replace the original statement with the sequence.  */
  gsi_replace_with_seq (si_p, stmts, false);
}

/* Helper function for update_gimple_call and
   gimplify_and_update_call_from_tree.  A GIMPLE_CALL STMT is being replaced
   with GIMPLE_CALL NEW_STMT.  */

static void
finish_update_gimple_call (gimple_stmt_iterator *si_p, gimple *new_stmt,
			   gimple *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  gimple_call_set_lhs (new_stmt, lhs);
  if (lhs && TREE_CODE (lhs) == SSA_NAME)
    SSA_NAME_DEF_STMT (lhs) = new_stmt;
  gimple_move_vops (new_stmt, stmt);
  gimple_set_location (new_stmt, gimple_location (stmt));
  if (gimple_block (new_stmt) == NULL_TREE)
    gimple_set_block (new_stmt, gimple_block (stmt));
  gsi_replace (si_p, new_stmt, false);
}

/* Update a GIMPLE_CALL statement at iterator *SI_P to call to FN
   with number of arguments NARGS, where the arguments in GIMPLE form
   follow NARGS argument.  */

bool
update_gimple_call (gimple_stmt_iterator *si_p, tree fn, int nargs, ...)
{
  va_list ap;
  gcall *new_stmt, *stmt = as_a <gcall *> (gsi_stmt (*si_p));

  gcc_assert (is_gimple_call (stmt));
  va_start (ap, nargs);
  new_stmt = gimple_build_call_valist (fn, nargs, ap);
  finish_update_gimple_call (si_p, new_stmt, stmt);
  va_end (ap);
  return true;
}

/* Return true if EXPR is a CALL_EXPR suitable for representation
   as a single GIMPLE_CALL statement.  If the arguments require
   further gimplification, return false.  */

static bool
valid_gimple_call_p (tree expr)
{
  unsigned i, nargs;

  if (TREE_CODE (expr) != CALL_EXPR)
    return false;

  nargs = call_expr_nargs (expr);
  for (i = 0; i < nargs; i++)
    {
      tree arg = CALL_EXPR_ARG (expr, i);
      if (is_gimple_reg_type (TREE_TYPE (arg)))
	{
	  if (!is_gimple_val (arg))
	    return false;
	}
      else
	if (!is_gimple_lvalue (arg))
	  return false;
    }

  return true;
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
  gimple *stmt, *new_stmt;
  gimple_stmt_iterator i;
  gimple_seq stmts = NULL;

  stmt = gsi_stmt (*si_p);

  gcc_assert (is_gimple_call (stmt));

  if (valid_gimple_call_p (expr))
    {
      /* The call has simplified to another call.  */
      tree fn = CALL_EXPR_FN (expr);
      unsigned i;
      unsigned nargs = call_expr_nargs (expr);
      vec<tree> args = vNULL;
      gcall *new_stmt;

      if (nargs > 0)
	{
	  args.create (nargs);
	  args.safe_grow_cleared (nargs, true);

	  for (i = 0; i < nargs; i++)
	    args[i] = CALL_EXPR_ARG (expr, i);
	}

      new_stmt = gimple_build_call_vec (fn, args);
      finish_update_gimple_call (si_p, new_stmt, stmt);
      args.release ();
      return;
    }

  lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    {
      push_gimplify_context (gimple_in_ssa_p (cfun));
      gimplify_and_add (expr, &stmts);
      pop_gimplify_context (NULL);

      /* We can end up with folding a memcpy of an empty class assignment
	 which gets optimized away by C++ gimplification.  */
      if (gimple_seq_empty_p (stmts))
	{
	  if (gimple_in_ssa_p (cfun))
	    {
	      unlink_stmt_vdef (stmt);
	      release_defs (stmt);
	    }
	  gsi_replace (si_p, gimple_build_nop (), false);
	  return;
	}
    }
  else
    {
      tree tmp = force_gimple_operand (expr, &stmts, false, NULL_TREE);
      new_stmt = gimple_build_assign (lhs, tmp);
      i = gsi_last (stmts);
      gsi_insert_after_without_update (&i, new_stmt,
				       GSI_CONTINUE_LINKING);
    }

  gsi_replace_with_seq_vops (si_p, stmts);
}


/* Replace the call at *GSI with the gimple value VAL.  */

void
replace_call_with_value (gimple_stmt_iterator *gsi, tree val)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);
  gimple *repl;
  if (lhs)
    {
      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (val)))
	val = fold_convert (TREE_TYPE (lhs), val);
      repl = gimple_build_assign (lhs, val);
    }
  else
    repl = gimple_build_nop ();
  tree vdef = gimple_vdef (stmt);
  if (vdef && TREE_CODE (vdef) == SSA_NAME)
    {
      unlink_stmt_vdef (stmt);
      release_ssa_name (vdef);
    }
  gsi_replace (gsi, repl, false);
}

/* Replace the call at *GSI with the new call REPL and fold that
   again.  */

static void
replace_call_with_call_and_fold (gimple_stmt_iterator *gsi, gimple *repl)
{
  gimple *stmt = gsi_stmt (*gsi);
  gimple_call_set_lhs (repl, gimple_call_lhs (stmt));
  gimple_set_location (repl, gimple_location (stmt));
  gimple_move_vops (repl, stmt);
  gsi_replace (gsi, repl, false);
  fold_stmt (gsi);
}

/* Return true if VAR is a VAR_DECL or a component thereof.  */

static bool
var_decl_component_p (tree var)
{
  tree inner = var;
  while (handled_component_p (inner))
    inner = TREE_OPERAND (inner, 0);
  return (DECL_P (inner)
	  || (TREE_CODE (inner) == MEM_REF
	      && TREE_CODE (TREE_OPERAND (inner, 0)) == ADDR_EXPR));
}

/* Return TRUE if the SIZE argument, representing the size of an
   object, is in a range of values of which exactly zero is valid.  */

static bool
size_must_be_zero_p (tree size)
{
  if (integer_zerop (size))
    return true;

  if (TREE_CODE (size) != SSA_NAME || !INTEGRAL_TYPE_P (TREE_TYPE (size)))
    return false;

  tree type = TREE_TYPE (size);
  int prec = TYPE_PRECISION (type);

  /* Compute the value of SSIZE_MAX, the largest positive value that
     can be stored in ssize_t, the signed counterpart of size_t.  */
  wide_int ssize_max = wi::lshift (wi::one (prec), prec - 1) - 1;
  wide_int zero = wi::zero (TYPE_PRECISION (type));
  value_range valid_range (type, zero, ssize_max);
  value_range vr;
  if (cfun)
    get_range_query (cfun)->range_of_expr (vr, size);
  else
    get_global_range_query ()->range_of_expr (vr, size);
  if (vr.undefined_p ())
    vr.set_varying (TREE_TYPE (size));
  vr.intersect (valid_range);
  return vr.zero_p ();
}

/* Fold function call to builtin mem{{,p}cpy,move}.  Try to detect and
   diagnose (otherwise undefined) overlapping copies without preventing
   folding.  When folded, GCC guarantees that overlapping memcpy has
   the same semantics as memmove.  Call to the library memcpy need not
   provide the same guarantee.  Return false if no simplification can
   be made.  */

static bool
gimple_fold_builtin_memory_op (gimple_stmt_iterator *gsi,
			       tree dest, tree src, enum built_in_function code)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);
  tree len = gimple_call_arg (stmt, 2);
  location_t loc = gimple_location (stmt);

  /* If the LEN parameter is a constant zero or in range where
     the only valid value is zero, return DEST.  */
  if (size_must_be_zero_p (len))
    {
      gimple *repl;
      if (gimple_call_lhs (stmt))
	repl = gimple_build_assign (gimple_call_lhs (stmt), dest);
      else
	repl = gimple_build_nop ();
      tree vdef = gimple_vdef (stmt);
      if (vdef && TREE_CODE (vdef) == SSA_NAME)
	{
	  unlink_stmt_vdef (stmt);
	  release_ssa_name (vdef);
	}
      gsi_replace (gsi, repl, false);
      return true;
    }

  /* If SRC and DEST are the same (and not volatile), return
     DEST{,+LEN,+LEN-1}.  */
  if (operand_equal_p (src, dest, 0))
    {
      /* Avoid diagnosing exact overlap in calls to __builtin_memcpy.
	 It's safe and may even be emitted by GCC itself (see bug
	 32667).  */
      unlink_stmt_vdef (stmt);
      if (gimple_vdef (stmt) && TREE_CODE (gimple_vdef (stmt)) == SSA_NAME)
	release_ssa_name (gimple_vdef (stmt));
      if (!lhs)
	{
	  gsi_replace (gsi, gimple_build_nop (), false);
	  return true;
	}
      goto done;
    }
  else
    {
      /* We cannot (easily) change the type of the copy if it is a storage
	 order barrier, i.e. is equivalent to a VIEW_CONVERT_EXPR that can
	 modify the storage order of objects (see storage_order_barrier_p).  */
      tree srctype
	= POINTER_TYPE_P (TREE_TYPE (src))
	  ? TREE_TYPE (TREE_TYPE (src)) : NULL_TREE;
      tree desttype
	= POINTER_TYPE_P (TREE_TYPE (dest))
	  ? TREE_TYPE (TREE_TYPE (dest)) : NULL_TREE;
      tree destvar, srcvar, srcoff;
      unsigned int src_align, dest_align;
      unsigned HOST_WIDE_INT tmp_len;
      const char *tmp_str;

      /* Build accesses at offset zero with a ref-all character type.  */
      tree off0
	= build_int_cst (build_pointer_type_for_mode (char_type_node,
						      ptr_mode, true), 0);

      /* If we can perform the copy efficiently with first doing all loads
         and then all stores inline it that way.  Currently efficiently
	 means that we can load all the memory into a single integer
	 register which is what MOVE_MAX gives us.  */
      src_align = get_pointer_alignment (src);
      dest_align = get_pointer_alignment (dest);
      if (tree_fits_uhwi_p (len)
	  && compare_tree_int (len, MOVE_MAX) <= 0
	  /* FIXME: Don't transform copies from strings with known length.
	     Until GCC 9 this prevented a case in gcc.dg/strlenopt-8.c
	     from being handled, and the case was XFAILed for that reason.
	     Now that it is handled and the XFAIL removed, as soon as other
	     strlenopt tests that rely on it for passing are adjusted, this
	     hack can be removed.  */
	  && !c_strlen (src, 1)
	  && !((tmp_str = getbyterep (src, &tmp_len)) != NULL
	       && memchr (tmp_str, 0, tmp_len) == NULL)
	  && !(srctype
	       && AGGREGATE_TYPE_P (srctype)
	       && TYPE_REVERSE_STORAGE_ORDER (srctype))
	  && !(desttype
	       && AGGREGATE_TYPE_P (desttype)
	       && TYPE_REVERSE_STORAGE_ORDER (desttype)))
	{
	  unsigned ilen = tree_to_uhwi (len);
	  if (pow2p_hwi (ilen))
	    {
	      /* Detect out-of-bounds accesses without issuing warnings.
		 Avoid folding out-of-bounds copies but to avoid false
		 positives for unreachable code defer warning until after
		 DCE has worked its magic.
		 -Wrestrict is still diagnosed.  */
	      if (int warning = check_bounds_or_overlap (as_a <gcall *>(stmt),
							 dest, src, len, len,
							 false, false))
		if (warning != OPT_Wrestrict)
		  return false;

	      scalar_int_mode mode;
	      if (int_mode_for_size (ilen * 8, 0).exists (&mode)
		  && GET_MODE_SIZE (mode) * BITS_PER_UNIT == ilen * 8
		  /* If the destination pointer is not aligned we must be able
		     to emit an unaligned store.  */
		  && (dest_align >= GET_MODE_ALIGNMENT (mode)
		      || !targetm.slow_unaligned_access (mode, dest_align)
		      || (optab_handler (movmisalign_optab, mode)
			  != CODE_FOR_nothing)))
		{
		  tree type = build_nonstandard_integer_type (ilen * 8, 1);
		  tree srctype = type;
		  tree desttype = type;
		  if (src_align < GET_MODE_ALIGNMENT (mode))
		    srctype = build_aligned_type (type, src_align);
		  tree srcmem = fold_build2 (MEM_REF, srctype, src, off0);
		  tree tem = fold_const_aggregate_ref (srcmem);
		  if (tem)
		    srcmem = tem;
		  else if (src_align < GET_MODE_ALIGNMENT (mode)
			   && targetm.slow_unaligned_access (mode, src_align)
			   && (optab_handler (movmisalign_optab, mode)
			       == CODE_FOR_nothing))
		    srcmem = NULL_TREE;
		  if (srcmem)
		    {
		      gimple *new_stmt;
		      if (is_gimple_reg_type (TREE_TYPE (srcmem)))
			{
			  new_stmt = gimple_build_assign (NULL_TREE, srcmem);
			  srcmem
			    = create_tmp_reg_or_ssa_name (TREE_TYPE (srcmem),
							  new_stmt);
			  gimple_assign_set_lhs (new_stmt, srcmem);
			  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
			  gimple_set_location (new_stmt, loc);
			  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
			}
		      if (dest_align < GET_MODE_ALIGNMENT (mode))
			desttype = build_aligned_type (type, dest_align);
		      new_stmt
			= gimple_build_assign (fold_build2 (MEM_REF, desttype,
							    dest, off0),
					       srcmem);
		      gimple_move_vops (new_stmt, stmt);
		      if (!lhs)
			{
			  gsi_replace (gsi, new_stmt, false);
			  return true;
			}
		      gimple_set_location (new_stmt, loc);
		      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
		      goto done;
		    }
		}
	    }
	}

      if (code == BUILT_IN_MEMMOVE)
	{
	  /* Both DEST and SRC must be pointer types.
	     ??? This is what old code did.  Is the testing for pointer types
	     really mandatory?

	     If either SRC is readonly or length is 1, we can use memcpy.  */
	  if (!dest_align || !src_align)
	    return false;
	  if (readonly_data_expr (src)
	      || (tree_fits_uhwi_p (len)
		  && (MIN (src_align, dest_align) / BITS_PER_UNIT
		      >= tree_to_uhwi (len))))
	    {
	      tree fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
	      if (!fn)
		return false;
	      gimple_call_set_fndecl (stmt, fn);
	      gimple_call_set_arg (stmt, 0, dest);
	      gimple_call_set_arg (stmt, 1, src);
	      fold_stmt (gsi);
	      return true;
	    }

	  /* If *src and *dest can't overlap, optimize into memcpy as well.  */
	  if (TREE_CODE (src) == ADDR_EXPR
	      && TREE_CODE (dest) == ADDR_EXPR)
	    {
	      tree src_base, dest_base, fn;
	      poly_int64 src_offset = 0, dest_offset = 0;
	      poly_uint64 maxsize;

	      srcvar = TREE_OPERAND (src, 0);
	      src_base = get_addr_base_and_unit_offset (srcvar, &src_offset);
	      if (src_base == NULL)
		src_base = srcvar;
	      destvar = TREE_OPERAND (dest, 0);
	      dest_base = get_addr_base_and_unit_offset (destvar,
							 &dest_offset);
	      if (dest_base == NULL)
		dest_base = destvar;
	      if (!poly_int_tree_p (len, &maxsize))
		maxsize = -1;
	      if (SSA_VAR_P (src_base)
		  && SSA_VAR_P (dest_base))
		{
		  if (operand_equal_p (src_base, dest_base, 0)
		      && ranges_maybe_overlap_p (src_offset, maxsize,
						 dest_offset, maxsize))
		    return false;
		}
	      else if (TREE_CODE (src_base) == MEM_REF
		       && TREE_CODE (dest_base) == MEM_REF)
		{
		  if (! operand_equal_p (TREE_OPERAND (src_base, 0),
					 TREE_OPERAND (dest_base, 0), 0))
		    return false;
		  poly_offset_int full_src_offset
		    = mem_ref_offset (src_base) + src_offset;
		  poly_offset_int full_dest_offset
		    = mem_ref_offset (dest_base) + dest_offset;
		  if (ranges_maybe_overlap_p (full_src_offset, maxsize,
					      full_dest_offset, maxsize))
		    return false;
		}
	      else
		return false;

	      fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
	      if (!fn)
		return false;
	      gimple_call_set_fndecl (stmt, fn);
	      gimple_call_set_arg (stmt, 0, dest);
	      gimple_call_set_arg (stmt, 1, src);
	      fold_stmt (gsi);
	      return true;
	    }

	  /* If the destination and source do not alias optimize into
	     memcpy as well.  */
	  if ((is_gimple_min_invariant (dest)
	       || TREE_CODE (dest) == SSA_NAME)
	      && (is_gimple_min_invariant (src)
		  || TREE_CODE (src) == SSA_NAME))
	    {
	      ao_ref destr, srcr;
	      ao_ref_init_from_ptr_and_size (&destr, dest, len);
	      ao_ref_init_from_ptr_and_size (&srcr, src, len);
	      if (!refs_may_alias_p_1 (&destr, &srcr, false))
		{
		  tree fn;
		  fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
		  if (!fn)
		    return false;
		  gimple_call_set_fndecl (stmt, fn);
		  gimple_call_set_arg (stmt, 0, dest);
		  gimple_call_set_arg (stmt, 1, src);
		  fold_stmt (gsi);
		  return true;
		}
	    }

	  return false;
	}

      if (!tree_fits_shwi_p (len))
	return false;
      if (!srctype
	  || (AGGREGATE_TYPE_P (srctype)
	      && TYPE_REVERSE_STORAGE_ORDER (srctype)))
	return false;
      if (!desttype
	  || (AGGREGATE_TYPE_P (desttype)
	      && TYPE_REVERSE_STORAGE_ORDER (desttype)))
	return false;
      /* In the following try to find a type that is most natural to be
	 used for the memcpy source and destination and that allows
	 the most optimization when memcpy is turned into a plain assignment
	 using that type.  In theory we could always use a char[len] type
	 but that only gains us that the destination and source possibly
	 no longer will have their address taken.  */
      if (TREE_CODE (srctype) == ARRAY_TYPE
	  && !tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len))
	srctype = TREE_TYPE (srctype);
      if (TREE_CODE (desttype) == ARRAY_TYPE
	  && !tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len))
	desttype = TREE_TYPE (desttype);
      if (TREE_ADDRESSABLE (srctype)
	  || TREE_ADDRESSABLE (desttype))
	return false;

      /* Make sure we are not copying using a floating-point mode or
         a type whose size possibly does not match its precision.  */
      if (FLOAT_MODE_P (TYPE_MODE (desttype))
	  || TREE_CODE (desttype) == BOOLEAN_TYPE
	  || TREE_CODE (desttype) == ENUMERAL_TYPE)
	desttype = bitwise_type_for_mode (TYPE_MODE (desttype));
      if (FLOAT_MODE_P (TYPE_MODE (srctype))
	  || TREE_CODE (srctype) == BOOLEAN_TYPE
	  || TREE_CODE (srctype) == ENUMERAL_TYPE)
	srctype = bitwise_type_for_mode (TYPE_MODE (srctype));
      if (!srctype)
	srctype = desttype;
      if (!desttype)
	desttype = srctype;
      if (!srctype)
	return false;

      src_align = get_pointer_alignment (src);
      dest_align = get_pointer_alignment (dest);

      /* Choose between src and destination type for the access based
         on alignment, whether the access constitutes a register access
	 and whether it may actually expose a declaration for SSA rewrite
	 or SRA decomposition.  Also try to expose a string constant, we
	 might be able to concatenate several of them later into a single
	 string store.  */
      destvar = NULL_TREE;
      srcvar = NULL_TREE;
      if (TREE_CODE (dest) == ADDR_EXPR
	  && var_decl_component_p (TREE_OPERAND (dest, 0))
	  && tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len)
	  && dest_align >= TYPE_ALIGN (desttype)
	  && (is_gimple_reg_type (desttype)
	      || src_align >= TYPE_ALIGN (desttype)))
	destvar = fold_build2 (MEM_REF, desttype, dest, off0);
      else if (TREE_CODE (src) == ADDR_EXPR
	       && var_decl_component_p (TREE_OPERAND (src, 0))
	       && tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len)
	       && src_align >= TYPE_ALIGN (srctype)
	       && (is_gimple_reg_type (srctype)
		   || dest_align >= TYPE_ALIGN (srctype)))
	srcvar = fold_build2 (MEM_REF, srctype, src, off0);
      /* FIXME: Don't transform copies from strings with known original length.
	 As soon as strlenopt tests that rely on it for passing are adjusted,
	 this hack can be removed.  */
      else if (gimple_call_alloca_for_var_p (stmt)
	       && (srcvar = string_constant (src, &srcoff, NULL, NULL))
	       && integer_zerop (srcoff)
	       && tree_int_cst_equal (TYPE_SIZE_UNIT (TREE_TYPE (srcvar)), len)
	       && dest_align >= TYPE_ALIGN (TREE_TYPE (srcvar)))
	srctype = TREE_TYPE (srcvar);
      else
	return false;

      /* Now that we chose an access type express the other side in
         terms of it if the target allows that with respect to alignment
	 constraints.  */
      if (srcvar == NULL_TREE)
	{
	  if (src_align >= TYPE_ALIGN (desttype))
	    srcvar = fold_build2 (MEM_REF, desttype, src, off0);
	  else
	    {
	      enum machine_mode mode = TYPE_MODE (desttype);
	      if ((mode == BLKmode && STRICT_ALIGNMENT)
		  || (targetm.slow_unaligned_access (mode, src_align)
		      && (optab_handler (movmisalign_optab, mode)
			  == CODE_FOR_nothing)))
		return false;
	      srctype = build_aligned_type (TYPE_MAIN_VARIANT (desttype),
					    src_align);
	      srcvar = fold_build2 (MEM_REF, srctype, src, off0);
	    }
	}
      else if (destvar == NULL_TREE)
	{
	  if (dest_align >= TYPE_ALIGN (srctype))
	    destvar = fold_build2 (MEM_REF, srctype, dest, off0);
	  else
	    {
	      enum machine_mode mode = TYPE_MODE (srctype);
	      if ((mode == BLKmode && STRICT_ALIGNMENT)
		  || (targetm.slow_unaligned_access (mode, dest_align)
		      && (optab_handler (movmisalign_optab, mode)
			  == CODE_FOR_nothing)))
		return false;
	      desttype = build_aligned_type (TYPE_MAIN_VARIANT (srctype),
					     dest_align);
	      destvar = fold_build2 (MEM_REF, desttype, dest, off0);
	    }
	}

      /* Same as above, detect out-of-bounds accesses without issuing
	 warnings.  Avoid folding out-of-bounds copies but to avoid
	 false positives for unreachable code defer warning until
	 after DCE has worked its magic.
	 -Wrestrict is still diagnosed.  */
      if (int warning = check_bounds_or_overlap (as_a <gcall *>(stmt),
						 dest, src, len, len,
						 false, false))
	if (warning != OPT_Wrestrict)
	  return false;

      gimple *new_stmt;
      if (is_gimple_reg_type (TREE_TYPE (srcvar)))
	{
	  tree tem = fold_const_aggregate_ref (srcvar);
	  if (tem)
	    srcvar = tem;
	  if (! is_gimple_min_invariant (srcvar))
	    {
	      new_stmt = gimple_build_assign (NULL_TREE, srcvar);
	      srcvar = create_tmp_reg_or_ssa_name (TREE_TYPE (srcvar),
						   new_stmt);
	      gimple_assign_set_lhs (new_stmt, srcvar);
	      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
	      gimple_set_location (new_stmt, loc);
	      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
	    }
	  new_stmt = gimple_build_assign (destvar, srcvar);
	  goto set_vop_and_replace;
	}

      /* We get an aggregate copy.  If the source is a STRING_CST, then
	 directly use its type to perform the copy.  */
      if (TREE_CODE (srcvar) == STRING_CST)
	  desttype = srctype;

      /* Or else, use an unsigned char[] type to perform the copy in order
	 to preserve padding and to avoid any issues with TREE_ADDRESSABLE
	 types or float modes behavior on copying.  */
      else
	{
	  desttype = build_array_type_nelts (unsigned_char_type_node,
					     tree_to_uhwi (len));
	  srctype = desttype;
	  if (src_align > TYPE_ALIGN (srctype))
	    srctype = build_aligned_type (srctype, src_align);
	  srcvar = fold_build2 (MEM_REF, srctype, src, off0);
	}

      if (dest_align > TYPE_ALIGN (desttype))
	desttype = build_aligned_type (desttype, dest_align);
      destvar = fold_build2 (MEM_REF, desttype, dest, off0);
      new_stmt = gimple_build_assign (destvar, srcvar);

set_vop_and_replace:
      gimple_move_vops (new_stmt, stmt);
      if (!lhs)
	{
	  gsi_replace (gsi, new_stmt, false);
	  return true;
	}
      gimple_set_location (new_stmt, loc);
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
    }

done:
  gimple_seq stmts = NULL;
  if (code == BUILT_IN_MEMCPY || code == BUILT_IN_MEMMOVE)
    len = NULL_TREE;
  else if (code == BUILT_IN_MEMPCPY)
    {
      len = gimple_convert_to_ptrofftype (&stmts, loc, len);
      dest = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
			   TREE_TYPE (dest), dest, len);
    }
  else
    gcc_unreachable ();

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  gimple *repl = gimple_build_assign (lhs, dest);
  gsi_replace (gsi, repl, false);
  return true;
}

/* Transform a call to built-in bcmp(a, b, len) at *GSI into one
   to built-in memcmp (a, b, len).  */

static bool
gimple_fold_builtin_bcmp (gimple_stmt_iterator *gsi)
{
  tree fn = builtin_decl_implicit (BUILT_IN_MEMCMP);

  if (!fn)
    return false;

  /* Transform bcmp (a, b, len) into memcmp (a, b, len).  */

  gimple *stmt = gsi_stmt (*gsi);
  tree a = gimple_call_arg (stmt, 0);
  tree b = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);

  gimple *repl = gimple_build_call (fn, 3, a, b, len);
  replace_call_with_call_and_fold (gsi, repl);

  return true;
}

/* Transform a call to built-in bcopy (src, dest, len) at *GSI into one
   to built-in memmove (dest, src, len).  */

static bool
gimple_fold_builtin_bcopy (gimple_stmt_iterator *gsi)
{
  tree fn = builtin_decl_implicit (BUILT_IN_MEMMOVE);

  if (!fn)
    return false;

  /* bcopy has been removed from POSIX in Issue 7 but Issue 6 specifies
     it's quivalent to memmove (not memcpy).  Transform bcopy (src, dest,
     len) into memmove (dest, src, len).  */

  gimple *stmt = gsi_stmt (*gsi);
  tree src = gimple_call_arg (stmt, 0);
  tree dest = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);

  gimple *repl = gimple_build_call (fn, 3, dest, src, len);
  gimple_call_set_fntype (as_a <gcall *> (stmt), TREE_TYPE (fn));
  replace_call_with_call_and_fold (gsi, repl);

  return true;
}

/* Transform a call to built-in bzero (dest, len) at *GSI into one
   to built-in memset (dest, 0, len).  */

static bool
gimple_fold_builtin_bzero (gimple_stmt_iterator *gsi)
{
  tree fn = builtin_decl_implicit (BUILT_IN_MEMSET);

  if (!fn)
    return false;

  /* Transform bzero (dest, len) into memset (dest, 0, len).  */

  gimple *stmt = gsi_stmt (*gsi);
  tree dest = gimple_call_arg (stmt, 0);
  tree len = gimple_call_arg (stmt, 1);

  gimple_seq seq = NULL;
  gimple *repl = gimple_build_call (fn, 3, dest, integer_zero_node, len);
  gimple_seq_add_stmt_without_update (&seq, repl);
  gsi_replace_with_seq_vops (gsi, seq);
  fold_stmt (gsi);

  return true;
}

/* Fold function call to builtin memset or bzero at *GSI setting the
   memory of size LEN to VAL.  Return whether a simplification was made.  */

static bool
gimple_fold_builtin_memset (gimple_stmt_iterator *gsi, tree c, tree len)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree etype;
  unsigned HOST_WIDE_INT length, cval;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      replace_call_with_value (gsi, gimple_call_arg (stmt, 0));
      return true;
    }

  if (! tree_fits_uhwi_p (len))
    return false;

  if (TREE_CODE (c) != INTEGER_CST)
    return false;

  tree dest = gimple_call_arg (stmt, 0);
  tree var = dest;
  if (TREE_CODE (var) != ADDR_EXPR)
    return false;

  var = TREE_OPERAND (var, 0);
  if (TREE_THIS_VOLATILE (var))
    return false;

  etype = TREE_TYPE (var);
  if (TREE_CODE (etype) == ARRAY_TYPE)
    etype = TREE_TYPE (etype);

  if (!INTEGRAL_TYPE_P (etype)
      && !POINTER_TYPE_P (etype))
    return NULL_TREE;

  if (! var_decl_component_p (var))
    return NULL_TREE;

  length = tree_to_uhwi (len);
  if (GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (etype)) != length
      || (GET_MODE_PRECISION (SCALAR_INT_TYPE_MODE (etype))
	  != GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (etype)))
      || get_pointer_alignment (dest) / BITS_PER_UNIT < length)
    return NULL_TREE;

  if (length > HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT)
    return NULL_TREE;

  if (!type_has_mode_precision_p (etype))
    etype = lang_hooks.types.type_for_mode (SCALAR_INT_TYPE_MODE (etype),
					    TYPE_UNSIGNED (etype));

  if (integer_zerop (c))
    cval = 0;
  else
    {
      if (CHAR_BIT != 8 || BITS_PER_UNIT != 8 || HOST_BITS_PER_WIDE_INT > 64)
	return NULL_TREE;

      cval = TREE_INT_CST_LOW (c);
      cval &= 0xff;
      cval |= cval << 8;
      cval |= cval << 16;
      cval |= (cval << 31) << 1;
    }

  var = fold_build2 (MEM_REF, etype, dest, build_int_cst (ptr_type_node, 0));
  gimple *store = gimple_build_assign (var, build_int_cst_type (etype, cval));
  gimple_move_vops (store, stmt);
  gimple_set_location (store, gimple_location (stmt));
  gsi_insert_before (gsi, store, GSI_SAME_STMT);
  if (gimple_call_lhs (stmt))
    {
      gimple *asgn = gimple_build_assign (gimple_call_lhs (stmt), dest);
      gsi_replace (gsi, asgn, false);
    }
  else
    {
      gimple_stmt_iterator gsi2 = *gsi;
      gsi_prev (gsi);
      gsi_remove (&gsi2, true);
    }

  return true;
}

/* Helper of get_range_strlen for ARG that is not an SSA_NAME.  */

static bool
get_range_strlen_tree (tree arg, bitmap visited, strlen_range_kind rkind,
		       c_strlen_data *pdata, unsigned eltsize)
{
  gcc_assert (TREE_CODE (arg) != SSA_NAME);

  /* The length computed by this invocation of the function.  */
  tree val = NULL_TREE;

  /* True if VAL is an optimistic (tight) bound determined from
     the size of the character array in which the string may be
     stored.  In that case, the computed VAL is used to set
     PDATA->MAXBOUND.  */
  bool tight_bound = false;

  /* We can end up with &(*iftmp_1)[0] here as well, so handle it.  */
  if (TREE_CODE (arg) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (arg, 0)) == ARRAY_REF)
    {
      tree op = TREE_OPERAND (arg, 0);
      if (integer_zerop (TREE_OPERAND (op, 1)))
	{
	  tree aop0 = TREE_OPERAND (op, 0);
	  if (TREE_CODE (aop0) == INDIRECT_REF
	      && TREE_CODE (TREE_OPERAND (aop0, 0)) == SSA_NAME)
	    return get_range_strlen (TREE_OPERAND (aop0, 0), visited, rkind,
				     pdata, eltsize);
	}
      else if (TREE_CODE (TREE_OPERAND (op, 0)) == COMPONENT_REF
	       && rkind == SRK_LENRANGE)
	{
	  /* Fail if an array is the last member of a struct object
	     since it could be treated as a (fake) flexible array
	     member.  */
	  tree idx = TREE_OPERAND (op, 1);

	  arg = TREE_OPERAND (op, 0);
	  tree optype = TREE_TYPE (arg);
	  if (tree dom = TYPE_DOMAIN (optype))
	    if (tree bound = TYPE_MAX_VALUE (dom))
	      if (TREE_CODE (bound) == INTEGER_CST
		  && TREE_CODE (idx) == INTEGER_CST
		  && tree_int_cst_lt (bound, idx))
		return false;
	}
    }

  if (rkind == SRK_INT_VALUE)
    {
      /* We are computing the maximum value (not string length).  */
      val = arg;
      if (TREE_CODE (val) != INTEGER_CST
	  || tree_int_cst_sgn (val) < 0)
	return false;
    }
  else
    {
      c_strlen_data lendata = { };
      val = c_strlen (arg, 1, &lendata, eltsize);

      if (!val && lendata.decl)
	{
	  /* ARG refers to an unterminated const character array.
	     DATA.DECL with size DATA.LEN.  */
	  val = lendata.minlen;
	  pdata->decl = lendata.decl;
	}
    }

  /* Set if VAL represents the maximum length based on array size (set
     when exact length cannot be determined).  */
  bool maxbound = false;

  if (!val && rkind == SRK_LENRANGE)
    {
      if (TREE_CODE (arg) == ADDR_EXPR)
	return get_range_strlen (TREE_OPERAND (arg, 0), visited, rkind,
				 pdata, eltsize);

      if (TREE_CODE (arg) == ARRAY_REF)
	{
	  tree optype = TREE_TYPE (TREE_OPERAND (arg, 0));

	  /* Determine the "innermost" array type.  */
	  while (TREE_CODE (optype) == ARRAY_TYPE
		 && TREE_CODE (TREE_TYPE (optype)) == ARRAY_TYPE)
	    optype = TREE_TYPE (optype);

	  /* Avoid arrays of pointers.  */
	  tree eltype = TREE_TYPE (optype);
	  if (TREE_CODE (optype) != ARRAY_TYPE
	      || !INTEGRAL_TYPE_P (eltype))
	    return false;

	  /* Fail when the array bound is unknown or zero.  */
	  val = TYPE_SIZE_UNIT (optype);
	  if (!val
	      || TREE_CODE (val) != INTEGER_CST
	      || integer_zerop (val))
	    return false;

	  val = fold_build2 (MINUS_EXPR, TREE_TYPE (val), val,
			      integer_one_node);

	  /* Set the minimum size to zero since the string in
	     the array could have zero length.  */
	  pdata->minlen = ssize_int (0);

	  tight_bound = true;
	}
      else if (TREE_CODE (arg) == COMPONENT_REF
	       && (TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 1)))
		   == ARRAY_TYPE))
	{
	  /* Use the type of the member array to determine the upper
	     bound on the length of the array.  This may be overly
	     optimistic if the array itself isn't NUL-terminated and
	     the caller relies on the subsequent member to contain
	     the NUL but that would only be considered valid if
	     the array were the last member of a struct.  */

	  tree fld = TREE_OPERAND (arg, 1);

	  tree optype = TREE_TYPE (fld);

	  /* Determine the "innermost" array type.  */
	  while (TREE_CODE (optype) == ARRAY_TYPE
		 && TREE_CODE (TREE_TYPE (optype)) == ARRAY_TYPE)
	    optype = TREE_TYPE (optype);

	  /* Fail when the array bound is unknown or zero.  */
	  val = TYPE_SIZE_UNIT (optype);
	  if (!val
	      || TREE_CODE (val) != INTEGER_CST
	      || integer_zerop (val))
	    return false;
	  val = fold_build2 (MINUS_EXPR, TREE_TYPE (val), val,
			     integer_one_node);

	  /* Set the minimum size to zero since the string in
	     the array could have zero length.  */
	  pdata->minlen = ssize_int (0);

	  /* The array size determined above is an optimistic bound
	     on the length.  If the array isn't nul-terminated the
	     length computed by the library function would be greater.
	     Even though using strlen to cross the subobject boundary
	     is undefined, avoid drawing conclusions from the member
	     type about the length here.  */
	  tight_bound = true;
	}
      else if (TREE_CODE (arg) == MEM_REF
	       && TREE_CODE (TREE_TYPE (arg)) == ARRAY_TYPE
	       && TREE_CODE (TREE_TYPE (TREE_TYPE (arg))) == INTEGER_TYPE
	       && TREE_CODE (TREE_OPERAND (arg, 0)) == ADDR_EXPR)
	{
	  /* Handle a MEM_REF into a DECL accessing an array of integers,
	     being conservative about references to extern structures with
	     flexible array members that can be initialized to arbitrary
	     numbers of elements as an extension (static structs are okay).  */
	  tree ref = TREE_OPERAND (TREE_OPERAND (arg, 0), 0);
	  if ((TREE_CODE (ref) == PARM_DECL || VAR_P (ref))
	      && (decl_binds_to_current_def_p (ref)
		  || !array_ref_flexible_size_p (arg)))
	    {
	      /* Fail if the offset is out of bounds.  Such accesses
		 should be diagnosed at some point.  */
	      val = DECL_SIZE_UNIT (ref);
	      if (!val
		  || TREE_CODE (val) != INTEGER_CST
		  || integer_zerop (val))
		return false;

	      poly_offset_int psiz = wi::to_offset (val);
	      poly_offset_int poff = mem_ref_offset (arg);
	      if (known_le (psiz, poff))
		return false;

	      pdata->minlen = ssize_int (0);

	      /* Subtract the offset and one for the terminating nul.  */
	      psiz -= poff;
	      psiz -= 1;
	      val = wide_int_to_tree (TREE_TYPE (val), psiz);
	      /* Since VAL reflects the size of a declared object
		 rather the type of the access it is not a tight bound.  */
	    }
	}
      else if (TREE_CODE (arg) == PARM_DECL || VAR_P (arg))
	{
	  /* Avoid handling pointers to arrays.  GCC might misuse
	     a pointer to an array of one bound to point to an array
	     object of a greater bound.  */
	  tree argtype = TREE_TYPE (arg);
	  if (TREE_CODE (argtype) == ARRAY_TYPE)
	    {
	      val = TYPE_SIZE_UNIT (argtype);
	      if (!val
		  || TREE_CODE (val) != INTEGER_CST
		  || integer_zerop (val))
		return false;
	      val = wide_int_to_tree (TREE_TYPE (val),
				      wi::sub (wi::to_wide (val), 1));

	      /* Set the minimum size to zero since the string in
		 the array could have zero length.  */
	      pdata->minlen = ssize_int (0);
	    }
	}
      maxbound = true;
    }

  if (!val)
    return false;

  /* Adjust the lower bound on the string length as necessary.  */
  if (!pdata->minlen
      || (rkind != SRK_STRLEN
	  && TREE_CODE (pdata->minlen) == INTEGER_CST
	  && TREE_CODE (val) == INTEGER_CST
	  && tree_int_cst_lt (val, pdata->minlen)))
    pdata->minlen = val;

  if (pdata->maxbound && TREE_CODE (pdata->maxbound) == INTEGER_CST)
    {
      /* Adjust the tighter (more optimistic) string length bound
	 if necessary and proceed to adjust the more conservative
	 bound.  */
      if (TREE_CODE (val) == INTEGER_CST)
	{
	  if (tree_int_cst_lt (pdata->maxbound, val))
	    pdata->maxbound = val;
	}
      else
	pdata->maxbound = val;
    }
  else if (pdata->maxbound || maxbound)
    /* Set PDATA->MAXBOUND only if it either isn't INTEGER_CST or
       if VAL corresponds to the maximum length determined based
       on the type of the object.  */
    pdata->maxbound = val;

  if (tight_bound)
    {
      /* VAL computed above represents an optimistically tight bound
	 on the length of the string based on the referenced object's
	 or subobject's type.  Determine the conservative upper bound
	 based on the enclosing object's size if possible.  */
      if (rkind == SRK_LENRANGE)
	{
	  poly_int64 offset;
	  tree base = get_addr_base_and_unit_offset (arg, &offset);
	  if (!base)
	    {
	      /* When the call above fails due to a non-constant offset
		 assume the offset is zero and use the size of the whole
		 enclosing object instead.  */
	      base = get_base_address (arg);
	      offset = 0;
	    }
	  /* If the base object is a pointer no upper bound on the length
	     can be determined.  Otherwise the maximum length is equal to
	     the size of the enclosing object minus the offset of
	     the referenced subobject minus 1 (for the terminating nul).  */
	  tree type = TREE_TYPE (base);
	  if (TREE_CODE (type) == POINTER_TYPE
	      || (TREE_CODE (base) != PARM_DECL && !VAR_P (base))
	      || !(val = DECL_SIZE_UNIT (base)))
	    val = build_all_ones_cst (size_type_node);
	  else
	    {
	      val = DECL_SIZE_UNIT (base);
	      val = fold_build2 (MINUS_EXPR, TREE_TYPE (val), val,
				 size_int (offset + 1));
	    }
	}
      else
	return false;
    }

  if (pdata->maxlen)
    {
      /* Adjust the more conservative bound if possible/necessary
	 and fail otherwise.  */
      if (rkind != SRK_STRLEN)
	{
	  if (TREE_CODE (pdata->maxlen) != INTEGER_CST
	      || TREE_CODE (val) != INTEGER_CST)
	    return false;

	  if (tree_int_cst_lt (pdata->maxlen, val))
	    pdata->maxlen = val;
	  return true;
	}
      else if (simple_cst_equal (val, pdata->maxlen) != 1)
	{
	  /* Fail if the length of this ARG is different from that
	     previously determined from another ARG.  */
	  return false;
	}
    }

  pdata->maxlen = val;
  return rkind == SRK_LENRANGE || !integer_all_onesp (val);
}

/* For an ARG referencing one or more strings, try to obtain the range
   of their lengths, or the size of the largest array ARG referes to if
   the range of lengths cannot be determined, and store all in *PDATA.
   For an integer ARG (when RKIND == SRK_INT_VALUE), try to determine
   the maximum constant value.
   If ARG is an SSA_NAME, follow its use-def chains.  When RKIND ==
   SRK_STRLEN, then if PDATA->MAXLEN is not equal to the determined
   length or if we are unable to determine the length, return false.
   VISITED is a bitmap of visited variables.
   RKIND determines the kind of value or range to obtain (see
   strlen_range_kind).
   Set PDATA->DECL if ARG refers to an unterminated constant array.
   On input, set ELTSIZE to 1 for normal single byte character strings,
   and either 2 or 4 for wide characer strings (the size of wchar_t).
   Return true if *PDATA was successfully populated and false otherwise.  */

static bool
get_range_strlen (tree arg, bitmap visited,
		  strlen_range_kind rkind,
		  c_strlen_data *pdata, unsigned eltsize)
{

  if (TREE_CODE (arg) != SSA_NAME)
    return get_range_strlen_tree (arg, visited, rkind, pdata, eltsize);

  /* If ARG is registered for SSA update we cannot look at its defining
     statement.  */
  if (name_registered_for_update_p (arg))
    return false;

  /* If we were already here, break the infinite cycle.  */
  if (!bitmap_set_bit (visited, SSA_NAME_VERSION (arg)))
    return true;

  tree var = arg;
  gimple *def_stmt = SSA_NAME_DEF_STMT (var);

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
	    return get_range_strlen (rhs, visited, rkind, pdata, eltsize);
          }
	else if (gimple_assign_rhs_code (def_stmt) == COND_EXPR)
	  {
	    tree ops[2] = { gimple_assign_rhs2 (def_stmt),
			    gimple_assign_rhs3 (def_stmt) };

	    for (unsigned int i = 0; i < 2; i++)
	      if (!get_range_strlen (ops[i], visited, rkind, pdata, eltsize))
		{
		  if (rkind != SRK_LENRANGE)
		    return false;
		  /* Set the upper bound to the maximum to prevent
		     it from being adjusted in the next iteration but
		     leave MINLEN and the more conservative MAXBOUND
		     determined so far alone (or leave them null if
		     they haven't been set yet).  That the MINLEN is
		     in fact zero can be determined from MAXLEN being
		     unbounded but the discovered minimum is used for
		     diagnostics.  */
		  pdata->maxlen = build_all_ones_cst (size_type_node);
		}
	    return true;
	  }
        return false;

      case GIMPLE_PHI:
	/* Unless RKIND == SRK_LENRANGE, all arguments of the PHI node
	   must have a constant length.  */
	for (unsigned i = 0; i < gimple_phi_num_args (def_stmt); i++)
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

	    if (!get_range_strlen (arg, visited, rkind, pdata, eltsize))
	      {
		if (rkind != SRK_LENRANGE)
		  return false;
		/* Set the upper bound to the maximum to prevent
		   it from being adjusted in the next iteration but
		   leave MINLEN and the more conservative MAXBOUND
		   determined so far alone (or leave them null if
		   they haven't been set yet).  That the MINLEN is
		   in fact zero can be determined from MAXLEN being
		   unbounded but the discovered minimum is used for
		   diagnostics.  */
		pdata->maxlen = build_all_ones_cst (size_type_node);
	      }
          }
        return true;

      default:
        return false;
    }
}

/* Try to obtain the range of the lengths of the string(s) referenced
   by ARG, or the size of the largest array ARG refers to if the range
   of lengths cannot be determined, and store all in *PDATA which must
   be zero-initialized on input except PDATA->MAXBOUND may be set to
   a non-null tree node other than INTEGER_CST to request to have it
   set to the length of the longest string in a PHI.  ELTSIZE is
   the expected size of the string element in bytes: 1 for char and
   some power of 2 for wide characters.
   Return true if the range [PDATA->MINLEN, PDATA->MAXLEN] is suitable
   for optimization.  Returning false means that a nonzero PDATA->MINLEN
   doesn't reflect the true lower bound of the range when PDATA->MAXLEN
   is -1 (in that case, the actual range is indeterminate, i.e.,
   [0, PTRDIFF_MAX - 2].  */

bool
get_range_strlen (tree arg, c_strlen_data *pdata, unsigned eltsize)
{
  auto_bitmap visited;
  tree maxbound = pdata->maxbound;

  if (!get_range_strlen (arg, visited, SRK_LENRANGE, pdata, eltsize))
    {
      /* On failure extend the length range to an impossible maximum
	 (a valid MAXLEN must be less than PTRDIFF_MAX - 1).  Other
	 members can stay unchanged regardless.  */
      pdata->minlen = ssize_int (0);
      pdata->maxlen = build_all_ones_cst (size_type_node);
    }
  else if (!pdata->minlen)
    pdata->minlen = ssize_int (0);

  /* If it's unchanged from it initial non-null value, set the conservative
     MAXBOUND to SIZE_MAX.  Otherwise leave it null (if it is null).  */
  if (maxbound && pdata->maxbound == maxbound)
    pdata->maxbound = build_all_ones_cst (size_type_node);

  return !integer_all_onesp (pdata->maxlen);
}

/* Return the maximum value for ARG given RKIND (see strlen_range_kind).
   For ARG of pointer types, NONSTR indicates if the caller is prepared
   to handle unterminated strings.   For integer ARG and when RKIND ==
   SRK_INT_VALUE, NONSTR must be null.

   If an unterminated array is discovered and our caller handles
   unterminated arrays, then bubble up the offending DECL and
   return the maximum size.  Otherwise return NULL.  */

static tree
get_maxval_strlen (tree arg, strlen_range_kind rkind, tree *nonstr = NULL)
{
  /* A non-null NONSTR is meaningless when determining the maximum
     value of an integer ARG.  */
  gcc_assert (rkind != SRK_INT_VALUE || nonstr == NULL);
  /* ARG must have an integral type when RKIND says so.  */
  gcc_assert (rkind != SRK_INT_VALUE || INTEGRAL_TYPE_P (TREE_TYPE (arg)));

  auto_bitmap visited;

  /* Reset DATA.MAXLEN if the call fails or when DATA.MAXLEN
     is unbounded.  */
  c_strlen_data lendata = { };
  if (!get_range_strlen (arg, visited, rkind, &lendata, /* eltsize = */1))
    lendata.maxlen = NULL_TREE;
  else if (lendata.maxlen && integer_all_onesp (lendata.maxlen))
    lendata.maxlen = NULL_TREE;

  if (nonstr)
    {
      /* For callers prepared to handle unterminated arrays set
	 *NONSTR to point to the declaration of the array and return
	 the maximum length/size. */
      *nonstr = lendata.decl;
      return lendata.maxlen;
    }

  /* Fail if the constant array isn't nul-terminated.  */
  return lendata.decl ? NULL_TREE : lendata.maxlen;
}

/* Return true if LEN is known to be less than or equal to (or if STRICT is
   true, strictly less than) the lower bound of SIZE at compile time and false
   otherwise.  */

static bool
known_lower (gimple *stmt, tree len, tree size, bool strict = false)
{
  if (len == NULL_TREE)
    return false;

  wide_int size_range[2];
  wide_int len_range[2];
  if (get_range (len, stmt, len_range) && get_range (size, stmt, size_range))
    {
      if (strict)
	return wi::ltu_p (len_range[1], size_range[0]);
      else
       return wi::leu_p (len_range[1], size_range[0]);
    }

  return false;
}

/* Fold function call to builtin strcpy with arguments DEST and SRC.
   If LEN is not NULL, it represents the length of the string to be
   copied.  Return NULL_TREE if no simplification can be made.  */

static bool
gimple_fold_builtin_strcpy (gimple_stmt_iterator *gsi,
			    tree dest, tree src)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree fn;

  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (operand_equal_p (src, dest, 0))
    {
      /* Issue -Wrestrict unless the pointers are null (those do
	 not point to objects and so do not indicate an overlap;
	 such calls could be the result of sanitization and jump
	 threading).  */
      if (!integer_zerop (dest) && !warning_suppressed_p (stmt, OPT_Wrestrict))
	{
	  tree func = gimple_call_fndecl (stmt);

	  warning_at (loc, OPT_Wrestrict,
		      "%qD source argument is the same as destination",
		      func);
	}

      replace_call_with_value (gsi, dest);
      return true;
    }

  if (optimize_function_for_size_p (cfun))
    return false;

  fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
  if (!fn)
    return false;

  /* Set to non-null if ARG refers to an unterminated array.  */
  tree nonstr = NULL;
  tree len = get_maxval_strlen (src, SRK_STRLEN, &nonstr);

  if (nonstr)
    {
      /* Avoid folding calls with unterminated arrays.  */
      if (!warning_suppressed_p (stmt, OPT_Wstringop_overread))
	warn_string_no_nul (loc, stmt, "strcpy", src, nonstr);
      suppress_warning (stmt, OPT_Wstringop_overread);
      return false;
    }

  if (!len)
    return false;

  len = fold_convert_loc (loc, size_type_node, len);
  len = size_binop_loc (loc, PLUS_EXPR, len, build_int_cst (size_type_node, 1));
  len = force_gimple_operand_gsi (gsi, len, true,
				  NULL_TREE, true, GSI_SAME_STMT);
  gimple *repl = gimple_build_call (fn, 3, dest, src, len);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Fold function call to builtin strncpy with arguments DEST, SRC, and LEN.
   If SLEN is not NULL, it represents the length of the source string.
   Return NULL_TREE if no simplification can be made.  */

static bool
gimple_fold_builtin_strncpy (gimple_stmt_iterator *gsi,
			     tree dest, tree src, tree len)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  bool nonstring = get_attr_nonstring_decl (dest) != NULL_TREE;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      /* Avoid warning if the destination refers to an array/pointer
	 decorate with attribute nonstring.  */
      if (!nonstring)
	{
	  tree fndecl = gimple_call_fndecl (stmt);

	  /* Warn about the lack of nul termination: the result is not
	     a (nul-terminated) string.  */
	  tree slen = get_maxval_strlen (src, SRK_STRLEN);
	  if (slen && !integer_zerop (slen))
	    warning_at (loc, OPT_Wstringop_truncation,
			"%qD destination unchanged after copying no bytes "
			"from a string of length %E",
			fndecl, slen);
	  else
	    warning_at (loc, OPT_Wstringop_truncation,
			"%qD destination unchanged after copying no bytes",
			fndecl);
	}

      replace_call_with_value (gsi, dest);
      return true;
    }

  /* We can't compare slen with len as constants below if len is not a
     constant.  */
  if (TREE_CODE (len) != INTEGER_CST)
    return false;

  /* Now, we must be passed a constant src ptr parameter.  */
  tree slen = get_maxval_strlen (src, SRK_STRLEN);
  if (!slen || TREE_CODE (slen) != INTEGER_CST)
    return false;

  /* The size of the source string including the terminating nul.  */
  tree ssize = size_binop_loc (loc, PLUS_EXPR, slen, ssize_int (1));

  /* We do not support simplification of this case, though we do
     support it when expanding trees into RTL.  */
  /* FIXME: generate a call to __builtin_memset.  */
  if (tree_int_cst_lt (ssize, len))
    return false;

  /* Diagnose truncation that leaves the copy unterminated.  */
  maybe_diag_stxncpy_trunc (*gsi, src, len);

  /* OK transform into builtin memcpy.  */
  tree fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
  if (!fn)
    return false;

  len = fold_convert_loc (loc, size_type_node, len);
  len = force_gimple_operand_gsi (gsi, len, true,
				  NULL_TREE, true, GSI_SAME_STMT);
  gimple *repl = gimple_build_call (fn, 3, dest, src, len);
  replace_call_with_call_and_fold (gsi, repl);

  return true;
}

/* Fold function call to builtin strchr or strrchr.
   If both arguments are constant, evaluate and fold the result,
   otherwise simplify str(r)chr (str, 0) into str + strlen (str).
   In general strlen is significantly faster than strchr
   due to being a simpler operation.  */
static bool
gimple_fold_builtin_strchr (gimple_stmt_iterator *gsi, bool is_strrchr)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree str = gimple_call_arg (stmt, 0);
  tree c = gimple_call_arg (stmt, 1);
  location_t loc = gimple_location (stmt);
  const char *p;
  char ch;

  if (!gimple_call_lhs (stmt))
    return false;

  /* Avoid folding if the first argument is not a nul-terminated array.
     Defer warning until later.  */
  if (!check_nul_terminated_array (NULL_TREE, str))
    return false;

  if ((p = c_getstr (str)) && target_char_cst_p (c, &ch))
    {
      const char *p1 = is_strrchr ? strrchr (p, ch) : strchr (p, ch);

      if (p1 == NULL)
	{
	  replace_call_with_value (gsi, integer_zero_node);
	  return true;
	}

      tree len = build_int_cst (size_type_node, p1 - p);
      gimple_seq stmts = NULL;
      gimple *new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
					      POINTER_PLUS_EXPR, str, len);
      gimple_seq_add_stmt_without_update (&stmts, new_stmt);
      gsi_replace_with_seq_vops (gsi, stmts);
      return true;
    }

  if (!integer_zerop (c))
    return false;

  /* Transform strrchr (s, 0) to strchr (s, 0) when optimizing for size.  */
  if (is_strrchr && optimize_function_for_size_p (cfun))
    {
      tree strchr_fn = builtin_decl_implicit (BUILT_IN_STRCHR);

      if (strchr_fn)
	{
	  gimple *repl = gimple_build_call (strchr_fn, 2, str, c);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}

      return false;
    }

  tree len;
  tree strlen_fn = builtin_decl_implicit (BUILT_IN_STRLEN);

  if (!strlen_fn)
    return false;

  /* Create newstr = strlen (str).  */
  gimple_seq stmts = NULL;
  gimple *new_stmt = gimple_build_call (strlen_fn, 1, str);
  gimple_set_location (new_stmt, loc);
  len = create_tmp_reg_or_ssa_name (size_type_node);
  gimple_call_set_lhs (new_stmt, len);
  gimple_seq_add_stmt_without_update (&stmts, new_stmt);

  /* Create (str p+ strlen (str)).  */
  new_stmt = gimple_build_assign (gimple_call_lhs (stmt),
				  POINTER_PLUS_EXPR, str, len);
  gimple_seq_add_stmt_without_update (&stmts, new_stmt);
  gsi_replace_with_seq_vops (gsi, stmts);
  /* gsi now points at the assignment to the lhs, get a
     stmt iterator to the strlen.
     ???  We can't use gsi_for_stmt as that doesn't work when the
     CFG isn't built yet.  */
  gimple_stmt_iterator gsi2 = *gsi;
  gsi_prev (&gsi2);
  fold_stmt (&gsi2);
  return true;
}

/* Fold function call to builtin strstr.
   If both arguments are constant, evaluate and fold the result,
   additionally fold strstr (x, "") into x and strstr (x, "c")
   into strchr (x, 'c').  */
static bool
gimple_fold_builtin_strstr (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  if (!gimple_call_lhs (stmt))
    return false;

  tree haystack = gimple_call_arg (stmt, 0);
  tree needle = gimple_call_arg (stmt, 1);

  /* Avoid folding if either argument is not a nul-terminated array.
     Defer warning until later.  */
  if (!check_nul_terminated_array (NULL_TREE, haystack)
      || !check_nul_terminated_array (NULL_TREE, needle))
    return false;

  const char *q = c_getstr (needle);
  if (q == NULL)
    return false;

  if (const char *p = c_getstr (haystack))
    {
      const char *r = strstr (p, q);

      if (r == NULL)
	{
	  replace_call_with_value (gsi, integer_zero_node);
	  return true;
	}

      tree len = build_int_cst (size_type_node, r - p);
      gimple_seq stmts = NULL;
      gimple *new_stmt
	= gimple_build_assign (gimple_call_lhs (stmt), POINTER_PLUS_EXPR,
			       haystack, len);
      gimple_seq_add_stmt_without_update (&stmts, new_stmt);
      gsi_replace_with_seq_vops (gsi, stmts);
      return true;
    }

  /* For strstr (x, "") return x.  */
  if (q[0] == '\0')
    {
      replace_call_with_value (gsi, haystack);
      return true;
    }

  /* Transform strstr (x, "c") into strchr (x, 'c').  */
  if (q[1] == '\0')
    {
      tree strchr_fn = builtin_decl_implicit (BUILT_IN_STRCHR);
      if (strchr_fn)
	{
	  tree c = build_int_cst (integer_type_node, q[0]);
	  gimple *repl = gimple_build_call (strchr_fn, 2, haystack, c);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  return false;
}

/* Simplify a call to the strcat builtin.  DST and SRC are the arguments
   to the call.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.

   The simplified form may be a constant or other expression which
   computes the same value, but in a more efficient manner (including
   calls to other builtin functions).

   The call may contain arguments which need to be evaluated, but
   which are not useful to determine the result of the call.  In
   this case we return a chain of COMPOUND_EXPRs.  The LHS of each
   COMPOUND_EXPR will be an argument which must be evaluated.
   COMPOUND_EXPRs are chained through their RHS.  The RHS of the last
   COMPOUND_EXPR in the chain will contain the tree for the simplified
   form of the builtin function call.  */

static bool
gimple_fold_builtin_strcat (gimple_stmt_iterator *gsi, tree dst, tree src)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);

  const char *p = c_getstr (src);

  /* If the string length is zero, return the dst parameter.  */
  if (p && *p == '\0')
    {
      replace_call_with_value (gsi, dst);
      return true;
    }

  if (!optimize_bb_for_speed_p (gimple_bb (stmt)))
    return false;

  /* See if we can store by pieces into (dst + strlen(dst)).  */
  tree newdst;
  tree strlen_fn = builtin_decl_implicit (BUILT_IN_STRLEN);
  tree memcpy_fn = builtin_decl_implicit (BUILT_IN_MEMCPY);

  if (!strlen_fn || !memcpy_fn)
    return false;

  /* If the length of the source string isn't computable don't
     split strcat into strlen and memcpy.  */
  tree len = get_maxval_strlen (src, SRK_STRLEN);
  if (! len)
    return false;

  /* Create strlen (dst).  */
  gimple_seq stmts = NULL, stmts2;
  gimple *repl = gimple_build_call (strlen_fn, 1, dst);
  gimple_set_location (repl, loc);
  newdst = create_tmp_reg_or_ssa_name (size_type_node);
  gimple_call_set_lhs (repl, newdst);
  gimple_seq_add_stmt_without_update (&stmts, repl);

  /* Create (dst p+ strlen (dst)).  */
  newdst = fold_build_pointer_plus_loc (loc, dst, newdst);
  newdst = force_gimple_operand (newdst, &stmts2, true, NULL_TREE);
  gimple_seq_add_seq_without_update (&stmts, stmts2);

  len = fold_convert_loc (loc, size_type_node, len);
  len = size_binop_loc (loc, PLUS_EXPR, len,
			build_int_cst (size_type_node, 1));
  len = force_gimple_operand (len, &stmts2, true, NULL_TREE);
  gimple_seq_add_seq_without_update (&stmts, stmts2);

  repl = gimple_build_call (memcpy_fn, 3, newdst, src, len);
  gimple_seq_add_stmt_without_update (&stmts, repl);
  if (gimple_call_lhs (stmt))
    {
      repl = gimple_build_assign (gimple_call_lhs (stmt), dst);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      gsi_replace_with_seq_vops (gsi, stmts);
      /* gsi now points at the assignment to the lhs, get a
         stmt iterator to the memcpy call.
	 ???  We can't use gsi_for_stmt as that doesn't work when the
	 CFG isn't built yet.  */
      gimple_stmt_iterator gsi2 = *gsi;
      gsi_prev (&gsi2);
      fold_stmt (&gsi2);
    }
  else
    {
      gsi_replace_with_seq_vops (gsi, stmts);
      fold_stmt (gsi);
    }
  return true;
}

/* Fold a call to the __strcat_chk builtin FNDECL.  DEST, SRC, and SIZE
   are the arguments to the call.  */

static bool
gimple_fold_builtin_strcat_chk (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree dest = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree size = gimple_call_arg (stmt, 2);
  tree fn;
  const char *p;


  p = c_getstr (src);
  /* If the SRC parameter is "", return DEST.  */
  if (p && *p == '\0')
    {
      replace_call_with_value (gsi, dest);
      return true;
    }

  if (! tree_fits_uhwi_p (size) || ! integer_all_onesp (size))
    return false;

  /* If __builtin_strcat_chk is used, assume strcat is available.  */
  fn = builtin_decl_explicit (BUILT_IN_STRCAT);
  if (!fn)
    return false;

  gimple *repl = gimple_build_call (fn, 2, dest, src);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Simplify a call to the strncat builtin.  */

static bool
gimple_fold_builtin_strncat (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree dst = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);
  tree src_len = c_strlen (src, 1);

  /* If the requested length is zero, or the src parameter string
     length is zero, return the dst parameter.  */
  if (integer_zerop (len) || (src_len && integer_zerop (src_len)))
    {
      replace_call_with_value (gsi, dst);
      return true;
    }

  /* Return early if the requested len is less than the string length.
     Warnings will be issued elsewhere later.  */
  if (!src_len || known_lower (stmt, len, src_len, true))
    return false;

  /* Warn on constant LEN.  */
  if (TREE_CODE (len) == INTEGER_CST)
    {
      bool nowarn = warning_suppressed_p (stmt, OPT_Wstringop_overflow_);
      tree dstsize;

      if (!nowarn && compute_builtin_object_size (dst, 1, &dstsize)
	  && TREE_CODE (dstsize) == INTEGER_CST)
	{
	  int cmpdst = tree_int_cst_compare (len, dstsize);

	  if (cmpdst >= 0)
	    {
	      tree fndecl = gimple_call_fndecl (stmt);

	      /* Strncat copies (at most) LEN bytes and always appends
		 the terminating NUL so the specified bound should never
		 be equal to (or greater than) the size of the destination.
		 If it is, the copy could overflow.  */
	      location_t loc = gimple_location (stmt);
	      nowarn = warning_at (loc, OPT_Wstringop_overflow_,
				   cmpdst == 0
				   ? G_("%qD specified bound %E equals "
					"destination size")
				   : G_("%qD specified bound %E exceeds "
					"destination size %E"),
				   fndecl, len, dstsize);
	      if (nowarn)
		suppress_warning (stmt, OPT_Wstringop_overflow_);
	    }
	}

      if (!nowarn && TREE_CODE (src_len) == INTEGER_CST
	  && tree_int_cst_compare (src_len, len) == 0)
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  location_t loc = gimple_location (stmt);

	  /* To avoid possible overflow the specified bound should also
	     not be equal to the length of the source, even when the size
	     of the destination is unknown (it's not an uncommon mistake
	     to specify as the bound to strncpy the length of the source).  */
	  if (warning_at (loc, OPT_Wstringop_overflow_,
			  "%qD specified bound %E equals source length",
			  fndecl, len))
	    suppress_warning (stmt, OPT_Wstringop_overflow_);
	}
    }

  if (!known_lower (stmt, src_len, len))
    return false;

  tree fn = builtin_decl_implicit (BUILT_IN_STRCAT);

  /* If the replacement _DECL isn't initialized, don't do the
     transformation.  */
  if (!fn)
    return false;

  /* Otherwise, emit a call to strcat.  */
  gcall *repl = gimple_build_call (fn, 2, dst, src);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Fold a call to the __strncat_chk builtin with arguments DEST, SRC,
   LEN, and SIZE.  */

static bool 
gimple_fold_builtin_strncat_chk (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree dest = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);
  tree size = gimple_call_arg (stmt, 3);
  tree fn;
  const char *p;

  p = c_getstr (src);
  /* If the SRC parameter is "" or if LEN is 0, return DEST.  */
  if ((p && *p == '\0')
      || integer_zerop (len))
    {
      replace_call_with_value (gsi, dest);
      return true;
    }

  if (! integer_all_onesp (size))
    {
      tree src_len = c_strlen (src, 1);
      if (known_lower (stmt, src_len, len))
	{
	  /* If LEN >= strlen (SRC), optimize into __strcat_chk.  */
	  fn = builtin_decl_explicit (BUILT_IN_STRCAT_CHK);
	  if (!fn)
	    return false;

	  gimple *repl = gimple_build_call (fn, 3, dest, src, size);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
      return false;
    }

  /* If __builtin_strncat_chk is used, assume strncat is available.  */
  fn = builtin_decl_explicit (BUILT_IN_STRNCAT);
  if (!fn)
    return false;

  gimple *repl = gimple_build_call (fn, 3, dest, src, len);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Build and append gimple statements to STMTS that would load a first
   character of a memory location identified by STR.  LOC is location
   of the statement.  */

static tree
gimple_load_first_char (location_t loc, tree str, gimple_seq *stmts)
{
  tree var;

  tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
  tree cst_uchar_ptr_node
    = build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);
  tree off0 = build_int_cst (cst_uchar_ptr_node, 0);

  tree temp = fold_build2_loc (loc, MEM_REF, cst_uchar_node, str, off0);
  gassign *stmt = gimple_build_assign (NULL_TREE, temp);
  var = create_tmp_reg_or_ssa_name (cst_uchar_node, stmt);

  gimple_assign_set_lhs (stmt, var);
  gimple_seq_add_stmt_without_update (stmts, stmt);

  return var;
}

/* Fold a call to the str{n}{case}cmp builtin pointed by GSI iterator.  */

static bool
gimple_fold_builtin_string_compare (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree callee = gimple_call_fndecl (stmt);
  enum built_in_function fcode = DECL_FUNCTION_CODE (callee);

  tree type = integer_type_node;
  tree str1 = gimple_call_arg (stmt, 0);
  tree str2 = gimple_call_arg (stmt, 1);
  tree lhs = gimple_call_lhs (stmt);

  tree bound_node = NULL_TREE;
  unsigned HOST_WIDE_INT bound = HOST_WIDE_INT_M1U;

  /* Handle strncmp and strncasecmp functions.  */
  if (gimple_call_num_args (stmt) == 3)
    {
      bound_node = gimple_call_arg (stmt, 2);
      if (tree_fits_uhwi_p (bound_node))
	bound = tree_to_uhwi (bound_node);
    }

  /* If the BOUND parameter is zero, return zero.  */
  if (bound == 0)
    {
      replace_call_with_value (gsi, integer_zero_node);
      return true;
    }

  /* If ARG1 and ARG2 are the same (and not volatile), return zero.  */
  if (operand_equal_p (str1, str2, 0))
    {
      replace_call_with_value (gsi, integer_zero_node);
      return true;
    }

  /* Initially set to the number of characters, including the terminating
     nul if each array has one.   LENx == strnlen (Sx, LENx) implies that
     the array Sx is not terminated by a nul.
     For nul-terminated strings then adjusted to their length so that
     LENx == NULPOSx holds.  */
  unsigned HOST_WIDE_INT len1 = HOST_WIDE_INT_MAX, len2 = len1;
  const char *p1 = getbyterep (str1, &len1);
  const char *p2 = getbyterep (str2, &len2);

  /* The position of the terminating nul character if one exists, otherwise
     a value greater than LENx.  */
  unsigned HOST_WIDE_INT nulpos1 = HOST_WIDE_INT_MAX, nulpos2 = nulpos1;

  if (p1)
    {
      size_t n = strnlen (p1, len1);
      if (n < len1)
	len1 = nulpos1 = n;
    }

  if (p2)
    {
      size_t n = strnlen (p2, len2);
      if (n < len2)
	len2 = nulpos2 = n;
    }

  /* For known strings, return an immediate value.  */
  if (p1 && p2)
    {
      int r = 0;
      bool known_result = false;

      switch (fcode)
	{
	case BUILT_IN_STRCMP:
	case BUILT_IN_STRCMP_EQ:
	  if (len1 != nulpos1 || len2 != nulpos2)
	    break;

	  r = strcmp (p1, p2);
	  known_result = true;
	  break;

	case BUILT_IN_STRNCMP:
	case BUILT_IN_STRNCMP_EQ:
	  {
	    if (bound == HOST_WIDE_INT_M1U)
	      break;

	    /* Reduce the bound to be no more than the length
	       of the shorter of the two strings, or the sizes
	       of the unterminated arrays.  */
	    unsigned HOST_WIDE_INT n = bound;

	    if (len1 == nulpos1 && len1 < n)
	      n = len1 + 1;
	    if (len2 == nulpos2 && len2 < n)
	      n = len2 + 1;

	    if (MIN (nulpos1, nulpos2) + 1 < n)
	      break;

	    r = strncmp (p1, p2, n);
	    known_result = true;
	    break;
	  }
	/* Only handleable situation is where the string are equal (result 0),
	   which is already handled by operand_equal_p case.  */
	case BUILT_IN_STRCASECMP:
	  break;
	case BUILT_IN_STRNCASECMP:
	  {
	    if (bound == HOST_WIDE_INT_M1U)
	      break;
	    r = strncmp (p1, p2, bound);
	    if (r == 0)
	      known_result = true;
	    break;
	  }
	default:
	  gcc_unreachable ();
	}

      if (known_result)
	{
	  replace_call_with_value (gsi, build_cmp_result (type, r));
	  return true;
	}
    }

  bool nonzero_bound = (bound >= 1 && bound < HOST_WIDE_INT_M1U)
    || fcode == BUILT_IN_STRCMP
    || fcode == BUILT_IN_STRCMP_EQ
    || fcode == BUILT_IN_STRCASECMP;

  location_t loc = gimple_location (stmt);

  /* If the second arg is "", return *(const unsigned char*)arg1.  */
  if (p2 && *p2 == '\0' && nonzero_bound)
    {
      gimple_seq stmts = NULL;
      tree var = gimple_load_first_char (loc, str1, &stmts);
      if (lhs)
	{
	  stmt = gimple_build_assign (lhs, NOP_EXPR, var);
	  gimple_seq_add_stmt_without_update (&stmts, stmt);
	}

      gsi_replace_with_seq_vops (gsi, stmts);
      return true;
    }

  /* If the first arg is "", return -*(const unsigned char*)arg2.  */
  if (p1 && *p1 == '\0' && nonzero_bound)
    {
      gimple_seq stmts = NULL;
      tree var = gimple_load_first_char (loc, str2, &stmts);

      if (lhs)
	{
	  tree c = create_tmp_reg_or_ssa_name (integer_type_node);
	  stmt = gimple_build_assign (c, NOP_EXPR, var);
	  gimple_seq_add_stmt_without_update (&stmts, stmt);

	  stmt = gimple_build_assign (lhs, NEGATE_EXPR, c);
	  gimple_seq_add_stmt_without_update (&stmts, stmt);
	}

      gsi_replace_with_seq_vops (gsi, stmts);
      return true;
    }

  /* If BOUND is one, return an expression corresponding to
     (*(const unsigned char*)arg2 - *(const unsigned char*)arg1).  */
  if (fcode == BUILT_IN_STRNCMP && bound == 1)
    {
      gimple_seq stmts = NULL;
      tree temp1 = gimple_load_first_char (loc, str1, &stmts);
      tree temp2 = gimple_load_first_char (loc, str2, &stmts);

      if (lhs)
	{
	  tree c1 = create_tmp_reg_or_ssa_name (integer_type_node);
	  gassign *convert1 = gimple_build_assign (c1, NOP_EXPR, temp1);
	  gimple_seq_add_stmt_without_update (&stmts, convert1);

	  tree c2 = create_tmp_reg_or_ssa_name (integer_type_node);
	  gassign *convert2 = gimple_build_assign (c2, NOP_EXPR, temp2);
	  gimple_seq_add_stmt_without_update (&stmts, convert2);

	  stmt = gimple_build_assign (lhs, MINUS_EXPR, c1, c2);
	  gimple_seq_add_stmt_without_update (&stmts, stmt);
	}

      gsi_replace_with_seq_vops (gsi, stmts);
      return true;
    }

  /* If BOUND is greater than the length of one constant string,
     and the other argument is also a nul-terminated string, replace
     strncmp with strcmp.  */
  if (fcode == BUILT_IN_STRNCMP
      && bound > 0 && bound < HOST_WIDE_INT_M1U
      && ((p2 && len2 < bound && len2 == nulpos2)
	  || (p1 && len1 < bound && len1 == nulpos1)))
    {
      tree fn = builtin_decl_implicit (BUILT_IN_STRCMP);
      if (!fn)
        return false;
      gimple *repl = gimple_build_call (fn, 2, str1, str2);
      replace_call_with_call_and_fold (gsi, repl);
      return true;
    }

  return false;
}

/* Fold a call to the memchr pointed by GSI iterator.  */

static bool
gimple_fold_builtin_memchr (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);
  tree arg1 = gimple_call_arg (stmt, 0);
  tree arg2 = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);

  /* If the LEN parameter is zero, return zero.  */
  if (integer_zerop (len))
    {
      replace_call_with_value (gsi, build_int_cst (ptr_type_node, 0));
      return true;
    }

  char c;
  if (TREE_CODE (arg2) != INTEGER_CST
      || !tree_fits_uhwi_p (len)
      || !target_char_cst_p (arg2, &c))
    return false;

  unsigned HOST_WIDE_INT length = tree_to_uhwi (len);
  unsigned HOST_WIDE_INT string_length;
  const char *p1 = getbyterep (arg1, &string_length);

  if (p1)
    {
      const char *r = (const char *)memchr (p1, c, MIN (length, string_length));
      if (r == NULL)
	{
	  tree mem_size, offset_node;
	  byte_representation (arg1, &offset_node, &mem_size, NULL);
	  unsigned HOST_WIDE_INT offset = (offset_node == NULL_TREE)
					  ? 0 : tree_to_uhwi (offset_node);
	  /* MEM_SIZE is the size of the array the string literal
	     is stored in.  */
	  unsigned HOST_WIDE_INT string_size = tree_to_uhwi (mem_size) - offset;
	  gcc_checking_assert (string_length <= string_size);
	  if (length <= string_size)
	    {
	      replace_call_with_value (gsi, build_int_cst (ptr_type_node, 0));
	      return true;
	    }
	}
      else
	{
	  unsigned HOST_WIDE_INT offset = r - p1;
	  gimple_seq stmts = NULL;
	  if (lhs != NULL_TREE)
	    {
	      tree offset_cst = build_int_cst (sizetype, offset);
	      gassign *stmt = gimple_build_assign (lhs, POINTER_PLUS_EXPR,
						   arg1, offset_cst);
	      gimple_seq_add_stmt_without_update (&stmts, stmt);
	    }
	  else
	    gimple_seq_add_stmt_without_update (&stmts,
						gimple_build_nop ());

	  gsi_replace_with_seq_vops (gsi, stmts);
	  return true;
	}
    }

  return false;
}

/* Fold a call to the fputs builtin.  ARG0 and ARG1 are the arguments
   to the call.  IGNORE is true if the value returned
   by the builtin will be ignored.  UNLOCKED is true is true if this
   actually a call to fputs_unlocked.  If LEN in non-NULL, it represents
   the known length of the string.  Return NULL_TREE if no simplification
   was possible.  */

static bool
gimple_fold_builtin_fputs (gimple_stmt_iterator *gsi,
			   tree arg0, tree arg1,
			   bool unlocked)
{
  gimple *stmt = gsi_stmt (*gsi);

  /* If we're using an unlocked function, assume the other unlocked
     functions exist explicitly.  */
  tree const fn_fputc = (unlocked
			 ? builtin_decl_explicit (BUILT_IN_FPUTC_UNLOCKED)
			 : builtin_decl_implicit (BUILT_IN_FPUTC));
  tree const fn_fwrite = (unlocked
			  ? builtin_decl_explicit (BUILT_IN_FWRITE_UNLOCKED)
			  : builtin_decl_implicit (BUILT_IN_FWRITE));

  /* If the return value is used, don't do the transformation.  */
  if (gimple_call_lhs (stmt))
    return false;

  /* Get the length of the string passed to fputs.  If the length
     can't be determined, punt.  */
  tree len = get_maxval_strlen (arg0, SRK_STRLEN);
  if (!len || TREE_CODE (len) != INTEGER_CST)
    return false;

  switch (compare_tree_int (len, 1))
    {
    case -1: /* length is 0, delete the call entirely .  */
      replace_call_with_value (gsi, integer_zero_node);
      return true;

    case 0: /* length is 1, call fputc.  */
      {
	const char *p = c_getstr (arg0);
	if (p != NULL)
	  {
	    if (!fn_fputc)
	      return false;

	    gimple *repl
	      = gimple_build_call (fn_fputc, 2,
				   build_int_cst (integer_type_node, p[0]),
				   arg1);
	    replace_call_with_call_and_fold (gsi, repl);
	    return true;
	  }
      }
      /* FALLTHROUGH */
    case 1: /* length is greater than 1, call fwrite.  */
      {
	/* If optimizing for size keep fputs.  */
	if (optimize_function_for_size_p (cfun))
	  return false;
	/* New argument list transforming fputs(string, stream) to
	   fwrite(string, 1, len, stream).  */
	if (!fn_fwrite)
	  return false;

	gimple *repl
	  = gimple_build_call (fn_fwrite, 4, arg0, size_one_node,
			       fold_convert (size_type_node, len), arg1);
	replace_call_with_call_and_fold (gsi, repl);
	return true;
      }
    default:
      gcc_unreachable ();
    }
}

/* Fold a call to the __mem{cpy,pcpy,move,set}_chk builtin.
   DEST, SRC, LEN, and SIZE are the arguments to the call.
   IGNORE is true, if return value can be ignored.  FCODE is the BUILT_IN_*
   code of the builtin.  If MAXLEN is not NULL, it is maximum length
   passed as third argument.  */

static bool
gimple_fold_builtin_memory_chk (gimple_stmt_iterator *gsi,
				tree dest, tree src, tree len, tree size,
				enum built_in_function fcode)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  bool ignore = gimple_call_lhs (stmt) == NULL_TREE;
  tree fn;

  /* If SRC and DEST are the same (and not volatile), return DEST
     (resp. DEST+LEN for __mempcpy_chk).  */
  if (fcode != BUILT_IN_MEMSET_CHK && operand_equal_p (src, dest, 0))
    {
      if (fcode != BUILT_IN_MEMPCPY_CHK)
	{
	  replace_call_with_value (gsi, dest);
	  return true;
	}
      else
	{
	  gimple_seq stmts = NULL;
	  len = gimple_convert_to_ptrofftype (&stmts, loc, len);
	  tree temp = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
				    TREE_TYPE (dest), dest, len);
	  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	  replace_call_with_value (gsi, temp);
	  return true;
	}
    }

  tree maxlen = get_maxval_strlen (len, SRK_INT_VALUE);
  if (! integer_all_onesp (size)
      && !known_lower (stmt, len, size)
      && !known_lower (stmt, maxlen, size))
    {
      /* MAXLEN and LEN both cannot be proved to be less than SIZE, at
	 least try to optimize (void) __mempcpy_chk () into
	 (void) __memcpy_chk () */
      if (fcode == BUILT_IN_MEMPCPY_CHK && ignore)
	{
	  fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
	  if (!fn)
	    return false;

	  gimple *repl = gimple_build_call (fn, 4, dest, src, len, size);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
      return false;
    }

  fn = NULL_TREE;
  /* If __builtin_mem{cpy,pcpy,move,set}_chk is used, assume
     mem{cpy,pcpy,move,set} is available.  */
  switch (fcode)
    {
    case BUILT_IN_MEMCPY_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMCPY);
      break;
    case BUILT_IN_MEMPCPY_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMPCPY);
      break;
    case BUILT_IN_MEMMOVE_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMMOVE);
      break;
    case BUILT_IN_MEMSET_CHK:
      fn = builtin_decl_explicit (BUILT_IN_MEMSET);
      break;
    default:
      break;
    }

  if (!fn)
    return false;

  gimple *repl = gimple_build_call (fn, 3, dest, src, len);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Print a message in the dump file recording transformation of FROM to TO.  */

static void
dump_transformation (gcall *from, gcall *to)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, from, "simplified %T to %T\n",
		     gimple_call_fn (from), gimple_call_fn (to));
}

/* Fold a call to the __st[rp]cpy_chk builtin.
   DEST, SRC, and SIZE are the arguments to the call.
   IGNORE is true if return value can be ignored.  FCODE is the BUILT_IN_*
   code of the builtin.  If MAXLEN is not NULL, it is maximum length of
   strings passed as second argument.  */

static bool
gimple_fold_builtin_stxcpy_chk (gimple_stmt_iterator *gsi,
				tree dest,
				tree src, tree size,
				enum built_in_function fcode)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  location_t loc = gimple_location (stmt);
  bool ignore = gimple_call_lhs (stmt) == NULL_TREE;
  tree len, fn;

  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (fcode == BUILT_IN_STRCPY_CHK && operand_equal_p (src, dest, 0))
    {
      /* Issue -Wrestrict unless the pointers are null (those do
	 not point to objects and so do not indicate an overlap;
	 such calls could be the result of sanitization and jump
	 threading).  */
      if (!integer_zerop (dest)
	  && !warning_suppressed_p (stmt, OPT_Wrestrict))
	{
	  tree func = gimple_call_fndecl (stmt);

	  warning_at (loc, OPT_Wrestrict,
		      "%qD source argument is the same as destination",
		      func);
	}

      replace_call_with_value (gsi, dest);
      return true;
    }

  tree maxlen = get_maxval_strlen (src, SRK_STRLENMAX);
  if (! integer_all_onesp (size))
    {
      len = c_strlen (src, 1);
      if (!known_lower (stmt, len, size, true)
	  && !known_lower (stmt, maxlen, size, true))
	{
	  if (fcode == BUILT_IN_STPCPY_CHK)
	    {
	      if (! ignore)
		return false;

	      /* If return value of __stpcpy_chk is ignored,
		 optimize into __strcpy_chk.  */
	      fn = builtin_decl_explicit (BUILT_IN_STRCPY_CHK);
	      if (!fn)
		return false;

	      gimple *repl = gimple_build_call (fn, 3, dest, src, size);
	      replace_call_with_call_and_fold (gsi, repl);
	      return true;
	    }

	  if (! len || TREE_SIDE_EFFECTS (len))
	    return false;

	  /* If c_strlen returned something, but not provably less than size,
	     transform __strcpy_chk into __memcpy_chk.  */
	  fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
	  if (!fn)
	    return false;

	  gimple_seq stmts = NULL;
	  len = force_gimple_operand (len, &stmts, true, NULL_TREE);
	  len = gimple_convert (&stmts, loc, size_type_node, len);
	  len = gimple_build (&stmts, loc, PLUS_EXPR, size_type_node, len,
			      build_int_cst (size_type_node, 1));
	  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	  gimple *repl = gimple_build_call (fn, 4, dest, src, len, size);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  /* If __builtin_st{r,p}cpy_chk is used, assume st{r,p}cpy is available.  */
  fn = builtin_decl_explicit (fcode == BUILT_IN_STPCPY_CHK && !ignore
			      ? BUILT_IN_STPCPY : BUILT_IN_STRCPY);
  if (!fn)
    return false;

  gcall *repl = gimple_build_call (fn, 2, dest, src);
  dump_transformation (stmt, repl);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Fold a call to the __st{r,p}ncpy_chk builtin.  DEST, SRC, LEN, and SIZE
   are the arguments to the call.  If MAXLEN is not NULL, it is maximum
   length passed as third argument. IGNORE is true if return value can be
   ignored. FCODE is the BUILT_IN_* code of the builtin. */

static bool
gimple_fold_builtin_stxncpy_chk (gimple_stmt_iterator *gsi,
				 tree dest, tree src,
				 tree len, tree size,
				 enum built_in_function fcode)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  bool ignore = gimple_call_lhs (stmt) == NULL_TREE;
  tree fn;

  tree maxlen = get_maxval_strlen (len, SRK_INT_VALUE);
  if (! integer_all_onesp (size)
      && !known_lower (stmt, len, size) && !known_lower (stmt, maxlen, size))
    {
      if (fcode == BUILT_IN_STPNCPY_CHK && ignore)
	{
	  /* If return value of __stpncpy_chk is ignored,
	     optimize into __strncpy_chk.  */
	  fn = builtin_decl_explicit (BUILT_IN_STRNCPY_CHK);
	  if (fn)
	    {
	      gimple *repl = gimple_build_call (fn, 4, dest, src, len, size);
	      replace_call_with_call_and_fold (gsi, repl);
	      return true;
	    }
	}
      return false;
    }

  /* If __builtin_st{r,p}ncpy_chk is used, assume st{r,p}ncpy is available.  */
  fn = builtin_decl_explicit (fcode == BUILT_IN_STPNCPY_CHK && !ignore
			      ? BUILT_IN_STPNCPY : BUILT_IN_STRNCPY);
  if (!fn)
    return false;

  gcall *repl = gimple_build_call (fn, 3, dest, src, len);
  dump_transformation (stmt, repl);
  replace_call_with_call_and_fold (gsi, repl);
  return true;
}

/* Fold function call to builtin stpcpy with arguments DEST and SRC.
   Return NULL_TREE if no simplification can be made.  */

static bool
gimple_fold_builtin_stpcpy (gimple_stmt_iterator *gsi)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  location_t loc = gimple_location (stmt);
  tree dest = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree fn, lenp1;

  /* If the result is unused, replace stpcpy with strcpy.  */
  if (gimple_call_lhs (stmt) == NULL_TREE)
    {
      tree fn = builtin_decl_implicit (BUILT_IN_STRCPY);
      if (!fn)
	return false;
      gimple_call_set_fndecl (stmt, fn);
      fold_stmt (gsi);
      return true;
    }

  /* Set to non-null if ARG refers to an unterminated array.  */
  c_strlen_data data = { };
  /* The size of the unterminated array if SRC referes to one.  */
  tree size;
  /* True if the size is exact/constant, false if it's the lower bound
     of a range.  */
  bool exact;
  tree len = c_strlen (src, 1, &data, 1);
  if (!len
      || TREE_CODE (len) != INTEGER_CST)
    {
      data.decl = unterminated_array (src, &size, &exact);
      if (!data.decl)
	return false;
    }

  if (data.decl)
    {
      /* Avoid folding calls with unterminated arrays.  */
      if (!warning_suppressed_p (stmt, OPT_Wstringop_overread))
	warn_string_no_nul (loc, stmt, "stpcpy", src, data.decl, size,
			    exact);
      suppress_warning (stmt, OPT_Wstringop_overread);
      return false;
    }

  if (optimize_function_for_size_p (cfun)
      /* If length is zero it's small enough.  */
      && !integer_zerop (len))
    return false;

  /* If the source has a known length replace stpcpy with memcpy.  */
  fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
  if (!fn)
    return false;

  gimple_seq stmts = NULL;
  tree tem = gimple_convert (&stmts, loc, size_type_node, len);
  lenp1 = gimple_build (&stmts, loc, PLUS_EXPR, size_type_node,
			tem, build_int_cst (size_type_node, 1));
  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  gcall *repl = gimple_build_call (fn, 3, dest, src, lenp1);
  gimple_move_vops (repl, stmt);
  gsi_insert_before (gsi, repl, GSI_SAME_STMT);
  /* Replace the result with dest + len.  */
  stmts = NULL;
  tem = gimple_convert (&stmts, loc, sizetype, len);
  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  gassign *ret = gimple_build_assign (gimple_call_lhs (stmt),
				      POINTER_PLUS_EXPR, dest, tem);
  gsi_replace (gsi, ret, false);
  /* Finally fold the memcpy call.  */
  gimple_stmt_iterator gsi2 = *gsi;
  gsi_prev (&gsi2);
  fold_stmt (&gsi2);
  return true;
}

/* Fold a call EXP to {,v}snprintf having NARGS passed as ARGS.  Return
   NULL_TREE if a normal call should be emitted rather than expanding
   the function inline.  FCODE is either BUILT_IN_SNPRINTF_CHK or
   BUILT_IN_VSNPRINTF_CHK.  If MAXLEN is not NULL, it is maximum length
   passed as second argument.  */

static bool
gimple_fold_builtin_snprintf_chk (gimple_stmt_iterator *gsi,
				  enum built_in_function fcode)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree dest, size, len, fn, fmt, flag;
  const char *fmt_str;

  /* Verify the required arguments in the original call.  */
  if (gimple_call_num_args (stmt) < 5)
    return false;

  dest = gimple_call_arg (stmt, 0);
  len = gimple_call_arg (stmt, 1);
  flag = gimple_call_arg (stmt, 2);
  size = gimple_call_arg (stmt, 3);
  fmt = gimple_call_arg (stmt, 4);

  tree maxlen = get_maxval_strlen (len, SRK_INT_VALUE);
  if (! integer_all_onesp (size)
      && !known_lower (stmt, len, size) && !known_lower (stmt, maxlen, size))
    return false;

  if (!init_target_chars ())
    return false;

  /* Only convert __{,v}snprintf_chk to {,v}snprintf if flag is 0
     or if format doesn't contain % chars or is "%s".  */
  if (! integer_zerop (flag))
    {
      fmt_str = c_getstr (fmt);
      if (fmt_str == NULL)
	return false;
      if (strchr (fmt_str, target_percent) != NULL
	  && strcmp (fmt_str, target_percent_s))
	return false;
    }

  /* If __builtin_{,v}snprintf_chk is used, assume {,v}snprintf is
     available.  */
  fn = builtin_decl_explicit (fcode == BUILT_IN_VSNPRINTF_CHK
			      ? BUILT_IN_VSNPRINTF : BUILT_IN_SNPRINTF);
  if (!fn)
    return false;

  /* Replace the called function and the first 5 argument by 3 retaining
     trailing varargs.  */
  gimple_call_set_fndecl (stmt, fn);
  gimple_call_set_fntype (stmt, TREE_TYPE (fn));
  gimple_call_set_arg (stmt, 0, dest);
  gimple_call_set_arg (stmt, 1, len);
  gimple_call_set_arg (stmt, 2, fmt);
  for (unsigned i = 3; i < gimple_call_num_args (stmt) - 2; ++i)
    gimple_call_set_arg (stmt, i, gimple_call_arg (stmt, i + 2));
  gimple_set_num_ops (stmt, gimple_num_ops (stmt) - 2);
  fold_stmt (gsi);
  return true;
}

/* Fold a call EXP to __{,v}sprintf_chk having NARGS passed as ARGS.
   Return NULL_TREE if a normal call should be emitted rather than
   expanding the function inline.  FCODE is either BUILT_IN_SPRINTF_CHK
   or BUILT_IN_VSPRINTF_CHK.  */

static bool
gimple_fold_builtin_sprintf_chk (gimple_stmt_iterator *gsi,
				 enum built_in_function fcode)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree dest, size, len, fn, fmt, flag;
  const char *fmt_str;
  unsigned nargs = gimple_call_num_args (stmt);

  /* Verify the required arguments in the original call.  */
  if (nargs < 4)
    return false;
  dest = gimple_call_arg (stmt, 0);
  flag = gimple_call_arg (stmt, 1);
  size = gimple_call_arg (stmt, 2);
  fmt = gimple_call_arg (stmt, 3);

  len = NULL_TREE;

  if (!init_target_chars ())
    return false;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str != NULL)
    {
      /* If the format doesn't contain % args or %%, we know the size.  */
      if (strchr (fmt_str, target_percent) == 0)
	{
	  if (fcode != BUILT_IN_SPRINTF_CHK || nargs == 4)
	    len = build_int_cstu (size_type_node, strlen (fmt_str));
	}
      /* If the format is "%s" and first ... argument is a string literal,
	 we know the size too.  */
      else if (fcode == BUILT_IN_SPRINTF_CHK
	       && strcmp (fmt_str, target_percent_s) == 0)
	{
	  tree arg;

	  if (nargs == 5)
	    {
	      arg = gimple_call_arg (stmt, 4);
	      if (POINTER_TYPE_P (TREE_TYPE (arg)))
		len = c_strlen (arg, 1);
	    }
	}
    }

  if (! integer_all_onesp (size) && !known_lower (stmt, len, size, true))
    return false;

  /* Only convert __{,v}sprintf_chk to {,v}sprintf if flag is 0
     or if format doesn't contain % chars or is "%s".  */
  if (! integer_zerop (flag))
    {
      if (fmt_str == NULL)
	return false;
      if (strchr (fmt_str, target_percent) != NULL
	  && strcmp (fmt_str, target_percent_s))
	return false;
    }

  /* If __builtin_{,v}sprintf_chk is used, assume {,v}sprintf is available.  */
  fn = builtin_decl_explicit (fcode == BUILT_IN_VSPRINTF_CHK
			      ? BUILT_IN_VSPRINTF : BUILT_IN_SPRINTF);
  if (!fn)
    return false;

  /* Replace the called function and the first 4 argument by 2 retaining
     trailing varargs.  */
  gimple_call_set_fndecl (stmt, fn);
  gimple_call_set_fntype (stmt, TREE_TYPE (fn));
  gimple_call_set_arg (stmt, 0, dest);
  gimple_call_set_arg (stmt, 1, fmt);
  for (unsigned i = 2; i < gimple_call_num_args (stmt) - 2; ++i)
    gimple_call_set_arg (stmt, i, gimple_call_arg (stmt, i + 2));
  gimple_set_num_ops (stmt, gimple_num_ops (stmt) - 2);
  fold_stmt (gsi);
  return true;
}

/* Simplify a call to the sprintf builtin with arguments DEST, FMT, and ORIG.
   ORIG may be null if this is a 2-argument call.  We don't attempt to
   simplify calls with more than 3 arguments.

   Return true if simplification was possible, otherwise false.  */

bool
gimple_fold_builtin_sprintf (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);

  /* Verify the required arguments in the original call.  We deal with two
     types of sprintf() calls: 'sprintf (str, fmt)' and
     'sprintf (dest, "%s", orig)'.  */
  if (gimple_call_num_args (stmt) > 3)
    return false;

  tree orig = NULL_TREE;
  if (gimple_call_num_args (stmt) == 3)
    orig = gimple_call_arg (stmt, 2);

  /* Check whether the format is a literal string constant.  */
  tree fmt = gimple_call_arg (stmt, 1);
  const char *fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return false;

  tree dest = gimple_call_arg (stmt, 0);

  if (!init_target_chars ())
    return false;

  tree fn = builtin_decl_implicit (BUILT_IN_STRCPY);
  if (!fn)
    return false;

  /* If the format doesn't contain % args or %%, use strcpy.  */
  if (strchr (fmt_str, target_percent) == NULL)
    {
      /* Don't optimize sprintf (buf, "abc", ptr++).  */
      if (orig)
	return false;

      /* Convert sprintf (str, fmt) into strcpy (str, fmt) when
	 'format' is known to contain no % formats.  */
      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, fmt);

      /* Propagate the NO_WARNING bit to avoid issuing the same
	 warning more than once.  */
      copy_warning (repl, stmt);

      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (tree lhs = gimple_call_lhs (stmt))
	{
	  repl = gimple_build_assign (lhs, build_int_cst (TREE_TYPE (lhs),
							  strlen (fmt_str)));
	  gimple_seq_add_stmt_without_update (&stmts, repl);
	  gsi_replace_with_seq_vops (gsi, stmts);
	  /* gsi now points at the assignment to the lhs, get a
	     stmt iterator to the memcpy call.
	     ???  We can't use gsi_for_stmt as that doesn't work when the
	     CFG isn't built yet.  */
	  gimple_stmt_iterator gsi2 = *gsi;
	  gsi_prev (&gsi2);
	  fold_stmt (&gsi2);
	}
      else
	{
	  gsi_replace_with_seq_vops (gsi, stmts);
	  fold_stmt (gsi);
	}
      return true;
    }

  /* If the format is "%s", use strcpy if the result isn't used.  */
  else if (fmt_str && strcmp (fmt_str, target_percent_s) == 0)
    {
      /* Don't crash on sprintf (str1, "%s").  */
      if (!orig)
	return false;

      /* Don't fold calls with source arguments of invalid (nonpointer)
	 types.  */
      if (!POINTER_TYPE_P (TREE_TYPE (orig)))
	return false;

      tree orig_len = NULL_TREE;
      if (gimple_call_lhs (stmt))
	{
	  orig_len = get_maxval_strlen (orig, SRK_STRLEN);
	  if (!orig_len)
	    return false;
	}

      /* Convert sprintf (str1, "%s", str2) into strcpy (str1, str2).  */
      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, orig);

      /* Propagate the NO_WARNING bit to avoid issuing the same
	 warning more than once.  */
      copy_warning (repl, stmt);

      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (tree lhs = gimple_call_lhs (stmt))
	{
	  if (!useless_type_conversion_p (TREE_TYPE (lhs),
					  TREE_TYPE (orig_len)))
	    orig_len = fold_convert (TREE_TYPE (lhs), orig_len);
	  repl = gimple_build_assign (lhs, orig_len);
	  gimple_seq_add_stmt_without_update (&stmts, repl);
	  gsi_replace_with_seq_vops (gsi, stmts);
	  /* gsi now points at the assignment to the lhs, get a
	     stmt iterator to the memcpy call.
	     ???  We can't use gsi_for_stmt as that doesn't work when the
	     CFG isn't built yet.  */
	  gimple_stmt_iterator gsi2 = *gsi;
	  gsi_prev (&gsi2);
	  fold_stmt (&gsi2);
	}
      else
	{
	  gsi_replace_with_seq_vops (gsi, stmts);
	  fold_stmt (gsi);
	}
      return true;
    }
  return false;
}

/* Simplify a call to the snprintf builtin with arguments DEST, DESTSIZE,
   FMT, and ORIG.  ORIG may be null if this is a 3-argument call.  We don't
   attempt to simplify calls with more than 4 arguments.

   Return true if simplification was possible, otherwise false.  */

bool
gimple_fold_builtin_snprintf (gimple_stmt_iterator *gsi)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree dest = gimple_call_arg (stmt, 0);
  tree destsize = gimple_call_arg (stmt, 1);
  tree fmt = gimple_call_arg (stmt, 2);
  tree orig = NULL_TREE;
  const char *fmt_str = NULL;

  if (gimple_call_num_args (stmt) > 4)
    return false;

  if (gimple_call_num_args (stmt) == 4)
    orig = gimple_call_arg (stmt, 3);

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return false;

  if (!init_target_chars ())
    return false;

  /* If the format doesn't contain % args or %%, use strcpy.  */
  if (strchr (fmt_str, target_percent) == NULL)
    {
      tree fn = builtin_decl_implicit (BUILT_IN_STRCPY);
      if (!fn)
	return false;

      /* Don't optimize snprintf (buf, 4, "abc", ptr++).  */
      if (orig)
	return false;

      tree len = build_int_cstu (TREE_TYPE (destsize), strlen (fmt_str));

      /* We could expand this as
	 memcpy (str, fmt, cst - 1); str[cst - 1] = '\0';
	 or to
	 memcpy (str, fmt_with_nul_at_cstm1, cst);
	 but in the former case that might increase code size
	 and in the latter case grow .rodata section too much.
	 So punt for now.  */
      if (!known_lower (stmt, len, destsize, true))
	return false;

      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, fmt);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (tree lhs = gimple_call_lhs (stmt))
	{
	  repl = gimple_build_assign (lhs,
				      fold_convert (TREE_TYPE (lhs), len));
	  gimple_seq_add_stmt_without_update (&stmts, repl);
	  gsi_replace_with_seq_vops (gsi, stmts);
	  /* gsi now points at the assignment to the lhs, get a
	     stmt iterator to the memcpy call.
	     ???  We can't use gsi_for_stmt as that doesn't work when the
	     CFG isn't built yet.  */
	  gimple_stmt_iterator gsi2 = *gsi;
	  gsi_prev (&gsi2);
	  fold_stmt (&gsi2);
	}
      else
	{
	  gsi_replace_with_seq_vops (gsi, stmts);
	  fold_stmt (gsi);
	}
      return true;
    }

  /* If the format is "%s", use strcpy if the result isn't used.  */
  else if (fmt_str && strcmp (fmt_str, target_percent_s) == 0)
    {
      tree fn = builtin_decl_implicit (BUILT_IN_STRCPY);
      if (!fn)
	return false;

      /* Don't crash on snprintf (str1, cst, "%s").  */
      if (!orig)
	return false;

      tree orig_len = get_maxval_strlen (orig, SRK_STRLEN);

      /* We could expand this as
	 memcpy (str1, str2, cst - 1); str1[cst - 1] = '\0';
	 or to
	 memcpy (str1, str2_with_nul_at_cstm1, cst);
	 but in the former case that might increase code size
	 and in the latter case grow .rodata section too much.
	 So punt for now.  */
      if (!known_lower (stmt, orig_len, destsize, true))
	return false;

      /* Convert snprintf (str1, cst, "%s", str2) into
	 strcpy (str1, str2) if strlen (str2) < cst.  */
      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, orig);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (tree lhs = gimple_call_lhs (stmt))
	{
	  if (!useless_type_conversion_p (TREE_TYPE (lhs),
					  TREE_TYPE (orig_len)))
	    orig_len = fold_convert (TREE_TYPE (lhs), orig_len);
	  repl = gimple_build_assign (lhs, orig_len);
	  gimple_seq_add_stmt_without_update (&stmts, repl);
	  gsi_replace_with_seq_vops (gsi, stmts);
	  /* gsi now points at the assignment to the lhs, get a
	     stmt iterator to the memcpy call.
	     ???  We can't use gsi_for_stmt as that doesn't work when the
	     CFG isn't built yet.  */
	  gimple_stmt_iterator gsi2 = *gsi;
	  gsi_prev (&gsi2);
	  fold_stmt (&gsi2);
	}
      else
	{
	  gsi_replace_with_seq_vops (gsi, stmts);
	  fold_stmt (gsi);
	}
      return true;
    }
  return false;
}

/* Fold a call to the {,v}fprintf{,_unlocked} and __{,v}printf_chk builtins.
   FP, FMT, and ARG are the arguments to the call.  We don't fold calls with
   more than 3 arguments, and ARG may be null in the 2-argument case.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.  FCODE is the BUILT_IN_*
   code of the function to be simplified.  */

static bool 
gimple_fold_builtin_fprintf (gimple_stmt_iterator *gsi,
			     tree fp, tree fmt, tree arg,
			     enum built_in_function fcode)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree fn_fputc, fn_fputs;
  const char *fmt_str = NULL;

  /* If the return value is used, don't do the transformation.  */
  if (gimple_call_lhs (stmt) != NULL_TREE)
    return false;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return false;

  if (fcode == BUILT_IN_FPRINTF_UNLOCKED)
    {
      /* If we're using an unlocked function, assume the other
	 unlocked functions exist explicitly.  */
      fn_fputc = builtin_decl_explicit (BUILT_IN_FPUTC_UNLOCKED);
      fn_fputs = builtin_decl_explicit (BUILT_IN_FPUTS_UNLOCKED);
    }
  else
    {
      fn_fputc = builtin_decl_implicit (BUILT_IN_FPUTC);
      fn_fputs = builtin_decl_implicit (BUILT_IN_FPUTS);
    }

  if (!init_target_chars ())
    return false;

  /* If the format doesn't contain % args or %%, use strcpy.  */
  if (strchr (fmt_str, target_percent) == NULL)
    {
      if (fcode != BUILT_IN_VFPRINTF && fcode != BUILT_IN_VFPRINTF_CHK
	  && arg)
	return false;

      /* If the format specifier was "", fprintf does nothing.  */
      if (fmt_str[0] == '\0')
	{
	  replace_call_with_value (gsi, NULL_TREE);
	  return true;
	}

      /* When "string" doesn't contain %, replace all cases of
	 fprintf (fp, string) with fputs (string, fp).  The fputs
	 builtin will take care of special cases like length == 1.  */
      if (fn_fputs)
	{
	  gcall *repl = gimple_build_call (fn_fputs, 2, fmt, fp);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  /* The other optimizations can be done only on the non-va_list variants.  */
  else if (fcode == BUILT_IN_VFPRINTF || fcode == BUILT_IN_VFPRINTF_CHK)
    return false;

  /* If the format specifier was "%s", call __builtin_fputs (arg, fp).  */
  else if (strcmp (fmt_str, target_percent_s) == 0)
    {
      if (!arg || ! POINTER_TYPE_P (TREE_TYPE (arg)))
	return false;
      if (fn_fputs)
	{
	  gcall *repl = gimple_build_call (fn_fputs, 2, arg, fp);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  /* If the format specifier was "%c", call __builtin_fputc (arg, fp).  */
  else if (strcmp (fmt_str, target_percent_c) == 0)
    {
      if (!arg
	  || ! useless_type_conversion_p (integer_type_node, TREE_TYPE (arg)))
	return false;
      if (fn_fputc)
	{
	  gcall *repl = gimple_build_call (fn_fputc, 2, arg, fp);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  return false;
}

/* Fold a call to the {,v}printf{,_unlocked} and __{,v}printf_chk builtins.
   FMT and ARG are the arguments to the call; we don't fold cases with
   more than 2 arguments, and ARG may be null if this is a 1-argument case.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.  FCODE is the BUILT_IN_*
   code of the function to be simplified.  */

static bool
gimple_fold_builtin_printf (gimple_stmt_iterator *gsi, tree fmt,
			    tree arg, enum built_in_function fcode)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree fn_putchar, fn_puts, newarg;
  const char *fmt_str = NULL;

  /* If the return value is used, don't do the transformation.  */
  if (gimple_call_lhs (stmt) != NULL_TREE)
    return false;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return false;

  if (fcode == BUILT_IN_PRINTF_UNLOCKED)
    {
      /* If we're using an unlocked function, assume the other
	 unlocked functions exist explicitly.  */
      fn_putchar = builtin_decl_explicit (BUILT_IN_PUTCHAR_UNLOCKED);
      fn_puts = builtin_decl_explicit (BUILT_IN_PUTS_UNLOCKED);
    }
  else
    {
      fn_putchar = builtin_decl_implicit (BUILT_IN_PUTCHAR);
      fn_puts = builtin_decl_implicit (BUILT_IN_PUTS);
    }

  if (!init_target_chars ())
    return false;

  if (strcmp (fmt_str, target_percent_s) == 0
      || strchr (fmt_str, target_percent) == NULL)
    {
      const char *str;

      if (strcmp (fmt_str, target_percent_s) == 0)
	{
	  if (fcode == BUILT_IN_VPRINTF || fcode == BUILT_IN_VPRINTF_CHK)
	    return false;

	  if (!arg || ! POINTER_TYPE_P (TREE_TYPE (arg)))
	    return false;

	  str = c_getstr (arg);
	  if (str == NULL)
	    return false;
	}
      else
	{
	  /* The format specifier doesn't contain any '%' characters.  */
	  if (fcode != BUILT_IN_VPRINTF && fcode != BUILT_IN_VPRINTF_CHK
	      && arg)
	    return false;
	  str = fmt_str;
	}

      /* If the string was "", printf does nothing.  */
      if (str[0] == '\0')
	{
	  replace_call_with_value (gsi, NULL_TREE);
	  return true;
	}

      /* If the string has length of 1, call putchar.  */
      if (str[1] == '\0')
	{
	  /* Given printf("c"), (where c is any one character,)
	     convert "c"[0] to an int and pass that to the replacement
	     function.  */
	  newarg = build_int_cst (integer_type_node, str[0]);
	  if (fn_putchar)
	    {
	      gcall *repl = gimple_build_call (fn_putchar, 1, newarg);
	      replace_call_with_call_and_fold (gsi, repl);
	      return true;
	    }
	}
      else
	{
	  /* If the string was "string\n", call puts("string").  */
	  size_t len = strlen (str);
	  if ((unsigned char)str[len - 1] == target_newline
	      && (size_t) (int) len == len
	      && (int) len > 0)
	    {
	      char *newstr;

	      /* Create a NUL-terminated string that's one char shorter
		 than the original, stripping off the trailing '\n'.  */
	      newstr = xstrdup (str);
	      newstr[len - 1] = '\0';
	      newarg = build_string_literal (len, newstr);
	      free (newstr);
	      if (fn_puts)
		{
		  gcall *repl = gimple_build_call (fn_puts, 1, newarg);
		  replace_call_with_call_and_fold (gsi, repl);
		  return true;
		}
	    }
	  else
	    /* We'd like to arrange to call fputs(string,stdout) here,
	       but we need stdout and don't have a way to get it yet.  */
	    return false;
	}
    }

  /* The other optimizations can be done only on the non-va_list variants.  */
  else if (fcode == BUILT_IN_VPRINTF || fcode == BUILT_IN_VPRINTF_CHK)
    return false;

  /* If the format specifier was "%s\n", call __builtin_puts(arg).  */
  else if (strcmp (fmt_str, target_percent_s_newline) == 0)
    {
      if (!arg || ! POINTER_TYPE_P (TREE_TYPE (arg)))
	return false;
      if (fn_puts)
	{
	  gcall *repl = gimple_build_call (fn_puts, 1, arg);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  /* If the format specifier was "%c", call __builtin_putchar(arg).  */
  else if (strcmp (fmt_str, target_percent_c) == 0)
    {
      if (!arg || ! useless_type_conversion_p (integer_type_node,
					       TREE_TYPE (arg)))
	return false;
      if (fn_putchar)
	{
	  gcall *repl = gimple_build_call (fn_putchar, 1, arg);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }

  return false;
}



/* Fold a call to __builtin_strlen with known length LEN.  */

static bool
gimple_fold_builtin_strlen (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree arg = gimple_call_arg (stmt, 0);

  wide_int minlen;
  wide_int maxlen;

  c_strlen_data lendata = { };
  if (get_range_strlen (arg, &lendata, /* eltsize = */ 1)
      && !lendata.decl
      && lendata.minlen && TREE_CODE (lendata.minlen) == INTEGER_CST
      && lendata.maxlen && TREE_CODE (lendata.maxlen) == INTEGER_CST)
    {
      /* The range of lengths refers to either a single constant
	 string or to the longest and shortest constant string
	 referenced by the argument of the strlen() call, or to
	 the strings that can possibly be stored in the arrays
	 the argument refers to.  */
      minlen = wi::to_wide (lendata.minlen);
      maxlen = wi::to_wide (lendata.maxlen);
    }
  else
    {
      unsigned prec = TYPE_PRECISION (sizetype);

      minlen = wi::shwi (0, prec);
      maxlen = wi::to_wide (max_object_size (), prec) - 2;
    }

  if (minlen == maxlen)
    {
      /* Fold the strlen call to a constant.  */
      tree type = TREE_TYPE (lendata.minlen);
      tree len = force_gimple_operand_gsi (gsi,
					   wide_int_to_tree (type, minlen),
					   true, NULL, true, GSI_SAME_STMT);
      replace_call_with_value (gsi, len);
      return true;
    }

  /* Set the strlen() range to [0, MAXLEN].  */
  if (tree lhs = gimple_call_lhs (stmt))
    set_strlen_range (lhs, minlen, maxlen);

  return false;
}

/* Fold a call to __builtin_acc_on_device.  */

static bool
gimple_fold_builtin_acc_on_device (gimple_stmt_iterator *gsi, tree arg0)
{
  /* Defer folding until we know which compiler we're in.  */
  if (symtab->state != EXPANSION)
    return false;

  unsigned val_host = GOMP_DEVICE_HOST;
  unsigned val_dev = GOMP_DEVICE_NONE;

#ifdef ACCEL_COMPILER
  val_host = GOMP_DEVICE_NOT_HOST;
  val_dev = ACCEL_COMPILER_acc_device;
#endif

  location_t loc = gimple_location (gsi_stmt (*gsi));
  
  tree host_eq = make_ssa_name (boolean_type_node);
  gimple *host_ass = gimple_build_assign
    (host_eq, EQ_EXPR, arg0, build_int_cst (TREE_TYPE (arg0), val_host));
  gimple_set_location (host_ass, loc);
  gsi_insert_before (gsi, host_ass, GSI_SAME_STMT);

  tree dev_eq = make_ssa_name (boolean_type_node);
  gimple *dev_ass = gimple_build_assign
    (dev_eq, EQ_EXPR, arg0, build_int_cst (TREE_TYPE (arg0), val_dev));
  gimple_set_location (dev_ass, loc);
  gsi_insert_before (gsi, dev_ass, GSI_SAME_STMT);

  tree result = make_ssa_name (boolean_type_node);
  gimple *result_ass = gimple_build_assign
    (result, BIT_IOR_EXPR, host_eq, dev_eq);
  gimple_set_location (result_ass, loc);
  gsi_insert_before (gsi, result_ass, GSI_SAME_STMT);

  replace_call_with_value (gsi, result);

  return true;
}

/* Fold realloc (0, n) -> malloc (n).  */

static bool
gimple_fold_builtin_realloc (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree arg = gimple_call_arg (stmt, 0);
  tree size = gimple_call_arg (stmt, 1);

  if (operand_equal_p (arg, null_pointer_node, 0))
    {
      tree fn_malloc = builtin_decl_implicit (BUILT_IN_MALLOC);
      if (fn_malloc)
	{
	  gcall *repl = gimple_build_call (fn_malloc, 1, size);
	  replace_call_with_call_and_fold (gsi, repl);
	  return true;
	}
    }
  return false;
}

/* Number of bytes into which any type but aggregate or vector types
   should fit.  */
static constexpr size_t clear_padding_unit
  = MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT;
/* Buffer size on which __builtin_clear_padding folding code works.  */
static const size_t clear_padding_buf_size = 32 * clear_padding_unit;

/* Data passed through __builtin_clear_padding folding.  */
struct clear_padding_struct {
  location_t loc;
  /* 0 during __builtin_clear_padding folding, nonzero during
     clear_type_padding_in_mask.  In that case, instead of clearing the
     non-padding bits in union_ptr array clear the padding bits in there.  */
  bool clear_in_mask;
  tree base;
  tree alias_type;
  gimple_stmt_iterator *gsi;
  /* Alignment of buf->base + 0.  */
  unsigned align;
  /* Offset from buf->base.  Should be always a multiple of UNITS_PER_WORD.  */
  HOST_WIDE_INT off;
  /* Number of padding bytes before buf->off that don't have padding clear
     code emitted yet.  */
  HOST_WIDE_INT padding_bytes;
  /* The size of the whole object.  Never emit code to touch
     buf->base + buf->sz or following bytes.  */
  HOST_WIDE_INT sz;
  /* Number of bytes recorded in buf->buf.  */
  size_t size;
  /* When inside union, instead of emitting code we and bits inside of
     the union_ptr array.  */
  unsigned char *union_ptr;
  /* Set bits mean padding bits that need to be cleared by the builtin.  */
  unsigned char buf[clear_padding_buf_size + clear_padding_unit];
};

/* Emit code to clear padding requested in BUF->buf - set bits
   in there stand for padding that should be cleared.  FULL is true
   if everything from the buffer should be flushed, otherwise
   it can leave up to 2 * clear_padding_unit bytes for further
   processing.  */

static void
clear_padding_flush (clear_padding_struct *buf, bool full)
{
  gcc_assert ((clear_padding_unit % UNITS_PER_WORD) == 0);
  if (!full && buf->size < 2 * clear_padding_unit)
    return;
  gcc_assert ((buf->off % UNITS_PER_WORD) == 0);
  size_t end = buf->size;
  if (!full)
    end = ((end - clear_padding_unit - 1) / clear_padding_unit
	   * clear_padding_unit);
  size_t padding_bytes = buf->padding_bytes;
  if (buf->union_ptr)
    {
      if (buf->clear_in_mask)
	{
	  /* During clear_type_padding_in_mask, clear the padding
	     bits set in buf->buf in the buf->union_ptr mask.  */
	  for (size_t i = 0; i < end; i++)
	    {
	      if (buf->buf[i] == (unsigned char) ~0)
		padding_bytes++;
	      else
		{
		  memset (&buf->union_ptr[buf->off + i - padding_bytes],
			  0, padding_bytes);
		  padding_bytes = 0;
		  buf->union_ptr[buf->off + i] &= ~buf->buf[i];
		}
	    }
	  if (full)
	    {
	      memset (&buf->union_ptr[buf->off + end - padding_bytes],
		      0, padding_bytes);
	      buf->off = 0;
	      buf->size = 0;
	      buf->padding_bytes = 0;
	    }
	  else
	    {
	      memmove (buf->buf, buf->buf + end, buf->size - end);
	      buf->off += end;
	      buf->size -= end;
	      buf->padding_bytes = padding_bytes;
	    }
	  return;
	}
      /* Inside of a union, instead of emitting any code, instead
	 clear all bits in the union_ptr buffer that are clear
	 in buf.  Whole padding bytes don't clear anything.  */
      for (size_t i = 0; i < end; i++)
	{
	  if (buf->buf[i] == (unsigned char) ~0)
	    padding_bytes++;
	  else
	    {
	      padding_bytes = 0;
	      buf->union_ptr[buf->off + i] &= buf->buf[i];
	    }
	}
      if (full)
	{
	  buf->off = 0;
	  buf->size = 0;
	  buf->padding_bytes = 0;
	}
      else
	{
	  memmove (buf->buf, buf->buf + end, buf->size - end);
	  buf->off += end;
	  buf->size -= end;
	  buf->padding_bytes = padding_bytes;
	}
      return;
    }
  size_t wordsize = UNITS_PER_WORD;
  for (size_t i = 0; i < end; i += wordsize)
    {
      size_t nonzero_first = wordsize;
      size_t nonzero_last = 0;
      size_t zero_first = wordsize;
      size_t zero_last = 0;
      bool all_ones = true, bytes_only = true;
      if ((unsigned HOST_WIDE_INT) (buf->off + i + wordsize)
	  > (unsigned HOST_WIDE_INT) buf->sz)
	{
	  gcc_assert (wordsize > 1);
	  wordsize /= 2;
	  i -= wordsize;
	  continue;
	}
      for (size_t j = i; j < i + wordsize && j < end; j++)
	{
	  if (buf->buf[j])
	    {
	      if (nonzero_first == wordsize)
		{
		  nonzero_first = j - i;
		  nonzero_last = j - i;
		}
	      if (nonzero_last != j - i)
		all_ones = false;
	      nonzero_last = j + 1 - i;
	    }
	  else
	    {
	      if (zero_first == wordsize)
		zero_first = j - i;
	      zero_last = j + 1 - i;
	    }
	  if (buf->buf[j] != 0 && buf->buf[j] != (unsigned char) ~0)
	    {
	      all_ones = false;
	      bytes_only = false;
	    }
	}
      size_t padding_end = i;
      if (padding_bytes)
	{
	  if (nonzero_first == 0
	      && nonzero_last == wordsize
	      && all_ones)
	    {
	      /* All bits are padding and we had some padding
		 before too.  Just extend it.  */
	      padding_bytes += wordsize;
	      continue;
	    }
	  if (all_ones && nonzero_first == 0)
	    {
	      padding_bytes += nonzero_last;
	      padding_end += nonzero_last;
	      nonzero_first = wordsize;
	      nonzero_last = 0;
	    }
	  else if (bytes_only && nonzero_first == 0)
	    {
	      gcc_assert (zero_first && zero_first != wordsize);
	      padding_bytes += zero_first;
	      padding_end += zero_first;
	    }
	  tree atype, src;
	  if (padding_bytes == 1)
	    {
	      atype = char_type_node;
	      src = build_zero_cst (char_type_node);
	    }
	  else
	    {
	      atype = build_array_type_nelts (char_type_node, padding_bytes);
	      src = build_constructor (atype, NULL);
	    }
	  tree dst = build2_loc (buf->loc, MEM_REF, atype, buf->base,
				 build_int_cst (buf->alias_type,
						buf->off + padding_end
						- padding_bytes));
	  gimple *g = gimple_build_assign (dst, src);
	  gimple_set_location (g, buf->loc);
	  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
	  padding_bytes = 0;
	  buf->padding_bytes = 0;
	}
      if (nonzero_first == wordsize)
	/* All bits in a word are 0, there are no padding bits.  */
	continue;
      if (all_ones && nonzero_last == wordsize)
	{
	  /* All bits between nonzero_first and end of word are padding
	     bits, start counting padding_bytes.  */
	  padding_bytes = nonzero_last - nonzero_first;
	  continue;
	}
      if (bytes_only)
	{
	  /* If bitfields aren't involved in this word, prefer storing
	     individual bytes or groups of them over performing a RMW
	     operation on the whole word.  */
	  gcc_assert (i + zero_last <= end);
	  for (size_t j = padding_end; j < i + zero_last; j++)
	    {
	      if (buf->buf[j])
		{
		  size_t k;
		  for (k = j; k < i + zero_last; k++)
		    if (buf->buf[k] == 0)
		      break;
		  HOST_WIDE_INT off = buf->off + j;
		  tree atype, src;
		  if (k - j == 1)
		    {
		      atype = char_type_node;
		      src = build_zero_cst (char_type_node);
		    }
		  else
		    {
		      atype = build_array_type_nelts (char_type_node, k - j);
		      src = build_constructor (atype, NULL);
		    }
		  tree dst = build2_loc (buf->loc, MEM_REF, atype,
					 buf->base,
					 build_int_cst (buf->alias_type, off));
		  gimple *g = gimple_build_assign (dst, src);
		  gimple_set_location (g, buf->loc);
		  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
		  j = k;
		}
	    }
	  if (nonzero_last == wordsize)
	    padding_bytes = nonzero_last - zero_last;
	  continue;
	}
      for (size_t eltsz = 1; eltsz <= wordsize; eltsz <<= 1)
	{
	  if (nonzero_last - nonzero_first <= eltsz
	      && ((nonzero_first & ~(eltsz - 1))
		  == ((nonzero_last - 1) & ~(eltsz - 1))))
	    {
	      tree type;
	      if (eltsz == 1)
		type = char_type_node;
	      else
		type = lang_hooks.types.type_for_size (eltsz * BITS_PER_UNIT,
						       0);
	      size_t start = nonzero_first & ~(eltsz - 1);
	      HOST_WIDE_INT off = buf->off + i + start;
	      tree atype = type;
	      if (eltsz > 1 && buf->align < TYPE_ALIGN (type))
		atype = build_aligned_type (type, buf->align);
	      tree dst = build2_loc (buf->loc, MEM_REF, atype, buf->base,
				     build_int_cst (buf->alias_type, off));
	      tree src;
	      gimple *g;
	      if (all_ones
		  && nonzero_first == start
		  && nonzero_last == start + eltsz)
		src = build_zero_cst (type);
	      else
		{
		  src = make_ssa_name (type);
		  tree tmp_dst = unshare_expr (dst);
		  /* The folding introduces a read from the tmp_dst, we should
		     prevent uninitialized warning analysis from issuing warning
		     for such fake read.  In order to suppress warning only for
		     this expr, we should set the location of tmp_dst to
		     UNKNOWN_LOCATION first, then suppress_warning will call
		     set_no_warning_bit to set the no_warning flag only for
		     tmp_dst.  */
		  SET_EXPR_LOCATION (tmp_dst, UNKNOWN_LOCATION);
		  suppress_warning (tmp_dst, OPT_Wuninitialized);
		  g = gimple_build_assign (src, tmp_dst);
		  gimple_set_location (g, buf->loc);
		  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
		  tree mask = native_interpret_expr (type,
						     buf->buf + i + start,
						     eltsz);
		  gcc_assert (mask && TREE_CODE (mask) == INTEGER_CST);
		  mask = fold_build1 (BIT_NOT_EXPR, type, mask);
		  tree src_masked = make_ssa_name (type);
		  g = gimple_build_assign (src_masked, BIT_AND_EXPR,
					   src, mask);
		  gimple_set_location (g, buf->loc);
		  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
		  src = src_masked;
		}
	      g = gimple_build_assign (dst, src);
	      gimple_set_location (g, buf->loc);
	      gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
	      break;
	    }
	}
    }
  if (full)
    {
      if (padding_bytes)
	{
	  tree atype, src;
	  if (padding_bytes == 1)
	    {
	      atype = char_type_node;
	      src = build_zero_cst (char_type_node);
	    }
	  else
	    {
	      atype = build_array_type_nelts (char_type_node, padding_bytes);
	      src = build_constructor (atype, NULL);
	    }
	  tree dst = build2_loc (buf->loc, MEM_REF, atype, buf->base,
				 build_int_cst (buf->alias_type,
						buf->off + end
						- padding_bytes));
	  gimple *g = gimple_build_assign (dst, src);
	  gimple_set_location (g, buf->loc);
	  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
	}
      size_t end_rem = end % UNITS_PER_WORD;
      buf->off += end - end_rem;
      buf->size = end_rem;
      memset (buf->buf, 0, buf->size);
      buf->padding_bytes = 0;
    }
  else
    {
      memmove (buf->buf, buf->buf + end, buf->size - end);
      buf->off += end;
      buf->size -= end;
      buf->padding_bytes = padding_bytes;
    }
}

/* Append PADDING_BYTES padding bytes.  */

static void
clear_padding_add_padding (clear_padding_struct *buf,
			   HOST_WIDE_INT padding_bytes)
{
  if (padding_bytes == 0)
    return;
  if ((unsigned HOST_WIDE_INT) padding_bytes + buf->size
      > (unsigned HOST_WIDE_INT) clear_padding_buf_size)
    clear_padding_flush (buf, false);
  if ((unsigned HOST_WIDE_INT) padding_bytes + buf->size
      > (unsigned HOST_WIDE_INT) clear_padding_buf_size)
    {
      memset (buf->buf + buf->size, ~0, clear_padding_buf_size - buf->size);
      padding_bytes -= clear_padding_buf_size - buf->size;
      buf->size = clear_padding_buf_size;
      clear_padding_flush (buf, false);
      gcc_assert (buf->padding_bytes);
      /* At this point buf->buf[0] through buf->buf[buf->size - 1]
	 is guaranteed to be all ones.  */
      padding_bytes += buf->size;
      buf->size = padding_bytes % UNITS_PER_WORD;
      memset (buf->buf, ~0, buf->size);
      buf->off += padding_bytes - buf->size;
      buf->padding_bytes += padding_bytes - buf->size;
    }
  else
    {
      memset (buf->buf + buf->size, ~0, padding_bytes);
      buf->size += padding_bytes;
    }
}

static void clear_padding_type (clear_padding_struct *, tree,
				HOST_WIDE_INT, bool);

/* Clear padding bits of union type TYPE.  */

static void
clear_padding_union (clear_padding_struct *buf, tree type,
		     HOST_WIDE_INT sz, bool for_auto_init)
{
  clear_padding_struct *union_buf;
  HOST_WIDE_INT start_off = 0, next_off = 0;
  size_t start_size = 0;
  if (buf->union_ptr)
    {
      start_off = buf->off + buf->size;
      next_off = start_off + sz;
      start_size = start_off % UNITS_PER_WORD;
      start_off -= start_size;
      clear_padding_flush (buf, true);
      union_buf = buf;
    }
  else
    {
      if (sz + buf->size > clear_padding_buf_size)
	clear_padding_flush (buf, false);
      union_buf = XALLOCA (clear_padding_struct);
      union_buf->loc = buf->loc;
      union_buf->clear_in_mask = buf->clear_in_mask;
      union_buf->base = NULL_TREE;
      union_buf->alias_type = NULL_TREE;
      union_buf->gsi = NULL;
      union_buf->align = 0;
      union_buf->off = 0;
      union_buf->padding_bytes = 0;
      union_buf->sz = sz;
      union_buf->size = 0;
      if (sz + buf->size <= clear_padding_buf_size)
	union_buf->union_ptr = buf->buf + buf->size;
      else
	union_buf->union_ptr = XNEWVEC (unsigned char, sz);
      memset (union_buf->union_ptr, ~0, sz);
    }

  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL && !DECL_PADDING_P (field))
      {
	if (DECL_SIZE_UNIT (field) == NULL_TREE)
	  {
	    if (TREE_TYPE (field) == error_mark_node)
	      continue;
	    gcc_assert (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
			&& !COMPLETE_TYPE_P (TREE_TYPE (field)));
	    if (!buf->clear_in_mask && !for_auto_init)
	      error_at (buf->loc, "flexible array member %qD does not have "
				  "well defined padding bits for %qs",
			field, "__builtin_clear_padding");
	    continue;
	  }
	HOST_WIDE_INT fldsz = tree_to_shwi (DECL_SIZE_UNIT (field));
	gcc_assert (union_buf->size == 0);
	union_buf->off = start_off;
	union_buf->size = start_size;
	memset (union_buf->buf, ~0, start_size);
	clear_padding_type (union_buf, TREE_TYPE (field), fldsz, for_auto_init);
	clear_padding_add_padding (union_buf, sz - fldsz);
	clear_padding_flush (union_buf, true);
      }

  if (buf == union_buf)
    {
      buf->off = next_off;
      buf->size = next_off % UNITS_PER_WORD;
      buf->off -= buf->size;
      memset (buf->buf, ~0, buf->size);
    }
  else if (sz + buf->size <= clear_padding_buf_size)
    buf->size += sz;
  else
    {
      unsigned char *union_ptr = union_buf->union_ptr;
      while (sz)
	{
	  clear_padding_flush (buf, false);
	  HOST_WIDE_INT this_sz
	    = MIN ((unsigned HOST_WIDE_INT) sz,
		   clear_padding_buf_size - buf->size);
	  memcpy (buf->buf + buf->size, union_ptr, this_sz);
	  buf->size += this_sz;
	  union_ptr += this_sz;
	  sz -= this_sz;
	}
      XDELETE (union_buf->union_ptr);
    }
}

/* The only known floating point formats with padding bits are the
   IEEE extended ones.  */

static bool
clear_padding_real_needs_padding_p (tree type)
{
  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
  return (fmt->b == 2
	  && fmt->signbit_ro == fmt->signbit_rw
	  && (fmt->signbit_ro == 79 || fmt->signbit_ro == 95));
}

/* Return true if TYPE might contain any padding bits.  */

bool
clear_padding_type_may_have_padding_p (tree type)
{
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
      return true;
    case ARRAY_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
      return clear_padding_type_may_have_padding_p (TREE_TYPE (type));
    case REAL_TYPE:
      return clear_padding_real_needs_padding_p (type);
    default:
      return false;
    }
}

/* Emit a runtime loop:
   for (; buf.base != end; buf.base += sz)
     __builtin_clear_padding (buf.base);  */

static void
clear_padding_emit_loop (clear_padding_struct *buf, tree type,
			 tree end, bool for_auto_init)
{
  tree l1 = create_artificial_label (buf->loc);
  tree l2 = create_artificial_label (buf->loc);
  tree l3 = create_artificial_label (buf->loc);
  gimple *g = gimple_build_goto (l2);
  gimple_set_location (g, buf->loc);
  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
  g = gimple_build_label (l1);
  gimple_set_location (g, buf->loc);
  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
  clear_padding_type (buf, type, buf->sz, for_auto_init);
  clear_padding_flush (buf, true);
  g = gimple_build_assign (buf->base, POINTER_PLUS_EXPR, buf->base,
			   size_int (buf->sz));
  gimple_set_location (g, buf->loc);
  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
  g = gimple_build_label (l2);
  gimple_set_location (g, buf->loc);
  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
  g = gimple_build_cond (NE_EXPR, buf->base, end, l1, l3);
  gimple_set_location (g, buf->loc);
  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
  g = gimple_build_label (l3);
  gimple_set_location (g, buf->loc);
  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
}

/* Clear padding bits for TYPE.  Called recursively from
   gimple_fold_builtin_clear_padding.  If FOR_AUTO_INIT is true,
   the __builtin_clear_padding is not called by the end user,
   instead, it's inserted by the compiler to initialize the
   paddings of automatic variable.  Therefore, we should not
   emit the error messages for flexible array members to confuse
   the end user.  */

static void
clear_padding_type (clear_padding_struct *buf, tree type,
		    HOST_WIDE_INT sz, bool for_auto_init)
{
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      HOST_WIDE_INT cur_pos;
      cur_pos = 0;
      for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL && !DECL_PADDING_P (field))
	  {
	    tree ftype = TREE_TYPE (field);
	    if (DECL_BIT_FIELD (field))
	      {
		HOST_WIDE_INT fldsz = TYPE_PRECISION (ftype);
		if (fldsz == 0)
		  continue;
		HOST_WIDE_INT pos = int_byte_position (field);
		if (pos >= sz)
		  continue;
		HOST_WIDE_INT bpos
		  = tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field));
		bpos %= BITS_PER_UNIT;
		HOST_WIDE_INT end
		  = ROUND_UP (bpos + fldsz, BITS_PER_UNIT) / BITS_PER_UNIT;
		if (pos + end > cur_pos)
		  {
		    clear_padding_add_padding (buf, pos + end - cur_pos);
		    cur_pos = pos + end;
		  }
		gcc_assert (cur_pos > pos
			    && ((unsigned HOST_WIDE_INT) buf->size
				>= (unsigned HOST_WIDE_INT) cur_pos - pos));
		unsigned char *p = buf->buf + buf->size - (cur_pos - pos);
		if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
		  sorry_at (buf->loc, "PDP11 bit-field handling unsupported"
				      " in %qs", "__builtin_clear_padding");
		else if (BYTES_BIG_ENDIAN)
		  {
		    /* Big endian.  */
		    if (bpos + fldsz <= BITS_PER_UNIT)
		      *p &= ~(((1 << fldsz) - 1)
			      << (BITS_PER_UNIT - bpos - fldsz));
		    else
		      {
			if (bpos)
			  {
			    *p &= ~(((1U << BITS_PER_UNIT) - 1) >> bpos);
			    p++;
			    fldsz -= BITS_PER_UNIT - bpos;
			  }
			memset (p, 0, fldsz / BITS_PER_UNIT);
			p += fldsz / BITS_PER_UNIT;
			fldsz %= BITS_PER_UNIT;
			if (fldsz)
			  *p &= ((1U << BITS_PER_UNIT) - 1) >> fldsz;
		      }
		  }
		else
		  {
		    /* Little endian.  */
		    if (bpos + fldsz <= BITS_PER_UNIT)
		      *p &= ~(((1 << fldsz) - 1) << bpos);
		    else
		      {
			if (bpos)
			  {
			    *p &= ~(((1 << BITS_PER_UNIT) - 1) << bpos);
			    p++;
			    fldsz -= BITS_PER_UNIT - bpos;
			  }
			memset (p, 0, fldsz / BITS_PER_UNIT);
			p += fldsz / BITS_PER_UNIT;
			fldsz %= BITS_PER_UNIT;
			if (fldsz)
			  *p &= ~((1 << fldsz) - 1);
		      }
		  }
	      }
	    else if (DECL_SIZE_UNIT (field) == NULL_TREE)
	      {
		if (ftype == error_mark_node)
		  continue;
		gcc_assert (TREE_CODE (ftype) == ARRAY_TYPE
			    && !COMPLETE_TYPE_P (ftype));
		if (!buf->clear_in_mask && !for_auto_init)
		  error_at (buf->loc, "flexible array member %qD does not "
				      "have well defined padding bits for %qs",
			    field, "__builtin_clear_padding");
	      }
	    else if (is_empty_type (ftype))
	      continue;
	    else
	      {
		HOST_WIDE_INT pos = int_byte_position (field);
		if (pos >= sz)
		  continue;
		HOST_WIDE_INT fldsz = tree_to_shwi (DECL_SIZE_UNIT (field));
		gcc_assert (pos >= 0 && fldsz >= 0 && pos >= cur_pos);
		clear_padding_add_padding (buf, pos - cur_pos);
		cur_pos = pos;
		if (tree asbase = lang_hooks.types.classtype_as_base (field))
		  ftype = asbase;
		clear_padding_type (buf, ftype, fldsz, for_auto_init);
		cur_pos += fldsz;
	      }
	  }
      gcc_assert (sz >= cur_pos);
      clear_padding_add_padding (buf, sz - cur_pos);
      break;
    case ARRAY_TYPE:
      HOST_WIDE_INT nelts, fldsz;
      fldsz = int_size_in_bytes (TREE_TYPE (type));
      if (fldsz == 0)
	break;
      nelts = sz / fldsz;
      if (nelts > 1
	  && sz > 8 * UNITS_PER_WORD
	  && buf->union_ptr == NULL
	  && clear_padding_type_may_have_padding_p (TREE_TYPE (type)))
	{
	  /* For sufficiently large array of more than one elements,
	     emit a runtime loop to keep code size manageable.  */
	  tree base = buf->base;
	  unsigned int prev_align = buf->align;
	  HOST_WIDE_INT off = buf->off + buf->size;
	  HOST_WIDE_INT prev_sz = buf->sz;
	  clear_padding_flush (buf, true);
	  tree elttype = TREE_TYPE (type);
	  buf->base = create_tmp_var (build_pointer_type (elttype));
	  tree end = make_ssa_name (TREE_TYPE (buf->base));
	  gimple *g = gimple_build_assign (buf->base, POINTER_PLUS_EXPR,
					   base, size_int (off));
	  gimple_set_location (g, buf->loc);
	  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
	  g = gimple_build_assign (end, POINTER_PLUS_EXPR, buf->base,
				   size_int (sz));
	  gimple_set_location (g, buf->loc);
	  gsi_insert_before (buf->gsi, g, GSI_SAME_STMT);
	  buf->sz = fldsz;
	  buf->align = TYPE_ALIGN (elttype);
	  buf->off = 0;
	  buf->size = 0;
	  clear_padding_emit_loop (buf, elttype, end, for_auto_init);
	  buf->base = base;
	  buf->sz = prev_sz;
	  buf->align = prev_align;
	  buf->size = off % UNITS_PER_WORD;
	  buf->off = off - buf->size;
	  memset (buf->buf, 0, buf->size);
	  break;
	}
      for (HOST_WIDE_INT i = 0; i < nelts; i++)
	clear_padding_type (buf, TREE_TYPE (type), fldsz, for_auto_init);
      break;
    case UNION_TYPE:
      clear_padding_union (buf, type, sz, for_auto_init);
      break;
    case REAL_TYPE:
      gcc_assert ((size_t) sz <= clear_padding_unit);
      if ((unsigned HOST_WIDE_INT) sz + buf->size > clear_padding_buf_size)
	clear_padding_flush (buf, false);
      if (clear_padding_real_needs_padding_p (type))
	{
	  /* Use native_interpret_real + native_encode_expr to figure out
	     which bits are padding.  */
	  memset (buf->buf + buf->size, ~0, sz);
	  tree cst = native_interpret_real (type, buf->buf + buf->size, sz);
	  gcc_assert (cst && TREE_CODE (cst) == REAL_CST);
	  int len = native_encode_expr (cst, buf->buf + buf->size, sz);
	  gcc_assert (len > 0 && (size_t) len == (size_t) sz);
	  for (size_t i = 0; i < (size_t) sz; i++)
	    buf->buf[buf->size + i] ^= ~0;
	}
      else
	memset (buf->buf + buf->size, 0, sz);
      buf->size += sz;
      break;
    case COMPLEX_TYPE:
      fldsz = int_size_in_bytes (TREE_TYPE (type));
      clear_padding_type (buf, TREE_TYPE (type), fldsz, for_auto_init);
      clear_padding_type (buf, TREE_TYPE (type), fldsz, for_auto_init);
      break;
    case VECTOR_TYPE:
      nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
      fldsz = int_size_in_bytes (TREE_TYPE (type));
      for (HOST_WIDE_INT i = 0; i < nelts; i++)
	clear_padding_type (buf, TREE_TYPE (type), fldsz, for_auto_init);
      break;
    case NULLPTR_TYPE:
      gcc_assert ((size_t) sz <= clear_padding_unit);
      if ((unsigned HOST_WIDE_INT) sz + buf->size > clear_padding_buf_size)
	clear_padding_flush (buf, false);
      memset (buf->buf + buf->size, ~0, sz);
      buf->size += sz;
      break;
    default:
      gcc_assert ((size_t) sz <= clear_padding_unit);
      if ((unsigned HOST_WIDE_INT) sz + buf->size > clear_padding_buf_size)
	clear_padding_flush (buf, false);
      memset (buf->buf + buf->size, 0, sz);
      buf->size += sz;
      break;
    }
}

/* Clear padding bits of TYPE in MASK.  */

void
clear_type_padding_in_mask (tree type, unsigned char *mask)
{
  clear_padding_struct buf;
  buf.loc = UNKNOWN_LOCATION;
  buf.clear_in_mask = true;
  buf.base = NULL_TREE;
  buf.alias_type = NULL_TREE;
  buf.gsi = NULL;
  buf.align = 0;
  buf.off = 0;
  buf.padding_bytes = 0;
  buf.sz = int_size_in_bytes (type);
  buf.size = 0;
  buf.union_ptr = mask;
  clear_padding_type (&buf, type, buf.sz, false);
  clear_padding_flush (&buf, true);
}

/* Fold __builtin_clear_padding builtin.  */

static bool
gimple_fold_builtin_clear_padding (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  gcc_assert (gimple_call_num_args (stmt) == 2);
  tree ptr = gimple_call_arg (stmt, 0);
  tree typearg = gimple_call_arg (stmt, 1);
  /* The 2nd argument of __builtin_clear_padding's value is used to
     distinguish whether this call is made by the user or by the compiler
     for automatic variable initialization.  */
  bool for_auto_init = (bool) TREE_INT_CST_LOW (typearg);
  tree type = TREE_TYPE (TREE_TYPE (typearg));
  location_t loc = gimple_location (stmt);
  clear_padding_struct buf;
  gimple_stmt_iterator gsiprev = *gsi;
  /* This should be folded during the lower pass.  */
  gcc_assert (!gimple_in_ssa_p (cfun) && cfun->cfg == NULL);
  gcc_assert (COMPLETE_TYPE_P (type));
  gsi_prev (&gsiprev);

  buf.loc = loc;
  buf.clear_in_mask = false;
  buf.base = ptr;
  buf.alias_type = NULL_TREE;
  buf.gsi = gsi;
  buf.align = get_pointer_alignment (ptr);
  unsigned int talign = min_align_of_type (type) * BITS_PER_UNIT;
  buf.align = MAX (buf.align, talign);
  buf.off = 0;
  buf.padding_bytes = 0;
  buf.size = 0;
  buf.sz = int_size_in_bytes (type);
  buf.union_ptr = NULL;
  if (buf.sz < 0 && int_size_in_bytes (strip_array_types (type)) < 0)
    sorry_at (loc, "%s not supported for variable length aggregates",
	      "__builtin_clear_padding");
  /* The implementation currently assumes 8-bit host and target
     chars which is the case for all currently supported targets
     and hosts and is required e.g. for native_{encode,interpret}* APIs.  */
  else if (CHAR_BIT != 8 || BITS_PER_UNIT != 8)
    sorry_at (loc, "%s not supported on this target",
	      "__builtin_clear_padding");
  else if (!clear_padding_type_may_have_padding_p (type))
    ;
  else if (TREE_CODE (type) == ARRAY_TYPE && buf.sz < 0)
    {
      tree sz = TYPE_SIZE_UNIT (type);
      tree elttype = type;
      /* Only supports C/C++ VLAs and flattens all the VLA levels.  */
      while (TREE_CODE (elttype) == ARRAY_TYPE
	     && int_size_in_bytes (elttype) < 0)
	elttype = TREE_TYPE (elttype);
      HOST_WIDE_INT eltsz = int_size_in_bytes (elttype);
      gcc_assert (eltsz >= 0);
      if (eltsz)
	{
	  buf.base = create_tmp_var (build_pointer_type (elttype));
	  tree end = make_ssa_name (TREE_TYPE (buf.base));
	  gimple *g = gimple_build_assign (buf.base, ptr);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  g = gimple_build_assign (end, POINTER_PLUS_EXPR, buf.base, sz);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  buf.sz = eltsz;
	  buf.align = TYPE_ALIGN (elttype);
	  buf.alias_type = build_pointer_type (elttype);
	  clear_padding_emit_loop (&buf, elttype, end, for_auto_init);
	}
    }
  else
    {
      if (!is_gimple_mem_ref_addr (buf.base))
	{
	  buf.base = make_ssa_name (TREE_TYPE (ptr));
	  gimple *g = gimple_build_assign (buf.base, ptr);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	}
      buf.alias_type = build_pointer_type (type);
      clear_padding_type (&buf, type, buf.sz, for_auto_init);
      clear_padding_flush (&buf, true);
    }

  gimple_stmt_iterator gsiprev2 = *gsi;
  gsi_prev (&gsiprev2);
  if (gsi_stmt (gsiprev) == gsi_stmt (gsiprev2))
    gsi_replace (gsi, gimple_build_nop (), true);
  else
    {
      gsi_remove (gsi, true);
      *gsi = gsiprev2;
    }
  return true;
}

/* Fold the non-target builtin at *GSI and return whether any simplification
   was made.  */

static bool
gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  gcall *stmt = as_a <gcall *>(gsi_stmt (*gsi));
  tree callee = gimple_call_fndecl (stmt);

  /* Give up for always_inline inline builtins until they are
     inlined.  */
  if (avoid_folding_inline_builtin (callee))
    return false;

  unsigned n = gimple_call_num_args (stmt);
  enum built_in_function fcode = DECL_FUNCTION_CODE (callee);
  switch (fcode)
    {
    case BUILT_IN_BCMP:
      return gimple_fold_builtin_bcmp (gsi);
    case BUILT_IN_BCOPY:
      return gimple_fold_builtin_bcopy (gsi);
    case BUILT_IN_BZERO:
      return gimple_fold_builtin_bzero (gsi);

    case BUILT_IN_MEMSET:
      return gimple_fold_builtin_memset (gsi,
					 gimple_call_arg (stmt, 1),
					 gimple_call_arg (stmt, 2));
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMMOVE:
      return gimple_fold_builtin_memory_op (gsi, gimple_call_arg (stmt, 0),
					    gimple_call_arg (stmt, 1), fcode);
    case BUILT_IN_SPRINTF_CHK:
    case BUILT_IN_VSPRINTF_CHK:
      return gimple_fold_builtin_sprintf_chk (gsi, fcode);
    case BUILT_IN_STRCAT_CHK:
      return gimple_fold_builtin_strcat_chk (gsi);
    case BUILT_IN_STRNCAT_CHK:
      return gimple_fold_builtin_strncat_chk (gsi);
    case BUILT_IN_STRLEN:
      return gimple_fold_builtin_strlen (gsi);
    case BUILT_IN_STRCPY:
      return gimple_fold_builtin_strcpy (gsi,
					 gimple_call_arg (stmt, 0),
					 gimple_call_arg (stmt, 1));
    case BUILT_IN_STRNCPY:
      return gimple_fold_builtin_strncpy (gsi,
					  gimple_call_arg (stmt, 0),
					  gimple_call_arg (stmt, 1),
					  gimple_call_arg (stmt, 2));
    case BUILT_IN_STRCAT:
      return gimple_fold_builtin_strcat (gsi, gimple_call_arg (stmt, 0),
					 gimple_call_arg (stmt, 1));
    case BUILT_IN_STRNCAT:
      return gimple_fold_builtin_strncat (gsi);
    case BUILT_IN_INDEX:
    case BUILT_IN_STRCHR:
      return gimple_fold_builtin_strchr (gsi, false);
    case BUILT_IN_RINDEX:
    case BUILT_IN_STRRCHR:
      return gimple_fold_builtin_strchr (gsi, true);
    case BUILT_IN_STRSTR:
      return gimple_fold_builtin_strstr (gsi);
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRCMP_EQ:
    case BUILT_IN_STRCASECMP:
    case BUILT_IN_STRNCMP:
    case BUILT_IN_STRNCMP_EQ:
    case BUILT_IN_STRNCASECMP:
      return gimple_fold_builtin_string_compare (gsi);
    case BUILT_IN_MEMCHR:
      return gimple_fold_builtin_memchr (gsi);
    case BUILT_IN_FPUTS:
      return gimple_fold_builtin_fputs (gsi, gimple_call_arg (stmt, 0),
					gimple_call_arg (stmt, 1), false);
    case BUILT_IN_FPUTS_UNLOCKED:
      return gimple_fold_builtin_fputs (gsi, gimple_call_arg (stmt, 0),
					gimple_call_arg (stmt, 1), true);
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
      return gimple_fold_builtin_memory_chk (gsi,
					     gimple_call_arg (stmt, 0),
					     gimple_call_arg (stmt, 1),
					     gimple_call_arg (stmt, 2),
					     gimple_call_arg (stmt, 3),
					     fcode);
    case BUILT_IN_STPCPY:
      return gimple_fold_builtin_stpcpy (gsi);
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
      return gimple_fold_builtin_stxcpy_chk (gsi,
					     gimple_call_arg (stmt, 0),
					     gimple_call_arg (stmt, 1),
					     gimple_call_arg (stmt, 2),
					     fcode);
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STPNCPY_CHK:
      return gimple_fold_builtin_stxncpy_chk (gsi,
					      gimple_call_arg (stmt, 0),
					      gimple_call_arg (stmt, 1),
					      gimple_call_arg (stmt, 2),
					      gimple_call_arg (stmt, 3),
					      fcode);
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      return gimple_fold_builtin_snprintf_chk (gsi, fcode);

    case BUILT_IN_FPRINTF:
    case BUILT_IN_FPRINTF_UNLOCKED:
    case BUILT_IN_VFPRINTF:
      if (n == 2 || n == 3)
	return gimple_fold_builtin_fprintf (gsi,
					    gimple_call_arg (stmt, 0),
					    gimple_call_arg (stmt, 1),
					    n == 3
					    ? gimple_call_arg (stmt, 2)
					    : NULL_TREE,
					    fcode);
      break;
    case BUILT_IN_FPRINTF_CHK:
    case BUILT_IN_VFPRINTF_CHK:
      if (n == 3 || n == 4)
	return gimple_fold_builtin_fprintf (gsi,
					    gimple_call_arg (stmt, 0),
					    gimple_call_arg (stmt, 2),
					    n == 4
					    ? gimple_call_arg (stmt, 3)
					    : NULL_TREE,
					    fcode);
      break;
    case BUILT_IN_PRINTF:
    case BUILT_IN_PRINTF_UNLOCKED:
    case BUILT_IN_VPRINTF:
      if (n == 1 || n == 2)
	return gimple_fold_builtin_printf (gsi, gimple_call_arg (stmt, 0),
					   n == 2
					   ? gimple_call_arg (stmt, 1)
					   : NULL_TREE, fcode);
      break;
    case BUILT_IN_PRINTF_CHK:
    case BUILT_IN_VPRINTF_CHK:
      if (n == 2 || n == 3)
	return gimple_fold_builtin_printf (gsi, gimple_call_arg (stmt, 1),
					   n == 3
					   ? gimple_call_arg (stmt, 2)
					   : NULL_TREE, fcode);
      break;
    case BUILT_IN_ACC_ON_DEVICE:
      return gimple_fold_builtin_acc_on_device (gsi,
						gimple_call_arg (stmt, 0));
    case BUILT_IN_REALLOC:
      return gimple_fold_builtin_realloc (gsi);

    case BUILT_IN_CLEAR_PADDING:
      return gimple_fold_builtin_clear_padding (gsi);

    default:;
    }

  /* Try the generic builtin folder.  */
  bool ignore = (gimple_call_lhs (stmt) == NULL);
  tree result = fold_call_stmt (stmt, ignore);
  if (result)
    {
      if (ignore)
	STRIP_NOPS (result);
      else
	result = fold_convert (gimple_call_return_type (stmt), result);
      gimplify_and_update_call_from_tree (gsi, result);
      return true;
    }

  return false;
}

/* Transform IFN_GOACC_DIM_SIZE and IFN_GOACC_DIM_POS internal
   function calls to constants, where possible.  */

static tree
fold_internal_goacc_dim (const gimple *call)
{
  int axis = oacc_get_ifn_dim_arg (call);
  int size = oacc_get_fn_dim_size (current_function_decl, axis);
  tree result = NULL_TREE;
  tree type = TREE_TYPE (gimple_call_lhs (call));

  switch (gimple_call_internal_fn (call))
    {
    case IFN_GOACC_DIM_POS:
      /* If the size is 1, we know the answer.  */
      if (size == 1)
	result = build_int_cst (type, 0);
      break;
    case IFN_GOACC_DIM_SIZE:
      /* If the size is not dynamic, we know the answer.  */
      if (size)
	result = build_int_cst (type, size);
      break;
    default:
      break;
    }

  return result;
}

/* Return true if stmt is __atomic_compare_exchange_N call which is suitable
   for conversion into ATOMIC_COMPARE_EXCHANGE if the second argument is
   &var where var is only addressable because of such calls.  */

bool
optimize_atomic_compare_exchange_p (gimple *stmt)
{
  if (gimple_call_num_args (stmt) != 6
      || !flag_inline_atomics
      || !optimize
      || sanitize_flags_p (SANITIZE_THREAD | SANITIZE_ADDRESS)
      || !gimple_call_builtin_p (stmt, BUILT_IN_NORMAL)
      || !gimple_vdef (stmt)
      || !gimple_vuse (stmt))
    return false;

  tree fndecl = gimple_call_fndecl (stmt);
  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:
      break;
    default:
      return false;
    }

  tree expected = gimple_call_arg (stmt, 1);
  if (TREE_CODE (expected) != ADDR_EXPR
      || !SSA_VAR_P (TREE_OPERAND (expected, 0)))
    return false;

  tree etype = TREE_TYPE (TREE_OPERAND (expected, 0));
  if (!is_gimple_reg_type (etype)
      || !auto_var_in_fn_p (TREE_OPERAND (expected, 0), current_function_decl)
      || TREE_THIS_VOLATILE (etype)
      || VECTOR_TYPE_P (etype)
      || TREE_CODE (etype) == COMPLEX_TYPE
      /* Don't optimize floating point expected vars, VIEW_CONVERT_EXPRs
	 might not preserve all the bits.  See PR71716.  */
      || SCALAR_FLOAT_TYPE_P (etype)
      || maybe_ne (TYPE_PRECISION (etype),
		   GET_MODE_BITSIZE (TYPE_MODE (etype))))
    return false;

  tree weak = gimple_call_arg (stmt, 3);
  if (!integer_zerop (weak) && !integer_onep (weak))
    return false;

  tree parmt = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  tree itype = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (parmt)));
  machine_mode mode = TYPE_MODE (itype);

  if (direct_optab_handler (atomic_compare_and_swap_optab, mode)
      == CODE_FOR_nothing
      && optab_handler (sync_compare_and_swap_optab, mode) == CODE_FOR_nothing)
    return false;

  if (maybe_ne (int_size_in_bytes (etype), GET_MODE_SIZE (mode)))
    return false;

  return true;
}

/* Fold
     r = __atomic_compare_exchange_N (p, &e, d, w, s, f);
   into
     _Complex uintN_t t = ATOMIC_COMPARE_EXCHANGE (p, e, d, w * 256 + N, s, f);
     i = IMAGPART_EXPR <t>;
     r = (_Bool) i;
     e = REALPART_EXPR <t>;  */

void
fold_builtin_atomic_compare_exchange (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree fndecl = gimple_call_fndecl (stmt);
  tree parmt = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  tree itype = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (parmt)));
  tree ctype = build_complex_type (itype);
  tree expected = TREE_OPERAND (gimple_call_arg (stmt, 1), 0);
  bool throws = false;
  edge e = NULL;
  gimple *g = gimple_build_assign (make_ssa_name (TREE_TYPE (expected)),
				   expected);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  gimple_stmt_iterator gsiret = gsi_for_stmt (g);
  if (!useless_type_conversion_p (itype, TREE_TYPE (expected)))
    {
      g = gimple_build_assign (make_ssa_name (itype), VIEW_CONVERT_EXPR,
			       build1 (VIEW_CONVERT_EXPR, itype,
				       gimple_assign_lhs (g)));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }
  int flag = (integer_onep (gimple_call_arg (stmt, 3)) ? 256 : 0)
	     + int_size_in_bytes (itype);
  g = gimple_build_call_internal (IFN_ATOMIC_COMPARE_EXCHANGE, 6,
				  gimple_call_arg (stmt, 0),
				  gimple_assign_lhs (g),
				  gimple_call_arg (stmt, 2),
				  build_int_cst (integer_type_node, flag),
				  gimple_call_arg (stmt, 4),
				  gimple_call_arg (stmt, 5));
  tree lhs = make_ssa_name (ctype);
  gimple_call_set_lhs (g, lhs);
  gimple_move_vops (g, stmt);
  tree oldlhs = gimple_call_lhs (stmt);
  if (stmt_can_throw_internal (cfun, stmt))
    {
      throws = true;
      e = find_fallthru_edge (gsi_bb (*gsi)->succs);
    }
  gimple_call_set_nothrow (as_a <gcall *> (g),
			   gimple_call_nothrow_p (as_a <gcall *> (stmt)));
  gimple_call_set_lhs (stmt, NULL_TREE);
  gsi_replace (gsi, g, true);
  if (oldlhs)
    {
      g = gimple_build_assign (make_ssa_name (itype), IMAGPART_EXPR,
			       build1 (IMAGPART_EXPR, itype, lhs));
      if (throws)
	{
	  gsi_insert_on_edge_immediate (e, g);
	  *gsi = gsi_for_stmt (g);
	}
      else
	gsi_insert_after (gsi, g, GSI_NEW_STMT);
      g = gimple_build_assign (oldlhs, NOP_EXPR, gimple_assign_lhs (g));
      gsi_insert_after (gsi, g, GSI_NEW_STMT);
    }
  g = gimple_build_assign (make_ssa_name (itype), REALPART_EXPR,
			   build1 (REALPART_EXPR, itype, lhs));
  if (throws && oldlhs == NULL_TREE)
    {
      gsi_insert_on_edge_immediate (e, g);
      *gsi = gsi_for_stmt (g);
    }
  else
    gsi_insert_after (gsi, g, GSI_NEW_STMT);
  if (!useless_type_conversion_p (TREE_TYPE (expected), itype))
    {
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (expected)),
			       VIEW_CONVERT_EXPR,
			       build1 (VIEW_CONVERT_EXPR, TREE_TYPE (expected),
				       gimple_assign_lhs (g)));
      gsi_insert_after (gsi, g, GSI_NEW_STMT);
    }
  g = gimple_build_assign (expected, SSA_NAME, gimple_assign_lhs (g));
  gsi_insert_after (gsi, g, GSI_NEW_STMT);
  *gsi = gsiret;
}

/* Return true if ARG0 CODE ARG1 in infinite signed precision operation
   doesn't fit into TYPE.  The test for overflow should be regardless of
   -fwrapv, and even for unsigned types.  */

bool
arith_overflowed_p (enum tree_code code, const_tree type,
		    const_tree arg0, const_tree arg1)
{
  widest2_int warg0 = widest2_int_cst (arg0);
  widest2_int warg1 = widest2_int_cst (arg1);
  widest2_int wres;
  switch (code)
    {
    case PLUS_EXPR: wres = wi::add (warg0, warg1); break;
    case MINUS_EXPR: wres = wi::sub (warg0, warg1); break;
    case MULT_EXPR: wres = wi::mul (warg0, warg1); break;
    default: gcc_unreachable ();
    }
  signop sign = TYPE_SIGN (type);
  if (sign == UNSIGNED && wi::neg_p (wres))
    return true;
  return wi::min_precision (wres, sign) > TYPE_PRECISION (type);
}

/* If IFN_{MASK,LEN,MASK_LEN}_LOAD/STORE call CALL is unconditional,
   return a MEM_REF for the memory it references, otherwise return null.
   VECTYPE is the type of the memory vector.  MASK_P indicates it's for
   MASK if true, otherwise it's for LEN.  */

static tree
gimple_fold_partial_load_store_mem_ref (gcall *call, tree vectype, bool mask_p)
{
  tree ptr = gimple_call_arg (call, 0);
  tree alias_align = gimple_call_arg (call, 1);
  if (!tree_fits_uhwi_p (alias_align))
    return NULL_TREE;

  if (mask_p)
    {
      tree mask = gimple_call_arg (call, 2);
      if (!integer_all_onesp (mask))
	return NULL_TREE;
    }
  else
    {
      internal_fn ifn = gimple_call_internal_fn (call);
      int len_index = internal_fn_len_index (ifn);
      tree basic_len = gimple_call_arg (call, len_index);
      if (!poly_int_tree_p (basic_len))
	return NULL_TREE;
      tree bias = gimple_call_arg (call, len_index + 1);
      gcc_assert (TREE_CODE (bias) == INTEGER_CST);
      /* For LEN_LOAD/LEN_STORE/MASK_LEN_LOAD/MASK_LEN_STORE,
	 we don't fold when (bias + len) != VF.  */
      if (maybe_ne (wi::to_poly_widest (basic_len) + wi::to_widest (bias),
		    GET_MODE_NUNITS (TYPE_MODE (vectype))))
	return NULL_TREE;

      /* For MASK_LEN_{LOAD,STORE}, we should also check whether
	  the mask is all ones mask.  */
      if (ifn == IFN_MASK_LEN_LOAD || ifn == IFN_MASK_LEN_STORE)
	{
	  tree mask = gimple_call_arg (call, internal_fn_mask_index (ifn));
	  if (!integer_all_onesp (mask))
	    return NULL_TREE;
	}
    }

  unsigned HOST_WIDE_INT align = tree_to_uhwi (alias_align);
  if (TYPE_ALIGN (vectype) != align)
    vectype = build_aligned_type (vectype, align);
  tree offset = build_zero_cst (TREE_TYPE (alias_align));
  return fold_build2 (MEM_REF, vectype, ptr, offset);
}

/* Try to fold IFN_{MASK,LEN}_LOAD call CALL.  Return true on success.
   MASK_P indicates it's for MASK if true, otherwise it's for LEN.  */

static bool
gimple_fold_partial_load (gimple_stmt_iterator *gsi, gcall *call, bool mask_p)
{
  tree lhs = gimple_call_lhs (call);
  if (!lhs)
    return false;

  if (tree rhs
      = gimple_fold_partial_load_store_mem_ref (call, TREE_TYPE (lhs), mask_p))
    {
      gassign *new_stmt = gimple_build_assign (lhs, rhs);
      gimple_set_location (new_stmt, gimple_location (call));
      gimple_move_vops (new_stmt, call);
      gsi_replace (gsi, new_stmt, false);
      return true;
    }
  return false;
}

/* Try to fold IFN_{MASK,LEN}_STORE call CALL.  Return true on success.
   MASK_P indicates it's for MASK if true, otherwise it's for LEN.  */

static bool
gimple_fold_partial_store (gimple_stmt_iterator *gsi, gcall *call,
			   bool mask_p)
{
  internal_fn ifn = gimple_call_internal_fn (call);
  tree rhs = gimple_call_arg (call, internal_fn_stored_value_index (ifn));
  if (tree lhs
      = gimple_fold_partial_load_store_mem_ref (call, TREE_TYPE (rhs), mask_p))
    {
      gassign *new_stmt = gimple_build_assign (lhs, rhs);
      gimple_set_location (new_stmt, gimple_location (call));
      gimple_move_vops (new_stmt, call);
      gsi_replace (gsi, new_stmt, false);
      return true;
    }
  return false;
}

/* Attempt to fold a call statement referenced by the statement iterator GSI.
   The statement may be replaced by another statement, e.g., if the call
   simplifies to a constant value. Return true if any changes were made.
   It is assumed that the operands have been previously folded.  */

static bool
gimple_fold_call (gimple_stmt_iterator *gsi, bool inplace)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree callee;
  bool changed = false;

  /* Check for virtual calls that became direct calls.  */
  callee = gimple_call_fn (stmt);
  if (callee && TREE_CODE (callee) == OBJ_TYPE_REF)
    {
      if (gimple_call_addr_fndecl (OBJ_TYPE_REF_EXPR (callee)) != NULL_TREE)
	{
          if (dump_file && virtual_method_call_p (callee)
	      && !possible_polymorphic_call_target_p
		    (callee, stmt, cgraph_node::get (gimple_call_addr_fndecl
						     (OBJ_TYPE_REF_EXPR (callee)))))
	    {
	      fprintf (dump_file,
		       "Type inheritance inconsistent devirtualization of ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      fprintf (dump_file, " to ");
	      print_generic_expr (dump_file, callee, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }

	  gimple_call_set_fn (stmt, OBJ_TYPE_REF_EXPR (callee));
	  changed = true;
	}
      else if (flag_devirtualize && !inplace && virtual_method_call_p (callee))
	{
	  bool final;
	  vec <cgraph_node *>targets
	    = possible_polymorphic_call_targets (callee, stmt, &final);
	  if (final && targets.length () <= 1 && dbg_cnt (devirt))
	    {
	      tree lhs = gimple_call_lhs (stmt);
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
				   "folding virtual function call to %s\n",
		 		   targets.length () == 1
		  		   ? targets[0]->name ()
		  		   : "__builtin_unreachable");
		}
	      if (targets.length () == 1)
		{
		  tree fndecl = targets[0]->decl;
		  gimple_call_set_fndecl (stmt, fndecl);
		  changed = true;
		  /* If changing the call to __cxa_pure_virtual
		     or similar noreturn function, adjust gimple_call_fntype
		     too.  */
		  if (gimple_call_noreturn_p (stmt)
		      && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fndecl)))
		      && TYPE_ARG_TYPES (TREE_TYPE (fndecl))
		      && (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)))
			  == void_type_node))
		    gimple_call_set_fntype (stmt, TREE_TYPE (fndecl));
		  /* If the call becomes noreturn, remove the lhs.  */
		  if (lhs
		      && gimple_call_noreturn_p (stmt)
		      && (VOID_TYPE_P (TREE_TYPE (gimple_call_fntype (stmt)))
			  || should_remove_lhs_p (lhs)))
		    {
		      if (TREE_CODE (lhs) == SSA_NAME)
			{
			  tree var = create_tmp_var (TREE_TYPE (lhs));
			  tree def = get_or_create_ssa_default_def (cfun, var);
			  gimple *new_stmt = gimple_build_assign (lhs, def);
			  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
			}
		      gimple_call_set_lhs (stmt, NULL_TREE);
		    }
		  maybe_remove_unused_call_args (cfun, stmt);
		}
	      else
		{
		  location_t loc = gimple_location (stmt);
		  gimple *new_stmt = gimple_build_builtin_unreachable (loc);
		  gimple_call_set_ctrl_altering (new_stmt, false);
		  /* If the call had a SSA name as lhs morph that into
		     an uninitialized value.  */
		  if (lhs && TREE_CODE (lhs) == SSA_NAME)
		    {
		      tree var = create_tmp_var (TREE_TYPE (lhs));
		      SET_SSA_NAME_VAR_OR_IDENTIFIER (lhs, var);
		      SSA_NAME_DEF_STMT (lhs) = gimple_build_nop ();
		      set_ssa_default_def (cfun, var, lhs);
		    }
		  gimple_move_vops (new_stmt, stmt);
		  gsi_replace (gsi, new_stmt, false);
		  return true;
		}
	    }
	}
    }

  /* Check for indirect calls that became direct calls, and then
     no longer require a static chain.  */
  if (gimple_call_chain (stmt))
    {
      tree fn = gimple_call_fndecl (stmt);
      if (fn && !DECL_STATIC_CHAIN (fn))
	{
	  gimple_call_set_chain (stmt, NULL);
	  changed = true;
	}
    }

  if (inplace)
    return changed;

  /* Check for builtins that CCP can handle using information not
     available in the generic fold routines.  */
  if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      if (gimple_fold_builtin (gsi))
        changed = true;
    }
  else if (gimple_call_builtin_p (stmt, BUILT_IN_MD))
    {
	changed |= targetm.gimple_fold_builtin (gsi);
    }
  else if (gimple_call_internal_p (stmt))
    {
      enum tree_code subcode = ERROR_MARK;
      tree result = NULL_TREE;
      bool cplx_result = false;
      bool uaddc_usubc = false;
      tree overflow = NULL_TREE;
      switch (gimple_call_internal_fn (stmt))
	{
	case IFN_BUILTIN_EXPECT:
	  result = fold_builtin_expect (gimple_location (stmt),
					gimple_call_arg (stmt, 0),
					gimple_call_arg (stmt, 1),
					gimple_call_arg (stmt, 2),
					NULL_TREE);
	  break;
	case IFN_UBSAN_OBJECT_SIZE:
	  {
	    tree offset = gimple_call_arg (stmt, 1);
	    tree objsize = gimple_call_arg (stmt, 2);
	    if (integer_all_onesp (objsize)
		|| (TREE_CODE (offset) == INTEGER_CST
		    && TREE_CODE (objsize) == INTEGER_CST
		    && tree_int_cst_le (offset, objsize)))
	      {
		replace_call_with_value (gsi, NULL_TREE);
		return true;
	      }
	  }
	  break;
	case IFN_UBSAN_PTR:
	  if (integer_zerop (gimple_call_arg (stmt, 1)))
	    {
	      replace_call_with_value (gsi, NULL_TREE);
	      return true;
	    }
	  break;
	case IFN_UBSAN_BOUNDS:
	  {
	    tree index = gimple_call_arg (stmt, 1);
	    tree bound = gimple_call_arg (stmt, 2);
	    if (TREE_CODE (index) == INTEGER_CST
		&& TREE_CODE (bound) == INTEGER_CST)
	      {
		index = fold_convert (TREE_TYPE (bound), index);
		if (TREE_CODE (index) == INTEGER_CST
		    && tree_int_cst_lt (index, bound))
		  {
		    replace_call_with_value (gsi, NULL_TREE);
		    return true;
		  }
	      }
	  }
	  break;
	case IFN_GOACC_DIM_SIZE:
	case IFN_GOACC_DIM_POS:
	  result = fold_internal_goacc_dim (stmt);
	  break;
	case IFN_UBSAN_CHECK_ADD:
	  subcode = PLUS_EXPR;
	  break;
	case IFN_UBSAN_CHECK_SUB:
	  subcode = MINUS_EXPR;
	  break;
	case IFN_UBSAN_CHECK_MUL:
	  subcode = MULT_EXPR;
	  break;
	case IFN_ADD_OVERFLOW:
	  subcode = PLUS_EXPR;
	  cplx_result = true;
	  break;
	case IFN_SUB_OVERFLOW:
	  subcode = MINUS_EXPR;
	  cplx_result = true;
	  break;
	case IFN_MUL_OVERFLOW:
	  subcode = MULT_EXPR;
	  cplx_result = true;
	  break;
	case IFN_UADDC:
	  subcode = PLUS_EXPR;
	  cplx_result = true;
	  uaddc_usubc = true;
	  break;
	case IFN_USUBC:
	  subcode = MINUS_EXPR;
	  cplx_result = true;
	  uaddc_usubc = true;
	  break;
	case IFN_MASK_LOAD:
	  changed |= gimple_fold_partial_load (gsi, stmt, true);
	  break;
	case IFN_MASK_STORE:
	  changed |= gimple_fold_partial_store (gsi, stmt, true);
	  break;
	case IFN_LEN_LOAD:
	case IFN_MASK_LEN_LOAD:
	  changed |= gimple_fold_partial_load (gsi, stmt, false);
	  break;
	case IFN_LEN_STORE:
	case IFN_MASK_LEN_STORE:
	  changed |= gimple_fold_partial_store (gsi, stmt, false);
	  break;
	default:
	  break;
	}
      if (subcode != ERROR_MARK)
	{
	  tree arg0 = gimple_call_arg (stmt, 0);
	  tree arg1 = gimple_call_arg (stmt, 1);
	  tree arg2 = NULL_TREE;
	  tree type = TREE_TYPE (arg0);
	  if (cplx_result)
	    {
	      tree lhs = gimple_call_lhs (stmt);
	      if (lhs == NULL_TREE)
		type = NULL_TREE;
	      else
		type = TREE_TYPE (TREE_TYPE (lhs));
	      if (uaddc_usubc)
		arg2 = gimple_call_arg (stmt, 2);
	    }
	  if (type == NULL_TREE)
	    ;
	  else if (uaddc_usubc)
	    {
	      if (!integer_zerop (arg2))
		;
	      /* x = y + 0 + 0; x = y - 0 - 0; */
	      else if (integer_zerop (arg1))
		result = arg0;
	      /* x = 0 + y + 0; */
	      else if (subcode != MINUS_EXPR && integer_zerop (arg0))
		result = arg1;
	      /* x = y - y - 0; */
	      else if (subcode == MINUS_EXPR
		       && operand_equal_p (arg0, arg1, 0))
		result = integer_zero_node;
	    }
	  /* x = y + 0; x = y - 0; x = y * 0; */
	  else if (integer_zerop (arg1))
	    result = subcode == MULT_EXPR ? integer_zero_node : arg0;
	  /* x = 0 + y; x = 0 * y; */
	  else if (subcode != MINUS_EXPR && integer_zerop (arg0))
	    result = subcode == MULT_EXPR ? integer_zero_node : arg1;
	  /* x = y - y; */
	  else if (subcode == MINUS_EXPR && operand_equal_p (arg0, arg1, 0))
	    result = integer_zero_node;
	  /* x = y * 1; x = 1 * y; */
	  else if (subcode == MULT_EXPR && integer_onep (arg1))
	    result = arg0;
	  else if (subcode == MULT_EXPR && integer_onep (arg0))
	    result = arg1;
	  if (result)
	    {
	      if (result == integer_zero_node)
		result = build_zero_cst (type);
	      else if (cplx_result && TREE_TYPE (result) != type)
		{
		  if (TREE_CODE (result) == INTEGER_CST)
		    {
		      if (arith_overflowed_p (PLUS_EXPR, type, result,
					      integer_zero_node))
			overflow = build_one_cst (type);
		    }
		  else if ((!TYPE_UNSIGNED (TREE_TYPE (result))
			    && TYPE_UNSIGNED (type))
			   || (TYPE_PRECISION (type)
			       < (TYPE_PRECISION (TREE_TYPE (result))
				  + (TYPE_UNSIGNED (TREE_TYPE (result))
				     && !TYPE_UNSIGNED (type)))))
		    result = NULL_TREE;
		  if (result)
		    result = fold_convert (type, result);
		}
	    }
	}

      if (result)
	{
	  if (TREE_CODE (result) == INTEGER_CST && TREE_OVERFLOW (result))
	    result = drop_tree_overflow (result);
	  if (cplx_result)
	    {
	      if (overflow == NULL_TREE)
		overflow = build_zero_cst (TREE_TYPE (result));
	      tree ctype = build_complex_type (TREE_TYPE (result));
	      if (TREE_CODE (result) == INTEGER_CST
		  && TREE_CODE (overflow) == INTEGER_CST)
		result = build_complex (ctype, result, overflow);
	      else
		result = build2_loc (gimple_location (stmt), COMPLEX_EXPR,
				     ctype, result, overflow);
	    }
	  gimplify_and_update_call_from_tree (gsi, result);
	  changed = true;
	}
    }

  return changed;
}


/* Return true whether NAME has a use on STMT.  Note this can return
   false even though there's a use on STMT if SSA operands are not
   up-to-date.  */

static bool
has_use_on_stmt (tree name, gimple *stmt)
{
  ssa_op_iter iter;
  tree op;
  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
    if (op == name)
      return true;
  return false;
}

/* Worker for fold_stmt_1 dispatch to pattern based folding with
   gimple_simplify.

   Replaces *GSI with the simplification result in RCODE and OPS
   and the associated statements in *SEQ.  Does the replacement
   according to INPLACE and returns true if the operation succeeded.  */

static bool
replace_stmt_with_simplification (gimple_stmt_iterator *gsi,
				  gimple_match_op *res_op,
				  gimple_seq *seq, bool inplace)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree *ops = res_op->ops;
  unsigned int num_ops = res_op->num_ops;

  /* Play safe and do not allow abnormals to be mentioned in
     newly created statements.  See also maybe_push_res_to_seq.
     As an exception allow such uses if there was a use of the
     same SSA name on the old stmt.  */
  for (unsigned int i = 0; i < num_ops; ++i)
    if (TREE_CODE (ops[i]) == SSA_NAME
	&& SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ops[i])
	&& !has_use_on_stmt (ops[i], stmt))
      return false;

  if (num_ops > 0 && COMPARISON_CLASS_P (ops[0]))
    for (unsigned int i = 0; i < 2; ++i)
      if (TREE_CODE (TREE_OPERAND (ops[0], i)) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND (ops[0], i))
	  && !has_use_on_stmt (TREE_OPERAND (ops[0], i), stmt))
	return false;

  /* Don't insert new statements when INPLACE is true, even if we could
     reuse STMT for the final statement.  */
  if (inplace && !gimple_seq_empty_p (*seq))
    return false;

  if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
    {
      gcc_assert (res_op->code.is_tree_code ());
      auto code = tree_code (res_op->code);
      if (TREE_CODE_CLASS (code) == tcc_comparison
	  /* GIMPLE_CONDs condition may not throw.  */
	  && (!flag_exceptions
	      || !cfun->can_throw_non_call_exceptions
	      || !operation_could_trap_p (code,
					  FLOAT_TYPE_P (TREE_TYPE (ops[0])),
					  false, NULL_TREE)))
	gimple_cond_set_condition (cond_stmt, code, ops[0], ops[1]);
      else if (code == SSA_NAME)
	gimple_cond_set_condition (cond_stmt, NE_EXPR, ops[0],
				   build_zero_cst (TREE_TYPE (ops[0])));
      else if (code == INTEGER_CST)
	{
	  if (integer_zerop (ops[0]))
	    gimple_cond_make_false (cond_stmt);
	  else
	    gimple_cond_make_true (cond_stmt);
	}
      else if (!inplace)
	{
	  tree res = maybe_push_res_to_seq (res_op, seq);
	  if (!res)
	    return false;
	  gimple_cond_set_condition (cond_stmt, NE_EXPR, res,
				     build_zero_cst (TREE_TYPE (res)));
	}
      else
	return false;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "gimple_simplified to ");
	  if (!gimple_seq_empty_p (*seq))
	    print_gimple_seq (dump_file, *seq, 0, TDF_SLIM);
	  print_gimple_stmt (dump_file, gsi_stmt (*gsi),
			     0, TDF_SLIM);
	}
      gsi_insert_seq_before (gsi, *seq, GSI_SAME_STMT);
      return true;
    }
  else if (is_gimple_assign (stmt)
	   && res_op->code.is_tree_code ())
    {
      auto code = tree_code (res_op->code);
      if (!inplace
	  || gimple_num_ops (stmt) > get_gimple_rhs_num_ops (code))
	{
	  maybe_build_generic_op (res_op);
	  gimple_assign_set_rhs_with_ops (gsi, code,
					  res_op->op_or_null (0),
					  res_op->op_or_null (1),
					  res_op->op_or_null (2));
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "gimple_simplified to ");
	      if (!gimple_seq_empty_p (*seq))
		print_gimple_seq (dump_file, *seq, 0, TDF_SLIM);
	      print_gimple_stmt (dump_file, gsi_stmt (*gsi),
				 0, TDF_SLIM);
	    }
	  gsi_insert_seq_before (gsi, *seq, GSI_SAME_STMT);
	  return true;
	}
    }
  else if (res_op->code.is_fn_code ()
	   && gimple_call_combined_fn (stmt) == combined_fn (res_op->code))
    {
      gcc_assert (num_ops == gimple_call_num_args (stmt));
      for (unsigned int i = 0; i < num_ops; ++i)
	gimple_call_set_arg (stmt, i, ops[i]);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "gimple_simplified to ");
	  if (!gimple_seq_empty_p (*seq))
	    print_gimple_seq (dump_file, *seq, 0, TDF_SLIM);
	  print_gimple_stmt (dump_file, gsi_stmt (*gsi), 0, TDF_SLIM);
	}
      gsi_insert_seq_before (gsi, *seq, GSI_SAME_STMT);
      return true;
    }
  else if (!inplace)
    {
      if (gimple_has_lhs (stmt))
	{
	  tree lhs = gimple_get_lhs (stmt);
	  if (!maybe_push_res_to_seq (res_op, seq, lhs))
	    return false;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "gimple_simplified to ");
	      print_gimple_seq (dump_file, *seq, 0, TDF_SLIM);
	    }
	  gsi_replace_with_seq_vops (gsi, *seq);
	  return true;
	}
      else
	gcc_unreachable ();
    }

  return false;
}

/* Canonicalize MEM_REFs invariant address operand after propagation.  */

static bool
maybe_canonicalize_mem_ref_addr (tree *t, bool is_debug = false)
{
  bool res = false;
  tree *orig_t = t;

  if (TREE_CODE (*t) == ADDR_EXPR)
    t = &TREE_OPERAND (*t, 0);

  /* The C and C++ frontends use an ARRAY_REF for indexing with their
     generic vector extension.  The actual vector referenced is
     view-converted to an array type for this purpose.  If the index
     is constant the canonical representation in the middle-end is a
     BIT_FIELD_REF so re-write the former to the latter here.  */
  if (TREE_CODE (*t) == ARRAY_REF
      && TREE_CODE (TREE_OPERAND (*t, 0)) == VIEW_CONVERT_EXPR
      && TREE_CODE (TREE_OPERAND (*t, 1)) == INTEGER_CST
      && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (*t, 0), 0))))
    {
      tree vtype = TREE_TYPE (TREE_OPERAND (TREE_OPERAND (*t, 0), 0));
      if (VECTOR_TYPE_P (vtype))
	{
	  tree low = array_ref_low_bound (*t);
	  if (TREE_CODE (low) == INTEGER_CST)
	    {
	      if (tree_int_cst_le (low, TREE_OPERAND (*t, 1)))
		{
		  widest_int idx = wi::sub (wi::to_widest (TREE_OPERAND (*t, 1)),
					    wi::to_widest (low));
		  idx = wi::mul (idx, wi::to_widest
					 (TYPE_SIZE (TREE_TYPE (*t))));
		  widest_int ext
		    = wi::add (idx, wi::to_widest (TYPE_SIZE (TREE_TYPE (*t))));
		  if (wi::les_p (ext, wi::to_widest (TYPE_SIZE (vtype))))
		    {
		      *t = build3_loc (EXPR_LOCATION (*t), BIT_FIELD_REF,
				       TREE_TYPE (*t),
				       TREE_OPERAND (TREE_OPERAND (*t, 0), 0),
				       TYPE_SIZE (TREE_TYPE (*t)),
				       wide_int_to_tree (bitsizetype, idx));
		      res = true;
		    }
		}
	    }
	}
    }

  while (handled_component_p (*t))
    t = &TREE_OPERAND (*t, 0);

  /* Canonicalize MEM [&foo.bar, 0] which appears after propagating
     of invariant addresses into a SSA name MEM_REF address.  */
  if (TREE_CODE (*t) == MEM_REF
      || TREE_CODE (*t) == TARGET_MEM_REF)
    {
      tree addr = TREE_OPERAND (*t, 0);
      if (TREE_CODE (addr) == ADDR_EXPR
	  && (TREE_CODE (TREE_OPERAND (addr, 0)) == MEM_REF
	      || handled_component_p (TREE_OPERAND (addr, 0))))
	{
	  tree base;
	  poly_int64 coffset;
	  base = get_addr_base_and_unit_offset (TREE_OPERAND (addr, 0),
						&coffset);
	  if (!base)
	    {
	      if (is_debug)
		return false;
	      gcc_unreachable ();
	    }

	  TREE_OPERAND (*t, 0) = build_fold_addr_expr (base);
	  TREE_OPERAND (*t, 1) = int_const_binop (PLUS_EXPR,
						  TREE_OPERAND (*t, 1),
						  size_int (coffset));
	  res = true;
	}
      gcc_checking_assert (TREE_CODE (TREE_OPERAND (*t, 0)) == DEBUG_EXPR_DECL
			   || is_gimple_mem_ref_addr (TREE_OPERAND (*t, 0)));
    }

  /* Canonicalize back MEM_REFs to plain reference trees if the object
     accessed is a decl that has the same access semantics as the MEM_REF.  */
  if (TREE_CODE (*t) == MEM_REF
      && TREE_CODE (TREE_OPERAND (*t, 0)) == ADDR_EXPR
      && integer_zerop (TREE_OPERAND (*t, 1))
      && MR_DEPENDENCE_CLIQUE (*t) == 0)
    {
      tree decl = TREE_OPERAND (TREE_OPERAND (*t, 0), 0);
      tree alias_type = TREE_TYPE (TREE_OPERAND (*t, 1));
      if (/* Same volatile qualification.  */
	  TREE_THIS_VOLATILE (*t) == TREE_THIS_VOLATILE (decl)
	  /* Same TBAA behavior with -fstrict-aliasing.  */
	  && !TYPE_REF_CAN_ALIAS_ALL (alias_type)
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (decl))
	      == TYPE_MAIN_VARIANT (TREE_TYPE (alias_type)))
	  /* Same alignment.  */
	  && TYPE_ALIGN (TREE_TYPE (decl)) == TYPE_ALIGN (TREE_TYPE (*t))
	  /* We have to look out here to not drop a required conversion
	     from the rhs to the lhs if *t appears on the lhs or vice-versa
	     if it appears on the rhs.  Thus require strict type
	     compatibility.  */
	  && types_compatible_p (TREE_TYPE (*t), TREE_TYPE (decl)))
	{
	  *t = TREE_OPERAND (TREE_OPERAND (*t, 0), 0);
	  res = true;
	}
    }

  else if (TREE_CODE (*orig_t) == ADDR_EXPR
	   && TREE_CODE (*t) == MEM_REF
	   && TREE_CODE (TREE_OPERAND (*t, 0)) == INTEGER_CST)
    {
      tree base;
      poly_int64 coffset;
      base = get_addr_base_and_unit_offset (TREE_OPERAND (*orig_t, 0),
					    &coffset);
      if (base)
	{
	  gcc_assert (TREE_CODE (base) == MEM_REF);
	  poly_int64 moffset;
	  if (mem_ref_offset (base).to_shwi (&moffset))
	    {
	      coffset += moffset;
	      if (wi::to_poly_wide (TREE_OPERAND (base, 0)).to_shwi (&moffset))
		{
		  coffset += moffset;
		  *orig_t = build_int_cst (TREE_TYPE (*orig_t), coffset);
		  return true;
		}
	    }
	}
    }

  /* Canonicalize TARGET_MEM_REF in particular with respect to
     the indexes becoming constant.  */
  else if (TREE_CODE (*t) == TARGET_MEM_REF)
    {
      tree tem = maybe_fold_tmr (*t);
      if (tem)
	{
	  *t = tem;
	  if (TREE_CODE (*orig_t) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr (*orig_t);
	  res = true;
	}
    }

  return res;
}

/* Worker for both fold_stmt and fold_stmt_inplace.  The INPLACE argument
   distinguishes both cases.  */

static bool
fold_stmt_1 (gimple_stmt_iterator *gsi, bool inplace, tree (*valueize) (tree))
{
  bool changed = false;
  gimple *stmt = gsi_stmt (*gsi);
  bool nowarning = warning_suppressed_p (stmt, OPT_Wstrict_overflow);
  unsigned i;
  fold_defer_overflow_warnings ();

  /* First do required canonicalization of [TARGET_]MEM_REF addresses
     after propagation.
     ???  This shouldn't be done in generic folding but in the
     propagation helpers which also know whether an address was
     propagated.
     Also canonicalize operand order.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      if (gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
	{
	  tree *rhs = gimple_assign_rhs1_ptr (stmt);
	  if ((REFERENCE_CLASS_P (*rhs)
	       || TREE_CODE (*rhs) == ADDR_EXPR)
	      && maybe_canonicalize_mem_ref_addr (rhs))
	    changed = true;
	  tree *lhs = gimple_assign_lhs_ptr (stmt);
	  if (REFERENCE_CLASS_P (*lhs)
	      && maybe_canonicalize_mem_ref_addr (lhs))
	    changed = true;
	  /* Canonicalize &MEM[ssa_n, CST] to ssa_n p+ CST.
	     This cannot be done in maybe_canonicalize_mem_ref_addr
	     as the gimple now has two operands rather than one.
	     The same reason why this can't be done in
	     maybe_canonicalize_mem_ref_addr is the same reason why
	     this can't be done inplace.  */
	  if (!inplace && TREE_CODE (*rhs) == ADDR_EXPR)
	    {
	      tree inner = TREE_OPERAND (*rhs, 0);
	      if (TREE_CODE (inner) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (inner, 0)) == SSA_NAME
		  && TREE_CODE (TREE_OPERAND (inner, 1)) == INTEGER_CST)
		{
		  tree ptr = TREE_OPERAND (inner, 0);
		  tree addon = TREE_OPERAND (inner, 1);
		  addon = fold_convert (sizetype, addon);
		  gimple_assign_set_rhs_with_ops (gsi, POINTER_PLUS_EXPR,
						  ptr, addon);
		  changed = true;
		  stmt = gsi_stmt (*gsi);
		}
	    }
	}
      else
	{
	  /* Canonicalize operand order.  */
	  enum tree_code code = gimple_assign_rhs_code (stmt);
	  if (TREE_CODE_CLASS (code) == tcc_comparison
	      || commutative_tree_code (code)
	      || commutative_ternary_tree_code (code))
	    {
	      tree rhs1 = gimple_assign_rhs1 (stmt);
	      tree rhs2 = gimple_assign_rhs2 (stmt);
	      if (tree_swap_operands_p (rhs1, rhs2))
		{
		  gimple_assign_set_rhs1 (stmt, rhs2);
		  gimple_assign_set_rhs2 (stmt, rhs1);
		  if (TREE_CODE_CLASS (code) == tcc_comparison)
		    gimple_assign_set_rhs_code (stmt,
						swap_tree_comparison (code));
		  changed = true;
		}
	    }
	}
      break;
    case GIMPLE_CALL:
      {
	gcall *call = as_a<gcall *> (stmt);
	for (i = 0; i < gimple_call_num_args (call); ++i)
	  {
	    tree *arg = gimple_call_arg_ptr (call, i);
	    if (REFERENCE_CLASS_P (*arg)
		&& maybe_canonicalize_mem_ref_addr (arg))
	      changed = true;
	  }
	tree *lhs = gimple_call_lhs_ptr (call);
	if (*lhs
	    && REFERENCE_CLASS_P (*lhs)
	    && maybe_canonicalize_mem_ref_addr (lhs))
	  changed = true;
	if (*lhs)
	  {
	    combined_fn cfn = gimple_call_combined_fn (call);
	    internal_fn ifn = associated_internal_fn (cfn, TREE_TYPE (*lhs));
	    int opno = first_commutative_argument (ifn);
	    if (opno >= 0)
	      {
		tree arg1 = gimple_call_arg (call, opno);
		tree arg2 = gimple_call_arg (call, opno + 1);
		if (tree_swap_operands_p (arg1, arg2))
		  {
		    gimple_call_set_arg (call, opno, arg2);
		    gimple_call_set_arg (call, opno + 1, arg1);
		    changed = true;
		  }
	      }
	  }
	break;
      }
    case GIMPLE_ASM:
      {
	gasm *asm_stmt = as_a <gasm *> (stmt);
	for (i = 0; i < gimple_asm_noutputs (asm_stmt); ++i)
	  {
	    tree link = gimple_asm_output_op (asm_stmt, i);
	    tree op = TREE_VALUE (link);
	    if (REFERENCE_CLASS_P (op)
		&& maybe_canonicalize_mem_ref_addr (&TREE_VALUE (link)))
	      changed = true;
	  }
	for (i = 0; i < gimple_asm_ninputs (asm_stmt); ++i)
	  {
	    tree link = gimple_asm_input_op (asm_stmt, i);
	    tree op = TREE_VALUE (link);
	    if ((REFERENCE_CLASS_P (op)
		 || TREE_CODE (op) == ADDR_EXPR)
		&& maybe_canonicalize_mem_ref_addr (&TREE_VALUE (link)))
	      changed = true;
	  }
      }
      break;
    case GIMPLE_DEBUG:
      if (gimple_debug_bind_p (stmt))
	{
	  tree *val = gimple_debug_bind_get_value_ptr (stmt);
	  if (*val
	      && (REFERENCE_CLASS_P (*val)
		  || TREE_CODE (*val) == ADDR_EXPR)
	      && maybe_canonicalize_mem_ref_addr (val, true))
	    changed = true;
	}
      break;
    case GIMPLE_COND:
      {
	/* Canonicalize operand order.  */
	tree lhs = gimple_cond_lhs (stmt);
	tree rhs = gimple_cond_rhs (stmt);
	if (tree_swap_operands_p (lhs, rhs))
	  {
	    gcond *gc = as_a <gcond *> (stmt);
	    gimple_cond_set_lhs (gc, rhs);
	    gimple_cond_set_rhs (gc, lhs);
	    gimple_cond_set_code (gc,
				  swap_tree_comparison (gimple_cond_code (gc)));
	    changed = true;
	  }
      }
    default:;
    }

  /* Dispatch to pattern-based folding.  */
  if (!inplace
      || is_gimple_assign (stmt)
      || gimple_code (stmt) == GIMPLE_COND)
    {
      gimple_seq seq = NULL;
      gimple_match_op res_op;
      if (gimple_simplify (stmt, &res_op, inplace ? NULL : &seq,
			   valueize, valueize))
	{
	  if (replace_stmt_with_simplification (gsi, &res_op, &seq, inplace))
	    changed = true;
	  else
	    gimple_seq_discard (seq);
	}
    }

  stmt = gsi_stmt (*gsi);

  /* Fold the main computation performed by the statement.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      {
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
		  gimple_assign_set_rhs_with_ops (gsi, TREE_CODE (op1), op1);
		/* Only for one-bit precision typed X the transformation
		   !X -> ~X is valied.  */
		else if (TYPE_PRECISION (type) == 1)
		  gimple_assign_set_rhs_with_ops (gsi, BIT_NOT_EXPR, op1);
		/* Otherwise we use !X -> X ^ 1.  */
		else
		  gimple_assign_set_rhs_with_ops (gsi, BIT_XOR_EXPR, op1,
						  build_int_cst (type, 1));
		changed = true;
		break;
	      }
	  }

	unsigned old_num_ops = gimple_num_ops (stmt);
	tree lhs = gimple_assign_lhs (stmt);
	tree new_rhs = fold_gimple_assign (gsi);
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

    case GIMPLE_CALL:
      changed |= gimple_fold_call (gsi, inplace);
      break;

    case GIMPLE_DEBUG:
      if (gimple_debug_bind_p (stmt))
	{
	  tree val = gimple_debug_bind_get_value (stmt);
	  if (val && REFERENCE_CLASS_P (val))
	    {
	      tree tem = maybe_fold_reference (val);
	      if (tem)
		{
		  gimple_debug_bind_set_value (stmt, tem);
		  changed = true;
		}
	    }
	}
      break;

    case GIMPLE_RETURN:
      {
	greturn *ret_stmt = as_a<greturn *> (stmt);
	tree ret = gimple_return_retval(ret_stmt);

	if (ret && TREE_CODE (ret) == SSA_NAME && valueize)
	  {
	    tree val = valueize (ret);
	    if (val && val != ret
		&& may_propagate_copy (ret, val))
	      {
		gimple_return_set_retval (ret_stmt, val);
		changed = true;
	      }
	  }
      }
      break;

    default:;
    }

  stmt = gsi_stmt (*gsi);

  fold_undefer_overflow_warnings (changed && !nowarning, stmt, 0);
  return changed;
}

/* Valueziation callback that ends up not following SSA edges.  */

tree
no_follow_ssa_edges (tree)
{
  return NULL_TREE;
}

/* Valueization callback that ends up following single-use SSA edges only.  */

tree
follow_single_use_edges (tree val)
{
  if (TREE_CODE (val) == SSA_NAME
      && !has_single_use (val))
    return NULL_TREE;
  return val;
}

/* Valueization callback that follows all SSA edges.  */

tree
follow_all_ssa_edges (tree val)
{
  return val;
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
  return fold_stmt_1 (gsi, false, no_follow_ssa_edges);
}

bool
fold_stmt (gimple_stmt_iterator *gsi, tree (*valueize) (tree))
{
  return fold_stmt_1 (gsi, false, valueize);
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
  gimple *stmt = gsi_stmt (*gsi);
  bool changed = fold_stmt_1 (gsi, true, no_follow_ssa_edges);
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
      else if (COMPARISON_CLASS_P (expr))
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
      else if (COMPARISON_CLASS_P (expr))
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
  gimple *s;

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
  if (COMPARISON_CLASS_P (op2)
      && same_bool_comparison_p (op1, TREE_CODE (op2),
				 TREE_OPERAND (op2, 0),
				 TREE_OPERAND (op2, 1)))
    return true;
  if (COMPARISON_CLASS_P (op1)
      && same_bool_comparison_p (op2, TREE_CODE (op1),
				 TREE_OPERAND (op1, 0),
				 TREE_OPERAND (op1, 1)))
    return true;

  /* Default case.  */
  return false;
}

/* Forward declarations for some mutually recursive functions.  */

static tree
and_comparisons_1 (tree type, enum tree_code code1, tree op1a, tree op1b,
		   enum tree_code code2, tree op2a, tree op2b, basic_block);
static tree
and_var_with_comparison (tree type, tree var, bool invert,
			 enum tree_code code2, tree op2a, tree op2b,
			 basic_block);
static tree
and_var_with_comparison_1 (tree type, gimple *stmt,
			   enum tree_code code2, tree op2a, tree op2b,
			   basic_block);
static tree
or_comparisons_1 (tree, enum tree_code code1, tree op1a, tree op1b,
		  enum tree_code code2, tree op2a, tree op2b,
		  basic_block);
static tree
or_var_with_comparison (tree, tree var, bool invert,
			enum tree_code code2, tree op2a, tree op2b,
			basic_block);
static tree
or_var_with_comparison_1 (tree, gimple *stmt,
			  enum tree_code code2, tree op2a, tree op2b,
			  basic_block);

/* Helper function for and_comparisons_1:  try to simplify the AND of the
   ssa variable VAR with the comparison specified by (OP2A CODE2 OP2B).
   If INVERT is true, invert the value of the VAR before doing the AND.
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
and_var_with_comparison (tree type, tree var, bool invert,
			 enum tree_code code2, tree op2a, tree op2b,
			 basic_block outer_cond_bb)
{
  tree t;
  gimple *stmt = SSA_NAME_DEF_STMT (var);

  /* We can only deal with variables whose definitions are assignments.  */
  if (!is_gimple_assign (stmt))
    return NULL_TREE;
  
  /* If we have an inverted comparison, apply DeMorgan's law and rewrite
     !var AND (op2a code2 op2b) => !(var OR !(op2a code2 op2b))
     Then we only have to consider the simpler non-inverted cases.  */
  if (invert)
    t = or_var_with_comparison_1 (type, stmt,
				  invert_tree_comparison (code2, false),
				  op2a, op2b, outer_cond_bb);
  else
    t = and_var_with_comparison_1 (type, stmt, code2, op2a, op2b,
				   outer_cond_bb);
  return canonicalize_bool (t, invert);
}

/* Try to simplify the AND of the ssa variable defined by the assignment
   STMT with the comparison specified by (OP2A CODE2 OP2B).
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
and_var_with_comparison_1 (tree type, gimple *stmt,
			   enum tree_code code2, tree op2a, tree op2b,
			   basic_block outer_cond_bb)
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
      tree t = and_comparisons_1 (type, innercode,
				  gimple_assign_rhs1 (stmt),
				  gimple_assign_rhs2 (stmt),
				  code2,
				  op2a,
				  op2b, outer_cond_bb);
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
      gimple *s;
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
		: and_var_with_comparison (type, inner2, false, code2, op2a,
					   op2b, outer_cond_bb));
      else if (inner2 == false_test_var)
	return (is_and
		? boolean_false_node
		: and_var_with_comparison (type, inner1, false, code2, op2a,
					   op2b, outer_cond_bb));

      /* Next, redistribute/reassociate the AND across the inner tests.
	 Compute the first partial result, (inner1 AND (op2a code op2b))  */
      if (TREE_CODE (inner1) == SSA_NAME
	  && is_gimple_assign (s = SSA_NAME_DEF_STMT (inner1))
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison
	  && (t = maybe_fold_and_comparisons (type, gimple_assign_rhs_code (s),
					      gimple_assign_rhs1 (s),
					      gimple_assign_rhs2 (s),
					      code2, op2a, op2b,
					      outer_cond_bb)))
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
	  && (t = maybe_fold_and_comparisons (type, gimple_assign_rhs_code (s),
					      gimple_assign_rhs1 (s),
					      gimple_assign_rhs2 (s),
					      code2, op2a, op2b,
					      outer_cond_bb)))
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
and_comparisons_1 (tree type, enum tree_code code1, tree op1a, tree op1b,
		   enum tree_code code2, tree op2a, tree op2b,
		   basic_block outer_cond_bb)
{
  tree truth_type = truth_type_for (TREE_TYPE (op1a));

  /* First check for ((x CODE1 y) AND (x CODE2 y)).  */
  if (operand_equal_p (op1a, op2a, 0)
      && operand_equal_p (op1b, op2b, 0))
    {
      /* Result will be either NULL_TREE, or a combined comparison.  */
      tree t = combine_comparisons (UNKNOWN_LOCATION,
				    TRUTH_ANDIF_EXPR, code1, code2,
				    truth_type, op1a, op1b);
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
				    truth_type, op1a, op1b);
      if (t)
	return t;
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
      gimple *stmt = SSA_NAME_DEF_STMT (op1a);
      switch (gimple_code (stmt))
	{
	case GIMPLE_ASSIGN:
	  /* Try to simplify by copy-propagating the definition.  */
	  return and_var_with_comparison (type, op1a, invert, code2, op2a,
					  op2b, outer_cond_bb);

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
		      gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
		      /* In simple cases we can look through PHI nodes,
			 but we have to be careful with loops.
			 See PR49073.  */
		      if (! dom_info_available_p (CDI_DOMINATORS)
			  || gimple_bb (def_stmt) == gimple_bb (stmt)
			  || dominated_by_p (CDI_DOMINATORS,
					     gimple_bb (def_stmt),
					     gimple_bb (stmt)))
			return NULL_TREE;
		      temp = and_var_with_comparison (type, arg, invert, code2,
						      op2a, op2b,
						      outer_cond_bb);
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

static basic_block fosa_bb;
static vec<std::pair<tree, flow_sensitive_info_storage> > *fosa_unwind;
static tree
follow_outer_ssa_edges (tree val)
{
  if (TREE_CODE (val) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (val))
    {
      basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (val));
      if (!def_bb
	  || def_bb == fosa_bb
	  || (dom_info_available_p (CDI_DOMINATORS)
	      && (def_bb == fosa_bb
		  || dominated_by_p (CDI_DOMINATORS, fosa_bb, def_bb))))
	return val;
      /* We cannot temporarily rewrite stmts with undefined overflow
	 behavior, so avoid expanding them.  */
      if ((ANY_INTEGRAL_TYPE_P (TREE_TYPE (val))
	   || POINTER_TYPE_P (TREE_TYPE (val)))
	  && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (val)))
	return NULL_TREE;
      flow_sensitive_info_storage storage;
      storage.save_and_clear (val);
      /* If the definition does not dominate fosa_bb temporarily reset
	 flow-sensitive info.  */
      fosa_unwind->safe_push (std::make_pair (val, storage));
      return val;
    }
  return val;
}

/* Helper function for maybe_fold_and_comparisons and maybe_fold_or_comparisons
   : try to simplify the AND/OR of the ssa variable VAR with the comparison
   specified by (OP2A CODE2 OP2B) from match.pd.  Return NULL_EXPR if we can't
   simplify this to a single expression.  As we are going to lower the cost
   of building SSA names / gimple stmts significantly, we need to allocate
   them ont the stack.  This will cause the code to be a bit ugly.  */

static tree
maybe_fold_comparisons_from_match_pd (tree type, enum tree_code code,
				      enum tree_code code1,
				      tree op1a, tree op1b,
				      enum tree_code code2, tree op2a,
				      tree op2b,
				      basic_block outer_cond_bb)
{
  /* Allocate gimple stmt1 on the stack.  */
  gassign *stmt1
    = (gassign *) XALLOCAVEC (char, gimple_size (GIMPLE_ASSIGN, 3));
  gimple_init (stmt1, GIMPLE_ASSIGN, 3);
  gimple_assign_set_rhs_code (stmt1, code1);
  gimple_assign_set_rhs1 (stmt1, op1a);
  gimple_assign_set_rhs2 (stmt1, op1b);
  gimple_set_bb (stmt1, NULL);

  /* Allocate gimple stmt2 on the stack.  */
  gassign *stmt2
    = (gassign *) XALLOCAVEC (char, gimple_size (GIMPLE_ASSIGN, 3));
  gimple_init (stmt2, GIMPLE_ASSIGN, 3);
  gimple_assign_set_rhs_code (stmt2, code2);
  gimple_assign_set_rhs1 (stmt2, op2a);
  gimple_assign_set_rhs2 (stmt2, op2b);
  gimple_set_bb (stmt2, NULL);

  /* Allocate SSA names(lhs1) on the stack.  */
  tree lhs1 = (tree)XALLOCA (tree_ssa_name);
  memset (lhs1, 0, sizeof (tree_ssa_name));
  TREE_SET_CODE (lhs1, SSA_NAME);
  TREE_TYPE (lhs1) = type;
  init_ssa_name_imm_use (lhs1);

  /* Allocate SSA names(lhs2) on the stack.  */
  tree lhs2 = (tree)XALLOCA (tree_ssa_name);
  memset (lhs2, 0, sizeof (tree_ssa_name));
  TREE_SET_CODE (lhs2, SSA_NAME);
  TREE_TYPE (lhs2) = type;
  init_ssa_name_imm_use (lhs2);

  gimple_assign_set_lhs (stmt1, lhs1);
  gimple_assign_set_lhs (stmt2, lhs2);

  gimple_match_op op (gimple_match_cond::UNCOND, code,
		      type, gimple_assign_lhs (stmt1),
		      gimple_assign_lhs (stmt2));
  fosa_bb = outer_cond_bb;
  auto_vec<std::pair<tree, flow_sensitive_info_storage>, 8> unwind_stack;
  fosa_unwind = &unwind_stack;
  if (op.resimplify (NULL, (!outer_cond_bb
			    ? follow_all_ssa_edges : follow_outer_ssa_edges)))
    {
      fosa_unwind = NULL;
      for (auto p : unwind_stack)
	p.second.restore (p.first);
      if (gimple_simplified_result_is_gimple_val (&op))
	{
	  tree res = op.ops[0];
	  if (res == lhs1)
	    return build2 (code1, type, op1a, op1b);
	  else if (res == lhs2)
	    return build2 (code2, type, op2a, op2b);
	  else
	    return res;
	}
      else if (op.code.is_tree_code ()
	       && TREE_CODE_CLASS ((tree_code)op.code) == tcc_comparison)
	{
	  tree op0 = op.ops[0];
	  tree op1 = op.ops[1];
	  if (op0 == lhs1 || op0 == lhs2 || op1 == lhs1 || op1 == lhs2)
	    return NULL_TREE;  /* not simple */

	  return build2 ((enum tree_code)op.code, op.type, op0, op1);
	}
    }
  fosa_unwind = NULL;
  for (auto p : unwind_stack)
    p.second.restore (p.first);

  return NULL_TREE;
}

/* Try to simplify the AND of two comparisons, specified by
   (OP1A CODE1 OP1B) and (OP2B CODE2 OP2B), respectively.
   If this can be simplified to a single expression (without requiring
   introducing more SSA variables to hold intermediate values),
   return the resulting tree.  Otherwise return NULL_TREE.
   If the result expression is non-null, it has boolean type.  */

tree
maybe_fold_and_comparisons (tree type,
			    enum tree_code code1, tree op1a, tree op1b,
			    enum tree_code code2, tree op2a, tree op2b,
			    basic_block outer_cond_bb)
{
  if (tree t = and_comparisons_1 (type, code1, op1a, op1b, code2, op2a, op2b,
				  outer_cond_bb))
    return t;

  if (tree t = and_comparisons_1 (type, code2, op2a, op2b, code1, op1a, op1b,
				  outer_cond_bb))
    return t;

  if (tree t = maybe_fold_comparisons_from_match_pd (type, BIT_AND_EXPR, code1,
						     op1a, op1b, code2, op2a,
						     op2b, outer_cond_bb))
    return t;

  return NULL_TREE;
}

/* Helper function for or_comparisons_1:  try to simplify the OR of the
   ssa variable VAR with the comparison specified by (OP2A CODE2 OP2B).
   If INVERT is true, invert the value of VAR before doing the OR.
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
or_var_with_comparison (tree type, tree var, bool invert,
			enum tree_code code2, tree op2a, tree op2b,
			basic_block outer_cond_bb)
{
  tree t;
  gimple *stmt = SSA_NAME_DEF_STMT (var);

  /* We can only deal with variables whose definitions are assignments.  */
  if (!is_gimple_assign (stmt))
    return NULL_TREE;
  
  /* If we have an inverted comparison, apply DeMorgan's law and rewrite
     !var OR (op2a code2 op2b) => !(var AND !(op2a code2 op2b))
     Then we only have to consider the simpler non-inverted cases.  */
  if (invert)
    t = and_var_with_comparison_1 (type, stmt,
				   invert_tree_comparison (code2, false),
				   op2a, op2b, outer_cond_bb);
  else
    t = or_var_with_comparison_1 (type, stmt, code2, op2a, op2b,
				  outer_cond_bb);
  return canonicalize_bool (t, invert);
}

/* Try to simplify the OR of the ssa variable defined by the assignment
   STMT with the comparison specified by (OP2A CODE2 OP2B).
   Return NULL_EXPR if we can't simplify this to a single expression.  */

static tree
or_var_with_comparison_1 (tree type, gimple *stmt,
			  enum tree_code code2, tree op2a, tree op2b,
			  basic_block outer_cond_bb)
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
      tree t = or_comparisons_1 (type, innercode,
				 gimple_assign_rhs1 (stmt),
				 gimple_assign_rhs2 (stmt),
				 code2, op2a, op2b, outer_cond_bb);
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
      gimple *s;
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
		: or_var_with_comparison (type, inner2, false, code2, op2a,
					  op2b, outer_cond_bb));
      else if (inner2 == false_test_var)
	return (is_or
		? boolean_true_node
		: or_var_with_comparison (type, inner1, false, code2, op2a,
					  op2b, outer_cond_bb));
      
      /* Next, redistribute/reassociate the OR across the inner tests.
	 Compute the first partial result, (inner1 OR (op2a code op2b))  */
      if (TREE_CODE (inner1) == SSA_NAME
	  && is_gimple_assign (s = SSA_NAME_DEF_STMT (inner1))
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison
	  && (t = maybe_fold_or_comparisons (type, gimple_assign_rhs_code (s),
					     gimple_assign_rhs1 (s),
					     gimple_assign_rhs2 (s),
					     code2, op2a, op2b,
					     outer_cond_bb)))
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
	  && (t = maybe_fold_or_comparisons (type, gimple_assign_rhs_code (s),
					     gimple_assign_rhs1 (s),
					     gimple_assign_rhs2 (s),
					     code2, op2a, op2b,
					     outer_cond_bb)))
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
or_comparisons_1 (tree type, enum tree_code code1, tree op1a, tree op1b,
		  enum tree_code code2, tree op2a, tree op2b,
		  basic_block outer_cond_bb)
{
  tree truth_type = truth_type_for (TREE_TYPE (op1a));

  /* First check for ((x CODE1 y) OR (x CODE2 y)).  */
  if (operand_equal_p (op1a, op2a, 0)
      && operand_equal_p (op1b, op2b, 0))
    {
      /* Result will be either NULL_TREE, or a combined comparison.  */
      tree t = combine_comparisons (UNKNOWN_LOCATION,
				    TRUTH_ORIF_EXPR, code1, code2,
				    truth_type, op1a, op1b);
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
				    truth_type, op1a, op1b);
      if (t)
	return t;
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
      gimple *stmt = SSA_NAME_DEF_STMT (op1a);
      switch (gimple_code (stmt))
	{
	case GIMPLE_ASSIGN:
	  /* Try to simplify by copy-propagating the definition.  */
	  return or_var_with_comparison (type, op1a, invert, code2, op2a,
					 op2b, outer_cond_bb);

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
		      gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
		      /* In simple cases we can look through PHI nodes,
			 but we have to be careful with loops.
			 See PR49073.  */
		      if (! dom_info_available_p (CDI_DOMINATORS)
			  || gimple_bb (def_stmt) == gimple_bb (stmt)
			  || dominated_by_p (CDI_DOMINATORS,
					     gimple_bb (def_stmt),
					     gimple_bb (stmt)))
			return NULL_TREE;
		      temp = or_var_with_comparison (type, arg, invert, code2,
						     op2a, op2b, outer_cond_bb);
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
maybe_fold_or_comparisons (tree type,
			   enum tree_code code1, tree op1a, tree op1b,
			   enum tree_code code2, tree op2a, tree op2b,
			   basic_block outer_cond_bb)
{
  if (tree t = or_comparisons_1 (type, code1, op1a, op1b, code2, op2a, op2b,
				 outer_cond_bb))
    return t;

  if (tree t = or_comparisons_1 (type, code2, op2a, op2b, code1, op1a, op1b,
				 outer_cond_bb))
    return t;

  if (tree t = maybe_fold_comparisons_from_match_pd (type, BIT_IOR_EXPR, code1,
						     op1a, op1b, code2, op2a,
						     op2b, outer_cond_bb))
    return t;

  return NULL_TREE;
}

/* Fold STMT to a constant using VALUEIZE to valueize SSA names.

   Either NULL_TREE, a simplified but non-constant or a constant
   is returned.

   ???  This should go into a gimple-fold-inline.h file to be eventually
   privatized with the single valueize function used in the various TUs
   to avoid the indirect function call overhead.  */

tree
gimple_fold_stmt_to_constant_1 (gimple *stmt, tree (*valueize) (tree),
				tree (*gvalueize) (tree))
{
  gimple_match_op res_op;
  /* ???  The SSA propagators do not correctly deal with following SSA use-def
     edges if there are intermediate VARYING defs.  For this reason
     do not follow SSA edges here even though SCCVN can technically
     just deal fine with that.  */
  if (gimple_simplify (stmt, &res_op, NULL, gvalueize, valueize))
    {
      tree res = NULL_TREE;
      if (gimple_simplified_result_is_gimple_val (&res_op))
	res = res_op.ops[0];
      else if (mprts_hook)
	res = mprts_hook (&res_op);
      if (res)
	{
	  if (dump_file && dump_flags & TDF_DETAILS)
	    {
	      fprintf (dump_file, "Match-and-simplified ");
	      print_gimple_expr (dump_file, stmt, 0, TDF_SLIM);
	      fprintf (dump_file, " to ");
	      print_generic_expr (dump_file, res);
	      fprintf (dump_file, "\n");
	    }
	  return res;
	}
    }

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
		  poly_int64 offset = 0;
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
		       && known_eq (CONSTRUCTOR_NELTS (rhs),
				    TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs))))
		{
		  unsigned i, nelts;
		  tree val;

		  nelts = CONSTRUCTOR_NELTS (rhs);
		  tree_vector_builder vec (TREE_TYPE (rhs), nelts, 1);
		  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), i, val)
		    {
		      val = (*valueize) (val);
		      if (TREE_CODE (val) == INTEGER_CST
			  || TREE_CODE (val) == REAL_CST
			  || TREE_CODE (val) == FIXED_CST)
			vec.quick_push (val);
		      else
			return NULL_TREE;
		    }

		  return vec.build ();
		}
	      if (subcode == OBJ_TYPE_REF)
		{
		  tree val = (*valueize) (OBJ_TYPE_REF_EXPR (rhs));
		  /* If callee is constant, we can fold away the wrapper.  */
		  if (is_gimple_min_invariant (val))
		    return val;
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
	    return NULL_TREE;

          case GIMPLE_BINARY_RHS:
	    /* Translate &x + CST into an invariant form suitable for
	       further propagation.  */
	    if (subcode == POINTER_PLUS_EXPR)
	      {
		tree op0 = (*valueize) (gimple_assign_rhs1 (stmt));
		tree op1 = (*valueize) (gimple_assign_rhs2 (stmt));
		if (TREE_CODE (op0) == ADDR_EXPR
		    && TREE_CODE (op1) == INTEGER_CST)
		  {
		    tree off = fold_convert (ptr_type_node, op1);
		    return build1_loc
			(loc, ADDR_EXPR, TREE_TYPE (op0),
			 fold_build2 (MEM_REF,
				      TREE_TYPE (TREE_TYPE (op0)),
				      unshare_expr (op0), off));
		  }
	      }
	    /* Canonicalize bool != 0 and bool == 0 appearing after
	       valueization.  While gimple_simplify handles this
	       it can get confused by the ~X == 1 -> X == 0 transform
	       which we cant reduce to a SSA name or a constant
	       (and we have no way to tell gimple_simplify to not
	       consider those transforms in the first place).  */
	    else if (subcode == EQ_EXPR
		     || subcode == NE_EXPR)
	      {
		tree lhs = gimple_assign_lhs (stmt);
		tree op0 = gimple_assign_rhs1 (stmt);
		if (useless_type_conversion_p (TREE_TYPE (lhs),
					       TREE_TYPE (op0)))
		  {
		    tree op1 = (*valueize) (gimple_assign_rhs2 (stmt));
		    op0 = (*valueize) (op0);
		    if (TREE_CODE (op0) == INTEGER_CST)
		      std::swap (op0, op1);
		    if (TREE_CODE (op1) == INTEGER_CST
			&& ((subcode == NE_EXPR && integer_zerop (op1))
			    || (subcode == EQ_EXPR && integer_onep (op1))))
		      return op0;
		  }
	      }
	    return NULL_TREE;

          case GIMPLE_TERNARY_RHS:
            {
              /* Handle ternary operators that can appear in GIMPLE form.  */
              tree op0 = (*valueize) (gimple_assign_rhs1 (stmt));
              tree op1 = (*valueize) (gimple_assign_rhs2 (stmt));
              tree op2 = (*valueize) (gimple_assign_rhs3 (stmt));
              return fold_ternary_loc (loc, subcode,
				       TREE_TYPE (gimple_assign_lhs (stmt)),
				       op0, op1, op2);
            }

          default:
            gcc_unreachable ();
          }
      }

    case GIMPLE_CALL:
      {
	tree fn;
	gcall *call_stmt = as_a <gcall *> (stmt);

	if (gimple_call_internal_p (stmt))
	  {
	    enum tree_code subcode = ERROR_MARK;
	    switch (gimple_call_internal_fn (stmt))
	      {
	      case IFN_UBSAN_CHECK_ADD:
		subcode = PLUS_EXPR;
		break;
	      case IFN_UBSAN_CHECK_SUB:
		subcode = MINUS_EXPR;
		break;
	      case IFN_UBSAN_CHECK_MUL:
		subcode = MULT_EXPR;
		break;
	      case IFN_BUILTIN_EXPECT:
		  {
		    tree arg0 = gimple_call_arg (stmt, 0);
		    tree op0 = (*valueize) (arg0);
		    if (TREE_CODE (op0) == INTEGER_CST)
		      return op0;
		    return NULL_TREE;
		  }
	      default:
		return NULL_TREE;
	      }
	    tree arg0 = gimple_call_arg (stmt, 0);
	    tree arg1 = gimple_call_arg (stmt, 1);
	    tree op0 = (*valueize) (arg0);
	    tree op1 = (*valueize) (arg1);

	    if (TREE_CODE (op0) != INTEGER_CST
		|| TREE_CODE (op1) != INTEGER_CST)
	      {
		switch (subcode)
		  {
		  case MULT_EXPR:
		    /* x * 0 = 0 * x = 0 without overflow.  */
		    if (integer_zerop (op0) || integer_zerop (op1))
		      return build_zero_cst (TREE_TYPE (arg0));
		    break;
		  case MINUS_EXPR:
		    /* y - y = 0 without overflow.  */
		    if (operand_equal_p (op0, op1, 0))
		      return build_zero_cst (TREE_TYPE (arg0));
		    break;
		  default:
		    break;
		  }
	      }
	    tree res
	      = fold_binary_loc (loc, subcode, TREE_TYPE (arg0), op0, op1);
	    if (res
		&& TREE_CODE (res) == INTEGER_CST
		&& !TREE_OVERFLOW (res))
	      return res;
	    return NULL_TREE;
	  }

	fn = (*valueize) (gimple_call_fn (stmt));
	if (TREE_CODE (fn) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
	    && fndecl_built_in_p (TREE_OPERAND (fn, 0))
	    && gimple_builtin_call_types_compatible_p (stmt,
						       TREE_OPERAND (fn, 0)))
	  {
	    tree *args = XALLOCAVEC (tree, gimple_call_num_args (stmt));
	    tree retval;
	    unsigned i;
	    for (i = 0; i < gimple_call_num_args (stmt); ++i)
	      args[i] = (*valueize) (gimple_call_arg (stmt, i));
	    retval = fold_builtin_call_array (loc,
					 gimple_call_return_type (call_stmt),
					 fn, gimple_call_num_args (stmt), args);
	    if (retval)
	      {
		/* fold_call_expr wraps the result inside a NOP_EXPR.  */
		STRIP_NOPS (retval);
		retval = fold_convert (gimple_call_return_type (call_stmt),
				       retval);
	      }
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
gimple_fold_stmt_to_constant (gimple *stmt, tree (*valueize) (tree))
{
  tree res = gimple_fold_stmt_to_constant_1 (stmt, valueize);
  if (res && is_gimple_min_invariant (res))
    return res;
  return NULL_TREE;
}


/* The following set of functions are supposed to fold references using
   their constant initializers.  */

/* See if we can find constructor defining value of BASE.
   When we know the consructor with constant offset (such as
   base is array[40] and we do know constructor of array), then
   BIT_OFFSET is adjusted accordingly.

   As a special case, return error_mark_node when constructor
   is not explicitly available, but it is known to be zero
   such as 'static const int a;'.  */
static tree
get_base_constructor (tree base, poly_int64_pod *bit_offset,
		      tree (*valueize)(tree))
{
  poly_int64 bit_offset2, size, max_size;
  bool reverse;

  if (TREE_CODE (base) == MEM_REF)
    {
      poly_offset_int boff = *bit_offset + mem_ref_offset (base) * BITS_PER_UNIT;
      if (!boff.to_shwi (bit_offset))
	return NULL_TREE;

      if (valueize
	  && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
	base = valueize (TREE_OPERAND (base, 0));
      if (!base || TREE_CODE (base) != ADDR_EXPR)
        return NULL_TREE;
      base = TREE_OPERAND (base, 0);
    }
  else if (valueize
	   && TREE_CODE (base) == SSA_NAME)
    base = valueize (base);

  /* Get a CONSTRUCTOR.  If BASE is a VAR_DECL, get its
     DECL_INITIAL.  If BASE is a nested reference into another
     ARRAY_REF or COMPONENT_REF, make a recursive call to resolve
     the inner reference.  */
  switch (TREE_CODE (base))
    {
    case VAR_DECL:
    case CONST_DECL:
      {
	tree init = ctor_for_folding (base);

	/* Our semantic is exact opposite of ctor_for_folding;
	   NULL means unknown, while error_mark_node is 0.  */
	if (init == error_mark_node)
	  return NULL_TREE;
	if (!init)
	  return error_mark_node;
	return init;
      }

    case VIEW_CONVERT_EXPR:
      return get_base_constructor (TREE_OPERAND (base, 0),
				   bit_offset, valueize);

    case ARRAY_REF:
    case COMPONENT_REF:
      base = get_ref_base_and_extent (base, &bit_offset2, &size, &max_size,
				      &reverse);
      if (!known_size_p (max_size) || maybe_ne (size, max_size))
	return NULL_TREE;
      *bit_offset +=  bit_offset2;
      return get_base_constructor (base, bit_offset, valueize);

    case CONSTRUCTOR:
      return base;

    default:
      if (CONSTANT_CLASS_P (base))
	return base;

      return NULL_TREE;
    }
}

/* CTOR is a CONSTRUCTOR of an array or vector type.  Fold a reference of SIZE
   bits to the memory at bit OFFSET.  If non-null, TYPE is the expected type of
   the reference; otherwise the type of the referenced element is used instead.
   When SIZE is zero, attempt to fold a reference to the entire element OFFSET
   refers to.  Increment *SUBOFF by the bit offset of the accessed element.  */

static tree
fold_array_ctor_reference (tree type, tree ctor,
			   unsigned HOST_WIDE_INT offset,
			   unsigned HOST_WIDE_INT size,
			   tree from_decl,
			   unsigned HOST_WIDE_INT *suboff)
{
  offset_int low_bound;
  offset_int elt_size;
  offset_int access_index;
  tree domain_type = NULL_TREE;
  HOST_WIDE_INT inner_offset;

  /* Compute low bound and elt size.  */
  if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE)
    domain_type = TYPE_DOMAIN (TREE_TYPE (ctor));
  if (domain_type && TYPE_MIN_VALUE (domain_type))
    {
      /* Static constructors for variably sized objects make no sense.  */
      if (TREE_CODE (TYPE_MIN_VALUE (domain_type)) != INTEGER_CST)
	return NULL_TREE;
      low_bound = wi::to_offset (TYPE_MIN_VALUE (domain_type));
    }
  else
    low_bound = 0;
  /* Static constructors for variably sized objects make no sense.  */
  if (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ctor)))) != INTEGER_CST)
    return NULL_TREE;
  elt_size = wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ctor))));

  /* When TYPE is non-null, verify that it specifies a constant-sized
     access of a multiple of the array element size.  Avoid division
     by zero below when ELT_SIZE is zero, such as with the result of
     an initializer for a zero-length array or an empty struct.  */
  if (elt_size == 0
      || (type
	  && (!TYPE_SIZE_UNIT (type)
	      || TREE_CODE (TYPE_SIZE_UNIT (type)) != INTEGER_CST)))
    return NULL_TREE;

  /* Compute the array index we look for.  */
  access_index = wi::udiv_trunc (offset_int (offset / BITS_PER_UNIT),
				 elt_size);
  access_index += low_bound;

  /* And offset within the access.  */
  inner_offset = offset % (elt_size.to_uhwi () * BITS_PER_UNIT);

  unsigned HOST_WIDE_INT elt_sz = elt_size.to_uhwi ();
  if (size > elt_sz * BITS_PER_UNIT)
    {
      /* native_encode_expr constraints.  */
      if (size > MAX_BITSIZE_MODE_ANY_MODE
	  || size % BITS_PER_UNIT != 0
	  || inner_offset % BITS_PER_UNIT != 0
	  || elt_sz > MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT)
	return NULL_TREE;

      unsigned ctor_idx;
      tree val = get_array_ctor_element_at_index (ctor, access_index,
						  &ctor_idx);
      if (!val && ctor_idx >= CONSTRUCTOR_NELTS  (ctor))
	return build_zero_cst (type);

      /* native-encode adjacent ctor elements.  */
      unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
      unsigned bufoff = 0;
      offset_int index = 0;
      offset_int max_index = access_index;
      constructor_elt *elt = CONSTRUCTOR_ELT (ctor, ctor_idx);
      if (!val)
	val = build_zero_cst (TREE_TYPE (TREE_TYPE (ctor)));
      else if (!CONSTANT_CLASS_P (val))
	return NULL_TREE;
      if (!elt->index)
	;
      else if (TREE_CODE (elt->index) == RANGE_EXPR)
	{
	  index = wi::to_offset (TREE_OPERAND (elt->index, 0));
	  max_index = wi::to_offset (TREE_OPERAND (elt->index, 1));
	}
      else
	index = max_index = wi::to_offset (elt->index);
      index = wi::umax (index, access_index);
      do
	{
	  if (bufoff + elt_sz > sizeof (buf))
	    elt_sz = sizeof (buf) - bufoff;
	  int len = native_encode_expr (val, buf + bufoff, elt_sz,
					inner_offset / BITS_PER_UNIT);
	  if (len != (int) elt_sz - inner_offset / BITS_PER_UNIT)
	    return NULL_TREE;
	  inner_offset = 0;
	  bufoff += len;

	  access_index += 1;
	  if (wi::cmpu (access_index, index) == 0)
	    val = elt->value;
	  else if (wi::cmpu (access_index, max_index) > 0)
	    {
	      ctor_idx++;
	      if (ctor_idx >= CONSTRUCTOR_NELTS (ctor))
		{
		  val = build_zero_cst (TREE_TYPE (TREE_TYPE (ctor)));
		  ++max_index;
		}
	      else
		{
		  elt = CONSTRUCTOR_ELT (ctor, ctor_idx);
		  index = 0;
		  max_index = access_index;
		  if (!elt->index)
		    ;
		  else if (TREE_CODE (elt->index) == RANGE_EXPR)
		    {
		      index = wi::to_offset (TREE_OPERAND (elt->index, 0));
		      max_index = wi::to_offset (TREE_OPERAND (elt->index, 1));
		    }
		  else
		    index = max_index = wi::to_offset (elt->index);
		  index = wi::umax (index, access_index);
		  if (wi::cmpu (access_index, index) == 0)
		    val = elt->value;
		  else
		    val = build_zero_cst (TREE_TYPE (TREE_TYPE (ctor)));
		}
	    }
	}
      while (bufoff < size / BITS_PER_UNIT);
      *suboff += size;
      return native_interpret_expr (type, buf, size / BITS_PER_UNIT);
    }

  if (tree val = get_array_ctor_element_at_index (ctor, access_index))
    {
      if (!size && TREE_CODE (val) != CONSTRUCTOR)
	{
	  /* For the final reference to the entire accessed element
	     (SIZE is zero), reset INNER_OFFSET, disegard TYPE (which
	     may be null) in favor of the type of the element, and set
	     SIZE to the size of the accessed element.  */
	  inner_offset = 0;
	  type = TREE_TYPE (val);
	  size = elt_sz * BITS_PER_UNIT;
	}
      else if (size && access_index < CONSTRUCTOR_NELTS (ctor) - 1
	       && TREE_CODE (val) == CONSTRUCTOR
	       && (elt_sz * BITS_PER_UNIT - inner_offset) < size)
	/* If this isn't the last element in the CTOR and a CTOR itself
	   and it does not cover the whole object we are requesting give up
	   since we're not set up for combining from multiple CTORs.  */
	return NULL_TREE;

      *suboff += access_index.to_uhwi () * elt_sz * BITS_PER_UNIT;
      return fold_ctor_reference (type, val, inner_offset, size, from_decl,
				  suboff);
    }

  /* Memory not explicitly mentioned in constructor is 0 (or
     the reference is out of range).  */
  return type ? build_zero_cst (type) : NULL_TREE;
}

/* CTOR is a CONSTRUCTOR of a record or union type.  Fold a reference of SIZE
   bits to the memory at bit OFFSET.  If non-null, TYPE is the expected type of
   the reference; otherwise the type of the referenced member is used instead.
   When SIZE is zero, attempt to fold a reference to the entire member OFFSET
   refers to.  Increment *SUBOFF by the bit offset of the accessed member.  */

static tree
fold_nonarray_ctor_reference (tree type, tree ctor,
			      unsigned HOST_WIDE_INT offset,
			      unsigned HOST_WIDE_INT size,
			      tree from_decl,
			      unsigned HOST_WIDE_INT *suboff)
{
  unsigned HOST_WIDE_INT cnt;
  tree cfield, cval;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield, cval)
    {
      tree byte_offset = DECL_FIELD_OFFSET (cfield);
      tree field_offset = DECL_FIELD_BIT_OFFSET (cfield);
      tree field_size = DECL_SIZE (cfield);

      if (!field_size)
	{
	  /* Determine the size of the flexible array member from
	     the size of the initializer provided for it.  */
	  field_size = TYPE_SIZE (TREE_TYPE (cval));
	}

      /* Variable sized objects in static constructors makes no sense,
	 but field_size can be NULL for flexible array members.  */
      gcc_assert (TREE_CODE (field_offset) == INTEGER_CST
		  && TREE_CODE (byte_offset) == INTEGER_CST
		  && (field_size != NULL_TREE
		      ? TREE_CODE (field_size) == INTEGER_CST
		      : TREE_CODE (TREE_TYPE (cfield)) == ARRAY_TYPE));

      /* Compute bit offset of the field.  */
      offset_int bitoffset
	= (wi::to_offset (field_offset)
	   + (wi::to_offset (byte_offset) << LOG2_BITS_PER_UNIT));
      /* Compute bit offset where the field ends.  */
      offset_int bitoffset_end;
      if (field_size != NULL_TREE)
	bitoffset_end = bitoffset + wi::to_offset (field_size);
      else
	bitoffset_end = 0;

      /* Compute the bit offset of the end of the desired access.
	 As a special case, if the size of the desired access is
	 zero, assume the access is to the entire field (and let
	 the caller make any necessary adjustments by storing
	 the actual bounds of the field in FIELDBOUNDS).  */
      offset_int access_end = offset_int (offset);
      if (size)
	access_end += size;
      else
	access_end = bitoffset_end;

      /* Is there any overlap between the desired access at
	 [OFFSET, OFFSET+SIZE) and the offset of the field within
	 the object at [BITOFFSET, BITOFFSET_END)?  */
      if (wi::cmps (access_end, bitoffset) > 0
	  && (field_size == NULL_TREE
	      || wi::lts_p (offset, bitoffset_end)))
	{
	  *suboff += bitoffset.to_uhwi ();

	  if (!size && TREE_CODE (cval) != CONSTRUCTOR)
	    {
	      /* For the final reference to the entire accessed member
		 (SIZE is zero), reset OFFSET, disegard TYPE (which may
		 be null) in favor of the type of the member, and set
		 SIZE to the size of the accessed member.  */
	      offset = bitoffset.to_uhwi ();
	      type = TREE_TYPE (cval);
	      size = (bitoffset_end - bitoffset).to_uhwi ();
	    }

	  /* We do have overlap.  Now see if the field is large enough
	     to cover the access.  Give up for accesses that extend
	     beyond the end of the object or that span multiple fields.  */
	  if (wi::cmps (access_end, bitoffset_end) > 0)
	    return NULL_TREE;
	  if (offset < bitoffset)
	    return NULL_TREE;

	  offset_int inner_offset = offset_int (offset) - bitoffset;

	  /* Integral bit-fields are left-justified on big-endian targets, so
	     we must arrange for native_encode_int to start at their MSB.  */
	  if (DECL_BIT_FIELD (cfield) && INTEGRAL_TYPE_P (TREE_TYPE (cfield)))
	    {
	      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
		return NULL_TREE;
	      const unsigned int encoding_size
		= GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (TREE_TYPE (cfield)));
	      if (BYTES_BIG_ENDIAN)
		inner_offset += encoding_size - wi::to_offset (field_size);
	    }

	  return fold_ctor_reference (type, cval,
				      inner_offset.to_uhwi (), size,
				      from_decl, suboff);
	}
    }

  if (!type)
    return NULL_TREE;

  return build_zero_cst (type);
}

/* CTOR is a value initializing memory.  Fold a reference of TYPE and
   bit size POLY_SIZE to the memory at bit POLY_OFFSET.  When POLY_SIZE
   is zero, attempt to fold a reference to the entire subobject
   which OFFSET refers to.  This is used when folding accesses to
   string members of aggregates.  When non-null, set *SUBOFF to
   the bit offset of the accessed subobject.  */

tree
fold_ctor_reference (tree type, tree ctor, const poly_uint64 &poly_offset,
		     const poly_uint64 &poly_size, tree from_decl,
		     unsigned HOST_WIDE_INT *suboff /* = NULL */)
{
  tree ret;

  /* We found the field with exact match.  */
  if (type
      && useless_type_conversion_p (type, TREE_TYPE (ctor))
      && known_eq (poly_offset, 0U))
    return canonicalize_constructor_val (unshare_expr (ctor), from_decl);

  /* The remaining optimizations need a constant size and offset.  */
  unsigned HOST_WIDE_INT size, offset;
  if (!poly_size.is_constant (&size) || !poly_offset.is_constant (&offset))
    return NULL_TREE;

  /* We are at the end of walk, see if we can view convert the
     result.  */
  if (!AGGREGATE_TYPE_P (TREE_TYPE (ctor)) && !offset
      /* VIEW_CONVERT_EXPR is defined only for matching sizes.  */
      && known_eq (wi::to_poly_widest (TYPE_SIZE (type)), size)
      && known_eq (wi::to_poly_widest (TYPE_SIZE (TREE_TYPE (ctor))), size))
    {
      ret = canonicalize_constructor_val (unshare_expr (ctor), from_decl);
      if (ret)
	{
	  ret = fold_unary (VIEW_CONVERT_EXPR, type, ret);
	  if (ret)
	    STRIP_USELESS_TYPE_CONVERSION (ret);
	}
      return ret;
    }

  /* For constants and byte-aligned/sized reads, try to go through
     native_encode/interpret.  */
  if (CONSTANT_CLASS_P (ctor)
      && BITS_PER_UNIT == 8
      && offset % BITS_PER_UNIT == 0
      && offset / BITS_PER_UNIT <= INT_MAX
      && size % BITS_PER_UNIT == 0
      && size <= MAX_BITSIZE_MODE_ANY_MODE
      && can_native_interpret_type_p (type))
    {
      unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
      int len = native_encode_expr (ctor, buf, size / BITS_PER_UNIT,
				    offset / BITS_PER_UNIT);
      if (len > 0)
	return native_interpret_expr (type, buf, len);
    }

  /* For constructors, try first a recursive local processing, but in any case
     this requires the native storage order.  */
  if (TREE_CODE (ctor) == CONSTRUCTOR
      && !(AGGREGATE_TYPE_P (TREE_TYPE (ctor))
	   && TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (ctor))))
    {
      unsigned HOST_WIDE_INT dummy = 0;
      if (!suboff)
	suboff = &dummy;

      tree ret;
      if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (ctor)) == VECTOR_TYPE)
	ret = fold_array_ctor_reference (type, ctor, offset, size,
					 from_decl, suboff);
      else
	ret = fold_nonarray_ctor_reference (type, ctor, offset, size,
					    from_decl, suboff);

      /* Otherwise fall back to native_encode_initializer.  This may be done
	 only from the outermost fold_ctor_reference call (because it itself
	 recurses into CONSTRUCTORs and doesn't update suboff).  */
      if (ret == NULL_TREE
	  && suboff == &dummy
	  && BITS_PER_UNIT == 8
	  && offset % BITS_PER_UNIT == 0
	  && offset / BITS_PER_UNIT <= INT_MAX
	  && size % BITS_PER_UNIT == 0
	  && size <= MAX_BITSIZE_MODE_ANY_MODE
	  && can_native_interpret_type_p (type))
	{
	  unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
	  int len = native_encode_initializer (ctor, buf, size / BITS_PER_UNIT,
					       offset / BITS_PER_UNIT);
	  if (len > 0)
	    return native_interpret_expr (type, buf, len);
	}

      return ret;
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
  poly_int64 offset, size, max_size;
  tree tem;
  bool reverse;

  if (TREE_THIS_VOLATILE (t))
    return NULL_TREE;

  if (DECL_P (t))
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
	  && poly_int_tree_p (idx))
	{
	  tree low_bound, unit_size;

	  /* If the resulting bit-offset is constant, track it.  */
	  if ((low_bound = array_ref_low_bound (t),
	       poly_int_tree_p (low_bound))
	      && (unit_size = array_ref_element_size (t),
		  tree_fits_uhwi_p (unit_size)))
	    {
	      poly_offset_int woffset
		= wi::sext (wi::to_poly_offset (idx)
			    - wi::to_poly_offset (low_bound),
			    TYPE_PRECISION (sizetype));
	      woffset *= tree_to_uhwi (unit_size);
	      woffset *= BITS_PER_UNIT;
	      if (woffset.to_shwi (&offset))
		{
		  base = TREE_OPERAND (t, 0);
		  ctor = get_base_constructor (base, &offset, valueize);
		  /* Empty constructor.  Always fold to 0.  */
		  if (ctor == error_mark_node)
		    return build_zero_cst (TREE_TYPE (t));
		  /* Out of bound array access.  Value is undefined,
		     but don't fold.  */
		  if (maybe_lt (offset, 0))
		    return NULL_TREE;
		  /* We cannot determine ctor.  */
		  if (!ctor)
		    return NULL_TREE;
		  return fold_ctor_reference (TREE_TYPE (t), ctor, offset,
					      tree_to_uhwi (unit_size)
					      * BITS_PER_UNIT,
					      base);
		}
	    }
	}
      /* Fallthru.  */

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case TARGET_MEM_REF:
    case MEM_REF:
      base = get_ref_base_and_extent (t, &offset, &size, &max_size, &reverse);
      ctor = get_base_constructor (base, &offset, valueize);

      /* Empty constructor.  Always fold to 0.  */
      if (ctor == error_mark_node)
	return build_zero_cst (TREE_TYPE (t));
      /* We do not know precise address.  */
      if (!known_size_p (max_size) || maybe_ne (max_size, size))
	return NULL_TREE;
      /* We cannot determine ctor.  */
      if (!ctor)
	return NULL_TREE;

      /* Out of bound array access.  Value is undefined, but don't fold.  */
      if (maybe_lt (offset, 0))
	return NULL_TREE;

      tem = fold_ctor_reference (TREE_TYPE (t), ctor, offset, size, base);
      if (tem)
	return tem;

      /* For bit field reads try to read the representative and
	 adjust.  */
      if (TREE_CODE (t) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (t, 1))
	  && DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (t, 1)))
	{
	  HOST_WIDE_INT csize, coffset;
	  tree field = TREE_OPERAND (t, 1);
	  tree repr = DECL_BIT_FIELD_REPRESENTATIVE (field);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (repr))
	      && size.is_constant (&csize)
	      && offset.is_constant (&coffset)
	      && (coffset % BITS_PER_UNIT != 0
		  || csize % BITS_PER_UNIT != 0)
	      && !reverse
	      && BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN)
	    {
	      poly_int64 bitoffset;
	      poly_uint64 field_offset, repr_offset;
	      if (poly_int_tree_p (DECL_FIELD_OFFSET (field), &field_offset)
		  && poly_int_tree_p (DECL_FIELD_OFFSET (repr), &repr_offset))
		bitoffset = (field_offset - repr_offset) * BITS_PER_UNIT;
	      else
		bitoffset = 0;
	      bitoffset += (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field))
			    - tree_to_uhwi (DECL_FIELD_BIT_OFFSET (repr)));
	      HOST_WIDE_INT bitoff;
	      int diff = (TYPE_PRECISION (TREE_TYPE (repr))
			  - TYPE_PRECISION (TREE_TYPE (field)));
	      if (bitoffset.is_constant (&bitoff)
		  && bitoff >= 0
		  && bitoff <= diff)
		{
		  offset -= bitoff;
		  size = tree_to_uhwi (DECL_SIZE (repr));

		  tem = fold_ctor_reference (TREE_TYPE (repr), ctor, offset,
					     size, base);
		  if (tem && TREE_CODE (tem) == INTEGER_CST)
		    {
		      if (!BYTES_BIG_ENDIAN)
			tem = wide_int_to_tree (TREE_TYPE (field),
						wi::lrshift (wi::to_wide (tem),
							     bitoff));
		      else
			tem = wide_int_to_tree (TREE_TYPE (field),
						wi::lrshift (wi::to_wide (tem),
							     diff - bitoff));
		      return tem;
		    }
		}
	    }
	}
      break;

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

/* Lookup virtual method with index TOKEN in a virtual table V
   at OFFSET.  
   Set CAN_REFER if non-NULL to false if method
   is not referable or if the virtual table is ill-formed (such as rewriten
   by non-C++ produced symbol). Otherwise just return NULL in that calse.  */

tree
gimple_get_virt_method_for_vtable (HOST_WIDE_INT token,
				   tree v,
				   unsigned HOST_WIDE_INT offset,
				   bool *can_refer)
{
  tree vtable = v, init, fn;
  unsigned HOST_WIDE_INT size;
  unsigned HOST_WIDE_INT elt_size, access_index;
  tree domain_type;

  if (can_refer)
    *can_refer = true;

  /* First of all double check we have virtual table.  */
  if (!VAR_P (v) || !DECL_VIRTUAL_P (v))
    {
      /* Pass down that we lost track of the target.  */
      if (can_refer)
	*can_refer = false;
      return NULL_TREE;
    }

  init = ctor_for_folding (v);

  /* The virtual tables should always be born with constructors
     and we always should assume that they are avaialble for
     folding.  At the moment we do not stream them in all cases,
     but it should never happen that ctor seem unreachable.  */
  gcc_assert (init);
  if (init == error_mark_node)
    {
      /* Pass down that we lost track of the target.  */
      if (can_refer)
	*can_refer = false;
      return NULL_TREE;
    }
  gcc_checking_assert (TREE_CODE (TREE_TYPE (v)) == ARRAY_TYPE);
  size = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (TREE_TYPE (v))));
  offset *= BITS_PER_UNIT;
  offset += token * size;

  /* Lookup the value in the constructor that is assumed to be array.
     This is equivalent to
     fn = fold_ctor_reference (TREE_TYPE (TREE_TYPE (v)), init,
			       offset, size, NULL);
     but in a constant time.  We expect that frontend produced a simple
     array without indexed initializers.  */

  gcc_checking_assert (TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE);
  domain_type = TYPE_DOMAIN (TREE_TYPE (init));
  gcc_checking_assert (integer_zerop (TYPE_MIN_VALUE (domain_type)));
  elt_size = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (init))));

  access_index = offset / BITS_PER_UNIT / elt_size;
  gcc_checking_assert (offset % (elt_size * BITS_PER_UNIT) == 0);

  /* The C++ FE can now produce indexed fields, and we check if the indexes
     match.  */
  if (access_index < CONSTRUCTOR_NELTS (init))
    {
      fn = CONSTRUCTOR_ELT (init, access_index)->value;
      tree idx = CONSTRUCTOR_ELT (init, access_index)->index;
      gcc_checking_assert (!idx || tree_to_uhwi (idx) == access_index);
      STRIP_NOPS (fn);
    }
  else
    fn = NULL;

  /* For type inconsistent program we may end up looking up virtual method
     in virtual table that does not contain TOKEN entries.  We may overrun
     the virtual table and pick up a constant or RTTI info pointer.
     In any case the call is undefined.  */
  if (!fn
      || (TREE_CODE (fn) != ADDR_EXPR && TREE_CODE (fn) != FDESC_EXPR)
      || TREE_CODE (TREE_OPERAND (fn, 0)) != FUNCTION_DECL)
    fn = builtin_decl_unreachable ();
  else
    {
      fn = TREE_OPERAND (fn, 0);

      /* When cgraph node is missing and function is not public, we cannot
	 devirtualize.  This can happen in WHOPR when the actual method
	 ends up in other partition, because we found devirtualization
	 possibility too late.  */
      if (!can_refer_decl_in_current_unit_p (fn, vtable))
	{
	  if (can_refer)
	    {
	      *can_refer = false;
	      return fn;
	    }
	  return NULL_TREE;
	}
    }

  /* Make sure we create a cgraph node for functions we'll reference.
     They can be non-existent if the reference comes from an entry
     of an external vtable for example.  */
  cgraph_node::get_create (fn);

  return fn;
}

/* Return a declaration of a function which an OBJ_TYPE_REF references. TOKEN
   is integer form of OBJ_TYPE_REF_TOKEN of the reference expression.
   KNOWN_BINFO carries the binfo describing the true type of
   OBJ_TYPE_REF_OBJECT(REF).
   Set CAN_REFER if non-NULL to false if method
   is not referable or if the virtual table is ill-formed (such as rewriten
   by non-C++ produced symbol). Otherwise just return NULL in that calse.  */

tree
gimple_get_virt_method_for_binfo (HOST_WIDE_INT token, tree known_binfo,
				  bool *can_refer)
{
  unsigned HOST_WIDE_INT offset;
  tree v;

  v = BINFO_VTABLE (known_binfo);
  /* If there is no virtual methods table, leave the OBJ_TYPE_REF alone.  */
  if (!v)
    return NULL_TREE;

  if (!vtable_pointer_value_to_vtable (v, &v, &offset))
    {
      if (can_refer)
	*can_refer = false;
      return NULL_TREE;
    }
  return gimple_get_virt_method_for_vtable (token, v, offset, can_refer);
}

/* Given a pointer value T, return a simplified version of an
   indirection through T, or NULL_TREE if no simplification is
   possible.  Note that the resulting type may be different from
   the type pointed to in the sense that it is still compatible
   from the langhooks point of view. */

tree
gimple_fold_indirect_ref (tree t)
{
  tree ptype = TREE_TYPE (t), type = TREE_TYPE (ptype);
  tree sub = t;
  tree subtype;

  STRIP_NOPS (sub);
  subtype = TREE_TYPE (sub);
  if (!POINTER_TYPE_P (subtype)
      || TYPE_REF_CAN_ALIAS_ALL (ptype))
    return NULL_TREE;

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);
      /* *&p => p */
      if (useless_type_conversion_p (type, optype))
        return op;

      /* *(foo *)&fooarray => fooarray[0] */
      if (TREE_CODE (optype) == ARRAY_TYPE
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (optype))) == INTEGER_CST
	  && useless_type_conversion_p (type, TREE_TYPE (optype)))
       {
         tree type_domain = TYPE_DOMAIN (optype);
         tree min_val = size_zero_node;
         if (type_domain && TYPE_MIN_VALUE (type_domain))
           min_val = TYPE_MIN_VALUE (type_domain);
	 if (TREE_CODE (min_val) == INTEGER_CST)
	   return build4 (ARRAY_REF, type, op, min_val, NULL_TREE, NULL_TREE);
       }
      /* *(foo *)&complexfoo => __real__ complexfoo */
      else if (TREE_CODE (optype) == COMPLEX_TYPE
               && useless_type_conversion_p (type, TREE_TYPE (optype)))
        return fold_build1 (REALPART_EXPR, type, op);
      /* *(foo *)&vectorfoo => BIT_FIELD_REF<vectorfoo,...> */
      else if (TREE_CODE (optype) == VECTOR_TYPE
               && useless_type_conversion_p (type, TREE_TYPE (optype)))
        {
          tree part_width = TYPE_SIZE (type);
          tree index = bitsize_int (0);
          return fold_build3 (BIT_FIELD_REF, type, op, part_width, index);
        }
    }

  /* *(p + CST) -> ...  */
  if (TREE_CODE (sub) == POINTER_PLUS_EXPR
      && TREE_CODE (TREE_OPERAND (sub, 1)) == INTEGER_CST)
    {
      tree addr = TREE_OPERAND (sub, 0);
      tree off = TREE_OPERAND (sub, 1);
      tree addrtype;

      STRIP_NOPS (addr);
      addrtype = TREE_TYPE (addr);

      /* ((foo*)&vectorfoo)[1] -> BIT_FIELD_REF<vectorfoo,...> */
      if (TREE_CODE (addr) == ADDR_EXPR
	  && TREE_CODE (TREE_TYPE (addrtype)) == VECTOR_TYPE
	  && useless_type_conversion_p (type, TREE_TYPE (TREE_TYPE (addrtype)))
	  && tree_fits_uhwi_p (off))
	{
          unsigned HOST_WIDE_INT offset = tree_to_uhwi (off);
          tree part_width = TYPE_SIZE (type);
          unsigned HOST_WIDE_INT part_widthi
            = tree_to_shwi (part_width) / BITS_PER_UNIT;
          unsigned HOST_WIDE_INT indexi = offset * BITS_PER_UNIT;
          tree index = bitsize_int (indexi);
	  if (known_lt (offset / part_widthi,
			TYPE_VECTOR_SUBPARTS (TREE_TYPE (addrtype))))
            return fold_build3 (BIT_FIELD_REF, type, TREE_OPERAND (addr, 0),
                                part_width, index);
	}

      /* ((foo*)&complexfoo)[1] -> __imag__ complexfoo */
      if (TREE_CODE (addr) == ADDR_EXPR
	  && TREE_CODE (TREE_TYPE (addrtype)) == COMPLEX_TYPE
	  && useless_type_conversion_p (type, TREE_TYPE (TREE_TYPE (addrtype))))
        {
          tree size = TYPE_SIZE_UNIT (type);
          if (tree_int_cst_equal (size, off))
            return fold_build1 (IMAGPART_EXPR, type, TREE_OPERAND (addr, 0));
        }

      /* *(p + CST) -> MEM_REF <p, CST>.  */
      if (TREE_CODE (addr) != ADDR_EXPR
	  || DECL_P (TREE_OPERAND (addr, 0)))
	return fold_build2 (MEM_REF, type,
			    addr,
			    wide_int_to_tree (ptype, wi::to_wide (off)));
    }

  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
      && TREE_CODE (TYPE_SIZE (TREE_TYPE (TREE_TYPE (subtype)))) == INTEGER_CST
      && useless_type_conversion_p (type, TREE_TYPE (TREE_TYPE (subtype))))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      tree osub = sub;
      sub = gimple_fold_indirect_ref (sub);
      if (! sub)
	sub = build1 (INDIRECT_REF, TREE_TYPE (subtype), osub);
      type_domain = TYPE_DOMAIN (TREE_TYPE (sub));
      if (type_domain && TYPE_MIN_VALUE (type_domain))
        min_val = TYPE_MIN_VALUE (type_domain);
      if (TREE_CODE (min_val) == INTEGER_CST)
	return build4 (ARRAY_REF, type, sub, min_val, NULL_TREE, NULL_TREE);
    }

  return NULL_TREE;
}

/* Return true if CODE is an operation that when operating on signed
   integer types involves undefined behavior on overflow and the
   operation can be expressed with unsigned arithmetic.  */

bool
arith_code_with_undefined_signed_overflow (tree_code code)
{
  switch (code)
    {
    case ABS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case NEGATE_EXPR:
    case POINTER_PLUS_EXPR:
      return true;
    default:
      return false;
    }
}

/* Rewrite STMT, an assignment with a signed integer or pointer arithmetic
   operation that can be transformed to unsigned arithmetic by converting
   its operand, carrying out the operation in the corresponding unsigned
   type and converting the result back to the original type.

   If IN_PLACE is true, adjust the stmt in place and return NULL.
   Otherwise returns a sequence of statements that replace STMT and also
   contain a modified form of STMT itself.  */

gimple_seq
rewrite_to_defined_overflow (gimple *stmt, bool in_place /* = false */)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "rewriting stmt with undefined signed "
	       "overflow ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  tree lhs = gimple_assign_lhs (stmt);
  tree type = unsigned_type_for (TREE_TYPE (lhs));
  gimple_seq stmts = NULL;
  if (gimple_assign_rhs_code (stmt) == ABS_EXPR)
    gimple_assign_set_rhs_code (stmt, ABSU_EXPR);
  else
    for (unsigned i = 1; i < gimple_num_ops (stmt); ++i)
      {
	tree op = gimple_op (stmt, i);
	op = gimple_convert (&stmts, type, op);
	gimple_set_op (stmt, i, op);
      }
  gimple_assign_set_lhs (stmt, make_ssa_name (type, stmt));
  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    gimple_assign_set_rhs_code (stmt, PLUS_EXPR);
  gimple_set_modified (stmt, true);
  if (in_place)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      if (stmts)
	gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
      stmts = NULL;
    }
  else
    gimple_seq_add_stmt (&stmts, stmt);
  gimple *cvt = gimple_build_assign (lhs, NOP_EXPR, gimple_assign_lhs (stmt));
  if (in_place)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_insert_after (&gsi, cvt, GSI_SAME_STMT);
      update_stmt (stmt);
    }
  else
    gimple_seq_add_stmt (&stmts, cvt);

  return stmts;
}


/* The valueization hook we use for the gimple_build API simplification.
   This makes us match fold_buildN behavior by only combining with
   statements in the sequence(s) we are currently building.  */

static tree
gimple_build_valueize (tree op)
{
  if (gimple_bb (SSA_NAME_DEF_STMT (op)) == NULL)
    return op;
  return NULL_TREE;
}

/* Helper for gimple_build to perform the final insertion of stmts on SEQ.  */

static inline void
gimple_build_insert_seq (gimple_stmt_iterator *gsi,
			 bool before, gsi_iterator_update update,
			 gimple_seq seq)
{
  if (before)
    {
      if (gsi->bb)
	gsi_insert_seq_before (gsi, seq, update);
      else
	gsi_insert_seq_before_without_update (gsi, seq, update);
    }
  else
    {
      if (gsi->bb)
	gsi_insert_seq_after (gsi, seq, update);
      else
	gsi_insert_seq_after_without_update (gsi, seq, update);
    }
}

/* Build the expression CODE OP0 of type TYPE with location LOC,
   simplifying it first if possible.  Returns the built
   expression value and inserts statements possibly defining it
   before GSI if BEFORE is true or after GSI if false and advance
   the iterator accordingly.
   If gsi refers to a basic block simplifying is allowed to look
   at all SSA defs while when it does not it is restricted to
   SSA defs that are not associated with a basic block yet,
   indicating they belong to the currently building sequence.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, enum tree_code code, tree type, tree op0)
{
  gimple_seq seq = NULL;
  tree res
    = gimple_simplify (code, type, op0, &seq,
		       gsi->bb ? follow_all_ssa_edges : gimple_build_valueize);
  if (!res)
    {
      res = create_tmp_reg_or_ssa_name (type);
      gimple *stmt;
      if (code == REALPART_EXPR
	  || code == IMAGPART_EXPR
	  || code == VIEW_CONVERT_EXPR)
	stmt = gimple_build_assign (res, code, build1 (code, type, op0));
      else
	stmt = gimple_build_assign (res, code, op0);
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (&seq, stmt);
    }
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build the expression OP0 CODE OP1 of type TYPE with location LOC,
   simplifying it first if possible.  Returns the built
   expression value inserting any new statements at GSI honoring BEFORE
   and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, enum tree_code code, tree type,
	      tree op0, tree op1)
{
  gimple_seq seq = NULL;
  tree res
    = gimple_simplify (code, type, op0, op1, &seq,
		       gsi->bb ? follow_all_ssa_edges : gimple_build_valueize);
  if (!res)
    {
      res = create_tmp_reg_or_ssa_name (type);
      gimple *stmt = gimple_build_assign (res, code, op0, op1);
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (&seq, stmt);
    }
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build the expression (CODE OP0 OP1 OP2) of type TYPE with location LOC,
   simplifying it first if possible.  Returns the built
   expression value inserting any new statements at GSI honoring BEFORE
   and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, enum tree_code code, tree type,
	      tree op0, tree op1, tree op2)
{

  gimple_seq seq = NULL;
  tree res
    = gimple_simplify (code, type, op0, op1, op2, &seq,
		       gsi->bb ? follow_all_ssa_edges : gimple_build_valueize);
  if (!res)
    {
      res = create_tmp_reg_or_ssa_name (type);
      gimple *stmt;
      if (code == BIT_FIELD_REF)
	stmt = gimple_build_assign (res, code,
				    build3 (code, type, op0, op1, op2));
      else
	stmt = gimple_build_assign (res, code, op0, op1, op2);
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (&seq, stmt);
    }
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build the call FN () with a result of type TYPE (or no result if TYPE is
   void) with a location LOC.  Returns the built expression value (or NULL_TREE
   if TYPE is void) inserting any new statements at GSI honoring BEFORE
   and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, combined_fn fn, tree type)
{
  tree res = NULL_TREE;
  gimple_seq seq = NULL;
  gcall *stmt;
  if (internal_fn_p (fn))
    stmt = gimple_build_call_internal (as_internal_fn (fn), 0);
  else
    {
      tree decl = builtin_decl_implicit (as_builtin_fn (fn));
      stmt = gimple_build_call (decl, 0);
    }
  if (!VOID_TYPE_P (type))
    {
      res = create_tmp_reg_or_ssa_name (type);
      gimple_call_set_lhs (stmt, res);
    }
  gimple_set_location (stmt, loc);
  gimple_seq_add_stmt_without_update (&seq, stmt);
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build the call FN (ARG0) with a result of type TYPE
   (or no result if TYPE is void) with location LOC,
   simplifying it first if possible.  Returns the built
   expression value (or NULL_TREE if TYPE is void) inserting any new
   statements at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, combined_fn fn,
	      tree type, tree arg0)
{
  gimple_seq seq = NULL;
  tree res = gimple_simplify (fn, type, arg0, &seq, gimple_build_valueize);
  if (!res)
    {
      gcall *stmt;
      if (internal_fn_p (fn))
	stmt = gimple_build_call_internal (as_internal_fn (fn), 1, arg0);
      else
	{
	  tree decl = builtin_decl_implicit (as_builtin_fn (fn));
	  stmt = gimple_build_call (decl, 1, arg0);
	}
      if (!VOID_TYPE_P (type))
	{
	  res = create_tmp_reg_or_ssa_name (type);
	  gimple_call_set_lhs (stmt, res);
	}
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (&seq, stmt);
    }
  gimple_build_insert_seq  (gsi, before, update, seq);
  return res;
}

/* Build the call FN (ARG0, ARG1) with a result of type TYPE
   (or no result if TYPE is void) with location LOC,
   simplifying it first if possible.  Returns the built
   expression value (or NULL_TREE if TYPE is void) inserting any new
   statements at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, combined_fn fn,
	      tree type, tree arg0, tree arg1)
{
  gimple_seq seq = NULL;
  tree res = gimple_simplify (fn, type, arg0, arg1, &seq,
			      gimple_build_valueize);
  if (!res)
    {
      gcall *stmt;
      if (internal_fn_p (fn))
	stmt = gimple_build_call_internal (as_internal_fn (fn), 2, arg0, arg1);
      else
	{
	  tree decl = builtin_decl_implicit (as_builtin_fn (fn));
	  stmt = gimple_build_call (decl, 2, arg0, arg1);
	}
      if (!VOID_TYPE_P (type))
	{
	  res = create_tmp_reg_or_ssa_name (type);
	  gimple_call_set_lhs (stmt, res);
	}
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (&seq, stmt);
    }
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build the call FN (ARG0, ARG1, ARG2) with a result of type TYPE
   (or no result if TYPE is void) with location LOC,
   simplifying it first if possible.  Returns the built
   expression value (or NULL_TREE if TYPE is void) inserting any new
   statements at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, combined_fn fn,
	      tree type, tree arg0, tree arg1, tree arg2)
{
  gimple_seq seq = NULL;
  tree res = gimple_simplify (fn, type, arg0, arg1, arg2,
			      &seq, gimple_build_valueize);
  if (!res)
    {
      gcall *stmt;
      if (internal_fn_p (fn))
	stmt = gimple_build_call_internal (as_internal_fn (fn),
					   3, arg0, arg1, arg2);
      else
	{
	  tree decl = builtin_decl_implicit (as_builtin_fn (fn));
	  stmt = gimple_build_call (decl, 3, arg0, arg1, arg2);
	}
      if (!VOID_TYPE_P (type))
	{
	  res = create_tmp_reg_or_ssa_name (type);
	  gimple_call_set_lhs (stmt, res);
	}
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (&seq, stmt);
    }
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build CODE (OP0) with a result of type TYPE (or no result if TYPE is
   void) with location LOC, simplifying it first if possible.  Returns the
   built expression value (or NULL_TREE if TYPE is void) inserting any new
   statements at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, code_helper code, tree type, tree op0)
{
  if (code.is_tree_code ())
    return gimple_build (gsi, before, update, loc, tree_code (code), type, op0);
  return gimple_build (gsi, before, update, loc, combined_fn (code), type, op0);
}

/* Build CODE (OP0, OP1) with a result of type TYPE (or no result if TYPE is
   void) with location LOC, simplifying it first if possible.  Returns the
   built expression value (or NULL_TREE if TYPE is void) inserting any new
   statements at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, code_helper code, tree type, tree op0, tree op1)
{
  if (code.is_tree_code ())
    return gimple_build (gsi, before, update,
			 loc, tree_code (code), type, op0, op1);
  return gimple_build (gsi, before, update,
		       loc, combined_fn (code), type, op0, op1);
}

/* Build CODE (OP0, OP1, OP2) with a result of type TYPE (or no result if TYPE
   is void) with location LOC, simplifying it first if possible.  Returns the
   built expression value (or NULL_TREE if TYPE is void) inserting any new
   statements at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build (gimple_stmt_iterator *gsi,
	      bool before, gsi_iterator_update update,
	      location_t loc, code_helper code,
	      tree type, tree op0, tree op1, tree op2)
{
  if (code.is_tree_code ())
    return gimple_build (gsi, before, update,
			 loc, tree_code (code), type, op0, op1, op2);
  return gimple_build (gsi, before, update,
		       loc, combined_fn (code), type, op0, op1, op2);
}

/* Build the conversion (TYPE) OP with a result of type TYPE
   with location LOC if such conversion is neccesary in GIMPLE,
   simplifying it first.
   Returns the built expression inserting any new statements
   at GSI honoring BEFORE and UPDATE.  */

tree
gimple_convert (gimple_stmt_iterator *gsi,
		bool before, gsi_iterator_update update,
		location_t loc, tree type, tree op)
{
  if (useless_type_conversion_p (type, TREE_TYPE (op)))
    return op;
  return gimple_build (gsi, before, update, loc, NOP_EXPR, type, op);
}

/* Build the conversion (ptrofftype) OP with a result of a type
   compatible with ptrofftype with location LOC if such conversion
   is neccesary in GIMPLE, simplifying it first.
   Returns the built expression value inserting any new statements
   at GSI honoring BEFORE and UPDATE.  */

tree
gimple_convert_to_ptrofftype (gimple_stmt_iterator *gsi,
			      bool before, gsi_iterator_update update,
			      location_t loc, tree op)
{
  if (ptrofftype_p (TREE_TYPE (op)))
    return op;
  return gimple_convert (gsi, before, update, loc, sizetype, op);
}

/* Build a vector of type TYPE in which each element has the value OP.
   Return a gimple value for the result, inserting any new statements
   at GSI honoring BEFORE and UPDATE.  */

tree
gimple_build_vector_from_val (gimple_stmt_iterator *gsi,
			      bool before, gsi_iterator_update update,
			      location_t loc, tree type, tree op)
{
  if (!TYPE_VECTOR_SUBPARTS (type).is_constant ()
      && !CONSTANT_CLASS_P (op))
    return gimple_build (gsi, before, update,
			 loc, VEC_DUPLICATE_EXPR, type, op);

  tree res, vec = build_vector_from_val (type, op);
  if (is_gimple_val (vec))
    return vec;
  if (gimple_in_ssa_p (cfun))
    res = make_ssa_name (type);
  else
    res = create_tmp_reg (type);
  gimple_seq seq = NULL;
  gimple *stmt = gimple_build_assign (res, vec);
  gimple_set_location (stmt, loc);
  gimple_seq_add_stmt_without_update (&seq, stmt);
  gimple_build_insert_seq (gsi, before, update, seq);
  return res;
}

/* Build a vector from BUILDER, handling the case in which some elements
   are non-constant.  Return a gimple value for the result, inserting
   any new instructions to GSI honoring BEFORE and UPDATE.

   BUILDER must not have a stepped encoding on entry.  This is because
   the function is not geared up to handle the arithmetic that would
   be needed in the variable case, and any code building a vector that
   is known to be constant should use BUILDER->build () directly.  */

tree
gimple_build_vector (gimple_stmt_iterator *gsi,
		     bool before, gsi_iterator_update update,
		     location_t loc, tree_vector_builder *builder)
{
  gcc_assert (builder->nelts_per_pattern () <= 2);
  unsigned int encoded_nelts = builder->encoded_nelts ();
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    if (!CONSTANT_CLASS_P ((*builder)[i]))
      {
	gimple_seq seq = NULL;
	tree type = builder->type ();
	unsigned int nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
	vec<constructor_elt, va_gc> *v;
	vec_alloc (v, nelts);
	for (i = 0; i < nelts; ++i)
	  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, builder->elt (i));

	tree res;
	if (gimple_in_ssa_p (cfun))
	  res = make_ssa_name (type);
	else
	  res = create_tmp_reg (type);
	gimple *stmt = gimple_build_assign (res, build_constructor (type, v));
	gimple_set_location (stmt, loc);
	gimple_seq_add_stmt_without_update (&seq, stmt);
	gimple_build_insert_seq (gsi, before, update, seq);
	return res;
      }
  return builder->build ();
}

/* Emit gimple statements into &stmts that take a value given in OLD_SIZE
   and generate a value guaranteed to be rounded upwards to ALIGN.

   Return the tree node representing this size, it is of TREE_TYPE TYPE.  */

tree
gimple_build_round_up (gimple_stmt_iterator *gsi,
		       bool before, gsi_iterator_update update,
		       location_t loc, tree type,
		       tree old_size, unsigned HOST_WIDE_INT align)
{
  unsigned HOST_WIDE_INT tg_mask = align - 1;
  /* tree new_size = (old_size + tg_mask) & ~tg_mask;  */
  gcc_assert (INTEGRAL_TYPE_P (type));
  tree tree_mask = build_int_cst (type, tg_mask);
  tree oversize = gimple_build (gsi, before, update,
				loc, PLUS_EXPR, type, old_size, tree_mask);

  tree mask = build_int_cst (type, -align);
  return gimple_build (gsi, before, update,
		       loc, BIT_AND_EXPR, type, oversize, mask);
}

/* Return true if the result of assignment STMT is known to be non-negative.
   If the return value is based on the assumption that signed overflow is
   undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

static bool
gimple_assign_nonnegative_warnv_p (gimple *stmt, bool *strict_overflow_p,
				   int depth)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  switch (get_gimple_rhs_class (code))
    {
    case GIMPLE_UNARY_RHS:
      return tree_unary_nonnegative_warnv_p (gimple_assign_rhs_code (stmt),
					     type,
					     gimple_assign_rhs1 (stmt),
					     strict_overflow_p, depth);
    case GIMPLE_BINARY_RHS:
      return tree_binary_nonnegative_warnv_p (gimple_assign_rhs_code (stmt),
					      type,
					      gimple_assign_rhs1 (stmt),
					      gimple_assign_rhs2 (stmt),
					      strict_overflow_p, depth);
    case GIMPLE_TERNARY_RHS:
      return false;
    case GIMPLE_SINGLE_RHS:
      return tree_single_nonnegative_warnv_p (gimple_assign_rhs1 (stmt),
					      strict_overflow_p, depth);
    case GIMPLE_INVALID_RHS:
      break;
    }
  gcc_unreachable ();
}

/* Return true if return value of call STMT is known to be non-negative.
   If the return value is based on the assumption that signed overflow is
   undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

static bool
gimple_call_nonnegative_warnv_p (gimple *stmt, bool *strict_overflow_p,
				 int depth)
{
  tree arg0 = gimple_call_num_args (stmt) > 0 ?
    gimple_call_arg (stmt, 0) : NULL_TREE;
  tree arg1 = gimple_call_num_args (stmt) > 1 ?
    gimple_call_arg (stmt, 1) : NULL_TREE;
  tree lhs = gimple_call_lhs (stmt);
  return (lhs
	  && tree_call_nonnegative_warnv_p (TREE_TYPE (lhs),
					    gimple_call_combined_fn (stmt),
					    arg0, arg1,
					    strict_overflow_p, depth));
}

/* Return true if return value of call STMT is known to be non-negative.
   If the return value is based on the assumption that signed overflow is
   undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

static bool
gimple_phi_nonnegative_warnv_p (gimple *stmt, bool *strict_overflow_p,
				int depth)
{
  for (unsigned i = 0; i < gimple_phi_num_args (stmt); ++i)
    {
      tree arg = gimple_phi_arg_def (stmt, i);
      if (!tree_single_nonnegative_warnv_p (arg, strict_overflow_p, depth + 1))
	return false;
    }
  return true;
}

/* Return true if STMT is known to compute a non-negative value.
   If the return value is based on the assumption that signed overflow is
   undefined, set *STRICT_OVERFLOW_P to true; otherwise, don't change
   *STRICT_OVERFLOW_P.  DEPTH is the current nesting depth of the query.  */

bool
gimple_stmt_nonnegative_warnv_p (gimple *stmt, bool *strict_overflow_p,
				 int depth)
{
  tree type = gimple_range_type (stmt);
  if (type && frange::supports_p (type))
    {
      frange r;
      bool sign;
      if (get_global_range_query ()->range_of_stmt (r, stmt)
	  && r.signbit_p (sign))
	return !sign;
    }
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      return gimple_assign_nonnegative_warnv_p (stmt, strict_overflow_p,
						depth);
    case GIMPLE_CALL:
      return gimple_call_nonnegative_warnv_p (stmt, strict_overflow_p,
					      depth);
    case GIMPLE_PHI:
      return gimple_phi_nonnegative_warnv_p (stmt, strict_overflow_p,
					     depth);
    default:
      return false;
    }
}

/* Return true if the floating-point value computed by assignment STMT
   is known to have an integer value.  We also allow +Inf, -Inf and NaN
   to be considered integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

static bool
gimple_assign_integer_valued_real_p (gimple *stmt, int depth)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  switch (get_gimple_rhs_class (code))
    {
    case GIMPLE_UNARY_RHS:
      return integer_valued_real_unary_p (gimple_assign_rhs_code (stmt),
					  gimple_assign_rhs1 (stmt), depth);
    case GIMPLE_BINARY_RHS:
      return integer_valued_real_binary_p (gimple_assign_rhs_code (stmt),
					   gimple_assign_rhs1 (stmt),
					   gimple_assign_rhs2 (stmt), depth);
    case GIMPLE_TERNARY_RHS:
      return false;
    case GIMPLE_SINGLE_RHS:
      return integer_valued_real_single_p (gimple_assign_rhs1 (stmt), depth);
    case GIMPLE_INVALID_RHS:
      break;
    }
  gcc_unreachable ();
}

/* Return true if the floating-point value computed by call STMT is known
   to have an integer value.  We also allow +Inf, -Inf and NaN to be
   considered integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

static bool
gimple_call_integer_valued_real_p (gimple *stmt, int depth)
{
  tree arg0 = (gimple_call_num_args (stmt) > 0
	       ? gimple_call_arg (stmt, 0)
	       : NULL_TREE);
  tree arg1 = (gimple_call_num_args (stmt) > 1
	       ? gimple_call_arg (stmt, 1)
	       : NULL_TREE);
  return integer_valued_real_call_p (gimple_call_combined_fn (stmt),
				     arg0, arg1, depth);
}

/* Return true if the floating-point result of phi STMT is known to have
   an integer value.  We also allow +Inf, -Inf and NaN to be considered
   integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

static bool
gimple_phi_integer_valued_real_p (gimple *stmt, int depth)
{
  for (unsigned i = 0; i < gimple_phi_num_args (stmt); ++i)
    {
      tree arg = gimple_phi_arg_def (stmt, i);
      if (!integer_valued_real_single_p (arg, depth + 1))
	return false;
    }
  return true;
}

/* Return true if the floating-point value computed by STMT is known
   to have an integer value.  We also allow +Inf, -Inf and NaN to be
   considered integer values. Return false for signaling NaN.

   DEPTH is the current nesting depth of the query.  */

bool
gimple_stmt_integer_valued_real_p (gimple *stmt, int depth)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      return gimple_assign_integer_valued_real_p (stmt, depth);
    case GIMPLE_CALL:
      return gimple_call_integer_valued_real_p (stmt, depth);
    case GIMPLE_PHI:
      return gimple_phi_integer_valued_real_p (stmt, depth);
    default:
      return false;
    }
}
