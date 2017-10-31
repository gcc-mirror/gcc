/* Statement simplification on GIMPLE.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.
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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "stmt.h"
#include "expr.h"
#include "stor-layout.h"
#include "dumpfile.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
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
#include "ipa-chkp.h"
#include "tree-cfg.h"
#include "fold-const-call.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"

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

  /* Static objects can be referred only if they was not optimized out yet.  */
  if (!TREE_PUBLIC (decl) && !DECL_EXTERNAL (decl))
    {
      /* Before we start optimizing unreachable code we can be sure all
	 static objects are defined.  */
      if (symtab->function_flags_ready)
	return true;
      snode = symtab_node::get (decl);
      if (!snode || !snode->definition)
	return false;
      node = dyn_cast <cgraph_node *> (snode);
      return !node || !node->global.inlined_to;
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
  return !node || !node->global.inlined_to;
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
	TREE_ADDRESSABLE (base) = 1;
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
  if (TREE_OVERFLOW_P (cval))
    return drop_tree_overflow (cval);
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
	  if (val && is_gimple_min_invariant (val))
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



/* Subroutine of fold_stmt.  We perform several simplifications of the
   memory reference tree EXPR and make sure to re-gimplify them properly
   after propagation of constant addresses.  IS_LHS is true if the
   reference is supposed to be an lvalue.  */

static tree
maybe_fold_reference (tree expr, bool is_lhs)
{
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

  if (!is_lhs
      && (result = fold_const_aggregate_ref (expr))
      && is_gimple_min_invariant (result))
    return result;

  return NULL_TREE;
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
	  return maybe_fold_reference (rhs, false);

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
			location_t loc = gimple_location_safe (stmt);
			dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
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
		      /* We can not use __builtin_unreachable here because it
			 can not have address taken.  */
		      val = build_int_cst (TREE_TYPE (val), 0);
		    return val;
		  }
	      }
	  }

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

	    if (result)
	      {
		/* Strip away useless type conversions.  Both the
		   NON_LVALUE_EXPR that may have been added by fold, and
		   "useless" type conversions that might now be apparent
		   due to propagation.  */
		STRIP_USELESS_TYPE_CONVERSION (result);

		if (result != rhs && valid_gimple_rhs_p (result))
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

	else if (DECL_P (rhs))
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

static void
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

  push_gimplify_context (gimple_in_ssa_p (cfun));

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

  pop_gimplify_context (NULL);

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
  if (gimple_vdef (stmt)
      && TREE_CODE (gimple_vdef (stmt)) == SSA_NAME)
    {
      gimple_set_vdef (repl, gimple_vdef (stmt));
      SSA_NAME_DEF_STMT (gimple_vdef (repl)) = repl;
    }
  if (gimple_vuse (stmt))
    gimple_set_vuse (repl, gimple_vuse (stmt));
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
  return SSA_VAR_P (inner);
}

/* If the SIZE argument representing the size of an object is in a range
   of values of which exactly one is valid (and that is zero), return
   true, otherwise false.  */

static bool
size_must_be_zero_p (tree size)
{
  if (integer_zerop (size))
    return true;

  if (TREE_CODE (size) != SSA_NAME)
    return false;

  wide_int min, max;
  enum value_range_type rtype = get_range_info (size, &min, &max);
  if (rtype != VR_ANTI_RANGE)
    return false;

  tree type = TREE_TYPE (size);
  int prec = TYPE_PRECISION (type);

  wide_int wone = wi::one (prec);

  /* Compute the value of SSIZE_MAX, the largest positive value that
     can be stored in ssize_t, the signed counterpart of size_t.  */
  wide_int ssize_max = wi::lshift (wi::one (prec), prec - 1) - 1;

  return wi::eq_p (min, wone) && wi::geu_p (max, ssize_max);
}

/* Fold function call to builtin mem{{,p}cpy,move}.  Return
   false if no simplification can be made.
   If ENDP is 0, return DEST (like memcpy).
   If ENDP is 1, return DEST+LEN (like mempcpy).
   If ENDP is 2, return DEST+LEN-1 (like stpcpy).
   If ENDP is 3, return DEST, additionally *SRC and *DEST may overlap
   (memmove).   */

static bool
gimple_fold_builtin_memory_op (gimple_stmt_iterator *gsi,
			       tree dest, tree src, int endp)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);
  tree len = gimple_call_arg (stmt, 2);
  tree destvar, srcvar;
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
      tree srctype, desttype;
      unsigned int src_align, dest_align;
      tree off0;

      /* Inlining of memcpy/memmove may cause bounds lost (if we copy
	 pointers as wide integer) and also may result in huge function
	 size because of inlined bounds copy.  Thus don't inline for
	 functions we want to instrument.  */
      if (flag_check_pointer_bounds
	  && chkp_instrumentable_p (cfun->decl)
	  /* Even if data may contain pointers we can inline if copy
	     less than a pointer size.  */
	  && (!tree_fits_uhwi_p (len)
	      || compare_tree_int (len, POINTER_SIZE_UNITS) >= 0))
	return false;

      /* Build accesses at offset zero with a ref-all character type.  */
      off0 = build_int_cst (build_pointer_type_for_mode (char_type_node,
							 ptr_mode, true), 0);

      /* If we can perform the copy efficiently with first doing all loads
         and then all stores inline it that way.  Currently efficiently
	 means that we can load all the memory into a single integer
	 register which is what MOVE_MAX gives us.  */
      src_align = get_pointer_alignment (src);
      dest_align = get_pointer_alignment (dest);
      if (tree_fits_uhwi_p (len)
	  && compare_tree_int (len, MOVE_MAX) <= 0
	  /* ???  Don't transform copies from strings with known length this
	     confuses the tree-ssa-strlen.c.  This doesn't handle
	     the case in gcc.dg/strlenopt-8.c which is XFAILed for that
	     reason.  */
	  && !c_strlen (src, 2))
	{
	  unsigned ilen = tree_to_uhwi (len);
	  if (pow2p_hwi (ilen))
	    {
	      scalar_int_mode mode;
	      tree type = lang_hooks.types.type_for_size (ilen * 8, 1);
	      if (type
		  && is_a <scalar_int_mode> (TYPE_MODE (type), &mode)
		  && GET_MODE_SIZE (mode) * BITS_PER_UNIT == ilen * 8
		  /* If the destination pointer is not aligned we must be able
		     to emit an unaligned store.  */
		  && (dest_align >= GET_MODE_ALIGNMENT (mode)
		      || !targetm.slow_unaligned_access (mode, dest_align)
		      || (optab_handler (movmisalign_optab, mode)
			  != CODE_FOR_nothing)))
		{
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
			  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
			}
		      if (dest_align < GET_MODE_ALIGNMENT (mode))
			desttype = build_aligned_type (type, dest_align);
		      new_stmt
			= gimple_build_assign (fold_build2 (MEM_REF, desttype,
							    dest, off0),
					       srcmem);
		      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
		      gimple_set_vdef (new_stmt, gimple_vdef (stmt));
		      if (gimple_vdef (new_stmt)
			  && TREE_CODE (gimple_vdef (new_stmt)) == SSA_NAME)
			SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;
		      if (!lhs)
			{
			  gsi_replace (gsi, new_stmt, false);
			  return true;
			}
		      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
		      goto done;
		    }
		}
	    }
	}

      if (endp == 3)
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
	      HOST_WIDE_INT src_offset = 0, dest_offset = 0;
	      HOST_WIDE_INT maxsize;

	      srcvar = TREE_OPERAND (src, 0);
	      src_base = get_addr_base_and_unit_offset (srcvar, &src_offset);
	      if (src_base == NULL)
		src_base = srcvar;
	      destvar = TREE_OPERAND (dest, 0);
	      dest_base = get_addr_base_and_unit_offset (destvar,
							 &dest_offset);
	      if (dest_base == NULL)
		dest_base = destvar;
	      if (tree_fits_uhwi_p (len))
		maxsize = tree_to_uhwi (len);
	      else
		maxsize = -1;
	      if (SSA_VAR_P (src_base)
		  && SSA_VAR_P (dest_base))
		{
		  if (operand_equal_p (src_base, dest_base, 0)
		      && ranges_overlap_p (src_offset, maxsize,
					   dest_offset, maxsize))
		    return false;
		}
	      else if (TREE_CODE (src_base) == MEM_REF
		       && TREE_CODE (dest_base) == MEM_REF)
		{
		  if (! operand_equal_p (TREE_OPERAND (src_base, 0),
					 TREE_OPERAND (dest_base, 0), 0))
		    return false;
		  offset_int off = mem_ref_offset (src_base) + src_offset;
		  if (!wi::fits_shwi_p (off))
		    return false;
		  src_offset = off.to_shwi ();

		  off = mem_ref_offset (dest_base) + dest_offset;
		  if (!wi::fits_shwi_p (off))
		    return false;
		  dest_offset = off.to_shwi ();
		  if (ranges_overlap_p (src_offset, maxsize,
					dest_offset, maxsize))
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
      /* FIXME:
         This logic lose for arguments like (type *)malloc (sizeof (type)),
         since we strip the casts of up to VOID return value from malloc.
	 Perhaps we ought to inherit type from non-VOID argument here?  */
      STRIP_NOPS (src);
      STRIP_NOPS (dest);
      if (!POINTER_TYPE_P (TREE_TYPE (src))
	  || !POINTER_TYPE_P (TREE_TYPE (dest)))
	return false;
      /* In the following try to find a type that is most natural to be
	 used for the memcpy source and destination and that allows
	 the most optimization when memcpy is turned into a plain assignment
	 using that type.  In theory we could always use a char[len] type
	 but that only gains us that the destination and source possibly
	 no longer will have their address taken.  */
      /* As we fold (void *)(p + CST) to (void *)p + CST undo this here.  */
      if (TREE_CODE (src) == POINTER_PLUS_EXPR)
	{
	  tree tem = TREE_OPERAND (src, 0);
	  STRIP_NOPS (tem);
	  if (tem != TREE_OPERAND (src, 0))
	    src = build1 (NOP_EXPR, TREE_TYPE (tem), src);
	}
      if (TREE_CODE (dest) == POINTER_PLUS_EXPR)
	{
	  tree tem = TREE_OPERAND (dest, 0);
	  STRIP_NOPS (tem);
	  if (tem != TREE_OPERAND (dest, 0))
	    dest = build1 (NOP_EXPR, TREE_TYPE (tem), dest);
	}
      srctype = TREE_TYPE (TREE_TYPE (src));
      if (TREE_CODE (srctype) == ARRAY_TYPE
	  && !tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len))
	{
	  srctype = TREE_TYPE (srctype);
	  STRIP_NOPS (src);
	  src = build1 (NOP_EXPR, build_pointer_type (srctype), src);
	}
      desttype = TREE_TYPE (TREE_TYPE (dest));
      if (TREE_CODE (desttype) == ARRAY_TYPE
	  && !tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len))
	{
	  desttype = TREE_TYPE (desttype);
	  STRIP_NOPS (dest);
	  dest = build1 (NOP_EXPR, build_pointer_type (desttype), dest);
	}
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
      if (dest_align < TYPE_ALIGN (desttype)
	  || src_align < TYPE_ALIGN (srctype))
	return false;

      destvar = dest;
      STRIP_NOPS (destvar);
      if (TREE_CODE (destvar) == ADDR_EXPR
	  && var_decl_component_p (TREE_OPERAND (destvar, 0))
	  && tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len))
	destvar = fold_build2 (MEM_REF, desttype, destvar, off0);
      else
	destvar = NULL_TREE;

      srcvar = src;
      STRIP_NOPS (srcvar);
      if (TREE_CODE (srcvar) == ADDR_EXPR
	  && var_decl_component_p (TREE_OPERAND (srcvar, 0))
	  && tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len))
	{
	  if (!destvar
	      || src_align >= TYPE_ALIGN (desttype))
	    srcvar = fold_build2 (MEM_REF, destvar ? desttype : srctype,
				  srcvar, off0);
	  else if (!STRICT_ALIGNMENT)
	    {
	      srctype = build_aligned_type (TYPE_MAIN_VARIANT (desttype),
					    src_align);
	      srcvar = fold_build2 (MEM_REF, srctype, srcvar, off0);
	    }
	  else
	    srcvar = NULL_TREE;
	}
      else
	srcvar = NULL_TREE;

      if (srcvar == NULL_TREE && destvar == NULL_TREE)
	return false;

      if (srcvar == NULL_TREE)
	{
	  STRIP_NOPS (src);
	  if (src_align >= TYPE_ALIGN (desttype))
	    srcvar = fold_build2 (MEM_REF, desttype, src, off0);
	  else
	    {
	      if (STRICT_ALIGNMENT)
		return false;
	      srctype = build_aligned_type (TYPE_MAIN_VARIANT (desttype),
					    src_align);
	      srcvar = fold_build2 (MEM_REF, srctype, src, off0);
	    }
	}
      else if (destvar == NULL_TREE)
	{
	  STRIP_NOPS (dest);
	  if (dest_align >= TYPE_ALIGN (srctype))
	    destvar = fold_build2 (MEM_REF, srctype, dest, off0);
	  else
	    {
	      if (STRICT_ALIGNMENT)
		return false;
	      desttype = build_aligned_type (TYPE_MAIN_VARIANT (srctype),
					     dest_align);
	      destvar = fold_build2 (MEM_REF, desttype, dest, off0);
	    }
	}

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
	      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
	    }
	}
      new_stmt = gimple_build_assign (destvar, srcvar);
      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
      gimple_set_vdef (new_stmt, gimple_vdef (stmt));
      if (gimple_vdef (new_stmt)
	  && TREE_CODE (gimple_vdef (new_stmt)) == SSA_NAME)
	SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;
      if (!lhs)
	{
	  gsi_replace (gsi, new_stmt, false);
	  return true;
	}
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
    }

done:
  gimple_seq stmts = NULL;
  if (endp == 0 || endp == 3)
    len = NULL_TREE;
  else if (endp == 2)
    len = gimple_build (&stmts, loc, MINUS_EXPR, TREE_TYPE (len), len,
			ssize_int (1));
  if (endp == 2 || endp == 1)
    {
      len = gimple_convert_to_ptrofftype (&stmts, loc, len);
      dest = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
			   TREE_TYPE (dest), dest, len);
    }

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
      || get_pointer_alignment (dest) / BITS_PER_UNIT < length)
    return NULL_TREE;

  if (length > HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT)
    return NULL_TREE;

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
  gimple_set_vuse (store, gimple_vuse (stmt));
  tree vdef = gimple_vdef (stmt);
  if (vdef && TREE_CODE (vdef) == SSA_NAME)
    {
      gimple_set_vdef (store, gimple_vdef (stmt));
      SSA_NAME_DEF_STMT (gimple_vdef (stmt)) = store;
    }
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


/* Obtain the minimum and maximum string length or minimum and maximum
   value of ARG in LENGTH[0] and LENGTH[1], respectively.
   If ARG is an SSA name variable, follow its use-def chains.  When
   TYPE == 0, if LENGTH[1] is not equal to the length we determine or
   if we are unable to determine the length or value, return False.
   VISITED is a bitmap of visited variables.
   TYPE is 0 if string length should be obtained, 1 for maximum string
   length and 2 for maximum value ARG can have.
   When FUZZY is set and the length of a string cannot be determined,
   the function instead considers as the maximum possible length the
   size of a character array it may refer to.
   Set *FLEXP to true if the range of the string lengths has been
   obtained from the upper bound of an array at the end of a struct.
   Such an array may hold a string that's longer than its upper bound
   due to it being used as a poor-man's flexible array member.  */

static bool
get_range_strlen (tree arg, tree length[2], bitmap *visited, int type,
		  bool fuzzy, bool *flexp)
{
  tree var, val;
  gimple *def_stmt;

  /* The minimum and maximum length.  The MAXLEN pointer stays unchanged
     but MINLEN may be cleared during the execution of the function.  */
  tree *minlen = length;
  tree *const maxlen = length + 1;

  if (TREE_CODE (arg) != SSA_NAME)
    {
      /* We can end up with &(*iftmp_1)[0] here as well, so handle it.  */
      if (TREE_CODE (arg) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (arg, 0)) == ARRAY_REF
	  && integer_zerop (TREE_OPERAND (TREE_OPERAND (arg, 0), 1)))
	{
	  tree aop0 = TREE_OPERAND (TREE_OPERAND (arg, 0), 0);
	  if (TREE_CODE (aop0) == INDIRECT_REF
	      && TREE_CODE (TREE_OPERAND (aop0, 0)) == SSA_NAME)
	    return get_range_strlen (TREE_OPERAND (aop0, 0),
				     length, visited, type, fuzzy, flexp);
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

      if (!val && fuzzy)
	{
	  if (TREE_CODE (arg) == ADDR_EXPR)
	    return get_range_strlen (TREE_OPERAND (arg, 0), length,
				     visited, type, fuzzy, flexp);

	  if (TREE_CODE (arg) == COMPONENT_REF
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 1))) == ARRAY_TYPE)
	    {
	      /* Use the type of the member array to determine the upper
		 bound on the length of the array.  This may be overly
		 optimistic if the array itself isn't NUL-terminated and
		 the caller relies on the subsequent member to contain
		 the NUL.
		 Set *FLEXP to true if the array whose bound is being
		 used is at the end of a struct.  */
	      if (array_at_struct_end_p (arg))
		*flexp = true;

	      arg = TREE_OPERAND (arg, 1);
	      val = TYPE_SIZE_UNIT (TREE_TYPE (arg));
	      if (!val || integer_zerop (val))
		return false;
	      val = fold_build2 (MINUS_EXPR, TREE_TYPE (val), val,
				 integer_one_node);
	      /* Set the minimum size to zero since the string in
		 the array could have zero length.  */
	      *minlen = ssize_int (0);
	    }
	}

      if (!val)
	return false;

      if (minlen
	  && (!*minlen
	      || (type > 0
		  && TREE_CODE (*minlen) == INTEGER_CST
		  && TREE_CODE (val) == INTEGER_CST
		  && tree_int_cst_lt (val, *minlen))))
	*minlen = val;

      if (*maxlen)
	{
	  if (type > 0)
	    {
	      if (TREE_CODE (*maxlen) != INTEGER_CST
		  || TREE_CODE (val) != INTEGER_CST)
		return false;

	      if (tree_int_cst_lt (*maxlen, val))
		*maxlen = val;
	      return true;
	    }
	  else if (simple_cst_equal (val, *maxlen) != 1)
	    return false;
	}

      *maxlen = val;
      return true;
    }

  /* If ARG is registered for SSA update we cannot look at its defining
     statement.  */
  if (name_registered_for_update_p (arg))
    return false;

  /* If we were already here, break the infinite cycle.  */
  if (!*visited)
    *visited = BITMAP_ALLOC (NULL);
  if (!bitmap_set_bit (*visited, SSA_NAME_VERSION (arg)))
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
	    return get_range_strlen (rhs, length, visited, type, fuzzy, flexp);
          }
	else if (gimple_assign_rhs_code (def_stmt) == COND_EXPR)
	  {
	    tree op2 = gimple_assign_rhs2 (def_stmt);
	    tree op3 = gimple_assign_rhs3 (def_stmt);
	    return get_range_strlen (op2, length, visited, type, fuzzy, flexp)
	      && get_range_strlen (op3, length, visited, type, fuzzy, flexp);
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

	    if (!get_range_strlen (arg, length, visited, type, fuzzy, flexp))
	      {
		if (fuzzy)
		  *maxlen = build_all_ones_cst (size_type_node);
		else
		  return false;
	      }
          }
        }
        return true;

      default:
        return false;
    }
}

/* Determine the minimum and maximum value or string length that ARG
   refers to and store each in the first two elements of MINMAXLEN.
   For expressions that point to strings of unknown lengths that are
   character arrays, use the upper bound of the array as the maximum
   length.  For example, given an expression like 'x ? array : "xyz"'
   and array declared as 'char array[8]', MINMAXLEN[0] will be set
   to 3 and MINMAXLEN[1] to 7, the longest string that could be
   stored in array.
   Return true if the range of the string lengths has been obtained
   from the upper bound of an array at the end of a struct.  Such
   an array may hold a string that's longer than its upper bound
   due to it being used as a poor-man's flexible array member.  */

bool
get_range_strlen (tree arg, tree minmaxlen[2])
{
  bitmap visited = NULL;

  minmaxlen[0] = NULL_TREE;
  minmaxlen[1] = NULL_TREE;

  bool flexarray = false;
  get_range_strlen (arg, minmaxlen, &visited, 1, true, &flexarray);

  if (visited)
    BITMAP_FREE (visited);

  return flexarray;
}

tree
get_maxval_strlen (tree arg, int type)
{
  bitmap visited = NULL;
  tree len[2] = { NULL_TREE, NULL_TREE };

  bool dummy;
  if (!get_range_strlen (arg, len, &visited, type, false, &dummy))
    len[1] = NULL_TREE;
  if (visited)
    BITMAP_FREE (visited);

  return len[1];
}


/* Fold function call to builtin strcpy with arguments DEST and SRC.
   If LEN is not NULL, it represents the length of the string to be
   copied.  Return NULL_TREE if no simplification can be made.  */

static bool
gimple_fold_builtin_strcpy (gimple_stmt_iterator *gsi,
			    tree dest, tree src)
{
  location_t loc = gimple_location (gsi_stmt (*gsi));
  tree fn;

  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (operand_equal_p (src, dest, 0))
    {
      replace_call_with_value (gsi, dest);
      return true;
    }

  if (optimize_function_for_size_p (cfun))
    return false;

  fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
  if (!fn)
    return false;

  tree len = get_maxval_strlen (src, 0);
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
  location_t loc = gimple_location (gsi_stmt (*gsi));
  tree fn;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      replace_call_with_value (gsi, dest);
      return true;
    }

  /* We can't compare slen with len as constants below if len is not a
     constant.  */
  if (TREE_CODE (len) != INTEGER_CST)
    return false;

  /* Now, we must be passed a constant src ptr parameter.  */
  tree slen = get_maxval_strlen (src, 0);
  if (!slen || TREE_CODE (slen) != INTEGER_CST)
    return false;

  slen = size_binop_loc (loc, PLUS_EXPR, slen, ssize_int (1));

  /* We do not support simplification of this case, though we do
     support it when expanding trees into RTL.  */
  /* FIXME: generate a call to __builtin_memset.  */
  if (tree_int_cst_lt (slen, len))
    return false;

  /* OK transform into builtin memcpy.  */
  fn = builtin_decl_implicit (BUILT_IN_MEMCPY);
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
  tree haystack = gimple_call_arg (stmt, 0);
  tree needle = gimple_call_arg (stmt, 1);
  const char *p, *q;

  if (!gimple_call_lhs (stmt))
    return false;

  q = c_getstr (needle);
  if (q == NULL)
    return false;

  if ((p = c_getstr (haystack)))
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
  tree len = get_maxval_strlen (src, 0);
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
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree dst = gimple_call_arg (stmt, 0);
  tree src = gimple_call_arg (stmt, 1);
  tree len = gimple_call_arg (stmt, 2);

  const char *p = c_getstr (src);

  /* If the requested length is zero, or the src parameter string
     length is zero, return the dst parameter.  */
  if (integer_zerop (len) || (p && *p == '\0'))
    {
      replace_call_with_value (gsi, dst);
      return true;
    }

  /* If the requested len is greater than or equal to the string
     length, call strcat.  */
  if (TREE_CODE (len) == INTEGER_CST && p
      && compare_tree_int (len, strlen (p)) >= 0)
    {
      tree fn = builtin_decl_implicit (BUILT_IN_STRCAT);

      /* If the replacement _DECL isn't initialized, don't do the
	 transformation.  */
      if (!fn)
	return false;

      gcall *repl = gimple_build_call (fn, 2, dst, src);
      replace_call_with_call_and_fold (gsi, repl);
      return true;
    }

  return false;
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

  if (! tree_fits_uhwi_p (size))
    return false;

  if (! integer_all_onesp (size))
    {
      tree src_len = c_strlen (src, 1);
      if (src_len
	  && tree_fits_uhwi_p (src_len)
	  && tree_fits_uhwi_p (len)
	  && ! tree_int_cst_lt (len, src_len))
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

/* Fold a call to the str{n}{case}cmp builtin pointed by GSI iterator.
   FCODE is the name of the builtin.  */

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
  HOST_WIDE_INT length = -1;

  /* Handle strncmp and strncasecmp functions.  */
  if (gimple_call_num_args (stmt) == 3)
    {
      tree len = gimple_call_arg (stmt, 2);
      if (tree_fits_uhwi_p (len))
	length = tree_to_uhwi (len);
    }

  /* If the LEN parameter is zero, return zero.  */
  if (length == 0)
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

  const char *p1 = c_getstr (str1);
  const char *p2 = c_getstr (str2);

  /* For known strings, return an immediate value.  */
  if (p1 && p2)
    {
      int r = 0;
      bool known_result = false;

      switch (fcode)
	{
	case BUILT_IN_STRCMP:
	  {
	    r = strcmp (p1, p2);
	    known_result = true;
	    break;
	  }
	case BUILT_IN_STRNCMP:
	  {
	    if (length == -1)
	      break;
	    r = strncmp (p1, p2, length);
	    known_result = true;
	    break;
	  }
	/* Only handleable situation is where the string are equal (result 0),
	   which is already handled by operand_equal_p case.  */
	case BUILT_IN_STRCASECMP:
	  break;
	case BUILT_IN_STRNCASECMP:
	  {
	    if (length == -1)
	      break;
	    r = strncmp (p1, p2, length);
	    if (r == 0)
	      known_result = true;
	    break;;
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

  bool nonzero_length = length >= 1
    || fcode == BUILT_IN_STRCMP
    || fcode == BUILT_IN_STRCASECMP;

  location_t loc = gimple_location (stmt);

  /* If the second arg is "", return *(const unsigned char*)arg1.  */
  if (p2 && *p2 == '\0' && nonzero_length)
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
  if (p1 && *p1 == '\0' && nonzero_length)
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

  /* If len parameter is one, return an expression corresponding to
     (*(const unsigned char*)arg2 - *(const unsigned char*)arg1).  */
  if (fcode == BUILT_IN_STRNCMP && length == 1)
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
  const char *p1 = c_getstr (arg1, &string_length);

  if (p1)
    {
      const char *r = (const char *)memchr (p1, c, MIN (length, string_length));
      if (r == NULL)
	{
	  if (length <= string_length)
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
	      tree offset_cst = build_int_cst (TREE_TYPE (len), offset);
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
  tree len = get_maxval_strlen (arg0, 0);
  if (!len
      || TREE_CODE (len) != INTEGER_CST)
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

	    gimple *repl = gimple_build_call (fn_fputc, 2,
					     build_int_cst
					     (integer_type_node, p[0]), arg1);
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

	gimple *repl = gimple_build_call (fn_fwrite, 4, arg0,
					 size_one_node, len, arg1);
	replace_call_with_call_and_fold (gsi, repl);
	return true;
      }
    default:
      gcc_unreachable ();
    }
  return false;
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

  if (! tree_fits_uhwi_p (size))
    return false;

  tree maxlen = get_maxval_strlen (len, 2);
  if (! integer_all_onesp (size))
    {
      if (! tree_fits_uhwi_p (len))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! tree_fits_uhwi_p (maxlen))
	    {
	      if (fcode == BUILT_IN_MEMPCPY_CHK && ignore)
		{
		  /* (void) __mempcpy_chk () can be optimized into
		     (void) __memcpy_chk ().  */
		  fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
		  if (!fn)
		    return false;

		  gimple *repl = gimple_build_call (fn, 4, dest, src, len, size);
		  replace_call_with_call_and_fold (gsi, repl);
		  return true;
		}
	      return false;
	    }
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
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
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  bool ignore = gimple_call_lhs (stmt) == NULL_TREE;
  tree len, fn;

  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (fcode == BUILT_IN_STRCPY_CHK && operand_equal_p (src, dest, 0))
    {
      replace_call_with_value (gsi, dest);
      return true;
    }

  if (! tree_fits_uhwi_p (size))
    return false;

  tree maxlen = get_maxval_strlen (src, 1);
  if (! integer_all_onesp (size))
    {
      len = c_strlen (src, 1);
      if (! len || ! tree_fits_uhwi_p (len))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! tree_fits_uhwi_p (maxlen))
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

	      /* If c_strlen returned something, but not a constant,
		 transform __strcpy_chk into __memcpy_chk.  */
	      fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
	      if (!fn)
		return false;

	      gimple_seq stmts = NULL;
	      len = gimple_convert (&stmts, loc, size_type_node, len);
	      len = gimple_build (&stmts, loc, PLUS_EXPR, size_type_node, len,
				  build_int_cst (size_type_node, 1));
	      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	      gimple *repl = gimple_build_call (fn, 4, dest, src, len, size);
	      replace_call_with_call_and_fold (gsi, repl);
	      return true;
	    }
	}
      else
	maxlen = len;

      if (! tree_int_cst_lt (maxlen, size))
	return false;
    }

  /* If __builtin_st{r,p}cpy_chk is used, assume st{r,p}cpy is available.  */
  fn = builtin_decl_explicit (fcode == BUILT_IN_STPCPY_CHK
			      ? BUILT_IN_STPCPY : BUILT_IN_STRCPY);
  if (!fn)
    return false;

  gimple *repl = gimple_build_call (fn, 2, dest, src);
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
  gimple *stmt = gsi_stmt (*gsi);
  bool ignore = gimple_call_lhs (stmt) == NULL_TREE;
  tree fn;

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

  if (! tree_fits_uhwi_p (size))
    return false;

  tree maxlen = get_maxval_strlen (len, 2);
  if (! integer_all_onesp (size))
    {
      if (! tree_fits_uhwi_p (len))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! tree_fits_uhwi_p (maxlen))
	    return false;
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
	return false;
    }

  /* If __builtin_st{r,p}ncpy_chk is used, assume st{r,p}ncpy is available.  */
  fn = builtin_decl_explicit (fcode == BUILT_IN_STPNCPY_CHK
			      ? BUILT_IN_STPNCPY : BUILT_IN_STRNCPY);
  if (!fn)
    return false;

  gimple *repl = gimple_build_call (fn, 3, dest, src, len);
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
  tree fn, len, lenp1;

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

  len = c_strlen (src, 1);
  if (!len
      || TREE_CODE (len) != INTEGER_CST)
    return false;

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
  gimple_set_vuse (repl, gimple_vuse (stmt));
  gimple_set_vdef (repl, gimple_vdef (stmt));
  if (gimple_vdef (repl)
      && TREE_CODE (gimple_vdef (repl)) == SSA_NAME)
    SSA_NAME_DEF_STMT (gimple_vdef (repl)) = repl;
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

  if (! tree_fits_uhwi_p (size))
    return false;

  if (! integer_all_onesp (size))
    {
      tree maxlen = get_maxval_strlen (len, 2);
      if (! tree_fits_uhwi_p (len))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! tree_fits_uhwi_p (maxlen))
	    return false;
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
	return false;
    }

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

  if (! tree_fits_uhwi_p (size))
    return false;

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
		{
		  len = c_strlen (arg, 1);
		  if (! len || ! tree_fits_uhwi_p (len))
		    len = NULL_TREE;
		}
	    }
	}
    }

  if (! integer_all_onesp (size))
    {
      if (! len || ! tree_int_cst_lt (len, size))
	return false;
    }

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
  tree dest = gimple_call_arg (stmt, 0);
  tree fmt = gimple_call_arg (stmt, 1);
  tree orig = NULL_TREE;
  const char *fmt_str = NULL;

  /* Verify the required arguments in the original call.  We deal with two
     types of sprintf() calls: 'sprintf (str, fmt)' and
     'sprintf (dest, "%s", orig)'.  */
  if (gimple_call_num_args (stmt) > 3)
    return false;

  if (gimple_call_num_args (stmt) == 3)
    orig = gimple_call_arg (stmt, 2);

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

      /* Don't optimize sprintf (buf, "abc", ptr++).  */
      if (orig)
	return false;

      /* Convert sprintf (str, fmt) into strcpy (str, fmt) when
	 'format' is known to contain no % formats.  */
      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, fmt);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (gimple_call_lhs (stmt))
	{
	  repl = gimple_build_assign (gimple_call_lhs (stmt),
				      build_int_cst (integer_type_node,
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
      tree fn;
      fn = builtin_decl_implicit (BUILT_IN_STRCPY);

      if (!fn)
	return false;

      /* Don't crash on sprintf (str1, "%s").  */
      if (!orig)
	return false;

      tree orig_len = NULL_TREE;
      if (gimple_call_lhs (stmt))
	{
	  orig_len = get_maxval_strlen (orig, 0);
	  if (!orig_len)
	    return false;
	}

      /* Convert sprintf (str1, "%s", str2) into strcpy (str1, str2).  */
      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, orig);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (gimple_call_lhs (stmt))
	{
	  if (!useless_type_conversion_p (integer_type_node,
					  TREE_TYPE (orig_len)))
	    orig_len = fold_convert (integer_type_node, orig_len);
	  repl = gimple_build_assign (gimple_call_lhs (stmt), orig_len);
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

  if (!tree_fits_uhwi_p (destsize))
    return false;
  unsigned HOST_WIDE_INT destlen = tree_to_uhwi (destsize);

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

      /* We could expand this as
	 memcpy (str, fmt, cst - 1); str[cst - 1] = '\0';
	 or to
	 memcpy (str, fmt_with_nul_at_cstm1, cst);
	 but in the former case that might increase code size
	 and in the latter case grow .rodata section too much.
	 So punt for now.  */
      size_t len = strlen (fmt_str);
      if (len >= destlen)
	return false;

      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, fmt);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (gimple_call_lhs (stmt))
	{
	  repl = gimple_build_assign (gimple_call_lhs (stmt),
				      build_int_cst (integer_type_node, len));
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

      tree orig_len = get_maxval_strlen (orig, 0);
      if (!orig_len || TREE_CODE (orig_len) != INTEGER_CST)
	return false;

      /* We could expand this as
	 memcpy (str1, str2, cst - 1); str1[cst - 1] = '\0';
	 or to
	 memcpy (str1, str2_with_nul_at_cstm1, cst);
	 but in the former case that might increase code size
	 and in the latter case grow .rodata section too much.
	 So punt for now.  */
      if (compare_tree_int (orig_len, destlen) >= 0)
	return false;

      /* Convert snprintf (str1, cst, "%s", str2) into
	 strcpy (str1, str2) if strlen (str2) < cst.  */
      gimple_seq stmts = NULL;
      gimple *repl = gimple_build_call (fn, 2, dest, orig);
      gimple_seq_add_stmt_without_update (&stmts, repl);
      if (gimple_call_lhs (stmt))
	{
	  if (!useless_type_conversion_p (integer_type_node,
					  TREE_TYPE (orig_len)))
	    orig_len = fold_convert (integer_type_node, orig_len);
	  repl = gimple_build_assign (gimple_call_lhs (stmt), orig_len);
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
	      tree offset_node, string_cst;

	      /* Create a NUL-terminated string that's one char shorter
		 than the original, stripping off the trailing '\n'.  */
	      newarg = build_string_literal (len, str);
	      string_cst = string_constant (newarg, &offset_node);
	      gcc_checking_assert (string_cst
				   && (TREE_STRING_LENGTH (string_cst)
				       == (int) len)
				   && integer_zerop (offset_node)
				   && (unsigned char)
				      TREE_STRING_POINTER (string_cst)[len - 1]
				      == target_newline);
	      /* build_string_literal creates a new STRING_CST,
		 modify it in place to avoid double copying.  */
	      newstr = CONST_CAST (char *, TREE_STRING_POINTER (string_cst));
	      newstr[len - 1] = '\0';
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
  tree len = get_maxval_strlen (gimple_call_arg (stmt, 0), 0);
  if (!len)
    return false;
  len = force_gimple_operand_gsi (gsi, len, true, NULL, true, GSI_SAME_STMT);
  replace_call_with_value (gsi, len);
  return true;
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
      return gimple_fold_builtin_memory_op (gsi, gimple_call_arg (stmt, 0),
					    gimple_call_arg (stmt, 1), 0);
    case BUILT_IN_MEMPCPY:
      return gimple_fold_builtin_memory_op (gsi, gimple_call_arg (stmt, 0),
					    gimple_call_arg (stmt, 1), 1);
    case BUILT_IN_MEMMOVE:
      return gimple_fold_builtin_memory_op (gsi, gimple_call_arg (stmt, 0),
					    gimple_call_arg (stmt, 1), 3);
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
    case BUILT_IN_STRCASECMP:
    case BUILT_IN_STRNCMP:
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
      if (!update_call_from_tree (gsi, result))
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
  bool is_pos = gimple_call_internal_fn (call) == IFN_GOACC_DIM_POS;
  tree result = NULL_TREE;

  /* If the size is 1, or we only want the size and it is not dynamic,
     we know the answer.  */
  if (size == 1 || (!is_pos && size))
    {
      tree type = TREE_TYPE (gimple_call_lhs (call));
      result = build_int_cst (type, size - is_pos);
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
      || TYPE_PRECISION (etype) != GET_MODE_BITSIZE (TYPE_MODE (etype)))
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

  if (int_size_in_bytes (etype) != GET_MODE_SIZE (mode))
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
  gimple_set_vdef (g, gimple_vdef (stmt));
  gimple_set_vuse (g, gimple_vuse (stmt));
  SSA_NAME_DEF_STMT (gimple_vdef (g)) = g;
  tree oldlhs = gimple_call_lhs (stmt);
  if (stmt_can_throw_internal (stmt))
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
  typedef FIXED_WIDE_INT (WIDE_INT_MAX_PRECISION * 2) widest2_int;
  typedef generic_wide_int <wi::extended_tree <WIDE_INT_MAX_PRECISION * 2> >
    widest2_int_cst;
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
		  location_t loc = gimple_location_safe (stmt);
		  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
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
		  tree fndecl = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
		  gimple *new_stmt = gimple_build_call (fndecl, 0);
		  gimple_set_location (new_stmt, gimple_location (stmt));
		  /* If the call had a SSA name as lhs morph that into
		     an uninitialized value.  */
		  if (lhs && TREE_CODE (lhs) == SSA_NAME)
		    {
		      tree var = create_tmp_var (TREE_TYPE (lhs));
		      SET_SSA_NAME_VAR_OR_IDENTIFIER (lhs, var);
		      SSA_NAME_DEF_STMT (lhs) = gimple_build_nop ();
		      set_ssa_default_def (cfun, var, lhs);
		    }
		  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
		  gimple_set_vdef (new_stmt, gimple_vdef (stmt));
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
      else
	{
	  tree tmp = maybe_fold_reference (gimple_call_chain (stmt), false);
	  if (tmp)
	    {
	      gimple_call_set_chain (stmt, tmp);
	      changed = true;
	    }
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
      tree overflow = NULL_TREE;
      switch (gimple_call_internal_fn (stmt))
	{
	case IFN_BUILTIN_EXPECT:
	  result = fold_builtin_expect (gimple_location (stmt),
					gimple_call_arg (stmt, 0),
					gimple_call_arg (stmt, 1),
					gimple_call_arg (stmt, 2));
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
		    && tree_int_cst_le (index, bound))
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
	default:
	  break;
	}
      if (subcode != ERROR_MARK)
	{
	  tree arg0 = gimple_call_arg (stmt, 0);
	  tree arg1 = gimple_call_arg (stmt, 1);
	  tree type = TREE_TYPE (arg0);
	  if (cplx_result)
	    {
	      tree lhs = gimple_call_lhs (stmt);
	      if (lhs == NULL_TREE)
		type = NULL_TREE;
	      else
		type = TREE_TYPE (TREE_TYPE (lhs));
	    }
	  if (type == NULL_TREE)
	    ;
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
	  else if (TREE_CODE (arg0) == INTEGER_CST
		   && TREE_CODE (arg1) == INTEGER_CST)
	    {
	      if (cplx_result)
		result = int_const_binop (subcode, fold_convert (type, arg0),
					  fold_convert (type, arg1));
	      else
		result = int_const_binop (subcode, arg0, arg1);
	      if (result && arith_overflowed_p (subcode, type, arg0, arg1))
		{
		  if (cplx_result)
		    overflow = build_one_cst (type);
		  else
		    result = NULL_TREE;
		}
	    }
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
	  if (!update_call_from_tree (gsi, result))
	    gimplify_and_update_call_from_tree (gsi, result);
	  changed = true;
	}
    }

  return changed;
}


/* Return true whether NAME has a use on STMT.  */

static bool
has_use_on_stmt (tree name, gimple *stmt)
{
  imm_use_iterator iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    if (USE_STMT (use_p) == stmt)
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
				  code_helper rcode, tree *ops,
				  gimple_seq *seq, bool inplace)
{
  gimple *stmt = gsi_stmt (*gsi);

  /* Play safe and do not allow abnormals to be mentioned in
     newly created statements.  See also maybe_push_res_to_seq.
     As an exception allow such uses if there was a use of the
     same SSA name on the old stmt.  */
  if ((TREE_CODE (ops[0]) == SSA_NAME
       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ops[0])
       && !has_use_on_stmt (ops[0], stmt))
      || (ops[1]
	  && TREE_CODE (ops[1]) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ops[1])
	  && !has_use_on_stmt (ops[1], stmt))
      || (ops[2]
	  && TREE_CODE (ops[2]) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ops[2])
	  && !has_use_on_stmt (ops[2], stmt))
      || (COMPARISON_CLASS_P (ops[0])
	  && ((TREE_CODE (TREE_OPERAND (ops[0], 0)) == SSA_NAME
	       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND (ops[0], 0))
	       && !has_use_on_stmt (TREE_OPERAND (ops[0], 0), stmt))
	      || (TREE_CODE (TREE_OPERAND (ops[0], 1)) == SSA_NAME
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND (ops[0], 1))
		  && !has_use_on_stmt (TREE_OPERAND (ops[0], 1), stmt)))))
    return false;

  /* Don't insert new statements when INPLACE is true, even if we could
     reuse STMT for the final statement.  */
  if (inplace && !gimple_seq_empty_p (*seq))
    return false;

  if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
    {
      gcc_assert (rcode.is_tree_code ());
      if (TREE_CODE_CLASS ((enum tree_code)rcode) == tcc_comparison
	  /* GIMPLE_CONDs condition may not throw.  */
	  && (!flag_exceptions
	      || !cfun->can_throw_non_call_exceptions
	      || !operation_could_trap_p (rcode,
					  FLOAT_TYPE_P (TREE_TYPE (ops[0])),
					  false, NULL_TREE)))
	gimple_cond_set_condition (cond_stmt, rcode, ops[0], ops[1]);
      else if (rcode == SSA_NAME)
	gimple_cond_set_condition (cond_stmt, NE_EXPR, ops[0],
				   build_zero_cst (TREE_TYPE (ops[0])));
      else if (rcode == INTEGER_CST)
	{
	  if (integer_zerop (ops[0]))
	    gimple_cond_make_false (cond_stmt);
	  else
	    gimple_cond_make_true (cond_stmt);
	}
      else if (!inplace)
	{
	  tree res = maybe_push_res_to_seq (rcode, boolean_type_node,
					    ops, seq);
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
	   && rcode.is_tree_code ())
    {
      if (!inplace
	  || gimple_num_ops (stmt) > get_gimple_rhs_num_ops (rcode))
	{
	  maybe_build_generic_op (rcode,
				  TREE_TYPE (gimple_assign_lhs (stmt)), ops);
	  gimple_assign_set_rhs_with_ops (gsi, rcode, ops[0], ops[1], ops[2]);
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
  else if (rcode.is_fn_code ()
	   && gimple_call_combined_fn (stmt) == rcode)
    {
      unsigned i;
      for (i = 0; i < gimple_call_num_args (stmt); ++i)
	{
	  gcc_assert (ops[i] != NULL_TREE);
	  gimple_call_set_arg (stmt, i, ops[i]);
	}
      if (i < 3)
	gcc_assert (ops[i] == NULL_TREE);
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
	  if (!maybe_push_res_to_seq (rcode, TREE_TYPE (lhs),
				      ops, seq, lhs))
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
maybe_canonicalize_mem_ref_addr (tree *t)
{
  bool res = false;

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
	  HOST_WIDE_INT coffset;
	  base = get_addr_base_and_unit_offset (TREE_OPERAND (addr, 0),
						&coffset);
	  if (!base)
	    gcc_unreachable ();

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

  /* Canonicalize TARGET_MEM_REF in particular with respect to
     the indexes becoming constant.  */
  else if (TREE_CODE (*t) == TARGET_MEM_REF)
    {
      tree tem = maybe_fold_tmr (*t);
      if (tem)
	{
	  *t = tem;
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
  bool nowarning = gimple_no_warning_p (stmt);
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
	for (i = 0; i < gimple_call_num_args (stmt); ++i)
	  {
	    tree *arg = gimple_call_arg_ptr (stmt, i);
	    if (REFERENCE_CLASS_P (*arg)
		&& maybe_canonicalize_mem_ref_addr (arg))
	      changed = true;
	  }
	tree *lhs = gimple_call_lhs_ptr (stmt);
	if (*lhs
	    && REFERENCE_CLASS_P (*lhs)
	    && maybe_canonicalize_mem_ref_addr (lhs))
	  changed = true;
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
	      && maybe_canonicalize_mem_ref_addr (val))
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
      code_helper rcode;
      tree ops[3] = {};
      if (gimple_simplify (stmt, &rcode, ops, inplace ? NULL : &seq,
			   valueize, valueize))
	{
	  if (replace_stmt_with_simplification (gsi, rcode, ops, &seq, inplace))
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

    case GIMPLE_ASM:
      /* Fold *& in asm operands.  */
      {
	gasm *asm_stmt = as_a <gasm *> (stmt);
	size_t noutputs;
	const char **oconstraints;
	const char *constraint;
	bool allows_mem, allows_reg;

	noutputs = gimple_asm_noutputs (asm_stmt);
	oconstraints = XALLOCAVEC (const char *, noutputs);

	for (i = 0; i < gimple_asm_noutputs (asm_stmt); ++i)
	  {
	    tree link = gimple_asm_output_op (asm_stmt, i);
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
	for (i = 0; i < gimple_asm_ninputs (asm_stmt); ++i)
	  {
	    tree link = gimple_asm_input_op (asm_stmt, i);
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
	  else if (val
		   && TREE_CODE (val) == ADDR_EXPR)
	    {
	      tree ref = TREE_OPERAND (val, 0);
	      tree tem = maybe_fold_reference (ref, false);
	      if (tem)
		{
		  tem = build_fold_addr_expr_with_type (tem, TREE_TYPE (val));
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

  /* Fold *& on the lhs.  */
  if (gimple_has_lhs (stmt))
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
and_comparisons_1 (enum tree_code code1, tree op1a, tree op1b,
		   enum tree_code code2, tree op2a, tree op2b);
static tree
and_var_with_comparison (tree var, bool invert,
			 enum tree_code code2, tree op2a, tree op2b);
static tree
and_var_with_comparison_1 (gimple *stmt,
			   enum tree_code code2, tree op2a, tree op2b);
static tree
or_comparisons_1 (enum tree_code code1, tree op1a, tree op1b,
		  enum tree_code code2, tree op2a, tree op2b);
static tree
or_var_with_comparison (tree var, bool invert,
			enum tree_code code2, tree op2a, tree op2b);
static tree
or_var_with_comparison_1 (gimple *stmt,
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
  gimple *stmt = SSA_NAME_DEF_STMT (var);

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
and_var_with_comparison_1 (gimple *stmt,
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
      gimple *stmt = SSA_NAME_DEF_STMT (op1a);
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
  gimple *stmt = SSA_NAME_DEF_STMT (var);

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
or_var_with_comparison_1 (gimple *stmt,
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
      gimple *stmt = SSA_NAME_DEF_STMT (op1a);
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
gimple_fold_stmt_to_constant_1 (gimple *stmt, tree (*valueize) (tree),
				tree (*gvalueize) (tree))
{
  code_helper rcode;
  tree ops[3] = {};
  /* ???  The SSA propagators do not correctly deal with following SSA use-def
     edges if there are intermediate VARYING defs.  For this reason
     do not follow SSA edges here even though SCCVN can technically
     just deal fine with that.  */
  if (gimple_simplify (stmt, &rcode, ops, NULL, gvalueize, valueize))
    {
      tree res = NULL_TREE;
      if (gimple_simplified_result_is_gimple_val (rcode, ops))
	res = ops[0];
      else if (mprts_hook)
	res = mprts_hook (rcode, gimple_expr_type (stmt), ops);
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
		  HOST_WIDE_INT offset = 0;
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
		  unsigned i, nelts;
		  tree val;

		  nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs));
		  auto_vec<tree, 32> vec (nelts);
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

		  return build_vector (TREE_TYPE (rhs), vec);
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
		    return build_fold_addr_expr_loc
			(loc,
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
				       gimple_expr_type (stmt), op0, op1, op2);
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
	    && DECL_BUILT_IN (TREE_OPERAND (fn, 0))
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
get_base_constructor (tree base, HOST_WIDE_INT *bit_offset,
		      tree (*valueize)(tree))
{
  HOST_WIDE_INT bit_offset2, size, max_size;
  bool reverse;

  if (TREE_CODE (base) == MEM_REF)
    {
      if (!integer_zerop (TREE_OPERAND (base, 1)))
	{
	  if (!tree_fits_shwi_p (TREE_OPERAND (base, 1)))
	    return NULL_TREE;
	  *bit_offset += (mem_ref_offset (base).to_short_addr ()
			  * BITS_PER_UNIT);
	}

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
      if (max_size == -1 || size != max_size)
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

/* CTOR is CONSTRUCTOR of an array type.  Fold reference of type TYPE and size
   SIZE to the memory at bit OFFSET.  */

static tree
fold_array_ctor_reference (tree type, tree ctor,
			   unsigned HOST_WIDE_INT offset,
			   unsigned HOST_WIDE_INT size,
			   tree from_decl)
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
      /* Static constructors for variably sized objects makes no sense.  */
      if (TREE_CODE (TYPE_MIN_VALUE (domain_type)) != INTEGER_CST)
	return NULL_TREE;
      low_bound = wi::to_offset (TYPE_MIN_VALUE (domain_type));
    }
  else
    low_bound = 0;
  /* Static constructors for variably sized objects makes no sense.  */
  if (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ctor)))) != INTEGER_CST)
    return NULL_TREE;
  elt_size = wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ctor))));

  /* We can handle only constantly sized accesses that are known to not
     be larger than size of array element.  */
  if (!TYPE_SIZE_UNIT (type)
      || TREE_CODE (TYPE_SIZE_UNIT (type)) != INTEGER_CST
      || elt_size < wi::to_offset (TYPE_SIZE_UNIT (type))
      || elt_size == 0)
    return NULL_TREE;

  /* Compute the array index we look for.  */
  access_index = wi::udiv_trunc (offset_int (offset / BITS_PER_UNIT),
				 elt_size);
  access_index += low_bound;

  /* And offset within the access.  */
  inner_offset = offset % (elt_size.to_uhwi () * BITS_PER_UNIT);

  /* See if the array field is large enough to span whole access.  We do not
     care to fold accesses spanning multiple array indexes.  */
  if (inner_offset + size > elt_size.to_uhwi () * BITS_PER_UNIT)
    return NULL_TREE;
  if (tree val = get_array_ctor_element_at_index (ctor, access_index))
    return fold_ctor_reference (type, val, inner_offset, size, from_decl);

  /* When memory is not explicitely mentioned in constructor,
     it is 0 (or out of range).  */
  return build_zero_cst (type);
}

/* CTOR is CONSTRUCTOR of an aggregate or vector.
   Fold reference of type TYPE and size SIZE to the memory at bit OFFSET.  */

static tree
fold_nonarray_ctor_reference (tree type, tree ctor,
			      unsigned HOST_WIDE_INT offset,
			      unsigned HOST_WIDE_INT size,
			      tree from_decl)
{
  unsigned HOST_WIDE_INT cnt;
  tree cfield, cval;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield,
			    cval)
    {
      tree byte_offset = DECL_FIELD_OFFSET (cfield);
      tree field_offset = DECL_FIELD_BIT_OFFSET (cfield);
      tree field_size = DECL_SIZE (cfield);
      offset_int bitoffset;
      offset_int bitoffset_end, access_end;

      /* Variable sized objects in static constructors makes no sense,
	 but field_size can be NULL for flexible array members.  */
      gcc_assert (TREE_CODE (field_offset) == INTEGER_CST
		  && TREE_CODE (byte_offset) == INTEGER_CST
		  && (field_size != NULL_TREE
		      ? TREE_CODE (field_size) == INTEGER_CST
		      : TREE_CODE (TREE_TYPE (cfield)) == ARRAY_TYPE));

      /* Compute bit offset of the field.  */
      bitoffset = (wi::to_offset (field_offset)
		   + (wi::to_offset (byte_offset) << LOG2_BITS_PER_UNIT));
      /* Compute bit offset where the field ends.  */
      if (field_size != NULL_TREE)
	bitoffset_end = bitoffset + wi::to_offset (field_size);
      else
	bitoffset_end = 0;

      access_end = offset_int (offset) + size;

      /* Is there any overlap between [OFFSET, OFFSET+SIZE) and
	 [BITOFFSET, BITOFFSET_END)?  */
      if (wi::cmps (access_end, bitoffset) > 0
	  && (field_size == NULL_TREE
	      || wi::lts_p (offset, bitoffset_end)))
	{
	  offset_int inner_offset = offset_int (offset) - bitoffset;
	  /* We do have overlap.  Now see if field is large enough to
	     cover the access.  Give up for accesses spanning multiple
	     fields.  */
	  if (wi::cmps (access_end, bitoffset_end) > 0)
	    return NULL_TREE;
	  if (offset < bitoffset)
	    return NULL_TREE;
	  return fold_ctor_reference (type, cval,
				      inner_offset.to_uhwi (), size,
				      from_decl);
	}
    }
  /* When memory is not explicitely mentioned in constructor, it is 0.  */
  return build_zero_cst (type);
}

/* CTOR is value initializing memory, fold reference of type TYPE and size SIZE
   to the memory at bit OFFSET.  */

tree
fold_ctor_reference (tree type, tree ctor, unsigned HOST_WIDE_INT offset,
		     unsigned HOST_WIDE_INT size, tree from_decl)
{
  tree ret;

  /* We found the field with exact match.  */
  if (useless_type_conversion_p (type, TREE_TYPE (ctor))
      && !offset)
    return canonicalize_constructor_val (unshare_expr (ctor), from_decl);

  /* We are at the end of walk, see if we can view convert the
     result.  */
  if (!AGGREGATE_TYPE_P (TREE_TYPE (ctor)) && !offset
      /* VIEW_CONVERT_EXPR is defined only for matching sizes.  */
      && !compare_tree_int (TYPE_SIZE (type), size)
      && !compare_tree_int (TYPE_SIZE (TREE_TYPE (ctor)), size))
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
  /* For constants and byte-aligned/sized reads try to go through
     native_encode/interpret.  */
  if (CONSTANT_CLASS_P (ctor)
      && BITS_PER_UNIT == 8
      && offset % BITS_PER_UNIT == 0
      && size % BITS_PER_UNIT == 0
      && size <= MAX_BITSIZE_MODE_ANY_MODE)
    {
      unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
      int len = native_encode_expr (ctor, buf, size / BITS_PER_UNIT,
				    offset / BITS_PER_UNIT);
      if (len > 0)
	return native_interpret_expr (type, buf, len);
    }
  if (TREE_CODE (ctor) == CONSTRUCTOR)
    {

      if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (ctor)) == VECTOR_TYPE)
	return fold_array_ctor_reference (type, ctor, offset, size,
					  from_decl);
      else
	return fold_nonarray_ctor_reference (type, ctor, offset, size,
					     from_decl);
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
	  && TREE_CODE (idx) == INTEGER_CST)
	{
	  tree low_bound, unit_size;

	  /* If the resulting bit-offset is constant, track it.  */
	  if ((low_bound = array_ref_low_bound (t),
	       TREE_CODE (low_bound) == INTEGER_CST)
	      && (unit_size = array_ref_element_size (t),
		  tree_fits_uhwi_p (unit_size)))
	    {
	      offset_int woffset
		= wi::sext (wi::to_offset (idx) - wi::to_offset (low_bound),
			    TYPE_PRECISION (TREE_TYPE (idx)));

	      if (wi::fits_shwi_p (woffset))
		{
		  offset = woffset.to_shwi ();
		  /* TODO: This code seems wrong, multiply then check
		     to see if it fits.  */
		  offset *= tree_to_uhwi (unit_size);
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
      if (max_size == -1 || max_size != size)
	return NULL_TREE;
      /* We can not determine ctor.  */
      if (!ctor)
	return NULL_TREE;

      /* Out of bound array access.  Value is undefined, but don't fold.  */
      if (offset < 0)
	return NULL_TREE;

      return fold_ctor_reference (TREE_TYPE (t), ctor, offset, size,
				  base);

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

  /* This code makes an assumption that there are no 
     indexed fileds produced by C++ FE, so we can directly index the array. */
  if (access_index < CONSTRUCTOR_NELTS (init))
    {
      fn = CONSTRUCTOR_ELT (init, access_index)->value;
      gcc_checking_assert (!CONSTRUCTOR_ELT (init, access_index)->index);
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
    fn = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
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
          if (offset / part_widthi
	      < TYPE_VECTOR_SUBPARTS (TREE_TYPE (addrtype)))
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

   Returns a sequence of statements that replace STMT and also contain
   a modified form of STMT itself.  */

gimple_seq
rewrite_to_defined_overflow (gimple *stmt)
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
  for (unsigned i = 1; i < gimple_num_ops (stmt); ++i)
    {
      tree op = gimple_op (stmt, i);
      op = gimple_convert (&stmts, type, op);
      gimple_set_op (stmt, i, op);
    }
  gimple_assign_set_lhs (stmt, make_ssa_name (type, stmt));
  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    gimple_assign_set_rhs_code (stmt, PLUS_EXPR);
  gimple_seq_add_stmt (&stmts, stmt);
  gimple *cvt = gimple_build_assign (lhs, NOP_EXPR, gimple_assign_lhs (stmt));
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

/* Build the expression CODE OP0 of type TYPE with location LOC,
   simplifying it first if possible.  Returns the built
   expression value and appends statements possibly defining it
   to SEQ.  */

tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum tree_code code, tree type, tree op0)
{
  tree res = gimple_simplify (code, type, op0, seq, gimple_build_valueize);
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
      gimple_seq_add_stmt_without_update (seq, stmt);
    }
  return res;
}

/* Build the expression OP0 CODE OP1 of type TYPE with location LOC,
   simplifying it first if possible.  Returns the built
   expression value and appends statements possibly defining it
   to SEQ.  */

tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum tree_code code, tree type, tree op0, tree op1)
{
  tree res = gimple_simplify (code, type, op0, op1, seq, gimple_build_valueize);
  if (!res)
    {
      res = create_tmp_reg_or_ssa_name (type);
      gimple *stmt = gimple_build_assign (res, code, op0, op1);
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (seq, stmt);
    }
  return res;
}

/* Build the expression (CODE OP0 OP1 OP2) of type TYPE with location LOC,
   simplifying it first if possible.  Returns the built
   expression value and appends statements possibly defining it
   to SEQ.  */

tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum tree_code code, tree type, tree op0, tree op1, tree op2)
{
  tree res = gimple_simplify (code, type, op0, op1, op2,
			      seq, gimple_build_valueize);
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
      gimple_seq_add_stmt_without_update (seq, stmt);
    }
  return res;
}

/* Build the call FN (ARG0) with a result of type TYPE
   (or no result if TYPE is void) with location LOC,
   simplifying it first if possible.  Returns the built
   expression value (or NULL_TREE if TYPE is void) and appends
   statements possibly defining it to SEQ.  */

tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum built_in_function fn, tree type, tree arg0)
{
  tree res = gimple_simplify (fn, type, arg0, seq, gimple_build_valueize);
  if (!res)
    {
      tree decl = builtin_decl_implicit (fn);
      gimple *stmt = gimple_build_call (decl, 1, arg0);
      if (!VOID_TYPE_P (type))
	{
	  res = create_tmp_reg_or_ssa_name (type);
	  gimple_call_set_lhs (stmt, res);
	}
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (seq, stmt);
    }
  return res;
}

/* Build the call FN (ARG0, ARG1) with a result of type TYPE
   (or no result if TYPE is void) with location LOC,
   simplifying it first if possible.  Returns the built
   expression value (or NULL_TREE if TYPE is void) and appends
   statements possibly defining it to SEQ.  */

tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum built_in_function fn, tree type, tree arg0, tree arg1)
{
  tree res = gimple_simplify (fn, type, arg0, arg1, seq, gimple_build_valueize);
  if (!res)
    {
      tree decl = builtin_decl_implicit (fn);
      gimple *stmt = gimple_build_call (decl, 2, arg0, arg1);
      if (!VOID_TYPE_P (type))
	{
	  res = create_tmp_reg_or_ssa_name (type);
	  gimple_call_set_lhs (stmt, res);
	}
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (seq, stmt);
    }
  return res;
}

/* Build the call FN (ARG0, ARG1, ARG2) with a result of type TYPE
   (or no result if TYPE is void) with location LOC,
   simplifying it first if possible.  Returns the built
   expression value (or NULL_TREE if TYPE is void) and appends
   statements possibly defining it to SEQ.  */

tree
gimple_build (gimple_seq *seq, location_t loc,
	      enum built_in_function fn, tree type,
	      tree arg0, tree arg1, tree arg2)
{
  tree res = gimple_simplify (fn, type, arg0, arg1, arg2,
			      seq, gimple_build_valueize);
  if (!res)
    {
      tree decl = builtin_decl_implicit (fn);
      gimple *stmt = gimple_build_call (decl, 3, arg0, arg1, arg2);
      if (!VOID_TYPE_P (type))
	{
	  res = create_tmp_reg_or_ssa_name (type);
	  gimple_call_set_lhs (stmt, res);
	}
      gimple_set_location (stmt, loc);
      gimple_seq_add_stmt_without_update (seq, stmt);
    }
  return res;
}

/* Build the conversion (TYPE) OP with a result of type TYPE
   with location LOC if such conversion is neccesary in GIMPLE,
   simplifying it first.
   Returns the built expression value and appends
   statements possibly defining it to SEQ.  */

tree
gimple_convert (gimple_seq *seq, location_t loc, tree type, tree op)
{
  if (useless_type_conversion_p (type, TREE_TYPE (op)))
    return op;
  return gimple_build (seq, loc, NOP_EXPR, type, op);
}

/* Build the conversion (ptrofftype) OP with a result of a type
   compatible with ptrofftype with location LOC if such conversion
   is neccesary in GIMPLE, simplifying it first.
   Returns the built expression value and appends
   statements possibly defining it to SEQ.  */

tree
gimple_convert_to_ptrofftype (gimple_seq *seq, location_t loc, tree op)
{
  if (ptrofftype_p (TREE_TYPE (op)))
    return op;
  return gimple_convert (seq, loc, sizetype, op);
}

/* Build a vector of type TYPE in which each element has the value OP.
   Return a gimple value for the result, appending any new statements
   to SEQ.  */

tree
gimple_build_vector_from_val (gimple_seq *seq, location_t loc, tree type,
			      tree op)
{
  tree res, vec = build_vector_from_val (type, op);
  if (is_gimple_val (vec))
    return vec;
  if (gimple_in_ssa_p (cfun))
    res = make_ssa_name (type);
  else
    res = create_tmp_reg (type);
  gimple *stmt = gimple_build_assign (res, vec);
  gimple_set_location (stmt, loc);
  gimple_seq_add_stmt_without_update (seq, stmt);
  return res;
}

/* Build a vector of type TYPE in which the elements have the values
   given by ELTS.  Return a gimple value for the result, appending any
   new instructions to SEQ.  */

tree
gimple_build_vector (gimple_seq *seq, location_t loc, tree type,
		     vec<tree> elts)
{
  unsigned int nelts = elts.length ();
  gcc_assert (nelts == TYPE_VECTOR_SUBPARTS (type));
  for (unsigned int i = 0; i < nelts; ++i)
    if (!TREE_CONSTANT (elts[i]))
      {
	vec<constructor_elt, va_gc> *v;
	vec_alloc (v, nelts);
	for (i = 0; i < nelts; ++i)
	  CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, elts[i]);

	tree res;
	if (gimple_in_ssa_p (cfun))
	  res = make_ssa_name (type);
	else
	  res = create_tmp_reg (type);
	gimple *stmt = gimple_build_assign (res, build_constructor (type, v));
	gimple_set_location (stmt, loc);
	gimple_seq_add_stmt_without_update (seq, stmt);
	return res;
      }
  return build_vector (type, elts);
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
  switch (get_gimple_rhs_class (code))
    {
    case GIMPLE_UNARY_RHS:
      return tree_unary_nonnegative_warnv_p (gimple_assign_rhs_code (stmt),
					     gimple_expr_type (stmt),
					     gimple_assign_rhs1 (stmt),
					     strict_overflow_p, depth);
    case GIMPLE_BINARY_RHS:
      return tree_binary_nonnegative_warnv_p (gimple_assign_rhs_code (stmt),
					      gimple_expr_type (stmt),
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

  return tree_call_nonnegative_warnv_p (gimple_expr_type (stmt),
					gimple_call_combined_fn (stmt),
					arg0,
					arg1,
					strict_overflow_p, depth);
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
