/* Const/copy propagation and SSA_NAME replacement support routines.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "langhooks.h"

/* This file provides a handful of interfaces for performing const/copy
   propagation and simple expression replacement which keep variable
   annotations up-to-date.

   We require that for any copy operation where the RHS and LHS have
   a non-null memory tag the memory tag be the same.   It is OK
   for one or both of the memory tags to be NULL.

   We also require tracking if a variable is dereferenced in a load or
   store operation.

   We enforce these requirements by having all copy propagation and
   replacements of one SSA_NAME with a different SSA_NAME to use the
   APIs defined in this file.  */


/* Return true if we may propagate ORIG into DEST, false otherwise.  */

bool
may_propagate_copy (tree dest, tree orig)
{
  tree type_d = TREE_TYPE (dest);
  tree type_o = TREE_TYPE (orig);

  /* Do not copy between types for which we *do* need a conversion.  */
  if (!tree_ssa_useless_type_conversion_1 (type_d, type_o))
    return false;

  /* FIXME.  GIMPLE is allowing pointer assignments and comparisons of
     pointers that have different alias sets.  This means that these
     pointers will have different memory tags associated to them.

     If we allow copy propagation in these cases, statements de-referencing
     the new pointer will now have a reference to a different memory tag
     with potentially incorrect SSA information.

     This was showing up in libjava/java/util/zip/ZipFile.java with code
     like:

     	struct java.io.BufferedInputStream *T.660;
	struct java.io.BufferedInputStream *T.647;
	struct java.io.InputStream *is;
	struct java.io.InputStream *is.662;
	[ ... ]
	T.660 = T.647;
	is = T.660;	<-- This ought to be type-casted
	is.662 = is;

     Also, f/name.c exposed a similar problem with a COND_EXPR predicate
     that was causing DOM to generate and equivalence with two pointers of
     alias-incompatible types:

     	struct _ffename_space *n;
	struct _ffename *ns;
	[ ... ]
	if (n == ns)
	  goto lab;
	...
	lab:
	return n;

     I think that GIMPLE should emit the appropriate type-casts.  For the
     time being, blocking copy-propagation in these cases is the safe thing
     to do.  */
  if (TREE_CODE (dest) == SSA_NAME && TREE_CODE (orig) == SSA_NAME
      && POINTER_TYPE_P (type_d) && POINTER_TYPE_P (type_o))
    {
      tree mt_dest = var_ann (SSA_NAME_VAR (dest))->type_mem_tag;
      tree mt_orig = var_ann (SSA_NAME_VAR (orig))->type_mem_tag;
      if (mt_dest && mt_orig && mt_dest != mt_orig)
	return false;
      else if (!lang_hooks.types_compatible_p (type_d, type_o))
	return false;
      else if (get_alias_set (TREE_TYPE (type_d)) != 
	       get_alias_set (TREE_TYPE (type_o)))
	return false;
    }

  /* If the destination is a SSA_NAME for a virtual operand, then we have
     some special cases to handle.  */
  if (TREE_CODE (dest) == SSA_NAME && !is_gimple_reg (dest))
    {
      /* If both operands are SSA_NAMEs referring to virtual operands, then
	 we can always propagate.  */
      if (TREE_CODE (orig) == SSA_NAME)
	{
	  if (!is_gimple_reg (orig))
	    return true;

#ifdef ENABLE_CHECKING
	  /* If we have one real and one virtual operand, then something has
	     gone terribly wrong.  */
	  gcc_assert (!is_gimple_reg (orig));
#endif
	}

      /* We have a "copy" from something like a constant into a virtual
	 operand.  Reject these.  */
      return false;
    }

  /* If ORIG flows in from an abnormal edge, it cannot be propagated.  */
  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  /* If DEST is an SSA_NAME that flows from an abnormal edge, then it
     cannot be replaced.  */
  if (TREE_CODE (dest) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (dest))
    return false;

  /* Anything else is OK.  */
  return true;
}

/* Similarly, but we know that we're propagating into an ASM_EXPR.  */

bool
may_propagate_copy_into_asm (tree dest)
{
  /* Hard register operands of asms are special.  Do not bypass.  */
  return !(TREE_CODE (dest) == SSA_NAME
	   && TREE_CODE (SSA_NAME_VAR (dest)) == VAR_DECL
	   && DECL_HARD_REGISTER (SSA_NAME_VAR (dest)));
}


/* Given two SSA_NAMEs pointers ORIG and NEW such that we are copy
   propagating NEW into ORIG, consolidate aliasing information so that
   they both share the same memory tags.  */

static void
merge_alias_info (tree orig, tree new)
{
  tree new_sym = SSA_NAME_VAR (new);
  tree orig_sym = SSA_NAME_VAR (orig);
  var_ann_t new_ann = var_ann (new_sym);
  var_ann_t orig_ann = var_ann (orig_sym);

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (orig)));
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (new)));

#if defined ENABLE_CHECKING
  gcc_assert (lang_hooks.types_compatible_p (TREE_TYPE (orig),
					     TREE_TYPE (new)));

  /* If the pointed-to alias sets are different, these two pointers
     would never have the same memory tag.  In this case, NEW should
     not have been propagated into ORIG.  */
  gcc_assert (get_alias_set (TREE_TYPE (TREE_TYPE (new_sym)))
	      == get_alias_set (TREE_TYPE (TREE_TYPE (orig_sym))));
#endif

  /* Synchronize the type tags.  If both pointers had a tag and they
     are different, then something has gone wrong.  */
  if (new_ann->type_mem_tag == NULL_TREE)
    new_ann->type_mem_tag = orig_ann->type_mem_tag;
  else if (orig_ann->type_mem_tag == NULL_TREE)
    orig_ann->type_mem_tag = new_ann->type_mem_tag;
  else
    gcc_assert (new_ann->type_mem_tag == orig_ann->type_mem_tag);

#if defined ENABLE_CHECKING
  {
    struct ptr_info_def *orig_ptr_info = SSA_NAME_PTR_INFO (orig);
    struct ptr_info_def *new_ptr_info = SSA_NAME_PTR_INFO (new);

    if (orig_ptr_info
	&& new_ptr_info
	&& orig_ptr_info->name_mem_tag
	&& new_ptr_info->name_mem_tag
	&& orig_ptr_info->pt_vars
	&& new_ptr_info->pt_vars)
    {
      /* Note that pointer NEW may actually have a different set of
	 pointed-to variables.  However, since NEW is being
	 copy-propagated into ORIG, it must always be true that the
	 pointed-to set for pointer NEW is the same, or a subset, of
	 the pointed-to set for pointer ORIG.  If this isn't the case,
	 we shouldn't have been able to do the propagation of NEW into
	 ORIG.  */
      gcc_assert (bitmap_intersect_p (new_ptr_info->pt_vars,
				      orig_ptr_info->pt_vars));
    }
  }
#endif
}   


/* Common code for propagate_value and replace_exp.

   Replace use operand OP_P with VAL.  FOR_PROPAGATION indicates if the
   replacement is done to propagate a value or not.  */

static void
replace_exp_1 (use_operand_p op_p, tree val,
	       bool for_propagation ATTRIBUTE_UNUSED)
{
  tree op = USE_FROM_PTR (op_p);

#if defined ENABLE_CHECKING
  gcc_assert (!(for_propagation
		&& TREE_CODE (op) == SSA_NAME
		&& TREE_CODE (val) == SSA_NAME
		&& !may_propagate_copy (op, val)));
#endif

  if (TREE_CODE (val) == SSA_NAME)
    {
      if (TREE_CODE (op) == SSA_NAME && POINTER_TYPE_P (TREE_TYPE (op)))
	merge_alias_info (op, val);
      SET_USE (op_p, val);
    }
  else
    SET_USE (op_p, unsave_expr_now (val));
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the operand pointed by OP_P.

   Use this version for const/copy propagation as it will perform additional
   checks to ensure validity of the const/copy propagation.  */

void
propagate_value (use_operand_p op_p, tree val)
{
  replace_exp_1 (op_p, val, true);
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the tree pointed by OP_P.

   Use this version for const/copy propagation when SSA operands are not
   available.  It will perform the additional checks to ensure validity of
   the const/copy propagation, but will not update any operand information.
   Be sure to mark the stmt as modified.  */

void
propagate_tree_value (tree *op_p, tree val)
{
#if defined ENABLE_CHECKING
  gcc_assert (!(TREE_CODE (val) == SSA_NAME
		&& TREE_CODE (*op_p) == SSA_NAME
		&& !may_propagate_copy (*op_p, val)));
#endif

  if (TREE_CODE (val) == SSA_NAME)
    {
      if (TREE_CODE (*op_p) == SSA_NAME && POINTER_TYPE_P (TREE_TYPE (*op_p)))
	merge_alias_info (*op_p, val);
      *op_p = val;
    }
  else
    *op_p = unsave_expr_now (val);
}


/* Replace *OP_P with value VAL (assumed to be a constant or another SSA_NAME).

   Use this version when not const/copy propagating values.  For example,
   PRE uses this version when building expressions as they would appear
   in specific blocks taking into account actions of PHI nodes.  */

void
replace_exp (use_operand_p op_p, tree val)
{
  replace_exp_1 (op_p, val, false);
}
