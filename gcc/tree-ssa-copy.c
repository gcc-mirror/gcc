/* Const/copy propagation and SSA_NAME replacement support routines.
   Copyright (C) 2004 Free Software Foundation, Inc.

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
   a non-null memory tag that the memory tag be the same.   It is OK
   for one or both of the memory tags to be NULL.

   We also require tracking if a variable is dereferenced in a load or
   store operation.

   We enforce these requirements by having all copy propagation and
   replacements of one SSA_NAME with a different SSA_NAME to use the
   APIs defined in this file.  */

/* Given two SSA_NAMEs, replace the one pointed to by OP_P with VAR.

   If *OP_P is a pointer, copy the memory tag used originally by *OP_P into
   VAR.  This is needed in cases where VAR had never been dereferenced in the
   program.

   If FOR_PROPAGATION is true, then perform additional checks to ensure
   that const/copy propagation of var for *OP_P is valid.  */
   
static void
replace_ssa_names (tree *op_p,
		   tree var,
		   bool for_propagation ATTRIBUTE_UNUSED)
{
#if defined ENABLE_CHECKING
  if (for_propagation && !may_propagate_copy (*op_p, var))
    abort ();
#endif

  /* If VAR doesn't have a memory tag, copy the one from the original
     operand.  Also copy the dereferenced flags.  */
  if (POINTER_TYPE_P (TREE_TYPE (*op_p)))
    {
      var_ann_t new_ann = var_ann (SSA_NAME_VAR (var));
      var_ann_t orig_ann = var_ann (SSA_NAME_VAR (*op_p));

      if (new_ann->type_mem_tag == NULL_TREE)
	new_ann->type_mem_tag = orig_ann->type_mem_tag;
      else if (orig_ann->type_mem_tag == NULL_TREE)
	orig_ann->type_mem_tag = new_ann->type_mem_tag;
      else if (new_ann->type_mem_tag != orig_ann->type_mem_tag)
	abort ();
    }

  *op_p = var;
}

/* Common code for propagate_value and replace_exp.

   Replace *OP_P with VAL.  FOR_PROPAGATION indicates if the replacement
   is done to propagate a value or not.  */

static void
replace_exp_1 (tree *op_p, tree val, bool for_propagation)
{
  if (TREE_CODE (val) == SSA_NAME)
    {
      if (TREE_CODE (*op_p) == SSA_NAME)
	replace_ssa_names (op_p, val, for_propagation);
      else
	*op_p = val;
    }
  else
    *op_p = lhd_unsave_expr_now (val);
}

/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the operand pointed by OP_P.

   Use this version for const/copy propagation as it will perform additional
   checks to ensure validity of the const/copy propagation.  */

void
propagate_value (tree *op_p, tree val)
{
  replace_exp_1 (op_p, val, true);
}

/* Replace *OP_P with value VAL (assumed to be a constant or another SSA_NAME).

   Use this version when not const/copy propagating values.  For example,
   PRE uses this version when building expressions as they would appear
   in specific blocks taking into account actions of PHI nodes.  */

void
replace_exp (tree *op_p, tree val)
{
  replace_exp_1 (op_p, val, false);
}

/* Replace *OP_P in STMT with any known equivalent value for *OP_P from
   CONST_AND_COPIES.  */

static bool
cprop_operand (stmt_ann_t ann, tree *op_p, varray_type const_and_copies)
{
  bool may_have_exposed_new_symbols = false;
  tree val;

  /* If the operand has a known constant value or it is known to be a
     copy of some other variable, use the value or copy stored in
     CONST_AND_COPIES.  */
  val = VARRAY_TREE (const_and_copies, SSA_NAME_VERSION (*op_p));
  if (val)
    {
      tree op_type, val_type;

      /* Do not change the base variable in the virtual operand
	 tables.  That would make it impossible to reconstruct
	 the renamed virtual operand if we later modify this
	 statement.  Also only allow the new value to be an SSA_NAME
	 for propagation into virtual operands.  */
      if (!is_gimple_reg (*op_p)
	  && (get_virtual_var (val) != get_virtual_var (*op_p)
	      || TREE_CODE (val) != SSA_NAME))
	return false;

      /* Get the toplevel type of each operand.  */
      op_type = TREE_TYPE (*op_p);
      val_type = TREE_TYPE (val);

      /* While both types are pointers, get the type of the object
	 pointed to.  */
      while (POINTER_TYPE_P (op_type) && POINTER_TYPE_P (val_type))
	{
	  op_type = TREE_TYPE (op_type);
	  val_type = TREE_TYPE (val_type);
	}

      /* Make sure underlying types match before propagating a
	 constant by converting the constant to the proper type.  Note
	 that convert may return a non-gimple expression, in which case
	 we ignore this propagation opportunity.  */
     if (!lang_hooks.types_compatible_p (op_type, val_type)
           && TREE_CODE (val) != SSA_NAME)
	{
	  val = fold_convert (TREE_TYPE (*op_p), val);
	  if (!is_gimple_min_invariant (val)
	      && TREE_CODE (val) != SSA_NAME)
	    return false;
	}

      /* Certain operands are not allowed to be copy propagated due
	 to their interaction with exception handling and some GCC
	 extensions.  */
      if (TREE_CODE (val) == SSA_NAME
	  && !may_propagate_copy (*op_p, val))
	return false;

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, *op_p, dump_flags);
	  fprintf (dump_file, "' with %s '",
		   (TREE_CODE (val) != SSA_NAME ? "constant" : "variable"));
	  print_generic_expr (dump_file, val, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      /* If VAL is an ADDR_EXPR or a constant of pointer type, note
	 that we may have exposed a new symbol for SSA renaming.  */
      if (TREE_CODE (val) == ADDR_EXPR
	  || (POINTER_TYPE_P (TREE_TYPE (*op_p))
	      && is_gimple_min_invariant (val)))
	may_have_exposed_new_symbols = true;

      propagate_value (op_p, val);

      /* And note that we modified this statement.  This is now
	 safe, even if we changed virtual operands since we will
	 rescan the statement and rewrite its operands again.  */
      ann->modified = 1;
    }
  return may_have_exposed_new_symbols;
}

/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).  

   Propagate values from CONST_AND_COPIES into the uses, vuses and
   vdef_ops of STMT.  */

bool
cprop_into_stmt (tree stmt, varray_type const_and_copies)
{
  bool may_have_exposed_new_symbols = false;
  stmt_ann_t ann = stmt_ann (stmt);
  size_t i, num_uses, num_vuses, num_vdefs;
  vuse_optype vuses;
  vdef_optype vdefs;
  use_optype uses;

  uses = USE_OPS (ann);
  num_uses = NUM_USES (uses);
  for (i = 0; i < num_uses; i++)
    {
      tree *op_p = USE_OP_PTR (uses, i);
      if (TREE_CODE (*op_p) == SSA_NAME)
	may_have_exposed_new_symbols
	  |= cprop_operand (ann, op_p, const_and_copies);
    }

  vuses = VUSE_OPS (ann);
  num_vuses = NUM_VUSES (vuses);
  for (i = 0; i < num_vuses; i++)
    {
      tree *op_p = VUSE_OP_PTR (vuses, i);
      if (TREE_CODE (*op_p) == SSA_NAME)
	may_have_exposed_new_symbols
	  |= cprop_operand (ann, op_p, const_and_copies);
    }

  vdefs = VDEF_OPS (ann);
  num_vdefs = NUM_VDEFS (vdefs);
  for (i = 0; i < num_vdefs; i++)
    {
      tree *op_p = VDEF_OP_PTR (vdefs, i);
      if (TREE_CODE (*op_p) == SSA_NAME)
	may_have_exposed_new_symbols
	  |= cprop_operand (ann, op_p, const_and_copies);
    }
  return may_have_exposed_new_symbols;
}

/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).  

   NONZERO_VARS is the set SSA_NAMES known to have a nonzero value,
   even if we don't know their precise value.

   Propagate values from CONST_AND_COPIES and NONZERO_VARS into the PHI
   nodes of the successors of BB.  */

void
cprop_into_successor_phis (basic_block bb,
			   varray_type const_and_copies,
			   bitmap nonzero_vars)
{
  edge e;

  /* This can get rather expensive if the implementation is naive in
     how it finds the phi alternative associated with a particular edge.  */
  for (e = bb->succ; e; e = e->succ_next)
    {
      tree phi;
      int phi_num_args;
      int hint;

      /* If this is an abnormal edge, then we do not want to copy propagate
	 into the PHI alternative associated with this edge.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      phi = phi_nodes (e->dest);
      if (! phi)
	continue;

      /* There is no guarantee that for any two PHI nodes in a block that
	 the phi alternative associated with a particular edge will be
	 at the same index in the phi alternative array.

	 However, it is very likely they will be the same.  So we keep
	 track of the index of the alternative where we found the edge in
	 the previous phi node and check that index first in the next
	 phi node.  If that hint fails, then we actually search all
	 the entries.  */
      phi_num_args = PHI_NUM_ARGS (phi);
      hint = phi_num_args;
      for ( ; phi; phi = TREE_CHAIN (phi))
	{
	  int i;
	  tree new;
	  tree *orig_p;

	  /* If the hint is valid (!= phi_num_args), see if it points
	     us to the desired phi alternative.  */
	  if (hint != phi_num_args && PHI_ARG_EDGE (phi, hint) == e)
	    ;
	  else
	    {
	      /* The hint was either invalid or did not point to the
		 correct phi alternative.  Search all the alternatives
		 for the correct one.  Update the hint.  */
	      for (i = 0; i < phi_num_args; i++)
		if (PHI_ARG_EDGE (phi, i) == e)
		  break;
	      hint = i;
	    }

#ifdef ENABLE_CHECKING
	  /* If we did not find the proper alternative, then something is
	     horribly wrong.  */
	  if (hint == phi_num_args)
	    abort ();
#endif

	  /* The alternative may be associated with a constant, so verify
	     it is an SSA_NAME before doing anything with it.  */
	  orig_p = &PHI_ARG_DEF (phi, hint);
	  if (TREE_CODE (*orig_p) != SSA_NAME)
	    continue;

	  /* If the alternative is known to have a nonzero value, record
	     that fact in the PHI node itself for future use.  */
	  if (bitmap_bit_p (nonzero_vars, SSA_NAME_VERSION (*orig_p)))
	    PHI_ARG_NONZERO (phi, i) = true;

	  /* If we have *ORIG_P in our constant/copy table, then replace
	     ORIG_P with its value in our constant/copy table.  */
	  new = VARRAY_TREE (const_and_copies, SSA_NAME_VERSION (*orig_p));
	  if (new
	      && (TREE_CODE (new) == SSA_NAME
		  || is_gimple_min_invariant (new))
	      && may_propagate_copy (*orig_p, new))
	    propagate_value (orig_p, new);
	}
    }
}
