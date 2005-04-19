/* Conditional constant propagation pass for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   Adapted from original RTL SSA-CCP by Daniel Berlin <dberlin@dberlin.org>
   Adapted to GIMPLE trees by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Conditional constant propagation.

   References:

     Constant propagation with conditional branches,
     Wegman and Zadeck, ACM TOPLAS 13(2):181-210.

     Building an Optimizing Compiler,
     Robert Morgan, Butterworth-Heinemann, 1998, Section 8.9.

     Advanced Compiler Design and Implementation,
     Steven Muchnick, Morgan Kaufmann, 1997, Section 12.6  */

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
#include "tree-ssa-propagate.h"
#include "langhooks.h"


/* Possible lattice values.  */
typedef enum
{
  UNINITIALIZED = 0,
  UNDEFINED,
  UNKNOWN_VAL,
  CONSTANT,
  VARYING
} latticevalue;

/* Main structure for CCP.  Contains the lattice value and, if it's a
    constant, the constant value.  */
typedef struct
{
  latticevalue lattice_val;
  tree const_val;
} value;

/* This is used to track the current value of each variable.  */
static value *value_vector;


/* Dump lattice value VAL to file OUTF prefixed by PREFIX.  */

static void
dump_lattice_value (FILE *outf, const char *prefix, value val)
{
  switch (val.lattice_val)
    {
    case UNDEFINED:
      fprintf (outf, "%sUNDEFINED", prefix);
      break;
    case VARYING:
      fprintf (outf, "%sVARYING", prefix);
      break;
    case UNKNOWN_VAL:
      fprintf (outf, "%sUNKNOWN_VAL", prefix);
      break;
    case CONSTANT:
      fprintf (outf, "%sCONSTANT ", prefix);
      print_generic_expr (outf, val.const_val, dump_flags);
      break;
    default:
      gcc_unreachable ();
    }
}


/* Return a default value for variable VAR using the following rules:

   1- Function arguments are considered VARYING.
   
   2- Global and static variables that are declared constant are
      considered CONSTANT.

   3- Any other virtually defined variable is considered UNKNOWN_VAL.

   4- Any other value is considered UNDEFINED.  This is useful when
      considering PHI nodes.  PHI arguments that are undefined do not
      change the constant value of the PHI node, which allows for more
      constants to be propagated.  */

static value
get_default_value (tree var)
{
  value val;
  tree sym;

  if (TREE_CODE (var) == SSA_NAME)
    sym = SSA_NAME_VAR (var);
  else
    {
      gcc_assert (DECL_P (var));
      sym = var;
    }

  val.lattice_val = UNDEFINED;
  val.const_val = NULL_TREE;

  if (TREE_CODE (var) == SSA_NAME
      && SSA_NAME_VALUE (var)
      && is_gimple_min_invariant (SSA_NAME_VALUE (var)))
    {
      val.lattice_val = CONSTANT;
      val.const_val = SSA_NAME_VALUE (var);
    }
  else if (TREE_CODE (sym) == PARM_DECL || TREE_THIS_VOLATILE (sym))
    {
      /* Function arguments and volatile variables are considered VARYING.  */
      val.lattice_val = VARYING;
    }
  else if (TREE_STATIC (sym))
    {
      /* Globals and static variables are considered UNKNOWN_VAL,
         unless they are declared 'const'.  */
      if (TREE_READONLY (sym)
	  && DECL_INITIAL (sym)
	  && is_gimple_min_invariant (DECL_INITIAL (sym)))
	{
	  val.lattice_val = CONSTANT;
	  val.const_val = DECL_INITIAL (sym);
	}
      else
        {
          val.const_val = NULL_TREE;
	  val.lattice_val = UNKNOWN_VAL;
	}
    }
  else if (!is_gimple_reg (sym))
    {
      val.const_val = NULL_TREE;
      val.lattice_val = UNKNOWN_VAL;
    }
  else
    {
      enum tree_code code;
      tree stmt = SSA_NAME_DEF_STMT (var);

      if (!IS_EMPTY_STMT (stmt))
        {
	  code = TREE_CODE (stmt);
	  if (code != MODIFY_EXPR && code != PHI_NODE)
	    val.lattice_val = VARYING;
	}
    }

  return val;
}

/* Get the constant value associated with variable VAR.  */

static value *
get_value (tree var)
{
  value *val;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  val = &value_vector[SSA_NAME_VERSION (var)];
  if (val->lattice_val == UNINITIALIZED)
    *val = get_default_value (var);

  return val;
}


/* Set the lattice value for variable VAR to VAL.  Return true if VAL
   is different from VAR's previous value.  */

static bool
set_lattice_value (tree var, value val)
{
  value *old = get_value (var);

  if (val.lattice_val == UNDEFINED)
    {
      /* CONSTANT->UNDEFINED is never a valid state transition.  */
      gcc_assert (old->lattice_val != CONSTANT);
	
      /* UNKNOWN_VAL->UNDEFINED is never a valid state transition.  */
      gcc_assert (old->lattice_val != UNKNOWN_VAL);

      /* VARYING->UNDEFINED is generally not a valid state transition,
	 except for values which are initialized to VARYING.  */
      gcc_assert (old->lattice_val != VARYING
		  || get_default_value (var).lattice_val == VARYING);
    }
  else if (val.lattice_val == CONSTANT)
    /* VARYING -> CONSTANT is an invalid state transition, except
	for objects which start off in a VARYING state.  */
    gcc_assert (old->lattice_val != VARYING
		|| get_default_value (var).lattice_val == VARYING);

  /* If the constant for VAR has changed, then this VAR is really varying.  */
  if (old->lattice_val == CONSTANT
      && val.lattice_val == CONSTANT
      && !simple_cst_equal (old->const_val, val.const_val))
    {
      val.lattice_val = VARYING;
      val.const_val = NULL_TREE;
    }

  if (old->lattice_val != val.lattice_val)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  dump_lattice_value (dump_file, "Lattice value changed to ", val);
	  fprintf (dump_file, ".  Adding definition to SSA edges.\n");
	}

      *old = val;
      return true;
    }

  return false;
}


/* Set the lattice value for the variable VAR to VARYING.  */

static void
def_to_varying (tree var)
{
  value val;
  val.lattice_val = VARYING;
  val.const_val = NULL_TREE;
  set_lattice_value (var, val);
}


/* Return the likely latticevalue for STMT.

   If STMT has no operands, then return CONSTANT.

   Else if any operands of STMT are undefined, then return UNDEFINED.

   Else if any operands of STMT are constants, then return CONSTANT.

   Else return VARYING.  */

static latticevalue
likely_value (tree stmt)
{
  vuse_optype vuses;
  int found_constant = 0;
  stmt_ann_t ann;
  tree use;
  ssa_op_iter iter;

  /* If the statement makes aliased loads or has volatile operands, it
     won't fold to a constant value.  */
  ann = stmt_ann (stmt);
  if (ann->makes_aliased_loads || ann->has_volatile_ops)
    return VARYING;

  /* A CALL_EXPR is assumed to be varying.  This may be overly conservative,
     in the presence of const and pure calls.  */
  if (get_call_expr_in (stmt) != NULL_TREE)
    return VARYING;

  get_stmt_operands (stmt);

  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      value *val = get_value (use);

      if (val->lattice_val == UNDEFINED)
	return UNDEFINED;

      if (val->lattice_val == CONSTANT)
	found_constant = 1;
    }
    
  vuses = VUSE_OPS (ann);
  
  if (NUM_VUSES (vuses))
    {
      tree vuse = VUSE_OP (vuses, 0);
      value *val = get_value (vuse);
      
      if (val->lattice_val == UNKNOWN_VAL)
        return UNKNOWN_VAL;
	
      /* There should be no VUSE operands that are UNDEFINED.  */
      gcc_assert (val->lattice_val != UNDEFINED);
	
      if (val->lattice_val == CONSTANT)
	found_constant = 1;
    }

  return ((found_constant || (!USE_OPS (ann) && !vuses)) ? CONSTANT : VARYING);
}


/* Function indicating whether we ought to include information for VAR
   when calculating immediate uses.  */

static bool
need_imm_uses_for (tree var)
{
  return get_value (var)->lattice_val != VARYING;
}


/* Initialize local data structures for CCP.  */

static void
ccp_initialize (void)
{
  basic_block bb;
  sbitmap is_may_def;

  value_vector = (value *) xmalloc (num_ssa_names * sizeof (value));
  memset (value_vector, 0, num_ssa_names * sizeof (value));

  /* Set of SSA_NAMEs that are defined by a V_MAY_DEF.  */
  is_may_def = sbitmap_alloc (num_ssa_names);
  sbitmap_zero (is_may_def);

  /* Initialize simulation flags for PHI nodes and statements.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;

      /* Mark all V_MAY_DEF operands VARYING.  */
      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
        {
	  bool is_varying = false;
	  tree stmt = bsi_stmt (i);
	  ssa_op_iter iter;
	  tree def;

	  get_stmt_operands (stmt);

	  /* Get the default value for each DEF and V_MUST_DEF.  */
	  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, 
				     (SSA_OP_DEF | SSA_OP_VMUSTDEF))
	    {
	      if (get_value (def)->lattice_val == VARYING)
		is_varying = true;
	    }

	  /* Mark all V_MAY_DEF operands VARYING.  */
	  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_VMAYDEF)
	    {
	      get_value (def)->lattice_val = VARYING;
	      SET_BIT (is_may_def, SSA_NAME_VERSION (def));
	    }

	  /* Statements other than MODIFY_EXPR, COND_EXPR and
	     SWITCH_EXPR are not interesting for constant propagation.
	     Mark them VARYING.  */
	  if (TREE_CODE (stmt) != MODIFY_EXPR
	      && TREE_CODE (stmt) != COND_EXPR
	      && TREE_CODE (stmt) != SWITCH_EXPR)
	    is_varying = true;

	  DONT_SIMULATE_AGAIN (stmt) = is_varying;
	}
    }

  /* Now process PHI nodes.  */
  FOR_EACH_BB (bb)
    {
      tree phi, var;
      int x;

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  value *val = get_value (PHI_RESULT (phi));

	  for (x = 0; x < PHI_NUM_ARGS (phi); x++)
	    {
	      var = PHI_ARG_DEF (phi, x);

	      /* If one argument has a V_MAY_DEF, the result is
		 VARYING.  */
	      if (TREE_CODE (var) == SSA_NAME)
		{
		  if (TEST_BIT (is_may_def, SSA_NAME_VERSION (var)))
		    {
		      val->lattice_val = VARYING;
		      SET_BIT (is_may_def, SSA_NAME_VERSION (PHI_RESULT (phi)));
		      break;
		    }
		}
	    }

	  DONT_SIMULATE_AGAIN (phi) = (val->lattice_val == VARYING);
	}
    }

  sbitmap_free (is_may_def);

  /* Compute immediate uses for variables we care about.  */
  compute_immediate_uses (TDFA_USE_OPS | TDFA_USE_VOPS, need_imm_uses_for);
}


/* Replace USE references in statement STMT with their immediate reaching
   definition.  Return true if at least one reference was replaced.  If
   REPLACED_ADDRESSES_P is given, it will be set to true if an address
   constant was replaced.  */

static bool
replace_uses_in (tree stmt, bool *replaced_addresses_p)
{
  bool replaced = false;
  use_operand_p use;
  ssa_op_iter iter;

  if (replaced_addresses_p)
    *replaced_addresses_p = false;

  get_stmt_operands (stmt);

  FOR_EACH_SSA_USE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      tree tuse = USE_FROM_PTR (use);
      value *val = get_value (tuse);

      if (val->lattice_val != CONSTANT)
	continue;

      if (TREE_CODE (stmt) == ASM_EXPR
	  && !may_propagate_copy_into_asm (tuse))
	continue;

      SET_USE (use, val->const_val);

      replaced = true;
      if (POINTER_TYPE_P (TREE_TYPE (tuse)) && replaced_addresses_p)
	*replaced_addresses_p = true;
    }

  return replaced;
}


/* Replace the VUSE references in statement STMT with its immediate reaching
   definition.  Return true if the reference was replaced.  If
   REPLACED_ADDRESSES_P is given, it will be set to true if an address
   constant was replaced.  */

static bool
replace_vuse_in (tree stmt, bool *replaced_addresses_p)
{
  bool replaced = false;
  vuse_optype vuses;
  use_operand_p vuse;
  value *val;

  if (replaced_addresses_p)
    *replaced_addresses_p = false;

  get_stmt_operands (stmt);

  vuses = STMT_VUSE_OPS (stmt);

  if (NUM_VUSES (vuses) != 1)
    return false;

  vuse = VUSE_OP_PTR (vuses, 0);
  val = get_value (USE_FROM_PTR (vuse));

  if (val->lattice_val == CONSTANT
      && TREE_CODE (stmt) == MODIFY_EXPR
      && DECL_P (TREE_OPERAND (stmt, 1))
      && TREE_OPERAND (stmt, 1) == SSA_NAME_VAR (USE_FROM_PTR (vuse)))
    {
      TREE_OPERAND (stmt, 1) = val->const_val;
      replaced = true;
      if (POINTER_TYPE_P (TREE_TYPE (USE_FROM_PTR (vuse))) 
          && replaced_addresses_p)
        *replaced_addresses_p = true;
    }

  return replaced;
}


/* Perform final substitution and folding.  After this pass the program
   should still be in SSA form.  */

static void
substitute_and_fold (void)
{
  basic_block bb;
  unsigned int i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "\nSubstituing constants and folding statements\n\n");

  /* Substitute constants in every statement of every basic block.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;
      tree phi;

      /* Propagate our known constants into PHI nodes.  */
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  int i;

	  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
	    {
	      value *new_val;
	      use_operand_p orig_p = PHI_ARG_DEF_PTR (phi, i);
	      tree orig = USE_FROM_PTR (orig_p);

	      if (! SSA_VAR_P (orig))
		break;

	      new_val = get_value (orig);
	      if (new_val->lattice_val == CONSTANT
		  && may_propagate_copy (orig, new_val->const_val))
		SET_USE (orig_p, new_val->const_val);
	    }
	}

      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
	{
          bool replaced_address;
	  tree stmt = bsi_stmt (i);

	  /* Skip statements that have been folded already.  */
	  if (stmt_modified_p (stmt) || !is_exec_stmt (stmt))
	    continue;

	  /* Replace the statement with its folded version and mark it
	     folded.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Line %d: replaced ", get_lineno (stmt));
	      print_generic_stmt (dump_file, stmt, TDF_SLIM);
	    }

	  if (replace_uses_in (stmt, &replaced_address)
	      || replace_vuse_in (stmt, &replaced_address))
	    {
	      bool changed = fold_stmt (bsi_stmt_ptr (i));
	      stmt = bsi_stmt(i);

	      /* If we folded a builtin function, we'll likely
		 need to rename VDEFs.  */
	      if (replaced_address || changed)
		mark_new_vars_to_rename (stmt, vars_to_rename);

              /* If we cleaned up EH information from the statement,
                 remove EH edges.  */
	      if (maybe_clean_eh_stmt (stmt))
		tree_purge_dead_eh_edges (bb);

	      modify_stmt (stmt);
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " with ");
	      print_generic_stmt (dump_file, stmt, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  /* And transfer what we learned from VALUE_VECTOR into the
     SSA_NAMEs themselves.  This probably isn't terribly important
     since we probably constant propagated the values to their
     use sites above.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      value *value;

      if (!name)
	continue;

      value = get_value (name);
      if (value->lattice_val == CONSTANT
          && is_gimple_reg (name)
	  && is_gimple_min_invariant (value->const_val))
	SSA_NAME_VALUE (name) = value->const_val;
    }
}


/* Free allocated storage.  */

static void
ccp_finalize (void)
{
  /* Perform substitutions based on the known constant values.  */
  substitute_and_fold ();

  free (value_vector);
}



/* Compute the meet operator between VAL1 and VAL2:

   		any  M UNDEFINED     = any
		any  M VARYING       = VARYING
		any  M UNKNOWN_VAL   = UNKNOWN_VAL
		Ci   M Cj	     = Ci	if (i == j)
		Ci   M Cj	     = VARYING	if (i != j)  */
static value
ccp_lattice_meet (value val1, value val2)
{
  value result;

  /* any M UNDEFINED = any.  */
  if (val1.lattice_val == UNDEFINED)
    return val2;
  else if (val2.lattice_val == UNDEFINED)
    return val1;

  /* any M VARYING = VARYING.  */
  if (val1.lattice_val == VARYING || val2.lattice_val == VARYING)
    {
      result.lattice_val = VARYING;
      result.const_val = NULL_TREE;
      return result;
    }

  /* any M UNKNOWN_VAL = UNKNOWN_VAL.  */
  if (val1.lattice_val == UNKNOWN_VAL 
      || val2.lattice_val == UNKNOWN_VAL)
    {
      result.lattice_val = UNKNOWN_VAL;
      result.const_val = NULL_TREE;
      return result;
    }

  /* Ci M Cj = Ci	if (i == j)
     Ci M Cj = VARYING	if (i != j)  */
  if (simple_cst_equal (val1.const_val, val2.const_val) == 1)
    {
      result.lattice_val = CONSTANT;
      result.const_val = val1.const_val;
    }
  else
    {
      result.lattice_val = VARYING;
      result.const_val = NULL_TREE;
    }

  return result;
}


/* Loop through the PHI_NODE's parameters for BLOCK and compare their
   lattice values to determine PHI_NODE's lattice value.  The value of a
   PHI node is determined calling ccp_lattice_meet() with all the arguments
   of the PHI node that are incoming via executable edges.  */

static enum ssa_prop_result
ccp_visit_phi_node (tree phi)
{
  value new_val, *old_val;
  int i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_generic_expr (dump_file, phi, dump_flags);
    }

  old_val = get_value (PHI_RESULT (phi));
  switch (old_val->lattice_val)
    {
    case VARYING:
      return SSA_PROP_NOT_INTERESTING;

    case CONSTANT:
      new_val = *old_val;
      break;

    case UNKNOWN_VAL:
      /* To avoid the default value of UNKNOWN_VAL overriding
         that of its possible constant arguments, temporarily
	 set the PHI node's default lattice value to be 
	 UNDEFINED.  If the PHI node's old value was UNKNOWN_VAL and
	 the new value is UNDEFINED, then we prevent the invalid
	 transition by not calling set_lattice_value.  */
      new_val.lattice_val = UNDEFINED;
      new_val.const_val = NULL_TREE;
      break;

    case UNDEFINED:
    case UNINITIALIZED:
      new_val.lattice_val = UNDEFINED;
      new_val.const_val = NULL_TREE;
      break;

    default:
      gcc_unreachable ();
    }

  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
    {
      /* Compute the meet operator over all the PHI arguments.  */
      edge e = PHI_ARG_EDGE (phi, i);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
	      "\n    Argument #%d (%d -> %d %sexecutable)\n",
	      i, e->src->index, e->dest->index,
	      (e->flags & EDGE_EXECUTABLE) ? "" : "not ");
	}

      /* If the incoming edge is executable, Compute the meet operator for
	 the existing value of the PHI node and the current PHI argument.  */
      if (e->flags & EDGE_EXECUTABLE)
	{
	  tree rdef = PHI_ARG_DEF (phi, i);
	  value *rdef_val, val;

	  if (is_gimple_min_invariant (rdef))
	    {
	      val.lattice_val = CONSTANT;
	      val.const_val = rdef;
	      rdef_val = &val;
	    }
	  else
	    rdef_val = get_value (rdef);

	  new_val = ccp_lattice_meet (new_val, *rdef_val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\t");
	      print_generic_expr (dump_file, rdef, dump_flags);
	      dump_lattice_value (dump_file, "\tValue: ", *rdef_val);
	      fprintf (dump_file, "\n");
	    }

	  if (new_val.lattice_val == VARYING)
	    break;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_lattice_value (dump_file, "\n    PHI node value: ", new_val);
      fprintf (dump_file, "\n\n");
    }

  /* Check for an invalid change from UNKNOWN_VAL to UNDEFINED.  */
  if (old_val->lattice_val == UNKNOWN_VAL
      && new_val.lattice_val == UNDEFINED)
    return SSA_PROP_NOT_INTERESTING;

  /* Otherwise, make the transition to the new value.  */
  if (set_lattice_value (PHI_RESULT (phi), new_val))
    {
      if (new_val.lattice_val == VARYING)
	return SSA_PROP_VARYING;
      else
	return SSA_PROP_INTERESTING;
    }
  else
    return SSA_PROP_NOT_INTERESTING;
}


/* CCP specific front-end to the non-destructive constant folding
   routines.

   Attempt to simplify the RHS of STMT knowing that one or more
   operands are constants.

   If simplification is possible, return the simplified RHS,
   otherwise return the original RHS.  */

static tree
ccp_fold (tree stmt)
{
  tree rhs = get_rhs (stmt);
  enum tree_code code = TREE_CODE (rhs);
  enum tree_code_class kind = TREE_CODE_CLASS (code);
  tree retval = NULL_TREE;
  vuse_optype vuses;
  
  vuses = STMT_VUSE_OPS (stmt);

  /* If the RHS is just a variable, then that variable must now have
     a constant value that we can return directly.  */
  if (TREE_CODE (rhs) == SSA_NAME)
    return get_value (rhs)->const_val;
  else if (DECL_P (rhs) 
           && NUM_VUSES (vuses) == 1
           && rhs == SSA_NAME_VAR (VUSE_OP (vuses, 0)))
    return get_value (VUSE_OP (vuses, 0))->const_val;

  /* Unary operators.  Note that we know the single operand must
     be a constant.  So this should almost always return a
     simplified RHS.  */
  if (kind == tcc_unary)
    {
      /* Handle unary operators which can appear in GIMPLE form.  */
      tree op0 = TREE_OPERAND (rhs, 0);

      /* Simplify the operand down to a constant.  */
      if (TREE_CODE (op0) == SSA_NAME)
	{
	  value *val = get_value (op0);
	  if (val->lattice_val == CONSTANT)
	    op0 = get_value (op0)->const_val;
	}

      retval = fold_unary_to_constant (code, TREE_TYPE (rhs), op0);

      /* If we folded, but did not create an invariant, then we can not
	 use this expression.  */
      if (retval && ! is_gimple_min_invariant (retval))
	return NULL;

      /* If we could not fold the expression, but the arguments are all
         constants and gimple values, then build and return the new
	 expression. 

	 In some cases the new expression is still something we can
	 use as a replacement for an argument.  This happens with
	 NOP conversions of types for example.

	 In other cases the new expression can not be used as a
	 replacement for an argument (as it would create non-gimple
	 code).  But the new expression can still be used to derive
	 other constants.  */
      if (! retval && is_gimple_min_invariant (op0))
	return build1 (code, TREE_TYPE (rhs), op0);
    }

  /* Binary and comparison operators.  We know one or both of the
     operands are constants.  */
  else if (kind == tcc_binary
           || kind == tcc_comparison
           || code == TRUTH_AND_EXPR
           || code == TRUTH_OR_EXPR
           || code == TRUTH_XOR_EXPR)
    {
      /* Handle binary and comparison operators that can appear in
         GIMPLE form.  */
      tree op0 = TREE_OPERAND (rhs, 0);
      tree op1 = TREE_OPERAND (rhs, 1);

      /* Simplify the operands down to constants when appropriate.  */
      if (TREE_CODE (op0) == SSA_NAME)
	{
	  value *val = get_value (op0);
	  if (val->lattice_val == CONSTANT)
	    op0 = val->const_val;
	}

      if (TREE_CODE (op1) == SSA_NAME)
	{
	  value *val = get_value (op1);
	  if (val->lattice_val == CONSTANT)
	    op1 = val->const_val;
	}

      retval = fold_binary_to_constant (code, TREE_TYPE (rhs), op0, op1);

      /* If we folded, but did not create an invariant, then we can not
	 use this expression.  */
      if (retval && ! is_gimple_min_invariant (retval))
	return NULL;
      
      /* If we could not fold the expression, but the arguments are all
         constants and gimple values, then build and return the new
	 expression. 

	 In some cases the new expression is still something we can
	 use as a replacement for an argument.  This happens with
	 NOP conversions of types for example.

	 In other cases the new expression can not be used as a
	 replacement for an argument (as it would create non-gimple
	 code).  But the new expression can still be used to derive
	 other constants.  */
      if (! retval
	  && is_gimple_min_invariant (op0)
	  && is_gimple_min_invariant (op1))
	return build (code, TREE_TYPE (rhs), op0, op1);
    }

  /* We may be able to fold away calls to builtin functions if their
     arguments are constants.  */
  else if (code == CALL_EXPR
	   && TREE_CODE (TREE_OPERAND (rhs, 0)) == ADDR_EXPR
	   && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (rhs, 0), 0))
	       == FUNCTION_DECL)
	   && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (rhs, 0), 0)))
    {
      use_optype uses = STMT_USE_OPS (stmt);
      if (NUM_USES (uses) != 0)
	{
	  tree *orig;
	  size_t i;

	  /* Preserve the original values of every operand.  */
	  orig = xmalloc (sizeof (tree) * NUM_USES (uses));
	  for (i = 0; i < NUM_USES (uses); i++)
	    orig[i] = USE_OP (uses, i);

	  /* Substitute operands with their values and try to fold.  */
	  replace_uses_in (stmt, NULL);
	  retval = fold_builtin (rhs, false);

	  /* Restore operands to their original form.  */
	  for (i = 0; i < NUM_USES (uses); i++)
	    SET_USE_OP (uses, i, orig[i]);
	  free (orig);
	}
    }
  else
    return rhs;

  /* If we got a simplified form, see if we need to convert its type.  */
  if (retval)
    return fold_convert (TREE_TYPE (rhs), retval);

  /* No simplification was possible.  */
  return rhs;
}


/* Evaluate statement STMT.  */

static value
evaluate_stmt (tree stmt)
{
  value val;
  tree simplified;
  latticevalue likelyvalue = likely_value (stmt);

  /* If the statement is likely to have a CONSTANT result, then try
     to fold the statement to determine the constant value.  */
  if (likelyvalue == CONSTANT)
    simplified = ccp_fold (stmt);
  /* If the statement is likely to have a VARYING result, then do not
     bother folding the statement.  */
  else if (likelyvalue == VARYING)
    simplified = get_rhs (stmt);
  /* Otherwise the statement is likely to have an UNDEFINED value and
     there will be nothing to do.  */
  else
    simplified = NULL_TREE;

  if (simplified && is_gimple_min_invariant (simplified))
    {
      /* The statement produced a constant value.  */
      val.lattice_val = CONSTANT;
      val.const_val = simplified;
    }
  else
    {
      /* The statement produced a nonconstant value.  If the statement
         had undefined or virtual operands, then the result of the 
	 statement should be undefined or virtual respectively.  
	 Else the result of the statement is VARYING.  */
      val.lattice_val = (likelyvalue == UNDEFINED ? UNDEFINED : VARYING);
      val.lattice_val = (likelyvalue == UNKNOWN_VAL 
                           ? UNKNOWN_VAL : val.lattice_val);
      val.const_val = NULL_TREE;
    }

  return val;
}


/* Visit the assignment statement STMT.  Set the value of its LHS to the
   value computed by the RHS and store LHS in *OUTPUT_P.  */

static enum ssa_prop_result
visit_assignment (tree stmt, tree *output_p)
{
  value val;
  tree lhs, rhs;
  vuse_optype vuses;
  v_must_def_optype v_must_defs;

  lhs = TREE_OPERAND (stmt, 0);
  rhs = TREE_OPERAND (stmt, 1);
  vuses = STMT_VUSE_OPS (stmt);
  v_must_defs = STMT_V_MUST_DEF_OPS (stmt);

  gcc_assert (NUM_V_MAY_DEFS (STMT_V_MAY_DEF_OPS (stmt)) == 0);
  gcc_assert (NUM_V_MUST_DEFS (v_must_defs) == 1
	      || TREE_CODE (lhs) == SSA_NAME);

  /* We require the SSA version number of the lhs for the value_vector.
     Make sure we have it.  */
  if (TREE_CODE (lhs) != SSA_NAME)
    {
      /* If we make it here, then stmt only has one definition:
         a V_MUST_DEF.  */
      lhs = V_MUST_DEF_RESULT (v_must_defs, 0);
    }

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      /* For a simple copy operation, we copy the lattice values.  */
      value *nval = get_value (rhs);
      val = *nval;
    }
  else if (DECL_P (rhs) 
           && NUM_VUSES (vuses) == 1
           && rhs == SSA_NAME_VAR (VUSE_OP (vuses, 0)))
    {
      /* Same as above, but the rhs is not a gimple register and yet
        has a known VUSE.  */
      value *nval = get_value (VUSE_OP (vuses, 0));
      val = *nval;
    }
  else
    /* Evaluate the statement.  */
      val = evaluate_stmt (stmt);

  /* If the original LHS was a VIEW_CONVERT_EXPR, modify the constant
     value to be a VIEW_CONVERT_EXPR of the old constant value.

     ??? Also, if this was a definition of a bitfield, we need to widen
     the constant value into the type of the destination variable.  This
     should not be necessary if GCC represented bitfields properly.  */
  {
    tree orig_lhs = TREE_OPERAND (stmt, 0);

    if (TREE_CODE (orig_lhs) == VIEW_CONVERT_EXPR
	&& val.lattice_val == CONSTANT)
      {
	tree w = fold (build1 (VIEW_CONVERT_EXPR,
			       TREE_TYPE (TREE_OPERAND (orig_lhs, 0)),
			       val.const_val));

	orig_lhs = TREE_OPERAND (orig_lhs, 1);
	if (w && is_gimple_min_invariant (w))
	  val.const_val = w;
	else
	  {
	    val.lattice_val = VARYING;
	    val.const_val = NULL;
	  }
      }

    if (val.lattice_val == CONSTANT
	&& TREE_CODE (orig_lhs) == COMPONENT_REF
	&& DECL_BIT_FIELD (TREE_OPERAND (orig_lhs, 1)))
      {
	tree w = widen_bitfield (val.const_val, TREE_OPERAND (orig_lhs, 1),
				 orig_lhs);

	if (w && is_gimple_min_invariant (w))
	  val.const_val = w;
	else
	  {
	    val.lattice_val = VARYING;
	    val.const_val = NULL;
	  }
      }
  }

  /* If LHS is not a gimple register, then it cannot take on an
     UNDEFINED value.  */
  if (!is_gimple_reg (SSA_NAME_VAR (lhs)) 
      && val.lattice_val == UNDEFINED)
    val.lattice_val = UNKNOWN_VAL;      

  /* Set the lattice value of the statement's output.  */
  if (set_lattice_value (lhs, val))
    {
      *output_p = lhs;
      if (val.lattice_val == VARYING)
	return SSA_PROP_VARYING;
      else
	return SSA_PROP_INTERESTING;
    }
  else
    return SSA_PROP_NOT_INTERESTING;
}


/* Visit the conditional statement STMT.  Return SSA_PROP_INTERESTING
   if it can determine which edge will be taken.  Otherwise, return
   SSA_PROP_VARYING.  */

static enum ssa_prop_result
visit_cond_stmt (tree stmt, edge *taken_edge_p)
{
  value val;
  basic_block block;

  block = bb_for_stmt (stmt);
  val = evaluate_stmt (stmt);

  /* Find which edge out of the conditional block will be taken and add it
     to the worklist.  If no single edge can be determined statically,
     return SSA_PROP_VARYING to feed all the outgoing edges to the
     propagation engine.  */
  *taken_edge_p = val.const_val ? find_taken_edge (block, val.const_val) : 0;
  if (*taken_edge_p)
    return SSA_PROP_INTERESTING;
  else
    return SSA_PROP_VARYING;
}


/* Evaluate statement STMT.  If the statement produces an output value and
   its evaluation changes the lattice value of its output, return
   SSA_PROP_INTERESTING and set *OUTPUT_P to the SSA_NAME holding the
   output value.
   
   If STMT is a conditional branch and we can determine its truth
   value, set *TAKEN_EDGE_P accordingly.  If STMT produces a varying
   value, return SSA_PROP_VARYING.  */

static enum ssa_prop_result
ccp_visit_stmt (tree stmt, edge *taken_edge_p, tree *output_p)
{
  stmt_ann_t ann;
  v_may_def_optype v_may_defs;
  v_must_def_optype v_must_defs;
  tree def;
  ssa_op_iter iter;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting statement: ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  ann = stmt_ann (stmt);

  v_must_defs = V_MUST_DEF_OPS (ann);
  v_may_defs = V_MAY_DEF_OPS (ann);
  if (TREE_CODE (stmt) == MODIFY_EXPR
      && NUM_V_MAY_DEFS (v_may_defs) == 0
      && (NUM_V_MUST_DEFS (v_must_defs) == 1
          || TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME))
    {
      /* If the statement is an assignment that produces a single
	 output value, evaluate its RHS to see if the lattice value of
	 its output has changed.  */
      return visit_assignment (stmt, output_p);
    }
  else if (TREE_CODE (stmt) == COND_EXPR || TREE_CODE (stmt) == SWITCH_EXPR)
    {
      /* If STMT is a conditional branch, see if we can determine
	 which branch will be taken.  */
      return visit_cond_stmt (stmt, taken_edge_p);
    }

  /* Any other kind of statement is not interesting for constant
     propagation and, therefore, not worth simulating.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "No interesting values produced.  Marked VARYING.\n");

  /* Definitions made by statements other than assignments to
     SSA_NAMEs represent unknown modifications to their outputs.
     Mark them VARYING.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
    def_to_varying (def);

  /* Mark all V_MAY_DEF operands VARYING.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_VMAYDEF)
    def_to_varying (def);

  return SSA_PROP_VARYING;
}


/* Main entry point for SSA Conditional Constant Propagation.

   [ DESCRIBE MAIN ALGORITHM HERE ]  */

static void
execute_ssa_ccp (void)
{
  ccp_initialize ();
  ssa_propagate (ccp_visit_stmt, ccp_visit_phi_node);
  ccp_finalize ();
}


static bool
gate_ccp (void)
{
  return flag_tree_ccp != 0;
}


struct tree_opt_pass pass_ccp = 
{
  "ccp",				/* name */
  gate_ccp,				/* gate */
  execute_ssa_ccp,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CCP,				/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_cleanup_cfg | TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa
    | TODO_verify_stmts,		/* todo_flags_finish */
  0					/* letter */
};


/* Given a constant value VAL for bitfield FIELD, and a destination
   variable VAR, return VAL appropriately widened to fit into VAR.  If
   FIELD is wider than HOST_WIDE_INT, NULL is returned.  */

tree
widen_bitfield (tree val, tree field, tree var)
{
  unsigned HOST_WIDE_INT var_size, field_size;
  tree wide_val;
  unsigned HOST_WIDE_INT mask;
  unsigned int i;

  /* We can only do this if the size of the type and field and VAL are
     all constants representable in HOST_WIDE_INT.  */
  if (!host_integerp (TYPE_SIZE (TREE_TYPE (var)), 1)
      || !host_integerp (DECL_SIZE (field), 1)
      || !host_integerp (val, 0))
    return NULL_TREE;

  var_size = tree_low_cst (TYPE_SIZE (TREE_TYPE (var)), 1);
  field_size = tree_low_cst (DECL_SIZE (field), 1);

  /* Give up if either the bitfield or the variable are too wide.  */
  if (field_size > HOST_BITS_PER_WIDE_INT || var_size > HOST_BITS_PER_WIDE_INT)
    return NULL_TREE;

  gcc_assert (var_size >= field_size);

  /* If the sign bit of the value is not set or the field's type is unsigned,
     just mask off the high order bits of the value.  */
  if (DECL_UNSIGNED (field)
      || !(tree_low_cst (val, 0) & (((HOST_WIDE_INT)1) << (field_size - 1))))
    {
      /* Zero extension.  Build a mask with the lower 'field_size' bits
	 set and a BIT_AND_EXPR node to clear the high order bits of
	 the value.  */
      for (i = 0, mask = 0; i < field_size; i++)
	mask |= ((HOST_WIDE_INT) 1) << i;

      wide_val = build2 (BIT_AND_EXPR, TREE_TYPE (var), val, 
			 build_int_cst (TREE_TYPE (var), mask));
    }
  else
    {
      /* Sign extension.  Create a mask with the upper 'field_size'
	 bits set and a BIT_IOR_EXPR to set the high order bits of the
	 value.  */
      for (i = 0, mask = 0; i < (var_size - field_size); i++)
	mask |= ((HOST_WIDE_INT) 1) << (var_size - i - 1);

      wide_val = build2 (BIT_IOR_EXPR, TREE_TYPE (var), val,
			 build_int_cst (TREE_TYPE (var), mask));
    }

  return fold (wide_val);
}


/* A subroutine of fold_stmt_r.  Attempts to fold *(A+O) to A[X].
   BASE is an array type.  OFFSET is a byte displacement.  ORIG_TYPE
   is the desired result type.  */

static tree
maybe_fold_offset_to_array_ref (tree base, tree offset, tree orig_type)
{
  tree min_idx, idx, elt_offset = integer_zero_node;
  tree array_type, elt_type, elt_size;

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
  if (!lang_hooks.types_compatible_p (orig_type, elt_type))
    return NULL_TREE;
	
  /* If OFFSET and ELT_OFFSET are zero, we don't care about the size of the
     element type (so we can use the alignment if it's not constant).
     Otherwise, compute the offset as an index by using a division.  If the
     division isn't exact, then don't do anything.  */
  elt_size = TYPE_SIZE_UNIT (elt_type);
  if (integer_zerop (offset))
    {
      if (TREE_CODE (elt_size) != INTEGER_CST)
	elt_size = size_int (TYPE_ALIGN (elt_type));

      idx = integer_zero_node;
    }
  else
    {
      unsigned HOST_WIDE_INT lquo, lrem;
      HOST_WIDE_INT hquo, hrem;

      if (TREE_CODE (elt_size) != INTEGER_CST
	  || div_and_round_double (TRUNC_DIV_EXPR, 1,
				   TREE_INT_CST_LOW (offset),
				   TREE_INT_CST_HIGH (offset),
				   TREE_INT_CST_LOW (elt_size),
				   TREE_INT_CST_HIGH (elt_size),
				   &lquo, &hquo, &lrem, &hrem)
	  || lrem || hrem)
	return NULL_TREE;

      idx = build_int_cst_wide (NULL_TREE, lquo, hquo);
    }

  /* Assume the low bound is zero.  If there is a domain type, get the
     low bound, if any, convert the index into that type, and add the
     low bound.  */
  min_idx = integer_zero_node;
  if (TYPE_DOMAIN (array_type))
    {
      if (TYPE_MIN_VALUE (TYPE_DOMAIN (array_type)))
	min_idx = TYPE_MIN_VALUE (TYPE_DOMAIN (array_type));
      else
	min_idx = fold_convert (TYPE_DOMAIN (array_type), min_idx);

      if (TREE_CODE (min_idx) != INTEGER_CST)
	return NULL_TREE;

      idx = fold_convert (TYPE_DOMAIN (array_type), idx);
      elt_offset = fold_convert (TYPE_DOMAIN (array_type), elt_offset);
    }

  if (!integer_zerop (min_idx))
    idx = int_const_binop (PLUS_EXPR, idx, min_idx, 0);
  if (!integer_zerop (elt_offset))
    idx = int_const_binop (PLUS_EXPR, idx, elt_offset, 0);

  return build (ARRAY_REF, orig_type, base, idx, min_idx,
		size_int (tree_low_cst (elt_size, 1)
			  / (TYPE_ALIGN_UNIT (elt_type))));
}


/* A subroutine of fold_stmt_r.  Attempts to fold *(S+O) to S.X.
   BASE is a record type.  OFFSET is a byte displacement.  ORIG_TYPE
   is the desired result type.  */
/* ??? This doesn't handle class inheritance.  */

static tree
maybe_fold_offset_to_component_ref (tree record_type, tree base, tree offset,
				    tree orig_type, bool base_is_ptr)
{
  tree f, t, field_type, tail_array_field, field_offset;

  if (TREE_CODE (record_type) != RECORD_TYPE
      && TREE_CODE (record_type) != UNION_TYPE
      && TREE_CODE (record_type) != QUAL_UNION_TYPE)
    return NULL_TREE;

  /* Short-circuit silly cases.  */
  if (lang_hooks.types_compatible_p (record_type, orig_type))
    return NULL_TREE;

  tail_array_field = NULL_TREE;
  for (f = TYPE_FIELDS (record_type); f ; f = TREE_CHAIN (f))
    {
      int cmp;

      if (TREE_CODE (f) != FIELD_DECL)
	continue;
      if (DECL_BIT_FIELD (f))
	continue;

      field_offset = byte_position (f);
      if (TREE_CODE (field_offset) != INTEGER_CST)
	continue;

      /* ??? Java creates "interesting" fields for representing base classes.
	 They have no name, and have no context.  With no context, we get into
	 trouble with nonoverlapping_component_refs_p.  Skip them.  */
      if (!DECL_FIELD_CONTEXT (f))
	continue;

      /* The previous array field isn't at the end.  */
      tail_array_field = NULL_TREE;

      /* Check to see if this offset overlaps with the field.  */
      cmp = tree_int_cst_compare (field_offset, offset);
      if (cmp > 0)
	continue;

      field_type = TREE_TYPE (f);

      /* Here we exactly match the offset being checked.  If the types match,
	 then we can return that field.  */
      if (cmp == 0
	  && lang_hooks.types_compatible_p (orig_type, field_type))
	{
	  if (base_is_ptr)
	    base = build1 (INDIRECT_REF, record_type, base);
	  t = build (COMPONENT_REF, field_type, base, f, NULL_TREE);
	  return t;
	}
      
      /* Don't care about offsets into the middle of scalars.  */
      if (!AGGREGATE_TYPE_P (field_type))
	continue;

      /* Check for array at the end of the struct.  This is often
	 used as for flexible array members.  We should be able to
	 turn this into an array access anyway.  */
      if (TREE_CODE (field_type) == ARRAY_TYPE)
	tail_array_field = f;

      /* Check the end of the field against the offset.  */
      if (!DECL_SIZE_UNIT (f)
	  || TREE_CODE (DECL_SIZE_UNIT (f)) != INTEGER_CST)
	continue;
      t = int_const_binop (MINUS_EXPR, offset, field_offset, 1);
      if (!tree_int_cst_lt (t, DECL_SIZE_UNIT (f)))
	continue;

      /* If we matched, then set offset to the displacement into
	 this field.  */
      offset = t;
      goto found;
    }

  if (!tail_array_field)
    return NULL_TREE;

  f = tail_array_field;
  field_type = TREE_TYPE (f);
  offset = int_const_binop (MINUS_EXPR, offset, byte_position (f), 1);

 found:
  /* If we get here, we've got an aggregate field, and a possibly 
     nonzero offset into them.  Recurse and hope for a valid match.  */
  if (base_is_ptr)
    base = build1 (INDIRECT_REF, record_type, base);
  base = build (COMPONENT_REF, field_type, base, f, NULL_TREE);

  t = maybe_fold_offset_to_array_ref (base, offset, orig_type);
  if (t)
    return t;
  return maybe_fold_offset_to_component_ref (field_type, base, offset,
					     orig_type, false);
}


/* A subroutine of fold_stmt_r.  Attempt to simplify *(BASE+OFFSET).
   Return the simplified expression, or NULL if nothing could be done.  */

static tree
maybe_fold_stmt_indirect (tree expr, tree base, tree offset)
{
  tree t;

  /* We may well have constructed a double-nested PLUS_EXPR via multiple
     substitutions.  Fold that down to one.  Remove NON_LVALUE_EXPRs that
     are sometimes added.  */
  base = fold (base);
  STRIP_NOPS (base);
  TREE_OPERAND (expr, 0) = base;

  /* One possibility is that the address reduces to a string constant.  */
  t = fold_read_from_constant_string (expr);
  if (t)
    return t;

  /* Add in any offset from a PLUS_EXPR.  */
  if (TREE_CODE (base) == PLUS_EXPR)
    {
      tree offset2;

      offset2 = TREE_OPERAND (base, 1);
      if (TREE_CODE (offset2) != INTEGER_CST)
	return NULL_TREE;
      base = TREE_OPERAND (base, 0);

      offset = int_const_binop (PLUS_EXPR, offset, offset2, 1);
    }

  if (TREE_CODE (base) == ADDR_EXPR)
    {
      /* Strip the ADDR_EXPR.  */
      base = TREE_OPERAND (base, 0);

      /* Fold away CONST_DECL to its value, if the type is scalar.  */
      if (TREE_CODE (base) == CONST_DECL
	  && is_gimple_min_invariant (DECL_INITIAL (base)))
	return DECL_INITIAL (base);

      /* Try folding *(&B+O) to B[X].  */
      t = maybe_fold_offset_to_array_ref (base, offset, TREE_TYPE (expr));
      if (t)
	return t;

      /* Try folding *(&B+O) to B.X.  */
      t = maybe_fold_offset_to_component_ref (TREE_TYPE (base), base, offset,
					      TREE_TYPE (expr), false);
      if (t)
	return t;

      /* Fold *&B to B.  We can only do this if EXPR is the same type
	 as BASE.  We can't do this if EXPR is the element type of an array
	 and BASE is the array.  */
      if (integer_zerop (offset)
	  && lang_hooks.types_compatible_p (TREE_TYPE (base),
					    TREE_TYPE (expr)))
	return base;
    }
  else
    {
      /* We can get here for out-of-range string constant accesses, 
	 such as "_"[3].  Bail out of the entire substitution search
	 and arrange for the entire statement to be replaced by a
	 call to __builtin_trap.  In all likelyhood this will all be
	 constant-folded away, but in the meantime we can't leave with
	 something that get_expr_operands can't understand.  */

      t = base;
      STRIP_NOPS (t);
      if (TREE_CODE (t) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	{
	  /* FIXME: Except that this causes problems elsewhere with dead
	     code not being deleted, and we abort in the rtl expanders 
	     because we failed to remove some ssa_name.  In the meantime,
	     just return zero.  */
	  /* FIXME2: This condition should be signaled by
	     fold_read_from_constant_string directly, rather than 
	     re-checking for it here.  */
	  return integer_zero_node;
	}

      /* Try folding *(B+O) to B->X.  Still an improvement.  */
      if (POINTER_TYPE_P (TREE_TYPE (base)))
	{
          t = maybe_fold_offset_to_component_ref (TREE_TYPE (TREE_TYPE (base)),
						  base, offset,
						  TREE_TYPE (expr), true);
	  if (t)
	    return t;
	}
    }

  /* Otherwise we had an offset that we could not simplify.  */
  return NULL_TREE;
}


/* A subroutine of fold_stmt_r.  EXPR is a PLUS_EXPR.

   A quaint feature extant in our address arithmetic is that there
   can be hidden type changes here.  The type of the result need
   not be the same as the type of the input pointer.

   What we're after here is an expression of the form
	(T *)(&array + const)
   where the cast doesn't actually exist, but is implicit in the
   type of the PLUS_EXPR.  We'd like to turn this into
	&array[x]
   which may be able to propagate further.  */

static tree
maybe_fold_stmt_addition (tree expr)
{
  tree op0 = TREE_OPERAND (expr, 0);
  tree op1 = TREE_OPERAND (expr, 1);
  tree ptr_type = TREE_TYPE (expr);
  tree ptd_type;
  tree t;
  bool subtract = (TREE_CODE (expr) == MINUS_EXPR);

  /* We're only interested in pointer arithmetic.  */
  if (!POINTER_TYPE_P (ptr_type))
    return NULL_TREE;
  /* Canonicalize the integral operand to op1.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (op0)))
    {
      if (subtract)
	return NULL_TREE;
      t = op0, op0 = op1, op1 = t;
    }
  /* It had better be a constant.  */
  if (TREE_CODE (op1) != INTEGER_CST)
    return NULL_TREE;
  /* The first operand should be an ADDR_EXPR.  */
  if (TREE_CODE (op0) != ADDR_EXPR)
    return NULL_TREE;
  op0 = TREE_OPERAND (op0, 0);

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

	      array_idx = convert (TREE_TYPE (min_idx), array_idx);
	      if (!integer_zerop (min_idx))
		array_idx = int_const_binop (MINUS_EXPR, array_idx,
					     min_idx, 0);
	    }
	}

      /* Convert the index to a byte offset.  */
      array_idx = convert (sizetype, array_idx);
      array_idx = int_const_binop (MULT_EXPR, array_idx, elt_size, 0);

      /* Update the operands for the next round, or for folding.  */
      /* If we're manipulating unsigned types, then folding into negative
	 values can produce incorrect results.  Particularly if the type
	 is smaller than the width of the pointer.  */
      if (subtract
	  && TYPE_UNSIGNED (TREE_TYPE (op1))
	  && tree_int_cst_lt (array_idx, op1))
	return NULL;
      op1 = int_const_binop (subtract ? MINUS_EXPR : PLUS_EXPR,
			     array_idx, op1, 0);
      subtract = false;
      op0 = array_obj;
    }

  /* If we weren't able to fold the subtraction into another array reference,
     canonicalize the integer for passing to the array and component ref
     simplification functions.  */
  if (subtract)
    {
      if (TYPE_UNSIGNED (TREE_TYPE (op1)))
	return NULL;
      op1 = fold (build1 (NEGATE_EXPR, TREE_TYPE (op1), op1));
      /* ??? In theory fold should always produce another integer.  */
      if (TREE_CODE (op1) != INTEGER_CST)
	return NULL;
    }

  ptd_type = TREE_TYPE (ptr_type);

  /* At which point we can try some of the same things as for indirects.  */
  t = maybe_fold_offset_to_array_ref (op0, op1, ptd_type);
  if (!t)
    t = maybe_fold_offset_to_component_ref (TREE_TYPE (op0), op0, op1,
					    ptd_type, false);
  if (t)
    t = build1 (ADDR_EXPR, ptr_type, t);

  return t;
}


/* Subroutine of fold_stmt called via walk_tree.  We perform several
   simplifications of EXPR_P, mostly having to do with pointer arithmetic.  */

static tree
fold_stmt_r (tree *expr_p, int *walk_subtrees, void *data)
{
  bool *changed_p = data;
  tree expr = *expr_p, t;

  /* ??? It'd be nice if walk_tree had a pre-order option.  */
  switch (TREE_CODE (expr))
    {
    case INDIRECT_REF:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
	return t;
      *walk_subtrees = 0;

      t = maybe_fold_stmt_indirect (expr, TREE_OPERAND (expr, 0),
				    integer_zero_node);
      break;

      /* ??? Could handle ARRAY_REF here, as a variant of INDIRECT_REF.
	 We'd only want to bother decomposing an existing ARRAY_REF if
	 the base array is found to have another offset contained within.
	 Otherwise we'd be wasting time.  */

    case ADDR_EXPR:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
	return t;
      *walk_subtrees = 0;

      /* Set TREE_INVARIANT properly so that the value is properly
	 considered constant, and so gets propagated as expected.  */
      if (*changed_p)
        recompute_tree_invarant_for_addr_expr (expr);
      return NULL_TREE;

    case PLUS_EXPR:
    case MINUS_EXPR:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
	return t;
      t = walk_tree (&TREE_OPERAND (expr, 1), fold_stmt_r, data, NULL);
      if (t)
	return t;
      *walk_subtrees = 0;

      t = maybe_fold_stmt_addition (expr);
      break;

    case COMPONENT_REF:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
        return t;
      *walk_subtrees = 0;

      /* Make sure the FIELD_DECL is actually a field in the type on the lhs.
	 We've already checked that the records are compatible, so we should
	 come up with a set of compatible fields.  */
      {
	tree expr_record = TREE_TYPE (TREE_OPERAND (expr, 0));
	tree expr_field = TREE_OPERAND (expr, 1);

        if (DECL_FIELD_CONTEXT (expr_field) != TYPE_MAIN_VARIANT (expr_record))
	  {
	    expr_field = find_compatible_field (expr_record, expr_field);
	    TREE_OPERAND (expr, 1) = expr_field;
	  }
      }
      break;

    default:
      return NULL_TREE;
    }

  if (t)
    {
      *expr_p = t;
      *changed_p = true;
    }

  return NULL_TREE;
}


/* Return the string length of ARG in LENGTH.  If ARG is an SSA name variable,
   follow its use-def chains.  If LENGTH is not NULL and its value is not
   equal to the length we determine, or if we are unable to determine the
   length, return false.  VISITED is a bitmap of visited variables.  */

static bool
get_strlen (tree arg, tree *length, bitmap visited)
{
  tree var, def_stmt, val;
  
  if (TREE_CODE (arg) != SSA_NAME)
    {
      val = c_strlen (arg, 1);
      if (!val)
	return false;

      if (*length && simple_cst_equal (val, *length) != 1)
	return false;

      *length = val;
      return true;
    }

  /* If we were already here, break the infinite cycle.  */
  if (bitmap_bit_p (visited, SSA_NAME_VERSION (arg)))
    return true;
  bitmap_set_bit (visited, SSA_NAME_VERSION (arg));

  var = arg;
  def_stmt = SSA_NAME_DEF_STMT (var);

  switch (TREE_CODE (def_stmt))
    {
      case MODIFY_EXPR:
	{
	  tree len, rhs;
	  
	  /* The RHS of the statement defining VAR must either have a
	     constant length or come from another SSA_NAME with a constant
	     length.  */
	  rhs = TREE_OPERAND (def_stmt, 1);
	  STRIP_NOPS (rhs);
	  if (TREE_CODE (rhs) == SSA_NAME)
	    return get_strlen (rhs, length, visited);

	  /* See if the RHS is a constant length.  */
	  len = c_strlen (rhs, 1);
	  if (len)
	    {
	      if (*length && simple_cst_equal (len, *length) != 1)
		return false;

	      *length = len;
	      return true;
	    }

	  break;
	}

      case PHI_NODE:
	{
	  /* All the arguments of the PHI node must have the same constant
	     length.  */
	  int i;

	  for (i = 0; i < PHI_NUM_ARGS (def_stmt); i++)
	    {
	      tree arg = PHI_ARG_DEF (def_stmt, i);

	      /* If this PHI has itself as an argument, we cannot
		 determine the string length of this argument.  However,
		 if we can find a constant string length for the other
		 PHI args then we can still be sure that this is a
		 constant string length.  So be optimistic and just
		 continue with the next argument.  */
	      if (arg == PHI_RESULT (def_stmt))
		continue;

	      if (!get_strlen (arg, length, visited))
		return false;
	    }

	  return true;
	}

      default:
	break;
    }


  return false;
}


/* Fold builtin call FN in statement STMT.  If it cannot be folded into a
   constant, return NULL_TREE.  Otherwise, return its constant value.  */

static tree
ccp_fold_builtin (tree stmt, tree fn)
{
  tree result, strlen_val[2];
  tree callee, arglist, a;
  int strlen_arg, i;
  bitmap visited;
  bool ignore;

  ignore = TREE_CODE (stmt) != MODIFY_EXPR;

  /* First try the generic builtin folder.  If that succeeds, return the
     result directly.  */
  result = fold_builtin (fn, ignore);
  if (result)
  {
    if (ignore)
      STRIP_NOPS (result);
    return result;
  }

  /* Ignore MD builtins.  */
  callee = get_callee_fndecl (fn);
  if (DECL_BUILT_IN_CLASS (callee) == BUILT_IN_MD)
    return NULL_TREE;

  /* If the builtin could not be folded, and it has no argument list,
     we're done.  */
  arglist = TREE_OPERAND (fn, 1);
  if (!arglist)
    return NULL_TREE;

  /* Limit the work only for builtins we know how to simplify.  */
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
    case BUILT_IN_FPUTS:
    case BUILT_IN_FPUTS_UNLOCKED:
      strlen_arg = 1;
      break;
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRNCPY:
      strlen_arg = 2;
      break;
    default:
      return NULL_TREE;
    }

  /* Try to use the dataflow information gathered by the CCP process.  */
  visited = BITMAP_ALLOC (NULL);

  memset (strlen_val, 0, sizeof (strlen_val));
  for (i = 0, a = arglist;
       strlen_arg;
       i++, strlen_arg >>= 1, a = TREE_CHAIN (a))
    if (strlen_arg & 1)
      {
	bitmap_clear (visited);
	if (!get_strlen (TREE_VALUE (a), &strlen_val[i], visited))
	  strlen_val[i] = NULL_TREE;
      }

  BITMAP_FREE (visited);

  result = NULL_TREE;
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
      if (strlen_val[0])
	{
	  tree new = fold_convert (TREE_TYPE (fn), strlen_val[0]);

	  /* If the result is not a valid gimple value, or not a cast
	     of a valid gimple value, then we can not use the result.  */
	  if (is_gimple_val (new)
	      || (is_gimple_cast (new)
		  && is_gimple_val (TREE_OPERAND (new, 0))))
	    return new;
	}
      break;

    case BUILT_IN_STRCPY:
      if (strlen_val[1] && is_gimple_val (strlen_val[1]))
        result = fold_builtin_strcpy (fn, strlen_val[1]);
      break;

    case BUILT_IN_STRNCPY:
      if (strlen_val[1] && is_gimple_val (strlen_val[1]))
	result = fold_builtin_strncpy (fn, strlen_val[1]);
      break;

    case BUILT_IN_FPUTS:
      result = fold_builtin_fputs (arglist,
				   TREE_CODE (stmt) != MODIFY_EXPR, 0,
				   strlen_val[0]);
      break;

    case BUILT_IN_FPUTS_UNLOCKED:
      result = fold_builtin_fputs (arglist,
				   TREE_CODE (stmt) != MODIFY_EXPR, 1,
				   strlen_val[0]);
      break;

    default:
      gcc_unreachable ();
    }

  if (result && ignore)
    result = fold_ignored_result (result);
  return result;
}


/* Fold the statement pointed by STMT_P.  In some cases, this function may
   replace the whole statement with a new one.  Returns true iff folding
   makes any changes.  */

bool
fold_stmt (tree *stmt_p)
{
  tree rhs, result, stmt;
  bool changed = false;

  stmt = *stmt_p;

  /* If we replaced constants and the statement makes pointer dereferences,
     then we may need to fold instances of *&VAR into VAR, etc.  */
  if (walk_tree (stmt_p, fold_stmt_r, &changed, NULL))
    {
      *stmt_p
	= build_function_call_expr (implicit_built_in_decls[BUILT_IN_TRAP],
				    NULL);
      return true;
    }

  rhs = get_rhs (stmt);
  if (!rhs)
    return changed;
  result = NULL_TREE;

  if (TREE_CODE (rhs) == CALL_EXPR)
    {
      tree callee;

      /* Check for builtins that CCP can handle using information not
	 available in the generic fold routines.  */
      callee = get_callee_fndecl (rhs);
      if (callee && DECL_BUILT_IN (callee))
	result = ccp_fold_builtin (stmt, rhs);
      else
	{
	  /* Check for resolvable OBJ_TYPE_REF.  The only sorts we can resolve
	     here are when we've propagated the address of a decl into the
	     object slot.  */
	  /* ??? Should perhaps do this in fold proper.  However, doing it
	     there requires that we create a new CALL_EXPR, and that requires
	     copying EH region info to the new node.  Easier to just do it
	     here where we can just smash the call operand.  */
	  callee = TREE_OPERAND (rhs, 0);
	  if (TREE_CODE (callee) == OBJ_TYPE_REF
	      && lang_hooks.fold_obj_type_ref
	      && TREE_CODE (OBJ_TYPE_REF_OBJECT (callee)) == ADDR_EXPR
	      && DECL_P (TREE_OPERAND
			 (OBJ_TYPE_REF_OBJECT (callee), 0)))
	    {
	      tree t;

	      /* ??? Caution: Broken ADDR_EXPR semantics means that
		 looking at the type of the operand of the addr_expr
		 can yield an array type.  See silly exception in
		 check_pointer_types_r.  */

	      t = TREE_TYPE (TREE_TYPE (OBJ_TYPE_REF_OBJECT (callee)));
	      t = lang_hooks.fold_obj_type_ref (callee, t);
	      if (t && TREE_CODE (t) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
		  && cgraph_global_info_ready)
		{
		  /* If the method has been already finalized as unreachable,
		     avoid any new references to it.  */
		  struct cgraph_node *node;

		  node = cgraph_node (TREE_OPERAND (t, 0));
		  if (!node->reachable && node->local.finalized)
		    t = NULL_TREE;
		}
	      if (t)
		{
		  TREE_OPERAND (rhs, 0) = t;
		  changed = true;
		}
	    }
	}
    }

  /* If we couldn't fold the RHS, hand over to the generic fold routines.  */
  if (result == NULL_TREE)
    result = fold (rhs);

  /* Strip away useless type conversions.  Both the NON_LVALUE_EXPR that
     may have been added by fold, and "useless" type conversions that might
     now be apparent due to propagation.  */
  STRIP_USELESS_TYPE_CONVERSION (result);

  if (result != rhs)
    changed |= set_rhs (stmt_p, result);

  return changed;
}


/* Convert EXPR into a GIMPLE value suitable for substitution on the
   RHS of an assignment.  Insert the necessary statements before
   iterator *SI_P.  */

static tree
convert_to_gimple_builtin (block_stmt_iterator *si_p, tree expr)
{
  tree_stmt_iterator ti;
  tree stmt = bsi_stmt (*si_p);
  tree tmp, stmts = NULL;

  push_gimplify_context ();
  tmp = get_initialized_tmp_var (expr, &stmts, NULL);
  pop_gimplify_context (NULL);

  /* The replacement can expose previously unreferenced variables.  */
  for (ti = tsi_start (stmts); !tsi_end_p (ti); tsi_next (&ti))
    {
      find_new_referenced_vars (tsi_stmt_ptr (ti));
      mark_new_vars_to_rename (tsi_stmt (ti), vars_to_rename);
    }

  if (EXPR_HAS_LOCATION (stmt))
    annotate_all_with_locus (&stmts, EXPR_LOCATION (stmt));

  bsi_insert_before (si_p, stmts, BSI_SAME_STMT);

  return tmp;
}


/* A simple pass that attempts to fold all builtin functions.  This pass
   is run after we've propagated as many constants as we can.  */

static void
execute_fold_all_builtins (void)
{
  bool cfg_changed = false;
  basic_block bb;
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;
      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
	{
	  tree *stmtp = bsi_stmt_ptr (i);
	  tree call = get_rhs (*stmtp);
	  tree callee, result;

	  if (!call || TREE_CODE (call) != CALL_EXPR)
	    continue;
	  callee = get_callee_fndecl (call);
	  if (!callee || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL)
	    continue;

	  result = ccp_fold_builtin (*stmtp, call);
	  if (!result)
	    switch (DECL_FUNCTION_CODE (callee))
	      {
	      case BUILT_IN_CONSTANT_P:
		/* Resolve __builtin_constant_p.  If it hasn't been
		   folded to integer_one_node by now, it's fairly
		   certain that the value simply isn't constant.  */
		result = integer_zero_node;
		break;

	      default:
		continue;
	      }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Simplified\n  ");
	      print_generic_stmt (dump_file, *stmtp, dump_flags);
	    }

	  if (!set_rhs (stmtp, result))
	    {
	      result = convert_to_gimple_builtin (&i, result);
	      if (result && !set_rhs (stmtp, result))
		abort ();
	    }
	  modify_stmt (*stmtp);
	  if (maybe_clean_eh_stmt (*stmtp)
	      && tree_purge_dead_eh_edges (bb))
	    cfg_changed = true;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "to\n  ");
	      print_generic_stmt (dump_file, *stmtp, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  /* Delete unreachable blocks.  */
  if (cfg_changed)
    cleanup_tree_cfg ();
}


struct tree_opt_pass pass_fold_builtins = 
{
  "fab",				/* name */
  NULL,					/* gate */
  execute_fold_all_builtins,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_verify_ssa
    | TODO_rename_vars,			/* todo_flags_finish */
  0					/* letter */
};
