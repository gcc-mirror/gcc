/* Conditional constant propagation pass for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010 Free Software Foundation, Inc.
   Adapted from original RTL SSA-CCP by Daniel Berlin <dberlin@dberlin.org>
   Adapted to GIMPLE trees by Diego Novillo <dnovillo@redhat.com>

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

/* Conditional constant propagation (CCP) is based on the SSA
   propagation engine (tree-ssa-propagate.c).  Constant assignments of
   the form VAR = CST are propagated from the assignments into uses of
   VAR, which in turn may generate new constants.  The simulation uses
   a four level lattice to keep track of constant values associated
   with SSA names.  Given an SSA name V_i, it may take one of the
   following values:

	UNINITIALIZED   ->  the initial state of the value.  This value
			    is replaced with a correct initial value
			    the first time the value is used, so the
			    rest of the pass does not need to care about
			    it.  Using this value simplifies initialization
			    of the pass, and prevents us from needlessly
			    scanning statements that are never reached.

	UNDEFINED	->  V_i is a local variable whose definition
			    has not been processed yet.  Therefore we
			    don't yet know if its value is a constant
			    or not.

	CONSTANT	->  V_i has been found to hold a constant
			    value C.

	VARYING		->  V_i cannot take a constant value, or if it
			    does, it is not possible to determine it
			    at compile time.

   The core of SSA-CCP is in ccp_visit_stmt and ccp_visit_phi_node:

   1- In ccp_visit_stmt, we are interested in assignments whose RHS
      evaluates into a constant and conditional jumps whose predicate
      evaluates into a boolean true or false.  When an assignment of
      the form V_i = CONST is found, V_i's lattice value is set to
      CONSTANT and CONST is associated with it.  This causes the
      propagation engine to add all the SSA edges coming out the
      assignment into the worklists, so that statements that use V_i
      can be visited.

      If the statement is a conditional with a constant predicate, we
      mark the outgoing edges as executable or not executable
      depending on the predicate's value.  This is then used when
      visiting PHI nodes to know when a PHI argument can be ignored.


   2- In ccp_visit_phi_node, if all the PHI arguments evaluate to the
      same constant C, then the LHS of the PHI is set to C.  This
      evaluation is known as the "meet operation".  Since one of the
      goals of this evaluation is to optimistically return constant
      values as often as possible, it uses two main short cuts:

      - If an argument is flowing in through a non-executable edge, it
	is ignored.  This is useful in cases like this:

			if (PRED)
			  a_9 = 3;
			else
			  a_10 = 100;
			a_11 = PHI (a_9, a_10)

	If PRED is known to always evaluate to false, then we can
	assume that a_11 will always take its value from a_10, meaning
	that instead of consider it VARYING (a_9 and a_10 have
	different values), we can consider it CONSTANT 100.

      - If an argument has an UNDEFINED value, then it does not affect
	the outcome of the meet operation.  If a variable V_i has an
	UNDEFINED value, it means that either its defining statement
	hasn't been visited yet or V_i has no defining statement, in
	which case the original symbol 'V' is being used
	uninitialized.  Since 'V' is a local variable, the compiler
	may assume any initial value for it.


   After propagation, every variable V_i that ends up with a lattice
   value of CONSTANT will have the associated constant value in the
   array CONST_VAL[i].VALUE.  That is fed into substitute_and_fold for
   final substitution and folding.

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
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "value-prof.h"
#include "langhooks.h"
#include "target.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "dbgcnt.h"


/* Possible lattice values.  */
typedef enum
{
  UNINITIALIZED,
  UNDEFINED,
  CONSTANT,
  VARYING
} ccp_lattice_t;

/* Array of propagated constant values.  After propagation,
   CONST_VAL[I].VALUE holds the constant value for SSA_NAME(I).  If
   the constant is held in an SSA name representing a memory store
   (i.e., a VDEF), CONST_VAL[I].MEM_REF will contain the actual
   memory reference used to store (i.e., the LHS of the assignment
   doing the store).  */
static prop_value_t *const_val;

static void canonicalize_float_value (prop_value_t *);
static bool ccp_fold_stmt (gimple_stmt_iterator *);

/* Dump constant propagation value VAL to file OUTF prefixed by PREFIX.  */

static void
dump_lattice_value (FILE *outf, const char *prefix, prop_value_t val)
{
  switch (val.lattice_val)
    {
    case UNINITIALIZED:
      fprintf (outf, "%sUNINITIALIZED", prefix);
      break;
    case UNDEFINED:
      fprintf (outf, "%sUNDEFINED", prefix);
      break;
    case VARYING:
      fprintf (outf, "%sVARYING", prefix);
      break;
    case CONSTANT:
      fprintf (outf, "%sCONSTANT ", prefix);
      print_generic_expr (outf, val.value, dump_flags);
      break;
    default:
      gcc_unreachable ();
    }
}


/* Print lattice value VAL to stderr.  */

void debug_lattice_value (prop_value_t val);

DEBUG_FUNCTION void
debug_lattice_value (prop_value_t val)
{
  dump_lattice_value (stderr, "", val);
  fprintf (stderr, "\n");
}


/* Compute a default value for variable VAR and store it in the
   CONST_VAL array.  The following rules are used to get default
   values:

   1- Global and static variables that are declared constant are
      considered CONSTANT.

   2- Any other value is considered UNDEFINED.  This is useful when
      considering PHI nodes.  PHI arguments that are undefined do not
      change the constant value of the PHI node, which allows for more
      constants to be propagated.

   3- Variables defined by statements other than assignments and PHI
      nodes are considered VARYING.

   4- Initial values of variables that are not GIMPLE registers are
      considered VARYING.  */

static prop_value_t
get_default_value (tree var)
{
  tree sym = SSA_NAME_VAR (var);
  prop_value_t val = { UNINITIALIZED, NULL_TREE };
  gimple stmt;

  stmt = SSA_NAME_DEF_STMT (var);

  if (gimple_nop_p (stmt))
    {
      /* Variables defined by an empty statement are those used
	 before being initialized.  If VAR is a local variable, we
	 can assume initially that it is UNDEFINED, otherwise we must
	 consider it VARYING.  */
      if (is_gimple_reg (sym)
	  && TREE_CODE (sym) == VAR_DECL)
	val.lattice_val = UNDEFINED;
      else
	val.lattice_val = VARYING;
    }
  else if (is_gimple_assign (stmt)
	   /* Value-returning GIMPLE_CALL statements assign to
	      a variable, and are treated similarly to GIMPLE_ASSIGN.  */
	   || (is_gimple_call (stmt)
	       && gimple_call_lhs (stmt) != NULL_TREE)
	   || gimple_code (stmt) == GIMPLE_PHI)
    {
      tree cst;
      if (gimple_assign_single_p (stmt)
	  && DECL_P (gimple_assign_rhs1 (stmt))
	  && (cst = get_symbol_constant_value (gimple_assign_rhs1 (stmt))))
	{
	  val.lattice_val = CONSTANT;
	  val.value = cst;
	}
      else
	/* Any other variable defined by an assignment or a PHI node
	   is considered UNDEFINED.  */
	val.lattice_val = UNDEFINED;
    }
  else
    {
      /* Otherwise, VAR will never take on a constant value.  */
      val.lattice_val = VARYING;
    }

  return val;
}


/* Get the constant value associated with variable VAR.  */

static inline prop_value_t *
get_value (tree var)
{
  prop_value_t *val;

  if (const_val == NULL)
    return NULL;

  val = &const_val[SSA_NAME_VERSION (var)];
  if (val->lattice_val == UNINITIALIZED)
    *val = get_default_value (var);

  canonicalize_float_value (val);

  return val;
}

/* Return the constant tree value associated with VAR.  */

static inline tree
get_constant_value (tree var)
{
  prop_value_t *val = get_value (var);
  if (val && val->lattice_val == CONSTANT)
    return val->value;
  return NULL_TREE;
}

/* Sets the value associated with VAR to VARYING.  */

static inline void
set_value_varying (tree var)
{
  prop_value_t *val = &const_val[SSA_NAME_VERSION (var)];

  val->lattice_val = VARYING;
  val->value = NULL_TREE;
}

/* For float types, modify the value of VAL to make ccp work correctly
   for non-standard values (-0, NaN):

   If HONOR_SIGNED_ZEROS is false, and VAL = -0, we canonicalize it to 0.
   If HONOR_NANS is false, and VAL is NaN, we canonicalize it to UNDEFINED.
     This is to fix the following problem (see PR 29921): Suppose we have

     x = 0.0 * y

     and we set value of y to NaN.  This causes value of x to be set to NaN.
     When we later determine that y is in fact VARYING, fold uses the fact
     that HONOR_NANS is false, and we try to change the value of x to 0,
     causing an ICE.  With HONOR_NANS being false, the real appearance of
     NaN would cause undefined behavior, though, so claiming that y (and x)
     are UNDEFINED initially is correct.  */

static void
canonicalize_float_value (prop_value_t *val)
{
  enum machine_mode mode;
  tree type;
  REAL_VALUE_TYPE d;

  if (val->lattice_val != CONSTANT
      || TREE_CODE (val->value) != REAL_CST)
    return;

  d = TREE_REAL_CST (val->value);
  type = TREE_TYPE (val->value);
  mode = TYPE_MODE (type);

  if (!HONOR_SIGNED_ZEROS (mode)
      && REAL_VALUE_MINUS_ZERO (d))
    {
      val->value = build_real (type, dconst0);
      return;
    }

  if (!HONOR_NANS (mode)
      && REAL_VALUE_ISNAN (d))
    {
      val->lattice_val = UNDEFINED;
      val->value = NULL;
      return;
    }
}

/* Set the value for variable VAR to NEW_VAL.  Return true if the new
   value is different from VAR's previous value.  */

static bool
set_lattice_value (tree var, prop_value_t new_val)
{
  /* We can deal with old UNINITIALIZED values just fine here.  */
  prop_value_t *old_val = &const_val[SSA_NAME_VERSION (var)];

  canonicalize_float_value (&new_val);

  /* Lattice transitions must always be monotonically increasing in
     value.  If *OLD_VAL and NEW_VAL are the same, return false to
     inform the caller that this was a non-transition.  */

  gcc_assert (old_val->lattice_val < new_val.lattice_val
              || (old_val->lattice_val == new_val.lattice_val
		  && ((!old_val->value && !new_val.value)
		      || operand_equal_p (old_val->value, new_val.value, 0))));

  if (old_val->lattice_val != new_val.lattice_val)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  dump_lattice_value (dump_file, "Lattice value changed to ", new_val);
	  fprintf (dump_file, ".  Adding SSA edges to worklist.\n");
	}

      *old_val = new_val;

      gcc_assert (new_val.lattice_val != UNINITIALIZED);
      return true;
    }

  return false;
}

/* Return the value for the tree operand EXPR.  */

static prop_value_t
get_value_for_expr (tree expr)
{
  prop_value_t val;

  if (TREE_CODE (expr) == SSA_NAME)
    val = *(get_value (expr));
  else if (is_gimple_min_invariant (expr))
    {
      val.lattice_val = CONSTANT;
      val.value = expr;
      canonicalize_float_value (&val);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
    }

  return val;
}


/* Return the likely CCP lattice value for STMT.

   If STMT has no operands, then return CONSTANT.

   Else if undefinedness of operands of STMT cause its value to be
   undefined, then return UNDEFINED.

   Else if any operands of STMT are constants, then return CONSTANT.

   Else return VARYING.  */

static ccp_lattice_t
likely_value (gimple stmt)
{
  bool has_constant_operand, has_undefined_operand, all_undefined_operands;
  tree use;
  ssa_op_iter iter;
  unsigned i;

  enum gimple_code code = gimple_code (stmt);

  /* This function appears to be called only for assignments, calls,
     conditionals, and switches, due to the logic in visit_stmt.  */
  gcc_assert (code == GIMPLE_ASSIGN
              || code == GIMPLE_CALL
              || code == GIMPLE_COND
              || code == GIMPLE_SWITCH);

  /* If the statement has volatile operands, it won't fold to a
     constant value.  */
  if (gimple_has_volatile_ops (stmt))
    return VARYING;

  /* Arrive here for more complex cases.  */
  has_constant_operand = false;
  has_undefined_operand = false;
  all_undefined_operands = true;
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      prop_value_t *val = get_value (use);

      if (val->lattice_val == UNDEFINED)
	has_undefined_operand = true;
      else
	all_undefined_operands = false;

      if (val->lattice_val == CONSTANT)
	has_constant_operand = true;
    }

  /* There may be constants in regular rhs operands.  For calls we
     have to ignore lhs, fndecl and static chain, otherwise only
     the lhs.  */
  for (i = (is_gimple_call (stmt) ? 2 : 0) + gimple_has_lhs (stmt);
       i < gimple_num_ops (stmt); ++i)
    {
      tree op = gimple_op (stmt, i);
      if (!op || TREE_CODE (op) == SSA_NAME)
	continue;
      if (is_gimple_min_invariant (op))
	has_constant_operand = true;
    }

  if (has_constant_operand)
    all_undefined_operands = false;

  /* If the operation combines operands like COMPLEX_EXPR make sure to
     not mark the result UNDEFINED if only one part of the result is
     undefined.  */
  if (has_undefined_operand && all_undefined_operands)
    return UNDEFINED;
  else if (code == GIMPLE_ASSIGN && has_undefined_operand)
    {
      switch (gimple_assign_rhs_code (stmt))
	{
	/* Unary operators are handled with all_undefined_operands.  */
	case PLUS_EXPR:
	case MINUS_EXPR:
	case POINTER_PLUS_EXPR:
	  /* Not MIN_EXPR, MAX_EXPR.  One VARYING operand may be selected.
	     Not bitwise operators, one VARYING operand may specify the
	     result completely.  Not logical operators for the same reason.
	     Not COMPLEX_EXPR as one VARYING operand makes the result partly
	     not UNDEFINED.  Not *DIV_EXPR, comparisons and shifts because
	     the undefined operand may be promoted.  */
	  return UNDEFINED;

	default:
	  ;
	}
    }
  /* If there was an UNDEFINED operand but the result may be not UNDEFINED
     fall back to VARYING even if there were CONSTANT operands.  */
  if (has_undefined_operand)
    return VARYING;

  /* We do not consider virtual operands here -- load from read-only
     memory may have only VARYING virtual operands, but still be
     constant.  */
  if (has_constant_operand
      || gimple_references_memory_p (stmt))
    return CONSTANT;

  return VARYING;
}

/* Returns true if STMT cannot be constant.  */

static bool
surely_varying_stmt_p (gimple stmt)
{
  /* If the statement has operands that we cannot handle, it cannot be
     constant.  */
  if (gimple_has_volatile_ops (stmt))
    return true;

  /* If it is a call and does not return a value or is not a
     builtin and not an indirect call, it is varying.  */
  if (is_gimple_call (stmt))
    {
      tree fndecl;
      if (!gimple_call_lhs (stmt)
	  || ((fndecl = gimple_call_fndecl (stmt)) != NULL_TREE
	      && !DECL_BUILT_IN (fndecl)))
	return true;
    }

  /* Any other store operation is not interesting.  */
  else if (gimple_vdef (stmt))
    return true;

  /* Anything other than assignments and conditional jumps are not
     interesting for CCP.  */
  if (gimple_code (stmt) != GIMPLE_ASSIGN
      && gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_SWITCH
      && gimple_code (stmt) != GIMPLE_CALL)
    return true;

  return false;
}

/* Initialize local data structures for CCP.  */

static void
ccp_initialize (void)
{
  basic_block bb;

  const_val = XCNEWVEC (prop_value_t, num_ssa_names);

  /* Initialize simulation flags for PHI nodes and statements.  */
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
	  gimple stmt = gsi_stmt (i);
	  bool is_varying;

	  /* If the statement is a control insn, then we do not
	     want to avoid simulating the statement once.  Failure
	     to do so means that those edges will never get added.  */
	  if (stmt_ends_bb_p (stmt))
	    is_varying = false;
	  else
	    is_varying = surely_varying_stmt_p (stmt);

	  if (is_varying)
	    {
	      tree def;
	      ssa_op_iter iter;

	      /* If the statement will not produce a constant, mark
		 all its outputs VARYING.  */
	      FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_ALL_DEFS)
		set_value_varying (def);
	    }
          prop_set_simulate_again (stmt, !is_varying);
	}
    }

  /* Now process PHI nodes.  We never clear the simulate_again flag on
     phi nodes, since we do not know which edges are executable yet,
     except for phi nodes for virtual operands when we do not do store ccp.  */
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
        {
          gimple phi = gsi_stmt (i);

	  if (!is_gimple_reg (gimple_phi_result (phi)))
            prop_set_simulate_again (phi, false);
	  else
            prop_set_simulate_again (phi, true);
	}
    }
}

/* Debug count support. Reset the values of ssa names
   VARYING when the total number ssa names analyzed is
   beyond the debug count specified.  */

static void
do_dbg_cnt (void)
{
  unsigned i;
  for (i = 0; i < num_ssa_names; i++)
    {
      if (!dbg_cnt (ccp))
        {
          const_val[i].lattice_val = VARYING;
          const_val[i].value = NULL_TREE;
        }
    }
}


/* Do final substitution of propagated values, cleanup the flowgraph and
   free allocated storage.

   Return TRUE when something was optimized.  */

static bool
ccp_finalize (void)
{
  bool something_changed;

  do_dbg_cnt ();
  /* Perform substitutions based on the known constant values.  */
  something_changed = substitute_and_fold (const_val, ccp_fold_stmt, true);

  free (const_val);
  const_val = NULL;
  return something_changed;;
}


/* Compute the meet operator between *VAL1 and *VAL2.  Store the result
   in VAL1.

   		any  M UNDEFINED   = any
		any  M VARYING     = VARYING
		Ci   M Cj	   = Ci		if (i == j)
		Ci   M Cj	   = VARYING	if (i != j)
   */

static void
ccp_lattice_meet (prop_value_t *val1, prop_value_t *val2)
{
  if (val1->lattice_val == UNDEFINED)
    {
      /* UNDEFINED M any = any   */
      *val1 = *val2;
    }
  else if (val2->lattice_val == UNDEFINED)
    {
      /* any M UNDEFINED = any
         Nothing to do.  VAL1 already contains the value we want.  */
      ;
    }
  else if (val1->lattice_val == VARYING
           || val2->lattice_val == VARYING)
    {
      /* any M VARYING = VARYING.  */
      val1->lattice_val = VARYING;
      val1->value = NULL_TREE;
    }
  else if (val1->lattice_val == CONSTANT
	   && val2->lattice_val == CONSTANT
	   && simple_cst_equal (val1->value, val2->value) == 1)
    {
      /* Ci M Cj = Ci		if (i == j)
	 Ci M Cj = VARYING	if (i != j)

         If these two values come from memory stores, make sure that
	 they come from the same memory reference.
         Nothing to do.  VAL1 already contains the value we want.  */
    }
  else
    {
      /* Any other combination is VARYING.  */
      val1->lattice_val = VARYING;
      val1->value = NULL_TREE;
    }
}


/* Loop through the PHI_NODE's parameters for BLOCK and compare their
   lattice values to determine PHI_NODE's lattice value.  The value of a
   PHI node is determined calling ccp_lattice_meet with all the arguments
   of the PHI node that are incoming via executable edges.  */

static enum ssa_prop_result
ccp_visit_phi_node (gimple phi)
{
  unsigned i;
  prop_value_t *old_val, new_val;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_gimple_stmt (dump_file, phi, 0, dump_flags);
    }

  old_val = get_value (gimple_phi_result (phi));
  switch (old_val->lattice_val)
    {
    case VARYING:
      return SSA_PROP_VARYING;

    case CONSTANT:
      new_val = *old_val;
      break;

    case UNDEFINED:
      new_val.lattice_val = UNDEFINED;
      new_val.value = NULL_TREE;
      break;

    default:
      gcc_unreachable ();
    }

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      /* Compute the meet operator over all the PHI arguments flowing
	 through executable edges.  */
      edge e = gimple_phi_arg_edge (phi, i);

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
	  tree arg = gimple_phi_arg (phi, i)->def;
	  prop_value_t arg_val = get_value_for_expr (arg);

	  ccp_lattice_meet (&new_val, &arg_val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\t");
	      print_generic_expr (dump_file, arg, dump_flags);
	      dump_lattice_value (dump_file, "\tValue: ", arg_val);
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

  /* Make the transition to the new value.  */
  if (set_lattice_value (gimple_phi_result (phi), new_val))
    {
      if (new_val.lattice_val == VARYING)
	return SSA_PROP_VARYING;
      else
	return SSA_PROP_INTERESTING;
    }
  else
    return SSA_PROP_NOT_INTERESTING;
}

/* Return the constant value for OP or OP otherwise.  */

static tree
valueize_op (tree op)
{
  if (TREE_CODE (op) == SSA_NAME)
    {
      tree tem = get_constant_value (op);
      if (tem)
	return tem;
    }
  return op;
}

/* CCP specific front-end to the non-destructive constant folding
   routines.

   Attempt to simplify the RHS of STMT knowing that one or more
   operands are constants.

   If simplification is possible, return the simplified RHS,
   otherwise return the original RHS or NULL_TREE.  */

static tree
ccp_fold (gimple stmt)
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
                  return get_constant_value (rhs);
                }
	      /* Handle propagating invariant addresses into address operations.
		 The folding we do here matches that in tree-ssa-forwprop.c.  */
	      else if (TREE_CODE (rhs) == ADDR_EXPR)
		{
		  tree *base;
		  base = &TREE_OPERAND (rhs, 0);
		  while (handled_component_p (*base))
		    base = &TREE_OPERAND (*base, 0);
		  if (TREE_CODE (*base) == MEM_REF
		      && TREE_CODE (TREE_OPERAND (*base, 0)) == SSA_NAME)
		    {
		      tree val = get_constant_value (TREE_OPERAND (*base, 0));
		      if (val
			  && TREE_CODE (val) == ADDR_EXPR)
			{
			  tree ret, save = *base;
			  tree new_base;
			  new_base = fold_build2 (MEM_REF, TREE_TYPE (*base),
						  unshare_expr (val),
						  TREE_OPERAND (*base, 1));
			  /* We need to return a new tree, not modify the IL
			     or share parts of it.  So play some tricks to
			     avoid manually building it.  */
			  *base = new_base;
			  ret = unshare_expr (rhs);
			  recompute_tree_invariant_for_addr_expr (ret);
			  *base = save;
			  return ret;
			}
		    }
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
		      val = valueize_op (val);
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
		      tree val = get_constant_value (TREE_OPERAND (rhs, 0));
		      if (val)
			return fold_unary_loc (EXPR_LOCATION (rhs),
					       TREE_CODE (rhs),
					       TREE_TYPE (rhs), val);
		    }
		  else if (TREE_CODE (rhs) == MEM_REF
			   && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
		    {
		      tree val = get_constant_value (TREE_OPERAND (rhs, 0));
		      if (val
			  && TREE_CODE (val) == ADDR_EXPR)
			{
			  tree tem = fold_build2 (MEM_REF, TREE_TYPE (rhs),
						  unshare_expr (val),
						  TREE_OPERAND (rhs, 1));
			  if (tem)
			    rhs = tem;
			}
		    }
		  return fold_const_aggregate_ref (rhs);
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
              tree op0 = valueize_op (gimple_assign_rhs1 (stmt));

	      /* Conversions are useless for CCP purposes if they are
		 value-preserving.  Thus the restrictions that
		 useless_type_conversion_p places for pointer type conversions
		 do not apply here.  Substitution later will only substitute to
		 allowed places.  */
	      if (CONVERT_EXPR_CODE_P (subcode)
		  && POINTER_TYPE_P (TREE_TYPE (lhs))
		  && POINTER_TYPE_P (TREE_TYPE (op0)))
		{
		  tree tem;
		  /* Try to re-construct array references on-the-fly.  */
		  if (!useless_type_conversion_p (TREE_TYPE (lhs),
						  TREE_TYPE (op0))
		      && ((tem = maybe_fold_offset_to_address
			   (loc,
			    op0, integer_zero_node, TREE_TYPE (lhs)))
			  != NULL_TREE))
		    return tem;
		  return op0;
		}

              return
		fold_unary_ignore_overflow_loc (loc, subcode,
						gimple_expr_type (stmt), op0);
            }

          case GIMPLE_BINARY_RHS:
            {
              /* Handle binary operators that can appear in GIMPLE form.  */
              tree op0 = valueize_op (gimple_assign_rhs1 (stmt));
              tree op1 = valueize_op (gimple_assign_rhs2 (stmt));

	      /* Translate &x + CST into an invariant form suitable for
	         further propagation.  */
	      if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
		  && TREE_CODE (op0) == ADDR_EXPR
		  && TREE_CODE (op1) == INTEGER_CST)
		{
		  tree off = fold_convert (ptr_type_node, op1);
		  return build_fold_addr_expr
			   (fold_build2 (MEM_REF,
					 TREE_TYPE (TREE_TYPE (op0)),
					 unshare_expr (op0), off));
		}

              return fold_binary_loc (loc, subcode,
				      gimple_expr_type (stmt), op0, op1);
            }

          case GIMPLE_TERNARY_RHS:
            {
              /* Handle ternary operators that can appear in GIMPLE form.  */
              tree op0 = valueize_op (gimple_assign_rhs1 (stmt));
              tree op1 = valueize_op (gimple_assign_rhs2 (stmt));
              tree op2 = valueize_op (gimple_assign_rhs3 (stmt));

              return fold_ternary_loc (loc, subcode,
				       gimple_expr_type (stmt), op0, op1, op2);
            }

          default:
            gcc_unreachable ();
          }
      }
      break;

    case GIMPLE_CALL:
      {
	tree fn = valueize_op (gimple_call_fn (stmt));
	if (TREE_CODE (fn) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
	    && DECL_BUILT_IN (TREE_OPERAND (fn, 0)))
	  {
	    tree *args = XALLOCAVEC (tree, gimple_call_num_args (stmt));
	    tree call, retval;
	    unsigned i;
	    for (i = 0; i < gimple_call_num_args (stmt); ++i)
	      args[i] = valueize_op (gimple_call_arg (stmt, i));
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

    case GIMPLE_COND:
      {
        /* Handle comparison operators that can appear in GIMPLE form.  */
        tree op0 = valueize_op (gimple_cond_lhs (stmt));
        tree op1 = valueize_op (gimple_cond_rhs (stmt));
        enum tree_code code = gimple_cond_code (stmt);
        return fold_binary_loc (loc, code, boolean_type_node, op0, op1);
      }

    case GIMPLE_SWITCH:
      {
	/* Return the constant switch index.  */
        return valueize_op (gimple_switch_index (stmt));
      }

    default:
      gcc_unreachable ();
    }
}

/* Return the tree representing the element referenced by T if T is an
   ARRAY_REF or COMPONENT_REF into constant aggregates.  Return
   NULL_TREE otherwise.  */

tree
fold_const_aggregate_ref (tree t)
{
  tree base, ctor, idx, field;
  unsigned HOST_WIDE_INT cnt;
  tree cfield, cval;
  tree tem;

  if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_declaration)
    return get_symbol_constant_value (t);

  switch (TREE_CODE (t))
    {
    case ARRAY_REF:
      /* Get a CONSTRUCTOR.  If BASE is a VAR_DECL, get its
	 DECL_INITIAL.  If BASE is a nested reference into another
	 ARRAY_REF or COMPONENT_REF, make a recursive call to resolve
	 the inner reference.  */
      base = TREE_OPERAND (t, 0);
      switch (TREE_CODE (base))
	{
	case MEM_REF:
	  /* ???  We could handle this case.  */
	  if (!integer_zerop (TREE_OPERAND (base, 1)))
	    return NULL_TREE;
	  base = get_base_address (base);
	  if (!base
	      || TREE_CODE (base) != VAR_DECL)
	    return NULL_TREE;

	  /* Fallthru.  */
	case VAR_DECL:
	  if (!TREE_READONLY (base)
	      || TREE_CODE (TREE_TYPE (base)) != ARRAY_TYPE
	      || !targetm.binds_local_p (base))
	    return NULL_TREE;

	  ctor = DECL_INITIAL (base);
	  break;

	case ARRAY_REF:
	case COMPONENT_REF:
	  ctor = fold_const_aggregate_ref (base);
	  break;

	case STRING_CST:
	case CONSTRUCTOR:
	  ctor = base;
	  break;

	default:
	  return NULL_TREE;
	}

      if (ctor == NULL_TREE
	  || (TREE_CODE (ctor) != CONSTRUCTOR
	      && TREE_CODE (ctor) != STRING_CST)
	  || !TREE_STATIC (ctor))
	return NULL_TREE;

      /* Get the index.  If we have an SSA_NAME, try to resolve it
	 with the current lattice value for the SSA_NAME.  */
      idx = TREE_OPERAND (t, 1);
      switch (TREE_CODE (idx))
	{
	case SSA_NAME:
	  if ((tem = get_constant_value (idx))
	      && TREE_CODE (tem) == INTEGER_CST)
	    idx = tem;
	  else
	    return NULL_TREE;
	  break;

	case INTEGER_CST:
	  break;

	default:
	  return NULL_TREE;
	}

      /* Fold read from constant string.  */
      if (TREE_CODE (ctor) == STRING_CST)
	{
	  if ((TYPE_MODE (TREE_TYPE (t))
	       == TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
	      && (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
	          == MODE_INT)
	      && GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor)))) == 1
	      && compare_tree_int (idx, TREE_STRING_LENGTH (ctor)) < 0)
	    return build_int_cst_type (TREE_TYPE (t),
				       (TREE_STRING_POINTER (ctor)
					[TREE_INT_CST_LOW (idx)]));
	  return NULL_TREE;
	}

      /* Whoo-hoo!  I'll fold ya baby.  Yeah!  */
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield, cval)
	if (tree_int_cst_equal (cfield, idx))
	  {
	    STRIP_NOPS (cval);
	    if (TREE_CODE (cval) == ADDR_EXPR)
	      {
		tree base = get_base_address (TREE_OPERAND (cval, 0));
		if (base && TREE_CODE (base) == VAR_DECL)
		  add_referenced_var (base);
	      }
	    return cval;
	  }
      break;

    case COMPONENT_REF:
      /* Get a CONSTRUCTOR.  If BASE is a VAR_DECL, get its
	 DECL_INITIAL.  If BASE is a nested reference into another
	 ARRAY_REF or COMPONENT_REF, make a recursive call to resolve
	 the inner reference.  */
      base = TREE_OPERAND (t, 0);
      switch (TREE_CODE (base))
	{
	case VAR_DECL:
	  if (!TREE_READONLY (base)
	      || TREE_CODE (TREE_TYPE (base)) != RECORD_TYPE
	      || !targetm.binds_local_p (base))
	    return NULL_TREE;

	  ctor = DECL_INITIAL (base);
	  break;

	case ARRAY_REF:
	case COMPONENT_REF:
	  ctor = fold_const_aggregate_ref (base);
	  break;

	default:
	  return NULL_TREE;
	}

      if (ctor == NULL_TREE
	  || TREE_CODE (ctor) != CONSTRUCTOR
	  || !TREE_STATIC (ctor))
	return NULL_TREE;

      field = TREE_OPERAND (t, 1);

      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield, cval)
	if (cfield == field
	    /* FIXME: Handle bit-fields.  */
	    && ! DECL_BIT_FIELD (cfield))
	  {
	    STRIP_NOPS (cval);
	    if (TREE_CODE (cval) == ADDR_EXPR)
	      {
		tree base = get_base_address (TREE_OPERAND (cval, 0));
		if (base && TREE_CODE (base) == VAR_DECL)
		  add_referenced_var (base);
	      }
	    return cval;
	  }
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
	tree c = fold_const_aggregate_ref (TREE_OPERAND (t, 0));
	if (c && TREE_CODE (c) == COMPLEX_CST)
	  return fold_build1_loc (EXPR_LOCATION (t),
			      TREE_CODE (t), TREE_TYPE (t), c);
	break;
      }

    case MEM_REF:
      /* Get the base object we are accessing.  */
      base = TREE_OPERAND (t, 0);
      if (TREE_CODE (base) == SSA_NAME
	  && (tem = get_constant_value (base)))
	base = tem;
      if (TREE_CODE (base) != ADDR_EXPR)
	return NULL_TREE;
      base = TREE_OPERAND (base, 0);
      switch (TREE_CODE (base))
	{
	case VAR_DECL:
	  if (DECL_P (base)
	      && !AGGREGATE_TYPE_P (TREE_TYPE (base))
	      && integer_zerop (TREE_OPERAND (t, 1)))
	    {
	      tree res = get_symbol_constant_value (base);
	      if (res
		  && !useless_type_conversion_p
		        (TREE_TYPE (t), TREE_TYPE (res)))
		res = fold_unary (VIEW_CONVERT_EXPR, TREE_TYPE (t), res);
	      return res;
	    }

	  if (!TREE_READONLY (base)
	      || TREE_CODE (TREE_TYPE (base)) != ARRAY_TYPE
	      || !targetm.binds_local_p (base))
	    return NULL_TREE;

	  ctor = DECL_INITIAL (base);
	  break;

	case STRING_CST:
	case CONSTRUCTOR:
	  ctor = base;
	  break;

	default:
	  return NULL_TREE;
	}

      if (ctor == NULL_TREE
	  || (TREE_CODE (ctor) != CONSTRUCTOR
	      && TREE_CODE (ctor) != STRING_CST)
	  || !TREE_STATIC (ctor))
	return NULL_TREE;

      /* Get the byte offset.  */
      idx = TREE_OPERAND (t, 1);

      /* Fold read from constant string.  */
      if (TREE_CODE (ctor) == STRING_CST)
	{
	  if ((TYPE_MODE (TREE_TYPE (t))
	       == TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
	      && (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
	          == MODE_INT)
	      && GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor)))) == 1
	      && compare_tree_int (idx, TREE_STRING_LENGTH (ctor)) < 0)
	    return build_int_cst_type (TREE_TYPE (t),
				       (TREE_STRING_POINTER (ctor)
					[TREE_INT_CST_LOW (idx)]));
	  return NULL_TREE;
	}

      /* ???  Implement byte-offset indexing into a non-array CONSTRUCTOR.  */
      if (TREE_CODE (TREE_TYPE (ctor)) == ARRAY_TYPE
	  && (TYPE_MODE (TREE_TYPE (t))
	      == TYPE_MODE (TREE_TYPE (TREE_TYPE (ctor))))
	  && GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (t))) != 0
	  && integer_zerop
	       (int_const_binop
		  (TRUNC_MOD_EXPR, idx,
		   size_int (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (t)))), 0)))
	{
	  idx = int_const_binop (TRUNC_DIV_EXPR, idx,
				 size_int (GET_MODE_SIZE
					     (TYPE_MODE (TREE_TYPE (t)))), 0);
	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), cnt, cfield, cval)
	    if (tree_int_cst_equal (cfield, idx))
	      {
		STRIP_NOPS (cval);
		if (TREE_CODE (cval) == ADDR_EXPR)
		  {
		    tree base = get_base_address (TREE_OPERAND (cval, 0));
		    if (base && TREE_CODE (base) == VAR_DECL)
		      add_referenced_var (base);
		  }
		if (useless_type_conversion_p (TREE_TYPE (t), TREE_TYPE (cval)))
		  return cval;
		else if (CONSTANT_CLASS_P (cval))
		  return fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (t), cval);
		else
		  return NULL_TREE;
	      }
	}
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Evaluate statement STMT.
   Valid only for assignments, calls, conditionals, and switches. */

static prop_value_t
evaluate_stmt (gimple stmt)
{
  prop_value_t val;
  tree simplified = NULL_TREE;
  ccp_lattice_t likelyvalue = likely_value (stmt);
  bool is_constant;

  fold_defer_overflow_warnings ();

  /* If the statement is likely to have a CONSTANT result, then try
     to fold the statement to determine the constant value.  */
  /* FIXME.  This is the only place that we call ccp_fold.
     Since likely_value never returns CONSTANT for calls, we will
     not attempt to fold them, including builtins that may profit.  */
  if (likelyvalue == CONSTANT)
    simplified = ccp_fold (stmt);
  /* If the statement is likely to have a VARYING result, then do not
     bother folding the statement.  */
  else if (likelyvalue == VARYING)
    {
      enum gimple_code code = gimple_code (stmt);
      if (code == GIMPLE_ASSIGN)
        {
          enum tree_code subcode = gimple_assign_rhs_code (stmt);

          /* Other cases cannot satisfy is_gimple_min_invariant
             without folding.  */
          if (get_gimple_rhs_class (subcode) == GIMPLE_SINGLE_RHS)
            simplified = gimple_assign_rhs1 (stmt);
        }
      else if (code == GIMPLE_SWITCH)
        simplified = gimple_switch_index (stmt);
      else
	/* These cannot satisfy is_gimple_min_invariant without folding.  */
	gcc_assert (code == GIMPLE_CALL || code == GIMPLE_COND);
    }

  is_constant = simplified && is_gimple_min_invariant (simplified);

  fold_undefer_overflow_warnings (is_constant, stmt, 0);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "which is likely ");
      switch (likelyvalue)
	{
	case CONSTANT:
	  fprintf (dump_file, "CONSTANT");
	  break;
	case UNDEFINED:
	  fprintf (dump_file, "UNDEFINED");
	  break;
	case VARYING:
	  fprintf (dump_file, "VARYING");
	  break;
	default:;
	}
      fprintf (dump_file, "\n");
    }

  if (is_constant)
    {
      /* The statement produced a constant value.  */
      val.lattice_val = CONSTANT;
      val.value = simplified;
    }
  else
    {
      /* The statement produced a nonconstant value.  If the statement
	 had UNDEFINED operands, then the result of the statement
	 should be UNDEFINED.  Otherwise, the statement is VARYING.  */
      if (likelyvalue == UNDEFINED)
	val.lattice_val = likelyvalue;
      else
	val.lattice_val = VARYING;

      val.value = NULL_TREE;
    }

  return val;
}

/* Fold the stmt at *GSI with CCP specific information that propagating
   and regular folding does not catch.  */

static bool
ccp_fold_stmt (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      {
	prop_value_t val;
	/* Statement evaluation will handle type mismatches in constants
	   more gracefully than the final propagation.  This allows us to
	   fold more conditionals here.  */
	val = evaluate_stmt (stmt);
	if (val.lattice_val != CONSTANT
	    || TREE_CODE (val.value) != INTEGER_CST)
	  return false;

	if (integer_zerop (val.value))
	  gimple_cond_make_false (stmt);
	else
	  gimple_cond_make_true (stmt);

	return true;
      }

    case GIMPLE_CALL:
      {
	tree lhs = gimple_call_lhs (stmt);
	tree val;
	tree argt;
	bool changed = false;
	unsigned i;

	/* If the call was folded into a constant make sure it goes
	   away even if we cannot propagate into all uses because of
	   type issues.  */
	if (lhs
	    && TREE_CODE (lhs) == SSA_NAME
	    && (val = get_constant_value (lhs)))
	  {
	    tree new_rhs = unshare_expr (val);
	    bool res;
	    if (!useless_type_conversion_p (TREE_TYPE (lhs),
					    TREE_TYPE (new_rhs)))
	      new_rhs = fold_convert (TREE_TYPE (lhs), new_rhs);
	    res = update_call_from_tree (gsi, new_rhs);
	    gcc_assert (res);
	    return true;
	  }

	/* Propagate into the call arguments.  Compared to replace_uses_in
	   this can use the argument slot types for type verification
	   instead of the current argument type.  We also can safely
	   drop qualifiers here as we are dealing with constants anyway.  */
	argt = TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (gimple_call_fn (stmt))));
	for (i = 0; i < gimple_call_num_args (stmt) && argt;
	     ++i, argt = TREE_CHAIN (argt))
	  {
	    tree arg = gimple_call_arg (stmt, i);
	    if (TREE_CODE (arg) == SSA_NAME
		&& (val = get_constant_value (arg))
		&& useless_type_conversion_p
		     (TYPE_MAIN_VARIANT (TREE_VALUE (argt)),
		      TYPE_MAIN_VARIANT (TREE_TYPE (val))))
	      {
		gimple_call_set_arg (stmt, i, unshare_expr (val));
		changed = true;
	      }
	  }

	return changed;
      }

    case GIMPLE_ASSIGN:
      {
	tree lhs = gimple_assign_lhs (stmt);
	tree val;

	/* If we have a load that turned out to be constant replace it
	   as we cannot propagate into all uses in all cases.  */
	if (gimple_assign_single_p (stmt)
	    && TREE_CODE (lhs) == SSA_NAME
	    && (val = get_constant_value (lhs)))
	  {
	    tree rhs = unshare_expr (val);
	    if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	      rhs = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (lhs), rhs);
	    gimple_assign_set_rhs_from_tree (gsi, rhs);
	    return true;
	  }

	return false;
      }

    default:
      return false;
    }
}

/* Visit the assignment statement STMT.  Set the value of its LHS to the
   value computed by the RHS and store LHS in *OUTPUT_P.  If STMT
   creates virtual definitions, set the value of each new name to that
   of the RHS (if we can derive a constant out of the RHS).
   Value-returning call statements also perform an assignment, and
   are handled here.  */

static enum ssa_prop_result
visit_assignment (gimple stmt, tree *output_p)
{
  prop_value_t val;
  enum ssa_prop_result retval;

  tree lhs = gimple_get_lhs (stmt);

  gcc_assert (gimple_code (stmt) != GIMPLE_CALL
              || gimple_call_lhs (stmt) != NULL_TREE);

  if (gimple_assign_single_p (stmt)
      && gimple_assign_rhs_code (stmt) == SSA_NAME)
    /* For a simple copy operation, we copy the lattice values.  */
    val = *get_value (gimple_assign_rhs1 (stmt));
  else
    /* Evaluate the statement, which could be
       either a GIMPLE_ASSIGN or a GIMPLE_CALL.  */
    val = evaluate_stmt (stmt);

  retval = SSA_PROP_NOT_INTERESTING;

  /* Set the lattice value of the statement's output.  */
  if (TREE_CODE (lhs) == SSA_NAME)
    {
      /* If STMT is an assignment to an SSA_NAME, we only have one
	 value to set.  */
      if (set_lattice_value (lhs, val))
	{
	  *output_p = lhs;
	  if (val.lattice_val == VARYING)
	    retval = SSA_PROP_VARYING;
	  else
	    retval = SSA_PROP_INTERESTING;
	}
    }

  return retval;
}


/* Visit the conditional statement STMT.  Return SSA_PROP_INTERESTING
   if it can determine which edge will be taken.  Otherwise, return
   SSA_PROP_VARYING.  */

static enum ssa_prop_result
visit_cond_stmt (gimple stmt, edge *taken_edge_p)
{
  prop_value_t val;
  basic_block block;

  block = gimple_bb (stmt);
  val = evaluate_stmt (stmt);

  /* Find which edge out of the conditional block will be taken and add it
     to the worklist.  If no single edge can be determined statically,
     return SSA_PROP_VARYING to feed all the outgoing edges to the
     propagation engine.  */
  *taken_edge_p = val.value ? find_taken_edge (block, val.value) : 0;
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
ccp_visit_stmt (gimple stmt, edge *taken_edge_p, tree *output_p)
{
  tree def;
  ssa_op_iter iter;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting statement:\n");
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
    }

  switch (gimple_code (stmt))
    {
      case GIMPLE_ASSIGN:
        /* If the statement is an assignment that produces a single
           output value, evaluate its RHS to see if the lattice value of
           its output has changed.  */
        return visit_assignment (stmt, output_p);

      case GIMPLE_CALL:
        /* A value-returning call also performs an assignment.  */
        if (gimple_call_lhs (stmt) != NULL_TREE)
          return visit_assignment (stmt, output_p);
        break;

      case GIMPLE_COND:
      case GIMPLE_SWITCH:
        /* If STMT is a conditional branch, see if we can determine
           which branch will be taken.   */
        /* FIXME.  It appears that we should be able to optimize
           computed GOTOs here as well.  */
        return visit_cond_stmt (stmt, taken_edge_p);

      default:
        break;
    }

  /* Any other kind of statement is not interesting for constant
     propagation and, therefore, not worth simulating.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "No interesting values produced.  Marked VARYING.\n");

  /* Definitions made by statements other than assignments to
     SSA_NAMEs represent unknown modifications to their outputs.
     Mark them VARYING.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_ALL_DEFS)
    {
      prop_value_t v = { VARYING, NULL_TREE };
      set_lattice_value (def, v);
    }

  return SSA_PROP_VARYING;
}


/* Main entry point for SSA Conditional Constant Propagation.  */

static unsigned int
do_ssa_ccp (void)
{
  ccp_initialize ();
  ssa_propagate (ccp_visit_stmt, ccp_visit_phi_node);
  if (ccp_finalize ())
    return (TODO_cleanup_cfg | TODO_update_ssa | TODO_remove_unused_locals);
  else
    return 0;
}


static bool
gate_ccp (void)
{
  return flag_tree_ccp != 0;
}


struct gimple_opt_pass pass_ccp =
{
 {
  GIMPLE_PASS,
  "ccp",				/* name */
  gate_ccp,				/* gate */
  do_ssa_ccp,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CCP,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa
  | TODO_verify_stmts | TODO_ggc_collect/* todo_flags_finish */
 }
};



/* Try to optimize out __builtin_stack_restore.  Optimize it out
   if there is another __builtin_stack_restore in the same basic
   block and no calls or ASM_EXPRs are in between, or if this block's
   only outgoing edge is to EXIT_BLOCK and there are no calls or
   ASM_EXPRs after this __builtin_stack_restore.  */

static tree
optimize_stack_restore (gimple_stmt_iterator i)
{
  tree callee;
  gimple stmt;

  basic_block bb = gsi_bb (i);
  gimple call = gsi_stmt (i);

  if (gimple_code (call) != GIMPLE_CALL
      || gimple_call_num_args (call) != 1
      || TREE_CODE (gimple_call_arg (call, 0)) != SSA_NAME
      || !POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, 0))))
    return NULL_TREE;

  for (gsi_next (&i); !gsi_end_p (i); gsi_next (&i))
    {
      stmt = gsi_stmt (i);
      if (gimple_code (stmt) == GIMPLE_ASM)
	return NULL_TREE;
      if (gimple_code (stmt) != GIMPLE_CALL)
	continue;

      callee = gimple_call_fndecl (stmt);
      if (!callee
	  || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL
	  /* All regular builtins are ok, just obviously not alloca.  */
	  || DECL_FUNCTION_CODE (callee) == BUILT_IN_ALLOCA)
	return NULL_TREE;

      if (DECL_FUNCTION_CODE (callee) == BUILT_IN_STACK_RESTORE)
	goto second_stack_restore;
    }

  if (!gsi_end_p (i))
    return NULL_TREE;

  /* Allow one successor of the exit block, or zero successors.  */
  switch (EDGE_COUNT (bb->succs))
    {
    case 0:
      break;
    case 1:
      if (single_succ_edge (bb)->dest != EXIT_BLOCK_PTR)
	return NULL_TREE;
      break;
    default:
      return NULL_TREE;
    }
 second_stack_restore:

  /* If there's exactly one use, then zap the call to __builtin_stack_save.
     If there are multiple uses, then the last one should remove the call.
     In any case, whether the call to __builtin_stack_save can be removed
     or not is irrelevant to removing the call to __builtin_stack_restore.  */
  if (has_single_use (gimple_call_arg (call, 0)))
    {
      gimple stack_save = SSA_NAME_DEF_STMT (gimple_call_arg (call, 0));
      if (is_gimple_call (stack_save))
	{
	  callee = gimple_call_fndecl (stack_save);
	  if (callee
	      && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (callee) == BUILT_IN_STACK_SAVE)
	    {
	      gimple_stmt_iterator stack_save_gsi;
	      tree rhs;

	      stack_save_gsi = gsi_for_stmt (stack_save);
	      rhs = build_int_cst (TREE_TYPE (gimple_call_arg (call, 0)), 0);
	      update_call_from_tree (&stack_save_gsi, rhs);
	    }
	}
    }

  /* No effect, so the statement will be deleted.  */
  return integer_zero_node;
}

/* If va_list type is a simple pointer and nothing special is needed,
   optimize __builtin_va_start (&ap, 0) into ap = __builtin_next_arg (0),
   __builtin_va_end (&ap) out as NOP and __builtin_va_copy into a simple
   pointer assignment.  */

static tree
optimize_stdarg_builtin (gimple call)
{
  tree callee, lhs, rhs, cfun_va_list;
  bool va_list_simple_ptr;
  location_t loc = gimple_location (call);

  if (gimple_code (call) != GIMPLE_CALL)
    return NULL_TREE;

  callee = gimple_call_fndecl (call);

  cfun_va_list = targetm.fn_abi_va_list (callee);
  va_list_simple_ptr = POINTER_TYPE_P (cfun_va_list)
		       && (TREE_TYPE (cfun_va_list) == void_type_node
			   || TREE_TYPE (cfun_va_list) == char_type_node);

  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_VA_START:
      if (!va_list_simple_ptr
	  || targetm.expand_builtin_va_start != NULL
          || built_in_decls[BUILT_IN_NEXT_ARG] == NULL)
	return NULL_TREE;

      if (gimple_call_num_args (call) != 2)
	return NULL_TREE;

      lhs = gimple_call_arg (call, 0);
      if (!POINTER_TYPE_P (TREE_TYPE (lhs))
	  || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (lhs)))
	     != TYPE_MAIN_VARIANT (cfun_va_list))
	return NULL_TREE;

      lhs = build_fold_indirect_ref_loc (loc, lhs);
      rhs = build_call_expr_loc (loc, built_in_decls[BUILT_IN_NEXT_ARG],
                             1, integer_zero_node);
      rhs = fold_convert_loc (loc, TREE_TYPE (lhs), rhs);
      return build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, rhs);

    case BUILT_IN_VA_COPY:
      if (!va_list_simple_ptr)
	return NULL_TREE;

      if (gimple_call_num_args (call) != 2)
	return NULL_TREE;

      lhs = gimple_call_arg (call, 0);
      if (!POINTER_TYPE_P (TREE_TYPE (lhs))
	  || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (lhs)))
	     != TYPE_MAIN_VARIANT (cfun_va_list))
	return NULL_TREE;

      lhs = build_fold_indirect_ref_loc (loc, lhs);
      rhs = gimple_call_arg (call, 1);
      if (TYPE_MAIN_VARIANT (TREE_TYPE (rhs))
	  != TYPE_MAIN_VARIANT (cfun_va_list))
	return NULL_TREE;

      rhs = fold_convert_loc (loc, TREE_TYPE (lhs), rhs);
      return build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, rhs);

    case BUILT_IN_VA_END:
      /* No effect, so the statement will be deleted.  */
      return integer_zero_node;

    default:
      gcc_unreachable ();
    }
}

/* A simple pass that attempts to fold all builtin functions.  This pass
   is run after we've propagated as many constants as we can.  */

static unsigned int
execute_fold_all_builtins (void)
{
  bool cfg_changed = false;
  basic_block bb;
  unsigned int todoflags = 0;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator i;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); )
	{
          gimple stmt, old_stmt;
	  tree callee, result;
	  enum built_in_function fcode;

	  stmt = gsi_stmt (i);

          if (gimple_code (stmt) != GIMPLE_CALL)
	    {
	      gsi_next (&i);
	      continue;
	    }
	  callee = gimple_call_fndecl (stmt);
	  if (!callee || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL)
	    {
	      gsi_next (&i);
	      continue;
	    }
	  fcode = DECL_FUNCTION_CODE (callee);

	  result = gimple_fold_builtin (stmt);

	  if (result)
	    gimple_remove_stmt_histograms (cfun, stmt);

	  if (!result)
	    switch (DECL_FUNCTION_CODE (callee))
	      {
	      case BUILT_IN_CONSTANT_P:
		/* Resolve __builtin_constant_p.  If it hasn't been
		   folded to integer_one_node by now, it's fairly
		   certain that the value simply isn't constant.  */
                result = integer_zero_node;
		break;

	      case BUILT_IN_STACK_RESTORE:
		result = optimize_stack_restore (i);
		if (result)
		  break;
		gsi_next (&i);
		continue;

	      case BUILT_IN_VA_START:
	      case BUILT_IN_VA_END:
	      case BUILT_IN_VA_COPY:
		/* These shouldn't be folded before pass_stdarg.  */
		result = optimize_stdarg_builtin (stmt);
		if (result)
		  break;
		/* FALLTHRU */

	      default:
		gsi_next (&i);
		continue;
	      }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Simplified\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	    }

          old_stmt = stmt;
          if (!update_call_from_tree (&i, result))
	    {
	      gimplify_and_update_call_from_tree (&i, result);
	      todoflags |= TODO_update_address_taken;
	    }

	  stmt = gsi_stmt (i);
	  update_stmt (stmt);

	  if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt)
	      && gimple_purge_dead_eh_edges (bb))
	    cfg_changed = true;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "to\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	      fprintf (dump_file, "\n");
	    }

	  /* Retry the same statement if it changed into another
	     builtin, there might be new opportunities now.  */
          if (gimple_code (stmt) != GIMPLE_CALL)
	    {
	      gsi_next (&i);
	      continue;
	    }
	  callee = gimple_call_fndecl (stmt);
	  if (!callee
              || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL
	      || DECL_FUNCTION_CODE (callee) == fcode)
	    gsi_next (&i);
	}
    }

  /* Delete unreachable blocks.  */
  if (cfg_changed)
    todoflags |= TODO_cleanup_cfg;

  return todoflags;
}


struct gimple_opt_pass pass_fold_builtins =
{
 {
  GIMPLE_PASS,
  "fab",				/* name */
  NULL,					/* gate */
  execute_fold_all_builtins,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_verify_ssa
    | TODO_update_ssa			/* todo_flags_finish */
 }
};
