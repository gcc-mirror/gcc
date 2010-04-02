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


   Constant propagation in stores and loads (STORE-CCP)
   ----------------------------------------------------

   While CCP has all the logic to propagate constants in GIMPLE
   registers, it is missing the ability to associate constants with
   stores and loads (i.e., pointer dereferences, structures and
   global/aliased variables).  We don't keep loads and stores in
   SSA, but we do build a factored use-def web for them (in the
   virtual operands).

   For instance, consider the following code fragment:

	  struct A a;
	  const int B = 42;

	  void foo (int i)
	  {
	    if (i > 10)
	      a.a = 42;
	    else
	      {
		a.b = 21;
		a.a = a.b + 21;
	      }

	    if (a.a != B)
	      never_executed ();
	  }

   We should be able to deduce that the predicate 'a.a != B' is always
   false.  To achieve this, we associate constant values to the SSA
   names in the VDEF operands for each store.  Additionally,
   since we also glob partial loads/stores with the base symbol, we
   also keep track of the memory reference where the constant value
   was stored (in the MEM_REF field of PROP_VALUE_T).  For instance,

        # a_5 = VDEF <a_4>
        a.a = 2;

        # VUSE <a_5>
        x_3 = a.b;

   In the example above, CCP will associate value '2' with 'a_5', but
   it would be wrong to replace the load from 'a.b' with '2', because
   '2' had been stored into a.a.

   Note that the initial value of virtual operands is VARYING, not
   UNDEFINED.  Consider, for instance global variables:

   	int A;

   	foo (int i)
  	{
	  if (i_3 > 10)
	    A_4 = 3;
          # A_5 = PHI (A_4, A_2);

	  # VUSE <A_5>
	  A.0_6 = A;

	  return A.0_6;
	}

   The value of A_2 cannot be assumed to be UNDEFINED, as it may have
   been defined outside of foo.  If we were to assume it UNDEFINED, we
   would erroneously optimize the above into 'return 3;'.

   Though STORE-CCP is not too expensive, it does have to do more work
   than regular CCP, so it is only enabled at -O2.  Both regular CCP
   and STORE-CCP use the exact same algorithm.  The only distinction
   is that when doing STORE-CCP, the boolean variable DO_STORE_CCP is
   set to true.  This affects the evaluation of statements and PHI
   nodes.

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
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "value-prof.h"
#include "langhooks.h"
#include "target.h"
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

void
debug_lattice_value (prop_value_t val)
{
  dump_lattice_value (stderr, "", val);
  fprintf (stderr, "\n");
}



/* If SYM is a constant variable with known value, return the value.
   NULL_TREE is returned otherwise.  */

tree
get_symbol_constant_value (tree sym)
{
  if (TREE_STATIC (sym)
      && (TREE_READONLY (sym)
	  || TREE_CODE (sym) == CONST_DECL))
    {
      tree val = DECL_INITIAL (sym);
      if (val)
	{
	  STRIP_NOPS (val);
	  if (is_gimple_min_invariant (val))
	    {
	      if (TREE_CODE (val) == ADDR_EXPR)
		{
		  tree base = get_base_address (TREE_OPERAND (val, 0));
		  if (base && TREE_CODE (base) == VAR_DECL)
		    {
		      TREE_ADDRESSABLE (base) = 1;
		      if (gimple_referenced_vars (cfun))
			add_referenced_var (base);
		    }
		}
	      return val;
	    }
	}
      /* Variables declared 'const' without an initializer
	 have zero as the initializer if they may not be
	 overridden at link or run time.  */
      if (!val
	  && !DECL_EXTERNAL (sym)
	  && targetm.binds_local_p (sym)
          && (INTEGRAL_TYPE_P (TREE_TYPE (sym))
	       || SCALAR_FLOAT_TYPE_P (TREE_TYPE (sym))))
	return fold_convert (TREE_TYPE (sym), integer_zero_node);
    }

  return NULL_TREE;
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
      if (is_gimple_reg (sym) && TREE_CODE (sym) != PARM_DECL)
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
  prop_value_t *old_val = get_value (var);

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

      gcc_assert (new_val.lattice_val != UNDEFINED);
      return true;
    }

  return false;
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
  something_changed = substitute_and_fold (const_val, ccp_fold_stmt);

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
	 they come from the same memory reference.  */
      val1->lattice_val = CONSTANT;
      val1->value = val1->value;
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
	  prop_value_t arg_val;

	  if (is_gimple_min_invariant (arg))
	    {
	      arg_val.lattice_val = CONSTANT;
	      arg_val.value = arg;
	    }
	  else
	    arg_val = *(get_value (arg));

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

/* Return true if we may propagate the address expression ADDR into the
   dereference DEREF and cancel them.  */

bool
may_propagate_address_into_dereference (tree addr, tree deref)
{
  gcc_assert (INDIRECT_REF_P (deref)
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
                  return get_value (rhs)->value;
                }
	      /* Handle propagating invariant addresses into address operations.
		 The folding we do here matches that in tree-ssa-forwprop.c.  */
	      else if (TREE_CODE (rhs) == ADDR_EXPR)
		{
		  tree *base;
		  base = &TREE_OPERAND (rhs, 0);
		  while (handled_component_p (*base))
		    base = &TREE_OPERAND (*base, 0);
		  if (TREE_CODE (*base) == INDIRECT_REF
		      && TREE_CODE (TREE_OPERAND (*base, 0)) == SSA_NAME)
		    {
		      prop_value_t *val = get_value (TREE_OPERAND (*base, 0));
		      if (val->lattice_val == CONSTANT
			  && TREE_CODE (val->value) == ADDR_EXPR
			  && may_propagate_address_into_dereference
			       (val->value, *base))
			{
			  /* We need to return a new tree, not modify the IL
			     or share parts of it.  So play some tricks to
			     avoid manually building it.  */
			  tree ret, save = *base;
			  *base = TREE_OPERAND (val->value, 0);
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
		      if (TREE_CODE (val) == SSA_NAME
			  && get_value (val)->lattice_val == CONSTANT)
			val = get_value (val)->value;
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
		      prop_value_t *val = get_value (TREE_OPERAND (rhs, 0));
		      if (val->lattice_val == CONSTANT)
			return fold_unary_loc (EXPR_LOCATION (rhs),
					   TREE_CODE (rhs),
					   TREE_TYPE (rhs), val->value);
		    }
		  else if (TREE_CODE (rhs) == INDIRECT_REF
			   && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
		    {
		      prop_value_t *val = get_value (TREE_OPERAND (rhs, 0));
		      if (val->lattice_val == CONSTANT
			  && TREE_CODE (val->value) == ADDR_EXPR
			  && useless_type_conversion_p (TREE_TYPE (rhs),
							TREE_TYPE (TREE_TYPE (val->value))))
			rhs = TREE_OPERAND (val->value, 0);
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
              tree op0 = gimple_assign_rhs1 (stmt);

              /* Simplify the operand down to a constant.  */
              if (TREE_CODE (op0) == SSA_NAME)
                {
                  prop_value_t *val = get_value (op0);
                  if (val->lattice_val == CONSTANT)
                    op0 = get_value (op0)->value;
                }

	      /* Conversions are useless for CCP purposes if they are
		 value-preserving.  Thus the restrictions that
		 useless_type_conversion_p places for pointer type conversions
		 do not apply here.  Substitution later will only substitute to
		 allowed places.  */
	      if (CONVERT_EXPR_CODE_P (subcode)
		  && POINTER_TYPE_P (TREE_TYPE (lhs))
		  && POINTER_TYPE_P (TREE_TYPE (op0))
		  /* Do not allow differences in volatile qualification
		     as this might get us confused as to whether a
		     propagation destination statement is volatile
		     or not.  See PR36988.  */
		  && (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (lhs)))
		      == TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (op0)))))
		{
		  tree tem;
		  /* Still try to generate a constant of correct type.  */
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
              tree op0 = gimple_assign_rhs1 (stmt);
              tree op1 = gimple_assign_rhs2 (stmt);

              /* Simplify the operands down to constants when appropriate.  */
              if (TREE_CODE (op0) == SSA_NAME)
                {
                  prop_value_t *val = get_value (op0);
                  if (val->lattice_val == CONSTANT)
                    op0 = val->value;
                }

              if (TREE_CODE (op1) == SSA_NAME)
                {
                  prop_value_t *val = get_value (op1);
                  if (val->lattice_val == CONSTANT)
                    op1 = val->value;
                }

	      /* Fold &foo + CST into an invariant reference if possible.  */
	      if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
		  && TREE_CODE (op0) == ADDR_EXPR
		  && TREE_CODE (op1) == INTEGER_CST)
		{
		  tree tem = maybe_fold_offset_to_address
		    (loc, op0, op1, TREE_TYPE (op0));
		  if (tem != NULL_TREE)
		    return tem;
		}

              return fold_binary_loc (loc, subcode,
				  gimple_expr_type (stmt), op0, op1);
            }

          default:
            gcc_unreachable ();
          }
      }
      break;

    case GIMPLE_CALL:
      {
	tree fn = gimple_call_fn (stmt);
	prop_value_t *val;

	if (TREE_CODE (fn) == SSA_NAME)
	  {
	    val = get_value (fn);
	    if (val->lattice_val == CONSTANT)
	      fn = val->value;
	  }
	if (TREE_CODE (fn) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
	    && DECL_BUILT_IN (TREE_OPERAND (fn, 0)))
	  {
	    tree *args = XALLOCAVEC (tree, gimple_call_num_args (stmt));
	    tree call, retval;
	    unsigned i;
	    for (i = 0; i < gimple_call_num_args (stmt); ++i)
	      {
		args[i] = gimple_call_arg (stmt, i);
		if (TREE_CODE (args[i]) == SSA_NAME)
		  {
		    val = get_value (args[i]);
		    if (val->lattice_val == CONSTANT)
		      args[i] = val->value;
		  }
	      }
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
        tree op0 = gimple_cond_lhs (stmt);
        tree op1 = gimple_cond_rhs (stmt);
        enum tree_code code = gimple_cond_code (stmt);

        /* Simplify the operands down to constants when appropriate.  */
        if (TREE_CODE (op0) == SSA_NAME)
          {
            prop_value_t *val = get_value (op0);
            if (val->lattice_val == CONSTANT)
              op0 = val->value;
          }

        if (TREE_CODE (op1) == SSA_NAME)
          {
            prop_value_t *val = get_value (op1);
            if (val->lattice_val == CONSTANT)
              op1 = val->value;
          }

        return fold_binary_loc (loc, code, boolean_type_node, op0, op1);
      }

    case GIMPLE_SWITCH:
      {
        tree rhs = gimple_switch_index (stmt);

        if (TREE_CODE (rhs) == SSA_NAME)
          {
            /* If the RHS is an SSA_NAME, return its known constant value,
               if any.  */
            return get_value (rhs)->value;
          }

        return rhs;
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
  prop_value_t *value;
  tree base, ctor, idx, field;
  unsigned HOST_WIDE_INT cnt;
  tree cfield, cval;

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
	  if ((value = get_value (idx))
	      && value->lattice_val == CONSTANT
	      && TREE_CODE (value->value) == INTEGER_CST)
	    idx = value->value;
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

    case INDIRECT_REF:
      {
	tree base = TREE_OPERAND (t, 0);
	if (TREE_CODE (base) == SSA_NAME
	    && (value = get_value (base))
	    && value->lattice_val == CONSTANT
	    && TREE_CODE (value->value) == ADDR_EXPR
	    && useless_type_conversion_p (TREE_TYPE (t),
					  TREE_TYPE (TREE_TYPE (value->value))))
	  return fold_const_aggregate_ref (TREE_OPERAND (value->value, 0));
	break;
      }

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
	prop_value_t *val;
	tree argt;
	bool changed = false;
	unsigned i;

	/* If the call was folded into a constant make sure it goes
	   away even if we cannot propagate into all uses because of
	   type issues.  */
	if (lhs
	    && TREE_CODE (lhs) == SSA_NAME
	    && (val = get_value (lhs))
	    && val->lattice_val == CONSTANT)
	  {
	    tree new_rhs = unshare_expr (val->value);
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
		&& (val = get_value (arg))
		&& val->lattice_val == CONSTANT
		&& useless_type_conversion_p
		     (TYPE_MAIN_VARIANT (TREE_VALUE (argt)),
		      TYPE_MAIN_VARIANT (TREE_TYPE (val->value))))
	      {
		gimple_call_set_arg (stmt, i, unshare_expr (val->value));
		changed = true;
	      }
	  }

	return changed;
      }

    case GIMPLE_ASSIGN:
      {
	tree lhs = gimple_assign_lhs (stmt);
	prop_value_t *val;

	/* If we have a load that turned out to be constant replace it
	   as we cannot propagate into all uses in all cases.  */
	if (gimple_assign_single_p (stmt)
	    && TREE_CODE (lhs) == SSA_NAME
	    && (val = get_value (lhs))
	    && val->lattice_val == CONSTANT)
	  {
	    tree rhs = unshare_expr (val->value);
	    if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	      rhs = fold_convert (TREE_TYPE (lhs), rhs);
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

  if (gimple_assign_copy_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);

      if  (TREE_CODE (rhs) == SSA_NAME)
        {
          /* For a simple copy operation, we copy the lattice values.  */
          prop_value_t *nval = get_value (rhs);
          val = *nval;
        }
      else
        val = evaluate_stmt (stmt);
    }
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


/* A subroutine of fold_stmt.  Attempts to fold *(A+O) to A[X].
   BASE is an array type.  OFFSET is a byte displacement.  ORIG_TYPE
   is the desired result type.

   LOC is the location of the original expression.  */

static tree
maybe_fold_offset_to_array_ref (location_t loc, tree base, tree offset,
				tree orig_type,
				bool allow_negative_idx)
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
  if (!useless_type_conversion_p (orig_type, elt_type))
    return NULL_TREE;

  /* Use signed size type for intermediate computation on the index.  */
  idx_type = signed_type_for (size_type_node);

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
     give false warnings.  The same is true for
       struct A { long x; char d[0]; } *a;
       (char *)a - 4;
     which should be not folded to &a->d[-8].  */
  if (domain_type
      && TYPE_MAX_VALUE (domain_type)
      && TREE_CODE (TYPE_MAX_VALUE (domain_type)) == INTEGER_CST)
    {
      tree up_bound = TYPE_MAX_VALUE (domain_type);

      if (tree_int_cst_lt (up_bound, idx)
	  /* Accesses after the end of arrays of size 0 (gcc
	     extension) and 1 are likely intentional ("struct
	     hack").  */
	  && compare_tree_int (up_bound, 1) > 0)
	return NULL_TREE;
    }
  if (domain_type
      && TYPE_MIN_VALUE (domain_type))
    {
      if (!allow_negative_idx
	  && TREE_CODE (TYPE_MIN_VALUE (domain_type)) == INTEGER_CST
	  && tree_int_cst_lt (idx, TYPE_MIN_VALUE (domain_type)))
	return NULL_TREE;
    }
  else if (!allow_negative_idx
	   && compare_tree_int (idx, 0) < 0)
    return NULL_TREE;

  {
    tree t = build4 (ARRAY_REF, elt_type, base, idx, NULL_TREE, NULL_TREE);
    SET_EXPR_LOCATION (t, loc);
    return t;
  }
}


/* Attempt to fold *(S+O) to S.X.
   BASE is a record type.  OFFSET is a byte displacement.  ORIG_TYPE
   is the desired result type.

   LOC is the location of the original expression.  */

static tree
maybe_fold_offset_to_component_ref (location_t loc, tree record_type,
				    tree base, tree offset, tree orig_type)
{
  tree f, t, field_type, tail_array_field, field_offset;
  tree ret;
  tree new_base;

  if (TREE_CODE (record_type) != RECORD_TYPE
      && TREE_CODE (record_type) != UNION_TYPE
      && TREE_CODE (record_type) != QUAL_UNION_TYPE)
    return NULL_TREE;

  /* Short-circuit silly cases.  */
  if (useless_type_conversion_p (record_type, orig_type))
    return NULL_TREE;

  tail_array_field = NULL_TREE;
  for (f = TYPE_FIELDS (record_type); f ; f = TREE_CHAIN (f))
    {
      int cmp;

      if (TREE_CODE (f) != FIELD_DECL)
	continue;
      if (DECL_BIT_FIELD (f))
	continue;

      if (!DECL_FIELD_OFFSET (f))
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
	  && useless_type_conversion_p (orig_type, field_type))
	{
	  t = build3 (COMPONENT_REF, field_type, base, f, NULL_TREE);
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
      new_base = build3 (COMPONENT_REF, field_type, base, f, NULL_TREE);
      SET_EXPR_LOCATION (new_base, loc);

      /* Recurse to possibly find the match.  */
      ret = maybe_fold_offset_to_array_ref (loc, new_base, t, orig_type,
					    f == TYPE_FIELDS (record_type));
      if (ret)
	return ret;
      ret = maybe_fold_offset_to_component_ref (loc, field_type, new_base, t,
						orig_type);
      if (ret)
	return ret;
    }

  if (!tail_array_field)
    return NULL_TREE;

  f = tail_array_field;
  field_type = TREE_TYPE (f);
  offset = int_const_binop (MINUS_EXPR, offset, byte_position (f), 1);

  /* If we get here, we've got an aggregate field, and a possibly
     nonzero offset into them.  Recurse and hope for a valid match.  */
  base = build3 (COMPONENT_REF, field_type, base, f, NULL_TREE);
  SET_EXPR_LOCATION (base, loc);

  t = maybe_fold_offset_to_array_ref (loc, base, offset, orig_type,
				      f == TYPE_FIELDS (record_type));
  if (t)
    return t;
  return maybe_fold_offset_to_component_ref (loc, field_type, base, offset,
					     orig_type);
}

/* Attempt to express (ORIG_TYPE)BASE+OFFSET as BASE->field_of_orig_type
   or BASE[index] or by combination of those.

   LOC is the location of original expression.

   Before attempting the conversion strip off existing ADDR_EXPRs and
   handled component refs.  */

tree
maybe_fold_offset_to_reference (location_t loc, tree base, tree offset,
				tree orig_type)
{
  tree ret;
  tree type;

  STRIP_NOPS (base);
  if (TREE_CODE (base) != ADDR_EXPR)
    return NULL_TREE;

  base = TREE_OPERAND (base, 0);

  /* Handle case where existing COMPONENT_REF pick e.g. wrong field of union,
     so it needs to be removed and new COMPONENT_REF constructed.
     The wrong COMPONENT_REF are often constructed by folding the
     (type *)&object within the expression (type *)&object+offset  */
  if (handled_component_p (base))
    {
      HOST_WIDE_INT sub_offset, size, maxsize;
      tree newbase;
      newbase = get_ref_base_and_extent (base, &sub_offset,
					 &size, &maxsize);
      gcc_assert (newbase);
      if (size == maxsize
	  && size != -1
	  && !(sub_offset & (BITS_PER_UNIT - 1)))
	{
	  base = newbase;
	  if (sub_offset)
	    offset = int_const_binop (PLUS_EXPR, offset,
				      build_int_cst (TREE_TYPE (offset),
						     sub_offset / BITS_PER_UNIT), 1);
	}
    }
  if (useless_type_conversion_p (orig_type, TREE_TYPE (base))
      && integer_zerop (offset))
    return base;
  type = TREE_TYPE (base);

  ret = maybe_fold_offset_to_component_ref (loc, type, base, offset, orig_type);
  if (!ret)
    ret = maybe_fold_offset_to_array_ref (loc, base, offset, orig_type, true);

  return ret;
}

/* Attempt to express (ORIG_TYPE)&BASE+OFFSET as &BASE->field_of_orig_type
   or &BASE[index] or by combination of those.

   LOC is the location of the original expression.

   Before attempting the conversion strip off existing component refs.  */

tree
maybe_fold_offset_to_address (location_t loc, tree addr, tree offset,
			      tree orig_type)
{
  tree t;

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (addr))
	      && POINTER_TYPE_P (orig_type));

  t = maybe_fold_offset_to_reference (loc, addr, offset,
				      TREE_TYPE (orig_type));
  if (t != NULL_TREE)
    {
      tree orig = addr;
      tree ptr_type;

      /* For __builtin_object_size to function correctly we need to
         make sure not to fold address arithmetic so that we change
	 reference from one array to another.  This would happen for
	 example for

	   struct X { char s1[10]; char s2[10] } s;
	   char *foo (void) { return &s.s2[-4]; }

	 where we need to avoid generating &s.s1[6].  As the C and
	 C++ frontends create different initial trees
	 (char *) &s.s1 + -4  vs.  &s.s1[-4]  we have to do some
	 sophisticated comparisons here.  Note that checking for the
	 condition after the fact is easier than trying to avoid doing
	 the folding.  */
      STRIP_NOPS (orig);
      if (TREE_CODE (orig) == ADDR_EXPR)
	orig = TREE_OPERAND (orig, 0);
      if ((TREE_CODE (orig) == ARRAY_REF
	   || (TREE_CODE (orig) == COMPONENT_REF
	       && TREE_CODE (TREE_TYPE (TREE_OPERAND (orig, 1))) == ARRAY_TYPE))
	  && (TREE_CODE (t) == ARRAY_REF
	      || TREE_CODE (t) == COMPONENT_REF)
	  && !operand_equal_p (TREE_CODE (orig) == ARRAY_REF
			       ? TREE_OPERAND (orig, 0) : orig,
			       TREE_CODE (t) == ARRAY_REF
			       ? TREE_OPERAND (t, 0) : t, 0))
	return NULL_TREE;

      ptr_type = build_pointer_type (TREE_TYPE (t));
      if (!useless_type_conversion_p (orig_type, ptr_type))
	return NULL_TREE;
      return build_fold_addr_expr_with_type_loc (loc, t, ptr_type);
    }

  return NULL_TREE;
}

/* A subroutine of fold_stmt.  Attempt to simplify *(BASE+OFFSET).
   Return the simplified expression, or NULL if nothing could be done.  */

static tree
maybe_fold_stmt_indirect (tree expr, tree base, tree offset)
{
  tree t;
  bool volatile_p = TREE_THIS_VOLATILE (expr);
  location_t loc = EXPR_LOCATION (expr);

  /* We may well have constructed a double-nested PLUS_EXPR via multiple
     substitutions.  Fold that down to one.  Remove NON_LVALUE_EXPRs that
     are sometimes added.  */
  base = fold (base);
  STRIP_TYPE_NOPS (base);
  TREE_OPERAND (expr, 0) = base;

  /* One possibility is that the address reduces to a string constant.  */
  t = fold_read_from_constant_string (expr);
  if (t)
    return t;

  /* Add in any offset from a POINTER_PLUS_EXPR.  */
  if (TREE_CODE (base) == POINTER_PLUS_EXPR)
    {
      tree offset2;

      offset2 = TREE_OPERAND (base, 1);
      if (TREE_CODE (offset2) != INTEGER_CST)
	return NULL_TREE;
      base = TREE_OPERAND (base, 0);

      offset = fold_convert (sizetype,
			     int_const_binop (PLUS_EXPR, offset, offset2, 1));
    }

  if (TREE_CODE (base) == ADDR_EXPR)
    {
      tree base_addr = base;

      /* Strip the ADDR_EXPR.  */
      base = TREE_OPERAND (base, 0);

      /* Fold away CONST_DECL to its value, if the type is scalar.  */
      if (TREE_CODE (base) == CONST_DECL
	  && is_gimple_min_invariant (DECL_INITIAL (base)))
	return DECL_INITIAL (base);

      /* If there is no offset involved simply return the folded base.  */
      if (integer_zerop (offset))
	return base;

      /* Try folding *(&B+O) to B.X.  */
      t = maybe_fold_offset_to_reference (loc, base_addr, offset,
					  TREE_TYPE (expr));
      if (t)
	{
	  /* Preserve volatileness of the original expression.
	     We can end up with a plain decl here which is shared
	     and we shouldn't mess with its flags.  */
	  if (!SSA_VAR_P (t))
	    TREE_THIS_VOLATILE (t) = volatile_p;
	  return t;
	}
    }
  else
    {
      /* We can get here for out-of-range string constant accesses,
	 such as "_"[3].  Bail out of the entire substitution search
	 and arrange for the entire statement to be replaced by a
	 call to __builtin_trap.  In all likelihood this will all be
	 constant-folded away, but in the meantime we can't leave with
	 something that get_expr_operands can't understand.  */

      t = base;
      STRIP_NOPS (t);
      if (TREE_CODE (t) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	{
	  /* FIXME: Except that this causes problems elsewhere with dead
	     code not being deleted, and we die in the rtl expanders
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
          t = maybe_fold_offset_to_reference (loc, base, offset,
				              TREE_TYPE (expr));
	  if (t)
	    return t;
	}
    }

  /* Otherwise we had an offset that we could not simplify.  */
  return NULL_TREE;
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
	  && integer_zerop (TREE_OPERAND (op0, 1))
	  && TREE_CODE (op1) == SSA_NAME
	  && host_integerp (TYPE_SIZE_UNIT (TREE_TYPE (op0)), 1))
	{
	  gimple offset_def = SSA_NAME_DEF_STMT (op1);
	  if (!is_gimple_assign (offset_def))
	    return NULL_TREE;

	  if (gimple_assign_rhs_code (offset_def) == MULT_EXPR
	      && TREE_CODE (gimple_assign_rhs2 (offset_def)) == INTEGER_CST
	      && tree_int_cst_equal (gimple_assign_rhs2 (offset_def),
				     TYPE_SIZE_UNIT (TREE_TYPE (op0))))
	    return build_fold_addr_expr
			  (build4 (ARRAY_REF, TREE_TYPE (op0),
				   TREE_OPERAND (op0, 0),
				   gimple_assign_rhs1 (offset_def),
				   TREE_OPERAND (op0, 2),
				   TREE_OPERAND (op0, 3)));
	  else if (integer_onep (TYPE_SIZE_UNIT (TREE_TYPE (op0)))
		   && gimple_assign_rhs_code (offset_def) != MULT_EXPR)
	    return build_fold_addr_expr
			  (build4 (ARRAY_REF, TREE_TYPE (op0),
				   TREE_OPERAND (op0, 0),
				   op1,
				   TREE_OPERAND (op0, 2),
				   TREE_OPERAND (op0, 3)));
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
  t = maybe_fold_offset_to_array_ref (loc, op0, op1, ptd_type, true);
  if (!t)
    t = maybe_fold_offset_to_component_ref (loc, TREE_TYPE (op0), op0, op1,
					    ptd_type);
  if (t)
    {
      t = build1 (ADDR_EXPR, res_type, t);
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

  if (TREE_CODE (expr) == ARRAY_REF
      && !is_lhs)
    {
      tree tem = fold_read_from_constant_string (expr);
      if (tem)
	return tem;
    }

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

  if (TREE_CODE (*t) == INDIRECT_REF)
    {
      tree tem = maybe_fold_stmt_indirect (*t, TREE_OPERAND (*t, 0),
					   integer_zero_node);
      /* Avoid folding *"abc" = 5 into 'a' = 5.  */
      if (is_lhs && tem && CONSTANT_CLASS_P (tem))
	tem = NULL_TREE;
      if (!tem
	  && TREE_CODE (TREE_OPERAND (*t, 0)) == ADDR_EXPR)
	/* If we had a good reason for propagating the address here,
	   make sure we end up with valid gimple.  See PR34989.  */
	tem = TREE_OPERAND (TREE_OPERAND (*t, 0), 0);

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
  if (bitmap_bit_p (visited, SSA_NAME_VERSION (arg)))
    return true;
  bitmap_set_bit (visited, SSA_NAME_VERSION (arg));

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

static tree
ccp_fold_builtin (gimple stmt)
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
	     of a valid gimple value, then we can not use the result.  */
	  if (is_gimple_val (new_val)
	      || (is_gimple_cast (new_val)
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

	else if (TREE_CODE (rhs) == TARGET_MEM_REF)
	  return maybe_fold_tmr (rhs);

	else if (REFERENCE_CLASS_P (rhs))
	  return maybe_fold_reference (rhs, false);

	else if (TREE_CODE (rhs) == ADDR_EXPR)
	  {
	    tree tem = maybe_fold_reference (TREE_OPERAND (rhs, 0), true);
	    if (tem)
	      result = fold_convert (TREE_TYPE (rhs),
				     build_fold_addr_expr_loc (loc, tem));
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

static void gimplify_and_update_call_from_tree (gimple_stmt_iterator *, tree);

/* Attempt to fold a call statement referenced by the statement iterator GSI.
   The statement may be replaced by another statement, e.g., if the call
   simplifies to a constant value. Return true if any changes were made.
   It is assumed that the operands have been previously folded.  */

static bool
fold_gimple_call (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);

  tree callee = gimple_call_fndecl (stmt);

  /* Check for builtins that CCP can handle using information not
     available in the generic fold routines.  */
  if (callee && DECL_BUILT_IN (callee))
    {
      tree result = ccp_fold_builtin (stmt);

      if (result)
	{
          if (!update_call_from_tree (gsi, result))
	    gimplify_and_update_call_from_tree (gsi, result);
	  return true;
	}
    }
  else
    {
      /* Check for resolvable OBJ_TYPE_REF.  The only sorts we can resolve
         here are when we've propagated the address of a decl into the
         object slot.  */
      /* ??? Should perhaps do this in fold proper.  However, doing it
         there requires that we create a new CALL_EXPR, and that requires
         copying EH region info to the new node.  Easier to just do it
         here where we can just smash the call operand.  */
      /* ??? Is there a good reason not to do this in fold_stmt_inplace?  */
      callee = gimple_call_fn (stmt);
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
          if (t)
            {
              gimple_call_set_fn (stmt, t);
              return true;
            }
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
      /* The entire statement may be replaced in this case.  */
      if (!inplace)
	changed |= fold_gimple_call (gsi);
      break;

    case GIMPLE_ASM:
      /* Fold *& in asm operands.  */
      for (i = 0; i < gimple_asm_noutputs (stmt); ++i)
	{
	  tree link = gimple_asm_output_op (stmt, i);
	  tree op = TREE_VALUE (link);
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
	  if (REFERENCE_CLASS_P (op)
	      && (op = maybe_fold_reference (op, false)) != NULL_TREE)
	    {
	      TREE_VALUE (link) = op;
	      changed = true;
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

/* Convert EXPR into a GIMPLE value suitable for substitution on the
   RHS of an assignment.  Insert the necessary statements before
   iterator *SI_P.  The statement at *SI_P, which must be a GIMPLE_CALL
   is replaced.  If the call is expected to produces a result, then it
   is replaced by an assignment of the new RHS to the result variable.
   If the result is to be ignored, then the call is replaced by a
   GIMPLE_NOP.  */

static void
gimplify_and_update_call_from_tree (gimple_stmt_iterator *si_p, tree expr)
{
  tree lhs;
  tree tmp = NULL_TREE;  /* Silence warning.  */
  gimple stmt, new_stmt;
  gimple_stmt_iterator i;
  gimple_seq stmts = gimple_seq_alloc();
  struct gimplify_ctx gctx;

  stmt = gsi_stmt (*si_p);

  gcc_assert (is_gimple_call (stmt));

  lhs = gimple_call_lhs (stmt);

  push_gimplify_context (&gctx);

  if (lhs == NULL_TREE)
    gimplify_and_add (expr, &stmts);
  else
    tmp = get_initialized_tmp_var (expr, &stmts, NULL);

  pop_gimplify_context (NULL);

  if (gimple_has_location (stmt))
    annotate_all_with_location (stmts, gimple_location (stmt));

  /* The replacement can expose previously unreferenced variables.  */
  for (i = gsi_start (stmts); !gsi_end_p (i); gsi_next (&i))
  {
    new_stmt = gsi_stmt (i);
    find_new_referenced_vars (new_stmt);
    gsi_insert_before (si_p, new_stmt, GSI_NEW_STMT);
    mark_symbols_for_renaming (new_stmt);
    gsi_next (si_p);
  }

  if (lhs == NULL_TREE)
    {
      new_stmt = gimple_build_nop ();
      unlink_stmt_vdef (stmt);
      release_defs (stmt);
    }
  else
    {
      new_stmt = gimple_build_assign (lhs, tmp);
      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
      gimple_set_vdef (new_stmt, gimple_vdef (stmt));
      move_ssa_defining_stmt_for_defs (new_stmt, stmt);
    }

  gimple_set_location (new_stmt, gimple_location (stmt));
  gsi_replace (si_p, new_stmt, false);
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

	  result = ccp_fold_builtin (stmt);

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
