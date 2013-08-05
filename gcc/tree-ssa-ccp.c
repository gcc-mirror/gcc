/* Conditional constant propagation pass for the GNU compiler.
   Copyright (C) 2000-2013 Free Software Foundation, Inc.
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
#include "function.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "value-prof.h"
#include "langhooks.h"
#include "target.h"
#include "diagnostic-core.h"
#include "dbgcnt.h"
#include "gimple-fold.h"
#include "params.h"
#include "hash-table.h"


/* Possible lattice values.  */
typedef enum
{
  UNINITIALIZED,
  UNDEFINED,
  CONSTANT,
  VARYING
} ccp_lattice_t;

struct prop_value_d {
    /* Lattice value.  */
    ccp_lattice_t lattice_val;

    /* Propagated value.  */
    tree value;

    /* Mask that applies to the propagated value during CCP.  For
       X with a CONSTANT lattice value X & ~mask == value & ~mask.  */
    double_int mask;
};

typedef struct prop_value_d prop_value_t;

/* Array of propagated constant values.  After propagation,
   CONST_VAL[I].VALUE holds the constant value for SSA_NAME(I).  If
   the constant is held in an SSA name representing a memory store
   (i.e., a VDEF), CONST_VAL[I].MEM_REF will contain the actual
   memory reference used to store (i.e., the LHS of the assignment
   doing the store).  */
static prop_value_t *const_val;
static unsigned n_const_val;

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
      if (TREE_CODE (val.value) != INTEGER_CST
	  || val.mask.is_zero ())
	{
	  fprintf (outf, "%sCONSTANT ", prefix);
	  print_generic_expr (outf, val.value, dump_flags);
	}
      else
	{
	  double_int cval = tree_to_double_int (val.value).and_not (val.mask);
	  fprintf (outf, "%sCONSTANT " HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		   prefix, cval.high, cval.low);
	  fprintf (outf, " (" HOST_WIDE_INT_PRINT_DOUBLE_HEX ")",
		   val.mask.high, val.mask.low);
	}
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
  prop_value_t val = { UNINITIALIZED, NULL_TREE, { 0, 0 } };
  gimple stmt;

  stmt = SSA_NAME_DEF_STMT (var);

  if (gimple_nop_p (stmt))
    {
      /* Variables defined by an empty statement are those used
	 before being initialized.  If VAR is a local variable, we
	 can assume initially that it is UNDEFINED, otherwise we must
	 consider it VARYING.  */
      if (!virtual_operand_p (var)
	  && TREE_CODE (SSA_NAME_VAR (var)) == VAR_DECL)
	val.lattice_val = UNDEFINED;
      else
	{
	  val.lattice_val = VARYING;
	  val.mask = double_int_minus_one;
	}
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
      val.mask = double_int_minus_one;
    }

  return val;
}


/* Get the constant value associated with variable VAR.  */

static inline prop_value_t *
get_value (tree var)
{
  prop_value_t *val;

  if (const_val == NULL
      || SSA_NAME_VERSION (var) >= n_const_val)
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
  prop_value_t *val;
  if (TREE_CODE (var) != SSA_NAME)
    {
      if (is_gimple_min_invariant (var))
        return var;
      return NULL_TREE;
    }
  val = get_value (var);
  if (val
      && val->lattice_val == CONSTANT
      && (TREE_CODE (val->value) != INTEGER_CST
	  || val->mask.is_zero ()))
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
  val->mask = double_int_minus_one;
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

/* Return whether the lattice transition is valid.  */

static bool
valid_lattice_transition (prop_value_t old_val, prop_value_t new_val)
{
  /* Lattice transitions must always be monotonically increasing in
     value.  */
  if (old_val.lattice_val < new_val.lattice_val)
    return true;

  if (old_val.lattice_val != new_val.lattice_val)
    return false;

  if (!old_val.value && !new_val.value)
    return true;

  /* Now both lattice values are CONSTANT.  */

  /* Allow transitioning from PHI <&x, not executable> == &x
     to PHI <&x, &y> == common alignment.  */
  if (TREE_CODE (old_val.value) != INTEGER_CST
      && TREE_CODE (new_val.value) == INTEGER_CST)
    return true;

  /* Bit-lattices have to agree in the still valid bits.  */
  if (TREE_CODE (old_val.value) == INTEGER_CST
      && TREE_CODE (new_val.value) == INTEGER_CST)
    return tree_to_double_int (old_val.value).and_not (new_val.mask)
	   == tree_to_double_int (new_val.value).and_not (new_val.mask);

  /* Otherwise constant values have to agree.  */
  return operand_equal_p (old_val.value, new_val.value, 0);
}

/* Set the value for variable VAR to NEW_VAL.  Return true if the new
   value is different from VAR's previous value.  */

static bool
set_lattice_value (tree var, prop_value_t new_val)
{
  /* We can deal with old UNINITIALIZED values just fine here.  */
  prop_value_t *old_val = &const_val[SSA_NAME_VERSION (var)];

  canonicalize_float_value (&new_val);

  /* We have to be careful to not go up the bitwise lattice
     represented by the mask.
     ???  This doesn't seem to be the best place to enforce this.  */
  if (new_val.lattice_val == CONSTANT
      && old_val->lattice_val == CONSTANT
      && TREE_CODE (new_val.value) == INTEGER_CST
      && TREE_CODE (old_val->value) == INTEGER_CST)
    {
      double_int diff;
      diff = tree_to_double_int (new_val.value)
	     ^ tree_to_double_int (old_val->value);
      new_val.mask = new_val.mask | old_val->mask | diff;
    }

  gcc_assert (valid_lattice_transition (*old_val, new_val));

  /* If *OLD_VAL and NEW_VAL are the same, return false to inform the
     caller that this was a non-transition.  */
  if (old_val->lattice_val != new_val.lattice_val
      || (new_val.lattice_val == CONSTANT
	  && TREE_CODE (new_val.value) == INTEGER_CST
	  && (TREE_CODE (old_val->value) != INTEGER_CST
	      || new_val.mask != old_val->mask)))
    {
      /* ???  We would like to delay creation of INTEGER_CSTs from
	 partially constants here.  */

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

static prop_value_t get_value_for_expr (tree, bool);
static prop_value_t bit_value_binop (enum tree_code, tree, tree, tree);
static void bit_value_binop_1 (enum tree_code, tree, double_int *, double_int *,
			       tree, double_int, double_int,
			       tree, double_int, double_int);

/* Return a double_int that can be used for bitwise simplifications
   from VAL.  */

static double_int
value_to_double_int (prop_value_t val)
{
  if (val.value
      && TREE_CODE (val.value) == INTEGER_CST)
    return tree_to_double_int (val.value);
  else
    return double_int_zero;
}

/* Return the value for the address expression EXPR based on alignment
   information.  */

static prop_value_t
get_value_from_alignment (tree expr)
{
  tree type = TREE_TYPE (expr);
  prop_value_t val;
  unsigned HOST_WIDE_INT bitpos;
  unsigned int align;

  gcc_assert (TREE_CODE (expr) == ADDR_EXPR);

  get_pointer_alignment_1 (expr, &align, &bitpos);
  val.mask = (POINTER_TYPE_P (type) || TYPE_UNSIGNED (type)
	      ? double_int::mask (TYPE_PRECISION (type))
	      : double_int_minus_one)
	     .and_not (double_int::from_uhwi (align / BITS_PER_UNIT - 1));
  val.lattice_val = val.mask.is_minus_one () ? VARYING : CONSTANT;
  if (val.lattice_val == CONSTANT)
    val.value
      = double_int_to_tree (type,
			    double_int::from_uhwi (bitpos / BITS_PER_UNIT));
  else
    val.value = NULL_TREE;

  return val;
}

/* Return the value for the tree operand EXPR.  If FOR_BITS_P is true
   return constant bits extracted from alignment information for
   invariant addresses.  */

static prop_value_t
get_value_for_expr (tree expr, bool for_bits_p)
{
  prop_value_t val;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      val = *get_value (expr);
      if (for_bits_p
	  && val.lattice_val == CONSTANT
	  && TREE_CODE (val.value) == ADDR_EXPR)
	val = get_value_from_alignment (val.value);
    }
  else if (is_gimple_min_invariant (expr)
	   && (!for_bits_p || TREE_CODE (expr) != ADDR_EXPR))
    {
      val.lattice_val = CONSTANT;
      val.value = expr;
      val.mask = double_int_zero;
      canonicalize_float_value (&val);
    }
  else if (TREE_CODE (expr) == ADDR_EXPR)
    val = get_value_from_alignment (expr);
  else
    {
      val.lattice_val = VARYING;
      val.mask = double_int_minus_one;
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

	case ADDR_EXPR:
	  /* If any part of an address is UNDEFINED, like the index
	     of an ARRAY_EXPR, then treat the result as UNDEFINED.  */
	  return UNDEFINED;

	default:
	  ;
	}
    }
  /* If there was an UNDEFINED operand but the result may be not UNDEFINED
     fall back to CONSTANT.  During iteration UNDEFINED may still drop
     to CONSTANT.  */
  if (has_undefined_operand)
    return CONSTANT;

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

  n_const_val = num_ssa_names;
  const_val = XCNEWVEC (prop_value_t, n_const_val);

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

	  if (virtual_operand_p (gimple_phi_result (phi)))
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
	  const_val[i].mask = double_int_minus_one;
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
  unsigned i;

  do_dbg_cnt ();

  /* Derive alignment and misalignment information from partially
     constant pointers in the lattice.  */
  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      prop_value_t *val;
      unsigned int tem, align;

      if (!name
	  || !POINTER_TYPE_P (TREE_TYPE (name)))
	continue;

      val = get_value (name);
      if (val->lattice_val != CONSTANT
	  || TREE_CODE (val->value) != INTEGER_CST)
	continue;

      /* Trailing constant bits specify the alignment, trailing value
	 bits the misalignment.  */
      tem = val->mask.low;
      align = (tem & -tem);
      if (align > 1)
	set_ptr_info_alignment (get_ptr_info (name), align,
				TREE_INT_CST_LOW (val->value) & (align - 1));
    }

  /* Perform substitutions based on the known constant values.  */
  something_changed = substitute_and_fold (get_constant_value,
					   ccp_fold_stmt, true);

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
      val1->mask = double_int_minus_one;
      val1->value = NULL_TREE;
    }
  else if (val1->lattice_val == CONSTANT
	   && val2->lattice_val == CONSTANT
	   && TREE_CODE (val1->value) == INTEGER_CST
	   && TREE_CODE (val2->value) == INTEGER_CST)
    {
      /* Ci M Cj = Ci		if (i == j)
	 Ci M Cj = VARYING	if (i != j)

         For INTEGER_CSTs mask unequal bits.  If no equal bits remain,
	 drop to varying.  */
      val1->mask = val1->mask | val2->mask
		   | (tree_to_double_int (val1->value)
		      ^ tree_to_double_int (val2->value));
      if (val1->mask.is_minus_one ())
	{
	  val1->lattice_val = VARYING;
	  val1->value = NULL_TREE;
	}
    }
  else if (val1->lattice_val == CONSTANT
	   && val2->lattice_val == CONSTANT
	   && simple_cst_equal (val1->value, val2->value) == 1)
    {
      /* Ci M Cj = Ci		if (i == j)
	 Ci M Cj = VARYING	if (i != j)

         VAL1 already contains the value we want for equivalent values.  */
    }
  else if (val1->lattice_val == CONSTANT
	   && val2->lattice_val == CONSTANT
	   && (TREE_CODE (val1->value) == ADDR_EXPR
	       || TREE_CODE (val2->value) == ADDR_EXPR))
    {
      /* When not equal addresses are involved try meeting for
	 alignment.  */
      prop_value_t tem = *val2;
      if (TREE_CODE (val1->value) == ADDR_EXPR)
	*val1 = get_value_for_expr (val1->value, true);
      if (TREE_CODE (val2->value) == ADDR_EXPR)
	tem = get_value_for_expr (val2->value, true);
      ccp_lattice_meet (val1, &tem);
    }
  else
    {
      /* Any other combination is VARYING.  */
      val1->lattice_val = VARYING;
      val1->mask = double_int_minus_one;
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
	  prop_value_t arg_val = get_value_for_expr (arg, false);

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

    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      return gimple_fold_stmt_to_constant_1 (stmt, valueize_op);

    default:
      gcc_unreachable ();
    }
}

/* Apply the operation CODE in type TYPE to the value, mask pair
   RVAL and RMASK representing a value of type RTYPE and set
   the value, mask pair *VAL and *MASK to the result.  */

static void
bit_value_unop_1 (enum tree_code code, tree type,
		  double_int *val, double_int *mask,
		  tree rtype, double_int rval, double_int rmask)
{
  switch (code)
    {
    case BIT_NOT_EXPR:
      *mask = rmask;
      *val = ~rval;
      break;

    case NEGATE_EXPR:
      {
	double_int temv, temm;
	/* Return ~rval + 1.  */
	bit_value_unop_1 (BIT_NOT_EXPR, type, &temv, &temm, type, rval, rmask);
	bit_value_binop_1 (PLUS_EXPR, type, val, mask,
			 type, temv, temm,
			 type, double_int_one, double_int_zero);
	break;
      }

    CASE_CONVERT:
      {
	bool uns;

	/* First extend mask and value according to the original type.  */
	uns = TYPE_UNSIGNED (rtype);
	*mask = rmask.ext (TYPE_PRECISION (rtype), uns);
	*val = rval.ext (TYPE_PRECISION (rtype), uns);

	/* Then extend mask and value according to the target type.  */
	uns = TYPE_UNSIGNED (type);
	*mask = (*mask).ext (TYPE_PRECISION (type), uns);
	*val = (*val).ext (TYPE_PRECISION (type), uns);
	break;
      }

    default:
      *mask = double_int_minus_one;
      break;
    }
}

/* Apply the operation CODE in type TYPE to the value, mask pairs
   R1VAL, R1MASK and R2VAL, R2MASK representing a values of type R1TYPE
   and R2TYPE and set the value, mask pair *VAL and *MASK to the result.  */

static void
bit_value_binop_1 (enum tree_code code, tree type,
		   double_int *val, double_int *mask,
		   tree r1type, double_int r1val, double_int r1mask,
		   tree r2type, double_int r2val, double_int r2mask)
{
  bool uns = TYPE_UNSIGNED (type);
  /* Assume we'll get a constant result.  Use an initial varying value,
     we fall back to varying in the end if necessary.  */
  *mask = double_int_minus_one;
  switch (code)
    {
    case BIT_AND_EXPR:
      /* The mask is constant where there is a known not
	 set bit, (m1 | m2) & ((v1 | m1) & (v2 | m2)) */
      *mask = (r1mask | r2mask) & (r1val | r1mask) & (r2val | r2mask);
      *val = r1val & r2val;
      break;

    case BIT_IOR_EXPR:
      /* The mask is constant where there is a known
	 set bit, (m1 | m2) & ~((v1 & ~m1) | (v2 & ~m2)).  */
      *mask = (r1mask | r2mask)
	      .and_not (r1val.and_not (r1mask) | r2val.and_not (r2mask));
      *val = r1val | r2val;
      break;

    case BIT_XOR_EXPR:
      /* m1 | m2  */
      *mask = r1mask | r2mask;
      *val = r1val ^ r2val;
      break;

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (r2mask.is_zero ())
	{
	  HOST_WIDE_INT shift = r2val.low;
	  if (code == RROTATE_EXPR)
	    shift = -shift;
	  *mask = r1mask.lrotate (shift, TYPE_PRECISION (type));
	  *val = r1val.lrotate (shift, TYPE_PRECISION (type));
	}
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      /* ???  We can handle partially known shift counts if we know
	 its sign.  That way we can tell that (x << (y | 8)) & 255
	 is zero.  */
      if (r2mask.is_zero ())
	{
	  HOST_WIDE_INT shift = r2val.low;
	  if (code == RSHIFT_EXPR)
	    shift = -shift;
	  /* We need to know if we are doing a left or a right shift
	     to properly shift in zeros for left shift and unsigned
	     right shifts and the sign bit for signed right shifts.
	     For signed right shifts we shift in varying in case
	     the sign bit was varying.  */
	  if (shift > 0)
	    {
	      *mask = r1mask.llshift (shift, TYPE_PRECISION (type));
	      *val = r1val.llshift (shift, TYPE_PRECISION (type));
	    }
	  else if (shift < 0)
	    {
	      shift = -shift;
	      *mask = r1mask.rshift (shift, TYPE_PRECISION (type), !uns);
	      *val = r1val.rshift (shift, TYPE_PRECISION (type), !uns);
	    }
	  else
	    {
	      *mask = r1mask;
	      *val = r1val;
	    }
	}
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      {
	double_int lo, hi;
	/* Do the addition with unknown bits set to zero, to give carry-ins of
	   zero wherever possible.  */
	lo = r1val.and_not (r1mask) + r2val.and_not (r2mask);
	lo = lo.ext (TYPE_PRECISION (type), uns);
	/* Do the addition with unknown bits set to one, to give carry-ins of
	   one wherever possible.  */
	hi = (r1val | r1mask) + (r2val | r2mask);
	hi = hi.ext (TYPE_PRECISION (type), uns);
	/* Each bit in the result is known if (a) the corresponding bits in
	   both inputs are known, and (b) the carry-in to that bit position
	   is known.  We can check condition (b) by seeing if we got the same
	   result with minimised carries as with maximised carries.  */
	*mask = r1mask | r2mask | (lo ^ hi);
	*mask = (*mask).ext (TYPE_PRECISION (type), uns);
	/* It shouldn't matter whether we choose lo or hi here.  */
	*val = lo;
	break;
      }

    case MINUS_EXPR:
      {
	double_int temv, temm;
	bit_value_unop_1 (NEGATE_EXPR, r2type, &temv, &temm,
			  r2type, r2val, r2mask);
	bit_value_binop_1 (PLUS_EXPR, type, val, mask,
			   r1type, r1val, r1mask,
			   r2type, temv, temm);
	break;
      }

    case MULT_EXPR:
      {
	/* Just track trailing zeros in both operands and transfer
	   them to the other.  */
	int r1tz = (r1val | r1mask).trailing_zeros ();
	int r2tz = (r2val | r2mask).trailing_zeros ();
	if (r1tz + r2tz >= HOST_BITS_PER_DOUBLE_INT)
	  {
	    *mask = double_int_zero;
	    *val = double_int_zero;
	  }
	else if (r1tz + r2tz > 0)
	  {
	    *mask = ~double_int::mask (r1tz + r2tz);
	    *mask = (*mask).ext (TYPE_PRECISION (type), uns);
	    *val = double_int_zero;
	  }
	break;
      }

    case EQ_EXPR:
    case NE_EXPR:
      {
	double_int m = r1mask | r2mask;
	if (r1val.and_not (m) != r2val.and_not (m))
	  {
	    *mask = double_int_zero;
	    *val = ((code == EQ_EXPR) ? double_int_zero : double_int_one);
	  }
	else
	  {
	    /* We know the result of a comparison is always one or zero.  */
	    *mask = double_int_one;
	    *val = double_int_zero;
	  }
	break;
      }

    case GE_EXPR:
    case GT_EXPR:
      {
	double_int tem = r1val;
	r1val = r2val;
	r2val = tem;
	tem = r1mask;
	r1mask = r2mask;
	r2mask = tem;
	code = swap_tree_comparison (code);
      }
      /* Fallthru.  */
    case LT_EXPR:
    case LE_EXPR:
      {
	int minmax, maxmin;
	/* If the most significant bits are not known we know nothing.  */
	if (r1mask.is_negative () || r2mask.is_negative ())
	  break;

	/* For comparisons the signedness is in the comparison operands.  */
	uns = TYPE_UNSIGNED (r1type);

	/* If we know the most significant bits we know the values
	   value ranges by means of treating varying bits as zero
	   or one.  Do a cross comparison of the max/min pairs.  */
	maxmin = (r1val | r1mask).cmp (r2val.and_not (r2mask), uns);
	minmax = r1val.and_not (r1mask).cmp (r2val | r2mask, uns);
	if (maxmin < 0)  /* r1 is less than r2.  */
	  {
	    *mask = double_int_zero;
	    *val = double_int_one;
	  }
	else if (minmax > 0)  /* r1 is not less or equal to r2.  */
	  {
	    *mask = double_int_zero;
	    *val = double_int_zero;
	  }
	else if (maxmin == minmax)  /* r1 and r2 are equal.  */
	  {
	    /* This probably should never happen as we'd have
	       folded the thing during fully constant value folding.  */
	    *mask = double_int_zero;
	    *val = (code == LE_EXPR ? double_int_one :  double_int_zero);
	  }
	else
	  {
	    /* We know the result of a comparison is always one or zero.  */
	    *mask = double_int_one;
	    *val = double_int_zero;
	  }
	break;
      }

    default:;
    }
}

/* Return the propagation value when applying the operation CODE to
   the value RHS yielding type TYPE.  */

static prop_value_t
bit_value_unop (enum tree_code code, tree type, tree rhs)
{
  prop_value_t rval = get_value_for_expr (rhs, true);
  double_int value, mask;
  prop_value_t val;

  if (rval.lattice_val == UNDEFINED)
    return rval;

  gcc_assert ((rval.lattice_val == CONSTANT
	       && TREE_CODE (rval.value) == INTEGER_CST)
	      || rval.mask.is_minus_one ());
  bit_value_unop_1 (code, type, &value, &mask,
		    TREE_TYPE (rhs), value_to_double_int (rval), rval.mask);
  if (!mask.is_minus_one ())
    {
      val.lattice_val = CONSTANT;
      val.mask = mask;
      /* ???  Delay building trees here.  */
      val.value = double_int_to_tree (type, value);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = double_int_minus_one;
    }
  return val;
}

/* Return the propagation value when applying the operation CODE to
   the values RHS1 and RHS2 yielding type TYPE.  */

static prop_value_t
bit_value_binop (enum tree_code code, tree type, tree rhs1, tree rhs2)
{
  prop_value_t r1val = get_value_for_expr (rhs1, true);
  prop_value_t r2val = get_value_for_expr (rhs2, true);
  double_int value, mask;
  prop_value_t val;

  if (r1val.lattice_val == UNDEFINED
      || r2val.lattice_val == UNDEFINED)
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = double_int_minus_one;
      return val;
    }

  gcc_assert ((r1val.lattice_val == CONSTANT
	       && TREE_CODE (r1val.value) == INTEGER_CST)
	      || r1val.mask.is_minus_one ());
  gcc_assert ((r2val.lattice_val == CONSTANT
	       && TREE_CODE (r2val.value) == INTEGER_CST)
	      || r2val.mask.is_minus_one ());
  bit_value_binop_1 (code, type, &value, &mask,
		     TREE_TYPE (rhs1), value_to_double_int (r1val), r1val.mask,
		     TREE_TYPE (rhs2), value_to_double_int (r2val), r2val.mask);
  if (!mask.is_minus_one ())
    {
      val.lattice_val = CONSTANT;
      val.mask = mask;
      /* ???  Delay building trees here.  */
      val.value = double_int_to_tree (type, value);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = double_int_minus_one;
    }
  return val;
}

/* Return the propagation value when applying __builtin_assume_aligned to
   its arguments.  */

static prop_value_t
bit_value_assume_aligned (gimple stmt)
{
  tree ptr = gimple_call_arg (stmt, 0), align, misalign = NULL_TREE;
  tree type = TREE_TYPE (ptr);
  unsigned HOST_WIDE_INT aligni, misaligni = 0;
  prop_value_t ptrval = get_value_for_expr (ptr, true);
  prop_value_t alignval;
  double_int value, mask;
  prop_value_t val;
  if (ptrval.lattice_val == UNDEFINED)
    return ptrval;
  gcc_assert ((ptrval.lattice_val == CONSTANT
	       && TREE_CODE (ptrval.value) == INTEGER_CST)
	      || ptrval.mask.is_minus_one ());
  align = gimple_call_arg (stmt, 1);
  if (!host_integerp (align, 1))
    return ptrval;
  aligni = tree_low_cst (align, 1);
  if (aligni <= 1
      || (aligni & (aligni - 1)) != 0)
    return ptrval;
  if (gimple_call_num_args (stmt) > 2)
    {
      misalign = gimple_call_arg (stmt, 2);
      if (!host_integerp (misalign, 1))
	return ptrval;
      misaligni = tree_low_cst (misalign, 1);
      if (misaligni >= aligni)
	return ptrval;
    }
  align = build_int_cst_type (type, -aligni);
  alignval = get_value_for_expr (align, true);
  bit_value_binop_1 (BIT_AND_EXPR, type, &value, &mask,
		     type, value_to_double_int (ptrval), ptrval.mask,
		     type, value_to_double_int (alignval), alignval.mask);
  if (!mask.is_minus_one ())
    {
      val.lattice_val = CONSTANT;
      val.mask = mask;
      gcc_assert ((mask.low & (aligni - 1)) == 0);
      gcc_assert ((value.low & (aligni - 1)) == 0);
      value.low |= misaligni;
      /* ???  Delay building trees here.  */
      val.value = double_int_to_tree (type, value);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = double_int_minus_one;
    }
  return val;
}

/* Evaluate statement STMT.
   Valid only for assignments, calls, conditionals, and switches. */

static prop_value_t
evaluate_stmt (gimple stmt)
{
  prop_value_t val;
  tree simplified = NULL_TREE;
  ccp_lattice_t likelyvalue = likely_value (stmt);
  bool is_constant = false;
  unsigned int align;

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

  /* If the statement is likely to have a CONSTANT result, then try
     to fold the statement to determine the constant value.  */
  /* FIXME.  This is the only place that we call ccp_fold.
     Since likely_value never returns CONSTANT for calls, we will
     not attempt to fold them, including builtins that may profit.  */
  if (likelyvalue == CONSTANT)
    {
      fold_defer_overflow_warnings ();
      simplified = ccp_fold (stmt);
      is_constant = simplified && is_gimple_min_invariant (simplified);
      fold_undefer_overflow_warnings (is_constant, stmt, 0);
      if (is_constant)
	{
	  /* The statement produced a constant value.  */
	  val.lattice_val = CONSTANT;
	  val.value = simplified;
	  val.mask = double_int_zero;
	}
    }
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
      is_constant = simplified && is_gimple_min_invariant (simplified);
      if (is_constant)
	{
	  /* The statement produced a constant value.  */
	  val.lattice_val = CONSTANT;
	  val.value = simplified;
	  val.mask = double_int_zero;
	}
    }

  /* Resort to simplification for bitwise tracking.  */
  if (flag_tree_bit_ccp
      && (likelyvalue == CONSTANT || is_gimple_call (stmt))
      && !is_constant)
    {
      enum gimple_code code = gimple_code (stmt);
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = double_int_minus_one;
      if (code == GIMPLE_ASSIGN)
	{
	  enum tree_code subcode = gimple_assign_rhs_code (stmt);
	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  switch (get_gimple_rhs_class (subcode))
	    {
	    case GIMPLE_SINGLE_RHS:
	      if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		  || POINTER_TYPE_P (TREE_TYPE (rhs1)))
		val = get_value_for_expr (rhs1, true);
	      break;

	    case GIMPLE_UNARY_RHS:
	      if ((INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		   || POINTER_TYPE_P (TREE_TYPE (rhs1)))
		  && (INTEGRAL_TYPE_P (gimple_expr_type (stmt))
		      || POINTER_TYPE_P (gimple_expr_type (stmt))))
		val = bit_value_unop (subcode, gimple_expr_type (stmt), rhs1);
	      break;

	    case GIMPLE_BINARY_RHS:
	      if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		  || POINTER_TYPE_P (TREE_TYPE (rhs1)))
		{
		  tree lhs = gimple_assign_lhs (stmt);
		  tree rhs2 = gimple_assign_rhs2 (stmt);
		  val = bit_value_binop (subcode,
					 TREE_TYPE (lhs), rhs1, rhs2);
		}
	      break;

	    default:;
	    }
	}
      else if (code == GIMPLE_COND)
	{
	  enum tree_code code = gimple_cond_code (stmt);
	  tree rhs1 = gimple_cond_lhs (stmt);
	  tree rhs2 = gimple_cond_rhs (stmt);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	      || POINTER_TYPE_P (TREE_TYPE (rhs1)))
	    val = bit_value_binop (code, TREE_TYPE (rhs1), rhs1, rhs2);
	}
      else if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    case BUILT_IN_MALLOC:
	    case BUILT_IN_REALLOC:
	    case BUILT_IN_CALLOC:
	    case BUILT_IN_STRDUP:
	    case BUILT_IN_STRNDUP:
	      val.lattice_val = CONSTANT;
	      val.value = build_int_cst (TREE_TYPE (gimple_get_lhs (stmt)), 0);
	      val.mask = double_int::from_shwi
		  	   (~(((HOST_WIDE_INT) MALLOC_ABI_ALIGNMENT)
			      / BITS_PER_UNIT - 1));
	      break;

	    case BUILT_IN_ALLOCA:
	    case BUILT_IN_ALLOCA_WITH_ALIGN:
	      align = (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA_WITH_ALIGN
		       ? TREE_INT_CST_LOW (gimple_call_arg (stmt, 1))
		       : BIGGEST_ALIGNMENT);
	      val.lattice_val = CONSTANT;
	      val.value = build_int_cst (TREE_TYPE (gimple_get_lhs (stmt)), 0);
	      val.mask = double_int::from_shwi (~(((HOST_WIDE_INT) align)
						  / BITS_PER_UNIT - 1));
	      break;

	    /* These builtins return their first argument, unmodified.  */
	    case BUILT_IN_MEMCPY:
	    case BUILT_IN_MEMMOVE:
	    case BUILT_IN_MEMSET:
	    case BUILT_IN_STRCPY:
	    case BUILT_IN_STRNCPY:
	    case BUILT_IN_MEMCPY_CHK:
	    case BUILT_IN_MEMMOVE_CHK:
	    case BUILT_IN_MEMSET_CHK:
	    case BUILT_IN_STRCPY_CHK:
	    case BUILT_IN_STRNCPY_CHK:
	      val = get_value_for_expr (gimple_call_arg (stmt, 0), true);
	      break;

	    case BUILT_IN_ASSUME_ALIGNED:
	      val = bit_value_assume_aligned (stmt);
	      break;

	    default:;
	    }
	}
      is_constant = (val.lattice_val == CONSTANT);
    }

  if (!is_constant)
    {
      /* The statement produced a nonconstant value.  If the statement
	 had UNDEFINED operands, then the result of the statement
	 should be UNDEFINED.  Otherwise, the statement is VARYING.  */
      if (likelyvalue == UNDEFINED)
	{
	  val.lattice_val = likelyvalue;
	  val.mask = double_int_zero;
	}
      else
	{
	  val.lattice_val = VARYING;
	  val.mask = double_int_minus_one;
	}

      val.value = NULL_TREE;
    }

  return val;
}

typedef hash_table <pointer_hash <gimple_statement_d> > gimple_htab;

/* Given a BUILT_IN_STACK_SAVE value SAVED_VAL, insert a clobber of VAR before
   each matching BUILT_IN_STACK_RESTORE.  Mark visited phis in VISITED.  */

static void
insert_clobber_before_stack_restore (tree saved_val, tree var,
				     gimple_htab *visited)
{
  gimple stmt, clobber_stmt;
  tree clobber;
  imm_use_iterator iter;
  gimple_stmt_iterator i;
  gimple *slot;

  FOR_EACH_IMM_USE_STMT (stmt, iter, saved_val)
    if (gimple_call_builtin_p (stmt, BUILT_IN_STACK_RESTORE))
      {
	clobber = build_constructor (TREE_TYPE (var),
				     NULL);
	TREE_THIS_VOLATILE (clobber) = 1;
	clobber_stmt = gimple_build_assign (var, clobber);

	i = gsi_for_stmt (stmt);
	gsi_insert_before (&i, clobber_stmt, GSI_SAME_STMT);
      }
    else if (gimple_code (stmt) == GIMPLE_PHI)
      {
	if (!visited->is_created ())
	  visited->create (10);

	slot = visited->find_slot (stmt, INSERT);
	if (*slot != NULL)
	  continue;

	*slot = stmt;
	insert_clobber_before_stack_restore (gimple_phi_result (stmt), var,
					     visited);
      }
    else
      gcc_assert (is_gimple_debug (stmt));
}

/* Advance the iterator to the previous non-debug gimple statement in the same
   or dominating basic block.  */

static inline void
gsi_prev_dom_bb_nondebug (gimple_stmt_iterator *i)
{
  basic_block dom;

  gsi_prev_nondebug (i);
  while (gsi_end_p (*i))
    {
      dom = get_immediate_dominator (CDI_DOMINATORS, i->bb);
      if (dom == NULL || dom == ENTRY_BLOCK_PTR)
	return;

      *i = gsi_last_bb (dom);
    }
}

/* Find a BUILT_IN_STACK_SAVE dominating gsi_stmt (I), and insert
   a clobber of VAR before each matching BUILT_IN_STACK_RESTORE.

   It is possible that BUILT_IN_STACK_SAVE cannot be find in a dominator when a
   previous pass (such as DOM) duplicated it along multiple paths to a BB.  In
   that case the function gives up without inserting the clobbers.  */

static void
insert_clobbers_for_var (gimple_stmt_iterator i, tree var)
{
  gimple stmt;
  tree saved_val;
  gimple_htab visited;

  for (; !gsi_end_p (i); gsi_prev_dom_bb_nondebug (&i))
    {
      stmt = gsi_stmt (i);

      if (!gimple_call_builtin_p (stmt, BUILT_IN_STACK_SAVE))
	continue;

      saved_val = gimple_call_lhs (stmt);
      if (saved_val == NULL_TREE)
	continue;

      insert_clobber_before_stack_restore (saved_val, var, &visited);
      break;
    }

  if (visited.is_created ())
    visited.dispose ();
}

/* Detects a __builtin_alloca_with_align with constant size argument.  Declares
   fixed-size array and returns the address, if found, otherwise returns
   NULL_TREE.  */

static tree
fold_builtin_alloca_with_align (gimple stmt)
{
  unsigned HOST_WIDE_INT size, threshold, n_elem;
  tree lhs, arg, block, var, elem_type, array_type;

  /* Get lhs.  */
  lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return NULL_TREE;

  /* Detect constant argument.  */
  arg = get_constant_value (gimple_call_arg (stmt, 0));
  if (arg == NULL_TREE
      || TREE_CODE (arg) != INTEGER_CST
      || !host_integerp (arg, 1))
    return NULL_TREE;

  size = TREE_INT_CST_LOW (arg);

  /* Heuristic: don't fold large allocas.  */
  threshold = (unsigned HOST_WIDE_INT)PARAM_VALUE (PARAM_LARGE_STACK_FRAME);
  /* In case the alloca is located at function entry, it has the same lifetime
     as a declared array, so we allow a larger size.  */
  block = gimple_block (stmt);
  if (!(cfun->after_inlining
        && TREE_CODE (BLOCK_SUPERCONTEXT (block)) == FUNCTION_DECL))
    threshold /= 10;
  if (size > threshold)
    return NULL_TREE;

  /* Declare array.  */
  elem_type = build_nonstandard_integer_type (BITS_PER_UNIT, 1);
  n_elem = size * 8 / BITS_PER_UNIT;
  array_type = build_array_type_nelts (elem_type, n_elem);
  var = create_tmp_var (array_type, NULL);
  DECL_ALIGN (var) = TREE_INT_CST_LOW (gimple_call_arg (stmt, 1));
  {
    struct ptr_info_def *pi = SSA_NAME_PTR_INFO (lhs);
    if (pi != NULL && !pi->pt.anything)
      {
	bool singleton_p;
	unsigned uid;
	singleton_p = pt_solution_singleton_p (&pi->pt, &uid);
	gcc_assert (singleton_p);
	SET_DECL_PT_UID (var, uid);
      }
  }

  /* Fold alloca to the address of the array.  */
  return fold_convert (TREE_TYPE (lhs), build_fold_addr_expr (var));
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
	    || !val.mask.is_zero ())
	  return false;

	if (dump_file)
	  {
	    fprintf (dump_file, "Folding predicate ");
	    print_gimple_expr (dump_file, stmt, 0, 0);
	    fprintf (dump_file, " to ");
	    print_generic_expr (dump_file, val.value, 0);
	    fprintf (dump_file, "\n");
	  }

	if (integer_zerop (val.value))
	  gimple_cond_make_false (stmt);
	else
	  gimple_cond_make_true (stmt);

	return true;
      }

    case GIMPLE_CALL:
      {
	tree lhs = gimple_call_lhs (stmt);
	int flags = gimple_call_flags (stmt);
	tree val;
	tree argt;
	bool changed = false;
	unsigned i;

	/* If the call was folded into a constant make sure it goes
	   away even if we cannot propagate into all uses because of
	   type issues.  */
	if (lhs
	    && TREE_CODE (lhs) == SSA_NAME
	    && (val = get_constant_value (lhs))
	    /* Don't optimize away calls that have side-effects.  */
	    && (flags & (ECF_CONST|ECF_PURE)) != 0
	    && (flags & ECF_LOOPING_CONST_OR_PURE) == 0)
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

	/* Internal calls provide no argument types, so the extra laxity
	   for normal calls does not apply.  */
	if (gimple_call_internal_p (stmt))
	  return false;

        /* The heuristic of fold_builtin_alloca_with_align differs before and
	   after inlining, so we don't require the arg to be changed into a
	   constant for folding, but just to be constant.  */
        if (gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN))
          {
            tree new_rhs = fold_builtin_alloca_with_align (stmt);
            if (new_rhs)
	      {
		bool res = update_call_from_tree (gsi, new_rhs);
		tree var = TREE_OPERAND (TREE_OPERAND (new_rhs, 0),0);
		gcc_assert (res);
		insert_clobbers_for_var (*gsi, var);
		return true;
	      }
          }

	/* Propagate into the call arguments.  Compared to replace_uses_in
	   this can use the argument slot types for type verification
	   instead of the current argument type.  We also can safely
	   drop qualifiers here as we are dealing with constants anyway.  */
	argt = TYPE_ARG_TYPES (gimple_call_fntype (stmt));
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
  if (val.lattice_val != CONSTANT
      || !val.mask.is_zero ())
    return SSA_PROP_VARYING;

  /* Find which edge out of the conditional block will be taken and add it
     to the worklist.  If no single edge can be determined statically,
     return SSA_PROP_VARYING to feed all the outgoing edges to the
     propagation engine.  */
  *taken_edge_p = find_taken_edge (block, val.value);
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
      prop_value_t v = { VARYING, NULL_TREE, { -1, (HOST_WIDE_INT) -1 } };
      set_lattice_value (def, v);
    }

  return SSA_PROP_VARYING;
}


/* Main entry point for SSA Conditional Constant Propagation.  */

static unsigned int
do_ssa_ccp (void)
{
  unsigned int todo = 0;
  calculate_dominance_info (CDI_DOMINATORS);
  ccp_initialize ();
  ssa_propagate (ccp_visit_stmt, ccp_visit_phi_node);
  if (ccp_finalize ())
    todo = (TODO_cleanup_cfg | TODO_update_ssa);
  free_dominance_info (CDI_DOMINATORS);
  return todo;
}


static bool
gate_ccp (void)
{
  return flag_tree_ccp != 0;
}


namespace {

const pass_data pass_data_ccp =
{
  GIMPLE_PASS, /* type */
  "ccp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_CCP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_ssa | TODO_update_address_taken
    | TODO_verify_stmts ), /* todo_flags_finish */
};

class pass_ccp : public gimple_opt_pass
{
public:
  pass_ccp(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_ccp, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_ccp (ctxt_); }
  bool gate () { return gate_ccp (); }
  unsigned int execute () { return do_ssa_ccp (); }

}; // class pass_ccp

} // anon namespace

gimple_opt_pass *
make_pass_ccp (gcc::context *ctxt)
{
  return new pass_ccp (ctxt);
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
	  || DECL_FUNCTION_CODE (callee) == BUILT_IN_ALLOCA
	  || DECL_FUNCTION_CODE (callee) == BUILT_IN_ALLOCA_WITH_ALIGN)
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
	  || !builtin_decl_explicit_p (BUILT_IN_NEXT_ARG))
	return NULL_TREE;

      if (gimple_call_num_args (call) != 2)
	return NULL_TREE;

      lhs = gimple_call_arg (call, 0);
      if (!POINTER_TYPE_P (TREE_TYPE (lhs))
	  || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (lhs)))
	     != TYPE_MAIN_VARIANT (cfun_va_list))
	return NULL_TREE;

      lhs = build_fold_indirect_ref_loc (loc, lhs);
      rhs = build_call_expr_loc (loc, builtin_decl_explicit (BUILT_IN_NEXT_ARG),
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

/* Attemp to make the block of __builtin_unreachable I unreachable by changing
   the incoming jumps.  Return true if at least one jump was changed.  */

static bool
optimize_unreachable (gimple_stmt_iterator i)
{
  basic_block bb = gsi_bb (i);
  gimple_stmt_iterator gsi;
  gimple stmt;
  edge_iterator ei;
  edge e;
  bool ret;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);

      if (is_gimple_debug (stmt))
       continue;

      if (gimple_code (stmt) == GIMPLE_LABEL)
	{
	  /* Verify we do not need to preserve the label.  */
	  if (FORCED_LABEL (gimple_label_label (stmt)))
	    return false;

	  continue;
	}

      /* Only handle the case that __builtin_unreachable is the first statement
	 in the block.  We rely on DCE to remove stmts without side-effects
	 before __builtin_unreachable.  */
      if (gsi_stmt (gsi) != gsi_stmt (i))
        return false;
    }

  ret = false;
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      gsi = gsi_last_bb (e->src);
      if (gsi_end_p (gsi))
	continue;

      stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  if (e->flags & EDGE_TRUE_VALUE)
	    gimple_cond_make_false (stmt);
	  else if (e->flags & EDGE_FALSE_VALUE)
	    gimple_cond_make_true (stmt);
	  else
	    gcc_unreachable ();
	  update_stmt (stmt);
	}
      else
	{
	  /* Todo: handle other cases, f.i. switch statement.  */
	  continue;
	}

      ret = true;
    }

  return ret;
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
	      /* Remove all *ssaname_N ={v} {CLOBBER}; stmts,
		 after the last GIMPLE DSE they aren't needed and might
		 unnecessarily keep the SSA_NAMEs live.  */
	      if (gimple_clobber_p (stmt))
		{
		  tree lhs = gimple_assign_lhs (stmt);
		  if (TREE_CODE (lhs) == MEM_REF
		      && TREE_CODE (TREE_OPERAND (lhs, 0)) == SSA_NAME)
		    {
		      unlink_stmt_vdef (stmt);
		      gsi_remove (&i, true);
		      release_defs (stmt);
		      continue;
		    }
		}
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

	      case BUILT_IN_ASSUME_ALIGNED:
		/* Remove __builtin_assume_aligned.  */
		result = gimple_call_arg (stmt, 0);
		break;

	      case BUILT_IN_STACK_RESTORE:
		result = optimize_stack_restore (i);
		if (result)
		  break;
		gsi_next (&i);
		continue;

	      case BUILT_IN_UNREACHABLE:
		if (optimize_unreachable (i))
		  cfg_changed = true;
		break;

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

	  if (result == NULL_TREE)
	    break;

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


namespace {

const pass_data pass_data_fold_builtins =
{
  GIMPLE_PASS, /* type */
  "fab", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_ssa | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_fold_builtins : public gimple_opt_pass
{
public:
  pass_fold_builtins(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_fold_builtins, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_fold_builtins (ctxt_); }
  unsigned int execute () { return execute_fold_all_builtins (); }

}; // class pass_fold_builtins

} // anon namespace

gimple_opt_pass *
make_pass_fold_builtins (gcc::context *ctxt)
{
  return new pass_fold_builtins (ctxt);
}
