/* Conditional constant propagation pass for the GNU compiler.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
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
   propagation engine (tree-ssa-propagate.cc).  Constant assignments of
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

   This algorithm uses wide-ints at the max precision of the target.
   This means that, with one uninteresting exception, variables with
   UNSIGNED types never go to VARYING because the bits above the
   precision of the type of the variable are always zero.  The
   uninteresting case is a variable of UNSIGNED type that has the
   maximum precision of the target.  Such variables can go to VARYING,
   but this causes no loss of infomation since these variables will
   never be extended.

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
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-ssa-propagate.h"
#include "dbgcnt.h"
#include "builtins.h"
#include "cfgloop.h"
#include "stor-layout.h"
#include "optabs-query.h"
#include "tree-ssa-ccp.h"
#include "tree-dfa.h"
#include "diagnostic-core.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-vector-builder.h"
#include "cgraph.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "ipa-utils.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "internal-fn.h"

/* Possible lattice values.  */
typedef enum
{
  UNINITIALIZED,
  UNDEFINED,
  CONSTANT,
  VARYING
} ccp_lattice_t;

class ccp_prop_value_t {
public:
    /* Lattice value.  */
    ccp_lattice_t lattice_val;

    /* Propagated value.  */
    tree value;

    /* Mask that applies to the propagated value during CCP.  For X
       with a CONSTANT lattice value X & ~mask == value & ~mask.  The
       zero bits in the mask cover constant values.  The ones mean no
       information.  */
    widest_int mask;
};

class ccp_propagate : public ssa_propagation_engine
{
 public:
  enum ssa_prop_result visit_stmt (gimple *, edge *, tree *) final override;
  enum ssa_prop_result visit_phi (gphi *) final override;
};

/* Array of propagated constant values.  After propagation,
   CONST_VAL[I].VALUE holds the constant value for SSA_NAME(I).  If
   the constant is held in an SSA name representing a memory store
   (i.e., a VDEF), CONST_VAL[I].MEM_REF will contain the actual
   memory reference used to store (i.e., the LHS of the assignment
   doing the store).  */
static ccp_prop_value_t *const_val;
static unsigned n_const_val;

static void canonicalize_value (ccp_prop_value_t *);
static void ccp_lattice_meet (ccp_prop_value_t *, ccp_prop_value_t *);

/* Dump constant propagation value VAL to file OUTF prefixed by PREFIX.  */

static void
dump_lattice_value (FILE *outf, const char *prefix, ccp_prop_value_t val)
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
	  || val.mask == 0)
	{
	  fprintf (outf, "%sCONSTANT ", prefix);
	  print_generic_expr (outf, val.value, dump_flags);
	}
      else
	{
	  widest_int cval = wi::bit_and_not (wi::to_widest (val.value),
					     val.mask);
	  fprintf (outf, "%sCONSTANT ", prefix);
	  print_hex (cval, outf);
	  fprintf (outf, " (");
	  print_hex (val.mask, outf);
	  fprintf (outf, ")");
	}
      break;
    default:
      gcc_unreachable ();
    }
}


/* Print lattice value VAL to stderr.  */

void debug_lattice_value (ccp_prop_value_t val);

DEBUG_FUNCTION void
debug_lattice_value (ccp_prop_value_t val)
{
  dump_lattice_value (stderr, "", val);
  fprintf (stderr, "\n");
}

/* Extend NONZERO_BITS to a full mask, based on sgn.  */

static widest_int
extend_mask (const wide_int &nonzero_bits, signop sgn)
{
  return widest_int::from (nonzero_bits, sgn);
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

static ccp_prop_value_t
get_default_value (tree var)
{
  ccp_prop_value_t val = { UNINITIALIZED, NULL_TREE, 0 };
  gimple *stmt;

  stmt = SSA_NAME_DEF_STMT (var);

  if (gimple_nop_p (stmt))
    {
      /* Variables defined by an empty statement are those used
	 before being initialized.  If VAR is a local variable, we
	 can assume initially that it is UNDEFINED, otherwise we must
	 consider it VARYING.  */
      if (!virtual_operand_p (var)
	  && SSA_NAME_VAR (var)
	  && VAR_P (SSA_NAME_VAR (var)))
	val.lattice_val = UNDEFINED;
      else
	{
	  val.lattice_val = VARYING;
	  val.mask = -1;
	  if (flag_tree_bit_ccp)
	    {
	      wide_int nonzero_bits = get_nonzero_bits (var);
	      tree value;
	      widest_int mask;

	      if (SSA_NAME_VAR (var)
		  && TREE_CODE (SSA_NAME_VAR (var)) == PARM_DECL
		  && ipcp_get_parm_bits (SSA_NAME_VAR (var), &value, &mask))
		{
		  val.lattice_val = CONSTANT;
		  val.value = value;
		  widest_int ipa_value = wi::to_widest (value);
		  /* Unknown bits from IPA CP must be equal to zero.  */
		  gcc_assert (wi::bit_and (ipa_value, mask) == 0);
		  val.mask = mask;
		  if (nonzero_bits != -1)
		    val.mask &= extend_mask (nonzero_bits,
					     TYPE_SIGN (TREE_TYPE (var)));
		}
	      else if (nonzero_bits != -1)
		{
		  val.lattice_val = CONSTANT;
		  val.value = build_zero_cst (TREE_TYPE (var));
		  val.mask = extend_mask (nonzero_bits,
					  TYPE_SIGN (TREE_TYPE (var)));
		}
	    }
	}
    }
  else if (is_gimple_assign (stmt))
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
	{
	  /* Any other variable defined by an assignment is considered
	     UNDEFINED.  */
	  val.lattice_val = UNDEFINED;
	}
    }
  else if ((is_gimple_call (stmt)
	    && gimple_call_lhs (stmt) != NULL_TREE)
	   || gimple_code (stmt) == GIMPLE_PHI)
    {
      /* A variable defined by a call or a PHI node is considered
	 UNDEFINED.  */
      val.lattice_val = UNDEFINED;
    }
  else
    {
      /* Otherwise, VAR will never take on a constant value.  */
      val.lattice_val = VARYING;
      val.mask = -1;
    }

  return val;
}


/* Get the constant value associated with variable VAR.  */

static inline ccp_prop_value_t *
get_value (tree var)
{
  ccp_prop_value_t *val;

  if (const_val == NULL
      || SSA_NAME_VERSION (var) >= n_const_val)
    return NULL;

  val = &const_val[SSA_NAME_VERSION (var)];
  if (val->lattice_val == UNINITIALIZED)
    *val = get_default_value (var);

  canonicalize_value (val);

  return val;
}

/* Return the constant tree value associated with VAR.  */

static inline tree
get_constant_value (tree var)
{
  ccp_prop_value_t *val;
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
	  || val->mask == 0))
    return val->value;
  return NULL_TREE;
}

/* Sets the value associated with VAR to VARYING.  */

static inline void
set_value_varying (tree var)
{
  ccp_prop_value_t *val = &const_val[SSA_NAME_VERSION (var)];

  val->lattice_val = VARYING;
  val->value = NULL_TREE;
  val->mask = -1;
}

/* For integer constants, make sure to drop TREE_OVERFLOW.  */

static void
canonicalize_value (ccp_prop_value_t *val)
{
  if (val->lattice_val != CONSTANT)
    return;

  if (TREE_OVERFLOW_P (val->value))
    val->value = drop_tree_overflow (val->value);
}

/* Return whether the lattice transition is valid.  */

static bool
valid_lattice_transition (ccp_prop_value_t old_val, ccp_prop_value_t new_val)
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

  /* Allow arbitrary copy changes as we might look through PHI <a_1, ...>
     when only a single copy edge is executable.  */
  if (TREE_CODE (old_val.value) == SSA_NAME
      && TREE_CODE (new_val.value) == SSA_NAME)
    return true;

  /* Allow transitioning from a constant to a copy.  */
  if (is_gimple_min_invariant (old_val.value)
      && TREE_CODE (new_val.value) == SSA_NAME)
    return true;

  /* Allow transitioning from PHI <&x, not executable> == &x
     to PHI <&x, &y> == common alignment.  */
  if (TREE_CODE (old_val.value) != INTEGER_CST
      && TREE_CODE (new_val.value) == INTEGER_CST)
    return true;

  /* Bit-lattices have to agree in the still valid bits.  */
  if (TREE_CODE (old_val.value) == INTEGER_CST
      && TREE_CODE (new_val.value) == INTEGER_CST)
    return (wi::bit_and_not (wi::to_widest (old_val.value), new_val.mask)
	    == wi::bit_and_not (wi::to_widest (new_val.value), new_val.mask));

  /* Otherwise constant values have to agree.  */
  if (operand_equal_p (old_val.value, new_val.value, 0))
    return true;

  /* At least the kinds and types should agree now.  */
  if (TREE_CODE (old_val.value) != TREE_CODE (new_val.value)
      || !types_compatible_p (TREE_TYPE (old_val.value),
			      TREE_TYPE (new_val.value)))
    return false;

  /* For floats and !HONOR_NANS allow transitions from (partial) NaN
     to non-NaN.  */
  tree type = TREE_TYPE (new_val.value);
  if (SCALAR_FLOAT_TYPE_P (type)
      && !HONOR_NANS (type))
    {
      if (REAL_VALUE_ISNAN (TREE_REAL_CST (old_val.value)))
	return true;
    }
  else if (VECTOR_FLOAT_TYPE_P (type)
	   && !HONOR_NANS (type))
    {
      unsigned int count
	= tree_vector_builder::binary_encoded_nelts (old_val.value,
						     new_val.value);
      for (unsigned int i = 0; i < count; ++i)
	if (!REAL_VALUE_ISNAN
	       (TREE_REAL_CST (VECTOR_CST_ENCODED_ELT (old_val.value, i)))
	    && !operand_equal_p (VECTOR_CST_ENCODED_ELT (old_val.value, i),
				 VECTOR_CST_ENCODED_ELT (new_val.value, i), 0))
	  return false;
      return true;
    }
  else if (COMPLEX_FLOAT_TYPE_P (type)
	   && !HONOR_NANS (type))
    {
      if (!REAL_VALUE_ISNAN (TREE_REAL_CST (TREE_REALPART (old_val.value)))
	  && !operand_equal_p (TREE_REALPART (old_val.value),
			       TREE_REALPART (new_val.value), 0))
	return false;
      if (!REAL_VALUE_ISNAN (TREE_REAL_CST (TREE_IMAGPART (old_val.value)))
	  && !operand_equal_p (TREE_IMAGPART (old_val.value),
			       TREE_IMAGPART (new_val.value), 0))
	return false;
      return true;
    }
  return false;
}

/* Set the value for variable VAR to NEW_VAL.  Return true if the new
   value is different from VAR's previous value.  */

static bool
set_lattice_value (tree var, ccp_prop_value_t *new_val)
{
  /* We can deal with old UNINITIALIZED values just fine here.  */
  ccp_prop_value_t *old_val = &const_val[SSA_NAME_VERSION (var)];

  canonicalize_value (new_val);

  /* We have to be careful to not go up the bitwise lattice
     represented by the mask.  Instead of dropping to VARYING
     use the meet operator to retain a conservative value.
     Missed optimizations like PR65851 makes this necessary.
     It also ensures we converge to a stable lattice solution.  */
  if (old_val->lattice_val != UNINITIALIZED
      /* But avoid using meet for constant -> copy transitions.  */
      && !(old_val->lattice_val == CONSTANT
	   && CONSTANT_CLASS_P (old_val->value)
	   && new_val->lattice_val == CONSTANT
	   && TREE_CODE (new_val->value) == SSA_NAME))
    ccp_lattice_meet (new_val, old_val);

  gcc_checking_assert (valid_lattice_transition (*old_val, *new_val));

  /* If *OLD_VAL and NEW_VAL are the same, return false to inform the
     caller that this was a non-transition.  */
  if (old_val->lattice_val != new_val->lattice_val
      || (new_val->lattice_val == CONSTANT
	  && (TREE_CODE (new_val->value) != TREE_CODE (old_val->value)
	      || (TREE_CODE (new_val->value) == INTEGER_CST
		  && (new_val->mask != old_val->mask
		      || (wi::bit_and_not (wi::to_widest (old_val->value),
					   new_val->mask)
			  != wi::bit_and_not (wi::to_widest (new_val->value),
					      new_val->mask))))
	      || (TREE_CODE (new_val->value) != INTEGER_CST
		  && !operand_equal_p (new_val->value, old_val->value, 0)))))
    {
      /* ???  We would like to delay creation of INTEGER_CSTs from
	 partially constants here.  */

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  dump_lattice_value (dump_file, "Lattice value changed to ", *new_val);
	  fprintf (dump_file, ".  Adding SSA edges to worklist.\n");
	}

      *old_val = *new_val;

      gcc_assert (new_val->lattice_val != UNINITIALIZED);
      return true;
    }

  return false;
}

static ccp_prop_value_t get_value_for_expr (tree, bool);
static ccp_prop_value_t bit_value_binop (enum tree_code, tree, tree, tree);
void bit_value_binop (enum tree_code, signop, int, widest_int *, widest_int *,
		      signop, int, const widest_int &, const widest_int &,
		      signop, int, const widest_int &, const widest_int &);

/* Return a widest_int that can be used for bitwise simplifications
   from VAL.  */

static widest_int
value_to_wide_int (ccp_prop_value_t val)
{
  if (val.value
      && TREE_CODE (val.value) == INTEGER_CST)
    return wi::to_widest (val.value);

  return 0;
}

/* Return the value for the address expression EXPR based on alignment
   information.  */

static ccp_prop_value_t
get_value_from_alignment (tree expr)
{
  tree type = TREE_TYPE (expr);
  ccp_prop_value_t val;
  unsigned HOST_WIDE_INT bitpos;
  unsigned int align;

  gcc_assert (TREE_CODE (expr) == ADDR_EXPR);

  get_pointer_alignment_1 (expr, &align, &bitpos);
  val.mask = wi::bit_and_not
    (POINTER_TYPE_P (type) || TYPE_UNSIGNED (type)
     ? wi::mask <widest_int> (TYPE_PRECISION (type), false)
     : -1,
     align / BITS_PER_UNIT - 1);
  val.lattice_val
    = wi::sext (val.mask, TYPE_PRECISION (type)) == -1 ? VARYING : CONSTANT;
  if (val.lattice_val == CONSTANT)
    val.value = build_int_cstu (type, bitpos / BITS_PER_UNIT);
  else
    val.value = NULL_TREE;

  return val;
}

/* Return the value for the tree operand EXPR.  If FOR_BITS_P is true
   return constant bits extracted from alignment information for
   invariant addresses.  */

static ccp_prop_value_t
get_value_for_expr (tree expr, bool for_bits_p)
{
  ccp_prop_value_t val;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      ccp_prop_value_t *val_ = get_value (expr);
      if (val_)
	val = *val_;
      else
	{
	  val.lattice_val = VARYING;
	  val.value = NULL_TREE;
	  val.mask = -1;
	}
      if (for_bits_p
	  && val.lattice_val == CONSTANT)
	{
	  if (TREE_CODE (val.value) == ADDR_EXPR)
	    val = get_value_from_alignment (val.value);
	  else if (TREE_CODE (val.value) != INTEGER_CST)
	    {
	      val.lattice_val = VARYING;
	      val.value = NULL_TREE;
	      val.mask = -1;
	    }
	}
      /* Fall back to a copy value.  */
      if (!for_bits_p
	  && val.lattice_val == VARYING
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (expr))
	{
	  val.lattice_val = CONSTANT;
	  val.value = expr;
	  val.mask = -1;
	}
    }
  else if (is_gimple_min_invariant (expr)
	   && (!for_bits_p || TREE_CODE (expr) == INTEGER_CST))
    {
      val.lattice_val = CONSTANT;
      val.value = expr;
      val.mask = 0;
      canonicalize_value (&val);
    }
  else if (TREE_CODE (expr) == ADDR_EXPR)
    val = get_value_from_alignment (expr);
  else
    {
      val.lattice_val = VARYING;
      val.mask = -1;
      val.value = NULL_TREE;
    }

  if (val.lattice_val == VARYING
      && INTEGRAL_TYPE_P (TREE_TYPE (expr))
      && TYPE_UNSIGNED (TREE_TYPE (expr)))
    val.mask = wi::zext (val.mask, TYPE_PRECISION (TREE_TYPE (expr)));

  return val;
}

/* Return the likely CCP lattice value for STMT.

   If STMT has no operands, then return CONSTANT.

   Else if undefinedness of operands of STMT cause its value to be
   undefined, then return UNDEFINED.

   Else if any operands of STMT are constants, then return CONSTANT.

   Else return VARYING.  */

static ccp_lattice_t
likely_value (gimple *stmt)
{
  bool has_constant_operand, has_undefined_operand, all_undefined_operands;
  bool has_nsa_operand;
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

  /* .DEFERRED_INIT produces undefined.  */
  if (gimple_call_internal_p (stmt, IFN_DEFERRED_INIT))
    return UNDEFINED;

  /* Arrive here for more complex cases.  */
  has_constant_operand = false;
  has_undefined_operand = false;
  all_undefined_operands = true;
  has_nsa_operand = false;
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      ccp_prop_value_t *val = get_value (use);

      if (val && val->lattice_val == UNDEFINED)
	has_undefined_operand = true;
      else
	all_undefined_operands = false;

      if (val && val->lattice_val == CONSTANT)
	has_constant_operand = true;

      if (SSA_NAME_IS_DEFAULT_DEF (use)
	  || !prop_simulate_again_p (SSA_NAME_DEF_STMT (use)))
	has_nsa_operand = true;
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
      else if (TREE_CODE (op) == CONSTRUCTOR)
	{
	  unsigned j;
	  tree val;
	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (op), j, val)
	    if (CONSTANT_CLASS_P (val))
	      {
		has_constant_operand = true;
		break;
	      }
	}
    }

  if (has_constant_operand)
    all_undefined_operands = false;

  if (has_undefined_operand
      && code == GIMPLE_CALL
      && gimple_call_internal_p (stmt))
    switch (gimple_call_internal_fn (stmt))
      {
	/* These 3 builtins use the first argument just as a magic
	   way how to find out a decl uid.  */
      case IFN_GOMP_SIMD_LANE:
      case IFN_GOMP_SIMD_VF:
      case IFN_GOMP_SIMD_LAST_LANE:
	has_undefined_operand = false;
	break;
      default:
	break;
      }

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
	case BIT_XOR_EXPR:
	  /* Not MIN_EXPR, MAX_EXPR.  One VARYING operand may be selected.
	     Not bitwise operators, one VARYING operand may specify the
	     result completely.
	     Not logical operators for the same reason, apart from XOR.
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
     constant.  Also we can combine the stmt with definitions from
     operands whose definitions are not simulated again.  */
  if (has_constant_operand
      || has_nsa_operand
      || gimple_references_memory_p (stmt))
    return CONSTANT;

  return VARYING;
}

/* Returns true if STMT cannot be constant.  */

static bool
surely_varying_stmt_p (gimple *stmt)
{
  /* If the statement has operands that we cannot handle, it cannot be
     constant.  */
  if (gimple_has_volatile_ops (stmt))
    return true;

  /* If it is a call and does not return a value or is not a
     builtin and not an indirect call or a call to function with
     assume_aligned/alloc_align attribute, it is varying.  */
  if (is_gimple_call (stmt))
    {
      tree fndecl, fntype = gimple_call_fntype (stmt);
      if (!gimple_call_lhs (stmt)
	  || ((fndecl = gimple_call_fndecl (stmt)) != NULL_TREE
	      && !fndecl_built_in_p (fndecl)
	      && !lookup_attribute ("assume_aligned",
				    TYPE_ATTRIBUTES (fntype))
	      && !lookup_attribute ("alloc_align",
				    TYPE_ATTRIBUTES (fntype))))
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
  const_val = XCNEWVEC (ccp_prop_value_t, n_const_val);

  /* Initialize simulation flags for PHI nodes and statements.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
	  gimple *stmt = gsi_stmt (i);
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
  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator i;

      for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
        {
          gphi *phi = i.phi ();

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
	  const_val[i].mask = -1;
          const_val[i].value = NULL_TREE;
        }
    }
}


/* We want to provide our own GET_VALUE and FOLD_STMT virtual methods.  */
class ccp_folder : public substitute_and_fold_engine
{
 public:
  tree value_of_expr (tree, gimple *) final override;
  bool fold_stmt (gimple_stmt_iterator *) final override;
};

/* This method just wraps GET_CONSTANT_VALUE for now.  Over time
   naked calls to GET_CONSTANT_VALUE should be eliminated in favor
   of calling member functions.  */

tree
ccp_folder::value_of_expr (tree op, gimple *)
{
  return get_constant_value (op);
}

/* Do final substitution of propagated values, cleanup the flowgraph and
   free allocated storage.  If NONZERO_P, record nonzero bits.

   Return TRUE when something was optimized.  */

static bool
ccp_finalize (bool nonzero_p)
{
  bool something_changed;
  unsigned i;
  tree name;

  do_dbg_cnt ();

  /* Derive alignment and misalignment information from partially
     constant pointers in the lattice or nonzero bits from partially
     constant integers.  */
  FOR_EACH_SSA_NAME (i, name, cfun)
    {
      ccp_prop_value_t *val;
      unsigned int tem, align;

      if (!POINTER_TYPE_P (TREE_TYPE (name))
	  && (!INTEGRAL_TYPE_P (TREE_TYPE (name))
	      /* Don't record nonzero bits before IPA to avoid
		 using too much memory.  */
	      || !nonzero_p))
	continue;

      val = get_value (name);
      if (val->lattice_val != CONSTANT
	  || TREE_CODE (val->value) != INTEGER_CST
	  || val->mask == 0)
	continue;

      if (POINTER_TYPE_P (TREE_TYPE (name)))
	{
	  /* Trailing mask bits specify the alignment, trailing value
	     bits the misalignment.  */
	  tem = val->mask.to_uhwi ();
	  align = least_bit_hwi (tem);
	  if (align > 1)
	    set_ptr_info_alignment (get_ptr_info (name), align,
				    (TREE_INT_CST_LOW (val->value)
				     & (align - 1)));
	}
      else
	{
	  unsigned int precision = TYPE_PRECISION (TREE_TYPE (val->value));
	  wide_int value = wi::to_wide (val->value);
	  wide_int mask = wide_int::from (val->mask, precision, UNSIGNED);
	  value = value & ~mask;
	  set_bitmask (name, value, mask);
	}
    }

  /* Perform substitutions based on the known constant values.  */
  class ccp_folder ccp_folder;
  something_changed = ccp_folder.substitute_and_fold ();

  free (const_val);
  const_val = NULL;
  return something_changed;
}


/* Compute the meet operator between *VAL1 and *VAL2.  Store the result
   in VAL1.

   		any  M UNDEFINED   = any
		any  M VARYING     = VARYING
		Ci   M Cj	   = Ci		if (i == j)
		Ci   M Cj	   = VARYING	if (i != j)
   */

static void
ccp_lattice_meet (ccp_prop_value_t *val1, ccp_prop_value_t *val2)
{
  if (val1->lattice_val == UNDEFINED
      /* For UNDEFINED M SSA we can't always SSA because its definition
         may not dominate the PHI node.  Doing optimistic copy propagation
	 also causes a lot of gcc.dg/uninit-pred*.c FAILs.  */
      && (val2->lattice_val != CONSTANT
	  || TREE_CODE (val2->value) != SSA_NAME))
    {
      /* UNDEFINED M any = any   */
      *val1 = *val2;
    }
  else if (val2->lattice_val == UNDEFINED
	   /* See above.  */
	   && (val1->lattice_val != CONSTANT
	       || TREE_CODE (val1->value) != SSA_NAME))
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
      val1->mask = -1;
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
      val1->mask = (val1->mask | val2->mask
		    | (wi::to_widest (val1->value)
		       ^ wi::to_widest (val2->value)));
      if (wi::sext (val1->mask, TYPE_PRECISION (TREE_TYPE (val1->value))) == -1)
	{
	  val1->lattice_val = VARYING;
	  val1->value = NULL_TREE;
	}
    }
  else if (val1->lattice_val == CONSTANT
	   && val2->lattice_val == CONSTANT
	   && operand_equal_p (val1->value, val2->value, 0))
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
      ccp_prop_value_t tem = *val2;
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
      val1->mask = -1;
      val1->value = NULL_TREE;
    }
}


/* Loop through the PHI_NODE's parameters for BLOCK and compare their
   lattice values to determine PHI_NODE's lattice value.  The value of a
   PHI node is determined calling ccp_lattice_meet with all the arguments
   of the PHI node that are incoming via executable edges.  */

enum ssa_prop_result
ccp_propagate::visit_phi (gphi *phi)
{
  unsigned i;
  ccp_prop_value_t new_val;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_gimple_stmt (dump_file, phi, 0, dump_flags);
    }

  new_val.lattice_val = UNDEFINED;
  new_val.value = NULL_TREE;
  new_val.mask = 0;

  bool first = true;
  bool non_exec_edge = false;
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      /* Compute the meet operator over all the PHI arguments flowing
	 through executable edges.  */
      edge e = gimple_phi_arg_edge (phi, i);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
	      "\tArgument #%d (%d -> %d %sexecutable)\n",
	      i, e->src->index, e->dest->index,
	      (e->flags & EDGE_EXECUTABLE) ? "" : "not ");
	}

      /* If the incoming edge is executable, Compute the meet operator for
	 the existing value of the PHI node and the current PHI argument.  */
      if (e->flags & EDGE_EXECUTABLE)
	{
	  tree arg = gimple_phi_arg (phi, i)->def;
	  ccp_prop_value_t arg_val = get_value_for_expr (arg, false);

	  if (first)
	    {
	      new_val = arg_val;
	      first = false;
	    }
	  else
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
      else
	non_exec_edge = true;
    }

  /* In case there were non-executable edges and the value is a copy
     make sure its definition dominates the PHI node.  */
  if (non_exec_edge
      && new_val.lattice_val == CONSTANT
      && TREE_CODE (new_val.value) == SSA_NAME
      && ! SSA_NAME_IS_DEFAULT_DEF (new_val.value)
      && ! dominated_by_p (CDI_DOMINATORS, gimple_bb (phi),
			   gimple_bb (SSA_NAME_DEF_STMT (new_val.value))))
    {
      new_val.lattice_val = VARYING;
      new_val.value = NULL_TREE;
      new_val.mask = -1;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_lattice_value (dump_file, "\n    PHI node value: ", new_val);
      fprintf (dump_file, "\n\n");
    }

  /* Make the transition to the new value.  */
  if (set_lattice_value (gimple_phi_result (phi), &new_val))
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

/* Return the constant value for OP, but signal to not follow SSA
   edges if the definition may be simulated again.  */

static tree
valueize_op_1 (tree op)
{
  if (TREE_CODE (op) == SSA_NAME)
    {
      /* If the definition may be simulated again we cannot follow
         this SSA edge as the SSA propagator does not necessarily
	 re-visit the use.  */
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
      if (!gimple_nop_p (def_stmt)
	  && prop_simulate_again_p (def_stmt))
	return NULL_TREE;
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
ccp_fold (gimple *stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_SWITCH:
      {
	/* Return the constant switch index.  */
        return valueize_op (gimple_switch_index (as_a <gswitch *> (stmt)));
      }

    case GIMPLE_COND:
    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      return gimple_fold_stmt_to_constant_1 (stmt,
					     valueize_op, valueize_op_1);

    default:
      gcc_unreachable ();
    }
}

/* Determine the minimum and maximum values, *MIN and *MAX respectively,
   represented by the mask pair VAL and MASK with signedness SGN and
   precision PRECISION.  */

static void
value_mask_to_min_max (widest_int *min, widest_int *max,
		       const widest_int &val, const widest_int &mask,
		       signop sgn, int precision)
{
  *min = wi::bit_and_not (val, mask);
  *max = val | mask;
  if (sgn == SIGNED && wi::neg_p (mask))
    {
      widest_int sign_bit = wi::lshift (1, precision - 1);
      *min ^= sign_bit;
      *max ^= sign_bit;
      /* MAX is zero extended, and MIN is sign extended.  */
      *min = wi::ext (*min, precision, sgn);
      *max = wi::ext (*max, precision, sgn);
    }
}

/* Apply the operation CODE in type TYPE to the value, mask pair
   RVAL and RMASK representing a value of type RTYPE and set
   the value, mask pair *VAL and *MASK to the result.  */

void
bit_value_unop (enum tree_code code, signop type_sgn, int type_precision,
		widest_int *val, widest_int *mask,
		signop rtype_sgn, int rtype_precision,
		const widest_int &rval, const widest_int &rmask)
{
  switch (code)
    {
    case BIT_NOT_EXPR:
      *mask = rmask;
      *val = ~rval;
      break;

    case NEGATE_EXPR:
      {
	widest_int temv, temm;
	/* Return ~rval + 1.  */
	bit_value_unop (BIT_NOT_EXPR, type_sgn, type_precision, &temv, &temm,
			type_sgn, type_precision, rval, rmask);
	bit_value_binop (PLUS_EXPR, type_sgn, type_precision, val, mask,
			 type_sgn, type_precision, temv, temm,
			 type_sgn, type_precision, 1, 0);
	break;
      }

    CASE_CONVERT:
      {
	/* First extend mask and value according to the original type.  */
	*mask = wi::ext (rmask, rtype_precision, rtype_sgn);
	*val = wi::ext (rval, rtype_precision, rtype_sgn);

	/* Then extend mask and value according to the target type.  */
	*mask = wi::ext (*mask, type_precision, type_sgn);
	*val = wi::ext (*val, type_precision, type_sgn);
	break;
      }

    case ABS_EXPR:
    case ABSU_EXPR:
      if (wi::sext (rmask, rtype_precision) == -1)
	{
	  *mask = -1;
	  *val = 0;
	}
      else if (wi::neg_p (rmask))
	{
	  /* Result is either rval or -rval.  */
	  widest_int temv, temm;
	  bit_value_unop (NEGATE_EXPR, rtype_sgn, rtype_precision, &temv,
			  &temm, type_sgn, type_precision, rval, rmask);
	  temm |= (rmask | (rval ^ temv));
	  /* Extend the result.  */
	  *mask = wi::ext (temm, type_precision, type_sgn);
	  *val = wi::ext (temv, type_precision, type_sgn);
	}
      else if (wi::neg_p (rval))
	{
	  bit_value_unop (NEGATE_EXPR, type_sgn, type_precision, val, mask,
			  type_sgn, type_precision, rval, rmask);
	}
      else
	{
	  *mask = rmask;
	  *val = rval;
	}
      break;

    default:
      *mask = -1;
      *val = 0;
      break;
    }
}

/* Determine the mask pair *VAL and *MASK from multiplying the
   argument mask pair RVAL, RMASK by the unsigned constant C.  */
static void
bit_value_mult_const (signop sgn, int width,
		      widest_int *val, widest_int *mask,
		      const widest_int &rval, const widest_int &rmask,
		      widest_int c)
{
  widest_int sum_mask = 0;

  /* Ensure rval_lo only contains known bits.  */
  widest_int rval_lo = wi::bit_and_not (rval, rmask);

  if (rval_lo != 0)
    {
      /* General case (some bits of multiplicand are known set).  */
      widest_int sum_val = 0;
      while (c != 0)
	{
	  /* Determine the lowest bit set in the multiplier.  */
	  int bitpos = wi::ctz (c);
	  widest_int term_mask = rmask << bitpos;
	  widest_int term_val = rval_lo << bitpos;

	  /* sum += term.  */
	  widest_int lo = sum_val + term_val;
	  widest_int hi = (sum_val | sum_mask) + (term_val | term_mask);
	  sum_mask |= term_mask | (lo ^ hi);
	  sum_val = lo;

	  /* Clear this bit in the multiplier.  */
	  c ^= wi::lshift (1, bitpos);
	}
      /* Correctly extend the result value.  */
      *val = wi::ext (sum_val, width, sgn);
    }
  else
    {
      /* Special case (no bits of multiplicand are known set).  */
      while (c != 0)
	{
	  /* Determine the lowest bit set in the multiplier.  */
	  int bitpos = wi::ctz (c);
	  widest_int term_mask = rmask << bitpos;

	  /* sum += term.  */
	  widest_int hi = sum_mask + term_mask;
	  sum_mask |= term_mask | hi;

	  /* Clear this bit in the multiplier.  */
	  c ^= wi::lshift (1, bitpos);
	}
      *val = 0;
    }

  /* Correctly extend the result mask.  */
  *mask = wi::ext (sum_mask, width, sgn);
}

/* Fill up to MAX values in the BITS array with values representing
   each of the non-zero bits in the value X.  Returns the number of
   bits in X (capped at the maximum value MAX).  For example, an X
   value 11, places 1, 2 and 8 in BITS and returns the value 3.  */

static unsigned int
get_individual_bits (widest_int *bits, widest_int x, unsigned int max)
{
  unsigned int count = 0;
  while (count < max && x != 0)
    {
      int bitpos = wi::ctz (x);
      bits[count] = wi::lshift (1, bitpos);
      x ^= bits[count];
      count++;
    }
  return count;
}

/* Array of 2^N - 1 values representing the bits flipped between
   consecutive Gray codes.  This is used to efficiently enumerate
   all permutations on N bits using XOR.  */
static const unsigned char gray_code_bit_flips[63] = {
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4,
  0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0
};

/* Apply the operation CODE in type TYPE to the value, mask pairs
   R1VAL, R1MASK and R2VAL, R2MASK representing a values of type R1TYPE
   and R2TYPE and set the value, mask pair *VAL and *MASK to the result.  */

void
bit_value_binop (enum tree_code code, signop sgn, int width,
		 widest_int *val, widest_int *mask,
		 signop r1type_sgn, int r1type_precision,
		 const widest_int &r1val, const widest_int &r1mask,
		 signop r2type_sgn, int r2type_precision ATTRIBUTE_UNUSED,
		 const widest_int &r2val, const widest_int &r2mask)
{
  bool swap_p = false;

  /* Assume we'll get a constant result.  Use an initial non varying
     value, we fall back to varying in the end if necessary.  */
  *mask = -1;
  /* Ensure that VAL is initialized (to any value).  */
  *val = 0;

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
      *mask = wi::bit_and_not (r1mask | r2mask,
			       wi::bit_and_not (r1val, r1mask)
			       | wi::bit_and_not (r2val, r2mask));
      *val = r1val | r2val;
      break;

    case BIT_XOR_EXPR:
      /* m1 | m2  */
      *mask = r1mask | r2mask;
      *val = r1val ^ r2val;
      break;

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (r2mask == 0)
	{
	  widest_int shift = r2val;
	  if (shift == 0)
	    {
	      *mask = r1mask;
	      *val = r1val;
	    }
	  else
	    {
	      if (wi::neg_p (shift, r2type_sgn))
		{
		  shift = -shift;
		  if (code == RROTATE_EXPR)
		    code = LROTATE_EXPR;
		  else
		    code = RROTATE_EXPR;
		}
	      if (code == RROTATE_EXPR)
		{
		  *mask = wi::rrotate (r1mask, shift, width);
		  *val = wi::rrotate (r1val, shift, width);
		}
	      else
		{
		  *mask = wi::lrotate (r1mask, shift, width);
		  *val = wi::lrotate (r1val, shift, width);
		}
	      *mask = wi::ext (*mask, width, sgn);
	      *val = wi::ext (*val, width, sgn);
	    }
	}
      else if (wi::ltu_p (r2val | r2mask, width)
	       && wi::popcount (r2mask) <= 4)
	{
	  widest_int bits[4];
	  widest_int res_val, res_mask;
	  widest_int tmp_val, tmp_mask;
	  widest_int shift = wi::bit_and_not (r2val, r2mask);
	  unsigned int bit_count = get_individual_bits (bits, r2mask, 4);
	  unsigned int count = (1 << bit_count) - 1;

	  /* Initialize result to rotate by smallest value of shift.  */
	  if (code == RROTATE_EXPR)
	    {
	      res_mask = wi::rrotate (r1mask, shift, width);
	      res_val = wi::rrotate (r1val, shift, width);
	    }
	  else
	    {
	      res_mask = wi::lrotate (r1mask, shift, width);
	      res_val = wi::lrotate (r1val, shift, width);
	    }

	  /* Iterate through the remaining values of shift.  */
	  for (unsigned int i=0; i<count; i++)
	    {
	      shift ^= bits[gray_code_bit_flips[i]];
	      if (code == RROTATE_EXPR)
		{
		  tmp_mask = wi::rrotate (r1mask, shift, width);
		  tmp_val = wi::rrotate (r1val, shift, width);
		}
	      else
		{
		  tmp_mask = wi::lrotate (r1mask, shift, width);
		  tmp_val = wi::lrotate (r1val, shift, width);
		}
	      /* Accumulate the result.  */
	      res_mask |= tmp_mask | (res_val ^ tmp_val);
	    }
	  *val = wi::ext (wi::bit_and_not (res_val, res_mask), width, sgn);
	  *mask = wi::ext (res_mask, width, sgn);
	}
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      /* ???  We can handle partially known shift counts if we know
	 its sign.  That way we can tell that (x << (y | 8)) & 255
	 is zero.  */
      if (r2mask == 0)
	{
	  widest_int shift = r2val;
	  if (shift == 0)
	    {
	      *mask = r1mask;
	      *val = r1val;
	    }
	  else
	    {
	      if (wi::neg_p (shift, r2type_sgn))
		break;
	      if (code == RSHIFT_EXPR)
		{
		  *mask = wi::rshift (wi::ext (r1mask, width, sgn), shift, sgn);
		  *val = wi::rshift (wi::ext (r1val, width, sgn), shift, sgn);
		}
	      else
		{
		  *mask = wi::ext (r1mask << shift, width, sgn);
		  *val = wi::ext (r1val << shift, width, sgn);
		}
	    }
	}
      else if (wi::ltu_p (r2val | r2mask, width))
	{
	  if (wi::popcount (r2mask) <= 4)
	    {
	      widest_int bits[4];
	      widest_int arg_val, arg_mask;
	      widest_int res_val, res_mask;
	      widest_int tmp_val, tmp_mask;
	      widest_int shift = wi::bit_and_not (r2val, r2mask);
	      unsigned int bit_count = get_individual_bits (bits, r2mask, 4);
	      unsigned int count = (1 << bit_count) - 1;

	      /* Initialize result to shift by smallest value of shift.  */
	      if (code == RSHIFT_EXPR)
		{
		  arg_mask = wi::ext (r1mask, width, sgn);
		  arg_val = wi::ext (r1val, width, sgn);
		  res_mask = wi::rshift (arg_mask, shift, sgn);
		  res_val = wi::rshift (arg_val, shift, sgn);
		}
	      else
		{
		  arg_mask = r1mask;
		  arg_val = r1val;
		  res_mask = arg_mask << shift;
		  res_val = arg_val << shift;
		}

	      /* Iterate through the remaining values of shift.  */
	      for (unsigned int i=0; i<count; i++)
		{
		  shift ^= bits[gray_code_bit_flips[i]];
		  if (code == RSHIFT_EXPR)
		    {
		      tmp_mask = wi::rshift (arg_mask, shift, sgn);
		      tmp_val = wi::rshift (arg_val, shift, sgn);
		    }
		  else
		    {
		      tmp_mask = arg_mask << shift;
		      tmp_val = arg_val << shift;
		    }
		  /* Accumulate the result.  */
		  res_mask |= tmp_mask | (res_val ^ tmp_val);
		}
	      res_mask = wi::ext (res_mask, width, sgn);
	      res_val = wi::ext (res_val, width, sgn);
	      *val = wi::bit_and_not (res_val, res_mask);
	      *mask = res_mask;
	    }
	  else if ((r1val | r1mask) == 0)
	    {
	      /* Handle shifts of zero to avoid undefined wi::ctz below.  */
	      *mask = 0;
	      *val = 0;
	    }
	  else if (code == LSHIFT_EXPR)
	    {
	      widest_int tmp = wi::mask <widest_int> (width, false);
	      tmp <<= wi::ctz (r1val | r1mask);
	      tmp <<= wi::bit_and_not (r2val, r2mask);
	      *mask = wi::ext (tmp, width, sgn);
	      *val = 0;
	    }
	  else if (!wi::neg_p (r1val | r1mask, sgn))
	    {
	      /* Logical right shift, or zero sign bit.  */
	      widest_int arg = r1val | r1mask;
	      int lzcount = wi::clz (arg);
	      if (lzcount)
		lzcount -= wi::get_precision (arg) - width;
	      widest_int tmp = wi::mask <widest_int> (width, false);
	      tmp = wi::lrshift (tmp, lzcount);
	      tmp = wi::lrshift (tmp, wi::bit_and_not (r2val, r2mask));
	      *mask = wi::ext (tmp, width, sgn);
	      *val = 0;
	    }
	  else if (!wi::neg_p (r1mask))
	    {
	      /* Arithmetic right shift with set sign bit.  */
	      widest_int arg = wi::bit_and_not (r1val, r1mask);
	      int sbcount = wi::clrsb (arg);
	      sbcount -= wi::get_precision (arg) - width;
	      widest_int tmp = wi::mask <widest_int> (width, false);
	      tmp = wi::lrshift (tmp, sbcount);
	      tmp = wi::lrshift (tmp, wi::bit_and_not (r2val, r2mask));
	      *mask = wi::sext (tmp, width);
	      tmp = wi::bit_not (tmp);
	      *val = wi::sext (tmp, width);
	    }
	}
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      {
	/* Do the addition with unknown bits set to zero, to give carry-ins of
	   zero wherever possible.  */
	widest_int lo = (wi::bit_and_not (r1val, r1mask)
			 + wi::bit_and_not (r2val, r2mask));
	lo = wi::ext (lo, width, sgn);
	/* Do the addition with unknown bits set to one, to give carry-ins of
	   one wherever possible.  */
	widest_int hi = (r1val | r1mask) + (r2val | r2mask);
	hi = wi::ext (hi, width, sgn);
	/* Each bit in the result is known if (a) the corresponding bits in
	   both inputs are known, and (b) the carry-in to that bit position
	   is known.  We can check condition (b) by seeing if we got the same
	   result with minimised carries as with maximised carries.  */
	*mask = r1mask | r2mask | (lo ^ hi);
	*mask = wi::ext (*mask, width, sgn);
	/* It shouldn't matter whether we choose lo or hi here.  */
	*val = lo;
	break;
      }

    case MINUS_EXPR:
    case POINTER_DIFF_EXPR:
      {
	/* Subtraction is derived from the addition algorithm above.  */
	widest_int lo = wi::bit_and_not (r1val, r1mask) - (r2val | r2mask);
	lo = wi::ext (lo, width, sgn);
	widest_int hi = (r1val | r1mask) - wi::bit_and_not (r2val, r2mask);
	hi = wi::ext (hi, width, sgn);
	*mask = r1mask | r2mask | (lo ^ hi);
	*mask = wi::ext (*mask, width, sgn);
	*val = lo;
	break;
      }

    case MULT_EXPR:
      if (r2mask == 0
	  && !wi::neg_p (r2val, sgn)
	  && (flag_expensive_optimizations || wi::popcount (r2val) < 8))
	bit_value_mult_const (sgn, width, val, mask, r1val, r1mask, r2val);
      else if (r1mask == 0
	       && !wi::neg_p (r1val, sgn)
	       && (flag_expensive_optimizations || wi::popcount (r1val) < 8))
	bit_value_mult_const (sgn, width, val, mask, r2val, r2mask, r1val);
      else
	{
	  /* Just track trailing zeros in both operands and transfer
	     them to the other.  */
	  int r1tz = wi::ctz (r1val | r1mask);
	  int r2tz = wi::ctz (r2val | r2mask);
	  if (r1tz + r2tz >= width)
	    {
	      *mask = 0;
	      *val = 0;
	    }
	  else if (r1tz + r2tz > 0)
	    {
	      *mask = wi::ext (wi::mask <widest_int> (r1tz + r2tz, true),
			       width, sgn);
	      *val = 0;
	    }
	}
      break;

    case EQ_EXPR:
    case NE_EXPR:
      {
	widest_int m = r1mask | r2mask;
	if (wi::bit_and_not (r1val, m) != wi::bit_and_not (r2val, m))
	  {
	    *mask = 0;
	    *val = ((code == EQ_EXPR) ? 0 : 1);
	  }
	else
	  {
	    /* We know the result of a comparison is always one or zero.  */
	    *mask = 1;
	    *val = 0;
	  }
	break;
      }

    case GE_EXPR:
    case GT_EXPR:
      swap_p = true;
      code = swap_tree_comparison (code);
      /* Fall through.  */
    case LT_EXPR:
    case LE_EXPR:
      {
	widest_int min1, max1, min2, max2;
	int minmax, maxmin;

	const widest_int &o1val = swap_p ? r2val : r1val;
	const widest_int &o1mask = swap_p ? r2mask : r1mask;
	const widest_int &o2val = swap_p ? r1val : r2val;
	const widest_int &o2mask = swap_p ? r1mask : r2mask;

	value_mask_to_min_max (&min1, &max1, o1val, o1mask,
			       r1type_sgn, r1type_precision);
	value_mask_to_min_max (&min2, &max2, o2val, o2mask,
			       r1type_sgn, r1type_precision);

	/* For comparisons the signedness is in the comparison operands.  */
	/* Do a cross comparison of the max/min pairs.  */
	maxmin = wi::cmp (max1, min2, r1type_sgn);
	minmax = wi::cmp (min1, max2, r1type_sgn);
	if (maxmin < (code == LE_EXPR ? 1 : 0))  /* o1 < or <= o2.  */
	  {
	    *mask = 0;
	    *val = 1;
	  }
	else if (minmax > (code == LT_EXPR ? -1 : 0))  /* o1 >= or > o2.  */
	  {
	    *mask = 0;
	    *val = 0;
	  }
	else if (maxmin == minmax)  /* o1 and o2 are equal.  */
	  {
	    /* This probably should never happen as we'd have
	       folded the thing during fully constant value folding.  */
	    *mask = 0;
	    *val = (code == LE_EXPR ? 1 : 0);
	  }
	else
	  {
	    /* We know the result of a comparison is always one or zero.  */
	    *mask = 1;
	    *val = 0;
	  }
	break;
      }

    case MIN_EXPR:
    case MAX_EXPR:
      {
	widest_int min1, max1, min2, max2;

	value_mask_to_min_max (&min1, &max1, r1val, r1mask, sgn, width);
	value_mask_to_min_max (&min2, &max2, r2val, r2mask, sgn, width);

	if (wi::cmp (max1, min2, sgn) <= 0)  /* r1 is less than r2.  */
	  {
	    if (code == MIN_EXPR)
	      {
		*mask = r1mask;
		*val = r1val;
	      }
	    else
	      {
		*mask = r2mask;
		*val = r2val;
	      }
	  }
	else if (wi::cmp (min1, max2, sgn) >= 0)  /* r2 is less than r1.  */
	  {
	    if (code == MIN_EXPR)
	      {
		*mask = r2mask;
		*val = r2val;
	      }
	    else
	      {
		*mask = r1mask;
		*val = r1val;
	      }
	  }
	else
	  {
	    /* The result is either r1 or r2.  */
	    *mask = r1mask | r2mask | (r1val ^ r2val);
	    *val = r1val;
	  }
	break;
      }

    case TRUNC_MOD_EXPR:
      {
	widest_int r1max = r1val | r1mask;
	widest_int r2max = r2val | r2mask;
	if (r2mask == 0)
	  {
	    widest_int shift = wi::exact_log2 (r2val);
	    if (shift != -1)
	      {
		// Handle modulo by a power of 2 as a bitwise and.
		widest_int tem_val, tem_mask;
		bit_value_binop (BIT_AND_EXPR, sgn, width, &tem_val, &tem_mask,
				 r1type_sgn, r1type_precision, r1val, r1mask,
				 r2type_sgn, r2type_precision,
				 r2val - 1, r2mask);
		if (sgn == UNSIGNED
		    || !wi::neg_p (r1max)
		    || (tem_mask == 0 && tem_val == 0))
		  {
		    *val = tem_val;
		    *mask = tem_mask;
		    return;
		  }
	      }
	  }
	if (sgn == UNSIGNED
	    || (!wi::neg_p (r1max) && !wi::neg_p (r2max)))
	  {
	    /* Confirm R2 has some bits set, to avoid division by zero.  */
	    widest_int r2min = wi::bit_and_not (r2val, r2mask);
	    if (r2min != 0)
	      {
		/* R1 % R2 is R1 if R1 is always less than R2.  */
		if (wi::ltu_p (r1max, r2min))
		  {
		    *mask = r1mask;
		    *val = r1val;
		  }
		else
		  {
		    /* R1 % R2 is always less than the maximum of R2.  */
		    unsigned int lzcount = wi::clz (r2max);
		    unsigned int bits = wi::get_precision (r2max) - lzcount;
		    if (r2max == wi::lshift (1, bits))
		      bits--;
		    *mask = wi::mask <widest_int> (bits, false);
		    *val = 0;
		  }
	       }
	    }
	}
      break;

    case EXACT_DIV_EXPR:
    case TRUNC_DIV_EXPR:
      {
	widest_int r1max = r1val | r1mask;
	widest_int r2max = r2val | r2mask;
	if (r2mask == 0
	    && (code == EXACT_DIV_EXPR
		|| sgn == UNSIGNED
		|| !wi::neg_p (r1max)))
	  {
	    widest_int shift = wi::exact_log2 (r2val);
	    if (shift != -1)
	      {
		// Handle division by a power of 2 as an rshift.
		bit_value_binop (RSHIFT_EXPR, sgn, width, val, mask,
				 r1type_sgn, r1type_precision, r1val, r1mask,
				 r2type_sgn, r2type_precision, shift, r2mask);
		return;
	      }
	  }
	if (sgn == UNSIGNED
	    || (!wi::neg_p (r1max) && !wi::neg_p (r2max)))
	  {
	    /* Confirm R2 has some bits set, to avoid division by zero.  */
	    widest_int r2min = wi::bit_and_not (r2val, r2mask);
	    if (r2min != 0)
	      {
		/* R1 / R2 is zero if R1 is always less than R2.  */
		if (wi::ltu_p (r1max, r2min))
		  {
		    *mask = 0;
		    *val = 0;
		  }
		else
		  {
		    widest_int upper
		      = wi::udiv_trunc (wi::zext (r1max, width), r2min);
		    unsigned int lzcount = wi::clz (upper);
		    unsigned int bits = wi::get_precision (upper) - lzcount;
		    *mask = wi::mask <widest_int> (bits, false);
		    *val = 0;
		  }
	       }
	    }
	}
      break;

    default:;
    }
}

/* Return the propagation value when applying the operation CODE to
   the value RHS yielding type TYPE.  */

static ccp_prop_value_t
bit_value_unop (enum tree_code code, tree type, tree rhs)
{
  ccp_prop_value_t rval = get_value_for_expr (rhs, true);
  widest_int value, mask;
  ccp_prop_value_t val;

  if (rval.lattice_val == UNDEFINED)
    return rval;

  gcc_assert ((rval.lattice_val == CONSTANT
	       && TREE_CODE (rval.value) == INTEGER_CST)
	      || wi::sext (rval.mask, TYPE_PRECISION (TREE_TYPE (rhs))) == -1);
  bit_value_unop (code, TYPE_SIGN (type), TYPE_PRECISION (type), &value, &mask,
		  TYPE_SIGN (TREE_TYPE (rhs)), TYPE_PRECISION (TREE_TYPE (rhs)),
		  value_to_wide_int (rval), rval.mask);
  if (wi::sext (mask, TYPE_PRECISION (type)) != -1)
    {
      val.lattice_val = CONSTANT;
      val.mask = mask;
      /* ???  Delay building trees here.  */
      val.value = wide_int_to_tree (type, value);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = -1;
    }
  return val;
}

/* Return the propagation value when applying the operation CODE to
   the values RHS1 and RHS2 yielding type TYPE.  */

static ccp_prop_value_t
bit_value_binop (enum tree_code code, tree type, tree rhs1, tree rhs2)
{
  ccp_prop_value_t r1val = get_value_for_expr (rhs1, true);
  ccp_prop_value_t r2val = get_value_for_expr (rhs2, true);
  widest_int value, mask;
  ccp_prop_value_t val;

  if (r1val.lattice_val == UNDEFINED
      || r2val.lattice_val == UNDEFINED)
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = -1;
      return val;
    }

  gcc_assert ((r1val.lattice_val == CONSTANT
	       && TREE_CODE (r1val.value) == INTEGER_CST)
	      || wi::sext (r1val.mask,
			   TYPE_PRECISION (TREE_TYPE (rhs1))) == -1);
  gcc_assert ((r2val.lattice_val == CONSTANT
	       && TREE_CODE (r2val.value) == INTEGER_CST)
	      || wi::sext (r2val.mask,
			   TYPE_PRECISION (TREE_TYPE (rhs2))) == -1);
  bit_value_binop (code, TYPE_SIGN (type), TYPE_PRECISION (type), &value, &mask,
		   TYPE_SIGN (TREE_TYPE (rhs1)), TYPE_PRECISION (TREE_TYPE (rhs1)),
		   value_to_wide_int (r1val), r1val.mask,
		   TYPE_SIGN (TREE_TYPE (rhs2)), TYPE_PRECISION (TREE_TYPE (rhs2)),
		   value_to_wide_int (r2val), r2val.mask);

  /* (x * x) & 2 == 0.  */
  if (code == MULT_EXPR && rhs1 == rhs2 && TYPE_PRECISION (type) > 1)
    {
      widest_int m = 2;
      if (wi::sext (mask, TYPE_PRECISION (type)) != -1)
	value = wi::bit_and_not (value, m);
      else
	value = 0;
      mask = wi::bit_and_not (mask, m);
    }

  if (wi::sext (mask, TYPE_PRECISION (type)) != -1)
    {
      val.lattice_val = CONSTANT;
      val.mask = mask;
      /* ???  Delay building trees here.  */
      val.value = wide_int_to_tree (type, value);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = -1;
    }
  return val;
}

/* Return the propagation value for __builtin_assume_aligned
   and functions with assume_aligned or alloc_aligned attribute.
   For __builtin_assume_aligned, ATTR is NULL_TREE,
   for assume_aligned attribute ATTR is non-NULL and ALLOC_ALIGNED
   is false, for alloc_aligned attribute ATTR is non-NULL and
   ALLOC_ALIGNED is true.  */

static ccp_prop_value_t
bit_value_assume_aligned (gimple *stmt, tree attr, ccp_prop_value_t ptrval,
			  bool alloc_aligned)
{
  tree align, misalign = NULL_TREE, type;
  unsigned HOST_WIDE_INT aligni, misaligni = 0;
  ccp_prop_value_t alignval;
  widest_int value, mask;
  ccp_prop_value_t val;

  if (attr == NULL_TREE)
    {
      tree ptr = gimple_call_arg (stmt, 0);
      type = TREE_TYPE (ptr);
      ptrval = get_value_for_expr (ptr, true);
    }
  else
    {
      tree lhs = gimple_call_lhs (stmt);
      type = TREE_TYPE (lhs);
    }

  if (ptrval.lattice_val == UNDEFINED)
    return ptrval;
  gcc_assert ((ptrval.lattice_val == CONSTANT
	       && TREE_CODE (ptrval.value) == INTEGER_CST)
	      || wi::sext (ptrval.mask, TYPE_PRECISION (type)) == -1);
  if (attr == NULL_TREE)
    {
      /* Get aligni and misaligni from __builtin_assume_aligned.  */
      align = gimple_call_arg (stmt, 1);
      if (!tree_fits_uhwi_p (align))
	return ptrval;
      aligni = tree_to_uhwi (align);
      if (gimple_call_num_args (stmt) > 2)
	{
	  misalign = gimple_call_arg (stmt, 2);
	  if (!tree_fits_uhwi_p (misalign))
	    return ptrval;
	  misaligni = tree_to_uhwi (misalign);
	}
    }
  else
    {
      /* Get aligni and misaligni from assume_aligned or
	 alloc_align attributes.  */
      if (TREE_VALUE (attr) == NULL_TREE)
	return ptrval;
      attr = TREE_VALUE (attr);
      align = TREE_VALUE (attr);
      if (!tree_fits_uhwi_p (align))
	return ptrval;
      aligni = tree_to_uhwi (align);
      if (alloc_aligned)
	{
	  if (aligni == 0 || aligni > gimple_call_num_args (stmt))
	    return ptrval;
	  align = gimple_call_arg (stmt, aligni - 1);
	  if (!tree_fits_uhwi_p (align))
	    return ptrval;
	  aligni = tree_to_uhwi (align);
	}
      else if (TREE_CHAIN (attr) && TREE_VALUE (TREE_CHAIN (attr)))
	{
	  misalign = TREE_VALUE (TREE_CHAIN (attr));
	  if (!tree_fits_uhwi_p (misalign))
	    return ptrval;
	  misaligni = tree_to_uhwi (misalign);
	}
    }
  if (aligni <= 1 || (aligni & (aligni - 1)) != 0 || misaligni >= aligni)
    return ptrval;

  align = build_int_cst_type (type, -aligni);
  alignval = get_value_for_expr (align, true);
  bit_value_binop (BIT_AND_EXPR, TYPE_SIGN (type), TYPE_PRECISION (type), &value, &mask,
		   TYPE_SIGN (type), TYPE_PRECISION (type), value_to_wide_int (ptrval), ptrval.mask,
		   TYPE_SIGN (type), TYPE_PRECISION (type), value_to_wide_int (alignval), alignval.mask);

  if (wi::sext (mask, TYPE_PRECISION (type)) != -1)
    {
      val.lattice_val = CONSTANT;
      val.mask = mask;
      gcc_assert ((mask.to_uhwi () & (aligni - 1)) == 0);
      gcc_assert ((value.to_uhwi () & (aligni - 1)) == 0);
      value |= misaligni;
      /* ???  Delay building trees here.  */
      val.value = wide_int_to_tree (type, value);
    }
  else
    {
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = -1;
    }
  return val;
}

/* Evaluate statement STMT.
   Valid only for assignments, calls, conditionals, and switches. */

static ccp_prop_value_t
evaluate_stmt (gimple *stmt)
{
  ccp_prop_value_t val;
  tree simplified = NULL_TREE;
  ccp_lattice_t likelyvalue = likely_value (stmt);
  bool is_constant = false;
  unsigned int align;
  bool ignore_return_flags = false;

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
      if (simplified
	  && TREE_CODE (simplified) == SSA_NAME)
	{
	  /* We may not use values of something that may be simulated again,
	     see valueize_op_1.  */
	  if (SSA_NAME_IS_DEFAULT_DEF (simplified)
	      || ! prop_simulate_again_p (SSA_NAME_DEF_STMT (simplified)))
	    {
	      ccp_prop_value_t *val = get_value (simplified);
	      if (val && val->lattice_val != VARYING)
		{
		  fold_undefer_overflow_warnings (true, stmt, 0);
		  return *val;
		}
	    }
	  else
	    /* We may also not place a non-valueized copy in the lattice
	       as that might become stale if we never re-visit this stmt.  */
	    simplified = NULL_TREE;
	}
      is_constant = simplified && is_gimple_min_invariant (simplified);
      fold_undefer_overflow_warnings (is_constant, stmt, 0);
      if (is_constant)
	{
	  /* The statement produced a constant value.  */
	  val.lattice_val = CONSTANT;
	  val.value = simplified;
	  val.mask = 0;
	  return val;
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
        simplified = gimple_switch_index (as_a <gswitch *> (stmt));
      else
	/* These cannot satisfy is_gimple_min_invariant without folding.  */
	gcc_assert (code == GIMPLE_CALL || code == GIMPLE_COND);
      is_constant = simplified && is_gimple_min_invariant (simplified);
      if (is_constant)
	{
	  /* The statement produced a constant value.  */
	  val.lattice_val = CONSTANT;
	  val.value = simplified;
	  val.mask = 0;
	}
    }
  /* If the statement result is likely UNDEFINED, make it so.  */
  else if (likelyvalue == UNDEFINED)
    {
      val.lattice_val = UNDEFINED;
      val.value = NULL_TREE;
      val.mask = 0;
      return val;
    }

  /* Resort to simplification for bitwise tracking.  */
  if (flag_tree_bit_ccp
      && (likelyvalue == CONSTANT || is_gimple_call (stmt)
	  || (gimple_assign_single_p (stmt)
	      && gimple_assign_rhs_code (stmt) == ADDR_EXPR))
      && !is_constant)
    {
      enum gimple_code code = gimple_code (stmt);
      val.lattice_val = VARYING;
      val.value = NULL_TREE;
      val.mask = -1;
      if (code == GIMPLE_ASSIGN)
	{
	  enum tree_code subcode = gimple_assign_rhs_code (stmt);
	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  tree lhs = gimple_assign_lhs (stmt);
	  if ((INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	       || POINTER_TYPE_P (TREE_TYPE (lhs)))
	      && (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		  || POINTER_TYPE_P (TREE_TYPE (rhs1))))
	    switch (get_gimple_rhs_class (subcode))
	      {
	      case GIMPLE_SINGLE_RHS:
	        val = get_value_for_expr (rhs1, true);
		break;

	      case GIMPLE_UNARY_RHS:
		val = bit_value_unop (subcode, TREE_TYPE (lhs), rhs1);
		break;

	      case GIMPLE_BINARY_RHS:
		val = bit_value_binop (subcode, TREE_TYPE (lhs), rhs1,
				       gimple_assign_rhs2 (stmt));
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
	    case BUILT_IN_GOMP_REALLOC:
	    case BUILT_IN_CALLOC:
	    case BUILT_IN_STRDUP:
	    case BUILT_IN_STRNDUP:
	      val.lattice_val = CONSTANT;
	      val.value = build_int_cst (TREE_TYPE (gimple_get_lhs (stmt)), 0);
	      val.mask = ~((HOST_WIDE_INT) MALLOC_ABI_ALIGNMENT
			   / BITS_PER_UNIT - 1);
	      break;

	    CASE_BUILT_IN_ALLOCA:
	      align = (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA
		       ? BIGGEST_ALIGNMENT
		       : TREE_INT_CST_LOW (gimple_call_arg (stmt, 1)));
	      val.lattice_val = CONSTANT;
	      val.value = build_int_cst (TREE_TYPE (gimple_get_lhs (stmt)), 0);
	      val.mask = ~((HOST_WIDE_INT) align / BITS_PER_UNIT - 1);
	      break;

	    case BUILT_IN_ASSUME_ALIGNED:
	      val = bit_value_assume_aligned (stmt, NULL_TREE, val, false);
	      ignore_return_flags = true;
	      break;

	    case BUILT_IN_ALIGNED_ALLOC:
	    case BUILT_IN_GOMP_ALLOC:
	      {
		tree align = get_constant_value (gimple_call_arg (stmt, 0));
		if (align
		    && tree_fits_uhwi_p (align))
		  {
		    unsigned HOST_WIDE_INT aligni = tree_to_uhwi (align);
		    if (aligni > 1
			/* align must be power-of-two */
			&& (aligni & (aligni - 1)) == 0)
		      {
			val.lattice_val = CONSTANT;
			val.value = build_int_cst (ptr_type_node, 0);
			val.mask = -aligni;
		      }
		  }
		break;
	      }

	    case BUILT_IN_BSWAP16:
	    case BUILT_IN_BSWAP32:
	    case BUILT_IN_BSWAP64:
	    case BUILT_IN_BSWAP128:
	      val = get_value_for_expr (gimple_call_arg (stmt, 0), true);
	      if (val.lattice_val == UNDEFINED)
		break;
	      else if (val.lattice_val == CONSTANT
		       && val.value
		       && TREE_CODE (val.value) == INTEGER_CST)
		{
		  tree type = TREE_TYPE (gimple_call_lhs (stmt));
		  int prec = TYPE_PRECISION (type);
		  wide_int wval = wi::to_wide (val.value);
		  val.value
		    = wide_int_to_tree (type,
					wi::bswap (wide_int::from (wval, prec,
								   UNSIGNED)));
		  val.mask
		    = widest_int::from (wi::bswap (wide_int::from (val.mask,
								   prec,
								   UNSIGNED)),
					UNSIGNED);
		  if (wi::sext (val.mask, prec) != -1)
		    break;
		}
	      val.lattice_val = VARYING;
	      val.value = NULL_TREE;
	      val.mask = -1;
	      break;

	    default:;
	    }
	}
      if (is_gimple_call (stmt) && gimple_call_lhs (stmt))
	{
	  tree fntype = gimple_call_fntype (stmt);
	  if (fntype)
	    {
	      tree attrs = lookup_attribute ("assume_aligned",
					     TYPE_ATTRIBUTES (fntype));
	      if (attrs)
		val = bit_value_assume_aligned (stmt, attrs, val, false);
	      attrs = lookup_attribute ("alloc_align",
					TYPE_ATTRIBUTES (fntype));
	      if (attrs)
		val = bit_value_assume_aligned (stmt, attrs, val, true);
	    }
	  int flags = ignore_return_flags
		      ? 0 : gimple_call_return_flags (as_a <gcall *> (stmt));
	  if (flags & ERF_RETURNS_ARG
	      && (flags & ERF_RETURN_ARG_MASK) < gimple_call_num_args (stmt))
	    {
	      val = get_value_for_expr
			 (gimple_call_arg (stmt,
					   flags & ERF_RETURN_ARG_MASK), true);
	    }
	}
      is_constant = (val.lattice_val == CONSTANT);
    }

  if (flag_tree_bit_ccp
      && ((is_constant && TREE_CODE (val.value) == INTEGER_CST)
	  || !is_constant)
      && gimple_get_lhs (stmt)
      && TREE_CODE (gimple_get_lhs (stmt)) == SSA_NAME)
    {
      tree lhs = gimple_get_lhs (stmt);
      wide_int nonzero_bits = get_nonzero_bits (lhs);
      if (nonzero_bits != -1)
	{
	  if (!is_constant)
	    {
	      val.lattice_val = CONSTANT;
	      val.value = build_zero_cst (TREE_TYPE (lhs));
	      val.mask = extend_mask (nonzero_bits, TYPE_SIGN (TREE_TYPE (lhs)));
	      is_constant = true;
	    }
	  else
	    {
	      if (wi::bit_and_not (wi::to_wide (val.value), nonzero_bits) != 0)
		val.value = wide_int_to_tree (TREE_TYPE (lhs),
					      nonzero_bits
					      & wi::to_wide (val.value));
	      if (nonzero_bits == 0)
		val.mask = 0;
	      else
		val.mask = val.mask & extend_mask (nonzero_bits,
						   TYPE_SIGN (TREE_TYPE (lhs)));
	    }
	}
    }

  /* The statement produced a nonconstant value.  */
  if (!is_constant)
    {
      /* The statement produced a copy.  */
      if (simplified && TREE_CODE (simplified) == SSA_NAME
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (simplified))
	{
	  val.lattice_val = CONSTANT;
	  val.value = simplified;
	  val.mask = -1;
	}
      /* The statement is VARYING.  */
      else
	{
	  val.lattice_val = VARYING;
	  val.value = NULL_TREE;
	  val.mask = -1;
	}
    }

  return val;
}

typedef hash_table<nofree_ptr_hash<gimple> > gimple_htab;

/* Given a BUILT_IN_STACK_SAVE value SAVED_VAL, insert a clobber of VAR before
   each matching BUILT_IN_STACK_RESTORE.  Mark visited phis in VISITED.  */

static void
insert_clobber_before_stack_restore (tree saved_val, tree var,
				     gimple_htab **visited)
{
  gimple *stmt;
  gassign *clobber_stmt;
  tree clobber;
  imm_use_iterator iter;
  gimple_stmt_iterator i;
  gimple **slot;

  FOR_EACH_IMM_USE_STMT (stmt, iter, saved_val)
    if (gimple_call_builtin_p (stmt, BUILT_IN_STACK_RESTORE))
      {
	clobber = build_clobber (TREE_TYPE (var), CLOBBER_STORAGE_END);
	clobber_stmt = gimple_build_assign (var, clobber);

	i = gsi_for_stmt (stmt);
	gsi_insert_before (&i, clobber_stmt, GSI_SAME_STMT);
      }
    else if (gimple_code (stmt) == GIMPLE_PHI)
      {
	if (!*visited)
	  *visited = new gimple_htab (10);

	slot = (*visited)->find_slot (stmt, INSERT);
	if (*slot != NULL)
	  continue;

	*slot = stmt;
	insert_clobber_before_stack_restore (gimple_phi_result (stmt), var,
					     visited);
      }
    else if (gimple_assign_ssa_name_copy_p (stmt))
      insert_clobber_before_stack_restore (gimple_assign_lhs (stmt), var,
					   visited);
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
      dom = get_immediate_dominator (CDI_DOMINATORS, gsi_bb (*i));
      if (dom == NULL || dom == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	return;

      *i = gsi_last_bb (dom);
    }
}

/* Find a BUILT_IN_STACK_SAVE dominating gsi_stmt (I), and insert
   a clobber of VAR before each matching BUILT_IN_STACK_RESTORE.

   It is possible that BUILT_IN_STACK_SAVE cannot be found in a dominator when
   a previous pass (such as DOM) duplicated it along multiple paths to a BB.
   In that case the function gives up without inserting the clobbers.  */

static void
insert_clobbers_for_var (gimple_stmt_iterator i, tree var)
{
  gimple *stmt;
  tree saved_val;
  gimple_htab *visited = NULL;

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

  delete visited;
}

/* Detects a __builtin_alloca_with_align with constant size argument.  Declares
   fixed-size array and returns the address, if found, otherwise returns
   NULL_TREE.  */

static tree
fold_builtin_alloca_with_align (gimple *stmt)
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
      || !tree_fits_uhwi_p (arg))
    return NULL_TREE;

  size = tree_to_uhwi (arg);

  /* Heuristic: don't fold large allocas.  */
  threshold = (unsigned HOST_WIDE_INT)param_large_stack_frame;
  /* In case the alloca is located at function entry, it has the same lifetime
     as a declared array, so we allow a larger size.  */
  block = gimple_block (stmt);
  if (!(cfun->after_inlining
	&& block
        && TREE_CODE (BLOCK_SUPERCONTEXT (block)) == FUNCTION_DECL))
    threshold /= 10;
  if (size > threshold)
    return NULL_TREE;

  /* We have to be able to move points-to info.  We used to assert
     that we can but IPA PTA might end up with two UIDs here
     as it might need to handle more than one instance being
     live at the same time.  Instead of trying to detect this case
     (using the first UID would be OK) just give up for now.  */
  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (lhs);
  unsigned uid = 0;
  if (pi != NULL
      && !pi->pt.anything
      && !pt_solution_singleton_or_null_p (&pi->pt, &uid))
    return NULL_TREE;

  /* Declare array.  */
  elem_type = build_nonstandard_integer_type (BITS_PER_UNIT, 1);
  n_elem = size * 8 / BITS_PER_UNIT;
  array_type = build_array_type_nelts (elem_type, n_elem);

  if (tree ssa_name = SSA_NAME_IDENTIFIER (lhs))
    {
      /* Give the temporary a name derived from the name of the VLA
	 declaration so it can be referenced in diagnostics.  */
      const char *name = IDENTIFIER_POINTER (ssa_name);
      var = create_tmp_var (array_type, name);
    }
  else
    var = create_tmp_var (array_type);

  if (gimple *lhsdef = SSA_NAME_DEF_STMT (lhs))
    {
      /* Set the temporary's location to that of the VLA declaration
	 so it can be pointed to in diagnostics.  */
      location_t loc = gimple_location (lhsdef);
      DECL_SOURCE_LOCATION (var) = loc;
    }

  SET_DECL_ALIGN (var, TREE_INT_CST_LOW (gimple_call_arg (stmt, 1)));
  if (uid != 0)
    SET_DECL_PT_UID (var, uid);

  /* Fold alloca to the address of the array.  */
  return fold_convert (TREE_TYPE (lhs), build_fold_addr_expr (var));
}

/* Fold the stmt at *GSI with CCP specific information that propagating
   and regular folding does not catch.  */

bool
ccp_folder::fold_stmt (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      {
	gcond *cond_stmt = as_a <gcond *> (stmt);
	ccp_prop_value_t val;
	/* Statement evaluation will handle type mismatches in constants
	   more gracefully than the final propagation.  This allows us to
	   fold more conditionals here.  */
	val = evaluate_stmt (stmt);
	if (val.lattice_val != CONSTANT
	    || val.mask != 0)
	  return false;

	if (dump_file)
	  {
	    fprintf (dump_file, "Folding predicate ");
	    print_gimple_expr (dump_file, stmt, 0);
	    fprintf (dump_file, " to ");
	    print_generic_expr (dump_file, val.value);
	    fprintf (dump_file, "\n");
	  }

	if (integer_zerop (val.value))
	  gimple_cond_make_false (cond_stmt);
	else
	  gimple_cond_make_true (cond_stmt);

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
	    if (!useless_type_conversion_p (TREE_TYPE (lhs),
					    TREE_TYPE (new_rhs)))
	      new_rhs = fold_convert (TREE_TYPE (lhs), new_rhs);
	    gimplify_and_update_call_from_tree (gsi, new_rhs);
	    return true;
	  }

	/* Internal calls provide no argument types, so the extra laxity
	   for normal calls does not apply.  */
	if (gimple_call_internal_p (stmt))
	  return false;

        /* The heuristic of fold_builtin_alloca_with_align differs before and
	   after inlining, so we don't require the arg to be changed into a
	   constant for folding, but just to be constant.  */
        if (gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN)
	    || gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX))
          {
            tree new_rhs = fold_builtin_alloca_with_align (stmt);
            if (new_rhs)
	      {
		gimplify_and_update_call_from_tree (gsi, new_rhs);
		tree var = TREE_OPERAND (TREE_OPERAND (new_rhs, 0),0);
		insert_clobbers_for_var (*gsi, var);
		return true;
	      }
          }

	/* If there's no extra info from an assume_aligned call,
	   drop it so it doesn't act as otherwise useless dataflow
	   barrier.  */
	if (gimple_call_builtin_p (stmt, BUILT_IN_ASSUME_ALIGNED))
	  {
	    tree ptr = gimple_call_arg (stmt, 0);
	    ccp_prop_value_t ptrval = get_value_for_expr (ptr, true);
	    if (ptrval.lattice_val == CONSTANT
		&& TREE_CODE (ptrval.value) == INTEGER_CST
		&& ptrval.mask != 0)
	      {
		ccp_prop_value_t val
		  = bit_value_assume_aligned (stmt, NULL_TREE, ptrval, false);
		unsigned int ptralign = least_bit_hwi (ptrval.mask.to_uhwi ());
		unsigned int align = least_bit_hwi (val.mask.to_uhwi ());
		if (ptralign == align
		    && ((TREE_INT_CST_LOW (ptrval.value) & (align - 1))
			== (TREE_INT_CST_LOW (val.value) & (align - 1))))
		  {
		    replace_call_with_value (gsi, ptr);
		    return true;
		  }
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
visit_assignment (gimple *stmt, tree *output_p)
{
  ccp_prop_value_t val;
  enum ssa_prop_result retval = SSA_PROP_NOT_INTERESTING;

  tree lhs = gimple_get_lhs (stmt);
  if (TREE_CODE (lhs) == SSA_NAME)
    {
      /* Evaluate the statement, which could be
	 either a GIMPLE_ASSIGN or a GIMPLE_CALL.  */
      val = evaluate_stmt (stmt);

      /* If STMT is an assignment to an SSA_NAME, we only have one
	 value to set.  */
      if (set_lattice_value (lhs, &val))
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
visit_cond_stmt (gimple *stmt, edge *taken_edge_p)
{
  ccp_prop_value_t val;
  basic_block block;

  block = gimple_bb (stmt);
  val = evaluate_stmt (stmt);
  if (val.lattice_val != CONSTANT
      || val.mask != 0)
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

enum ssa_prop_result
ccp_propagate::visit_stmt (gimple *stmt, edge *taken_edge_p, tree *output_p)
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
    set_value_varying (def);

  return SSA_PROP_VARYING;
}


/* Main entry point for SSA Conditional Constant Propagation.  If NONZERO_P,
   record nonzero bits.  */

static unsigned int
do_ssa_ccp (bool nonzero_p)
{
  unsigned int todo = 0;
  calculate_dominance_info (CDI_DOMINATORS);

  ccp_initialize ();
  class ccp_propagate ccp_propagate;
  ccp_propagate.ssa_propagate ();
  if (ccp_finalize (nonzero_p || flag_ipa_bit_cp))
    {
      todo = (TODO_cleanup_cfg | TODO_update_ssa);

      /* ccp_finalize does not preserve loop-closed ssa.  */
      loops_state_clear (LOOP_CLOSED_SSA);
    }

  free_dominance_info (CDI_DOMINATORS);
  return todo;
}


namespace {

const pass_data pass_data_ccp =
{
  GIMPLE_PASS, /* type */
  "ccp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_CCP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_address_taken, /* todo_flags_finish */
};

class pass_ccp : public gimple_opt_pass
{
public:
  pass_ccp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_ccp, ctxt), nonzero_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_ccp (m_ctxt); }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      nonzero_p = param;
    }
  bool gate (function *) final override { return flag_tree_ccp != 0; }
  unsigned int execute (function *) final override
  {
    return do_ssa_ccp (nonzero_p);
  }

 private:
  /* Determines whether the pass instance records nonzero bits.  */
  bool nonzero_p;
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
  gimple *stmt;

  basic_block bb = gsi_bb (i);
  gimple *call = gsi_stmt (i);

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
	  || !fndecl_built_in_p (callee, BUILT_IN_NORMAL)
	  /* All regular builtins are ok, just obviously not alloca.  */
	  || ALLOCA_FUNCTION_CODE_P (DECL_FUNCTION_CODE (callee))
	  /* Do not remove stack updates before strub leave.  */
	  || fndecl_built_in_p (callee, BUILT_IN___STRUB_LEAVE))
	return NULL_TREE;

      if (fndecl_built_in_p (callee, BUILT_IN_STACK_RESTORE))
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
      if (single_succ_edge (bb)->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
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
      gimple *stack_save = SSA_NAME_DEF_STMT (gimple_call_arg (call, 0));
      if (is_gimple_call (stack_save))
	{
	  callee = gimple_call_fndecl (stack_save);
	  if (callee && fndecl_built_in_p (callee, BUILT_IN_STACK_SAVE))
	    {
	      gimple_stmt_iterator stack_save_gsi;
	      tree rhs;

	      stack_save_gsi = gsi_for_stmt (stack_save);
	      rhs = build_int_cst (TREE_TYPE (gimple_call_arg (call, 0)), 0);
	      replace_call_with_value (&stack_save_gsi, rhs);
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
optimize_stdarg_builtin (gimple *call)
{
  tree callee, lhs, rhs, cfun_va_list;
  bool va_list_simple_ptr;
  location_t loc = gimple_location (call);

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
  gimple *stmt;
  edge_iterator ei;
  edge e;
  bool ret;

  if (flag_sanitize & SANITIZE_UNREACHABLE)
    return false;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);

      if (is_gimple_debug (stmt))
       continue;

      if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	{
	  /* Verify we do not need to preserve the label.  */
	  if (FORCED_LABEL (gimple_label_label (label_stmt)))
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
      if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
	{
	  if (e->flags & EDGE_TRUE_VALUE)
	    gimple_cond_make_false (cond_stmt);
	  else if (e->flags & EDGE_FALSE_VALUE)
	    gimple_cond_make_true (cond_stmt);
	  else
	    gcc_unreachable ();
	  update_stmt (cond_stmt);
	}
      else
	{
	  /* Todo: handle other cases.  Note that unreachable switch case
	     statements have already been removed.  */
	  continue;
	}

      ret = true;
    }

  return ret;
}

/* Convert
   _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
   _7 = ~_1;
   _5 = (_Bool) _7;
   to
   _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
   _8 = _1 & 1;
   _5 = _8 == 0;
   and convert
   _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
   _7 = ~_1;
   _4 = (_Bool) _7;
   to
   _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
   _8 = _1 & 1;
   _4 = (_Bool) _8;

   USE_STMT is the gimplt statement which uses the return value of
   __atomic_fetch_or_*.  LHS is the return value of __atomic_fetch_or_*.
   MASK is the mask passed to __atomic_fetch_or_*.
 */

static gimple *
convert_atomic_bit_not (enum internal_fn fn, gimple *use_stmt,
			tree lhs, tree mask)
{
  tree and_mask;
  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
    {
      /* MASK must be ~1.  */
      if (!operand_equal_p (build_int_cst (TREE_TYPE (lhs),
					   ~HOST_WIDE_INT_1), mask, 0))
	return nullptr;
      and_mask = build_int_cst (TREE_TYPE (lhs), 1);
    }
  else
    {
      /* MASK must be 1.  */
      if (!operand_equal_p (build_int_cst (TREE_TYPE (lhs), 1), mask, 0))
	return nullptr;
      and_mask = mask;
    }

  tree use_lhs = gimple_assign_lhs (use_stmt);

  use_operand_p use_p;
  gimple *use_not_stmt;

  if (!single_imm_use (use_lhs, &use_p, &use_not_stmt)
      || !is_gimple_assign (use_not_stmt))
    return nullptr;

  if (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (use_not_stmt)))
    return nullptr;

  tree use_not_lhs = gimple_assign_lhs (use_not_stmt);
  if (TREE_CODE (TREE_TYPE (use_not_lhs)) != BOOLEAN_TYPE)
    return nullptr;

  gimple_stmt_iterator gsi;
  tree var = make_ssa_name (TREE_TYPE (lhs));
  /* use_stmt need to be removed after use_nop_stmt,
     so use_lhs can be released.  */
  gimple *use_stmt_removal = use_stmt;
  use_stmt = gimple_build_assign (var, BIT_AND_EXPR, lhs, and_mask);
  gsi = gsi_for_stmt (use_not_stmt);
  gsi_insert_before (&gsi, use_stmt, GSI_NEW_STMT);
  lhs = gimple_assign_lhs (use_not_stmt);
  gimple *g = gimple_build_assign (lhs, EQ_EXPR, var,
				   build_zero_cst (TREE_TYPE (mask)));
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  gsi = gsi_for_stmt (use_not_stmt);
  gsi_remove (&gsi, true);
  gsi = gsi_for_stmt (use_stmt_removal);
  gsi_remove (&gsi, true);
  return use_stmt;
}

/* match.pd function to match atomic_bit_test_and pattern which
   has nop_convert:
     _1 = __atomic_fetch_or_4 (&v, 1, 0);
     _2 = (int) _1;
     _5 = _2 & 1;
 */
extern bool gimple_nop_atomic_bit_test_and_p (tree, tree *,
					      tree (*) (tree));
extern bool gimple_nop_convert (tree, tree*, tree (*) (tree));

/* Optimize
     mask_2 = 1 << cnt_1;
     _4 = __atomic_fetch_or_* (ptr_6, mask_2, _3);
     _5 = _4 & mask_2;
   to
     _4 = .ATOMIC_BIT_TEST_AND_SET (ptr_6, cnt_1, 0, _3);
     _5 = _4;
   If _5 is only used in _5 != 0 or _5 == 0 comparisons, 1
   is passed instead of 0, and the builtin just returns a zero
   or 1 value instead of the actual bit.
   Similarly for __sync_fetch_and_or_* (without the ", _3" part
   in there), and/or if mask_2 is a power of 2 constant.
   Similarly for xor instead of or, use ATOMIC_BIT_TEST_AND_COMPLEMENT
   in that case.  And similarly for and instead of or, except that
   the second argument to the builtin needs to be one's complement
   of the mask instead of mask.  */

static bool
optimize_atomic_bit_test_and (gimple_stmt_iterator *gsip,
			      enum internal_fn fn, bool has_model_arg,
			      bool after)
{
  gimple *call = gsi_stmt (*gsip);
  tree lhs = gimple_call_lhs (call);
  use_operand_p use_p;
  gimple *use_stmt;
  tree mask;
  optab optab;

  if (!flag_inline_atomics
      || optimize_debug
      || !gimple_call_builtin_p (call, BUILT_IN_NORMAL)
      || !lhs
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
      || !single_imm_use (lhs, &use_p, &use_stmt)
      || !is_gimple_assign (use_stmt)
      || !gimple_vdef (call))
    return false;

  switch (fn)
    {
    case IFN_ATOMIC_BIT_TEST_AND_SET:
      optab = atomic_bit_test_and_set_optab;
      break;
    case IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT:
      optab = atomic_bit_test_and_complement_optab;
      break;
    case IFN_ATOMIC_BIT_TEST_AND_RESET:
      optab = atomic_bit_test_and_reset_optab;
      break;
    default:
      return false;
    }

  tree bit = nullptr;

  mask = gimple_call_arg (call, 1);
  tree_code rhs_code = gimple_assign_rhs_code (use_stmt);
  if (rhs_code != BIT_AND_EXPR)
    {
      if (rhs_code != NOP_EXPR && rhs_code != BIT_NOT_EXPR)
	return false;

      tree use_lhs = gimple_assign_lhs (use_stmt);
      if (TREE_CODE (use_lhs) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs))
	return false;

      tree use_rhs = gimple_assign_rhs1 (use_stmt);
      if (lhs != use_rhs)
	return false;

      if (optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)))
	  == CODE_FOR_nothing)
	return false;

      gimple *g;
      gimple_stmt_iterator gsi;
      tree var;
      int ibit = -1;

      if (rhs_code == BIT_NOT_EXPR)
	{
	  g = convert_atomic_bit_not (fn, use_stmt, lhs, mask);
	  if (!g)
	    return false;
	  use_stmt = g;
	  ibit = 0;
	}
      else if (TREE_CODE (TREE_TYPE (use_lhs)) == BOOLEAN_TYPE)
	{
	  tree and_mask;
	  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
	    {
	      /* MASK must be ~1.  */
	      if (!operand_equal_p (build_int_cst (TREE_TYPE (lhs),
						   ~HOST_WIDE_INT_1),
				    mask, 0))
		return false;

	      /* Convert
		 _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
		 _4 = (_Bool) _1;
		 to
		 _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
		 _5 = _1 & 1;
		 _4 = (_Bool) _5;
	       */
	      and_mask = build_int_cst (TREE_TYPE (lhs), 1);
	    }
	  else
	    {
	      and_mask = build_int_cst (TREE_TYPE (lhs), 1);
	      if (!operand_equal_p (and_mask, mask, 0))
		return false;

	      /* Convert
		 _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
		 _4 = (_Bool) _1;
		 to
		 _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
		 _5 = _1 & 1;
		 _4 = (_Bool) _5;
	       */
	    }
	  var = make_ssa_name (TREE_TYPE (use_rhs));
	  replace_uses_by (use_rhs, var);
	  g = gimple_build_assign (var, BIT_AND_EXPR, use_rhs,
				   and_mask);
	  gsi = gsi_for_stmt (use_stmt);
	  gsi_insert_before (&gsi, g, GSI_NEW_STMT);
	  use_stmt = g;
	  ibit = 0;
	}
      else if (TYPE_PRECISION (TREE_TYPE (use_lhs))
	       <= TYPE_PRECISION (TREE_TYPE (use_rhs)))
	{
	  gimple *use_nop_stmt;
	  if (!single_imm_use (use_lhs, &use_p, &use_nop_stmt)
	      || (!is_gimple_assign (use_nop_stmt)
		  && gimple_code (use_nop_stmt) != GIMPLE_COND))
	    return false;
	  /* Handle both
	     _4 = _5 < 0;
	     and
	     if (_5 < 0)
	   */
	  tree use_nop_lhs = nullptr;
	  rhs_code = ERROR_MARK;
	  if (is_gimple_assign (use_nop_stmt))
	    {
	      use_nop_lhs = gimple_assign_lhs (use_nop_stmt);
	      rhs_code = gimple_assign_rhs_code (use_nop_stmt);
	    }
	  if (!use_nop_lhs || rhs_code != BIT_AND_EXPR)
	    {
	      /* Also handle
		 if (_5 < 0)
	       */
	      if (use_nop_lhs
		  && TREE_CODE (use_nop_lhs) == SSA_NAME
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_nop_lhs))
		return false;
	      if (use_nop_lhs && rhs_code == BIT_NOT_EXPR)
		{
		  /* Handle
		     _7 = ~_2;
		   */
		  g = convert_atomic_bit_not (fn, use_nop_stmt, lhs,
					      mask);
		  if (!g)
		    return false;
		  /* Convert
		     _1 = __atomic_fetch_or_4 (ptr_6, 1, _3);
		     _2 = (int) _1;
		     _7 = ~_2;
		     _5 = (_Bool) _7;
		     to
		     _1 = __atomic_fetch_or_4 (ptr_6, ~1, _3);
		     _8 = _1 & 1;
		     _5 = _8 == 0;
		     and convert
		     _1 = __atomic_fetch_and_4 (ptr_6, ~1, _3);
		     _2 = (int) _1;
		     _7 = ~_2;
		     _5 = (_Bool) _7;
		     to
		     _1 = __atomic_fetch_and_4 (ptr_6, 1, _3);
		     _8 = _1 & 1;
		     _5 = _8 == 0;
		   */
		  gsi = gsi_for_stmt (use_stmt);
		  gsi_remove (&gsi, true);
		  use_stmt = g;
		  ibit = 0;
		}
	      else
		{
		  tree cmp_rhs1, cmp_rhs2;
		  if (use_nop_lhs)
		    {
		      /* Handle
			 _4 = _5 < 0;
		       */
		      if (TREE_CODE (TREE_TYPE (use_nop_lhs))
			  != BOOLEAN_TYPE)
			return false;
		      cmp_rhs1 = gimple_assign_rhs1 (use_nop_stmt);
		      cmp_rhs2 = gimple_assign_rhs2 (use_nop_stmt);
		    }
		  else
		    {
		      /* Handle
			 if (_5 < 0)
		       */
		      rhs_code = gimple_cond_code (use_nop_stmt);
		      cmp_rhs1 = gimple_cond_lhs (use_nop_stmt);
		      cmp_rhs2 = gimple_cond_rhs (use_nop_stmt);
		    }
		  if (rhs_code != GE_EXPR && rhs_code != LT_EXPR)
		    return false;
		  if (use_lhs != cmp_rhs1)
		    return false;
		  if (!integer_zerop (cmp_rhs2))
		    return false;

		  tree and_mask;

		  unsigned HOST_WIDE_INT bytes
		    = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (use_rhs)));
		  ibit = bytes * BITS_PER_UNIT - 1;
		  unsigned HOST_WIDE_INT highest
		    = HOST_WIDE_INT_1U << ibit;

		  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
		    {
		      /* Get the signed maximum of the USE_RHS type.  */
		      and_mask = build_int_cst (TREE_TYPE (use_rhs),
						highest - 1);
		      if (!operand_equal_p (and_mask, mask, 0))
			return false;

		      /* Convert
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _5 = (signed int) _1;
			 _4 = _5 < 0 or _5 >= 0;
			 to
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _6 = _1 & 0x80000000;
			 _4 = _6 != 0 or _6 == 0;
			 and convert
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _5 = (signed int) _1;
			 if (_5 < 0 or _5 >= 0)
			 to
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _6 = _1 & 0x80000000;
			 if (_6 != 0 or _6 == 0)
		       */
		      and_mask = build_int_cst (TREE_TYPE (use_rhs),
						highest);
		    }
		  else
		    {
		      /* Get the signed minimum of the USE_RHS type.  */
		      and_mask = build_int_cst (TREE_TYPE (use_rhs),
						highest);
		      if (!operand_equal_p (and_mask, mask, 0))
			return false;

		      /* Convert
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _5 = (signed int) _1;
			 _4 = _5 < 0 or _5 >= 0;
			 to
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _6 = _1 & 0x80000000;
			 _4 = _6 != 0 or _6 == 0;
			 and convert
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _5 = (signed int) _1;
			 if (_5 < 0 or _5 >= 0)
			 to
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _6 = _1 & 0x80000000;
			 if (_6 != 0 or _6 == 0)
		       */
		    }
		  var = make_ssa_name (TREE_TYPE (use_rhs));
		  gimple* use_stmt_removal = use_stmt;
		  g = gimple_build_assign (var, BIT_AND_EXPR, use_rhs,
					   and_mask);
		  gsi = gsi_for_stmt (use_nop_stmt);
		  gsi_insert_before (&gsi, g, GSI_NEW_STMT);
		  use_stmt = g;
		  rhs_code = rhs_code == GE_EXPR ? EQ_EXPR : NE_EXPR;
		  tree const_zero = build_zero_cst (TREE_TYPE (use_rhs));
		  if (use_nop_lhs)
		    g = gimple_build_assign (use_nop_lhs, rhs_code,
					     var, const_zero);
		  else
		    g = gimple_build_cond (rhs_code, var, const_zero,
					   nullptr, nullptr);
		  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
		  gsi = gsi_for_stmt (use_nop_stmt);
		  gsi_remove (&gsi, true);
		  gsi = gsi_for_stmt (use_stmt_removal);
		  gsi_remove (&gsi, true);
		}
	    }
	  else
	    {
	      tree match_op[3];
	      gimple *g;
	      if (!gimple_nop_atomic_bit_test_and_p (use_nop_lhs,
						     &match_op[0], NULL)
		  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (match_op[2])
		  || !single_imm_use (match_op[2], &use_p, &g)
		  || !is_gimple_assign (g))
		return false;
	      mask = match_op[0];
	      if (TREE_CODE (match_op[1]) == INTEGER_CST)
		{
		  ibit = tree_log2 (match_op[1]);
		  gcc_assert (ibit >= 0);
		}
	      else
		{
		  g = SSA_NAME_DEF_STMT (match_op[1]);
		  gcc_assert (is_gimple_assign (g));
		  bit = gimple_assign_rhs2 (g);
		}
	      /* Convert
		 _1 = __atomic_fetch_or_4 (ptr_6, mask, _3);
		 _2 = (int) _1;
		 _5 = _2 & mask;
		 to
		 _1 = __atomic_fetch_or_4 (ptr_6, mask, _3);
		 _6 = _1 & mask;
		 _5 = (int) _6;
		 and convert
		 _1 = ~mask_7;
		 _2 = (unsigned int) _1;
		 _3 = __atomic_fetch_and_4 (ptr_6, _2, 0);
		 _4 = (int) _3;
		 _5 = _4 & mask_7;
		 to
		 _1 = __atomic_fetch_and_* (ptr_6, ~mask_7, _3);
		 _12 = _3 & mask_7;
		 _5 = (int) _12;

		 and Convert
		 _1 = __atomic_fetch_and_4 (ptr_6, ~mask, _3);
		 _2 = (short int) _1;
		 _5 = _2 & mask;
		 to
		 _1 = __atomic_fetch_and_4 (ptr_6, ~mask, _3);
		 _8 = _1 & mask;
		 _5 = (short int) _8;
	      */
	      gimple_seq stmts = NULL;
	      match_op[1] = gimple_convert (&stmts,
					    TREE_TYPE (use_rhs),
					    match_op[1]);
	      var = gimple_build (&stmts, BIT_AND_EXPR,
				  TREE_TYPE (use_rhs), use_rhs, match_op[1]);
	      gsi = gsi_for_stmt (use_stmt);
	      gsi_remove (&gsi, true);
	      release_defs (use_stmt);
	      use_stmt = gimple_seq_last_stmt (stmts);
	      gsi = gsi_for_stmt (use_nop_stmt);
	      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
	      gimple_assign_set_rhs_with_ops (&gsi, CONVERT_EXPR, var);
	      update_stmt (use_nop_stmt);
	    }
	}
      else
	return false;

      if (!bit)
	{
	  if (ibit < 0)
	    gcc_unreachable ();
	  bit = build_int_cst (TREE_TYPE (lhs), ibit);
	}
    }
  else if (optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)))
	   == CODE_FOR_nothing)
    return false;

  tree use_lhs = gimple_assign_lhs (use_stmt);
  if (!use_lhs)
    return false;

  if (!bit)
    {
      if (TREE_CODE (mask) == INTEGER_CST)
	{
	  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
	    mask = const_unop (BIT_NOT_EXPR, TREE_TYPE (mask), mask);
	  mask = fold_convert (TREE_TYPE (lhs), mask);
	  int ibit = tree_log2 (mask);
	  if (ibit < 0)
	    return false;
	  bit = build_int_cst (TREE_TYPE (lhs), ibit);
	}
      else if (TREE_CODE (mask) == SSA_NAME)
	{
	  gimple *g = SSA_NAME_DEF_STMT (mask);
	  tree match_op;
	  if (gimple_nop_convert (mask, &match_op, NULL))
	    {
	      mask = match_op;
	      if (TREE_CODE (mask) != SSA_NAME)
		return false;
	      g = SSA_NAME_DEF_STMT (mask);
	    }
	  if (!is_gimple_assign (g))
	    return false;

	  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
	    {
	      if (gimple_assign_rhs_code (g) != BIT_NOT_EXPR)
		return false;
	      mask = gimple_assign_rhs1 (g);
	      if (TREE_CODE (mask) != SSA_NAME)
		return false;
	      g = SSA_NAME_DEF_STMT (mask);
	    }

	  if (!is_gimple_assign (g)
	      || gimple_assign_rhs_code (g) != LSHIFT_EXPR
	      || !integer_onep (gimple_assign_rhs1 (g)))
	    return false;
	  bit = gimple_assign_rhs2 (g);
	}
      else
	return false;

      tree cmp_mask;
      if (gimple_assign_rhs1 (use_stmt) == lhs)
	cmp_mask = gimple_assign_rhs2 (use_stmt);
      else
	cmp_mask = gimple_assign_rhs1 (use_stmt);

      tree match_op;
      if (gimple_nop_convert (cmp_mask, &match_op, NULL))
	cmp_mask = match_op;

      if (!operand_equal_p (cmp_mask, mask, 0))
	return false;
    }

  bool use_bool = true;
  bool has_debug_uses = false;
  imm_use_iterator iter;
  gimple *g;

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs))
    use_bool = false;
  FOR_EACH_IMM_USE_STMT (g, iter, use_lhs)
    {
      enum tree_code code = ERROR_MARK;
      tree op0 = NULL_TREE, op1 = NULL_TREE;
      if (is_gimple_debug (g))
	{
	  has_debug_uses = true;
	  continue;
	}
      else if (is_gimple_assign (g))
	switch (gimple_assign_rhs_code (g))
	  {
	  case COND_EXPR:
	    op1 = gimple_assign_rhs1 (g);
	    code = TREE_CODE (op1);
	    if (TREE_CODE_CLASS (code) != tcc_comparison)
	      break;
	    op0 = TREE_OPERAND (op1, 0);
	    op1 = TREE_OPERAND (op1, 1);
	    break;
	  case EQ_EXPR:
	  case NE_EXPR:
	    code = gimple_assign_rhs_code (g);
	    op0 = gimple_assign_rhs1 (g);
	    op1 = gimple_assign_rhs2 (g);
	    break;
	  default:
	    break;
	  }
      else if (gimple_code (g) == GIMPLE_COND)
	{
	  code = gimple_cond_code (g);
	  op0 = gimple_cond_lhs (g);
	  op1 = gimple_cond_rhs (g);
	}

      if ((code == EQ_EXPR || code == NE_EXPR)
	  && op0 == use_lhs
	  && integer_zerop (op1))
	{
	  use_operand_p use_p;
	  int n = 0;
	  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	    n++;
	  if (n == 1)
	    continue;
	}

      use_bool = false;
      break;
    }

  tree new_lhs = make_ssa_name (TREE_TYPE (lhs));
  tree flag = build_int_cst (TREE_TYPE (lhs), use_bool);
  if (has_model_arg)
    g = gimple_build_call_internal (fn, 5, gimple_call_arg (call, 0),
				    bit, flag, gimple_call_arg (call, 2),
				    gimple_call_fn (call));
  else
    g = gimple_build_call_internal (fn, 4, gimple_call_arg (call, 0),
				    bit, flag, gimple_call_fn (call));
  gimple_call_set_lhs (g, new_lhs);
  gimple_set_location (g, gimple_location (call));
  gimple_move_vops (g, call);
  bool throws = stmt_can_throw_internal (cfun, call);
  gimple_call_set_nothrow (as_a <gcall *> (g),
			   gimple_call_nothrow_p (as_a <gcall *> (call)));
  gimple_stmt_iterator gsi = *gsip;
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  edge e = NULL;
  if (throws)
    {
      maybe_clean_or_replace_eh_stmt (call, g);
      if (after || (use_bool && has_debug_uses))
	e = find_fallthru_edge (gsi_bb (gsi)->succs);
    }
  if (after)
    {
      /* The internal function returns the value of the specified bit
	 before the atomic operation.  If we are interested in the value
	 of the specified bit after the atomic operation (makes only sense
	 for xor, otherwise the bit content is compile time known),
	 we need to invert the bit.  */
      tree mask_convert = mask;
      gimple_seq stmts = NULL;
      if (!use_bool)
	mask_convert = gimple_convert (&stmts, TREE_TYPE (lhs), mask);
      new_lhs = gimple_build (&stmts, BIT_XOR_EXPR, TREE_TYPE (lhs), new_lhs,
			      use_bool ? build_int_cst (TREE_TYPE (lhs), 1)
				       : mask_convert);
      if (throws)
	{
	  gsi_insert_seq_on_edge_immediate (e, stmts);
	  gsi = gsi_for_stmt (gimple_seq_last (stmts));
	}
      else
	gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
    }
  if (use_bool && has_debug_uses)
    {
      tree temp = NULL_TREE;
      if (!throws || after || single_pred_p (e->dest))
	{
	  temp = build_debug_expr_decl (TREE_TYPE (lhs));
	  tree t = build2 (LSHIFT_EXPR, TREE_TYPE (lhs), new_lhs, bit);
	  g = gimple_build_debug_bind (temp, t, g);
	  if (throws && !after)
	    {
	      gsi = gsi_after_labels (e->dest);
	      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	    }
	  else
	    gsi_insert_after (&gsi, g, GSI_NEW_STMT);
	}
      FOR_EACH_IMM_USE_STMT (g, iter, use_lhs)
	if (is_gimple_debug (g))
	  {
	    use_operand_p use_p;
	    if (temp == NULL_TREE)
	      gimple_debug_bind_reset_value (g);
	    else
	      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		SET_USE (use_p, temp);
	    update_stmt (g);
	  }
    }
  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_lhs)
    = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs);
  replace_uses_by (use_lhs, new_lhs);
  gsi = gsi_for_stmt (use_stmt);
  gsi_remove (&gsi, true);
  release_defs (use_stmt);
  gsi_remove (gsip, true);
  release_ssa_name (lhs);
  return true;
}

/* Optimize
     _4 = __atomic_add_fetch_* (ptr_6, arg_2, _3);
     _5 = _4 == 0;
   to
     _4 = .ATOMIC_ADD_FETCH_CMP_0 (EQ_EXPR, ptr_6, arg_2, _3);
     _5 = _4;
   Similarly for __sync_add_and_fetch_* (without the ", _3" part
   in there).  */

static bool
optimize_atomic_op_fetch_cmp_0 (gimple_stmt_iterator *gsip,
				enum internal_fn fn, bool has_model_arg)
{
  gimple *call = gsi_stmt (*gsip);
  tree lhs = gimple_call_lhs (call);
  use_operand_p use_p;
  gimple *use_stmt;

  if (!flag_inline_atomics
      || optimize_debug
      || !gimple_call_builtin_p (call, BUILT_IN_NORMAL)
      || !lhs
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
      || !single_imm_use (lhs, &use_p, &use_stmt)
      || !gimple_vdef (call))
    return false;

  optab optab;
  switch (fn)
    {
    case IFN_ATOMIC_ADD_FETCH_CMP_0:
      optab = atomic_add_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_SUB_FETCH_CMP_0:
      optab = atomic_sub_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_AND_FETCH_CMP_0:
      optab = atomic_and_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_OR_FETCH_CMP_0:
      optab = atomic_or_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_XOR_FETCH_CMP_0:
      optab = atomic_xor_fetch_cmp_0_optab;
      break;
    default:
      return false;
    }

  if (optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)))
      == CODE_FOR_nothing)
    return false;

  tree use_lhs = lhs;
  if (gimple_assign_cast_p (use_stmt))
    {
      use_lhs = gimple_assign_lhs (use_stmt);
      if (!tree_nop_conversion_p (TREE_TYPE (use_lhs), TREE_TYPE (lhs))
	  || (!INTEGRAL_TYPE_P (TREE_TYPE (use_lhs))
	      && !POINTER_TYPE_P (TREE_TYPE (use_lhs)))
	  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs)
	  || !single_imm_use (use_lhs, &use_p, &use_stmt))
	return false;
    }
  enum tree_code code = ERROR_MARK;
  tree op0 = NULL_TREE, op1 = NULL_TREE;
  if (is_gimple_assign (use_stmt))
    switch (gimple_assign_rhs_code (use_stmt))
      {
      case COND_EXPR:
	op1 = gimple_assign_rhs1 (use_stmt);
	code = TREE_CODE (op1);
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    op0 = TREE_OPERAND (op1, 0);
	    op1 = TREE_OPERAND (op1, 1);
	  }
	break;
      default:
	code = gimple_assign_rhs_code (use_stmt);
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    op0 = gimple_assign_rhs1 (use_stmt);
	    op1 = gimple_assign_rhs2 (use_stmt);
	  }
	break;
      }
  else if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      code = gimple_cond_code (use_stmt);
      op0 = gimple_cond_lhs (use_stmt);
      op1 = gimple_cond_rhs (use_stmt);
    }

  switch (code)
    {
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (use_lhs))
	  || TREE_CODE (TREE_TYPE (use_lhs)) == BOOLEAN_TYPE
	  || TYPE_UNSIGNED (TREE_TYPE (use_lhs)))
	return false;
      /* FALLTHRU */
    case EQ_EXPR:
    case NE_EXPR:
      if (op0 == use_lhs && integer_zerop (op1))
	break;
      return false;
    default:
      return false;
    }

  int encoded;
  switch (code)
    {
    /* Use special encoding of the operation.  We want to also
       encode the mode in the first argument and for neither EQ_EXPR
       etc. nor EQ etc. we can rely it will fit into QImode.  */
    case EQ_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_EQ; break;
    case NE_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_NE; break;
    case LT_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_LT; break;
    case LE_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_LE; break;
    case GT_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_GT; break;
    case GE_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_GE; break;
    default: gcc_unreachable ();
    }

  tree new_lhs = make_ssa_name (boolean_type_node);
  gimple *g;
  tree flag = build_int_cst (TREE_TYPE (lhs), encoded);
  if (has_model_arg)
    g = gimple_build_call_internal (fn, 5, flag,
				    gimple_call_arg (call, 0),
				    gimple_call_arg (call, 1),
				    gimple_call_arg (call, 2),
				    gimple_call_fn (call));
  else
    g = gimple_build_call_internal (fn, 4, flag,
				    gimple_call_arg (call, 0),
				    gimple_call_arg (call, 1),
				    gimple_call_fn (call));
  gimple_call_set_lhs (g, new_lhs);
  gimple_set_location (g, gimple_location (call));
  gimple_move_vops (g, call);
  bool throws = stmt_can_throw_internal (cfun, call);
  gimple_call_set_nothrow (as_a <gcall *> (g),
			   gimple_call_nothrow_p (as_a <gcall *> (call)));
  gimple_stmt_iterator gsi = *gsip;
  gsi_insert_after (&gsi, g, GSI_SAME_STMT);
  if (throws)
    maybe_clean_or_replace_eh_stmt (call, g);
  if (is_gimple_assign (use_stmt))
    switch (gimple_assign_rhs_code (use_stmt))
      {
      case COND_EXPR:
	gimple_assign_set_rhs1 (use_stmt, new_lhs);
	break;
      default:
	gsi = gsi_for_stmt (use_stmt);
	if (tree ulhs = gimple_assign_lhs (use_stmt))
	  if (useless_type_conversion_p (TREE_TYPE (ulhs),
					 boolean_type_node))
	    {
	      gimple_assign_set_rhs_with_ops (&gsi, SSA_NAME, new_lhs);
	      break;
	    }
	gimple_assign_set_rhs_with_ops (&gsi, NOP_EXPR, new_lhs);
	break;
      }
  else if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      gcond *use_cond = as_a <gcond *> (use_stmt);
      gimple_cond_set_code (use_cond, NE_EXPR);
      gimple_cond_set_lhs (use_cond, new_lhs);
      gimple_cond_set_rhs (use_cond, boolean_false_node);
    }

  update_stmt (use_stmt);
  if (use_lhs != lhs)
    {
      gsi = gsi_for_stmt (SSA_NAME_DEF_STMT (use_lhs));
      gsi_remove (&gsi, true);
      release_ssa_name (use_lhs);
    }
  gsi_remove (gsip, true);
  release_ssa_name (lhs);
  return true;
}

/* A simple pass that attempts to fold all builtin functions.  This pass
   is run after we've propagated as many constants as we can.  */

namespace {

const pass_data pass_data_fold_builtins =
{
  GIMPLE_PASS, /* type */
  "fab", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_fold_builtins : public gimple_opt_pass
{
public:
  pass_fold_builtins (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_fold_builtins, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_fold_builtins (m_ctxt); }
  unsigned int execute (function *) final override;

}; // class pass_fold_builtins

unsigned int
pass_fold_builtins::execute (function *fun)
{
  bool cfg_changed = false;
  basic_block bb;
  unsigned int todoflags = 0;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator i;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); )
	{
	  gimple *stmt, *old_stmt;
	  tree callee;
	  enum built_in_function fcode;

	  stmt = gsi_stmt (i);

	  if (gimple_code (stmt) != GIMPLE_CALL)
	    {
	      gsi_next (&i);
	      continue;
	    }

	  callee = gimple_call_fndecl (stmt);
	  if (!callee
	      && gimple_call_internal_p (stmt, IFN_ASSUME))
	    {
	      gsi_remove (&i, true);
	      continue;
	    }
	  if (!callee || !fndecl_built_in_p (callee, BUILT_IN_NORMAL))
	    {
	      gsi_next (&i);
	      continue;
	    }

	  fcode = DECL_FUNCTION_CODE (callee);
	  if (fold_stmt (&i))
	    ;
	  else
	    {
	      tree result = NULL_TREE;
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

		case BUILT_IN_ATOMIC_ADD_FETCH_1:
		case BUILT_IN_ATOMIC_ADD_FETCH_2:
		case BUILT_IN_ATOMIC_ADD_FETCH_4:
		case BUILT_IN_ATOMIC_ADD_FETCH_8:
		case BUILT_IN_ATOMIC_ADD_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_ADD_FETCH_CMP_0,
						  true);
		  break;
		case BUILT_IN_SYNC_ADD_AND_FETCH_1:
		case BUILT_IN_SYNC_ADD_AND_FETCH_2:
		case BUILT_IN_SYNC_ADD_AND_FETCH_4:
		case BUILT_IN_SYNC_ADD_AND_FETCH_8:
		case BUILT_IN_SYNC_ADD_AND_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_ADD_FETCH_CMP_0,
						  false);
		  break;

		case BUILT_IN_ATOMIC_SUB_FETCH_1:
		case BUILT_IN_ATOMIC_SUB_FETCH_2:
		case BUILT_IN_ATOMIC_SUB_FETCH_4:
		case BUILT_IN_ATOMIC_SUB_FETCH_8:
		case BUILT_IN_ATOMIC_SUB_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_SUB_FETCH_CMP_0,
						  true);
		  break;
		case BUILT_IN_SYNC_SUB_AND_FETCH_1:
		case BUILT_IN_SYNC_SUB_AND_FETCH_2:
		case BUILT_IN_SYNC_SUB_AND_FETCH_4:
		case BUILT_IN_SYNC_SUB_AND_FETCH_8:
		case BUILT_IN_SYNC_SUB_AND_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_SUB_FETCH_CMP_0,
						  false);
		  break;

		case BUILT_IN_ATOMIC_FETCH_OR_1:
		case BUILT_IN_ATOMIC_FETCH_OR_2:
		case BUILT_IN_ATOMIC_FETCH_OR_4:
		case BUILT_IN_ATOMIC_FETCH_OR_8:
		case BUILT_IN_ATOMIC_FETCH_OR_16:
		  optimize_atomic_bit_test_and (&i,
						IFN_ATOMIC_BIT_TEST_AND_SET,
						true, false);
		  break;
		case BUILT_IN_SYNC_FETCH_AND_OR_1:
		case BUILT_IN_SYNC_FETCH_AND_OR_2:
		case BUILT_IN_SYNC_FETCH_AND_OR_4:
		case BUILT_IN_SYNC_FETCH_AND_OR_8:
		case BUILT_IN_SYNC_FETCH_AND_OR_16:
		  optimize_atomic_bit_test_and (&i,
						IFN_ATOMIC_BIT_TEST_AND_SET,
						false, false);
		  break;

		case BUILT_IN_ATOMIC_FETCH_XOR_1:
		case BUILT_IN_ATOMIC_FETCH_XOR_2:
		case BUILT_IN_ATOMIC_FETCH_XOR_4:
		case BUILT_IN_ATOMIC_FETCH_XOR_8:
		case BUILT_IN_ATOMIC_FETCH_XOR_16:
		  optimize_atomic_bit_test_and
			(&i, IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT, true, false);
		  break;
		case BUILT_IN_SYNC_FETCH_AND_XOR_1:
		case BUILT_IN_SYNC_FETCH_AND_XOR_2:
		case BUILT_IN_SYNC_FETCH_AND_XOR_4:
		case BUILT_IN_SYNC_FETCH_AND_XOR_8:
		case BUILT_IN_SYNC_FETCH_AND_XOR_16:
		  optimize_atomic_bit_test_and
			(&i, IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT, false, false);
		  break;

		case BUILT_IN_ATOMIC_XOR_FETCH_1:
		case BUILT_IN_ATOMIC_XOR_FETCH_2:
		case BUILT_IN_ATOMIC_XOR_FETCH_4:
		case BUILT_IN_ATOMIC_XOR_FETCH_8:
		case BUILT_IN_ATOMIC_XOR_FETCH_16:
		  if (optimize_atomic_bit_test_and
			(&i, IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT, true, true))
		    break;
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_XOR_FETCH_CMP_0,
						  true);
		  break;
		case BUILT_IN_SYNC_XOR_AND_FETCH_1:
		case BUILT_IN_SYNC_XOR_AND_FETCH_2:
		case BUILT_IN_SYNC_XOR_AND_FETCH_4:
		case BUILT_IN_SYNC_XOR_AND_FETCH_8:
		case BUILT_IN_SYNC_XOR_AND_FETCH_16:
		  if (optimize_atomic_bit_test_and
			(&i, IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT, false, true))
		    break;
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_XOR_FETCH_CMP_0,
						  false);
		  break;

		case BUILT_IN_ATOMIC_FETCH_AND_1:
		case BUILT_IN_ATOMIC_FETCH_AND_2:
		case BUILT_IN_ATOMIC_FETCH_AND_4:
		case BUILT_IN_ATOMIC_FETCH_AND_8:
		case BUILT_IN_ATOMIC_FETCH_AND_16:
		  optimize_atomic_bit_test_and (&i,
						IFN_ATOMIC_BIT_TEST_AND_RESET,
						true, false);
		  break;
		case BUILT_IN_SYNC_FETCH_AND_AND_1:
		case BUILT_IN_SYNC_FETCH_AND_AND_2:
		case BUILT_IN_SYNC_FETCH_AND_AND_4:
		case BUILT_IN_SYNC_FETCH_AND_AND_8:
		case BUILT_IN_SYNC_FETCH_AND_AND_16:
		  optimize_atomic_bit_test_and (&i,
						IFN_ATOMIC_BIT_TEST_AND_RESET,
						false, false);
		  break;

		case BUILT_IN_ATOMIC_AND_FETCH_1:
		case BUILT_IN_ATOMIC_AND_FETCH_2:
		case BUILT_IN_ATOMIC_AND_FETCH_4:
		case BUILT_IN_ATOMIC_AND_FETCH_8:
		case BUILT_IN_ATOMIC_AND_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_AND_FETCH_CMP_0,
						  true);
		  break;
		case BUILT_IN_SYNC_AND_AND_FETCH_1:
		case BUILT_IN_SYNC_AND_AND_FETCH_2:
		case BUILT_IN_SYNC_AND_AND_FETCH_4:
		case BUILT_IN_SYNC_AND_AND_FETCH_8:
		case BUILT_IN_SYNC_AND_AND_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_AND_FETCH_CMP_0,
						  false);
		  break;

		case BUILT_IN_ATOMIC_OR_FETCH_1:
		case BUILT_IN_ATOMIC_OR_FETCH_2:
		case BUILT_IN_ATOMIC_OR_FETCH_4:
		case BUILT_IN_ATOMIC_OR_FETCH_8:
		case BUILT_IN_ATOMIC_OR_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_OR_FETCH_CMP_0,
						  true);
		  break;
		case BUILT_IN_SYNC_OR_AND_FETCH_1:
		case BUILT_IN_SYNC_OR_AND_FETCH_2:
		case BUILT_IN_SYNC_OR_AND_FETCH_4:
		case BUILT_IN_SYNC_OR_AND_FETCH_8:
		case BUILT_IN_SYNC_OR_AND_FETCH_16:
		  optimize_atomic_op_fetch_cmp_0 (&i,
						  IFN_ATOMIC_OR_FETCH_CMP_0,
						  false);
		  break;

		case BUILT_IN_VA_START:
		case BUILT_IN_VA_END:
		case BUILT_IN_VA_COPY:
		  /* These shouldn't be folded before pass_stdarg.  */
		  result = optimize_stdarg_builtin (stmt);
		  break;

		default:;
		}

	      if (!result)
		{
		  gsi_next (&i);
		  continue;
		}

	      gimplify_and_update_call_from_tree (&i, result);
	    }

	  todoflags |= TODO_update_address_taken;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Simplified\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	    }

          old_stmt = stmt;
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
	      || !fndecl_built_in_p (callee, fcode))
	    gsi_next (&i);
	}
    }

  /* Delete unreachable blocks.  */
  if (cfg_changed)
    todoflags |= TODO_cleanup_cfg;

  return todoflags;
}

} // anon namespace

gimple_opt_pass *
make_pass_fold_builtins (gcc::context *ctxt)
{
  return new pass_fold_builtins (ctxt);
}

/* A simple pass that emits some warnings post IPA.  */

namespace {

const pass_data pass_data_post_ipa_warn =
{
  GIMPLE_PASS, /* type */
  "post_ipa_warn", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_post_ipa_warn : public gimple_opt_pass
{
public:
  pass_post_ipa_warn (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_post_ipa_warn, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_post_ipa_warn (m_ctxt); }
  bool gate (function *) final override { return warn_nonnull != 0; }
  unsigned int execute (function *) final override;

}; // class pass_fold_builtins

unsigned int
pass_post_ipa_warn::execute (function *fun)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (!is_gimple_call (stmt) || warning_suppressed_p (stmt, OPT_Wnonnull))
	    continue;

	  tree fntype = gimple_call_fntype (stmt);
	  bitmap nonnullargs = get_nonnull_args (fntype);
	  if (!nonnullargs)
	    continue;

	  tree fndecl = gimple_call_fndecl (stmt);
	  const bool closure = fndecl && DECL_LAMBDA_FUNCTION_P (fndecl);

	  for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
	    {
	      tree arg = gimple_call_arg (stmt, i);
	      if (TREE_CODE (TREE_TYPE (arg)) != POINTER_TYPE)
		continue;
	      if (!integer_zerop (arg))
		continue;
	      if (i == 0 && closure)
		/* Avoid warning for the first argument to lambda functions.  */
		continue;
	      if (!bitmap_empty_p (nonnullargs)
		  && !bitmap_bit_p (nonnullargs, i))
		continue;

	      /* In C++ non-static member functions argument 0 refers
		 to the implicit this pointer.  Use the same one-based
		 numbering for ordinary arguments.  */
	      unsigned argno = TREE_CODE (fntype) == METHOD_TYPE ? i : i + 1;
	      location_t loc = (EXPR_HAS_LOCATION (arg)
				? EXPR_LOCATION (arg)
				: gimple_location (stmt));
	      auto_diagnostic_group d;
	      if (argno == 0)
		{
		  if (warning_at (loc, OPT_Wnonnull,
				  "%qs pointer is null", "this")
		      && fndecl)
		    inform (DECL_SOURCE_LOCATION (fndecl),
			    "in a call to non-static member function %qD",
			    fndecl);
		  continue;
		}

	      if (!warning_at (loc, OPT_Wnonnull,
			       "argument %u null where non-null "
			       "expected", argno))
		continue;

	      tree fndecl = gimple_call_fndecl (stmt);
	      if (fndecl && DECL_IS_UNDECLARED_BUILTIN (fndecl))
		inform (loc, "in a call to built-in function %qD",
			fndecl);
	      else if (fndecl)
		inform (DECL_SOURCE_LOCATION (fndecl),
			"in a call to function %qD declared %qs",
			fndecl, "nonnull");
	    }
	  BITMAP_FREE (nonnullargs);
	}
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_post_ipa_warn (gcc::context *ctxt)
{
  return new pass_post_ipa_warn (ctxt);
}
