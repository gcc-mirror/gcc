/* Lower complex number operations to scalar operations.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.

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
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-ssa-propagate.h"
#include "tree-hasher.h"
#include "cfgloop.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "diagnostic-core.h"
#include "case-cfn-macros.h"
#include "builtins.h"
#include "optabs-tree.h"
#include "tree-ssa-dce.h"

/* For each complex ssa name, a lattice value.  We're interested in finding
   out whether a complex number is degenerate in some way, having only real
   or only complex parts.  */

enum
{
  UNINITIALIZED = 0,
  ONLY_REAL = 1,
  ONLY_IMAG = 2,
  VARYING = 3
};

/* The type complex_lattice_t holds combinations of the above
   constants.  */
typedef int complex_lattice_t;

#define PAIR(a, b)  ((a) << 2 | (b))

class complex_propagate : public ssa_propagation_engine
{
  enum ssa_prop_result visit_stmt (gimple *, edge *, tree *) final override;
  enum ssa_prop_result visit_phi (gphi *) final override;
};

static vec<complex_lattice_t> complex_lattice_values;

/* For each complex variable, a pair of variables for the components exists in
   the hashtable.  */
static int_tree_htab_type *complex_variable_components;

/* For each complex SSA_NAME, a pair of ssa names for the components.  */
static vec<tree> complex_ssa_name_components;

/* Vector of PHI triplets (original complex PHI and corresponding real and
   imag PHIs if real and/or imag PHIs contain temporarily
   non-SSA_NAME/non-invariant args that need to be replaced by SSA_NAMEs.  */
static vec<gphi *> phis_to_revisit;

/* BBs that need EH cleanup.  */
static bitmap need_eh_cleanup;

/* SSA defs we should try to DCE.  */
static bitmap dce_worklist;

/* Lookup UID in the complex_variable_components hashtable and return the
   associated tree.  */
static tree
cvc_lookup (unsigned int uid)
{
  struct int_tree_map in;
  in.uid = uid;
  return complex_variable_components->find_with_hash (in, uid).to;
}

/* Insert the pair UID, TO into the complex_variable_components hashtable.  */

static void
cvc_insert (unsigned int uid, tree to)
{
  int_tree_map h;
  int_tree_map *loc;

  h.uid = uid;
  loc = complex_variable_components->find_slot_with_hash (h, uid, INSERT);
  loc->uid = uid;
  loc->to = to;
}

/* Return true if T is not a zero constant.  In the case of real values,
   we're only interested in +0.0.  */

static int
some_nonzerop (tree t)
{
  int zerop = false;

  /* Operations with real or imaginary part of a complex number zero
     cannot be treated the same as operations with a real or imaginary
     operand if we care about the signs of zeros in the result.  */
  if (TREE_CODE (t) == REAL_CST && !flag_signed_zeros)
    zerop = real_identical (&TREE_REAL_CST (t), &dconst0);
  else if (TREE_CODE (t) == FIXED_CST)
    zerop = fixed_zerop (t);
  else if (TREE_CODE (t) == INTEGER_CST)
    zerop = integer_zerop (t);

  return !zerop;
}


/* Compute a lattice value from the components of a complex type REAL
   and IMAG.  */

static complex_lattice_t
find_lattice_value_parts (tree real, tree imag)
{
  int r, i;
  complex_lattice_t ret;

  r = some_nonzerop (real);
  i = some_nonzerop (imag);
  ret = r * ONLY_REAL + i * ONLY_IMAG;

  /* ??? On occasion we could do better than mapping 0+0i to real, but we
     certainly don't want to leave it UNINITIALIZED, which eventually gets
     mapped to VARYING.  */
  if (ret == UNINITIALIZED)
    ret = ONLY_REAL;

  return ret;
}


/* Compute a lattice value from gimple_val T.  */

static complex_lattice_t
find_lattice_value (tree t)
{
  tree real, imag;

  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      return complex_lattice_values[SSA_NAME_VERSION (t)];

    case COMPLEX_CST:
      real = TREE_REALPART (t);
      imag = TREE_IMAGPART (t);
      break;

    default:
      gcc_unreachable ();
    }

  return find_lattice_value_parts (real, imag);
}

/* Determine if LHS is something for which we're interested in seeing
   simulation results.  */

static bool
is_complex_reg (tree lhs)
{
  return TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE && is_gimple_reg (lhs);
}

/* Mark the incoming parameters to the function as VARYING.  */

static void
init_parameter_lattice_values (void)
{
  tree parm, ssa_name;

  for (parm = DECL_ARGUMENTS (cfun->decl); parm ; parm = DECL_CHAIN (parm))
    if (is_complex_reg (parm)
	&& (ssa_name = ssa_default_def (cfun, parm)) != NULL_TREE)
      complex_lattice_values[SSA_NAME_VERSION (ssa_name)] = VARYING;
}

/* Initialize simulation state for each statement.  Return false if we
   found no statements we want to simulate, and thus there's nothing
   for the entire pass to do.  */

static bool
init_dont_simulate_again (void)
{
  basic_block bb;
  bool saw_a_complex_op = false;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  prop_set_simulate_again (phi,
				   is_complex_reg (gimple_phi_result (phi)));
	}

      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt;
	  tree op0, op1;
	  bool sim_again_p;

	  stmt = gsi_stmt (gsi);
	  op0 = op1 = NULL_TREE;

	  /* Most control-altering statements must be initially
	     simulated, else we won't cover the entire cfg.  */
	  sim_again_p = stmt_ends_bb_p (stmt);

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_CALL:
	      if (gimple_call_lhs (stmt))
		{
		  sim_again_p = is_complex_reg (gimple_call_lhs (stmt));
		  switch (gimple_call_combined_fn (stmt))
		    {
		    CASE_CFN_CABS:
		      /* Expand cabs only if unsafe math and optimizing. */
		      if (optimize && flag_unsafe_math_optimizations)
			saw_a_complex_op = true;
		      break;
		    default:;
		    }
		}
	      break;

	    case GIMPLE_ASSIGN:
	      sim_again_p = is_complex_reg (gimple_assign_lhs (stmt));
	      if (gimple_assign_rhs_code (stmt) == REALPART_EXPR
		  || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR)
		op0 = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
	      else
		op0 = gimple_assign_rhs1 (stmt);
	      if (gimple_num_ops (stmt) > 2)
		op1 = gimple_assign_rhs2 (stmt);
	      break;

	    case GIMPLE_COND:
	      op0 = gimple_cond_lhs (stmt);
	      op1 = gimple_cond_rhs (stmt);
	      break;

	    default:
	      break;
	    }

	  if (op0 || op1)
	    switch (gimple_expr_code (stmt))
	      {
	      case EQ_EXPR:
	      case NE_EXPR:
	      case PLUS_EXPR:
	      case MINUS_EXPR:
	      case MULT_EXPR:
	      case TRUNC_DIV_EXPR:
	      case CEIL_DIV_EXPR:
	      case FLOOR_DIV_EXPR:
	      case ROUND_DIV_EXPR:
	      case RDIV_EXPR:
		if (TREE_CODE (TREE_TYPE (op0)) == COMPLEX_TYPE
		    || TREE_CODE (TREE_TYPE (op1)) == COMPLEX_TYPE)
		  saw_a_complex_op = true;
		break;

	      case NEGATE_EXPR:
	      case CONJ_EXPR:
	      case PAREN_EXPR:
		if (TREE_CODE (TREE_TYPE (op0)) == COMPLEX_TYPE)
		  saw_a_complex_op = true;
		break;

	      case REALPART_EXPR:
	      case IMAGPART_EXPR:
		/* The total store transformation performed during
		  gimplification creates such uninitialized loads
		  and we need to lower the statement to be able
		  to fix things up.  */
		if (TREE_CODE (op0) == SSA_NAME
		    && ssa_undefined_value_p (op0))
		  saw_a_complex_op = true;
		break;

	      default:
		/* When expand_complex_move would trigger make sure we
		   perform lowering even when there is no actual complex
		   operation.  This helps consistency and vectorization.  */
		if (TREE_CODE (TREE_TYPE (gimple_op (stmt, 0))) == COMPLEX_TYPE)
		  saw_a_complex_op = true;
		break;
	      }

	  prop_set_simulate_again (stmt, sim_again_p);
	}
    }

  return saw_a_complex_op;
}


/* Evaluate statement STMT against the complex lattice defined above.  */

enum ssa_prop_result
complex_propagate::visit_stmt (gimple *stmt, edge *taken_edge_p ATTRIBUTE_UNUSED,
			       tree *result_p)
{
  complex_lattice_t new_l, old_l, op1_l, op2_l;
  unsigned int ver;
  tree lhs;

  lhs = gimple_get_lhs (stmt);
  /* Skip anything but GIMPLE_ASSIGN and GIMPLE_CALL with a lhs.  */
  if (!lhs || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return SSA_PROP_VARYING;

  /* These conditions should be satisfied due to the initial filter
     set up in init_dont_simulate_again.  */
  gcc_assert (TREE_CODE (lhs) == SSA_NAME);
  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE);

  *result_p = lhs;
  ver = SSA_NAME_VERSION (lhs);
  old_l = complex_lattice_values[ver];

  switch (gimple_expr_code (stmt))
    {
    case SSA_NAME:
    case COMPLEX_CST:
      new_l = find_lattice_value (gimple_assign_rhs1 (stmt));
      break;

    case COMPLEX_EXPR:
      new_l = find_lattice_value_parts (gimple_assign_rhs1 (stmt),
				        gimple_assign_rhs2 (stmt));
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      op1_l = find_lattice_value (gimple_assign_rhs1 (stmt));
      op2_l = find_lattice_value (gimple_assign_rhs2 (stmt));

      /* We've set up the lattice values such that IOR neatly
	 models addition.  */
      new_l = op1_l | op2_l;
      break;

    case MULT_EXPR:
    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      op1_l = find_lattice_value (gimple_assign_rhs1 (stmt));
      op2_l = find_lattice_value (gimple_assign_rhs2 (stmt));

      /* Obviously, if either varies, so does the result.  */
      if (op1_l == VARYING || op2_l == VARYING)
	new_l = VARYING;
      /* Don't prematurely promote variables if we've not yet seen
	 their inputs.  */
      else if (op1_l == UNINITIALIZED)
	new_l = op2_l;
      else if (op2_l == UNINITIALIZED)
	new_l = op1_l;
      else
	{
	  /* At this point both numbers have only one component. If the
	     numbers are of opposite kind, the result is imaginary,
	     otherwise the result is real. The add/subtract translates
	     the real/imag from/to 0/1; the ^ performs the comparison.  */
	  new_l = ((op1_l - ONLY_REAL) ^ (op2_l - ONLY_REAL)) + ONLY_REAL;

	  /* Don't allow the lattice value to flip-flop indefinitely.  */
	  new_l |= old_l;
	}
      break;

    case NEGATE_EXPR:
    case PAREN_EXPR:
    case CONJ_EXPR:
      new_l = find_lattice_value (gimple_assign_rhs1 (stmt));
      break;

    default:
      new_l = VARYING;
      break;
    }

  /* If nothing changed this round, let the propagator know.  */
  if (new_l == old_l)
    return SSA_PROP_NOT_INTERESTING;

  complex_lattice_values[ver] = new_l;
  return new_l == VARYING ? SSA_PROP_VARYING : SSA_PROP_INTERESTING;
}

/* Evaluate a PHI node against the complex lattice defined above.  */

enum ssa_prop_result
complex_propagate::visit_phi (gphi *phi)
{
  complex_lattice_t new_l, old_l;
  unsigned int ver;
  tree lhs;
  int i;

  lhs = gimple_phi_result (phi);

  /* This condition should be satisfied due to the initial filter
     set up in init_dont_simulate_again.  */
  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE);

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return SSA_PROP_VARYING;

  /* We've set up the lattice values such that IOR neatly models PHI meet.  */
  new_l = UNINITIALIZED;
  for (i = gimple_phi_num_args (phi) - 1; i >= 0; --i)
    new_l |= find_lattice_value (gimple_phi_arg_def (phi, i));

  ver = SSA_NAME_VERSION (lhs);
  old_l = complex_lattice_values[ver];

  if (new_l == old_l)
    return SSA_PROP_NOT_INTERESTING;

  complex_lattice_values[ver] = new_l;
  return new_l == VARYING ? SSA_PROP_VARYING : SSA_PROP_INTERESTING;
}

/* Create one backing variable for a complex component of ORIG.  */

static tree
create_one_component_var (tree type, tree orig, const char *prefix,
			  const char *suffix, enum tree_code code)
{
  tree r = create_tmp_var (type, prefix);

  DECL_SOURCE_LOCATION (r) = DECL_SOURCE_LOCATION (orig);
  DECL_ARTIFICIAL (r) = 1;

  if (DECL_NAME (orig) && !DECL_IGNORED_P (orig))
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (orig));
      name = ACONCAT ((name, suffix, NULL));
      DECL_NAME (r) = get_identifier (name);

      SET_DECL_DEBUG_EXPR (r, build1 (code, type, orig));
      DECL_HAS_DEBUG_EXPR_P (r) = 1;
      DECL_IGNORED_P (r) = 0;
      copy_warning (r, orig);
    }
  else
    {
      DECL_IGNORED_P (r) = 1;
      suppress_warning (r);
    }

  return r;
}

/* Retrieve a value for a complex component of VAR.  */

static tree
get_component_var (tree var, bool imag_p)
{
  size_t decl_index = DECL_UID (var) * 2 + imag_p;
  tree ret = cvc_lookup (decl_index);

  if (ret == NULL)
    {
      ret = create_one_component_var (TREE_TYPE (TREE_TYPE (var)), var,
				      imag_p ? "CI" : "CR",
				      imag_p ? "$imag" : "$real",
				      imag_p ? IMAGPART_EXPR : REALPART_EXPR);
      cvc_insert (decl_index, ret);
    }

  return ret;
}

/* Retrieve a value for a complex component of SSA_NAME.  */

static tree
get_component_ssa_name (tree ssa_name, bool imag_p)
{
  complex_lattice_t lattice = find_lattice_value (ssa_name);
  size_t ssa_name_index;
  tree ret;

  if (lattice == (imag_p ? ONLY_REAL : ONLY_IMAG))
    {
      tree inner_type = TREE_TYPE (TREE_TYPE (ssa_name));
      if (SCALAR_FLOAT_TYPE_P (inner_type))
	return build_real (inner_type, dconst0);
      else
	return build_int_cst (inner_type, 0);
    }

  ssa_name_index = SSA_NAME_VERSION (ssa_name) * 2 + imag_p;
  ret = complex_ssa_name_components[ssa_name_index];
  if (ret == NULL)
    {
      if (SSA_NAME_VAR (ssa_name))
	ret = get_component_var (SSA_NAME_VAR (ssa_name), imag_p);
      else
	ret = TREE_TYPE (TREE_TYPE (ssa_name));
      ret = make_ssa_name (ret);

      /* Copy some properties from the original.  In particular, whether it
	 is used in an abnormal phi, and whether it's uninitialized.  */
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ret)
	= SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name);
      if (SSA_NAME_IS_DEFAULT_DEF (ssa_name)
	  && VAR_P (SSA_NAME_VAR (ssa_name)))
	{
	  SSA_NAME_DEF_STMT (ret) = SSA_NAME_DEF_STMT (ssa_name);
	  set_ssa_default_def (cfun, SSA_NAME_VAR (ret), ret);
	}

      complex_ssa_name_components[ssa_name_index] = ret;
    }

  return ret;
}

/* Set a value for a complex component of SSA_NAME, return a
   gimple_seq of stuff that needs doing.  */

static gimple_seq
set_component_ssa_name (tree ssa_name, bool imag_p, tree value)
{
  complex_lattice_t lattice = find_lattice_value (ssa_name);
  size_t ssa_name_index;
  tree comp;
  gimple *last;
  gimple_seq list;

  /* We know the value must be zero, else there's a bug in our lattice
     analysis.  But the value may well be a variable known to contain
     zero.  We should be safe ignoring it.  */
  if (lattice == (imag_p ? ONLY_REAL : ONLY_IMAG))
    return NULL;

  /* If we've already assigned an SSA_NAME to this component, then this
     means that our walk of the basic blocks found a use before the set.
     This is fine.  Now we should create an initialization for the value
     we created earlier.  */
  ssa_name_index = SSA_NAME_VERSION (ssa_name) * 2 + imag_p;
  comp = complex_ssa_name_components[ssa_name_index];
  if (comp)
    ;

  /* If we've nothing assigned, and the value we're given is already stable,
     then install that as the value for this SSA_NAME.  This preemptively
     copy-propagates the value, which avoids unnecessary memory allocation.  */
  else if (is_gimple_min_invariant (value)
	   && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name))
    {
      complex_ssa_name_components[ssa_name_index] = value;
      return NULL;
    }
  else if (TREE_CODE (value) == SSA_NAME
	   && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name))
    {
      /* Replace an anonymous base value with the variable from cvc_lookup.
	 This should result in better debug info.  */
      if (!SSA_NAME_IS_DEFAULT_DEF (value)
	  && SSA_NAME_VAR (ssa_name)
	  && (!SSA_NAME_VAR (value) || DECL_IGNORED_P (SSA_NAME_VAR (value)))
	  && !DECL_IGNORED_P (SSA_NAME_VAR (ssa_name)))
	{
	  comp = get_component_var (SSA_NAME_VAR (ssa_name), imag_p);
	  replace_ssa_name_symbol (value, comp);
	}

      complex_ssa_name_components[ssa_name_index] = value;
      return NULL;
    }

  /* Finally, we need to stabilize the result by installing the value into
     a new ssa name.  */
  else
    comp = get_component_ssa_name (ssa_name, imag_p);

  /* Do all the work to assign VALUE to COMP.  */
  list = NULL;
  value = force_gimple_operand (value, &list, false, NULL);
  last =  gimple_build_assign (comp, value);
  gimple_seq_add_stmt (&list, last);
  gcc_assert (SSA_NAME_DEF_STMT (comp) == last);

  return list;
}

/* Extract the real or imaginary part of a complex variable or constant.
   Make sure that it's a proper gimple_val and gimplify it if not.
   Emit any new code before gsi.  */

static tree
extract_component (gimple_stmt_iterator *gsi, tree t, bool imagpart_p,
		   bool gimple_p, bool phiarg_p = false)
{
  switch (TREE_CODE (t))
    {
    case COMPLEX_CST:
      return imagpart_p ? TREE_IMAGPART (t) : TREE_REALPART (t);

    case COMPLEX_EXPR:
      gcc_unreachable ();

    case BIT_FIELD_REF:
      {
	tree inner_type = TREE_TYPE (TREE_TYPE (t));
	t = unshare_expr (t);
	TREE_TYPE (t) = inner_type;
	TREE_OPERAND (t, 1) = TYPE_SIZE (inner_type);
	if (imagpart_p)
	  TREE_OPERAND (t, 2) = size_binop (PLUS_EXPR, TREE_OPERAND (t, 2),
					    TYPE_SIZE (inner_type));
	if (gimple_p)
	  t = force_gimple_operand_gsi (gsi, t, true, NULL, true,
					GSI_SAME_STMT);
	return t;
      }

    case VAR_DECL:
    case RESULT_DECL:
    case PARM_DECL:
    case COMPONENT_REF:
    case ARRAY_REF:
    case VIEW_CONVERT_EXPR:
    case MEM_REF:
      {
	tree inner_type = TREE_TYPE (TREE_TYPE (t));

	t = build1 ((imagpart_p ? IMAGPART_EXPR : REALPART_EXPR),
		    inner_type, unshare_expr (t));

	if (gimple_p)
	  t = force_gimple_operand_gsi (gsi, t, true, NULL, true,
                                        GSI_SAME_STMT);

	return t;
      }

    case SSA_NAME:
      t = get_component_ssa_name (t, imagpart_p);
      if (TREE_CODE (t) == SSA_NAME && SSA_NAME_DEF_STMT (t) == NULL)
	gcc_assert (phiarg_p);
      return t;

    default:
      gcc_unreachable ();
    }
}

/* Update the complex components of the ssa name on the lhs of STMT.  */

static void
update_complex_components (gimple_stmt_iterator *gsi, gimple *stmt, tree r,
			   tree i)
{
  tree lhs;
  gimple_seq list;

  lhs = gimple_get_lhs (stmt);

  list = set_component_ssa_name (lhs, false, r);
  if (list)
    gsi_insert_seq_after (gsi, list, GSI_CONTINUE_LINKING);

  list = set_component_ssa_name (lhs, true, i);
  if (list)
    gsi_insert_seq_after (gsi, list, GSI_CONTINUE_LINKING);
}

static void
update_complex_components_on_edge (edge e, tree lhs, tree r, tree i)
{
  gimple_seq list;

  list = set_component_ssa_name (lhs, false, r);
  if (list)
    gsi_insert_seq_on_edge (e, list);

  list = set_component_ssa_name (lhs, true, i);
  if (list)
    gsi_insert_seq_on_edge (e, list);
}


/* Update an assignment to a complex variable in place.  */

static void
update_complex_assignment (gimple_stmt_iterator *gsi, tree r, tree i)
{
  gimple *old_stmt = gsi_stmt (*gsi);
  gimple_assign_set_rhs_with_ops (gsi, COMPLEX_EXPR, r, i);
  gimple *stmt = gsi_stmt (*gsi);
  update_stmt (stmt);
  if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
    bitmap_set_bit (need_eh_cleanup, gimple_bb (stmt)->index);
  if (optimize)
    bitmap_set_bit (dce_worklist, SSA_NAME_VERSION (gimple_assign_lhs (stmt)));

  update_complex_components (gsi, gsi_stmt (*gsi), r, i);
}


/* Generate code at the entry point of the function to initialize the
   component variables for a complex parameter.  */

static void
update_parameter_components (void)
{
  edge entry_edge = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  tree parm;

  for (parm = DECL_ARGUMENTS (cfun->decl); parm ; parm = DECL_CHAIN (parm))
    {
      tree type = TREE_TYPE (parm);
      tree ssa_name, r, i;

      if (TREE_CODE (type) != COMPLEX_TYPE || !is_gimple_reg (parm))
	continue;

      type = TREE_TYPE (type);
      ssa_name = ssa_default_def (cfun, parm);
      if (!ssa_name)
	continue;

      r = build1 (REALPART_EXPR, type, ssa_name);
      i = build1 (IMAGPART_EXPR, type, ssa_name);
      update_complex_components_on_edge (entry_edge, ssa_name, r, i);
    }
}

/* Generate code to set the component variables of a complex variable
   to match the PHI statements in block BB.  */

static void
update_phi_components (basic_block bb)
{
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      if (is_complex_reg (gimple_phi_result (phi)))
	{
	  gphi *p[2] = { NULL, NULL };
	  unsigned int i, j, n;
	  bool revisit_phi = false;

	  for (j = 0; j < 2; j++)
	    {
	      tree l = get_component_ssa_name (gimple_phi_result (phi), j > 0);
	      if (TREE_CODE (l) == SSA_NAME)
		p[j] = create_phi_node (l, bb);
	    }

	  for (i = 0, n = gimple_phi_num_args (phi); i < n; ++i)
	    {
	      tree comp, arg = gimple_phi_arg_def (phi, i);
	      for (j = 0; j < 2; j++)
		if (p[j])
		  {
		    comp = extract_component (NULL, arg, j > 0, false, true);
		    if (TREE_CODE (comp) == SSA_NAME
			&& SSA_NAME_DEF_STMT (comp) == NULL)
		      {
			/* For the benefit of any gimple simplification during
			   this pass that might walk SSA_NAME def stmts,
			   don't add SSA_NAMEs without definitions into the
			   PHI arguments, but put a decl in there instead
			   temporarily, and revisit this PHI later on.  */
			if (SSA_NAME_VAR (comp))
			  comp = SSA_NAME_VAR (comp);
			else
			  comp = create_tmp_reg (TREE_TYPE (comp),
						 get_name (comp));
			revisit_phi = true;
		      }
		    SET_PHI_ARG_DEF (p[j], i, comp);
		  }
	    }

	  if (revisit_phi)
	    {
	      phis_to_revisit.safe_push (phi);
	      phis_to_revisit.safe_push (p[0]);
	      phis_to_revisit.safe_push (p[1]);
	    }
	}
    }
}

/* Expand a complex move to scalars.  */

static void
expand_complex_move (gimple_stmt_iterator *gsi, tree type)
{
  tree inner_type = TREE_TYPE (type);
  tree r, i, lhs, rhs;
  gimple *stmt = gsi_stmt (*gsi);

  if (is_gimple_assign (stmt))
    {
      lhs = gimple_assign_lhs (stmt);
      if (gimple_num_ops (stmt) == 2)
	rhs = gimple_assign_rhs1 (stmt);
      else
	rhs = NULL_TREE;
    }
  else if (is_gimple_call (stmt))
    {
      lhs = gimple_call_lhs (stmt);
      rhs = NULL_TREE;
    }
  else
    gcc_unreachable ();

  if (TREE_CODE (lhs) == SSA_NAME)
    {
      if (is_ctrl_altering_stmt (stmt))
	{
	  edge e;

	  /* The value is not assigned on the exception edges, so we need not
	     concern ourselves there.  We do need to update on the fallthru
	     edge.  Find it.  */
	  e = find_fallthru_edge (gsi_bb (*gsi)->succs);
	  if (!e)
	    gcc_unreachable ();

	  r = build1 (REALPART_EXPR, inner_type, lhs);
	  i = build1 (IMAGPART_EXPR, inner_type, lhs);
	  update_complex_components_on_edge (e, lhs, r, i);
	}
      else if (is_gimple_call (stmt)
	       || gimple_has_side_effects (stmt))
	{
	  r = build1 (REALPART_EXPR, inner_type, lhs);
	  i = build1 (IMAGPART_EXPR, inner_type, lhs);
	  update_complex_components (gsi, stmt, r, i);
	}
      else
	{
	  if (gimple_assign_rhs_code (stmt) != COMPLEX_EXPR)
	    {
	      r = extract_component (gsi, rhs, 0, true);
	      i = extract_component (gsi, rhs, 1, true);
	    }
	  else
	    {
	      r = gimple_assign_rhs1 (stmt);
	      i = gimple_assign_rhs2 (stmt);
	    }
	  update_complex_assignment (gsi, r, i);
	}
    }
  else if (rhs
	   && (TREE_CODE (rhs) == SSA_NAME || TREE_CODE (rhs) == COMPLEX_CST)
	   && !TREE_SIDE_EFFECTS (lhs))
    {
      tree x;
      gimple *t;
      location_t loc;

      loc = gimple_location (stmt);
      r = extract_component (gsi, rhs, 0, false);
      i = extract_component (gsi, rhs, 1, false);

      x = build1 (REALPART_EXPR, inner_type, unshare_expr (lhs));
      t = gimple_build_assign (x, r);
      gimple_set_location (t, loc);
      gsi_insert_before (gsi, t, GSI_SAME_STMT);

      if (stmt == gsi_stmt (*gsi))
	{
	  x = build1 (IMAGPART_EXPR, inner_type, unshare_expr (lhs));
	  gimple_assign_set_lhs (stmt, x);
	  gimple_assign_set_rhs1 (stmt, i);
	}
      else
	{
	  x = build1 (IMAGPART_EXPR, inner_type, unshare_expr (lhs));
	  t = gimple_build_assign (x, i);
	  gimple_set_location (t, loc);
	  gsi_insert_before (gsi, t, GSI_SAME_STMT);

	  stmt = gsi_stmt (*gsi);
	  gcc_assert (gimple_code (stmt) == GIMPLE_RETURN);
	  gimple_return_set_retval (as_a <greturn *> (stmt), lhs);
	}

      update_stmt (stmt);
    }
}

/* Expand complex addition to scalars:
	a + b = (ar + br) + i(ai + bi)
	a - b = (ar - br) + i(ai + bi)
*/

static void
expand_complex_addition (gimple_stmt_iterator *gsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code,
			 complex_lattice_t al, complex_lattice_t bl)
{
  tree rr, ri;
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  switch (PAIR (al, bl))
    {
    case PAIR (ONLY_REAL, ONLY_REAL):
      rr = gimple_build (&stmts, loc, code, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (ONLY_REAL, ONLY_IMAG):
      rr = ar;
      if (code == MINUS_EXPR)
	ri = gimple_build (&stmts, loc, MINUS_EXPR, inner_type, ai, bi);
      else
	ri = bi;
      break;

    case PAIR (ONLY_IMAG, ONLY_REAL):
      if (code == MINUS_EXPR)
	rr = gimple_build (&stmts, loc, MINUS_EXPR, inner_type, ar, br);
      else
	rr = br;
      ri = ai;
      break;

    case PAIR (ONLY_IMAG, ONLY_IMAG):
      rr = ar;
      ri = gimple_build (&stmts, loc, code, inner_type, ai, bi);
      break;

    case PAIR (VARYING, ONLY_REAL):
      rr = gimple_build (&stmts, loc, code, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (VARYING, ONLY_IMAG):
      rr = ar;
      ri = gimple_build (&stmts, loc, code, inner_type, ai, bi);
      break;

    case PAIR (ONLY_REAL, VARYING):
      if (code == MINUS_EXPR)
	goto general;
      rr = gimple_build (&stmts, loc, code, inner_type, ar, br);
      ri = bi;
      break;

    case PAIR (ONLY_IMAG, VARYING):
      if (code == MINUS_EXPR)
	goto general;
      rr = br;
      ri = gimple_build (&stmts, loc, code, inner_type, ai, bi);
      break;

    case PAIR (VARYING, VARYING):
    general:
      rr = gimple_build (&stmts, loc, code, inner_type, ar, br);
      /* (a+ai) + (b+bi) -> (a+b)+(a+b)i
	  small optimization to remove one new statement. */
      if (operand_equal_p (ar, ai) && operand_equal_p (br, bi))
	ri = rr;
      else
	ri = gimple_build (&stmts, loc, code, inner_type, ai, bi);
      break;

    default:
      gcc_unreachable ();
    }

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, rr, ri);
}

/* Expand a complex multiplication or division to a libcall to the c99
   compliant routines.  TYPE is the complex type of the operation.
   If INPLACE_P replace the statement at GSI with
   the libcall and return NULL_TREE.  Else insert the call, assign its
   result to an output variable and return that variable.  If INPLACE_P
   is true then the statement being replaced should be an assignment
   statement.  */

static tree
expand_complex_libcall (gimple_stmt_iterator *gsi, tree type, tree ar, tree ai,
			tree br, tree bi, enum tree_code code, bool inplace_p)
{
  machine_mode mode;
  enum built_in_function bcode;
  tree fn, lhs;
  gcall *stmt;

  mode = TYPE_MODE (type);
  gcc_assert (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT);

  if (code == MULT_EXPR)
    bcode = ((enum built_in_function)
	     (BUILT_IN_COMPLEX_MUL_MIN + mode - MIN_MODE_COMPLEX_FLOAT));
  else if (code == RDIV_EXPR)
    bcode = ((enum built_in_function)
	     (BUILT_IN_COMPLEX_DIV_MIN + mode - MIN_MODE_COMPLEX_FLOAT));
  else
    gcc_unreachable ();
  fn = builtin_decl_explicit (bcode);
  stmt = gimple_build_call (fn, 4, ar, ai, br, bi);

  if (inplace_p)
    {
      gimple *old_stmt = gsi_stmt (*gsi);
      gimple_call_set_nothrow (stmt, !stmt_could_throw_p (cfun, old_stmt));
      lhs = gimple_assign_lhs (old_stmt);
      gimple_call_set_lhs (stmt, lhs);
      gsi_replace (gsi, stmt, true);

      type = TREE_TYPE (type);
      if (stmt_can_throw_internal (cfun, stmt))
	{
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
	      if (!(e->flags & EDGE_EH))
		break;
	  basic_block bb = split_edge (e);
	  gimple_stmt_iterator gsi2 = gsi_start_bb (bb);
	  update_complex_components (&gsi2, stmt,
				     build1 (REALPART_EXPR, type, lhs),
				     build1 (IMAGPART_EXPR, type, lhs));
	  return NULL_TREE;
	}
      else
	update_complex_components (gsi, stmt,
				   build1 (REALPART_EXPR, type, lhs),
				   build1 (IMAGPART_EXPR, type, lhs));
      SSA_NAME_DEF_STMT (lhs) = stmt;
      return NULL_TREE;
    }

  gimple_call_set_nothrow (stmt, true);
  lhs = make_ssa_name (type);
  gimple_call_set_lhs (stmt, lhs);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

  return lhs;
}

/* Perform a complex multiplication on two complex constants A, B represented
   by AR, AI, BR, BI of type TYPE.
   The operation we want is: a * b = (ar*br - ai*bi) + i(ar*bi + br*ai).
   Insert the GIMPLE statements into GSI.  Store the real and imaginary
   components of the result into RR and RI.  */

static void
expand_complex_multiplication_components (gimple_seq *stmts, location_t loc,
					  tree type, tree ar, tree ai,
					  tree br, tree bi,
					  tree *rr, tree *ri)
{
  tree t1, t2, t3, t4;

  t1 = gimple_build (stmts, loc, MULT_EXPR, type, ar, br);
  t2 = gimple_build (stmts, loc, MULT_EXPR, type, ai, bi);
  t3 = gimple_build (stmts, loc, MULT_EXPR, type, ar, bi);

  /* Avoid expanding redundant multiplication for the common
     case of squaring a complex number.  */
  if (ar == br && ai == bi)
    t4 = t3;
  else
    t4 = gimple_build (stmts, loc, MULT_EXPR, type, ai, br);

  *rr = gimple_build (stmts, loc, MINUS_EXPR, type, t1, t2);
  *ri = gimple_build (stmts, loc, PLUS_EXPR, type, t3, t4);
}

/* Expand complex multiplication to scalars:
	a * b = (ar*br - ai*bi) + i(ar*bi + br*ai)
*/

static void
expand_complex_multiplication (gimple_stmt_iterator *gsi, tree type,
			       tree ar, tree ai, tree br, tree bi,
			       complex_lattice_t al, complex_lattice_t bl)
{
  tree rr, ri;
  tree inner_type = TREE_TYPE (type);
  location_t loc = gimple_location (gsi_stmt (*gsi));
  gimple_seq stmts = NULL;

  if (al < bl)
    {
      complex_lattice_t tl;
      rr = ar, ar = br, br = rr;
      ri = ai, ai = bi, bi = ri;
      tl = al, al = bl, bl = tl;
    }

  switch (PAIR (al, bl))
    {
    case PAIR (ONLY_REAL, ONLY_REAL):
      rr = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (ONLY_IMAG, ONLY_REAL):
      rr = ar;
      if (TREE_CODE (ai) == REAL_CST
	  && real_identical (&TREE_REAL_CST (ai), &dconst1))
	ri = br;
      else
	ri = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, br);
      break;

    case PAIR (ONLY_IMAG, ONLY_IMAG):
      rr = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, bi);
      rr = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, rr);
      ri = ar;
      break;

    case PAIR (VARYING, ONLY_REAL):
      rr = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, br);
      ri = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, br);
      break;

    case PAIR (VARYING, ONLY_IMAG):
      rr = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, bi);
      rr = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, rr);
      ri = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, bi);
      break;

    case PAIR (VARYING, VARYING):
      if (flag_complex_method == 2 && SCALAR_FLOAT_TYPE_P (inner_type))
	{
	  /* If optimizing for size or not at all just do a libcall.
	     Same if there are exception-handling edges or signaling NaNs.  */
	  if (optimize == 0 || optimize_bb_for_size_p (gsi_bb (*gsi))
	     || stmt_can_throw_internal (cfun, gsi_stmt (*gsi))
	     || flag_signaling_nans)
	    {
	      expand_complex_libcall (gsi, type, ar, ai, br, bi,
				      MULT_EXPR, true);
	      return;
	    }

	  if (!HONOR_NANS (inner_type))
	    {
	      /* If we are not worrying about NaNs expand to
		 (ar*br - ai*bi) + i(ar*bi + br*ai) directly.  */
	      expand_complex_multiplication_components (&stmts, loc, inner_type,
							ar, ai, br, bi,
							&rr, &ri);
	      break;
	    }

	  /* Else, expand x = a * b into
	     x = (ar*br - ai*bi) + i(ar*bi + br*ai);
	     if (isunordered (__real__ x, __imag__ x))
		x = __muldc3 (a, b);  */

	  tree tmpr, tmpi;
	  expand_complex_multiplication_components (&stmts, loc,
						    inner_type, ar, ai,
						    br, bi, &tmpr, &tmpi);
	  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	  stmts = NULL;

	  gimple *check
	    = gimple_build_cond (UNORDERED_EXPR, tmpr, tmpi,
				 NULL_TREE, NULL_TREE);

	  basic_block orig_bb = gsi_bb (*gsi);
	  /* We want to keep track of the original complex multiplication
	     statement as we're going to modify it later in
	     update_complex_assignment.  Make sure that insert_cond_bb leaves
	     that statement in the join block.  */
	  gsi_prev (gsi);
	  basic_block cond_bb
	    = insert_cond_bb (gsi_bb (*gsi), gsi_stmt (*gsi), check,
			      profile_probability::very_unlikely ());

	  gimple_stmt_iterator cond_bb_gsi = gsi_last_bb (cond_bb);
	  gsi_insert_after (&cond_bb_gsi, gimple_build_nop (), GSI_NEW_STMT);

	  tree libcall_res
	    = expand_complex_libcall (&cond_bb_gsi, type, ar, ai, br,
				      bi, MULT_EXPR, false);
	  gimple_seq stmts2 = NULL;
	  tree cond_real = gimple_build (&stmts2, loc, REALPART_EXPR,
					 inner_type, libcall_res);
	  tree cond_imag = gimple_build (&stmts2, loc, IMAGPART_EXPR,
					 inner_type, libcall_res);
	  gsi_insert_seq_before (&cond_bb_gsi, stmts2, GSI_SAME_STMT);

	  basic_block join_bb = single_succ_edge (cond_bb)->dest;
	  *gsi = gsi_start_nondebug_after_labels_bb (join_bb);

	  /* We have a conditional block with some assignments in cond_bb.
	     Wire up the PHIs to wrap up.  */
	  rr = make_ssa_name (inner_type);
	  ri = make_ssa_name (inner_type);
	  edge cond_to_join = single_succ_edge (cond_bb);
	  edge orig_to_join = find_edge (orig_bb, join_bb);

	  gphi *real_phi = create_phi_node (rr, gsi_bb (*gsi));
	  add_phi_arg (real_phi, cond_real, cond_to_join, UNKNOWN_LOCATION);
	  add_phi_arg (real_phi, tmpr, orig_to_join, UNKNOWN_LOCATION);

	  gphi *imag_phi = create_phi_node (ri, gsi_bb (*gsi));
	  add_phi_arg (imag_phi, cond_imag, cond_to_join, UNKNOWN_LOCATION);
	  add_phi_arg (imag_phi, tmpi, orig_to_join, UNKNOWN_LOCATION);
	}
      else
	/* If we are not worrying about NaNs expand to
	  (ar*br - ai*bi) + i(ar*bi + br*ai) directly.  */
	expand_complex_multiplication_components (&stmts, loc,
						  inner_type, ar, ai,
						  br, bi, &rr, &ri);
      break;

    default:
      gcc_unreachable ();
    }

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, rr, ri);
}

/* Keep this algorithm in sync with fold-const.cc:const_binop().

   Expand complex division to scalars, straightforward algorithm.
	a / b = ((ar*br + ai*bi)/t) + i((ai*br - ar*bi)/t)
	    t = br*br + bi*bi
*/

static void
expand_complex_div_straight (gimple_stmt_iterator *gsi, tree inner_type,
			     tree ar, tree ai, tree br, tree bi,
			     enum tree_code code)
{
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));
  tree rr, ri, div, t1, t2, t3;

  t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, br, br);
  t2 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, bi, bi);
  div = gimple_build (&stmts, loc, PLUS_EXPR, inner_type, t1, t2);

  t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, br);
  t2 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, bi);
  t3 = gimple_build (&stmts, loc, PLUS_EXPR, inner_type, t1, t2);
  rr = gimple_build (&stmts, loc, code, inner_type, t3, div);

  t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, br);
  t2 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, bi);
  t3 = gimple_build (&stmts, loc, MINUS_EXPR, inner_type, t1, t2);
  ri = gimple_build (&stmts, loc, code, inner_type, t3, div);

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, rr, ri);
}

/* Keep this algorithm in sync with fold-const.cc:const_binop().

   Expand complex division to scalars, modified algorithm to minimize
   overflow with wide input ranges.  */

static void
expand_complex_div_wide (gimple_stmt_iterator *gsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  tree rr, ri, ratio, div, t1, t2, tr, ti, compare;
  basic_block bb_cond, bb_true, bb_false, bb_join;
  gimple *stmt;
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  /* Examine |br| < |bi|, and branch.  */
  t1 = gimple_build (&stmts, loc, ABS_EXPR, inner_type, br);
  t2 = gimple_build (&stmts, loc, ABS_EXPR, inner_type, bi);
  compare = gimple_build (&stmts, loc,
			  LT_EXPR, boolean_type_node, t1, t2);

  bb_cond = bb_true = bb_false = bb_join = NULL;
  rr = ri = tr = ti = NULL;
  if (TREE_CODE (compare) != INTEGER_CST)
    {
      edge e;
      gimple *stmt;

      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      stmts = NULL;
      stmt = gimple_build_cond (NE_EXPR, compare, boolean_false_node,
				NULL_TREE, NULL_TREE);
      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

      /* Split the original block, and create the TRUE and FALSE blocks.  */
      e = split_block (gsi_bb (*gsi), stmt);
      bb_cond = e->src;
      bb_join = e->dest;
      bb_true = create_empty_bb (bb_cond);
      bb_false = create_empty_bb (bb_true);
      bb_true->count = bb_false->count
	 = bb_cond->count.apply_probability (profile_probability::even ());

      /* Wire the blocks together.  */
      e->flags = EDGE_TRUE_VALUE;
      /* TODO: With value profile we could add an historgram to determine real
	 branch outcome.  */
      e->probability = profile_probability::even ();
      redirect_edge_succ (e, bb_true);
      edge e2 = make_edge (bb_cond, bb_false, EDGE_FALSE_VALUE);
      e2->probability = profile_probability::even ();
      make_single_succ_edge (bb_true, bb_join, EDGE_FALLTHRU);
      make_single_succ_edge (bb_false, bb_join, EDGE_FALLTHRU);
      add_bb_to_loop (bb_true, bb_cond->loop_father);
      add_bb_to_loop (bb_false, bb_cond->loop_father);

      /* Update dominance info.  Note that bb_join's data was
         updated by split_block.  */
      if (dom_info_available_p (CDI_DOMINATORS))
        {
          set_immediate_dominator (CDI_DOMINATORS, bb_true, bb_cond);
          set_immediate_dominator (CDI_DOMINATORS, bb_false, bb_cond);
        }

      rr = create_tmp_reg (inner_type);
      ri = create_tmp_reg (inner_type);
    }
  else
    {
      gimple_seq_discard (stmts);
      stmts = NULL;
    }

  /* In the TRUE branch, we compute
      ratio = br/bi;
      div = (br * ratio) + bi;
      tr = (ar * ratio) + ai;
      ti = (ai * ratio) - ar;
      tr = tr / div;
      ti = ti / div;  */
  if (bb_true || integer_nonzerop (compare))
    {
      if (bb_true)
	{
	  *gsi = gsi_last_bb (bb_true);
	  gsi_insert_after (gsi, gimple_build_nop (), GSI_NEW_STMT);
	}

      ratio = gimple_build (&stmts, loc, code, inner_type, br, bi);

      t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, br, ratio);
      div = gimple_build (&stmts, loc, PLUS_EXPR, inner_type, t1, bi);

      t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, ratio);
      tr = gimple_build (&stmts, loc, PLUS_EXPR, inner_type, t1, ai);

      t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, ratio);
      ti = gimple_build (&stmts, loc, MINUS_EXPR, inner_type, t1, ar);

      tr = gimple_build (&stmts, loc, code, inner_type, tr, div);
      ti = gimple_build (&stmts, loc, code, inner_type, ti, div);
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      stmts = NULL;

     if (bb_true)
       {
	 stmt = gimple_build_assign (rr, tr);
	 gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	 stmt = gimple_build_assign (ri, ti);
	 gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	 gsi_remove (gsi, true);
       }
    }

  /* In the FALSE branch, we compute
      ratio = d/c;
      divisor = (d * ratio) + c;
      tr = (b * ratio) + a;
      ti = b - (a * ratio);
      tr = tr / div;
      ti = ti / div;  */
  if (bb_false || integer_zerop (compare))
    {
      if (bb_false)
	{
	  *gsi = gsi_last_bb (bb_false);
	  gsi_insert_after (gsi, gimple_build_nop (), GSI_NEW_STMT);
	}

      ratio = gimple_build (&stmts, loc, code, inner_type, bi, br);

      t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, bi, ratio);
      div = gimple_build (&stmts, loc, PLUS_EXPR, inner_type, t1, br);

      t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ai, ratio);
      tr = gimple_build (&stmts, loc, PLUS_EXPR, inner_type, t1, ar);

      t1 = gimple_build (&stmts, loc, MULT_EXPR, inner_type, ar, ratio);
      ti = gimple_build (&stmts, loc, MINUS_EXPR, inner_type, ai, t1);

      tr = gimple_build (&stmts, loc, code, inner_type, tr, div);
      ti = gimple_build (&stmts, loc, code, inner_type, ti, div);
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      stmts = NULL;

     if (bb_false)
       {
	 stmt = gimple_build_assign (rr, tr);
	 gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	 stmt = gimple_build_assign (ri, ti);
	 gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	 gsi_remove (gsi, true);
       }
    }

  if (bb_join)
    *gsi = gsi_start_bb (bb_join);
  else
    rr = tr, ri = ti;

  update_complex_assignment (gsi, rr, ri);
}

/* Expand complex division to scalars.  */

static void
expand_complex_division (gimple_stmt_iterator *gsi, tree type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code,
			 complex_lattice_t al, complex_lattice_t bl)
{
  tree rr, ri;
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  tree inner_type = TREE_TYPE (type);
  switch (PAIR (al, bl))
    {
    case PAIR (ONLY_REAL, ONLY_REAL):
      rr = gimple_build (&stmts, loc, code, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (ONLY_REAL, ONLY_IMAG):
      rr = ai;
      ri = gimple_build (&stmts, loc, code, inner_type, ar, bi);
      ri = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, ri);
      break;

    case PAIR (ONLY_IMAG, ONLY_REAL):
      rr = ar;
      ri = gimple_build (&stmts, loc, code, inner_type, ai, br);
      break;

    case PAIR (ONLY_IMAG, ONLY_IMAG):
      rr = gimple_build (&stmts, loc, code, inner_type, ai, bi);
      ri = ar;
      break;

    case PAIR (VARYING, ONLY_REAL):
      rr = gimple_build (&stmts, loc, code, inner_type, ar, br);
      ri = gimple_build (&stmts, loc, code, inner_type, ai, br);
      break;

    case PAIR (VARYING, ONLY_IMAG):
      rr = gimple_build (&stmts, loc, code, inner_type, ai, bi);
      ri = gimple_build (&stmts, loc, code, inner_type, ar, bi);
      ri = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, ri);
      break;

    case PAIR (ONLY_REAL, VARYING):
    case PAIR (ONLY_IMAG, VARYING):
    case PAIR (VARYING, VARYING):
      switch (flag_complex_method)
	{
	case 0:
	  /* straightforward implementation of complex divide acceptable.  */
	  expand_complex_div_straight (gsi, inner_type, ar, ai, br, bi, code);
	  break;

	case 2:
	  if (SCALAR_FLOAT_TYPE_P (inner_type))
	    {
	      expand_complex_libcall (gsi, type, ar, ai, br, bi, code, true);
	      break;
	    }
	  /* FALLTHRU */

	case 1:
	  /* wide ranges of inputs must work for complex divide.  */
	  expand_complex_div_wide (gsi, inner_type, ar, ai, br, bi, code);
	  break;

	default:
	  gcc_unreachable ();
	}
      return;

    default:
      gcc_unreachable ();
    }

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, rr, ri);
}

/* Expand complex negation to scalars:
	-a = (-ar) + i(-ai)
*/

static void
expand_complex_negation (gimple_stmt_iterator *gsi, tree inner_type,
			 tree ar, tree ai)
{
  tree rr, ri;
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  rr = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, ar);
  ri = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, ai);

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, rr, ri);
}

/* Expand complex paren to scalars:
	((a)) = ((ar)) + i((ai))
*/

static void
expand_complex_paren (gimple_stmt_iterator *gsi, tree inner_type,
		      tree ar, tree ai)
{
  tree rr, ri;
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  rr = gimple_build (&stmts, loc, PAREN_EXPR, inner_type, ar);
  ri = gimple_build (&stmts, loc, PAREN_EXPR, inner_type, ai);

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, rr, ri);
}

/* Expand complex conjugate to scalars:
	~a = (ar) + i(-ai)
*/

static void
expand_complex_conjugate (gimple_stmt_iterator *gsi, tree inner_type,
			  tree ar, tree ai)
{
  tree ri;
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  ri = gimple_build (&stmts, loc, NEGATE_EXPR, inner_type, ai);

  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  update_complex_assignment (gsi, ar, ri);
}

/* Expand complex comparison (EQ or NE only).  */

static void
expand_complex_comparison (gimple_stmt_iterator *gsi, tree ar, tree ai,
			   tree br, tree bi, enum tree_code code)
{
  tree cr, ci, cc, type;
  gimple *stmt = gsi_stmt (*gsi);
  gimple_seq stmts = NULL;
  location_t loc = gimple_location (stmt);

  cr = gimple_build (&stmts, loc, code, boolean_type_node, ar, br);
  ci = gimple_build (&stmts, loc, code, boolean_type_node, ai, bi);
  cc = gimple_build (&stmts, loc,
		     (code == EQ_EXPR ? BIT_AND_EXPR : BIT_IOR_EXPR),
		     boolean_type_node, cr, ci);
  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      type = TREE_TYPE (gimple_assign_lhs (stmt));
      gimple_assign_set_rhs_from_tree (gsi, fold_convert (type, cc));
      stmt = gsi_stmt (*gsi);
      break;

    case GIMPLE_COND:
      {
	gcond *cond_stmt = as_a <gcond *> (stmt);
	gimple_cond_set_code (cond_stmt, EQ_EXPR);
	gimple_cond_set_lhs (cond_stmt, cc);
	gimple_cond_set_rhs (cond_stmt, boolean_true_node);
      }
      break;

    default:
      gcc_unreachable ();
    }

  update_stmt (stmt);
  if (maybe_clean_eh_stmt (stmt))
    bitmap_set_bit (need_eh_cleanup, gimple_bb (stmt)->index);
}

/* Expand inline asm that sets some complex SSA_NAMEs.  */

static void
expand_complex_asm (gimple_stmt_iterator *gsi)
{
  gasm *stmt = as_a <gasm *> (gsi_stmt (*gsi));
  unsigned int i;
  bool diagnosed_p = false;

  for (i = 0; i < gimple_asm_noutputs (stmt); ++i)
    {
      tree link = gimple_asm_output_op (stmt, i);
      tree op = TREE_VALUE (link);
      if (TREE_CODE (op) == SSA_NAME
	  && TREE_CODE (TREE_TYPE (op)) == COMPLEX_TYPE)
	{
	  if (gimple_asm_nlabels (stmt) > 0)
	    {
	      if (!diagnosed_p)
		{
		  sorry_at (gimple_location (stmt),
			    "%<asm goto%> with complex typed outputs");
		  diagnosed_p = true;
		}
	      /* Make sure to not ICE later, see PR105165.  */
	      tree zero = build_zero_cst (TREE_TYPE (TREE_TYPE (op)));
	      set_component_ssa_name (op, false, zero);
	      set_component_ssa_name (op, true, zero);
	      continue;
	    }
	  tree type = TREE_TYPE (op);
	  tree inner_type = TREE_TYPE (type);
	  tree r = build1 (REALPART_EXPR, inner_type, op);
	  tree i = build1 (IMAGPART_EXPR, inner_type, op);
	  gimple_seq list = set_component_ssa_name (op, false, r);

	  if (list)
	    gsi_insert_seq_after (gsi, list, GSI_CONTINUE_LINKING);

	  list = set_component_ssa_name (op, true, i);
	  if (list)
	    gsi_insert_seq_after (gsi, list, GSI_CONTINUE_LINKING);
	}
    }
}


/* ARG is the argument to a cabs builtin call in GSI from the
   original OLD_STMT.  Create a sequence of statements prior
   to GSI that calculates sqrt(R*R + I*I), where R and
   I are the real and imaginary components of ARG, respectively.  */

static void
gimple_expand_builtin_cabs (gimple_stmt_iterator *gsi, gimple *old_stmt)
{
  tree real_part, imag_part, addend1, addend2, sum;
  tree arg = gimple_call_arg (old_stmt, 0);
  tree type = TREE_TYPE (TREE_TYPE (arg));
  machine_mode mode = TYPE_MODE (type);
  gimple *new_stmt;

  tree lhs = gimple_call_lhs (old_stmt);

  /* If there is not a LHS, then just keep the statement around.  */
  if (!lhs)
    return;

  real_part = extract_component (gsi, arg, false, true);
  imag_part = extract_component (gsi, arg, true, true);
  location_t loc = gimple_location (old_stmt);

  gimple_seq stmts = NULL;

  /* cabs(x+0i) -> abs(x).
     cabs(0+xi) -> abs(x).
     These 2 can be done even without unsafe math optimizations.  */
  if (real_zerop (imag_part)
      || real_zerop (real_part))
    {
      tree other = real_zerop (imag_part) ? real_part : imag_part;
      sum = gimple_build (&stmts, loc, ABS_EXPR, type, other);
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      new_stmt = gimple_build_assign (lhs, sum);
      gimple_set_location (new_stmt, loc);
      gsi_replace (gsi, new_stmt, true);
      return;
    }

  if (!flag_unsafe_math_optimizations)
    return;

  /* cabs(x+xi) -> fabs(x)*sqrt(2).  */
  if (operand_equal_p (real_part, imag_part))
    {
      tree sqrt2 = build_real_truncate (type, dconst_sqrt2 ());
      sum = gimple_build (&stmts, loc, ABS_EXPR, type, real_part);
      sum = gimple_build (&stmts, loc, MULT_EXPR, type, sum, sqrt2);
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      new_stmt = gimple_build_assign (lhs, sum);
      gimple_set_location (new_stmt, loc);
      gsi_replace (gsi, new_stmt, true);
      return;
    }

  /* cabs(a+bi) -> sqrt(a*a+b*b) if sqrt exists on the target
     and optimizing for speed.  */
  tree sqrtfn = mathfn_built_in (type, BUILT_IN_SQRT);
  if (!optimize_bb_for_speed_p (gimple_bb (old_stmt))
      || !sqrtfn
      || optab_handler (sqrt_optab, mode) == CODE_FOR_nothing)
    return;

  addend1 = gimple_build (&stmts, loc, MULT_EXPR, type, real_part, real_part);
  addend2 = gimple_build (&stmts, loc, MULT_EXPR, type, imag_part, imag_part);
  sum = gimple_build (&stmts, loc, PLUS_EXPR, type, addend1, addend2);
  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);

  /* Build the sqrt call. */
  new_stmt = gimple_build_call (sqrtfn, 1, sum);
  gimple_set_location (new_stmt, loc);
  gimple_call_set_lhs (new_stmt, lhs);
  gsi_replace (gsi, new_stmt, true);
}

/* Process one statement.  If we identify a complex operation, expand it.  */

static void
expand_complex_operations_1 (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree type, inner_type, lhs;
  tree ac, ar, ai, bc, br, bi;
  complex_lattice_t al, bl;
  enum tree_code code;
  if (gimple_code (stmt) == GIMPLE_CALL)
    {
      switch (gimple_call_combined_fn (stmt))
	{
	CASE_CFN_CABS:
	  gimple_expand_builtin_cabs (gsi, stmt);
	  return;
	default:;
	}
    }

  if (gimple_code (stmt) == GIMPLE_ASM)
    {
      expand_complex_asm (gsi);
      return;
    }

  lhs = gimple_get_lhs (stmt);
  if (!lhs && gimple_code (stmt) != GIMPLE_COND)
    return;

  type = TREE_TYPE (gimple_op (stmt, 0));
  code = gimple_expr_code (stmt);

  /* Initial filter for operations we handle.  */
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case NEGATE_EXPR:
    case PAREN_EXPR:
    case CONJ_EXPR:
      if (TREE_CODE (type) != COMPLEX_TYPE)
	return;
      inner_type = TREE_TYPE (type);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      /* Note, both GIMPLE_ASSIGN and GIMPLE_COND may have an EQ_EXPR
	 subcode, so we need to access the operands using gimple_op.  */
      inner_type = TREE_TYPE (gimple_op (stmt, 1));
      if (TREE_CODE (inner_type) != COMPLEX_TYPE)
	return;
      break;

    default:
      {
	tree rhs;

	/* GIMPLE_COND may also fallthru here, but we do not need to
	   do anything with it.  */
	if (gimple_code (stmt) == GIMPLE_COND)
	  return;

	if (TREE_CODE (type) == COMPLEX_TYPE)
	  expand_complex_move (gsi, type);
	else if (is_gimple_assign (stmt)
		 && (gimple_assign_rhs_code (stmt) == REALPART_EXPR
		     || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR)
		 && TREE_CODE (lhs) == SSA_NAME)
	  {
	    rhs = gimple_assign_rhs1 (stmt);
	    rhs = extract_component (gsi, TREE_OPERAND (rhs, 0),
		                     gimple_assign_rhs_code (stmt)
				       == IMAGPART_EXPR,
				     false);
	    gimple_assign_set_rhs_from_tree (gsi, rhs);
	    stmt = gsi_stmt (*gsi);
	    update_stmt (stmt);
	  }
      }
      return;
    }

  /* Extract the components of the two complex values.  Make sure and
     handle the common case of the same value used twice specially.  */
  if (is_gimple_assign (stmt))
    {
      ac = gimple_assign_rhs1 (stmt);
      bc = (gimple_num_ops (stmt) > 2) ? gimple_assign_rhs2 (stmt) : NULL;
    }
  /* GIMPLE_CALL cannot get here.  */
  else
    {
      ac = gimple_cond_lhs (stmt);
      bc = gimple_cond_rhs (stmt);
    }

  ar = extract_component (gsi, ac, false, true);
  ai = extract_component (gsi, ac, true, true);

  if (ac == bc)
    br = ar, bi = ai;
  else if (bc)
    {
      br = extract_component (gsi, bc, 0, true);
      bi = extract_component (gsi, bc, 1, true);
    }
  else
    br = bi = NULL_TREE;

  al = find_lattice_value (ac);
  if (al == UNINITIALIZED)
    al = VARYING;

  if (TREE_CODE_CLASS (code) == tcc_unary)
    bl = UNINITIALIZED;
  else if (ac == bc)
    bl = al;
  else
    {
      bl = find_lattice_value (bc);
      if (bl == UNINITIALIZED)
	bl = VARYING;
    }

  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      expand_complex_addition (gsi, inner_type, ar, ai, br, bi, code, al, bl);
      break;

    case MULT_EXPR:
      expand_complex_multiplication (gsi, type, ar, ai, br, bi, al, bl);
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
      expand_complex_division (gsi, type, ar, ai, br, bi, code, al, bl);
      break;

    case NEGATE_EXPR:
      expand_complex_negation (gsi, inner_type, ar, ai);
      break;

    case CONJ_EXPR:
      expand_complex_conjugate (gsi, inner_type, ar, ai);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      expand_complex_comparison (gsi, ar, ai, br, bi, code);
      break;

    case PAREN_EXPR:
      expand_complex_paren (gsi, inner_type, ar, ai);
      break;

    default:
      gcc_unreachable ();
    }
}


/* Entry point for complex operation lowering during optimization.  */

static unsigned int
tree_lower_complex (void)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  int n_bbs, i;
  int *rpo;

  if (!init_dont_simulate_again ())
    return 0;

  complex_lattice_values.create (num_ssa_names);
  complex_lattice_values.safe_grow_cleared (num_ssa_names, true);

  init_parameter_lattice_values ();
  class complex_propagate complex_propagate;
  complex_propagate.ssa_propagate ();

  need_eh_cleanup = BITMAP_ALLOC (NULL);
  if (optimize)
    dce_worklist = BITMAP_ALLOC (NULL);

  complex_variable_components = new int_tree_htab_type (10);

  complex_ssa_name_components.create (2 * num_ssa_names);
  complex_ssa_name_components.safe_grow_cleared (2 * num_ssa_names, true);

  update_parameter_components ();

  rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));
  n_bbs = pre_and_rev_post_order_compute (NULL, rpo, false);
  for (i = 0; i < n_bbs; i++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, rpo[i]);
      if (!bb)
	continue;
      update_phi_components (bb);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	expand_complex_operations_1 (&gsi);
    }

  free (rpo);

  if (!phis_to_revisit.is_empty ())
    {
      unsigned int n = phis_to_revisit.length ();
      for (unsigned int j = 0; j < n; j += 3)
	for (unsigned int k = 0; k < 2; k++)
	  if (gphi *phi = phis_to_revisit[j + k + 1])
	    {
	      unsigned int m = gimple_phi_num_args (phi);
	      for (unsigned int l = 0; l < m; ++l)
		{
		  tree op = gimple_phi_arg_def (phi, l);
		  if (TREE_CODE (op) == SSA_NAME
		      || is_gimple_min_invariant (op))
		    continue;
		  tree arg = gimple_phi_arg_def (phis_to_revisit[j], l);
		  op = extract_component (NULL, arg, k > 0, false, false);
		  SET_PHI_ARG_DEF (phi, l, op);
		}
	    }
      phis_to_revisit.release ();
    }

  gsi_commit_edge_inserts ();

  if (optimize)
    {
      simple_dce_from_worklist (dce_worklist, need_eh_cleanup);
      BITMAP_FREE (dce_worklist);
    }

  unsigned todo
    = gimple_purge_all_dead_eh_edges (need_eh_cleanup) ? TODO_cleanup_cfg : 0;
  BITMAP_FREE (need_eh_cleanup);

  delete complex_variable_components;
  complex_variable_components = NULL;
  complex_ssa_name_components.release ();
  complex_lattice_values.release ();
  return todo;
}

namespace {

const pass_data pass_data_lower_complex =
{
  GIMPLE_PASS, /* type */
  "cplxlower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  PROP_gimple_lcx, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_lower_complex : public gimple_opt_pass
{
public:
  pass_lower_complex (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_complex, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_lower_complex (m_ctxt); }
  unsigned int execute (function *) final override
  {
    return tree_lower_complex ();
  }

}; // class pass_lower_complex

} // anon namespace

gimple_opt_pass *
make_pass_lower_complex (gcc::context *ctxt)
{
  return new pass_lower_complex (ctxt);
}


namespace {

const pass_data pass_data_lower_complex_O0 =
{
  GIMPLE_PASS, /* type */
  "cplxlower0", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_gimple_lcx, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_lower_complex_O0 : public gimple_opt_pass
{
public:
  pass_lower_complex_O0 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_complex_O0, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
    {
      /* With errors, normal optimization passes are not run.  If we don't
	 lower complex operations at all, rtl expansion will abort.  */
      return !(fun->curr_properties & PROP_gimple_lcx);
    }

  unsigned int execute (function *) final override
  {
    return tree_lower_complex ();
  }

}; // class pass_lower_complex_O0

} // anon namespace

gimple_opt_pass *
make_pass_lower_complex_O0 (gcc::context *ctxt)
{
  return new pass_lower_complex_O0 (ctxt);
}
