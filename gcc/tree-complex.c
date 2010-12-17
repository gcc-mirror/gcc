/* Lower complex number operations to scalar operations.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"


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

DEF_VEC_I(complex_lattice_t);
DEF_VEC_ALLOC_I(complex_lattice_t, heap);

static VEC(complex_lattice_t, heap) *complex_lattice_values;

/* For each complex variable, a pair of variables for the components exists in
   the hashtable.  */
static htab_t complex_variable_components;

/* For each complex SSA_NAME, a pair of ssa names for the components.  */
static VEC(tree, heap) *complex_ssa_name_components;

/* Lookup UID in the complex_variable_components hashtable and return the
   associated tree.  */
static tree
cvc_lookup (unsigned int uid)
{
  struct int_tree_map *h, in;
  in.uid = uid;
  h = (struct int_tree_map *) htab_find_with_hash (complex_variable_components, &in, uid);
  return h ? h->to : NULL;
}

/* Insert the pair UID, TO into the complex_variable_components hashtable.  */

static void
cvc_insert (unsigned int uid, tree to)
{
  struct int_tree_map *h;
  void **loc;

  h = XNEW (struct int_tree_map);
  h->uid = uid;
  h->to = to;
  loc = htab_find_slot_with_hash (complex_variable_components, h,
				  uid, INSERT);
  *(struct int_tree_map **) loc = h;
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
    zerop = REAL_VALUES_IDENTICAL (TREE_REAL_CST (t), dconst0);
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
      return VEC_index (complex_lattice_t, complex_lattice_values,
			SSA_NAME_VERSION (t));

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
	&& var_ann (parm) != NULL
	&& (ssa_name = gimple_default_def (cfun, parm)) != NULL_TREE)
      VEC_replace (complex_lattice_t, complex_lattice_values,
		   SSA_NAME_VERSION (ssa_name), VARYING);
}

/* Initialize simulation state for each statement.  Return false if we
   found no statements we want to simulate, and thus there's nothing
   for the entire pass to do.  */

static bool
init_dont_simulate_again (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple phi;
  bool saw_a_complex_op = false;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  phi = gsi_stmt (gsi);
	  prop_set_simulate_again (phi,
				   is_complex_reg (gimple_phi_result (phi)));
	}

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt;
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
	        sim_again_p = is_complex_reg (gimple_call_lhs (stmt));
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
		break;
	      }

	  prop_set_simulate_again (stmt, sim_again_p);
	}
    }

  return saw_a_complex_op;
}


/* Evaluate statement STMT against the complex lattice defined above.  */

static enum ssa_prop_result
complex_visit_stmt (gimple stmt, edge *taken_edge_p ATTRIBUTE_UNUSED,
		    tree *result_p)
{
  complex_lattice_t new_l, old_l, op1_l, op2_l;
  unsigned int ver;
  tree lhs;

  lhs = gimple_get_lhs (stmt);
  /* Skip anything but GIMPLE_ASSIGN and GIMPLE_CALL with a lhs.  */
  if (!lhs)
    return SSA_PROP_VARYING;

  /* These conditions should be satisfied due to the initial filter
     set up in init_dont_simulate_again.  */
  gcc_assert (TREE_CODE (lhs) == SSA_NAME);
  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE);

  *result_p = lhs;
  ver = SSA_NAME_VERSION (lhs);
  old_l = VEC_index (complex_lattice_t, complex_lattice_values, ver);

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

  VEC_replace (complex_lattice_t, complex_lattice_values, ver, new_l);
  return new_l == VARYING ? SSA_PROP_VARYING : SSA_PROP_INTERESTING;
}

/* Evaluate a PHI node against the complex lattice defined above.  */

static enum ssa_prop_result
complex_visit_phi (gimple phi)
{
  complex_lattice_t new_l, old_l;
  unsigned int ver;
  tree lhs;
  int i;

  lhs = gimple_phi_result (phi);

  /* This condition should be satisfied due to the initial filter
     set up in init_dont_simulate_again.  */
  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE);

  /* We've set up the lattice values such that IOR neatly models PHI meet.  */
  new_l = UNINITIALIZED;
  for (i = gimple_phi_num_args (phi) - 1; i >= 0; --i)
    new_l |= find_lattice_value (gimple_phi_arg_def (phi, i));

  ver = SSA_NAME_VERSION (lhs);
  old_l = VEC_index (complex_lattice_t, complex_lattice_values, ver);

  if (new_l == old_l)
    return SSA_PROP_NOT_INTERESTING;

  VEC_replace (complex_lattice_t, complex_lattice_values, ver, new_l);
  return new_l == VARYING ? SSA_PROP_VARYING : SSA_PROP_INTERESTING;
}

/* Create one backing variable for a complex component of ORIG.  */

static tree
create_one_component_var (tree type, tree orig, const char *prefix,
			  const char *suffix, enum tree_code code)
{
  tree r = create_tmp_var (type, prefix);
  add_referenced_var (r);

  DECL_SOURCE_LOCATION (r) = DECL_SOURCE_LOCATION (orig);
  DECL_ARTIFICIAL (r) = 1;

  if (DECL_NAME (orig) && !DECL_IGNORED_P (orig))
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (orig));

      DECL_NAME (r) = get_identifier (ACONCAT ((name, suffix, NULL)));

      SET_DECL_DEBUG_EXPR (r, build1 (code, type, orig));
      DECL_DEBUG_EXPR_IS_FROM (r) = 1;
      DECL_IGNORED_P (r) = 0;
      TREE_NO_WARNING (r) = TREE_NO_WARNING (orig);
    }
  else
    {
      DECL_IGNORED_P (r) = 1;
      TREE_NO_WARNING (r) = 1;
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
  ret = VEC_index (tree, complex_ssa_name_components, ssa_name_index);
  if (ret == NULL)
    {
      ret = get_component_var (SSA_NAME_VAR (ssa_name), imag_p);
      ret = make_ssa_name (ret, NULL);

      /* Copy some properties from the original.  In particular, whether it
	 is used in an abnormal phi, and whether it's uninitialized.  */
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ret)
	= SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name);
      if (TREE_CODE (SSA_NAME_VAR (ssa_name)) == VAR_DECL
	  && gimple_nop_p (SSA_NAME_DEF_STMT (ssa_name)))
	{
	  SSA_NAME_DEF_STMT (ret) = SSA_NAME_DEF_STMT (ssa_name);
	  set_default_def (SSA_NAME_VAR (ret), ret);
	}

      VEC_replace (tree, complex_ssa_name_components, ssa_name_index, ret);
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
  gimple last;
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
  comp = VEC_index (tree, complex_ssa_name_components, ssa_name_index);
  if (comp)
    ;

  /* If we've nothing assigned, and the value we're given is already stable,
     then install that as the value for this SSA_NAME.  This preemptively
     copy-propagates the value, which avoids unnecessary memory allocation.  */
  else if (is_gimple_min_invariant (value)
	   && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name))
    {
      VEC_replace (tree, complex_ssa_name_components, ssa_name_index, value);
      return NULL;
    }
  else if (TREE_CODE (value) == SSA_NAME
	   && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ssa_name))
    {
      /* Replace an anonymous base value with the variable from cvc_lookup.
	 This should result in better debug info.  */
      if (DECL_IGNORED_P (SSA_NAME_VAR (value))
	  && !DECL_IGNORED_P (SSA_NAME_VAR (ssa_name)))
	{
	  comp = get_component_var (SSA_NAME_VAR (ssa_name), imag_p);
	  replace_ssa_name_symbol (value, comp);
	}

      VEC_replace (tree, complex_ssa_name_components, ssa_name_index, value);
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
		   bool gimple_p)
{
  switch (TREE_CODE (t))
    {
    case COMPLEX_CST:
      return imagpart_p ? TREE_IMAGPART (t) : TREE_REALPART (t);

    case COMPLEX_EXPR:
      gcc_unreachable ();

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
      return get_component_ssa_name (t, imagpart_p);

    default:
      gcc_unreachable ();
    }
}

/* Update the complex components of the ssa name on the lhs of STMT.  */

static void
update_complex_components (gimple_stmt_iterator *gsi, gimple stmt, tree r,
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
  gimple_stmt_iterator orig_si = *gsi;
  gimple stmt;

  if (gimple_in_ssa_p (cfun))
    update_complex_components (gsi, gsi_stmt (*gsi), r, i);

  gimple_assign_set_rhs_with_ops (&orig_si, COMPLEX_EXPR, r, i);
  stmt = gsi_stmt (orig_si);
  update_stmt (stmt);
  if (maybe_clean_eh_stmt (stmt))
    gimple_purge_dead_eh_edges (gimple_bb (stmt));
}


/* Generate code at the entry point of the function to initialize the
   component variables for a complex parameter.  */

static void
update_parameter_components (void)
{
  edge entry_edge = single_succ_edge (ENTRY_BLOCK_PTR);
  tree parm;

  for (parm = DECL_ARGUMENTS (cfun->decl); parm ; parm = DECL_CHAIN (parm))
    {
      tree type = TREE_TYPE (parm);
      tree ssa_name, r, i;

      if (TREE_CODE (type) != COMPLEX_TYPE || !is_gimple_reg (parm))
	continue;

      type = TREE_TYPE (type);
      ssa_name = gimple_default_def (cfun, parm);
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
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple phi = gsi_stmt (gsi);

      if (is_complex_reg (gimple_phi_result (phi)))
	{
	  tree lr, li;
	  gimple pr = NULL, pi = NULL;
	  unsigned int i, n;

	  lr = get_component_ssa_name (gimple_phi_result (phi), false);
	  if (TREE_CODE (lr) == SSA_NAME)
	    {
	      pr = create_phi_node (lr, bb);
	      SSA_NAME_DEF_STMT (lr) = pr;
	    }

	  li = get_component_ssa_name (gimple_phi_result (phi), true);
	  if (TREE_CODE (li) == SSA_NAME)
	    {
	      pi = create_phi_node (li, bb);
	      SSA_NAME_DEF_STMT (li) = pi;
	    }

	  for (i = 0, n = gimple_phi_num_args (phi); i < n; ++i)
	    {
	      tree comp, arg = gimple_phi_arg_def (phi, i);
	      if (pr)
		{
		  comp = extract_component (NULL, arg, false, false);
		  SET_PHI_ARG_DEF (pr, i, comp);
		}
	      if (pi)
		{
		  comp = extract_component (NULL, arg, true, false);
		  SET_PHI_ARG_DEF (pi, i, comp);
		}
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
  gimple stmt = gsi_stmt (*gsi);

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
	       || gimple_has_side_effects (stmt)
	       || gimple_assign_rhs_code (stmt) == PAREN_EXPR)
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
  else if (rhs && TREE_CODE (rhs) == SSA_NAME && !TREE_SIDE_EFFECTS (lhs))
    {
      tree x;
      gimple t;

      r = extract_component (gsi, rhs, 0, false);
      i = extract_component (gsi, rhs, 1, false);

      x = build1 (REALPART_EXPR, inner_type, unshare_expr (lhs));
      t = gimple_build_assign (x, r);
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
	  gsi_insert_before (gsi, t, GSI_SAME_STMT);

	  stmt = gsi_stmt (*gsi);
	  gcc_assert (gimple_code (stmt) == GIMPLE_RETURN);
	  gimple_return_set_retval (stmt, lhs);
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

  switch (PAIR (al, bl))
    {
    case PAIR (ONLY_REAL, ONLY_REAL):
      rr = gimplify_build2 (gsi, code, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (ONLY_REAL, ONLY_IMAG):
      rr = ar;
      if (code == MINUS_EXPR)
	ri = gimplify_build2 (gsi, MINUS_EXPR, inner_type, ai, bi);
      else
	ri = bi;
      break;

    case PAIR (ONLY_IMAG, ONLY_REAL):
      if (code == MINUS_EXPR)
	rr = gimplify_build2 (gsi, MINUS_EXPR, inner_type, ar, br);
      else
	rr = br;
      ri = ai;
      break;

    case PAIR (ONLY_IMAG, ONLY_IMAG):
      rr = ar;
      ri = gimplify_build2 (gsi, code, inner_type, ai, bi);
      break;

    case PAIR (VARYING, ONLY_REAL):
      rr = gimplify_build2 (gsi, code, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (VARYING, ONLY_IMAG):
      rr = ar;
      ri = gimplify_build2 (gsi, code, inner_type, ai, bi);
      break;

    case PAIR (ONLY_REAL, VARYING):
      if (code == MINUS_EXPR)
	goto general;
      rr = gimplify_build2 (gsi, code, inner_type, ar, br);
      ri = bi;
      break;

    case PAIR (ONLY_IMAG, VARYING):
      if (code == MINUS_EXPR)
	goto general;
      rr = br;
      ri = gimplify_build2 (gsi, code, inner_type, ai, bi);
      break;

    case PAIR (VARYING, VARYING):
    general:
      rr = gimplify_build2 (gsi, code, inner_type, ar, br);
      ri = gimplify_build2 (gsi, code, inner_type, ai, bi);
      break;

    default:
      gcc_unreachable ();
    }

  update_complex_assignment (gsi, rr, ri);
}

/* Expand a complex multiplication or division to a libcall to the c99
   compliant routines.  */

static void
expand_complex_libcall (gimple_stmt_iterator *gsi, tree ar, tree ai,
			tree br, tree bi, enum tree_code code)
{
  enum machine_mode mode;
  enum built_in_function bcode;
  tree fn, type, lhs;
  gimple old_stmt, stmt;

  old_stmt = gsi_stmt (*gsi);
  lhs = gimple_assign_lhs (old_stmt);
  type = TREE_TYPE (lhs);

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
  fn = built_in_decls[bcode];

  stmt = gimple_build_call (fn, 4, ar, ai, br, bi);
  gimple_call_set_lhs (stmt, lhs);
  update_stmt (stmt);
  gsi_replace (gsi, stmt, false);

  if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
    gimple_purge_dead_eh_edges (gsi_bb (*gsi));

  if (gimple_in_ssa_p (cfun))
    {
      type = TREE_TYPE (type);
      update_complex_components (gsi, stmt,
				 build1 (REALPART_EXPR, type, lhs),
				 build1 (IMAGPART_EXPR, type, lhs));
      SSA_NAME_DEF_STMT (lhs) = stmt;
    }
}

/* Expand complex multiplication to scalars:
	a * b = (ar*br - ai*bi) + i(ar*bi + br*ai)
*/

static void
expand_complex_multiplication (gimple_stmt_iterator *gsi, tree inner_type,
			       tree ar, tree ai, tree br, tree bi,
			       complex_lattice_t al, complex_lattice_t bl)
{
  tree rr, ri;

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
      rr = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (ONLY_IMAG, ONLY_REAL):
      rr = ar;
      if (TREE_CODE (ai) == REAL_CST
	  && REAL_VALUES_IDENTICAL (TREE_REAL_CST (ai), dconst1))
	ri = br;
      else
	ri = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, br);
      break;

    case PAIR (ONLY_IMAG, ONLY_IMAG):
      rr = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, bi);
      rr = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, rr);
      ri = ar;
      break;

    case PAIR (VARYING, ONLY_REAL):
      rr = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, br);
      ri = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, br);
      break;

    case PAIR (VARYING, ONLY_IMAG):
      rr = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, bi);
      rr = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, rr);
      ri = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, bi);
      break;

    case PAIR (VARYING, VARYING):
      if (flag_complex_method == 2 && SCALAR_FLOAT_TYPE_P (inner_type))
	{
	  expand_complex_libcall (gsi, ar, ai, br, bi, MULT_EXPR);
	  return;
	}
      else
	{
	  tree t1, t2, t3, t4;

	  t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, br);
	  t2 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, bi);
	  t3 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, bi);

	  /* Avoid expanding redundant multiplication for the common
	     case of squaring a complex number.  */
	  if (ar == br && ai == bi)
	    t4 = t3;
	  else
	    t4 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, br);

	  rr = gimplify_build2 (gsi, MINUS_EXPR, inner_type, t1, t2);
	  ri = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t3, t4);
	}
      break;

    default:
      gcc_unreachable ();
    }

  update_complex_assignment (gsi, rr, ri);
}

/* Keep this algorithm in sync with fold-const.c:const_binop().

   Expand complex division to scalars, straightforward algorithm.
	a / b = ((ar*br + ai*bi)/t) + i((ai*br - ar*bi)/t)
	    t = br*br + bi*bi
*/

static void
expand_complex_div_straight (gimple_stmt_iterator *gsi, tree inner_type,
			     tree ar, tree ai, tree br, tree bi,
			     enum tree_code code)
{
  tree rr, ri, div, t1, t2, t3;

  t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, br, br);
  t2 = gimplify_build2 (gsi, MULT_EXPR, inner_type, bi, bi);
  div = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t1, t2);

  t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, br);
  t2 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, bi);
  t3 = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t1, t2);
  rr = gimplify_build2 (gsi, code, inner_type, t3, div);

  t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, br);
  t2 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, bi);
  t3 = gimplify_build2 (gsi, MINUS_EXPR, inner_type, t1, t2);
  ri = gimplify_build2 (gsi, code, inner_type, t3, div);

  update_complex_assignment (gsi, rr, ri);
}

/* Keep this algorithm in sync with fold-const.c:const_binop().

   Expand complex division to scalars, modified algorithm to minimize
   overflow with wide input ranges.  */

static void
expand_complex_div_wide (gimple_stmt_iterator *gsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code)
{
  tree rr, ri, ratio, div, t1, t2, tr, ti, compare;
  basic_block bb_cond, bb_true, bb_false, bb_join;
  gimple stmt;

  /* Examine |br| < |bi|, and branch.  */
  t1 = gimplify_build1 (gsi, ABS_EXPR, inner_type, br);
  t2 = gimplify_build1 (gsi, ABS_EXPR, inner_type, bi);
  compare = fold_build2_loc (gimple_location (gsi_stmt (*gsi)),
			     LT_EXPR, boolean_type_node, t1, t2);
  STRIP_NOPS (compare);

  bb_cond = bb_true = bb_false = bb_join = NULL;
  rr = ri = tr = ti = NULL;
  if (TREE_CODE (compare) != INTEGER_CST)
    {
      edge e;
      gimple stmt;
      tree cond, tmp;

      tmp = create_tmp_var (boolean_type_node, NULL);
      stmt = gimple_build_assign (tmp, compare);
      if (gimple_in_ssa_p (cfun))
	{
	  tmp = make_ssa_name (tmp,  stmt);
	  gimple_assign_set_lhs (stmt, tmp);
	}

      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

      cond = fold_build2_loc (gimple_location (stmt),
			  EQ_EXPR, boolean_type_node, tmp, boolean_true_node);
      stmt = gimple_build_cond_from_tree (cond, NULL_TREE, NULL_TREE);
      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

      /* Split the original block, and create the TRUE and FALSE blocks.  */
      e = split_block (gsi_bb (*gsi), stmt);
      bb_cond = e->src;
      bb_join = e->dest;
      bb_true = create_empty_bb (bb_cond);
      bb_false = create_empty_bb (bb_true);

      /* Wire the blocks together.  */
      e->flags = EDGE_TRUE_VALUE;
      redirect_edge_succ (e, bb_true);
      make_edge (bb_cond, bb_false, EDGE_FALSE_VALUE);
      make_edge (bb_true, bb_join, EDGE_FALLTHRU);
      make_edge (bb_false, bb_join, EDGE_FALLTHRU);

      /* Update dominance info.  Note that bb_join's data was
         updated by split_block.  */
      if (dom_info_available_p (CDI_DOMINATORS))
        {
          set_immediate_dominator (CDI_DOMINATORS, bb_true, bb_cond);
          set_immediate_dominator (CDI_DOMINATORS, bb_false, bb_cond);
        }

      rr = make_rename_temp (inner_type, NULL);
      ri = make_rename_temp (inner_type, NULL);
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

      ratio = gimplify_build2 (gsi, code, inner_type, br, bi);

      t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, br, ratio);
      div = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t1, bi);

      t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, ratio);
      tr = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t1, ai);

      t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, ratio);
      ti = gimplify_build2 (gsi, MINUS_EXPR, inner_type, t1, ar);

      tr = gimplify_build2 (gsi, code, inner_type, tr, div);
      ti = gimplify_build2 (gsi, code, inner_type, ti, div);

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

      ratio = gimplify_build2 (gsi, code, inner_type, bi, br);

      t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, bi, ratio);
      div = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t1, br);

      t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ai, ratio);
      tr = gimplify_build2 (gsi, PLUS_EXPR, inner_type, t1, ar);

      t1 = gimplify_build2 (gsi, MULT_EXPR, inner_type, ar, ratio);
      ti = gimplify_build2 (gsi, MINUS_EXPR, inner_type, ai, t1);

      tr = gimplify_build2 (gsi, code, inner_type, tr, div);
      ti = gimplify_build2 (gsi, code, inner_type, ti, div);

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
expand_complex_division (gimple_stmt_iterator *gsi, tree inner_type,
			 tree ar, tree ai, tree br, tree bi,
			 enum tree_code code,
			 complex_lattice_t al, complex_lattice_t bl)
{
  tree rr, ri;

  switch (PAIR (al, bl))
    {
    case PAIR (ONLY_REAL, ONLY_REAL):
      rr = gimplify_build2 (gsi, code, inner_type, ar, br);
      ri = ai;
      break;

    case PAIR (ONLY_REAL, ONLY_IMAG):
      rr = ai;
      ri = gimplify_build2 (gsi, code, inner_type, ar, bi);
      ri = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, ri);
      break;

    case PAIR (ONLY_IMAG, ONLY_REAL):
      rr = ar;
      ri = gimplify_build2 (gsi, code, inner_type, ai, br);
      break;

    case PAIR (ONLY_IMAG, ONLY_IMAG):
      rr = gimplify_build2 (gsi, code, inner_type, ai, bi);
      ri = ar;
      break;

    case PAIR (VARYING, ONLY_REAL):
      rr = gimplify_build2 (gsi, code, inner_type, ar, br);
      ri = gimplify_build2 (gsi, code, inner_type, ai, br);
      break;

    case PAIR (VARYING, ONLY_IMAG):
      rr = gimplify_build2 (gsi, code, inner_type, ai, bi);
      ri = gimplify_build2 (gsi, code, inner_type, ar, bi);
      ri = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, ri);

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
	      expand_complex_libcall (gsi, ar, ai, br, bi, code);
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

  rr = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, ar);
  ri = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, ai);

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

  ri = gimplify_build1 (gsi, NEGATE_EXPR, inner_type, ai);

  update_complex_assignment (gsi, ar, ri);
}

/* Expand complex comparison (EQ or NE only).  */

static void
expand_complex_comparison (gimple_stmt_iterator *gsi, tree ar, tree ai,
			   tree br, tree bi, enum tree_code code)
{
  tree cr, ci, cc, type;
  gimple stmt;

  cr = gimplify_build2 (gsi, code, boolean_type_node, ar, br);
  ci = gimplify_build2 (gsi, code, boolean_type_node, ai, bi);
  cc = gimplify_build2 (gsi,
			(code == EQ_EXPR ? TRUTH_AND_EXPR : TRUTH_OR_EXPR),
			boolean_type_node, cr, ci);

  stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_RETURN:
      type = TREE_TYPE (gimple_return_retval (stmt));
      gimple_return_set_retval (stmt, fold_convert (type, cc));
      break;

    case GIMPLE_ASSIGN:
      type = TREE_TYPE (gimple_assign_lhs (stmt));
      gimple_assign_set_rhs_from_tree (gsi, fold_convert (type, cc));
      stmt = gsi_stmt (*gsi);
      break;

    case GIMPLE_COND:
      gimple_cond_set_code (stmt, EQ_EXPR);
      gimple_cond_set_lhs (stmt, cc);
      gimple_cond_set_rhs (stmt, boolean_true_node);
      break;

    default:
      gcc_unreachable ();
    }

  update_stmt (stmt);
}


/* Process one statement.  If we identify a complex operation, expand it.  */

static void
expand_complex_operations_1 (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree type, inner_type, lhs;
  tree ac, ar, ai, bc, br, bi;
  complex_lattice_t al, bl;
  enum tree_code code;

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
    case CONJ_EXPR:
      if (TREE_CODE (type) != COMPLEX_TYPE)
	return;
      inner_type = TREE_TYPE (type);
      break;

    case EQ_EXPR:
    case NE_EXPR:
      /* Note, both GIMPLE_ASSIGN and GIMPLE_COND may have an EQ_EXPR
	 subocde, so we need to access the operands using gimple_op.  */
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
  /* GIMPLE_CALL can not get here.  */
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

  if (gimple_in_ssa_p (cfun))
    {
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
    }
  else
    al = bl = VARYING;

  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      expand_complex_addition (gsi, inner_type, ar, ai, br, bi, code, al, bl);
      break;

    case MULT_EXPR:
      expand_complex_multiplication (gsi, inner_type, ar, ai, br, bi, al, bl);
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
      expand_complex_division (gsi, inner_type, ar, ai, br, bi, code, al, bl);
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

    default:
      gcc_unreachable ();
    }
}


/* Entry point for complex operation lowering during optimization.  */

static unsigned int
tree_lower_complex (void)
{
  int old_last_basic_block;
  gimple_stmt_iterator gsi;
  basic_block bb;

  if (!init_dont_simulate_again ())
    return 0;

  complex_lattice_values = VEC_alloc (complex_lattice_t, heap, num_ssa_names);
  VEC_safe_grow_cleared (complex_lattice_t, heap,
			 complex_lattice_values, num_ssa_names);

  init_parameter_lattice_values ();
  ssa_propagate (complex_visit_stmt, complex_visit_phi);

  complex_variable_components = htab_create (10,  int_tree_map_hash,
					     int_tree_map_eq, free);

  complex_ssa_name_components = VEC_alloc (tree, heap, 2*num_ssa_names);
  VEC_safe_grow_cleared (tree, heap, complex_ssa_name_components,
			 2 * num_ssa_names);

  update_parameter_components ();

  /* ??? Ideally we'd traverse the blocks in breadth-first order.  */
  old_last_basic_block = last_basic_block;
  FOR_EACH_BB (bb)
    {
      if (bb->index >= old_last_basic_block)
	continue;

      update_phi_components (bb);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	expand_complex_operations_1 (&gsi);
    }

  gsi_commit_edge_inserts ();

  htab_delete (complex_variable_components);
  VEC_free (tree, heap, complex_ssa_name_components);
  VEC_free (complex_lattice_t, heap, complex_lattice_values);
  return 0;
}

struct gimple_opt_pass pass_lower_complex =
{
 {
  GIMPLE_PASS,
  "cplxlower",				/* name */
  0,					/* gate */
  tree_lower_complex,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_ssa,				/* properties_required */
  PROP_gimple_lcx,			/* properties_provided */
  0,                       		/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_ggc_collect
    | TODO_update_ssa
    | TODO_verify_stmts	 		/* todo_flags_finish */
 }
};


static bool
gate_no_optimization (void)
{
  /* With errors, normal optimization passes are not run.  If we don't
     lower complex operations at all, rtl expansion will abort.  */
  return !(cfun->curr_properties & PROP_gimple_lcx);
}

struct gimple_opt_pass pass_lower_complex_O0 =
{
 {
  GIMPLE_PASS,
  "cplxlower0",				/* name */
  gate_no_optimization,			/* gate */
  tree_lower_complex,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  PROP_gimple_lcx,			/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_ggc_collect
    | TODO_update_ssa
    | TODO_verify_stmts	 		/* todo_flags_finish */
 }
};
