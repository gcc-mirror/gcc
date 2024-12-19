/* Processing rules for constraints.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.
   Contributed by Andrew Sutton (andrew.n.sutton@gmail.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "timevar.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "intl.h"
#include "flags.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "cp-objcp-common.h"
#include "tree-inline.h"
#include "decl.h"
#include "toplev.h"
#include "type-utils.h"

static tree satisfaction_value (tree t);

/* When we're parsing or substuting a constraint expression, we have slightly
   different expression semantics.  In particular, we don't want to reduce a
   concept-id to a satisfaction value.  */

processing_constraint_expression_sentinel::
processing_constraint_expression_sentinel ()
{
  ++scope_chain->x_processing_constraint;
}

processing_constraint_expression_sentinel::
~processing_constraint_expression_sentinel ()
{
  --scope_chain->x_processing_constraint;
}

bool
processing_constraint_expression_p ()
{
  return scope_chain->x_processing_constraint != 0;
}

/*---------------------------------------------------------------------------
		       Constraint expressions
---------------------------------------------------------------------------*/

/* Information provided to substitution.  */

struct subst_info
{
  subst_info (tsubst_flags_t cmp, tree in)
    : complain (cmp), in_decl (in)
  { }

  /* True if we should not diagnose errors.  */
  bool quiet () const
  {
    return !(complain & tf_warning_or_error);
  }

  /* True if we should diagnose errors.  */
  bool noisy () const
  {
    return !quiet ();
  }

  tsubst_flags_t complain;
  tree in_decl;
};

/* Provides additional context for satisfaction.

   During satisfaction:
    - The flag noisy() controls whether to diagnose ill-formed satisfaction,
      such as the satisfaction value of an atom being non-bool or non-constant.
    - The flag diagnose_unsatisfaction_p() controls whether to additionally
      explain why a constraint is not satisfied.
    - We enter satisfaction with noisy+unsat from diagnose_constraints.
    - We enter satisfaction with noisy-unsat from the replay inside
      constraint_satisfaction_value.
    - We enter satisfaction quietly (both flags cleared) from
      constraints_satisfied_p.

   During evaluation of a requires-expression:
    - The flag noisy() controls whether to diagnose ill-formed types and
      expressions inside its requirements.
    - The flag diagnose_unsatisfaction_p() controls whether to additionally
      explain why the requires-expression evaluates to false.
    - We enter tsubst_requires_expr with noisy+unsat from
      diagnose_atomic_constraint and potentially from
      satisfy_nondeclaration_constraints.
    - We enter tsubst_requires_expr with noisy-unsat from
      cp_parser_requires_expression when processing a requires-expression that
      appears outside a template.
    - We enter tsubst_requires_expr quietly (both flags cleared) when
      substituting through a requires-expression as part of template
      instantiation.  */

struct sat_info : subst_info
{
  sat_info (tsubst_flags_t cmp, tree in, bool diag_unsat = false)
    : subst_info (cmp, in), diagnose_unsatisfaction (diag_unsat)
  {
    if (diagnose_unsatisfaction_p ())
      gcc_checking_assert (noisy ());
  }

  /* True if we should diagnose the cause of satisfaction failure.
     Implies noisy().  */
  bool
  diagnose_unsatisfaction_p () const
  {
    return diagnose_unsatisfaction;
  }

  bool diagnose_unsatisfaction;
};

static tree constraint_satisfaction_value (tree, tree, sat_info);

/* True if T is known to be some type other than bool.  Note that this
   is false for dependent types and errors.  */

static inline bool
known_non_bool_p (tree t)
{
  return (t && !WILDCARD_TYPE_P (t) && TREE_CODE (t) != BOOLEAN_TYPE);
}

static bool
check_constraint_atom (cp_expr expr)
{
  if (known_non_bool_p (TREE_TYPE (expr)))
    {
      error_at (expr.get_location (),
		"constraint expression does not have type %<bool%>");
      return false;
    }

  return true;
}

static bool
check_constraint_operands (location_t, cp_expr lhs, cp_expr rhs)
{
  return check_constraint_atom (lhs) && check_constraint_atom (rhs);
}

/* Validate the semantic properties of the constraint expression.  */

static cp_expr
finish_constraint_binary_op (location_t loc,
			     tree_code code,
			     cp_expr lhs,
			     cp_expr rhs)
{
  gcc_assert (processing_constraint_expression_p ());
  if (lhs == error_mark_node || rhs == error_mark_node)
    return error_mark_node;
  if (!check_constraint_operands (loc, lhs, rhs))
    return error_mark_node;
  cp_expr expr
    = build_min_nt_loc (loc, code, lhs.get_value (), rhs.get_value ());
  expr.set_range (lhs.get_start (), rhs.get_finish ());
  return expr;
}

cp_expr
finish_constraint_or_expr (location_t loc, cp_expr lhs, cp_expr rhs)
{
  return finish_constraint_binary_op (loc, TRUTH_ORIF_EXPR, lhs, rhs);
}

cp_expr
finish_constraint_and_expr (location_t loc, cp_expr lhs, cp_expr rhs)
{
  return finish_constraint_binary_op (loc, TRUTH_ANDIF_EXPR, lhs, rhs);
}

cp_expr
finish_constraint_primary_expr (cp_expr expr)
{
  if (expr == error_mark_node)
    return error_mark_node;
  if (!check_constraint_atom (expr))
    return cp_expr (error_mark_node, expr.get_location ());
  return expr;
}

/* Combine two constraint-expressions with a logical-and.  */

tree
combine_constraint_expressions (tree lhs, tree rhs)
{
  processing_constraint_expression_sentinel pce;
  if (!lhs)
    return rhs;
  if (!rhs)
    return lhs;
  /* Use UNKNOWN_LOCATION so write_template_args can tell the difference
     between this and a && the user wrote.  */
  return finish_constraint_and_expr (UNKNOWN_LOCATION, lhs, rhs);
}

/* Extract the TEMPLATE_DECL from a concept check.  */

tree
get_concept_check_template (tree t)
{
  gcc_assert (concept_check_p (t));
  return TREE_OPERAND (t, 0);
}

/*---------------------------------------------------------------------------
                       Expansion of concept definitions
---------------------------------------------------------------------------*/

/* Returns the definition of a concept.  */

static tree
get_concept_definition (tree decl)
{
  gcc_assert (TREE_CODE (decl) == CONCEPT_DECL);
  return DECL_INITIAL (decl);
}

/*---------------------------------------------------------------------------
		      Normalization of expressions

This set of functions will transform an expression into a constraint
in a sequence of steps.
---------------------------------------------------------------------------*/

void
debug_parameter_mapping (tree map)
{
  for (tree p = map; p; p = TREE_CHAIN (p))
    {
      tree parm = TREE_VALUE (p);
      tree arg = TREE_PURPOSE (p);
      if (TYPE_P (parm))
	verbatim ("MAP %qD TO %qT", TEMPLATE_TYPE_DECL (parm), arg);
      else
	verbatim ("MAP %qD TO %qE", TEMPLATE_PARM_DECL (parm), arg);
      // debug_tree (parm);
      // debug_tree (arg);
    }
}

void
debug_argument_list (tree args)
{
  for (int i = 0; i < TREE_VEC_LENGTH (args); ++i)
    {
      tree arg = TREE_VEC_ELT (args, i);
      if (TYPE_P (arg))
	verbatim ("argument %qT", arg);
      else
	verbatim ("argument %qE", arg);
    }
}

/* Associate each parameter in PARMS with its corresponding template
   argument in ARGS.  */

static tree
map_arguments (tree parms, tree args)
{
  for (tree p = parms; p; p = TREE_CHAIN (p))
    if (args)
      {
	int level;
	int index;
	template_parm_level_and_index (TREE_VALUE (p), &level, &index);
	TREE_PURPOSE (p) = TMPL_ARG (args, level, index);
      }
    else
      TREE_PURPOSE (p) = template_parm_to_arg (p);

  return parms;
}

/* Build the parameter mapping for EXPR using ARGS, where CTX_PARMS
   are the template parameters in scope for EXPR.  */

static tree
build_parameter_mapping (tree expr, tree args, tree ctx_parms)
{
  tree parms = find_template_parameters (expr, ctx_parms);
  tree map = map_arguments (parms, args);
  return map;
}

/* True if the parameter mappings of two atomic constraints formed
   from the same expression are equivalent.  */

static bool
parameter_mapping_equivalent_p (tree t1, tree t2)
{
  tree map1 = ATOMIC_CONSTR_MAP (t1);
  tree map2 = ATOMIC_CONSTR_MAP (t2);
  while (map1 && map2)
    {
      gcc_checking_assert (TREE_VALUE (map1) == TREE_VALUE (map2));
      tree arg1 = TREE_PURPOSE (map1);
      tree arg2 = TREE_PURPOSE (map2);
      if (!template_args_equal (arg1, arg2))
        return false;
      map1 = TREE_CHAIN (map1);
      map2 = TREE_CHAIN (map2);
    }
  gcc_checking_assert (!map1 && !map2);
  return true;
}

/* Provides additional context for normalization.  */

struct norm_info : subst_info
{
  explicit norm_info (bool diag)
    : norm_info (NULL_TREE, diag)
  {}

  /* Construct a top-level context for DECL.  */

  norm_info (tree in_decl, bool diag)
    : subst_info (tf_warning_or_error, in_decl),
      generate_diagnostics (diag)
  {
    if (in_decl)
      {
	initial_parms = DECL_TEMPLATE_PARMS (in_decl);
	if (generate_diagnostics)
	  context = build_tree_list (NULL_TREE, in_decl);
      }
    else
      initial_parms = current_template_parms;
  }

  void update_context (tree expr, tree args)
  {
    if (generate_diagnostics)
      {
	tree map = build_parameter_mapping (expr, args, ctx_parms ());
	context = tree_cons (map, expr, context);
      }
    in_decl = get_concept_check_template (expr);
  }

  /* Returns the template parameters that are in scope for the current
     normalization context.  */

  tree ctx_parms ()
  {
    if (in_decl)
      return DECL_TEMPLATE_PARMS (in_decl);
    else
      return initial_parms;
  }

  /* Provides information about the source of a constraint.  This is a
     TREE_LIST whose VALUE is either a concept check or a constrained
     declaration.  The PURPOSE, for concept checks is a parameter mapping
     for that check.  */

  tree context = NULL_TREE;

  /* The declaration whose constraints we're normalizing.  The targets
     of the parameter mapping of each atom will be in terms of the
     template parameters of ORIG_DECL.  */

  tree initial_parms = NULL_TREE;

  /* Whether to build diagnostic information during normalization.  */

  bool generate_diagnostics;
};

static tree normalize_expression (tree, tree, norm_info);

/* Transform a logical-or or logical-and expression into either
   a conjunction or disjunction.  */

static tree
normalize_logical_operation (tree t, tree args, tree_code c, norm_info info)
{
  tree t0 = normalize_expression (TREE_OPERAND (t, 0), args, info);
  tree t1 = normalize_expression (TREE_OPERAND (t, 1), args, info);

  /* Build a new info object for the constraint.  */
  tree ci = (info.generate_diagnostics
	     ? build_tree_list (t, info.context) : NULL_TREE);

  return build2 (c, ci, t0, t1);
}

/* Data types and hash functions for caching the normal form of a concept-id.
   This essentially memoizes calls to normalize_concept_check.  */

struct GTY((for_user)) norm_entry
{
  /* The CONCEPT_DECL of the concept-id.  */
  tree tmpl;
  /* The arguments of the concept-id.  */
  tree args;
  /* The normal form of the concept-id.  */
  tree norm;
};

struct norm_hasher : ggc_ptr_hash<norm_entry>
{
  static hashval_t hash (norm_entry *e)
  {
    ++comparing_specializations;
    hashval_t val = iterative_hash_template_arg (e->tmpl, 0);
    val = iterative_hash_template_arg (e->args, val);
    --comparing_specializations;
    return val;
  }

  static bool equal (norm_entry *e1, norm_entry *e2)
  {
    ++comparing_specializations;
    bool eq = e1->tmpl == e2->tmpl
      && template_args_equal (e1->args, e2->args);
    --comparing_specializations;
    return eq;
  }
};

static GTY((deletable)) hash_table<norm_hasher> *norm_cache;

/* Normalize the concept check CHECK where ARGS are the
   arguments to be substituted into CHECK's arguments.  */

static tree
normalize_concept_check (tree check, tree args, norm_info info)
{
  gcc_assert (concept_check_p (check));
  tree tmpl = TREE_OPERAND (check, 0);
  tree targs = TREE_OPERAND (check, 1);

  /* Substitute through the arguments of the concept check.  */
  if (args)
    targs = tsubst_template_args (targs, args, info.complain, info.in_decl);
  if (targs == error_mark_node)
    return error_mark_node;
  if (template_args_equal (targs, generic_targs_for (tmpl)))
    /* Canonicalize generic arguments as NULL_TREE, as an optimization.  */
    targs = NULL_TREE;

  /* Build the substitution for the concept definition.  */
  tree parms = TREE_VALUE (DECL_TEMPLATE_PARMS (tmpl));
  if (targs && args)
    /* As an optimization, coerce the arguments only if necessary
       (i.e. if they were substituted).  */
    targs = coerce_template_parms (parms, targs, tmpl, tf_none);
  if (targs == error_mark_node)
    return error_mark_node;

  if (!norm_cache)
    norm_cache = hash_table<norm_hasher>::create_ggc (31);
  norm_entry *entry = nullptr;
  if (!info.generate_diagnostics)
    {
      /* Cache the normal form of the substituted concept-id (when not
	 diagnosing).  */
      norm_entry elt = {tmpl, targs, NULL_TREE};
      norm_entry **slot = norm_cache->find_slot (&elt, INSERT);
      if (*slot)
	return (*slot)->norm;
      entry = ggc_alloc<norm_entry> ();
      *entry = elt;
      *slot = entry;
    }

  tree def = get_concept_definition (DECL_TEMPLATE_RESULT (tmpl));
  info.update_context (check, args);
  tree norm = normalize_expression (def, targs, info);
  if (entry)
    entry->norm = norm;
  return norm;
}

/* A structural hasher for ATOMIC_CONSTRs.  */

struct atom_hasher : default_hash_traits<tree>
{
  static hashval_t hash (tree t)
  {
    ++comparing_specializations;
    hashval_t val = hash_atomic_constraint (t);
    --comparing_specializations;
    return val;
  }

  static bool equal (tree t1, tree t2)
  {
    ++comparing_specializations;
    bool eq = atomic_constraints_identical_p (t1, t2);
    --comparing_specializations;
    return eq;
  }
};

/* Used by normalize_atom to cache ATOMIC_CONSTRs.  */

static GTY((deletable)) hash_table<atom_hasher> *atom_cache;

/* The normal form of an atom is an atomic constraint.  */

static tree
normalize_atom (tree t, tree args, norm_info info)
{
  /* Concept checks are not atomic.  */
  if (concept_check_p (t))
    return normalize_concept_check (t, args, info);

  /* Build the parameter mapping for the atom.  */
  tree map = build_parameter_mapping (t, args, info.ctx_parms ());

  /* Build a new info object for the atom.  */
  tree ci = build_tree_list (t, info.context);

  tree atom = build1 (ATOMIC_CONSTR, ci, map);

  /* Remember whether the expression of this atomic constraint belongs to
     a concept definition by inspecting in_decl, which should always be set
     in this case either by norm_info::update_context (when recursing into a
     concept-id during normalization) or by normalize_concept_definition
     (when starting out with a concept-id).  */
  if (info.in_decl && concept_definition_p (info.in_decl))
    ATOMIC_CONSTR_EXPR_FROM_CONCEPT_P (atom) = true;

  if (!info.generate_diagnostics)
    {
      /* Cache the ATOMIC_CONSTRs that we return, so that sat_hasher::equal
	 later can cheaply compare two atoms using just pointer equality.  */
      if (!atom_cache)
	atom_cache = hash_table<atom_hasher>::create_ggc (31);
      tree *slot = atom_cache->find_slot (atom, INSERT);
      if (*slot)
	return *slot;

      /* Find all template parameters used in the targets of the parameter
	 mapping, and store a list of them in the TREE_TYPE of the mapping.
	 This list will be used by sat_hasher to determine the subset of
	 supplied template arguments that the satisfaction value of the atom
	 depends on.  */
      if (map)
	{
	  tree targets = make_tree_vec (list_length (map));
	  int i = 0;
	  for (tree node = map; node; node = TREE_CHAIN (node))
	    {
	      tree target = TREE_PURPOSE (node);
	      TREE_VEC_ELT (targets, i++) = target;
	    }
	  tree target_parms = find_template_parameters (targets,
							info.initial_parms);
	  TREE_TYPE (map) = target_parms;
	}

      *slot = atom;
    }
  return atom;
}

/* Returns the normal form of an expression.  */

static tree
normalize_expression (tree t, tree args, norm_info info)
{
  if (!t)
    return NULL_TREE;

  if (t == error_mark_node)
    return error_mark_node;

  switch (TREE_CODE (t))
    {
    case TRUTH_ANDIF_EXPR:
      return normalize_logical_operation (t, args, CONJ_CONSTR, info);
    case TRUTH_ORIF_EXPR:
      return normalize_logical_operation (t, args, DISJ_CONSTR, info);
    default:
      return normalize_atom (t, args, info);
    }
}

/* Cache of the normalized form of constraints.  Marked as deletable because it
   can all be recalculated.  */
static GTY((deletable)) hash_map<tree,tree> *normalized_map;

static tree
get_normalized_constraints (tree t, norm_info info)
{
  auto_timevar time (TV_CONSTRAINT_NORM);
  return normalize_expression (t, NULL_TREE, info);
}

/* Returns the normalized constraints from a constraint-info object
   or NULL_TREE if the constraints are null.  IN_DECL provides the
   declaration to which the constraints belong.  */

static tree
get_normalized_constraints_from_info (tree ci, tree in_decl, bool diag = false)
{
  if (ci == NULL_TREE)
    return NULL_TREE;

  /* Substitution errors during normalization are fatal.  */
  ++processing_template_decl;
  norm_info info (in_decl, diag);
  tree t = get_normalized_constraints (CI_ASSOCIATED_CONSTRAINTS (ci), info);
  --processing_template_decl;

  return t;
}

/* Returns the normalized constraints for the declaration D.  */

static tree
get_normalized_constraints_from_decl (tree d, bool diag = false)
{
  tree tmpl;
  tree decl;

  /* For inherited constructors, consider the original declaration;
     it has the correct template information attached.  */
  d = strip_inheriting_ctors (d);

  if (regenerated_lambda_fn_p (d))
    {
      /* If this lambda was regenerated, DECL_TEMPLATE_PARMS doesn't contain
	 all in-scope template parameters, but the lambda from which it was
	 ultimately regenerated does, so use that instead.  */
      tree lambda = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (d));
      lambda = most_general_lambda (lambda);
      d = lambda_function (lambda);
    }

  if (TREE_CODE (d) == TEMPLATE_DECL)
    {
      tmpl = d;
      decl = DECL_TEMPLATE_RESULT (tmpl);
    }
  else
    {
      if (tree ti = DECL_TEMPLATE_INFO (d))
	tmpl = TI_TEMPLATE (ti);
      else
	tmpl = NULL_TREE;
      decl = d;
    }

  /* Get the most general template for the declaration, and compute
     arguments from that.  This ensures that the arguments used for
     normalization are always template parameters and not arguments
     used for outer specializations.  For example:

        template<typename T>
        struct S {
	  template<typename U> requires C<T, U> void f(U);
        };

        S<int>::f(0);

     When we normalize the requirements for S<int>::f, we want the
     arguments to be {T, U}, not {int, U}.  One reason for this is that
     accepting the latter causes the template parameter level of U
     to be reduced in a way that makes it overly difficult substitute
     concrete arguments (i.e., eventually {int, int} during satisfaction.  */
  if (tmpl)
  {
    if (DECL_LANG_SPECIFIC (tmpl) && !DECL_TEMPLATE_SPECIALIZATION (tmpl))
      tmpl = most_general_template (tmpl);
  }

  d = tmpl ? tmpl : decl;

  /* If we're not diagnosing errors, use cached constraints, if any.  */
  if (!diag)
    if (tree *p = hash_map_safe_get (normalized_map, d))
      return *p;

  tree norm = NULL_TREE;
  if (tree ci = get_constraints (d))
    {
      push_access_scope_guard pas (decl);
      norm = get_normalized_constraints_from_info (ci, tmpl, diag);
    }

  if (!diag)
    hash_map_safe_put<hm_ggc> (normalized_map, d, norm);

  return norm;
}

/* Returns the normal form of TMPL's definition.  */

static tree
normalize_concept_definition (tree tmpl, bool diag)
{
  if (!norm_cache)
    norm_cache = hash_table<norm_hasher>::create_ggc (31);
  norm_entry entry = {tmpl, NULL_TREE, NULL_TREE};

  if (!diag)
    if (norm_entry *found = norm_cache->find (&entry))
      return found->norm;

  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);
  tree def = get_concept_definition (DECL_TEMPLATE_RESULT (tmpl));
  ++processing_template_decl;
  norm_info info (tmpl, diag);
  tree norm = get_normalized_constraints (def, info);
  --processing_template_decl;

  if (!diag)
    {
      norm_entry **slot = norm_cache->find_slot (&entry, INSERT);
      entry.norm = norm;
      *slot = ggc_alloc<norm_entry> ();
      **slot = entry;
    }

  return norm;
}

/* Normalize an EXPR as a constraint.  */

static tree
normalize_constraint_expression (tree expr, norm_info info)
{
  if (!expr || expr == error_mark_node)
    return expr;

  if (!info.generate_diagnostics)
    if (tree *p = hash_map_safe_get (normalized_map, expr))
      return *p;

  ++processing_template_decl;
  tree norm = get_normalized_constraints (expr, info);
  --processing_template_decl;

  if (!info.generate_diagnostics)
    hash_map_safe_put<hm_ggc> (normalized_map, expr, norm);

  return norm;
}

/* 17.4.1.2p2.  Two constraints are identical if they are formed
   from the same expression and the targets of the parameter mapping
   are equivalent.  */

bool
atomic_constraints_identical_p (tree t1, tree t2)
{
  gcc_assert (TREE_CODE (t1) == ATOMIC_CONSTR);
  gcc_assert (TREE_CODE (t2) == ATOMIC_CONSTR);

  if (ATOMIC_CONSTR_EXPR (t1) != ATOMIC_CONSTR_EXPR (t2))
    return false;

  if (!parameter_mapping_equivalent_p (t1, t2))
    return false;

  return true;
}

/* True if T1 and T2 are equivalent, meaning they have the same syntactic
   structure and all corresponding constraints are identical.  */

bool
constraints_equivalent_p (tree t1, tree t2)
{
  gcc_assert (CONSTR_P (t1));
  gcc_assert (CONSTR_P (t2));

  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  switch (TREE_CODE (t1))
    {
    case CONJ_CONSTR:
    case DISJ_CONSTR:
      if (!constraints_equivalent_p (TREE_OPERAND (t1, 0),
				     TREE_OPERAND (t2, 0)))
	return false;
      if (!constraints_equivalent_p (TREE_OPERAND (t1, 1),
				     TREE_OPERAND (t2, 1)))
	return false;
      break;
    case ATOMIC_CONSTR:
      if (!atomic_constraints_identical_p (t1, t2))
	return false;
      break;
    default:
      gcc_unreachable ();
    }
  return true;
}

/* Compute the hash value for T.  */

hashval_t
hash_atomic_constraint (tree t)
{
  gcc_assert (TREE_CODE (t) == ATOMIC_CONSTR);

  /* Hash the identity of the expression.  */
  hashval_t val = htab_hash_pointer (ATOMIC_CONSTR_EXPR (t));

  /* Hash the targets of the parameter map.  */
  tree p = ATOMIC_CONSTR_MAP (t);
  while (p)
    {
      val = iterative_hash_template_arg (TREE_PURPOSE (p), val);
      p = TREE_CHAIN (p);
    }

  return val;
}

namespace inchash
{

static void
add_constraint (tree t, hash& h)
{
  h.add_int (TREE_CODE (t));
  switch (TREE_CODE (t))
    {
    case CONJ_CONSTR:
    case DISJ_CONSTR:
      add_constraint (TREE_OPERAND (t, 0), h);
      add_constraint (TREE_OPERAND (t, 1), h);
      break;
    case ATOMIC_CONSTR:
      h.merge_hash (hash_atomic_constraint (t));
      break;
    default:
      gcc_unreachable ();
    }
}

}

/* Computes a hash code for the constraint T.  */

hashval_t
iterative_hash_constraint (tree t, hashval_t val)
{
  gcc_assert (CONSTR_P (t));
  inchash::hash h (val);
  inchash::add_constraint (t, h);
  return h.end ();
}

// -------------------------------------------------------------------------- //
// Constraint Semantic Processing
//
// The following functions are called by the parser and substitution rules
// to create and evaluate constraint-related nodes.

// The constraints associated with the current template parameters.
tree
current_template_constraints (void)
{
  if (!current_template_parms)
    return NULL_TREE;
  tree tmpl_constr = TEMPLATE_PARMS_CONSTRAINTS (current_template_parms);
  return build_constraints (tmpl_constr, NULL_TREE);
}

/* If the recently parsed TYPE declares or defines a template or
   template specialization, get its corresponding constraints from the
   current template parameters and bind them to TYPE's declaration.  */

tree
associate_classtype_constraints (tree type)
{
  if (!type || type == error_mark_node || !CLASS_TYPE_P (type))
    return type;

  /* An explicit class template specialization has no template parameters.  */
  if (!current_template_parms)
    return type;

  if (CLASSTYPE_IS_TEMPLATE (type) || CLASSTYPE_TEMPLATE_SPECIALIZATION (type))
    {
      tree decl = TYPE_STUB_DECL (type);
      tree ci = current_template_constraints ();

      /* An implicitly instantiated member template declaration already
	 has associated constraints.  If it is defined outside of its
	 class, then we need match these constraints against those of
	 original declaration.  */
      if (tree orig_ci = get_constraints (decl))
        {
	  if (int extra_levels = (TMPL_PARMS_DEPTH (current_template_parms)
				  - TMPL_ARGS_DEPTH (TYPE_TI_ARGS (type))))
	    {
	      /* If there is a discrepancy between the current template depth
		 and the template depth of the original declaration, then we
		 must be redeclaring a class template as part of a friend
		 declaration within another class template.  Before matching
		 constraints, we need to reduce the template parameter level
		 within the current constraints via substitution.  */
	      tree outer_gtargs = template_parms_to_args (current_template_parms);
	      TREE_VEC_LENGTH (outer_gtargs) = extra_levels;
	      ci = tsubst_constraint_info (ci, outer_gtargs, tf_none, NULL_TREE);
	    }
          if (!equivalent_constraints (ci, orig_ci))
            {
	      auto_diagnostic_group d;
	      error ("%qT does not match original declaration", type);
	      tree tmpl = CLASSTYPE_TI_TEMPLATE (type);
	      location_t loc = DECL_SOURCE_LOCATION (tmpl);
	      inform (loc, "original template declaration here");
	      /* Fall through, so that we define the type anyway.  */
            }
          return type;
        }
      set_constraints (decl, ci);
    }
  return type;
}

/* Create an empty constraint info block.  */

static inline tree_constraint_info*
build_constraint_info ()
{
  return (tree_constraint_info *)make_node (CONSTRAINT_INFO);
}

/* Build a constraint-info object that contains the associated constraints
   of a declaration.  This also includes the declaration's template
   requirements (TREQS) and any trailing requirements for a function
   declarator (DREQS).  Note that both TREQS and DREQS must be constraints.

   If the declaration has neither template nor declaration requirements
   this returns NULL_TREE, indicating an unconstrained declaration.  */

tree
build_constraints (tree tr, tree dr)
{
  if (!tr && !dr)
    return NULL_TREE;

  tree_constraint_info* ci = build_constraint_info ();
  ci->template_reqs = tr;
  ci->declarator_reqs = dr;
  ci->associated_constr = combine_constraint_expressions (tr, dr);

  return (tree)ci;
}

/* Add constraint RHS to the end of CONSTRAINT_INFO ci.  */

tree
append_constraint (tree ci, tree rhs)
{
  tree tr = ci ? CI_TEMPLATE_REQS (ci) : NULL_TREE;
  tree dr = ci ? CI_DECLARATOR_REQS (ci) : NULL_TREE;
  dr = combine_constraint_expressions (dr, rhs);
  if (ci)
    {
      CI_DECLARATOR_REQS (ci) = dr;
      tree ac = combine_constraint_expressions (tr, dr);
      CI_ASSOCIATED_CONSTRAINTS (ci) = ac;
    }
  else
    ci = build_constraints (tr, dr);
  return ci;
}

/* A mapping from declarations to constraint information.  */

static GTY ((cache)) decl_tree_cache_map *decl_constraints;

/* Returns the template constraints of declaration T.  If T is not
   constrained, return NULL_TREE.  Note that T must be non-null.  */

tree
get_constraints (const_tree t)
{
  if (!flag_concepts)
    return NULL_TREE;
  if (!decl_constraints)
    return NULL_TREE;

  gcc_assert (DECL_P (t));
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);
  tree* found = decl_constraints->get (CONST_CAST_TREE (t));
  if (found)
    return *found;
  else
    return NULL_TREE;
}

/* Associate the given constraint information CI with the declaration
   T.  If T is a template, then the constraints are associated with
   its underlying declaration.  Don't build associations if CI is
   NULL_TREE.  */

void
set_constraints (tree t, tree ci)
{
  if (!ci)
    return;
  gcc_assert (t && flag_concepts);
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);
  bool found = hash_map_safe_put<hm_ggc> (decl_constraints, t, ci);
  gcc_assert (!found);
}

/* Remove the associated constraints of the declaration T.  */

void
remove_constraints (tree t)
{
  gcc_checking_assert (DECL_P (t));
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  if (decl_constraints)
    decl_constraints->remove (t);
}

/* If DECL is a friend, substitute into REQS to produce requirements suitable
   for declaration matching.  */

tree
maybe_substitute_reqs_for (tree reqs, const_tree decl)
{
  if (reqs == NULL_TREE)
    return NULL_TREE;

  decl = STRIP_TEMPLATE (decl);
  if (DECL_UNIQUE_FRIEND_P (decl) && DECL_TEMPLATE_INFO (decl))
    {
      tree tmpl = DECL_TI_TEMPLATE (decl);
      tree outer_args = outer_template_args (decl);
      processing_template_decl_sentinel s;
      if (PRIMARY_TEMPLATE_P (tmpl)
	  || uses_template_parms (outer_args))
	++processing_template_decl;
      reqs = tsubst_constraint (reqs, outer_args,
				tf_warning_or_error, NULL_TREE);
    }
  return reqs;
}

/* Returns the trailing requires clause of the declarator of
   a template declaration T or NULL_TREE if none.  */

tree
get_trailing_function_requirements (tree t)
{
  tree ci = get_constraints (t);
  if (!ci)
    return NULL_TREE;
  return CI_DECLARATOR_REQS (ci);
}

/* Construct a sequence of template arguments by prepending
   ARG to REST.  Either ARG or REST may be null.  */

static tree
build_concept_check_arguments (tree arg, tree rest)
{
  gcc_assert (!rest || TREE_CODE (rest) == TREE_VEC);
  tree args;
  if (arg)
    {
      int n = rest ? TREE_VEC_LENGTH (rest) : 0;
      args = make_tree_vec (n + 1);
      TREE_VEC_ELT (args, 0) = arg;
      if (rest)
        for (int i = 0; i < n; ++i)
          TREE_VEC_ELT (args, i + 1) = TREE_VEC_ELT (rest, i);
      int def = rest ? GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (rest) : 0;
      SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (args, def + 1);
    }
  else
    args = rest;
  return args;
}

/* Construct an expression that checks TMPL using ARGS.  */

tree
build_concept_check (tree tmpl, tree args, tsubst_flags_t complain)
{
  return build_concept_check (tmpl, NULL_TREE, args, complain);
}

/* Construct an expression that checks the concept given by TMPL.  */

tree
build_concept_check (tree tmpl, tree arg, tree rest, tsubst_flags_t complain)
{
  if (TREE_DEPRECATED (DECL_TEMPLATE_RESULT (tmpl)))
    warn_deprecated_use (DECL_TEMPLATE_RESULT (tmpl), NULL_TREE);

  tree parms = DECL_INNERMOST_TEMPLATE_PARMS (tmpl);
  tree args = build_concept_check_arguments (arg, rest);
  args = coerce_template_parms (parms, args, tmpl, complain);
  if (args == error_mark_node)
    return error_mark_node;
  return build2 (TEMPLATE_ID_EXPR, boolean_type_node, tmpl, args);
}

/* Build a template-id that can participate in a concept check.  */

static tree
build_concept_id (tree decl, tree args)
{
  return build_concept_check (decl, args, tf_warning_or_error);
}

/* Build a template-id that can participate in a concept check, preserving
   the source location of the original template-id.  */

tree
build_concept_id (tree expr)
{
  gcc_assert (TREE_CODE (expr) == TEMPLATE_ID_EXPR);
  tree id = build_concept_id (TREE_OPERAND (expr, 0), TREE_OPERAND (expr, 1));
  protected_set_expr_location (id, cp_expr_location (expr));
  return id;
}

/* Build as template-id with a placeholder that can be used as a
   type constraint.

   Note that this will diagnose errors if the initial concept check
   cannot be built.  */

tree
build_type_constraint (tree decl, tree args, tsubst_flags_t complain)
{
  tree proto = template_parm_to_arg (concept_prototype_parameter (decl));
  ++processing_template_decl;
  tree check = build_concept_check (decl, proto, args, complain);
  --processing_template_decl;
  return check;
}

/* Returns a TYPE_DECL that contains sufficient information to
   build a template parameter of the same kind as PROTO and
   constrained by the concept declaration CNC.  Note that PROTO
   is the first template parameter of CNC.

   If specified, ARGS provides additional arguments to the
   constraint check.  */
tree
build_constrained_parameter (tree cnc, tree proto, tree args)
{
  tree name = DECL_NAME (cnc);
  tree type = TREE_TYPE (proto);
  tree decl = build_decl (input_location, TYPE_DECL, name, type);
  CONSTRAINED_PARM_PROTOTYPE (decl) = proto;
  CONSTRAINED_PARM_CONCEPT (decl) = cnc;
  CONSTRAINED_PARM_EXTRA_ARGS (decl) = args;
  return decl;
}

/* Create a constraint expression for the given DECL that evaluates the
   requirements specified by CONSTR, a TYPE_DECL that contains all the
   information necessary to build the requirements (see finish_concept_name
   for the layout of that TYPE_DECL).

   Note that the constraints are neither reduced nor decomposed.  That is
   done only after the requires clause has been parsed (or not).  */

tree
finish_shorthand_constraint (tree decl, tree constr)
{
  /* No requirements means no constraints.  */
  if (!constr)
    return NULL_TREE;

  if (error_operand_p (constr))
    return NULL_TREE;

  tree proto = CONSTRAINED_PARM_PROTOTYPE (constr);
  tree con = CONSTRAINED_PARM_CONCEPT (constr);
  tree args = CONSTRAINED_PARM_EXTRA_ARGS (constr);

  bool variadic_concept_p = template_parameter_pack_p (proto);
  bool declared_pack_p = template_parameter_pack_p (decl);
  bool apply_to_each_p = (cxx_dialect >= cxx20) ? true : !variadic_concept_p;

  /* Get the argument and overload used for the requirement
     and adjust it if we're going to expand later.  */
  tree arg = template_parm_to_arg (decl);
  if (apply_to_each_p && declared_pack_p)
    arg = PACK_EXPANSION_PATTERN (TREE_VEC_ELT (ARGUMENT_PACK_ARGS (arg), 0));

  /* Build the concept constraint-expression.  */
  tree tmpl = DECL_TI_TEMPLATE (con);
  tree check = build_concept_check (tmpl, arg, args, tf_warning_or_error);

  /* Make the check a fold-expression if needed.
     Use UNKNOWN_LOCATION so write_template_args can tell the
     difference between this and a fold the user wrote.  */
  if (apply_to_each_p && declared_pack_p)
    check = finish_left_unary_fold_expr (UNKNOWN_LOCATION,
					 check, TRUTH_ANDIF_EXPR);

  return check;
}

/* Returns a conjunction of shorthand requirements for the template
   parameter list PARMS.  Note that the requirements are stored in
   the TYPE of each tree node.  */

tree
get_shorthand_constraints (tree parms)
{
  tree result = NULL_TREE;
  parms = INNERMOST_TEMPLATE_PARMS (parms);
  for (int i = 0; i < TREE_VEC_LENGTH (parms); ++i)
    {
      tree parm = TREE_VEC_ELT (parms, i);
      tree constr = TEMPLATE_PARM_CONSTRAINTS (parm);
      result = combine_constraint_expressions (result, constr);
    }
  return result;
}

/* Returns true iff the placeholders C1 and C2 are equivalent.  C1
   and C2 can be either TEMPLATE_TYPE_PARM or template-ids.  */

bool
equivalent_placeholder_constraints (tree c1, tree c2)
{
  if (c1 && TREE_CODE (c1) == TEMPLATE_TYPE_PARM)
    /* A constrained auto.  */
    c1 = PLACEHOLDER_TYPE_CONSTRAINTS (c1);
  if (c2 && TREE_CODE (c2) == TEMPLATE_TYPE_PARM)
    c2 = PLACEHOLDER_TYPE_CONSTRAINTS (c2);

  if (c1 == c2)
    return true;
  if (!c1 || !c2)
    return false;
  if (c1 == error_mark_node || c2 == error_mark_node)
    /* We get here during satisfaction; when a deduction constraint
       fails, substitution can produce an error_mark_node for the
       placeholder constraints.  */
    return false;

  gcc_assert (concept_check_p (c1) && concept_check_p (c2));
  tree t1 = TREE_OPERAND (c1, 0);
  tree a1 = TREE_OPERAND (c1, 1);
  tree t2 = TREE_OPERAND (c2, 0);
  tree a2 = TREE_OPERAND (c2, 1);

  if (t1 != t2)
    return false;

  int len1 = TREE_VEC_LENGTH (a1);
  int len2 = TREE_VEC_LENGTH (a2);
  if (len1 != len2)
    return false;

  /* Skip the first argument so we don't infinitely recurse.
     Also, they may differ in template parameter index.  */
  for (int i = 1; i < len1; ++i)
    if (!template_args_equal (TREE_VEC_ELT (a1, i),
			      TREE_VEC_ELT (a2, i)))
      return false;
  return true;
}

/* Return a hash value for the placeholder ATOMIC_CONSTR C.  */

hashval_t
iterative_hash_placeholder_constraint (tree c, hashval_t val)
{
  gcc_assert (concept_check_p (c));
  tree t = TREE_OPERAND (c, 0);
  tree a = TREE_OPERAND (c, 1);

  /* Like hash_tmpl_and_args, but skip the first argument.  */
  val = iterative_hash_object (DECL_UID (t), val);

  for (int i = TREE_VEC_LENGTH (a)-1; i > 0; --i)
    val = iterative_hash_template_arg (TREE_VEC_ELT (a, i), val);

  return val;
}

/* Substitute through the expression of a simple requirement or
   compound requirement.  */

static tree
tsubst_valid_expression_requirement (tree t, tree args, sat_info info)
{
  tsubst_flags_t quiet = info.complain & ~tf_warning_or_error;
  tree r = tsubst_expr (t, args, quiet, info.in_decl);
  if (convert_to_void (r, ICV_STATEMENT, quiet) != error_mark_node)
    return r;

  if (info.diagnose_unsatisfaction_p ())
    {
      location_t loc = cp_expr_loc_or_input_loc (t);
      if (diagnosing_failed_constraint::replay_errors_p ())
	{
	  inform (loc, "the required expression %qE is invalid, because", t);
	  auto_diagnostic_nesting_level sentinel;
	  if (r == error_mark_node)
	    tsubst_expr (t, args, info.complain, info.in_decl);
	  else
	    convert_to_void (r, ICV_STATEMENT, info.complain);
	}
      else
	inform (loc, "the required expression %qE is invalid", t);
    }
  else if (info.noisy ())
    {
      r = tsubst_expr (t, args, info.complain, info.in_decl);
      convert_to_void (r, ICV_STATEMENT, info.complain);
    }

  return error_mark_node;
}


/* Substitute through the simple requirement.  */

static tree
tsubst_simple_requirement (tree t, tree args, sat_info info)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree expr = tsubst_valid_expression_requirement (t0, args, info);
  if (expr == error_mark_node)
    return error_mark_node;
  if (processing_template_decl)
    return finish_simple_requirement (EXPR_LOCATION (t), expr);
  return boolean_true_node;
}

/* Subroutine of tsubst_type_requirement that performs the actual substitution
   and diagnosing.  Also used by tsubst_compound_requirement.  */

static tree
tsubst_type_requirement_1 (tree t, tree args, sat_info info, location_t loc)
{
  tsubst_flags_t quiet = info.complain & ~tf_warning_or_error;
  tree r = tsubst (t, args, quiet, info.in_decl);
  if (r != error_mark_node)
    return r;

  if (info.diagnose_unsatisfaction_p ())
    {
      if (diagnosing_failed_constraint::replay_errors_p ())
	{
	  /* Replay the substitution error.  */
	  inform (loc, "the required type %qT is invalid, because", t);
	  tsubst (t, args, info.complain, info.in_decl);
	}
      else
	inform (loc, "the required type %qT is invalid", t);
    }
  else if (info.noisy ())
    tsubst (t, args, info.complain, info.in_decl);

  return error_mark_node;
}


/* Substitute through the type requirement.  */

static tree
tsubst_type_requirement (tree t, tree args, sat_info info)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree type = tsubst_type_requirement_1 (t0, args, info, EXPR_LOCATION (t));
  if (type == error_mark_node)
    return error_mark_node;
  if (processing_template_decl)
    return finish_type_requirement (EXPR_LOCATION (t), type);
  return boolean_true_node;
}

/* True if TYPE can be deduced from EXPR.  */

static bool
type_deducible_p (tree expr, tree type, tree placeholder, tree args,
                  subst_info info)
{
  /* Make sure deduction is performed against ( EXPR ), so that
     references are preserved in the result.  */
  expr = force_paren_expr_uneval (expr);

  tree deduced_type = do_auto_deduction (type, expr, placeholder,
					 info.complain, adc_requirement,
					 /*outer_targs=*/args);

  return deduced_type != error_mark_node;
}

/* True if EXPR can not be converted to TYPE.  */

static bool
expression_convertible_p (tree expr, tree type, subst_info info)
{
  tree conv =
    perform_direct_initialization_if_possible (type, expr, false,
					       info.complain);
  if (conv == error_mark_node)
    return false;
  if (conv == NULL_TREE)
    {
      if (info.complain & tf_error)
        {
          location_t loc = EXPR_LOC_OR_LOC (expr, input_location);
          error_at (loc, "cannot convert %qE to %qT", expr, type);
        }
      return false;
    }
  return true;
}


/* Substitute through the compound requirement.  */

static tree
tsubst_compound_requirement (tree t, tree args, sat_info info)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree t1 = TREE_OPERAND (t, 1);
  tree expr = tsubst_valid_expression_requirement (t0, args, info);
  if (expr == error_mark_node)
    return error_mark_node;

  location_t loc = cp_expr_loc_or_input_loc (expr);

  subst_info quiet (info.complain & ~tf_warning_or_error, info.in_decl);

  /* Check the noexcept condition.  */
  bool noexcept_p = COMPOUND_REQ_NOEXCEPT_P (t);
  if (noexcept_p && !processing_template_decl
      && !expr_noexcept_p (expr, quiet.complain))
    {
      if (info.diagnose_unsatisfaction_p ())
	inform (loc, "%qE is not %<noexcept%>", expr);
      else
	return error_mark_node;
    }

  /* Substitute through the type expression, if any.  */
  tree type = tsubst_type_requirement_1 (t1, args, info, EXPR_LOCATION (t));
  if (type == error_mark_node)
    return error_mark_node;

  /* Check expression against the result type.  */
  if (type && !processing_template_decl)
    {
      if (tree placeholder = type_uses_auto (type))
	{
	  if (!type_deducible_p (expr, type, placeholder, args, quiet))
	    {
	      if (info.diagnose_unsatisfaction_p ())
		{
		  if (diagnosing_failed_constraint::replay_errors_p ())
		    {
		      inform (loc,
			      "%qE does not satisfy return-type-requirement, "
			      "because", t0);
		      /* Further explain the reason for the error.  */
		      type_deducible_p (expr, type, placeholder, args, info);
		    }
		  else
		    inform (loc,
			    "%qE does not satisfy return-type-requirement", t0);
		}
	      return error_mark_node;
	    }
	}
      else if (!expression_convertible_p (expr, type, quiet))
	{
	  if (info.diagnose_unsatisfaction_p ())
	    {
	      if (diagnosing_failed_constraint::replay_errors_p ())
		{
		  inform (loc, "cannot convert %qE to %qT because", t0, type);
		  /* Further explain the reason for the error.  */
		  expression_convertible_p (expr, type, info);
		}
	      else
		inform (loc, "cannot convert %qE to %qT", t0, type);
	    }
	  return error_mark_node;
	}
    }

  if (processing_template_decl)
    return finish_compound_requirement (EXPR_LOCATION (t),
					expr, type, noexcept_p);
  return boolean_true_node;
}

/* Substitute through the nested requirement.  */

static tree
tsubst_nested_requirement (tree t, tree args, sat_info info)
{
  if (processing_template_decl)
    {
      tree req = TREE_OPERAND (t, 0);
      req = tsubst_constraint (req, args, info.complain, info.in_decl);
      if (req == error_mark_node)
	return error_mark_node;
      return finish_nested_requirement (EXPR_LOCATION (t), req);
    }

  sat_info quiet (info.complain & ~tf_warning_or_error, info.in_decl);
  tree result = constraint_satisfaction_value (t, args, quiet);
  if (result == boolean_true_node)
    return boolean_true_node;

  if (result == boolean_false_node
      && info.diagnose_unsatisfaction_p ())
    {
      tree expr = TREE_OPERAND (t, 0);
      location_t loc = cp_expr_location (t);
      if (diagnosing_failed_constraint::replay_errors_p ())
	{
	  /* Replay the substitution error.  */
	  inform (loc, "nested requirement %qE is not satisfied, because", expr);
	  constraint_satisfaction_value (t, args, info);
	}
      else
	inform (loc, "nested requirement %qE is not satisfied", expr);
    }

  return error_mark_node;
}

/* Substitute ARGS into the requirement T.  */

static tree
tsubst_requirement (tree t, tree args, sat_info info)
{
  iloc_sentinel loc_s (cp_expr_location (t));
  switch (TREE_CODE (t))
    {
    case SIMPLE_REQ:
      return tsubst_simple_requirement (t, args, info);
    case TYPE_REQ:
      return tsubst_type_requirement (t, args, info);
    case COMPOUND_REQ:
      return tsubst_compound_requirement (t, args, info);
    case NESTED_REQ:
      return tsubst_nested_requirement (t, args, info);
    default:
      break;
    }
  gcc_unreachable ();
}

static tree
declare_constraint_vars (tree parms, tree vars)
{
  tree s = vars;
  for (tree t = parms; t; t = DECL_CHAIN (t))
    {
      if (DECL_PACK_P (t))
        {
          tree pack = extract_fnparm_pack (t, &s);
          register_local_specialization (pack, t);
        }
      else
        {
          register_local_specialization (s, t);
          s = DECL_CHAIN (s);
        }
    }
  return vars;
}

/* Substitute through as if checking function parameter types.  This
   will diagnose common parameter type errors.  Returns error_mark_node
   if an error occurred.  */

static tree
check_constraint_variables (tree t, tree args, subst_info info)
{
  tree types = NULL_TREE;
  tree p = t;
  while (p && !VOID_TYPE_P (p))
    {
      types = tree_cons (NULL_TREE, TREE_TYPE (p), types);
      p = TREE_CHAIN (p);
    }
  types = chainon (nreverse (types), void_list_node);
  return tsubst_function_parms (types, args, info.complain, info.in_decl);
}

/* A subroutine of tsubst_parameterized_constraint.  Substitute ARGS
   into the parameter list T, producing a sequence of constraint
   variables, declared in the current scope.

   Note that the caller must establish a local specialization stack
   prior to calling this function since this substitution will
   declare the substituted parameters.  */

static tree
tsubst_constraint_variables (tree t, tree args, subst_info info)
{
  /* Perform a trial substitution to check for type errors.  */
  tree parms = check_constraint_variables (t, args, info);
  if (parms == error_mark_node)
    return error_mark_node;

  /* Clear cp_unevaluated_operand across tsubst so that we get a proper chain
     of PARM_DECLs.  */
  int saved_unevaluated_operand = cp_unevaluated_operand;
  cp_unevaluated_operand = 0;
  tree vars = tsubst (t, args, info.complain, info.in_decl);
  cp_unevaluated_operand = saved_unevaluated_operand;
  if (vars == error_mark_node)
    return error_mark_node;
  return declare_constraint_vars (t, vars);
}

/* Substitute ARGS into the requires-expression T. [8.4.7]p6.  The
   substitution of template arguments into a requires-expression
   may result in the formation of invalid types or expressions
   in its requirements ...  In such cases, the expression evaluates
   to false; it does not cause the program to be ill-formed.

   When substituting through a REQUIRES_EXPR as part of template
   instantiation, we call this routine with info.quiet() true.

   When evaluating a REQUIRES_EXPR that appears outside a template in
   cp_parser_requires_expression, we call this routine with
   info.noisy() true.

   Finally, when diagnosing unsatisfaction from diagnose_atomic_constraint
   and when diagnosing a false REQUIRES_EXPR via diagnose_constraints,
   we call this routine with info.diagnose_unsatisfaction_p() true.  */

static tree
tsubst_requires_expr (tree t, tree args, sat_info info)
{
  local_specialization_stack stack (lss_copy);

  /* We need to check access during the substitution.  */
  deferring_access_check_sentinel acs (dk_no_deferred);

  /* A requires-expression is an unevaluated context.  */
  cp_unevaluated u;

  args = add_extra_args (REQUIRES_EXPR_EXTRA_ARGS (t), args,
			 info.complain, info.in_decl);
  if (processing_template_decl
      && !processing_constraint_expression_p ())
    {
      /* We're partially instantiating a generic lambda.  Substituting into
	 this requires-expression now may cause its requirements to get
	 checked out of order, so instead just remember the template
	 arguments and wait until we can substitute them all at once.

	 Except if this requires-expr is part of associated constraints
	 that we're substituting into directly (for e.g. declaration
	 matching or dguide constraint rewriting), in which case we need
	 to partially substitute.  */
      t = copy_node (t);
      REQUIRES_EXPR_EXTRA_ARGS (t) = NULL_TREE;
      REQUIRES_EXPR_EXTRA_ARGS (t) = build_extra_args (t, args, info.complain);
      return t;
    }

  tree parms = REQUIRES_EXPR_PARMS (t);
  if (parms)
    {
      parms = tsubst_constraint_variables (parms, args, info);
      if (parms == error_mark_node)
	return boolean_false_node;
    }

  tree result = boolean_true_node;
  if (processing_template_decl)
    result = NULL_TREE;
  for (tree reqs = REQUIRES_EXPR_REQS (t); reqs; reqs = TREE_CHAIN (reqs))
    {
      tree req = TREE_VALUE (reqs);
      req = tsubst_requirement (req, args, info);
      if (req == error_mark_node)
	{
	  result = boolean_false_node;
	  if (info.diagnose_unsatisfaction_p ())
	    /* Keep going so that we diagnose all failed requirements.  */;
	  else
	    break;
	}
      else if (processing_template_decl)
	result = tree_cons (NULL_TREE, req, result);
    }
  if (processing_template_decl && result != boolean_false_node)
    result = finish_requires_expr (EXPR_LOCATION (t), parms, nreverse (result));
  return result;
}

/* Public wrapper for the above.  */

tree
tsubst_requires_expr (tree t, tree args,
		      tsubst_flags_t complain, tree in_decl)
{
  sat_info info (complain, in_decl);
  return tsubst_requires_expr (t, args, info);
}

/* Substitute ARGS into the constraint information CI, producing a new
   constraint record.  */

tree
tsubst_constraint_info (tree t, tree args,
                        tsubst_flags_t complain, tree in_decl)
{
  if (!t || t == error_mark_node || !check_constraint_info (t))
    return NULL_TREE;

  tree tr = tsubst_constraint (CI_TEMPLATE_REQS (t), args, complain, in_decl);
  tree dr = tsubst_constraint (CI_DECLARATOR_REQS (t), args, complain, in_decl);
  return build_constraints (tr, dr);
}

/* Substitute through a parameter mapping, in order to get the actual
   arguments used to instantiate an atomic constraint.  This may fail
   if the substitution into arguments produces something ill-formed.  */

static tree
tsubst_parameter_mapping (tree map, tree args, subst_info info)
{
  if (!map)
    return NULL_TREE;

  tsubst_flags_t complain = info.complain;
  tree in_decl = info.in_decl;

  tree result = NULL_TREE;
  for (tree p = map; p; p = TREE_CHAIN (p))
    {
      if (p == error_mark_node)
        return error_mark_node;
      tree parm = TREE_VALUE (p);
      tree arg = TREE_PURPOSE (p);
      tree new_arg;
      if (ARGUMENT_PACK_P (arg))
	new_arg = tsubst_argument_pack (arg, args, complain, in_decl);
      else
	{
	  new_arg = tsubst_template_arg (arg, args, complain, in_decl);
	  if (TYPE_P (new_arg))
	    new_arg = canonicalize_type_argument (new_arg, complain);
	}
      if (TREE_CODE (new_arg) == TYPE_ARGUMENT_PACK)
	{
	  tree pack_args = ARGUMENT_PACK_ARGS (new_arg);
	  for (tree& pack_arg : tree_vec_range (pack_args))
	    if (TYPE_P (pack_arg))
	      pack_arg = canonicalize_type_argument (pack_arg, complain);
	}
      if (new_arg == error_mark_node)
	return error_mark_node;

      result = tree_cons (new_arg, parm, result);
    }
  return nreverse (result);
}

tree
tsubst_parameter_mapping (tree map, tree args, tsubst_flags_t complain, tree in_decl)
{
  return tsubst_parameter_mapping (map, args, subst_info (complain, in_decl));
}

/*---------------------------------------------------------------------------
                        Constraint satisfaction
---------------------------------------------------------------------------*/

/* True if we are currently satisfying a constraint.  */

static bool satisfying_constraint;

/* A vector of incomplete types (and of declarations with undeduced return type),
   appended to by note_failed_type_completion_for_satisfaction.  The
   satisfaction caches use this in order to keep track of "potentially unstable"
   satisfaction results.

   Since references to entries in this vector are stored only in the
   GC-deletable sat_cache, it's safe to make this deletable as well.  */

static GTY((deletable)) vec<tree, va_gc> *failed_type_completions;

/* Called whenever a type completion (or return type deduction) failure occurs
   that definitely affects the meaning of the program, by e.g. inducing
   substitution failure.  */

void
note_failed_type_completion_for_satisfaction (tree t)
{
  if (satisfying_constraint)
    {
      gcc_checking_assert ((TYPE_P (t) && !COMPLETE_TYPE_P (t))
			   || (DECL_P (t) && undeduced_auto_decl (t)));
      vec_safe_push (failed_type_completions, t);
    }
}

/* Returns true if the range [BEGIN, END) of elements within the
   failed_type_completions vector contains a complete type (or a
   declaration with a non-placeholder return type).  */

static bool
some_type_complete_p (int begin, int end)
{
  for (int i = begin; i < end; i++)
    {
      tree t = (*failed_type_completions)[i];
      if (TYPE_P (t) && COMPLETE_TYPE_P (t))
	return true;
      if (DECL_P (t) && !undeduced_auto_decl (t))
	return true;
    }
  return false;
}

/* Hash functions and data types for satisfaction cache entries.  */

struct GTY((for_user)) sat_entry
{
  /* The relevant ATOMIC_CONSTR.  */
  tree atom;

  /* The relevant template arguments.  */
  tree args;

  /* The result of satisfaction of ATOM+ARGS.
     This is either boolean_true_node, boolean_false_node or error_mark_node,
     where error_mark_node indicates ill-formed satisfaction.
     It's set to NULL_TREE while computing satisfaction of ATOM+ARGS for
     the first time.  */
  tree result;

  /* The value of input_location when satisfaction of ATOM+ARGS was first
     performed.  */
  location_t location;

  /* The range of elements appended to the failed_type_completions vector
     during computation of this satisfaction result, encoded as a begin/end
     pair of offsets.  */
  int ftc_begin, ftc_end;

  /* True if we want to diagnose the above instability when it's detected.
     We don't always want to do so, in order to avoid emitting duplicate
     diagnostics in some cases.  */
  bool diagnose_instability;

  /* True if we're in the middle of computing this satisfaction result.
     Used during both quiet and noisy satisfaction to detect self-recursive
     satisfaction.  */
  bool evaluating;
};

struct sat_hasher : ggc_ptr_hash<sat_entry>
{
  static hashval_t hash (sat_entry *e)
  {
    auto cso = make_temp_override (comparing_specializations);
    ++comparing_specializations;

    if (ATOMIC_CONSTR_MAP_INSTANTIATED_P (e->atom))
      {
	/* Atoms with instantiated mappings are built during satisfaction.
	   They live only inside the sat_cache, and we build one to query
	   the cache with each time we instantiate a mapping.  */
	gcc_assert (!e->args);
	return hash_atomic_constraint (e->atom);
      }

    /* Atoms with uninstantiated mappings are built during normalization.
       Since normalize_atom caches the atoms it returns, we can assume
       pointer-based identity for fast hashing and comparison.  Even if this
       assumption is violated, that's okay, we'll just get a cache miss.  */
    hashval_t value = htab_hash_pointer (e->atom);

    if (tree map = ATOMIC_CONSTR_MAP (e->atom))
      /* Only the parameters that are used in the targets of the mapping
	 affect the satisfaction value of the atom.  So we consider only
	 the arguments for these parameters, and ignore the rest.  */
      for (tree target_parms = TREE_TYPE (map);
	   target_parms;
	   target_parms = TREE_CHAIN (target_parms))
	{
	  int level, index;
	  tree parm = TREE_VALUE (target_parms);
	  template_parm_level_and_index (parm, &level, &index);
	  tree arg = TMPL_ARG (e->args, level, index);
	  value = iterative_hash_template_arg (arg, value);
	}
    return value;
  }

  static bool equal (sat_entry *e1, sat_entry *e2)
  {
    auto cso = make_temp_override (comparing_specializations);
    ++comparing_specializations;

    if (ATOMIC_CONSTR_MAP_INSTANTIATED_P (e1->atom)
	!= ATOMIC_CONSTR_MAP_INSTANTIATED_P (e2->atom))
      return false;

    /* See sat_hasher::hash.  */
    if (ATOMIC_CONSTR_MAP_INSTANTIATED_P (e1->atom))
      {
	gcc_assert (!e1->args && !e2->args);
	return atomic_constraints_identical_p (e1->atom, e2->atom);
      }

    if (e1->atom != e2->atom)
      return false;

    if (tree map = ATOMIC_CONSTR_MAP (e1->atom))
      for (tree target_parms = TREE_TYPE (map);
	   target_parms;
	   target_parms = TREE_CHAIN (target_parms))
	{
	  int level, index;
	  tree parm = TREE_VALUE (target_parms);
	  template_parm_level_and_index (parm, &level, &index);
	  tree arg1 = TMPL_ARG (e1->args, level, index);
	  tree arg2 = TMPL_ARG (e2->args, level, index);
	  if (!template_args_equal (arg1, arg2))
	    return false;
	}
    return true;
  }
};

/* Cache the result of satisfy_atom.  */
static GTY((deletable)) hash_table<sat_hasher> *sat_cache;

/* Cache the result of satisfy_declaration_constraints.  */
static GTY((deletable)) hash_map<tree, tree> *decl_satisfied_cache;

/* A tool used by satisfy_atom to help manage satisfaction caching and to
   diagnose "unstable" satisfaction values.  We insert into the cache only
   when performing satisfaction quietly.  */

struct satisfaction_cache
{
  satisfaction_cache (tree, tree, sat_info);
  tree get ();
  tree save (tree);

  sat_entry *entry;
  sat_info info;
  int ftc_begin;
};

/* Constructor for the satisfaction_cache class.  We're performing satisfaction
   of ATOM+ARGS according to INFO.  */

satisfaction_cache
::satisfaction_cache (tree atom, tree args, sat_info info)
  : entry(nullptr), info(info), ftc_begin(-1)
{
  if (!sat_cache)
    sat_cache = hash_table<sat_hasher>::create_ggc (31);

  /* When noisy, we query the satisfaction cache in order to diagnose
     "unstable" satisfaction values.  */
  if (info.noisy ())
    {
      /* When noisy, constraints have been re-normalized, and that breaks the
	 pointer-based identity assumption of sat_cache (for atoms with
	 uninstantiated mappings).  So undo this re-normalization by looking in
	 the atom_cache for the corresponding atom that was used during quiet
	 satisfaction.  */
      if (!ATOMIC_CONSTR_MAP_INSTANTIATED_P (atom))
	{
	  if (tree found = atom_cache->find (atom))
	    atom = found;
	  else
	    /* The lookup should always succeed, but if it fails then let's
	       just leave 'entry' empty, effectively disabling the cache.  */
	    return;
	}
    }

  /* Look up or create the corresponding satisfaction entry.  */
  sat_entry elt;
  elt.atom = atom;
  elt.args = args;
  sat_entry **slot = sat_cache->find_slot (&elt, INSERT);
  if (*slot)
    entry = *slot;
  else if (info.quiet ())
    {
      entry = ggc_alloc<sat_entry> ();
      entry->atom = atom;
      entry->args = args;
      entry->result = NULL_TREE;
      entry->location = input_location;
      entry->ftc_begin = entry->ftc_end = -1;
      entry->diagnose_instability = false;
      if (ATOMIC_CONSTR_MAP_INSTANTIATED_P (atom))
	/* We always want to diagnose instability of an atom with an
	   instantiated parameter mapping.  For atoms with an uninstantiated
	   mapping, we set this flag (in satisfy_atom) only if substitution
	   into its mapping previously failed.  */
	entry->diagnose_instability = true;
      entry->evaluating = false;
      *slot = entry;
    }
  else
    {
      /* We're evaluating this atom for the first time, and doing so noisily.
	 This shouldn't happen outside of error recovery situations involving
	 unstable satisfaction.  Let's just leave 'entry' empty, effectively
	 disabling the cache, and remove the empty slot.  */
      gcc_checking_assert (seen_error ());
      /* Appease hash_table::check_complete_insertion.  */
      *slot = ggc_alloc<sat_entry> ();
      sat_cache->clear_slot (slot);
    }
}

/* Returns the cached satisfaction result if we have one and we're not
   recomputing the satisfaction result from scratch.  Otherwise returns
   NULL_TREE.  */

tree
satisfaction_cache::get ()
{
  if (!entry)
    return NULL_TREE;

  if (entry->evaluating)
    {
      /* If we get here, it means satisfaction is self-recursive.  */
      gcc_checking_assert (!entry->result || seen_error ());
      if (info.noisy ())
	error_at (EXPR_LOCATION (ATOMIC_CONSTR_EXPR (entry->atom)),
		  "satisfaction of atomic constraint %qE depends on itself",
		  entry->atom);
      return error_mark_node;
    }

  /* This satisfaction result is "potentially unstable" if a type for which
     type completion failed during its earlier computation is now complete.  */
  bool maybe_unstable = some_type_complete_p (entry->ftc_begin,
					      entry->ftc_end);

  if (info.noisy () || maybe_unstable || !entry->result)
    {
      /* We're computing the satisfaction result from scratch.  */
      entry->evaluating = true;
      ftc_begin = vec_safe_length (failed_type_completions);
      return NULL_TREE;
    }
  else
    return entry->result;
}

/* RESULT is the computed satisfaction result.  If RESULT differs from the
   previously cached result, this routine issues an appropriate error.
   Otherwise, when evaluating quietly, updates the cache appropriately.  */

tree
satisfaction_cache::save (tree result)
{
  if (!entry)
    return result;

  gcc_checking_assert (entry->evaluating);
  entry->evaluating = false;

  if (entry->result && result != entry->result)
    {
      if (info.quiet ())
	/* Return error_mark_node to force satisfaction to get replayed
	   noisily.  */
	return error_mark_node;
      else
	{
	  if (entry->diagnose_instability)
	    {
	      auto_diagnostic_group d;
	      error_at (EXPR_LOCATION (ATOMIC_CONSTR_EXPR (entry->atom)),
			"satisfaction value of atomic constraint %qE changed "
			"from %qE to %qE", entry->atom, entry->result, result);
	      inform (entry->location,
		      "satisfaction value first evaluated to %qE from here",
		      entry->result);
	    }
	  /* For sake of error recovery, allow this latest satisfaction result
	     to prevail.  */
	  entry->result = result;
	  return result;
	}
    }

  if (info.quiet ())
    {
      entry->result = result;
      /* Store into this entry the list of relevant failed type completions
	 that occurred during (re)computation of the satisfaction result.  */
      gcc_checking_assert (ftc_begin != -1);
      entry->ftc_begin = ftc_begin;
      entry->ftc_end = vec_safe_length (failed_type_completions);
    }

  return result;
}

/* Substitute ARGS into constraint-expression T during instantiation of
   a member of a class template.  */

tree
tsubst_constraint (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  /* We also don't want to evaluate concept-checks when substituting the
     constraint-expressions of a declaration.  */
  processing_constraint_expression_sentinel s;
  cp_unevaluated u;
  tree expr = tsubst_expr (t, args, complain, in_decl);
  return expr;
}

static tree satisfy_constraint_r (tree, tree, sat_info info);

/* Compute the satisfaction of a conjunction.  */

static tree
satisfy_conjunction (tree t, tree args, sat_info info)
{
  tree lhs = satisfy_constraint_r (TREE_OPERAND (t, 0), args, info);
  if (lhs == error_mark_node || lhs == boolean_false_node)
    return lhs;
  return satisfy_constraint_r (TREE_OPERAND (t, 1), args, info);
}

/* The current depth at which we're replaying an error during recursive
   diagnosis of a constraint satisfaction failure.  */

static int current_constraint_diagnosis_depth;

/* Whether CURRENT_CONSTRAINT_DIAGNOSIS_DEPTH has ever exceeded
   CONCEPTS_DIAGNOSTICS_MAX_DEPTH during recursive diagnosis of a constraint
   satisfaction error.  */

static bool concepts_diagnostics_max_depth_exceeded_p;

/* Recursive subroutine of collect_operands_of_disjunction.  T is a normalized
   subexpression of a constraint (composed of CONJ_CONSTRs and DISJ_CONSTRs)
   and E is the corresponding unnormalized subexpression (composed of
   TRUTH_ANDIF_EXPRs and TRUTH_ORIF_EXPRs).  */

static void
collect_operands_of_disjunction_r (tree t, tree e,
				   auto_vec<tree_pair> *operands)
{
  if (TREE_CODE (e) == TRUTH_ORIF_EXPR)
    {
      collect_operands_of_disjunction_r (TREE_OPERAND (t, 0),
					 TREE_OPERAND (e, 0), operands);
      collect_operands_of_disjunction_r (TREE_OPERAND (t, 1),
					 TREE_OPERAND (e, 1), operands);
    }
  else
    {
      tree_pair p = std::make_pair (t, e);
      operands->safe_push (p);
    }
}

/* Recursively collect the normalized and unnormalized operands of the
   disjunction T and append them to OPERANDS in order.  */

static void
collect_operands_of_disjunction (tree t, auto_vec<tree_pair> *operands)
{
  collect_operands_of_disjunction_r (t, CONSTR_EXPR (t), operands);
}

/* Compute the satisfaction of a disjunction.  */

static tree
satisfy_disjunction (tree t, tree args, sat_info info)
{
  /* Evaluate each operand with unsatisfaction diagnostics disabled.  */
  sat_info sub = info;
  sub.diagnose_unsatisfaction = false;

  tree lhs = satisfy_constraint_r (TREE_OPERAND (t, 0), args, sub);
  if (lhs == boolean_true_node || lhs == error_mark_node)
    return lhs;

  tree rhs = satisfy_constraint_r (TREE_OPERAND (t, 1), args, sub);
  if (rhs == boolean_true_node || rhs == error_mark_node)
    return rhs;

  /* Both branches evaluated to false.  Explain the satisfaction failure in
     each branch.  */
  if (info.diagnose_unsatisfaction_p ())
    {
      diagnosing_failed_constraint failure (t, args, info.noisy ());
      cp_expr disj_expr = CONSTR_EXPR (t);
      inform (disj_expr.get_location (),
	      "no operand of the disjunction is satisfied");
      if (diagnosing_failed_constraint::replay_errors_p ())
	{
	  auto_diagnostic_nesting_level sentinel;
	  /* Replay the error in each branch of the disjunction.  */
	  auto_vec<tree_pair> operands;
	  collect_operands_of_disjunction (t, &operands);
	  for (unsigned i = 0; i < operands.length (); i++)
	    {
	      tree norm_op = operands[i].first;
	      tree op = operands[i].second;
	      location_t loc = make_location (cp_expr_location (op),
					      disj_expr.get_start (),
					      disj_expr.get_finish ());
	      inform (loc, "the operand %qE is unsatisfied because", op);
	      auto_diagnostic_nesting_level sentinel;
	      satisfy_constraint_r (norm_op, args, info);
	    }
	}
    }

  return boolean_false_node;
}

/* Ensures that T is a truth value and not (accidentally, as sometimes
   happens) an integer value.  */

tree
satisfaction_value (tree t)
{
  if (t == error_mark_node || t == boolean_true_node || t == boolean_false_node)
    return t;

  gcc_assert (TREE_CODE (t) == INTEGER_CST
	      && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (t),
							    boolean_type_node));
  if (integer_zerop (t))
    return boolean_false_node;
  else
    return boolean_true_node;
}

/* Build a new template argument vector corresponding to the parameter
   mapping of the atomic constraint T, using arguments from ARGS.  */

static tree
get_mapped_args (tree t, tree args)
{
  tree map = ATOMIC_CONSTR_MAP (t);

  /* No map, no arguments.  */
  if (!map)
    return NULL_TREE;

  /* Determine the depth of the resulting argument vector.  */
  int depth;
  if (ATOMIC_CONSTR_EXPR_FROM_CONCEPT_P (t))
    /* The expression of this atomic constraint comes from a concept
       definition, whose template depth is always one, so the resulting
       argument vector will also have depth one.  */
    depth = 1;
  else
    /* Otherwise, the expression of this atomic constraint comes from
       the context of the constrained entity, whose template depth is that
       of ARGS.  */
    depth = TMPL_ARGS_DEPTH (args);

  /* Place each argument at its corresponding position in the argument
     list.  Note that the list will be sparse (not all arguments supplied),
     but instantiation is guaranteed to only use the parameters in the
     mapping, so null arguments would never be used.  */
  auto_vec< vec<tree> > lists (depth);
  lists.quick_grow_cleared (depth);
  for (tree p = map; p; p = TREE_CHAIN (p))
    {
      int level;
      int index;
      template_parm_level_and_index (TREE_VALUE (p), &level, &index);

      /* Insert the argument into its corresponding position.  */
      vec<tree> &list = lists[level - 1];
      if (index >= (int)list.length ())
	list.safe_grow_cleared (index + 1, /*exact=*/false);
      list[index] = TREE_PURPOSE (p);
    }

  /* Build the new argument list.  */
  args = make_tree_vec (lists.length ());
  for (unsigned i = 0; i != lists.length (); ++i)
    {
      vec<tree> &list = lists[i];
      tree level = make_tree_vec (list.length ());
      for (unsigned j = 0; j < list.length (); ++j)
	TREE_VEC_ELT (level, j) = list[j];
      SET_TMPL_ARGS_LEVEL (args, i + 1, level);
      list.release ();
    }
  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (args, 0);

  if (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (args)
      && TMPL_ARGS_DEPTH (args) == 1)
    {
      /* Get rid of the redundant outer TREE_VEC.  */
      tree level = TMPL_ARGS_LEVEL (args, 1);
      ggc_free (args);
      args = level;
    }

  return args;
}

static void diagnose_atomic_constraint (tree, tree, tree, sat_info);

/* Compute the satisfaction of an atomic constraint.  */

static tree
satisfy_atom (tree t, tree args, sat_info info)
{
  /* In case there is a diagnostic, we want to establish the context
     prior to printing errors.  If no errors occur, this context is
     removed before returning.  */
  diagnosing_failed_constraint failure (t, args, info.noisy ());

  satisfaction_cache cache (t, args, info);
  if (tree r = cache.get ())
    return r;

  /* Perform substitution quietly.  */
  subst_info quiet (tf_none, NULL_TREE);

  /* Instantiate the parameter mapping.  */
  tree map = tsubst_parameter_mapping (ATOMIC_CONSTR_MAP (t), args, quiet);
  if (map == error_mark_node)
    {
      /* If instantiation of the parameter mapping fails, the constraint is
	 not satisfied.  Replay the substitution.  */
      if (info.diagnose_unsatisfaction_p ())
	tsubst_parameter_mapping (ATOMIC_CONSTR_MAP (t), args, info);
      if (info.quiet ())
	/* Since instantiation of the parameter mapping failed, we
	   want to diagnose potential instability of this satisfaction
	   result.  */
	cache.entry->diagnose_instability = true;
      return cache.save (boolean_false_node);
    }

  /* Now build a new atom using the instantiated mapping.  We use
     this atom as a second key to the satisfaction cache, and we
     also pass it to diagnose_atomic_constraint so that diagnostics
     which refer to the atom display the instantiated mapping.  */
  t = copy_node (t);
  ATOMIC_CONSTR_MAP (t) = map;
  gcc_assert (!ATOMIC_CONSTR_MAP_INSTANTIATED_P (t));
  ATOMIC_CONSTR_MAP_INSTANTIATED_P (t) = true;
  satisfaction_cache inst_cache (t, /*args=*/NULL_TREE, info);
  if (tree r = inst_cache.get ())
    {
      cache.entry->location = inst_cache.entry->location;
      return cache.save (r);
    }

  /* Rebuild the argument vector from the parameter mapping.  */
  args = get_mapped_args (t, args);

  /* Apply the parameter mapping (i.e., just substitute).  */
  tree expr = ATOMIC_CONSTR_EXPR (t);
  tree result = tsubst_expr (expr, args, quiet.complain, quiet.in_decl);
  if (result == error_mark_node)
    {
      /* If substitution results in an invalid type or expression, the
	 constraint is not satisfied.  Replay the substitution.  */
      if (info.diagnose_unsatisfaction_p ())
	tsubst_expr (expr, args, info.complain, info.in_decl);
      return cache.save (inst_cache.save (boolean_false_node));
    }

  /* [17.4.1.2] ... lvalue-to-rvalue conversion is performed as necessary,
     and EXPR shall be a constant expression of type bool.  */
  result = force_rvalue (result, info.complain);
  if (result == error_mark_node)
    return cache.save (inst_cache.save (error_mark_node));
  if (!same_type_p (TREE_TYPE (result), boolean_type_node))
    {
      if (info.noisy ())
	diagnose_atomic_constraint (t, args, result, info);
      return cache.save (inst_cache.save (error_mark_node));
    }

  /* Compute the value of the constraint.  */
  if (info.noisy ())
    {
      iloc_sentinel ils (EXPR_LOCATION (result));
      result = cxx_constant_value (result);
    }
  else
    {
      result = maybe_constant_value (result, NULL_TREE, mce_true);
      if (!TREE_CONSTANT (result))
	result = error_mark_node;
    }
  result = satisfaction_value (result);
  if (result == boolean_false_node && info.diagnose_unsatisfaction_p ())
    diagnose_atomic_constraint (t, args, result, info);

  return cache.save (inst_cache.save (result));
}

/* Determine if the normalized constraint T is satisfied.
   Returns boolean_true_node if the expression/constraint is
   satisfied, boolean_false_node if not, and error_mark_node
   if there was an error evaluating the constraint.

   The parameter mapping of atomic constraints is simply the
   set of template arguments that will be substituted into
   the expression, regardless of template parameters appearing
   within.  Whether a template argument is used in the atomic
   constraint only matters for subsumption.  */

static tree
satisfy_constraint_r (tree t, tree args, sat_info info)
{
  if (t == error_mark_node)
    return error_mark_node;

  switch (TREE_CODE (t))
    {
    case CONJ_CONSTR:
      return satisfy_conjunction (t, args, info);
    case DISJ_CONSTR:
      return satisfy_disjunction (t, args, info);
    case ATOMIC_CONSTR:
      return satisfy_atom (t, args, info);
    default:
      gcc_unreachable ();
    }
}

/* Check that the normalized constraint T is satisfied for ARGS.  */

static tree
satisfy_normalized_constraints (tree t, tree args, sat_info info)
{
  auto_timevar time (TV_CONSTRAINT_SAT);

  auto ovr = make_temp_override (satisfying_constraint, true);

  /* Turn off template processing.  Constraint satisfaction only applies
     to non-dependent terms, so we want to ensure full checking here.  */
  processing_template_decl_sentinel proc (true);

  /* We need to check access during satisfaction.  */
  deferring_access_check_sentinel acs (dk_no_deferred);

  /* Constraints are unevaluated operands.  */
  cp_unevaluated u;

  return satisfy_constraint_r (t, args, info);
}

/* Return the normal form of the constraints on the placeholder 'auto'
   type T.  */

static tree
normalize_placeholder_type_constraints (tree t, bool diag)
{
  gcc_assert (is_auto (t));
  tree ci = PLACEHOLDER_TYPE_CONSTRAINTS_INFO (t);
  if (!ci)
    return NULL_TREE;

  tree constr = TREE_VALUE (ci);
  /* The TREE_PURPOSE contains the set of template parameters that were in
     scope for this placeholder type; use them as the initial template
     parameters for normalization.  */
  tree initial_parms = TREE_PURPOSE (ci);

  /* The 'auto' itself is used as the first argument in its own constraints,
     and its level is one greater than its template depth.  So in order to
     capture all used template parameters, we need to add an extra level of
     template parameters to the context; a dummy level suffices.  */
  initial_parms
    = tree_cons (size_int (initial_parms
			   ? TMPL_PARMS_DEPTH (initial_parms) + 1 : 1),
		 make_tree_vec (0), initial_parms);

  norm_info info (diag);
  info.initial_parms = initial_parms;
  return normalize_constraint_expression (constr, info);
}

/* Evaluate the constraints of T using ARGS, returning a satisfaction value.
   Here, T can be a concept-id, nested-requirement, placeholder 'auto', or
   requires-expression.  */

static tree
satisfy_nondeclaration_constraints (tree t, tree args, sat_info info)
{
  if (t == error_mark_node)
    return error_mark_node;

  /* Handle REQUIRES_EXPR directly, bypassing satisfaction.  */
  if (TREE_CODE (t) == REQUIRES_EXPR)
    {
      auto ovr = make_temp_override (current_constraint_diagnosis_depth);
      if (info.noisy ())
	++current_constraint_diagnosis_depth;
      return tsubst_requires_expr (t, args, info);
    }

  /* Get the normalized constraints.  */
  tree norm;
  if (concept_check_p (t))
    {
      gcc_assert (!args);
      args = TREE_OPERAND (t, 1);
      tree tmpl = get_concept_check_template (t);
      norm = normalize_concept_definition (tmpl, info.noisy ());
    }
  else if (TREE_CODE (t) == NESTED_REQ)
    {
      norm_info ninfo (info.noisy ());
      /* The TREE_TYPE contains the set of template parameters that were in
	 scope for this nested requirement; use them as the initial template
	 parameters for normalization.  */
      ninfo.initial_parms = TREE_TYPE (t);
      norm = normalize_constraint_expression (TREE_OPERAND (t, 0), ninfo);
    }
  else if (is_auto (t))
    {
      norm = normalize_placeholder_type_constraints (t, info.noisy ());
      if (!norm)
	return boolean_true_node;
    }
  else
    gcc_unreachable ();

  /* Perform satisfaction.  */
  return satisfy_normalized_constraints (norm, args, info);
}

/* Evaluate the associated constraints of the template specialization T
   according to INFO, returning a satisfaction value.  */

static tree
satisfy_declaration_constraints (tree t, sat_info info)
{
  gcc_assert (DECL_P (t) && TREE_CODE (t) != TEMPLATE_DECL);
  const tree saved_t = t;

  /* For inherited constructors, consider the original declaration;
     it has the correct template information attached.  */
  t = strip_inheriting_ctors (t);
  tree inh_ctor_targs = NULL_TREE;
  if (t != saved_t)
    if (tree ti = DECL_TEMPLATE_INFO (saved_t))
      /* The inherited constructor points to an instantiation of a constructor
	 template; remember its template arguments.  */
      inh_ctor_targs = TI_ARGS (ti);

  /* Update the declaration for diagnostics.  */
  info.in_decl = t;

  if (info.quiet ())
    if (tree *result = hash_map_safe_get (decl_satisfied_cache, saved_t))
      return *result;

  tree args = NULL_TREE;
  if (tree ti = DECL_TEMPLATE_INFO (t))
    {
      /* The initial parameter mapping is the complete set of
	 template arguments substituted into the declaration.  */
      args = TI_ARGS (ti);
      if (inh_ctor_targs)
	args = add_outermost_template_args (args, inh_ctor_targs);
    }

  if (regenerated_lambda_fn_p (t))
    {
      /* The TI_ARGS of a regenerated lambda contains only the innermost
	 set of template arguments.  Augment this with the outer template
	 arguments that were used to regenerate the lambda.  */
      gcc_assert (!args || TMPL_ARGS_DEPTH (args) == 1);
      tree regen_args = lambda_regenerating_args (t);
      if (args)
	args = add_to_template_args (regen_args, args);
      else
	args = regen_args;
    }

  /* If the innermost arguments are dependent, or if the outer arguments
     are dependent and are needed by the constraints, we can't check
     satisfaction yet so pretend they're satisfied for now.  */
  if (uses_template_parms (args)
      && ((DECL_TEMPLATE_INFO (t)
	   && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (t))
	   && (TMPL_ARGS_DEPTH (args) == 1
	       || uses_template_parms (INNERMOST_TEMPLATE_ARGS (args))))
	  || uses_outer_template_parms_in_constraints (t)))
    return boolean_true_node;

  /* Get the normalized constraints.  */
  tree norm = get_normalized_constraints_from_decl (t, info.noisy ());

  unsigned ftc_count = vec_safe_length (failed_type_completions);

  tree result = boolean_true_node;
  if (norm)
    {
      if (!push_tinst_level (t))
	return result;
      push_to_top_level ();
      push_access_scope (t);
      result = satisfy_normalized_constraints (norm, args, info);
      pop_access_scope (t);
      pop_from_top_level ();
      pop_tinst_level ();
    }

  /* True if this satisfaction is (heuristically) potentially unstable, i.e.
     if its result may depend on where in the program it was performed.  */
  bool maybe_unstable_satisfaction = false;
  if (ftc_count != vec_safe_length (failed_type_completions))
    /* Type completion failure occurred during satisfaction.  The satisfaction
       result may (or may not) materially depend on the completeness of a type,
       so we consider it potentially unstable.   */
    maybe_unstable_satisfaction = true;

  if (maybe_unstable_satisfaction)
    /* Don't cache potentially unstable satisfaction, to allow satisfy_atom
       to check the stability the next time around.  */;
  else if (info.quiet ())
    hash_map_safe_put<hm_ggc> (decl_satisfied_cache, saved_t, result);

  return result;
}

/* Evaluate the associated constraints of the template T using ARGS as the
   innermost set of template arguments and according to INFO, returning a
   satisfaction value.  */

static tree
satisfy_declaration_constraints (tree t, tree args, sat_info info)
{
  /* Update the declaration for diagnostics.  */
  info.in_decl = t;

  gcc_assert (TREE_CODE (t) == TEMPLATE_DECL);

  if (regenerated_lambda_fn_p (t))
    {
      /* As in the two-parameter version of this function.  */
      gcc_assert (TMPL_ARGS_DEPTH (args) == 1);
      tree lambda = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (t));
      tree outer_args = TI_ARGS (LAMBDA_EXPR_REGEN_INFO (lambda));
      args = add_to_template_args (outer_args, args);
    }
  else
    args = add_outermost_template_args (t, args);

  /* If the innermost arguments are dependent, or if the outer arguments
     are dependent and are needed by the constraints, we can't check
     satisfaction yet so pretend they're satisfied for now.  */
  if (uses_template_parms (args)
      && (TMPL_ARGS_DEPTH (args) == 1
	  || uses_template_parms (INNERMOST_TEMPLATE_ARGS (args))
	  || uses_outer_template_parms_in_constraints (t)))
    return boolean_true_node;

  tree result = boolean_true_node;
  if (tree norm = get_normalized_constraints_from_decl (t, info.noisy ()))
    {
      if (!push_tinst_level (t, args))
	return result;
      tree pattern = DECL_TEMPLATE_RESULT (t);
      push_to_top_level ();
      push_access_scope (pattern);
      result = satisfy_normalized_constraints (norm, args, info);
      pop_access_scope (pattern);
      pop_from_top_level ();
      pop_tinst_level ();
    }

  return result;
}

/* A wrapper around satisfy_declaration_constraints and
   satisfy_nondeclaration_constraints which additionally replays
   quiet ill-formed satisfaction noisily, so that ill-formed
   satisfaction always gets diagnosed.  */

static tree
constraint_satisfaction_value (tree t, tree args, sat_info info)
{
  tree r;
  if (DECL_P (t))
    {
      if (args)
	r = satisfy_declaration_constraints (t, args, info);
      else
	r = satisfy_declaration_constraints (t, info);
    }
  else
    r = satisfy_nondeclaration_constraints (t, args, info);
  if (r == error_mark_node && info.quiet ()
      && !(DECL_P (t) && warning_suppressed_p (t)))
    {
      /* Replay the error noisily.  */
      sat_info noisy (tf_warning_or_error, info.in_decl);
      constraint_satisfaction_value (t, args, noisy);
      if (DECL_P (t) && !args)
	/* Avoid giving these errors again.  */
	suppress_warning (t);
    }
  return r;
}

/* True iff the result of satisfying T using ARGS is BOOLEAN_TRUE_NODE
   and false otherwise, even in the case of errors.

   Here, T can be:
     - a template declaration
     - a template specialization (in which case ARGS must be empty)
     - a concept-id (in which case ARGS must be empty)
     - a nested-requirement
     - a placeholder 'auto'
     - a requires-expression.  */

bool
constraints_satisfied_p (tree t, tree args/*= NULL_TREE */)
{
  if (!flag_concepts)
    return true;

  sat_info quiet (tf_none, NULL_TREE);
  return constraint_satisfaction_value (t, args, quiet) == boolean_true_node;
}

/* Evaluate a concept check of the form C<ARGS>.  This is only used for the
   evaluation of template-ids as id-expressions.  */

tree
evaluate_concept_check (tree check)
{
  if (check == error_mark_node)
    return error_mark_node;

  gcc_assert (concept_check_p (check));

  /* Check for satisfaction without diagnostics.  */
  sat_info quiet (tf_none, NULL_TREE);
  return constraint_satisfaction_value (check, /*args=*/NULL_TREE, quiet);
}

/* Evaluate the requires-expression T, returning either boolean_true_node
   or boolean_false_node.  This is used during folding and constexpr
   evaluation.  */

tree
evaluate_requires_expr (tree t)
{
  gcc_assert (TREE_CODE (t) == REQUIRES_EXPR);
  sat_info quiet (tf_none, NULL_TREE);
  return constraint_satisfaction_value (t, /*args=*/NULL_TREE, quiet);
}

/*---------------------------------------------------------------------------
                Semantic analysis of requires-expressions
---------------------------------------------------------------------------*/

/* Finish a requires expression for the given PARMS (possibly
   null) and the non-empty sequence of requirements.  */

tree
finish_requires_expr (location_t loc, tree parms, tree reqs)
{
  /* Build the node.  */
  tree r = build_min (REQUIRES_EXPR, boolean_type_node, parms, reqs, NULL_TREE);
  TREE_SIDE_EFFECTS (r) = false;
  TREE_CONSTANT (r) = true;
  SET_EXPR_LOCATION (r, loc);
  return r;
}

/* Construct a requirement for the validity of EXPR.   */

tree
finish_simple_requirement (location_t loc, tree expr)
{
  tree r = build_nt (SIMPLE_REQ, expr);
  SET_EXPR_LOCATION (r, loc);
  return r;
}

/* Construct a requirement for the validity of TYPE.  */

tree
finish_type_requirement (location_t loc, tree type)
{
  tree r = build_nt (TYPE_REQ, type);
  SET_EXPR_LOCATION (r, loc);
  return r;
}

/* Construct a requirement for the validity of EXPR, along with
   its properties.  If TYPE is non-null, then it specifies either
   an implicit conversion or argument deduction constraint,
   depending on whether any placeholders occur in the type name.
   NOEXCEPT_P is true iff the noexcept keyword was specified.  */

tree
finish_compound_requirement (location_t loc, tree expr, tree type, bool noexcept_p)
{
  tree req = build_nt (COMPOUND_REQ, expr, type);
  SET_EXPR_LOCATION (req, loc);
  COMPOUND_REQ_NOEXCEPT_P (req) = noexcept_p;
  return req;
}

/* Finish a nested requirement.  */

tree
finish_nested_requirement (location_t loc, tree expr)
{
  /* Build the requirement, saving the set of in-scope template
     parameters as its type.  */
  tree r = build1 (NESTED_REQ, current_template_parms, expr);
  SET_EXPR_LOCATION (r, loc);
  return r;
}

/*---------------------------------------------------------------------------
                        Equivalence of constraints
---------------------------------------------------------------------------*/

/* Returns true when A and B are equivalent constraints.  */
bool
equivalent_constraints (tree a, tree b)
{
  gcc_assert (!a || TREE_CODE (a) == CONSTRAINT_INFO);
  gcc_assert (!b || TREE_CODE (b) == CONSTRAINT_INFO);
  return cp_tree_equal (a, b);
}

/* Returns true if the template declarations A and B have equivalent
   constraints.  This is the case when A's constraints subsume B's and
   when B's also constrain A's.  */
bool
equivalently_constrained (tree d1, tree d2)
{
  gcc_assert (TREE_CODE (d1) == TREE_CODE (d2));
  return equivalent_constraints (get_constraints (d1), get_constraints (d2));
}

/*---------------------------------------------------------------------------
                     Partial ordering of constraints
---------------------------------------------------------------------------*/

/* Returns true when the constraints in CI strictly subsume
   the associated constraints of TMPL.  */

bool
strictly_subsumes (tree ci, tree tmpl)
{
  tree n1 = get_normalized_constraints_from_info (ci, NULL_TREE);
  tree n2 = get_normalized_constraints_from_decl (tmpl);

  return subsumes (n1, n2) && !subsumes (n2, n1);
}

/* Returns true when the template template parameter constraints in CI
   subsume the associated constraints of the template template argument
   TMPL.  */

bool
ttp_subsumes (tree ci, tree tmpl)
{
  tree n1 = get_normalized_constraints_from_info (ci, tmpl);
  tree n2 = get_normalized_constraints_from_decl (tmpl);

  return subsumes (n1, n2);
}

/* Determines which of the declarations, A or B, is more constrained.
   That is, which declaration's constraints subsume but are not subsumed
   by the other's?

   Returns 1 if D1 is more constrained than D2, -1 if D2 is more constrained
   than D1, and 0 otherwise.  */

int
more_constrained (tree d1, tree d2)
{
  tree n1 = get_normalized_constraints_from_decl (d1);
  tree n2 = get_normalized_constraints_from_decl (d2);

  int winner = 0;
  if (subsumes (n1, n2))
    ++winner;
  if (subsumes (n2, n1))
    --winner;
  return winner;
}

/* Return whether D1 is at least as constrained as D2.  */

bool
at_least_as_constrained (tree d1, tree d2)
{
  tree n1 = get_normalized_constraints_from_decl (d1);
  tree n2 = get_normalized_constraints_from_decl (d2);

  return subsumes (n1, n2);
}

/*---------------------------------------------------------------------------
                        Constraint diagnostics
---------------------------------------------------------------------------*/

/* Returns the best location to diagnose a constraint error.  */

static location_t
get_constraint_error_location (tree t)
{
  if (location_t loc = cp_expr_location (t))
    return loc;

  /* If we have a specific location give it.  */
  tree expr = CONSTR_EXPR (t);
  if (location_t loc = cp_expr_location (expr))
    return loc;

  /* If the constraint is normalized from a requires-clause, give
     the location as that of the constrained declaration.  */
  tree cxt = CONSTR_CONTEXT (t);
  tree src = cxt ? TREE_VALUE (cxt) : NULL_TREE;
  if (!src)
    /* TODO: This only happens for constrained non-template declarations.  */
    ;
  else if (DECL_P (src))
    return DECL_SOURCE_LOCATION (src);
  /* Otherwise, give the location as the defining concept.  */
  else if (concept_check_p (src))
    {
      tree tmpl = TREE_OPERAND (src, 0);
      return DECL_SOURCE_LOCATION (tmpl);
    }

  return input_location;
}

/* Emit a diagnostic for a failed trait.  */

static void
diagnose_trait_expr (tree expr, tree args)
{
  location_t loc = cp_expr_location (expr);

  /* Build a "fake" version of the instantiated trait, so we can
     get the instantiated types from result.  */
  ++processing_template_decl;
  expr = tsubst_expr (expr, args, tf_none, NULL_TREE);
  --processing_template_decl;

  tree t1 = TRAIT_EXPR_TYPE1 (expr);
  tree t2 = TRAIT_EXPR_TYPE2 (expr);
  if (t2 && TREE_CODE (t2) == TREE_VEC)
    {
      /* Convert the TREE_VEC of arguments into a TREE_LIST, since we can't
	 directly print a TREE_VEC but we can a TREE_LIST via the E format
	 specifier.  */
      tree list = NULL_TREE;
      for (tree t : tree_vec_range (t2))
	list = tree_cons (NULL_TREE, t, list);
      t2 = nreverse (list);
    }
  switch (TRAIT_EXPR_KIND (expr))
    {
    case CPTK_HAS_NOTHROW_ASSIGN:
      inform (loc, "  %qT is not nothrow copy assignable", t1);
      break;
    case CPTK_HAS_NOTHROW_CONSTRUCTOR:
      inform (loc, "  %qT is not nothrow default constructible", t1);
      break;
    case CPTK_HAS_NOTHROW_COPY:
      inform (loc, "  %qT is not nothrow copy constructible", t1);
      break;
    case CPTK_HAS_TRIVIAL_ASSIGN:
      inform (loc, "  %qT is not trivially copy assignable", t1);
      break;
    case CPTK_HAS_TRIVIAL_CONSTRUCTOR:
      inform (loc, "  %qT is not trivially default constructible", t1);
      break;
    case CPTK_HAS_TRIVIAL_COPY:
      inform (loc, "  %qT is not trivially copy constructible", t1);
      break;
    case CPTK_HAS_TRIVIAL_DESTRUCTOR:
      inform (loc, "  %qT is not trivially destructible", t1);
      break;
    case CPTK_HAS_UNIQUE_OBJ_REPRESENTATIONS:
      inform (loc, "  %qT does not have unique object representations", t1);
      break;
    case CPTK_HAS_VIRTUAL_DESTRUCTOR:
      inform (loc, "  %qT does not have a virtual destructor", t1);
      break;
    case CPTK_IS_ABSTRACT:
      inform (loc, "  %qT is not an abstract class", t1);
      break;
    case CPTK_IS_AGGREGATE:
      inform (loc, "  %qT is not an aggregate", t1);
      break;
    case CPTK_IS_ARRAY:
      inform (loc, "  %qT is not an array", t1);
      break;
    case CPTK_IS_ASSIGNABLE:
      inform (loc, "  %qT is not assignable from %qT", t1, t2);
      break;
    case CPTK_IS_BASE_OF:
      inform (loc, "  %qT is not a base of %qT", t1, t2);
      break;
    case CPTK_IS_BOUNDED_ARRAY:
      inform (loc, "  %qT is not a bounded array", t1);
      break;
    case CPTK_IS_CLASS:
      inform (loc, "  %qT is not a class", t1);
      break;
    case CPTK_IS_CONST:
      inform (loc, "  %qT is not a const type", t1);
      break;
    case CPTK_IS_CONSTRUCTIBLE:
      if (!t2)
	inform (loc, "  %qT is not default constructible", t1);
      else
	inform (loc, "  %qT is not constructible from %qE", t1, t2);
      break;
    case CPTK_IS_CONVERTIBLE:
      inform (loc, "  %qT is not convertible from %qE", t2, t1);
      break;
    case CPTK_IS_EMPTY:
      inform (loc, "  %qT is not an empty class", t1);
      break;
    case CPTK_IS_ENUM:
      inform (loc, "  %qT is not an enum", t1);
      break;
    case CPTK_IS_FINAL:
      inform (loc, "  %qT is not a final class", t1);
      break;
    case CPTK_IS_FUNCTION:
      inform (loc, "  %qT is not a function", t1);
      break;
    case CPTK_IS_INVOCABLE:
      if (!t2)
	inform (loc, "  %qT is not invocable", t1);
      else
	inform (loc, "  %qT is not invocable by %qE", t1, t2);
      break;
    case CPTK_IS_LAYOUT_COMPATIBLE:
      inform (loc, "  %qT is not layout compatible with %qT", t1, t2);
      break;
    case CPTK_IS_LITERAL_TYPE:
      inform (loc, "  %qT is not a literal type", t1);
      break;
    case CPTK_IS_MEMBER_FUNCTION_POINTER:
      inform (loc, "  %qT is not a member function pointer", t1);
      break;
    case CPTK_IS_MEMBER_OBJECT_POINTER:
      inform (loc, "  %qT is not a member object pointer", t1);
      break;
    case CPTK_IS_MEMBER_POINTER:
      inform (loc, "  %qT is not a member pointer", t1);
      break;
    case CPTK_IS_NOTHROW_ASSIGNABLE:
      inform (loc, "  %qT is not nothrow assignable from %qT", t1, t2);
      break;
    case CPTK_IS_NOTHROW_CONSTRUCTIBLE:
      if (!t2)
	inform (loc, "  %qT is not nothrow default constructible", t1);
      else
	inform (loc, "  %qT is not nothrow constructible from %qE", t1, t2);
      break;
    case CPTK_IS_NOTHROW_CONVERTIBLE:
      inform (loc, "  %qT is not nothrow convertible from %qE", t2, t1);
      break;
    case CPTK_IS_NOTHROW_INVOCABLE:
      if (!t2)
	inform (loc, "  %qT is not nothrow invocable", t1);
      else
	inform (loc, "  %qT is not nothrow invocable by %qE", t1, t2);
      break;
    case CPTK_IS_OBJECT:
      inform (loc, "  %qT is not an object type", t1);
      break;
    case CPTK_IS_POINTER_INTERCONVERTIBLE_BASE_OF:
      inform (loc, "  %qT is not pointer-interconvertible base of %qT",
	      t1, t2);
      break;
    case CPTK_IS_POD:
      inform (loc, "  %qT is not a POD type", t1);
      break;
    case CPTK_IS_POINTER:
      inform (loc, "  %qT is not a pointer", t1);
      break;
    case CPTK_IS_POLYMORPHIC:
      inform (loc, "  %qT is not a polymorphic type", t1);
      break;
    case CPTK_IS_REFERENCE:
      inform (loc, "  %qT is not a reference", t1);
      break;
    case CPTK_IS_SAME:
      inform (loc, "  %qT is not the same as %qT", t1, t2);
      break;
    case CPTK_IS_SCOPED_ENUM:
      inform (loc, "  %qT is not a scoped enum", t1);
      break;
    case CPTK_IS_STD_LAYOUT:
      inform (loc, "  %qT is not an standard layout type", t1);
      break;
    case CPTK_IS_TRIVIAL:
      inform (loc, "  %qT is not a trivial type", t1);
      break;
    case CPTK_IS_TRIVIALLY_ASSIGNABLE:
      inform (loc, "  %qT is not trivially assignable from %qT", t1, t2);
      break;
    case CPTK_IS_TRIVIALLY_CONSTRUCTIBLE:
      if (!t2)
	inform (loc, "  %qT is not trivially default constructible", t1);
      else
	inform (loc, "  %qT is not trivially constructible from %qE", t1, t2);
      break;
    case CPTK_IS_TRIVIALLY_COPYABLE:
      inform (loc, "  %qT is not trivially copyable", t1);
      break;
    case CPTK_IS_UNBOUNDED_ARRAY:
      inform (loc, "  %qT is not an unbounded array", t1);
      break;
    case CPTK_IS_UNION:
      inform (loc, "  %qT is not a union", t1);
      break;
    case CPTK_IS_VIRTUAL_BASE_OF:
      inform (loc, "  %qT is not a virtual base of %qT", t1, t2);
      break;
    case CPTK_IS_VOLATILE:
      inform (loc, "  %qT is not a volatile type", t1);
      break;
    case CPTK_RANK:
      inform (loc, "  %qT cannot yield a rank", t1);
      break;
    case CPTK_REF_CONSTRUCTS_FROM_TEMPORARY:
      inform (loc, "  %qT is not a reference that binds to a temporary "
	      "object of type %qT (direct-initialization)", t1, t2);
      break;
    case CPTK_REF_CONVERTS_FROM_TEMPORARY:
      inform (loc, "  %qT is not a reference that binds to a temporary "
	      "object of type %qT (copy-initialization)", t1, t2);
      break;
    case CPTK_IS_DEDUCIBLE:
      inform (loc, "  %qD is not deducible from %qT", t1, t2);
      break;
#define DEFTRAIT_TYPE(CODE, NAME, ARITY) \
    case CPTK_##CODE:
#include "cp-trait.def"
#undef DEFTRAIT_TYPE
      /* Type-yielding traits aren't expressions.  */
      gcc_unreachable ();
    /* We deliberately omit the default case so that when adding a new
       trait we'll get reminded (by way of a warning) to handle it here.  */
    }
}

/* Diagnose a substitution failure in the atomic constraint T using ARGS.  */

static void
diagnose_atomic_constraint (tree t, tree args, tree result, sat_info info)
{
  /* If the constraint is already ill-formed, we've previously diagnosed
     the reason.  We should still say why the constraints aren't satisfied.  */
  if (t == error_mark_node)
    {
      location_t loc;
      if (info.in_decl)
        loc = DECL_SOURCE_LOCATION (info.in_decl);
      else
        loc = input_location;
      inform (loc, "invalid constraints");
      return;
    }

  location_t loc = get_constraint_error_location (t);
  iloc_sentinel loc_s (loc);

  /* Generate better diagnostics for certain kinds of expressions.  */
  tree expr = ATOMIC_CONSTR_EXPR (t);
  STRIP_ANY_LOCATION_WRAPPER (expr);
  switch (TREE_CODE (expr))
    {
    case TRAIT_EXPR:
      diagnose_trait_expr (expr, args);
      break;
    case REQUIRES_EXPR:
      gcc_checking_assert (info.diagnose_unsatisfaction_p ());
      /* Clear in_decl before replaying the substitution to avoid emitting
	 seemingly unhelpful "in declaration ..." notes that follow some
	 substitution failure error messages.  */
      info.in_decl = NULL_TREE;
      tsubst_requires_expr (expr, args, info);
      break;
    default:
      if (!same_type_p (TREE_TYPE (result), boolean_type_node))
	error_at (loc, "constraint %qE has type %qT, not %<bool%>",
		  t, TREE_TYPE (result));
      else
	inform (loc, "the expression %qE evaluated to %<false%>", t);
    }
}

GTY(()) tree current_failed_constraint;

diagnosing_failed_constraint::
diagnosing_failed_constraint (tree t, tree args, bool diag)
  : diagnosing_error (diag)
{
  if (diagnosing_error)
    {
      current_failed_constraint
	= tree_cons (args, t, current_failed_constraint);
      ++current_constraint_diagnosis_depth;
    }
}

diagnosing_failed_constraint::
~diagnosing_failed_constraint ()
{
  if (diagnosing_error)
    {
      --current_constraint_diagnosis_depth;
      if (current_failed_constraint)
	current_failed_constraint = TREE_CHAIN (current_failed_constraint);
    }

}

/* Whether we are allowed to replay an error that underlies a constraint failure
   at the current diagnosis depth.  */

bool
diagnosing_failed_constraint::replay_errors_p ()
{
  if (current_constraint_diagnosis_depth >= concepts_diagnostics_max_depth)
    {
      concepts_diagnostics_max_depth_exceeded_p = true;
      return false;
    }
  else
    return true;
}

/* Emit diagnostics detailing the failure ARGS to satisfy the constraints
   of T.  Here, T and ARGS are as in constraints_satisfied_p.  */

void
diagnose_constraints (location_t loc, tree t, tree args)
{
  inform (loc, "constraints not satisfied");

  if (concepts_diagnostics_max_depth == 0)
    return;

  auto_diagnostic_nesting_level sentinel;

  /* Replay satisfaction, but diagnose unsatisfaction.  */
  sat_info noisy (tf_warning_or_error, NULL_TREE, /*diag_unsat=*/true);
  constraint_satisfaction_value (t, args, noisy);

  static bool suggested_p;
  if (concepts_diagnostics_max_depth_exceeded_p
      && current_constraint_diagnosis_depth == 0
      && !suggested_p)
    {
      inform (UNKNOWN_LOCATION,
	      "set %qs to at least %d for more detail",
	      "-fconcepts-diagnostics-depth=",
	      concepts_diagnostics_max_depth + 1);
      suggested_p = true;
    }
}

#include "gt-cp-constraint.h"
