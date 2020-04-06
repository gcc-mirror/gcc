/* Processing rules for constraints.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.
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
  bool quiet() const
  {
    return complain == tf_none;
  }

  /* True if we should diagnose errors.  */
  bool noisy() const
  {
    return !quiet ();
  }

  tsubst_flags_t complain;
  tree in_decl;
};

static tree satisfy_constraint (tree, tree, subst_info);

/* True if T is known to be some type other than bool. Note that this
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

  /* Check that we're using function concepts correctly.  */
  if (concept_check_p (expr))
    {
      tree id = unpack_concept_check (expr);
      tree tmpl = TREE_OPERAND (id, 0);
      if (OVL_P (tmpl) && TREE_CODE (expr) == TEMPLATE_ID_EXPR)
        {
	  error_at (EXPR_LOC_OR_LOC (expr, input_location),
		    "function concept must be called");
	  return false;
	}
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
  tree overload;
  cp_expr expr = build_x_binary_op (loc, code,
				    lhs, TREE_CODE (lhs),
				    rhs, TREE_CODE (rhs),
				    &overload, tf_none);
  /* When either operand is dependent, the overload set may be non-empty.  */
  if (expr == error_mark_node)
    return error_mark_node;
  expr.set_location (loc);
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
  return finish_constraint_and_expr (input_location, lhs, rhs);
}

/* Extract the template-id from a concept check. For standard and variable
   checks, this is simply T. For function concept checks, this is the
   called function.  */

tree
unpack_concept_check (tree t)
{
  gcc_assert (concept_check_p (t));

  if (TREE_CODE (t) == CALL_EXPR)
    t = CALL_EXPR_FN (t);

  gcc_assert (TREE_CODE (t) == TEMPLATE_ID_EXPR);
  return t;
}

/* Extract the TEMPLATE_DECL from a concept check.  */

tree
get_concept_check_template (tree t)
{
  tree id = unpack_concept_check (t);
  tree tmpl = TREE_OPERAND (id, 0);
  if (OVL_P (tmpl))
    tmpl = OVL_FIRST (tmpl);
  return tmpl;
}

/* Returns true if any of the arguments in the template argument list is
   a wildcard or wildcard pack.  */

bool
contains_wildcard_p (tree args)
{
  for (int i = 0; i < TREE_VEC_LENGTH (args); ++i)
    {
      tree arg = TREE_VEC_ELT (args, i);
      if (TREE_CODE (arg) == WILDCARD_DECL)
	return true;
    }
  return false;
}

/*---------------------------------------------------------------------------
                    Resolution of qualified concept names
---------------------------------------------------------------------------*/

/* This facility is used to resolve constraint checks from requirement
   expressions. A constraint check is a call to a function template declared
   with the keyword 'concept'.

   The result of resolution is a pair (a TREE_LIST) whose value is the
   matched declaration, and whose purpose contains the coerced template
   arguments that can be substituted into the call.  */

/* Given an overload set OVL, try to find a unique definition that can be
   instantiated by the template arguments ARGS.

   This function is not called for arbitrary call expressions. In particular,
   the call expression must be written with explicit template arguments
   and no function arguments. For example:

        f<T, U>()

   If a single match is found, this returns a TREE_LIST whose VALUE
   is the constraint function (not the template), and its PURPOSE is
   the complete set of arguments substituted into the parameter list.  */

static tree
resolve_function_concept_overload (tree ovl, tree args)
{
  int nerrs = 0;
  tree cands = NULL_TREE;
  for (lkp_iterator iter (ovl); iter; ++iter)
    {
      tree tmpl = *iter;
      if (TREE_CODE (tmpl) != TEMPLATE_DECL)
        continue;

      /* Don't try to deduce checks for non-concepts. We often end up trying
         to resolve constraints in functional casts as part of a
         postfix-expression. We can save time and headaches by not
         instantiating those declarations.

         NOTE: This masks a potential error, caused by instantiating
         non-deduced contexts using placeholder arguments. */
      tree fn = DECL_TEMPLATE_RESULT (tmpl);
      if (DECL_ARGUMENTS (fn))
        continue;
      if (!DECL_DECLARED_CONCEPT_P (fn))
        continue;

      /* Remember the candidate if we can deduce a substitution.  */
      ++processing_template_decl;
      tree parms = TREE_VALUE (DECL_TEMPLATE_PARMS (tmpl));
      if (tree subst = coerce_template_parms (parms, args, tmpl))
        {
          if (subst == error_mark_node)
            ++nerrs;
          else
	    cands = tree_cons (subst, fn, cands);
        }
      --processing_template_decl;
    }

  if (!cands)
    /* We either had no candidates or failed deductions.  */
    return nerrs ? error_mark_node : NULL_TREE;
  else if (TREE_CHAIN (cands))
    /* There are multiple candidates.  */
    return error_mark_node;

  return cands;
}

/* Determine if the call expression CALL is a constraint check, and
   return the concept declaration and arguments being checked. If CALL
   does not denote a constraint check, return NULL.  */

tree
resolve_function_concept_check (tree call)
{
  gcc_assert (TREE_CODE (call) == CALL_EXPR);

  /* A constraint check must be only a template-id expression.
     If it's a call to a base-link, its function(s) should be a
     template-id expression. If this is not a template-id, then
     it cannot be a concept-check.  */
  tree target = CALL_EXPR_FN (call);
  if (BASELINK_P (target))
    target = BASELINK_FUNCTIONS (target);
  if (TREE_CODE (target) != TEMPLATE_ID_EXPR)
    return NULL_TREE;

  /* Get the overload set and template arguments and try to
     resolve the target.  */
  tree ovl = TREE_OPERAND (target, 0);

  /* This is a function call of a variable concept... ill-formed.  */
  if (TREE_CODE (ovl) == TEMPLATE_DECL)
    {
      error_at (location_of (call),
		"function call of variable concept %qE", call);
      return error_mark_node;
    }

  tree args = TREE_OPERAND (target, 1);
  return resolve_function_concept_overload (ovl, args);
}

/* Returns a pair containing the checked concept and its associated
   prototype parameter. The result is a TREE_LIST whose TREE_VALUE
   is the concept (non-template) and whose TREE_PURPOSE contains
   the converted template arguments, including the deduced prototype
   parameter (in position 0). */

tree
resolve_concept_check (tree check)
{
  gcc_assert (concept_check_p (check));
  tree id = unpack_concept_check (check);
  tree tmpl = TREE_OPERAND (id, 0);

  /* If this is an overloaded function concept, perform overload
     resolution (this only happens when deducing prototype parameters
     and template introductions).  */
  if (TREE_CODE (tmpl) == OVERLOAD)
    {
      if (OVL_CHAIN (tmpl))
	return resolve_function_concept_check (check);
      tmpl = OVL_FIRST (tmpl);
    }

  tree args = TREE_OPERAND (id, 1);
  tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (tmpl));
  ++processing_template_decl;
  tree result = coerce_template_parms (parms, args, tmpl);
  --processing_template_decl;
  if (result == error_mark_node)
    return error_mark_node;
  return build_tree_list (result, DECL_TEMPLATE_RESULT (tmpl));
}

/* Given a call expression or template-id expression to a concept EXPR
   possibly including a wildcard, deduce the concept being checked and
   the prototype parameter. Returns true if the constraint and prototype
   can be deduced and false otherwise.  Note that the CHECK and PROTO
   arguments are set to NULL_TREE if this returns false.  */

bool
deduce_constrained_parameter (tree expr, tree& check, tree& proto)
{
  tree info = resolve_concept_check (expr);
  if (info && info != error_mark_node)
    {
      check = TREE_VALUE (info);
      tree arg = TREE_VEC_ELT (TREE_PURPOSE (info), 0);
      if (ARGUMENT_PACK_P (arg))
	arg = TREE_VEC_ELT (ARGUMENT_PACK_ARGS (arg), 0);
      proto = TREE_TYPE (arg);
      return true;
    }

  check = proto = NULL_TREE;
  return false;
}

/* Given a call expression or template-id expression to a concept, EXPR,
   deduce the concept being checked and return the template arguments.
   Returns NULL_TREE if deduction fails.  */
static tree
deduce_concept_introduction (tree check)
{
  tree info = resolve_concept_check (check);
  if (info && info != error_mark_node)
    return TREE_PURPOSE (info);
  return NULL_TREE;
}

/* Build a constrained placeholder type where SPEC is a type-constraint.
   SPEC can be anything were concept_definition_p is true.

   If DECLTYPE_P is true, then the placeholder is decltype(auto).

   Returns a pair whose FIRST is the concept being checked and whose
   SECOND is the prototype parameter.  */

tree_pair
finish_type_constraints (tree spec, tree args, tsubst_flags_t complain)
{
  gcc_assert (concept_definition_p (spec));

  /* Build an initial concept check.  */
  tree check = build_type_constraint (spec, args, complain);
  if (check == error_mark_node)
    return std::make_pair (error_mark_node, NULL_TREE);

  /* Extract the concept and prototype parameter from the check. */
  tree con;
  tree proto;
  if (!deduce_constrained_parameter (check, con, proto))
    return std::make_pair (error_mark_node, NULL_TREE);

  return std::make_pair (con, proto);
}

/*---------------------------------------------------------------------------
                       Expansion of concept definitions
---------------------------------------------------------------------------*/

/* Returns the expression of a function concept. */

static tree
get_returned_expression (tree fn)
{
  /* Extract the body of the function minus the return expression.  */
  tree body = DECL_SAVED_TREE (fn);
  if (!body)
    return error_mark_node;
  if (TREE_CODE (body) == BIND_EXPR)
    body = BIND_EXPR_BODY (body);
  if (TREE_CODE (body) != RETURN_EXPR)
    return error_mark_node;

  return TREE_OPERAND (body, 0);
}

/* Returns the initializer of a variable concept. */

static tree
get_variable_initializer (tree var)
{
  tree init = DECL_INITIAL (var);
  if (!init)
    return error_mark_node;
  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && CONSTRUCTOR_NELTS (init) == 1)
    init = CONSTRUCTOR_ELT (init, 0)->value;
  return init;
}

/* Returns the definition of a variable or function concept.  */

static tree
get_concept_definition (tree decl)
{
  if (TREE_CODE (decl) == OVERLOAD)
    decl = OVL_FIRST (decl);

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    decl = DECL_TEMPLATE_RESULT (decl);

  if (TREE_CODE (decl) == CONCEPT_DECL)
    return DECL_INITIAL (decl);
  if (VAR_P (decl))
    return get_variable_initializer (decl);
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return get_returned_expression (decl);
  gcc_unreachable ();
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
	verbatim ("ARG %qT", arg);
      else
	verbatim ("ARG %qE", arg);
    }
}

/* Associate each parameter in PARMS with its corresponding template
   argument in ARGS.  */

static tree
map_arguments (tree parms, tree args)
{
  for (tree p = parms; p; p = TREE_CHAIN (p))
    {
      int level;
      int index;
      template_parm_level_and_index (TREE_VALUE (p), &level, &index);
      TREE_PURPOSE (p) = TMPL_ARG (args, level, index);
    }
  return parms;
}

/* Build the parameter mapping for EXPR using ARGS.  */

static tree
build_parameter_mapping (tree expr, tree args, tree decl)
{
  tree ctx_parms = NULL_TREE;
  if (decl)
    {
      gcc_assert (TREE_CODE (decl) == TEMPLATE_DECL);
      ctx_parms = DECL_TEMPLATE_PARMS (decl);
    }
  else if (current_template_parms)
    {
      /* TODO: This should probably be the only case, but because the
	 point of declaration of concepts is currently set after the
	 initializer, the template parameter lists are not available
	 when normalizing concept definitions, hence the case above.  */
      ctx_parms = current_template_parms;
    }

  tree parms = find_template_parameters (expr, ctx_parms);
  tree map = map_arguments (parms, args);
  return map;
}

/* True if the parameter mappings of two atomic constraints are equivalent.  */

static bool
parameter_mapping_equivalent_p (tree t1, tree t2)
{
  tree map1 = ATOMIC_CONSTR_MAP (t1);
  tree map2 = ATOMIC_CONSTR_MAP (t2);
  while (map1 && map2)
    {
      tree arg1 = TREE_PURPOSE (map1);
      tree arg2 = TREE_PURPOSE (map2);
      if (!template_args_equal (arg1, arg2))
        return false;
      map1 = TREE_CHAIN (map1);
      map2 = TREE_CHAIN (map2);
    }
  return true;
}

/* Provides additional context for normalization.  */

struct norm_info : subst_info
{
  explicit norm_info (tsubst_flags_t complain)
    : subst_info (tf_warning_or_error | complain, NULL_TREE),
      context()
  {}

  /* Construct a top-level context for DECL.  */

  norm_info (tree in_decl, tsubst_flags_t complain)
    : subst_info (tf_warning_or_error | complain, in_decl),
      context (make_context (in_decl))
  {}

  bool generate_diagnostics() const
  {
    return complain & tf_norm;
  }

  tree make_context(tree in_decl)
  {
    if (generate_diagnostics ())
      return build_tree_list (NULL_TREE, in_decl);
    return NULL_TREE;
  }

  void update_context(tree expr, tree args)
  {
    if (generate_diagnostics ())
      {
	tree map = build_parameter_mapping (expr, args, in_decl);
	context = tree_cons (map, expr, context);
      }
    in_decl = get_concept_check_template (expr);
  }

  /* Provides information about the source of a constraint. This is a
     TREE_LIST whose VALUE is either a concept check or a constrained
     declaration. The PURPOSE, for concept checks is a parameter mapping
     for that check.  */

  tree context;
};

static tree normalize_expression (tree, tree, norm_info);

/* Transform a logical-or or logical-and expression into either
   a conjunction or disjunction. */

static tree
normalize_logical_operation (tree t, tree args, tree_code c, norm_info info)
{
  tree t0 = normalize_expression (TREE_OPERAND (t, 0), args, info);
  tree t1 = normalize_expression (TREE_OPERAND (t, 1), args, info);

  /* Build a new info object for the constraint.  */
  tree ci = info.generate_diagnostics()
    ? build_tree_list (t, info.context)
    : NULL_TREE;

  return build2 (c, ci, t0, t1);
}

static tree
normalize_concept_check (tree check, tree args, norm_info info)
{
  tree id = unpack_concept_check (check);
  tree tmpl = TREE_OPERAND (id, 0);
  tree targs = TREE_OPERAND (id, 1);

  /* A function concept is wrapped in an overload.  */
  if (TREE_CODE (tmpl) == OVERLOAD)
    {
      /* TODO: Can we diagnose this error during parsing?  */
      if (TREE_CODE (check) == TEMPLATE_ID_EXPR)
	error_at (EXPR_LOC_OR_LOC (check, input_location),
		  "function concept must be called");
      tmpl = OVL_FIRST (tmpl);
    }

  /* Substitute through the arguments of the concept check. */
  targs = tsubst_template_args (targs, args, info.complain, info.in_decl);
  if (targs == error_mark_node)
    return error_mark_node;

  /* Build the substitution for the concept definition.  */
  tree parms = TREE_VALUE (DECL_TEMPLATE_PARMS (tmpl));
  /* Turn on template processing; coercing non-type template arguments
     will automatically assume they're non-dependent.  */
  ++processing_template_decl;
  tree subst = coerce_template_parms (parms, targs, tmpl);
  --processing_template_decl;
  if (subst == error_mark_node)
    return error_mark_node;

  /* The concept may have been ill-formed.  */
  tree def = get_concept_definition (DECL_TEMPLATE_RESULT (tmpl));
  if (def == error_mark_node)
    return error_mark_node;

  info.update_context (check, args);
  return normalize_expression (def, subst, info);
}

/* The normal form of an atom depends on the expression. The normal
   form of a function call to a function concept is a check constraint
   for that concept. The normal form of a reference to a variable
   concept is a check constraint for that concept. Otherwise, the
   constraint is a predicate constraint.  */

static tree
normalize_atom (tree t, tree args, norm_info info)
{
  /* Concept checks are not atomic.  */
  if (concept_check_p (t))
    return normalize_concept_check (t, args, info);

  /* Build the parameter mapping for the atom.  */
  tree map = build_parameter_mapping (t, args, info.in_decl);

  /* Build a new info object for the atom.  */
  tree ci = build_tree_list (t, info.context);

  return build1 (ATOMIC_CONSTR, ci, map);
}

/* Returns the normal form of an expression. */

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
get_normalized_constraints (tree t, tree args, norm_info info)
{
  auto_timevar time (TV_CONSTRAINT_NORM);
  return normalize_expression (t, args, info);
}

/* Returns the normalized constraints from a constraint-info object
   or NULL_TREE if the constraints are null. ARGS provide the initial
   arguments for normalization and IN_DECL provides the declaration
   to which the constraints belong.  */

static tree
get_normalized_constraints_from_info (tree ci, tree args, tree in_decl,
				      bool diag = false)
{
  if (ci == NULL_TREE)
    return NULL_TREE;

  /* Substitution errors during normalization are fatal.  */
  ++processing_template_decl;
  norm_info info (in_decl, diag ? tf_norm : tf_none);
  tree t = get_normalized_constraints (CI_ASSOCIATED_CONSTRAINTS (ci),
				       args, info);
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
     it has the correct template information attached. */
  d = strip_inheriting_ctors (d);

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
     arguments from that. This ensures that the arguments used for
     normalization are always template parameters and not arguments
     used for outer specializations.  For example:

        template<typename T>
        struct S {
	  template<typename U> requires C<T, U> void f(U);
        };

        S<int>::f(0);

     When we normalize the requirements for S<int>::f, we want the
     arguments to be {T, U}, not {int, U}. One reason for this is that
     accepting the latter causes the template parameter level of U
     to be reduced in a way that makes it overly difficult substitute
     concrete arguments (i.e., eventually {int, int} during satisfaction.  */
  if (tmpl)
  {
    if (DECL_LANG_SPECIFIC(tmpl) && !DECL_TEMPLATE_SPECIALIZATION (tmpl))
      tmpl = most_general_template (tmpl);
  }

  /* If we're not diagnosing errors, use cached constraints, if any.  */
  if (!diag)
    if (tree *p = hash_map_safe_get (normalized_map, tmpl))
      return *p;

  tree args = generic_targs_for (tmpl);
  tree ci = get_constraints (decl);
  tree norm = get_normalized_constraints_from_info (ci, args, tmpl, diag);

  if (!diag)
    hash_map_safe_put<hm_ggc> (normalized_map, tmpl, norm);

  return norm;
}

/* Returns the normal form of TMPL's definition.  */

static tree
normalize_concept_definition (tree tmpl, bool diag = false)
{
  if (!diag)
    if (tree *p = hash_map_safe_get (normalized_map, tmpl))
      return *p;

  gcc_assert (concept_definition_p (tmpl));
  if (OVL_P (tmpl))
    tmpl = OVL_FIRST (tmpl);
  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);
  tree args = generic_targs_for (tmpl);
  tree def = get_concept_definition (DECL_TEMPLATE_RESULT (tmpl));
  ++processing_template_decl;
  norm_info info (tmpl, diag ? tf_norm : tf_none);
  tree norm = get_normalized_constraints (def, args, info);
  --processing_template_decl;

  if (!diag)
    hash_map_safe_put<hm_ggc> (normalized_map, tmpl, norm);

  return norm;
}

/* Returns the normal form of TMPL's requirements.  */

static tree
normalize_template_requirements (tree tmpl, bool diag = false)
{
  return get_normalized_constraints_from_decl (tmpl, diag);
}

/* Returns the normal form of TMPL's requirements.  */

static tree
normalize_nontemplate_requirements (tree decl, bool diag = false)
{
  return get_normalized_constraints_from_decl (decl, diag);
}

/* Normalize an EXPR as a constraint using ARGS.  */

static tree
normalize_constraint_expression (tree expr, tree args, bool diag = false)
{
  if (!expr || expr == error_mark_node)
    return expr;
  ++processing_template_decl;
  norm_info info (diag ? tf_norm : tf_none);
  tree norm = get_normalized_constraints (expr, args, info);
  --processing_template_decl;
  return norm;
}

/* Normalize an EXPR as a constraint.  */

static tree
normalize_constraint_expression (tree expr, bool diag = false)
{
  if (!expr || expr == error_mark_node)
    return expr;

  /* For concept checks, use the supplied template arguments as those used
     for normalization. Otherwise, there are no template arguments.  */
  tree args;
  if (concept_check_p (expr))
    {
      tree id = unpack_concept_check (expr);
      args = TREE_OPERAND (id, 1);
    }
  else
    args = NULL_TREE;

  return normalize_constraint_expression (expr, args, diag);
}

/* 17.4.1.2p2. Two constraints are identical if they are formed
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
    if (!constraints_equivalent_p (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
      return false;
    if (!constraints_equivalent_p (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1)))
      return false;
    break;
  case ATOMIC_CONSTR:
    if (!atomic_constraints_identical_p(t1, t2))
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
  h.add_int(TREE_CODE (t));
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
	 has associated constraints. If it is defined outside of its
	 class, then we need match these constraints against those of
	 original declaration.  */
      if (tree orig_ci = get_constraints (decl))
        {
          if (!equivalent_constraints (ci, orig_ci))
            {
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

/* Returns the template constraints of declaration T. If T is not
   constrained, return NULL_TREE. Note that T must be non-null. */

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
   T. If T is a template, then the constraints are associated with
   its underlying declaration. Don't build associations if CI is
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
  gcc_assert (DECL_P (t));
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  if (decl_constraints)
    decl_constraints->remove (t);
}

/* If DECL is a friend, substitute into REQS to produce requirements suitable
   for declaration matching.  */

tree
maybe_substitute_reqs_for (tree reqs, const_tree decl_)
{
  if (reqs == NULL_TREE)
    return NULL_TREE;
  tree decl = CONST_CAST_TREE (decl_);
  tree result = STRIP_TEMPLATE (decl);
  if (DECL_FRIEND_P (result))
    {
      tree tmpl = decl == result ? DECL_TI_TEMPLATE (result) : decl;
      tree gargs = generic_targs_for (tmpl);
      processing_template_decl_sentinel s;
      if (uses_template_parms (gargs))
	++processing_template_decl;
      reqs = tsubst_constraint (reqs, gargs,
				tf_warning_or_error, NULL_TREE);
    }
  return reqs;
}

/* Returns the template-head requires clause for the template
   declaration T or NULL_TREE if none.  */

tree
get_template_head_requirements (tree t)
{
  tree ci = get_constraints (t);
  if (!ci)
    return NULL_TREE;
  return CI_TEMPLATE_REQS (ci);
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
   ARG to REST. Either ARG or REST may be null. */
static tree
build_concept_check_arguments (tree arg, tree rest)
{
  gcc_assert (rest ? TREE_CODE (rest) == TREE_VEC : true);
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
    {
      gcc_assert (rest != NULL_TREE);
      args = rest;
    }
  return args;
}

/* Builds an id-expression of the form `C<Args...>()` where C is a function
   concept.  */

static tree
build_function_check (tree tmpl, tree args, tsubst_flags_t /*complain*/)
{
  if (TREE_CODE (tmpl) == TEMPLATE_DECL)
    {
      /* If we just got a template, wrap it in an overload so it looks like any
	 other template-id. */
      tmpl = ovl_make (tmpl);
      TREE_TYPE (tmpl) = boolean_type_node;
    }

  /* Perform function concept resolution now so we always have a single
     function of the overload set (even if we started with only one; the
     resolution function converts template arguments). Note that we still
     wrap this in an overload set so we don't upset other parts of the
     compiler that expect template-ids referring to function concepts
     to have an overload set.  */
  tree info = resolve_function_concept_overload (tmpl, args);
  if (info == error_mark_node)
    return error_mark_node;
  if (!info)
    {
      error ("no matching concepts for %qE", tmpl);
      return error_mark_node;
    }
  args = TREE_PURPOSE (info);
  tmpl = DECL_TI_TEMPLATE (TREE_VALUE (info));

  /* Rebuild the singleton overload set; mark the type bool.  */
  tmpl = ovl_make (tmpl, NULL_TREE);
  TREE_TYPE (tmpl) = boolean_type_node;

  /* Build the id-expression around the overload set.  */
  tree id = build2 (TEMPLATE_ID_EXPR, boolean_type_node, tmpl, args);

  /* Finally, build the call expression around the overload.  */
  ++processing_template_decl;
  vec<tree, va_gc> *fargs = make_tree_vector ();
  tree call = build_min_nt_call_vec (id, fargs);
  TREE_TYPE (call) = boolean_type_node;
  release_tree_vector (fargs);
  --processing_template_decl;

  return call;
}

/* Builds an id-expression of the form `C<Args...>` where C is a variable
   concept.  */

static tree
build_variable_check (tree tmpl, tree args, tsubst_flags_t complain)
{
  gcc_assert (variable_concept_p (tmpl));
  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);
  tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (tmpl));
  args = coerce_template_parms (parms, args, tmpl, complain);
  if (args == error_mark_node)
    return error_mark_node;
  return build2 (TEMPLATE_ID_EXPR, boolean_type_node, tmpl, args);
}

/* Builds an id-expression of the form `C<Args...>` where C is a standard
   concept.  */

static tree
build_standard_check (tree tmpl, tree args, tsubst_flags_t complain)
{
  gcc_assert (standard_concept_p (tmpl));
  gcc_assert (TREE_CODE (tmpl) == TEMPLATE_DECL);
  tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (tmpl));
  args = coerce_template_parms (parms, args, tmpl, complain);
  if (args == error_mark_node)
    return error_mark_node;
  return build2 (TEMPLATE_ID_EXPR, boolean_type_node, tmpl, args);
}

/* Construct an expression that checks TARGET using ARGS.  */

tree
build_concept_check (tree target, tree args, tsubst_flags_t complain)
{
  return build_concept_check (target, NULL_TREE, args, complain);
}

/* Construct an expression that checks the concept given by DECL. If
   concept_definition_p (DECL) is false, this returns null.  */

tree
build_concept_check (tree decl, tree arg, tree rest, tsubst_flags_t complain)
{
  if (arg == NULL_TREE && rest == NULL_TREE)
    {
      tree id = build_nt (TEMPLATE_ID_EXPR, decl, rest);
      error ("invalid use concept %qE", id);
      return error_mark_node;
    }

  tree args = build_concept_check_arguments (arg, rest);

  if (standard_concept_p (decl))
    return build_standard_check (decl, args, complain);
  if (variable_concept_p (decl))
    return build_variable_check (decl, args, complain);
  if (function_concept_p (decl))
    return build_function_check (decl, args, complain);

  return error_mark_node;
}

/* Build a template-id that can participate in a concept check.  */

static tree
build_concept_id (tree decl, tree args)
{
  tree check = build_concept_check (decl, args, tf_warning_or_error);
  if (check == error_mark_node)
    return error_mark_node;
  return unpack_concept_check (check);
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
  tree wildcard = build_nt (WILDCARD_DECL);
  tree check = build_concept_check (decl, wildcard, args, complain);
  if (check == error_mark_node)
    return error_mark_node;
  return unpack_concept_check (check);
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

   Note that the constraints are neither reduced nor decomposed. That is
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

  /* The TS lets use shorthand to constrain a pack of arguments, but the
     standard does not.

     For the TS, consider:

	template<C... Ts> struct s;

     If C is variadic (and because Ts is a pack), we associate the
     constraint C<Ts...>. In all other cases, we associate
     the constraint (C<Ts> && ...).

     The standard behavior cannot be overridden by -fconcepts-ts.  */
  bool variadic_concept_p = template_parameter_pack_p (proto);
  bool declared_pack_p = template_parameter_pack_p (decl);
  bool apply_to_each_p = (cxx_dialect >= cxx2a) ? true : !variadic_concept_p;

  /* Get the argument and overload used for the requirement
     and adjust it if we're going to expand later.  */
  tree arg = template_parm_to_arg (build_tree_list (NULL_TREE, decl));
  if (apply_to_each_p && declared_pack_p)
    arg = PACK_EXPANSION_PATTERN (TREE_VEC_ELT (ARGUMENT_PACK_ARGS (arg), 0));

  /* Build the concept constraint-expression.  */
  tree tmpl = DECL_TI_TEMPLATE (con);
  tree check = tmpl;
  if (TREE_CODE (con) == FUNCTION_DECL)
    check = ovl_make (tmpl);
  check = build_concept_check (check, arg, args, tf_warning_or_error);

  /* Make the check a fold-expression if needed.  */
  if (apply_to_each_p && declared_pack_p)
    check = finish_left_unary_fold_expr (check, TRUTH_ANDIF_EXPR);

  return check;
}

/* Returns a conjunction of shorthand requirements for the template
   parameter list PARMS. Note that the requirements are stored in
   the TYPE of each tree node. */

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

/* Get the deduced wildcard from a DEDUCED placeholder.  If the deduced
   wildcard is a pack, return the first argument of that pack.  */

static tree
get_deduced_wildcard (tree wildcard)
{
  if (ARGUMENT_PACK_P (wildcard))
    wildcard = TREE_VEC_ELT (ARGUMENT_PACK_ARGS (wildcard), 0);
  gcc_assert (TREE_CODE (wildcard) == WILDCARD_DECL);
  return wildcard;
}

/* Returns the prototype parameter for the nth deduced wildcard.  */

static tree
get_introduction_prototype (tree wildcards, int index)
{
  return TREE_TYPE (get_deduced_wildcard (TREE_VEC_ELT (wildcards, index)));
}

/* Introduce a type template parameter.  */

static tree
introduce_type_template_parameter (tree wildcard, bool& non_type_p)
{
  non_type_p = false;
  return finish_template_type_parm (class_type_node, DECL_NAME (wildcard));
}

/* Introduce a template template parameter.  */

static tree
introduce_template_template_parameter (tree wildcard, bool& non_type_p)
{
  non_type_p = false;
  begin_template_parm_list ();
  current_template_parms = DECL_TEMPLATE_PARMS (TREE_TYPE (wildcard));
  end_template_parm_list ();
  return finish_template_template_parm (class_type_node, DECL_NAME (wildcard));
}

/* Introduce a template non-type parameter.  */

static tree
introduce_nontype_template_parameter (tree wildcard, bool& non_type_p)
{
  non_type_p = true;
  tree parm = copy_decl (TREE_TYPE (wildcard));
  DECL_NAME (parm) = DECL_NAME (wildcard);
  return parm;
}

/* Introduce a single template parameter.  */

static tree
build_introduced_template_parameter (tree wildcard, bool& non_type_p)
{
  tree proto = TREE_TYPE (wildcard);

  tree parm;
  if (TREE_CODE (proto) == TYPE_DECL)
    parm = introduce_type_template_parameter (wildcard, non_type_p);
  else if (TREE_CODE (proto) == TEMPLATE_DECL)
    parm = introduce_template_template_parameter (wildcard, non_type_p);
  else
    parm = introduce_nontype_template_parameter (wildcard, non_type_p);

  /* Wrap in a TREE_LIST for process_template_parm. Note that introduced
     parameters do not retain the defaults from the source parameter.  */
  return build_tree_list (NULL_TREE, parm);
}

/* Introduce a single template parameter.  */

static tree
introduce_template_parameter (tree parms, tree wildcard)
{
  gcc_assert (!ARGUMENT_PACK_P (wildcard));
  tree proto = TREE_TYPE (wildcard);
  location_t loc = DECL_SOURCE_LOCATION (wildcard);

  /* Diagnose the case where we have C{...Args}.  */
  if (WILDCARD_PACK_P (wildcard))
    {
      tree id = DECL_NAME (wildcard);
      error_at (loc, "%qE cannot be introduced with an ellipsis %<...%>", id);
      inform (DECL_SOURCE_LOCATION (proto), "prototype declared here");
    }

  bool non_type_p;
  tree parm = build_introduced_template_parameter (wildcard, non_type_p);
  return process_template_parm (parms, loc, parm, non_type_p, false);
}

/* Introduce a template parameter pack.  */

static tree
introduce_template_parameter_pack (tree parms, tree wildcard)
{
  bool non_type_p;
  tree parm = build_introduced_template_parameter (wildcard, non_type_p);
  location_t loc = DECL_SOURCE_LOCATION (wildcard);
  return process_template_parm (parms, loc, parm, non_type_p, true);
}

/* Introduce the nth template parameter.  */

static tree
introduce_template_parameter (tree parms, tree wildcards, int& index)
{
  tree deduced = TREE_VEC_ELT (wildcards, index++);
  return introduce_template_parameter (parms, deduced);
}

/* Introduce either a template parameter pack or a list of template
   parameters.  */

static tree
introduce_template_parameters (tree parms, tree wildcards, int& index)
{
  /* If the prototype was a parameter, we better have deduced an
     argument pack, and that argument must be the last deduced value
     in the wildcard vector.  */
  tree deduced = TREE_VEC_ELT (wildcards, index++);
  gcc_assert (ARGUMENT_PACK_P (deduced));
  gcc_assert (index == TREE_VEC_LENGTH (wildcards));

  /* Introduce each element in the pack.  */
  tree args = ARGUMENT_PACK_ARGS (deduced);
  for (int i = 0; i < TREE_VEC_LENGTH (args); ++i)
    {
      tree arg = TREE_VEC_ELT (args, i);
      if (WILDCARD_PACK_P (arg))
	parms = introduce_template_parameter_pack (parms, arg);
      else
	parms = introduce_template_parameter (parms, arg);
    }

  return parms;
}

/* Builds the template parameter list PARMS by chaining introduced
   parameters from the WILDCARD vector.  INDEX is the position of
   the current parameter.  */

static tree
process_introduction_parms (tree parms, tree wildcards, int& index)
{
  tree proto = get_introduction_prototype (wildcards, index);
  if (template_parameter_pack_p (proto))
    return introduce_template_parameters (parms, wildcards, index);
  else
    return introduce_template_parameter (parms, wildcards, index);
}

/* Ensure that all template parameters have been introduced for the concept
   named in CHECK.  If not, emit a diagnostic.

   Note that implicitly introducing a parameter with a default argument
     creates a case where a parameter is declared, but unnamed, making
     it unusable in the definition.  */

static bool
check_introduction_list (tree intros, tree check)
{
  check = unpack_concept_check (check);
  tree tmpl = TREE_OPERAND (check, 0);
  if (OVL_P (tmpl))
    tmpl = OVL_FIRST (tmpl);

  tree parms = DECL_INNERMOST_TEMPLATE_PARMS (tmpl);
  if (TREE_VEC_LENGTH (intros) < TREE_VEC_LENGTH (parms))
    {
      error_at (input_location, "all template parameters of %qD must "
				"be introduced", tmpl);
      return false;
    }

   return true;
}

/* Associates a constraint check to the current template based on the
   introduction parameters.  INTRO_LIST must be a TREE_VEC of WILDCARD_DECLs
   containing a chained PARM_DECL which contains the identifier as well as
   the source location. TMPL_DECL is the decl for the concept being used.
   If we take a concept, C, this will form a check in the form of
   C<INTRO_LIST> filling in any extra arguments needed by the defaults
   deduced.

   Returns NULL_TREE if no concept could be matched and error_mark_node if
   an error occurred when matching.  */

tree
finish_template_introduction (tree tmpl_decl,
			      tree intro_list,
			      location_t intro_loc)
{
  /* Build a concept check to deduce the actual parameters.  */
  tree expr = build_concept_check (tmpl_decl, intro_list, tf_none);
  if (expr == error_mark_node)
    {
      error_at (intro_loc, "cannot deduce template parameters from "
			   "introduction list");
      return error_mark_node;
    }

  if (!check_introduction_list (intro_list, expr))
    return error_mark_node;

  tree parms = deduce_concept_introduction (expr);
  if (!parms)
    return NULL_TREE;

  /* Build template parameter scope for introduction.  */
  tree parm_list = NULL_TREE;
  begin_template_parm_list ();
  int nargs = MIN (TREE_VEC_LENGTH (parms), TREE_VEC_LENGTH (intro_list));
  for (int n = 0; n < nargs; )
    parm_list = process_introduction_parms (parm_list, parms, n);
  parm_list = end_template_parm_list (parm_list);

  /* Update the number of arguments to reflect the number of deduced
     template parameter introductions.  */
  nargs = TREE_VEC_LENGTH (parm_list);

  /* Determine if any errors occurred during matching.  */
  for (int i = 0; i < TREE_VEC_LENGTH (parm_list); ++i)
    if (TREE_VALUE (TREE_VEC_ELT (parm_list, i)) == error_mark_node)
      {
        end_template_decl ();
        return error_mark_node;
      }

  /* Build a concept check for our constraint.  */
  tree check_args = make_tree_vec (nargs);
  int n = 0;
  for (; n < TREE_VEC_LENGTH (parm_list); ++n)
    {
      tree parm = TREE_VEC_ELT (parm_list, n);
      TREE_VEC_ELT (check_args, n) = template_parm_to_arg (parm);
    }
  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (check_args, n);

  /* If the template expects more parameters we should be able
     to use the defaults from our deduced concept.  */
  for (; n < TREE_VEC_LENGTH (parms); ++n)
    TREE_VEC_ELT (check_args, n) = TREE_VEC_ELT (parms, n);

  /* Associate the constraint.  */
  tree check = build_concept_check (tmpl_decl,
				    check_args,
				    tf_warning_or_error);
  TEMPLATE_PARMS_CONSTRAINTS (current_template_parms) = check;

  return parm_list;
}


/* Given the concept check T from a constrained-type-specifier, extract
   its TMPL and ARGS.  FIXME why do we need two different forms of
   constrained-type-specifier?  */

void
placeholder_extract_concept_and_args (tree t, tree &tmpl, tree &args)
{
  if (concept_check_p (t))
    {
      t = unpack_concept_check (t);
      tmpl = TREE_OPERAND (t, 0);
      if (TREE_CODE (tmpl) == OVERLOAD)
        tmpl = OVL_FIRST (tmpl);
      args = TREE_OPERAND (t, 1);
      return;
    }

  if (TREE_CODE (t) == TYPE_DECL)
    {
      /* A constrained parameter.  Build a constraint check
         based on the prototype parameter and then extract the
         arguments from that.  */
      tree proto = CONSTRAINED_PARM_PROTOTYPE (t);
      tree check = finish_shorthand_constraint (proto, t);
      placeholder_extract_concept_and_args (check, tmpl, args);
      return;
    }
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

  tree t1, t2, a1, a2;
  placeholder_extract_concept_and_args (c1, t1, a1);
  placeholder_extract_concept_and_args (c2, t2, a2);

  if (t1 != t2)
    return false;

  int len1 = TREE_VEC_LENGTH (a1);
  int len2 = TREE_VEC_LENGTH (a2);
  if (len1 != len2)
    return false;

  /* Skip the first argument so we don't infinitely recurse.
     Also, they may differ in template parameter index.  */
  for (int i = 1; i < len1; ++i)
    {
      tree t1 = TREE_VEC_ELT (a1, i);
      tree t2 = TREE_VEC_ELT (a2, i);
      if (!template_args_equal (t1, t2))
      return false;
    }
  return true;
}

/* Return a hash value for the placeholder ATOMIC_CONSTR C.  */

hashval_t
hash_placeholder_constraint (tree c)
{
  tree t, a;
  placeholder_extract_concept_and_args (c, t, a);

  /* Like hash_tmpl_and_args, but skip the first argument.  */
  hashval_t val = iterative_hash_object (DECL_UID (t), 0);

  for (int i = TREE_VEC_LENGTH (a)-1; i > 0; --i)
    val = iterative_hash_template_arg (TREE_VEC_ELT (a, i), val);

  return val;
}

/* Substitute through the simple requirement.  */

static tree
tsubst_valid_expression_requirement (tree t, tree args, subst_info info)
{
  tree r = tsubst_expr (t, args, info.complain, info.in_decl, false);
  if (convert_to_void (r, ICV_STATEMENT, info.complain) == error_mark_node)
    return error_mark_node;
  return r;
}


/* Substitute through the simple requirement.  */

static tree
tsubst_simple_requirement (tree t, tree args, subst_info info)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree expr = tsubst_valid_expression_requirement (t0, args, info);
  if (expr == error_mark_node)
    return error_mark_node;
  return finish_simple_requirement (EXPR_LOCATION (t), expr);
}

/* Substitute through the type requirement.  */

static tree
tsubst_type_requirement (tree t, tree args, subst_info info)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree type = tsubst (t0, args, info.complain, info.in_decl);
  if (type == error_mark_node)
    return error_mark_node;
  return finish_type_requirement (EXPR_LOCATION (t), type);
}

/* True if TYPE can be deduced from EXPR.  */

static bool
type_deducible_p (tree expr, tree type, tree placeholder, tree args,
                  subst_info info)
{
  /* Make sure deduction is performed against ( EXPR ), so that
     references are preserved in the result.  */
  expr = force_paren_expr_uneval (expr);

  /* Replace the constraints with the instantiated constraints. This
     substitutes args into any template parameters in the trailing
     result type.  */
  tree saved_constr = PLACEHOLDER_TYPE_CONSTRAINTS (placeholder);
  tree subst_constr
    = tsubst_constraint (saved_constr,
			 args,
			 info.complain | tf_partial,
			 info.in_decl);

  if (subst_constr == error_mark_node)
    return false;

  PLACEHOLDER_TYPE_CONSTRAINTS (placeholder) = subst_constr;

  /* Temporarily unlink the canonical type.  */
  tree saved_type = TYPE_CANONICAL (placeholder);
  TYPE_CANONICAL (placeholder) = NULL_TREE;

  tree deduced_type
    = do_auto_deduction (type,
			 expr,
			 placeholder,
			 info.complain,
			 adc_requirement);

  PLACEHOLDER_TYPE_CONSTRAINTS (placeholder) = saved_constr;
  TYPE_CANONICAL (placeholder) = saved_type;

  if (deduced_type == error_mark_node)
    return false;

  return true;
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
tsubst_compound_requirement (tree t, tree args, subst_info info)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree t1 = TREE_OPERAND (t, 1);
  tree expr = tsubst_valid_expression_requirement (t0, args, info);
  if (expr == error_mark_node)
    return error_mark_node;

  /* Check the noexcept condition.  */
  bool noexcept_p = COMPOUND_REQ_NOEXCEPT_P (t);
  if (noexcept_p && !expr_noexcept_p (expr, tf_none))
    return error_mark_node;

  /* Substitute through the type expression, if any.  */
  tree type = tsubst (t1, args, info.complain, info.in_decl);
  if (type == error_mark_node)
    return error_mark_node;

  subst_info quiet (tf_none, info.in_decl);

  /* Check expression against the result type.  */
  if (type)
    {
      if (tree placeholder = type_uses_auto (type))
	{
	  if (!type_deducible_p (expr, type, placeholder, args, quiet))
	    return error_mark_node;
	}
      else if (!expression_convertible_p (expr, type, quiet))
	return error_mark_node;
    }

  return finish_compound_requirement (EXPR_LOCATION (t),
				      expr, type, noexcept_p);
}

static tree
tsubst_nested_requirement (tree t, tree args, subst_info info)
{
  gcc_assert (!uses_template_parms (args));

  /* Ensure that we're in an evaluation context prior to satisfaction.  */
  tree norm = TREE_VALUE (TREE_TYPE (t));
  tree result = satisfy_constraint (norm, args, info);
  if (result == error_mark_node && info.quiet ())
    {
      subst_info noisy (tf_warning_or_error, info.in_decl);
      satisfy_constraint (norm, args, noisy);
    }
  if (result != boolean_true_node)
    return error_mark_node;
  return result;
}

/* Substitute ARGS into the requirement T.  */

static tree
tsubst_requirement (tree t, tree args, subst_info info)
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

/* Substitute ARGS into the list of requirements T. Note that
   substitution failures here result in ill-formed programs. */

static tree
tsubst_requirement_body (tree t, tree args, subst_info info)
{
  tree result = NULL_TREE;
  while (t)
    {
      tree req = tsubst_requirement (TREE_VALUE (t), args, info);
      if (req == error_mark_node)
	return error_mark_node;
      result = tree_cons (NULL_TREE, req, result);
      t = TREE_CHAIN (t);
    }
  return nreverse (result);
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

/* Substitute through as if checking function parameter types. This
   will diagnose common parameter type errors.  Returns error_mark_node
   if an error occurred.  */

static tree
check_constaint_variables (tree t, tree args, subst_info info)
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

/* A subroutine of tsubst_parameterized_constraint. Substitute ARGS
   into the parameter list T, producing a sequence of constraint
   variables, declared in the current scope.

   Note that the caller must establish a local specialization stack
   prior to calling this function since this substitution will
   declare the substituted parameters. */

static tree
tsubst_constraint_variables (tree t, tree args, subst_info info)
{
  /* Perform a trial substitution to check for type errors.  */
  tree parms = check_constaint_variables (t, args, info);
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

/* Substitute ARGS into the requires-expression T. [8.4.7]p6. The
   substitution of template arguments into a requires-expression
   may result in the formation of invalid types or expressions
   in its requirements ... In such cases, the expression evaluates
   to false; it does not cause the program to be ill-formed.

   However, there are cases where substitution must produce a
   new requires-expression, that is not a template constraint.
   For example:

        template<typename T>
        class X {
          template<typename U>
          static constexpr bool var = requires (U u) { T::fn(u); };
        };

   In the instantiation of X<Y> (assuming Y defines fn), then the
   instantiated requires-expression would include Y::fn(u). If any
   substitution in the requires-expression fails, we can immediately
   fold the expression to false, as would be the case e.g., when
   instantiation X<int>.  */

tree
tsubst_requires_expr (tree t, tree args,
		      tsubst_flags_t complain, tree in_decl)
{
  local_specialization_stack stack (lss_copy);

  subst_info info (complain, in_decl);

  /* A requires-expression is an unevaluated context.  */
  cp_unevaluated u;

  tree parms = TREE_OPERAND (t, 0);
  if (parms)
    {
      parms = tsubst_constraint_variables (parms, args, info);
      if (parms == error_mark_node)
	return boolean_false_node;
    }

  tree reqs = TREE_OPERAND (t, 1);
  reqs = tsubst_requirement_body (reqs, args, info);
  if (reqs == error_mark_node)
    return boolean_false_node;

  /* In certain cases, produce a new requires-expression.
     Otherwise the value of the expression is true.  */
  if (processing_template_decl && uses_template_parms (args))
    return finish_requires_expr (cp_expr_location (t), parms, reqs);

  return boolean_true_node;
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
      tree new_arg = NULL_TREE;
      if (TYPE_P (arg))
        {
          /* If a template parameter is declared with a placeholder, we can
             get those in the argument list if decltype is applied to the
             placeholder. For example:

		template<auto T>
		  requires C<decltype(T)>
		void f() { }

	     The normalized argument for C will be an auto type, so we'll
             need to deduce the actual argument from the corresponding
             initializer (whatever argument is provided for T), and use
             that result in the instantiated parameter mapping.  */
          if (tree auto_node = type_uses_auto (arg))
            {
              int level;
              int index;
	      template_parm_level_and_index (parm, &level, &index);
	      tree init = TMPL_ARG (args, level, index);
              new_arg = do_auto_deduction (arg, init, auto_node,
					   complain, adc_variable_type,
					   make_tree_vec (0));
            }
        }
      else if (ARGUMENT_PACK_P (arg))
	new_arg = tsubst_argument_pack (arg, args, complain, in_decl);
      if (!new_arg)
	{
	  new_arg = tsubst_template_arg (arg, args, complain, in_decl);
	  if (TYPE_P (new_arg))
	    new_arg = canonicalize_type_argument (new_arg, complain);
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

/* Hash functions for satisfaction entries.  */

struct GTY((for_user)) sat_entry
{
  tree constr;
  tree args;
  tree result;
};

struct sat_hasher : ggc_ptr_hash<sat_entry>
{
  static hashval_t hash (sat_entry *e)
  {
    hashval_t value = hash_atomic_constraint (e->constr);
    return iterative_hash_template_arg (e->args, value);
  }

  static bool equal (sat_entry *e1, sat_entry *e2)
  {
    if (!atomic_constraints_identical_p (e1->constr, e2->constr))
      return false;
    return template_args_equal (e1->args, e2->args);
  }
};

/* Cache the result of satisfy_atom.  */
static GTY((deletable)) hash_table<sat_hasher> *sat_cache;

/* Cache the result of constraint_satisfaction_value.  */
static GTY((deletable)) hash_map<tree, tree> *decl_satisfied_cache;

static tree
get_satisfaction (tree constr, tree args)
{
  if (!sat_cache)
    return NULL_TREE;
  sat_entry elt = { constr, args, NULL_TREE };
  sat_entry* found = sat_cache->find (&elt);
  if (found)
    return found->result;
  else
    return NULL_TREE;
}

static void
save_satisfaction (tree constr, tree args, tree result)
{
  if (!sat_cache)
    sat_cache = hash_table<sat_hasher>::create_ggc (31);
  sat_entry elt = {constr, args, result};
  sat_entry** slot = sat_cache->find_slot (&elt, INSERT);
  sat_entry* entry = ggc_alloc<sat_entry> ();
  *entry = elt;
  *slot = entry;
}

void
clear_satisfaction_cache ()
{
  if (sat_cache)
    sat_cache->empty ();
  if (decl_satisfied_cache)
    decl_satisfied_cache->empty ();
}

/* A tool to help manage satisfaction caching in satisfy_constraint_r.
   Note the cache is only used when not diagnosing errors.  */

struct satisfaction_cache
{
  satisfaction_cache (tree constr, tree args, tsubst_flags_t complain)
    : constr(constr), args(args), complain(complain)
  { }

  tree get ()
  {
    if (complain == tf_none)
      return get_satisfaction (constr, args);
    return NULL_TREE;
  }

  tree save (tree result)
  {
    if (complain == tf_none)
      save_satisfaction (constr, args, result);
    return result;
  }

  tree constr;
  tree args;
  tsubst_flags_t complain;
};

static int satisfying_constraint = 0;

/* Returns true if we are currently satisfying a constraint.

   This is used to guard against recursive calls to evaluate_concept_check
   during template argument substitution.

   TODO: Do we need this now that we fully normalize prior to evaluation?
   I think not. */

bool
satisfying_constraint_p ()
{
  return satisfying_constraint;
}

/* Substitute ARGS into constraint-expression T during instantiation of
   a member of a class template.  */

tree
tsubst_constraint (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  /* We also don't want to evaluate concept-checks when substituting the
     constraint-expressions of a declaration.  */
  processing_constraint_expression_sentinel s;
  tree expr = tsubst_expr (t, args, complain, in_decl, false);
  return expr;
}

static tree satisfy_constraint_r (tree, tree, subst_info info);

/* Compute the satisfaction of a conjunction.  */

static tree
satisfy_conjunction (tree t, tree args, subst_info info)
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
satisfy_disjunction (tree t, tree args, subst_info info)
{
  /* Evaluate the operands quietly.  */
  subst_info quiet (tf_none, NULL_TREE);

  /* Register the constraint for diagnostics, if needed.  */
  diagnosing_failed_constraint failure (t, args, info.noisy ());

  tree lhs = satisfy_constraint_r (TREE_OPERAND (t, 0), args, quiet);
  if (lhs == boolean_true_node)
    return boolean_true_node;
  tree rhs = satisfy_constraint_r (TREE_OPERAND (t, 1), args, quiet);
  if (rhs != boolean_true_node && info.noisy ())
    {
      cp_expr disj_expr = CONSTR_EXPR (t);
      inform (disj_expr.get_location (),
	      "no operand of the disjunction is satisfied");
      if (diagnosing_failed_constraint::replay_errors_p ())
	{
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
	      satisfy_constraint_r (norm_op, args, info);
	    }
	}
    }
  return rhs;
}

/* Ensures that T is a truth value and not (accidentally, as sometimes
   happens) an integer value.  */

tree
satisfaction_value (tree t)
{
  if (t == error_mark_node)
    return t;
  if (t == boolean_true_node || t == integer_one_node)
    return boolean_true_node;
  if (t == boolean_false_node || t == integer_zero_node)
    return boolean_false_node;

  /* Anything else should be invalid.  */
  gcc_assert (false);
}

/* Build a new template argument list with template arguments corresponding
   to the parameters used in an atomic constraint.  */

tree
get_mapped_args (tree map)
{
  /* No map, no arguments.  */
  if (!map)
    return NULL_TREE;

  /* Find the mapped parameter with the highest level.  */
  int count = 0;
  for (tree p = map; p; p = TREE_CHAIN (p))
    {
      int level;
      int index;
      template_parm_level_and_index (TREE_VALUE (p), &level, &index);
      if (level > count)
        count = level;
    }

  /* Place each argument at its corresponding position in the argument
     list. Note that the list will be sparse (not all arguments supplied),
     but instantiation is guaranteed to only use the parameters in the
     mapping, so null arguments would never be used.  */
  auto_vec< vec<tree> > lists (count);
  lists.quick_grow_cleared (count);
  for (tree p = map; p; p = TREE_CHAIN (p))
    {
      int level;
      int index;
      template_parm_level_and_index (TREE_VALUE (p), &level, &index);

      /* Insert the argument into its corresponding position.  */
      vec<tree> &list = lists[level - 1];
      if (index >= (int)list.length ())
	list.safe_grow_cleared (index + 1);
      list[index] = TREE_PURPOSE (p);
    }

  /* Build the new argument list.  */
  tree args = make_tree_vec (lists.length ());
  for (unsigned i = 0; i != lists.length (); ++i)
    {
      vec<tree> &list = lists[i];
      tree level = make_tree_vec (list.length ());
      for (unsigned j = 0; j < list.length(); ++j)
	TREE_VEC_ELT (level, j) = list[j];
      SET_TMPL_ARGS_LEVEL (args, i + 1, level);
      list.release ();
    }
  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (args, 0);

  return args;
}

static void diagnose_atomic_constraint (tree, tree, tree, subst_info);

/* Compute the satisfaction of an atomic constraint.  */

static tree
satisfy_atom (tree t, tree args, subst_info info)
{
  satisfaction_cache cache (t, args, info.complain);
  if (tree r = cache.get ())
    return r;

  /* Perform substitution quietly.  */
  subst_info quiet (tf_none, NULL_TREE);

  /* In case there is a diagnostic, we want to establish the context
     prior to printing errors.  If no errors occur, this context is
     removed before returning.  */
  diagnosing_failed_constraint failure (t, args, info.noisy ());

  /* Instantiate the parameter mapping.  */
  tree map = tsubst_parameter_mapping (ATOMIC_CONSTR_MAP (t), args, quiet);
  if (map == error_mark_node)
    {
      /* If instantiation of the parameter mapping fails, the program
         is ill-formed.  */
      if (info.noisy())
	tsubst_parameter_mapping (ATOMIC_CONSTR_MAP (t), args, info);
      return cache.save (boolean_false_node);
    }

  /* Rebuild the argument vector from the parameter mapping.  */
  args = get_mapped_args (map);

  /* Apply the parameter mapping (i.e., just substitute).  */
  tree expr = ATOMIC_CONSTR_EXPR (t);
  tree result = tsubst_expr (expr, args, quiet.complain, quiet.in_decl, false);
  if (result == error_mark_node)
    {
      /* If substitution results in an invalid type or expression, the constraint
	 is not satisfied. Replay the substitution.  */
      if (info.noisy ())
	tsubst_expr (expr, args, info.complain, info.in_decl, false);
      return cache.save (boolean_false_node);
    }

  /* [17.4.1.2] ... lvalue-to-rvalue conversion is performed as necessary,
     and EXPR shall be a constant expression of type bool.  */
  result = force_rvalue (result, info.complain);
  if (result == error_mark_node)
    return cache.save (error_mark_node);
  if (!same_type_p (TREE_TYPE (result), boolean_type_node))
    {
      if (info.noisy ())
	diagnose_atomic_constraint (t, map, result, info);
      return cache.save (error_mark_node);
    }

  /* Compute the value of the constraint.  */
  if (info.noisy ())
    result = cxx_constant_value (result);
  else
    {
      result = maybe_constant_value (result);
      if (!TREE_CONSTANT (result))
	result = error_mark_node;
    }
  result = satisfaction_value (result);
  if (result == boolean_false_node && info.noisy ())
    diagnose_atomic_constraint (t, map, result, info);

  return cache.save (result);
}

/* Determine if the normalized constraint T is satisfied.
   Returns boolean_true_node if the expression/constraint is
   satisfied, boolean_false_node if not, and error_mark_node
   if the there was an error evaluating the constraint.

   The parameter mapping of atomic constraints is simply the
   set of template arguments that will be substituted into
   the expression, regardless of template parameters appearing
   withing. Whether a template argument is used in the atomic
   constraint only matters for subsumption.  */

static tree
satisfy_constraint_r (tree t, tree args, subst_info info)
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
satisfy_constraint (tree t, tree args, subst_info info)
{
  auto_timevar time (TV_CONSTRAINT_SAT);

  /* Turn off template processing. Constraint satisfaction only applies
     to non-dependent terms, so we want to ensure full checking here.  */
  processing_template_decl_sentinel proc (true);

  /* We need to check access during satisfaction.  */
  deferring_access_check_sentinel acs (dk_no_deferred);

  return satisfy_constraint_r (t, args, info);
}

/* Check the normalized constraints T against ARGS, returning a satisfaction
   value (either true, false, or error).  */

static tree
satisfy_associated_constraints (tree t, tree args, subst_info info)
{
  /* If there are no constraints then this is trivially satisfied.  */
  if (!t)
    return boolean_true_node;

  /* If any arguments depend on template parameters, we can't
     check constraints. Pretend they're satisfied for now.  */
  if (args && uses_template_parms (args))
    return boolean_true_node;

  return satisfy_constraint (t, args, info);
}

/* Evaluate EXPR as a constraint expression using ARGS, returning a
   satisfaction value. */

static tree
satisfy_constraint_expression (tree t, tree args, subst_info info)
{
  if (t == error_mark_node)
    return error_mark_node;

  gcc_assert (EXPR_P (t));

  /* Get the normalized constraints.  */
  tree norm;
  if (args == NULL_TREE && concept_check_p (t))
    {
      tree id = unpack_concept_check (t);
      args = TREE_OPERAND (id, 1);
      tree tmpl = get_concept_check_template (id);
      norm = normalize_concept_definition (tmpl, info.noisy ());
    }
  else
    norm = normalize_constraint_expression (t, info.noisy ());

  /* Perform satisfaction.  */
  return satisfy_constraint (norm, args, info);
}

/* Used only to evaluate requires-expressions during constant expression
   evaluation.  */

tree
satisfy_constraint_expression (tree expr)
{
  subst_info info (tf_none, NULL_TREE);
  return satisfy_constraint_expression (expr, NULL_TREE, info);
}

static tree
satisfy_declaration_constraints (tree t, subst_info info)
{
  gcc_assert (DECL_P (t));

  /* For inherited constructors, consider the original declaration;
     it has the correct template information attached. */
  if (flag_new_inheriting_ctors)
    t = strip_inheriting_ctors (t);

  /* Update the declaration for diagnostics.  */
  info.in_decl = t;

  if (info.quiet ())
    if (tree *result = hash_map_safe_get (decl_satisfied_cache, t))
      return *result;

  /* Get the normalized constraints.  */
  tree norm = NULL_TREE;
  tree args = NULL_TREE;
  if (tree ti = DECL_TEMPLATE_INFO (t))
    {
      tree tmpl = TI_TEMPLATE (ti);
      norm = normalize_template_requirements (tmpl, info.noisy ());

      /* The initial parameter mapping is the complete set of
	 template arguments substituted into the declaration.  */
      args = TI_ARGS (ti);
    }
  else
    {
      /* These should be empty until we allow constraints on non-templates.  */
      norm = normalize_nontemplate_requirements (t, info.noisy ());
    }

  tree result = boolean_true_node;
  if (norm)
    {
      if (!push_tinst_level (t))
	return result;
      push_access_scope (t);
      result = satisfy_associated_constraints (norm, args, info);
      pop_access_scope (t);
      pop_tinst_level ();
    }

  if (info.quiet ())
    hash_map_safe_put<hm_ggc> (decl_satisfied_cache, t, result);

  return result;
}

static tree
satisfy_declaration_constraints (tree t, tree args, subst_info info)
{
  /* Update the declaration for diagnostics.  */
  info.in_decl = t;

  gcc_assert (TREE_CODE (t) == TEMPLATE_DECL);
  if (tree norm = normalize_template_requirements (t, info.noisy ()))
    {
      tree pattern = DECL_TEMPLATE_RESULT (t);
      push_access_scope (pattern);
      tree result = satisfy_associated_constraints (norm, args, info);
      pop_access_scope (pattern);
      return result;
    }

  return boolean_true_node;
}

static tree
constraint_satisfaction_value (tree t, tsubst_flags_t complain)
{
  subst_info info (complain, NULL_TREE);
  tree r;
  if (DECL_P (t))
    r = satisfy_declaration_constraints (t, info);
  else
    r = satisfy_constraint_expression (t, NULL_TREE, info);
  if (r == error_mark_node && info.quiet ()
      && !(DECL_P (t) && TREE_NO_WARNING (t)))
      {
	constraint_satisfaction_value (t, tf_warning_or_error);
	if (DECL_P (t))
	  /* Avoid giving these errors again.  */
	  TREE_NO_WARNING (t) = true;
      }
  return r;
}

static tree
constraint_satisfaction_value (tree t, tree args, tsubst_flags_t complain)
{
  subst_info info (complain, NULL_TREE);
  tree r;
  if (DECL_P (t))
    r = satisfy_declaration_constraints (t, args, info);
  else
    r = satisfy_constraint_expression (t, args, info);
  if (r == error_mark_node && info.quiet ())
    constraint_satisfaction_value (t, args, tf_warning_or_error);
  return r;
}

/* True iff the result of satisfying T is BOOLEAN_TRUE_NODE and false
   otherwise, even in the case of errors.  */

bool
constraints_satisfied_p (tree t)
{
  return constraint_satisfaction_value (t, tf_none) == boolean_true_node;
}

/* True iff the result of satisfying T with ARGS is BOOLEAN_TRUE_NODE
    and false otherwise, even in the case of errors.  */

bool
constraints_satisfied_p (tree t, tree args)
{
  return constraint_satisfaction_value (t, args, tf_none) == boolean_true_node;
}

/* Evaluate a concept check of the form C<ARGS>. This is only used for the
   evaluation of template-ids as id-expressions.  */

tree
evaluate_concept_check (tree check, tsubst_flags_t complain)
{
  if (check == error_mark_node)
    return error_mark_node;

  gcc_assert (concept_check_p (check));

  /* Check for satisfaction without diagnostics.  */
  subst_info quiet (tf_none, NULL_TREE);
  tree result = satisfy_constraint_expression (check, NULL_TREE, quiet);
  if (result == error_mark_node && (complain & tf_error))
  {
    /* Replay the error with re-normalized requirements.  */
    subst_info noisy (tf_warning_or_error, NULL_TREE);
    satisfy_constraint_expression (check, NULL_TREE, noisy);
  }
  return result;
}

/*---------------------------------------------------------------------------
                Semantic analysis of requires-expressions
---------------------------------------------------------------------------*/

/* Finish a requires expression for the given PARMS (possibly
   null) and the non-empty sequence of requirements.  */

tree
finish_requires_expr (location_t loc, tree parms, tree reqs)
{
  /* Modify the declared parameters by removing their context
     so they don't refer to the enclosing scope and explicitly
     indicating that they are constraint variables. */
  for (tree parm = parms; parm; parm = DECL_CHAIN (parm))
    {
      DECL_CONTEXT (parm) = NULL_TREE;
      CONSTRAINT_VAR_P (parm) = true;
    }

  /* Build the node. */
  tree r = build_min (REQUIRES_EXPR, boolean_type_node, parms, reqs);
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
   its properties. if TYPE is non-null, then it specifies either
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
  /* Save the normalized constraint and complete set of normalization
     arguments with the requirement.  We keep the complete set of arguments
     around for re-normalization during diagnostics.  */
  tree args = current_template_parms
    ? template_parms_to_args (current_template_parms) : NULL_TREE;
  tree norm = normalize_constraint_expression (expr, args, false);
  tree info = build_tree_list (args, norm);

  /* Build the constraint, saving its normalization as its type.  */
  tree r = build1 (NESTED_REQ, info, expr);
  SET_EXPR_LOCATION (r, loc);
  return r;
}

/* Check that FN satisfies the structural requirements of a
   function concept definition.  */
tree
check_function_concept (tree fn)
{
  /* Check that the function is comprised of only a return statement.  */
  tree body = DECL_SAVED_TREE (fn);
  if (TREE_CODE (body) == BIND_EXPR)
    body = BIND_EXPR_BODY (body);

  /* Sometimes a function call results in the creation of clean up
     points. Allow these to be preserved in the body of the
     constraint, as we might actually need them for some constexpr
     evaluations.  */
  if (TREE_CODE (body) == CLEANUP_POINT_EXPR)
    body = TREE_OPERAND (body, 0);

  /* Check that the definition is written correctly.  */
  if (TREE_CODE (body) != RETURN_EXPR)
    {
      location_t loc = DECL_SOURCE_LOCATION (fn);
      if (TREE_CODE (body) == STATEMENT_LIST && !STATEMENT_LIST_HEAD (body))
	{
	  if (seen_error ())
	    /* The definition was probably erroneous, not empty.  */;
	  else
	    error_at (loc, "definition of concept %qD is empty", fn);
	}
      else
        error_at (loc, "definition of concept %qD has multiple statements", fn);
    }

  return NULL_TREE;
}


// Check that a constrained friend declaration function declaration,
// FN, is admissible. This is the case only when the declaration depends
// on template parameters and does not declare a specialization.
void
check_constrained_friend (tree fn, tree reqs)
{
  if (fn == error_mark_node)
    return;
  gcc_assert (TREE_CODE (fn) == FUNCTION_DECL);

  // If there are not constraints, this cannot be an error.
  if (!reqs)
    return;

  // Constrained friend functions that don't depend on template
  // arguments are effectively meaningless.
  if (!uses_template_parms (TREE_TYPE (fn)))
    {
      error_at (location_of (fn),
		"constrained friend does not depend on template parameters");
      return;
    }
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
   constraints. This is the case when A's constraints subsume B's and
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

/* Returns true when the constraints in A subsume those in B.  */

bool
subsumes_constraints (tree a, tree b)
{
  gcc_assert (!a || TREE_CODE (a) == CONSTRAINT_INFO);
  gcc_assert (!b || TREE_CODE (b) == CONSTRAINT_INFO);
  return subsumes (a, b);
}

/* Returns true when the constraints in CI (with arguments
   ARGS) strictly subsume the associated constraints of TMPL.  */

bool
strictly_subsumes (tree ci, tree args, tree tmpl)
{
  tree n1 = get_normalized_constraints_from_info (ci, args, NULL_TREE);
  tree n2 = get_normalized_constraints_from_decl (tmpl);

  return subsumes (n1, n2) && !subsumes (n2, n1);
}

/* REturns true when the constraints in CI (with arguments ARGS) subsume
   the associated constraints of TMPL.  */

bool
weakly_subsumes (tree ci, tree args, tree tmpl)
{
  tree n1 = get_normalized_constraints_from_info (ci, args, NULL_TREE);
  tree n2 = get_normalized_constraints_from_decl (tmpl);

  return subsumes (n1, n2);
}

/* Determines which of the declarations, A or B, is more constrained.
   That is, which declaration's constraints subsume but are not subsumed
   by the other's?

   Returns 1 if D1 is more constrained than D2, -1 if D2 is more constrained
   than D1, and 0 otherwise. */

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
      tree id = unpack_concept_check (src);
      tree tmpl = TREE_OPERAND (id, 0);
      if (OVL_P (tmpl))
	tmpl = OVL_FIRST (tmpl);
      return DECL_SOURCE_LOCATION (tmpl);
    }

  return input_location;
}

/* Emit a diagnostic for a failed trait.  */

void
diagnose_trait_expr (tree expr, tree map)
{
  location_t loc = cp_expr_location (expr);
  tree args = get_mapped_args (map);

  /* Build a "fake" version of the instantiated trait, so we can
     get the instantiated types from result.  */
  ++processing_template_decl;
  expr = tsubst_expr (expr, args, tf_none, NULL_TREE, false);
  --processing_template_decl;

  tree t1 = TRAIT_EXPR_TYPE1 (expr);
  tree t2 = TRAIT_EXPR_TYPE2 (expr);
  switch (TRAIT_EXPR_KIND (expr))
    {
    case CPTK_HAS_NOTHROW_ASSIGN:
      inform (loc, "  %qT is not %<nothrow%> copy assignable", t1);
      break;
    case CPTK_HAS_NOTHROW_CONSTRUCTOR:
      inform (loc, "  %qT is not %<nothrow%> default constructible", t1);
      break;
    case CPTK_HAS_NOTHROW_COPY:
      inform (loc, "  %qT is not %<nothrow%> copy constructible", t1);
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
    case CPTK_HAS_VIRTUAL_DESTRUCTOR:
      inform (loc, "  %qT does not have a virtual destructor", t1);
      break;
    case CPTK_IS_ABSTRACT:
      inform (loc, "  %qT is not an abstract class", t1);
      break;
    case CPTK_IS_BASE_OF:
      inform (loc, "  %qT is not a base of %qT", t1, t2);
      break;
    case CPTK_IS_CLASS:
      inform (loc, "  %qT is not a class", t1);
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
    case CPTK_IS_LITERAL_TYPE:
      inform (loc, "  %qT is not a literal type", t1);
      break;
    case CPTK_IS_POD:
      inform (loc, "  %qT is not a POD type", t1);
      break;
    case CPTK_IS_POLYMORPHIC:
      inform (loc, "  %qT is not a polymorphic type", t1);
      break;
    case CPTK_IS_SAME_AS:
      inform (loc, "  %qT is not the same as %qT", t1, t2);
      break;
    case CPTK_IS_STD_LAYOUT:
      inform (loc, "  %qT is not an standard layout type", t1);
      break;
    case CPTK_IS_TRIVIAL:
      inform (loc, "  %qT is not a trivial type", t1);
      break;
    case CPTK_IS_UNION:
      inform (loc, "  %qT is not a union", t1);
      break;
    default:
      gcc_unreachable ();
    }
}

static tree
diagnose_valid_expression (tree expr, tree args, tree in_decl)
{
  tree result = tsubst_expr (expr, args, tf_none, in_decl, false);
  if (result != error_mark_node)
    return result;

  location_t loc = cp_expr_loc_or_input_loc (expr);
  if (diagnosing_failed_constraint::replay_errors_p ())
    {
      /* Replay the substitution error.  */
      inform (loc, "the required expression %qE is invalid, because", expr);
      tsubst_expr (expr, args, tf_error, in_decl, false);
    }
  else
    inform (loc, "the required expression %qE is invalid", expr);

  return error_mark_node;
}

static tree
diagnose_valid_type (tree type, tree args, tree in_decl)
{
  tree result = tsubst (type, args, tf_none, in_decl);
  if (result != error_mark_node)
    return result;

  location_t loc = cp_expr_loc_or_input_loc (type);
  if (diagnosing_failed_constraint::replay_errors_p ())
    {
      /* Replay the substitution error.  */
      inform (loc, "the required type %qT is invalid, because", type);
      tsubst (type, args, tf_error, in_decl);
    }
  else
    inform (loc, "the required type %qT is invalid", type);

  return error_mark_node;
}

static void
diagnose_simple_requirement (tree req, tree args, tree in_decl)
{
  diagnose_valid_expression (TREE_OPERAND (req, 0), args, in_decl);
}

static void
diagnose_compound_requirement (tree req, tree args, tree in_decl)
{
  tree expr = TREE_OPERAND (req, 0);
  expr = diagnose_valid_expression (expr, args, in_decl);
  if (expr == error_mark_node)
    return;

  location_t loc = cp_expr_loc_or_input_loc (expr);

  /* Check the noexcept condition.  */
  if (COMPOUND_REQ_NOEXCEPT_P (req) && !expr_noexcept_p (expr, tf_none))
    inform (loc, "%qE is not %<noexcept%>", expr);

  tree type = TREE_OPERAND (req, 1);
  type = diagnose_valid_type (type, args, in_decl);
  if (type == error_mark_node)
    return;

  if (type)
    {
      subst_info quiet (tf_none, in_decl);
      subst_info noisy (tf_error, in_decl);

      /* Check the expression against the result type.  */
      if (tree placeholder = type_uses_auto (type))
	{
	  if (!type_deducible_p (expr, type, placeholder, args, quiet))
	    {
	      tree orig_expr = TREE_OPERAND (req, 0);
	      if (diagnosing_failed_constraint::replay_errors_p ())
		{
		  inform (loc,
			  "%qE does not satisfy return-type-requirement, "
			  "because", orig_expr);
		  /* Further explain the reason for the error.  */
		  type_deducible_p (expr, type, placeholder, args, noisy);
		}
	      else
		inform (loc, "%qE does not satisfy return-type-requirement",
			orig_expr);
	    }
	}
      else if (!expression_convertible_p (expr, type, quiet))
	{
	  tree orig_expr = TREE_OPERAND (req, 0);
	  if (diagnosing_failed_constraint::replay_errors_p ())
	    {
	      inform (loc, "cannot convert %qE to %qT because", orig_expr, type);
	      /* Further explain the reason for the error.  */
	      expression_convertible_p (expr, type, noisy);
	    }
	  else
	    inform (loc, "cannot convert %qE to %qT", orig_expr, type);
	}
    }
}

static void
diagnose_type_requirement (tree req, tree args, tree in_decl)
{
  tree type = TREE_OPERAND (req, 0);
  diagnose_valid_type (type, args, in_decl);
}

static void
diagnose_nested_requirement (tree req, tree args)
{
  /* Quietly check for satisfaction first. We can elaborate details
     later if needed.  */
  tree norm = TREE_VALUE (TREE_TYPE (req));
  subst_info info (tf_none, NULL_TREE);
  tree result = satisfy_constraint (norm, args, info);
  if (result == boolean_true_node)
    return;

  tree expr = TREE_OPERAND (req, 0);
  location_t loc = cp_expr_location (expr);
  if (diagnosing_failed_constraint::replay_errors_p ())
    {
      /* Replay the substitution error.  */
      inform (loc, "nested requirement %qE is not satisfied, because", expr);
      subst_info noisy (tf_warning_or_error, NULL_TREE);
      satisfy_constraint_expression (expr, args, noisy);
    }
  else
    inform (loc, "nested requirement %qE is not satisfied", expr);

}

static void
diagnose_requirement (tree req, tree args, tree in_decl)
{
  iloc_sentinel loc_s (cp_expr_location (req));
  switch (TREE_CODE (req))
    {
    case SIMPLE_REQ:
      return diagnose_simple_requirement (req, args, in_decl);
    case COMPOUND_REQ:
      return diagnose_compound_requirement (req, args, in_decl);
    case TYPE_REQ:
      return diagnose_type_requirement (req, args, in_decl);
    case NESTED_REQ:
      return diagnose_nested_requirement (req, args);
    default:
       gcc_unreachable ();
    }
}

static void
diagnose_requires_expr (tree expr, tree map, tree in_decl)
{
  local_specialization_stack stack (lss_copy);
  tree parms = TREE_OPERAND (expr, 0);
  tree body = TREE_OPERAND (expr, 1);
  tree args = get_mapped_args (map);

  cp_unevaluated u;
  subst_info info (tf_warning_or_error, NULL_TREE);
  tree vars = tsubst_constraint_variables (parms, args, info);
  if (vars == error_mark_node)
    return;

  tree p = body;
  while (p)
    {
      tree req = TREE_VALUE (p);
      diagnose_requirement (req, args, in_decl);
      p = TREE_CHAIN (p);
    }
}

/* Diagnose a substitution failure in the atomic constraint T when applied
   with the instantiated parameter mapping MAP.  */

static void
diagnose_atomic_constraint (tree t, tree map, tree result, subst_info info)
{
  /* If the constraint is already ill-formed, we've previously diagnosed
     the reason. We should still say why the constraints aren't satisfied.  */
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
      diagnose_trait_expr (expr, map);
      break;
    case REQUIRES_EXPR:
      diagnose_requires_expr (expr, map, info.in_decl);
      break;
    default:
      tree a = copy_node (t);
      ATOMIC_CONSTR_MAP (a) = map;
      if (!same_type_p (TREE_TYPE (result), boolean_type_node))
	error_at (loc, "constraint %qE has type %qT, not %<bool%>",
		  a, TREE_TYPE (result));
      else
	inform (loc, "the expression %qE evaluated to %<false%>", a);
      ggc_free (a);
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
   of T. Here, T can be either a constraint or a declaration.  */

void
diagnose_constraints (location_t loc, tree t, tree args)
{
  inform (loc, "constraints not satisfied");

  if (concepts_diagnostics_max_depth == 0)
    return;

  /* Replay satisfaction, but diagnose errors.  */
  if (!args)
    constraint_satisfaction_value (t, tf_warning_or_error);
  else
    constraint_satisfaction_value (t, args, tf_warning_or_error);

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
