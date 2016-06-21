/* Processing rules for constraints.
   Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

/*---------------------------------------------------------------------------
                       Operations on constraints
---------------------------------------------------------------------------*/

/* Returns true if C is a constraint tree code. Note that ERROR_MARK
   is a valid constraint.  */

static inline bool
constraint_p (tree_code c)
{
  return (PRED_CONSTR <= c && c <= DISJ_CONSTR) || c == ERROR_MARK;
}

/* Returns true if T is a constraint. Note that error_mark_node
   is a valid constraint.  */

bool
constraint_p (tree t)
{
  return constraint_p (TREE_CODE (t));
}

/* Make a predicate constraint from the given expression. */

tree
make_predicate_constraint (tree expr)
{
  return build_nt (PRED_CONSTR, expr);
}

/* Returns the conjunction of two constraints A and B. Note that
   conjoining a non-null constraint with NULL_TREE is an identity
   operation. That is, for non-null A,

      conjoin_constraints(a, NULL_TREE) == a

   and

      conjoin_constraints (NULL_TREE, a) == a

   If both A and B are NULL_TREE, the result is also NULL_TREE. */

tree
conjoin_constraints (tree a, tree b)
{
  gcc_assert (a ? constraint_p (a) : true);
  gcc_assert (b ? constraint_p (b) : true);
  if (a)
    return b ? build_nt (CONJ_CONSTR, a, b) : a;
  else if (b)
    return b;
  else
    return NULL_TREE;
}

/* Transform the vector of expressions in the T into a conjunction
   of requirements. T must be a TREE_VEC. */

tree
conjoin_constraints (tree t)
{
  gcc_assert (TREE_CODE (t) == TREE_VEC);
  tree r = NULL_TREE;
  for (int i = 0; i < TREE_VEC_LENGTH (t); ++i)
    r = conjoin_constraints (r, TREE_VEC_ELT (t, i));
  return r;
}

/* Returns true if T is a call expression to a function
   concept. */

bool
function_concept_check_p (tree t)
{
  gcc_assert (TREE_CODE (t) == CALL_EXPR);
  tree fn = CALL_EXPR_FN (t);
  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR
      && TREE_CODE (TREE_OPERAND (fn, 0)) == OVERLOAD)
    {
      tree f1 = get_first_fn (fn);
      if (TREE_CODE (f1) == TEMPLATE_DECL
	  && DECL_DECLARED_CONCEPT_P (DECL_TEMPLATE_RESULT (f1)))
        return true;
    }
  return false;
}

/*---------------------------------------------------------------------------
                    Resolution of qualified concept names
---------------------------------------------------------------------------*/

/* This facility is used to resolve constraint checks from
   requirement expressions. A constraint check is a call to
   a function template declared with the keyword 'concept'.

   The result of resolution is a pair (a TREE_LIST) whose value
   is the matched declaration, and whose purpose contains the
   coerced template arguments that can be substituted into the
   call.  */

// Given an overload set OVL, try to find a unique definition that can be
// instantiated by the template arguments ARGS.
//
// This function is not called for arbitrary call expressions. In particular,
// the call expression must be written with explicit template arguments
// and no function arguments. For example:
//
//      f<T, U>()
//
// If a single match is found, this returns a TREE_LIST whose VALUE
// is the constraint function (not the template), and its PURPOSE is
// the complete set of arguments substituted into the parameter list.
static tree
resolve_constraint_check (tree ovl, tree args)
{
  tree cands = NULL_TREE;
  for (tree p = ovl; p != NULL_TREE; p = OVL_NEXT (p))
    {
      // Get the next template overload.
      tree tmpl = OVL_CURRENT (p);
      if (TREE_CODE (tmpl) != TEMPLATE_DECL)
        continue;

      // Don't try to deduce checks for non-concepts. We often
      // end up trying to resolve constraints in functional casts
      // as part of a postfix-expression. We can save time and
      // headaches by not instantiating those declarations.
      //
      // NOTE: This masks a potential error, caused by instantiating
      // non-deduced contexts using placeholder arguments.
      tree fn = DECL_TEMPLATE_RESULT (tmpl);
      if (DECL_ARGUMENTS (fn))
        continue;
      if (!DECL_DECLARED_CONCEPT_P (fn))
        continue;

      // Remember the candidate if we can deduce a substitution.
      ++processing_template_decl;
      tree parms = TREE_VALUE (DECL_TEMPLATE_PARMS (tmpl));
      if (tree subst = coerce_template_parms (parms, args, tmpl))
        if (subst != error_mark_node)
          cands = tree_cons (subst, fn, cands);
      --processing_template_decl;
    }

  // If we didn't find a unique candidate, then this is
  // not a constraint check.
  if (!cands || TREE_CHAIN (cands))
    return NULL_TREE;

  return cands;
}

// Determine if the the call expression CALL is a constraint check, and
// return the concept declaration and arguments being checked. If CALL
// does not denote a constraint check, return NULL.
tree
resolve_constraint_check (tree call)
{
  gcc_assert (TREE_CODE (call) == CALL_EXPR);

  // A constraint check must be only a template-id expression. If
  // it's a call to a base-link, its function(s) should be a
  // template-id expression. If this is not a template-id, then it
  // cannot be a concept-check.
  tree target = CALL_EXPR_FN (call);
  if (BASELINK_P (target))
    target = BASELINK_FUNCTIONS (target);
  if (TREE_CODE (target) != TEMPLATE_ID_EXPR)
    return NULL_TREE;

  // Get the overload set and template arguments and try to
  // resolve the target.
  tree ovl = TREE_OPERAND (target, 0);

  /* This is a function call of a variable concept... ill-formed. */
  if (TREE_CODE (ovl) == TEMPLATE_DECL)
    {
      error_at (location_of (call),
		"function call of variable concept %qE", call);
      return error_mark_node;
    }

  tree args = TREE_OPERAND (target, 1);
  return resolve_constraint_check (ovl, args);
}

/* Returns a pair containing the checked variable concept
   and its associated prototype parameter.  The result
   is a TREE_LIST whose TREE_VALUE is the variable concept
   and whose TREE_PURPOSE is the prototype parameter.  */

tree
resolve_variable_concept_check (tree id)
{
  tree tmpl = TREE_OPERAND (id, 0);
  tree args = TREE_OPERAND (id, 1);

  if (!variable_concept_p (tmpl))
    return NULL_TREE;

  /* Make sure that we have the right parameters before
     assuming that it works.  Note that failing to deduce
     will result in diagnostics.  */
  tree parms = INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (tmpl));
  tree result = coerce_template_parms (parms, args, tmpl);
  if (result != error_mark_node)
    {
      tree decl = DECL_TEMPLATE_RESULT (tmpl);
      return build_tree_list (result, decl);
    }
  else
    return NULL_TREE;
}


/* Given a call expression or template-id expression to
  a concept EXPR possibly including a wildcard, deduce
  the concept being checked and the prototype parameter.
  Returns true if the constraint and prototype can be
  deduced and false otherwise.  Note that the CHECK and
  PROTO arguments are set to NULL_TREE if this returns
  false.  */

bool
deduce_constrained_parameter (tree expr, tree& check, tree& proto)
{
  tree info = NULL_TREE;
  if (TREE_CODE (expr) == TEMPLATE_ID_EXPR)
    info = resolve_variable_concept_check (expr);
  else if (TREE_CODE (expr) == CALL_EXPR)
    info = resolve_constraint_check (expr);
  else
    gcc_unreachable ();

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

// Given a call expression or template-id expression to a concept, EXPR,
// deduce the concept being checked and return the template arguments.
// Returns NULL_TREE if deduction fails.
static tree
deduce_concept_introduction (tree expr)
{
  tree info = NULL_TREE;
  if (TREE_CODE (expr) == TEMPLATE_ID_EXPR)
    info = resolve_variable_concept_check (expr);
  else if (TREE_CODE (expr) == CALL_EXPR)
    info = resolve_constraint_check (expr);
  else
    gcc_unreachable ();

  if (info && info != error_mark_node)
    return TREE_PURPOSE (info);
  return NULL_TREE;
}

namespace {

/*---------------------------------------------------------------------------
                       Lifting of concept definitions
---------------------------------------------------------------------------*/

/* Part of constraint normalization.  Whenever we find a reference to
   a variable concept or a call to a function concept, we lift or
   inline that concept's definition into the constraint.  This ensures
   that constraints are always checked in the immediate instantiation
   context. */

tree lift_expression (tree);

/* If the tree T has operands, then lift any concepts out of them.  */
tree
lift_operands (tree t)
{
  if (int n = tree_operand_length (t))
    {
      t = copy_node (t);
      for (int i = 0; i < n; ++i)
        TREE_OPERAND (t, i) = lift_expression (TREE_OPERAND (t, i));
    }
  return t;
}

/* Recursively lift all operands of the function call. Also, check
   that the call target is not accidentally a variable concept
   since that's ill-formed.  */
tree
lift_function_call (tree t)
{
  gcc_assert (TREE_CODE (t) == CALL_EXPR);
  gcc_assert (!VAR_P (CALL_EXPR_FN (t)));
  return lift_operands (t);
}

/* Inline a function (concept) definition by substituting
   ARGS into its body. */
tree
lift_function_definition (tree fn, tree args)
{
  /* Extract the body of the function minus the return expression.  */
  tree body = DECL_SAVED_TREE (fn);
  if (!body)
    return error_mark_node;
  if (TREE_CODE (body) == BIND_EXPR)
    body = BIND_EXPR_BODY (body);
  if (TREE_CODE (body) != RETURN_EXPR)
    return error_mark_node;

  body = TREE_OPERAND (body, 0);

  /* Substitute template arguments to produce our inline expression.  */
  tree result = tsubst_expr (body, args, tf_none, NULL_TREE, false);
  if (result == error_mark_node)
    return error_mark_node;

  return lift_expression (result);
}

/* Inline a reference to a function concept.  */
tree
lift_call_expression (tree t)
{
  /* Try to resolve this function call as a concept.  If not, then
     it can be returned as-is.  */
  tree check = resolve_constraint_check (t);
  if (!check)
    return lift_function_call (t);
  if (check == error_mark_node)
    return error_mark_node;

  tree fn = TREE_VALUE (check);
  tree args = TREE_PURPOSE (check);
  return lift_function_definition (fn, args);
}

tree
lift_variable_initializer (tree var, tree args)
{
  /* Extract the body from the variable initializer.  */
  tree init = DECL_INITIAL (var);
  if (!init)
    return error_mark_node;

  /* Substitute the arguments to form our new inline expression.  */
  tree result = tsubst_expr (init, args, tf_none, NULL_TREE, false);
  if (result == error_mark_node)
    return error_mark_node;

  return lift_expression (result);
}

/* Determine if a template-id is a variable concept and inline.  */

tree
lift_template_id (tree t)
{
  if (tree info = resolve_variable_concept_check (t))
    {
      tree decl = TREE_VALUE (info);
      tree args = TREE_PURPOSE (info);
      return lift_variable_initializer (decl, args);
    }

  /* Check that we didn't refer to a function concept like
      a variable.

     TODO: Add a note on how to fix this.  */
  tree tmpl = TREE_OPERAND (t, 0);
  if (TREE_CODE (tmpl) == OVERLOAD)
    {
      tree fn = OVL_FUNCTION (tmpl);
      if (TREE_CODE (fn) == TEMPLATE_DECL
          && DECL_DECLARED_CONCEPT_P (DECL_TEMPLATE_RESULT (fn)))
        {
          error_at (location_of (t),
		    "invalid reference to function concept %qD", fn);
          return error_mark_node;
        }
    }

  return t;
}

/* Lift any constraints appearing in a nested requirement of
   a requires-expression. */
tree
lift_requires_expression (tree t)
{
  tree parms = TREE_OPERAND (t, 0);
  tree reqs = TREE_OPERAND (t, 1);
  tree result = NULL_TREE;
  for (; reqs != NULL_TREE; reqs = TREE_CHAIN (reqs))
    {
      tree req = TREE_VALUE (reqs);
      if (TREE_CODE (req) == NESTED_REQ)
        {
          tree expr = lift_expression (TREE_OPERAND (req, 0));
          req = finish_nested_requirement (expr);
        }
      result = tree_cons (NULL_TREE, req, result);
    }
  return finish_requires_expr (parms, result);
}

/* Inline references to specializations of concepts.  */
tree
lift_expression (tree t)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  if (t == error_mark_node)
    return error_mark_node;

  /* Concepts can be referred to by call or variable. All other
     nodes are preserved.  */
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return lift_call_expression (t);

    case TEMPLATE_ID_EXPR:
      return lift_template_id (t);

    case REQUIRES_EXPR:
      return lift_requires_expression (t);

    case EXPR_PACK_EXPANSION:
      /* Use copy_node rather than make_pack_expansion so that
	 PACK_EXPANSION_PARAMETER_PACKS stays the same.  */
      t = copy_node (t);
      SET_PACK_EXPANSION_PATTERN
	(t, lift_expression (PACK_EXPANSION_PATTERN (t)));
      return t;

    case TREE_LIST:
      {
        t = copy_node (t);
        TREE_VALUE (t) = lift_expression (TREE_VALUE (t));
        TREE_CHAIN (t) = lift_expression (TREE_CHAIN (t));
        return t;
      }

    default:
      return lift_operands (t);
    }
}

/*---------------------------------------------------------------------------
                Transformation of expressions into constraints
---------------------------------------------------------------------------*/

/* Part of constraint normalization. The following functions rewrite
   expressions as constraints.  */

tree transform_expression (tree);

/* Check that the logical-or or logical-and expression does
   not result in a call to a user-defined user-defined operator
   (temp.constr.op). Returns true if the logical operator is
   admissible and false otherwise. */

bool
check_logical_expr (tree t)
{
  /* We can't do much for type dependent expressions. */
  if (type_dependent_expression_p (t))
    return true;

  /* Resolve the logical operator. Note that template processing is
     disabled so we get the actual call or target expression back.
     not_processing_template_sentinel sentinel.

     TODO: This check is actually subsumed by the requirement that
     constraint operands have type bool. I'm not sure we need it
     unless we allow conversions.  */
  tree arg1 = TREE_OPERAND (t, 0);
  tree arg2 = TREE_OPERAND (t, 1);
  tree ovl = NULL_TREE;
  tree expr = build_x_binary_op (EXPR_LOC_OR_LOC (arg2, input_location),
                                 TREE_CODE (t),
                                 arg1, TREE_CODE (arg1),
                                 arg2, TREE_CODE (arg2),
                                 &ovl,
                                 tf_none);
  if (TREE_CODE (expr) != TREE_CODE (t))
    {
      error ("user-defined operator %qs in constraint %q+E",
	     operator_name_info[TREE_CODE (t)].name, t);
      return false;
    }
  return true;
}

/* Transform a logical-or or logical-and expression into either
   a conjunction or disjunction. */

tree
xform_logical (tree t, tree_code c)
{
  if (!check_logical_expr (t))
    return error_mark_node;
  tree t0 = transform_expression (TREE_OPERAND (t, 0));
  tree t1 = transform_expression (TREE_OPERAND (t, 1));
  return build_nt (c, t0, t1);
}

/* A simple requirement T introduces an expression constraint
   for its expression. */

inline tree
xform_simple_requirement (tree t)
{
  return build_nt (EXPR_CONSTR, TREE_OPERAND (t, 0));
}

/* A type requirement T introduce a type constraint for its type.  */

inline tree
xform_type_requirement (tree t)
{
  return build_nt (TYPE_CONSTR, TREE_OPERAND (t, 0));
}

/* A compound requirement T introduces a conjunction of constraints
   depending on its form.  The conjunction always includes an
   expression constraint for the expression of the requirement.
   If a trailing return type was specified, the conjunction includes
   either an implicit conversion constraint or an argument deduction
   constraint.  If the noexcept specifier is present, the conjunction
   includes an exception constraint.  */

tree
xform_compound_requirement (tree t)
{
  tree expr = TREE_OPERAND (t, 0);
  tree constr = build_nt (EXPR_CONSTR, TREE_OPERAND (t, 0));

  /* If a type is given, append an implicit conversion or
     argument deduction constraint.  */
  if (tree type = TREE_OPERAND (t, 1))
    {
      tree type_constr;
      /* TODO: We should be extracting a list of auto nodes
         from type_uses_auto, not a single node */
      if (tree placeholder = type_uses_auto (type))
        type_constr = build_nt (DEDUCT_CONSTR, expr, type, placeholder);
      else
        type_constr = build_nt (ICONV_CONSTR, expr, type);
      constr = conjoin_constraints (constr, type_constr);
    }

  /* If noexcept is present, append an exception constraint. */
  if (COMPOUND_REQ_NOEXCEPT_P (t))
    {
      tree except = build_nt (EXCEPT_CONSTR, expr);
      constr = conjoin_constraints (constr, except);
    }

  return constr;
}

/* A nested requirement T introduces a conjunction of constraints
   corresponding to its constraint-expression.

   If the result of transforming T is error_mark_node, the resulting
   constraint is a predicate constraint whose operand is also
   error_mark_node. This preserves the constraint structure, but
   will guarantee that the constraint is never satisfied.  */

inline tree
xform_nested_requirement (tree t)
{
  return transform_expression (TREE_OPERAND (t, 0));
}

/* Transform a requirement T into one or more constraints.  */

tree
xform_requirement (tree t)
{
  switch (TREE_CODE (t))
    {
    case SIMPLE_REQ:
      return xform_simple_requirement (t);

    case TYPE_REQ:
      return xform_type_requirement (t);

    case COMPOUND_REQ:
      return xform_compound_requirement (t);

    case NESTED_REQ:
      return xform_nested_requirement (t);

    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Transform a sequence of requirements into a conjunction of
   constraints. */

tree
xform_requirements (tree t)
{
  tree result = NULL_TREE;
  for (; t; t = TREE_CHAIN (t))
    {
      tree constr = xform_requirement (TREE_VALUE (t));
      result = conjoin_constraints (result, constr);
    }
  return result;
}

/* Transform a requires-expression into a parameterized constraint.  */

tree
xform_requires_expr (tree t)
{
  tree operand = xform_requirements (TREE_OPERAND (t, 1));
  if (tree parms = TREE_OPERAND (t, 0))
    return build_nt (PARM_CONSTR, parms, operand);
  else
    return operand;
}

/* Transform an expression into an atomic predicate constraint.
   After substitution, the expression of a predicate constraint
   shall have type bool (temp.constr.pred).  For non-type-dependent
   expressions, we can check that now.  */

tree
xform_atomic (tree t)
{
  if (TREE_TYPE (t) && !type_dependent_expression_p (t))
  {
    tree type = cv_unqualified (TREE_TYPE (t));
    if (!same_type_p (type, boolean_type_node))
      {
        error ("predicate constraint %q+E does not have type %<bool%>", t);
        return error_mark_node;
      }
  }
  return build_nt (PRED_CONSTR, t);
}

/* Push down the pack expansion EXP into the leaves of the constraint PAT.  */

tree
push_down_pack_expansion (tree exp, tree pat)
{
  switch (TREE_CODE (pat))
    {
    case CONJ_CONSTR:
    case DISJ_CONSTR:
      {
	pat = copy_node (pat);
	TREE_OPERAND (pat, 0)
	  = push_down_pack_expansion (exp, TREE_OPERAND (pat, 0));
	TREE_OPERAND (pat, 1)
	  = push_down_pack_expansion (exp, TREE_OPERAND (pat, 1));
	return pat;
      }
    default:
      {
	exp = copy_node (exp);
	SET_PACK_EXPANSION_PATTERN (exp, pat);
	return exp;
      }
    }
}

/* Transform a pack expansion into a constraint.  First we transform the
   pattern of the pack expansion, then we push the pack expansion down into the
   leaves of the constraint so that partial ordering will work.  */

tree
xform_pack_expansion (tree t)
{
  tree pat = transform_expression (PACK_EXPANSION_PATTERN (t));
  return push_down_pack_expansion (t, pat);
}

/* Transform an expression into a constraint.  */

tree
xform_expr (tree t)
{
  switch (TREE_CODE (t))
    {
    case TRUTH_ANDIF_EXPR:
      return xform_logical (t, CONJ_CONSTR);

    case TRUTH_ORIF_EXPR:
      return xform_logical (t, DISJ_CONSTR);

    case REQUIRES_EXPR:
      return xform_requires_expr (t);

    case BIND_EXPR:
      return transform_expression (BIND_EXPR_BODY (t));

    case EXPR_PACK_EXPANSION:
      return xform_pack_expansion (t);

    default:
      /* All other constraints are atomic. */
      return xform_atomic (t);
    }
}

/* Transform a statement into an expression.  */

tree
xform_stmt (tree t)
{
  switch (TREE_CODE (t))
    {
    case RETURN_EXPR:
      return transform_expression (TREE_OPERAND (t, 0));
    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Reduction rules for the declaration T.  */

tree
xform_decl (tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      return xform_atomic (t);
    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Transform a lifted expression into a constraint. This either
   returns a constraint, or it returns error_mark_node when
   a constraint cannot be formed.  */

tree
transform_expression (tree t)
{
  if (!t)
    return NULL_TREE;

  if (t == error_mark_node)
    return error_mark_node;

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_unary:
    case tcc_binary:
    case tcc_expression:
    case tcc_vl_exp:
      return xform_expr (t);

    case tcc_statement:
      return xform_stmt (t);

    case tcc_declaration:
      return xform_decl (t);

    case tcc_exceptional:
    case tcc_constant:
    case tcc_reference:
    case tcc_comparison:
      /* These are all atomic predicate constraints. */
      return xform_atomic (t);

    default:
      /* Unhandled node kind. */
      gcc_unreachable ();
    }
  return error_mark_node;
}

/*---------------------------------------------------------------------------
                        Constraint normalization
---------------------------------------------------------------------------*/

tree normalize_constraint (tree);

/* The normal form of the disjunction T0 /\ T1 is the conjunction
   of the normal form of T0 and the normal form of T1.  */

inline tree
normalize_conjunction (tree t)
{
  tree t0 = normalize_constraint (TREE_OPERAND (t, 0));
  tree t1 = normalize_constraint (TREE_OPERAND (t, 1));
  return build_nt (CONJ_CONSTR, t0, t1);
}

/* The normal form of the disjunction T0 \/ T1 is the disjunction
   of the normal form of T0 and the normal form of T1.  */

inline tree
normalize_disjunction (tree t)
{
  tree t0 = normalize_constraint (TREE_OPERAND (t, 0));
  tree t1 = normalize_constraint (TREE_OPERAND (t, 1));
  return build_nt (DISJ_CONSTR, t0, t1);
}

/* A predicate constraint is normalized in two stages.  First all
   references specializations of concepts are replaced by their
   substituted definitions.  Then, the resulting expression is
   transformed into a constraint by transforming && expressions
   into conjunctions and || into disjunctions.  */

tree
normalize_predicate_constraint (tree t)
{
  ++processing_template_decl;
  tree expr = PRED_CONSTR_EXPR (t);
  tree lifted = lift_expression (expr);
  tree constr = transform_expression (lifted);
  --processing_template_decl;
  return constr;
}

/* The normal form of a parameterized constraint is the normal
   form of its operand.  */

tree
normalize_parameterized_constraint (tree t)
{
  tree parms = PARM_CONSTR_PARMS (t);
  tree operand = normalize_constraint (PARM_CONSTR_OPERAND (t));
  return build_nt (PARM_CONSTR, parms, operand);
}

/* Normalize the constraint T by reducing it so that it is
   comprised of only conjunctions and disjunctions of atomic
   constraints.  */

tree
normalize_constraint (tree t)
{
  if (!t)
    return NULL_TREE;

  if (t == error_mark_node)
    return t;

  switch (TREE_CODE (t))
    {
      case CONJ_CONSTR:
        return normalize_conjunction (t);

      case DISJ_CONSTR:
        return normalize_disjunction (t);

      case PRED_CONSTR:
        return normalize_predicate_constraint (t);

      case PARM_CONSTR:
        return normalize_parameterized_constraint (t);

      case EXPR_CONSTR:
      case TYPE_CONSTR:
      case ICONV_CONSTR:
      case DEDUCT_CONSTR:
      case EXCEPT_CONSTR:
        /* These constraints are defined to be atomic. */
        return t;

      default:
        /* CONSTR was not a constraint. */
        gcc_unreachable();
    }
  return error_mark_node;
}

} /* namespace */


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
  tree tmpl_constr = TEMPLATE_PARM_CONSTRAINTS (current_template_parms);
  return build_constraints (tmpl_constr, NULL_TREE);
}

// If the recently parsed TYPE declares or defines a template or template
// specialization, get its corresponding constraints from the current
// template parameters and bind them to TYPE's declaration.
tree
associate_classtype_constraints (tree type)
{
  if (!type || type == error_mark_node || TREE_CODE (type) != RECORD_TYPE)
    return type;

  // An explicit class template specialization has no template
  // parameters.
  if (!current_template_parms)
    return type;

  if (CLASSTYPE_IS_TEMPLATE (type) || CLASSTYPE_TEMPLATE_SPECIALIZATION (type))
    {
      tree decl = TYPE_STUB_DECL (type);
      tree ci = current_template_constraints ();

      // An implicitly instantiated member template declaration already
      // has associated constraints. If it is defined outside of its
      // class, then we need match these constraints against those of
      // original declaration.
      if (tree orig_ci = get_constraints (decl))
        {
          if (!equivalent_constraints (ci, orig_ci))
            {
              // FIXME: Improve diagnostics.
              error ("%qT does not match any declaration", type);
              return error_mark_node;
            }
          return type;
        }
      set_constraints (decl, ci);
    }
  return type;
}

namespace {

// Create an empty constraint info block.
inline tree_constraint_info*
build_constraint_info ()
{
  return (tree_constraint_info *)make_node (CONSTRAINT_INFO);
}

} // namespace

/* Build a constraint-info object that contains the associated constraints
   of a declaration.  This also includes the declaration's template
   requirements (TREQS) and any trailing requirements for a function
   declarator (DREQS).  Note that both TREQS and DREQS must be constraints.

   If the declaration has neither template nor declaration requirements
   this returns NULL_TREE, indicating an unconstrained declaration.  */

tree
build_constraints (tree tmpl_reqs, tree decl_reqs)
{
  gcc_assert (tmpl_reqs ? constraint_p (tmpl_reqs) : true);
  gcc_assert (decl_reqs ? constraint_p (decl_reqs) : true);

  if (!tmpl_reqs && !decl_reqs)
    return NULL_TREE;

  tree_constraint_info* ci = build_constraint_info ();
  ci->template_reqs = tmpl_reqs;
  ci->declarator_reqs = decl_reqs;
  ci->associated_constr = conjoin_constraints (tmpl_reqs, decl_reqs);

  ++processing_template_decl;
  ci->normalized_constr = normalize_constraint (ci->associated_constr);
  --processing_template_decl;

  ci->assumptions = decompose_assumptions (ci->normalized_constr);
  return (tree)ci;
}

namespace {

/* Returns true if any of the arguments in the template
   argument list is a wildcard or wildcard pack. */
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

/* Build a new call expression, but don't actually generate
   a new function call. We just want the tree, not the
   semantics. */
inline tree
build_call_check (tree id)
{
  ++processing_template_decl;
  vec<tree, va_gc> *fargs = make_tree_vector();
  tree call = finish_call_expr (id, &fargs, false, false, tf_none);
  release_tree_vector (fargs);
  --processing_template_decl;
  return call;
}

/* Build an expression that will check a variable concept. If any
   argument contains a wildcard, don't try to finish the variable
   template because we can't substitute into a non-existent
   declaration.  */
tree
build_variable_check (tree id)
{
  gcc_assert (TREE_CODE (id) == TEMPLATE_ID_EXPR);
  if (contains_wildcard_p (TREE_OPERAND (id, 1)))
    return id;

  ++processing_template_decl;
  tree var = finish_template_variable (id);
  --processing_template_decl;
  return var;
}

/* Construct a sequence of template arguments by prepending
   ARG to REST. Either ARG or REST may be null. */
tree
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

} // namespace

/* Construct an expression that checks the concept given by
   TARGET. The TARGET must be:

   - an OVERLOAD referring to one or more function concepts
   - a BASELINK referring to an overload set of the above, or
   - a TEMPLTATE_DECL referring to a variable concept.

   ARG and REST are the explicit template arguments for the
   eventual concept check. */
tree
build_concept_check (tree target, tree arg, tree rest)
{
  tree args = build_concept_check_arguments (arg, rest);
  if (variable_template_p (target))
    return build_variable_check (lookup_template_variable (target, args));
  else
    return build_call_check (lookup_template_function (target, args));
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

/* Create a constraint expression for the given DECL that
   evaluates the requirements specified by CONSTR, a TYPE_DECL
   that contains all the information necessary to build the
   requirements (see finish_concept_name for the layout of
   that TYPE_DECL).

   Note that the constraints are neither reduced nor decomposed.
   That is done only after the requires clause has been parsed
   (or not). */
tree
finish_shorthand_constraint (tree decl, tree constr)
{
  /* No requirements means no constraints.  */
  if (!constr)
    return NULL_TREE;

  tree proto = CONSTRAINED_PARM_PROTOTYPE (constr);
  tree con = CONSTRAINED_PARM_CONCEPT (constr);
  tree args = CONSTRAINED_PARM_EXTRA_ARGS (constr);

  /* If the parameter declaration is variadic, but the concept
     is not then we need to apply the concept to every element
     in the pack.  */
  bool is_proto_pack = template_parameter_pack_p (proto);
  bool is_decl_pack = template_parameter_pack_p (decl);
  bool apply_to_all_p = is_decl_pack && !is_proto_pack;

  /* Get the argument and overload used for the requirement
     and adjust it if we're going to expand later.  */
  tree arg = template_parm_to_arg (build_tree_list (NULL_TREE, decl));
  if (apply_to_all_p)
    arg = PACK_EXPANSION_PATTERN (TREE_VEC_ELT (ARGUMENT_PACK_ARGS (arg), 0));

  /* Build the concept check. If it the constraint needs to be
     applied to all elements of the parameter pack, then make
     the constraint an expansion. */
  tree check;
  tree tmpl = DECL_TI_TEMPLATE (con);
  if (TREE_CODE (con) == VAR_DECL)
    {
      check = build_concept_check (tmpl, arg, args);
    }
  else
    {
      tree ovl = build_overload (tmpl, NULL_TREE);
      check = build_concept_check (ovl, arg, args);
    }

  /* Make the check a pack expansion if needed.

     FIXME: We should be making a fold expression. */
  if (apply_to_all_p)
    {
      check = make_pack_expansion (check);
      TREE_TYPE (check) = boolean_type_node;
    }

  return make_predicate_constraint (check);
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
      result = conjoin_constraints (result, constr);
    }
  return result;
}

// Returns and chains a new parameter for PARAMETER_LIST which will conform
// to the prototype given by SRC_PARM.  The new parameter will have its
// identifier and location set according to IDENT and PARM_LOC respectively.
static tree
process_introduction_parm (tree parameter_list, tree src_parm)
{
  // If we have a pack, we should have a single pack argument which is the
  // placeholder we want to look at.
  bool is_parameter_pack = ARGUMENT_PACK_P (src_parm);
  if (is_parameter_pack)
    src_parm = TREE_VEC_ELT (ARGUMENT_PACK_ARGS (src_parm), 0);

  // At this point we should have a wildcard, but we want to
  // grab the associated decl from it.  Also grab the stored
  // identifier and location that should be chained to it in
  // a PARM_DECL.
  gcc_assert (TREE_CODE (src_parm) == WILDCARD_DECL);

  tree ident = DECL_NAME (src_parm);
  location_t parm_loc = DECL_SOURCE_LOCATION (src_parm);

  // If we expect a pack and the deduced template is not a pack, or if the
  // template is using a pack and we didn't declare a pack, throw an error.
  if (is_parameter_pack != WILDCARD_PACK_P (src_parm))
    {
      error_at (parm_loc, "cannot match pack for introduced parameter");
      tree err_parm = build_tree_list (error_mark_node, error_mark_node);
      return chainon (parameter_list, err_parm);
    }

  src_parm = TREE_TYPE (src_parm);

  tree parm;
  bool is_non_type;
  if (TREE_CODE (src_parm) == TYPE_DECL)
    {
      is_non_type = false;
      parm = finish_template_type_parm (class_type_node, ident);
    }
  else if (TREE_CODE (src_parm) == TEMPLATE_DECL)
    {
      is_non_type = false;
      begin_template_parm_list ();
      current_template_parms = DECL_TEMPLATE_PARMS (src_parm);
      end_template_parm_list ();
      parm = finish_template_template_parm (class_type_node, ident);
    }
  else
    {
      is_non_type = true;

      // Since we don't have a declarator, so we can copy the source
      // parameter and change the name and eventually the location.
      parm = copy_decl (src_parm);
      DECL_NAME (parm) = ident;
    }

  // Wrap in a TREE_LIST for process_template_parm.  Introductions do not
  // retain the defaults from the source template.
  parm = build_tree_list (NULL_TREE, parm);

  return process_template_parm (parameter_list, parm_loc, parm,
                                is_non_type, is_parameter_pack);
}

/* Associates a constraint check to the current template based
   on the introduction parameters.  INTRO_LIST must be a TREE_VEC
   of WILDCARD_DECLs containing a chained PARM_DECL which
   contains the identifier as well as the source location.
   TMPL_DECL is the decl for the concept being used.  If we
   take a concept, C, this will form a check in the form of
   C<INTRO_LIST> filling in any extra arguments needed by the
   defaults deduced.

   Returns NULL_TREE if no concept could be matched and
   error_mark_node if an error occurred when matching.  */
tree
finish_template_introduction (tree tmpl_decl, tree intro_list)
{
  /* Deduce the concept check.  */
  tree expr = build_concept_check (tmpl_decl, NULL_TREE, intro_list);
  if (expr == error_mark_node)
    return NULL_TREE;

  tree parms = deduce_concept_introduction (expr);
  if (!parms)
    return NULL_TREE;

  /* Build template parameter scope for introduction.  */
  tree parm_list = NULL_TREE;
  begin_template_parm_list ();
  int nargs = MIN (TREE_VEC_LENGTH (parms), TREE_VEC_LENGTH (intro_list));
  for (int n = 0; n < nargs; ++n)
    parm_list = process_introduction_parm (parm_list, TREE_VEC_ELT (parms, n));
  parm_list = end_template_parm_list (parm_list);
  for (int i = 0; i < TREE_VEC_LENGTH (parm_list); ++i)
    if (TREE_VALUE (TREE_VEC_ELT (parm_list, i)) == error_mark_node)
      {
        end_template_decl ();
        return error_mark_node;
      }

  /* Build a concept check for our constraint.  */
  tree check_args = make_tree_vec (TREE_VEC_LENGTH (parms));
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

  /* Associate the constraint. */
  tree check = build_concept_check (tmpl_decl, NULL_TREE, check_args);
  tree constr = make_predicate_constraint (check);
  TEMPLATE_PARMS_CONSTRAINTS (current_template_parms) = constr;

  return parm_list;
}


/* Given the predicate constraint T from a constrained-type-specifier, extract
   its TMPL and ARGS.  FIXME why do we need two different forms of
   constrained-type-specifier?  */

void
placeholder_extract_concept_and_args (tree t, tree &tmpl, tree &args)
{
  if (TREE_CODE (t) == TYPE_DECL)
    {
      /* A constrained parameter.  */
      tmpl = DECL_TI_TEMPLATE (CONSTRAINED_PARM_CONCEPT (t));
      args = CONSTRAINED_PARM_EXTRA_ARGS (t);
      return;
    }

  gcc_assert (TREE_CODE (t) == PRED_CONSTR);
  t = PRED_CONSTR_EXPR (t);
  gcc_assert (TREE_CODE (t) == CALL_EXPR
              || TREE_CODE (t) == TEMPLATE_ID_EXPR
              || VAR_P (t));

  if (TREE_CODE (t) == CALL_EXPR)
    t = CALL_EXPR_FN (t);
  if (TREE_CODE (t) == TEMPLATE_ID_EXPR)
    {
      tmpl = TREE_OPERAND (t, 0);
      if (TREE_CODE (tmpl) == OVERLOAD)
	{
	  gcc_assert (OVL_CHAIN (tmpl) == NULL_TREE);
	  tmpl = OVL_FUNCTION (tmpl);
	}
      args = TREE_OPERAND (t, 1);
    }
  else if (DECL_P (t))
    {
      tmpl = DECL_TI_TEMPLATE (t);
      args = DECL_TI_ARGS (t);
    }
  else
    gcc_unreachable ();
}

/* Returns true iff the placeholders C1 and C2 are equivalent.  C1
   and C2 can be either PRED_CONSTR_EXPR or TEMPLATE_TYPE_PARM.  */

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

  tree t1, t2, a1, a2;
  placeholder_extract_concept_and_args (c1, t1, a1);
  placeholder_extract_concept_and_args (c2, t2, a2);

  if (t1 != t2)
    return false;

  /* Skip the first argument to avoid infinite recursion on the
     placeholder auto itself.  */
  bool skip1 = (TREE_CODE (c1) == PRED_CONSTR);
  bool skip2 = (TREE_CODE (c2) == PRED_CONSTR);

  int len1 = (a1 ? TREE_VEC_LENGTH (a1) : 0) - skip1;
  int len2 = (a2 ? TREE_VEC_LENGTH (a2) : 0) - skip2;

  if (len1 != len2)
    return false;

  for (int i = 0; i < len1; ++i)
    if (!cp_tree_equal (TREE_VEC_ELT (a1, i + skip1),
			TREE_VEC_ELT (a2, i + skip2)))
      return false;
  return true;
}

/* Return a hash value for the placeholder PRED_CONSTR C.  */

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

/*---------------------------------------------------------------------------
                        Constraint substitution
---------------------------------------------------------------------------*/

/* The following functions implement substitution rules for constraints.
   Substitution without checking constraints happens only in the
   instantiation of class templates. For example:

      template<C1 T> struct S {
        void f(T) requires C2<T>;
        void g(T) requires T::value;
      };

      S<int> s; // error instantiating S<int>::g(T)

   When we instantiate S, we substitute into its member declarations,
   including their constraints. However, those constraints are not
   checked. Substituting int into C2<T> yields C2<int>, and substituting
   into T::value yields a substitution failure, making the program
   ill-formed.

   Note that we only ever substitute into the associated constraints
   of a declaration. That is, substitution is defined only for predicate
   constraints and conjunctions. */

/* Substitute into the predicate constraints. Returns error_mark_node
   if the substitution into the expression fails. */
tree
tsubst_predicate_constraint (tree t, tree args,
                             tsubst_flags_t complain, tree in_decl)
{
  tree expr = PRED_CONSTR_EXPR (t);
  ++processing_template_decl;
  tree result = tsubst_expr (expr, args, complain, in_decl, false);
  --processing_template_decl;
  return build_nt (PRED_CONSTR, result);
}

/* Substitute into the conjunction of constraints. Returns
   error_mark_node if substitution into either operand fails. */
tree
tsubst_conjunction (tree t, tree args,
                    tsubst_flags_t complain, tree in_decl)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree r0 = tsubst_constraint (t0, args, complain, in_decl);
  tree t1 = TREE_OPERAND (t, 1);
  tree r1 = tsubst_constraint (t1, args, complain, in_decl);
  return build_nt (CONJ_CONSTR, r0, r1);
}

/* Substitute ARGS into the constraint T. */
tree
tsubst_constraint (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  if (t == NULL_TREE)
    return t;
  if (TREE_CODE (t) == CONJ_CONSTR)
    return tsubst_conjunction (t, args, complain, in_decl);
  else if (TREE_CODE (t) == PRED_CONSTR)
    return tsubst_predicate_constraint (t, args, complain, in_decl);
  else
    gcc_unreachable ();
  return error_mark_node;
}

namespace {

/* A subroutine of tsubst_constraint_variables. Register local
   specializations for each of parameter in PARMS and its
   corresponding substituted constraint variable in VARS.
   Returns VARS. */
tree
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

/* A subroutine of tsubst_parameterized_constraint. Substitute ARGS
   into the parameter list T, producing a sequence of constraint
   variables, declared in the current scope.

   Note that the caller must establish a local specialization stack
   prior to calling this function since this substitution will
   declare the substituted parameters. */
tree
tsubst_constraint_variables (tree t, tree args,
                             tsubst_flags_t complain, tree in_decl)
{
  /* Clear cp_unevaluated_operand across tsubst so that we get a proper chain
     of PARM_DECLs.  */
  int saved_unevaluated_operand = cp_unevaluated_operand;
  cp_unevaluated_operand = 0;
  tree vars = tsubst (t, args, complain, in_decl);
  cp_unevaluated_operand = saved_unevaluated_operand;
  if (vars == error_mark_node)
    return error_mark_node;
  return declare_constraint_vars (t, vars);
}

/* Substitute ARGS into the simple requirement T. Note that
   substitution may result in an ill-formed expression without
   causing the program to be ill-formed. In such cases, the
   requirement wraps an error_mark_node. */
inline tree
tsubst_simple_requirement (tree t, tree args,
                           tsubst_flags_t complain, tree in_decl)
{
  ++processing_template_decl;
  tree expr = tsubst_expr (TREE_OPERAND (t, 0), args, complain, in_decl, false);
  --processing_template_decl;
  return finish_simple_requirement (expr);
}

/* Substitute ARGS into the type requirement T. Note that
   substitution may result in an ill-formed type without
   causing the program to be ill-formed. In such cases, the
   requirement wraps an error_mark_node. */

inline tree
tsubst_type_requirement (tree t, tree args,
                         tsubst_flags_t complain, tree in_decl)
{
  ++processing_template_decl;
  tree type = tsubst (TREE_OPERAND (t, 0), args, complain, in_decl);
  --processing_template_decl;
  return finish_type_requirement (type);
}

/* Substitute args into the compound requirement T. If substituting
   into either the expression or the type fails, the corresponding
   operands in the resulting node will be error_mark_node. This
   preserves a requirement for the purpose of partial ordering, but
   it will never be satisfied. */

tree
tsubst_compound_requirement (tree t, tree args,
                             tsubst_flags_t complain, tree in_decl)
{
  ++processing_template_decl;
  tree expr = tsubst_expr (TREE_OPERAND (t, 0), args, complain, in_decl, false);
  tree type = tsubst (TREE_OPERAND (t, 1), args, complain, in_decl);
  --processing_template_decl;
  bool noexcept_p = COMPOUND_REQ_NOEXCEPT_P (t);
  return finish_compound_requirement (expr, type, noexcept_p);
}

/* Substitute ARGS into the nested requirement T. */

tree
tsubst_nested_requirement (tree t, tree args,
                           tsubst_flags_t complain, tree in_decl)
{
  ++processing_template_decl;
  tree expr = tsubst_expr (TREE_OPERAND (t, 0), args, complain, in_decl, false);
  --processing_template_decl;
  return finish_nested_requirement (expr);
}

inline tree
tsubst_requirement (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  switch (TREE_CODE (t))
    {
    case SIMPLE_REQ:
      return tsubst_simple_requirement (t, args, complain, in_decl);
    case TYPE_REQ:
      return tsubst_type_requirement (t, args, complain, in_decl);
    case COMPOUND_REQ:
      return tsubst_compound_requirement (t, args, complain, in_decl);
    case NESTED_REQ:
      return tsubst_nested_requirement (t, args, complain, in_decl);
    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Substitute ARGS into the list of requirements T. Note that
   substitution failures here result in ill-formed programs. */

tree
tsubst_requirement_body (tree t, tree args,
                         tsubst_flags_t complain, tree in_decl)
{
  tree r = NULL_TREE;
  while (t)
    {
      tree e = tsubst_requirement (TREE_VALUE (t), args, complain, in_decl);
      if (e == error_mark_node)
        return error_mark_node;
      r = tree_cons (NULL_TREE, e, r);
      t = TREE_CHAIN (t);
    }
  return r;
}

} /* namespace */

/* Substitute ARGS into the requires expression T. Note that this
   results in the re-declaration of local parameters when
   substituting through the parameter list. If either substitution
   fails, the program is ill-formed. */

tree
tsubst_requires_expr (tree t, tree args,
                      tsubst_flags_t complain, tree in_decl)
{
  local_specialization_stack stack;

  tree parms = TREE_OPERAND (t, 0);
  if (parms)
    {
      parms = tsubst_constraint_variables (parms, args, complain, in_decl);
      if (parms == error_mark_node)
        return error_mark_node;
    }

  tree reqs = TREE_OPERAND (t, 1);
  reqs = tsubst_requirement_body (reqs, args, complain, in_decl);
  if (reqs == error_mark_node)
    return error_mark_node;

  return finish_requires_expr (parms, reqs);
}

/* Substitute ARGS into the constraint information CI, producing a new
   constraint record. */
tree
tsubst_constraint_info (tree t, tree args,
                        tsubst_flags_t complain, tree in_decl)
{
  if (!t || t == error_mark_node || !check_constraint_info (t))
    return NULL_TREE;

  tree tmpl_constr = NULL_TREE;
  if (tree r = CI_TEMPLATE_REQS (t))
    tmpl_constr = tsubst_constraint (r, args, complain, in_decl);

  tree decl_constr = NULL_TREE;
  if (tree r = CI_DECLARATOR_REQS (t))
    decl_constr = tsubst_constraint (r, args, complain, in_decl);

  return build_constraints (tmpl_constr, decl_constr);
}


/*---------------------------------------------------------------------------
                        Constraint satisfaction
---------------------------------------------------------------------------*/

/* The following functions determine if a constraint, when
   substituting template arguments, is satisfied. For convenience,
   satisfaction reduces a constraint to either true or false (and
   nothing else). */

namespace {

tree satisfy_constraint_1 (tree, tree, tsubst_flags_t, tree);

/* Check the constraint pack expansion.  */

tree
satisfy_pack_expansion (tree t, tree args,
                      tsubst_flags_t complain, tree in_decl)
{
  /* Get the vector of satisfaction results.
     gen_elem_of_pack_expansion_instantiation will check that each element of
     the expansion is satisfied.  */
  tree exprs = tsubst_pack_expansion (t, args, complain, in_decl);
  if (exprs == error_mark_node)
    return boolean_false_node;
  int n = TREE_VEC_LENGTH (exprs);

  for (int i = 0; i < n; ++i)
    if (TREE_VEC_ELT (exprs, i) != boolean_true_node)
      return boolean_false_node;
  return boolean_true_node;
}

/* A predicate constraint is satisfied if its expression evaluates
   to true. If substitution into that node fails, the constraint
   is not satisfied ([temp.constr.pred]).

   Note that a predicate constraint is a constraint expression
   of type bool. If neither of those are true, the program is
   ill-formed; they are not SFINAE'able errors. */

tree
satisfy_predicate_constraint (tree t, tree args,
                              tsubst_flags_t complain, tree in_decl)
{
  tree original = TREE_OPERAND (t, 0);

  /* We should never have a naked pack expansion in a predicate constraint.  */
  gcc_assert (TREE_CODE (original) != EXPR_PACK_EXPANSION);

  tree expr = tsubst_expr (original, args, complain, in_decl, false);
  if (expr == error_mark_node)
    return boolean_false_node;

  /* A predicate constraint shall have type bool. In some
     cases, substitution gives us const-qualified bool, which
     is also acceptable.  */
  tree type = cv_unqualified (TREE_TYPE (expr));
  if (!same_type_p (type, boolean_type_node))
    {
      error_at (EXPR_LOC_OR_LOC (expr, input_location),
                "constraint %qE does not have type %qT",
                expr, boolean_type_node);
      return boolean_false_node;
    }

  tree value = cxx_constant_value (expr);
  return value;
}

/* Check an expression constraint. The constraint is satisfied if
   substitution succeeds ([temp.constr.expr]).

   Note that the expression is unevaluated. */

tree
satisfy_expression_constraint (tree t, tree args,
                               tsubst_flags_t complain, tree in_decl)
{
  cp_unevaluated guard;
  deferring_access_check_sentinel deferring;

  tree expr = EXPR_CONSTR_EXPR (t);
  tree check = tsubst_expr (expr, args, complain, in_decl, false);
  if (check == error_mark_node)
    return boolean_false_node;
  if (!perform_deferred_access_checks (tf_none))
    return boolean_false_node;

  return boolean_true_node;
}

/* Check a type constraint. The constraint is satisfied if
   substitution succeeds. */

inline tree
satisfy_type_constraint (tree t, tree args,
                         tsubst_flags_t complain, tree in_decl)
{
  deferring_access_check_sentinel deferring;
  tree type = TYPE_CONSTR_TYPE (t);
  gcc_assert (TYPE_P (type) || type == error_mark_node);
  tree check = tsubst (type, args, complain, in_decl);
  if (error_operand_p (check))
    return boolean_false_node;
  if (!perform_deferred_access_checks (complain))
    return boolean_false_node;

  return boolean_true_node;
}

/* Check an implicit conversion constraint.  */

tree
satisfy_implicit_conversion_constraint (tree t, tree args,
                                        tsubst_flags_t complain, tree in_decl)
{
  /* Don't tsubst as if we're processing a template. If we try
     to we can end up generating template-like expressions
     (e.g., modop-exprs) that aren't properly typed.  */
  tree expr =
    tsubst_expr (ICONV_CONSTR_EXPR (t), args, complain, in_decl, false);
  if (expr == error_mark_node)
    return boolean_false_node;

  /* Get the transformed target type.  */
  tree type = tsubst (ICONV_CONSTR_TYPE (t), args, complain, in_decl);
  if (type == error_mark_node)
    return boolean_false_node;

  /* Attempt the conversion as a direct initialization
     of the form TYPE <unspecified> = EXPR.  */
  tree conv =
    perform_direct_initialization_if_possible (type, expr, false, complain);
  if (conv == NULL_TREE || conv == error_mark_node)
    return boolean_false_node;
  else
    return boolean_true_node;
}

/* Check an argument deduction constraint. */

tree
satisfy_argument_deduction_constraint (tree t, tree args,
                                       tsubst_flags_t complain, tree in_decl)
{
  /* Substitute through the expression. */
  tree expr = DEDUCT_CONSTR_EXPR (t);
  tree init = tsubst_expr (expr, args, complain, in_decl, false);
  if (expr == error_mark_node)
    return boolean_false_node;

  /* Perform auto or decltype(auto) deduction to get the result. */
  tree pattern = DEDUCT_CONSTR_PATTERN (t);
  tree placeholder = DEDUCT_CONSTR_PLACEHOLDER (t);
  tree constr = PLACEHOLDER_TYPE_CONSTRAINTS (placeholder);
  tree type_canonical = TYPE_CANONICAL (placeholder);
  PLACEHOLDER_TYPE_CONSTRAINTS (placeholder)
    = tsubst_constraint (constr, args, complain|tf_partial, in_decl);
  TYPE_CANONICAL (placeholder) = NULL_TREE;
  tree type = do_auto_deduction (pattern, init, placeholder,
                                 complain, adc_requirement);
  PLACEHOLDER_TYPE_CONSTRAINTS (placeholder) = constr;
  TYPE_CANONICAL (placeholder) = type_canonical;
  if (type == error_mark_node)
    return boolean_false_node;

  return boolean_true_node;
}

/* Check an exception constraint. An exception constraint for an
   expression e is satisfied when noexcept(e) is true. */

tree
satisfy_exception_constraint (tree t, tree args,
                              tsubst_flags_t complain, tree in_decl)
{
  tree expr = EXCEPT_CONSTR_EXPR (t);
  tree check = tsubst_expr (expr, args, complain, in_decl, false);
  if (check == error_mark_node)
    return boolean_false_node;

  if (expr_noexcept_p (check, complain))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Check a parameterized constraint. */

tree
satisfy_parameterized_constraint (tree t, tree args,
                                  tsubst_flags_t complain, tree in_decl)
{
  local_specialization_stack stack;
  tree parms = PARM_CONSTR_PARMS (t);
  tree vars = tsubst_constraint_variables (parms, args, complain, in_decl);
  if (vars == error_mark_node)
    return boolean_false_node;
  tree constr = PARM_CONSTR_OPERAND (t);
  return satisfy_constraint_1 (constr, args, complain, in_decl);
}

/* Check that the conjunction of constraints is satisfied. Note
   that if left operand is not satisfied, the right operand
   is not checked.

   FIXME: Check that this wouldn't result in a user-defined
   operator. Note that this error is partially diagnosed in
   satisfy_predicate_constraint. It would be nice to diagnose
   the overload, but I don't think it's strictly necessary.  */

tree
satisfy_conjunction (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree t0 = satisfy_constraint_1 (TREE_OPERAND (t, 0), args, complain, in_decl);
  if (t0 == boolean_false_node)
    return t0;
  tree t1 = satisfy_constraint_1 (TREE_OPERAND (t, 1), args, complain, in_decl);
  if (t1 == boolean_false_node)
    return t1;
  return boolean_true_node;
}

/* Check that the disjunction of constraints is satisfied. Note
   that if the left operand is satisfied, the right operand is not
   checked.  */

tree
satisfy_disjunction (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree t0 = satisfy_constraint_1 (TREE_OPERAND (t, 0), args, complain, in_decl);
  if (t0 == boolean_true_node)
    return boolean_true_node;
  tree t1 = satisfy_constraint_1 (TREE_OPERAND (t, 1), args, complain, in_decl);
  if (t1 == boolean_true_node)
    return boolean_true_node;
  return boolean_false_node;
}

/* Dispatch to an appropriate satisfaction routine depending on the
   tree code of T.  */

tree
satisfy_constraint_1 (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  gcc_assert (!processing_template_decl);

  if (!t)
    return boolean_false_node;

  if (t == error_mark_node)
    return boolean_false_node;

  switch (TREE_CODE (t))
  {
  case PRED_CONSTR:
    return satisfy_predicate_constraint (t, args, complain, in_decl);

  case EXPR_CONSTR:
    return satisfy_expression_constraint (t, args, complain, in_decl);

  case TYPE_CONSTR:
    return satisfy_type_constraint (t, args, complain, in_decl);

  case ICONV_CONSTR:
    return satisfy_implicit_conversion_constraint (t, args, complain, in_decl);

  case DEDUCT_CONSTR:
    return satisfy_argument_deduction_constraint (t, args, complain, in_decl);

  case EXCEPT_CONSTR:
    return satisfy_exception_constraint (t, args, complain, in_decl);

  case PARM_CONSTR:
    return satisfy_parameterized_constraint (t, args, complain, in_decl);

  case CONJ_CONSTR:
    return satisfy_conjunction (t, args, complain, in_decl);

  case DISJ_CONSTR:
    return satisfy_disjunction (t, args, complain, in_decl);

  case EXPR_PACK_EXPANSION:
    return satisfy_pack_expansion (t, args, complain, in_decl);

  default:
    gcc_unreachable ();
  }
  return boolean_false_node;
}

/* Check that the constraint is satisfied, according to the rules
   for that constraint. Note that each satisfy_* function returns
   true or false, depending on whether it is satisfied or not.  */

tree
satisfy_constraint (tree t, tree args)
{
  /* Turn off template processing. Constraint satisfaction only applies
     to non-dependent terms, so we want full checking here.  */
  processing_template_decl_sentinel sentinel (true);
  /* Avoid early exit in tsubst and tsubst_copy from null args; since earlier
     substitution was done with processing_template_decl forced on, there will
     be expressions that still need semantic processing, possibly buried in
     decltype or a template argument.  */
  if (args == NULL_TREE)
    args = make_tree_vec (1);
  return satisfy_constraint_1 (t, args, tf_none, NULL_TREE);
}

/* Check the associated constraints in CI against the given
   ARGS, returning true when the constraints are satisfied
   and false otherwise.  */

tree
satisfy_associated_constraints (tree ci, tree args)
{
  /* If there are no constraints then this is trivially satisfied. */
  if (!ci)
    return boolean_true_node;

  /* If any arguments depend on template parameters, we can't
     check constraints. */
  if (args && uses_template_parms (args))
    return boolean_true_node;

  /* Invalid requirements cannot be satisfied. */
  if (!valid_constraints_p (ci))
    return boolean_false_node;

  return satisfy_constraint (CI_NORMALIZED_CONSTRAINTS (ci), args);
}

} /* namespace */

/* Evaluate the given constraint, returning boolean_true_node
   if the constraint is satisfied and boolean_false_node
   otherwise. */

tree
evaluate_constraints (tree constr, tree args)
{
  gcc_assert (constraint_p (constr));
  return satisfy_constraint (normalize_constraint (constr), args);
}

/* Evaluate the function concept FN by substituting its own args
   into its definition and evaluating that as the result. Returns
   boolean_true_node if the constraints are satisfied and
   boolean_false_node otherwise.  */

tree
evaluate_function_concept (tree fn, tree args)
{
  ++processing_template_decl;
  /* We lift using DECL_TI_ARGS because we want to delay producing
     non-dependent expressions until we're doing satisfaction.  We can't just
     go without any substitution because we need to lower the level of 'auto's
     in type deduction constraints.  */
  tree constr = transform_expression (lift_function_definition
				      (fn, DECL_TI_ARGS (fn)));
  --processing_template_decl;
  return satisfy_constraint (constr, args);
}

/* Evaluate the variable concept VAR by substituting its own args into
   its initializer and checking the resulting constraint. Returns
   boolean_true_node if the constraints are satisfied and
   boolean_false_node otherwise.  */

tree
evaluate_variable_concept (tree decl, tree args)
{
  ++processing_template_decl;
  tree constr = transform_expression (lift_variable_initializer
				      (decl, DECL_TI_ARGS (decl)));
  --processing_template_decl;
  return satisfy_constraint (constr, args);
}

/* Evaluate the given expression as if it were a predicate
   constraint. Returns boolean_true_node if the constraint
   is satisfied and boolean_false_node otherwise. */

tree
evaluate_constraint_expression (tree expr, tree args)
{
  ++processing_template_decl;
  tree constr = transform_expression (lift_expression (expr));
  --processing_template_decl;
  return satisfy_constraint (constr, args);
}

/* Returns true if the DECL's constraints are satisfied.
   This is used in cases where a declaration is formed but
   before it is used (e.g., overload resolution). */

bool
constraints_satisfied_p (tree decl)
{
  /* Get the constraints to check for satisfaction. This depends
     on whether we're looking at a template specialization or not. */
  tree ci;
  tree args = NULL_TREE;
  if (tree ti = DECL_TEMPLATE_INFO (decl))
    {
      tree tmpl = TI_TEMPLATE (ti);
      ci = get_constraints (tmpl);
      int depth = TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl));
      args = get_innermost_template_args (TI_ARGS (ti), depth);
    }
  else
    {
      ci = get_constraints (decl);
    }

  tree eval = satisfy_associated_constraints (ci, args);
  return eval == boolean_true_node;
}

/* Returns true if the constraints are satisfied by ARGS.
   Here, T can be either a constraint or a constrained
   declaration.  */

bool
constraints_satisfied_p (tree t, tree args)
{
  tree eval;
  if (constraint_p (t))
    eval = evaluate_constraints (t, args);
  else
    eval = satisfy_associated_constraints (get_constraints (t), args);
  return eval == boolean_true_node;
}

namespace
{

/* Normalize EXPR and determine if the resulting constraint is
   satisfied by ARGS. Returns true if and only if the constraint
   is satisfied.  This is used extensively by diagnostics to
   determine causes for failure.  */

inline bool
constraint_expression_satisfied_p (tree expr, tree args)
{
  return evaluate_constraint_expression (expr, args) == boolean_true_node;
}

} /* namespace */


/*---------------------------------------------------------------------------
                Semantic analysis of requires-expressions
---------------------------------------------------------------------------*/

/* Finish a requires expression for the given PARMS (possibly
   null) and the non-empty sequence of requirements. */
tree
finish_requires_expr (tree parms, tree reqs)
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
  return r;
}

/* Construct a requirement for the validity of EXPR. */
tree
finish_simple_requirement (tree expr)
{
  return build_nt (SIMPLE_REQ, expr);
}

/* Construct a requirement for the validity of TYPE. */
tree
finish_type_requirement (tree type)
{
  return build_nt (TYPE_REQ, type);
}

/* Construct a requirement for the validity of EXPR, along with
   its properties. if TYPE is non-null, then it specifies either
   an implicit conversion or argument deduction constraint,
   depending on whether any placeholders occur in the type name.
   NOEXCEPT_P is true iff the noexcept keyword was specified. */
tree
finish_compound_requirement (tree expr, tree type, bool noexcept_p)
{
  tree req = build_nt (COMPOUND_REQ, expr, type);
  COMPOUND_REQ_NOEXCEPT_P (req) = noexcept_p;
  return req;
}

/* Finish a nested requirement. */
tree
finish_nested_requirement (tree expr)
{
  return build_nt (NESTED_REQ, expr);
}

// Check that FN satisfies the structural requirements of a
// function concept definition.
tree
check_function_concept (tree fn)
{
  // Check that the function is comprised of only a single
  // return statement.
  tree body = DECL_SAVED_TREE (fn);
  if (TREE_CODE (body) == BIND_EXPR)
    body = BIND_EXPR_BODY (body);

  // Sometimes a function call results in the creation of clean up
  // points. Allow these to be preserved in the body of the
  // constraint, as we might actually need them for some constexpr
  // evaluations.
  if (TREE_CODE (body) == CLEANUP_POINT_EXPR)
    body = TREE_OPERAND (body, 0);

  /* Check that the definition is written correctly. */
  if (TREE_CODE (body) != RETURN_EXPR)
    {
      location_t loc = DECL_SOURCE_LOCATION (fn);
      if (TREE_CODE (body) == STATEMENT_LIST && !STATEMENT_LIST_HEAD (body))
        error_at (loc, "definition of concept %qD is empty", fn);
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

/* Returns true when the the constraints in A subsume those in B.  */
bool
subsumes_constraints (tree a, tree b)
{
  gcc_assert (!a || TREE_CODE (a) == CONSTRAINT_INFO);
  gcc_assert (!b || TREE_CODE (b) == CONSTRAINT_INFO);
  return subsumes (a, b);
}

/* Returns true when the the constraints in A subsume those in B, but
   the constraints in B do not subsume the constraints in A.  */

bool
strictly_subsumes (tree a, tree b)
{
  return subsumes (a, b) && !subsumes (b, a);
}

/* Determines which of the declarations, A or B, is more constrained.
   That is, which declaration's constraints subsume but are not subsumed
   by the other's?

   Returns 1 if A is more constrained than B, -1 if B is more constrained
   than A, and 0 otherwise. */
int
more_constrained (tree d1, tree d2)
{
  tree c1 = get_constraints (d1);
  tree c2 = get_constraints (d2);
  int winner = 0;
  if (subsumes_constraints (c1, c2))
    ++winner;
  if (subsumes_constraints (c2, c1))
    --winner;
  return winner;
}

/* Returns true if D1 is at least as constrained as D2. That is, the
   associated constraints of D1 subsume those of D2, or both declarations
   are unconstrained. */
bool
at_least_as_constrained (tree d1, tree d2)
{
  tree c1 = get_constraints (d1);
  tree c2 = get_constraints (d2);
  return subsumes_constraints (c1, c2);
}


/*---------------------------------------------------------------------------
                        Constraint diagnostics
---------------------------------------------------------------------------*/

/* The diagnosis of constraints performs a combination of
   normalization and satisfaction testing. We recursively
   walk through the conjunction (or disjunctions) of associated
   constraints, testing each sub-expression in turn.

   We currently restrict diagnostics to just the top-level
   conjunctions within the associated constraints. A fully
   recursive walk is possible, but it can generate a lot
   of errors. */


namespace {

void diagnose_expression (location_t, tree, tree);
void diagnose_constraint (location_t, tree, tree);

/* Diagnose a conjunction of constraints. */
void
diagnose_logical_operation (location_t loc, tree t, tree args)
{
  diagnose_expression (loc, TREE_OPERAND (t, 0), args);
  diagnose_expression (loc, TREE_OPERAND (t, 0), args);
}

/* Determine if the trait expression T is satisfied by ARGS.
   Emit a precise diagnostic if it is not. */
void
diagnose_trait_expression (location_t loc, tree t, tree args)
{
  if (constraint_expression_satisfied_p (t, args))
    return;

  /* Rebuild the trait expression so we can diagnose the
     specific failure. */
  ++processing_template_decl;
  tree expr = tsubst_expr (t, args, tf_none, NULL_TREE, false);
  --processing_template_decl;

  tree t1 = TRAIT_EXPR_TYPE1 (expr);
  tree t2 = TRAIT_EXPR_TYPE2 (expr);
  switch (TRAIT_EXPR_KIND (t))
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

/* Determine if the call expression T, when normalized as a constraint,
   is satisfied by ARGS.

   TODO: If T is refers to a concept, We could recursively analyze
   its definition to identify the exact failure, but that could
   emit a *lot* of error messages (defeating the purpose of
   improved diagnostics). Consider adding a flag to control the
   depth of diagnostics. */
void
diagnose_call_expression (location_t loc, tree t, tree args)
{
  if (constraint_expression_satisfied_p (t, args))
    return;

  /* Rebuild the expression for the purpose of diagnostics. */
  ++processing_template_decl;
  tree expr = tsubst_expr (t, args, tf_none, NULL_TREE, false);
  --processing_template_decl;

  /* If the function call is known to be a concept check, then
     diagnose it differently (i.e., we may recurse). */
  if (resolve_constraint_check (t))
    inform (loc, "  concept %qE was not satisfied", expr);
  else
    inform (loc, "  %qE evaluated to false", expr);
}

/* Determine if the template-id T, when normalized as a constraint
   is satisfied by ARGS. */
void
diagnose_template_id (location_t loc, tree t, tree args)
{
  /* Check for invalid template-ids. */
  if (!variable_template_p (TREE_OPERAND (t, 0)))
    {
      inform (loc, "  invalid constraint %qE", t);
      return;
    }

  if (constraint_expression_satisfied_p (t, args))
    return;

  /* Rebuild the expression for the purpose of diagnostics. */
  ++processing_template_decl;
  tree expr = tsubst_expr (t, args, tf_none, NULL_TREE, false);
  --processing_template_decl;

  tree var = DECL_TEMPLATE_RESULT (TREE_OPERAND (t, 0));
  if (DECL_DECLARED_CONCEPT_P (var))
    inform (loc, "  concept %qE was not satisfied", expr);
  else
    inform (loc, "  %qE evaluated to false", expr);
}

/* Determine if the requires-expression, when normalized as a
   constraint is satisfied by ARGS.

   TODO: Build sets of expressions, types, and constraints
   based on the requirements in T and emit specific diagnostics
   for those. */
void
diagnose_requires_expression (location_t loc, tree t, tree args)
{
  if (constraint_expression_satisfied_p (t, args))
    return;
  inform (loc, "requirements not satisfied");
}

void
diagnose_pack_expansion (location_t loc, tree t, tree args)
{
  if (constraint_expression_satisfied_p (t, args))
    return;

  /* Make sure that we don't have naked packs that we don't expect. */
  if (!same_type_p (TREE_TYPE (t), boolean_type_node))
    {
      inform (loc, "invalid pack expansion in constraint %qE", t);
      return;
    }

  inform (loc, "  in the expansion of %qE", t);

  /* Get the vector of expanded arguments. Note that n must not
     be 0 since this constraint is not satisfied.  */
  ++processing_template_decl;
  tree exprs = tsubst_pack_expansion (t, args, tf_none, NULL_TREE);
  --processing_template_decl;
  if (exprs == error_mark_node)
    {
      /* TODO: This error message could be better. */
      inform (loc, "    substitution failure occurred during expansion");
      return;
    }

  /* Check each expanded constraint separately. */
  int n = TREE_VEC_LENGTH (exprs);
  for (int i = 0; i < n; ++i)
    {
      tree expr = TREE_VEC_ELT (exprs, i);
      if (!constraint_expression_satisfied_p (expr, args))
        inform (loc, "    %qE was not satisfied", expr);
    }
}

/* Diagnose an expression that would be characterized as
   a predicate constraint. */
void
diagnose_other_expression (location_t loc, tree t, tree args)
{
  if (constraint_expression_satisfied_p (t, args))
    return;
  inform (loc, "  %qE evaluated to false", t);
}

void
diagnose_expression (location_t loc, tree t, tree args)
{
  switch (TREE_CODE (t))
    {
    case TRUTH_ANDIF_EXPR:
      diagnose_logical_operation (loc, t, args);
      break;

    case TRUTH_ORIF_EXPR:
      diagnose_logical_operation (loc, t, args);
      break;

    case CALL_EXPR:
      diagnose_call_expression (loc, t, args);
      break;

    case TEMPLATE_ID_EXPR:
      diagnose_template_id (loc, t, args);
      break;

    case REQUIRES_EXPR:
      diagnose_requires_expression (loc, t, args);
      break;

    case TRAIT_EXPR:
      diagnose_trait_expression (loc, t, args);
      break;

    case EXPR_PACK_EXPANSION:
      diagnose_pack_expansion (loc, t, args);
      break;

    default:
      diagnose_other_expression (loc, t, args);
      break;
    }
}

inline void
diagnose_predicate_constraint (location_t loc, tree t, tree args)
{
  diagnose_expression (loc, PRED_CONSTR_EXPR (t), args);
}

inline void
diagnose_conjunction (location_t loc, tree t, tree args)
{
  diagnose_constraint (loc, TREE_OPERAND (t, 0), args);
  diagnose_constraint (loc, TREE_OPERAND (t, 1), args);
}

/* Diagnose the constraint T for the given ARGS. This is only
   ever invoked on the associated constraints, so we can
   only have conjunctions of predicate constraints. */
void
diagnose_constraint (location_t loc, tree t, tree args)
{
  switch (TREE_CODE (t))
    {
    case CONJ_CONSTR:
      diagnose_conjunction (loc, t, args);
      break;

    case PRED_CONSTR:
      diagnose_predicate_constraint (loc, t, args);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Diagnose the reason(s) why ARGS do not satisfy the constraints
   of declaration DECL. */

void
diagnose_declaration_constraints (location_t loc, tree decl, tree args)
{
  inform (loc, "  constraints not satisfied");

  /* Constraints are attached to the template.  */
  if (tree ti = DECL_TEMPLATE_INFO (decl))
    {
      decl = TI_TEMPLATE (ti);
      if (!args)
	args = TI_ARGS (ti);
    }

  /* Check that the constraints are actually valid.  */
  tree ci = get_constraints (decl);
  if (!valid_constraints_p (ci))
    {
      inform (loc, "    invalid constraints");
      return;
    }

  /* Recursively diagnose the associated constraints.  */
  diagnose_constraint (loc, CI_ASSOCIATED_CONSTRAINTS (ci), args);
}

} // namespace

/* Emit diagnostics detailing the failure ARGS to satisfy the
   constraints of T. Here, T can be either a constraint
   or a declaration.  */

void
diagnose_constraints (location_t loc, tree t, tree args)
{
  if (constraint_p (t))
    diagnose_constraint (loc, t, args);
  else
    diagnose_declaration_constraints (loc, t, args);
}
