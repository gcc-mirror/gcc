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

/*---------------------------------------------------------------------------
                       Operations on constraints
---------------------------------------------------------------------------*/

/* Returns true if C is a constraint tree code. Note that ERROR_MARK
   is a valid constraint.  */

static inline bool
constraint_p (tree_code c)
{
  return ((PRED_CONSTR <= c && c <= DISJ_CONSTR)
          || c == EXPR_PACK_EXPANSION
          || c == ERROR_MARK);
}

/* Returns true if T is a constraint. Note that error_mark_node
   is a valid constraint.  */

bool
constraint_p (tree t)
{
  return constraint_p (TREE_CODE (t));
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

/* Returns true if any of the arguments in the template
   argument list is a wildcard or wildcard pack.  */

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

/* Build a new call expression, but don't actually generate a
   new function call. We just want the tree, not the semantics.  */

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
  int nerrs = 0;
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
  ++processing_template_decl;
  tree result = coerce_template_parms (parms, args, tmpl);
  --processing_template_decl;
  if (result != error_mark_node)
    {
      tree decl = DECL_TEMPLATE_RESULT (tmpl);
      return build_tree_list (result, decl);
    }
  else
    return error_mark_node;
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
                       Constraint implication learning
---------------------------------------------------------------------------*/

/* The implication context determines how we memoize concept checks.
   Given two checks C1 and C2, the direction of implication depends
   on whether we are learning implications of a conjunction or disjunction.
   For example:

      template<typename T> concept bool C = ...;
      template<typenaem T> concept bool D = C<T> && true;

   From this, we can learn that D<T> implies C<T>. We cannot learn,
   without further testing, that C<T> does not imply D<T>. If, for
   example, C<T> were defined as true, then these constraints would
   be logically equivalent.

   In rare cases, we may start with a logical equivalence. For example:

      template<typename T> concept bool C = ...;
      template<typename T> concept bool D = C<T>;

   Here, we learn that C<T> implies D<T> and vice versa.   */

enum implication_context
{
  conjunction_cxt, /* C1 implies C2. */
  disjunction_cxt, /* C2 implies C1. */
  equivalence_cxt  /* C1 implies C2, C2 implies C1. */
};

void learn_implications(tree, tree, implication_context);

void
learn_implication (tree parent, tree child, implication_context cxt)
{
  switch (cxt)
    {
      case conjunction_cxt:
        save_subsumption_result (parent, child, true);
        break;
      case disjunction_cxt:
        save_subsumption_result (child, parent, true);
        break;
      case equivalence_cxt:
        save_subsumption_result (parent, child, true);
        save_subsumption_result (child, parent, true);
        break;
    }
}

void
learn_logical_operation (tree parent, tree constr, implication_context cxt)
{
  learn_implications (parent, TREE_OPERAND (constr, 0), cxt);
  learn_implications (parent, TREE_OPERAND (constr, 1), cxt);
}

void
learn_implications (tree parent, tree constr, implication_context cxt)
{
  switch (TREE_CODE (constr))
    {
      case CHECK_CONSTR:
        return learn_implication (parent, constr, cxt);

      case CONJ_CONSTR:
        if (cxt == disjunction_cxt)
          return;
        return learn_logical_operation (parent, constr, cxt);

      case DISJ_CONSTR:
        if (cxt == conjunction_cxt)
          return;
        return learn_logical_operation (parent, constr, cxt);

      default:
        break;
    }
}

/* Quickly scan the top-level constraints of CONSTR to learn and
   cache logical relations between concepts.  The search does not
   include conjunctions of disjunctions or vice versa.  */

void
learn_implications (tree tmpl, tree args, tree constr)
{
  /* Don't memoize relations between non-dependent arguemnts. It's not
     helpful. */
  if (!uses_template_parms (args))
    return;

  /* Build a check constraint for the purpose of caching. */
  tree parent = build_nt (CHECK_CONSTR, tmpl, args);

  /* Start learning based on the kind of the top-level contraint. */
  if (TREE_CODE (constr) == CONJ_CONSTR)
    return learn_logical_operation (parent, constr, conjunction_cxt);
  else if (TREE_CODE (constr) == DISJ_CONSTR)
    return learn_logical_operation (parent, constr, disjunction_cxt);
  else if (TREE_CODE (constr) == CHECK_CONSTR)
    /* This is the rare concept alias case. */
    return learn_implication (parent, constr, equivalence_cxt);
}

/*---------------------------------------------------------------------------
                       Expansion of concept definitions
---------------------------------------------------------------------------*/

/* Returns the expression of a function concept. */

tree
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

tree
get_variable_initializer (tree var)
{
  tree init = DECL_INITIAL (var);
  if (!init)
    return error_mark_node;
  return init;
}

/* Returns the definition of a variable or function concept.  */

tree
get_concept_definition (tree decl)
{
  if (TREE_CODE (decl) == VAR_DECL)
    return get_variable_initializer (decl);
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    return get_returned_expression (decl);
  gcc_unreachable ();
}

int expansion_level = 0;

struct expanding_concept_sentinel
{
  expanding_concept_sentinel ()
  {
    ++expansion_level;
  }

  ~expanding_concept_sentinel()
  {
    --expansion_level;
  }
};


} /* namespace */

/* Returns true when a concept is being expanded.  */

bool
expanding_concept()
{
  return expansion_level > 0;
}

/* Expand a concept declaration (not a template) and its arguments to
   a constraint defined by the concept's initializer or definition.  */

tree
expand_concept (tree decl, tree args)
{
  expanding_concept_sentinel sentinel;

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    decl = DECL_TEMPLATE_RESULT (decl);
  tree tmpl = DECL_TI_TEMPLATE (decl);

  /* Check for a previous specialization. */
  if (tree spec = get_concept_expansion (tmpl, args))
    return spec;

  /* Substitute the arguments to form a new definition expression.  */
  tree def = get_concept_definition (decl);

  ++processing_template_decl;
  tree result = tsubst_expr (def, args, tf_none, NULL_TREE, true);
  --processing_template_decl;
  if (result == error_mark_node)
    return error_mark_node;

  /* And lastly, normalize it, check for implications, and save
     the specialization for later.  */
  tree norm = normalize_expression (result);
  learn_implications (tmpl, args, norm);
  return save_concept_expansion (tmpl, args, norm);
}


/*---------------------------------------------------------------------------
                Stepwise normalization of expressions

This set of functions will transform an expression into a constraint
in a sequence of steps. Normalization does not not look into concept
definitions.
---------------------------------------------------------------------------*/

/* Transform a logical-or or logical-and expression into either
   a conjunction or disjunction. */

tree
normalize_logical_operation (tree t, tree_code c)
{
  tree t0 = normalize_expression (TREE_OPERAND (t, 0));
  tree t1 = normalize_expression (TREE_OPERAND (t, 1));
  return build_nt (c, t0, t1);
}

/* A simple requirement T introduces an expression constraint
   for its expression. */

inline tree
normalize_simple_requirement (tree t)
{
  return build_nt (EXPR_CONSTR, TREE_OPERAND (t, 0));
}

/* A type requirement T introduce a type constraint for its type.  */

inline tree
normalize_type_requirement (tree t)
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
normalize_compound_requirement (tree t)
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
normalize_nested_requirement (tree t)
{
  return normalize_expression (TREE_OPERAND (t, 0));
}

/* Transform a requirement T into one or more constraints.  */

tree
normalize_requirement (tree t)
{
  switch (TREE_CODE (t))
    {
    case SIMPLE_REQ:
      return normalize_simple_requirement (t);

    case TYPE_REQ:
      return normalize_type_requirement (t);

    case COMPOUND_REQ:
      return normalize_compound_requirement (t);

    case NESTED_REQ:
      return normalize_nested_requirement (t);

    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Transform a sequence of requirements into a conjunction of
   constraints. */

tree
normalize_requirements (tree t)
{
  tree result = NULL_TREE;
  for (; t; t = TREE_CHAIN (t))
    {
      tree constr = normalize_requirement (TREE_VALUE (t));
      result = conjoin_constraints (result, constr);
    }
  return result;
}

/* The normal form of a requires-expression is a parameterized
   constraint having the same parameters and a conjunction of
   constraints representing the normal form of requirements.  */

tree
normalize_requires_expression (tree t)
{
  tree operand = normalize_requirements (TREE_OPERAND (t, 1));
  if (tree parms = TREE_OPERAND (t, 0))
    return build_nt (PARM_CONSTR, parms, operand);
  else
    return operand;
}

/* For a template-id referring to a variable concept, returns
   a check constraint. Otherwise, returns a predicate constraint. */

tree
normalize_template_id_expression (tree t)
{
  if (tree info = resolve_variable_concept_check (t))
    {
      if (info == error_mark_node)
        {
          /* We get this when the template arguments don't match
             the variable concept. */
          error ("invalid reference to concept %qE", t);
          return error_mark_node;
        }

      tree decl = TREE_VALUE (info);
      tree args = TREE_PURPOSE (info);
      return build_nt (CHECK_CONSTR, decl, args);
    }

  /* Check that we didn't refer to a function concept like a variable.  */
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

  return build_nt (PRED_CONSTR, t);
}

/* For a call expression to a function concept, returns a check
   constraint. Otherwise, returns a predicate constraint. */

tree
normalize_call_expression (tree t)
{
  /* Try to resolve this function call as a concept.  If not, then
     it can be returned as a predicate constraint.  */
  tree check = resolve_constraint_check (t);
  if (!check)
    return build_nt (PRED_CONSTR, t);
  if (check == error_mark_node)
    {
      /* TODO: Improve diagnostics. We could report why the reference
         is invalid. */
      error ("invalid reference to concept %qE", t);
      return error_mark_node;
    }

  tree fn = TREE_VALUE (check);
  tree args = TREE_PURPOSE (check);
  return build_nt (CHECK_CONSTR, fn, args);
}

/* If T is a call to an overloaded && or || operator, diagnose that
   as a non-SFINAEable error.  Returns true if an error is emitted.

   TODO: It would be better to diagnose this at the point of definition,
   if possible. Perhaps we should immediately do a first-pass normalization
   of a concept definition to catch obvious non-dependent errors like
   this.  */

bool
check_for_logical_overloads (tree t)
{
  if (TREE_CODE (t) != CALL_EXPR)
    return false;

  tree fn = CALL_EXPR_FN (t);

  /* For member calls, try extracting the function from the
     component ref.  */
  if (TREE_CODE (fn) == COMPONENT_REF)
    {
      fn = TREE_OPERAND (fn, 1);
      if (TREE_CODE (fn) == BASELINK)
        fn = BASELINK_FUNCTIONS (fn);
    }

  if (TREE_CODE (fn) != FUNCTION_DECL)
    return false;

  if (DECL_OVERLOADED_OPERATOR_P (fn))
    {
      location_t loc = EXPR_LOC_OR_LOC (t, input_location);
      error_at (loc, "constraint %qE, uses overloaded operator", t);
      return true;
    }

  return false;
}

/* The normal form of an atom depends on the expression. The normal
   form of a function call to a function concept is a check constraint
   for that concept. The normal form of a reference to a variable
   concept is a check constraint for that concept. Otherwise, the
   constraint is a predicate constraint.  */

tree
normalize_atom (tree t)
{
  /* We can get constraints pushed down through pack expansions, so
     just return them. */
  if (constraint_p (t))
    return t;

  tree type = TREE_TYPE (t);
  if (!type || type_unknown_p (t) || TREE_CODE (type) == TEMPLATE_TYPE_PARM)
    ;
  else if (!dependent_type_p (type))
    {
      if (check_for_logical_overloads (t))
        return error_mark_node;

      type = cv_unqualified (type);
      if (!same_type_p (type, boolean_type_node))
	{
	  error ("predicate constraint %q+E does not have type %<bool%>", t);
	  return error_mark_node;
	}
    }

  if (TREE_CODE (t) == TEMPLATE_ID_EXPR)
    return normalize_template_id_expression (t);
  if (TREE_CODE (t) == CALL_EXPR)
    return normalize_call_expression (t);
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
normalize_pack_expansion (tree t)
{
  tree pat = normalize_expression (PACK_EXPANSION_PATTERN (t));
  return push_down_pack_expansion (t, pat);
}

/* Transform an expression into a constraint.  */

tree
normalize_any_expression (tree t)
{
  switch (TREE_CODE (t))
    {
    case TRUTH_ANDIF_EXPR:
      return normalize_logical_operation (t, CONJ_CONSTR);

    case TRUTH_ORIF_EXPR:
      return normalize_logical_operation (t, DISJ_CONSTR);

    case REQUIRES_EXPR:
      return normalize_requires_expression (t);

    case BIND_EXPR:
      return normalize_expression (BIND_EXPR_BODY (t));

    case EXPR_PACK_EXPANSION:
      return normalize_pack_expansion (t);

    default:
      /* All other constraints are atomic. */
      return normalize_atom (t);
    }
}

/* Transform a statement into an expression.  */
tree
normalize_any_statement (tree t)
{
  switch (TREE_CODE (t))
    {
    case RETURN_EXPR:
      return normalize_expression (TREE_OPERAND (t, 0));
    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Reduction rules for the declaration T.  */

tree
normalize_any_declaration (tree t)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      return normalize_atom (t);
    default:
      gcc_unreachable ();
    }
  return error_mark_node;
}

/* Returns the normal form of a constraint expression. */

tree
normalize_expression (tree t)
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
      return normalize_any_expression (t);

    case tcc_statement:
      return normalize_any_statement (t);

    case tcc_declaration:
      return normalize_any_declaration (t);

    case tcc_exceptional:
    case tcc_constant:
    case tcc_reference:
    case tcc_comparison:
      /* These are all atomic predicate constraints. */
      return normalize_atom (t);

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
  tree constr = normalize_expression (expr);
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

  return (tree)ci;
}

namespace {

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
   (or not).

   This will always return a CHECK_CONSTR. */
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

  return normalize_expression (check);
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
  tree constr = normalize_expression (check);
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
      /* A constrained parameter.  Build a constraint check
         based on the prototype parameter and then extract the
         arguments from that.  */
      tree proto = CONSTRAINED_PARM_PROTOTYPE (t);
      tree check = finish_shorthand_constraint (proto, t);
      placeholder_extract_concept_and_args (check, tmpl, args);
      return;
    }

  if (TREE_CODE (t) == CHECK_CONSTR)
    {
      tree decl = CHECK_CONSTR_CONCEPT (t);
      tmpl = DECL_TI_TEMPLATE (decl);
      args = CHECK_CONSTR_ARGS (t);
      return;
    }

    gcc_unreachable ();
}

/* Returns true iff the placeholders C1 and C2 are equivalent.  C1
   and C2 can be either CHECK_CONSTR or TEMPLATE_TYPE_PARM.  */

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

/* Substitute into a check constraint. */

tree
tsubst_check_constraint (tree t, tree args,
                         tsubst_flags_t complain, tree in_decl)
{
  tree decl = CHECK_CONSTR_CONCEPT (t);
  tree tmpl = DECL_TI_TEMPLATE (decl);
  tree targs = CHECK_CONSTR_ARGS (t);

  /* Substitute through by building an template-id expression
     and then substituting into that. */
  tree expr = build_nt(TEMPLATE_ID_EXPR, tmpl, targs);
  ++processing_template_decl;
  tree result = tsubst_expr (expr, args, complain, in_decl, false);
  --processing_template_decl;

  if (result == error_mark_node)
    return error_mark_node;

  /* Extract the results and rebuild the check constraint. */
  decl = DECL_TEMPLATE_RESULT (TREE_OPERAND (result, 0));
  args = TREE_OPERAND (result, 1);

  return build_nt (CHECK_CONSTR, decl, args);
}

/* Substitute into the conjunction of constraints. Returns
   error_mark_node if substitution into either operand fails. */

tree
tsubst_logical_operator (tree t, tree args,
			 tsubst_flags_t complain, tree in_decl)
{
  tree t0 = TREE_OPERAND (t, 0);
  tree r0 = tsubst_constraint (t0, args, complain, in_decl);
  if (r0 == error_mark_node)
    return error_mark_node;
  tree t1 = TREE_OPERAND (t, 1);
  tree r1 = tsubst_constraint (t1, args, complain, in_decl);
  if (r1 == error_mark_node)
    return error_mark_node;
  return build_nt (TREE_CODE (t), r0, r1);
}

namespace {

/* Substitute ARGS into the expression constraint T.  */

tree
tsubst_expr_constr (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  cp_unevaluated guard;
  tree expr = EXPR_CONSTR_EXPR (t);
  tree ret = tsubst_expr (expr, args, complain, in_decl, false);
  if (ret == error_mark_node)
    return error_mark_node;
  return build_nt (EXPR_CONSTR, ret);
}

/* Substitute ARGS into the type constraint T.  */

tree
tsubst_type_constr (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  tree type = TYPE_CONSTR_TYPE (t);
  tree ret = tsubst (type, args, complain, in_decl);
  if (ret == error_mark_node)
    return error_mark_node;
  return build_nt (TYPE_CONSTR, ret);
}

/* Substitute ARGS into the implicit conversion constraint T.  */

tree
tsubst_implicit_conversion_constr (tree t, tree args, tsubst_flags_t complain,
                                   tree in_decl)
{
  cp_unevaluated guard;
  tree expr = ICONV_CONSTR_EXPR (t);
  tree type = ICONV_CONSTR_TYPE (t);
  tree new_expr = tsubst_expr (expr, args, complain, in_decl, false);
  if (new_expr == error_mark_node)
    return error_mark_node;
  tree new_type = tsubst (type, args, complain, in_decl);
  if (new_type == error_mark_node)
    return error_mark_node;
  return build_nt (ICONV_CONSTR, new_expr, new_type);
}

/* Substitute ARGS into the argument deduction constraint T.  */

tree
tsubst_argument_deduction_constr (tree t, tree args, tsubst_flags_t complain,
                                  tree in_decl)
{
  cp_unevaluated guard;
  tree expr = DEDUCT_CONSTR_EXPR (t);
  tree pattern = DEDUCT_CONSTR_PATTERN (t);
  tree autos = DEDUCT_CONSTR_PLACEHOLDER(t);
  tree new_expr = tsubst_expr (expr, args, complain, in_decl, false);
  if (new_expr == error_mark_node)
    return error_mark_node;
  /* It seems like substituting through the pattern will not affect the
     placeholders.  We should (?) be able to reuse the existing list
     without any problems.  If not, then we probably want to create a
     new list of placeholders and then instantiate the pattern using
     those.  */
  tree new_pattern = tsubst (pattern, args, complain, in_decl);
  if (new_pattern == error_mark_node)
    return error_mark_node;
  return build_nt (DEDUCT_CONSTR, new_expr, new_pattern, autos);
}

/* Substitute ARGS into the exception constraint T.  */

tree
tsubst_exception_constr (tree t, tree args, tsubst_flags_t complain,
			 tree in_decl)
{
  cp_unevaluated guard;
  tree expr = EXCEPT_CONSTR_EXPR (t);
  tree ret = tsubst_expr (expr, args, complain, in_decl, false);
  if (ret == error_mark_node)
    return error_mark_node;
  return build_nt (EXCEPT_CONSTR, ret);
}

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

/* Substitute ARGS into the parameterized constraint T.  */

tree
tsubst_parameterized_constraint (tree t, tree args,
				 tsubst_flags_t complain, tree in_decl)
{
  local_specialization_stack stack;
  tree vars = tsubst_constraint_variables (PARM_CONSTR_PARMS (t),
					   args, complain, in_decl);
  if (vars == error_mark_node)
    return error_mark_node;
  tree expr = tsubst_constraint (PARM_CONSTR_OPERAND (t), args,
				 complain, in_decl);
  if (expr == error_mark_node)
    return error_mark_node;
  return build_nt (PARM_CONSTR, vars, expr);
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

/* Substitute ARGS into the requirement T.  */

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
  /* Ensure that the order of constraints is the same as the original.  */
  return nreverse (r);
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

/* Substitute ARGS into the constraint T. */

tree
tsubst_constraint (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
  if (t == NULL_TREE)
    return t;
  switch (TREE_CODE (t))
  {
  case PRED_CONSTR:
    return tsubst_predicate_constraint (t, args, complain, in_decl);
  case CHECK_CONSTR:
    return tsubst_check_constraint (t, args, complain, in_decl);
  case CONJ_CONSTR:
  case DISJ_CONSTR:
    return tsubst_logical_operator (t, args, complain, in_decl);
  case PARM_CONSTR:
    return tsubst_parameterized_constraint (t, args, complain, in_decl);
  case EXPR_CONSTR:
    return tsubst_expr_constr (t, args, complain, in_decl);
  case TYPE_CONSTR:
    return tsubst_type_constr (t, args, complain, in_decl);
  case ICONV_CONSTR:
    return tsubst_implicit_conversion_constr (t, args, complain, in_decl);
  case DEDUCT_CONSTR:
    return tsubst_argument_deduction_constr (t, args, complain, in_decl);
  case EXCEPT_CONSTR:
    return tsubst_exception_constr (t, args, complain, in_decl);
  default:
    gcc_unreachable ();
  }
  return error_mark_node;
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

  /* TODO: It might be better to normalize each expanded term
     and evaluate them separately. That would provide better
     opportunities for diagnostics.  */
  for (int i = 0; i < TREE_VEC_LENGTH (exprs); ++i)
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
  tree expr = TREE_OPERAND (t, 0);

  /* We should never have a naked pack expansion in a predicate constraint.  */
  gcc_assert (TREE_CODE (expr) != EXPR_PACK_EXPANSION);

  /* If substitution into the expression fails, the constraint
     is not satisfied.  */
  expr = tsubst_expr (expr, args, complain, in_decl, false);
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

  return cxx_constant_value (expr);
}

/* A concept check constraint like C<CARGS> is satisfied if substituting ARGS
   into CARGS succeeds and C is satisfied for the resulting arguments.  */

tree
satisfy_check_constraint (tree t, tree args,
                          tsubst_flags_t complain, tree in_decl)
{
  tree decl = CHECK_CONSTR_CONCEPT (t);
  tree tmpl = DECL_TI_TEMPLATE (decl);
  tree cargs = CHECK_CONSTR_ARGS (t);

  /* Instantiate the concept check arguments.  */
  tree targs = tsubst (cargs, args, tf_none, NULL_TREE);
  if (targs == error_mark_node)
    return boolean_false_node;

  /* Search for a previous value.  */
  if (tree prev = lookup_concept_satisfaction (tmpl, targs))
    return prev;

  /* Expand the concept; failure here implies non-satisfaction.  */
  tree def = expand_concept (decl, targs);
  if (def == error_mark_node)
    return memoize_concept_satisfaction (tmpl, args, boolean_false_node);

  /* Recursively satisfy the constraint.  */
  tree result = satisfy_constraint_1 (def, targs, complain, in_decl);
  return memoize_concept_satisfaction (tmpl, targs, result);
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
    return boolean_false_node;
  return satisfy_constraint_1 (TREE_OPERAND (t, 1), args, complain, in_decl);
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
  return satisfy_constraint_1 (TREE_OPERAND (t, 1), args, complain, in_decl);
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

  case CHECK_CONSTR:
    return satisfy_check_constraint (t, args, complain, in_decl);

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
  auto_timevar time (TV_CONSTRAINT_SAT);

  /* Turn off template processing. Constraint satisfaction only applies
     to non-dependent terms, so we want to ensure full checking here.  */
  processing_template_decl_sentinel proc (true);

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

  /* Check if we've seen a previous result. */
  if (tree prev = lookup_constraint_satisfaction (ci, args))
    return prev;

  /* Actually test for satisfaction. */
  tree result = satisfy_constraint (CI_ASSOCIATED_CONSTRAINTS (ci), args);
  return memoize_constraint_satisfaction (ci, args, result);
}

} /* namespace */

/* Evaluate the given constraint, returning boolean_true_node
   if the constraint is satisfied and boolean_false_node
   otherwise. */

tree
evaluate_constraints (tree constr, tree args)
{
  gcc_assert (constraint_p (constr));
  return satisfy_constraint (constr, args);
}

/* Evaluate the function concept FN by substituting its own args
   into its definition and evaluating that as the result. Returns
   boolean_true_node if the constraints are satisfied and
   boolean_false_node otherwise.  */

tree
evaluate_function_concept (tree fn, tree args)
{
  tree constr = build_nt (CHECK_CONSTR, fn, args);
  return satisfy_constraint (constr, args);
}

/* Evaluate the variable concept VAR by substituting its own args into
   its initializer and checking the resulting constraint. Returns
   boolean_true_node if the constraints are satisfied and
   boolean_false_node otherwise.  */

tree
evaluate_variable_concept (tree var, tree args)
{
  tree constr = build_nt (CHECK_CONSTR, var, args);
  return satisfy_constraint (constr, args);
}

/* Evaluate the given expression as if it were a predicate
   constraint. Returns boolean_true_node if the constraint
   is satisfied and boolean_false_node otherwise. */

tree
evaluate_constraint_expression (tree expr, tree args)
{
  tree constr = normalize_expression (expr);
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
      ci = get_constraints (TI_TEMPLATE (ti));
      args = INNERMOST_TEMPLATE_ARGS (TI_ARGS (ti));
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

FIXME: Normalize expressions into constraints before evaluating them.
This should be the general pattern for all such diagnostics.
---------------------------------------------------------------------------*/

/* The number of detailed constraint failures.  */

int constraint_errors = 0;

/* Do not generate errors after diagnosing this number of constraint
   failures.

   FIXME: This is a really arbitrary number. Provide better control of
   constraint diagnostics with a command line option.  */

int constraint_thresh = 20;


/* Returns true if we should elide the diagnostic for a constraint failure.
   This is the case when the number of errors has exceeded the pre-configured
   threshold.  */

inline bool
elide_constraint_failure_p ()
{
  bool ret = constraint_thresh <= constraint_errors;
  ++constraint_errors;
  return ret;
}

/* Returns the number of undiagnosed errors. */

inline int
undiagnosed_constraint_failures ()
{
  return constraint_errors - constraint_thresh;
}

/* The diagnosis of constraints performs a combination of normalization
   and satisfaction testing. We recursively walk through the conjunction or
   disjunction of associated constraints, testing each sub-constraint in
   turn.  */

namespace {

void diagnose_constraint (location_t, tree, tree, tree);

/* Emit a specific diagnostics for a failed trait.  */

void
diagnose_trait_expression (location_t loc, tree, tree cur, tree args)
{
  if (constraint_expression_satisfied_p (cur, args))
    return;
  if (elide_constraint_failure_p())
    return;

  tree expr = PRED_CONSTR_EXPR (cur);
  ++processing_template_decl;
  expr = tsubst_expr (expr, args, tf_none, NULL_TREE, false);
  --processing_template_decl;

  tree t1 = TRAIT_EXPR_TYPE1 (expr);
  tree t2 = TRAIT_EXPR_TYPE2 (expr);
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

/* Diagnose the expression of a predicate constraint.  */

void
diagnose_other_expression (location_t loc, tree, tree cur, tree args)
{
  if (constraint_expression_satisfied_p (cur, args))
    return;
  if (elide_constraint_failure_p())
    return;
  inform (loc, "%qE evaluated to false", cur);
}

/* Do our best to infer meaning from predicates.  */

inline void
diagnose_predicate_constraint (location_t loc, tree orig, tree cur, tree args)
{
  if (TREE_CODE (PRED_CONSTR_EXPR (cur)) == TRAIT_EXPR)
    diagnose_trait_expression (loc, orig, cur, args);
  else
    diagnose_other_expression (loc, orig, cur, args);
}

/* Diagnose a failed pack expansion, possibly containing constraints.  */

void
diagnose_pack_expansion (location_t loc, tree, tree cur, tree args)
{
  if (constraint_expression_satisfied_p (cur, args))
    return;
  if (elide_constraint_failure_p())
    return;

  /* Make sure that we don't have naked packs that we don't expect. */
  if (!same_type_p (TREE_TYPE (cur), boolean_type_node))
    {
      inform (loc, "invalid pack expansion in constraint %qE", cur);
      return;
    }

  inform (loc, "in the expansion of %qE", cur);

  /* Get the vector of expanded arguments. Note that n must not
     be 0 since this constraint is not satisfied.  */
  ++processing_template_decl;
  tree exprs = tsubst_pack_expansion (cur, args, tf_none, NULL_TREE);
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

/* Diagnose a potentially unsatisfied concept check constraint DECL<CARGS>.
   Parameters are as for diagnose_constraint.  */

void
diagnose_check_constraint (location_t loc, tree orig, tree cur, tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;

  tree decl = CHECK_CONSTR_CONCEPT (cur);
  tree cargs = CHECK_CONSTR_ARGS (cur);
  tree tmpl = DECL_TI_TEMPLATE (decl);
  tree check = build_nt (CHECK_CONSTR, decl, cargs);

  /* Instantiate the concept check arguments.  */
  tree targs = tsubst (cargs, args, tf_none, NULL_TREE);
  if (targs == error_mark_node)
    {
      if (elide_constraint_failure_p ())
        return;
      inform (loc, "invalid use of the concept %qE", check);
      tsubst (cargs, args, tf_warning_or_error, NULL_TREE);
      return;
    }

  tree sub = build_tree_list (tmpl, targs);
  /* Update to the expanded definitions. */
  cur = expand_concept (decl, targs);
  if (cur == error_mark_node)
    {
      if (elide_constraint_failure_p ())
        return;
      inform (loc, "in the expansion of concept %qE %S", check, sub);
      cur = get_concept_definition (decl);
      tsubst_expr (cur, targs, tf_warning_or_error, NULL_TREE, false);
      return;
    }

  orig = get_concept_definition (CHECK_CONSTR_CONCEPT (orig));
  orig = normalize_expression (orig);

  location_t dloc = DECL_SOURCE_LOCATION (decl);
  inform (dloc, "within %qS", sub);
  diagnose_constraint (dloc, orig, cur, targs);
}

/* Diagnose a potentially unsatisfied conjunction or disjunction.  Parameters
   are as for diagnose_constraint.  */

void
diagnose_logical_constraint (location_t loc, tree orig, tree cur, tree args)
{
  tree t0 = TREE_OPERAND (cur, 0);
  tree t1 = TREE_OPERAND (cur, 1);
  if (!constraints_satisfied_p (t0, args))
    diagnose_constraint (loc, TREE_OPERAND (orig, 0), t0, args);
  else if (TREE_CODE (orig) == TRUTH_ORIF_EXPR)
    return;
  if (!constraints_satisfied_p (t1, args))
    diagnose_constraint (loc, TREE_OPERAND (orig, 1), t1, args);
}

/* Diagnose a potential expression constraint failure. */

void
diagnose_expression_constraint (location_t loc, tree orig, tree cur, tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;
  if (elide_constraint_failure_p())
    return;

  tree expr = EXPR_CONSTR_EXPR (orig);
  inform (loc, "the required expression %qE would be ill-formed", expr);

  // TODO: We should have a flag that controls this substitution.
  // I'm finding it very useful for resolving concept check errors.

  // inform (input_location, "==== BEGIN DUMP ====");
  // tsubst_expr (EXPR_CONSTR_EXPR (orig), args, tf_warning_or_error, NULL_TREE, false);
  // inform (input_location, "==== END DUMP ====");
}

/* Diagnose a potentially failed type constraint. */

void
diagnose_type_constraint (location_t loc, tree orig, tree cur, tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;
  if (elide_constraint_failure_p())
    return;

  tree type = TYPE_CONSTR_TYPE (orig);
  inform (loc, "the required type %qT would be ill-formed", type);
}

/* Diagnose a potentially unsatisfied conversion constraint. */

void
diagnose_implicit_conversion_constraint (location_t loc, tree orig, tree cur,
					 tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;

  /* The expression and type will previously have been substituted into,
     and therefore may already be an error. Also, we will have already
     diagnosed substitution failures into an expression since this must be
     part of a compound requirement.  */
  tree expr = ICONV_CONSTR_EXPR (cur);
  if (error_operand_p (expr))
    return;

  /* Don't elide a previously diagnosed failure.  */
  if (elide_constraint_failure_p())
    return;

  tree type = ICONV_CONSTR_TYPE (cur);
  if (error_operand_p (type))
    {
      inform (loc, "substitution into type %qT failed",
	      ICONV_CONSTR_TYPE (orig));
      return;
    }

  inform(loc, "%qE is not implicitly convertible to %qT", expr, type);
}

/* Diagnose an argument deduction constraint. */

void
diagnose_argument_deduction_constraint (location_t loc, tree orig, tree cur,
					tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;

  /* The expression and type will previously have been substituted into,
     and therefore may already be an error. Also, we will have already
     diagnosed substution failures into an expression since this must be
     part of a compound requirement.  */
  tree expr = DEDUCT_CONSTR_EXPR (cur);
  if (error_operand_p (expr))
    return;

  /* Don't elide a previously diagnosed failure.  */
  if (elide_constraint_failure_p ())
    return;

  tree pattern = DEDUCT_CONSTR_PATTERN (cur);
  if (error_operand_p (pattern))
    {
      inform (loc, "substitution into type %qT failed",
	      DEDUCT_CONSTR_PATTERN (orig));
      return;
    }

  inform (loc, "unable to deduce placeholder type %qT from %qE",
	  pattern, expr);
}

/* Diagnose an exception constraint. */

void
diagnose_exception_constraint (location_t loc, tree orig, tree cur, tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;
  if (elide_constraint_failure_p ())
    return;

  /* Rebuild a noexcept expression. */
  tree expr = EXCEPT_CONSTR_EXPR (cur);
  if (error_operand_p (expr))
    return;

  inform (loc, "%qE evaluated to false", EXCEPT_CONSTR_EXPR (orig));
}

/* Diagnose a potentially unsatisfied parameterized constraint.  */

void
diagnose_parameterized_constraint (location_t loc, tree orig, tree cur,
				   tree args)
{
  if (constraints_satisfied_p (cur, args))
    return;

  local_specialization_stack stack;
  tree parms = PARM_CONSTR_PARMS (cur);
  tree vars = tsubst_constraint_variables (parms, args, tf_warning_or_error,
					   NULL_TREE);
  if (vars == error_mark_node)
    {
      if (elide_constraint_failure_p ())
        return;

      /* TODO: Check which variable failed and use orig to diagnose
         that substitution error.  */
      inform (loc, "failed to instantiate constraint variables");
      return;
    }

  /* TODO: It would be better write these in a list. */
  while (vars)
    {
      inform (loc, "    with %q#D", vars);
      vars = TREE_CHAIN (vars);
    }
  orig = PARM_CONSTR_OPERAND (orig);
  cur = PARM_CONSTR_OPERAND (cur);
  return diagnose_constraint (loc, orig, cur, args);
}

/* Diagnose the constraint CUR for the given ARGS. This is only ever invoked
   on the associated constraints, so we can only have conjunctions of
   predicate constraints.  The ORIGinal (dependent) constructs follow
   the current constraints to enable better diagnostics.  Note that ORIG
   and CUR must be the same kinds of node, except when CUR is an error.  */

void
diagnose_constraint (location_t loc, tree orig, tree cur, tree args)
{
  switch (TREE_CODE (cur))
    {
    case EXPR_CONSTR:
      diagnose_expression_constraint (loc, orig, cur, args);
      break;

    case TYPE_CONSTR:
      diagnose_type_constraint (loc, orig, cur, args);
      break;

    case ICONV_CONSTR:
      diagnose_implicit_conversion_constraint (loc, orig, cur, args);
      break;

    case DEDUCT_CONSTR:
      diagnose_argument_deduction_constraint (loc, orig, cur, args);
      break;

    case EXCEPT_CONSTR:
      diagnose_exception_constraint (loc, orig, cur, args);
      break;

    case CONJ_CONSTR:
    case DISJ_CONSTR:
      diagnose_logical_constraint (loc, orig, cur, args);
      break;

    case PRED_CONSTR:
      diagnose_predicate_constraint (loc, orig, cur, args);
      break;

    case PARM_CONSTR:
      diagnose_parameterized_constraint (loc, orig, cur, args);
      break;

    case CHECK_CONSTR:
      diagnose_check_constraint (loc, orig, cur, args);
      break;

    case EXPR_PACK_EXPANSION:
      diagnose_pack_expansion (loc, orig, cur, args);
      break;

    case ERROR_MARK:
      /* TODO: Can we improve the diagnostic with the original?  */
      inform (input_location, "ill-formed constraint");
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

  /* Recursively diagnose the associated constraints.  */
  tree ci = get_constraints (decl);
  tree t = CI_ASSOCIATED_CONSTRAINTS (ci);
  diagnose_constraint (loc, t, t, args);
}

} // namespace

/* Emit diagnostics detailing the failure ARGS to satisfy the
   constraints of T. Here, T can be either a constraint
   or a declaration.  */

void
diagnose_constraints (location_t loc, tree t, tree args)
{
  constraint_errors = 0;

  if (constraint_p (t))
    diagnose_constraint (loc, t, t, args);
  else if (DECL_P (t))
    diagnose_declaration_constraints (loc, t, args);
  else
    gcc_unreachable ();

  /* Note the number of elided failures. */
  int n = undiagnosed_constraint_failures ();
  if (n > 0)
    inform (loc, "... and %d more constraint errors not shown", n);
}
