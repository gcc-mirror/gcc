// Copyright (C) 2020-2022 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "attribs.h"
#include "escaped_string.h"

namespace Rust {

void
mark_exp_read (tree exp)
{
  if (exp == NULL)
    return;

  switch (TREE_CODE (exp))
    {
    case VAR_DECL:
      gcc_fallthrough ();
    case PARM_DECL:
      DECL_READ_P (exp) = 1;
      break;
    case ARRAY_REF:
    case COMPONENT_REF:
    case MODIFY_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    CASE_CONVERT:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case FLOAT_EXPR:
    case NON_DEPENDENT_EXPR:
    case VIEW_CONVERT_EXPR:
      mark_exp_read (TREE_OPERAND (exp, 0));
      break;
    case COMPOUND_EXPR:
      mark_exp_read (TREE_OPERAND (exp, 1));
      break;
    case COND_EXPR:
      if (TREE_OPERAND (exp, 1))
	mark_exp_read (TREE_OPERAND (exp, 1));
      if (TREE_OPERAND (exp, 2))
	mark_exp_read (TREE_OPERAND (exp, 2));
      break;
    default:
      break;
    }
}

tree
convert_from_reference (tree val)
{
  if (TREE_TYPE (val) && TYPE_REF_P (TREE_TYPE (val)))
    {
      tree t = TREE_TYPE (TREE_TYPE (val));
      tree ref = build1 (INDIRECT_REF, t, val);

      mark_exp_read (val);

      TREE_SIDE_EFFECTS (ref)
	= (TREE_THIS_VOLATILE (ref) || TREE_SIDE_EFFECTS (val));
      val = ref;
    }

  return val;
}

tree
mark_use (tree expr, bool rvalue_p, bool read_p,
	  location_t loc /* = UNKNOWN_LOCATION */,
	  bool reject_builtin /* = true */)
{
#define RECUR(t) mark_use ((t), rvalue_p, read_p, loc, reject_builtin)

  if (expr == NULL_TREE || error_operand_p (expr))
    return expr;

  if (reject_builtin)
    return error_mark_node;

  if (read_p)
    mark_exp_read (expr);

  bool recurse_op[3] = {false, false, false};
  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
    case NON_DEPENDENT_EXPR:
      recurse_op[0] = true;
      break;
    case COMPOUND_EXPR:
      recurse_op[1] = true;
      break;
    case COND_EXPR:
      recurse_op[2] = true;
      if (TREE_OPERAND (expr, 1))
	recurse_op[1] = true;
      break;
    case INDIRECT_REF:
      if (REFERENCE_REF_P (expr))
	{
	  /* Try to look through the reference.  */
	  tree ref = TREE_OPERAND (expr, 0);
	  tree r = mark_rvalue_use (ref, loc, reject_builtin);
	  if (r != ref)
	    expr = convert_from_reference (r);
	}
      break;

    case VIEW_CONVERT_EXPR:
      if (location_wrapper_p (expr))
	{
	  loc = EXPR_LOCATION (expr);
	  tree op = TREE_OPERAND (expr, 0);
	  tree nop = RECUR (op);
	  if (nop == error_mark_node)
	    return error_mark_node;
	  else if (op == nop)
	    /* No change.  */;
	  else if (DECL_P (nop) || CONSTANT_CLASS_P (nop))
	    {
	      /* Reuse the location wrapper.  */
	      TREE_OPERAND (expr, 0) = nop;
	      /* If we're replacing a DECL with a constant, we also need to
		 change the TREE_CODE of the location wrapper.  */
	      if (rvalue_p)
		TREE_SET_CODE (expr, NON_LVALUE_EXPR);
	    }
	  else
	    {
	      /* Drop the location wrapper.  */
	      expr = nop;
	      protected_set_expr_location (expr, loc);
	    }
	  return expr;
	}
      gcc_fallthrough ();
    CASE_CONVERT:
      recurse_op[0] = true;
      break;

    default:
      break;
    }

  for (int i = 0; i < 3; ++i)
    if (recurse_op[i])
      {
	tree op = TREE_OPERAND (expr, i);
	op = RECUR (op);
	if (op == error_mark_node)
	  return error_mark_node;
	TREE_OPERAND (expr, i) = op;
      }

  return expr;
#undef RECUR
}

tree
mark_rvalue_use (tree e, location_t loc /* = UNKNOWN_LOCATION */,
		 bool reject_builtin /* = true */)
{
  return mark_use (e, true, true, loc, reject_builtin);
}

tree
mark_lvalue_use (tree expr)
{
  return mark_use (expr, false, true, input_location, false);
}

tree
mark_lvalue_use_nonread (tree expr)
{
  return mark_use (expr, false, false, input_location, false);
}

tree
mark_discarded_use (tree expr)
{
  if (expr == NULL_TREE)
    return expr;

  STRIP_ANY_LOCATION_WRAPPER (expr);

  switch (TREE_CODE (expr))
    {
    case COND_EXPR:
      TREE_OPERAND (expr, 2) = mark_discarded_use (TREE_OPERAND (expr, 2));
      gcc_fallthrough ();
    case COMPOUND_EXPR:
      TREE_OPERAND (expr, 1) = mark_discarded_use (TREE_OPERAND (expr, 1));
      return expr;

    case COMPONENT_REF:
    case ARRAY_REF:
    case INDIRECT_REF:
    case MEMBER_REF:
      break;
    default:
      if (DECL_P (expr))
	break;
      else
	return expr;
    }

  return mark_use (expr, true, true, input_location, false);
}

tree
convert_to_void (tree expr, impl_conv_void implicit)
{
  location_t loc = expr_loc_or_input_loc (expr);
  if (expr == error_mark_node || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  expr = mark_discarded_use (expr);
  if (implicit == ICV_CAST)
    /* An explicit cast to void avoids all -Wunused-but-set* warnings.  */
    mark_exp_read (expr);

  if (!TREE_TYPE (expr))
    return expr;

  if (VOID_TYPE_P (TREE_TYPE (expr)))
    return expr;
  switch (TREE_CODE (expr))
    {
      case COND_EXPR: {
	/* The two parts of a cond expr might be separate lvalues.  */
	tree op1 = TREE_OPERAND (expr, 1);
	tree op2 = TREE_OPERAND (expr, 2);
	bool side_effects
	  = ((op1 && TREE_SIDE_EFFECTS (op1)) || TREE_SIDE_EFFECTS (op2));
	tree new_op1, new_op2;
	new_op1 = NULL_TREE;
	if (implicit != ICV_CAST && !side_effects)
	  {
	    if (op1)
	      new_op1 = convert_to_void (op1, ICV_SECOND_OF_COND);
	    new_op2 = convert_to_void (op2, ICV_THIRD_OF_COND);
	  }
	else
	  {
	    if (op1)
	      new_op1 = convert_to_void (op1, ICV_CAST);
	    new_op2 = convert_to_void (op2, ICV_CAST);
	  }

	expr = build3_loc (loc, COND_EXPR, TREE_TYPE (new_op2),
			   TREE_OPERAND (expr, 0), new_op1, new_op2);
	break;
      }

      case COMPOUND_EXPR: {
	/* The second part of a compound expr contains the value.  */
	tree op1 = TREE_OPERAND (expr, 1);
	tree new_op1;
	if (implicit != ICV_CAST
	    && !warning_suppressed_p (expr /* What warning? */))
	  new_op1 = convert_to_void (op1, ICV_RIGHT_OF_COMMA);
	else
	  new_op1 = convert_to_void (op1, ICV_CAST);

	if (new_op1 != op1)
	  {
	    tree t = build2_loc (loc, COMPOUND_EXPR, TREE_TYPE (new_op1),
				 TREE_OPERAND (expr, 0), new_op1);
	    expr = t;
	  }

	break;
      }

    case NON_LVALUE_EXPR:
    case NOP_EXPR:
      /* These have already decayed to rvalue.  */
      break;

    case CALL_EXPR:
      maybe_warn_nodiscard (expr, implicit);
      break;

      case INDIRECT_REF: {
	tree type = TREE_TYPE (expr);
	int is_reference = TYPE_REF_P (TREE_TYPE (TREE_OPERAND (expr, 0)));
	int is_volatile = TYPE_VOLATILE (type);
	int is_complete = COMPLETE_TYPE_P (type);

	/* Can't load the value if we don't know the type.  */
	if (is_volatile && !is_complete)
	  {
	    switch (implicit)
	      {
	      case ICV_CAST:
		warning_at (loc, 0,
			    "conversion to void will not access "
			    "object of incomplete type %qT",
			    type);
		break;
	      case ICV_SECOND_OF_COND:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "incomplete type %qT in second operand "
			    "of conditional expression",
			    type);
		break;
	      case ICV_THIRD_OF_COND:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "incomplete type %qT in third operand "
			    "of conditional expression",
			    type);
		break;
	      case ICV_RIGHT_OF_COMMA:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "incomplete type %qT in right operand of "
			    "comma operator",
			    type);
		break;
	      case ICV_LEFT_OF_COMMA:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "incomplete type %qT in left operand of "
			    "comma operator",
			    type);
		break;
	      case ICV_STATEMENT:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "incomplete type %qT in statement",
			    type);
		break;
	      case ICV_THIRD_IN_FOR:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "incomplete type %qT in for increment "
			    "expression",
			    type);
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
	/* Don't load the value if this is an implicit dereference, or if
	   the type needs to be handled by ctors/dtors.  */
	else if (is_volatile && is_reference)
	  {
	    switch (implicit)
	      {
	      case ICV_CAST:
		warning_at (loc, 0,
			    "conversion to void will not access "
			    "object of type %qT",
			    type);
		break;
	      case ICV_SECOND_OF_COND:
		warning_at (loc, 0,
			    "implicit dereference will not access "
			    "object of type %qT in second operand of "
			    "conditional expression",
			    type);
		break;
	      case ICV_THIRD_OF_COND:
		warning_at (loc, 0,
			    "implicit dereference will not access "
			    "object of type %qT in third operand of "
			    "conditional expression",
			    type);
		break;
	      case ICV_RIGHT_OF_COMMA:
		warning_at (loc, 0,
			    "implicit dereference will not access "
			    "object of type %qT in right operand of "
			    "comma operator",
			    type);
		break;
	      case ICV_LEFT_OF_COMMA:
		warning_at (loc, 0,
			    "implicit dereference will not access "
			    "object of type %qT in left operand of comma "
			    "operator",
			    type);
		break;
	      case ICV_STATEMENT:
		warning_at (loc, 0,
			    "implicit dereference will not access "
			    "object of type %qT in statement",
			    type);
		break;
	      case ICV_THIRD_IN_FOR:
		warning_at (loc, 0,
			    "implicit dereference will not access "
			    "object of type %qT in for increment expression",
			    type);
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
	else if (is_volatile && TREE_ADDRESSABLE (type))
	  {
	    switch (implicit)
	      {
	      case ICV_CAST:
		warning_at (loc, 0,
			    "conversion to void will not access "
			    "object of non-trivially-copyable type %qT",
			    type);
		break;
	      case ICV_SECOND_OF_COND:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "non-trivially-copyable type %qT in second "
			    "operand of conditional expression",
			    type);
		break;
	      case ICV_THIRD_OF_COND:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "non-trivially-copyable type %qT in third "
			    "operand of conditional expression",
			    type);
		break;
	      case ICV_RIGHT_OF_COMMA:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "non-trivially-copyable type %qT in right "
			    "operand of comma operator",
			    type);
		break;
	      case ICV_LEFT_OF_COMMA:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "non-trivially-copyable type %qT in left "
			    "operand of comma operator",
			    type);
		break;
	      case ICV_STATEMENT:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "non-trivially-copyable type %qT in statement",
			    type);
		break;
	      case ICV_THIRD_IN_FOR:
		warning_at (loc, 0,
			    "indirection will not access object of "
			    "non-trivially-copyable type %qT in for "
			    "increment expression",
			    type);
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
	if (is_reference || !is_volatile || !is_complete
	    || TREE_ADDRESSABLE (type))
	  {
	    /* Emit a warning (if enabled) when the "effect-less" INDIRECT_REF
	       operation is stripped off. Note that we don't warn about
	       - an expression with TREE_NO_WARNING set. (For an example of
		 such expressions, see build_over_call in call.cc.)
	       - automatic dereferencing of references, since the user cannot
		 control it. (See also warn_if_unused_value() in c-common.cc.)
	     */
	    if (warn_unused_value && implicit != ICV_CAST
		&& !warning_suppressed_p (expr, OPT_Wunused_value)
		&& !is_reference)
	      warning_at (loc, OPT_Wunused_value, "value computed is not used");
	    expr = TREE_OPERAND (expr, 0);
	    if (TREE_CODE (expr) == CALL_EXPR)
	      maybe_warn_nodiscard (expr, implicit);
	  }

	break;
      }

      case VAR_DECL: {
	/* External variables might be incomplete.  */
	tree type = TREE_TYPE (expr);
	int is_complete = COMPLETE_TYPE_P (type);

	if (TYPE_VOLATILE (type) && !is_complete)
	  switch (implicit)
	    {
	    case ICV_CAST:
	      warning_at (loc, 0,
			  "conversion to void will not access "
			  "object %qE of incomplete type %qT",
			  expr, type);
	      break;
	    case ICV_SECOND_OF_COND:
	      warning_at (loc, 0,
			  "variable %qE of incomplete type %qT will "
			  "not be accessed in second operand of "
			  "conditional expression",
			  expr, type);
	      break;
	    case ICV_THIRD_OF_COND:
	      warning_at (loc, 0,
			  "variable %qE of incomplete type %qT will "
			  "not be accessed in third operand of "
			  "conditional expression",
			  expr, type);
	      break;
	    case ICV_RIGHT_OF_COMMA:
	      warning_at (loc, 0,
			  "variable %qE of incomplete type %qT will "
			  "not be accessed in right operand of comma operator",
			  expr, type);
	      break;
	    case ICV_LEFT_OF_COMMA:
	      warning_at (loc, 0,
			  "variable %qE of incomplete type %qT will "
			  "not be accessed in left operand of comma operator",
			  expr, type);
	      break;
	    case ICV_STATEMENT:
	      warning_at (loc, 0,
			  "variable %qE of incomplete type %qT will "
			  "not be accessed in statement",
			  expr, type);
	      break;
	    case ICV_THIRD_IN_FOR:
	      warning_at (loc, 0,
			  "variable %qE of incomplete type %qT will "
			  "not be accessed in for increment expression",
			  expr, type);
	      break;
	    default:
	      gcc_unreachable ();
	    }

	break;
      }

    default:;
    }

  if (!TREE_SIDE_EFFECTS (expr))
    expr = void_node;

  return expr;
}

void
maybe_warn_nodiscard (tree expr, impl_conv_void implicit)
{
  tree call = expr;
  if (TREE_CODE (expr) == TARGET_EXPR)
    call = TARGET_EXPR_INITIAL (expr);

  location_t loc = expr_loc_or_input_loc (call);
  tree callee = CALL_EXPR_FN (call);
  if (!callee)
    return;

  tree type = TREE_TYPE (callee);
  if (INDIRECT_TYPE_P (type))
    type = TREE_TYPE (type);

  tree rettype = TREE_TYPE (type);
  tree fn = get_fndecl_from_callee (callee);
  tree attr;
  if (implicit != ICV_CAST && fn
      && (attr = lookup_attribute ("nodiscard", DECL_ATTRIBUTES (fn))))
    {
      escaped_string msg;
      tree args = TREE_VALUE (attr);
      if (args)
	msg.escape (TREE_STRING_POINTER (TREE_VALUE (args)));
      const char *format
	= (msg ? G_ ("ignoring return value of %qD, that must be used: %<%s%>")
	       : G_ ("ignoring return value of %qD, that must be used"));
      const char *raw_msg = msg ? (const char *) msg : "";
      auto_diagnostic_group d;
      if (warning_at (loc, OPT_Wunused_result, format, fn, raw_msg))
	inform (DECL_SOURCE_LOCATION (fn), "declared here");
    }
  else if (implicit != ICV_CAST
	   && (attr
	       = lookup_attribute ("nodiscard", TYPE_ATTRIBUTES (rettype))))
    {
      escaped_string msg;
      tree args = TREE_VALUE (attr);
      if (args)
	msg.escape (TREE_STRING_POINTER (TREE_VALUE (args)));
      const char *format
	= (msg ? G_ (
	     "ignoring returned value of type %qT, that must be used: %<%s%>")
	       : G_ ("ignoring returned value of type %qT, that must be used"));
      const char *raw_msg = msg ? (const char *) msg : "";
      auto_diagnostic_group d;
      if (warning_at (loc, OPT_Wunused_result, format, rettype, raw_msg))
	{
	  if (fn)
	    inform (DECL_SOURCE_LOCATION (fn), "in call to %qD, declared here",
		    fn);
	  inform (DECL_SOURCE_LOCATION (TYPE_NAME (rettype)),
		  "%qT declared here", rettype);
	}
    }
}

location_t
expr_loc_or_loc (const_tree t, location_t or_loc)
{
  location_t loc = EXPR_LOCATION (t);
  if (loc == UNKNOWN_LOCATION)
    loc = or_loc;
  return loc;
}

location_t
expr_loc_or_input_loc (const_tree t)
{
  return expr_loc_or_loc (t, input_location);
}

// FN is the callee of a CALL_EXPR or AGGR_INIT_EXPR; return the FUNCTION_DECL
// if we can.
tree
get_fndecl_from_callee (tree fn)
{
  if (fn == NULL_TREE)
    return fn;
  if (TREE_CODE (fn) == FUNCTION_DECL)
    return fn;
  tree type = TREE_TYPE (fn);
  if (type == NULL_TREE || !INDIRECT_TYPE_P (type))
    return NULL_TREE;

  STRIP_NOPS (fn);
  if (TREE_CODE (fn) == ADDR_EXPR || TREE_CODE (fn) == FDESC_EXPR)
    fn = TREE_OPERAND (fn, 0);
  if (TREE_CODE (fn) == FUNCTION_DECL)
    return fn;
  return NULL_TREE;
}

tree
pointer_offset_expression (tree base_tree, tree index_tree, location_t location)
{
  tree element_type_tree = TREE_TYPE (TREE_TYPE (base_tree));
  if (base_tree == error_mark_node || TREE_TYPE (base_tree) == error_mark_node
      || index_tree == error_mark_node || element_type_tree == error_mark_node)
    return error_mark_node;

  tree element_size = TYPE_SIZE_UNIT (element_type_tree);
  index_tree = fold_convert_loc (location, sizetype, index_tree);
  tree offset
    = fold_build2_loc (location, MULT_EXPR, sizetype, index_tree, element_size);

  return fold_build2_loc (location, POINTER_PLUS_EXPR, TREE_TYPE (base_tree),
			  base_tree, offset);
}

// forked from gcc/cp/tree.cc cp_walk_subtrees
/* Apply FUNC to all language-specific sub-trees of TP in a pre-order
   traversal.  Called from walk_tree.  */

tree
rs_walk_subtrees (tree *tp, int *walk_subtrees_p, walk_tree_fn func, void *data,
		  hash_set<tree> *pset)
{
  enum tree_code code = TREE_CODE (*tp);
  tree result;

#define WALK_SUBTREE(NODE)                                                     \
  do                                                                           \
    {                                                                          \
      result = rs_walk_tree (&(NODE), func, data, pset);                       \
      if (result)                                                              \
	goto out;                                                              \
    }                                                                          \
  while (0)

  if (TYPE_P (*tp))
    {
      /* If *WALK_SUBTREES_P is 1, we're interested in the syntactic form of
	 the argument, so don't look through typedefs, but do walk into
	 template arguments for alias templates (and non-typedefed classes).

	 If *WALK_SUBTREES_P > 1, we're interested in type identity or
	 equivalence, so look through typedefs, ignoring template arguments for
	 alias templates, and walk into template args of classes.

	 See find_abi_tags_r for an example of setting *WALK_SUBTREES_P to 2
	 when that's the behavior the walk_tree_fn wants.  */
      if (*walk_subtrees_p == 1 && typedef_variant_p (*tp))
	{
	  *walk_subtrees_p = 0;
	  return NULL_TREE;
	}
    }

  /* Not one of the easy cases.  We must explicitly go through the
     children.  */
  result = NULL_TREE;
  switch (code)
    {
    case TREE_LIST:
      WALK_SUBTREE (TREE_PURPOSE (*tp));
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (*tp))
	WALK_SUBTREE (TYPE_PTRMEMFUNC_FN_TYPE_RAW (*tp));
      break;

    case CONSTRUCTOR:
      if (COMPOUND_LITERAL_P (*tp))
	WALK_SUBTREE (TREE_TYPE (*tp));
      break;

    case DECL_EXPR:
      /* User variables should be mentioned in BIND_EXPR_VARS
	 and their initializers and sizes walked when walking
	 the containing BIND_EXPR.  Compiler temporaries are
	 handled here.  And also normal variables in templates,
	 since do_poplevel doesn't build a BIND_EXPR then.  */
      if (VAR_P (TREE_OPERAND (*tp, 0))
	  && (DECL_ARTIFICIAL (TREE_OPERAND (*tp, 0))
	      && !TREE_STATIC (TREE_OPERAND (*tp, 0))))
	{
	  tree decl = TREE_OPERAND (*tp, 0);
	  WALK_SUBTREE (DECL_INITIAL (decl));
	  WALK_SUBTREE (DECL_SIZE (decl));
	  WALK_SUBTREE (DECL_SIZE_UNIT (decl));
	}
      break;

    default:
      return NULL_TREE;
    }

  /* We didn't find what we were looking for.  */
out:
  return result;

#undef WALK_SUBTREE
}

// forked from gcc/cp/tree.cc cp_expr_location

/* Like EXPR_LOCATION, but also handle some tcc_exceptional that have
   locations.  */

location_t
rs_expr_location (const_tree t_)
{
  tree t = CONST_CAST_TREE (t_);
  if (t == NULL_TREE)
    return UNKNOWN_LOCATION;

  return EXPR_LOCATION (t);
}

// forked from gcc/cp/class.cc is_really_empty_class

/* Returns true if TYPE contains no actual data, just various
   possible combinations of empty classes.  If IGNORE_VPTR is true,
   a vptr doesn't prevent the class from being considered empty.  Typically
   we want to ignore the vptr on assignment, and not on initialization.  */

bool
is_really_empty_class (tree type, bool ignore_vptr)
{
  if (CLASS_TYPE_P (type))
    {
      tree field;
      tree binfo;
      tree base_binfo;
      int i;

      /* CLASSTYPE_EMPTY_P isn't set properly until the class is actually laid
	 out, but we'd like to be able to check this before then.  */
      if (COMPLETE_TYPE_P (type) && is_empty_class (type))
	return true;

      if (!ignore_vptr && TYPE_CONTAINS_VPTR_P (type))
	return false;

      for (binfo = TYPE_BINFO (type), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
	if (!is_really_empty_class (BINFO_TYPE (base_binfo), ignore_vptr))
	  return false;
      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && !DECL_ARTIFICIAL (field)
	    /* An unnamed bit-field is not a data member.  */
	    && !DECL_UNNAMED_BIT_FIELD (field)
	    && !is_really_empty_class (TREE_TYPE (field), ignore_vptr))
	  return false;
      return true;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    return (integer_zerop (array_type_nelts_top (type))
	    || is_really_empty_class (TREE_TYPE (type), ignore_vptr));
  return false;
}

// forked from gcc/cp/class.cc is_empty_class

/* Returns 1 if TYPE contains only padding bytes.  */

int
is_empty_class (tree type)
{
  if (type == error_mark_node)
    return 0;

  if (!CLASS_TYPE_P (type))
    return 0;

  return CLASSTYPE_EMPTY_P (type);
}

// forked from gcc/cp/tree.cc array_type_nelts_top

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This counts only elements of the top
   array.  */

tree
array_type_nelts_top (tree type)
{
  return fold_build2_loc (input_location, PLUS_EXPR, sizetype,
			  array_type_nelts (type), size_one_node);
}

// forked from gcc/cp/tree.cc builtin_valid_in_constant_expr_p

/* Test whether DECL is a builtin that may appear in a
   constant-expression. */

bool
builtin_valid_in_constant_expr_p (const_tree decl)
{
  STRIP_ANY_LOCATION_WRAPPER (decl);
  if (TREE_CODE (decl) != FUNCTION_DECL)
    /* Not a function.  */
    return false;
  if (DECL_BUILT_IN_CLASS (decl) != BUILT_IN_NORMAL)
    {
      if (fndecl_built_in_p (decl, BUILT_IN_FRONTEND))
	switch (DECL_FE_FUNCTION_CODE (decl))
	  {
	  case RS_BUILT_IN_IS_CONSTANT_EVALUATED:
	  case RS_BUILT_IN_SOURCE_LOCATION:
	  case RS_BUILT_IN_IS_CORRESPONDING_MEMBER:
	  case RS_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS:
	    return true;
	  default:
	    break;
	  }
      /* Not a built-in.  */
      return false;
    }
  switch (DECL_FUNCTION_CODE (decl))
    {
      /* These always have constant results like the corresponding
	 macros/symbol.  */
    case BUILT_IN_FILE:
    case BUILT_IN_FUNCTION:
    case BUILT_IN_LINE:

      /* The following built-ins are valid in constant expressions
	 when their arguments are.  */
    case BUILT_IN_ADD_OVERFLOW_P:
    case BUILT_IN_SUB_OVERFLOW_P:
    case BUILT_IN_MUL_OVERFLOW_P:

      /* These have constant results even if their operands are
	 non-constant.  */
    case BUILT_IN_CONSTANT_P:
    case BUILT_IN_ATOMIC_ALWAYS_LOCK_FREE:
      return true;
    default:
      return false;
    }
}

// forked from gcc/cp/decl2.cc decl_maybe_constant_var_p

/* Returns true if DECL could be a symbolic constant variable, depending on
   its initializer.  */

bool
decl_maybe_constant_var_p (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (!VAR_P (decl))
    return false;
  if (DECL_DECLARED_CONSTEXPR_P (decl))
    return true;
  if (DECL_HAS_VALUE_EXPR_P (decl))
    /* A proxy isn't constant.  */
    return false;
  if (TYPE_REF_P (type))
    /* References can be constant.  */;
  else if (RS_TYPE_CONST_NON_VOLATILE_P (type)
	   && INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    /* And const integers.  */;
  else
    return false;

  if (DECL_INITIAL (decl) && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl))
    /* We know the initializer, and it isn't constant.  */
    return false;
  else
    return true;
}

// forked from gcc/cp/typeck.cc cp_type_quals

/* Returns the type qualifiers for this type, including the qualifiers on the
   elements for an array type.  */

int
rs_type_quals (const_tree type)
{
  int quals;
  /* This CONST_CAST is okay because strip_array_types returns its
     argument unmodified and we assign it to a const_tree.  */
  type = strip_array_types (CONST_CAST_TREE (type));
  if (type == error_mark_node
      /* Quals on a FUNCTION_TYPE are memfn quals.  */
      || TREE_CODE (type) == FUNCTION_TYPE)
    return TYPE_UNQUALIFIED;
  quals = TYPE_QUALS (type);
  /* METHOD and REFERENCE_TYPEs should never have quals.  */
  gcc_assert (
    (TREE_CODE (type) != METHOD_TYPE && !TYPE_REF_P (type))
    || ((quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)) == TYPE_UNQUALIFIED));
  return quals;
}

// forked from gcc/cp/decl.cc cp_global_trees

/* The following symbols are subsumed in the cp_global_trees array, and
   listed here individually for documentation purposes.

   C++ extensions
	tree wchar_decl_node;

	tree vtable_entry_type;
	tree delta_type_node;
	tree __t_desc_type_node;

	tree class_type_node;
	tree unknown_type_node;

   Array type `vtable_entry_type[]'

	tree vtbl_type_node;
	tree vtbl_ptr_type_node;

   Namespaces,

	tree std_node;
	tree abi_node;

   A FUNCTION_DECL which can call `abort'.  Not necessarily the
   one that the user will declare, but sufficient to be called
   by routines that want to abort the program.

	tree abort_fndecl;

   Used by RTTI
	tree type_info_type_node, tinfo_decl_id, tinfo_decl_type;
	tree tinfo_var_id;  */

tree cp_global_trees[CPTI_MAX];

// forked from gcc/cp/module.cc fixed_trees

static GTY (()) vec<tree, va_gc> *fixed_trees;

// forked from gcc/cp/module.cc maybe_add_global

/* VAL is a global tree, add it to the global vec if it is
   interesting.  Add some of its targets, if they too are
   interesting.  We do not add identifiers, as they can be re-found
   via the identifier hash table.  There is a cost to the number of
   global trees.  */

static int
maybe_add_global (tree val, unsigned &crc)
{
  int v = 0;

  if (val && !(TREE_CODE (val) == IDENTIFIER_NODE || TREE_VISITED (val)))
    {
      TREE_VISITED (val) = true;
      crc = crc32_unsigned (crc, fixed_trees->length ());
      vec_safe_push (fixed_trees, val);
      v++;

      if (CODE_CONTAINS_STRUCT (TREE_CODE (val), TS_TYPED))
	v += maybe_add_global (TREE_TYPE (val), crc);
      if (CODE_CONTAINS_STRUCT (TREE_CODE (val), TS_TYPE_COMMON))
	v += maybe_add_global (TYPE_NAME (val), crc);
    }

  return v;
}

// forked from gcc/cp/module.cc global_tree_arys

/* Global trees.  */
static const std::pair<tree *, unsigned> global_tree_arys[] = {
  std::pair<tree *, unsigned> (cp_global_trees, CPTI_MODULE_HWM),
};

// forked from gcc/cp/module.cc init_modules

void
init_modules ()
{
  unsigned crc = 0;
  vec_alloc (fixed_trees, 200);

  const tree *ptr = global_tree_arys[0].first;
  unsigned limit = global_tree_arys[0].second;
  for (unsigned ix = 0; ix != limit; ix++, ptr++)
    {
      maybe_add_global (*ptr, crc);
    }
}

// forked from gcc/cp/constexpr.cc var_in_constexpr_fn

/* True if T was declared in a function declared to be constexpr, and
   therefore potentially constant in C++14.  */

bool
var_in_constexpr_fn (tree t)
{
  tree ctx = DECL_CONTEXT (t);
  return (ctx && TREE_CODE (ctx) == FUNCTION_DECL
	  && DECL_DECLARED_CONSTEXPR_P (ctx));
}

// forked from gcc/cp/name-lookup.cc member_vec_linear_search

/* Linear search of (unordered) MEMBER_VEC for NAME.  */

static tree
member_vec_linear_search (vec<tree, va_gc> *member_vec, tree name)
{
  for (int ix = member_vec->length (); ix--;)
    if (tree binding = (*member_vec)[ix])
      if (OVL_NAME (binding) == name)
	return binding;

  return NULL_TREE;
}

// forked from gcc/cp/name-lookup.cc member_vec_binary_search

/* Binary search of (ordered) MEMBER_VEC for NAME.  */

static tree
member_vec_binary_search (vec<tree, va_gc> *member_vec, tree name)
{
  for (unsigned lo = 0, hi = member_vec->length (); lo < hi;)
    {
      unsigned mid = (lo + hi) / 2;
      tree binding = (*member_vec)[mid];
      tree binding_name = OVL_NAME (binding);

      if (binding_name > name)
	hi = mid;
      else if (binding_name < name)
	lo = mid + 1;
      else
	return binding;
    }

  return NULL_TREE;
}

// forked from gcc/cp/tree.cc is_overloaded_fn

/* Returns nonzero if X is an expression for a (possibly overloaded)
   function.  If "f" is a function or function template, "f", "c->f",
   "c.f", "C::f", and "f<int>" will all be considered possibly
   overloaded functions.  Returns 2 if the function is actually
   overloaded, i.e., if it is impossible to know the type of the
   function without performing overload resolution.  */

int
is_overloaded_fn (tree x)
{
  STRIP_ANY_LOCATION_WRAPPER (x);

  if (TREE_CODE (x) == COMPONENT_REF)
    x = TREE_OPERAND (x, 1);

  if ((TREE_CODE (x) == OVERLOAD && !OVL_SINGLE_P (x)))
    return 2;

  return OVL_P (x);
}

// forked from gcc/cp/tree.cc ovl_make

/* Make a raw overload node containing FN.  */

tree
ovl_make (tree fn, tree next)
{
  tree result = make_node (OVERLOAD);

  if (TREE_CODE (fn) == OVERLOAD)
    OVL_NESTED_P (result) = true;

  TREE_TYPE (result)
    = (next || TREE_CODE (fn) == TEMPLATE_DECL ? unknown_type_node
					       : TREE_TYPE (fn));
  if (next && TREE_CODE (next) == OVERLOAD && OVL_DEDUP_P (next))
    OVL_DEDUP_P (result) = true;
  OVL_FUNCTION (result) = fn;
  OVL_CHAIN (result) = next;
  return result;
}

// forked from gcc/cp/name-lookup.cc lookup_add

/* Add a set of new FNS into a lookup.  */

tree
lookup_add (tree fns, tree lookup)
{
  if (fns == error_mark_node || lookup == error_mark_node)
    return error_mark_node;

  if (lookup || TREE_CODE (fns) == TEMPLATE_DECL)
    {
      lookup = ovl_make (fns, lookup);
      OVL_LOOKUP_P (lookup) = true;
    }
  else
    lookup = fns;

  return lookup;
}
} // namespace Rust
