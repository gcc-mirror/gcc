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

} // namespace Rust
