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
#include "libiberty.h"
#include "stor-layout.h"
#include "hash-map.h"
#include "diagnostic.h"
#include "timevar.h"
#include "convert.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "function.h"
#include "gcc-rich-location.h"

// forked from gcc/c-family/c-common.cc c_global_trees
tree c_global_trees[CTI_MAX];
// forked from gcc/cp/decl.cc cp_global_trees
tree cp_global_trees[CPTI_MAX];

struct saved_scope *scope_chain;

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

/* The following symbols are subsumed in the c_global_trees array, and
   listed here individually for documentation purposes.

   INTEGER_TYPE and REAL_TYPE nodes for the standard data types.

	tree short_integer_type_node;
	tree long_integer_type_node;
	tree long_long_integer_type_node;

	tree short_unsigned_type_node;
	tree long_unsigned_type_node;
	tree long_long_unsigned_type_node;

	tree truthvalue_type_node;
	tree truthvalue_false_node;
	tree truthvalue_true_node;

	tree ptrdiff_type_node;

	tree unsigned_char_type_node;
	tree signed_char_type_node;
	tree wchar_type_node;

	tree char8_type_node;
	tree char16_type_node;
	tree char32_type_node;

	tree float_type_node;
	tree double_type_node;
	tree long_double_type_node;

	tree complex_integer_type_node;
	tree complex_float_type_node;
	tree complex_double_type_node;
	tree complex_long_double_type_node;

	tree dfloat32_type_node;
	tree dfloat64_type_node;
	tree_dfloat128_type_node;

	tree intQI_type_node;
	tree intHI_type_node;
	tree intSI_type_node;
	tree intDI_type_node;
	tree intTI_type_node;

	tree unsigned_intQI_type_node;
	tree unsigned_intHI_type_node;
	tree unsigned_intSI_type_node;
	tree unsigned_intDI_type_node;
	tree unsigned_intTI_type_node;

	tree widest_integer_literal_type_node;
	tree widest_unsigned_literal_type_node;

   Nodes for types `void *' and `const void *'.

	tree ptr_type_node, const_ptr_type_node;

   Nodes for types `char *' and `const char *'.

	tree string_type_node, const_string_type_node;

   Type `char[SOMENUMBER]'.
   Used when an array of char is needed and the size is irrelevant.

	tree char_array_type_node;

   Type `wchar_t[SOMENUMBER]' or something like it.
   Used when a wide string literal is created.

	tree wchar_array_type_node;

   Type `char8_t[SOMENUMBER]' or something like it.
   Used when a UTF-8 string literal is created.

	tree char8_array_type_node;

   Type `char16_t[SOMENUMBER]' or something like it.
   Used when a UTF-16 string literal is created.

	tree char16_array_type_node;

   Type `char32_t[SOMENUMBER]' or something like it.
   Used when a UTF-32 string literal is created.

	tree char32_array_type_node;

   Type `int ()' -- used for implicit declaration of functions.

	tree default_function_type;

   A VOID_TYPE node, packaged in a TREE_LIST.

	tree void_list_node;

  The lazily created VAR_DECLs for __FUNCTION__, __PRETTY_FUNCTION__,
  and __func__. (C doesn't generate __FUNCTION__ and__PRETTY_FUNCTION__
  VAR_DECLS, but C++ does.)

	tree function_name_decl_node;
	tree pretty_function_name_decl_node;
	tree c99_function_name_decl_node;

  Stack of nested function name VAR_DECLs.

	tree saved_function_name_decls;

*/

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
  std::pair<tree *, unsigned> (c_global_trees, CTI_MODULE_HWM),
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

  ptr = global_tree_arys[1].first;
  limit = global_tree_arys[1].second;
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

  TREE_TYPE (result) = (next ? unknown_type_node : TREE_TYPE (fn));
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

  lookup = fns;

  return lookup;
}

// forked from gcc/cp/typeck.cc type_memfn_quals

/* Returns the function-cv-quals for TYPE, which must be a FUNCTION_TYPE or
   METHOD_TYPE.  */

int
type_memfn_quals (const_tree type)
{
  if (TREE_CODE (type) == FUNCTION_TYPE)
    return TYPE_QUALS (type);
  else if (TREE_CODE (type) == METHOD_TYPE)
    return rs_type_quals (class_of_this_parm (type));
  else
    gcc_unreachable ();
}

// forked from gcc/cp/pt.cc find_parameter_pack_data

/* Structure used to track the progress of find_parameter_packs_r.  */
struct find_parameter_pack_data
{
  /* TREE_LIST that will contain all of the parameter packs found by
     the traversal.  */
  tree *parameter_packs;

  /* Set of AST nodes that have been visited by the traversal.  */
  hash_set<tree> *visited;

  /* True iff we're making a type pack expansion.  */
  bool type_pack_expansion_p;

  /* True iff we found a subtree that has the extra args mechanism.  */
  bool found_extra_args_tree_p = false;
};

// forked from gcc/cp/lex.cc conv_type_hasher

/* Hasher for the conversion operator name hash table.  */
struct conv_type_hasher : ggc_ptr_hash<tree_node>
{
  /* Hash NODE, an identifier node in the table.  TYPE_UID is
     suitable, as we're not concerned about matching canonicalness
     here.  */
  static hashval_t hash (tree node)
  {
    return (hashval_t) TYPE_UID (TREE_TYPE (node));
  }

  /* Compare NODE, an identifier node in the table, against TYPE, an
     incoming TYPE being looked up.  */
  static bool equal (tree node, tree type) { return TREE_TYPE (node) == type; }
};

static GTY (()) hash_table<conv_type_hasher> *conv_type_names;

// forked from gcc/cp/lex.cc make_conv_op_name

/* Return an identifier for a conversion operator to TYPE.  We can get
   from the returned identifier to the type.  We store TYPE, which is
   not necessarily the canonical type,  which allows us to report the
   form the user used in error messages.  All these identifiers are
   not in the identifier hash table, and have the same string name.
   These IDENTIFIERS are not in the identifier hash table, and all
   have the same IDENTIFIER_STRING.  */

tree
make_conv_op_name (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (conv_type_names == NULL)
    conv_type_names = hash_table<conv_type_hasher>::create_ggc (31);

  tree *slot
    = conv_type_names->find_slot_with_hash (type, (hashval_t) TYPE_UID (type),
					    INSERT);
  tree identifier = *slot;
  if (!identifier)
    {
      /* Create a raw IDENTIFIER outside of the identifier hash
	 table.  */
      identifier = copy_node (conv_op_identifier);

      /* Just in case something managed to bind.  */
      IDENTIFIER_BINDING (identifier) = NULL;

      /* Hang TYPE off the identifier so it can be found easily later
	 when performing conversions.  */
      TREE_TYPE (identifier) = type;

      *slot = identifier;
    }

  return identifier;
}

// forked from gcc/cp/pt.cc builtin_pack_fn_p

/* True iff FN is a function representing a built-in variadic parameter
   pack.  */

bool
builtin_pack_fn_p (tree fn)
{
  if (!fn || TREE_CODE (fn) != FUNCTION_DECL
      || !DECL_IS_UNDECLARED_BUILTIN (fn))
    return false;

  if (id_equal (DECL_NAME (fn), "__integer_pack"))
    return true;

  return false;
}

// forked from gcc/cp/pt.cc builtin_pack_call_p

/* True iff CALL is a call to a function representing a built-in variadic
   parameter pack.  */

static bool
builtin_pack_call_p (tree call)
{
  if (TREE_CODE (call) != CALL_EXPR)
    return false;
  return builtin_pack_fn_p (CALL_EXPR_FN (call));
}

// forked from gcc/cp/pt.cc has_extra_args_mechanism_p

/* Return true if the tree T has the extra args mechanism for
   avoiding partial instantiation.  */

static bool
has_extra_args_mechanism_p (const_tree t)
{
  return false;
}

// forked from gcc/cp/pt.cc find_parameter_packs_r

/* Identifies all of the argument packs that occur in a template
   argument and appends them to the TREE_LIST inside DATA, which is a
   find_parameter_pack_data structure. This is a subroutine of
   make_pack_expansion and uses_parameter_packs.  */
static tree
find_parameter_packs_r (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;
  struct find_parameter_pack_data *ppd
    = (struct find_parameter_pack_data *) data;
  bool parameter_pack_p = false;

#define WALK_SUBTREE(NODE)                                                     \
  rs_walk_tree (&(NODE), &find_parameter_packs_r, ppd, ppd->visited)

  /* Don't look through typedefs; we are interested in whether a
     parameter pack is actually written in the expression/type we're
     looking at, not the target type.  */
  if (TYPE_P (t) && typedef_variant_p (t))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  /* Identify whether this is a parameter pack or not.  */
  switch (TREE_CODE (t))
    {
    case FIELD_DECL:
    case PARM_DECL:
      break;

    case VAR_DECL:
      break;

    case CALL_EXPR:
      if (builtin_pack_call_p (t))
	parameter_pack_p = true;
      break;

    case BASES:
      parameter_pack_p = true;
      break;
    default:
      /* Not a parameter pack.  */
      break;
    }

  if (parameter_pack_p)
    {
      /* Add this parameter pack to the list.  */
      *ppd->parameter_packs = tree_cons (NULL_TREE, t, *ppd->parameter_packs);
    }

  if (TYPE_P (t))
    rs_walk_tree (&TYPE_CONTEXT (t), &find_parameter_packs_r, ppd,
		  ppd->visited);

  /* This switch statement will return immediately if we don't find a
     parameter pack.  ??? Should some of these be in cp_walk_subtrees?  */
  switch (TREE_CODE (t))
    {
      case DECL_EXPR: {
	tree decl = DECL_EXPR_DECL (t);
	if (is_typedef_decl (decl))
	  /* Since we stop at typedefs above, we need to look through them at
	     the point of the DECL_EXPR.  */
	  rs_walk_tree (&DECL_ORIGINAL_TYPE (decl), &find_parameter_packs_r,
			ppd, ppd->visited);
	return NULL_TREE;
      }

    case INTEGER_TYPE:
      rs_walk_tree (&TYPE_MAX_VALUE (t), &find_parameter_packs_r, ppd,
		    ppd->visited);
      *walk_subtrees = 0;
      return NULL_TREE;

    case IDENTIFIER_NODE:
      rs_walk_tree (&TREE_TYPE (t), &find_parameter_packs_r, ppd, ppd->visited);
      *walk_subtrees = 0;
      return NULL_TREE;

      case DECLTYPE_TYPE: {
	/* When traversing a DECLTYPE_TYPE_EXPR, we need to set
	   type_pack_expansion_p to false so that any placeholders
	   within the expression don't get marked as parameter packs.  */
	bool type_pack_expansion_p = ppd->type_pack_expansion_p;
	ppd->type_pack_expansion_p = false;
	rs_walk_tree (&DECLTYPE_TYPE_EXPR (t), &find_parameter_packs_r, ppd,
		      ppd->visited);
	ppd->type_pack_expansion_p = type_pack_expansion_p;
	*walk_subtrees = 0;
	return NULL_TREE;
      }

    case IF_STMT:
      rs_walk_tree (&IF_COND (t), &find_parameter_packs_r, ppd, ppd->visited);
      rs_walk_tree (&THEN_CLAUSE (t), &find_parameter_packs_r, ppd,
		    ppd->visited);
      rs_walk_tree (&ELSE_CLAUSE (t), &find_parameter_packs_r, ppd,
		    ppd->visited);
      /* Don't walk into IF_STMT_EXTRA_ARGS.  */
      *walk_subtrees = 0;
      return NULL_TREE;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_RAISES_EXCEPTIONS (t));
      break;

    default:
      return NULL_TREE;
    }

#undef WALK_SUBTREE

  return NULL_TREE;
}

// forked from gcc/cp/typeck.cc type_memfn_rqual

/* Returns the function-ref-qualifier for TYPE */

rs_ref_qualifier
type_memfn_rqual (const_tree type)
{
  gcc_assert (FUNC_OR_METHOD_TYPE_P (type));

  if (!FUNCTION_REF_QUALIFIED (type))
    return REF_QUAL_NONE;
  else if (FUNCTION_RVALUE_QUALIFIED (type))
    return REF_QUAL_RVALUE;
  else
    return REF_QUAL_LVALUE;
}

// forked from gcc/cp/lex.cc maybe_add_lang_type_raw

/* Add a raw lang_type to T, a type, should it need one.  */

bool
maybe_add_lang_type_raw (tree t)
{
  if (!RECORD_OR_UNION_CODE_P (TREE_CODE (t)))
    return false;

  auto *lt = (struct lang_type *) (ggc_internal_cleared_alloc (
    sizeof (struct lang_type)));
  TYPE_LANG_SPECIFIC (t) = lt;

  if (GATHER_STATISTICS)
    {
      tree_node_counts[(int) lang_type] += 1;
      tree_node_sizes[(int) lang_type] += sizeof (struct lang_type);
    }

  return true;
}

// forked from gcc/c-family/c-lex.cc get_fileinfo

static splay_tree file_info_tree;

struct c_fileinfo *
get_fileinfo (const char *name)
{
  splay_tree_node n;
  struct c_fileinfo *fi;

  if (!file_info_tree)
    file_info_tree = splay_tree_new (splay_tree_compare_strings, 0,
				     splay_tree_delete_pointers);

  n = splay_tree_lookup (file_info_tree, (splay_tree_key) name);
  if (n)
    return (struct c_fileinfo *) n->value;

  fi = XNEW (struct c_fileinfo);
  fi->time = 0;
  fi->interface_only = 0;
  fi->interface_unknown = 1;
  splay_tree_insert (file_info_tree, (splay_tree_key) name,
		     (splay_tree_value) fi);
  return fi;
}

// forked from gcc/cp/lex.cc cxx_make_type

tree
cxx_make_type (enum tree_code code MEM_STAT_DECL)
{
  tree t = make_node (code PASS_MEM_STAT);

  if (maybe_add_lang_type_raw (t))
    {
      /* Set up some flags that give proper default behavior.  */
      struct c_fileinfo *finfo = get_fileinfo (LOCATION_FILE (input_location));
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, finfo->interface_unknown);
      CLASSTYPE_INTERFACE_ONLY (t) = finfo->interface_only;
    }

  if (code == RECORD_TYPE || code == UNION_TYPE)
    TYPE_CXX_ODR_P (t) = 1;

  return t;
}

// forked from gcc/cp/tree.cc build_min_array_type

/* Build an ARRAY_TYPE without laying it out.  */

static tree
build_min_array_type (tree elt_type, tree index_type)
{
  tree t = cxx_make_type (ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;
  return t;
}

// forked from gcc/cp/name-lookup.cc fields_linear_search

/* Linear search of (partially ordered) fields of KLASS for NAME.  */

static tree
fields_linear_search (tree klass, tree name, bool want_type)
{
  for (tree fields = TYPE_FIELDS (klass); fields; fields = DECL_CHAIN (fields))
    {
      tree decl = fields;

      if (DECL_NAME (decl) != name)
	continue;

      if (DECL_DECLARES_FUNCTION_P (decl))
	/* Functions are found separately.  */
	continue;

      if (!want_type || DECL_DECLARES_TYPE_P (decl))
	return decl;
    }
}

// forked from gcc/cp/except.cc canonnothrow_spec_pical_eh_spec

/* Return true iff SPEC is throw() or noexcept(true).  */

bool
nothrow_spec_p (const_tree spec)
{
  if (spec == empty_except_spec || spec == noexcept_true_spec)
    return true;

  gcc_assert (!spec || TREE_VALUE (spec) || spec == noexcept_false_spec
	      || TREE_PURPOSE (spec) == error_mark_node);

  return false;
}

// forked from gcc/cp/tree.cc may_get_fns

/* Get the overload set FROM refers to.  Returns NULL if it's not an
   overload set.  */

tree
maybe_get_fns (tree from)
{
  STRIP_ANY_LOCATION_WRAPPER (from);

  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (from) == COMPONENT_REF)
    from = TREE_OPERAND (from, 1);

  if (OVL_P (from))
    return from;

  return NULL;
}

// forked from gcc/cp/tree.cc get_fns

/* FROM refers to an overload set.  Return that set (or die).  */

tree
get_fns (tree from)
{
  tree res = maybe_get_fns (from);

  gcc_assert (res);
  return res;
}

// forked from gcc/cp/tree.cc get_first_fn

/* Return the first function of the overload set FROM refers to.  */

tree
get_first_fn (tree from)
{
  return OVL_FIRST (get_fns (from));
}

// forked from gcc/cp/tree.cc dependent_name

/* X is the CALL_EXPR_FN of a CALL_EXPR.  If X represents a dependent name
   (14.6.2), return the IDENTIFIER_NODE for that name.  Otherwise, return
   NULL_TREE.  */

tree
dependent_name (tree x)
{
  /* FIXME a dependent name must be unqualified, but this function doesn't
     distinguish between qualified and unqualified identifiers.  */
  if (identifier_p (x))
    return x;

  if (OVL_P (x))
    return OVL_NAME (x);
  return NULL_TREE;
}

// forked from gcc/cp/tree.cc called_fns_equal

/* Subroutine of rs_tree_equal: t1 and t2 are the CALL_EXPR_FNs of two
   CALL_EXPRS.  Return whether they are equivalent.  */

static bool
called_fns_equal (tree t1, tree t2)
{
  /* Core 1321: dependent names are equivalent even if the overload sets
     are different.  But do compare explicit template arguments.  */
  tree name1 = dependent_name (t1);
  tree name2 = dependent_name (t2);
  if (name1 || name2)
    {
      tree targs1 = NULL_TREE, targs2 = NULL_TREE;

      if (name1 != name2)
	return false;

      /* FIXME dependent_name currently returns an unqualified name regardless
	 of whether the function was named with a qualified- or unqualified-id.
	 Until that's fixed, check that we aren't looking at overload sets from
	 different scopes.  */
      if (is_overloaded_fn (t1) && is_overloaded_fn (t2)
	  && (DECL_CONTEXT (get_first_fn (t1))
	      != DECL_CONTEXT (get_first_fn (t2))))
	return false;

      return rs_tree_equal (targs1, targs2);
    }
  else
    return rs_tree_equal (t1, t2);
}

// forked from gcc/cp/tree.cc canonical_eh_spec

/* Return the canonical version of exception-specification RAISES for a C++17
   function type, for use in type comparison and building TYPE_CANONICAL.  */

tree
canonical_eh_spec (tree raises)
{
  if (raises == NULL_TREE)
    return raises;
  else if (nothrow_spec_p (raises))
    /* throw() -> noexcept.  */
    return noexcept_true_spec;
  else
    /* For C++17 type matching, anything else -> nothing.  */
    return NULL_TREE;
}

/* Like cp_tree_operand_length, but takes a tree_code CODE.  */

int
rs_tree_code_length (enum tree_code code)
{
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  switch (code)
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      return 1;

    case ARRAY_REF:
      return 2;

    default:
      return TREE_CODE_LENGTH (code);
    }
}

// forked from gcc/cp/tree.cc rs_tree_operand_length

/* Return the number of operands in T that we care about for things like
   mangling.  */

int
rs_tree_operand_length (const_tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (TREE_CODE_CLASS (code) == tcc_vl_exp)
    return VL_EXP_OPERAND_LENGTH (t);

  return rs_tree_code_length (code);
}

// forked from gcc/cp/tree.cc cp_tree_equal

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same. Return 0 if they are different.  */

bool
rs_tree_equal (tree t1, tree t2)
{
  enum tree_code code1, code2;

  if (t1 == t2)
    return true;
  if (!t1 || !t2)
    return false;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  if (code1 != code2)
    return false;

  if (CONSTANT_CLASS_P (t1) && !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return false;

  switch (code1)
    {
    case VOID_CST:
      /* There's only a single VOID_CST node, so we should never reach
	 here.  */
      gcc_unreachable ();

    case INTEGER_CST:
      return tree_int_cst_equal (t1, t2);

    case REAL_CST:
      return real_identical (&TREE_REAL_CST (t1), &TREE_REAL_CST (t2));

    case STRING_CST:
      return TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	     && !memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
			 TREE_STRING_LENGTH (t1));

    case FIXED_CST:
      return FIXED_VALUES_IDENTICAL (TREE_FIXED_CST (t1), TREE_FIXED_CST (t2));

    case COMPLEX_CST:
      return rs_tree_equal (TREE_REALPART (t1), TREE_REALPART (t2))
	     && rs_tree_equal (TREE_IMAGPART (t1), TREE_IMAGPART (t2));

    case VECTOR_CST:
      return operand_equal_p (t1, t2, OEP_ONLY_CONST);

    case CONSTRUCTOR:
      /* We need to do this when determining whether or not two
	 non-type pointer to member function template arguments
	 are the same.  */
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2))
	  || CONSTRUCTOR_NELTS (t1) != CONSTRUCTOR_NELTS (t2))
	return false;
      {
	tree field, value;
	unsigned int i;
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t1), i, field, value)
	  {
	    constructor_elt *elt2 = CONSTRUCTOR_ELT (t2, i);
	    if (!rs_tree_equal (field, elt2->index)
		|| !rs_tree_equal (value, elt2->value))
	      return false;
	  }
      }
      return true;

    case TREE_LIST:
      if (!rs_tree_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2)))
	return false;
      if (!rs_tree_equal (TREE_VALUE (t1), TREE_VALUE (t2)))
	return false;
      return rs_tree_equal (TREE_CHAIN (t1), TREE_CHAIN (t2));

    case SAVE_EXPR:
      return rs_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

      case CALL_EXPR: {
	if (KOENIG_LOOKUP_P (t1) != KOENIG_LOOKUP_P (t2))
	  return false;

	if (!called_fns_equal (CALL_EXPR_FN (t1), CALL_EXPR_FN (t2)))
	  return false;

	call_expr_arg_iterator iter1, iter2;
	init_call_expr_arg_iterator (t1, &iter1);
	init_call_expr_arg_iterator (t2, &iter2);
	if (iter1.n != iter2.n)
	  return false;

	while (more_call_expr_args_p (&iter1))
	  {
	    tree arg1 = next_call_expr_arg (&iter1);
	    tree arg2 = next_call_expr_arg (&iter2);

	    gcc_checking_assert (arg1 && arg2);
	    if (!rs_tree_equal (arg1, arg2))
	      return false;
	  }

	return true;
      }

      case TARGET_EXPR: {
	tree o1 = TREE_OPERAND (t1, 0);
	tree o2 = TREE_OPERAND (t2, 0);

	/* Special case: if either target is an unallocated VAR_DECL,
	   it means that it's going to be unified with whatever the
	   TARGET_EXPR is really supposed to initialize, so treat it
	   as being equivalent to anything.  */
	if (VAR_P (o1) && DECL_NAME (o1) == NULL_TREE && !DECL_RTL_SET_P (o1))
	  /*Nop*/;
	else if (VAR_P (o2) && DECL_NAME (o2) == NULL_TREE
		 && !DECL_RTL_SET_P (o2))
	  /*Nop*/;
	else if (!rs_tree_equal (o1, o2))
	  return false;

	return rs_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));
      }

    case PARM_DECL:
      /* For comparing uses of parameters in late-specified return types
	 with an out-of-class definition of the function, but can also come
	 up for expressions that involve 'this' in a member function
	 template.  */

      if (same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	{
	  if (DECL_ARTIFICIAL (t1) ^ DECL_ARTIFICIAL (t2))
	    return false;
	  if (CONSTRAINT_VAR_P (t1) ^ CONSTRAINT_VAR_P (t2))
	    return false;
	  if (DECL_ARTIFICIAL (t1)
	      || (DECL_PARM_LEVEL (t1) == DECL_PARM_LEVEL (t2)
		  && DECL_PARM_INDEX (t1) == DECL_PARM_INDEX (t2)))
	    return true;
	}
      return false;

    case VAR_DECL:
    case CONST_DECL:
    case FIELD_DECL:
    case FUNCTION_DECL:
    case IDENTIFIER_NODE:
    case SSA_NAME:
      return false;

    case TREE_VEC:
      return true;

    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      /* Used for location wrappers with possibly NULL types.  */
      if (!TREE_TYPE (t1) || !TREE_TYPE (t2))
	{
	  if (TREE_TYPE (t1) || TREE_TYPE (t2))
	    return false;
	  break;
	}

    default:
      break;
    }

  switch (TREE_CODE_CLASS (code1))
    {
    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
    case tcc_vl_exp:
    case tcc_reference:
      case tcc_statement: {
	int n = rs_tree_operand_length (t1);
	if (TREE_CODE_CLASS (code1) == tcc_vl_exp
	    && n != TREE_OPERAND_LENGTH (t2))
	  return false;

	for (int i = 0; i < n; ++i)
	  if (!rs_tree_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i)))
	    return false;

	return true;
      }

    case tcc_type:
      return same_type_p (t1, t2);

    default:
      gcc_unreachable ();
    }

  /* We can get here with --disable-checking.  */
  return false;
}

// forked from gcc/cp/class.cc publicly_uniquely_derived_p

/* TRUE iff TYPE is publicly & uniquely derived from PARENT.  */

bool
publicly_uniquely_derived_p (tree parent, tree type)
{
  return false;
}

// forked from gcc/cp/typeck.cc comp_except_types

/* Compare two exception specifier types for exactness or subsetness, if
   allowed. Returns false for mismatch, true for match (same, or
   derived and !exact).

   [except.spec] "If a class X ... objects of class X or any class publicly
   and unambiguously derived from X. Similarly, if a pointer type Y * ...
   exceptions of type Y * or that are pointers to any type publicly and
   unambiguously derived from Y. Otherwise a function only allows exceptions
   that have the same type ..."
   This does not mention cv qualifiers and is different to what throw
   [except.throw] and catch [except.catch] will do. They will ignore the
   top level cv qualifiers, and allow qualifiers in the pointer to class
   example.

   We implement the letter of the standard.  */

static bool
comp_except_types (tree a, tree b, bool exact)
{
  if (same_type_p (a, b))
    return true;
  else if (!exact)
    {
      if (rs_type_quals (a) || rs_type_quals (b))
	return false;

      if (TYPE_PTR_P (a) && TYPE_PTR_P (b))
	{
	  a = TREE_TYPE (a);
	  b = TREE_TYPE (b);
	  if (rs_type_quals (a) || rs_type_quals (b))
	    return false;
	}

      if (TREE_CODE (a) != RECORD_TYPE || TREE_CODE (b) != RECORD_TYPE)
	return false;

      if (publicly_uniquely_derived_p (a, b))
	return true;
    }
  return false;
}

// forked from gcc/cp/typeck.cc comp_except_specs

/* Return true if TYPE1 and TYPE2 are equivalent exception specifiers.
   If EXACT is ce_derived, T2 can be stricter than T1 (according to 15.4/5).
   If EXACT is ce_type, the C++17 type compatibility rules apply.
   If EXACT is ce_normal, the compatibility rules in 15.4/3 apply.
   If EXACT is ce_exact, the specs must be exactly the same. Exception lists
   are unordered, but we've already filtered out duplicates. Most lists will
   be in order, we should try to make use of that.  */

bool
comp_except_specs (const_tree t1, const_tree t2, int exact)
{
  const_tree probe;
  const_tree base;
  int length = 0;

  if (t1 == t2)
    return true;

  /* First handle noexcept.  */
  if (exact < ce_exact)
    {
      if (exact == ce_type
	  && (canonical_eh_spec (CONST_CAST_TREE (t1))
	      == canonical_eh_spec (CONST_CAST_TREE (t2))))
	return true;

      /* noexcept(false) is compatible with no exception-specification,
	 and less strict than any spec.  */
      if (t1 == noexcept_false_spec)
	return t2 == NULL_TREE || exact == ce_derived;
      /* Even a derived noexcept(false) is compatible with no
	 exception-specification.  */
      if (t2 == noexcept_false_spec)
	return t1 == NULL_TREE;

      /* Otherwise, if we aren't looking for an exact match, noexcept is
	 equivalent to throw().  */
      if (t1 == noexcept_true_spec)
	t1 = empty_except_spec;
      if (t2 == noexcept_true_spec)
	t2 = empty_except_spec;
    }

  /* If any noexcept is left, it is only comparable to itself;
     either we're looking for an exact match or we're redeclaring a
     template with dependent noexcept.  */
  if ((t1 && TREE_PURPOSE (t1)) || (t2 && TREE_PURPOSE (t2)))
    return (t1 && t2 && rs_tree_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2)));

  if (t1 == NULL_TREE) /* T1 is ...  */
    return t2 == NULL_TREE || exact == ce_derived;
  if (!TREE_VALUE (t1)) /* t1 is EMPTY */
    return t2 != NULL_TREE && !TREE_VALUE (t2);
  if (t2 == NULL_TREE) /* T2 is ...  */
    return false;
  if (TREE_VALUE (t1) && !TREE_VALUE (t2)) /* T2 is EMPTY, T1 is not */
    return exact == ce_derived;

  /* Neither set is ... or EMPTY, make sure each part of T2 is in T1.
     Count how many we find, to determine exactness. For exact matching and
     ordered T1, T2, this is an O(n) operation, otherwise its worst case is
     O(nm).  */
  for (base = t1; t2 != NULL_TREE; t2 = TREE_CHAIN (t2))
    {
      for (probe = base; probe != NULL_TREE; probe = TREE_CHAIN (probe))
	{
	  tree a = TREE_VALUE (probe);
	  tree b = TREE_VALUE (t2);

	  if (comp_except_types (a, b, exact))
	    {
	      if (probe == base && exact > ce_derived)
		base = TREE_CHAIN (probe);
	      length++;
	      break;
	    }
	}
      if (probe == NULL_TREE)
	return false;
    }
  return exact == ce_derived || base == NULL_TREE || length == list_length (t1);
}

// forked from gcc/cp/typeck.cc compparms

/* Subroutines of `comptypes'.  */

/* Return true if two parameter type lists PARMS1 and PARMS2 are
   equivalent in the sense that functions with those parameter types
   can have equivalent types.  The two lists must be equivalent,
   element by element.  */

bool
compparms (const_tree parms1, const_tree parms2)
{
  const_tree t1, t2;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  for (t1 = parms1, t2 = parms2; t1 || t2;
       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    {
      /* If one parmlist is shorter than the other,
	 they fail to match.  */
      if (!t1 || !t2)
	return false;
      if (!same_type_p (TREE_VALUE (t1), TREE_VALUE (t2)))
	return false;
    }
  return true;
}

/* Set TYPE_CANONICAL like build_array_type_1, but using
   build_cplus_array_type.  */

static void
set_array_type_canon (tree t, tree elt_type, tree index_type, bool dep)
{
  /* Set the canonical type for this new node.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (elt_type)
      || (index_type && TYPE_STRUCTURAL_EQUALITY_P (index_type)))
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (TYPE_CANONICAL (elt_type) != elt_type
	   || (index_type && TYPE_CANONICAL (index_type) != index_type))
    TYPE_CANONICAL (t)
      = build_cplus_array_type (TYPE_CANONICAL (elt_type),
				index_type ? TYPE_CANONICAL (index_type)
					   : index_type,
				dep);
  else
    TYPE_CANONICAL (t) = t;
}

// forked from gcc/cp/tree.cc cplus_array_info

struct cplus_array_info
{
  tree type;
  tree domain;
};

// forked from gcc/cp/tree.cc cplus_array_hasher

struct cplus_array_hasher : ggc_ptr_hash<tree_node>
{
  typedef cplus_array_info *compare_type;

  static hashval_t hash (tree t);
  static bool equal (tree, cplus_array_info *);
};

/* Hash an ARRAY_TYPE.  K is really of type `tree'.  */

hashval_t
cplus_array_hasher::hash (tree t)
{
  hashval_t hash;

  hash = TYPE_UID (TREE_TYPE (t));
  if (TYPE_DOMAIN (t))
    hash ^= TYPE_UID (TYPE_DOMAIN (t));
  return hash;
}

/* Compare two ARRAY_TYPEs.  K1 is really of type `tree', K2 is really
   of type `cplus_array_info*'. */

bool
cplus_array_hasher::equal (tree t1, cplus_array_info *t2)
{
  return (TREE_TYPE (t1) == t2->type && TYPE_DOMAIN (t1) == t2->domain);
}

// forked from gcc/cp/tree.cc cplus_array_htab

/* Hash table containing dependent array types, which are unsuitable for
   the language-independent type hash table.  */
static GTY (()) hash_table<cplus_array_hasher> *cplus_array_htab;

// forked from gcc/cp/tree.cc is_byte_access_type

/* Returns true if TYPE is char, unsigned char, or std::byte.  */

bool
is_byte_access_type (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  if (type == char_type_node || type == unsigned_char_type_node)
    return true;

  return (TREE_CODE (type) == ENUMERAL_TYPE && TYPE_CONTEXT (type) == std_node
	  && !strcmp ("byte", TYPE_NAME_STRING (type)));
}

// forked from gcc/cp/tree.cc build_cplus_array_type

/* Like build_array_type, but handle special C++ semantics: an array of a
   variant element type is a variant of the array of the main variant of
   the element type.  IS_DEPENDENT is -ve if we should determine the
   dependency.  Otherwise its bool value indicates dependency.  */

tree
build_cplus_array_type (tree elt_type, tree index_type, int dependent)
{
  tree t;

  if (elt_type == error_mark_node || index_type == error_mark_node)
    return error_mark_node;

  if (dependent < 0)
    dependent = 0;

  if (elt_type != TYPE_MAIN_VARIANT (elt_type))
    /* Start with an array of the TYPE_MAIN_VARIANT.  */
    t = build_cplus_array_type (TYPE_MAIN_VARIANT (elt_type), index_type,
				dependent);
  else if (dependent)
    {
      /* Since type_hash_canon calls layout_type, we need to use our own
	 hash table.  */
      cplus_array_info cai;
      hashval_t hash;

      if (cplus_array_htab == NULL)
	cplus_array_htab = hash_table<cplus_array_hasher>::create_ggc (61);

      hash = TYPE_UID (elt_type);
      if (index_type)
	hash ^= TYPE_UID (index_type);
      cai.type = elt_type;
      cai.domain = index_type;

      tree *e = cplus_array_htab->find_slot_with_hash (&cai, hash, INSERT);
      if (*e)
	/* We have found the type: we're done.  */
	return (tree) *e;
      else
	{
	  /* Build a new array type.  */
	  t = build_min_array_type (elt_type, index_type);

	  /* Store it in the hash table. */
	  *e = t;

	  /* Set the canonical type for this new node.  */
	  set_array_type_canon (t, elt_type, index_type, dependent);

	  /* Mark it as dependent now, this saves time later.  */
	  TYPE_DEPENDENT_P_VALID (t) = true;
	  TYPE_DEPENDENT_P (t) = true;
	}
    }
  else
    {
      bool typeless_storage = is_byte_access_type (elt_type);
      t = build_array_type (elt_type, index_type, typeless_storage);

      /* Mark as non-dependenty now, this will save time later.  */
      TYPE_DEPENDENT_P_VALID (t) = true;
    }

  /* Now check whether we already have this array variant.  */
  if (elt_type != TYPE_MAIN_VARIANT (elt_type))
    {
      tree m = t;
      for (t = m; t; t = TYPE_NEXT_VARIANT (t))
	if (TREE_TYPE (t) == elt_type && TYPE_NAME (t) == NULL_TREE
	    && TYPE_ATTRIBUTES (t) == NULL_TREE)
	  break;
      if (!t)
	{
	  t = build_min_array_type (elt_type, index_type);
	  /* Mark dependency now, this saves time later.  */
	  TYPE_DEPENDENT_P_VALID (t) = true;
	  TYPE_DEPENDENT_P (t) = dependent;
	  set_array_type_canon (t, elt_type, index_type, dependent);
	  if (!dependent)
	    {
	      layout_type (t);
	      /* Make sure sizes are shared with the main variant.
		 layout_type can't be called after setting TYPE_NEXT_VARIANT,
		 as it will overwrite alignment etc. of all variants.  */
	      TYPE_SIZE (t) = TYPE_SIZE (m);
	      TYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (m);
	      TYPE_TYPELESS_STORAGE (t) = TYPE_TYPELESS_STORAGE (m);
	    }

	  TYPE_MAIN_VARIANT (t) = m;
	  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
	  TYPE_NEXT_VARIANT (m) = t;
	}
    }

  /* Avoid spurious warnings with VLAs (c++/54583).  */
  if (TYPE_SIZE (t) && EXPR_P (TYPE_SIZE (t)))
    suppress_warning (TYPE_SIZE (t), OPT_Wunused);

  /* Push these needs up to the ARRAY_TYPE so that initialization takes
     place more easily.  */
  bool needs_ctor
    = (TYPE_NEEDS_CONSTRUCTING (t) = TYPE_NEEDS_CONSTRUCTING (elt_type));
  bool needs_dtor = (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
		     = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (elt_type));

  if (!dependent && t == TYPE_MAIN_VARIANT (t) && !COMPLETE_TYPE_P (t)
      && COMPLETE_TYPE_P (elt_type))
    {
      /* The element type has been completed since the last time we saw
	 this array type; update the layout and 'tor flags for any variants
	 that need it.  */
      layout_type (t);
      for (tree v = TYPE_NEXT_VARIANT (t); v; v = TYPE_NEXT_VARIANT (v))
	{
	  TYPE_NEEDS_CONSTRUCTING (v) = needs_ctor;
	  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (v) = needs_dtor;
	}
    }

  return t;
}

// forked from gcc/cp/tree.cc cp_build_qualified_type_real

/* Make a variant of TYPE, qualified with the TYPE_QUALS.  Handles
   arrays correctly.  In particular, if TYPE is an array of T's, and
   TYPE_QUALS is non-empty, returns an array of qualified T's.

   FLAGS determines how to deal with ill-formed qualifications. If
   tf_ignore_bad_quals is set, then bad qualifications are dropped
   (this is permitted if TYPE was introduced via a typedef or template
   type parameter). If bad qualifications are dropped and tf_warning
   is set, then a warning is issued for non-const qualifications.  If
   tf_ignore_bad_quals is not set and tf_error is not set, we
   return error_mark_node. Otherwise, we issue an error, and ignore
   the qualifications.

   Qualification of a reference type is valid when the reference came
   via a typedef or template type argument. [dcl.ref] No such
   dispensation is provided for qualifying a function type.  [dcl.fct]
   DR 295 queries this and the proposed resolution brings it into line
   with qualifying a reference.  We implement the DR.  We also behave
   in a similar manner for restricting non-pointer types.  */

tree
rs_build_qualified_type_real (tree type, int type_quals,
			      tsubst_flags_t complain)
{
  tree result;
  int bad_quals = TYPE_UNQUALIFIED;

  if (type == error_mark_node)
    return type;

  if (type_quals == rs_type_quals (type))
    return type;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* In C++, the qualification really applies to the array element
	 type.  Obtain the appropriately qualified element type.  */
      tree t;
      tree element_type
	= rs_build_qualified_type_real (TREE_TYPE (type), type_quals, complain);

      if (element_type == error_mark_node)
	return error_mark_node;

      /* See if we already have an identically qualified type.  Tests
	 should be equivalent to those in check_qualified_type.  */
      for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	if (TREE_TYPE (t) == element_type && TYPE_NAME (t) == TYPE_NAME (type)
	    && TYPE_CONTEXT (t) == TYPE_CONTEXT (type)
	    && attribute_list_equal (TYPE_ATTRIBUTES (t),
				     TYPE_ATTRIBUTES (type)))
	  break;

      if (!t)
	{
	  /* If we already know the dependentness, tell the array type
	     constructor.  This is important for module streaming, as we cannot
	     dynamically determine that on read in.  */
	  t = build_cplus_array_type (element_type, TYPE_DOMAIN (type),
				      TYPE_DEPENDENT_P_VALID (type)
					? int (TYPE_DEPENDENT_P (type))
					: -1);

	  /* Keep the typedef name.  */
	  if (TYPE_NAME (t) != TYPE_NAME (type))
	    {
	      t = build_variant_type_copy (t);
	      TYPE_NAME (t) = TYPE_NAME (type);
	      SET_TYPE_ALIGN (t, TYPE_ALIGN (type));
	      TYPE_USER_ALIGN (t) = TYPE_USER_ALIGN (type);
	    }
	}

      /* Even if we already had this variant, we update
	 TYPE_NEEDS_CONSTRUCTING and TYPE_HAS_NONTRIVIAL_DESTRUCTOR in case
	 they changed since the variant was originally created.

	 This seems hokey; if there is some way to use a previous
	 variant *without* coming through here,
	 TYPE_NEEDS_CONSTRUCTING will never be updated.  */
      TYPE_NEEDS_CONSTRUCTING (t)
	= TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (element_type));
      TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
	= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TYPE_MAIN_VARIANT (element_type));
      return t;
    }

  /* A reference or method type shall not be cv-qualified.
     [dcl.ref], [dcl.fct].  This used to be an error, but as of DR 295
     (in CD1) we always ignore extra cv-quals on functions.  */

  /* [dcl.ref/1] Cv-qualified references are ill-formed except when
     the cv-qualifiers are introduced through the use of a typedef-name
     ([dcl.typedef], [temp.param]) or decltype-specifier
     ([dcl.type.decltype]),in which case the cv-qualifiers are
     ignored.  */
  if (type_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)
      && (TYPE_REF_P (type) || FUNC_OR_METHOD_TYPE_P (type)))
    {
      if (TYPE_REF_P (type)
	  && (!typedef_variant_p (type) || FUNC_OR_METHOD_TYPE_P (type)))
	bad_quals |= type_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
      type_quals &= ~(TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
    }

  /* But preserve any function-cv-quals on a FUNCTION_TYPE.  */
  if (TREE_CODE (type) == FUNCTION_TYPE)
    type_quals |= type_memfn_quals (type);

  /* A restrict-qualified type must be a pointer (or reference)
     to object or incomplete type. */
  if ((type_quals & TYPE_QUAL_RESTRICT) && TREE_CODE (type) != TYPENAME_TYPE
      && !INDIRECT_TYPE_P (type))
    {
      bad_quals |= TYPE_QUAL_RESTRICT;
      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (bad_quals == TYPE_UNQUALIFIED || (complain & tf_ignore_bad_quals))
    /*OK*/;
  else if (!(complain & tf_error))
    return error_mark_node;
  else
    {
      tree bad_type = build_qualified_type (ptr_type_node, bad_quals);
      error ("%qV qualifiers cannot be applied to %qT", bad_type, type);
    }

  /* Retrieve (or create) the appropriately qualified variant.  */
  result = build_qualified_type (type, type_quals);

  return result;
}

// forked from gcc/cp/c-common.cc vector_targets_convertible_p

/* vector_targets_convertible_p is used for vector pointer types.  The
   callers perform various checks that the qualifiers are satisfactory,
   while OTOH vector_targets_convertible_p ignores the number of elements
   in the vectors.  That's fine with vector pointers as we can consider,
   say, a vector of 8 elements as two consecutive vectors of 4 elements,
   and that does not require and conversion of the pointer values.
   In contrast, vector_types_convertible_p and
   vector_types_compatible_elements_p are used for vector value types.  */
/* True if pointers to distinct types T1 and T2 can be converted to
   each other without an explicit cast.  Only returns true for opaque
   vector types.  */
bool
vector_targets_convertible_p (const_tree t1, const_tree t2)
{
  if (VECTOR_TYPE_P (t1) && VECTOR_TYPE_P (t2)
      && (TYPE_VECTOR_OPAQUE (t1) || TYPE_VECTOR_OPAQUE (t2))
      && tree_int_cst_equal (TYPE_SIZE (t1), TYPE_SIZE (t2)))
    return true;

  return false;
}

// forked from gcc/cp/typeck.cc comp_array_types

/* Compare the array types T1 and T2.  CB says how we should behave when
   comparing array bounds: bounds_none doesn't allow dimensionless arrays,
   bounds_either says than any array can be [], bounds_first means that
   onlt T1 can be an array with unknown bounds.  STRICT is true if
   qualifiers must match when comparing the types of the array elements.  */

static bool
comp_array_types (const_tree t1, const_tree t2, compare_bounds_t cb,
		  bool strict)
{
  tree d1;
  tree d2;
  tree max1, max2;

  if (t1 == t2)
    return true;

  /* The type of the array elements must be the same.  */
  if (strict ? !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2))
	     : !similar_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
    return false;

  d1 = TYPE_DOMAIN (t1);
  d2 = TYPE_DOMAIN (t2);

  if (d1 == d2)
    return true;

  /* If one of the arrays is dimensionless, and the other has a
     dimension, they are of different types.  However, it is valid to
     write:

       extern int a[];
       int a[3];

     by [basic.link]:

       declarations for an array object can specify
       array types that differ by the presence or absence of a major
       array bound (_dcl.array_).  */
  if (!d1 && d2)
    return cb >= bounds_either;
  else if (d1 && !d2)
    return cb == bounds_either;

  /* Check that the dimensions are the same.  */

  if (!rs_tree_equal (TYPE_MIN_VALUE (d1), TYPE_MIN_VALUE (d2)))
    return false;
  max1 = TYPE_MAX_VALUE (d1);
  max2 = TYPE_MAX_VALUE (d2);

  if (!rs_tree_equal (max1, max2))
    return false;

  return true;
}

// forked from gcc/cp/typeck.cc same_type_ignoring_top_level_qualifiers_p

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, ignoring
   top-level qualifiers.  */

bool
same_type_ignoring_top_level_qualifiers_p (tree type1, tree type2)
{
  if (type1 == error_mark_node || type2 == error_mark_node)
    return false;
  if (type1 == type2)
    return true;

  type1 = rs_build_qualified_type (type1, TYPE_UNQUALIFIED);
  type2 = rs_build_qualified_type (type2, TYPE_UNQUALIFIED);
  return same_type_p (type1, type2);
}

// forked from gcc/cp/typeck.cc comp_ptr_ttypes_const

/* Return true if TO and FROM (both of which are POINTER_TYPEs or
   pointer-to-member types) are the same, ignoring cv-qualification at
   all levels.  CB says how we should behave when comparing array bounds.  */

bool
comp_ptr_ttypes_const (tree to, tree from, compare_bounds_t cb)
{
  bool is_opaque_pointer = false;

  for (;; to = TREE_TYPE (to), from = TREE_TYPE (from))
    {
      if (TREE_CODE (to) != TREE_CODE (from))
	return false;

      if (TREE_CODE (from) == OFFSET_TYPE
	  && same_type_p (TYPE_OFFSET_BASETYPE (from),
			  TYPE_OFFSET_BASETYPE (to)))
	continue;

      if (VECTOR_TYPE_P (to))
	is_opaque_pointer = vector_targets_convertible_p (to, from);

      if (TREE_CODE (to) == ARRAY_TYPE
	  /* Ignore cv-qualification, but if we see e.g. int[3] and int[4],
	     we must fail.  */
	  && !comp_array_types (to, from, cb, /*strict=*/false))
	return false;

      /* CWG 330 says we need to look through arrays.  */
      if (!TYPE_PTR_P (to) && TREE_CODE (to) != ARRAY_TYPE)
	return (is_opaque_pointer
		|| same_type_ignoring_top_level_qualifiers_p (to, from));
    }
}

// forked from gcc/cp/typeck.cc similar_type_p

/* Returns nonzero iff TYPE1 and TYPE2 are similar, as per [conv.qual].  */

bool
similar_type_p (tree type1, tree type2)
{
  if (type1 == error_mark_node || type2 == error_mark_node)
    return false;

  /* Informally, two types are similar if, ignoring top-level cv-qualification:
     * they are the same type; or
     * they are both pointers, and the pointed-to types are similar; or
     * they are both pointers to member of the same class, and the types of
       the pointed-to members are similar; or
     * they are both arrays of the same size or both arrays of unknown bound,
       and the array element types are similar.  */

  if (same_type_ignoring_top_level_qualifiers_p (type1, type2))
    return true;

  if ((TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
      || (TYPE_PTRDATAMEM_P (type1) && TYPE_PTRDATAMEM_P (type2))
      || (TREE_CODE (type1) == ARRAY_TYPE && TREE_CODE (type2) == ARRAY_TYPE))
    return comp_ptr_ttypes_const (type1, type2, bounds_either);

  return false;
}

// forked from gcc/cp/typeck.cc structural_comptypes
// note: this fork only handles strict == COMPARE_STRICT
// if you pass in any other value for strict i.e. COMPARE_BASE,
// COMPARE_DERIVED, COMPARE_REDECLARATION or COMPARE_STRUCTURAL
// see the original function in gcc/cp/typeck.cc and port the required bits
// specifically under case UNION_TYPE.

/* Subroutine in comptypes.  */

static bool
structural_comptypes (tree t1, tree t2, int strict)
{
  /* Both should be types that are not obviously the same.  */
  gcc_checking_assert (t1 != t2 && TYPE_P (t1) && TYPE_P (t2));

  if (TYPE_PTRMEMFUNC_P (t1))
    t1 = TYPE_PTRMEMFUNC_FN_TYPE (t1);
  if (TYPE_PTRMEMFUNC_P (t2))
    t2 = TYPE_PTRMEMFUNC_FN_TYPE (t2);

  /* Different classes of types can't be compatible.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  /* Qualifiers must match.  For array types, we will check when we
     recur on the array element types.  */
  if (TREE_CODE (t1) != ARRAY_TYPE && rs_type_quals (t1) != rs_type_quals (t2))
    return false;
  if (TREE_CODE (t1) == FUNCTION_TYPE
      && type_memfn_quals (t1) != type_memfn_quals (t2))
    return false;
  /* Need to check this before TYPE_MAIN_VARIANT.
     FIXME function qualifiers should really change the main variant.  */
  if (FUNC_OR_METHOD_TYPE_P (t1))
    {
      if (type_memfn_rqual (t1) != type_memfn_rqual (t2))
	return false;
      if (/* cxx_dialect >= cxx17 && */
	  !comp_except_specs (TYPE_RAISES_EXCEPTIONS (t1),
			      TYPE_RAISES_EXCEPTIONS (t2), ce_type))
	return false;
    }

  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     qualifiers (just above).  */
  if (TREE_CODE (t1) != ARRAY_TYPE
      && TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return true;

  /* Compare the types.  Return false on known not-same. Break on not
     known.   Never return true from this switch -- you'll break
     specialization comparison.    */
  switch (TREE_CODE (t1))
    {
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      /* All void and bool types are the same.  */
      break;

    case OPAQUE_TYPE:
    case INTEGER_TYPE:
    case FIXED_POINT_TYPE:
    case REAL_TYPE:
      /* With these nodes, we can't determine type equivalence by
	 looking at what is stored in the nodes themselves, because
	 two nodes might have different TYPE_MAIN_VARIANTs but still
	 represent the same type.  For example, wchar_t and int could
	 have the same properties (TYPE_PRECISION, TYPE_MIN_VALUE,
	 TYPE_MAX_VALUE, etc.), but have different TYPE_MAIN_VARIANTs
	 and are distinct types. On the other hand, int and the
	 following typedef

	   typedef int INT __attribute((may_alias));

	 have identical properties, different TYPE_MAIN_VARIANTs, but
	 represent the same type.  The canonical type system keeps
	 track of equivalence in this case, so we fall back on it.  */
      if (TYPE_CANONICAL (t1) != TYPE_CANONICAL (t2))
	return false;

      /* We don't need or want the attribute comparison.  */
      return true;

    case RECORD_TYPE:
    case UNION_TYPE:
      return false;

    case OFFSET_TYPE:
      if (!comptypes (TYPE_OFFSET_BASETYPE (t1), TYPE_OFFSET_BASETYPE (t2),
		      strict & ~COMPARE_REDECLARATION))
	return false;
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case REFERENCE_TYPE:
      if (TYPE_REF_IS_RVALUE (t1) != TYPE_REF_IS_RVALUE (t2))
	return false;
      /* fall through to checks for pointer types */
      gcc_fallthrough ();

    case POINTER_TYPE:
      if (TYPE_MODE (t1) != TYPE_MODE (t2)
	  || !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      /* Exception specs and memfn_rquals were checked above.  */
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      if (!compparms (TYPE_ARG_TYPES (t1), TYPE_ARG_TYPES (t2)))
	return false;
      break;

    case ARRAY_TYPE:
      /* Target types must match incl. qualifiers.  */
      if (!comp_array_types (t1, t2,
			     ((strict & COMPARE_REDECLARATION) ? bounds_either
							       : bounds_none),
			     /*strict=*/true))
	return false;
      break;

    case COMPLEX_TYPE:
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    case VECTOR_TYPE:
      if (gnu_vector_type_p (t1) != gnu_vector_type_p (t2)
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (t1), TYPE_VECTOR_SUBPARTS (t2))
	  || !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      break;

    default:
      return false;
    }

  /* If we get here, we know that from a target independent POV the
     types are the same.  Make sure the target attributes are also
     the same.  */
  if (!comp_type_attributes (t1, t2))
    return false;

  return true;
}

// forked from gcc/cp/typeck.cc comptypes

/* Return true if T1 and T2 are related as allowed by STRICT.  STRICT
   is a bitwise-or of the COMPARE_* flags.  */

bool
comptypes (tree t1, tree t2, int strict)
{
  gcc_checking_assert (t1 && t2);

  /* TYPE_ARGUMENT_PACKS are not really types.  */
  gcc_checking_assert (TREE_CODE (t1) != TYPE_ARGUMENT_PACK
		       && TREE_CODE (t2) != TYPE_ARGUMENT_PACK);

  if (t1 == t2)
    return true;

  /* Suppress errors caused by previously reported errors.  */
  if (t1 == error_mark_node || t2 == error_mark_node)
    return false;

  if (strict == COMPARE_STRICT)
    {
      if (TYPE_STRUCTURAL_EQUALITY_P (t1) || TYPE_STRUCTURAL_EQUALITY_P (t2))
	/* At least one of the types requires structural equality, so
	   perform a deep check. */
	return structural_comptypes (t1, t2, strict);

      if (flag_checking && param_use_canonical_types)
	{
	  bool result = structural_comptypes (t1, t2, strict);

	  if (result && TYPE_CANONICAL (t1) != TYPE_CANONICAL (t2))
	    /* The two types are structurally equivalent, but their
	       canonical types were different. This is a failure of the
	       canonical type propagation code.*/
	    internal_error (
	      "canonical types differ for identical types %qT and %qT", t1, t2);
	  else if (!result && TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2))
	    /* Two types are structurally different, but the canonical
	       types are the same. This means we were over-eager in
	       assigning canonical types. */
	    internal_error (
	      "same canonical type node for different types %qT and %qT", t1,
	      t2);

	  return result;
	}
      if (!flag_checking && param_use_canonical_types)
	return TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2);
      else
	return structural_comptypes (t1, t2, strict);
    }
  else if (strict == COMPARE_STRUCTURAL)
    return structural_comptypes (t1, t2, COMPARE_STRICT);
  else
    return structural_comptypes (t1, t2, strict);
}

// forked from gcc/cp/decl.cc next_initializable_field

/* FIELD is an element of TYPE_FIELDS or NULL.  In the former case, the value
   returned is the next FIELD_DECL (possibly FIELD itself) that can be
   initialized.  If there are no more such fields, the return value
   will be NULL.  */

tree
next_initializable_field (tree field)
{
  while (field
	 && (TREE_CODE (field) != FIELD_DECL || DECL_UNNAMED_BIT_FIELD (field)
	     || (DECL_ARTIFICIAL (field)
		 /* Don't skip vptr fields.  We might see them when we're
		    called from reduced_constant_expression_p.  */
		 && !DECL_VIRTUAL_P (field))))
    field = DECL_CHAIN (field);

  return field;
}

// forked from gcc/cp/call.cc sufficient_parms_p

/* Returns nonzero if PARMLIST consists of only default parms,
   ellipsis, and/or undeduced parameter packs.  */

bool
sufficient_parms_p (const_tree parmlist)
{
  for (; parmlist && parmlist != void_list_node;
       parmlist = TREE_CHAIN (parmlist))
    if (!TREE_PURPOSE (parmlist))
      return false;
  return true;
}

// forked from gcc/cp/class.cc default_ctor_p

/* Returns true if FN is a default constructor.  */

bool
default_ctor_p (const_tree fn)
{
  return (DECL_CONSTRUCTOR_P (fn)
	  && sufficient_parms_p (FUNCTION_FIRST_USER_PARMTYPE (fn)));
}

// forked from gcc/cp/class.cc user_provided_p

/* Returns true iff FN is a user-provided function, i.e. user-declared
   and not defaulted at its first declaration.  */

bool
user_provided_p (tree fn)
{
  return (!DECL_ARTIFICIAL (fn)
	  && !(DECL_INITIALIZED_IN_CLASS_P (fn)
	       && (DECL_DEFAULTED_FN (fn) || DECL_DELETED_FN (fn))));
}

// forked from gcc/cp/class.cc type_has_non_user_provided_default_constructor

/* Returns true iff class T has a non-user-provided (i.e. implicitly
   declared or explicitly defaulted in the class body) default
   constructor.  */

bool
type_has_non_user_provided_default_constructor (tree t)
{
  if (!TYPE_HAS_DEFAULT_CONSTRUCTOR (t))
    return false;
  if (CLASSTYPE_LAZY_DEFAULT_CTOR (t))
    return true;

  for (ovl_iterator iter (CLASSTYPE_CONSTRUCTORS (t)); iter; ++iter)
    {
      tree fn = *iter;
      if (TREE_CODE (fn) == FUNCTION_DECL && default_ctor_p (fn)
	  && !user_provided_p (fn))
	return true;
    }

  return false;
}

// forked from gcc/cp/class.cc default_init_uninitialized_part

/* If default-initialization leaves part of TYPE uninitialized, returns
   a DECL for the field or TYPE itself (DR 253).  */

tree
default_init_uninitialized_part (tree type)
{
  tree t, r, binfo;
  int i;

  type = strip_array_types (type);
  if (!CLASS_TYPE_P (type))
    return type;
  if (!type_has_non_user_provided_default_constructor (type))
    return NULL_TREE;
  for (binfo = TYPE_BINFO (type), i = 0; BINFO_BASE_ITERATE (binfo, i, t); ++i)
    {
      r = default_init_uninitialized_part (BINFO_TYPE (t));
      if (r)
	return r;
    }
  for (t = next_initializable_field (TYPE_FIELDS (type)); t;
       t = next_initializable_field (DECL_CHAIN (t)))
    if (!DECL_INITIAL (t) && !DECL_ARTIFICIAL (t))
      {
	r = default_init_uninitialized_part (TREE_TYPE (t));
	if (r)
	  return DECL_P (r) ? r : t;
      }

  return NULL_TREE;
}

// forked from gcc/cp/name-lookup.cc extract_conversion_operator

/* FNS is an overload set of conversion functions.  Return the
   overloads converting to TYPE.  */

static tree
extract_conversion_operator (tree fns, tree type)
{
  tree convs = NULL_TREE;
  tree tpls = NULL_TREE;

  for (ovl_iterator iter (fns); iter; ++iter)
    {
      if (same_type_p (DECL_CONV_FN_TYPE (*iter), type))
	convs = lookup_add (*iter, convs);
    }

  if (!convs)
    convs = tpls;

  return convs;
}

// forked from gcc/cp/name-lookup.cc

/* Look for NAME as an immediate member of KLASS (including
   anon-members or unscoped enum member).  TYPE_OR_FNS is zero for
   regular search.  >0 to get a type binding (if there is one) and <0
   if you want (just) the member function binding.

   Use this if you do not want lazy member creation.  */

tree
get_class_binding_direct (tree klass, tree name, bool want_type)
{
  gcc_checking_assert (RECORD_OR_UNION_TYPE_P (klass));

  /* Conversion operators can only be found by the marker conversion
     operator name.  */
  bool conv_op = IDENTIFIER_CONV_OP_P (name);
  tree lookup = conv_op ? conv_op_identifier : name;
  tree val = NULL_TREE;
  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);

  if (COMPLETE_TYPE_P (klass) && member_vec)
    {
      val = member_vec_binary_search (member_vec, lookup);
      if (!val)
	;
      else if (STAT_HACK_P (val))
	val = want_type ? STAT_TYPE (val) : STAT_DECL (val);
      else if (want_type && !DECL_DECLARES_TYPE_P (val))
	val = NULL_TREE;
    }
  else
    {
      if (member_vec && !want_type)
	val = member_vec_linear_search (member_vec, lookup);

      if (!val || (TREE_CODE (val) == OVERLOAD && OVL_DEDUP_P (val)))
	/* Dependent using declarations are a 'field', make sure we
	   return that even if we saw an overload already.  */
	if (tree field_val = fields_linear_search (klass, lookup, want_type))
	  {
	    if (!val)
	      val = field_val;
	    else if (TREE_CODE (field_val) == USING_DECL)
	      val = ovl_make (field_val, val);
	  }
    }

  /* Extract the conversion operators asked for, unless the general
     conversion operator was requested.   */
  if (val && conv_op)
    {
      gcc_checking_assert (OVL_FUNCTION (val) == conv_op_marker);
      val = OVL_CHAIN (val);
      if (tree type = TREE_TYPE (name))
	val = extract_conversion_operator (val, type);
    }

  return val;
}

#if defined ENABLE_TREE_CHECKING

// forked from gcc/cp/tree.cc lang_check_failed

/* Complain that some language-specific thing hanging off a tree
   node has been accessed improperly.  */

void
lang_check_failed (const char *file, int line, const char *function)
{
  internal_error ("%<lang_*%> check: failed in %s, at %s:%d", function,
		  trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

// forked from gcc/cp/tree.cc skip_artificial_parms_for

/* Given a FUNCTION_DECL FN and a chain LIST, skip as many elements of LIST
   as there are artificial parms in FN.  */

tree
skip_artificial_parms_for (const_tree fn, tree list)
{
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
    list = TREE_CHAIN (list);
  else
    return list;

  if (DECL_HAS_IN_CHARGE_PARM_P (fn))
    list = TREE_CHAIN (list);
  if (DECL_HAS_VTT_PARM_P (fn))
    list = TREE_CHAIN (list);
  return list;
}

// forked from gcc/cp/class.cc in_class_defaulted_default_constructor

/* Returns the defaulted constructor if T has one. Otherwise, returns
   NULL_TREE.  */

tree
in_class_defaulted_default_constructor (tree t)
{
  if (!TYPE_HAS_USER_CONSTRUCTOR (t))
    return NULL_TREE;

  for (ovl_iterator iter (CLASSTYPE_CONSTRUCTORS (t)); iter; ++iter)
    {
      tree fn = *iter;

      if (DECL_DEFAULTED_IN_CLASS_P (fn) && default_ctor_p (fn))
	return fn;
    }

  return NULL_TREE;
}

// forked from gcc/cp/constexpr.cc

/* Returns true iff FUN is an instantiation of a constexpr function
   template or a defaulted constexpr function.  */

bool
is_instantiation_of_constexpr (tree fun)
{
  return ((DECL_DEFAULTED_FN (fun) && DECL_DECLARED_CONSTEXPR_P (fun)));
}

// forked from gcc/cp/decl.cc check_for_uninitialized_const_var

/* Issue an error message if DECL is an uninitialized const variable.
   CONSTEXPR_CONTEXT_P is true when the function is called in a constexpr
   context from potential_constant_expression.  Returns true if all is well,
   false otherwise.  */

bool
check_for_uninitialized_const_var (tree decl, bool constexpr_context_p,
				   tsubst_flags_t complain)
{
  tree type = strip_array_types (TREE_TYPE (decl));

  /* ``Unless explicitly declared extern, a const object does not have
     external linkage and must be initialized. ($8.4; $12.1)'' ARM
     7.1.6 */
  if (VAR_P (decl) && !TYPE_REF_P (type) && (RS_TYPE_CONST_P (type))
      && !DECL_NONTRIVIALLY_INITIALIZED_P (decl))
    {
      tree field = default_init_uninitialized_part (type);
      if (!field)
	return true;

      bool show_notes = true;

      if (!constexpr_context_p)
	{
	  if (RS_TYPE_CONST_P (type))
	    {
	      if (complain & tf_error)
		show_notes = permerror (DECL_SOURCE_LOCATION (decl),
					"uninitialized %<const %D%>", decl);
	    }
	  else
	    {
	      if (!is_instantiation_of_constexpr (current_function_decl)
		  && (complain & tf_error))
		error_at (DECL_SOURCE_LOCATION (decl),
			  "uninitialized variable %qD in %<constexpr%> "
			  "function",
			  decl);
	      else
		show_notes = false;
	    }
	}
      else if (complain & tf_error)
	error_at (DECL_SOURCE_LOCATION (decl),
		  "uninitialized variable %qD in %<constexpr%> context", decl);

      if (show_notes && CLASS_TYPE_P (type) && (complain & tf_error))
	{
	  tree defaulted_ctor;

	  inform (DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)),
		  "%q#T has no user-provided default constructor", type);
	  defaulted_ctor = in_class_defaulted_default_constructor (type);
	  if (defaulted_ctor)
	    inform (DECL_SOURCE_LOCATION (defaulted_ctor),
		    "constructor is not user-provided because it is "
		    "explicitly defaulted in the class body");
	  inform (DECL_SOURCE_LOCATION (field),
		  "and the implicitly-defined constructor does not "
		  "initialize %q#D",
		  field);
	}

      return false;
    }

  return true;
}

// forked from gcc/cp/tree.cc cv_unqualified

/* Return TYPE with const and volatile removed.  */

tree
cv_unqualified (tree type)
{
  int quals;

  if (type == error_mark_node)
    return type;

  quals = rs_type_quals (type);
  quals &= ~(TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
  return rs_build_qualified_type (type, quals);
}

/* The C and C++ parsers both use vectors to hold function arguments.
   For efficiency, we keep a cache of unused vectors.  This is the
   cache.  */

typedef vec<tree, va_gc> *tree_gc_vec;
static GTY ((deletable)) vec<tree_gc_vec, va_gc> *tree_vector_cache;

// forked from gcc/c-family/c-common.c make_tree_vector

/* Return a new vector from the cache.  If the cache is empty,
   allocate a new vector.  These vectors are GC'ed, so it is OK if the
   pointer is not released..  */

vec<tree, va_gc> *
make_tree_vector (void)
{
  if (tree_vector_cache && !tree_vector_cache->is_empty ())
    return tree_vector_cache->pop ();
  else
    {
      /* Passing 0 to vec::alloc returns NULL, and our callers require
	 that we always return a non-NULL value.  The vector code uses
	 4 when growing a NULL vector, so we do too.  */
      vec<tree, va_gc> *v;
      vec_alloc (v, 4);
      return v;
    }
}

// forked from gcc/c-family/c-common.c release_tree_vector

/* Release a vector of trees back to the cache.  */

void
release_tree_vector (vec<tree, va_gc> *vec)
{
  if (vec != NULL)
    {
      if (vec->allocated () >= 16)
	/* Don't cache vecs that have expanded more than once.  On a p64
	   target, vecs double in alloc size with each power of 2 elements, e.g
	   at 16 elements the alloc increases from 128 to 256 bytes.  */
	vec_free (vec);
      else
	{
	  vec->truncate (0);
	  vec_safe_push (tree_vector_cache, vec);
	}
    }
}

// forked from gcc/cp/cvt.cc instantiation_dependent_expression_p

/* As above, but also check value-dependence of the expression as a whole.  */

bool
instantiation_dependent_expression_p (tree expression)
{
  return false;
}

// forked from gcc/cp/cvt.cc cp_get_callee

/* If CALL is a call, return the callee; otherwise null.  */

tree
cp_get_callee (tree call)
{
  if (call == NULL_TREE)
    return call;
  else if (TREE_CODE (call) == CALL_EXPR)
    return CALL_EXPR_FN (call);
  return NULL_TREE;
}

// forked from gcc/cp/typeck.cc build_nop

/* Return a NOP_EXPR converting EXPR to TYPE.  */

tree
build_nop (tree type, tree expr)
{
  if (type == error_mark_node || error_operand_p (expr))
    return expr;
  return build1_loc (EXPR_LOCATION (expr), NOP_EXPR, type, expr);
}

// forked from gcc/cp/tree.cc scalarish_type_p

/* Returns 1 iff type T is something we want to treat as a scalar type for
   the purpose of deciding whether it is trivial/POD/standard-layout.  */

bool
scalarish_type_p (const_tree t)
{
  if (t == error_mark_node)
    return 1;

  return (SCALAR_TYPE_P (t) || VECTOR_TYPE_P (t));
}

// forked from gcc/cp/tree.cc type_has_nontrivial_copy_init

/* Returns true iff copying an object of type T (including via move
   constructor) is non-trivial.  That is, T has no non-trivial copy
   constructors and no non-trivial move constructors, and not all copy/move
   constructors are deleted.  This function implements the ABI notion of
   non-trivial copy, which has diverged from the one in the standard.  */

bool
type_has_nontrivial_copy_init (const_tree type)
{
  return false;
}

// forked from gcc/cp/tree.cc build_local_temp

/* Return an undeclared local temporary of type TYPE for use in building a
   TARGET_EXPR.  */

tree
build_local_temp (tree type)
{
  tree slot = build_decl (input_location, VAR_DECL, NULL_TREE, type);
  DECL_ARTIFICIAL (slot) = 1;
  DECL_IGNORED_P (slot) = 1;
  DECL_CONTEXT (slot) = current_function_decl;
  layout_decl (slot, 0);
  return slot;
}

// forked from gcc/cp/lambda.cc is_normal_capture_proxy

/* Returns true iff DECL is a capture proxy for a normal capture
   (i.e. without explicit initializer).  */

bool
is_normal_capture_proxy (tree decl)
{
  return false;
}

// forked from gcc/cp/c-common.cc reject_gcc_builtin

/* For an EXPR of a FUNCTION_TYPE that references a GCC built-in function
   with no library fallback or for an ADDR_EXPR whose operand is such type
   issues an error pointing to the location LOC.
   Returns true when the expression has been diagnosed and false
   otherwise.  */

bool
reject_gcc_builtin (const_tree expr, location_t loc /* = UNKNOWN_LOCATION */)
{
  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);

  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_TYPE (expr) && TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE
      && TREE_CODE (expr) == FUNCTION_DECL
      /* The intersection of DECL_BUILT_IN and DECL_IS_UNDECLARED_BUILTIN avoids
	 false positives for user-declared built-ins such as abs or
	 strlen, and for C++ operators new and delete.
	 The c_decl_implicit() test avoids false positives for implicitly
	 declared built-ins with library fallbacks (such as abs).  */
      && fndecl_built_in_p (expr) && DECL_IS_UNDECLARED_BUILTIN (expr)
      && !DECL_ASSEMBLER_NAME_SET_P (expr))
    {
      if (loc == UNKNOWN_LOCATION)
	loc = EXPR_LOC_OR_LOC (expr, input_location);

      /* Reject arguments that are built-in functions with
	 no library fallback.  */
      error_at (loc, "built-in function %qE must be directly called", expr);

      return true;
    }

  return false;
}

// forked from gcc/cp/typeck.cc is_bitfield_expr_with_lowered_type

/* If EXP is a reference to a bit-field, and the type of EXP does not
   match the declared type of the bit-field, return the declared type
   of the bit-field.  Otherwise, return NULL_TREE.  */

tree
is_bitfield_expr_with_lowered_type (const_tree exp)
{
  switch (TREE_CODE (exp))
    {
    case COND_EXPR:
      if (!is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 1)
						 ? TREE_OPERAND (exp, 1)
						 : TREE_OPERAND (exp, 0)))
	return NULL_TREE;
      return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 2));

    case COMPOUND_EXPR:
      return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 1));

    case MODIFY_EXPR:
    case SAVE_EXPR:
    case UNARY_PLUS_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case NEGATE_EXPR:
    case NON_LVALUE_EXPR:
    case BIT_NOT_EXPR:
      return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 0));

      case COMPONENT_REF: {
	tree field;

	field = TREE_OPERAND (exp, 1);
	if (TREE_CODE (field) != FIELD_DECL || !DECL_BIT_FIELD_TYPE (field))
	  return NULL_TREE;
	if (same_type_ignoring_top_level_qualifiers_p (
	      TREE_TYPE (exp), DECL_BIT_FIELD_TYPE (field)))
	  return NULL_TREE;
	return DECL_BIT_FIELD_TYPE (field);
      }

    case VAR_DECL:
      if (DECL_HAS_VALUE_EXPR_P (exp))
	return is_bitfield_expr_with_lowered_type (
	  DECL_VALUE_EXPR (CONST_CAST_TREE (exp)));
      return NULL_TREE;

    case VIEW_CONVERT_EXPR:
      if (location_wrapper_p (exp))
	return is_bitfield_expr_with_lowered_type (TREE_OPERAND (exp, 0));
      else
	return NULL_TREE;

    default:
      return NULL_TREE;
    }
}

// forked from gcc/cp/semantics.cc maybe_undo_parenthesized_ref

/* If T is an id-expression obfuscated by force_paren_expr, undo the
   obfuscation and return the underlying id-expression.  Otherwise
   return T.  */

tree
maybe_undo_parenthesized_ref (tree t)
{
  if ((TREE_CODE (t) == PAREN_EXPR || TREE_CODE (t) == VIEW_CONVERT_EXPR)
      && REF_PARENTHESIZED_P (t))
    t = TREE_OPERAND (t, 0);

  return t;
}

// forked from gcc/c-family/c-common.cc fold_offsetof

/* Fold an offsetof-like expression.  EXPR is a nested sequence of component
   references with an INDIRECT_REF of a constant at the bottom; much like the
   traditional rendering of offsetof as a macro.  TYPE is the desired type of
   the whole expression.  Return the folded result.  */

tree
fold_offsetof (tree expr, tree type, enum tree_code ctx)
{
  tree base, off, t;
  tree_code code = TREE_CODE (expr);
  switch (code)
    {
    case ERROR_MARK:
      return expr;

    case VAR_DECL:
      error ("cannot apply %<offsetof%> to static data member %qD", expr);
      return error_mark_node;

    case CALL_EXPR:
    case TARGET_EXPR:
      error ("cannot apply %<offsetof%> when %<operator[]%> is overloaded");
      return error_mark_node;

    case NOP_EXPR:
    case INDIRECT_REF:
      if (!TREE_CONSTANT (TREE_OPERAND (expr, 0)))
	{
	  error ("cannot apply %<offsetof%> to a non constant address");
	  return error_mark_node;
	}
      return convert (type, TREE_OPERAND (expr, 0));

    case COMPONENT_REF:
      base = fold_offsetof (TREE_OPERAND (expr, 0), type, code);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      if (DECL_C_BIT_FIELD (t))
	{
	  error ("attempt to take address of bit-field structure "
		 "member %qD",
		 t);
	  return error_mark_node;
	}
      off = size_binop_loc (input_location, PLUS_EXPR, DECL_FIELD_OFFSET (t),
			    size_int (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (t))
				      / BITS_PER_UNIT));
      break;

    case ARRAY_REF:
      base = fold_offsetof (TREE_OPERAND (expr, 0), type, code);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      STRIP_ANY_LOCATION_WRAPPER (t);

      /* Check if the offset goes beyond the upper bound of the array.  */
      if (TREE_CODE (t) == INTEGER_CST && tree_int_cst_sgn (t) >= 0)
	{
	  tree upbound = array_ref_up_bound (expr);
	  if (upbound != NULL_TREE && TREE_CODE (upbound) == INTEGER_CST
	      && !tree_int_cst_equal (upbound,
				      TYPE_MAX_VALUE (TREE_TYPE (upbound))))
	    {
	      if (ctx != ARRAY_REF && ctx != COMPONENT_REF)
		upbound = size_binop (PLUS_EXPR, upbound,
				      build_int_cst (TREE_TYPE (upbound), 1));
	      if (tree_int_cst_lt (upbound, t))
		{
		  tree v;

		  for (v = TREE_OPERAND (expr, 0);
		       TREE_CODE (v) == COMPONENT_REF; v = TREE_OPERAND (v, 0))
		    if (TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			== RECORD_TYPE)
		      {
			tree fld_chain = DECL_CHAIN (TREE_OPERAND (v, 1));
			for (; fld_chain; fld_chain = DECL_CHAIN (fld_chain))
			  if (TREE_CODE (fld_chain) == FIELD_DECL)
			    break;

			if (fld_chain)
			  break;
		      }
		  /* Don't warn if the array might be considered a poor
		     man's flexible array member with a very permissive
		     definition thereof.  */
		  if (TREE_CODE (v) == ARRAY_REF
		      || TREE_CODE (v) == COMPONENT_REF)
		    warning (OPT_Warray_bounds,
			     "index %E denotes an offset "
			     "greater than size of %qT",
			     t, TREE_TYPE (TREE_OPERAND (expr, 0)));
		}
	    }
	}

      t = convert (sizetype, t);
      off = size_binop (MULT_EXPR, TYPE_SIZE_UNIT (TREE_TYPE (expr)), t);
      break;

    case COMPOUND_EXPR:
      /* Handle static members of volatile structs.  */
      t = TREE_OPERAND (expr, 1);
      gcc_checking_assert (VAR_P (get_base_address (t)));
      return fold_offsetof (t, type);

    default:
      gcc_unreachable ();
    }

  if (!POINTER_TYPE_P (type))
    return size_binop (PLUS_EXPR, base, convert (type, off));
  return fold_build_pointer_plus (base, off);
}

// forked from gcc/cp/tree.cc char_type_p

/* Returns nonzero if TYPE is a character type, including wchar_t.  */

int
char_type_p (tree type)
{
  return (same_type_p (type, char_type_node)
	  || same_type_p (type, unsigned_char_type_node)
	  || same_type_p (type, signed_char_type_node)
	  || same_type_p (type, char8_type_node)
	  || same_type_p (type, char16_type_node)
	  || same_type_p (type, char32_type_node)
	  || same_type_p (type, wchar_type_node));
}

// forked from gcc/cp/pt.cc resolve_nondeduced_context

/* Core DR 115: In contexts where deduction is done and fails, or in
   contexts where deduction is not done, if a template argument list is
   specified and it, along with any default template arguments, identifies
   a single function template specialization, then the template-id is an
   lvalue for the function template specialization.  */

tree
resolve_nondeduced_context (tree orig_expr, tsubst_flags_t complain)
{
  tree expr, offset, baselink;
  bool addr;

  if (!type_unknown_p (orig_expr))
    return orig_expr;

  expr = orig_expr;
  addr = false;
  offset = NULL_TREE;
  baselink = NULL_TREE;

  if (TREE_CODE (expr) == ADDR_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      addr = true;
    }

  return orig_expr;
}

// forked from gcc/cp/pt.cc instantiate_non_dependent_or_null

/* Like instantiate_non_dependent_expr, but return NULL_TREE rather than
   an uninstantiated expression.  */

tree
instantiate_non_dependent_or_null (tree expr)
{
  if (expr == NULL_TREE)
    return NULL_TREE;

  return expr;
}

// forked from gcc/cp/pt.cc resolve_nondeduced_context_or_error

/* As above, but error out if the expression remains overloaded.  */

tree
resolve_nondeduced_context_or_error (tree exp, tsubst_flags_t complain)
{
  exp = resolve_nondeduced_context (exp, complain);
  if (type_unknown_p (exp))
    {
      if (complain & tf_error)
	cxx_incomplete_type_error (exp, TREE_TYPE (exp));
      return error_mark_node;
    }
  return exp;
}

// forked from gcc/cp/tree.cc really_overloaded_fn

/* Returns true iff X is an expression for an overloaded function
   whose type cannot be known without performing overload
   resolution.  */

bool
really_overloaded_fn (tree x)
{
  return is_overloaded_fn (x) == 2;
}

// forked from gcc/cp/typeck..cc invalid_nonstatic_memfn_p

/* EXPR is being used in a context that is not a function call.
   Enforce:

     [expr.ref]

     The expression can be used only as the left-hand operand of a
     member function call.

     [expr.mptr.operator]

     If the result of .* or ->* is a function, then that result can be
     used only as the operand for the function call operator ().

   by issuing an error message if appropriate.  Returns true iff EXPR
   violates these rules.  */

bool
invalid_nonstatic_memfn_p (location_t loc, tree expr, tsubst_flags_t complain)
{
  if (expr == NULL_TREE)
    return false;
  /* Don't enforce this in MS mode.  */
  if (flag_ms_extensions)
    return false;
  if (is_overloaded_fn (expr) && !really_overloaded_fn (expr))
    expr = get_first_fn (expr);
  if (DECL_NONSTATIC_MEMBER_FUNCTION_P (expr))
    {
      if (complain & tf_error)
	{
	  if (DECL_P (expr))
	    {
	      error_at (loc, "invalid use of non-static member function %qD",
			expr);
	      inform (DECL_SOURCE_LOCATION (expr), "declared here");
	    }
	  else
	    error_at (loc,
		      "invalid use of non-static member function of "
		      "type %qT",
		      TREE_TYPE (expr));
	}
      return true;
    }
  return false;
}

// forked from gcc/cp/call.cc strip_top_quals

tree
strip_top_quals (tree t)
{
  if (TREE_CODE (t) == ARRAY_TYPE)
    return t;
  return rs_build_qualified_type (t, 0);
}

// forked from gcc/cp/typeck2.cc cxx_incomplete_type_inform

/* Print an inform about the declaration of the incomplete type TYPE.  */

void
cxx_incomplete_type_inform (const_tree type)
{
  if (!TYPE_MAIN_DECL (type))
    return;

  location_t loc = DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type));
  tree ptype = strip_top_quals (CONST_CAST_TREE (type));

  if (current_class_type && TYPE_BEING_DEFINED (current_class_type)
      && same_type_p (ptype, current_class_type))
    inform (loc,
	    "definition of %q#T is not complete until "
	    "the closing brace",
	    ptype);
  else
    inform (loc, "forward declaration of %q#T", ptype);
}

// forked from gcc/cp/typeck2.cc cxx_incomplete_type_diagnostic

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  DIAG_KIND indicates the
   type of diagnostic (see diagnostic.def).  */

void
cxx_incomplete_type_diagnostic (location_t loc, const_tree value,
				const_tree type, diagnostic_t diag_kind)
{
  bool is_decl = false, complained = false;

  gcc_assert (diag_kind == DK_WARNING || diag_kind == DK_PEDWARN
	      || diag_kind == DK_ERROR);

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value)
    {
      STRIP_ANY_LOCATION_WRAPPER (value);

      if (VAR_P (value) || TREE_CODE (value) == PARM_DECL
	  || TREE_CODE (value) == FIELD_DECL)
	{
	  complained = emit_diagnostic (diag_kind, DECL_SOURCE_LOCATION (value),
					0, "%qD has incomplete type", value);
	  is_decl = true;
	}
    }
retry:
  /* We must print an error message.  Be clever about what it says.  */

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (!is_decl)
	complained
	  = emit_diagnostic (diag_kind, loc, 0,
			     "invalid use of incomplete type %q#T", type);
      if (complained)
	cxx_incomplete_type_inform (type);
      break;

    case VOID_TYPE:
      emit_diagnostic (diag_kind, loc, 0, "invalid use of %qT", type);
      break;

    case ARRAY_TYPE:
      if (TYPE_DOMAIN (type))
	{
	  type = TREE_TYPE (type);
	  goto retry;
	}
      emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of array with unspecified bounds");
      break;

    case OFFSET_TYPE:
      bad_member : {
	tree member = TREE_OPERAND (value, 1);
	if (is_overloaded_fn (member))
	  member = get_first_fn (member);

	if (DECL_FUNCTION_MEMBER_P (member) && !flag_ms_extensions)
	  {
	    gcc_rich_location richloc (loc);
	    /* If "member" has no arguments (other than "this"), then
	       add a fix-it hint.  */
	    if (type_num_arguments (TREE_TYPE (member)) == 1)
	      richloc.add_fixit_insert_after ("()");
	    emit_diagnostic (diag_kind, &richloc, 0,
			     "invalid use of member function %qD "
			     "(did you forget the %<()%> ?)",
			     member);
	  }
	else
	  emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of member %qD "
			   "(did you forget the %<&%> ?)",
			   member);
      }
      break;

    case LANG_TYPE:
      if (type == init_list_type_node)
	{
	  emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of brace-enclosed initializer list");
	  break;
	}
      gcc_assert (type == unknown_type_node);
      if (value && TREE_CODE (value) == COMPONENT_REF)
	goto bad_member;
      else if (value && TREE_CODE (value) == ADDR_EXPR)
	emit_diagnostic (diag_kind, loc, 0,
			 "address of overloaded function with no contextual "
			 "type information");
      else if (value && TREE_CODE (value) == OVERLOAD)
	emit_diagnostic (
	  diag_kind, loc, 0,
	  "overloaded function with no contextual type information");
      else
	emit_diagnostic (
	  diag_kind, loc, 0,
	  "insufficient contextual information to determine type");
      break;

    default:
      gcc_unreachable ();
    }
}

// forked from gcc/cp/decl2.cc decl_constant_var_p

/* Nonzero for a VAR_DECL whose value can be used in a constant expression.

      [expr.const]

      An integral constant-expression can only involve ... const
      variables of integral or enumeration types initialized with
      constant expressions ...

      C++0x also allows constexpr variables and temporaries initialized
      with constant expressions.  We handle the former here, but the latter
      are just folded away in cxx_eval_constant_expression.

   The standard does not require that the expression be non-volatile.
   G++ implements the proposed correction in DR 457.  */

bool
decl_constant_var_p (tree decl)
{
  if (!decl_maybe_constant_var_p (decl))
    return false;

  return DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl);
}

// forked from gcc/cp/decl.cc undeduced_auto_decl

/* Returns true iff DECL is a variable or function declared with an auto type
   that has not yet been deduced to a real type.  */

bool
undeduced_auto_decl (tree decl)
{
  return false;
}

// forked from gcc/cp/decl.cc require_deduced_type

/* Complain if DECL has an undeduced return type.  */

bool
require_deduced_type (tree decl, tsubst_flags_t complain)
{
  return true;
}

/* Return the location of a tree passed to %+ formats.  */

location_t
location_of (tree t)
{
  if (TYPE_P (t))
    {
      t = TYPE_MAIN_DECL (t);
      if (t == NULL_TREE)
	return input_location;
    }
  else if (TREE_CODE (t) == OVERLOAD)
    t = OVL_FIRST (t);

  if (DECL_P (t))
    return DECL_SOURCE_LOCATION (t);

  return EXPR_LOCATION (t);
}

/* For element type ELT_TYPE, return the appropriate type of the heap object
   containing such element(s).  COOKIE_SIZE is NULL or the size of cookie
   in bytes.  FULL_SIZE is NULL if it is unknown how big the heap allocation
   will be, otherwise size of the heap object.  If COOKIE_SIZE is NULL,
   return array type ELT_TYPE[FULL_SIZE / sizeof(ELT_TYPE)], otherwise return
   struct { size_t[COOKIE_SIZE/sizeof(size_t)]; ELT_TYPE[N]; }
   where N is nothing (flexible array member) if FULL_SIZE is NULL, otherwise
   it is computed such that the size of the struct fits into FULL_SIZE.  */

tree
build_new_constexpr_heap_type (tree elt_type, tree cookie_size, tree full_size)
{
  gcc_assert (cookie_size == NULL_TREE || tree_fits_uhwi_p (cookie_size));
  gcc_assert (full_size == NULL_TREE || tree_fits_uhwi_p (full_size));
  unsigned HOST_WIDE_INT csz = cookie_size ? tree_to_uhwi (cookie_size) : 0;
  tree itype2 = NULL_TREE;
  if (full_size)
    {
      unsigned HOST_WIDE_INT fsz = tree_to_uhwi (full_size);
      gcc_assert (fsz >= csz);
      fsz -= csz;
      fsz /= int_size_in_bytes (elt_type);
      itype2 = build_index_type (size_int (fsz - 1));
      if (!cookie_size)
	return build_cplus_array_type (elt_type, itype2);
    }
  else
    gcc_assert (cookie_size);
  csz /= int_size_in_bytes (sizetype);
  tree itype1 = build_index_type (size_int (csz - 1));
  tree atype1 = build_cplus_array_type (sizetype, itype1);
  tree atype2 = build_cplus_array_type (elt_type, itype2);
  tree rtype = cxx_make_type (RECORD_TYPE);
  TYPE_NAME (rtype) = heap_identifier;
  tree fld1 = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE, atype1);
  tree fld2 = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE, atype2);
  DECL_FIELD_CONTEXT (fld1) = rtype;
  DECL_FIELD_CONTEXT (fld2) = rtype;
  DECL_ARTIFICIAL (fld1) = true;
  DECL_ARTIFICIAL (fld2) = true;
  TYPE_FIELDS (rtype) = fld1;
  DECL_CHAIN (fld1) = fld2;
  layout_type (rtype);
  return rtype;
}

// forked from gcc/cp/class.cc field_poverlapping_p

/* Return true iff FIELD_DECL DECL is potentially overlapping.  */

static bool
field_poverlapping_p (tree decl)
{
  return lookup_attribute ("no_unique_address", DECL_ATTRIBUTES (decl));
}

// forked from gcc/cp/class.cc is_empty_field

/* Return true iff DECL is an empty field, either for an empty base or a
   [[no_unique_address]] data member.  */

bool
is_empty_field (tree decl)
{
  if (!decl || TREE_CODE (decl) != FIELD_DECL)
    return false;

  bool r = (is_empty_class (TREE_TYPE (decl)) && (field_poverlapping_p (decl)));

  /* Empty fields should have size zero.  */
  gcc_checking_assert (!r || integer_zerop (DECL_SIZE (decl)));

  return r;
}

} // namespace Rust
