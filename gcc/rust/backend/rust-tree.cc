// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "target.h"
#include "file-prefix-map.h"
#include "cgraph.h"
#include "output.h"
#include "memmodel.h"
#include "tm_p.h"

// forked from gcc/c-family/c-common.cc c_global_trees
tree c_global_trees[CTI_MAX];
// forked from gcc/cp/decl.cc cp_global_trees
tree cp_global_trees[CPTI_MAX];

struct saved_scope *scope_chain;

namespace Rust {

void
mark_exp_read (tree exp)
{
  char tmp_name[32];
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lsrc_loc", 1);

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
		rust_unreachable ();
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
		rust_unreachable ();
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
		rust_unreachable ();
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
	      rust_unreachable ();
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
  // gcc_assert (
  //   (TREE_CODE (type) != METHOD_TYPE && !TYPE_REF_P (type))
  //   || ((quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)) ==
  //   TYPE_UNQUALIFIED));
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
    rust_unreachable ();
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
struct rust_conv_type_hasher : ggc_ptr_hash<tree_node>
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

static GTY (()) hash_table<rust_conv_type_hasher> *conv_type_names;

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
    conv_type_names = hash_table<rust_conv_type_hasher>::create_ggc (31);

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

//// forked from gcc/cp/pt.cc has_extra_args_mechanism_p
//
///* Return true if the tree T has the extra args mechanism for
//   avoiding partial instantiation.  */
//
// static bool
// has_extra_args_mechanism_p (const_tree t)
//{
//  return false;
//}

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

// forked from gcc/cp/name-lookup.cc resort_data

} // namespace Rust

static struct
{
  gt_pointer_operator new_value;
  void *cookie;
} resort_data;

// forked from gcc/cp/name-lookup.cc resort_member_name_cmp

/* This routine compares two fields like member_name_cmp but using the
   pointer operator in resort_field_decl_data.  We don't have to deal
   with duplicates here.  */

static int
resort_member_name_cmp (const void *a_p, const void *b_p)
{
  tree a = *(const tree *) a_p;
  tree b = *(const tree *) b_p;
  tree name_a = OVL_NAME (a);
  tree name_b = OVL_NAME (b);

  resort_data.new_value (&name_a, &name_a, resort_data.cookie);
  resort_data.new_value (&name_b, &name_b, resort_data.cookie);

  gcc_checking_assert (name_a != name_b);

  return name_a < name_b ? -1 : +1;
}

// forked from gcc/cp/name-lookup.cc resort_type_member_vec

/* Resort CLASSTYPE_MEMBER_VEC because pointers have been reordered.  */

void
resort_type_member_vec (void *obj, void * /*orig_obj*/,
			gt_pointer_operator new_value, void *cookie)
{
  if (vec<tree, va_gc> *member_vec = (vec<tree, va_gc> *) obj)
    {
      resort_data.new_value = new_value;
      resort_data.cookie = cookie;
      member_vec->qsort (resort_member_name_cmp);
    }
}

namespace Rust {

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

  return NULL_TREE;
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
      rust_unreachable ();

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
      rust_unreachable ();
    }

  /* We can get here with --disable-checking.  */
  return false;
}

// forked from gcc/cp/class.cc publicly_uniquely_derived_p

/* TRUE iff TYPE is publicly & uniquely derived from PARENT.  */

bool publicly_uniquely_derived_p (tree, tree) { return false; }

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

struct rust_cplus_array_hasher : ggc_ptr_hash<tree_node>
{
  typedef cplus_array_info *compare_type;

  static hashval_t hash (tree t);
  static bool equal (tree, cplus_array_info *);
};

/* Hash an ARRAY_TYPE.  K is really of type `tree'.  */

hashval_t
rust_cplus_array_hasher::hash (tree t)
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
rust_cplus_array_hasher::equal (tree t1, cplus_array_info *t2)
{
  return (TREE_TYPE (t1) == t2->type && TYPE_DOMAIN (t1) == t2->domain);
}

// forked from gcc/cp/tree.cc cplus_array_htab

/* Hash table containing dependent array types, which are unsuitable for
   the language-independent type hash table.  */
static GTY (()) hash_table<rust_cplus_array_hasher> *cplus_array_htab;

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
	cplus_array_htab = hash_table<rust_cplus_array_hasher>::create_ggc (61);

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

      if (!flag_checking)
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
	  // tree defaulted_ctor;

	  // inform (DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)),
	  //         "%q#T has no user-provided default constructor", type);
	  // defaulted_ctor = in_class_defaulted_default_constructor (type);
	  // if (defaulted_ctor)
	  //   inform (DECL_SOURCE_LOCATION (defaulted_ctor),
	  //           "constructor is not user-provided because it is "
	  //           "explicitly defaulted in the class body");
	  // inform (DECL_SOURCE_LOCATION (field),
	  //         "and the implicitly-defined constructor does not "
	  //         "initialize %q#D",
	  //         field);
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

bool instantiation_dependent_expression_p (tree) { return false; }

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

bool type_has_nontrivial_copy_init (const_tree) { return false; }

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

bool is_normal_capture_proxy (tree) { return false; }

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
		    warning (OPT_Warray_bounds_,
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
      rust_unreachable ();
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
resolve_nondeduced_context (tree orig_expr, tsubst_flags_t)
{
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

// void
// cxx_incomplete_type_inform (const_tree type)
// {
//   if (!TYPE_MAIN_DECL (type))
//     return;

//   location_t loc = DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type));
//   tree ptype = strip_top_quals (CONST_CAST_TREE (type));

//   if (current_class_type && TYPE_BEING_DEFINED (current_class_type)
//       && same_type_p (ptype, current_class_type))
//     inform (loc,
// 	    "definition of %q#T is not complete until "
// 	    "the closing brace",
// 	    ptype);
//   else
//     inform (loc, "forward declaration of %q#T", ptype);
// }

// forked from gcc/cp/typeck2.cc cxx_incomplete_type_diagnostic

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  DIAG_KIND indicates the
   type of diagnostic (see diagnostic.def).  */

void
cxx_incomplete_type_diagnostic (location_t loc, const_tree value,
				const_tree type, diagnostic_t diag_kind)
{
  //  bool is_decl = false, complained = false;

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
	  // complained = emit_diagnostic (diag_kind, DECL_SOURCE_LOCATION
	  // (value),
	  //       			0, "%qD has incomplete type", value);
	  // is_decl = true;
	}
    }
retry:
  /* We must print an error message.  Be clever about what it says.  */

  switch (TREE_CODE (type))
    {
      // case RECORD_TYPE:
      // case UNION_TYPE:
      // case ENUMERAL_TYPE:
      //   if (!is_decl)
      //     complained
      //       = emit_diagnostic (diag_kind, loc, 0,
      //     		     "invalid use of incomplete type %q#T", type);
      //   if (complained)
      //     cxx_incomplete_type_inform (type);
      //   break;

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
      rust_unreachable ();
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

bool undeduced_auto_decl (tree) { return false; }

// forked from gcc/cp/decl.cc require_deduced_type

/* Complain if DECL has an undeduced return type.  */

bool require_deduced_type (tree, tsubst_flags_t) { return true; }

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

// forked from gcc/cp/call.cc in_immediate_context

/* Return true if in an immediate function context, or an unevaluated operand,
   or a subexpression of an immediate invocation.  */

bool
in_immediate_context ()
{
  return false;
}

// forked from gcc/cp/cvt.cc cp_get_fndecl_from_callee

/* FN is the callee of a CALL_EXPR or AGGR_INIT_EXPR; return the FUNCTION_DECL
   if we can.  */

tree
rs_get_fndecl_from_callee (tree fn, bool fold /* = true */)
{
  if (fn == NULL_TREE)
    return fn;
  if (TREE_CODE (fn) == FUNCTION_DECL)
    return fn;
  tree type = TREE_TYPE (fn);
  if (type == NULL_TREE || !INDIRECT_TYPE_P (type))
    return NULL_TREE;
  if (fold)
    fn = Compile::maybe_constant_init (fn);
  STRIP_NOPS (fn);
  if (TREE_CODE (fn) == ADDR_EXPR || TREE_CODE (fn) == FDESC_EXPR)
    fn = TREE_OPERAND (fn, 0);
  if (TREE_CODE (fn) == FUNCTION_DECL)
    return fn;
  return NULL_TREE;
}

// forked from gcc/cp/cvt.cc cp_get_callee_fndecl_nofold
tree
rs_get_callee_fndecl_nofold (tree call)
{
  return rs_get_fndecl_from_callee (cp_get_callee (call), false);
}

// forked from gcc/cp/init.cc is_class_type

/* Report an error if TYPE is not a user-defined, class type.  If
   OR_ELSE is nonzero, give an error message.  */

int
is_class_type (tree type, int or_else)
{
  if (type == error_mark_node)
    return 0;

  if (!CLASS_TYPE_P (type))
    {
      if (or_else)
	error ("%qT is not a class type", type);
      return 0;
    }
  return 1;
}

// forked from gcc/cp/decl.cc lookup_enumerator

/* Look for an enumerator with the given NAME within the enumeration
   type ENUMTYPE.  This routine is used primarily for qualified name
   lookup into an enumerator in C++0x, e.g.,

     enum class Color { Red, Green, Blue };

     Color color = Color::Red;

   Returns the value corresponding to the enumerator, or
   NULL_TREE if no such enumerator was found.  */
tree
lookup_enumerator (tree enumtype, tree name)
{
  tree e;
  gcc_assert (enumtype && TREE_CODE (enumtype) == ENUMERAL_TYPE);

  e = purpose_member (name, TYPE_VALUES (enumtype));
  return e ? TREE_VALUE (e) : NULL_TREE;
}

// forked from gcc/cp/init.cc constant_value_1
// commented out mark_used

/* If DECL is a scalar enumeration constant or variable with a
   constant initializer, return the initializer (or, its initializers,
   recursively); otherwise, return DECL.  If STRICT_P, the
   initializer is only returned if DECL is a
   constant-expression.  If RETURN_AGGREGATE_CST_OK_P, it is ok to
   return an aggregate constant.  If UNSHARE_P, return an unshared
   copy of the initializer.  */

static tree
constant_value_1 (tree decl, bool strict_p, bool return_aggregate_cst_ok_p,
		  bool unshare_p)
{
  while (TREE_CODE (decl) == CONST_DECL || decl_constant_var_p (decl)
	 || (!strict_p && VAR_P (decl)
	     && RS_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (decl))))
    {
      tree init;
      /* If DECL is a static data member in a template
	 specialization, we must instantiate it here.  The
	 initializer for the static data member is not processed
	 until needed; we need it now.  */
      // mark_used (decl, tf_none);
      init = DECL_INITIAL (decl);
      if (init == error_mark_node)
	{
	  if (TREE_CODE (decl) == CONST_DECL
	      || DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl))
	    /* Treat the error as a constant to avoid cascading errors on
	       excessively recursive template instantiation (c++/9335).  */
	    return init;
	  else
	    return decl;
	}

      /* Instantiate a non-dependent initializer for user variables.  We
	 mustn't do this for the temporary for an array compound literal;
	 trying to instatiate the initializer will keep creating new
	 temporaries until we crash.  Probably it's not useful to do it for
	 other artificial variables, either.  */
      if (!DECL_ARTIFICIAL (decl))
	init = instantiate_non_dependent_or_null (init);
      if (!init || !TREE_TYPE (init) || !TREE_CONSTANT (init)
	  || (!return_aggregate_cst_ok_p
	      /* Unless RETURN_AGGREGATE_CST_OK_P is true, do not
		 return an aggregate constant (of which string
		 literals are a special case), as we do not want
		 to make inadvertent copies of such entities, and
		 we must be sure that their addresses are the
		 same everywhere.  */
	      && (TREE_CODE (init) == CONSTRUCTOR
		  || TREE_CODE (init) == STRING_CST)))
	break;
      /* Don't return a CONSTRUCTOR for a variable with partial run-time
	 initialization, since it doesn't represent the entire value.
	 Similarly for VECTOR_CSTs created by cp_folding those
	 CONSTRUCTORs.  */
      if ((TREE_CODE (init) == CONSTRUCTOR || TREE_CODE (init) == VECTOR_CST)
	  && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl))
	break;
      /* If the variable has a dynamic initializer, don't use its
	 DECL_INITIAL which doesn't reflect the real value.  */
      if (VAR_P (decl) && TREE_STATIC (decl)
	  && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl)
	  && DECL_NONTRIVIALLY_INITIALIZED_P (decl))
	break;
      decl = init;
    }
  return unshare_p ? unshare_expr (decl) : decl;
}

// forked from gcc/cp/init.cc decl_constant_value

/* A more relaxed version of decl_really_constant_value, used by the
   common C/C++ code.  */

tree
decl_constant_value (tree decl, bool unshare_p)
{
  return constant_value_1 (decl, /*strict_p=*/false,
			   /*return_aggregate_cst_ok_p=*/true,
			   /*unshare_p=*/unshare_p);
}

// Below is forked from gcc/cp/init.cc decl_constant_value

tree
decl_constant_value (tree decl)
{
  return decl_constant_value (decl, /*unshare_p=*/true);
}

// Below is forked from gcc/cp/cp-gimplify.cc

/* Type for source_location_table hash_set.  */
struct GTY ((for_user)) source_location_table_entry
{
  location_t loc;
  unsigned uid;
  tree var;
};

// exit/reenter namespace to declare some external functions

} // namespace Rust

extern void
gt_pch_nx (Rust::source_location_table_entry &);
extern void
gt_pch_nx (Rust::source_location_table_entry *, gt_pointer_operator, void *);

namespace Rust {

/* Traits class for function start hash maps below.  */

struct rust_source_location_table_entry_hash
  : ggc_remove<source_location_table_entry>
{
  typedef source_location_table_entry value_type;
  typedef source_location_table_entry compare_type;

  static hashval_t hash (const source_location_table_entry &ref)
  {
    inchash::hash hstate (0);
    hstate.add_int (ref.loc);
    hstate.add_int (ref.uid);
    return hstate.end ();
  }

  static bool equal (const source_location_table_entry &ref1,
		     const source_location_table_entry &ref2)
  {
    return ref1.loc == ref2.loc && ref1.uid == ref2.uid;
  }

  static void mark_deleted (source_location_table_entry &ref)
  {
    ref.loc = UNKNOWN_LOCATION;
    ref.uid = -1U;
    ref.var = NULL_TREE;
  }

  static const bool empty_zero_p = true;

  static void mark_empty (source_location_table_entry &ref)
  {
    ref.loc = UNKNOWN_LOCATION;
    ref.uid = 0;
    ref.var = NULL_TREE;
  }

  static bool is_deleted (const source_location_table_entry &ref)
  {
    return (ref.loc == UNKNOWN_LOCATION && ref.uid == -1U
	    && ref.var == NULL_TREE);
  }

  static bool is_empty (const source_location_table_entry &ref)
  {
    return (ref.loc == UNKNOWN_LOCATION && ref.uid == 0
	    && ref.var == NULL_TREE);
  }

  static void pch_nx (source_location_table_entry &p) { gt_pch_nx (p); }

  static void pch_nx (source_location_table_entry &p, gt_pointer_operator op,
		      void *cookie)
  {
    gt_pch_nx (&p, op, cookie);
  }
};

static GTY (())
  hash_table<rust_source_location_table_entry_hash> *source_location_table;
static GTY (()) unsigned int source_location_id;

// Above is forked from gcc/cp/cp-gimplify.cc

// forked from gcc/cp/tree.cc lvalue_kind

/* If REF is an lvalue, returns the kind of lvalue that REF is.
   Otherwise, returns clk_none.  */

cp_lvalue_kind
lvalue_kind (const_tree ref)
{
  cp_lvalue_kind op1_lvalue_kind = clk_none;
  cp_lvalue_kind op2_lvalue_kind = clk_none;

  /* Expressions of reference type are sometimes wrapped in
     INDIRECT_REFs.  INDIRECT_REFs are just internal compiler
     representation, not part of the language, so we have to look
     through them.  */
  if (REFERENCE_REF_P (ref))
    return lvalue_kind (TREE_OPERAND (ref, 0));

  if (TREE_TYPE (ref) && TYPE_REF_P (TREE_TYPE (ref)))
    {
      /* unnamed rvalue references are rvalues */
      if (TYPE_REF_IS_RVALUE (TREE_TYPE (ref)) && TREE_CODE (ref) != PARM_DECL
	  && !VAR_P (ref)
	  && TREE_CODE (ref) != COMPONENT_REF
	  /* Functions are always lvalues.  */
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (ref))) != FUNCTION_TYPE)
	{
	  op1_lvalue_kind = clk_rvalueref;
	  if (implicit_rvalue_p (ref))
	    op1_lvalue_kind |= clk_implicit_rval;
	  return op1_lvalue_kind;
	}

      /* lvalue references and named rvalue references are lvalues.  */
      return clk_ordinary;
    }

  if (ref == current_class_ptr)
    return clk_none;

  /* Expressions with cv void type are prvalues.  */
  if (TREE_TYPE (ref) && VOID_TYPE_P (TREE_TYPE (ref)))
    return clk_none;

  switch (TREE_CODE (ref))
    {
    case SAVE_EXPR:
      return clk_none;

      /* preincrements and predecrements are valid lvals, provided
	 what they refer to are valid lvals.  */
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case TRY_CATCH_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      return lvalue_kind (TREE_OPERAND (ref, 0));

      case ARRAY_REF: {
	tree op1 = TREE_OPERAND (ref, 0);
	if (TREE_CODE (TREE_TYPE (op1)) == ARRAY_TYPE)
	  {
	    op1_lvalue_kind = lvalue_kind (op1);
	    if (op1_lvalue_kind == clk_class)
	      /* in the case of an array operand, the result is an lvalue if
		 that operand is an lvalue and an xvalue otherwise */
	      op1_lvalue_kind = clk_rvalueref;
	    return op1_lvalue_kind;
	  }
	else
	  return clk_ordinary;
      }

    case MEMBER_REF:
    case DOTSTAR_EXPR:
      if (TREE_CODE (ref) == MEMBER_REF)
	op1_lvalue_kind = clk_ordinary;
      else
	op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
      if (TYPE_PTRMEMFUNC_P (TREE_TYPE (TREE_OPERAND (ref, 1))))
	op1_lvalue_kind = clk_none;
      else if (op1_lvalue_kind == clk_class)
	/* The result of a .* expression whose second operand is a pointer to a
	   data member is an lvalue if the first operand is an lvalue and an
	   xvalue otherwise.  */
	op1_lvalue_kind = clk_rvalueref;
      return op1_lvalue_kind;

    case COMPONENT_REF:
      op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
      if (op1_lvalue_kind == clk_class)
	/* If E1 is an lvalue, then E1.E2 is an lvalue;
	   otherwise E1.E2 is an xvalue.  */
	op1_lvalue_kind = clk_rvalueref;

      /* Look at the member designator.  */
      if (!op1_lvalue_kind)
	;
      else if (is_overloaded_fn (TREE_OPERAND (ref, 1)))
	/* The "field" can be a FUNCTION_DECL or an OVERLOAD in some
	   situations.  If we're seeing a COMPONENT_REF, it's a non-static
	   member, so it isn't an lvalue. */
	op1_lvalue_kind = clk_none;
      else if (TREE_CODE (TREE_OPERAND (ref, 1)) != FIELD_DECL)
	/* This can be IDENTIFIER_NODE in a template.  */;
      else if (DECL_C_BIT_FIELD (TREE_OPERAND (ref, 1)))
	{
	  /* Clear the ordinary bit.  If this object was a class
	     rvalue we want to preserve that information.  */
	  op1_lvalue_kind &= ~clk_ordinary;
	  /* The lvalue is for a bitfield.  */
	  op1_lvalue_kind |= clk_bitfield;
	}
      else if (DECL_PACKED (TREE_OPERAND (ref, 1)))
	op1_lvalue_kind |= clk_packed;

      return op1_lvalue_kind;

    case STRING_CST:
    case COMPOUND_LITERAL_EXPR:
      return clk_ordinary;

    case CONST_DECL:
      /* CONST_DECL without TREE_STATIC are enumeration values and
	 thus not lvalues.  With TREE_STATIC they are used by ObjC++
	 in objc_build_string_object and need to be considered as
	 lvalues.  */
      if (!TREE_STATIC (ref))
	return clk_none;
      /* FALLTHRU */
    case VAR_DECL:
      if (VAR_P (ref) && DECL_HAS_VALUE_EXPR_P (ref))
	return lvalue_kind (DECL_VALUE_EXPR (CONST_CAST_TREE (ref)));

      if (TREE_READONLY (ref) && !TREE_STATIC (ref) && DECL_LANG_SPECIFIC (ref)
	  && DECL_IN_AGGR_P (ref))
	return clk_none;
      /* FALLTHRU */
    case INDIRECT_REF:
    case ARROW_EXPR:
    case PARM_DECL:
    case RESULT_DECL:
    case PLACEHOLDER_EXPR:
      return clk_ordinary;

    case MAX_EXPR:
    case MIN_EXPR:
      /* Disallow <? and >? as lvalues if either argument side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (ref, 0))
	  || TREE_SIDE_EFFECTS (TREE_OPERAND (ref, 1)))
	return clk_none;
      op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
      op2_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 1));
      break;

      case COND_EXPR: {
	tree op1 = TREE_OPERAND (ref, 1);
	if (!op1)
	  op1 = TREE_OPERAND (ref, 0);
	tree op2 = TREE_OPERAND (ref, 2);
	op1_lvalue_kind = lvalue_kind (op1);
	op2_lvalue_kind = lvalue_kind (op2);
	if (!op1_lvalue_kind != !op2_lvalue_kind)
	  {
	    /* The second or the third operand (but not both) is a
	       throw-expression; the result is of the type
	       and value category of the other.  */
	    if (op1_lvalue_kind && TREE_CODE (op2) == THROW_EXPR)
	      op2_lvalue_kind = op1_lvalue_kind;
	    else if (op2_lvalue_kind && TREE_CODE (op1) == THROW_EXPR)
	      op1_lvalue_kind = op2_lvalue_kind;
	  }
      }
      break;

    case MODIFY_EXPR:
    case TYPEID_EXPR:
      return clk_ordinary;

    case COMPOUND_EXPR:
      return lvalue_kind (TREE_OPERAND (ref, 1));

    case TARGET_EXPR:
      return clk_class;

    case VA_ARG_EXPR:
      return (CLASS_TYPE_P (TREE_TYPE (ref)) ? clk_class : clk_none);

    case CALL_EXPR:
      /* We can see calls outside of TARGET_EXPR in templates.  */
      if (CLASS_TYPE_P (TREE_TYPE (ref)))
	return clk_class;
      return clk_none;

    case FUNCTION_DECL:
      /* All functions (except non-static-member functions) are
	 lvalues.  */
      return (DECL_NONSTATIC_MEMBER_FUNCTION_P (ref) ? clk_none : clk_ordinary);

    case PAREN_EXPR:
      return lvalue_kind (TREE_OPERAND (ref, 0));

    case TEMPLATE_PARM_INDEX:
      if (CLASS_TYPE_P (TREE_TYPE (ref)))
	/* A template parameter object is an lvalue.  */
	return clk_ordinary;
      return clk_none;

    default:
      if (!TREE_TYPE (ref))
	return clk_none;
      if (CLASS_TYPE_P (TREE_TYPE (ref))
	  || TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE)
	return clk_class;
      return clk_none;
    }

  /* If one operand is not an lvalue at all, then this expression is
     not an lvalue.  */
  if (!op1_lvalue_kind || !op2_lvalue_kind)
    return clk_none;

  /* Otherwise, it's an lvalue, and it has all the odd properties
     contributed by either operand.  */
  op1_lvalue_kind = op1_lvalue_kind | op2_lvalue_kind;
  /* It's not an ordinary lvalue if it involves any other kind.  */
  if ((op1_lvalue_kind & ~clk_ordinary) != clk_none)
    op1_lvalue_kind &= ~clk_ordinary;
  /* It can't be both a pseudo-lvalue and a non-addressable lvalue.
     A COND_EXPR of those should be wrapped in a TARGET_EXPR.  */
  if ((op1_lvalue_kind & (clk_rvalueref | clk_class))
      && (op1_lvalue_kind & (clk_bitfield | clk_packed)))
    op1_lvalue_kind = clk_none;
  return op1_lvalue_kind;
}

// forked from gcc/cp/tree.cc glvalue_p

/* This differs from lvalue_p in that xvalues are included.  */

bool
glvalue_p (const_tree ref)
{
  cp_lvalue_kind kind = lvalue_kind (ref);
  if (kind & clk_class)
    return false;
  else
    return (kind != clk_none);
}

// forked from gcc/cp/init.cc cv_qualified_p

/* Returns nonzero if TYPE is const or volatile.  */

bool
cv_qualified_p (const_tree type)
{
  int quals = rs_type_quals (type);
  return (quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)) != 0;
}

// forked from gcc/cp/tree.cc rvalue

/* EXPR is being used in an rvalue context.  Return a version of EXPR
   that is marked as an rvalue.  */

tree
rvalue (tree expr)
{
  tree type;

  if (error_operand_p (expr))
    return expr;

  expr = mark_rvalue_use (expr);

  /* [basic.lval]

     Non-class rvalues always have cv-unqualified types.  */
  type = TREE_TYPE (expr);
  if (!CLASS_TYPE_P (type) && cv_qualified_p (type))
    type = cv_unqualified (type);

  /* We need to do this for rvalue refs as well to get the right answer
     from decltype; see c++/36628.  */
  if (glvalue_p (expr))
    {
      /* But don't use this function for class lvalues; use move (to treat an
	 lvalue as an xvalue) or force_rvalue (to make a prvalue copy).  */
      gcc_checking_assert (!CLASS_TYPE_P (type));
      expr = build1 (NON_LVALUE_EXPR, type, expr);
    }
  else if (type != TREE_TYPE (expr))
    expr = build_nop (type, expr);

  return expr;
}

// forked from gcc/cp/tree.cc bitfield_p

/* True if REF is a bit-field.  */

bool
bitfield_p (const_tree ref)
{
  return (lvalue_kind (ref) & clk_bitfield);
}

// forked from gcc/cp/typeck.cc cxx_mark_addressable

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is true if successful.  ARRAY_REF_P is true if this
   is for ARRAY_REF construction - in that case we don't want
   to look through VIEW_CONVERT_EXPR from VECTOR_TYPE to ARRAY_TYPE,
   it is fine to use ARRAY_REFs for vector subscripts on vector
   register variables.

   C++: we do not allow `current_class_ptr' to be addressable.  */

bool
cxx_mark_addressable (tree exp, bool array_ref_p)
{
  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case VIEW_CONVERT_EXPR:
	if (array_ref_p && TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	    && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (x, 0))))
	  return true;
	x = TREE_OPERAND (x, 0);
	break;

      case COMPONENT_REF:
	if (bitfield_p (x))
	  error ("attempt to take address of bit-field");
	/* FALLTHRU */
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case PARM_DECL:
	if (x == current_class_ptr)
	  {
	    error ("cannot take the address of %<this%>, which is an rvalue "
		   "expression");
	    TREE_ADDRESSABLE (x) = 1; /* so compiler doesn't die later.  */
	    return true;
	  }
	/* Fall through.  */

      case VAR_DECL:
	/* Caller should not be trying to mark initialized
	   constant fields addressable.  */
	gcc_assert (DECL_LANG_SPECIFIC (x) == 0 || DECL_IN_AGGR_P (x) == 0
		    || TREE_STATIC (x) || DECL_EXTERNAL (x));
	/* Fall through.  */

      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x) && !DECL_ARTIFICIAL (x))
	  {
	    if (VAR_P (x) && DECL_HARD_REGISTER (x))
	      {
		error ("address of explicit register variable %qD requested",
		       x);
		return false;
	      }
	    else if (extra_warnings)
	      warning (
		OPT_Wextra,
		"address requested for %qD, which is declared %<register%>", x);
	  }
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case CONST_DECL:
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case TARGET_EXPR:
	TREE_ADDRESSABLE (x) = 1;
	cxx_mark_addressable (TREE_OPERAND (x, 0));
	return true;

      default:
	return true;
      }
}

// forked from gcc/cp/typeck.cc build_address

/* Returns the address of T.  This function will fold away
   ADDR_EXPR of INDIRECT_REF.  This is only for low-level usage;
   most places should use cp_build_addr_expr instead.  */

tree
build_address (tree t)
{
  if (error_operand_p (t) || !cxx_mark_addressable (t))
    return error_mark_node;
  gcc_checking_assert (TREE_CODE (t) != CONSTRUCTOR);
  t = build_fold_addr_expr_loc (EXPR_LOCATION (t), t);
  if (TREE_CODE (t) != ADDR_EXPR)
    t = rvalue (t);
  return t;
}

// forked from gcc/cp/gp-gimplify.cc fold_builtin_source_location

/* Fold __builtin_source_location () call.  LOC is the location
   of the call.  */

tree
fold_builtin_source_location (location_t loc)
{
  //  if (source_location_impl == NULL_TREE)
  //  {
  //    auto_diagnostic_group d;
  //    source_location_impl = get_source_location_impl_type (loc);
  //    if (source_location_impl == error_mark_node)
  // inform (loc, "evaluating %qs", "__builtin_source_location");
  //  }
  if (source_location_impl == error_mark_node)
    return build_zero_cst (const_ptr_type_node);
  if (source_location_table == NULL)
    source_location_table
      = hash_table<rust_source_location_table_entry_hash>::create_ggc (64);
  const line_map_ordinary *map;
  source_location_table_entry entry;
  entry.loc = linemap_resolve_location (line_table, loc,
					LRK_MACRO_EXPANSION_POINT, &map);
  entry.uid = current_function_decl ? DECL_UID (current_function_decl) : -1;
  entry.var = error_mark_node;
  source_location_table_entry *entryp
    = source_location_table->find_slot (entry, INSERT);
  tree var;
  if (entryp->var)
    var = entryp->var;
  else
    {
      char tmp_name[32];
      ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lsrc_loc", source_location_id++);
      var = build_decl (loc, VAR_DECL, get_identifier (tmp_name),
			source_location_impl);
      TREE_STATIC (var) = 1;
      TREE_PUBLIC (var) = 0;
      DECL_ARTIFICIAL (var) = 1;
      DECL_IGNORED_P (var) = 1;
      DECL_EXTERNAL (var) = 0;
      DECL_DECLARED_CONSTEXPR_P (var) = 1;
      DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (var) = 1;
      layout_decl (var, 0);

      vec<constructor_elt, va_gc> *v = NULL;
      vec_alloc (v, 4);
      for (tree field = TYPE_FIELDS (source_location_impl);
	   (field = next_initializable_field (field)) != NULL_TREE;
	   field = DECL_CHAIN (field))
	{
	  const char *n = IDENTIFIER_POINTER (DECL_NAME (field));
	  tree val = NULL_TREE;
	  if (strcmp (n, "_M_file_name") == 0)
	    {
	      if (const char *fname = LOCATION_FILE (loc))
		{
		  fname = remap_macro_filename (fname);
		  val = build_string_literal (strlen (fname) + 1, fname);
		}
	      else
		val = build_string_literal (1, "");
	    }
	  else if (strcmp (n, "_M_function_name") == 0)
	    {
	      const char *name = "todo: add funciton name here";

	      // if (current_function_decl)
	      // name = cxx_printable_name (current_function_decl, 2);

	      val = build_string_literal (strlen (name) + 1, name);
	    }
	  else if (strcmp (n, "_M_line") == 0)
	    val = build_int_cst (TREE_TYPE (field), LOCATION_LINE (loc));
	  else if (strcmp (n, "_M_column") == 0)
	    val = build_int_cst (TREE_TYPE (field), LOCATION_COLUMN (loc));
	  else
	    rust_unreachable ();
	  CONSTRUCTOR_APPEND_ELT (v, field, val);
	}

      tree ctor = build_constructor (source_location_impl, v);
      TREE_CONSTANT (ctor) = 1;
      TREE_STATIC (ctor) = 1;
      DECL_INITIAL (var) = ctor;
      varpool_node::finalize_decl (var);
      *entryp = entry;
      entryp->var = var;
    }

  return build_fold_addr_expr_with_type_loc (loc, var, const_ptr_type_node);
}

// forked from gcc/c-family/c-common.cc braced_lists_to_strings

/* Attempt to convert a braced array initializer list CTOR for array
   TYPE into a STRING_CST for convenience and efficiency.  Return
   the converted string on success or the original ctor on failure.  */

static tree
braced_list_to_string (tree type, tree ctor, bool member)
{
  /* Ignore non-members with unknown size like arrays with unspecified
     bound.  */
  tree typesize = TYPE_SIZE_UNIT (type);
  if (!member && !tree_fits_uhwi_p (typesize))
    return ctor;

  /* If the target char size differes from the host char size, we'd risk
     loosing data and getting object sizes wrong by converting to
     host chars.  */
  if (TYPE_PRECISION (char_type_node) != CHAR_BIT)
    return ctor;

  /* If the array has an explicit bound, use it to constrain the size
     of the string.  If it doesn't, be sure to create a string that's
     as long as implied by the index of the last zero specified via
     a designator, as in:
       const char a[] = { [7] = 0 };  */
  unsigned HOST_WIDE_INT maxelts;
  if (typesize)
    {
      maxelts = tree_to_uhwi (typesize);
      maxelts /= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (type)));
    }
  else
    maxelts = HOST_WIDE_INT_M1U;

  /* Avoid converting initializers for zero-length arrays (but do
     create them for flexible array members).  */
  if (!maxelts)
    return ctor;

  unsigned HOST_WIDE_INT nelts = CONSTRUCTOR_NELTS (ctor);

  auto_vec<char> str;
  str.reserve (nelts + 1);

  unsigned HOST_WIDE_INT i;
  tree index, value;

  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), i, index, value)
    {
      unsigned HOST_WIDE_INT idx = i;
      if (index)
	{
	  if (!tree_fits_uhwi_p (index))
	    return ctor;
	  idx = tree_to_uhwi (index);
	}

      /* auto_vec is limited to UINT_MAX elements.  */
      if (idx > UINT_MAX)
	return ctor;

      /* Avoid non-constant initializers.  */
      if (!tree_fits_shwi_p (value))
	return ctor;

      /* Skip over embedded nuls except the last one (initializer
	 elements are in ascending order of indices).  */
      HOST_WIDE_INT val = tree_to_shwi (value);
      if (!val && i + 1 < nelts)
	continue;

      if (idx < str.length ())
	return ctor;

      /* Bail if the CTOR has a block of more than 256 embedded nuls
	 due to implicitly initialized elements.  */
      unsigned nchars = (idx - str.length ()) + 1;
      if (nchars > 256)
	return ctor;

      if (nchars > 1)
	{
	  str.reserve (idx);
	  str.quick_grow_cleared (idx);
	}

      if (idx >= maxelts)
	return ctor;

      str.safe_insert (idx, val);
    }

  /* Append a nul string termination.  */
  if (maxelts != HOST_WIDE_INT_M1U && str.length () < maxelts)
    str.safe_push (0);

  /* Build a STRING_CST with the same type as the array.  */
  tree res = build_string (str.length (), str.begin ());
  TREE_TYPE (res) = type;
  return res;
}

// forked from gcc/c-family/c-common.cc braced_lists_to_strings

/* Implementation of the two-argument braced_lists_to_string withe
   the same arguments plus MEMBER which is set for struct members
   to allow initializers for flexible member arrays.  */

static tree
braced_lists_to_strings (tree type, tree ctor, bool member)
{
  if (TREE_CODE (ctor) != CONSTRUCTOR)
    return ctor;

  tree_code code = TREE_CODE (type);

  tree ttp;
  if (code == ARRAY_TYPE)
    ttp = TREE_TYPE (type);
  else if (code == RECORD_TYPE)
    {
      ttp = TREE_TYPE (ctor);
      if (TREE_CODE (ttp) == ARRAY_TYPE)
	{
	  type = ttp;
	  ttp = TREE_TYPE (ttp);
	}
    }
  else
    return ctor;

  if ((TREE_CODE (ttp) == ARRAY_TYPE || TREE_CODE (ttp) == INTEGER_TYPE)
      && TYPE_STRING_FLAG (ttp))
    return braced_list_to_string (type, ctor, member);

  code = TREE_CODE (ttp);
  if (code == ARRAY_TYPE || RECORD_OR_UNION_TYPE_P (ttp))
    {
      bool rec = RECORD_OR_UNION_TYPE_P (ttp);

      /* Handle array of arrays or struct member initializers.  */
      tree val;
      unsigned HOST_WIDE_INT idx;
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (ctor), idx, val)
	{
	  val = braced_lists_to_strings (ttp, val, rec);
	  CONSTRUCTOR_ELT (ctor, idx)->value = val;
	}
    }

  return ctor;
}

// forked from gcc/c-family/c-common.cc braced_lists_to_strings

/* Attempt to convert a CTOR containing braced array initializer lists
   for array TYPE into one containing STRING_CSTs, for convenience and
   efficiency.  Recurse for arrays of arrays and member initializers.
   Return the converted CTOR or STRING_CST on success or the original
   CTOR otherwise.  */

tree
braced_lists_to_strings (tree type, tree ctor)
{
  return braced_lists_to_strings (type, ctor, false);
}

/*---------------------------------------------------------------------------
			Constraint satisfaction
---------------------------------------------------------------------------*/

// forked from gcc/cp/constraint.cc satisfying_constraint

/* True if we are currently satisfying a failed_type_completions.  */

static bool satisfying_constraint;

// forked from gcc/cp/constraint.cc satisfying_constraint

/* A vector of incomplete types (and of declarations with undeduced return
   type), appended to by note_failed_type_completion_for_satisfaction.  The
   satisfaction caches use this in order to keep track of "potentially unstable"
   satisfaction results.

   Since references to entries in this vector are stored only in the
   GC-deletable sat_cache, it's safe to make this deletable as well.  */

static GTY ((deletable)) vec<tree, va_gc> *failed_type_completions;

// forked from gcc/cp/constraint.cc note_failed_type_completion_for_satisfaction

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

// forked from gcc/cp/typeck.cc complete_type

/* Try to complete TYPE, if it is incomplete.  For example, if TYPE is
   a template instantiation, do the instantiation.  Returns TYPE,
   whether or not it could be completed, unless something goes
   horribly wrong, in which case the error_mark_node is returned.  */

tree
complete_type (tree type)
{
  if (type == NULL_TREE)
    /* Rather than crash, we return something sure to cause an error
       at some point.  */
    return error_mark_node;

  if (type == error_mark_node || COMPLETE_TYPE_P (type))
    ;
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree t = complete_type (TREE_TYPE (type));
      unsigned int needs_constructing, has_nontrivial_dtor;
      if (COMPLETE_TYPE_P (t))
	layout_type (type);
      needs_constructing = TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (t));
      has_nontrivial_dtor
	= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TYPE_MAIN_VARIANT (t));
      for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	{
	  TYPE_NEEDS_CONSTRUCTING (t) = needs_constructing;
	  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) = has_nontrivial_dtor;
	}
    }

  return type;
}

// forked from gcc/cp/typeck.cc complete_type_or_maybe_complain

/* Like complete_type, but issue an error if the TYPE cannot be completed.
   VALUE is used for informative diagnostics.
   Returns NULL_TREE if the type cannot be made complete.  */

tree
complete_type_or_maybe_complain (tree type, tree value, tsubst_flags_t complain)
{
  type = complete_type (type);
  if (type == error_mark_node)
    /* We already issued an error.  */
    return NULL_TREE;
  else if (!COMPLETE_TYPE_P (type))
    {
      if (complain & tf_error)
	cxx_incomplete_type_diagnostic (value, type, DK_ERROR);
      note_failed_type_completion_for_satisfaction (type);
      return NULL_TREE;
    }
  else
    return type;
}

// forked from gcc/cp/typeck.cc complete_type_or_else

tree
complete_type_or_else (tree type, tree value)
{
  return complete_type_or_maybe_complain (type, value, tf_warning_or_error);
}

// forked from gcc/cp/tree.cc std_layout_type_p

/* Returns true iff T is a standard-layout type, as defined in
   [basic.types].  */

bool
std_layout_type_p (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    return !CLASSTYPE_NON_STD_LAYOUT (t);
  else
    return scalarish_type_p (t);
}

// forked from /gcc/cp/semantics.cc first_nonstatic_data_member_p

/* Helper function for fold_builtin_is_pointer_inverconvertible_with_class,
   return true if MEMBERTYPE is the type of the first non-static data member
   of TYPE or for unions of any members.  */
static bool
first_nonstatic_data_member_p (tree type, tree membertype)
{
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;
      if (DECL_FIELD_IS_BASE (field) && is_empty_field (field))
	continue;
      if (DECL_FIELD_IS_BASE (field))
	return first_nonstatic_data_member_p (TREE_TYPE (field), membertype);
      if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  if ((TREE_CODE (TREE_TYPE (field)) == UNION_TYPE
	       || std_layout_type_p (TREE_TYPE (field)))
	      && first_nonstatic_data_member_p (TREE_TYPE (field), membertype))
	    return true;
	}
      else if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field),
							  membertype))
	return true;
      if (TREE_CODE (type) != UNION_TYPE)
	return false;
    }
  return false;
}

// forked from gcc/cp/semantics.cc
// fold_builtin_is_pointer_inverconvertible_with_class

/* Fold __builtin_is_pointer_interconvertible_with_class call.  */

tree
fold_builtin_is_pointer_inverconvertible_with_class (location_t loc, int nargs,
						     tree *args)
{
  /* Unless users call the builtin directly, the following 3 checks should be
     ensured from std::is_pointer_interconvertible_with_class function
     template.  */
  if (nargs != 1)
    {
      error_at (loc, "%<__builtin_is_pointer_interconvertible_with_class%> "
		     "needs a single argument");
      return boolean_false_node;
    }
  tree arg = args[0];
  if (error_operand_p (arg))
    return boolean_false_node;
  if (!TYPE_PTRMEM_P (TREE_TYPE (arg)))
    {
      error_at (loc, "%<__builtin_is_pointer_interconvertible_with_class%> "
		     "argument is not pointer to member");
      return boolean_false_node;
    }

  if (!TYPE_PTRDATAMEM_P (TREE_TYPE (arg)))
    return boolean_false_node;

  tree membertype = TREE_TYPE (TREE_TYPE (arg));
  tree basetype = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg));
  if (!complete_type_or_else (basetype, NULL_TREE))
    return boolean_false_node;

  if (TREE_CODE (basetype) != UNION_TYPE && !std_layout_type_p (basetype))
    return boolean_false_node;

  if (!first_nonstatic_data_member_p (basetype, membertype))
    return boolean_false_node;

  if (integer_nonzerop (arg))
    return boolean_false_node;
  if (integer_zerop (arg))
    return boolean_true_node;

  return fold_build2 (EQ_EXPR, boolean_type_node, arg,
		      build_zero_cst (TREE_TYPE (arg)));
}

// forked from gcc/c-family/c-common.cc registered_builtin_types

/* Used for communication between c_common_type_for_mode and
   c_register_builtin_type.  */
tree registered_builtin_types;

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.
   If the mode is a fixed-point mode,
   then UNSIGNEDP selects between saturating and nonsaturating types.  */

// forked from gcc/c-family/c-common.cc c_common_type_for_mode

tree
c_common_type_for_mode (machine_mode mode, int unsignedp)
{
  tree t;
  int i;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node
		     : long_long_integer_type_node;

  for (i = 0; i < NUM_INT_N_ENTS; i++)
    if (int_n_enabled_p[i] && mode == int_n_data[i].m)
      return (unsignedp ? int_n_trees[i].unsigned_type
			: int_n_trees[i].signed_type);

  if (mode == QImode)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == HImode)
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (mode == SImode)
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (mode == DImode)
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    if (FLOATN_NX_TYPE_NODE (i) != NULL_TREE
	&& mode == TYPE_MODE (FLOATN_NX_TYPE_NODE (i)))
      return FLOATN_NX_TYPE_NODE (i);

  if (mode == TYPE_MODE (void_type_node))
    return void_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node))
      || mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    {
      unsigned int precision
	= GET_MODE_PRECISION (as_a<scalar_int_mode> (mode));
      return (unsignedp ? make_unsigned_type (precision)
			: make_signed_type (precision));
    }

  if (COMPLEX_MODE_P (mode))
    {
      machine_mode inner_mode;
      tree inner_type;

      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;

      for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
	if (COMPLEX_FLOATN_NX_TYPE_NODE (i) != NULL_TREE
	    && mode == TYPE_MODE (COMPLEX_FLOATN_NX_TYPE_NODE (i)))
	  return COMPLEX_FLOATN_NX_TYPE_NODE (i);

      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
	return complex_integer_type_node;

      inner_mode = GET_MODE_INNER (mode);
      inner_type = c_common_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_complex_type (inner_type);
    }
  else if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
	   && valid_vector_subparts_p (GET_MODE_NUNITS (mode)))
    {
      unsigned int elem_bits
	= vector_element_size (GET_MODE_PRECISION (mode), GET_MODE_NUNITS (mode));
      tree bool_type = build_nonstandard_boolean_type (elem_bits);
      return build_vector_type_for_mode (bool_type, mode);
    }
  else if (VECTOR_MODE_P (mode)
	   && valid_vector_subparts_p (GET_MODE_NUNITS (mode)))
    {
      machine_mode inner_mode = GET_MODE_INNER (mode);
      tree inner_type = c_common_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
	return build_vector_type_for_mode (inner_type, mode);
    }

  if (dfloat32_type_node != NULL_TREE && mode == TYPE_MODE (dfloat32_type_node))
    return dfloat32_type_node;
  if (dfloat64_type_node != NULL_TREE && mode == TYPE_MODE (dfloat64_type_node))
    return dfloat64_type_node;
  if (dfloat128_type_node != NULL_TREE
      && mode == TYPE_MODE (dfloat128_type_node))
    return dfloat128_type_node;

  if (ALL_SCALAR_FIXED_POINT_MODE_P (mode))
    {
      if (mode == TYPE_MODE (short_fract_type_node))
	return unsignedp ? sat_short_fract_type_node : short_fract_type_node;
      if (mode == TYPE_MODE (fract_type_node))
	return unsignedp ? sat_fract_type_node : fract_type_node;
      if (mode == TYPE_MODE (long_fract_type_node))
	return unsignedp ? sat_long_fract_type_node : long_fract_type_node;
      if (mode == TYPE_MODE (long_long_fract_type_node))
	return unsignedp ? sat_long_long_fract_type_node
			 : long_long_fract_type_node;

      if (mode == TYPE_MODE (unsigned_short_fract_type_node))
	return unsignedp ? sat_unsigned_short_fract_type_node
			 : unsigned_short_fract_type_node;
      if (mode == TYPE_MODE (unsigned_fract_type_node))
	return unsignedp ? sat_unsigned_fract_type_node
			 : unsigned_fract_type_node;
      if (mode == TYPE_MODE (unsigned_long_fract_type_node))
	return unsignedp ? sat_unsigned_long_fract_type_node
			 : unsigned_long_fract_type_node;
      if (mode == TYPE_MODE (unsigned_long_long_fract_type_node))
	return unsignedp ? sat_unsigned_long_long_fract_type_node
			 : unsigned_long_long_fract_type_node;

      if (mode == TYPE_MODE (short_accum_type_node))
	return unsignedp ? sat_short_accum_type_node : short_accum_type_node;
      if (mode == TYPE_MODE (accum_type_node))
	return unsignedp ? sat_accum_type_node : accum_type_node;
      if (mode == TYPE_MODE (long_accum_type_node))
	return unsignedp ? sat_long_accum_type_node : long_accum_type_node;
      if (mode == TYPE_MODE (long_long_accum_type_node))
	return unsignedp ? sat_long_long_accum_type_node
			 : long_long_accum_type_node;

      if (mode == TYPE_MODE (unsigned_short_accum_type_node))
	return unsignedp ? sat_unsigned_short_accum_type_node
			 : unsigned_short_accum_type_node;
      if (mode == TYPE_MODE (unsigned_accum_type_node))
	return unsignedp ? sat_unsigned_accum_type_node
			 : unsigned_accum_type_node;
      if (mode == TYPE_MODE (unsigned_long_accum_type_node))
	return unsignedp ? sat_unsigned_long_accum_type_node
			 : unsigned_long_accum_type_node;
      if (mode == TYPE_MODE (unsigned_long_long_accum_type_node))
	return unsignedp ? sat_unsigned_long_long_accum_type_node
			 : unsigned_long_long_accum_type_node;

      if (mode == QQmode)
	return unsignedp ? sat_qq_type_node : qq_type_node;
      if (mode == HQmode)
	return unsignedp ? sat_hq_type_node : hq_type_node;
      if (mode == SQmode)
	return unsignedp ? sat_sq_type_node : sq_type_node;
      if (mode == DQmode)
	return unsignedp ? sat_dq_type_node : dq_type_node;
      if (mode == TQmode)
	return unsignedp ? sat_tq_type_node : tq_type_node;

      if (mode == UQQmode)
	return unsignedp ? sat_uqq_type_node : uqq_type_node;
      if (mode == UHQmode)
	return unsignedp ? sat_uhq_type_node : uhq_type_node;
      if (mode == USQmode)
	return unsignedp ? sat_usq_type_node : usq_type_node;
      if (mode == UDQmode)
	return unsignedp ? sat_udq_type_node : udq_type_node;
      if (mode == UTQmode)
	return unsignedp ? sat_utq_type_node : utq_type_node;

      if (mode == HAmode)
	return unsignedp ? sat_ha_type_node : ha_type_node;
      if (mode == SAmode)
	return unsignedp ? sat_sa_type_node : sa_type_node;
      if (mode == DAmode)
	return unsignedp ? sat_da_type_node : da_type_node;
      if (mode == TAmode)
	return unsignedp ? sat_ta_type_node : ta_type_node;

      if (mode == UHAmode)
	return unsignedp ? sat_uha_type_node : uha_type_node;
      if (mode == USAmode)
	return unsignedp ? sat_usa_type_node : usa_type_node;
      if (mode == UDAmode)
	return unsignedp ? sat_uda_type_node : uda_type_node;
      if (mode == UTAmode)
	return unsignedp ? sat_uta_type_node : uta_type_node;
    }

  for (t = registered_builtin_types; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);
      if (TYPE_MODE (type) == mode
	  && VECTOR_TYPE_P (type) == VECTOR_MODE_P (mode)
	  && !!unsignedp == !!TYPE_UNSIGNED (type))
	return type;
    }
  return NULL_TREE;
}

// forked from gcc/cp/semantics.cc finish_underlying_type

/* Implement the __underlying_type keyword: Return the underlying
   type of TYPE, suitable for use as a type-specifier.  */

tree
finish_underlying_type (tree type)
{
  tree underlying_type;

  if (!complete_type_or_else (type, NULL_TREE))
    return error_mark_node;

  if (TREE_CODE (type) != ENUMERAL_TYPE)
    {
      error ("%qT is not an enumeration type", type);
      return error_mark_node;
    }

  underlying_type = ENUM_UNDERLYING_TYPE (type);

  /* Fixup necessary in this case because ENUM_UNDERLYING_TYPE
     includes TYPE_MIN_VALUE and TYPE_MAX_VALUE information.
     See finish_enum_value_list for details.  */
  if (!ENUM_FIXED_UNDERLYING_TYPE_P (type))
    underlying_type = c_common_type_for_mode (TYPE_MODE (underlying_type),
					      TYPE_UNSIGNED (underlying_type));

  return underlying_type;
}

// forked from gcc/cp/typeck.cc layout_compatible_type_p

/* Return true if TYPE1 and TYPE2 are layout-compatible types.  */

bool
layout_compatible_type_p (tree type1, tree type2)
{
  if (type1 == error_mark_node || type2 == error_mark_node)
    return false;
  if (type1 == type2)
    return true;
  if (TREE_CODE (type1) != TREE_CODE (type2))
    return false;

  type1 = rs_build_qualified_type (type1, TYPE_UNQUALIFIED);
  type2 = rs_build_qualified_type (type2, TYPE_UNQUALIFIED);

  if (TREE_CODE (type1) == ENUMERAL_TYPE)
    return (TYPE_ALIGN (type1) == TYPE_ALIGN (type2)
	    && tree_int_cst_equal (TYPE_SIZE (type1), TYPE_SIZE (type2))
	    && same_type_p (finish_underlying_type (type1),
			    finish_underlying_type (type2)));

  if (CLASS_TYPE_P (type1) && std_layout_type_p (type1)
      && std_layout_type_p (type2) && TYPE_ALIGN (type1) == TYPE_ALIGN (type2)
      && tree_int_cst_equal (TYPE_SIZE (type1), TYPE_SIZE (type2)))
    {
      tree field1 = TYPE_FIELDS (type1);
      tree field2 = TYPE_FIELDS (type2);
      if (TREE_CODE (type1) == RECORD_TYPE)
	{
	  while (1)
	    {
	      if (!next_common_initial_seqence (field1, field2))
		return false;
	      if (field1 == NULL_TREE)
		return true;
	      field1 = DECL_CHAIN (field1);
	      field2 = DECL_CHAIN (field2);
	    }
	}
      /* Otherwise both types must be union types.
	 The standard says:
	 "Two standard-layout unions are layout-compatible if they have
	 the same number of non-static data members and corresponding
	 non-static data members (in any order) have layout-compatible
	 types."
	 but the code anticipates that bitfield vs. non-bitfield,
	 different bitfield widths or presence/absence of
	 [[no_unique_address]] should be checked as well.  */
      auto_vec<tree, 16> vec;
      unsigned int count = 0;
      for (; field1; field1 = DECL_CHAIN (field1))
	if (TREE_CODE (field1) == FIELD_DECL)
	  count++;
      for (; field2; field2 = DECL_CHAIN (field2))
	if (TREE_CODE (field2) == FIELD_DECL)
	  vec.safe_push (field2);
      /* Discussions on core lean towards treating multiple union fields
	 of the same type as the same field, so this might need changing
	 in the future.  */
      if (count != vec.length ())
	return false;
      for (field1 = TYPE_FIELDS (type1); field1; field1 = DECL_CHAIN (field1))
	{
	  if (TREE_CODE (field1) != FIELD_DECL)
	    continue;
	  unsigned int j;
	  tree t1 = DECL_BIT_FIELD_TYPE (field1);
	  if (t1 == NULL_TREE)
	    t1 = TREE_TYPE (field1);
	  FOR_EACH_VEC_ELT (vec, j, field2)
	    {
	      tree t2 = DECL_BIT_FIELD_TYPE (field2);
	      if (t2 == NULL_TREE)
		t2 = TREE_TYPE (field2);
	      if (DECL_BIT_FIELD_TYPE (field1))
		{
		  if (!DECL_BIT_FIELD_TYPE (field2))
		    continue;
		  if (TYPE_PRECISION (TREE_TYPE (field1))
		      != TYPE_PRECISION (TREE_TYPE (field2)))
		    continue;
		}
	      else if (DECL_BIT_FIELD_TYPE (field2))
		continue;
	      if (!layout_compatible_type_p (t1, t2))
		continue;
	      if ((!lookup_attribute ("no_unique_address",
				      DECL_ATTRIBUTES (field1)))
		  != !lookup_attribute ("no_unique_address",
					DECL_ATTRIBUTES (field2)))
		continue;
	      break;
	    }
	  if (j == vec.length ())
	    return false;
	  vec.unordered_remove (j);
	}
      return true;
    }

  return same_type_p (type1, type2);
}

// forked from gcc/cp/semnatics.cc is_corresponding_member_union

/* Helper function for is_corresponding_member_aggr.  Return true if
   MEMBERTYPE pointer-to-data-member ARG can be found in anonymous
   union or structure BASETYPE.  */

static bool
is_corresponding_member_union (tree basetype, tree membertype, tree arg)
{
  for (tree field = TYPE_FIELDS (basetype); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) != FIELD_DECL || DECL_BIT_FIELD_TYPE (field))
      continue;
    else if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field),
							membertype))
      {
	if (TREE_CODE (arg) != INTEGER_CST
	    || tree_int_cst_equal (arg, byte_position (field)))
	  return true;
      }
    else if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
      {
	tree narg = arg;
	if (TREE_CODE (basetype) != UNION_TYPE
	    && TREE_CODE (narg) == INTEGER_CST)
	  narg = size_binop (MINUS_EXPR, arg, byte_position (field));
	if (is_corresponding_member_union (TREE_TYPE (field), membertype, narg))
	  return true;
      }
  return false;
}

// forked from gcc/cp/typeck.cc next_common_initial_seqence

/* Helper function for layout_compatible_type_p and
   is_corresponding_member_aggr.  Advance to next members (NULL if
   no further ones) and return true if those members are still part of
   the common initial sequence.  */

bool
next_common_initial_seqence (tree &memb1, tree &memb2)
{
  while (memb1)
    {
      if (TREE_CODE (memb1) != FIELD_DECL
	  || (DECL_FIELD_IS_BASE (memb1) && is_empty_field (memb1)))
	{
	  memb1 = DECL_CHAIN (memb1);
	  continue;
	}
      if (DECL_FIELD_IS_BASE (memb1))
	{
	  memb1 = TYPE_FIELDS (TREE_TYPE (memb1));
	  continue;
	}
      break;
    }
  while (memb2)
    {
      if (TREE_CODE (memb2) != FIELD_DECL
	  || (DECL_FIELD_IS_BASE (memb2) && is_empty_field (memb2)))
	{
	  memb2 = DECL_CHAIN (memb2);
	  continue;
	}
      if (DECL_FIELD_IS_BASE (memb2))
	{
	  memb2 = TYPE_FIELDS (TREE_TYPE (memb2));
	  continue;
	}
      break;
    }
  if (memb1 == NULL_TREE && memb2 == NULL_TREE)
    return true;
  if (memb1 == NULL_TREE || memb2 == NULL_TREE)
    return false;
  if (DECL_BIT_FIELD_TYPE (memb1))
    {
      if (!DECL_BIT_FIELD_TYPE (memb2))
	return false;
      if (!layout_compatible_type_p (DECL_BIT_FIELD_TYPE (memb1),
				     DECL_BIT_FIELD_TYPE (memb2)))
	return false;
      if (TYPE_PRECISION (TREE_TYPE (memb1))
	  != TYPE_PRECISION (TREE_TYPE (memb2)))
	return false;
    }
  else if (DECL_BIT_FIELD_TYPE (memb2))
    return false;
  else if (!layout_compatible_type_p (TREE_TYPE (memb1), TREE_TYPE (memb2)))
    return false;
  if ((!lookup_attribute ("no_unique_address", DECL_ATTRIBUTES (memb1)))
      != !lookup_attribute ("no_unique_address", DECL_ATTRIBUTES (memb2)))
    return false;
  if (!tree_int_cst_equal (bit_position (memb1), bit_position (memb2)))
    return false;
  return true;
}

// forked from gcc/cp/semantics.cc is_corresponding_member_aggr

/* Helper function for fold_builtin_is_corresponding_member call.
   Return boolean_false_node if MEMBERTYPE1 BASETYPE1::*ARG1 and
   MEMBERTYPE2 BASETYPE2::*ARG2 aren't corresponding members,
   boolean_true_node if they are corresponding members, or for
   non-constant ARG2 the highest member offset for corresponding
   members.  */

static tree
is_corresponding_member_aggr (location_t loc, tree basetype1, tree membertype1,
			      tree arg1, tree basetype2, tree membertype2,
			      tree arg2)
{
  tree field1 = TYPE_FIELDS (basetype1);
  tree field2 = TYPE_FIELDS (basetype2);
  tree ret = boolean_false_node;
  while (1)
    {
      bool r = next_common_initial_seqence (field1, field2);
      if (field1 == NULL_TREE || field2 == NULL_TREE)
	break;
      if (r
	  && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field1),
							membertype1)
	  && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field2),
							membertype2))
	{
	  tree pos = byte_position (field1);
	  if (TREE_CODE (arg1) == INTEGER_CST && tree_int_cst_equal (arg1, pos))
	    {
	      if (TREE_CODE (arg2) == INTEGER_CST)
		return boolean_true_node;
	      return pos;
	    }
	  else if (TREE_CODE (arg1) != INTEGER_CST)
	    ret = pos;
	}
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (field1))
	       && ANON_AGGR_TYPE_P (TREE_TYPE (field2)))
	{
	  if ((!lookup_attribute ("no_unique_address",
				  DECL_ATTRIBUTES (field1)))
	      != !lookup_attribute ("no_unique_address",
				    DECL_ATTRIBUTES (field2)))
	    break;
	  if (!tree_int_cst_equal (bit_position (field1),
				   bit_position (field2)))
	    break;
	  bool overlap = true;
	  tree pos = byte_position (field1);
	  if (TREE_CODE (arg1) == INTEGER_CST)
	    {
	      tree off1 = fold_convert (sizetype, arg1);
	      tree sz1 = TYPE_SIZE_UNIT (TREE_TYPE (field1));
	      if (tree_int_cst_lt (off1, pos)
		  || tree_int_cst_le (size_binop (PLUS_EXPR, pos, sz1), off1))
		overlap = false;
	    }
	  if (TREE_CODE (arg2) == INTEGER_CST)
	    {
	      tree off2 = fold_convert (sizetype, arg2);
	      tree sz2 = TYPE_SIZE_UNIT (TREE_TYPE (field2));
	      if (tree_int_cst_lt (off2, pos)
		  || tree_int_cst_le (size_binop (PLUS_EXPR, pos, sz2), off2))
		overlap = false;
	    }
	  if (overlap && NON_UNION_CLASS_TYPE_P (TREE_TYPE (field1))
	      && NON_UNION_CLASS_TYPE_P (TREE_TYPE (field2)))
	    {
	      tree narg1 = arg1;
	      if (TREE_CODE (arg1) == INTEGER_CST)
		narg1
		  = size_binop (MINUS_EXPR, fold_convert (sizetype, arg1), pos);
	      tree narg2 = arg2;
	      if (TREE_CODE (arg2) == INTEGER_CST)
		narg2
		  = size_binop (MINUS_EXPR, fold_convert (sizetype, arg2), pos);
	      tree t1 = TREE_TYPE (field1);
	      tree t2 = TREE_TYPE (field2);
	      tree nret
		= is_corresponding_member_aggr (loc, t1, membertype1, narg1, t2,
						membertype2, narg2);
	      if (nret != boolean_false_node)
		{
		  if (nret == boolean_true_node)
		    return nret;
		  if (TREE_CODE (arg1) == INTEGER_CST)
		    return size_binop (PLUS_EXPR, nret, pos);
		  ret = size_binop (PLUS_EXPR, nret, pos);
		}
	    }
	  else if (overlap && TREE_CODE (TREE_TYPE (field1)) == UNION_TYPE
		   && TREE_CODE (TREE_TYPE (field2)) == UNION_TYPE)
	    {
	      tree narg1 = arg1;
	      if (TREE_CODE (arg1) == INTEGER_CST)
		narg1
		  = size_binop (MINUS_EXPR, fold_convert (sizetype, arg1), pos);
	      tree narg2 = arg2;
	      if (TREE_CODE (arg2) == INTEGER_CST)
		narg2
		  = size_binop (MINUS_EXPR, fold_convert (sizetype, arg2), pos);
	      if (is_corresponding_member_union (TREE_TYPE (field1),
						 membertype1, narg1)
		  && is_corresponding_member_union (TREE_TYPE (field2),
						    membertype2, narg2))
		{
		  sorry_at (loc, "%<__builtin_is_corresponding_member%> "
				 "not well defined for anonymous unions");
		  return boolean_false_node;
		}
	    }
	}
      if (!r)
	break;
      field1 = DECL_CHAIN (field1);
      field2 = DECL_CHAIN (field2);
    }
  return ret;
}

// forked from gcc/cp/call.cc null_member_pointer_value_p

/* Returns true iff T is a null member pointer value (4.11).  */

bool
null_member_pointer_value_p (tree t)
{
  tree type = TREE_TYPE (t);
  if (!type)
    return false;
  else if (TYPE_PTRMEMFUNC_P (type))
    return (TREE_CODE (t) == CONSTRUCTOR && CONSTRUCTOR_NELTS (t)
	    && integer_zerop (CONSTRUCTOR_ELT (t, 0)->value));
  else if (TYPE_PTRDATAMEM_P (type))
    return integer_all_onesp (t);
  else
    return false;
}

// forked from gcc/cp/semantics.cc fold_builtin_is_corresponding_member

/* Fold __builtin_is_corresponding_member call.  */

tree
fold_builtin_is_corresponding_member (location_t loc, int nargs, tree *args)
{
  /* Unless users call the builtin directly, the following 3 checks should be
     ensured from std::is_corresponding_member function template.  */
  if (nargs != 2)
    {
      error_at (loc, "%<__builtin_is_corresponding_member%> "
		     "needs two arguments");
      return boolean_false_node;
    }
  tree arg1 = args[0];
  tree arg2 = args[1];
  if (error_operand_p (arg1) || error_operand_p (arg2))
    return boolean_false_node;
  if (!TYPE_PTRMEM_P (TREE_TYPE (arg1)) || !TYPE_PTRMEM_P (TREE_TYPE (arg2)))
    {
      error_at (loc, "%<__builtin_is_corresponding_member%> "
		     "argument is not pointer to member");
      return boolean_false_node;
    }

  if (!TYPE_PTRDATAMEM_P (TREE_TYPE (arg1))
      || !TYPE_PTRDATAMEM_P (TREE_TYPE (arg2)))
    return boolean_false_node;

  tree membertype1 = TREE_TYPE (TREE_TYPE (arg1));
  tree basetype1 = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg1));
  if (!complete_type_or_else (basetype1, NULL_TREE))
    return boolean_false_node;

  tree membertype2 = TREE_TYPE (TREE_TYPE (arg2));
  tree basetype2 = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg2));
  if (!complete_type_or_else (basetype2, NULL_TREE))
    return boolean_false_node;

  if (!NON_UNION_CLASS_TYPE_P (basetype1) || !NON_UNION_CLASS_TYPE_P (basetype2)
      || !std_layout_type_p (basetype1) || !std_layout_type_p (basetype2))
    return boolean_false_node;

  /* If the member types aren't layout compatible, then they
     can't be corresponding members.  */
  if (!layout_compatible_type_p (membertype1, membertype2))
    return boolean_false_node;

  if (null_member_pointer_value_p (arg1) || null_member_pointer_value_p (arg2))
    return boolean_false_node;

  if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg2) == INTEGER_CST
      && !tree_int_cst_equal (arg1, arg2))
    return boolean_false_node;

  if (TREE_CODE (arg2) == INTEGER_CST && TREE_CODE (arg1) != INTEGER_CST)
    {
      std::swap (arg1, arg2);
      std::swap (membertype1, membertype2);
      std::swap (basetype1, basetype2);
    }

  tree ret = is_corresponding_member_aggr (loc, basetype1, membertype1, arg1,
					   basetype2, membertype2, arg2);
  if (TREE_TYPE (ret) == boolean_type_node)
    return ret;
  /* If both arg1 and arg2 are INTEGER_CSTs, is_corresponding_member_aggr
     already returns boolean_{true,false}_node whether those particular
     members are corresponding members or not.  Otherwise, if only
     one of them is INTEGER_CST (canonicalized to first being INTEGER_CST
     above), it returns boolean_false_node if it is certainly not a
     corresponding member and otherwise we need to do a runtime check that
     those two OFFSET_TYPE offsets are equal.
     If neither of the operands is INTEGER_CST, is_corresponding_member_aggr
     returns the largest offset at which the members would be corresponding
     members, so perform arg1 <= ret && arg1 == arg2 runtime check.  */
  gcc_assert (TREE_CODE (arg2) != INTEGER_CST);
  if (TREE_CODE (arg1) == INTEGER_CST)
    return fold_build2 (EQ_EXPR, boolean_type_node, arg1,
			fold_convert (TREE_TYPE (arg1), arg2));
  ret = fold_build2 (LE_EXPR, boolean_type_node,
		     fold_convert (pointer_sized_int_node, arg1),
		     fold_convert (pointer_sized_int_node, ret));
  return fold_build2 (TRUTH_AND_EXPR, boolean_type_node, ret,
		      fold_build2 (EQ_EXPR, boolean_type_node, arg1,
				   fold_convert (TREE_TYPE (arg1), arg2)));
}

// forked from gcc/cp/tree.cc lvalue_type

/* The type of ARG when used as an lvalue.  */

tree
lvalue_type (tree arg)
{
  tree type = TREE_TYPE (arg);
  return type;
}

// forked from gcc/c-family/c-warn.cc lvalue_error

/* Print an error message for an invalid lvalue.  USE says
   how the lvalue is being used and so selects the error message.  LOC
   is the location for the error.  */

void
lvalue_error (location_t loc, enum lvalue_use use)
{
  switch (use)
    {
    case lv_assign:
      error_at (loc, "lvalue required as left operand of assignment");
      break;
    case lv_increment:
      error_at (loc, "lvalue required as increment operand");
      break;
    case lv_decrement:
      error_at (loc, "lvalue required as decrement operand");
      break;
    case lv_addressof:
      error_at (loc, "lvalue required as unary %<&%> operand");
      break;
    case lv_asm:
      error_at (loc, "lvalue required in %<asm%> statement");
      break;
    default:
      rust_unreachable ();
    }
}

// forked from gcc/cp/cp--gimplify.cc cp_fold_maybe_rvalue

/* Fold expression X which is used as an rvalue if RVAL is true.  */

tree
cp_fold_maybe_rvalue (tree x, bool rval)
{
  while (true)
    {
      x = fold (x);
      if (rval)
	x = mark_rvalue_use (x);
      if (rval && DECL_P (x) && !TYPE_REF_P (TREE_TYPE (x)))
	{
	  tree v = decl_constant_value (x);
	  if (v != x && v != error_mark_node)
	    {
	      x = v;
	      continue;
	    }
	}
      break;
    }
  return x;
}

// forked from gcc/cp/cp--gimplify.cc cp_fold_rvalue

/* Fold expression X which is used as an rvalue.  */

tree
cp_fold_rvalue (tree x)
{
  return cp_fold_maybe_rvalue (x, true);
}

/* Returns true iff class T has a constexpr destructor or has an
   implicitly declared destructor that we can't tell if it's constexpr
   without forcing a lazy declaration (which might cause undesired
   instantiations).  */

static bool
type_maybe_constexpr_destructor (tree t)
{
  /* Until C++20, only trivial destruction is constexpr.  */
  if (TYPE_HAS_TRIVIAL_DESTRUCTOR (t))
    return true;

  if (CLASS_TYPE_P (t) && CLASSTYPE_LAZY_DESTRUCTOR (t))
    /* Assume it's constexpr.  */
    return true;
  tree fn = CLASSTYPE_DESTRUCTOR (t);
  return (fn && Compile::maybe_constexpr_fn (fn));
}

/* T is a non-literal type used in a context which requires a constant
   expression.  Explain why it isn't literal.  */

void
explain_non_literal_class (tree t)
{
  static hash_set<tree> *diagnosed;

  if (!CLASS_TYPE_P (t))
    return;
  t = TYPE_MAIN_VARIANT (t);

  if (diagnosed == NULL)
    diagnosed = new hash_set<tree>;
  if (diagnosed->add (t))
    /* Already explained.  */
    return;

  auto_diagnostic_group d;
  inform (UNKNOWN_LOCATION, "%q+T is not literal because:", t);
  if (LAMBDA_TYPE_P (t))
    inform (UNKNOWN_LOCATION,
	    "  %qT is a closure type, which is only literal in "
	    "C++17 and later",
	    t);
  else if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
	   && !type_maybe_constexpr_destructor (t))
    inform (UNKNOWN_LOCATION, "  %q+T does not have %<constexpr%> destructor",
	    t);
  else if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t))
    inform (UNKNOWN_LOCATION, "  %q+T has a non-trivial destructor", t);
  else if (CLASSTYPE_NON_AGGREGATE (t) && !TYPE_HAS_TRIVIAL_DFLT (t)
	   && !LAMBDA_TYPE_P (t) && !TYPE_HAS_CONSTEXPR_CTOR (t))
    {
      inform (UNKNOWN_LOCATION,
	      "  %q+T is not an aggregate, does not have a trivial "
	      "default constructor, and has no %<constexpr%> constructor that "
	      "is not a copy or move constructor",
	      t);
      if (type_has_non_user_provided_default_constructor (t))
	/* Note that we can't simply call locate_ctor because when the
	   constructor is deleted it just returns NULL_TREE.  */
	for (ovl_iterator iter (CLASSTYPE_CONSTRUCTORS (t)); iter; ++iter)
	  {
	    tree fn = *iter;
	    tree parms = TYPE_ARG_TYPES (TREE_TYPE (fn));

	    parms = skip_artificial_parms_for (fn, parms);

	    if (sufficient_parms_p (parms))
	      {
		Compile::explain_invalid_constexpr_fn (fn);
		break;
	      }
	  }
    }
  else
    {
      tree binfo, base_binfo, field;
      int i;
      for (binfo = TYPE_BINFO (t), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	{
	  tree basetype = TREE_TYPE (base_binfo);
	  if (!CLASSTYPE_LITERAL_P (basetype))
	    {
	      inform (UNKNOWN_LOCATION,
		      "  base class %qT of %q+T is non-literal", basetype, t);
	      explain_non_literal_class (basetype);
	      return;
	    }
	}
      for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
	{
	  tree ftype;
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  ftype = TREE_TYPE (field);
	  if (!Compile::literal_type_p (ftype))
	    {
	      inform (DECL_SOURCE_LOCATION (field),
		      "  non-static data member %qD has non-literal type",
		      field);
	      if (CLASS_TYPE_P (ftype))
		explain_non_literal_class (ftype);
	    }
	  if (RS_TYPE_VOLATILE_P (ftype))
	    inform (DECL_SOURCE_LOCATION (field),
		    "  non-static data member %qD has volatile type", field);
	}
    }
}

// forked from gcc/cp/call.cc reference_related_p

/* Returns nonzero if T1 is reference-related to T2.  */

bool
reference_related_p (tree t1, tree t2)
{
  if (t1 == error_mark_node || t2 == error_mark_node)
    return false;

  t1 = TYPE_MAIN_VARIANT (t1);
  t2 = TYPE_MAIN_VARIANT (t2);

  /* [dcl.init.ref]

     Given types "cv1 T1" and "cv2 T2," "cv1 T1" is reference-related
     to "cv2 T2" if T1 is similar to T2, or T1 is a base class of T2.  */
  return (similar_type_p (t1, t2)
	  /*|| (CLASS_TYPE_P (t1) && CLASS_TYPE_P (t2)
	      && DERIVED_FROM_P (t1, t2))*/);
}

// forked from gcc/cp/typeck2.cc ordinary_char_type_p

/* True iff TYPE is a C++20 "ordinary" character type.  */

bool
ordinary_char_type_p (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  return (type == char_type_node || type == signed_char_type_node
	  || type == unsigned_char_type_node);
}

// forked from gcc/cp/typeck2.cc array_string_literal_compatible_p

/* True iff the string literal INIT has a type suitable for initializing array
   TYPE.  */

bool
array_string_literal_compatible_p (tree type, tree init)
{
  tree to_char_type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
  tree from_char_type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (init)));

  if (to_char_type == from_char_type)
    return true;
  /* The array element type does not match the initializing string
     literal element type; this is only allowed when both types are
     ordinary character type.  There are no string literals of
     signed or unsigned char type in the language, but we can get
     them internally from converting braced-init-lists to
     STRING_CST.  */
  if (ordinary_char_type_p (to_char_type)
      && ordinary_char_type_p (from_char_type))
    return true;
  return false;
}

} // namespace Rust

using namespace Rust;

#include "gt-rust-rust-tree.h"
