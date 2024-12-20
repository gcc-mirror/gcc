/* Language-dependent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987-2024 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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
#include "tree.h"
#include "cp-tree.h"
#include "gimple-expr.h"
#include "cgraph.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "debug.h"
#include "convert.h"
#include "gimplify.h"
#include "stringpool.h"
#include "attribs.h"
#include "flags.h"
#include "selftest.h"

static tree bot_manip (tree *, int *, void *);
static tree bot_replace (tree *, int *, void *);
static hashval_t list_hash_pieces (tree, tree, tree);
static tree build_target_expr (tree, tree, tsubst_flags_t);
static tree count_trees_r (tree *, int *, void *);
static tree verify_stmt_tree_r (tree *, int *, void *);

static tree handle_init_priority_attribute (tree *, tree, tree, int, bool *);
static tree handle_abi_tag_attribute (tree *, tree, tree, int, bool *);
static tree handle_contract_attribute (tree *, tree, tree, int, bool *);
static tree handle_no_dangling_attribute (tree *, tree, tree, int, bool *);

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

  if (TREE_TYPE (ref)
      && TYPE_REF_P (TREE_TYPE (ref)))
    {
      /* unnamed rvalue references are rvalues */
      if (TYPE_REF_IS_RVALUE (TREE_TYPE (ref))
	  && TREE_CODE (ref) != PARM_DECL
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
      op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
      /* As for ARRAY_REF and COMPONENT_REF, these codes turn a class prvalue
	 into an xvalue: we need to materialize the temporary before we mess
	 with it.  Except VIEW_CONVERT_EXPR that doesn't actually change the
	 type, as in location wrapper and REF_PARENTHESIZED_P.	*/
      if (op1_lvalue_kind == clk_class
	  && !(TREE_CODE (ref) == VIEW_CONVERT_EXPR
	       && (same_type_ignoring_top_level_qualifiers_p
		   (TREE_TYPE (ref), TREE_TYPE (TREE_OPERAND (ref, 0))))))
	return clk_rvalueref;
      return op1_lvalue_kind;

    case ARRAY_REF:
      {
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
      if (BASELINK_P (TREE_OPERAND (ref, 1)))
	{
	  tree fn = BASELINK_FUNCTIONS (TREE_OPERAND (ref, 1));

	  /* For static member function recurse on the BASELINK, we can get
	     here e.g. from reference_binding.  If BASELINK_FUNCTIONS is
	     OVERLOAD, the overload is resolved first if possible through
	     resolve_address_of_overloaded_function.  */
	  if (TREE_CODE (fn) == FUNCTION_DECL && DECL_STATIC_FUNCTION_P (fn))
	    return lvalue_kind (TREE_OPERAND (ref, 1));
	}
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
      if (! TREE_STATIC (ref))
	return clk_none;
      /* FALLTHRU */
    case VAR_DECL:
      if (VAR_P (ref) && DECL_HAS_VALUE_EXPR_P (ref))
	return lvalue_kind (DECL_VALUE_EXPR (CONST_CAST_TREE (ref)));

      if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	  && DECL_LANG_SPECIFIC (ref)
	  && DECL_IN_AGGR_P (ref))
	return clk_none;
      /* FALLTHRU */
    case INDIRECT_REF:
    case ARROW_EXPR:
    case PARM_DECL:
    case RESULT_DECL:
    case PLACEHOLDER_EXPR:
      return clk_ordinary;

      /* A scope ref in a template, left as SCOPE_REF to support later
	 access checking.  */
    case SCOPE_REF:
      gcc_assert (!type_dependent_expression_p (CONST_CAST_TREE (ref)));
      {
	tree op = TREE_OPERAND (ref, 1);
	if (TREE_CODE (op) == FIELD_DECL)
	  return (DECL_C_BIT_FIELD (op) ? clk_bitfield : clk_ordinary);
	else
	  return lvalue_kind (op);
      }

    case MAX_EXPR:
    case MIN_EXPR:
      /* Disallow <? and >? as lvalues if either argument side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (ref, 0))
	  || TREE_SIDE_EFFECTS (TREE_OPERAND (ref, 1)))
	return clk_none;
      op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
      op2_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 1));
      break;

    case COND_EXPR:
      if (processing_template_decl)
	{
	  /* Within templates, a REFERENCE_TYPE will indicate whether
	     the COND_EXPR result is an ordinary lvalue or rvalueref.
	     Since REFERENCE_TYPEs are handled above, if we reach this
	     point, we know we got a plain rvalue.  Unless we have a
	     type-dependent expr, that is, but we shouldn't be testing
	     lvalueness if we can't even tell the types yet!  */
	  gcc_assert (!type_dependent_expression_p (CONST_CAST_TREE (ref)));
	  goto default_;
	}
      {
	tree op1 = TREE_OPERAND (ref, 1);
	if (!op1) op1 = TREE_OPERAND (ref, 0);
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

    case MODOP_EXPR:
      /* We expect to see unlowered MODOP_EXPRs only during
	 template processing.  */
      gcc_assert (processing_template_decl);
      if (CLASS_TYPE_P (TREE_TYPE (TREE_OPERAND (ref, 0))))
	goto default_;
      else
	return clk_ordinary;

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
      return (DECL_IOBJ_MEMBER_FUNCTION_P (ref)
	      ? clk_none : clk_ordinary);

    case BASELINK:
      /* We now represent a reference to a single static member function
	 with a BASELINK.  */
      /* This CONST_CAST is okay because BASELINK_FUNCTIONS returns
	 its argument unmodified and we assign it to a const_tree.  */
      return lvalue_kind (BASELINK_FUNCTIONS (CONST_CAST_TREE (ref)));

    case PAREN_EXPR:
      return lvalue_kind (TREE_OPERAND (ref, 0));

    case TEMPLATE_PARM_INDEX:
      if (CLASS_TYPE_P (TREE_TYPE (ref)))
	/* A template parameter object is an lvalue.  */
	return clk_ordinary;
      return clk_none;

    default:
    default_:
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
  if ((op1_lvalue_kind & (clk_rvalueref|clk_class))
      && (op1_lvalue_kind & (clk_bitfield|clk_packed)))
    op1_lvalue_kind = clk_none;
  return op1_lvalue_kind;
}

/* Returns the kind of lvalue that REF is, in the sense of [basic.lval].  */

cp_lvalue_kind
real_lvalue_p (const_tree ref)
{
  cp_lvalue_kind kind = lvalue_kind (ref);
  if (kind & (clk_rvalueref|clk_class))
    return clk_none;
  else
    return kind;
}

/* c-common wants us to return bool.  */

bool
lvalue_p (const_tree t)
{
  return real_lvalue_p (t);
}

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

/* This differs from glvalue_p in that class prvalues are included.  */

bool
obvalue_p (const_tree ref)
{
  return (lvalue_kind (ref) != clk_none);
}

/* Returns true if REF is an xvalue (the result of dereferencing an rvalue
   reference), false otherwise.  */

bool
xvalue_p (const_tree ref)
{
  return (lvalue_kind (ref) & clk_rvalueref);
}

/* True if REF is a bit-field.  */

bool
bitfield_p (const_tree ref)
{
  return (lvalue_kind (ref) & clk_bitfield);
}

/* C++-specific version of stabilize_reference.  */

tree
cp_stabilize_reference (tree ref)
{
  if (processing_template_decl)
    /* As in cp_save_expr.  */
    return ref;

  STRIP_ANY_LOCATION_WRAPPER (ref);
  switch (TREE_CODE (ref))
    {
    /* We need to treat specially anything stabilize_reference doesn't
       handle specifically.  */
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case INDIRECT_REF:
    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case ERROR_MARK:
      break;
    default:
      cp_lvalue_kind kind = lvalue_kind (ref);
      if ((kind & ~clk_class) != clk_none)
	{
	  tree type = unlowered_expr_type (ref);
	  bool rval = !!(kind & clk_rvalueref);
	  type = cp_build_reference_type (type, rval);
	  /* This inhibits warnings in, eg, cxx_mark_addressable
	     (c++/60955).  */
	  warning_sentinel s (extra_warnings);
	  ref = build_static_cast (input_location, type, ref,
				   tf_error);
	}
    }

  return stabilize_reference (ref);
}

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
	  case CP_BUILT_IN_IS_CONSTANT_EVALUATED:
	  case CP_BUILT_IN_SOURCE_LOCATION:
	  case CP_BUILT_IN_IS_CORRESPONDING_MEMBER:
	  case CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS:
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

/* Build a TARGET_EXPR, initializing the DECL with the VALUE.  */

static tree
build_target_expr (tree decl, tree value, tsubst_flags_t complain)
{
  tree t;
  tree type = TREE_TYPE (decl);

  value = mark_rvalue_use (value);

  gcc_checking_assert (VOID_TYPE_P (TREE_TYPE (value))
		       || TREE_TYPE (decl) == TREE_TYPE (value)
		       /* On ARM ctors return 'this'.  */
		       || (TYPE_PTR_P (TREE_TYPE (value))
			   && TREE_CODE (value) == CALL_EXPR)
		       || useless_type_conversion_p (TREE_TYPE (decl),
						     TREE_TYPE (value)));

  /* Set TREE_READONLY for optimization, such as gimplify_init_constructor
     moving a constant aggregate into .rodata.  */
  if (CP_TYPE_CONST_NON_VOLATILE_P (type)
      && !TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && !VOID_TYPE_P (TREE_TYPE (value))
      && !TYPE_HAS_MUTABLE_P (type)
      && reduced_constant_expression_p (value))
    TREE_READONLY (decl) = true;

  if (complain & tf_no_cleanup)
    /* The caller is building a new-expr and does not need a cleanup.  */
    t = NULL_TREE;
  else
    {
      t = cxx_maybe_build_cleanup (decl, complain);
      if (t == error_mark_node)
	return error_mark_node;
    }

  set_target_expr_eliding (value);

  t = build4 (TARGET_EXPR, type, decl, value, t, NULL_TREE);
  if (location_t eloc = cp_expr_location (value))
    SET_EXPR_LOCATION (t, eloc);
  /* We always set TREE_SIDE_EFFECTS so that expand_expr does not
     ignore the TARGET_EXPR.  If there really turn out to be no
     side-effects, then the optimizer should be able to get rid of
     whatever code is generated anyhow.  */
  TREE_SIDE_EFFECTS (t) = 1;

  return t;
}

/* Return an undeclared local temporary of type TYPE for use in building a
   TARGET_EXPR.  */

tree
build_local_temp (tree type)
{
  tree slot = build_decl (input_location,
			  VAR_DECL, NULL_TREE, type);
  DECL_ARTIFICIAL (slot) = 1;
  DECL_IGNORED_P (slot) = 1;
  DECL_CONTEXT (slot) = current_function_decl;
  layout_decl (slot, 0);
  return slot;
}

/* Return whether DECL is such a local temporary (or one from
   create_tmp_var_raw).  */

bool
is_local_temp (tree decl)
{
  return (VAR_P (decl) && DECL_ARTIFICIAL (decl)
	  && !TREE_STATIC (decl));
}

/* Set various status flags when building an AGGR_INIT_EXPR object T.  */

static void
process_aggr_init_operands (tree t)
{
  bool side_effects;

  side_effects = TREE_SIDE_EFFECTS (t);
  if (!side_effects)
    {
      int i, n;
      n = TREE_OPERAND_LENGTH (t);
      for (i = 1; i < n; i++)
	{
	  tree op = TREE_OPERAND (t, i);
	  if (op && TREE_SIDE_EFFECTS (op))
	    {
	      side_effects = 1;
	      break;
	    }
	}
    }
  TREE_SIDE_EFFECTS (t) = side_effects;
}

/* Build an AGGR_INIT_EXPR of class tcc_vl_exp with the indicated RETURN_TYPE,
   FN, and SLOT.  NARGS is the number of call arguments which are specified
   as a tree array ARGS.  */

static tree
build_aggr_init_array (tree return_type, tree fn, tree slot, int nargs,
		       tree *args)
{
  tree t;
  int i;

  t = build_vl_exp (AGGR_INIT_EXPR, nargs + 3);
  TREE_TYPE (t) = return_type;
  AGGR_INIT_EXPR_FN (t) = fn;
  AGGR_INIT_EXPR_SLOT (t) = slot;
  for (i = 0; i < nargs; i++)
    AGGR_INIT_EXPR_ARG (t, i) = args[i];
  process_aggr_init_operands (t);
  return t;
}

/* INIT is a CALL_EXPR or AGGR_INIT_EXPR which needs info about its
   target.  TYPE is the type to be initialized.

   Build an AGGR_INIT_EXPR to represent the initialization.  This function
   differs from build_cplus_new in that an AGGR_INIT_EXPR can only be used
   to initialize another object, whereas a TARGET_EXPR can either
   initialize another object or create its own temporary object, and as a
   result building up a TARGET_EXPR requires that the type's destructor be
   callable.  */

tree
build_aggr_init_expr (tree type, tree init)
{
  tree fn;
  tree slot;
  tree rval;
  int is_ctor;

  gcc_assert (!VOID_TYPE_P (type));

  /* Don't build AGGR_INIT_EXPR in a template.  */
  if (processing_template_decl)
    return init;

  fn = cp_get_callee (init);
  if (fn == NULL_TREE)
    return convert (type, init);

  is_ctor = (TREE_CODE (fn) == ADDR_EXPR
	     && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
	     && DECL_CONSTRUCTOR_P (TREE_OPERAND (fn, 0)));

  /* We split the CALL_EXPR into its function and its arguments here.
     Then, in expand_expr, we put them back together.  The reason for
     this is that this expression might be a default argument
     expression.  In that case, we need a new temporary every time the
     expression is used.  That's what break_out_target_exprs does; it
     replaces every AGGR_INIT_EXPR with a copy that uses a fresh
     temporary slot.  Then, expand_expr builds up a call-expression
     using the new slot.  */

  /* If we don't need to use a constructor to create an object of this
     type, don't mess with AGGR_INIT_EXPR.  */
  if (is_ctor || TREE_ADDRESSABLE (type))
    {
      slot = build_local_temp (type);

      if (TREE_CODE (init) == CALL_EXPR)
	{
	  rval = build_aggr_init_array (void_type_node, fn, slot,
					call_expr_nargs (init),
					CALL_EXPR_ARGP (init));
	  AGGR_INIT_FROM_THUNK_P (rval)
	    = CALL_FROM_THUNK_P (init);
	}
      else
	{
	  rval = build_aggr_init_array (void_type_node, fn, slot,
					aggr_init_expr_nargs (init),
					AGGR_INIT_EXPR_ARGP (init));
	  AGGR_INIT_FROM_THUNK_P (rval)
	    = AGGR_INIT_FROM_THUNK_P (init);
	}
      TREE_SIDE_EFFECTS (rval) = 1;
      AGGR_INIT_VIA_CTOR_P (rval) = is_ctor;
      TREE_NOTHROW (rval) = TREE_NOTHROW (init);
      CALL_EXPR_OPERATOR_SYNTAX (rval) = CALL_EXPR_OPERATOR_SYNTAX (init);
      CALL_EXPR_ORDERED_ARGS (rval) = CALL_EXPR_ORDERED_ARGS (init);
      CALL_EXPR_REVERSE_ARGS (rval) = CALL_EXPR_REVERSE_ARGS (init);
      SET_EXPR_LOCATION (rval, EXPR_LOCATION (init));
    }
  else
    rval = init;

  return rval;
}

/* INIT is a CALL_EXPR or AGGR_INIT_EXPR which needs info about its
   target.  TYPE is the type that this initialization should appear to
   have.

   Build an encapsulation of the initialization to perform
   and return it so that it can be processed by language-independent
   and language-specific expression expanders.  */

tree
build_cplus_new (tree type, tree init, tsubst_flags_t complain)
{
  /* This function should cope with what build_special_member_call
     can produce.  When performing parenthesized aggregate initialization,
     it can produce a { }.  */
  if (BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      gcc_assert (cxx_dialect >= cxx20);
      return finish_compound_literal (type, init, complain);
    }

  tree rval = build_aggr_init_expr (type, init);
  tree slot;

  if (init == error_mark_node)
    return error_mark_node;

  if (!complete_type_or_maybe_complain (type, init, complain))
    return error_mark_node;

  /* Make sure that we're not trying to create an instance of an
     abstract class.  */
  if (abstract_virtuals_error (NULL_TREE, type, complain))
    return error_mark_node;

  if (TREE_CODE (rval) == AGGR_INIT_EXPR)
    slot = AGGR_INIT_EXPR_SLOT (rval);
  else if (TREE_CODE (rval) == CALL_EXPR
	   || TREE_CODE (rval) == CONSTRUCTOR)
    slot = build_local_temp (type);
  else
    return rval;

  rval = build_target_expr (slot, rval, complain);

  if (rval != error_mark_node)
    TARGET_EXPR_IMPLICIT_P (rval) = 1;

  return rval;
}

/* Subroutine of build_vec_init_expr: Build up a single element
   intialization as a proxy for the full array initialization to get things
   marked as used and any appropriate diagnostics.

   This used to be necessary because we were deferring building the actual
   constructor calls until gimplification time; now we only do it to set
   VEC_INIT_EXPR_IS_CONSTEXPR.

   We assume that init is either NULL_TREE, {}, void_type_node (indicating
   value-initialization), or another array to copy.  */

static tree
build_vec_init_elt (tree type, tree init, tsubst_flags_t complain)
{
  tree inner_type = strip_array_types (type);

  if (integer_zerop (array_type_nelts_total (type))
      || !CLASS_TYPE_P (inner_type))
    /* No interesting initialization to do.  */
    return integer_zero_node;
  if (init && BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      /* Even if init has initializers for some array elements,
	 we're interested in the {}-init of trailing elements.	*/
      if (CP_AGGREGATE_TYPE_P (inner_type))
	{
	  tree empty = build_constructor (init_list_type_node, nullptr);
	  return digest_init (inner_type, empty, complain);
	}
      else
	/* It's equivalent to value-init.  */
	init = void_type_node;
    }
  if (init == void_type_node)
    return build_value_init (inner_type, complain);

  releasing_vec argvec;
  if (init && !BRACE_ENCLOSED_INITIALIZER_P (init))
    {
      tree init_type = strip_array_types (TREE_TYPE (init));
      tree dummy = build_dummy_object (init_type);
      if (!lvalue_p (init))
	dummy = move (dummy);
      argvec->quick_push (dummy);
    }
  init = build_special_member_call (NULL_TREE, complete_ctor_identifier,
				    &argvec, inner_type, LOOKUP_NORMAL,
				    complain);

  /* For a trivial constructor, build_over_call creates a TARGET_EXPR.  But
     we don't want one here because we aren't creating a temporary.  */
  if (TREE_CODE (init) == TARGET_EXPR)
    init = TARGET_EXPR_INITIAL (init);

  return init;
}

/* Return a TARGET_EXPR which expresses the initialization of an array to
   be named later, either default-initialization or copy-initialization
   from another array of the same type.  */

tree
build_vec_init_expr (tree type, tree init, tsubst_flags_t complain)
{
  if (tree vi = get_vec_init_expr (init))
    return vi;

  tree elt_init;
  if (init && TREE_CODE (init) == CONSTRUCTOR
      && !BRACE_ENCLOSED_INITIALIZER_P (init))
    /* We built any needed constructor calls in digest_init.  */
    elt_init = init;
  else
    elt_init = build_vec_init_elt (type, init, complain);

  bool value_init = false;
  if (init == void_type_node)
    {
      value_init = true;
      init = NULL_TREE;
    }

  tree slot = build_local_temp (type);
  init = build2 (VEC_INIT_EXPR, type, slot, init);
  TREE_SIDE_EFFECTS (init) = true;
  SET_EXPR_LOCATION (init, input_location);

  if (cxx_dialect >= cxx11)
    {
      bool cx = potential_constant_expression (elt_init);
      if (BRACE_ENCLOSED_INITIALIZER_P (init))
	cx &= potential_constant_expression (init);
      VEC_INIT_EXPR_IS_CONSTEXPR (init) = cx;
    }
  VEC_INIT_EXPR_VALUE_INIT (init) = value_init;

  return init;
}

/* Call build_vec_init to expand VEC_INIT into TARGET (for which NULL_TREE
   means VEC_INIT_EXPR_SLOT).  */

tree
expand_vec_init_expr (tree target, tree vec_init, tsubst_flags_t complain,
		      vec<tree,va_gc> **flags)
{
  iloc_sentinel ils = EXPR_LOCATION (vec_init);

  if (!target)
    target = VEC_INIT_EXPR_SLOT (vec_init);
  tree init = VEC_INIT_EXPR_INIT (vec_init);
  int from_array = (init && TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE);
  return build_vec_init (target, NULL_TREE, init,
			 VEC_INIT_EXPR_VALUE_INIT (vec_init),
			 from_array, complain, flags);
}

/* Give a helpful diagnostic for a non-constexpr VEC_INIT_EXPR in a context
   that requires a constant expression.  */

void
diagnose_non_constexpr_vec_init (tree expr)
{
  tree type = TREE_TYPE (VEC_INIT_EXPR_SLOT (expr));
  tree init, elt_init;
  if (VEC_INIT_EXPR_VALUE_INIT (expr))
    init = void_type_node;
  else
    init = VEC_INIT_EXPR_INIT (expr);

  elt_init = build_vec_init_elt (type, init, tf_warning_or_error);
  require_potential_constant_expression (elt_init);
}

tree
build_array_copy (tree init)
{
  return get_target_expr (build_vec_init_expr
			  (TREE_TYPE (init), init, tf_warning_or_error));
}

/* Build a TARGET_EXPR using INIT to initialize a new temporary of the
   indicated TYPE.  */

tree
build_target_expr_with_type (tree init, tree type, tsubst_flags_t complain)
{
  gcc_assert (!VOID_TYPE_P (type));
  gcc_assert (!VOID_TYPE_P (TREE_TYPE (init)));

  if (TREE_CODE (init) == TARGET_EXPR
      || init == error_mark_node)
    return init;
  else if (CLASS_TYPE_P (type) && type_has_nontrivial_copy_init (type)
	   && TREE_CODE (init) != COND_EXPR
	   && TREE_CODE (init) != CONSTRUCTOR
	   && TREE_CODE (init) != VA_ARG_EXPR
	   && TREE_CODE (init) != CALL_EXPR)
    /* We need to build up a copy constructor call.  COND_EXPR is a special
       case because we already have copies on the arms and we don't want
       another one here.  A CONSTRUCTOR is aggregate initialization, which
       is handled separately.  A VA_ARG_EXPR is magic creation of an
       aggregate; there's no additional work to be done.  A CALL_EXPR
       already creates a prvalue.  */
    return force_rvalue (init, complain);

  return force_target_expr (type, init, complain);
}

/* Like the above function, but without the checking.  This function should
   only be used by code which is deliberately trying to subvert the type
   system, such as call_builtin_trap.  Or build_over_call, to avoid
   infinite recursion.  */

tree
force_target_expr (tree type, tree init, tsubst_flags_t complain)
{
  tree slot;

  gcc_assert (!VOID_TYPE_P (type));

  slot = build_local_temp (type);
  return build_target_expr (slot, init, complain);
}

/* Like build_target_expr_with_type, but use the type of INIT.  */

tree
get_target_expr (tree init, tsubst_flags_t complain /* = tf_warning_or_error */)
{
  if (TREE_CODE (init) == AGGR_INIT_EXPR)
    return build_target_expr (AGGR_INIT_EXPR_SLOT (init), init, complain);
  else if (TREE_CODE (init) == VEC_INIT_EXPR)
    return build_target_expr (VEC_INIT_EXPR_SLOT (init), init, complain);
  else
    {
      init = convert_bitfield_to_declared_type (init);
      return build_target_expr_with_type (init, TREE_TYPE (init), complain);
    }
}

/* If EXPR is a bitfield reference, convert it to the declared type of
   the bitfield, and return the resulting expression.  Otherwise,
   return EXPR itself.  */

tree
convert_bitfield_to_declared_type (tree expr)
{
  tree bitfield_type;

  bitfield_type = is_bitfield_expr_with_lowered_type (expr);
  if (bitfield_type)
    expr = convert_to_integer_nofold (TYPE_MAIN_VARIANT (bitfield_type),
				      expr);
  return expr;
}

/* EXPR is being used in an rvalue context.  Return a version of EXPR
   that is marked as an rvalue.  */

tree
rvalue (tree expr)
{
  tree type;

  if (error_operand_p (expr))
    return expr;

  expr = mark_rvalue_use (expr);

  /* [expr.type]: "If a prvalue initially has the type "cv T", where T is a
     cv-unqualified non-class, non-array type, the type of the expression is
     adjusted to T prior to any further analysis.  */
  type = TREE_TYPE (expr);
  if (!CLASS_TYPE_P (type) && TREE_CODE (type) != ARRAY_TYPE
      && cv_qualified_p (type))
    type = cv_unqualified (type);

  /* We need to do this for rvalue refs as well to get the right answer
     from decltype; see c++/36628.  */
  if (!processing_template_decl && glvalue_p (expr))
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


struct cplus_array_info
{
  tree type;
  tree domain;
};

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

/* Hash table containing dependent array types, which are unsuitable for
   the language-independent type hash table.  */
static GTY (()) hash_table<cplus_array_hasher> *cplus_array_htab;

/* Build an ARRAY_TYPE without laying it out.  */

static tree
build_min_array_type (tree elt_type, tree index_type)
{
  tree t = cxx_make_type (ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;
  return t;
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
				index_type
				? TYPE_CANONICAL (index_type) : index_type,
				dep);
  else
    TYPE_CANONICAL (t) = t;
}

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
    dependent = (uses_template_parms (elt_type)
		 || (index_type && uses_template_parms (index_type)));

  if (elt_type != TYPE_MAIN_VARIANT (elt_type))
    /* Start with an array of the TYPE_MAIN_VARIANT.  */
    t = build_cplus_array_type (TYPE_MAIN_VARIANT (elt_type),
				index_type, dependent);
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
	if (TREE_TYPE (t) == elt_type
	    && TYPE_NAME (t) == NULL_TREE
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
  bool needs_ctor = (TYPE_NEEDS_CONSTRUCTING (t)
		     = TYPE_NEEDS_CONSTRUCTING (elt_type));
  bool needs_dtor = (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t)
		     = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (elt_type));

  if (!dependent && t == TYPE_MAIN_VARIANT (t)
      && !COMPLETE_TYPE_P (t) && COMPLETE_TYPE_P (elt_type))
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

/* Return an ARRAY_TYPE with element type ELT and length N.  */

tree
build_array_of_n_type (tree elt, int n)
{
  return build_cplus_array_type (elt, build_index_type (size_int (n - 1)));
}

/* True iff T is an array of unknown bound.  */

bool
array_of_unknown_bound_p (const_tree t)
{
  return (TREE_CODE (t) == ARRAY_TYPE
	  && !TYPE_DOMAIN (t));
}

/* True iff T is an N3639 array of runtime bound (VLA).  These were approved
   for C++14 but then removed.  This should only be used for N3639
   specifically; code wondering more generally if something is a VLA should use
   vla_type_p.  */

bool
array_of_runtime_bound_p (tree t)
{
  if (!t || TREE_CODE (t) != ARRAY_TYPE)
    return false;
  if (variably_modified_type_p (TREE_TYPE (t), NULL_TREE))
    return false;
  tree dom = TYPE_DOMAIN (t);
  if (!dom)
    return false;
  tree max = TYPE_MAX_VALUE (dom);
  return (!potential_rvalue_constant_expression (max)
	  || (!value_dependent_expression_p (max) && !TREE_CONSTANT (max)));
}

/* True iff T is a variable length array.  */

bool
vla_type_p (tree t)
{
  for (; t && TREE_CODE (t) == ARRAY_TYPE;
       t = TREE_TYPE (t))
    if (tree dom = TYPE_DOMAIN (t))
      {
	tree max = TYPE_MAX_VALUE (dom);
	if (!potential_rvalue_constant_expression (max)
	    || (!value_dependent_expression_p (max) && !TREE_CONSTANT (max)))
	  return true;
      }
  return false;
}


/* Return a reference type node of MODE referring to TO_TYPE.  If MODE
   is VOIDmode the standard pointer mode will be picked.  If RVAL is
   true, return an rvalue reference type, otherwise return an lvalue
   reference type.  If a type node exists, reuse it, otherwise create
   a new one.  */
tree
cp_build_reference_type_for_mode (tree to_type, machine_mode mode, bool rval)
{
  tree lvalue_ref, t;

  if (to_type == error_mark_node)
    return error_mark_node;

  if (TYPE_REF_P (to_type))
    {
      rval = rval && TYPE_REF_IS_RVALUE (to_type);
      to_type = TREE_TYPE (to_type);
    }

  lvalue_ref = build_reference_type_for_mode (to_type, mode, false);

  if (!rval)
    return lvalue_ref;

  /* This code to create rvalue reference types is based on and tied
     to the code creating lvalue reference types in the middle-end
     functions build_reference_type_for_mode and build_reference_type.

     It works by putting the rvalue reference type nodes after the
     lvalue reference nodes in the TYPE_NEXT_REF_TO linked list, so
     they will effectively be ignored by the middle end.  */

  for (t = lvalue_ref; (t = TYPE_NEXT_REF_TO (t)); )
    if (TYPE_REF_IS_RVALUE (t))
      return t;

  t = build_distinct_type_copy (lvalue_ref);

  TYPE_REF_IS_RVALUE (t) = true;
  TYPE_NEXT_REF_TO (t) = TYPE_NEXT_REF_TO (lvalue_ref);
  TYPE_NEXT_REF_TO (lvalue_ref) = t;

  if (TYPE_STRUCTURAL_EQUALITY_P (to_type))
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (TYPE_CANONICAL (to_type) != to_type)
    TYPE_CANONICAL (t)
      = cp_build_reference_type_for_mode (TYPE_CANONICAL (to_type), mode, rval);
  else
    TYPE_CANONICAL (t) = t;

  layout_type (t);

  return t;

}

/* Return a reference type node referring to TO_TYPE.  If RVAL is
   true, return an rvalue reference type, otherwise return an lvalue
   reference type.  If a type node exists, reuse it, otherwise create
   a new one.  */
tree
cp_build_reference_type (tree to_type, bool rval)
{
  return cp_build_reference_type_for_mode (to_type, VOIDmode, rval);
}

/* Returns EXPR cast to rvalue reference type, like std::move.  */

tree
move (tree expr)
{
  tree type = TREE_TYPE (expr);
  gcc_assert (!TYPE_REF_P (type));
  if (xvalue_p (expr))
    return expr;
  type = cp_build_reference_type (type, /*rval*/true);
  return build_static_cast (input_location, type, expr,
			    tf_warning_or_error);
}

/* Used by the C++ front end to build qualified array types.  However,
   the C version of this function does not properly maintain canonical
   types (which are not used in C).  */
tree
c_build_qualified_type (tree type, int type_quals, tree /* orig_qual_type */,
			size_t /* orig_qual_indirect */)
{
  return cp_build_qualified_type (type, type_quals);
}


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
cp_build_qualified_type (tree type, int type_quals,
			 tsubst_flags_t complain /* = tf_warning_or_error */)
{
  tree result;
  int bad_quals = TYPE_UNQUALIFIED;

  if (type == error_mark_node)
    return type;

  if (type_quals == cp_type_quals (type))
    return type;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* In C++, the qualification really applies to the array element
	 type.  Obtain the appropriately qualified element type.  */
      tree t;
      tree element_type
	= cp_build_qualified_type (TREE_TYPE (type), type_quals, complain);

      if (element_type == error_mark_node)
	return error_mark_node;

      /* See if we already have an identically qualified type.  Tests
	 should be equivalent to those in check_qualified_type.  */
      for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	if (TREE_TYPE (t) == element_type
	    && TYPE_NAME (t) == TYPE_NAME (type)
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
				      ? int (TYPE_DEPENDENT_P (type)) : -1);

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
  else if (TREE_CODE (type) == TYPE_PACK_EXPANSION)
    {
      tree t = PACK_EXPANSION_PATTERN (type);

      t = cp_build_qualified_type (t, type_quals, complain);
      return make_pack_expansion (t, complain);
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
      && (TYPE_REF_P (type)
	  || FUNC_OR_METHOD_TYPE_P (type)))
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
  if ((type_quals & TYPE_QUAL_RESTRICT)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && TREE_CODE (type) != TYPENAME_TYPE
      && !INDIRECT_TYPE_P (type))
    {
      bad_quals |= TYPE_QUAL_RESTRICT;
      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (bad_quals == TYPE_UNQUALIFIED
      || (complain & tf_ignore_bad_quals))
    /*OK*/;
  else if (!(complain & tf_error))
    return error_mark_node;
  else
    {
      tree bad_type = build_qualified_type (ptr_type_node, bad_quals);
      error ("%qV qualifiers cannot be applied to %qT",
	     bad_type, type);
    }

  /* Retrieve (or create) the appropriately qualified variant.  */
  result = build_qualified_type (type, type_quals);

  return result;
}

/* Return TYPE with const and volatile removed.  */

tree
cv_unqualified (tree type)
{
  int quals;

  if (type == error_mark_node)
    return type;

  quals = cp_type_quals (type);
  quals &= ~(TYPE_QUAL_CONST|TYPE_QUAL_VOLATILE);
  return cp_build_qualified_type (type, quals);
}

/* Subroutine of strip_typedefs.  We want to apply to RESULT the attributes
   from ATTRIBS that affect type identity, and no others.  If any are not
   applied, set *remove_attributes to true.  */

static tree
apply_identity_attributes (tree result, tree attribs, bool *remove_attributes)
{
  tree first_ident = NULL_TREE;
  tree new_attribs = NULL_TREE;
  tree *p = &new_attribs;

  if (OVERLOAD_TYPE_P (result))
    {
      /* On classes and enums all attributes are ingrained.  */
      gcc_assert (attribs == TYPE_ATTRIBUTES (result));
      return result;
    }

  for (tree a = attribs; a; a = TREE_CHAIN (a))
    {
      const attribute_spec *as
	= lookup_attribute_spec (get_attribute_name (a));
      if (as && as->affects_type_identity)
	{
	  if (!first_ident)
	    first_ident = a;
	  else if (first_ident == error_mark_node)
	    {
	      *p = tree_cons (TREE_PURPOSE (a), TREE_VALUE (a), NULL_TREE);
	      p = &TREE_CHAIN (*p);
	    }
	}
      else if (first_ident && first_ident != error_mark_node)
	{
	  for (tree a2 = first_ident; a2 != a; a2 = TREE_CHAIN (a2))
	    {
	      *p = tree_cons (TREE_PURPOSE (a2), TREE_VALUE (a2), NULL_TREE);
	      p = &TREE_CHAIN (*p);
	    }
	  first_ident = error_mark_node;
	}
    }
  if (first_ident != error_mark_node)
    new_attribs = first_ident;

  if (first_ident == attribs)
    /* All attributes affected type identity.  */;
  else
    *remove_attributes = true;

  return cp_build_type_attribute_variant (result, new_attribs);
}

/* Builds a qualified variant of T that is either not a typedef variant
   (the default behavior) or not a typedef variant of a user-facing type
   (if FLAGS contains STF_USER_FACING).  If T is not a type, then this
   just dispatches to strip_typedefs_expr.

   E.g. consider the following declarations:
     typedef const int ConstInt;
     typedef ConstInt* PtrConstInt;
   If T is PtrConstInt, this function returns a type representing
     const int*.
   In other words, if T is a typedef, the function returns the underlying type.
   The cv-qualification and attributes of the type returned match the
   input type.
   They will always be compatible types.
   The returned type is built so that all of its subtypes
   recursively have their typedefs stripped as well.

   This is different from just returning TYPE_CANONICAL (T)
   Because of several reasons:
    * If T is a type that needs structural equality
      its TYPE_CANONICAL (T) will be NULL.
    * TYPE_CANONICAL (T) desn't carry type attributes
      and loses template parameter names.

   If REMOVE_ATTRIBUTES is non-null, also strip attributes that don't
   affect type identity, and set the referent to true if any were
   stripped.  */

tree
strip_typedefs (tree t, bool *remove_attributes /* = NULL */,
		unsigned int flags /* = 0 */)
{
  tree result = NULL, type = NULL, t0 = NULL;

  if (!t || t == error_mark_node)
    return t;

  if (!TYPE_P (t))
    return strip_typedefs_expr (t, remove_attributes, flags);

  if (t == TYPE_CANONICAL (t))
    return t;

  if (typedef_variant_p (t))
    {
      if ((flags & STF_USER_VISIBLE)
	  && !user_facing_original_type_p (t))
	return t;

      if (dependent_opaque_alias_p (t))
	return t;

      if (alias_template_specialization_p (t, nt_opaque))
	{
	  if (dependent_alias_template_spec_p (t, nt_opaque)
	      && !(flags & STF_STRIP_DEPENDENT))
	    /* DR 1558: However, if the template-id is dependent, subsequent
	       template argument substitution still applies to the template-id.  */
	    return t;
	}
      else
	/* If T is a non-template alias or typedef, we can assume that
	   instantiating its definition will hit any substitution failure,
	   so we don't need to retain it here as well.  */
	flags |= STF_STRIP_DEPENDENT;

      result = strip_typedefs (DECL_ORIGINAL_TYPE (TYPE_NAME (t)),
			       remove_attributes, flags);
      goto stripped;
    }

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      type = strip_typedefs (TREE_TYPE (t), remove_attributes, flags);
      result = build_pointer_type_for_mode (type, TYPE_MODE (t), false);
      break;
    case REFERENCE_TYPE:
      type = strip_typedefs (TREE_TYPE (t), remove_attributes, flags);
      result = cp_build_reference_type_for_mode (type, TYPE_MODE (t), TYPE_REF_IS_RVALUE (t));
      break;
    case OFFSET_TYPE:
      t0 = strip_typedefs (TYPE_OFFSET_BASETYPE (t), remove_attributes, flags);
      type = strip_typedefs (TREE_TYPE (t), remove_attributes, flags);
      result = build_offset_type (t0, type);
      break;
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  t0 = strip_typedefs (TYPE_PTRMEMFUNC_FN_TYPE (t),
			       remove_attributes, flags);
	  result = build_ptrmemfunc_type (t0);
	}
      break;
    case ARRAY_TYPE:
      type = strip_typedefs (TREE_TYPE (t), remove_attributes, flags);
      t0  = strip_typedefs (TYPE_DOMAIN (t), remove_attributes, flags);
      gcc_checking_assert (TYPE_DEPENDENT_P_VALID (t)
			   || !dependent_type_p (t));
      result = build_cplus_array_type (type, t0, TYPE_DEPENDENT_P (t));
      break;
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree arg_types = NULL, arg_node, arg_node2, arg_type;
	bool changed;

	/* Because we stomp on TREE_PURPOSE of TYPE_ARG_TYPES in many places
	   around the compiler (e.g. cp_parser_late_parsing_default_args), we
	   can't expect that re-hashing a function type will find a previous
	   equivalent type, so try to reuse the input type if nothing has
	   changed.  If the type is itself a variant, that will change.  */
	bool is_variant = typedef_variant_p (t);
	if (remove_attributes
	    && (TYPE_ATTRIBUTES (t) || TYPE_USER_ALIGN (t)))
	  is_variant = true;

	type = strip_typedefs (TREE_TYPE (t), remove_attributes, flags);
	tree canon_spec = (flag_noexcept_type
			   ? canonical_eh_spec (TYPE_RAISES_EXCEPTIONS (t))
			   : NULL_TREE);
	changed = (type != TREE_TYPE (t) || is_variant
		   || TYPE_RAISES_EXCEPTIONS (t) != canon_spec);

	for (arg_node = TYPE_ARG_TYPES (t);
	     arg_node;
	     arg_node = TREE_CHAIN (arg_node))
	  {
	    if (arg_node == void_list_node)
	      break;
	    arg_type = strip_typedefs (TREE_VALUE (arg_node),
				       remove_attributes, flags);
	    gcc_assert (arg_type);
	    if (arg_type == TREE_VALUE (arg_node) && !changed)
	      continue;

	    if (!changed)
	      {
		changed = true;
		for (arg_node2 = TYPE_ARG_TYPES (t);
		     arg_node2 != arg_node;
		     arg_node2 = TREE_CHAIN (arg_node2))
		  arg_types
		    = tree_cons (TREE_PURPOSE (arg_node2),
				 TREE_VALUE (arg_node2), arg_types);
	      }

	    arg_types
	      = tree_cons (TREE_PURPOSE (arg_node), arg_type, arg_types);
	  }

	if (!changed)
	  return t;

	if (arg_types)
	  arg_types = nreverse (arg_types);

	/* A list of parameters not ending with an ellipsis
	   must end with void_list_node.  */
	if (arg_node)
	  arg_types = chainon (arg_types, void_list_node);

	if (TREE_CODE (t) == METHOD_TYPE)
	  {
	    tree class_type = TREE_TYPE (TREE_VALUE (arg_types));
	    gcc_assert (class_type);
	    result =
	      build_method_type_directly (class_type, type,
					  TREE_CHAIN (arg_types));
	  }
	else
	  {
	    result = build_function_type (type, arg_types);
	    result = apply_memfn_quals (result, type_memfn_quals (t));
	  }

	result = build_cp_fntype_variant (result,
					  type_memfn_rqual (t), canon_spec,
					  TYPE_HAS_LATE_RETURN_TYPE (t));
      }
      break;
    case TYPENAME_TYPE:
      {
	bool changed = false;
	tree fullname = TYPENAME_TYPE_FULLNAME (t);
	if (TREE_CODE (fullname) == TEMPLATE_ID_EXPR
	    && TREE_OPERAND (fullname, 1))
	  {
	    tree args = TREE_OPERAND (fullname, 1);
	    tree new_args = copy_node (args);
	    for (int i = 0; i < TREE_VEC_LENGTH (args); ++i)
	      {
		tree arg = TREE_VEC_ELT (args, i);
		tree strip_arg = strip_typedefs (arg, remove_attributes, flags);
		TREE_VEC_ELT (new_args, i) = strip_arg;
		if (strip_arg != arg)
		  changed = true;
	      }
	    if (changed)
	      {
		NON_DEFAULT_TEMPLATE_ARGS_COUNT (new_args)
		  = NON_DEFAULT_TEMPLATE_ARGS_COUNT (args);
		fullname
		  = lookup_template_function (TREE_OPERAND (fullname, 0),
					      new_args);
	      }
	    else
	      ggc_free (new_args);
	  }
	tree ctx = strip_typedefs (TYPE_CONTEXT (t), remove_attributes, flags);
	if (!changed && ctx == TYPE_CONTEXT (t) && !typedef_variant_p (t))
	  return t;
	tree name = fullname;
	if (TREE_CODE (fullname) == TEMPLATE_ID_EXPR)
	  name = TREE_OPERAND (fullname, 0);
	/* Use build_typename_type rather than make_typename_type because we
	   don't want to resolve it here, just strip typedefs.  */
	result = build_typename_type (ctx, name, fullname, typename_type);
      }
      break;
    case DECLTYPE_TYPE:
      result = strip_typedefs_expr (DECLTYPE_TYPE_EXPR (t),
				    remove_attributes, flags);
      if (result == DECLTYPE_TYPE_EXPR (t))
	result = NULL_TREE;
      else
	result = (finish_decltype_type
		  (result,
		   DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (t),
		   tf_none));
      break;
    case TRAIT_TYPE:
      {
	tree type1 = strip_typedefs (TRAIT_TYPE_TYPE1 (t),
				     remove_attributes, flags);
	tree type2 = strip_typedefs (TRAIT_TYPE_TYPE2 (t),
				     remove_attributes, flags);
	if (type1 == TRAIT_TYPE_TYPE1 (t) && type2 == TRAIT_TYPE_TYPE2 (t))
	  result = NULL_TREE;
	else
	  result = finish_trait_type (TRAIT_TYPE_KIND (t), type1, type2,
				      tf_warning_or_error);
      }
      break;
    case TYPE_PACK_EXPANSION:
      {
	tree pat = PACK_EXPANSION_PATTERN (t);
	if (TYPE_P (pat))
	  {
	    type = strip_typedefs (pat, remove_attributes, flags);
	    if (type != pat)
	      {
		result = build_distinct_type_copy (t);
		PACK_EXPANSION_PATTERN (result) = type;
	      }
	  }
      }
      break;
    default:
      break;
    }

  if (!result)
    result = TYPE_MAIN_VARIANT (t);

stripped:
  /*gcc_assert (!typedef_variant_p (result)
	      || dependent_alias_template_spec_p (result, nt_opaque)
	      || ((flags & STF_USER_VISIBLE)
		  && !user_facing_original_type_p (result)));*/

  if (COMPLETE_TYPE_P (result) && !COMPLETE_TYPE_P (t))
  /* If RESULT is complete and T isn't, it's likely the case that T
     is a variant of RESULT which hasn't been updated yet.  Skip the
     attribute handling.  */;
  else
    {
      if (TYPE_USER_ALIGN (t) != TYPE_USER_ALIGN (result)
	  || TYPE_ALIGN (t) != TYPE_ALIGN (result))
	{
	  gcc_assert (TYPE_USER_ALIGN (t));
	  if (remove_attributes)
	    *remove_attributes = true;
	  else
	    {
	      if (TYPE_ALIGN (t) == TYPE_ALIGN (result))
		result = build_variant_type_copy (result);
	      else
		result = build_aligned_type (result, TYPE_ALIGN (t));
	      TYPE_USER_ALIGN (result) = true;
	    }
	}

      if (TYPE_ATTRIBUTES (t))
	{
	  if (remove_attributes)
	    result = apply_identity_attributes (result, TYPE_ATTRIBUTES (t),
						remove_attributes);
	  else
	    result = cp_build_type_attribute_variant (result,
						      TYPE_ATTRIBUTES (t));
	}
    }

  return cp_build_qualified_type (result, cp_type_quals (t));
}

/* Like strip_typedefs above, but works on expressions (and other
   non-types such as TREE_VEC), so that in

   template<class T> struct A
   {
     typedef T TT;
     B<sizeof(TT)> b;
   };

   sizeof(TT) is replaced by sizeof(T).  */

tree
strip_typedefs_expr (tree t, bool *remove_attributes, unsigned int flags)
{
  unsigned i,n;
  tree r, type, *ops;
  enum tree_code code;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  STRIP_ANY_LOCATION_WRAPPER (t);

  if (DECL_P (t) || CONSTANT_CLASS_P (t))
    return t;

  code = TREE_CODE (t);
  switch (code)
    {
    case IDENTIFIER_NODE:
    case TEMPLATE_PARM_INDEX:
    case OVERLOAD:
    case BASELINK:
    case ARGUMENT_PACK_SELECT:
      return t;

    case TRAIT_EXPR:
      {
	tree type1 = strip_typedefs (TRAIT_EXPR_TYPE1 (t),
				     remove_attributes, flags);
	tree type2 = strip_typedefs (TRAIT_EXPR_TYPE2 (t),
				     remove_attributes, flags);
	if (type1 == TRAIT_EXPR_TYPE1 (t)
	    && type2 == TRAIT_EXPR_TYPE2 (t))
	  return t;
	r = copy_node (t);
	TRAIT_EXPR_TYPE1 (r) = type1;
	TRAIT_EXPR_TYPE2 (r) = type2;
	return r;
      }

    case TREE_LIST:
      {
	bool changed = false;
	auto_vec<tree_pair, 4> vec;
	r = t;
	for (; t; t = TREE_CHAIN (t))
	  {
	    tree purpose = strip_typedefs (TREE_PURPOSE (t),
					   remove_attributes, flags);
	    tree value = strip_typedefs (TREE_VALUE (t),
					 remove_attributes, flags);
	    if (purpose != TREE_PURPOSE (t) || value != TREE_VALUE (t))
	      changed = true;
	    vec.safe_push ({purpose, value});
	  }
	if (changed)
	  {
	    r = NULL_TREE;
	    for (int i = vec.length () - 1; i >= 0; i--)
	      r = tree_cons (vec[i].first, vec[i].second, r);
	  }
	return r;
      }

    case TREE_VEC:
      {
	bool changed = false;
	releasing_vec vec;
	n = TREE_VEC_LENGTH (t);
	vec_safe_reserve (vec, n);
	for (i = 0; i < n; ++i)
	  {
	    tree op = strip_typedefs (TREE_VEC_ELT (t, i),
				      remove_attributes, flags);
	    vec->quick_push (op);
	    if (op != TREE_VEC_ELT (t, i))
	      changed = true;
	  }
	if (changed)
	  {
	    r = copy_node (t);
	    for (i = 0; i < n; ++i)
	      TREE_VEC_ELT (r, i) = (*vec)[i];
	    NON_DEFAULT_TEMPLATE_ARGS_COUNT (r)
	      = NON_DEFAULT_TEMPLATE_ARGS_COUNT (t);
	  }
	else
	  r = t;
	return r;
      }

    case CONSTRUCTOR:
      {
	bool changed = false;
	vec<constructor_elt, va_gc> *vec
	  = vec_safe_copy (CONSTRUCTOR_ELTS (t));
	n = CONSTRUCTOR_NELTS (t);
	type = strip_typedefs (TREE_TYPE (t), remove_attributes, flags);
	for (i = 0; i < n; ++i)
	  {
	    constructor_elt *e = &(*vec)[i];
	    tree op = strip_typedefs (e->value, remove_attributes, flags);
	    if (op != e->value)
	      {
		changed = true;
		e->value = op;
	      }
	    gcc_checking_assert
	      (e->index == strip_typedefs (e->index, remove_attributes,
					   flags));
	  }

	if (!changed && type == TREE_TYPE (t))
	  {
	    vec_free (vec);
	    return t;
	  }
	else
	  {
	    r = copy_node (t);
	    TREE_TYPE (r) = type;
	    CONSTRUCTOR_ELTS (r) = vec;
	    return r;
	  }
      }

    case LAMBDA_EXPR:
    case STMT_EXPR:
      return t;

    default:
      break;
    }

  gcc_assert (EXPR_P (t));

  n = cp_tree_operand_length (t);
  ops = XALLOCAVEC (tree, n);
  type = TREE_TYPE (t);

  switch (code)
    {
    CASE_CONVERT:
    case IMPLICIT_CONV_EXPR:
    case DYNAMIC_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case CONST_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CAST_EXPR:
    case NEW_EXPR:
      type = strip_typedefs (type, remove_attributes, flags);
      /* fallthrough */

    default:
      for (i = 0; i < n; ++i)
	ops[i] = strip_typedefs (TREE_OPERAND (t, i),
				 remove_attributes, flags);
      break;
    }

  /* If nothing changed, return t.  */
  for (i = 0; i < n; ++i)
    if (ops[i] != TREE_OPERAND (t, i))
      break;
  if (i == n && type == TREE_TYPE (t))
    return t;

  r = copy_node (t);
  TREE_TYPE (r) = type;
  for (i = 0; i < n; ++i)
    TREE_OPERAND (r, i) = ops[i];
  return r;
}

/* Makes a copy of BINFO and TYPE, which is to be inherited into a
   graph dominated by T.  If BINFO is NULL, TYPE is a dependent base,
   and we do a shallow copy.  If BINFO is non-NULL, we do a deep copy.
   VIRT indicates whether TYPE is inherited virtually or not.
   IGO_PREV points at the previous binfo of the inheritance graph
   order chain.  The newly copied binfo's TREE_CHAIN forms this
   ordering.

   The CLASSTYPE_VBASECLASSES vector of T is constructed in the
   correct order. That is in the order the bases themselves should be
   constructed in.

   The BINFO_INHERITANCE of a virtual base class points to the binfo
   of the most derived type. ??? We could probably change this so that
   BINFO_INHERITANCE becomes synonymous with BINFO_PRIMARY, and hence
   remove a field.  They currently can only differ for primary virtual
   virtual bases.  */

tree
copy_binfo (tree binfo, tree type, tree t, tree *igo_prev, int virt)
{
  tree new_binfo;

  if (virt)
    {
      /* See if we've already made this virtual base.  */
      new_binfo = binfo_for_vbase (type, t);
      if (new_binfo)
	return new_binfo;
    }

  new_binfo = make_tree_binfo (binfo ? BINFO_N_BASE_BINFOS (binfo) : 0);
  BINFO_TYPE (new_binfo) = type;

  /* Chain it into the inheritance graph.  */
  TREE_CHAIN (*igo_prev) = new_binfo;
  *igo_prev = new_binfo;

  if (binfo && !BINFO_DEPENDENT_BASE_P (binfo))
    {
      int ix;
      tree base_binfo;

      gcc_assert (SAME_BINFO_TYPE_P (BINFO_TYPE (binfo), type));

      BINFO_OFFSET (new_binfo) = BINFO_OFFSET (binfo);
      BINFO_VIRTUALS (new_binfo) = BINFO_VIRTUALS (binfo);

      /* We do not need to copy the accesses, as they are read only.  */
      BINFO_BASE_ACCESSES (new_binfo) = BINFO_BASE_ACCESSES (binfo);

      /* Recursively copy base binfos of BINFO.  */
      for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
	{
	  tree new_base_binfo;
	  new_base_binfo = copy_binfo (base_binfo, BINFO_TYPE (base_binfo),
				       t, igo_prev,
				       BINFO_VIRTUAL_P (base_binfo));

	  if (!BINFO_INHERITANCE_CHAIN (new_base_binfo))
	    BINFO_INHERITANCE_CHAIN (new_base_binfo) = new_binfo;
	  BINFO_BASE_APPEND (new_binfo, new_base_binfo);
	}
    }
  else
    BINFO_DEPENDENT_BASE_P (new_binfo) = 1;

  if (virt)
    {
      /* Push it onto the list after any virtual bases it contains
	 will have been pushed.  */
      CLASSTYPE_VBASECLASSES (t)->quick_push (new_binfo);
      BINFO_VIRTUAL_P (new_binfo) = 1;
      BINFO_INHERITANCE_CHAIN (new_binfo) = TYPE_BINFO (t);
    }

  return new_binfo;
}

/* Hashing of lists so that we don't make duplicates.
   The entry point is `list_hash_canon'.  */

struct list_proxy
{
  tree purpose;
  tree value;
  tree chain;
};

struct list_hasher : ggc_ptr_hash<tree_node>
{
  typedef list_proxy *compare_type;

  static hashval_t hash (tree);
  static bool equal (tree, list_proxy *);
};

/* Now here is the hash table.  When recording a list, it is added
   to the slot whose index is the hash code mod the table size.
   Note that the hash table is used for several kinds of lists.
   While all these live in the same table, they are completely independent,
   and the hash code is computed differently for each of these.  */

static GTY (()) hash_table<list_hasher> *list_hash_table;

/* Compare ENTRY (an entry in the hash table) with DATA (a list_proxy
   for a node we are thinking about adding).  */

bool
list_hasher::equal (tree t, list_proxy *proxy)
{
  return (TREE_VALUE (t) == proxy->value
	  && TREE_PURPOSE (t) == proxy->purpose
	  && TREE_CHAIN (t) == proxy->chain);
}

/* Compute a hash code for a list (chain of TREE_LIST nodes
   with goodies in the TREE_PURPOSE, TREE_VALUE, and bits of the
   TREE_COMMON slots), by adding the hash codes of the individual entries.  */

static hashval_t
list_hash_pieces (tree purpose, tree value, tree chain)
{
  hashval_t hashcode = 0;

  if (chain)
    hashcode += TREE_HASH (chain);

  if (value)
    hashcode += TREE_HASH (value);
  else
    hashcode += 1007;
  if (purpose)
    hashcode += TREE_HASH (purpose);
  else
    hashcode += 1009;
  return hashcode;
}

/* Hash an already existing TREE_LIST.  */

hashval_t
list_hasher::hash (tree t)
{
  return list_hash_pieces (TREE_PURPOSE (t),
			   TREE_VALUE (t),
			   TREE_CHAIN (t));
}

/* Given list components PURPOSE, VALUE, AND CHAIN, return the canonical
   object for an identical list if one already exists.  Otherwise, build a
   new one, and record it as the canonical object.  */

tree
hash_tree_cons (tree purpose, tree value, tree chain)
{
  int hashcode = 0;
  tree *slot;
  struct list_proxy proxy;

  /* Hash the list node.  */
  hashcode = list_hash_pieces (purpose, value, chain);
  /* Create a proxy for the TREE_LIST we would like to create.  We
     don't actually create it so as to avoid creating garbage.  */
  proxy.purpose = purpose;
  proxy.value = value;
  proxy.chain = chain;
  /* See if it is already in the table.  */
  slot = list_hash_table->find_slot_with_hash (&proxy, hashcode, INSERT);
  /* If not, create a new node.  */
  if (!*slot)
    *slot = tree_cons (purpose, value, chain);
  return (tree) *slot;
}

/* Constructor for hashed lists.  */

tree
hash_tree_chain (tree value, tree chain)
{
  return hash_tree_cons (NULL_TREE, value, chain);
}

void
debug_binfo (tree elem)
{
  HOST_WIDE_INT n;
  tree virtuals;

  fprintf (stderr, "type \"%s\", offset = " HOST_WIDE_INT_PRINT_DEC
	   "\nvtable type:\n",
	   TYPE_NAME_STRING (BINFO_TYPE (elem)),
	   TREE_INT_CST_LOW (BINFO_OFFSET (elem)));
  debug_tree (BINFO_TYPE (elem));
  if (BINFO_VTABLE (elem))
    fprintf (stderr, "vtable decl \"%s\"\n",
	     IDENTIFIER_POINTER (DECL_NAME (get_vtbl_decl_for_binfo (elem))));
  else
    fprintf (stderr, "no vtable decl yet\n");
  fprintf (stderr, "virtuals:\n");
  virtuals = BINFO_VIRTUALS (elem);
  n = 0;

  while (virtuals)
    {
      tree fndecl = TREE_VALUE (virtuals);
      fprintf (stderr, "%s [" HOST_WIDE_INT_PRINT_DEC " =? "
		       HOST_WIDE_INT_PRINT_DEC "]\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl)),
	       n, TREE_INT_CST_LOW (DECL_VINDEX (fndecl)));
      ++n;
      virtuals = TREE_CHAIN (virtuals);
    }
}

/* Build a representation for the qualified name SCOPE::NAME.  TYPE is
   the type of the result expression, if known, or NULL_TREE if the
   resulting expression is type-dependent.  If TEMPLATE_P is true,
   NAME is known to be a template because the user explicitly used the
   "template" keyword after the "::".

   All SCOPE_REFs should be built by use of this function.  */

tree
build_qualified_name (tree type, tree scope, tree name, bool template_p)
{
  tree t;
  if (type == error_mark_node
      || scope == error_mark_node
      || name == error_mark_node)
    return error_mark_node;
  gcc_assert (TREE_CODE (name) != SCOPE_REF);
  t = build2 (SCOPE_REF, type, scope, name);
  QUALIFIED_NAME_IS_TEMPLATE (t) = template_p;
  PTRMEM_OK_P (t) = true;
  if (type)
    t = convert_from_reference (t);
  return t;
}

/* Like check_qualified_type, but also check ref-qualifier, exception
   specification, and whether the return type was specified after the
   parameters.  */

static bool
cp_check_qualified_type (const_tree cand, const_tree base, int type_quals,
			 cp_ref_qualifier rqual, tree raises, bool late)
{
  return (TYPE_QUALS (cand) == type_quals
	  && check_base_type (cand, base)
	  && comp_except_specs (raises, TYPE_RAISES_EXCEPTIONS (cand),
				ce_exact)
	  && TYPE_HAS_LATE_RETURN_TYPE (cand) == late
	  && type_memfn_rqual (cand) == rqual);
}

/* Build the FUNCTION_TYPE or METHOD_TYPE with the ref-qualifier RQUAL.  */

tree
build_ref_qualified_type (tree type, cp_ref_qualifier rqual)
{
  tree raises = TYPE_RAISES_EXCEPTIONS (type);
  bool late = TYPE_HAS_LATE_RETURN_TYPE (type);
  return build_cp_fntype_variant (type, rqual, raises, late);
}

tree
make_binding_vec (tree name, unsigned clusters MEM_STAT_DECL)
{
  /* Stored in an unsigned short, but we're limited to the number of
     modules anyway.  */
  gcc_checking_assert (clusters <= (unsigned short)(~0));
  size_t length = (offsetof (tree_binding_vec, vec)
		   + clusters * sizeof (binding_cluster));
  tree vec = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);
  TREE_SET_CODE (vec, BINDING_VECTOR);
  BINDING_VECTOR_NAME (vec) = name;
  BINDING_VECTOR_ALLOC_CLUSTERS (vec) = clusters;
  BINDING_VECTOR_NUM_CLUSTERS (vec) = 0;

  return vec;
}

/* Make a raw overload node containing FN.  */

tree
ovl_make (tree fn, tree next)
{
  tree result = make_node (OVERLOAD);

  if (TREE_CODE (fn) == OVERLOAD)
    OVL_NESTED_P (result) = true;

  TREE_TYPE (result) = (next || TREE_CODE (fn) == TEMPLATE_DECL
			? unknown_type_node : TREE_TYPE (fn));
  if (next && TREE_CODE (next) == OVERLOAD && OVL_DEDUP_P (next))
    OVL_DEDUP_P (result) = true;
  OVL_FUNCTION (result) = fn;
  OVL_CHAIN (result) = next;
  return result;
}

/* Add FN to the (potentially NULL) overload set OVL.  USING_OR_HIDDEN is > 0
   if this is a using-decl.  It is > 1 if we're revealing the using decl.
   It is > 2 if we're also exporting it.  USING_OR_HIDDEN is < 0, if FN is
   hidden.  (A decl cannot be both using and hidden.)  We keep the hidden
   decls first, but remaining ones are unordered.  */

tree
ovl_insert (tree fn, tree maybe_ovl, int using_or_hidden)
{
  tree result = maybe_ovl;
  tree insert_after = NULL_TREE;

  /* Skip hidden.  */
  for (; maybe_ovl && TREE_CODE (maybe_ovl) == OVERLOAD
	 && OVL_HIDDEN_P (maybe_ovl);
       maybe_ovl = OVL_CHAIN (maybe_ovl))
    {
      gcc_checking_assert (!OVL_LOOKUP_P (maybe_ovl));
      insert_after = maybe_ovl;
    }

  if (maybe_ovl || using_or_hidden || TREE_CODE (fn) == TEMPLATE_DECL)
    {
      maybe_ovl = ovl_make (fn, maybe_ovl);

      if (using_or_hidden < 0)
	OVL_HIDDEN_P (maybe_ovl) = true;
      if (using_or_hidden > 0)
	{
	  OVL_DEDUP_P (maybe_ovl) = OVL_USING_P (maybe_ovl) = true;
	  if (using_or_hidden > 1)
	    OVL_PURVIEW_P (maybe_ovl) = true;
	  if (using_or_hidden > 2)
	    OVL_EXPORT_P (maybe_ovl) = true;
	}
    }
  else
    maybe_ovl = fn;

  if (insert_after)
    {
      OVL_CHAIN (insert_after) = maybe_ovl;
      TREE_TYPE (insert_after) = unknown_type_node;
    }
  else
    result = maybe_ovl;

  return result;
}

/* Skip any hidden names at the beginning of OVL.   */

tree
ovl_skip_hidden (tree ovl)
{
  while (ovl && TREE_CODE (ovl) == OVERLOAD && OVL_HIDDEN_P (ovl))
    ovl = OVL_CHAIN (ovl);

  return ovl;
}

/* NODE is an OVL_HIDDEN_P node that is now revealed.  */

tree
ovl_iterator::reveal_node (tree overload, tree node)
{
  /* We cannot have returned NODE as part of a lookup overload, so we
     don't have to worry about preserving that.  */

  OVL_HIDDEN_P (node) = false;
  if (tree chain = OVL_CHAIN (node))
    if (TREE_CODE (chain) == OVERLOAD)
      {
	if (OVL_HIDDEN_P (chain))
	  {
	    /* The node needs moving, and the simplest way is to remove it
	       and reinsert.  */
	    overload = remove_node (overload, node);
	    overload = ovl_insert (OVL_FUNCTION (node), overload);
	  }
	else if (OVL_DEDUP_P (chain))
	  OVL_DEDUP_P (node) = true;
      }
  return overload;
}

/* NODE is on the overloads of OVL.  Remove it.
   The removed node is unaltered and may continue to be iterated
   from (i.e. it is safe to remove a node from an overload one is
   currently iterating over).  */

tree
ovl_iterator::remove_node (tree overload, tree node)
{
  tree *slot = &overload;
  while (*slot != node)
    {
      tree probe = *slot;
      gcc_checking_assert (!OVL_LOOKUP_P (probe));

      slot = &OVL_CHAIN (probe);
    }

  /* Stitch out NODE.  We don't have to worry about now making a
     singleton overload (and consequently maybe setting its type),
     because all uses of this function will be followed by inserting a
     new node that must follow the place we've cut this out from.  */
  if (TREE_CODE (node) != OVERLOAD)
    /* Cloned inherited ctors don't mark themselves as via_using.  */
    *slot = NULL_TREE;
  else
    *slot = OVL_CHAIN (node);

  return overload;
}

/* Mark or unmark a lookup set. */

void
lookup_mark (tree ovl, bool val)
{
  for (lkp_iterator iter (ovl); iter; ++iter)
    {
      gcc_checking_assert (LOOKUP_SEEN_P (*iter) != val);
      LOOKUP_SEEN_P (*iter) = val;
    }
}

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

/* FNS is a new overload set, add them to LOOKUP, if they are not
   already present there.  */

tree
lookup_maybe_add (tree fns, tree lookup, bool deduping)
{
  if (deduping)
    for (tree next, probe = fns; probe; probe = next)
      {
	tree fn = probe;
	next = NULL_TREE;

	if (TREE_CODE (probe) == OVERLOAD)
	  {
	    fn = OVL_FUNCTION (probe);
	    next = OVL_CHAIN (probe);
	  }

	if (!LOOKUP_SEEN_P (fn))
	  LOOKUP_SEEN_P (fn) = true;
	else
	  {
	    /* This function was already seen.  Insert all the
	       predecessors onto the lookup.  */
	    for (; fns != probe; fns = OVL_CHAIN (fns))
	      {
		/* Propagate OVL_USING, but OVL_HIDDEN &
		   OVL_DEDUP_P don't matter.  */
		if (OVL_USING_P (fns))
		  {
		    lookup = ovl_make (OVL_FUNCTION (fns), lookup);
		    OVL_USING_P (lookup) = true;
		  }
		else
		  lookup = lookup_add (OVL_FUNCTION (fns), lookup);
	      }

	    /* And now skip this function.  */
	    fns = next;
	  }
      }

  if (fns)
    /* We ended in a set of new functions.  Add them all in one go.  */
    lookup = lookup_add (fns, lookup);

  return lookup;
}

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

  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (x) == OFFSET_REF
      || TREE_CODE (x) == COMPONENT_REF)
    x = TREE_OPERAND (x, 1);
  x = MAYBE_BASELINK_FUNCTIONS (x);
  if (TREE_CODE (x) == TEMPLATE_ID_EXPR)
    x = TREE_OPERAND (x, 0);

  if (DECL_FUNCTION_TEMPLATE_P (OVL_FIRST (x))
      || (TREE_CODE (x) == OVERLOAD && !OVL_SINGLE_P (x)))
    return 2;

  return OVL_P (x);
}

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
  if (TREE_CODE (x) == TEMPLATE_ID_EXPR)
    x = TREE_OPERAND (x, 0);
  if (OVL_P (x))
    return OVL_NAME (x);
  return NULL_TREE;
}

/* Like dependent_name, but instead takes a CALL_EXPR and also checks
   its dependence.  */

tree
call_expr_dependent_name (tree x)
{
  if (TREE_TYPE (x) != NULL_TREE)
    /* X isn't dependent, so its callee isn't a dependent name.  */
    return NULL_TREE;
  return dependent_name (CALL_EXPR_FN (x));
}

/* Returns true iff X is an expression for an overloaded function
   whose type cannot be known without performing overload
   resolution.  */

bool
really_overloaded_fn (tree x)
{
  return is_overloaded_fn (x) == 2;
}

/* Get the overload set FROM refers to.  Returns NULL if it's not an
   overload set.  */

tree
maybe_get_fns (tree from)
{
  STRIP_ANY_LOCATION_WRAPPER (from);

  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (from) == OFFSET_REF
      || TREE_CODE (from) == COMPONENT_REF)
    from = TREE_OPERAND (from, 1);
  if (BASELINK_P (from))
    from = BASELINK_FUNCTIONS (from);
  if (TREE_CODE (from) == TEMPLATE_ID_EXPR)
    from = TREE_OPERAND (from, 0);

  if (OVL_P (from))
    return from;

  return NULL;
}

/* FROM refers to an overload set.  Return that set (or die).  */

tree
get_fns (tree from)
{
  tree res = maybe_get_fns (from);

  gcc_assert (res);
  return res;
}

/* Return the first function of the overload set FROM refers to.  */

tree
get_first_fn (tree from)
{
  return OVL_FIRST (get_fns (from));
}

/* Return the scope where the overloaded functions OVL were found.  */

tree
ovl_scope (tree ovl)
{
  if (TREE_CODE (ovl) == OFFSET_REF
      || TREE_CODE (ovl) == COMPONENT_REF)
    ovl = TREE_OPERAND (ovl, 1);
  if (TREE_CODE (ovl) == BASELINK)
    return BINFO_TYPE (BASELINK_BINFO (ovl));
  if (TREE_CODE (ovl) == TEMPLATE_ID_EXPR)
    ovl = TREE_OPERAND (ovl, 0);
  /* Skip using-declarations.  */
  lkp_iterator iter (ovl);
  do
    ovl = *iter;
  while (iter.using_p () && ++iter);

  return CP_DECL_CONTEXT (ovl);
}

#define PRINT_RING_SIZE 4

static const char *
cxx_printable_name_internal (tree decl, int v, bool translate)
{
  static unsigned int uid_ring[PRINT_RING_SIZE];
  static char *print_ring[PRINT_RING_SIZE];
  static bool trans_ring[PRINT_RING_SIZE];
  static int ring_counter;
  int i;

  /* Only cache functions.  */
  if (v < 2
      || TREE_CODE (decl) != FUNCTION_DECL
      || DECL_LANG_SPECIFIC (decl) == 0)
    return lang_decl_name (decl, v, translate);

  /* See if this print name is lying around.  */
  for (i = 0; i < PRINT_RING_SIZE; i++)
    if (uid_ring[i] == DECL_UID (decl) && translate == trans_ring[i])
      /* yes, so return it.  */
      return print_ring[i];

  if (++ring_counter == PRINT_RING_SIZE)
    ring_counter = 0;

  if (current_function_decl != NULL_TREE)
    {
      /* There may be both translated and untranslated versions of the
	 name cached.  */
      for (i = 0; i < 2; i++)
	{
	  if (uid_ring[ring_counter] == DECL_UID (current_function_decl))
	    ring_counter += 1;
	  if (ring_counter == PRINT_RING_SIZE)
	    ring_counter = 0;
	}
      gcc_assert (uid_ring[ring_counter] != DECL_UID (current_function_decl));
    }

  free (print_ring[ring_counter]);

  print_ring[ring_counter] = xstrdup (lang_decl_name (decl, v, translate));
  uid_ring[ring_counter] = DECL_UID (decl);
  trans_ring[ring_counter] = translate;
  return print_ring[ring_counter];
}

const char *
cxx_printable_name (tree decl, int v)
{
  return cxx_printable_name_internal (decl, v, false);
}

const char *
cxx_printable_name_translate (tree decl, int v)
{
  return cxx_printable_name_internal (decl, v, true);
}

/* Return the canonical version of exception-specification RAISES for a C++17
   function type, for use in type comparison and building TYPE_CANONICAL.  */

tree
canonical_eh_spec (tree raises)
{
  if (raises == NULL_TREE)
    return raises;
  else if (DEFERRED_NOEXCEPT_SPEC_P (raises)
	   || UNPARSED_NOEXCEPT_SPEC_P (raises)
	   || uses_template_parms (raises)
	   || uses_template_parms (TREE_PURPOSE (raises)))
    /* Keep a dependent or deferred exception specification.  */
    return raises;
  else if (nothrow_spec_p (raises))
    /* throw() -> noexcept.  */
    return noexcept_true_spec;
  else
    /* For C++17 type matching, anything else -> nothing.  */
    return NULL_TREE;
}

tree
build_cp_fntype_variant (tree type, cp_ref_qualifier rqual,
			 tree raises, bool late)
{
  cp_cv_quals type_quals = TYPE_QUALS (type);

  if (cp_check_qualified_type (type, type, type_quals, rqual, raises, late))
    return type;

  tree v = TYPE_MAIN_VARIANT (type);
  for (; v; v = TYPE_NEXT_VARIANT (v))
    if (cp_check_qualified_type (v, type, type_quals, rqual, raises, late))
      return v;

  /* Need to build a new variant.  */
  v = build_variant_type_copy (type);
  if (!TYPE_DEPENDENT_P (v))
    /* We no longer know that it's not type-dependent.  */
    TYPE_DEPENDENT_P_VALID (v) = false;
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  TYPE_HAS_LATE_RETURN_TYPE (v) = late;
  switch (rqual)
    {
    case REF_QUAL_RVALUE:
      FUNCTION_RVALUE_QUALIFIED (v) = 1;
      FUNCTION_REF_QUALIFIED (v) = 1;
      break;
    case REF_QUAL_LVALUE:
      FUNCTION_RVALUE_QUALIFIED (v) = 0;
      FUNCTION_REF_QUALIFIED (v) = 1;
      break;
    default:
      FUNCTION_REF_QUALIFIED (v) = 0;
      break;
    }

  /* Canonicalize the exception specification.  */
  tree cr = flag_noexcept_type ? canonical_eh_spec (raises) : NULL_TREE;
  bool complex_eh_spec_p = (cr && cr != noexcept_true_spec
			    && !UNPARSED_NOEXCEPT_SPEC_P (cr));

  if (!complex_eh_spec_p && TYPE_RAISES_EXCEPTIONS (type))
    /* We want to consider structural equality of the exception-less
       variant since we'll be replacing the exception specification.  */
    type = build_cp_fntype_variant (type, rqual, /*raises=*/NULL_TREE, late);
  if (TYPE_STRUCTURAL_EQUALITY_P (type) || complex_eh_spec_p)
    /* Propagate structural equality.  And always use structural equality
       for function types with a complex noexcept-spec since their identity
       may depend on e.g. whether comparing_specializations is set.  */
    SET_TYPE_STRUCTURAL_EQUALITY (v);
  else if (TYPE_CANONICAL (type) != type || cr != raises || late)
    /* Build the underlying canonical type, since it is different
       from TYPE. */
    TYPE_CANONICAL (v) = build_cp_fntype_variant (TYPE_CANONICAL (type),
						  rqual, cr, false);
  else
    /* T is its own canonical type. */
    TYPE_CANONICAL (v) = v;

  return v;
}

/* TYPE is a function or method type with a deferred exception
   specification that has been parsed to RAISES.  Fixup all the type
   variants that are affected in place.  Via decltype &| noexcept
   tricks, the unparsed spec could have escaped into the type system.  */

void
fixup_deferred_exception_variants (tree type, tree raises)
{
  tree original = TYPE_RAISES_EXCEPTIONS (type);

  gcc_checking_assert (UNPARSED_NOEXCEPT_SPEC_P (original));

  for (tree variant = TYPE_MAIN_VARIANT (type);
       variant; variant = TYPE_NEXT_VARIANT (variant))
    if (TYPE_RAISES_EXCEPTIONS (variant) == original)
      {
	gcc_checking_assert (variant != TYPE_MAIN_VARIANT (type));

	SET_TYPE_STRUCTURAL_EQUALITY (variant);
	TYPE_RAISES_EXCEPTIONS (variant) = raises;

	if (!TYPE_DEPENDENT_P (variant))
	  /* We no longer know that it's not type-dependent.  */
	  TYPE_DEPENDENT_P_VALID (variant) = false;
      }
}

/* Build the FUNCTION_TYPE or METHOD_TYPE which may throw exceptions
   listed in RAISES.  */

tree
build_exception_variant (tree type, tree raises)
{
  cp_ref_qualifier rqual = type_memfn_rqual (type);
  bool late = TYPE_HAS_LATE_RETURN_TYPE (type);
  return build_cp_fntype_variant (type, rqual, raises, late);
}

/* Given a TEMPLATE_TEMPLATE_PARM node T, create a new
   BOUND_TEMPLATE_TEMPLATE_PARM bound with NEWARGS as its template
   arguments.  */

tree
bind_template_template_parm (tree t, tree newargs)
{
  tree decl = TYPE_NAME (t);
  tree t2;

  t2 = cxx_make_type (BOUND_TEMPLATE_TEMPLATE_PARM);
  decl = build_decl (input_location,
		     TYPE_DECL, DECL_NAME (decl), NULL_TREE);
  SET_DECL_TEMPLATE_PARM_P (decl);

  /* These nodes have to be created to reflect new TYPE_DECL and template
     arguments.  */
  TEMPLATE_TYPE_PARM_INDEX (t2) = copy_node (TEMPLATE_TYPE_PARM_INDEX (t));
  TEMPLATE_PARM_DECL (TEMPLATE_TYPE_PARM_INDEX (t2)) = decl;
  TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t2)
    = build_template_info (TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (t), newargs);

  TREE_TYPE (decl) = t2;
  TYPE_NAME (t2) = decl;
  TYPE_STUB_DECL (t2) = decl;
  TYPE_SIZE (t2) = 0;

  if (any_template_arguments_need_structural_equality_p (newargs))
    SET_TYPE_STRUCTURAL_EQUALITY (t2);
  else
    TYPE_CANONICAL (t2) = canonical_type_parameter (t2);

  return t2;
}

/* Called from count_trees via walk_tree.  */

static tree
count_trees_r (tree *tp, int *walk_subtrees, void *data)
{
  ++*((int *) data);

  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Debugging function for measuring the rough complexity of a tree
   representation.  */

int
count_trees (tree t)
{
  int n_trees = 0;
  cp_walk_tree_without_duplicates (&t, count_trees_r, &n_trees);
  return n_trees;
}

/* Called from verify_stmt_tree via walk_tree.  */

static tree
verify_stmt_tree_r (tree* tp, int * /*walk_subtrees*/, void* data)
{
  tree t = *tp;
  hash_table<nofree_ptr_hash <tree_node> > *statements
      = static_cast <hash_table<nofree_ptr_hash <tree_node> > *> (data);
  tree_node **slot;

  if (!STATEMENT_CODE_P (TREE_CODE (t)))
    return NULL_TREE;

  /* If this statement is already present in the hash table, then
     there is a circularity in the statement tree.  */
  gcc_assert (!statements->find (t));

  slot = statements->find_slot (t, INSERT);
  *slot = t;

  return NULL_TREE;
}

/* Debugging function to check that the statement T has not been
   corrupted.  For now, this function simply checks that T contains no
   circularities.  */

void
verify_stmt_tree (tree t)
{
  hash_table<nofree_ptr_hash <tree_node> > statements (37);
  cp_walk_tree (&t, verify_stmt_tree_r, &statements, NULL);
}

/* Check if the type T depends on a type with no linkage and if so,
   return it.  If RELAXED_P then do not consider a class type declared
   within a vague-linkage function or in a module CMI to have no linkage,
   since it can still be accessed within a different TU.  Remember:
   no-linkage is not the same as internal-linkage.  */

tree
no_linkage_check (tree t, bool relaxed_p)
{
  tree r;

  /* Lambda types that don't have mangling scope have no linkage.  We
     check CLASSTYPE_LAMBDA_EXPR for error_mark_node because
     when we get here from pushtag none of the lambda information is
     set up yet, so we want to assume that the lambda has linkage and
     fix it up later if not.  We need to check this even in templates so
     that we properly handle a lambda-expression in the signature.  */
  if (LAMBDA_TYPE_P (t)
      && CLASSTYPE_LAMBDA_EXPR (t) != error_mark_node)
    {
      tree extra = LAMBDA_TYPE_EXTRA_SCOPE (t);
      if (!extra)
	return t;
    }

  /* Otherwise there's no point in checking linkage on template functions; we
     can't know their complete types.  */
  if (processing_template_decl)
    return NULL_TREE;

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	goto ptrmem;
      /* Fall through.  */
    case UNION_TYPE:
      if (!CLASS_TYPE_P (t))
	return NULL_TREE;
      /* Fall through.  */
    case ENUMERAL_TYPE:
      /* Only treat unnamed types as having no linkage if they're at
	 namespace scope.  This is core issue 966.  */
      if (TYPE_UNNAMED_P (t) && TYPE_NAMESPACE_SCOPE_P (t))
	return t;

      for (r = CP_TYPE_CONTEXT (t); ; )
	{
	  /* If we're a nested type of a !TREE_PUBLIC class, we might not
	     have linkage, or we might just be in an anonymous namespace.
	     If we're in a TREE_PUBLIC class, we have linkage.  */
	  if (TYPE_P (r) && !TREE_PUBLIC (TYPE_NAME (r)))
	    return no_linkage_check (TYPE_CONTEXT (t), relaxed_p);
	  else if (TREE_CODE (r) == FUNCTION_DECL)
	    {
	      if (relaxed_p
		  && (vague_linkage_p (r)
		      || (TREE_PUBLIC (r) && module_maybe_has_cmi_p ())))
		r = CP_DECL_CONTEXT (r);
	      else
		return t;
	    }
	  else
	    break;
	}

      return NULL_TREE;

    case ARRAY_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case VECTOR_TYPE:
      return no_linkage_check (TREE_TYPE (t), relaxed_p);

    case OFFSET_TYPE:
    ptrmem:
      r = no_linkage_check (TYPE_PTRMEM_POINTED_TO_TYPE (t),
			    relaxed_p);
      if (r)
	return r;
      return no_linkage_check (TYPE_PTRMEM_CLASS_TYPE (t), relaxed_p);

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      {
	tree parm = TYPE_ARG_TYPES (t);
	if (TREE_CODE (t) == METHOD_TYPE)
	  /* The 'this' pointer isn't interesting; a method has the same
	     linkage (or lack thereof) as its enclosing class.  */
	  parm = TREE_CHAIN (parm);
	for (;
	     parm && parm != void_list_node;
	     parm = TREE_CHAIN (parm))
	  {
	    r = no_linkage_check (TREE_VALUE (parm), relaxed_p);
	    if (r)
	      return r;
	  }
	return no_linkage_check (TREE_TYPE (t), relaxed_p);
      }

    default:
      return NULL_TREE;
    }
}

extern int depth_reached;

void
cxx_print_statistics (void)
{
  print_template_statistics ();
  if (GATHER_STATISTICS)
    fprintf (stderr, "maximum template instantiation depth reached: %d\n",
	     depth_reached);
}

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This one is a recursive count of all
   ARRAY_TYPEs that are clumped together.  */

tree
array_type_nelts_total (tree type)
{
  tree sz = array_type_nelts_top (type);
  type = TREE_TYPE (type);
  while (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree n = array_type_nelts_top (type);
      sz = fold_build2_loc (input_location,
			MULT_EXPR, sizetype, sz, n);
      type = TREE_TYPE (type);
    }
  return sz;
}

struct bot_data
{
  splay_tree target_remap;
  bool clear_location;
};

/* Called from break_out_target_exprs via mapcar.  */

static tree
bot_manip (tree* tp, int* walk_subtrees, void* data_)
{
  bot_data &data = *(bot_data*)data_;
  splay_tree target_remap = data.target_remap;
  tree t = *tp;

  if (!TYPE_P (t) && TREE_CONSTANT (t) && !TREE_SIDE_EFFECTS (t))
    {
      /* There can't be any TARGET_EXPRs or their slot variables below this
	 point.  But we must make a copy, in case subsequent processing
	 alters any part of it.  For example, during gimplification a cast
	 of the form (T) &X::f (where "f" is a member function) will lead
	 to replacing the PTRMEM_CST for &X::f with a VAR_DECL.  */
      *walk_subtrees = 0;
      *tp = unshare_expr (t);
      return NULL_TREE;
    }
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree u;

      if (TREE_CODE (TARGET_EXPR_INITIAL (t)) == AGGR_INIT_EXPR)
	{
	  u = build_cplus_new (TREE_TYPE (t), TARGET_EXPR_INITIAL (t),
			       tf_warning_or_error);
	  if (u == error_mark_node)
	    return u;
	  if (AGGR_INIT_ZERO_FIRST (TARGET_EXPR_INITIAL (t)))
	    AGGR_INIT_ZERO_FIRST (TARGET_EXPR_INITIAL (u)) = true;
	}
      else
	u = force_target_expr (TREE_TYPE (t), TARGET_EXPR_INITIAL (t),
			       tf_warning_or_error);

      TARGET_EXPR_IMPLICIT_P (u) = TARGET_EXPR_IMPLICIT_P (t);
      TARGET_EXPR_LIST_INIT_P (u) = TARGET_EXPR_LIST_INIT_P (t);
      TARGET_EXPR_DIRECT_INIT_P (u) = TARGET_EXPR_DIRECT_INIT_P (t);
      TARGET_EXPR_ELIDING_P (u) = TARGET_EXPR_ELIDING_P (t);

      /* Map the old variable to the new one.  */
      splay_tree_insert (target_remap,
			 (splay_tree_key) TARGET_EXPR_SLOT (t),
			 (splay_tree_value) TARGET_EXPR_SLOT (u));

      TARGET_EXPR_INITIAL (u) = break_out_target_exprs (TARGET_EXPR_INITIAL (u),
							data.clear_location);
      if (TARGET_EXPR_INITIAL (u) == error_mark_node)
	return error_mark_node;

      if (data.clear_location)
	SET_EXPR_LOCATION (u, input_location);

      /* Replace the old expression with the new version.  */
      *tp = u;
      /* We don't have to go below this point; the recursive call to
	 break_out_target_exprs will have handled anything below this
	 point.  */
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  if (TREE_CODE (*tp) == SAVE_EXPR)
    {
      t = *tp;
      splay_tree_node n = splay_tree_lookup (target_remap,
					     (splay_tree_key) t);
      if (n)
	{
	  *tp = (tree)n->value;
	  *walk_subtrees = 0;
	}
      else
	{
	  copy_tree_r (tp, walk_subtrees, NULL);
	  splay_tree_insert (target_remap,
			     (splay_tree_key)t,
			     (splay_tree_value)*tp);
	  /* Make sure we don't remap an already-remapped SAVE_EXPR.  */
	  splay_tree_insert (target_remap,
			     (splay_tree_key)*tp,
			     (splay_tree_value)*tp);
	}
      return NULL_TREE;
    }
  if (TREE_CODE (*tp) == DECL_EXPR
      && VAR_P (DECL_EXPR_DECL (*tp))
      && DECL_ARTIFICIAL (DECL_EXPR_DECL (*tp))
      && !TREE_STATIC (DECL_EXPR_DECL (*tp)))
    {
      tree t;
      splay_tree_node n
	= splay_tree_lookup (target_remap,
			     (splay_tree_key) DECL_EXPR_DECL (*tp));
      if (n)
	t = (tree) n->value;
      else
	{
	  t = create_temporary_var (TREE_TYPE (DECL_EXPR_DECL (*tp)));
	  DECL_INITIAL (t) = DECL_INITIAL (DECL_EXPR_DECL (*tp));
	  splay_tree_insert (target_remap,
			     (splay_tree_key) DECL_EXPR_DECL (*tp),
			     (splay_tree_value) t);
	}
      copy_tree_r (tp, walk_subtrees, NULL);
      DECL_EXPR_DECL (*tp) = t;
      if (data.clear_location && EXPR_HAS_LOCATION (*tp))
	SET_EXPR_LOCATION (*tp, input_location);
      return NULL_TREE;
    }
  if (TREE_CODE (*tp) == BIND_EXPR && BIND_EXPR_VARS (*tp))
    {
      copy_tree_r (tp, walk_subtrees, NULL);
      for (tree *p = &BIND_EXPR_VARS (*tp); *p; p = &DECL_CHAIN (*p))
	{
	  gcc_assert (VAR_P (*p) && DECL_ARTIFICIAL (*p) && !TREE_STATIC (*p));
	  tree t = create_temporary_var (TREE_TYPE (*p));
	  DECL_INITIAL (t) = DECL_INITIAL (*p);
	  DECL_CHAIN (t) = DECL_CHAIN (*p);
	  splay_tree_insert (target_remap, (splay_tree_key) *p,
			     (splay_tree_value) t);
	  *p = t;
	}
      if (data.clear_location && EXPR_HAS_LOCATION (*tp))
	SET_EXPR_LOCATION (*tp, input_location);
      return NULL_TREE;
    }

  /* Make a copy of this node.  */
  t = copy_tree_r (tp, walk_subtrees, NULL);
  if (TREE_CODE (*tp) == CALL_EXPR || TREE_CODE (*tp) == AGGR_INIT_EXPR)
    if (!processing_template_decl)
      set_flags_from_callee (*tp);
  if (data.clear_location && EXPR_HAS_LOCATION (*tp))
    SET_EXPR_LOCATION (*tp, input_location);
  return t;
}

/* Replace all remapped VAR_DECLs in T with their new equivalents.
   DATA is really a splay-tree mapping old variables to new
   variables.  */

static tree
bot_replace (tree* t, int */*walk_subtrees*/, void* data_)
{
  bot_data &data = *(bot_data*)data_;
  splay_tree target_remap = data.target_remap;

  if (VAR_P (*t))
    {
      splay_tree_node n = splay_tree_lookup (target_remap,
					     (splay_tree_key) *t);
      if (n)
	*t = (tree) n->value;
    }
  else if (TREE_CODE (*t) == PARM_DECL
	   && DECL_NAME (*t) == this_identifier
	   && !DECL_CONTEXT (*t))
    {
      /* In an NSDMI we need to replace the 'this' parameter we used for
	 parsing with the real one for this function.  */
      *t = current_class_ptr;
    }
  else if (TREE_CODE (*t) == CONVERT_EXPR
	   && CONVERT_EXPR_VBASE_PATH (*t))
    {
      /* In an NSDMI build_base_path defers building conversions to morally
	 virtual bases, and we handle it here.  */
      tree basetype = TREE_TYPE (*t);
      *t = convert_to_base (TREE_OPERAND (*t, 0), basetype,
			    /*check_access=*/false, /*nonnull=*/true,
			    tf_warning_or_error);
    }

  return NULL_TREE;
}

/* When we parse a default argument expression, we may create
   temporary variables via TARGET_EXPRs.  When we actually use the
   default-argument expression, we make a copy of the expression
   and replace the temporaries with appropriate local versions.

   If CLEAR_LOCATION is true, override any EXPR_LOCATION with
   input_location.  */

tree
break_out_target_exprs (tree t, bool clear_location /* = false */)
{
  static int target_remap_count;
  static splay_tree target_remap;

  /* We shouldn't be called on templated trees, nor do we want to
     produce them.  */
  gcc_checking_assert (!processing_template_decl);

  if (!target_remap_count++)
    target_remap = splay_tree_new (splay_tree_compare_pointers,
				   /*splay_tree_delete_key_fn=*/NULL,
				   /*splay_tree_delete_value_fn=*/NULL);
  bot_data data = { target_remap, clear_location };
  if (cp_walk_tree (&t, bot_manip, &data, NULL) == error_mark_node)
    t = error_mark_node;
  if (cp_walk_tree (&t, bot_replace, &data, NULL) == error_mark_node)
    t = error_mark_node;

  if (!--target_remap_count)
    {
      splay_tree_delete (target_remap);
      target_remap = NULL;
    }

  return t;
}

/* Build an expression for the subobject of OBJ at CONSTRUCTOR index INDEX,
   which we expect to have type TYPE.  */

tree
build_ctor_subob_ref (tree index, tree type, tree obj)
{
  if (index == NULL_TREE)
    /* Can't refer to a particular member of a vector.  */
    obj = NULL_TREE;
  else if (TREE_CODE (index) == INTEGER_CST)
    obj = cp_build_array_ref (input_location, obj, index, tf_none);
  else
    obj = build_class_member_access_expr (obj, index, NULL_TREE,
					  /*reference*/false, tf_none);
  if (obj)
    {
      tree objtype = TREE_TYPE (obj);
      if (TREE_CODE (objtype) == ARRAY_TYPE && !TYPE_DOMAIN (objtype))
	{
	  /* When the destination object refers to a flexible array member
	     verify that it matches the type of the source object except
	     for its domain and qualifiers.  */
	  gcc_assert (comptypes (TYPE_MAIN_VARIANT (type),
	  			 TYPE_MAIN_VARIANT (objtype),
	  			 COMPARE_REDECLARATION));
	}
      else
	gcc_assert (same_type_ignoring_top_level_qualifiers_p (type, objtype));
    }

  return obj;
}

struct replace_placeholders_t
{
  tree obj;	    /* The object to be substituted for a PLACEHOLDER_EXPR.  */
  tree exp;	    /* The outermost exp.  */
  bool seen;	    /* Whether we've encountered a PLACEHOLDER_EXPR.  */
  hash_set<tree> *pset;	/* To avoid walking same trees multiple times.  */
};

/* Like substitute_placeholder_in_expr, but handle C++ tree codes and
   build up subexpressions as we go deeper.  */

static tree
replace_placeholders_r (tree* t, int* walk_subtrees, void* data_)
{
  replace_placeholders_t *d = static_cast<replace_placeholders_t*>(data_);
  tree obj = d->obj;

  if (TYPE_P (*t) || TREE_CONSTANT (*t))
    {
      *walk_subtrees = false;
      return NULL_TREE;
    }

  switch (TREE_CODE (*t))
    {
    case PLACEHOLDER_EXPR:
      {
	tree x = obj;
	for (; !same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (*t),
							   TREE_TYPE (x));
	     x = TREE_OPERAND (x, 0))
	  gcc_assert (handled_component_p (x));
	*t = unshare_expr (x);
	*walk_subtrees = false;
	d->seen = true;
      }
      break;

    case CONSTRUCTOR:
      {
	constructor_elt *ce;
	vec<constructor_elt,va_gc> *v = CONSTRUCTOR_ELTS (*t);
	/* Don't walk into CONSTRUCTOR_PLACEHOLDER_BOUNDARY ctors
	   other than the d->exp one, those have PLACEHOLDER_EXPRs
	   related to another object.  */
	if ((CONSTRUCTOR_PLACEHOLDER_BOUNDARY (*t)
	     && *t != d->exp)
	    || d->pset->add (*t))
	  {
	    *walk_subtrees = false;
	    return NULL_TREE;
	  }
	for (unsigned i = 0; vec_safe_iterate (v, i, &ce); ++i)
	  {
	    tree *valp = &ce->value;
	    tree type = TREE_TYPE (*valp);
	    tree subob = obj;

	    /* Elements with RANGE_EXPR index shouldn't have any
	       placeholders in them.  */
	    if (ce->index && TREE_CODE (ce->index) == RANGE_EXPR)
	      continue;

	    if (TREE_CODE (*valp) == CONSTRUCTOR
		&& AGGREGATE_TYPE_P (type))
	      {
		/* If we're looking at the initializer for OBJ, then build
		   a sub-object reference.  If we're looking at an
		   initializer for another object, just pass OBJ down.  */
		if (same_type_ignoring_top_level_qualifiers_p
		    (TREE_TYPE (*t), TREE_TYPE (obj)))
		  subob = build_ctor_subob_ref (ce->index, type, obj);
		if (TREE_CODE (*valp) == TARGET_EXPR)
		  valp = &TARGET_EXPR_INITIAL (*valp);
	      }
	    d->obj = subob;
	    cp_walk_tree (valp, replace_placeholders_r, data_, NULL);
	    d->obj = obj;
	  }
	*walk_subtrees = false;
	break;
      }

    default:
      if (d->pset->add (*t))
	*walk_subtrees = false;
      break;
    }

  return NULL_TREE;
}

/* Replace PLACEHOLDER_EXPRs in EXP with object OBJ.  SEEN_P is set if
   a PLACEHOLDER_EXPR has been encountered.  */

tree
replace_placeholders (tree exp, tree obj, bool *seen_p /*= NULL*/)
{
  /* This is only relevant for C++14.  */
  if (cxx_dialect < cxx14)
    return exp;

  /* If the object isn't a (member of a) class, do nothing.  */
  tree op0 = obj;
  while (handled_component_p (op0))
    op0 = TREE_OPERAND (op0, 0);
  if (!CLASS_TYPE_P (strip_array_types (TREE_TYPE (op0))))
    return exp;

  tree *tp = &exp;
  if (TREE_CODE (exp) == TARGET_EXPR)
    tp = &TARGET_EXPR_INITIAL (exp);
  hash_set<tree> pset;
  replace_placeholders_t data = { obj, *tp, false, &pset };
  cp_walk_tree (tp, replace_placeholders_r, &data, NULL);
  if (seen_p)
    *seen_p = data.seen;
  return exp;
}

/* Callback function for find_placeholders.  */

static tree
find_placeholders_r (tree *t, int *walk_subtrees, void *)
{
  if (TYPE_P (*t) || TREE_CONSTANT (*t))
    {
      *walk_subtrees = false;
      return NULL_TREE;
    }

  switch (TREE_CODE (*t))
    {
    case PLACEHOLDER_EXPR:
      return *t;

    case CONSTRUCTOR:
      if (CONSTRUCTOR_PLACEHOLDER_BOUNDARY (*t))
	*walk_subtrees = false;
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Return true if EXP contains a PLACEHOLDER_EXPR.  Don't walk into
   ctors with CONSTRUCTOR_PLACEHOLDER_BOUNDARY flag set.  */

bool
find_placeholders (tree exp)
{
  /* This is only relevant for C++14.  */
  if (cxx_dialect < cxx14)
    return false;

  return cp_walk_tree_without_duplicates (&exp, find_placeholders_r, NULL);
}

/* Similar to `build_nt', but for template definitions of dependent
   expressions  */

tree
build_min_nt_loc (location_t loc, enum tree_code code, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, code);

  t = make_node (code);
  SET_EXPR_LOCATION (t, loc);
  length = TREE_CODE_LENGTH (code);

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

/* Similar to `build', but for template definitions.  */

tree
build_min (enum tree_code code, tree tt, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, tt);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);
  TREE_TYPE (t) = tt;

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
      if (x && !TYPE_P (x) && TREE_SIDE_EFFECTS (x))
	TREE_SIDE_EFFECTS (t) = 1;
    }

  va_end (p);

  return t;
}

/* Similar to `build', but for template definitions of non-dependent
   expressions. NON_DEP is the non-dependent expression that has been
   built.  */

tree
build_min_non_dep (enum tree_code code, tree non_dep, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, non_dep);

  if (REFERENCE_REF_P (non_dep))
    non_dep = TREE_OPERAND (non_dep, 0);

  t = make_node (code);
  SET_EXPR_LOCATION (t, cp_expr_loc_or_input_loc (non_dep));
  length = TREE_CODE_LENGTH (code);
  TREE_TYPE (t) = unlowered_expr_type (non_dep);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (non_dep);

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
      if (x && !TYPE_P (x))
	TREE_SIDE_EFFECTS (t) |= TREE_SIDE_EFFECTS (x);
    }

  va_end (p);
  return convert_from_reference (t);
}

/* Similar to build_min_nt, but call expressions  */

tree
build_min_nt_call_vec (tree fn, vec<tree, va_gc> *args)
{
  tree ret, t;
  unsigned int ix;

  ret = build_vl_exp (CALL_EXPR, vec_safe_length (args) + 3);
  CALL_EXPR_FN (ret) = fn;
  CALL_EXPR_STATIC_CHAIN (ret) = NULL_TREE;
  FOR_EACH_VEC_SAFE_ELT (args, ix, t)
    CALL_EXPR_ARG (ret, ix) = t;

  return ret;
}

/* Similar to `build_min_nt_call_vec', but for template definitions of
   non-dependent expressions. NON_DEP is the non-dependent expression
   that has been built.  */

tree
build_min_non_dep_call_vec (tree non_dep, tree fn, vec<tree, va_gc> *argvec)
{
  tree t = build_min_nt_call_vec (fn, argvec);
  if (REFERENCE_REF_P (non_dep))
    non_dep = TREE_OPERAND (non_dep, 0);
  TREE_TYPE (t) = TREE_TYPE (non_dep);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (non_dep);
  if (argvec)
    for (tree x : *argvec)
      if (x && !TYPE_P (x))
	TREE_SIDE_EFFECTS (t) |= TREE_SIDE_EFFECTS (x);
  return convert_from_reference (t);
}

/* Similar to build_min_non_dep, but for expressions that have been resolved to
   a call to an operator overload.  OP is the operator that has been
   overloaded.  NON_DEP is the non-dependent expression that's been built,
   which should be a CALL_EXPR or an INDIRECT_REF to a CALL_EXPR.  OVERLOAD is
   the overload that NON_DEP is calling.  */

tree
build_min_non_dep_op_overload (enum tree_code op,
			       tree non_dep,
			       tree overload, ...)
{
  va_list p;
  int nargs, expected_nargs;
  tree fn, call, obj = NULL_TREE;

  non_dep = extract_call_expr (non_dep);

  nargs = call_expr_nargs (non_dep);

  expected_nargs = cp_tree_code_length (op);
  if (DECL_OBJECT_MEMBER_FUNCTION_P (overload)
      /* For ARRAY_REF, operator[] is either a non-static member or newly
	 static member, never out of class and for the static member case
	 if user uses single index the operator[] needs to have a single
	 argument as well, but the function is called with 2 - the object
	 it is invoked on and the index.  */
      || op == ARRAY_REF)
    expected_nargs -= 1;
  if ((op == POSTINCREMENT_EXPR
       || op == POSTDECREMENT_EXPR)
      /* With -fpermissive non_dep could be operator++().  */
      && (!flag_permissive || nargs != expected_nargs))
    expected_nargs += 1;
  gcc_assert (nargs == expected_nargs);

  releasing_vec args;
  va_start (p, overload);

  if (!DECL_OBJECT_MEMBER_FUNCTION_P (overload))
    {
      fn = overload;
      if (op == ARRAY_REF)
	obj = va_arg (p, tree);
      for (int i = 0; i < nargs; i++)
	{
	  tree arg = va_arg (p, tree);
	  vec_safe_push (args, arg);
	}
    }
  else
    {
      tree object = va_arg (p, tree);
      tree binfo = TYPE_BINFO (TREE_TYPE (object));
      tree method = build_baselink (binfo, binfo, overload, NULL_TREE);
      fn = build_min (COMPONENT_REF, TREE_TYPE (overload),
		      object, method, NULL_TREE);
      for (int i = 0; i < nargs; i++)
	{
	  tree arg = va_arg (p, tree);
	  vec_safe_push (args, arg);
	}
    }

  va_end (p);
  call = build_min_non_dep_call_vec (non_dep, fn, args);

  tree call_expr = extract_call_expr (call);
  KOENIG_LOOKUP_P (call_expr) = KOENIG_LOOKUP_P (non_dep);
  CALL_EXPR_OPERATOR_SYNTAX (call_expr) = true;
  CALL_EXPR_ORDERED_ARGS (call_expr) = CALL_EXPR_ORDERED_ARGS (non_dep);
  CALL_EXPR_REVERSE_ARGS (call_expr) = CALL_EXPR_REVERSE_ARGS (non_dep);

  if (obj)
    return keep_unused_object_arg (call, obj, overload);
  return call;
}

/* Similar to above build_min_non_dep_op_overload, but arguments
   are taken from ARGS vector.  */

tree
build_min_non_dep_op_overload (tree non_dep, tree overload, tree object,
			       vec<tree, va_gc> *args)
{
  non_dep = extract_call_expr (non_dep);

  unsigned int nargs = call_expr_nargs (non_dep);
  tree fn = overload;
  if (DECL_OBJECT_MEMBER_FUNCTION_P (overload))
    {
      tree binfo = TYPE_BINFO (TREE_TYPE (object));
      tree method = build_baselink (binfo, binfo, overload, NULL_TREE);
      fn = build_min (COMPONENT_REF, TREE_TYPE (overload),
		      object, method, NULL_TREE);
      object = NULL_TREE;
    }
  gcc_assert (vec_safe_length (args) == nargs);

  tree call = build_min_non_dep_call_vec (non_dep, fn, args);

  tree call_expr = extract_call_expr (call);
  KOENIG_LOOKUP_P (call_expr) = KOENIG_LOOKUP_P (non_dep);
  CALL_EXPR_OPERATOR_SYNTAX (call_expr) = true;
  CALL_EXPR_ORDERED_ARGS (call_expr) = CALL_EXPR_ORDERED_ARGS (non_dep);
  CALL_EXPR_REVERSE_ARGS (call_expr) = CALL_EXPR_REVERSE_ARGS (non_dep);

  if (object)
    return keep_unused_object_arg (call, object, overload);
  return call;
}

/* Return a new tree vec copied from VEC, with ELT inserted at index IDX.  */

vec<tree, va_gc> *
vec_copy_and_insert (vec<tree, va_gc> *old_vec, tree elt, unsigned idx)
{
  unsigned len = vec_safe_length (old_vec);
  gcc_assert (idx <= len);

  vec<tree, va_gc> *new_vec = NULL;
  vec_alloc (new_vec, len + 1);

  unsigned i;
  for (i = 0; i < len; ++i)
    {
      if (i == idx)
	new_vec->quick_push (elt);
      new_vec->quick_push ((*old_vec)[i]);
    }
  if (i == idx)
    new_vec->quick_push (elt);

  return new_vec;
}

tree
get_type_decl (tree t)
{
  if (TREE_CODE (t) == TYPE_DECL)
    return t;
  if (TYPE_P (t))
    return TYPE_STUB_DECL (t);
  gcc_assert (t == error_mark_node);
  return t;
}

/* Returns the namespace that contains DECL, whether directly or
   indirectly.  */

tree
decl_namespace_context (tree decl)
{
  while (1)
    {
      if (TREE_CODE (decl) == NAMESPACE_DECL)
	return decl;
      else if (TYPE_P (decl))
	decl = CP_DECL_CONTEXT (TYPE_MAIN_DECL (decl));
      else
	decl = CP_DECL_CONTEXT (decl);
    }
}

/* Returns true if decl is within an anonymous namespace, however deeply
   nested, or false otherwise.  */

bool
decl_anon_ns_mem_p (tree decl)
{
  return !TREE_PUBLIC (decl_namespace_context (decl));
}

/* Returns true if the enclosing scope of DECL has internal or no linkage.  */

bool
decl_internal_context_p (const_tree decl)
{
  while (TREE_CODE (decl) != NAMESPACE_DECL)
    {
      /* Classes inside anonymous namespaces have TREE_PUBLIC == 0.  */
      if (TYPE_P (decl))
	return !TREE_PUBLIC (TYPE_MAIN_DECL (decl));

      decl = CP_DECL_CONTEXT (decl);
    }
  return !TREE_PUBLIC (decl);
}

/* Subroutine of cp_tree_equal: t1 and t2 are two CALL_EXPRs.
   Return whether their CALL_EXPR_FNs are equivalent.  */

static bool
called_fns_equal (tree t1, tree t2)
{
  /* Core 1321: dependent names are equivalent even if the overload sets
     are different.  But do compare explicit template arguments.  */
  tree name1 = call_expr_dependent_name (t1);
  tree name2 = call_expr_dependent_name (t2);
  t1 = CALL_EXPR_FN (t1);
  t2 = CALL_EXPR_FN (t2);
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

      if (TREE_CODE (t1) == TEMPLATE_ID_EXPR)
	targs1 = TREE_OPERAND (t1, 1);
      if (TREE_CODE (t2) == TEMPLATE_ID_EXPR)
	targs2 = TREE_OPERAND (t2, 1);
      return cp_tree_equal (targs1, targs2);
    }
  else
    return cp_tree_equal (t1, t2);
}

bool comparing_override_contracts;

/* In a component reference, return the innermost object of
   the postfix-expression.  */

static tree
get_innermost_component (tree t)
{
  gcc_assert (TREE_CODE (t) == COMPONENT_REF);
  while (TREE_CODE (t) == COMPONENT_REF)
    t = TREE_OPERAND (t, 0);
  return t;
}

/* Returns true if T is a possibly converted 'this' or '*this' expression.  */

static bool
is_this_expression (tree t)
{
  t = get_innermost_component (t);
  /* See through deferences and no-op conversions.  */
  if (INDIRECT_REF_P (t))
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == NOP_EXPR)
    t = TREE_OPERAND (t, 0);
  return is_this_parameter (t);
}

static bool
comparing_this_references (tree t1, tree t2)
{
  return is_this_expression (t1) && is_this_expression (t2);
}

static bool
equivalent_member_references (tree t1, tree t2)
{
  if (!comparing_this_references (t1, t2))
    return false;
  t1 = TREE_OPERAND (t1, 1);
  t2 = TREE_OPERAND (t2, 1);
  return t1 == t2;
}

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same. Return 0 if they are different.  */

bool
cp_tree_equal (tree t1, tree t2)
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

  if (CONSTANT_CLASS_P (t1)
      && !same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
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
      return FIXED_VALUES_IDENTICAL (TREE_FIXED_CST (t1),
				     TREE_FIXED_CST (t2));

    case COMPLEX_CST:
      return cp_tree_equal (TREE_REALPART (t1), TREE_REALPART (t2))
	&& cp_tree_equal (TREE_IMAGPART (t1), TREE_IMAGPART (t2));

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
	    if (!cp_tree_equal (field, elt2->index)
		|| !cp_tree_equal (value, elt2->value))
	      return false;
	  }
      }
      return true;

    case TREE_LIST:
      if (!cp_tree_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2)))
	return false;
      if (!cp_tree_equal (TREE_VALUE (t1), TREE_VALUE (t2)))
	return false;
      return cp_tree_equal (TREE_CHAIN (t1), TREE_CHAIN (t2));

    case SAVE_EXPR:
      return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      {
	if (KOENIG_LOOKUP_P (t1) != KOENIG_LOOKUP_P (t2))
	  return false;

	if (!called_fns_equal (t1, t2))
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
	    if (!cp_tree_equal (arg1, arg2))
	      return false;
	  }

	return true;
      }

    case TARGET_EXPR:
      {
	tree o1 = TARGET_EXPR_SLOT (t1);
	tree o2 = TARGET_EXPR_SLOT (t2);

	/* Special case: if either target is an unallocated VAR_DECL,
	   it means that it's going to be unified with whatever the
	   TARGET_EXPR is really supposed to initialize, so treat it
	   as being equivalent to anything.  */
	if (VAR_P (o1) && DECL_NAME (o1) == NULL_TREE
	    && !DECL_RTL_SET_P (o1))
	  /*Nop*/;
	else if (VAR_P (o2) && DECL_NAME (o2) == NULL_TREE
		 && !DECL_RTL_SET_P (o2))
	  /*Nop*/;
	else if (!cp_tree_equal (o1, o2))
	  return false;

	return cp_tree_equal (TARGET_EXPR_INITIAL (t1),
			      TARGET_EXPR_INITIAL (t2));
      }

    case AGGR_INIT_EXPR:
      {
	int n = aggr_init_expr_nargs (t1);
	if (n != aggr_init_expr_nargs (t2))
	  return false;

	if (!cp_tree_equal (AGGR_INIT_EXPR_FN (t1),
			    AGGR_INIT_EXPR_FN (t2)))
	  return false;

	tree o1 = AGGR_INIT_EXPR_SLOT (t1);
	tree o2 = AGGR_INIT_EXPR_SLOT (t2);

	/* Similarly to TARGET_EXPRs, if the VAR_DECL is unallocated we're
	   going to unify the initialization, so treat it as equivalent
	   to anything.  */
	if (VAR_P (o1) && DECL_NAME (o1) == NULL_TREE
	    && !DECL_RTL_SET_P (o1))
	  /*Nop*/;
	else if (VAR_P (o2) && DECL_NAME (o2) == NULL_TREE
		 && !DECL_RTL_SET_P (o2))
	  /*Nop*/;
	else if (!cp_tree_equal (o1, o2))
	  return false;

	for (int i = 0; i < n; ++i)
	  if (!cp_tree_equal (AGGR_INIT_EXPR_ARG (t1, i),
			      AGGR_INIT_EXPR_ARG (t2, i)))
	    return false;

	return true;
      }

    case PARM_DECL:
      /* For comparing uses of parameters in late-specified return types
	 with an out-of-class definition of the function, but can also come
	 up for expressions that involve 'this' in a member function
	 template.  */

      if (comparing_specializations
	  && DECL_CONTEXT (t1) != DECL_CONTEXT (t2))
	/* When comparing hash table entries, only an exact match is
	   good enough; we don't want to replace 'this' with the
	   version from another function.  But be more flexible
	   with parameters with identical contexts.  */
	return false;

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

    case TEMPLATE_DECL:
      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t1)
	  && DECL_TEMPLATE_TEMPLATE_PARM_P (t2))
	return cp_tree_equal (TREE_TYPE (t1), TREE_TYPE (t2));
      /* Fall through.  */
    case VAR_DECL:
    case CONST_DECL:
    case FIELD_DECL:
    case FUNCTION_DECL:
    case IDENTIFIER_NODE:
    case SSA_NAME:
    case USING_DECL:
    case DEFERRED_PARSE:
      return false;

    case BASELINK:
      return (BASELINK_BINFO (t1) == BASELINK_BINFO (t2)
	      && BASELINK_ACCESS_BINFO (t1) == BASELINK_ACCESS_BINFO (t2)
	      && BASELINK_QUALIFIED_P (t1) == BASELINK_QUALIFIED_P (t2)
	      && cp_tree_equal (BASELINK_FUNCTIONS (t1),
				BASELINK_FUNCTIONS (t2)));

    case TEMPLATE_PARM_INDEX:
      return (TEMPLATE_PARM_IDX (t1) == TEMPLATE_PARM_IDX (t2)
	      && TEMPLATE_PARM_LEVEL (t1) == TEMPLATE_PARM_LEVEL (t2)
	      && (TEMPLATE_PARM_PARAMETER_PACK (t1)
		  == TEMPLATE_PARM_PARAMETER_PACK (t2))
	      && same_type_p (TREE_TYPE (TEMPLATE_PARM_DECL (t1)),
			      TREE_TYPE (TEMPLATE_PARM_DECL (t2))));

    case TEMPLATE_ID_EXPR:
      if (!cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
	return false;
      if (!comp_template_args (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1)))
	return false;
      return true;

    case CONSTRAINT_INFO:
      return cp_tree_equal (CI_ASSOCIATED_CONSTRAINTS (t1),
                            CI_ASSOCIATED_CONSTRAINTS (t2));

    case TREE_VEC:
      /* These are template args.  Really we should be getting the
	 caller to do this as it knows it to be true.  */
      if (!comp_template_args (t1, t2))
	return false;
      return true;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      {
	tree o1 = TREE_OPERAND (t1, 0);
	tree o2 = TREE_OPERAND (t2, 0);

	if (code1 == SIZEOF_EXPR)
	  {
	    if (SIZEOF_EXPR_TYPE_P (t1))
	      o1 = TREE_TYPE (o1);
	    if (SIZEOF_EXPR_TYPE_P (t2))
	      o2 = TREE_TYPE (o2);
	  }
	else if (ALIGNOF_EXPR_STD_P (t1) != ALIGNOF_EXPR_STD_P (t2))
	  return false;

	if (TREE_CODE (o1) != TREE_CODE (o2))
	  return false;

	if (ARGUMENT_PACK_P (o1))
	  return template_args_equal (o1, o2);
	else if (TYPE_P (o1))
	  return same_type_p (o1, o2);
	else
	  return cp_tree_equal (o1, o2);
      }

    case MODOP_EXPR:
      {
	tree t1_op1, t2_op1;

	if (!cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
	  return false;

	t1_op1 = TREE_OPERAND (t1, 1);
	t2_op1 = TREE_OPERAND (t2, 1);
	if (TREE_CODE (t1_op1) != TREE_CODE (t2_op1))
	  return false;

	return cp_tree_equal (TREE_OPERAND (t1, 2), TREE_OPERAND (t2, 2));
      }

    case PTRMEM_CST:
      /* Two pointer-to-members are the same if they point to the same
	 field or function in the same class.  */
      if (PTRMEM_CST_MEMBER (t1) != PTRMEM_CST_MEMBER (t2))
	return false;

      return same_type_p (PTRMEM_CST_CLASS (t1), PTRMEM_CST_CLASS (t2));

    case OVERLOAD:
      {
	/* Two overloads. Must be exactly the same set of decls.  */
	lkp_iterator first (t1);
	lkp_iterator second (t2);

	for (; first && second; ++first, ++second)
	  if (*first != *second)
	    return false;
	return !(first || second);
      }

    case TRAIT_EXPR:
      if (TRAIT_EXPR_KIND (t1) != TRAIT_EXPR_KIND (t2))
	return false;
      return cp_tree_equal (TRAIT_EXPR_TYPE1 (t1), TRAIT_EXPR_TYPE1 (t2))
	&& cp_tree_equal (TRAIT_EXPR_TYPE2 (t1), TRAIT_EXPR_TYPE2 (t2));

    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      /* Used for location wrappers with possibly NULL types.  */
      if (!TREE_TYPE (t1) || !TREE_TYPE (t2))
	{
	  if (TREE_TYPE (t1) || TREE_TYPE (t2))
	    return false;
	  break;
	}
      /* FALLTHROUGH  */

    case CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    case NEW_EXPR:
    case BIT_CAST_EXPR:
    CASE_CONVERT:
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      /* Now compare operands as usual.  */
      break;

    case DEFERRED_NOEXCEPT:
      return (cp_tree_equal (DEFERRED_NOEXCEPT_PATTERN (t1),
			     DEFERRED_NOEXCEPT_PATTERN (t2))
	      && comp_template_args (DEFERRED_NOEXCEPT_ARGS (t1),
				     DEFERRED_NOEXCEPT_ARGS (t2)));

    case LAMBDA_EXPR:
      /* Two lambda-expressions are never considered equivalent.  */
      return false;

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      {
	tree p1 = ARGUMENT_PACK_ARGS (t1);
	tree p2 = ARGUMENT_PACK_ARGS (t2);
	int len = TREE_VEC_LENGTH (p1);
	if (TREE_VEC_LENGTH (p2) != len)
	  return false;

	for (int ix = 0; ix != len; ix++)
	  if (!template_args_equal (TREE_VEC_ELT (p1, ix),
				    TREE_VEC_ELT (p2, ix)))
	    return false;
	return true;
      }

    case EXPR_PACK_EXPANSION:
      if (!cp_tree_equal (PACK_EXPANSION_PATTERN (t1),
			  PACK_EXPANSION_PATTERN (t2)))
	return false;
      if (!comp_template_args (PACK_EXPANSION_EXTRA_ARGS (t1),
			       PACK_EXPANSION_EXTRA_ARGS (t2)))
	return false;
      return true;

    case PACK_INDEX_EXPR:
      if (!cp_tree_equal (PACK_INDEX_PACK (t1),
			  PACK_INDEX_PACK (t2)))
	return false;
      if (!cp_tree_equal (PACK_INDEX_INDEX (t1),
			  PACK_INDEX_INDEX (t2)))
	return false;
      return true;

    case COMPONENT_REF:
      /* If we're comparing contract conditions of overrides, member references
	 compare equal if they designate the same member.  */
      if (comparing_override_contracts)
	return equivalent_member_references (t1, t2);
      break;

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
    case tcc_statement:
      {
	int n = cp_tree_operand_length (t1);
	if (TREE_CODE_CLASS (code1) == tcc_vl_exp
	    && n != TREE_OPERAND_LENGTH (t2))
	  return false;

	for (int i = 0; i < n; ++i)
	  if (!cp_tree_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i)))
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

/* The type of ARG when used as an lvalue.  */

tree
lvalue_type (tree arg)
{
  tree type = TREE_TYPE (arg);
  return type;
}

/* The type of ARG for printing error messages; denote lvalues with
   reference types.  */

tree
error_type (tree arg)
{
  tree type = TREE_TYPE (arg);

  if (TREE_CODE (type) == ARRAY_TYPE)
    ;
  else if (TREE_CODE (type) == ERROR_MARK)
    ;
  else if (lvalue_p (arg))
    type = build_reference_type (lvalue_type (arg));
  else if (MAYBE_CLASS_TYPE_P (type))
    type = lvalue_type (arg);

  return type;
}

/* Does FUNCTION use a variable-length argument list?  */

int
varargs_function_p (const_tree function)
{
  return stdarg_p (TREE_TYPE (function));
}

/* Returns 1 if decl is a member of a class.  */

int
member_p (const_tree decl)
{
  const_tree const ctx = DECL_CONTEXT (decl);
  return (ctx && TYPE_P (ctx));
}

/* Create a placeholder for member access where we don't actually have an
   object that the access is against.  For a general declval<T> equivalent,
   use build_stub_object instead.  */

tree
build_dummy_object (tree type)
{
  tree decl = build1 (CONVERT_EXPR, build_pointer_type (type), void_node);
  return cp_build_fold_indirect_ref (decl);
}

/* We've gotten a reference to a member of TYPE.  Return *this if appropriate,
   or a dummy object otherwise.  If BINFOP is non-0, it is filled with the
   binfo path from current_class_type to TYPE, or 0.  */

tree
maybe_dummy_object (tree type, tree* binfop)
{
  tree decl, context;
  tree binfo;
  tree current = current_nonlambda_class_type ();

  if (current
      && (binfo = lookup_base (current, type, ba_any, NULL,
			       tf_warning_or_error)))
    context = current;
  else
    {
      /* Reference from a nested class member function.  */
      context = type;
      binfo = TYPE_BINFO (type);
    }

  if (binfop)
    *binfop = binfo;

  /* current_class_ref might not correspond to current_class_type if
     we're in tsubst_default_argument or a lambda-declarator; in either
     case, we want to use current_class_ref if it matches CONTEXT.  */
  tree ctype = current_class_ref ? TREE_TYPE (current_class_ref) : NULL_TREE;
  if (ctype
      && same_type_ignoring_top_level_qualifiers_p (ctype, context))
    decl = current_class_ref;
  else
    {
      /* Return a dummy object whose cv-quals are consistent with (the
	 non-lambda) 'this' if available.  */
      if (ctype)
	{
	  int quals = TYPE_UNQUALIFIED;
	  if (tree lambda = CLASSTYPE_LAMBDA_EXPR (ctype))
	    {
	      if (tree cap = lambda_expr_this_capture (lambda, false))
		quals = cp_type_quals (TREE_TYPE (TREE_TYPE (cap)));
	    }
	  else
	    quals = cp_type_quals (ctype);
	  context = cp_build_qualified_type (context, quals);
	}
      decl = build_dummy_object (context);
    }

  return decl;
}

/* Returns 1 if OB is a placeholder object, or a pointer to one.  */

bool
is_dummy_object (const_tree ob)
{
  if (INDIRECT_REF_P (ob))
    ob = TREE_OPERAND (ob, 0);
  return (TREE_CODE (ob) == CONVERT_EXPR
	  && TREE_OPERAND (ob, 0) == void_node);
}

/* Returns true if TYPE is char, unsigned char, or std::byte.  */

bool
is_byte_access_type (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  if (type == char_type_node
      || type == unsigned_char_type_node)
    return true;

  return (TREE_CODE (type) == ENUMERAL_TYPE
	  && TYPE_CONTEXT (type) == std_node
	  && !strcmp ("byte", TYPE_NAME_STRING (type)));
}

/* Returns true if TYPE is unsigned char or std::byte.  */

bool
is_byte_access_type_not_plain_char (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  if (type == char_type_node)
    return false;

  return is_byte_access_type (type);
}

/* Returns 1 iff type T is something we want to treat as a scalar type for
   the purpose of deciding whether it is trivial/POD/standard-layout.  */

bool
scalarish_type_p (const_tree t)
{
  if (t == error_mark_node)
    return 1;

  return (SCALAR_TYPE_P (t) || VECTOR_TYPE_P (t));
}

/* Returns true iff T requires non-trivial default initialization.  */

bool
type_has_nontrivial_default_init (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    return TYPE_HAS_COMPLEX_DFLT (t);
  else
    return 0;
}

/* Track classes with only deleted copy/move constructors so that we can warn
   if they are used in call/return by value.  */

static GTY(()) hash_set<tree>* deleted_copy_types;
static void
remember_deleted_copy (const_tree t)
{
  if (!deleted_copy_types)
    deleted_copy_types = hash_set<tree>::create_ggc(37);
  deleted_copy_types->add (CONST_CAST_TREE (t));
}
void
maybe_warn_parm_abi (tree t, location_t loc)
{
  if (!deleted_copy_types
      || !deleted_copy_types->contains (t))
    return;

  if ((flag_abi_version == 12 || warn_abi_version == 12)
      && classtype_has_non_deleted_move_ctor (t))
    {
      bool w;
      auto_diagnostic_group d;
      if (flag_abi_version > 12)
	w = warning_at (loc, OPT_Wabi, "%<-fabi-version=13%> (GCC 8.2) fixes "
			"the calling convention for %qT, which was "
			"accidentally changed in 8.1", t);
      else
	w = warning_at (loc, OPT_Wabi, "%<-fabi-version=12%> (GCC 8.1) "
			"accidentally changes the calling convention for %qT",
			t);
      if (w)
	inform (location_of (t), " declared here");
      return;
    }

  auto_diagnostic_group d;
  if (warning_at (loc, OPT_Wabi, "the calling convention for %qT changes in "
		  "%<-fabi-version=13%> (GCC 8.2)", t))
    inform (location_of (t), " because all of its copy and move "
	    "constructors are deleted");
}

/* Returns true iff copying an object of type T (including via move
   constructor) is non-trivial.  That is, T has no non-trivial copy
   constructors and no non-trivial move constructors, and not all copy/move
   constructors are deleted.  This function implements the ABI notion of
   non-trivial copy, which has diverged from the one in the standard.  */

bool
type_has_nontrivial_copy_init (const_tree type)
{
  tree t = strip_array_types (CONST_CAST_TREE (type));

  if (CLASS_TYPE_P (t))
    {
      gcc_assert (COMPLETE_TYPE_P (t));

      if (TYPE_HAS_COMPLEX_COPY_CTOR (t)
	  || TYPE_HAS_COMPLEX_MOVE_CTOR (t))
	/* Nontrivial.  */
	return true;

      if (cxx_dialect < cxx11)
	/* No deleted functions before C++11.  */
	return false;

      /* Before ABI v12 we did a bitwise copy of types with only deleted
	 copy/move constructors.  */
      if (!abi_version_at_least (12)
	  && !(warn_abi && abi_version_crosses (12)))
	return false;

      bool saw_copy = false;
      bool saw_non_deleted = false;
      bool saw_non_deleted_move = false;

      if (CLASSTYPE_LAZY_MOVE_CTOR (t))
	saw_copy = saw_non_deleted = true;
      else if (CLASSTYPE_LAZY_COPY_CTOR (t))
	{
	  saw_copy = true;
	  if (classtype_has_move_assign_or_move_ctor_p (t, true))
	    /* [class.copy]/8 If the class definition declares a move
	       constructor or move assignment operator, the implicitly declared
	       copy constructor is defined as deleted.... */;
	  else
	    /* Any other reason the implicitly-declared function would be
	       deleted would also cause TYPE_HAS_COMPLEX_COPY_CTOR to be
	       set.  */
	    saw_non_deleted = true;
	}

      if (!saw_non_deleted)
	for (ovl_iterator iter (CLASSTYPE_CONSTRUCTORS (t)); iter; ++iter)
	  {
	    tree fn = *iter;
	    if (copy_fn_p (fn))
	      {
		saw_copy = true;
		if (!DECL_DELETED_FN (fn))
		  {
		    /* Not deleted, therefore trivial.  */
		    saw_non_deleted = true;
		    break;
		  }
	      }
	    else if (move_fn_p (fn))
	      if (!DECL_DELETED_FN (fn))
		saw_non_deleted_move = true;
	  }

      gcc_assert (saw_copy);

      /* ABI v12 buggily ignored move constructors.  */
      bool v11nontriv = false;
      bool v12nontriv = !saw_non_deleted;
      bool v13nontriv = !saw_non_deleted && !saw_non_deleted_move;
      bool nontriv = (abi_version_at_least (13) ? v13nontriv
		      : flag_abi_version == 12 ? v12nontriv
		      : v11nontriv);
      bool warn_nontriv = (warn_abi_version >= 13 ? v13nontriv
			   : warn_abi_version == 12 ? v12nontriv
			   : v11nontriv);
      if (nontriv != warn_nontriv)
	remember_deleted_copy (t);

      return nontriv;
    }
  else
    return 0;
}

/* Returns 1 iff type T is a trivially copyable type, as defined in
   [basic.types] and [class].  */

bool
trivially_copyable_p (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    return ((!TYPE_HAS_COPY_CTOR (t)
	     || !TYPE_HAS_COMPLEX_COPY_CTOR (t))
	    && !TYPE_HAS_COMPLEX_MOVE_CTOR (t)
	    && (!TYPE_HAS_COPY_ASSIGN (t)
		|| !TYPE_HAS_COMPLEX_COPY_ASSIGN (t))
	    && !TYPE_HAS_COMPLEX_MOVE_ASSIGN (t)
	    && TYPE_HAS_TRIVIAL_DESTRUCTOR (t));
  else
    /* CWG 2094 makes volatile-qualified scalars trivially copyable again.  */
    return scalarish_type_p (t);
}

/* Returns 1 iff type T is a trivial type, as defined in [basic.types] and
   [class].  */

bool
trivial_type_p (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    /* A trivial class is a class that is trivially copyable and has one or
       more eligible default constructors, all of which are trivial.  */
    return (type_has_non_deleted_trivial_default_ctor (CONST_CAST_TREE (t))
	    && trivially_copyable_p (t));
  else
    return scalarish_type_p (t);
}

/* Returns 1 iff type T is a POD type, as defined in [basic.types].  */

bool
pod_type_p (const_tree t)
{
  /* This CONST_CAST is okay because strip_array_types returns its
     argument unmodified and we assign it to a const_tree.  */
  t = strip_array_types (CONST_CAST_TREE(t));

  if (!CLASS_TYPE_P (t))
    return scalarish_type_p (t);
  else if (cxx_dialect > cxx98)
    /* [class]/10: A POD struct is a class that is both a trivial class and a
       standard-layout class, and has no non-static data members of type
       non-POD struct, non-POD union (or array of such types).

       We don't need to check individual members because if a member is
       non-std-layout or non-trivial, the class will be too.  */
    return (std_layout_type_p (t) && trivial_type_p (t));
  else
    /* The C++98 definition of POD is different.  */
    return !CLASSTYPE_NON_LAYOUT_POD_P (t);
}

/* Returns true iff T is POD for the purpose of layout, as defined in the
   C++ ABI.  */

bool
layout_pod_type_p (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    return !CLASSTYPE_NON_LAYOUT_POD_P (t);
  else
    return scalarish_type_p (t);
}

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

static bool record_has_unique_obj_representations (const_tree, const_tree);

/* Returns true iff T satisfies std::has_unique_object_representations<T>,
   as defined in [meta.unary.prop].  */

bool
type_has_unique_obj_representations (const_tree t)
{
  bool ret;

  t = strip_array_types (CONST_CAST_TREE (t));

  if (!trivially_copyable_p (t))
    return false;

  if (CLASS_TYPE_P (t) && CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS_SET (t))
    return CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS (t);

  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* If some backend has any paddings in these types, we should add
	 a target hook for this and handle it there.  */
      return true;

    case BOOLEAN_TYPE:
      /* For bool values other than 0 and 1 should only appear with
	 undefined behavior.  */
      return true;

    case ENUMERAL_TYPE:
      return type_has_unique_obj_representations (ENUM_UNDERLYING_TYPE (t));

    case REAL_TYPE:
      /* XFmode certainly contains padding on x86, which the CPU doesn't store
	 when storing long double values, so for that we have to return false.
	 Other kinds of floating point values are questionable due to +.0/-.0
	 and NaNs, let's play safe for now.  */
      return false;

    case FIXED_POINT_TYPE:
      return false;

    case OFFSET_TYPE:
      return true;

    case COMPLEX_TYPE:
    case VECTOR_TYPE:
      return type_has_unique_obj_representations (TREE_TYPE (t));

    case RECORD_TYPE:
      ret = record_has_unique_obj_representations (t, TYPE_SIZE (t));
      if (CLASS_TYPE_P (t))
	{
	  CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS_SET (t) = 1;
	  CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS (t) = ret;
	}
      return ret;

    case UNION_TYPE:
      ret = true;
      bool any_fields;
      any_fields = false;
      for (tree field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    any_fields = true;
	    if (!type_has_unique_obj_representations (TREE_TYPE (field))
		|| simple_cst_equal (DECL_SIZE (field), TYPE_SIZE (t)) != 1)
	      {
		ret = false;
		break;
	      }
	  }
      if (!any_fields && !integer_zerop (TYPE_SIZE (t)))
	ret = false;
      if (CLASS_TYPE_P (t))
	{
	  CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS_SET (t) = 1;
	  CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS (t) = ret;
	}
      return ret;

    case NULLPTR_TYPE:
      return false;

    case ERROR_MARK:
      return false;

    default:
      gcc_unreachable ();
    }
}

/* Helper function for type_has_unique_obj_representations.  */

static bool
record_has_unique_obj_representations (const_tree t, const_tree sz)
{
  for (tree field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) != FIELD_DECL)
      ;
    /* For bases, can't use type_has_unique_obj_representations here, as in
	struct S { int i : 24; S (); };
	struct T : public S { int j : 8; T (); };
	S doesn't have unique obj representations, but T does.  */
    else if (DECL_FIELD_IS_BASE (field))
      {
	if (!record_has_unique_obj_representations (TREE_TYPE (field),
						    DECL_SIZE (field)))
	  return false;
      }
    else if (DECL_C_BIT_FIELD (field) && !DECL_UNNAMED_BIT_FIELD (field))
      {
	tree btype = DECL_BIT_FIELD_TYPE (field);
	if (!type_has_unique_obj_representations (btype))
	  return false;
      }
    else if (!type_has_unique_obj_representations (TREE_TYPE (field)))
      return false;

  offset_int cur = 0;
  for (tree field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL && !DECL_UNNAMED_BIT_FIELD (field))
      {
	offset_int fld = wi::to_offset (DECL_FIELD_OFFSET (field));
	offset_int bitpos = wi::to_offset (DECL_FIELD_BIT_OFFSET (field));
	fld = fld * BITS_PER_UNIT + bitpos;
	if (cur != fld)
	  return false;
	if (DECL_SIZE (field))
	  {
	    offset_int size = wi::to_offset (DECL_SIZE (field));
	    cur += size;
	  }
      }
  if (cur != wi::to_offset (sz))
    return false;

  return true;
}

/* Nonzero iff type T is a class template implicit specialization.  */

bool
class_tmpl_impl_spec_p (const_tree t)
{
  return CLASS_TYPE_P (t) && CLASSTYPE_TEMPLATE_INSTANTIATION (t);
}

/* Returns 1 iff zero initialization of type T means actually storing
   zeros in it.  */

int
zero_init_p (const_tree t)
{
  /* This CONST_CAST is okay because strip_array_types returns its
     argument unmodified and we assign it to a const_tree.  */
  t = strip_array_types (CONST_CAST_TREE(t));

  if (t == error_mark_node)
    return 1;

  /* NULL pointers to data members are initialized with -1.  */
  if (TYPE_PTRDATAMEM_P (t))
    return 0;

  /* Classes that contain types that can't be zero-initialized, cannot
     be zero-initialized themselves.  */
  if (CLASS_TYPE_P (t) && CLASSTYPE_NON_ZERO_INIT_P (t))
    return 0;

  return 1;
}

/* Returns true if the expression or initializer T is the result of
   zero-initialization for its type, taking pointers to members
   into consideration.  */

bool
zero_init_expr_p (tree t)
{
  tree type = TREE_TYPE (t);
  if (!type || uses_template_parms (type))
    return false;
  if (TYPE_PTRMEM_P (type))
    return null_member_pointer_value_p (t);
  if (TREE_CODE (t) == CONSTRUCTOR)
    {
      if (COMPOUND_LITERAL_P (t)
	  || BRACE_ENCLOSED_INITIALIZER_P (t))
	/* Undigested, conversions might change the zeroness.  */
	return false;
      for (constructor_elt &elt : CONSTRUCTOR_ELTS (t))
	{
	  if (TREE_CODE (type) == UNION_TYPE
	      && elt.index != first_field (type))
	    return false;
	  if (!zero_init_expr_p (elt.value))
	    return false;
	}
      return true;
    }
  if (zero_init_p (type))
    return initializer_zerop (t);
  return false;
}

/* True IFF T is a C++20 structural type (P1907R1) that can be used as a
   non-type template parameter.  If EXPLAIN, explain why not.  */

bool
structural_type_p (tree t, bool explain)
{
  /* A structural type is one of the following: */

  /* a scalar type, or */
  if (SCALAR_TYPE_P (t))
    return true;
  /* an lvalue reference type, or */
  if (TYPE_REF_P (t) && !TYPE_REF_IS_RVALUE (t))
    return true;
  /* a literal class type with the following properties:
     - all base classes and non-static data members are public and non-mutable
       and
     - the types of all bases classes and non-static data members are
       structural types or (possibly multi-dimensional) array thereof.  */
  if (!CLASS_TYPE_P (t))
    return false;
  if (!literal_type_p (t))
    {
      if (explain)
	explain_non_literal_class (t);
      return false;
    }
  for (tree m = next_aggregate_field (TYPE_FIELDS (t)); m;
       m = next_aggregate_field (DECL_CHAIN (m)))
    {
      if (TREE_PRIVATE (m) || TREE_PROTECTED (m))
	{
	  if (explain)
	    {
	      if (DECL_FIELD_IS_BASE (m))
		inform (location_of (m), "base class %qT is not public",
			TREE_TYPE (m));
	      else
		inform (location_of (m), "%qD is not public", m);
	    }
	  return false;
	}
      if (DECL_MUTABLE_P (m))
	{
	  if (explain)
	    inform (location_of (m), "%qD is mutable", m);
	  return false;
	}
      tree mtype = strip_array_types (TREE_TYPE (m));
      if (!structural_type_p (mtype))
	{
	  if (explain)
	    {
	      inform (location_of (m), "%qD has a non-structural type", m);
	      structural_type_p (mtype, true);
	    }
	  return false;
	}
    }
  return true;
}

/* Partially handle the C++11 [[carries_dependency]] attribute.
   Just emit a different diagnostics when it is used on something the
   spec doesn't allow vs. where it allows and we just choose to ignore
   it.  */

static tree
handle_carries_dependency_attribute (tree *node, tree name,
				     tree ARG_UNUSED (args),
				     int ARG_UNUSED (flags),
				     bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL
      && TREE_CODE (*node) != PARM_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute can only be applied to "
	       "functions or parameters", name);
      *no_add_attrs = true;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

/* Handle the C++17 [[nodiscard]] attribute, which is similar to the GNU
   warn_unused_result attribute.  */

static tree
handle_nodiscard_attribute (tree *node, tree name, tree args,
			    int /*flags*/, bool *no_add_attrs)
{
  if (args && TREE_CODE (TREE_VALUE (args)) != STRING_CST)
    {
      error ("%qE attribute argument must be a string constant", name);
      *no_add_attrs = true;
    }
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (*node)))
	  && !DECL_CONSTRUCTOR_P (*node))
	warning_at (DECL_SOURCE_LOCATION (*node),
		    OPT_Wattributes, "%qE attribute applied to %qD with void "
		    "return type", name, *node);
    }
  else if (OVERLOAD_TYPE_P (*node))
    /* OK */;
  else
    {
      warning (OPT_Wattributes, "%qE attribute can only be applied to "
	       "functions or to class or enumeration types", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

/* Handle a C++20 "no_unique_address" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_no_unique_addr_attribute (tree* node,
				 tree name,
				 tree /*args*/,
				 int /*flags*/,
				 bool* no_add_attrs)
{
  if (TREE_CODE (*node) == VAR_DECL)
    {
      DECL_MERGEABLE (*node) = true;
      if (pedantic)
	warning (OPT_Wattributes, "%qE attribute can only be applied to "
		 "non-static data members", name);
    }
  else if (TREE_CODE (*node) != FIELD_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute can only be applied to "
	       "non-static data members", name);
      *no_add_attrs = true;
    }
  else if (DECL_C_BIT_FIELD (*node))
    {
      warning (OPT_Wattributes, "%qE attribute cannot be applied to "
	       "a bit-field", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* The C++20 [[likely]] and [[unlikely]] attributes on labels map to the GNU
   hot/cold attributes.  */

static tree
handle_likeliness_attribute (tree *node, tree name, tree args,
			     int flags, bool *no_add_attrs)
{
  *no_add_attrs = true;
  if (TREE_CODE (*node) == LABEL_DECL
      || TREE_CODE (*node) == FUNCTION_DECL)
    {
      if (args)
	warning (OPT_Wattributes, "%qE attribute takes no arguments", name);
      tree bname = (is_attribute_p ("likely", name)
		    ? get_identifier ("hot") : get_identifier ("cold"));
      if (TREE_CODE (*node) == FUNCTION_DECL)
	warning (OPT_Wattributes, "ISO C++ %qE attribute does not apply to "
		 "functions; treating as %<[[gnu::%E]]%>", name, bname);
      tree battr = build_tree_list (bname, NULL_TREE);
      decl_attributes (node, battr, flags);
      return NULL_TREE;
    }
  else
    return error_mark_node;
}

/* The C++11 alignment specifier.  It mostly maps to GNU aligned attribute,
   but we need to do some extra pedantic checking.  */

static tree
handle_alignas_attribute (tree *node, tree name, tree args, int flags,
			  bool *no_add_attrs)
{
  tree t = *node;
  tree ret = handle_aligned_attribute (node, name, args, flags, no_add_attrs);
  if (pedantic)
    {
      if (TREE_CODE (*node) == FUNCTION_DECL)
	pedwarn (input_location, OPT_Wattributes,
		 "%<alignas%> on function declaration");
      else if (TREE_CODE (*node) == ENUMERAL_TYPE)
	pedwarn (input_location, OPT_Wattributes,
		 "%<alignas%> on enumerated type");
      else if (TYPE_P (*node) && t != *node)
	pedwarn (input_location, OPT_Wattributes,
		 "%<alignas%> on a type other than class");
      else if (TREE_CODE (*node) == FIELD_DECL && DECL_C_BIT_FIELD (*node))
	pedwarn (input_location, OPT_Wattributes, "%<alignas%> on bit-field");
      else if (TREE_CODE (t) == TYPE_DECL)
	pedwarn (input_location, OPT_Wattributes,
		 "%<alignas%> on a type alias");
    }
  return ret;
}

/* The C++14 [[deprecated]] attribute mostly maps to the GNU deprecated
   attribute.  */

static tree
handle_std_deprecated_attribute (tree *node, tree name, tree args, int flags,
				 bool *no_add_attrs)
{
  tree t = *node;
  tree ret = handle_deprecated_attribute (node, name, args, flags,
					  no_add_attrs);
  if (TYPE_P (*node) && t != *node)
    pedwarn (input_location, OPT_Wattributes,
	     "%qE on a type other than class or enumeration definition", name);
  else if (TREE_CODE (*node) == FIELD_DECL && DECL_UNNAMED_BIT_FIELD (*node))
    pedwarn (input_location, OPT_Wattributes, "%qE on unnamed bit-field",
	     name);
  return ret;
}

/* The C++17 [[maybe_unused]] attribute mostly maps to the GNU unused
   attribute.  */

static tree
handle_maybe_unused_attribute (tree *node, tree name, tree args, int flags,
			       bool *no_add_attrs)
{
  tree t = *node;
  tree ret = handle_unused_attribute (node, name, args, flags, no_add_attrs);
  if (TYPE_P (*node) && t != *node)
    pedwarn (input_location, OPT_Wattributes,
	     "%qE on a type other than class or enumeration definition", name);
  else if (TREE_CODE (*node) == FIELD_DECL && DECL_UNNAMED_BIT_FIELD (*node))
    pedwarn (input_location, OPT_Wattributes, "%qE on unnamed bit-field",
	     name);
  else if (TREE_CODE (*node) == LABEL_DECL && DECL_NAME (*node) == NULL_TREE)
    pedwarn (input_location, OPT_Wattributes,
	     "%qE on %<case%> or %<default%> label", name);
  return ret;
}

/* Table of valid C++ attributes.  */
static const attribute_spec cxx_gnu_attributes[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "init_priority",  1, 1, true,  false, false, false,
    handle_init_priority_attribute, NULL },
  { "abi_tag", 1, -1, false, false, false, true,
    handle_abi_tag_attribute, NULL },
  { "no_dangling", 0, 1, false, true, false, false,
    handle_no_dangling_attribute, NULL },
};

const scoped_attribute_specs cxx_gnu_attribute_table =
{
  "gnu", { cxx_gnu_attributes }
};

/* Table of C++ standard attributes.  */
static const attribute_spec std_attributes[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "deprecated", 0, 1, false, false, false, false,
    handle_std_deprecated_attribute, NULL },
  { "maybe_unused", 0, 0, false, false, false, false,
    handle_maybe_unused_attribute, NULL },
  { "nodiscard", 0, 1, false, false, false, false,
    handle_nodiscard_attribute, NULL },
  { "no_unique_address", 0, 0, true, false, false, false,
    handle_no_unique_addr_attribute, NULL },
  { "likely", 0, 0, false, false, false, false,
    handle_likeliness_attribute, attr_cold_hot_exclusions },
  { "unlikely", 0, 0, false, false, false, false,
    handle_likeliness_attribute, attr_cold_hot_exclusions },
  { "noreturn", 0, 0, true, false, false, false,
    handle_noreturn_attribute, attr_noreturn_exclusions },
  { "carries_dependency", 0, 0, true, false, false, false,
    handle_carries_dependency_attribute, NULL },
  { "pre", 0, -1, false, false, false, false,
    handle_contract_attribute, NULL },
  { "post", 0, -1, false, false, false, false,
    handle_contract_attribute, NULL }
};

const scoped_attribute_specs std_attribute_table =
{
  nullptr, { std_attributes }
};

/* Table of internal attributes.  */
static const attribute_spec internal_attributes[] =
{
  { "aligned", 0, 1, false, false, false, false,
    handle_alignas_attribute, attr_aligned_exclusions }
};

const scoped_attribute_specs internal_attribute_table =
{
  "internal ", { internal_attributes }
};

/* Handle an "init_priority" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_init_priority_attribute (tree* node,
				tree name,
				tree args,
				int /*flags*/,
				bool* no_add_attrs)
{
  if (!SUPPORTS_INIT_PRIORITY)
    /* Treat init_priority as an unrecognized attribute (mirroring
       __has_attribute) if the target doesn't support init priorities.  */
    return error_mark_node;

  tree initp_expr = TREE_VALUE (args);
  tree decl = *node;
  tree type = TREE_TYPE (decl);
  int pri;

  STRIP_NOPS (initp_expr);
  initp_expr = default_conversion (initp_expr);
  if (initp_expr)
    initp_expr = maybe_constant_value (initp_expr);

  if (!initp_expr || TREE_CODE (initp_expr) != INTEGER_CST)
    {
      error ("requested %<init_priority%> is not an integer constant");
      cxx_constant_value (initp_expr);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  pri = TREE_INT_CST_LOW (initp_expr);

  type = strip_array_types (type);

  if (decl == NULL_TREE
      || !VAR_P (decl)
      || !TREE_STATIC (decl)
      || DECL_EXTERNAL (decl)
      || (TREE_CODE (type) != RECORD_TYPE
	  && TREE_CODE (type) != UNION_TYPE)
      /* Static objects in functions are initialized the
	 first time control passes through that
	 function. This is not precise enough to pin down an
	 init_priority value, so don't allow it.  */
      || current_function_decl)
    {
      error ("can only use %qE attribute on file-scope definitions "
	     "of objects of class type", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (pri > MAX_INIT_PRIORITY || pri <= 0)
    {
      error ("requested %<init_priority%> %i is out of range [0, %i]",
	     pri, MAX_INIT_PRIORITY);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Check for init_priorities that are reserved for
     language and runtime support implementations.*/
  if (pri <= MAX_RESERVED_INIT_PRIORITY
      && !in_system_header_at (input_location))
    {
      warning
	(0, "requested %<init_priority%> %i is reserved for internal use",
	 pri);
    }

  SET_DECL_INIT_PRIORITY (decl, pri);
  DECL_HAS_INIT_PRIORITY_P (decl) = 1;
  return NULL_TREE;
}

/* DECL is being redeclared; the old declaration had the abi tags in OLD,
   and the new one has the tags in NEW_.  Give an error if there are tags
   in NEW_ that weren't in OLD.  */

bool
check_abi_tag_redeclaration (const_tree decl, const_tree old, const_tree new_)
{
  if (old && TREE_CODE (TREE_VALUE (old)) == TREE_LIST)
    old = TREE_VALUE (old);
  if (new_ && TREE_CODE (TREE_VALUE (new_)) == TREE_LIST)
    new_ = TREE_VALUE (new_);
  bool err = false;
  auto_diagnostic_group d;
  for (const_tree t = new_; t; t = TREE_CHAIN (t))
    {
      tree str = TREE_VALUE (t);
      for (const_tree in = old; in; in = TREE_CHAIN (in))
	{
	  tree ostr = TREE_VALUE (in);
	  if (cp_tree_equal (str, ostr))
	    goto found;
	}
      error ("redeclaration of %qD adds abi tag %qE", decl, str);
      err = true;
    found:;
    }
  if (err)
    {
      inform (DECL_SOURCE_LOCATION (decl), "previous declaration here");
      return false;
    }
  return true;
}

/* The abi_tag attribute with the name NAME was given ARGS.  If they are
   ill-formed, give an error and return false; otherwise, return true.  */

bool
check_abi_tag_args (tree args, tree name)
{
  if (!args)
    {
      error ("the %qE attribute requires arguments", name);
      return false;
    }
  for (tree arg = args; arg; arg = TREE_CHAIN (arg))
    {
      tree elt = TREE_VALUE (arg);
      if (TREE_CODE (elt) != STRING_CST
	  || (!same_type_ignoring_top_level_qualifiers_p
	      (strip_array_types (TREE_TYPE (elt)),
	       char_type_node)))
	{
	  error ("arguments to the %qE attribute must be narrow string "
		 "literals", name);
	  return false;
	}
      const char *begin = TREE_STRING_POINTER (elt);
      const char *end = begin + TREE_STRING_LENGTH (elt);
      for (const char *p = begin; p != end; ++p)
	{
	  char c = *p;
	  if (p == begin)
	    {
	      if (!ISALPHA (c) && c != '_')
		{
		  auto_diagnostic_group d;
		  error ("arguments to the %qE attribute must contain valid "
			 "identifiers", name);
		  inform (input_location, "%<%c%> is not a valid first "
			  "character for an identifier", c);
		  return false;
		}
	    }
	  else if (p == end - 1)
	    gcc_assert (c == 0);
	  else
	    {
	      if (!ISALNUM (c) && c != '_')
		{
		  auto_diagnostic_group d;
		  error ("arguments to the %qE attribute must contain valid "
			 "identifiers", name);
		  inform (input_location, "%<%c%> is not a valid character "
			  "in an identifier", c);
		  return false;
		}
	    }
	}
    }
  return true;
}

/* Handle an "abi_tag" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_abi_tag_attribute (tree* node, tree name, tree args,
			  int flags, bool* no_add_attrs)
{
  if (!check_abi_tag_args (args, name))
    goto fail;

  if (TYPE_P (*node))
    {
      if (!OVERLOAD_TYPE_P (*node))
	{
	  error ("%qE attribute applied to non-class, non-enum type %qT",
		 name, *node);
	  goto fail;
	}
      else if (!(flags & (int)ATTR_FLAG_TYPE_IN_PLACE))
	{
	  error ("%qE attribute applied to %qT after its definition",
		 name, *node);
	  goto fail;
	}
      else if (CLASS_TYPE_P (*node)
	       && CLASSTYPE_TEMPLATE_INSTANTIATION (*node))
	{
	  warning (OPT_Wattributes, "ignoring %qE attribute applied to "
		   "template instantiation %qT", name, *node);
	  goto fail;
	}
      else if (CLASS_TYPE_P (*node)
	       && CLASSTYPE_TEMPLATE_SPECIALIZATION (*node))
	{
	  warning (OPT_Wattributes, "ignoring %qE attribute applied to "
		   "template specialization %qT", name, *node);
	  goto fail;
	}

      tree attributes = TYPE_ATTRIBUTES (*node);
      tree decl = TYPE_NAME (*node);

      /* Make sure all declarations have the same abi tags.  */
      if (DECL_SOURCE_LOCATION (decl) != input_location)
	{
	  if (!check_abi_tag_redeclaration (decl,
					    lookup_attribute ("abi_tag",
							      attributes),
					    args))
	    goto fail;
	}
    }
  else
    {
      if (!VAR_OR_FUNCTION_DECL_P (*node))
	{
	  error ("%qE attribute applied to non-function, non-variable %qD",
		 name, *node);
	  goto fail;
	}
      else if (DECL_LANGUAGE (*node) == lang_c)
	{
	  error ("%qE attribute applied to extern \"C\" declaration %qD",
		 name, *node);
	  goto fail;
	}
    }

  return NULL_TREE;

 fail:
  *no_add_attrs = true;
  return NULL_TREE;
}

/* Perform checking for contract attributes.  */

tree
handle_contract_attribute (tree *ARG_UNUSED (node), tree ARG_UNUSED (name),
			   tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			   bool *ARG_UNUSED (no_add_attrs))
{
  /* TODO: Is there any checking we could do here?  */
  return NULL_TREE;
}

/* Handle a "no_dangling" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_no_dangling_attribute (tree *node, tree name, tree args, int,
			      bool *no_add_attrs)
{
  if (args && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
    {
      error ("%qE attribute argument must be an expression that evaluates "
	     "to true or false", name);
      *no_add_attrs = true;
    }
  else if (!FUNC_OR_METHOD_TYPE_P (*node)
	   && !RECORD_OR_UNION_TYPE_P (*node))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Return a new PTRMEM_CST of the indicated TYPE.  The MEMBER is the
   thing pointed to by the constant.  */

tree
make_ptrmem_cst (tree type, tree member)
{
  tree ptrmem_cst = make_node (PTRMEM_CST);
  TREE_TYPE (ptrmem_cst) = type;
  PTRMEM_CST_MEMBER (ptrmem_cst) = member;
  PTRMEM_CST_LOCATION (ptrmem_cst) = input_location;
  return ptrmem_cst;
}

/* Build a variant of TYPE that has the indicated ATTRIBUTES.  May
   return an existing type if an appropriate type already exists.  */

tree
cp_build_type_attribute_variant (tree type, tree attributes)
{
  tree new_type;

  new_type = build_type_attribute_variant (type, attributes);
  if (FUNC_OR_METHOD_TYPE_P (new_type))
    gcc_checking_assert (cxx_type_hash_eq (type, new_type));

  /* Making a new main variant of a class type is broken.  */
  gcc_assert (!CLASS_TYPE_P (type) || new_type == type);

  return new_type;
}

/* Return TRUE if TYPE1 and TYPE2 are identical for type hashing purposes.
   Called only after doing all language independent checks.  */

bool
cxx_type_hash_eq (const_tree typea, const_tree typeb)
{
  gcc_assert (FUNC_OR_METHOD_TYPE_P (typea));

  if (type_memfn_rqual (typea) != type_memfn_rqual (typeb))
    return false;
  if (TYPE_HAS_LATE_RETURN_TYPE (typea) != TYPE_HAS_LATE_RETURN_TYPE (typeb))
    return false;
  return comp_except_specs (TYPE_RAISES_EXCEPTIONS (typea),
			    TYPE_RAISES_EXCEPTIONS (typeb), ce_exact);
}

/* Copy the language-specific type variant modifiers from TYPEB to TYPEA.  For
   C++, these are the exception-specifier and ref-qualifier.  */

tree
cxx_copy_lang_qualifiers (const_tree typea, const_tree typeb)
{
  tree type = CONST_CAST_TREE (typea);
  if (FUNC_OR_METHOD_TYPE_P (type))
    type = build_cp_fntype_variant (type, type_memfn_rqual (typeb),
				    TYPE_RAISES_EXCEPTIONS (typeb),
				    TYPE_HAS_LATE_RETURN_TYPE (typeb));
  return type;
}

/* Apply FUNC to all language-specific sub-trees of TP in a pre-order
   traversal.  Called from walk_tree.  */

tree
cp_walk_subtrees (tree *tp, int *walk_subtrees_p, walk_tree_fn func,
		  void *data, hash_set<tree> *pset)
{
  tree t = *tp;
  enum tree_code code = TREE_CODE (t);
  tree result;

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = cp_walk_tree (&(NODE), func, data, pset);	\
      if (result) goto out;				\
    }							\
  while (0)

  if (TYPE_P (t))
    {
      /* If *WALK_SUBTREES_P is 1, we're interested in the syntactic form of
	 the argument, so don't look through typedefs, but do walk into
	 template arguments for alias templates (and non-typedefed classes).

	 If *WALK_SUBTREES_P > 1, we're interested in type identity or
	 equivalence, so look through typedefs, ignoring template arguments for
	 alias templates, and walk into template args of classes.

	 See find_abi_tags_r for an example of setting *WALK_SUBTREES_P to 2
	 when that's the behavior the walk_tree_fn wants.  */
      if (*walk_subtrees_p == 1 && typedef_variant_p (t))
	{
	  if (tree ti = TYPE_ALIAS_TEMPLATE_INFO (t))
	    WALK_SUBTREE (TI_ARGS (ti));
	  *walk_subtrees_p = 0;
	  return NULL_TREE;
	}

      if (tree ti = TYPE_TEMPLATE_INFO (t))
	WALK_SUBTREE (TI_ARGS (ti));
    }

  /* Not one of the easy cases.  We must explicitly go through the
     children.  */
  result = NULL_TREE;
  switch (code)
    {
    case TEMPLATE_TYPE_PARM:
      if (template_placeholder_p (t))
	WALK_SUBTREE (CLASS_PLACEHOLDER_TEMPLATE (t));
      /* Fall through.  */
    case DEFERRED_PARSE:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case UNBOUND_CLASS_TEMPLATE:
    case TEMPLATE_PARM_INDEX:
    case TYPEOF_TYPE:
      /* None of these have subtrees other than those already walked
	 above.  */
      *walk_subtrees_p = 0;
      break;

    case TYPENAME_TYPE:
      WALK_SUBTREE (TYPE_CONTEXT (t));
      WALK_SUBTREE (TYPENAME_TYPE_FULLNAME (t));
      *walk_subtrees_p = 0;
      break;

    case BASELINK:
      if (BASELINK_QUALIFIED_P (t))
	WALK_SUBTREE (BINFO_TYPE (BASELINK_ACCESS_BINFO (t)));
      WALK_SUBTREE (BASELINK_FUNCTIONS (t));
      *walk_subtrees_p = 0;
      break;

    case PTRMEM_CST:
      WALK_SUBTREE (TREE_TYPE (t));
      *walk_subtrees_p = 0;
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_PURPOSE (t));
      break;

    case OVERLOAD:
      WALK_SUBTREE (OVL_FUNCTION (t));
      WALK_SUBTREE (OVL_CHAIN (t));
      *walk_subtrees_p = 0;
      break;

    case USING_DECL:
      WALK_SUBTREE (DECL_NAME (t));
      WALK_SUBTREE (USING_DECL_SCOPE (t));
      WALK_SUBTREE (USING_DECL_DECLS (t));
      *walk_subtrees_p = 0;
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	WALK_SUBTREE (TYPE_PTRMEMFUNC_FN_TYPE_RAW (t));
      break;

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      {
	tree args = ARGUMENT_PACK_ARGS (t);
	for (tree arg : tree_vec_range (args))
	  WALK_SUBTREE (arg);
      }
      break;

    case TYPE_PACK_EXPANSION:
      WALK_SUBTREE (TREE_TYPE (t));
      WALK_SUBTREE (PACK_EXPANSION_EXTRA_ARGS (t));
      *walk_subtrees_p = 0;
      break;

    case EXPR_PACK_EXPANSION:
      WALK_SUBTREE (TREE_OPERAND (t, 0));
      WALK_SUBTREE (PACK_EXPANSION_EXTRA_ARGS (t));
      *walk_subtrees_p = 0;
      break;

    case PACK_INDEX_TYPE:
    case PACK_INDEX_EXPR:
      WALK_SUBTREE (PACK_INDEX_PACK (t));
      WALK_SUBTREE (PACK_INDEX_INDEX (t));
      *walk_subtrees_p = 0;
      break;

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    case BIT_CAST_EXPR:
      if (TREE_TYPE (t))
	WALK_SUBTREE (TREE_TYPE (t));
      break;

    case CONSTRUCTOR:
      if (COMPOUND_LITERAL_P (t))
	WALK_SUBTREE (TREE_TYPE (t));
      break;

    case TRAIT_EXPR:
      WALK_SUBTREE (TRAIT_EXPR_TYPE1 (t));
      WALK_SUBTREE (TRAIT_EXPR_TYPE2 (t));
      *walk_subtrees_p = 0;
      break;

    case TRAIT_TYPE:
      WALK_SUBTREE (TRAIT_TYPE_TYPE1 (t));
      WALK_SUBTREE (TRAIT_TYPE_TYPE2 (t));
      *walk_subtrees_p = 0;
      break;

    case DECLTYPE_TYPE:
      {
	cp_unevaluated u;
	WALK_SUBTREE (DECLTYPE_TYPE_EXPR (t));
	*walk_subtrees_p = 0;
	break;
      }

    case ALIGNOF_EXPR:
    case SIZEOF_EXPR:
    case NOEXCEPT_EXPR:
      {
	cp_unevaluated u;
	WALK_SUBTREE (TREE_OPERAND (t, 0));
	*walk_subtrees_p = 0;
	break;
      }

    case REQUIRES_EXPR:
      {
	cp_unevaluated u;
	for (tree parm = REQUIRES_EXPR_PARMS (t); parm; parm = DECL_CHAIN (parm))
	  /* Walk the types of each parameter, but not the parameter itself,
	     since doing so would cause false positives in the unexpanded pack
	     checker if the requires-expr introduces a function parameter pack,
	     e.g. requires (Ts... ts) { }.   */
	  WALK_SUBTREE (TREE_TYPE (parm));
	WALK_SUBTREE (REQUIRES_EXPR_REQS (t));
	*walk_subtrees_p = 0;
	break;
      }

    case DECL_EXPR:
      /* User variables should be mentioned in BIND_EXPR_VARS
	 and their initializers and sizes walked when walking
	 the containing BIND_EXPR.  Compiler temporaries are
	 handled here.  And also normal variables in templates,
	 since do_poplevel doesn't build a BIND_EXPR then.  */
      if (VAR_P (TREE_OPERAND (t, 0))
	  && (processing_template_decl
	      || (DECL_ARTIFICIAL (TREE_OPERAND (t, 0))
		  && !TREE_STATIC (TREE_OPERAND (t, 0)))))
	{
	  tree decl = TREE_OPERAND (t, 0);
	  WALK_SUBTREE (DECL_INITIAL (decl));
	  WALK_SUBTREE (DECL_SIZE (decl));
	  WALK_SUBTREE (DECL_SIZE_UNIT (decl));
	}
      break;

    case LAMBDA_EXPR:
      /* Don't walk into the body of the lambda, but the capture initializers
	 are part of the enclosing context.  */
      for (tree cap = LAMBDA_EXPR_CAPTURE_LIST (t); cap;
	   cap = TREE_CHAIN (cap))
	WALK_SUBTREE (TREE_VALUE (cap));
      break;

    case CO_YIELD_EXPR:
      if (TREE_OPERAND (t, 1))
	/* Operand 1 is the tree for the relevant co_await which has any
	   interesting sub-trees.  */
	WALK_SUBTREE (TREE_OPERAND (t, 1));
      break;

    case CO_AWAIT_EXPR:
      if (TREE_OPERAND (t, 1))
	/* Operand 1 is frame variable.  */
	WALK_SUBTREE (TREE_OPERAND (t, 1));
      if (TREE_OPERAND (t, 2))
	/* Operand 2 has the initialiser, and we need to walk any subtrees
	   there.  */
	WALK_SUBTREE (TREE_OPERAND (t, 2));
      break;

    case CO_RETURN_EXPR:
      if (TREE_OPERAND (t, 0))
	{
	  if (VOID_TYPE_P (TREE_OPERAND (t, 0)))
	    /* For void expressions, operand 1 is a trivial call, and any
	       interesting subtrees will be part of operand 0.  */
	    WALK_SUBTREE (TREE_OPERAND (t, 0));
	  else if (TREE_OPERAND (t, 1))
	    /* Interesting sub-trees will be in the return_value () call
	       arguments.  */
	    WALK_SUBTREE (TREE_OPERAND (t, 1));
	}
      break;

    case STATIC_ASSERT:
      WALK_SUBTREE (STATIC_ASSERT_CONDITION (t));
      WALK_SUBTREE (STATIC_ASSERT_MESSAGE (t));
      break;

    default:
      return NULL_TREE;
    }

  /* We didn't find what we were looking for.  */
 out:
  return result;

#undef WALK_SUBTREE
}

/* Like save_expr, but for C++.  */

tree
cp_save_expr (tree expr)
{
  /* There is no reason to create a SAVE_EXPR within a template; if
     needed, we can create the SAVE_EXPR when instantiating the
     template.  Furthermore, the middle-end cannot handle C++-specific
     tree codes.  */
  if (processing_template_decl)
    return expr;

  /* TARGET_EXPRs are only expanded once.  */
  if (TREE_CODE (expr) == TARGET_EXPR)
    return expr;

  return save_expr (expr);
}

/* Initialize tree.cc.  */

void
init_tree (void)
{
  list_hash_table = hash_table<list_hasher>::create_ggc (61);
}

/* Returns the kind of special function that DECL (a FUNCTION_DECL)
   is.  Note that sfk_none is zero, so this function can be used as a
   predicate to test whether or not DECL is a special function.  */

special_function_kind
special_function_p (const_tree decl)
{
  /* Rather than doing all this stuff with magic names, we should
     probably have a field of type `special_function_kind' in
     DECL_LANG_SPECIFIC.  */
  if (DECL_INHERITED_CTOR (decl))
    return sfk_inheriting_constructor;
  if (DECL_COPY_CONSTRUCTOR_P (decl))
    return sfk_copy_constructor;
  if (DECL_MOVE_CONSTRUCTOR_P (decl))
    return sfk_move_constructor;
  if (DECL_CONSTRUCTOR_P (decl))
    return sfk_constructor;
  if (DECL_ASSIGNMENT_OPERATOR_P (decl)
      && DECL_OVERLOADED_OPERATOR_IS (decl, NOP_EXPR))
    {
      if (copy_fn_p (decl))
	return sfk_copy_assignment;
      if (move_fn_p (decl))
	return sfk_move_assignment;
    }
  if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (decl))
    return sfk_destructor;
  if (DECL_COMPLETE_DESTRUCTOR_P (decl))
    return sfk_complete_destructor;
  if (DECL_BASE_DESTRUCTOR_P (decl))
    return sfk_base_destructor;
  if (DECL_DELETING_DESTRUCTOR_P (decl))
    return sfk_deleting_destructor;
  if (DECL_CONV_FN_P (decl))
    return sfk_conversion;
  if (deduction_guide_p (decl))
    return sfk_deduction_guide;
  if (DECL_OVERLOADED_OPERATOR_CODE_RAW (decl) >= OVL_OP_EQ_EXPR
      && DECL_OVERLOADED_OPERATOR_CODE_RAW (decl) <= OVL_OP_SPACESHIP_EXPR)
    return sfk_comparison;

  return sfk_none;
}

/* As above, but only if DECL is a special member function as per 11.3.3
   [special]: default/copy/move ctor, copy/move assignment, or destructor.  */

special_function_kind
special_memfn_p (const_tree decl)
{
  switch (special_function_kind sfk = special_function_p (decl))
    {
    case sfk_constructor:
      if (!default_ctor_p (decl))
	break;
      gcc_fallthrough();
    case sfk_copy_constructor:
    case sfk_copy_assignment:
    case sfk_move_assignment:
    case sfk_move_constructor:
    case sfk_destructor:
      return sfk;

    default:
      break;
    }
  return sfk_none;
}

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

/* Returns the kind of linkage associated with the indicated DECL.  The
   value returned is as specified by the language standard; it is
   independent of implementation details regarding template
   instantiation, etc.  For example, it is possible that a declaration
   to which this function assigns external linkage would not show up
   as a global symbol when you run `nm' on the resulting object file.  */

linkage_kind
decl_linkage (tree decl)
{
  /* This function doesn't attempt to calculate the linkage from first
     principles as given in [basic.link].  Instead, it makes use of
     the fact that we have already set TREE_PUBLIC appropriately, and
     then handles a few special cases.  Ideally, we would calculate
     linkage first, and then transform that into a concrete
     implementation.  */

  /* An explicit type alias has no linkage.  Nor do the built-in declarations
     of 'int' and such.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && !DECL_IMPLICIT_TYPEDEF_P (decl))
    {
      /* But this could be a typedef name for linkage purposes, in which
	 case we're interested in the linkage of the main decl.  */
      if (decl == TYPE_NAME (TYPE_MAIN_VARIANT (TREE_TYPE (decl)))
	  /* Likewise for the injected-class-name.  */
	  || DECL_SELF_REFERENCE_P (decl))
	decl = TYPE_MAIN_DECL (TREE_TYPE (decl));
      else
	return lk_none;
    }

  /* Namespace-scope entities with no name usually have no linkage.  */
  if (NAMESPACE_SCOPE_P (decl)
      && (!DECL_NAME (decl) || IDENTIFIER_ANON_P (DECL_NAME (decl))))
    {
      if (TREE_CODE (decl) == TYPE_DECL && !TYPE_ANON_P (TREE_TYPE (decl)))
	/* This entity has a typedef name for linkage purposes.  */;
      else if (TREE_CODE (decl) == NAMESPACE_DECL && cxx_dialect >= cxx11)
	/* An anonymous namespace has internal linkage since C++11.  */
	return lk_internal;
      else
	return lk_none;
    }

  /* Fields and parameters have no linkage.  */
  if (TREE_CODE (decl) == FIELD_DECL || TREE_CODE (decl) == PARM_DECL)
    return lk_none;

  /* Things in block scope do not have linkage.  */
  if (decl_function_context (decl))
    return lk_none;

  /* Things in class scope have the linkage of their owning class.  */
  if (tree ctype = DECL_CLASS_CONTEXT (decl))
    return decl_linkage (TYPE_NAME (ctype));

  /* Anonymous namespaces don't provide internal linkage in C++98,
     but otherwise consider such declarations to be internal.  */
  if (cxx_dialect >= cxx11 && decl_internal_context_p (decl))
    return lk_internal;

  /* Templates don't properly propagate TREE_PUBLIC, consider the
     template result instead.  Any template that isn't a variable
     or function must be external linkage by this point.  */
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      decl = DECL_TEMPLATE_RESULT (decl);
      if (!decl || !VAR_OR_FUNCTION_DECL_P (decl))
	return lk_external;
    }

  /* Things that are TREE_PUBLIC have external linkage.  */
  if (TREE_PUBLIC (decl))
    return lk_external;

  /* All types have external linkage in C++98, since anonymous namespaces
     didn't explicitly confer internal linkage.  */
  if (TREE_CODE (decl) == TYPE_DECL && cxx_dialect < cxx11)
    return lk_external;

  /* Variables or function decls not marked as TREE_PUBLIC might still
     be external linkage, such as for template instantiations on targets
     without weak symbols, decls referring to internal-linkage entities,
     or compiler-generated entities; in such cases, decls really meant to
     have internal linkage will have DECL_THIS_STATIC set.  */
  if (VAR_OR_FUNCTION_DECL_P (decl) && !DECL_THIS_STATIC (decl))
    return lk_external;

  /* Everything else has internal linkage.  */
  return lk_internal;
}

/* Returns the storage duration of the object or reference associated with
   the indicated DECL, which should be a VAR_DECL or PARM_DECL.  */

duration_kind
decl_storage_duration (tree decl)
{
  if (TREE_CODE (decl) == PARM_DECL)
    return dk_auto;
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return dk_static;
  gcc_assert (VAR_P (decl));
  if (!TREE_STATIC (decl)
      && !DECL_EXTERNAL (decl))
    return dk_auto;
  if (CP_DECL_THREAD_LOCAL_P (decl))
    return dk_thread;
  return dk_static;
}

/* EXP is an expression that we want to pre-evaluate.  Returns (in
   *INITP) an expression that will perform the pre-evaluation.  The
   value returned by this function is a side-effect free expression
   equivalent to the pre-evaluated expression.  Callers must ensure
   that *INITP is evaluated before EXP.

   Note that if EXPR is a glvalue, the return value is a glvalue denoting the
   same address; this function does not guard against modification of the
   stored value like save_expr or get_target_expr do.  */

tree
stabilize_expr (tree exp, tree* initp)
{
  tree init_expr;

  if (!TREE_SIDE_EFFECTS (exp))
    init_expr = NULL_TREE;
  else if (VOID_TYPE_P (TREE_TYPE (exp)))
    {
      init_expr = exp;
      exp = void_node;
    }
  /* There are no expressions with REFERENCE_TYPE, but there can be call
     arguments with such a type; just treat it as a pointer.  */
  else if (TYPE_REF_P (TREE_TYPE (exp))
	   || SCALAR_TYPE_P (TREE_TYPE (exp))
	   || !glvalue_p (exp))
    {
      init_expr = get_target_expr (exp);
      exp = TARGET_EXPR_SLOT (init_expr);
      if (CLASS_TYPE_P (TREE_TYPE (exp)))
	exp = move (exp);
      else
	exp = rvalue (exp);
    }
  else
    {
      bool xval = !lvalue_p (exp);
      exp = cp_build_addr_expr (exp, tf_warning_or_error);
      init_expr = get_target_expr (exp);
      exp = TARGET_EXPR_SLOT (init_expr);
      exp = cp_build_fold_indirect_ref (exp);
      if (xval)
	exp = move (exp);
    }
  *initp = init_expr;

  gcc_assert (!TREE_SIDE_EFFECTS (exp) || TREE_THIS_VOLATILE (exp));
  return exp;
}

/* Add NEW_EXPR, an expression whose value we don't care about, after the
   similar expression ORIG.  */

tree
add_stmt_to_compound (tree orig, tree new_expr)
{
  if (!new_expr || !TREE_SIDE_EFFECTS (new_expr))
    return orig;
  if (!orig || !TREE_SIDE_EFFECTS (orig))
    return new_expr;
  return build2 (COMPOUND_EXPR, void_type_node, orig, new_expr);
}

/* Like stabilize_expr, but for a call whose arguments we want to
   pre-evaluate.  CALL is modified in place to use the pre-evaluated
   arguments, while, upon return, *INITP contains an expression to
   compute the arguments.  */

void
stabilize_call (tree call, tree *initp)
{
  tree inits = NULL_TREE;
  int i;
  int nargs = call_expr_nargs (call);

  if (call == error_mark_node || processing_template_decl)
    {
      *initp = NULL_TREE;
      return;
    }

  gcc_assert (TREE_CODE (call) == CALL_EXPR);

  for (i = 0; i < nargs; i++)
    {
      tree init;
      CALL_EXPR_ARG (call, i) =
	stabilize_expr (CALL_EXPR_ARG (call, i), &init);
      inits = add_stmt_to_compound (inits, init);
    }

  *initp = inits;
}

/* Like stabilize_expr, but for an AGGR_INIT_EXPR whose arguments we want
   to pre-evaluate.  CALL is modified in place to use the pre-evaluated
   arguments, while, upon return, *INITP contains an expression to
   compute the arguments.  */

static void
stabilize_aggr_init (tree call, tree *initp)
{
  tree inits = NULL_TREE;
  int i;
  int nargs = aggr_init_expr_nargs (call);

  if (call == error_mark_node)
    return;

  gcc_assert (TREE_CODE (call) == AGGR_INIT_EXPR);

  for (i = 0; i < nargs; i++)
    {
      tree init;
      AGGR_INIT_EXPR_ARG (call, i) =
	stabilize_expr (AGGR_INIT_EXPR_ARG (call, i), &init);
      inits = add_stmt_to_compound (inits, init);
    }

  *initp = inits;
}

/* Like stabilize_expr, but for an initialization.

   If the initialization is for an object of class type, this function
   takes care not to introduce additional temporaries.

   Returns TRUE iff the expression was successfully pre-evaluated,
   i.e., if INIT is now side-effect free, except for, possibly, a
   single call to a constructor.  */

bool
stabilize_init (tree init, tree *initp)
{
  tree t = init;

  *initp = NULL_TREE;

  if (t == error_mark_node || processing_template_decl)
    return true;

  if (TREE_CODE (t) == INIT_EXPR)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == TARGET_EXPR)
    t = TARGET_EXPR_INITIAL (t);

  /* If the RHS can be stabilized without breaking copy elision, stabilize
     it.  We specifically don't stabilize class prvalues here because that
     would mean an extra copy, but they might be stabilized below.  */
  if (TREE_CODE (init) == INIT_EXPR
      && TREE_CODE (t) != CONSTRUCTOR
      && TREE_CODE (t) != AGGR_INIT_EXPR
      && (SCALAR_TYPE_P (TREE_TYPE (t))
	  || glvalue_p (t)))
    {
      TREE_OPERAND (init, 1) = stabilize_expr (t, initp);
      return true;
    }

  if (TREE_CODE (t) == COMPOUND_EXPR
      && TREE_CODE (init) == INIT_EXPR)
    {
      tree last = expr_last (t);
      /* Handle stabilizing the EMPTY_CLASS_EXPR pattern.  */
      if (!TREE_SIDE_EFFECTS (last))
	{
	  *initp = t;
	  TREE_OPERAND (init, 1) = last;
	  return true;
	}
    }

  if (TREE_CODE (t) == CONSTRUCTOR)
    {
      /* Aggregate initialization: stabilize each of the field
	 initializers.  */
      unsigned i;
      constructor_elt *ce;
      bool good = true;
      vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
      for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
	{
	  tree type = TREE_TYPE (ce->value);
	  tree subinit;
	  if (TYPE_REF_P (type)
	      || SCALAR_TYPE_P (type))
	    ce->value = stabilize_expr (ce->value, &subinit);
	  else if (!stabilize_init (ce->value, &subinit))
	    good = false;
	  *initp = add_stmt_to_compound (*initp, subinit);
	}
      return good;
    }

  if (TREE_CODE (t) == CALL_EXPR)
    {
      stabilize_call (t, initp);
      return true;
    }

  if (TREE_CODE (t) == AGGR_INIT_EXPR)
    {
      stabilize_aggr_init (t, initp);
      return true;
    }

  /* The initialization is being performed via a bitwise copy -- and
     the item copied may have side effects.  */
  return !TREE_SIDE_EFFECTS (init);
}

/* Returns true if a cast to TYPE may appear in an integral constant
   expression.  */

bool
cast_valid_in_integral_constant_expression_p (tree type)
{
  return (INTEGRAL_OR_ENUMERATION_TYPE_P (type)
	  || cxx_dialect >= cxx11
	  || dependent_type_p (type)
	  || type == error_mark_node);
}

/* Return true if we need to fix linkage information of DECL.  */

static bool
cp_fix_function_decl_p (tree decl)
{
  /* Skip if DECL is not externally visible.  */
  if (!TREE_PUBLIC (decl))
    return false;

  /* We need to fix DECL if it a appears to be exported but with no
     function body.  Thunks do not have CFGs and we may need to
     handle them specially later.   */
  if (!gimple_has_body_p (decl)
      && !DECL_THUNK_P (decl)
      && !DECL_EXTERNAL (decl))
    {
      struct cgraph_node *node = cgraph_node::get (decl);

      /* Don't fix same_body aliases.  Although they don't have their own
	 CFG, they share it with what they alias to.  */
      if (!node || !node->alias || !node->num_references ())
	return true;
    }

  return false;
}

/* Clean the C++ specific parts of the tree T. */

void
cp_free_lang_data (tree t)
{
  if (FUNC_OR_METHOD_TYPE_P (t))
    {
      /* Default args are not interesting anymore.  */
      tree argtypes = TYPE_ARG_TYPES (t);
      while (argtypes)
        {
	  TREE_PURPOSE (argtypes) = 0;
	  argtypes = TREE_CHAIN (argtypes);
	}
    }
  else if (TREE_CODE (t) == FUNCTION_DECL
	   && cp_fix_function_decl_p (t))
    {
      /* If T is used in this translation unit at all,  the definition
	 must exist somewhere else since we have decided to not emit it
	 in this TU.  So make it an external reference.  */
      DECL_EXTERNAL (t) = 1;
      TREE_STATIC (t) = 0;
    }
  if (TREE_CODE (t) == NAMESPACE_DECL)
    /* We do not need the leftover chaining of namespaces from the
       binding level.  */
    DECL_CHAIN (t) = NULL_TREE;
}

/* Stub for c-common.  Please keep in sync with c-decl.cc.
   FIXME: If address space support is target specific, then this
   should be a C target hook.  But currently this is not possible,
   because this function is called via REGISTER_TARGET_PRAGMAS.  */
void
c_register_addr_space (const char * /*word*/, addr_space_t /*as*/)
{
}

/* Return the number of operands in T that we care about for things like
   mangling.  */

int
cp_tree_operand_length (const_tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (TREE_CODE_CLASS (code) == tcc_vl_exp)
    return VL_EXP_OPERAND_LENGTH (t);

  return cp_tree_code_length (code);
}

/* Like cp_tree_operand_length, but takes a tree_code CODE.  */

int
cp_tree_code_length (enum tree_code code)
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

    case EXPR_PACK_EXPANSION:
      return 1;

    default:
      return TREE_CODE_LENGTH (code);
    }
}

/* Implement -Wzero_as_null_pointer_constant.  Return true if the
   conditions for the warning hold, false otherwise.  */
bool
maybe_warn_zero_as_null_pointer_constant (tree expr, location_t loc)
{
  if (c_inhibit_evaluation_warnings == 0
      && !null_node_p (expr) && !NULLPTR_TYPE_P (TREE_TYPE (expr)))
    {
      warning_at (loc, OPT_Wzero_as_null_pointer_constant,
		  "zero as null pointer constant");
      return true;
    }
  return false;
}

/* FNDECL is a function declaration whose type may have been altered by
   adding extra parameters such as this, in-charge, or VTT.  When this
   takes place, the positional arguments supplied by the user (as in the
   'format' attribute arguments) may refer to the wrong argument.  This
   function returns an integer indicating how many arguments should be
   skipped.  */

int
maybe_adjust_arg_pos_for_attribute (const_tree fndecl)
{
  if (!fndecl)
    return 0;
  int n = num_artificial_parms_for (fndecl);
  /* The manual states that it's the user's responsibility to account
     for the implicit this parameter.  */
  return n > 0 ? n - 1 : 0;
}


/* Release memory we no longer need after parsing.  */
void
cp_tree_c_finish_parsing ()
{
  if (previous_class_level)
    invalidate_class_lookup_cache ();
  deleted_copy_types = NULL;
}

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)
/* Complain that some language-specific thing hanging off a tree
   node has been accessed improperly.  */

void
lang_check_failed (const char* file, int line, const char* function)
{
  internal_error ("%<lang_*%> check: failed in %s, at %s:%d",
		  function, trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

#if CHECKING_P

namespace selftest {

/* Verify that lvalue_kind () works, for various expressions,
   and that location wrappers don't affect the results.  */

static void
test_lvalue_kind ()
{
  location_t loc = BUILTINS_LOCATION;

  /* Verify constants and parameters, without and with
     location wrappers.  */
  tree int_cst = build_int_cst (integer_type_node, 42);
  ASSERT_EQ (clk_none, lvalue_kind (int_cst));

  tree wrapped_int_cst = maybe_wrap_with_location (int_cst, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_int_cst));
  ASSERT_EQ (clk_none, lvalue_kind (wrapped_int_cst));

  tree string_lit = build_string (4, "foo");
  TREE_TYPE (string_lit) = char_array_type_node;
  string_lit = fix_string_type (string_lit);
  ASSERT_EQ (clk_ordinary, lvalue_kind (string_lit));

  tree wrapped_string_lit = maybe_wrap_with_location (string_lit, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_string_lit));
  ASSERT_EQ (clk_ordinary, lvalue_kind (wrapped_string_lit));

  tree parm = build_decl (UNKNOWN_LOCATION, PARM_DECL,
			  get_identifier ("some_parm"),
			  integer_type_node);
  ASSERT_EQ (clk_ordinary, lvalue_kind (parm));

  tree wrapped_parm = maybe_wrap_with_location (parm, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_parm));
  ASSERT_EQ (clk_ordinary, lvalue_kind (wrapped_parm));

  /* Verify that lvalue_kind of std::move on a parm isn't
     affected by location wrappers.  */
  tree rvalue_ref_of_parm = move (parm);
  ASSERT_EQ (clk_rvalueref, lvalue_kind (rvalue_ref_of_parm));
  tree rvalue_ref_of_wrapped_parm = move (wrapped_parm);
  ASSERT_EQ (clk_rvalueref, lvalue_kind (rvalue_ref_of_wrapped_parm));

  /* Verify lvalue_p.  */
  ASSERT_FALSE (lvalue_p (int_cst));
  ASSERT_FALSE (lvalue_p (wrapped_int_cst));
  ASSERT_TRUE (lvalue_p (parm));
  ASSERT_TRUE (lvalue_p (wrapped_parm));
  ASSERT_FALSE (lvalue_p (rvalue_ref_of_parm));
  ASSERT_FALSE (lvalue_p (rvalue_ref_of_wrapped_parm));
}

/* Run all of the selftests within this file.  */

void
cp_tree_cc_tests ()
{
  test_lvalue_kind ();
}

} // namespace selftest

#endif /* #if CHECKING_P */


#include "gt-cp-tree.h"
