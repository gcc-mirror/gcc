/* Language-dependent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987-2015 Free Software Foundation, Inc.
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
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "fold-const.h"
#include "tree-hasher.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "tree-iterator.h"
#include "cp-tree.h"
#include "flags.h"
#include "tree-inline.h"
#include "debug.h"
#include "convert.h"
#include "hard-reg-set.h"
#include "function.h"
#include "cgraph.h"
#include "splay-tree.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "attribs.h"

static tree bot_manip (tree *, int *, void *);
static tree bot_replace (tree *, int *, void *);
static hashval_t list_hash_pieces (tree, tree, tree);
static tree build_target_expr (tree, tree, tsubst_flags_t);
static tree count_trees_r (tree *, int *, void *);
static tree verify_stmt_tree_r (tree *, int *, void *);
static tree build_local_temp (tree);

static tree handle_java_interface_attribute (tree *, tree, tree, int, bool *);
static tree handle_com_interface_attribute (tree *, tree, tree, int, bool *);
static tree handle_init_priority_attribute (tree *, tree, tree, int, bool *);
static tree handle_abi_tag_attribute (tree *, tree, tree, int, bool *);

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
      && TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
    {
      /* unnamed rvalue references are rvalues */
      if (TYPE_REF_IS_RVALUE (TREE_TYPE (ref))
	  && TREE_CODE (ref) != PARM_DECL
	  && !VAR_P (ref)
	  && TREE_CODE (ref) != COMPONENT_REF
	  /* Functions are always lvalues.  */
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (ref))) != FUNCTION_TYPE)
	return clk_rvalueref;

      /* lvalue references and named rvalue references are lvalues.  */
      return clk_ordinary;
    }

  if (ref == current_class_ptr)
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
    case WITH_CLEANUP_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return lvalue_kind (TREE_OPERAND (ref, 0));

    case MEMBER_REF:
    case DOTSTAR_EXPR:
      if (TREE_CODE (ref) == MEMBER_REF)
	op1_lvalue_kind = clk_ordinary;
      else
	op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
      if (TYPE_PTRMEMFUNC_P (TREE_TYPE (TREE_OPERAND (ref, 1))))
	op1_lvalue_kind = clk_none;
      return op1_lvalue_kind;

    case COMPONENT_REF:
      op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 0));
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
    case VAR_DECL:
      if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	  && DECL_LANG_SPECIFIC (ref)
	  && DECL_IN_AGGR_P (ref))
	return clk_none;
    case INDIRECT_REF:
    case ARROW_EXPR:
    case ARRAY_REF:
    case ARRAY_NOTATION_REF:
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
      op1_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 1)
				    ? TREE_OPERAND (ref, 1)
				    : TREE_OPERAND (ref, 0));
      op2_lvalue_kind = lvalue_kind (TREE_OPERAND (ref, 2));
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
      return (DECL_NONSTATIC_MEMBER_FUNCTION_P (ref)
	      ? clk_none : clk_ordinary);

    case BASELINK:
      /* We now represent a reference to a single static member function
	 with a BASELINK.  */
      /* This CONST_CAST is okay because BASELINK_FUNCTIONS returns
	 its argument unmodified and we assign it to a const_tree.  */
      return lvalue_kind (BASELINK_FUNCTIONS (CONST_CAST_TREE (ref)));

    case NON_DEPENDENT_EXPR:
      /* We just return clk_ordinary for NON_DEPENDENT_EXPR in C++98, but
	 in C++11 lvalues don't bind to rvalue references, so we need to
	 work harder to avoid bogus errors (c++/44870).  */
      if (cxx_dialect < cxx11)
	return clk_ordinary;
      else
	return lvalue_kind (TREE_OPERAND (ref, 0));

    default:
      if (!TREE_TYPE (ref))
	return clk_none;
      if (CLASS_TYPE_P (TREE_TYPE (ref)))
	return clk_class;
      break;
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

/* Returns the kind of lvalue that REF is, in the sense of
   [basic.lval].  This function should really be named lvalue_p; it
   computes the C++ definition of lvalue.  */

cp_lvalue_kind
real_lvalue_p (const_tree ref)
{
  cp_lvalue_kind kind = lvalue_kind (ref);
  if (kind & (clk_rvalueref|clk_class))
    return clk_none;
  else
    return kind;
}

/* This differs from real_lvalue_p in that class rvalues are considered
   lvalues.  */

bool
lvalue_p (const_tree ref)
{
  return (lvalue_kind (ref) != clk_none);
}

/* This differs from real_lvalue_p in that rvalues formed by dereferencing
   rvalue references are considered rvalues.  */

bool
lvalue_or_rvalue_with_address_p (const_tree ref)
{
  cp_lvalue_kind kind = lvalue_kind (ref);
  if (kind & clk_class)
    return false;
  else
    return (kind != clk_none);
}

/* Returns true if REF is an xvalue, false otherwise.  */

bool
xvalue_p (const_tree ref)
{
  return (lvalue_kind (ref) == clk_rvalueref);
}

/* Test whether DECL is a builtin that may appear in a
   constant-expression. */

bool
builtin_valid_in_constant_expr_p (const_tree decl)
{
  /* At present BUILT_IN_CONSTANT_P is the only builtin we're allowing
     in constant-expressions.  We may want to add other builtins later. */
  return DECL_IS_BUILTIN_CONSTANT_P (decl);
}

/* Build a TARGET_EXPR, initializing the DECL with the VALUE.  */

static tree
build_target_expr (tree decl, tree value, tsubst_flags_t complain)
{
  tree t;
  tree type = TREE_TYPE (decl);

#ifdef ENABLE_CHECKING
  gcc_assert (VOID_TYPE_P (TREE_TYPE (value))
	      || TREE_TYPE (decl) == TREE_TYPE (value)
	      /* On ARM ctors return 'this'.  */
	      || (TYPE_PTR_P (TREE_TYPE (value))
		  && TREE_CODE (value) == CALL_EXPR)
	      || useless_type_conversion_p (TREE_TYPE (decl),
					    TREE_TYPE (value)));
#endif

  t = cxx_maybe_build_cleanup (decl, complain);
  if (t == error_mark_node)
    return error_mark_node;
  t = build4 (TARGET_EXPR, type, decl, value, t, NULL_TREE);
  /* We always set TREE_SIDE_EFFECTS so that expand_expr does not
     ignore the TARGET_EXPR.  If there really turn out to be no
     side-effects, then the optimizer should be able to get rid of
     whatever code is generated anyhow.  */
  TREE_SIDE_EFFECTS (t) = 1;

  return t;
}

/* Return an undeclared local temporary of type TYPE for use in building a
   TARGET_EXPR.  */

static tree
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

  /* Don't build AGGR_INIT_EXPR in a template.  */
  if (processing_template_decl)
    return init;

  if (TREE_CODE (init) == CALL_EXPR)
    fn = CALL_EXPR_FN (init);
  else if (TREE_CODE (init) == AGGR_INIT_EXPR)
    fn = AGGR_INIT_EXPR_FN (init);
  else
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

      if (TREE_CODE(init) == CALL_EXPR)
	rval = build_aggr_init_array (void_type_node, fn, slot,
				      call_expr_nargs (init),
				      CALL_EXPR_ARGP (init));
      else
	rval = build_aggr_init_array (void_type_node, fn, slot,
				      aggr_init_expr_nargs (init),
				      AGGR_INIT_EXPR_ARGP (init));
      TREE_SIDE_EFFECTS (rval) = 1;
      AGGR_INIT_VIA_CTOR_P (rval) = is_ctor;
      TREE_NOTHROW (rval) = TREE_NOTHROW (init);
      CALL_EXPR_LIST_INIT_P (rval) = CALL_EXPR_LIST_INIT_P (init);
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
  tree rval = build_aggr_init_expr (type, init);
  tree slot;

  if (!complete_type_or_maybe_complain (type, init, complain))
    return error_mark_node;

  /* Make sure that we're not trying to create an instance of an
     abstract class.  */
  if (abstract_virtuals_error_sfinae (NULL_TREE, type, complain))
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

   Since we're deferring building the actual constructor calls until
   gimplification time, we need to build one now and throw it away so
   that the relevant constructor gets mark_used before cgraph decides
   what functions are needed.  Here we assume that init is either
   NULL_TREE, void_type_node (indicating value-initialization), or
   another array to copy.  */

static tree
build_vec_init_elt (tree type, tree init, tsubst_flags_t complain)
{
  tree inner_type = strip_array_types (type);
  vec<tree, va_gc> *argvec;

  if (integer_zerop (array_type_nelts_total (type))
      || !CLASS_TYPE_P (inner_type))
    /* No interesting initialization to do.  */
    return integer_zero_node;
  else if (init == void_type_node)
    return build_value_init (inner_type, complain);

  gcc_assert (init == NULL_TREE
	      || (same_type_ignoring_top_level_qualifiers_p
		  (type, TREE_TYPE (init))));

  argvec = make_tree_vector ();
  if (init)
    {
      tree init_type = strip_array_types (TREE_TYPE (init));
      tree dummy = build_dummy_object (init_type);
      if (!real_lvalue_p (init))
	dummy = move (dummy);
      argvec->quick_push (dummy);
    }
  init = build_special_member_call (NULL_TREE, complete_ctor_identifier,
				    &argvec, inner_type, LOOKUP_NORMAL,
				    complain);
  release_tree_vector (argvec);

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
  tree slot;
  bool value_init = false;
  tree elt_init = build_vec_init_elt (type, init, complain);

  if (init == void_type_node)
    {
      value_init = true;
      init = NULL_TREE;
    }

  slot = build_local_temp (type);
  init = build2 (VEC_INIT_EXPR, type, slot, init);
  TREE_SIDE_EFFECTS (init) = true;
  SET_EXPR_LOCATION (init, input_location);

  if (cxx_dialect >= cxx11
      && potential_constant_expression (elt_init))
    VEC_INIT_EXPR_IS_CONSTEXPR (init) = true;
  VEC_INIT_EXPR_VALUE_INIT (init) = value_init;

  return init;
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
  return build_vec_init_expr (TREE_TYPE (init), init, tf_warning_or_error);
}

/* Build a TARGET_EXPR using INIT to initialize a new temporary of the
   indicated TYPE.  */

tree
build_target_expr_with_type (tree init, tree type, tsubst_flags_t complain)
{
  gcc_assert (!VOID_TYPE_P (type));

  if (TREE_CODE (init) == TARGET_EXPR
      || init == error_mark_node)
    return init;
  else if (CLASS_TYPE_P (type) && type_has_nontrivial_copy_init (type)
	   && !VOID_TYPE_P (TREE_TYPE (init))
	   && TREE_CODE (init) != COND_EXPR
	   && TREE_CODE (init) != CONSTRUCTOR
	   && TREE_CODE (init) != VA_ARG_EXPR)
    /* We need to build up a copy constructor call.  A void initializer
       means we're being called from bot_manip.  COND_EXPR is a special
       case because we already have copies on the arms and we don't want
       another one here.  A CONSTRUCTOR is aggregate initialization, which
       is handled separately.  A VA_ARG_EXPR is magic creation of an
       aggregate; there's no additional work to be done.  */
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
get_target_expr_sfinae (tree init, tsubst_flags_t complain)
{
  if (TREE_CODE (init) == AGGR_INIT_EXPR)
    return build_target_expr (AGGR_INIT_EXPR_SLOT (init), init, complain);
  else if (TREE_CODE (init) == VEC_INIT_EXPR)
    return build_target_expr (VEC_INIT_EXPR_SLOT (init), init, complain);
  else
    return build_target_expr_with_type (init, TREE_TYPE (init), complain);
}

tree
get_target_expr (tree init)
{
  return get_target_expr_sfinae (init, tf_warning_or_error);
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
    expr = convert_to_integer (TYPE_MAIN_VARIANT (bitfield_type),
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

  /* [basic.lval]

     Non-class rvalues always have cv-unqualified types.  */
  type = TREE_TYPE (expr);
  if (!CLASS_TYPE_P (type) && cv_qualified_p (type))
    type = cv_unqualified (type);

  /* We need to do this for rvalue refs as well to get the right answer
     from decltype; see c++/36628.  */
  if (!processing_template_decl && lvalue_or_rvalue_with_address_p (expr))
    expr = build1 (NON_LVALUE_EXPR, type, expr);
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
set_array_type_canon (tree t, tree elt_type, tree index_type)
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
				? TYPE_CANONICAL (index_type) : index_type);
  else
    TYPE_CANONICAL (t) = t;
}

/* Like build_array_type, but handle special C++ semantics: an array of a
   variant element type is a variant of the array of the main variant of
   the element type.  */

tree
build_cplus_array_type (tree elt_type, tree index_type)
{
  tree t;

  if (elt_type == error_mark_node || index_type == error_mark_node)
    return error_mark_node;

  bool dependent = (processing_template_decl
		    && (dependent_type_p (elt_type)
			|| (index_type && dependent_type_p (index_type))));

  if (elt_type != TYPE_MAIN_VARIANT (elt_type))
    /* Start with an array of the TYPE_MAIN_VARIANT.  */
    t = build_cplus_array_type (TYPE_MAIN_VARIANT (elt_type),
				index_type);
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
	  set_array_type_canon (t, elt_type, index_type);
	}
    }
  else
    {
      t = build_array_type (elt_type, index_type);
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
	  set_array_type_canon (t, elt_type, index_type);
	  if (!dependent)
	    {
	      layout_type (t);
	      /* Make sure sizes are shared with the main variant.
		 layout_type can't be called after setting TYPE_NEXT_VARIANT,
		 as it will overwrite alignment etc. of all variants.  */
	      TYPE_SIZE (t) = TYPE_SIZE (m);
	      TYPE_SIZE_UNIT (t) = TYPE_SIZE_UNIT (m);
	    }

	  TYPE_MAIN_VARIANT (t) = m;
	  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
	  TYPE_NEXT_VARIANT (m) = t;
	}
    }

  /* Avoid spurious warnings with VLAs (c++/54583).  */
  if (TYPE_SIZE (t) && EXPR_P (TYPE_SIZE (t)))
    TREE_NO_WARNING (TYPE_SIZE (t)) = 1;

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

/* True iff T is an N3639 array of runtime bound (VLA).  These were
   approved for C++14 but then removed.  */

bool
array_of_runtime_bound_p (tree t)
{
  if (!t || TREE_CODE (t) != ARRAY_TYPE)
    return false;
  tree dom = TYPE_DOMAIN (t);
  if (!dom)
    return false;
  tree max = TYPE_MAX_VALUE (dom);
  return (!potential_rvalue_constant_expression (max)
	  || (!value_dependent_expression_p (max) && !TREE_CONSTANT (max)));
}

/* Return a reference type node referring to TO_TYPE.  If RVAL is
   true, return an rvalue reference type, otherwise return an lvalue
   reference type.  If a type node exists, reuse it, otherwise create
   a new one.  */
tree
cp_build_reference_type (tree to_type, bool rval)
{
  tree lvalue_ref, t;
  lvalue_ref = build_reference_type (to_type);
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
      = cp_build_reference_type (TYPE_CANONICAL (to_type), rval);
  else
    TYPE_CANONICAL (t) = t;

  layout_type (t);

  return t;

}

/* Returns EXPR cast to rvalue reference type, like std::move.  */

tree
move (tree expr)
{
  tree type = TREE_TYPE (expr);
  gcc_assert (TREE_CODE (type) != REFERENCE_TYPE);
  type = cp_build_reference_type (type, /*rval*/true);
  return build_static_cast (type, expr, tf_warning_or_error);
}

/* Used by the C++ front end to build qualified array types.  However,
   the C version of this function does not properly maintain canonical
   types (which are not used in C).  */
tree
c_build_qualified_type (tree type, int type_quals)
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
cp_build_qualified_type_real (tree type,
			      int type_quals,
			      tsubst_flags_t complain)
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
	= cp_build_qualified_type_real (TREE_TYPE (type),
					type_quals,
					complain);

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
	  t = build_cplus_array_type (element_type, TYPE_DOMAIN (type));

	  /* Keep the typedef name.  */
	  if (TYPE_NAME (t) != TYPE_NAME (type))
	    {
	      t = build_variant_type_copy (t);
	      TYPE_NAME (t) = TYPE_NAME (type);
	      TYPE_ALIGN (t) = TYPE_ALIGN (type);
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

      t = cp_build_qualified_type_real (t, type_quals, complain);
      return make_pack_expansion (t);
    }

  /* A reference or method type shall not be cv-qualified.
     [dcl.ref], [dcl.fct].  This used to be an error, but as of DR 295
     (in CD1) we always ignore extra cv-quals on functions.  */
  if (type_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)
      && (TREE_CODE (type) == REFERENCE_TYPE
	  || TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE))
    {
      if (TREE_CODE (type) == REFERENCE_TYPE)
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
      && !POINTER_TYPE_P (type))
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

  /* Preserve exception specs and ref-qualifier since build_qualified_type
     doesn't know about them.  */
  if (TREE_CODE (result) == FUNCTION_TYPE
      || TREE_CODE (result) == METHOD_TYPE)
    {
      result = build_exception_variant (result, TYPE_RAISES_EXCEPTIONS (type));
      result = build_ref_qualified_type (result, type_memfn_rqual (type));
    }

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
      else if (first_ident)
	{
	  for (tree a2 = first_ident; a2; a2 = TREE_CHAIN (a2))
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

/* Builds a qualified variant of T that is not a typedef variant.
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
strip_typedefs (tree t, bool *remove_attributes)
{
  tree result = NULL, type = NULL, t0 = NULL;

  if (!t || t == error_mark_node)
    return t;

  if (TREE_CODE (t) == TREE_LIST)
    {
      bool changed = false;
      vec<tree,va_gc> *vec = make_tree_vector ();
      tree r = t;
      for (; t; t = TREE_CHAIN (t))
	{
	  gcc_assert (!TREE_PURPOSE (t));
	  tree elt = strip_typedefs (TREE_VALUE (t), remove_attributes);
	  if (elt != TREE_VALUE (t))
	    changed = true;
	  vec_safe_push (vec, elt);
	}
      if (changed)
	r = build_tree_list_vec (vec);
      release_tree_vector (vec);
      return r;
    }

  gcc_assert (TYPE_P (t));

  if (t == TYPE_CANONICAL (t))
    return t;

  if (dependent_alias_template_spec_p (t))
    /* DR 1558: However, if the template-id is dependent, subsequent
       template argument substitution still applies to the template-id.  */
    return t;

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      type = strip_typedefs (TREE_TYPE (t), remove_attributes);
      result = build_pointer_type (type);
      break;
    case REFERENCE_TYPE:
      type = strip_typedefs (TREE_TYPE (t), remove_attributes);
      result = cp_build_reference_type (type, TYPE_REF_IS_RVALUE (t));
      break;
    case OFFSET_TYPE:
      t0 = strip_typedefs (TYPE_OFFSET_BASETYPE (t), remove_attributes);
      type = strip_typedefs (TREE_TYPE (t), remove_attributes);
      result = build_offset_type (t0, type);
      break;
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  t0 = strip_typedefs (TYPE_PTRMEMFUNC_FN_TYPE (t), remove_attributes);
	  result = build_ptrmemfunc_type (t0);
	}
      break;
    case ARRAY_TYPE:
      type = strip_typedefs (TREE_TYPE (t), remove_attributes);
      t0  = strip_typedefs (TYPE_DOMAIN (t), remove_attributes);
      result = build_cplus_array_type (type, t0);
      break;
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree arg_types = NULL, arg_node, arg_type;
	for (arg_node = TYPE_ARG_TYPES (t);
	     arg_node;
	     arg_node = TREE_CHAIN (arg_node))
	  {
	    if (arg_node == void_list_node)
	      break;
	    arg_type = strip_typedefs (TREE_VALUE (arg_node),
				       remove_attributes);
	    gcc_assert (arg_type);

	    arg_types =
	      tree_cons (TREE_PURPOSE (arg_node), arg_type, arg_types);
	  }

	if (arg_types)
	  arg_types = nreverse (arg_types);

	/* A list of parameters not ending with an ellipsis
	   must end with void_list_node.  */
	if (arg_node)
	  arg_types = chainon (arg_types, void_list_node);

	type = strip_typedefs (TREE_TYPE (t), remove_attributes);
	if (TREE_CODE (t) == METHOD_TYPE)
	  {
	    tree class_type = TREE_TYPE (TREE_VALUE (arg_types));
	    gcc_assert (class_type);
	    result =
	      build_method_type_directly (class_type, type,
					  TREE_CHAIN (arg_types));
	    result
	      = build_ref_qualified_type (result, type_memfn_rqual (t));
	  }
	else
	  {
	    result = build_function_type (type,
					  arg_types);
	    result = apply_memfn_quals (result,
					type_memfn_quals (t),
					type_memfn_rqual (t));
	  }

	if (TYPE_RAISES_EXCEPTIONS (t))
	  result = build_exception_variant (result,
					    TYPE_RAISES_EXCEPTIONS (t));
	if (TYPE_HAS_LATE_RETURN_TYPE (t))
	  TYPE_HAS_LATE_RETURN_TYPE (result) = 1;
      }
      break;
    case TYPENAME_TYPE:
      {
	tree fullname = TYPENAME_TYPE_FULLNAME (t);
	if (TREE_CODE (fullname) == TEMPLATE_ID_EXPR
	    && TREE_OPERAND (fullname, 1))
	  {
	    tree args = TREE_OPERAND (fullname, 1);
	    tree new_args = copy_node (args);
	    bool changed = false;
	    for (int i = 0; i < TREE_VEC_LENGTH (args); ++i)
	      {
		tree arg = TREE_VEC_ELT (args, i);
		tree strip_arg;
		if (TYPE_P (arg))
		  strip_arg = strip_typedefs (arg, remove_attributes);
		else
		  strip_arg = strip_typedefs_expr (arg, remove_attributes);
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
	result = make_typename_type (strip_typedefs (TYPE_CONTEXT (t),
						     remove_attributes),
				     fullname, typename_type, tf_none);
      }
      break;
    case DECLTYPE_TYPE:
      result = strip_typedefs_expr (DECLTYPE_TYPE_EXPR (t),
				    remove_attributes);
      if (result == DECLTYPE_TYPE_EXPR (t))
	result = NULL_TREE;
      else
	result = (finish_decltype_type
		  (result,
		   DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (t),
		   tf_none));
      break;
    default:
      break;
    }

  if (!result)
      result = TYPE_MAIN_VARIANT (t);
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
	result = cp_build_type_attribute_variant (result, TYPE_ATTRIBUTES (t));
    }
  return cp_build_qualified_type (result, cp_type_quals (t));
}

/* Like strip_typedefs above, but works on expressions, so that in

   template<class T> struct A
   {
     typedef T TT;
     B<sizeof(TT)> b;
   };

   sizeof(TT) is replaced by sizeof(T).  */

tree
strip_typedefs_expr (tree t, bool *remove_attributes)
{
  unsigned i,n;
  tree r, type, *ops;
  enum tree_code code;

  if (t == NULL_TREE || t == error_mark_node)
    return t;

  if (DECL_P (t) || CONSTANT_CLASS_P (t))
    return t;

  /* Some expressions have type operands, so let's handle types here rather
     than check TYPE_P in multiple places below.  */
  if (TYPE_P (t))
    return strip_typedefs (t, remove_attributes);

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
	tree type1 = strip_typedefs (TRAIT_EXPR_TYPE1 (t), remove_attributes);
	tree type2 = strip_typedefs (TRAIT_EXPR_TYPE2 (t), remove_attributes);
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
	vec<tree, va_gc> *vec = make_tree_vector ();
	bool changed = false;
	tree it;
	for (it = t; it; it = TREE_CHAIN (it))
	  {
	    tree val = strip_typedefs_expr (TREE_VALUE (t), remove_attributes);
	    vec_safe_push (vec, val);
	    if (val != TREE_VALUE (t))
	      changed = true;
	    gcc_assert (TREE_PURPOSE (it) == NULL_TREE);
	  }
	if (changed)
	  {
	    r = NULL_TREE;
	    FOR_EACH_VEC_ELT_REVERSE (*vec, i, it)
	      r = tree_cons (NULL_TREE, it, r);
	  }
	else
	  r = t;
	release_tree_vector (vec);
	return r;
      }

    case TREE_VEC:
      {
	bool changed = false;
	vec<tree, va_gc> *vec = make_tree_vector ();
	n = TREE_VEC_LENGTH (t);
	vec_safe_reserve (vec, n);
	for (i = 0; i < n; ++i)
	  {
	    tree op = strip_typedefs_expr (TREE_VEC_ELT (t, i),
					   remove_attributes);
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
	release_tree_vector (vec);
	return r;
      }

    case CONSTRUCTOR:
      {
	bool changed = false;
	vec<constructor_elt, va_gc> *vec
	  = vec_safe_copy (CONSTRUCTOR_ELTS (t));
	n = CONSTRUCTOR_NELTS (t);
	type = strip_typedefs (TREE_TYPE (t), remove_attributes);
	for (i = 0; i < n; ++i)
	  {
	    constructor_elt *e = &(*vec)[i];
	    tree op = strip_typedefs_expr (e->value, remove_attributes);
	    if (op != e->value)
	      {
		changed = true;
		e->value = op;
	      }
	    gcc_checking_assert
	      (e->index == strip_typedefs_expr (e->index, remove_attributes));
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
      error ("lambda-expression in a constant expression");
      return error_mark_node;

    default:
      break;
    }

  gcc_assert (EXPR_P (t));

  n = TREE_OPERAND_LENGTH (t);
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
      type = strip_typedefs (type, remove_attributes);
      /* fallthrough */

    default:
      for (i = 0; i < n; ++i)
	ops[i] = strip_typedefs_expr (TREE_OPERAND (t, i), remove_attributes);
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
      fprintf (stderr, "%s [%ld =? %ld]\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl)),
	       (long) n, (long) TREE_INT_CST_LOW (DECL_VINDEX (fndecl)));
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
  t = build2 (SCOPE_REF, type, scope, name);
  QUALIFIED_NAME_IS_TEMPLATE (t) = template_p;
  PTRMEM_OK_P (t) = true;
  if (type)
    t = convert_from_reference (t);
  return t;
}

/* Like check_qualified_type, but also check ref-qualifier and exception
   specification.  */

static bool
cp_check_qualified_type (const_tree cand, const_tree base, int type_quals,
			 cp_ref_qualifier rqual, tree raises)
{
  return (check_qualified_type (cand, base, type_quals)
	  && comp_except_specs (raises, TYPE_RAISES_EXCEPTIONS (cand),
				ce_exact)
	  && type_memfn_rqual (cand) == rqual);
}

/* Build the FUNCTION_TYPE or METHOD_TYPE with the ref-qualifier RQUAL.  */

tree
build_ref_qualified_type (tree type, cp_ref_qualifier rqual)
{
  tree t;

  if (rqual == type_memfn_rqual (type))
    return type;

  int type_quals = TYPE_QUALS (type);
  tree raises = TYPE_RAISES_EXCEPTIONS (type);
  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    if (cp_check_qualified_type (t, type, type_quals, rqual, raises))
      return t;

  t = build_variant_type_copy (type);
  switch (rqual)
    {
    case REF_QUAL_RVALUE:
      FUNCTION_RVALUE_QUALIFIED (t) = 1;
      FUNCTION_REF_QUALIFIED (t) = 1;
      break;
    case REF_QUAL_LVALUE:
      FUNCTION_RVALUE_QUALIFIED (t) = 0;
      FUNCTION_REF_QUALIFIED (t) = 1;
      break;
    default:
      FUNCTION_REF_QUALIFIED (t) = 0;
      break;
    }

  if (TYPE_STRUCTURAL_EQUALITY_P (type))
    /* Propagate structural equality. */
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (TYPE_CANONICAL (type) != type)
    /* Build the underlying canonical type, since it is different
       from TYPE. */
    TYPE_CANONICAL (t) = build_ref_qualified_type (TYPE_CANONICAL (type),
						   rqual);
  else
    /* T is its own canonical type. */
    TYPE_CANONICAL (t) = t;

  return t;
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
  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (x) == OFFSET_REF
      || TREE_CODE (x) == COMPONENT_REF)
    x = TREE_OPERAND (x, 1);
  if (BASELINK_P (x))
    x = BASELINK_FUNCTIONS (x);
  if (TREE_CODE (x) == TEMPLATE_ID_EXPR)
    x = TREE_OPERAND (x, 0);
  if (DECL_FUNCTION_TEMPLATE_P (OVL_CURRENT (x))
      || (TREE_CODE (x) == OVERLOAD && OVL_CHAIN (x)))
    return 2;
  return  (TREE_CODE (x) == FUNCTION_DECL
	   || TREE_CODE (x) == OVERLOAD);
}

/* X is the CALL_EXPR_FN of a CALL_EXPR.  If X represents a dependent name
   (14.6.2), return the IDENTIFIER_NODE for that name.  Otherwise, return
   NULL_TREE.  */

tree
dependent_name (tree x)
{
  if (identifier_p (x))
    return x;
  if (TREE_CODE (x) != COMPONENT_REF
      && TREE_CODE (x) != OFFSET_REF
      && TREE_CODE (x) != BASELINK
      && is_overloaded_fn (x))
    return DECL_NAME (get_first_fn (x));
  return NULL_TREE;
}

/* Returns true iff X is an expression for an overloaded function
   whose type cannot be known without performing overload
   resolution.  */

bool
really_overloaded_fn (tree x)
{
  return is_overloaded_fn (x) == 2;
}

tree
get_fns (tree from)
{
  gcc_assert (is_overloaded_fn (from));
  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (from) == OFFSET_REF
      || TREE_CODE (from) == COMPONENT_REF)
    from = TREE_OPERAND (from, 1);
  if (BASELINK_P (from))
    from = BASELINK_FUNCTIONS (from);
  if (TREE_CODE (from) == TEMPLATE_ID_EXPR)
    from = TREE_OPERAND (from, 0);
  return from;
}

tree
get_first_fn (tree from)
{
  return OVL_CURRENT (get_fns (from));
}

/* Return a new OVL node, concatenating it with the old one.  */

tree
ovl_cons (tree decl, tree chain)
{
  tree result = make_node (OVERLOAD);
  TREE_TYPE (result) = unknown_type_node;
  OVL_FUNCTION (result) = decl;
  TREE_CHAIN (result) = chain;

  return result;
}

/* Build a new overloaded function. If this is the first one,
   just return it; otherwise, ovl_cons the _DECLs */

tree
build_overload (tree decl, tree chain)
{
  if (! chain && TREE_CODE (decl) != TEMPLATE_DECL)
    return decl;
  return ovl_cons (decl, chain);
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
  while (TREE_CODE (ovl) == OVERLOAD && OVL_USED (ovl) && OVL_CHAIN (ovl))
    ovl = OVL_CHAIN (ovl);
  return CP_DECL_CONTEXT (OVL_CURRENT (ovl));
}

/* Return TRUE if FN is a non-static member function, FALSE otherwise.
   This function looks into BASELINK and OVERLOAD nodes.  */

bool
non_static_member_function_p (tree fn)
{
  if (fn == NULL_TREE)
    return false;

  if (is_overloaded_fn (fn))
    fn = get_first_fn (fn);

  return (DECL_P (fn)
	  && DECL_NONSTATIC_MEMBER_FUNCTION_P (fn));
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

/* Build the FUNCTION_TYPE or METHOD_TYPE which may throw exceptions
   listed in RAISES.  */

tree
build_exception_variant (tree type, tree raises)
{
  tree v;
  int type_quals;

  if (comp_except_specs (raises, TYPE_RAISES_EXCEPTIONS (type), ce_exact))
    return type;

  type_quals = TYPE_QUALS (type);
  cp_ref_qualifier rqual = type_memfn_rqual (type);
  for (v = TYPE_MAIN_VARIANT (type); v; v = TYPE_NEXT_VARIANT (v))
    if (cp_check_qualified_type (v, type, type_quals, rqual, raises))
      return v;

  /* Need to build a new variant.  */
  v = build_variant_type_copy (type);
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  return v;
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
  SET_TYPE_STRUCTURAL_EQUALITY (t2);

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

/* Check if the type T depends on a type with no linkage and if so, return
   it.  If RELAXED_P then do not consider a class type declared within
   a vague-linkage function to have no linkage.  */

tree
no_linkage_check (tree t, bool relaxed_p)
{
  tree r;

  /* There's no point in checking linkage on template functions; we
     can't know their complete types.  */
  if (processing_template_decl)
    return NULL_TREE;

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	goto ptrmem;
      /* Lambda types that don't have mangling scope have no linkage.  We
	 check CLASSTYPE_LAMBDA_EXPR for error_mark_node because
	 when we get here from pushtag none of the lambda information is
	 set up yet, so we want to assume that the lambda has linkage and
	 fix it up later if not.  */
      if (CLASSTYPE_LAMBDA_EXPR (t)
	  && CLASSTYPE_LAMBDA_EXPR (t) != error_mark_node
	  && LAMBDA_TYPE_EXTRA_SCOPE (t) == NULL_TREE)
	return t;
      /* Fall through.  */
    case UNION_TYPE:
      if (!CLASS_TYPE_P (t))
	return NULL_TREE;
      /* Fall through.  */
    case ENUMERAL_TYPE:
      /* Only treat anonymous types as having no linkage if they're at
	 namespace scope.  This is core issue 966.  */
      if (TYPE_ANONYMOUS_P (t) && TYPE_NAMESPACE_SCOPE_P (t))
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
	      if (!relaxed_p || !vague_linkage_p (r))
		return t;
	      else
		r = CP_DECL_CONTEXT (r);
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
  print_search_statistics ();
  print_class_statistics ();
  print_template_statistics ();
  if (GATHER_STATISTICS)
    fprintf (stderr, "maximum template instantiation depth reached: %d\n",
	     depth_reached);
}

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This counts only elements of the top
   array.  */

tree
array_type_nelts_top (tree type)
{
  return fold_build2_loc (input_location,
		      PLUS_EXPR, sizetype,
		      array_type_nelts (type),
		      size_one_node);
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

/* Called from break_out_target_exprs via mapcar.  */

static tree
bot_manip (tree* tp, int* walk_subtrees, void* data)
{
  splay_tree target_remap = ((splay_tree) data);
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

      if (TREE_CODE (TREE_OPERAND (t, 1)) == AGGR_INIT_EXPR)
	{
	  u = build_cplus_new (TREE_TYPE (t), TREE_OPERAND (t, 1),
			       tf_warning_or_error);
	  if (AGGR_INIT_ZERO_FIRST (TREE_OPERAND (t, 1)))
	    AGGR_INIT_ZERO_FIRST (TREE_OPERAND (u, 1)) = true;
	}
      else
	u = build_target_expr_with_type (TREE_OPERAND (t, 1), TREE_TYPE (t),
					 tf_warning_or_error);

      TARGET_EXPR_IMPLICIT_P (u) = TARGET_EXPR_IMPLICIT_P (t);
      TARGET_EXPR_LIST_INIT_P (u) = TARGET_EXPR_LIST_INIT_P (t);
      TARGET_EXPR_DIRECT_INIT_P (u) = TARGET_EXPR_DIRECT_INIT_P (t);

      /* Map the old variable to the new one.  */
      splay_tree_insert (target_remap,
			 (splay_tree_key) TREE_OPERAND (t, 0),
			 (splay_tree_value) TREE_OPERAND (u, 0));

      TREE_OPERAND (u, 1) = break_out_target_exprs (TREE_OPERAND (u, 1));

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

  /* Make a copy of this node.  */
  t = copy_tree_r (tp, walk_subtrees, NULL);
  if (TREE_CODE (*tp) == CALL_EXPR)
    {
      set_flags_from_callee (*tp);

      /* builtin_LINE and builtin_FILE get the location where the default
	 argument is expanded, not where the call was written.  */
      tree callee = get_callee_fndecl (*tp);
      if (callee && DECL_BUILT_IN (callee))
	switch (DECL_FUNCTION_CODE (callee))
	  {
	  case BUILT_IN_FILE:
	  case BUILT_IN_LINE:
	    SET_EXPR_LOCATION (*tp, input_location);
	  default:
	    break;
	  }
    }
  return t;
}

/* Replace all remapped VAR_DECLs in T with their new equivalents.
   DATA is really a splay-tree mapping old variables to new
   variables.  */

static tree
bot_replace (tree* t, int* /*walk_subtrees*/, void* data)
{
  splay_tree target_remap = ((splay_tree) data);

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
      /* In an NSDMI build_base_path defers building conversions to virtual
	 bases, and we handle it here.  */
      tree basetype = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (*t)));
      vec<tree, va_gc> *vbases = CLASSTYPE_VBASECLASSES (current_class_type);
      int i; tree binfo;
      FOR_EACH_VEC_SAFE_ELT (vbases, i, binfo)
	if (BINFO_TYPE (binfo) == basetype)
	  break;
      *t = build_base_path (PLUS_EXPR, TREE_OPERAND (*t, 0), binfo, true,
			    tf_warning_or_error);
    }

  return NULL_TREE;
}

/* When we parse a default argument expression, we may create
   temporary variables via TARGET_EXPRs.  When we actually use the
   default-argument expression, we make a copy of the expression
   and replace the temporaries with appropriate local versions.  */

tree
break_out_target_exprs (tree t)
{
  static int target_remap_count;
  static splay_tree target_remap;

  if (!target_remap_count++)
    target_remap = splay_tree_new (splay_tree_compare_pointers,
				   /*splay_tree_delete_key_fn=*/NULL,
				   /*splay_tree_delete_value_fn=*/NULL);
  cp_walk_tree (&t, bot_manip, target_remap, NULL);
  cp_walk_tree (&t, bot_replace, target_remap, NULL);

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
    gcc_assert (same_type_ignoring_top_level_qualifiers_p (type,
							   TREE_TYPE (obj)));
  return obj;
}

/* Like substitute_placeholder_in_expr, but handle C++ tree codes and
   build up subexpressions as we go deeper.  */

static tree
replace_placeholders_r (tree* t, int* walk_subtrees, void* data_)
{
  tree obj = static_cast<tree>(data_);

  if (TREE_CONSTANT (*t))
    {
      *walk_subtrees = false;
      return NULL_TREE;
    }

  switch (TREE_CODE (*t))
    {
    case PLACEHOLDER_EXPR:
      {
	tree x = obj;
	for (; !(same_type_ignoring_top_level_qualifiers_p
		 (TREE_TYPE (*t), TREE_TYPE (x)));
	     x = TREE_OPERAND (x, 0))
	  gcc_assert (TREE_CODE (x) == COMPONENT_REF);
	*t = x;
	*walk_subtrees = false;
      }
      break;

    case CONSTRUCTOR:
      {
	constructor_elt *ce;
	vec<constructor_elt,va_gc> *v = CONSTRUCTOR_ELTS (*t);
	for (unsigned i = 0; vec_safe_iterate (v, i, &ce); ++i)
	  {
	    tree *valp = &ce->value;
	    tree type = TREE_TYPE (*valp);
	    tree subob = obj;

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

	    cp_walk_tree (valp, replace_placeholders_r,
			  subob, NULL);
	  }
	*walk_subtrees = false;
	break;
      }

    default:
      break;
    }

  return NULL_TREE;
}

tree
replace_placeholders (tree exp, tree obj)
{
  tree *tp = &exp;
  if (TREE_CODE (exp) == TARGET_EXPR)
    tp = &TARGET_EXPR_INITIAL (exp);
  cp_walk_tree (tp, replace_placeholders_r, obj, NULL);
  return exp;
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
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
    }

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
  length = TREE_CODE_LENGTH (code);
  TREE_TYPE (t) = TREE_TYPE (non_dep);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (non_dep);

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
    }

  if (code == COMPOUND_EXPR && TREE_CODE (non_dep) != COMPOUND_EXPR)
    /* This should not be considered a COMPOUND_EXPR, because it
       resolves to an overload.  */
    COMPOUND_EXPR_OVERLOADED (t) = 1;

  va_end (p);
  return convert_from_reference (t);
}

/* Similar to `build_nt_call_vec', but for template definitions of
   non-dependent expressions. NON_DEP is the non-dependent expression
   that has been built.  */

tree
build_min_non_dep_call_vec (tree non_dep, tree fn, vec<tree, va_gc> *argvec)
{
  tree t = build_nt_call_vec (fn, argvec);
  if (REFERENCE_REF_P (non_dep))
    non_dep = TREE_OPERAND (non_dep, 0);
  TREE_TYPE (t) = TREE_TYPE (non_dep);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (non_dep);
  return convert_from_reference (t);
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
decl_anon_ns_mem_p (const_tree decl)
{
  while (1)
    {
      if (decl == NULL_TREE || decl == error_mark_node)
	return false;
      if (TREE_CODE (decl) == NAMESPACE_DECL
	  && DECL_NAME (decl) == NULL_TREE)
	return true;
      /* Classes and namespaces inside anonymous namespaces have
         TREE_PUBLIC == 0, so we can shortcut the search.  */
      else if (TYPE_P (decl))
	return (TREE_PUBLIC (TYPE_MAIN_DECL (decl)) == 0);
      else if (TREE_CODE (decl) == NAMESPACE_DECL)
	return (TREE_PUBLIC (decl) == 0);
      else
	decl = DECL_CONTEXT (decl);
    }
}

/* Subroutine of cp_tree_equal: t1 and t2 are the CALL_EXPR_FNs of two
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

      if (TREE_CODE (t1) == TEMPLATE_ID_EXPR)
	targs1 = TREE_OPERAND (t1, 1);
      if (TREE_CODE (t2) == TEMPLATE_ID_EXPR)
	targs2 = TREE_OPERAND (t2, 1);
      return cp_tree_equal (targs1, targs2);
    }
  else
    return cp_tree_equal (t1, t2);
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

  switch (code1)
    {
    case VOID_CST:
      /* There's only a single VOID_CST node, so we should never reach
	 here.  */
      gcc_unreachable ();

    case INTEGER_CST:
      return tree_int_cst_equal (t1, t2);

    case REAL_CST:
      return REAL_VALUES_EQUAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

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
	tree arg1, arg2;
	call_expr_arg_iterator iter1, iter2;
	if (!called_fns_equal (CALL_EXPR_FN (t1), CALL_EXPR_FN (t2)))
	  return false;
	for (arg1 = first_call_expr_arg (t1, &iter1),
	       arg2 = first_call_expr_arg (t2, &iter2);
	     arg1 && arg2;
	     arg1 = next_call_expr_arg (&iter1),
	       arg2 = next_call_expr_arg (&iter2))
	  if (!cp_tree_equal (arg1, arg2))
	    return false;
	if (arg1 || arg2)
	  return false;
	return true;
      }

    case TARGET_EXPR:
      {
	tree o1 = TREE_OPERAND (t1, 0);
	tree o2 = TREE_OPERAND (t2, 0);

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

	return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));
      }

    case WITH_CLEANUP_EXPR:
      if (!cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
	return false;
      return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t1, 1));

    case COMPONENT_REF:
      if (TREE_OPERAND (t1, 1) != TREE_OPERAND (t2, 1))
	return false;
      return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case PARM_DECL:
      /* For comparing uses of parameters in late-specified return types
	 with an out-of-class definition of the function, but can also come
	 up for expressions that involve 'this' in a member function
	 template.  */

      if (comparing_specializations)
	/* When comparing hash table entries, only an exact match is
	   good enough; we don't want to replace 'this' with the
	   version from another function.  */
	return false;

      if (same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	{
	  if (DECL_ARTIFICIAL (t1) ^ DECL_ARTIFICIAL (t2))
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
    case TEMPLATE_DECL:
    case IDENTIFIER_NODE:
    case SSA_NAME:
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
      return (cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0))
	      && cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1)));

    case TREE_VEC:
      {
	unsigned ix;
	if (TREE_VEC_LENGTH (t1) != TREE_VEC_LENGTH (t2))
	  return false;
	for (ix = TREE_VEC_LENGTH (t1); ix--;)
	  if (!cp_tree_equal (TREE_VEC_ELT (t1, ix),
			      TREE_VEC_ELT (t2, ix)))
	    return false;
	return true;
      }

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
	if (TREE_CODE (o1) != TREE_CODE (o2))
	  return false;
	if (TYPE_P (o1))
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
      if (OVL_FUNCTION (t1) != OVL_FUNCTION (t2))
	return false;
      return cp_tree_equal (OVL_CHAIN (t1), OVL_CHAIN (t2));

    case TRAIT_EXPR:
      if (TRAIT_EXPR_KIND (t1) != TRAIT_EXPR_KIND (t2))
	return false;
      return same_type_p (TRAIT_EXPR_TYPE1 (t1), TRAIT_EXPR_TYPE1 (t2))
	&& cp_tree_equal (TRAIT_EXPR_TYPE2 (t1), TRAIT_EXPR_TYPE2 (t2));

    case CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
    case NEW_EXPR:
    CASE_CONVERT:
    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      if (!same_type_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;
      /* Now compare operands as usual.  */
      break;

    case DEFERRED_NOEXCEPT:
      return (cp_tree_equal (DEFERRED_NOEXCEPT_PATTERN (t1),
			     DEFERRED_NOEXCEPT_PATTERN (t2))
	      && comp_template_args (DEFERRED_NOEXCEPT_ARGS (t1),
				     DEFERRED_NOEXCEPT_ARGS (t2)));
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
	int i, n;

	n = cp_tree_operand_length (t1);
	if (TREE_CODE_CLASS (code1) == tcc_vl_exp
	    && n != TREE_OPERAND_LENGTH (t2))
	  return false;

	for (i = 0; i < n; ++i)
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
  else if (real_lvalue_p (arg))
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
   object that the access is against.  */

tree
build_dummy_object (tree type)
{
  tree decl = build1 (CONVERT_EXPR, build_pointer_type (type), void_node);
  return cp_build_indirect_ref (decl, RO_NULL, tf_warning_or_error);
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

  if (current_class_ref
      /* current_class_ref might not correspond to current_class_type if
	 we're in tsubst_default_argument or a lambda-declarator; in either
	 case, we want to use current_class_ref if it matches CONTEXT.  */
      && (same_type_ignoring_top_level_qualifiers_p
	  (TREE_TYPE (current_class_ref), context)))
    decl = current_class_ref;
  else
    decl = build_dummy_object (context);

  return decl;
}

/* Returns 1 if OB is a placeholder object, or a pointer to one.  */

int
is_dummy_object (const_tree ob)
{
  if (INDIRECT_REF_P (ob))
    ob = TREE_OPERAND (ob, 0);
  return (TREE_CODE (ob) == CONVERT_EXPR
	  && TREE_OPERAND (ob, 0) == void_node);
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

/* Returns true iff copying an object of type T (including via move
   constructor) is non-trivial.  That is, T has no non-trivial copy
   constructors and no non-trivial move constructors.  */

bool
type_has_nontrivial_copy_init (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    {
      gcc_assert (COMPLETE_TYPE_P (t));
      return ((TYPE_HAS_COPY_CTOR (t)
	       && TYPE_HAS_COMPLEX_COPY_CTOR (t))
	      || TYPE_HAS_COMPLEX_MOVE_CTOR (t));
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
    return !CP_TYPE_VOLATILE_P (t) && scalarish_type_p (t);
}

/* Returns 1 iff type T is a trivial type, as defined in [basic.types] and
   [class].  */

bool
trivial_type_p (const_tree t)
{
  t = strip_array_types (CONST_CAST_TREE (t));

  if (CLASS_TYPE_P (t))
    return (TYPE_HAS_TRIVIAL_DFLT (t)
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

/* Table of valid C++ attributes.  */
const struct attribute_spec cxx_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "java_interface", 0, 0, false, false, false,
    handle_java_interface_attribute, false },
  { "com_interface",  0, 0, false, false, false,
    handle_com_interface_attribute, false },
  { "init_priority",  1, 1, true,  false, false,
    handle_init_priority_attribute, false },
  { "abi_tag", 1, -1, false, false, false,
    handle_abi_tag_attribute, true },
  { NULL,	      0, 0, false, false, false, NULL, false }
};

/* Handle a "java_interface" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_java_interface_attribute (tree* node,
				 tree name,
				 tree /*args*/,
				 int flags,
				 bool* no_add_attrs)
{
  if (DECL_P (*node)
      || !CLASS_TYPE_P (*node)
      || !TYPE_FOR_JAVA (*node))
    {
      error ("%qE attribute can only be applied to Java class definitions",
	     name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
    *node = build_variant_type_copy (*node);
  TYPE_JAVA_INTERFACE (*node) = 1;

  return NULL_TREE;
}

/* Handle a "com_interface" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_com_interface_attribute (tree* node,
				tree name,
				tree /*args*/,
				int /*flags*/,
				bool* no_add_attrs)
{
  static int warned;

  *no_add_attrs = true;

  if (DECL_P (*node)
      || !CLASS_TYPE_P (*node)
      || *node != TYPE_MAIN_VARIANT (*node))
    {
      warning (OPT_Wattributes, "%qE attribute can only be applied "
	       "to class definitions", name);
      return NULL_TREE;
    }

  if (!warned++)
    warning (0, "%qE is obsolete; g++ vtables are now COM-compatible by default",
	     name);

  return NULL_TREE;
}

/* Handle an "init_priority" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_init_priority_attribute (tree* node,
				tree name,
				tree args,
				int /*flags*/,
				bool* no_add_attrs)
{
  tree initp_expr = TREE_VALUE (args);
  tree decl = *node;
  tree type = TREE_TYPE (decl);
  int pri;

  STRIP_NOPS (initp_expr);
  initp_expr = default_conversion (initp_expr);

  if (!initp_expr || TREE_CODE (initp_expr) != INTEGER_CST)
    {
      error ("requested init_priority is not an integer constant");
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
      error ("requested init_priority is out of range");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Check for init_priorities that are reserved for
     language and runtime support implementations.*/
  if (pri <= MAX_RESERVED_INIT_PRIORITY)
    {
      warning
	(0, "requested init_priority is reserved for internal use");
    }

  if (SUPPORTS_INIT_PRIORITY)
    {
      SET_DECL_INIT_PRIORITY (decl, pri);
      DECL_HAS_INIT_PRIORITY_P (decl) = 1;
      return NULL_TREE;
    }
  else
    {
      error ("%qE attribute is not supported on this platform", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
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
  for (const_tree t = new_; t; t = TREE_CHAIN (t))
    {
      tree str = TREE_VALUE (t);
      for (const_tree in = old; in; in = TREE_CHAIN (in))
	{
	  tree ostr = TREE_VALUE (in);
	  if (cp_tree_equal (str, ostr))
	    goto found;
	}
      error ("redeclaration of %qD adds abi tag %E", decl, str);
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
      else if (CLASSTYPE_TEMPLATE_INSTANTIATION (*node))
	{
	  warning (OPT_Wattributes, "ignoring %qE attribute applied to "
		   "template instantiation %qT", name, *node);
	  goto fail;
	}
      else if (CLASSTYPE_TEMPLATE_SPECIALIZATION (*node))
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

/* Return a new PTRMEM_CST of the indicated TYPE.  The MEMBER is the
   thing pointed to by the constant.  */

tree
make_ptrmem_cst (tree type, tree member)
{
  tree ptrmem_cst = make_node (PTRMEM_CST);
  TREE_TYPE (ptrmem_cst) = type;
  PTRMEM_CST_MEMBER (ptrmem_cst) = member;
  return ptrmem_cst;
}

/* Build a variant of TYPE that has the indicated ATTRIBUTES.  May
   return an existing type if an appropriate type already exists.  */

tree
cp_build_type_attribute_variant (tree type, tree attributes)
{
  tree new_type;

  new_type = build_type_attribute_variant (type, attributes);
  if (TREE_CODE (new_type) == FUNCTION_TYPE
      || TREE_CODE (new_type) == METHOD_TYPE)
    {
      new_type = build_exception_variant (new_type,
					  TYPE_RAISES_EXCEPTIONS (type));
      new_type = build_ref_qualified_type (new_type,
					   type_memfn_rqual (type));
    }

  /* Making a new main variant of a class type is broken.  */
  gcc_assert (!CLASS_TYPE_P (type) || new_type == type);
    
  return new_type;
}

/* Return TRUE if TYPE1 and TYPE2 are identical for type hashing purposes.
   Called only after doing all language independent checks.  Only
   to check TYPE_RAISES_EXCEPTIONS for FUNCTION_TYPE, the rest is already
   compared in type_hash_eq.  */

bool
cxx_type_hash_eq (const_tree typea, const_tree typeb)
{
  gcc_assert (TREE_CODE (typea) == FUNCTION_TYPE
	      || TREE_CODE (typea) == METHOD_TYPE);

  return comp_except_specs (TYPE_RAISES_EXCEPTIONS (typea),
			    TYPE_RAISES_EXCEPTIONS (typeb), ce_exact);
}

/* Apply FUNC to all language-specific sub-trees of TP in a pre-order
   traversal.  Called from walk_tree.  */

tree
cp_walk_subtrees (tree *tp, int *walk_subtrees_p, walk_tree_fn func,
		  void *data, hash_set<tree> *pset)
{
  enum tree_code code = TREE_CODE (*tp);
  tree result;

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = cp_walk_tree (&(NODE), func, data, pset);	\
      if (result) goto out;				\
    }							\
  while (0)

  /* Not one of the easy cases.  We must explicitly go through the
     children.  */
  result = NULL_TREE;
  switch (code)
    {
    case DEFAULT_ARG:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case UNBOUND_CLASS_TEMPLATE:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_TYPE_PARM:
    case TYPENAME_TYPE:
    case TYPEOF_TYPE:
    case UNDERLYING_TYPE:
      /* None of these have subtrees other than those already walked
	 above.  */
      *walk_subtrees_p = 0;
      break;

    case BASELINK:
      WALK_SUBTREE (BASELINK_FUNCTIONS (*tp));
      *walk_subtrees_p = 0;
      break;

    case PTRMEM_CST:
      WALK_SUBTREE (TREE_TYPE (*tp));
      *walk_subtrees_p = 0;
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_PURPOSE (*tp));
      break;

    case OVERLOAD:
      WALK_SUBTREE (OVL_FUNCTION (*tp));
      WALK_SUBTREE (OVL_CHAIN (*tp));
      *walk_subtrees_p = 0;
      break;

    case USING_DECL:
      WALK_SUBTREE (DECL_NAME (*tp));
      WALK_SUBTREE (USING_DECL_SCOPE (*tp));
      WALK_SUBTREE (USING_DECL_DECLS (*tp));
      *walk_subtrees_p = 0;
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (*tp))
	WALK_SUBTREE (TYPE_PTRMEMFUNC_FN_TYPE_RAW (*tp));
      break;

    case TYPE_ARGUMENT_PACK:
    case NONTYPE_ARGUMENT_PACK:
      {
        tree args = ARGUMENT_PACK_ARGS (*tp);
        int i, len = TREE_VEC_LENGTH (args);
        for (i = 0; i < len; i++)
          WALK_SUBTREE (TREE_VEC_ELT (args, i));
      }
      break;

    case TYPE_PACK_EXPANSION:
      WALK_SUBTREE (TREE_TYPE (*tp));
      WALK_SUBTREE (PACK_EXPANSION_EXTRA_ARGS (*tp));
      *walk_subtrees_p = 0;
      break;
      
    case EXPR_PACK_EXPANSION:
      WALK_SUBTREE (TREE_OPERAND (*tp, 0));
      WALK_SUBTREE (PACK_EXPANSION_EXTRA_ARGS (*tp));
      *walk_subtrees_p = 0;
      break;

    case CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case CONST_CAST_EXPR:
    case DYNAMIC_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      if (TREE_TYPE (*tp))
	WALK_SUBTREE (TREE_TYPE (*tp));

      {
        int i;
        for (i = 0; i < TREE_CODE_LENGTH (TREE_CODE (*tp)); ++i)
	  WALK_SUBTREE (TREE_OPERAND (*tp, i));
      }
      *walk_subtrees_p = 0;
      break;

    case TRAIT_EXPR:
      WALK_SUBTREE (TRAIT_EXPR_TYPE1 (*tp));
      WALK_SUBTREE (TRAIT_EXPR_TYPE2 (*tp));
      *walk_subtrees_p = 0;
      break;

    case DECLTYPE_TYPE:
      WALK_SUBTREE (DECLTYPE_TYPE_EXPR (*tp));
      *walk_subtrees_p = 0;
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
  return save_expr (expr);
}

/* Initialize tree.c.  */

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
  if (DECL_INHERITED_CTOR_BASE (decl))
    return sfk_inheriting_constructor;
  if (DECL_COPY_CONSTRUCTOR_P (decl))
    return sfk_copy_constructor;
  if (DECL_MOVE_CONSTRUCTOR_P (decl))
    return sfk_move_constructor;
  if (DECL_CONSTRUCTOR_P (decl))
    return sfk_constructor;
  if (DECL_OVERLOADED_OPERATOR_P (decl) == NOP_EXPR)
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

  return sfk_none;
}

/* Returns nonzero if TYPE is a character type, including wchar_t.  */

int
char_type_p (tree type)
{
  return (same_type_p (type, char_type_node)
	  || same_type_p (type, unsigned_char_type_node)
	  || same_type_p (type, signed_char_type_node)
	  || same_type_p (type, char16_type_node)
	  || same_type_p (type, char32_type_node)
	  || same_type_p (type, wchar_type_node));
}

/* Returns the kind of linkage associated with the indicated DECL.  Th
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

  /* Things that don't have names have no linkage.  */
  if (!DECL_NAME (decl))
    return lk_none;

  /* Fields have no linkage.  */
  if (TREE_CODE (decl) == FIELD_DECL)
    return lk_none;

  /* Things that are TREE_PUBLIC have external linkage.  */
  if (TREE_PUBLIC (decl))
    return lk_external;

  if (TREE_CODE (decl) == NAMESPACE_DECL)
    return lk_external;

  /* Linkage of a CONST_DECL depends on the linkage of the enumeration
     type.  */
  if (TREE_CODE (decl) == CONST_DECL)
    return decl_linkage (TYPE_NAME (DECL_CONTEXT (decl)));

  /* Things in local scope do not have linkage, if they don't have
     TREE_PUBLIC set.  */
  if (decl_function_context (decl))
    return lk_none;

  /* Members of the anonymous namespace also have TREE_PUBLIC unset, but
     are considered to have external linkage for language purposes, as do
     template instantiations on targets without weak symbols.  DECLs really
     meant to have internal linkage have DECL_THIS_STATIC set.  */
  if (TREE_CODE (decl) == TYPE_DECL)
    return lk_external;
  if (VAR_OR_FUNCTION_DECL_P (decl))
    {
      if (!DECL_THIS_STATIC (decl))
	return lk_external;

      /* Static data members and static member functions from classes
	 in anonymous namespace also don't have TREE_PUBLIC set.  */
      if (DECL_CLASS_CONTEXT (decl))
	return lk_external;
    }

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
   that *INITP is evaluated before EXP.  */

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
  else if (TREE_CODE (TREE_TYPE (exp)) == REFERENCE_TYPE
	   || SCALAR_TYPE_P (TREE_TYPE (exp))
	   || !lvalue_or_rvalue_with_address_p (exp))
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
      bool xval = !real_lvalue_p (exp);
      exp = cp_build_addr_expr (exp, tf_warning_or_error);
      init_expr = get_target_expr (exp);
      exp = TARGET_EXPR_SLOT (init_expr);
      exp = cp_build_indirect_ref (exp, RO_NULL, tf_warning_or_error);
      if (xval)
	exp = move (exp);
    }
  *initp = init_expr;

  gcc_assert (!TREE_SIDE_EFFECTS (exp));
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
	  || lvalue_or_rvalue_with_address_p (t)))
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
	  if (TREE_CODE (type) == REFERENCE_TYPE
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

/* Like "fold", but should be used whenever we might be processing the
   body of a template.  */

tree
fold_if_not_in_template (tree expr)
{
  /* In the body of a template, there is never any need to call
     "fold".  We will call fold later when actually instantiating the
     template.  Integral constant expressions in templates will be
     evaluated via instantiate_non_dependent_expr, as necessary.  */
  if (processing_template_decl)
    return expr;

  /* Fold C++ front-end specific tree codes.  */
  if (TREE_CODE (expr) == UNARY_PLUS_EXPR)
    return fold_convert (TREE_TYPE (expr), TREE_OPERAND (expr, 0));

  return fold (expr);
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
      if (!node || !node->alias
	  || !vec_safe_length (node->ref_list.references))
	return true;
    }

  return false;
}

/* Clean the C++ specific parts of the tree T. */

void
cp_free_lang_data (tree t)
{
  if (TREE_CODE (t) == METHOD_TYPE
      || TREE_CODE (t) == FUNCTION_TYPE)
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
    {
      /* The list of users of a namespace isn't useful for the middle-end
	 or debug generators.  */
      DECL_NAMESPACE_USERS (t) = NULL_TREE;
      /* Neither do we need the leftover chaining of namespaces
         from the binding level.  */
      DECL_CHAIN (t) = NULL_TREE;
    }
}

/* Stub for c-common.  Please keep in sync with c-decl.c.
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
      return TREE_OPERAND_LENGTH (t);
    }
}

/* Implement -Wzero_as_null_pointer_constant.  Return true if the
   conditions for the warning hold, false otherwise.  */
bool
maybe_warn_zero_as_null_pointer_constant (tree expr, location_t loc)
{
  if (c_inhibit_evaluation_warnings == 0
      && !NULLPTR_TYPE_P (TREE_TYPE (expr)))
    {
      warning_at (loc, OPT_Wzero_as_null_pointer_constant,
		  "zero as null pointer constant");
      return true;
    }
  return false;
}

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)
/* Complain that some language-specific thing hanging off a tree
   node has been accessed improperly.  */

void
lang_check_failed (const char* file, int line, const char* function)
{
  internal_error ("lang_* check: failed in %s, at %s:%d",
		  function, trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

#include "gt-cp-tree.h"
