/* C++ reflection code.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
   Written by Marek Polacek <polacek@redhat.com> and
   Jakub Jelinek <jakub@redhat.com>.

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
#include "target.h"
#include "tm.h"
#include "cp-tree.h"
#include "stringpool.h" // for get_identifier
#include "intl.h"
#include "attribs.h"
#include "c-family/c-pragma.h" // for parse_in
#include "gimplify.h" // for unshare_expr
#include "metafns.h"

static tree eval_is_function_type (tree);
static tree eval_is_object_type (location_t, tree);
static tree eval_reflect_constant (location_t, const constexpr_ctx *, tree,
				   tree, bool *, tree *, tree);
static tree eval_is_array_type (location_t, tree);
static tree eval_reflect_constant_array (location_t, const constexpr_ctx *,
					 tree, bool *, bool *, tree *, tree);
static tree eval_reflect_function (location_t, const constexpr_ctx *, tree,
				   tree, bool *, tree *, tree);
struct constexpr_ctx;

/* Return the appropriate tsubst flags for processing a metafunction.  */

static tsubst_flags_t
complain_flags (const constexpr_ctx *ctx)
{
  return cxx_constexpr_quiet_p (ctx) ? tf_none : tf_warning_or_error;
}

/* Initialize state for reflection; e.g., initialize meta_info_type_node.  */

void
init_reflection ()
{
  /* The type std::meta::info is a scalar type for which equality and
     inequality are meaningful, but for which no ordering relation is
     defined.  */
  meta_info_type_node = make_node (META_TYPE);
  /* Make it a complete type.  */
  TYPE_SIZE (meta_info_type_node) = bitsize_int (GET_MODE_BITSIZE (ptr_mode));
  TYPE_SIZE_UNIT (meta_info_type_node) = size_int (GET_MODE_SIZE (ptr_mode));
  /* Name it.  */
  record_builtin_type (RID_MAX, "decltype(^^int)", meta_info_type_node);

  /* Create the `std::meta' namespace.  */
  push_namespace (get_identifier ("std"));
  push_namespace (get_identifier ("meta"), /*inline*/false);
  std_meta_node = current_namespace;
  pop_namespace ();
  pop_namespace ();
}

/* Create a REFLECT_EXPR expression of kind KIND around T.  */

static tree
get_reflection_raw (location_t loc, tree t, reflect_kind kind = REFLECT_UNDEF)
{
  t = build1_loc (loc, REFLECT_EXPR, meta_info_type_node, t);
  SET_REFLECT_EXPR_KIND (t, kind);
  TREE_CONSTANT (t) = true;
  TREE_READONLY (t) = true;
  TREE_SIDE_EFFECTS (t) = false;
  return t;
}

/* Return the reflection for T.

    [basic.fundamental]: A value of type std::meta::info is called a reflection.
    There exists a unique null reflection; every other reflection is
    a representation of

    -- a value of scalar type,
    -- an object with static storage duration,
    -- a variable,
    -- a structured binding,
    -- a function,
    -- a function parameter,
    -- an enumerator,
    -- an annotation,
    -- a type alias,
    -- a type,
    -- a class member,
    -- an unnamed bit-field,
    -- a class template,
    -- a function template,
    -- a variable template,
    -- an alias template,
    -- a concept,
    -- a namespace alias,
    -- a namespace,
    -- a direct base class relationship, or
    -- a data member description.

   KIND is used to distinguish between categories that are represented
   by the same handle.  */

tree
get_reflection (location_t loc, tree t, reflect_kind kind/*=REFLECT_UNDEF*/)
{
  STRIP_ANY_LOCATION_WRAPPER (t);

  /* [expr.reflect] If the type-id designates a placeholder type, R is
     ill-formed.  */
  if (is_auto (t))
    {
      error_at (loc, "%<^^%> cannot be applied to a placeholder type");
      return error_mark_node;
    }
  /* Constant template parameters and pack-index-expressions cannot
     appear as operands of the reflection operator.  */
  else if (PACK_INDEX_P (t))
    {
      error_at (loc, "%<^^%> cannot be applied to a pack index");
      return error_mark_node;
    }
  else if (TREE_CODE (t) == CONST_DECL && DECL_TEMPLATE_PARM_P (t))
    {
      error_at (loc, "%<^^%> cannot be applied to a non-type template "
		"parameter %qD", t);
      return error_mark_node;
    }
  /* If the id-expression denotes a variable declared by an init-capture,
     R is ill-formed.  */
  else if (is_capture_proxy (t))
    {
      error_at (loc, "%<^^%> cannot be applied to a capture %qD", t);
      return error_mark_node;
    }
  /* If the id-expression denotes a local parameter introduced by
     a requires-expression, R is ill-formed.  */
  else if (TREE_CODE (t) == PARM_DECL && CONSTRAINT_VAR_P (t))
    {
      error_at (loc, "%<^^%> cannot be applied to a local parameter of "
		"a requires-expression %qD", t);
      return error_mark_node;
    }
  /* If the id-expression denotes a local entity E for which there is
     a lambda scope that intervenes between R and the point at which E
     was introduced, R is ill-formed.  */
  else if (outer_automatic_var_p (t)
	   /* Since outer_automatic_var_p is also true when we are in
	      a local class member function, additionally check that
	      we are in a lambda.  */
	   && ((current_function_decl
		&& LAMBDA_FUNCTION_P (current_function_decl))
	       || parsing_lambda_declarator ()))
    {
      auto_diagnostic_group d;
      error_at (loc, "%<^^%> cannot be applied a local entity for which "
		"there is an intervening lambda expression");
      inform (DECL_SOURCE_LOCATION (t), "%qD declared here", t);
      return error_mark_node;
    }
  /* If lookup finds a declaration that replaced a using-declarator during
     a single search, R is ill-formed.  */
  else if (TREE_CODE (t) == USING_DECL
	   || (TREE_CODE (t) == OVERLOAD && OVL_USING_P (t)))
    {
      error_at (loc, "%<^^%> cannot be applied to a using-declaration");
      return error_mark_node;
    }
  /* A concept is fine, but not Concept<arg>.  */
  else if (concept_check_p (t))
    {
      error_at (loc, "%<^^%> cannot be applied to a concept check");
      return error_mark_node;
    }

  /* Otherwise, if the template-name names a function template F,
     then the template-name interpreted as an id-expression shall
     denote an overload set containing only F.  R represents F.

     When we have:
       template<typename T>
       void foo (T) {}
       constexpr auto a = ^^foo;
     we will get an OVERLOAD containing only one function.  */
  tree r = MAYBE_BASELINK_FUNCTIONS (t);
  if (OVL_P (r))
    {
      if (!OVL_SINGLE_P (r))
	{
	  error_at (loc, "cannot take the reflection of an overload set");
	  return error_mark_node;
	}
    }
  /* [expr.reflect] If the id-expression denotes an overload set S,
     overload resolution for the expression &S with no target shall
     select a unique function; R represents that function.  */
  else if (!processing_template_decl && t != unknown_type_node)
    /* Resolve all TEMPLATE_ID_EXPRs here.  */
    t = resolve_nondeduced_context_or_error (t, tf_warning_or_error);

  /* For injected-class-name, use the main variant so that comparing
     reflections works (cf. compare3.C).  */
  if (RECORD_OR_UNION_TYPE_P (t)
      && TYPE_NAME (t)
      && DECL_SELF_REFERENCE_P (TYPE_NAME (t)))
    t = TYPE_MAIN_VARIANT (t);

  /* It's annoying to deal with BIT_NOT_EXPR in a reflection later, so
     look up the FUNCTION_DECL here.  */
  if (TREE_CODE (t) == BIT_NOT_EXPR
      && CLASS_TYPE_P (TREE_OPERAND (t, 0))
      && COMPLETE_TYPE_P (TREE_OPERAND (t, 0)))
    {
      r = TREE_OPERAND (t, 0);
      if (CLASSTYPE_LAZY_DESTRUCTOR (r))
	lazily_declare_fn (sfk_destructor, r);
      if (tree dtor = CLASSTYPE_DESTRUCTOR (r))
	t = dtor;
    }

  if (t == error_mark_node)
    return error_mark_node;

  return get_reflection_raw (loc, t, kind);
}

/* Null reflection shared tree.  */

static GTY(()) tree null_reflection;

/* Return a null reflection value.  */

tree
get_null_reflection ()
{
  if (!null_reflection)
    null_reflection = get_reflection_raw (UNKNOWN_LOCATION, unknown_type_node);
  return null_reflection;
}

/* Do strip_typedefs on T, but only for types.  */

static tree
maybe_strip_typedefs (tree t)
{
  if (TYPE_P (t))
    return strip_typedefs (t);
  return t;
}

/* If PARM_DECL comes from an earlier reflection of a function parameter
   and function definition is seen after that, DECL_ARGUMENTS is
   overwritten and so the old PARM_DECL is no longer present in the
   DECL_ARGUMENTS (DECL_CONTEXT (parm)) chain.  Return corresponding
   PARM_DECL which is in the chain.  */

static tree
maybe_update_function_parm (tree parm)
{
  if (!OLD_PARM_DECL_P (parm))
    return parm;
  tree fn = DECL_CONTEXT (parm);
  int oldlen = list_length (parm);
  int newlen = list_length (DECL_ARGUMENTS (fn));
  gcc_assert (newlen >= oldlen);
  tree ret = DECL_ARGUMENTS (fn);
  int n = newlen - oldlen;
  while (n)
    {
      ret = DECL_CHAIN (ret);
      --n;
    }
  return ret;
}

/* Return true if DECL comes from std::meta.  */

static bool
decl_in_std_meta_p (tree decl)
{
  return decl_namespace_context (decl) == std_meta_node;
}

/* Returns true if FNDECL, a FUNCTION_DECL, is a call to a metafunction
   declared in namespace std::meta.  */

bool
metafunction_p (tree fndecl)
{
  if (!flag_reflection)
    return false;

  /* Metafunctions are expected to be marked consteval.  */
  if (!DECL_IMMEDIATE_FUNCTION_P (fndecl))
    return false;

  if (special_function_p (fndecl))
    return false;

  if (!decl_in_std_meta_p (fndecl))
    return false;

  /* They should be user provided and not defined.  */
  if (!user_provided_p (fndecl)
      || (DECL_NAMESPACE_SCOPE_P (fndecl) && DECL_DELETED_FN (fndecl)))
    return false;
  if (DECL_INITIAL (fndecl))
    return false;

  return true;
}

/* Extract the N-th reflection argument from a metafunction call CALL.  */

static tree
get_info (const constexpr_ctx *ctx, tree call, int n, bool *non_constant_p,
	  bool *overflow_p, tree *jump_target)
{
  gcc_checking_assert (call_expr_nargs (call) > n);
  tree info = get_nth_callarg (call, n);
  gcc_checking_assert (REFLECTION_TYPE_P (TREE_TYPE (info)));
  info = cxx_eval_constant_expression (ctx, info, vc_prvalue,
				       non_constant_p, overflow_p,
				       jump_target);
  if (*jump_target)
    return NULL_TREE;
  if (!REFLECT_EXPR_P (info))
    {
      *non_constant_p = true;
      return NULL_TREE;
    }
  return info;
}

/* Try to get the underlying FUNCTION_DECL from reflection if any,
   otherwise return R.  */

static tree
maybe_get_reflection_fndecl (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  r = OVL_FIRST (r);
  return r;
}

/* Helper function for get_range_elts, called through cp_walk_tree.  */

static tree
replace_parm_r (tree *tp, int *walk_subtrees, void *data)
{
  tree *p = (tree *) data;
  if (*tp == p[0])
    *tp = p[1];
  else if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

static tree throw_exception (location_t, const constexpr_ctx *, const char *,
			     tree, bool *, tree *);

/* Kinds for get_range_elts.  */

enum get_range_elts_kind {
  GET_INFO_VEC,
  REFLECT_CONSTANT_STRING,
  REFLECT_CONSTANT_ARRAY
};

/* Extract the N-th input_range argument from a metafunction call CALL
   and return it as TREE_VEC or STRING_CST or CONSTRUCTOR.  Helper function
   for get_info_vec, eval_reflect_constant_string and
   eval_reflect_constant_array.  For GET_INFO_VEC kind, <meta> ensures
   the argument is reference to reflection_range concept and so both
   range_value_t is info and range_refernce_t is cv info or cv info & or
   cv info &&.  */

static tree
get_range_elts (location_t loc, const constexpr_ctx *ctx, tree call, int n,
		bool *non_constant_p, bool *overflow_p, tree *jump_target,
		get_range_elts_kind kind, tree fun)
{
  gcc_checking_assert (call_expr_nargs (call) > n);
  tree arg = get_nth_callarg (call, n);
  tree parm = DECL_ARGUMENTS (cp_get_callee_fndecl_nofold (call));
  for (int i = 0; i < n; ++i)
    parm = DECL_CHAIN (parm);
  tree type = TREE_TYPE (arg);
  gcc_checking_assert (TYPE_REF_P (type));
  arg = cxx_eval_constant_expression (ctx, arg, vc_prvalue, non_constant_p,
				      overflow_p, jump_target);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  tree map[2] = { parm, arg };
  /* To speed things up, check
     if constexpr (std::ranges::contiguous_range <_R>).  */
  tree ranges_ns = lookup_qualified_name (std_node, "ranges");
  if (TREE_CODE (ranges_ns) != NAMESPACE_DECL)
    {
      error_at (loc, "%<std::ranges%> is not a namespace");
      *non_constant_p = true;
      return NULL_TREE;
    }
  tree contiguous_range
    = lookup_qualified_name (ranges_ns, "contiguous_range");
  if (TREE_CODE (contiguous_range) != TEMPLATE_DECL
      || !concept_definition_p (contiguous_range))
    contiguous_range = NULL_TREE;
  else
    {
      tree args = make_tree_vec (1);
      TREE_VEC_ELT (args, 0) = TREE_TYPE (type);
      contiguous_range = build2_loc (loc, TEMPLATE_ID_EXPR, boolean_type_node,
				     contiguous_range, args);
      if (!integer_nonzerop (maybe_constant_value (contiguous_range)))
	contiguous_range = NULL_TREE;
    }
  tree valuet = meta_info_type_node;
  tree ret = NULL_TREE;
  if (kind != GET_INFO_VEC)
    {
      tree args = make_tree_vec (1);
      TREE_VEC_ELT (args, 0) = TREE_TYPE (type);
      tree inst = lookup_template_class (get_identifier ("range_value_t"),
					 args, /*in_decl*/NULL_TREE,
					 /*context*/ranges_ns,
					 tf_warning_or_error);
      inst = complete_type (inst);
      if (inst == error_mark_node)
	{
	  *non_constant_p = true;
	  return NULL_TREE;
	}
      valuet = TYPE_MAIN_VARIANT (inst);
      if (kind == REFLECT_CONSTANT_STRING
	  && valuet != char_type_node
	  && valuet != wchar_type_node
	  && valuet != char8_type_node
	  && valuet != char16_type_node
	  && valuet != char32_type_node)
	{
	  if (!cxx_constexpr_quiet_p (ctx))
	    error_at (loc, "%<reflect_constant_string%> called with %qT "
			   "rather than %<char%>, %<wchar_t%>, %<char8_t%>, "
			   "%<char16_t%> or %<char32_t%>", inst);
	  *non_constant_p = true;
	  return NULL_TREE;
	}
      /* Check for the reflect_object_string special-case, where r
	 refers to a string literal.  In that case CharT() should not
	 be appended.  */
      if (kind == REFLECT_CONSTANT_STRING
	  && TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE
	  && TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type))) == valuet
	  && TYPE_DOMAIN (TREE_TYPE (type)))
	{
	  tree a = arg;
	  tree maxv = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (type)));
	  STRIP_NOPS (a);
	  tree at;
	  if (TREE_CODE (a) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (a, 0)) == STRING_CST
	      && tree_fits_uhwi_p (maxv)
	      && ((unsigned) TREE_STRING_LENGTH (TREE_OPERAND (a, 0))
		  == ((tree_to_uhwi (maxv) + 1)
		       * tree_to_uhwi (TYPE_SIZE_UNIT (valuet))))
	      && (at = TREE_TYPE (TREE_OPERAND (a, 0)))
	      && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (type),
							    at))
	    return TREE_OPERAND (a, 0);
	}
      if (kind == REFLECT_CONSTANT_ARRAY)
	{
	  if (!structural_type_p (valuet))
	    {
	      if (!cxx_constexpr_quiet_p (ctx))
		{
		  auto_diagnostic_group d;
		  error_at (loc, "%<reflect_constant_array%> argument with "
				 "%qT which is not a structural type", inst);
		  structural_type_p (valuet, true);
		}
	      *non_constant_p = true;
	      return NULL_TREE;
	    }
	  TREE_VEC_ELT (args, 0)
	    = build_stub_type (valuet,
			       cp_type_quals (valuet) | TYPE_QUAL_CONST,
			       false);
	  if (!is_xible (INIT_EXPR, valuet, args))
	    {
	      if (!cxx_constexpr_quiet_p (ctx))
		error_at (loc, "%<reflect_constant_array%> argument with %qT "
			       "which is not copy constructible", inst);
	      *non_constant_p = true;
	      return NULL_TREE;
	    }
	  TREE_VEC_ELT (args, 0) = TREE_TYPE (type);
	  tree instr
	    = lookup_template_class (get_identifier ("range_reference_t"),
				     args, /*in_decl*/NULL_TREE,
				     /*context*/ranges_ns,
				     tf_warning_or_error);
	  instr = complete_type (instr);
	  if (instr == error_mark_node)
	    {
	      *non_constant_p = true;
	      return NULL_TREE;
	    }
	  tree referencet = TYPE_MAIN_VARIANT (instr);
	  TREE_VEC_ELT (args, 0) = referencet;
	  if (!is_xible (INIT_EXPR, valuet, args))
	    {
	      if (!cxx_constexpr_quiet_p (ctx))
		error_at (loc, "%<reflect_constant_array%> argument with %qT "
			       "which is not constructible from %qT "
			       "%<std::ranges::range_reference_t%>",
			  inst, referencet);
	      *non_constant_p = true;
	      return NULL_TREE;
	    }
	}
    }
  auto_vec<tree, 32> retvec;
  tree p = convert_from_reference (parm);
  auto obj_call = [=, &map] (tree obj, tsubst_flags_t complain) {
    releasing_vec args;
    vec_safe_push (args, p);
    tree call = finish_call_expr (obj, &args, true, false, complain);
    if (call == error_mark_node)
      return call;
    cp_walk_tree (&call, replace_parm_r, map, NULL);
    if (complain != tf_none)
      return call;
    call = cxx_eval_constant_expression (ctx, call, vc_prvalue, non_constant_p,
					 overflow_p, jump_target);
    if (*jump_target || *non_constant_p)
      return NULL_TREE;
    return call;
  };
  auto ret_retvec = [=, &retvec] () {
    unsigned HOST_WIDE_INT sz = retvec.length ();
    for (size_t i = 0; i < sz; ++i)
      {
	if (INTEGRAL_TYPE_P (valuet))
	  {
	    if (TREE_CODE (retvec[i]) != INTEGER_CST)
	      return throw_exception (loc, ctx,
				      "array element not a constant integer",
				      fun, non_constant_p, jump_target);
	  }
	else
	  {
	    gcc_assert (kind == REFLECT_CONSTANT_ARRAY);
	    tree expr = convert_reflect_constant_arg (valuet, retvec[i]);
	    if (expr == error_mark_node)
	      return throw_exception (loc, ctx, "reflect_constant failed",
				      fun, non_constant_p, jump_target);
	    if (VAR_P (expr))
	      expr = unshare_expr (DECL_INITIAL (expr));
	    retvec[i] = expr;
	  }
      }
    if (kind == REFLECT_CONSTANT_ARRAY && sz == 0)
      {
	/* Return std::array <valuet, 0> {}.  */
	tree args = make_tree_vec (2);
	TREE_VEC_ELT (args, 0) = valuet;
	TREE_VEC_ELT (args, 1) = size_zero_node;
	tree inst = lookup_template_class (get_identifier ("array"), args,
					   /*in_decl*/NULL_TREE,
					   /*context*/std_node,
					   tf_warning_or_error);
	tree type = complete_type (inst);
	if (type == error_mark_node)
	  {
	    *non_constant_p = true;
	    return NULL_TREE;
	  }
	tree ctor = build_constructor (init_list_type_node, nullptr);
	CONSTRUCTOR_IS_DIRECT_INIT (ctor) = true;
	TREE_CONSTANT (ctor) = true;
	TREE_STATIC (ctor) = true;
	tree r = finish_compound_literal (type, ctor, tf_warning_or_error,
					  fcl_functional);
	if (TREE_CODE (r) == TARGET_EXPR)
	  r = TARGET_EXPR_INITIAL (r);
	return r;
      }
    unsigned esz = tree_to_uhwi (TYPE_SIZE_UNIT (valuet));
    unsigned last = kind == REFLECT_CONSTANT_STRING ? esz : 0;
    tree index = build_index_type (size_int (last ? sz : sz - 1));
    tree at = build_array_type (valuet, index);
    at = cp_build_qualified_type (at, TYPE_QUAL_CONST);
    if (kind == REFLECT_CONSTANT_STRING
	|| ((valuet == char_type_node
	     || valuet == wchar_type_node
	     || valuet == char8_type_node
	     || valuet == char16_type_node
	     || valuet == char32_type_node)
	    && integer_zerop (retvec.last ())))
      {
	unsigned HOST_WIDE_INT szt = sz * esz;
	char *p;
	if (szt < 4096)
	  p = XALLOCAVEC (char, szt + last);
	else
	  p = XNEWVEC (char, szt + last);
	for (size_t i = 0; i < sz; ++i)
	  native_encode_expr (retvec[i], (unsigned char *) p + i * esz,
			      esz, 0);
	if (last)
	  memset (p + szt, '\0', last);
	tree ret = build_string (szt + last, p);
	TREE_TYPE (ret) = at;
	TREE_CONSTANT (ret) = 1;
	TREE_READONLY (ret) = 1;
	TREE_STATIC (ret) = 1;
	if (szt >= 4096)
	  XDELETEVEC (p);
	return ret;
      }
    vec<constructor_elt, va_gc> *elts = nullptr;
    for (unsigned i = 0; i < sz; ++i)
      CONSTRUCTOR_APPEND_ELT (elts, bitsize_int (i), retvec[i]);
    return build_constructor (at, elts);
  };
  /* If true, call std::ranges::data (p) and std::ranges::size (p)
     and if that works out and what the former returns can be handled,
     grab the elements from the initializer of the decl pointed by the
     first expression.  p has to be convert_from_reference (PARM_DECL)
     rather than its value, otherwise it is not considered lvalue.  */
  if (contiguous_range)
    {
      tree data = lookup_qualified_name (ranges_ns, "data");
      tree size = lookup_qualified_name (ranges_ns, "size");
      if (TREE_CODE (data) != VAR_DECL || TREE_CODE (size) != VAR_DECL)
	{
	  error_at (loc, "%<std::ranges::data%> or %<std::ranges::size%> "
			 "are not customization point objects");
	  *non_constant_p = true;
	  return NULL_TREE;
	}
      data = obj_call (data, tf_none);
      if (error_operand_p (data))
	goto non_contiguous;
      if (data == NULL_TREE)
	return NULL_TREE;
      size = obj_call (size, tf_none);
      if (error_operand_p (size))
	goto non_contiguous;
      if (size == NULL_TREE)
	return NULL_TREE;
      if (!tree_fits_uhwi_p (size) || tree_to_uhwi (size) > INT_MAX)
	goto non_contiguous;
      if (integer_zerop (size))
	{
	  if (kind == GET_INFO_VEC)
	    return make_tree_vec (0);
	  return ret_retvec ();
	}
      STRIP_NOPS (data);
      unsigned HOST_WIDE_INT minidx = 0, pplus = 0;
      if (TREE_CODE (data) == POINTER_PLUS_EXPR
	  && tree_fits_uhwi_p (TREE_OPERAND (data, 1))
	  && !wi::neg_p (wi::to_wide (TREE_OPERAND (data, 1))))
	{
	  pplus = tree_to_uhwi (TREE_OPERAND (data, 1));
	  data = TREE_OPERAND (data, 0);
	  STRIP_NOPS (data);
	}
      if (TREE_CODE (data) != ADDR_EXPR)
	goto non_contiguous;
      data = TREE_OPERAND (data, 0);
      if (TREE_CODE (data) == ARRAY_REF
	  && tree_fits_uhwi_p (TREE_OPERAND (data, 1)))
	{
	  minidx = tree_to_uhwi (TREE_OPERAND (data, 1));
	  data = TREE_OPERAND (data, 0);
	}
      data = cxx_eval_constant_expression (ctx, data, vc_prvalue,
					   non_constant_p, overflow_p,
					   jump_target);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      if (TREE_CODE (TREE_TYPE (data)) != ARRAY_TYPE
	  || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (data))) != valuet)
	goto non_contiguous;
      if (pplus
	  && (pplus % tree_to_uhwi (TYPE_SIZE_UNIT (valuet))) != 0)
	goto non_contiguous;
      minidx += pplus / tree_to_uhwi (TYPE_SIZE_UNIT (valuet));
      if (kind != GET_INFO_VEC && TREE_CODE (data) == STRING_CST)
	{
	  unsigned esz = tree_to_uhwi (TYPE_SIZE_UNIT (valuet));
	  unsigned HOST_WIDE_INT sz = tree_to_uhwi (size) * esz;
	  if (minidx > INT_MAX
	      || (unsigned) TREE_STRING_LENGTH (data) < sz + minidx * esz)
	    goto non_contiguous;
	  if (kind == REFLECT_CONSTANT_ARRAY && sz == 0)
	    return ret_retvec ();
	  tree index
	    = build_index_type (size_int ((kind == REFLECT_CONSTANT_ARRAY
					   ? -1 : 0) + tree_to_uhwi (size)));
	  tree at = build_array_type (valuet, index);
	  at = cp_build_qualified_type (at, TYPE_QUAL_CONST);
	  const unsigned char *q
	    = (const unsigned char *) TREE_STRING_POINTER (data);
	  q += minidx * esz;
	  if (kind == REFLECT_CONSTANT_ARRAY)
	    {
	      unsigned HOST_WIDE_INT i;
	      for (i = 0; i < esz; ++i)
		if (q[sz - esz + i])
		  break;
	      if (i != esz)
		{
		  /* Not a NUL terminated string.  Build a CONSTRUCTOR
		     instead.  */
		  for (i = 0; i < sz; i += esz)
		    {
		      tree t = native_interpret_expr (valuet, q + i, sz);
		      retvec.safe_push (t);
		    }
		  return ret_retvec ();
		}
	    }
	  char *p;
	  if (sz < 4096)
	    p = XALLOCAVEC (char, sz + esz);
	  else
	    p = XNEWVEC (char, sz + esz);
	  memcpy (p, q, sz);
	  memset (p + sz, '\0', esz);
	  ret = build_string (sz + (kind == REFLECT_CONSTANT_ARRAY
				    ? 0 : esz), p);
	  TREE_TYPE (ret) = at;
	  TREE_CONSTANT (ret) = 1;
	  TREE_READONLY (ret) = 1;
	  TREE_STATIC (ret) = 1;
	  if (sz >= 4096)
	    XDELETEVEC (p);
	  return ret;
	}
      if (TREE_CODE (data) != CONSTRUCTOR)
	goto non_contiguous;
      unsigned sz = tree_to_uhwi (size), i;
      unsigned HOST_WIDE_INT j = 0;
      tree *r, null = NULL_TREE;
      if (kind == GET_INFO_VEC)
	{
	  ret = make_tree_vec (sz);
	  r = TREE_VEC_BEGIN (ret);
	  null = get_null_reflection ();
	}
      else
	{
	  retvec.safe_grow (sz, true);
	  r = retvec.address ();
	}
      for (i = 0; i < sz; ++i)
	r[i] = null;
      tree field, value;
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (data), i, field, value)
	if (field == NULL_TREE)
	  {
	    if (j >= minidx && j - minidx < sz)
	      r[j - minidx] = value;
	    ++j;
	  }
	else if (TREE_CODE (field) == RANGE_EXPR)
	  {
	    tree lo = TREE_OPERAND (field, 0);
	    tree hi = TREE_OPERAND (field, 1);
	    if (!tree_fits_uhwi_p (lo) || !tree_fits_uhwi_p (hi))
	      goto non_contiguous;
	    unsigned HOST_WIDE_INT m = tree_to_uhwi (hi);
	    for (j = tree_to_uhwi (lo); j <= m; ++j)
	      if (j >= minidx && j - minidx < sz)
		r[j - minidx] = value;
	  }
	else if (tree_fits_uhwi_p (field))
	  {
	    j = tree_to_uhwi (field);
	    if (j >= minidx && j - minidx < sz)
	      r[j - minidx] = value;
	    ++j;
	  }
	else
	  goto non_contiguous;
      if (kind == GET_INFO_VEC)
	return ret;
      for (i = 0; i < sz; ++i)
	if (r[i] == NULL_TREE || !tree_fits_shwi_p (r[i]))
	  goto non_contiguous;
      return ret_retvec ();
    }
 non_contiguous:
  /* Otherwise, do it the slower way.  Initialize two temporaries,
     one to std::ranges::base (p) and another to std::ranges::end (p)
     and use a loop.  */
  tree begin = lookup_qualified_name (ranges_ns, "begin");
  tree end = lookup_qualified_name (ranges_ns, "end");
  if (TREE_CODE (begin) != VAR_DECL || TREE_CODE (end) != VAR_DECL)
    {
      error_at (loc, "missing %<std::ranges::begin%> or %<std::ranges::end%>");
      *non_constant_p = true;
      return NULL_TREE;
    }
  begin = obj_call (begin, tf_warning_or_error);
  if (error_operand_p (begin))
    {
      *non_constant_p = true;
      return NULL_TREE;
    }
  end = obj_call (end, tf_warning_or_error);
  if (error_operand_p (end))
    {
      *non_constant_p = true;
      return NULL_TREE;
    }
  if (!CLASS_TYPE_P (TREE_TYPE (begin)) && !POINTER_TYPE_P (TREE_TYPE (begin)))
    {
      error_at (loc, "incorrect type %qT of %<std::ranges::begin(arg)%>",
		TREE_TYPE (begin));
      *non_constant_p = true;
      return NULL_TREE;
    }
  if (VOID_TYPE_P (TREE_TYPE (end)))
    {
      error_at (loc, "incorrect type %qT of %<std::ranges::end(arg)%>",
		TREE_TYPE (end));
      *non_constant_p = true;
      return NULL_TREE;
    }
  begin = get_target_expr (begin);
  end = get_target_expr (end);
  begin = cxx_eval_constant_expression (ctx, begin, vc_glvalue, non_constant_p,
					overflow_p, jump_target);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  end = cxx_eval_constant_expression (ctx, end, vc_glvalue, non_constant_p,
				      overflow_p, jump_target);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  tree cmp = build_new_op (loc, NE_EXPR, LOOKUP_NORMAL, begin, end,
			   tf_warning_or_error);
  tree deref = build_new_op (loc, INDIRECT_REF, LOOKUP_NORMAL, begin,
			     NULL_TREE, tf_warning_or_error);
  tree inc = build_new_op (loc, PREINCREMENT_EXPR, LOOKUP_NORMAL, begin,
			   NULL_TREE, tf_warning_or_error);
  cmp = condition_conversion (cmp);
  if (error_operand_p (cmp)
      || error_operand_p (deref)
      || error_operand_p (inc))
    {
      *non_constant_p = true;
      return NULL_TREE;
    }
  // TODO: For REFLECT_CONSTANT_* handle proxy iterators.
  if (TYPE_MAIN_VARIANT (TREE_TYPE (deref)) != valuet)
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "unexpected type %qT of iterator dereference",
		  TREE_TYPE (deref));
      *non_constant_p = true;
      return NULL_TREE;
    }
  retvec.truncate (0);
  /* while (begin != end) { push (*begin); ++begin; }  */
  do
    {
      tree t = cxx_eval_constant_expression (ctx, cmp, vc_prvalue,
					     non_constant_p, overflow_p,
					     jump_target);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      if (integer_zerop (t))
	break;
      t = cxx_eval_constant_expression (ctx, deref, vc_prvalue, non_constant_p,
					overflow_p, jump_target);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      retvec.safe_push (t);
      cxx_eval_constant_expression (ctx, inc, vc_discard, non_constant_p,
				    overflow_p, jump_target);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  while (true);
  if (kind != GET_INFO_VEC)
    return ret_retvec ();
  ret = make_tree_vec (retvec.length ());
  tree v;
  unsigned int i;
  FOR_EACH_VEC_ELT (retvec, i, v)
    TREE_VEC_ELT (ret, i) = v;
  return ret;
}

/* Extract the N-th reflection_range argument from a metafunction call CALL
   and return it as TREE_VEC.  */

static tree
get_info_vec (location_t loc, const constexpr_ctx *ctx, tree call, int n,
	      bool *non_constant_p, bool *overflow_p, tree *jump_target,
	      tree fun)
{
  return get_range_elts (loc, ctx, call, n, non_constant_p, overflow_p,
			 jump_target, GET_INFO_VEC, fun);
}

/* Create std::meta::exception{ what, from }.  WHAT is the string for what(),
   and FROM is the info for from().  */

static tree
get_meta_exception_object (location_t loc, const char *what, tree from,
			   bool *non_constant_p)
{
  /* Don't throw in a template.  */
  // TODO For -fno-exceptions, report an error.
  if (processing_template_decl)
    {
      *non_constant_p = true;
      return NULL_TREE;
    }

  tree type = lookup_qualified_name (std_meta_node, "exception",
				     LOOK_want::TYPE, /*complain*/true);
  if (TREE_CODE (type) != TYPE_DECL || !CLASS_TYPE_P (TREE_TYPE (type)))
    {
      error_at (loc, "couldn%'t throw %qs", "std::meta::exception");
      return NULL_TREE;
    }
  type = TREE_TYPE (type);
  vec<constructor_elt, va_gc> *elts = nullptr;
  what = _(what);
  /* Translate what from SOURCE_CHARSET to exec charset.  */
  cpp_string istr, ostr;
  istr.len = strlen (what) + 1;
  istr.text = (const unsigned char *) what;
  if (!cpp_translate_string (parse_in, &istr, &ostr, CPP_STRING, false))
    {
      what = "";
      ostr.text = NULL;
    }
  else
    what = (const char *) ostr.text;
  if (TREE_CODE (from) == FUNCTION_DECL && DECL_TEMPLATE_INFO (from))
    from = DECL_TI_TEMPLATE (from);
  tree string_lit = build_string (strlen (what) + 1, what);
  free (const_cast <unsigned char *> (ostr.text));
  TREE_TYPE (string_lit) = char_array_type_node;
  string_lit = fix_string_type (string_lit);
  CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, string_lit);
  CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, get_reflection_raw (loc, from));
  tree ctor = build_constructor (init_list_type_node, elts);
  CONSTRUCTOR_IS_DIRECT_INIT (ctor) = true;
  TREE_CONSTANT (ctor) = true;
  TREE_STATIC (ctor) = true;
  return finish_compound_literal (type, ctor, tf_warning_or_error,
				  fcl_functional);
}

/* Perform 'throw std::meta::exception{...}'.  MSGID is the string for what(),
   FROM is the reflection for from().  */

static tree
throw_exception (location_t loc, const constexpr_ctx *ctx, const char *msgid,
		 tree from, bool *non_constant_p, tree *jump_target)
{
  if (tree obj = get_meta_exception_object (loc, msgid, from, non_constant_p))
    *jump_target = cxa_allocate_and_throw_exception (loc, ctx, obj);
  return NULL_TREE;
}

/* Wrapper around throw_exception to complain that the reflection does not
   represent a type.  */

static tree
throw_exception_nontype (location_t loc, const constexpr_ctx *ctx,
			 tree from, bool *non_constant_p, tree *jump_target)
{
  return throw_exception (loc, ctx,
			  "reflection does not represent a type",
			  from, non_constant_p, jump_target);
}

/* Wrapper around throw_exception to complain that the reflection does not
   represent something that satisfies has_template_arguments.  */

static tree
throw_exception_notargs (location_t loc, const constexpr_ctx *ctx,
			 tree from, bool *non_constant_p, tree *jump_target)
{
  return throw_exception (loc, ctx,
			  "reflection does not have template arguments",
			  from, non_constant_p, jump_target);
}

/* Wrapper around throw_exception to complain that the reflection does not
   represent a function or a function type.  */

static tree
throw_exception_nofn (location_t loc, const constexpr_ctx *ctx,
		      tree from, bool *non_constant_p, tree *jump_target)
{
  return throw_exception (loc, ctx, "reflection does not represent a "
				    "function or function type",
			  from, non_constant_p, jump_target);
}

/* The values of std::meta::operators enumerators corresponding to
   the ovl_op_code and IDENTIFIER_ASSIGN_OP_P pair.  */

static unsigned char meta_operators[2][OVL_OP_MAX];

/* Init the meta_operators table if not yet initialized.  */

static void
maybe_init_meta_operators (location_t loc)
{
  if (meta_operators[0][OVL_OP_ERROR_MARK])
    return;
  meta_operators[0][OVL_OP_ERROR_MARK] = 1;
  tree operators = lookup_qualified_name (std_meta_node, "operators");
  if (TREE_CODE (operators) != TYPE_DECL
      || TREE_CODE (TREE_TYPE (operators)) != ENUMERAL_TYPE)
    {
    fail:
      error_at (loc, "unexpected %<std::meta::operators%>");
      return;
    }
  char buf[sizeof "op_greater_greater_equals"];
  memcpy (buf, "op_", 3);
  for (int i = 0; i < 2; ++i)
    for (int j = OVL_OP_ERROR_MARK + 1; j < OVL_OP_MAX; ++j)
      if (ovl_op_info[i][j].meta_name)
	{
	  strcpy (buf + 3, ovl_op_info[i][j].meta_name);
	  tree id = get_identifier (buf);
	  tree t = lookup_enumerator (TREE_TYPE (operators), id);
	  if (t == NULL_TREE || TREE_CODE (t) != CONST_DECL)
	    goto fail;
	  tree v = DECL_INITIAL (t);
	  if (!tree_fits_uhwi_p (v) || tree_to_uhwi (v) > UCHAR_MAX)
	    goto fail;
	  meta_operators[i][j] = tree_to_uhwi (v);
	}
}

/* Process std::meta::is_variable.
   Returns: true if r represents a variable.  Otherwise, false.  */

static tree
eval_is_variable (const_tree r, reflect_kind kind)
{
  /* ^^param is a variable but parameters_of(parent_of(^^param))[0] is not.  */
  if ((TREE_CODE (r) == PARM_DECL && kind != REFLECT_PARM)
      || (VAR_P (r)
	  && kind == REFLECT_UNDEF
	  /* A structured binding is not a variable.  */
	  && !(DECL_DECOMPOSITION_P (r) && !DECL_DECOMP_IS_BASE (r)))
      || (VAR_P (r)
	  /* Underlying variable of tuple using structured binding is a
	     variable.  */
	  && kind == REFLECT_VAR
	  && DECL_DECOMPOSITION_P (r)
	  && !DECL_DECOMP_IS_BASE (r)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_type.
   Returns: true if r represents an entity whose underlying entity is
   a type.  Otherwise, false.  */

static tree
eval_is_type (const_tree r)
{
  /* Null reflection isn't a type.  */
  if (TYPE_P (r) && r != unknown_type_node)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_type_alias.
   Returns: true if r represents a type alias.  Otherwise, false.  */

static tree
eval_is_type_alias (const_tree r)
{
  if (TYPE_P (r) && typedef_variant_p (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_namespace.
   Returns: true if r represents an entity whose underlying entity is
   a namespace.  Otherwise, false.  */

static tree
eval_is_namespace (const_tree r)
{
  if (TREE_CODE (r) == NAMESPACE_DECL)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_namespace_alias.
   Returns: true if r represents a namespace alias.  Otherwise, false.  */

static tree
eval_is_namespace_alias (const_tree r)
{
  if (TREE_CODE (r) == NAMESPACE_DECL && DECL_NAMESPACE_ALIAS (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_function.
   Returns: true if r represents a function.  Otherwise, false.  */

static tree
eval_is_function (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (TREE_CODE (r) == FUNCTION_DECL)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_function_template.
   Returns: true if r represents a function template.  Otherwise, false.  */

static tree
eval_is_function_template (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (DECL_FUNCTION_TEMPLATE_P (r))
    return boolean_true_node;

  return boolean_false_node;
}

/* Process std::meta::is_variable_template.
   Returns: true if r represents a variable template.  Otherwise, false.  */

static tree
eval_is_variable_template (tree r)
{
  if (variable_template_p (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_class_template.
   Returns: true if r represents a class template.  Otherwise, false.  */

static tree
eval_is_class_template (const_tree r)
{
  if (DECL_CLASS_TEMPLATE_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_alias_template.
   Returns: true if r represents an alias template.  Otherwise, false.  */

static tree
eval_is_alias_template (const_tree r)
{
  if (DECL_ALIAS_TEMPLATE_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_concept.
   Returns: true if r represents a concept.  Otherwise, false.  */

static tree
eval_is_concept (const_tree r)
{
  if (concept_definition_p (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_object.
   Returns: true if r represents an object.  Otherwise, false.  */

static tree
eval_is_object (reflect_kind kind)
{
  if (kind == REFLECT_OBJECT)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_value.
   Returns: true if r represents a value.  Otherwise, false.  */

static tree
eval_is_value (reflect_kind kind)
{
  if (kind == REFLECT_VALUE)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Like get_info_vec, but throw exception if any of the elements aren't
   eval_is_type reflections and change their content to the corresponding
   REFLECT_EXPR_HANDLE.  */

static tree
get_type_info_vec (location_t loc, const constexpr_ctx *ctx, tree call, int n,
		   bool *non_constant_p, bool *overflow_p, tree *jump_target,
		   tree fun)
{
  tree vec = get_info_vec (loc, ctx, call, n, non_constant_p, overflow_p,
			   jump_target, fun);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  for (int i = 0; i < TREE_VEC_LENGTH (vec); i++)
    {
      tree type = REFLECT_EXPR_HANDLE (TREE_VEC_ELT (vec, i));
      if (eval_is_type (type) != boolean_true_node)
	return throw_exception_nontype (loc, ctx, fun, non_constant_p,
					jump_target);
      TREE_VEC_ELT (vec, i) = type;
    }
  return vec;
}

/* Process std::meta::is_structured_binding.
   Returns: true if r represents a structured binding.  Otherwise, false.  */

static tree
eval_is_structured_binding (const_tree r, reflect_kind kind)
{
  if (DECL_DECOMPOSITION_P (r)
      && !DECL_DECOMP_IS_BASE (r)
      && kind != REFLECT_VAR)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_class_member.
   Returns: true if r represents a class member.  Otherwise, false.  */

static tree
eval_is_class_member (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == CONST_DECL)
    {
      /* [class.mem.general]/5 - The enumerators of an unscoped enumeration
	 defined in the class are members of the class.  */
      if (UNSCOPED_ENUM_P (DECL_CONTEXT (r)))
	r = DECL_CONTEXT (r);
      else
	return boolean_false_node;
    }
  else if (TYPE_P (r) && typedef_variant_p (r))
    r = TYPE_NAME (r);
  if (DECL_P (r) && DECL_CLASS_SCOPE_P (r))
    return boolean_true_node;
  else if (TYPE_P (r) && TYPE_CLASS_SCOPE_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* For direct base class relationship R return the binfo related
   to the derived type.  */

static tree
direct_base_derived_binfo (tree r)
{
  /* Looping needed for multiple virtual inheritance.  */
  while (BINFO_INHERITANCE_CHAIN (r))
    r = BINFO_INHERITANCE_CHAIN (r);
  return r;
}

/* For direct base class relationship R return the derived type
   (i.e. when R is (D, B) it returns D).  */

tree
direct_base_derived (tree r)
{
  return BINFO_TYPE (direct_base_derived_binfo (r));
}

/* Helper function for eval_is_{public, protected, private}.  */

static tree
eval_is_expected_access (tree r, reflect_kind kind, tree expected_access)
{
  if (eval_is_class_member (r) == boolean_true_node)
    {
      r = maybe_get_reflection_fndecl (r);

      if (TYPE_P (r))
	{
	  if (TYPE_NAME (r) == NULL_TREE || !DECL_P (TYPE_NAME (r)))
	    return boolean_false_node;
	  r = TYPE_NAME (r);
	}

      bool matches = false;
      if (expected_access == access_private_node)
	matches = TREE_PRIVATE (r);
      else if (expected_access == access_protected_node)
	matches = TREE_PROTECTED (r);
      else if (expected_access == access_public_node)
	matches = !(TREE_PRIVATE (r) || TREE_PROTECTED (r));
      else
	gcc_unreachable ();

      if (matches)
	return boolean_true_node;
      else
	return boolean_false_node;
    }

  if (kind == REFLECT_BASE)
    {
      gcc_assert (TREE_CODE (r) == TREE_BINFO);
      tree c = direct_base_derived_binfo (r);

      tree base_binfo;
      for (unsigned ix = 0; BINFO_BASE_ITERATE (c, ix, base_binfo); ix++)
	if (base_binfo == r)
	  {
	    tree access = BINFO_BASE_ACCESS (c, ix);
	    if (access == expected_access)
	      return boolean_true_node;
	    else
	      return boolean_false_node;
	  }
      gcc_unreachable ();
    }

  return boolean_false_node;
}

/* Process std::meta::is_public.
   Returns: true if r represents either:
   - a class member or unnamed bit-field that is public or
   - a direct base class relationship (D, B) for which
   B is a public base class of D.
   Otherwise, false.  */

static tree
eval_is_public (tree r, reflect_kind kind)
{
  return eval_is_expected_access (r, kind, access_public_node);
}

/* Process std::meta::is_protected.
   Returns: true if r represents either:
   - a class member or unnamed bit-field that is protected, or
   - a direct base class relationship (D, B) for which
   B is a protected base class of D.
   Otherwise, false.  */

static tree
eval_is_protected (tree r, reflect_kind kind)
{
  return eval_is_expected_access (r, kind, access_protected_node);
}

/* Process std::meta::is_private
   Returns: true if r represents either:
   - a class member or unnamed bit-field that is private, or
   - a direct base class relationship (D, B) for which
   B is a private base class of D.
   Otherwise, false.  */

static tree
eval_is_private (tree r, reflect_kind kind)
{
  return eval_is_expected_access (r, kind, access_private_node);
}

/* Process std::meta::is_virtual.
   Returns: true if r represents either a virtual member function or a direct
   base class relationship (D,B) for which B is a virtual base class of D.
   Otherwise, false.  */

static tree
eval_is_virtual (tree r, reflect_kind kind)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_VIRTUAL_P (r))
    return boolean_true_node;

  if (kind == REFLECT_BASE && BINFO_VIRTUAL_P (r))
    return boolean_true_node;

  return boolean_false_node;
}

/* Process std::meta::is_pure_virtual.
   Returns: true if r represents a member function that is pure virtual.
   Otherwise, false.  */

static tree
eval_is_pure_virtual (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_PURE_VIRTUAL_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Helper function for eval_is_override, return true if FNDECL in TYPE
   overrides another function.  */

static bool
is_override (tree type, tree fndecl)
{
  tree binfo = TYPE_BINFO (type), base_binfo;

  for (unsigned ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      tree basetype = BINFO_TYPE (base_binfo);
      if (TYPE_POLYMORPHIC_P (basetype))
	{
	  if (look_for_overrides_here (basetype, fndecl))
	    return true;
	  if (is_override (basetype, fndecl))
	    return true;
	}
    }
  return false;
}

/* Process std::meta::is_override.
   Returns: true if r represents a member function that overrides another
   member function.  Otherwise, false.  */

static tree
eval_is_override (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL
      && DECL_VIRTUAL_P (r)
      && !DECL_STATIC_FUNCTION_P (r)
      && is_override (DECL_CONTEXT (r), r))
    return boolean_true_node;
  return boolean_false_node;
}

/* Process std::meta::is_namespace_member.
   Returns: true if r represents a namespace member.  Otherwise, false.  */

static tree
eval_is_namespace_member (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == CONST_DECL)
    {
      if (UNSCOPED_ENUM_P (DECL_CONTEXT (r)))
	r = DECL_CONTEXT (r);
      else
	return boolean_false_node;
    }
  else if (TYPE_P (r) && typedef_variant_p (r))
    r = TYPE_NAME (r);
  if (r == global_namespace || r == unknown_type_node)
    return boolean_false_node;
  if (DECL_P (r) && DECL_NAMESPACE_SCOPE_P (r))
    return boolean_true_node;
  else if (TYPE_P (r) && TYPE_NAMESPACE_SCOPE_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nonstatic_data_member.
   Returns: true if r represents a non-static data member.
   Otherwise, false.  */

static tree
eval_is_nonstatic_data_member (const_tree r)
{
  if (TREE_CODE (r) == FIELD_DECL && !DECL_UNNAMED_BIT_FIELD (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_static_member.
   Returns: true if r represents a static member.
   Otherwise, false.  */

static tree
eval_is_static_member (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_STATIC_FUNCTION_P (r))
    return boolean_true_node;
  else if (VAR_P (r) && DECL_CLASS_SCOPE_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_base.
   Returns: true if r represents a direct base class relationship.
   Otherwise, false.  */

static tree
eval_is_base (tree r, reflect_kind kind)
{
  if (kind == REFLECT_BASE)
    {
      gcc_assert (TREE_CODE (r) == TREE_BINFO);
      return boolean_true_node;
    }
  else
    return boolean_false_node;
}

/* Process std::meta::has_default_member_initializer.
   Returns: true if r represents a non-static data member that has a default
   member initializer.  Otherwise, false.  */

static tree
eval_has_default_member_initializer (const_tree r)
{
  if (TREE_CODE (r) == FIELD_DECL
      && !DECL_UNNAMED_BIT_FIELD (r)
      && DECL_INITIAL (r) != NULL_TREE)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_static_storage_duration.
   Returns: true if r represents an object or variable that has static
   storage duration.  Otherwise, false.  */

static tree
eval_has_static_storage_duration (const_tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_true_node
      && decl_storage_duration (CONST_CAST_TREE (r)) == dk_static)
    return boolean_true_node;
  /* This includes DECL_NTTP_OBJECT_P objects.  */
  else if (eval_is_object (kind) == boolean_true_node)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_thread_storage_duration.
   Returns: true if r represents an object or variable that has thread
   storage duration.  Otherwise, false.  */

static tree
eval_has_thread_storage_duration (const_tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_true_node
      && decl_storage_duration (CONST_CAST_TREE (r)) == dk_thread)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_automatic_storage_duration.
   Returns: true if r represents an object or variable that has automatic
   storage duration.  Otherwise, false.  */

static tree
eval_has_automatic_storage_duration (const_tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_true_node
      && decl_storage_duration (CONST_CAST_TREE (r)) == dk_auto)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_mutable_member.
   Returns: true if r represents a mutable non-static data member.
   Otherwise, false.  */

static tree
eval_is_mutable_member (tree r)
{
  if (TREE_CODE (r) == FIELD_DECL
      && !DECL_UNNAMED_BIT_FIELD (r)
      && DECL_MUTABLE_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_template.
   Returns: true if r represents a function template, class template, variable
   template, alias template, or concept.  Otherwise, false.  */

static tree
eval_is_template (tree r)
{
  if (eval_is_function_template (r) == boolean_true_node
      || eval_is_class_template (r) == boolean_true_node
      || eval_is_variable_template (r) == boolean_true_node
      || eval_is_alias_template (r) == boolean_true_node
      || eval_is_concept (r) == boolean_true_node)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_function_parameter.
   Returns: true if r represents a function parameter.  Otherwise, false.  */

static tree
eval_is_function_parameter (const_tree r, reflect_kind kind)
{
  if (kind == REFLECT_PARM)
    {
      gcc_checking_assert (TREE_CODE (r) == PARM_DECL);
      return boolean_true_node;
    }
  else
    return boolean_false_node;
}

/* Process std::meta::is_data_member_spec.
   Returns: true if r represents a data member description.
   Otherwise, false.  */

static tree
eval_is_data_member_spec (const_tree r, reflect_kind kind)
{
  if (kind == REFLECT_DATA_MEMBER_SPEC)
    {
      gcc_checking_assert (TREE_CODE (r) == TREE_VEC);
      return boolean_true_node;
    }
  else
    return boolean_false_node;
}

/* Process std::meta::is_explicit_object_parameter.
   Returns: true if r represents a function parameter that is an explicit
   object parameter.  Otherwise, false.  */

static tree
eval_is_explicit_object_parameter (const_tree r, reflect_kind kind)
{
  if (eval_is_function_parameter (r, kind) == boolean_true_node
      && r == DECL_ARGUMENTS (DECL_CONTEXT (r))
      && DECL_XOBJ_MEMBER_FUNCTION_P (DECL_CONTEXT (r)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_default_argument.
   Returns: If r represents a parameter P of a function F, then:
   -- If F is a specialization of a templated function T, then true if there
      exists a declaration D of T that precedes some point in the evaluation
      context and D specifies a default argument for the parameter of T
      corresponding to P.  Otherwise, false.
   -- Otherwise, if there exists a declaration D of F that precedes some
      point in the evaluation context and D specifies a default argument
      for P, then true.
   Otherwise, false.  */

static tree
eval_has_default_argument (tree r, reflect_kind kind)
{
  if (eval_is_function_parameter (r, kind) == boolean_false_node)
    return boolean_false_node;
  r = maybe_update_function_parm (r);
  tree fn = DECL_CONTEXT (r);
  tree args = FUNCTION_FIRST_USER_PARM (fn);
  tree types = FUNCTION_FIRST_USER_PARMTYPE (fn);
  while (r != args)
    {
      args = DECL_CHAIN (args);
      types = TREE_CHAIN (types);
    }
  if (TREE_PURPOSE (types))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_ellipsis_parameter.
   Returns: true if r represents a function or function type that has an
   ellipsis in its parameter-type-list.  Otherwise, false.  */

static tree
eval_has_ellipsis_parameter (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL)
    r = TREE_TYPE (r);
  if (FUNC_OR_METHOD_TYPE_P (r) && stdarg_p (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_deleted.
   Returns: true if r represents a function that is deleted.
   Otherwise, false.  */

static tree
eval_is_deleted (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_MAYBE_DELETED (r))
    {
      ++function_depth;
      maybe_synthesize_method (r);
      --function_depth;
    }
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_DELETED_FN (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_defaulted.
   Returns: true if r represents a function that is defaulted.
   Otherwise, false.  */

static tree
eval_is_defaulted (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_DEFAULTED_FN (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_user_provided.
   Returns: true if r represents a function that is user-provided.
   Otherwise, false.  */

static tree
eval_is_user_provided (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL
      && user_provided_p (r)
      // TODO: user_provided_p is false for non-members defaulted on
      // first declaration.
      && (!DECL_NAMESPACE_SCOPE_P (r) || !DECL_DELETED_FN (r)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_user_declared.
   Returns: true if r represents a function that is user-declared.
   Otherwise, false.  */

static tree
eval_is_user_declared (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL && !DECL_ARTIFICIAL (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_explicit.
   Returns: true if r represents
   a member function that is declared explicit.
   Otherwise, false.
   If r represents a member function template
   that is declared explicit, is_explicit(r)
   is still false because in general such queries
   for templates cannot be answered.  */

static tree
eval_is_explicit (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (TREE_CODE (r) == FUNCTION_DECL && DECL_NONCONVERTING_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_bit_field.
   Returns: true if r represents a bit-field, or if r represents a data member
   description (T,N,A,W,NUA) for which W is not _|_.  Otherwise, false.  */

static tree
eval_is_bit_field (const_tree r, reflect_kind kind)
{
  if (TREE_CODE (r) == FIELD_DECL && DECL_C_BIT_FIELD (r))
    return boolean_true_node;
  else if (kind == REFLECT_DATA_MEMBER_SPEC && TREE_VEC_ELT (r, 3))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_enumerator.
   Returns: true if r represents an enumerator.  Otherwise, false.  */

static tree
eval_is_enumerator (const_tree r)
{
  /* This doesn't check !DECL_TEMPLATE_PARM_P because such CONST_DECLs
     would already have been rejected in get_reflection.  */
  if (TREE_CODE (r) == CONST_DECL)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Get the linkage name for T, or NULL_TREE, if N/A.  */

static tree
type_linkage_name (tree t)
{
  if (TYPE_NAME (t) == NULL_TREE
      || !DECL_P (TYPE_NAME (t))
      || (!DECL_IMPLICIT_TYPEDEF_P (TYPE_NAME (t))
	  && TYPE_NAME (t) == TYPE_NAME (TYPE_MAIN_VARIANT (t))
	  && !TYPE_MAIN_DECL (t)))
    return NULL_TREE;

  return TYPE_NAME (t);
}

/* Process std::meta::has_internal_linkage.
   Returns: true if r represents a variable, function, type, template, or
   namespace whose name has internal linkage.  Otherwise, false.  */

static tree
eval_has_internal_linkage (tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_false_node
      && eval_is_function (r) == boolean_false_node
      && eval_is_type (r) == boolean_false_node
      && eval_is_template (r) == boolean_false_node
      && eval_is_namespace (r) == boolean_false_node)
    return boolean_false_node;
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  if (TYPE_P (r))
    {
      r = type_linkage_name (r);
      if (!r)
	return boolean_false_node;
    }
  if (decl_linkage (r) == lk_internal)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_module_linkage.
   Returns: true if r represents a variable, function, type, template, or
   namespace whose name has module linkage.  Otherwise, false.  */

static tree
eval_has_module_linkage (tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_false_node
      && eval_is_function (r) == boolean_false_node
      && eval_is_type (r) == boolean_false_node
      && eval_is_template (r) == boolean_false_node
      && eval_is_namespace (r) == boolean_false_node)
    return boolean_false_node;
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  if (TYPE_P (r))
    {
      r = type_linkage_name (r);
      if (!r)
	return boolean_false_node;
    }
  if (decl_linkage (r) == lk_external
      && DECL_LANG_SPECIFIC (r)
      && DECL_MODULE_ATTACH_P (r)
      && !DECL_MODULE_EXPORT_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_external_linkage.
   Returns: true if r represents a variable, function, type, template, or
   namespace whose name has external linkage.  Otherwise, false.  */

static tree
eval_has_external_linkage (tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_false_node
      && eval_is_function (r) == boolean_false_node
      && eval_is_type (r) == boolean_false_node
      && eval_is_template (r) == boolean_false_node
      && eval_is_namespace (r) == boolean_false_node)
    return boolean_false_node;
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  if (TYPE_P (r))
    {
      r = type_linkage_name (r);
      if (!r)
	return boolean_false_node;
    }
  if (decl_linkage (r) == lk_external
      && !(DECL_LANG_SPECIFIC (r)
	   && DECL_MODULE_ATTACH_P (r)
	   && !DECL_MODULE_EXPORT_P (r)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_c_language_linkage
   Returns: true if r represents a variable, function, or function type with
   C language linkage. Otherwise, false.  */

static tree
eval_has_c_language_linkage (tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_false_node
      && eval_is_function (r) == boolean_false_node
      && eval_is_function_type (r) == boolean_false_node)
    return boolean_false_node;
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  if (TYPE_P (r))
    {
      r = type_linkage_name (r);
      if (!r)
	return boolean_false_node;
    }
  if (decl_linkage (r) != lk_none && DECL_LANGUAGE (r) == lang_c)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_linkage.
   Returns: true if r represents a variable, function, type, template, or
   namespace whose name has any linkage.  Otherwise, false.  */

static tree
eval_has_linkage (tree r, reflect_kind kind)
{
  if (eval_is_variable (r, kind) == boolean_false_node
      && eval_is_function (r) == boolean_false_node
      && eval_is_type (r) == boolean_false_node
      && eval_is_template (r) == boolean_false_node
      && eval_is_namespace (r) == boolean_false_node)
    return boolean_false_node;
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  if (TYPE_P (r))
    {
      r = type_linkage_name (r);
      if (!r)
	return boolean_false_node;
    }
  if (decl_linkage (r) != lk_none)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_complete_type.
   Returns: true if is_type(r) is true and there is some point in the
   evaluation context from which the type represented by dealias(r) is
   not an incomplete type.  Otherwise, false.  */

static tree
eval_is_complete_type (const_tree r)
{
  if (eval_is_type (r) == boolean_true_node)
    {
      complete_type (const_cast<tree> (r));
      if (COMPLETE_TYPE_P (r))
	return boolean_true_node;
    }
  return boolean_false_node;
}

/* Process std::meta::is_enumerable_type.
   A type T is enumerable from a point P if either
   -- T is a class type complete at point P or
   -- T is an enumeration type defined by a declaration D such that D is
      reachable from P but P does not occur within an enum-specifier of D.
  Returns: true if dealias(r) represents a type that is enumerable from some
  point in the evaluation context.  Otherwise, false.  */

static tree
eval_is_enumerable_type (const_tree r)
{
  if (CLASS_TYPE_P (r))
    {
      complete_type (const_cast<tree> (r));
      if (COMPLETE_TYPE_P (r))
	return boolean_true_node;
     }
  else if (TREE_CODE (r) == ENUMERAL_TYPE)
    {
      r = TYPE_MAIN_VARIANT (r);
      if (!ENUM_IS_OPAQUE (r) && !ENUM_BEING_DEFINED_P (r))
	return boolean_true_node;
    }
  return boolean_false_node;
}

/* Process std::meta::is_annotation.
   Returns: true if r represents an annotation.  Otherwise, false.  */

static tree
eval_is_annotation (const_tree r, reflect_kind kind)
{
  if (kind == REFLECT_ANNOTATION)
    {
      gcc_assert (TREE_CODE (r) == TREE_LIST);
      return boolean_true_node;
    }
  else
    return boolean_false_node;
}

/* Process std::meta::is_conversion_function.
   Returns: true if r represents a function that is a conversion function.
   Otherwise, false.  */

static tree
eval_is_conversion_function (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_CONV_FN_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_operator_function.
   Returns: true if r represents a function that is an operator function.
   Otherwise, false.  */

static tree
eval_is_operator_function (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (TREE_CODE (r) == FUNCTION_DECL
      && DECL_OVERLOADED_OPERATOR_P (r)
      && !DECL_CONV_FN_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_literal_operator.
   Returns: true if r represents a function that is a literal operator.
   Otherwise, false.  */

static tree
eval_is_literal_operator (const_tree r)
{
  /* No MAYBE_BASELINK_FUNCTIONS here because a literal operator
     must be a non-member function.  */
  if (TREE_CODE (r) == FUNCTION_DECL && UDLIT_OPER_P (DECL_NAME (r)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_special_member_function.
   Returns: true if r represents a function that is a special member function.
   Otherwise, false.  */

static tree
eval_is_special_member_function (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL && special_memfn_p (r) != sfk_none)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_constructor.
   Returns: true if r represents a function that is a constructor.
   Otherwise, false.  */

static tree
eval_is_constructor (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_CONSTRUCTOR_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_default_constructor.
   Returns: true if r represents a function that is a default constructor.
   Otherwise, false.  */

static tree
eval_is_default_constructor (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL && default_ctor_p (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_copy_constructor.
   Returns: true if r represents a function that is a copy constructor.
   Otherwise, false.  */

static tree
eval_is_copy_constructor (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_COPY_CONSTRUCTOR_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_move_constructor.
   Returns: true if r represents a function that is a move constructor.
   Otherwise, false.  */

static tree
eval_is_move_constructor (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL && DECL_MOVE_CONSTRUCTOR_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_assignment.
   Returns: true if r represents a function that is an assignment operator.
   Otherwise, false.  */

static tree
eval_is_assignment (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL
      && DECL_ASSIGNMENT_OPERATOR_P (r)
      && DECL_OVERLOADED_OPERATOR_IS (r, NOP_EXPR))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_copy_assignment.
   Returns: true if r represents a function that is a copy assignment
   operator.  Otherwise, false.  */

static tree
eval_is_copy_assignment (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL
      && special_function_p (r) == sfk_copy_assignment)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_move_assignment.
   Returns: true if r represents a function that is a move assignment
   operator.  Otherwise, false.  */

static tree
eval_is_move_assignment (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL
      && special_function_p (r) == sfk_move_assignment)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_destructor.
   Returns: true if r represents a function that is a destructor.
   Otherwise, false.  */

static tree
eval_is_destructor (tree r)
{
  r = maybe_get_reflection_fndecl (r);
  if (TREE_CODE (r) == FUNCTION_DECL
      && DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_conversion_function_template.
   Returns: true if r represents a conversion function template.
   Otherwise, false.  */

static tree
eval_is_conversion_function_template (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (DECL_FUNCTION_TEMPLATE_P (r) && DECL_CONV_FN_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_operator_function_template.
   Returns: true if r represents an operator function template.
   Otherwise, false.  */

static tree
eval_is_operator_function_template (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (DECL_FUNCTION_TEMPLATE_P (r))
    {
      r = STRIP_TEMPLATE (r);
      if (DECL_OVERLOADED_OPERATOR_P (r) && !DECL_CONV_FN_P (r))
	return boolean_true_node;
    }

  return boolean_false_node;
}

/* Process std::meta::is_literal_operator_template.
   Returns: true if r represents a literal operator template.
   Otherwise, false.  */

static tree
eval_is_literal_operator_template (tree r)
{
  /* No MAYBE_BASELINK_FUNCTIONS here because a literal operator
     template must be a non-member function template.  */
  r = OVL_FIRST (r);

  if (DECL_FUNCTION_TEMPLATE_P (r) && UDLIT_OPER_P (DECL_NAME (r)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_constructor_template.
   Returns: true if r represents a function that is an operator function
   template.  Otherwise, false.  */

static tree
eval_is_constructor_template (tree r)
{
  r = maybe_get_reflection_fndecl (r);

  if (DECL_FUNCTION_TEMPLATE_P (r) && DECL_CONSTRUCTOR_P (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::operator_of.
   Returns: The value of the enumerator from the operators whose corresponding
   operator-function-id is the unqualified name of the entity represented by
   r.
   Throws: meta::exception unless r represents an operator function or
   operator function template.  */

static tree
eval_operator_of (location_t loc, const constexpr_ctx *ctx, tree r,
		  bool *non_constant_p, tree *jump_target, tree ret_type,
		  tree fun)
{
  if (eval_is_operator_function (r) == boolean_false_node
      && eval_is_operator_function_template (r) == boolean_false_node)
    return throw_exception (loc, ctx,
			    "reflection does not represent an operator "
			    "function or operator function template",
			    fun, non_constant_p, jump_target);
  r = maybe_get_reflection_fndecl (r);
  r = STRIP_TEMPLATE (r);
  maybe_init_meta_operators (loc);
  int i = IDENTIFIER_ASSIGN_OP_P (DECL_NAME (r)) ? 1 : 0;
  int j = IDENTIFIER_CP_INDEX (DECL_NAME (r));
  return build_int_cst (ret_type, meta_operators[i][j]);
}

/* Helper to build a string literal containing '\0' terminated NAME.
   ELT_TYPE must be either char_type_node or char8_type_node, and the
   function takes care of converting the name from SOURCE_CHARSET
   to ordinary literal charset resp. UTF-8.  Returns the string
   literal, or NULL_TREE if the conversion failed.  */

static tree
get_string_literal (const char *name, tree elt_type)
{
  cpp_string istr, ostr;
  istr.len = strlen (name) + 1;
  istr.text = (const unsigned char *) name;
  if (!cpp_translate_string (parse_in, &istr, &ostr,
			     elt_type == char_type_node
			     ? CPP_STRING : CPP_UTF8STRING, false))
    return NULL_TREE;
  name = (const char *) ostr.text;
  tree ret = build_string_literal (strlen (name) + 1, name, elt_type);
  free (const_cast <char *> (name));
  return ret;
}

/* Process std::meta::{,u8}symbol_of.
   Returns: A string_view or u8string_view containing the characters of the
   operator symbol name corresponding to op, respectively encoded with the
   ordinary literal encoding or with UTF-8.
   Throws: meta::exception unless the value of op corresponds to one of the
   enumerators in operators.  */

static tree
eval_symbol_of (location_t loc, const constexpr_ctx *ctx, tree expr,
		bool *non_constant_p, tree *jump_target, tree elt_type,
		tree ret_type, tree fun)
{
  maybe_init_meta_operators (loc);
  if (!tree_fits_uhwi_p (expr))
    {
    fail:
      return throw_exception (loc, ctx,
			      "operators argument is not a valid operator",
			      fun, non_constant_p, jump_target);
    }
  unsigned HOST_WIDE_INT val = tree_to_uhwi (expr);
  for (int i = 0; i < 2; ++i)
    for (int j = OVL_OP_ERROR_MARK + 1; j < OVL_OP_MAX; ++j)
      if (ovl_op_info[i][j].meta_name && meta_operators[i][j] == val)
	{
	  const char *name = ovl_op_info[i][j].name;
	  char buf[64];
	  if (const char *sp = strchr (name, ' '))
	    {
	      memcpy (buf, name, sp - name);
	      strcpy (buf + (sp - name), sp + 1);
	      name = buf;
	    }
	  tree str = get_string_literal (name, elt_type);
	  /* Basic character set ought to be better convertible
	     into ordinary literal character set and must be always
	     convertible into UTF-8.  */
	  gcc_checking_assert (str);
	  releasing_vec args (make_tree_vector_single (str));
	  tree r = build_special_member_call (NULL_TREE,
					      complete_ctor_identifier,
					      &args, ret_type, LOOKUP_NORMAL,
					      tf_warning_or_error);
	  return build_cplus_new (ret_type, r, tf_warning_or_error);
	}
  goto fail;
}

/* has-type (exposition only).
   Returns: true if r represents a value, annotation, object, variable,
   function whose type does not contain an undeduced placeholder type and
   that is not a constructor or destructor, enumerator, non-static data
   member, unnamed bit-field, direct base class relationship, data member
   description, or function parameter.  Otherwise, false.  */

static bool
has_type (tree r, reflect_kind kind)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL)
    {
      if (DECL_CONSTRUCTOR_P (r) || DECL_DESTRUCTOR_P (r))
	return false;
      if (undeduced_auto_decl (r))
	return false;
      return true;
    }
  if (CONSTANT_CLASS_P (r)
      || eval_is_variable (r, kind) == boolean_true_node
      || eval_is_enumerator (r) == boolean_true_node
      || TREE_CODE (r) == FIELD_DECL
      || eval_is_annotation (r, kind) == boolean_true_node
      || eval_is_function_parameter (r, kind) == boolean_true_node
      || eval_is_object (kind) == boolean_true_node
      || eval_is_value (kind) == boolean_true_node
      || kind == REFLECT_BASE
      || kind == REFLECT_DATA_MEMBER_SPEC)
    return true;
  return false;
}

/* Helper function for eval_type_of.  Assuming has_type is true, return
   the std::meta::type_of type (rather than reflection thereof).  */

static tree
type_of (tree r, reflect_kind kind)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == PARM_DECL && kind == REFLECT_PARM)
    {
      r = maybe_update_function_parm (r);
      tree fn = DECL_CONTEXT (r);
      tree args = FUNCTION_FIRST_USER_PARM (fn);
      tree type = FUNCTION_FIRST_USER_PARMTYPE (fn);
      while (r != args)
	{
	  args = DECL_CHAIN (args);
	  type = TREE_CHAIN (type);
	}
      r = TREE_VALUE (type);
    }
  else if (kind == REFLECT_BASE)
    r = BINFO_TYPE (r);
  else if (kind == REFLECT_DATA_MEMBER_SPEC)
    r = TREE_VEC_ELT (r, 0);
  else if (eval_is_annotation (r, kind) == boolean_true_node)
    {
      r = TREE_TYPE (TREE_VALUE (TREE_VALUE (r)));
      if (CLASS_TYPE_P (r))
	{
	  int quals = cp_type_quals (r);
	  quals |= TYPE_QUAL_CONST;
	  r = cp_build_qualified_type (r, quals);
	}
    }
  else if (TREE_CODE (r) == FIELD_DECL && DECL_BIT_FIELD_TYPE (r))
    r = DECL_BIT_FIELD_TYPE (r);
  else
    r = TREE_TYPE (r);
  return strip_typedefs (r);
}

/* Process std::meta::type_of.  Returns:
   -- If r represents the ith parameter of a function F, then the ith type
      in the parameter-type-list of F.
   -- Otherwise, if r represents a value, object, variable, function,
      non-static data member, or unnamed bit-field, then the type of what is
      represented by r.
   -- Otherwise, if r represents an annotation, then type_of(constant_of(r)).
   -- Otherwise, if r represents an enumerator N of an enumeration E, then:
      -- If E is defined by a declaration D that precedes a point P in the
	 evaluation context and P does not occur within an enum-specifier of
	 D, then a reflection of E.
      -- Otherwise, a reflection of the type of N prior to the closing brace
	 of the enum-specifier as specified in [dcl.enum].
   -- Otherwise, if r represents a direct base class relationship (D,B), then
      a reflection of B.
   -- Otherwise, for a data member description (T,N,A,W,NUA), a reflection of
      the type T.  */

static tree
eval_type_of (location_t loc, const constexpr_ctx *ctx, tree r,
	      reflect_kind kind, bool *non_constant_p, tree *jump_target,
	      tree fun)
{
  if (!has_type (r, kind))
    return throw_exception (loc, ctx, "reflection does not have a type",
			    fun, non_constant_p, jump_target);
  return get_reflection_raw (loc, type_of (r, kind));
}

/* Process std::meta::source_location_of.
   Returns: If r represents a value, a type other than a class type or an
   enumeration type, the global namespace, or a data member description,
   then source_location{}.  Otherwise, an implementation-defined
   source_location value.  */

static tree
eval_source_location_of (location_t loc, tree r, reflect_kind kind,
			 tree std_source_location)
{
  if (!NON_UNION_CLASS_TYPE_P (std_source_location))
    {
      error_at (loc, "%qT is not a class type", std_source_location);
      return error_mark_node;
    }
  location_t rloc = UNKNOWN_LOCATION;
  if (kind == REFLECT_BASE)
    /* We don't track location_t of the base specifiers, so at least
       for now use location_t of the base parent (i.e. the derived
       class).  */
    r = direct_base_derived (r);
  if (OVERLOAD_TYPE_P (r) || (TYPE_P (r) && typedef_variant_p (r)))
    rloc = DECL_SOURCE_LOCATION (TYPE_NAME (r));
  else if (DECL_P (r) && r != global_namespace)
    rloc = DECL_SOURCE_LOCATION (r);
  else if (eval_is_annotation (r, kind) == boolean_true_node)
    rloc = EXPR_LOCATION (TREE_VALUE (TREE_VALUE (r)));
  tree decl = NULL_TREE, field = NULL_TREE;
  if (rloc != UNKNOWN_LOCATION)
    {
      /* Make sure __builtin_source_location (which depends on
	 std::source_location::__impl) will work without errors.  */
      tree name = get_identifier ("__impl");
      decl = lookup_qualified_name (std_source_location, name);
      if (TREE_CODE (decl) != TYPE_DECL)
	decl = NULL_TREE;
      else
	{
	  name = get_identifier ("__builtin_source_location");
	  decl = lookup_qualified_name (global_namespace, name);
	  if (TREE_CODE (decl) != FUNCTION_DECL
	      || !fndecl_built_in_p (decl, BUILT_IN_FRONTEND)
	      || DECL_FE_FUNCTION_CODE (decl) != CP_BUILT_IN_SOURCE_LOCATION
	      || !require_deduced_type (decl, tf_warning_or_error))
	    decl = NULL_TREE;
	}
    }
  if (decl)
    {
      field = TYPE_FIELDS (std_source_location);
      field = next_aggregate_field (field);
      /* Make sure std::source_location has exactly a single non-static
	 data member (_M_impl in libstdc++, __ptr_ in libc++) with pointer
	 type.  Return {._M_impl = &*.Lsrc_locN}.  */
      if (field != NULL_TREE
	  && POINTER_TYPE_P (TREE_TYPE (field))
	  && !next_aggregate_field (DECL_CHAIN (field)))
	{
	  tree call = build_call_nary (TREE_TYPE (TREE_TYPE (decl)), decl, 0);
	  SET_EXPR_LOCATION (call, rloc);
	  call = fold_builtin_source_location (call);
	  return build_constructor_single (std_source_location, field, call);
	}
    }
  return build_constructor (std_source_location, nullptr);
}

/* If R is (const T &) &foo, get foo.  */

static tree
maybe_get_reference_referent (tree r)
{
  if (TREE_CODE (r) == NOP_EXPR
      && TYPE_REF_P (TREE_TYPE (r))
      && TREE_CODE (TREE_OPERAND (r, 0)) == ADDR_EXPR)
    {
      STRIP_NOPS (r);
      r = TREE_OPERAND (r, 0);
    }
  return r;
}

/* Process std::meta::object_of.
   Returns:
   -- If r represents an object, then r.
   -- Otherwise, if r represents a reference, then a reflection of the object
      referred to by that reference.
   -- Otherwise, r represents a variable; a reflection of the object declared
      by that variable.
   Throws: meta::exception unless r is a reflection representing either
   -- an object with static storage duration, or
   -- a variable that either declares or refers to such an object, and if that
      variable is a reference R, then either
      -- R is usable in constant expressions, or
      -- the lifetime of R began within the core constant expression currently
	 under evaluation.  */

static tree
eval_object_of (location_t loc, const constexpr_ctx *ctx, tree r,
		reflect_kind kind, bool *non_constant_p, bool *overflow_p,
		tree *jump_target, tree fun)
{
  tree orig = r;
  if (TYPE_REF_P (TREE_TYPE (r)))
    r = cxx_eval_constant_expression (ctx, r, vc_prvalue, non_constant_p,
				      overflow_p, jump_target);
  r = maybe_get_reference_referent (r);
  if (eval_has_static_storage_duration (orig, kind) == boolean_false_node
      && (orig == r
	  || eval_has_static_storage_duration (r, kind) == boolean_false_node))
    return throw_exception (loc, ctx, "reflection does not represent an"
				      " object with static storage duration,"
				      " or a reference to such an object",
			    fun, non_constant_p, jump_target);
  return get_reflection_raw (loc, r, REFLECT_OBJECT);
}

/* Process std::meta::constant_of.
   Let R be a constant expression of type info such that R == r is true.
   If r represents an annotation, then let C be its underlying constant.
   Effects: Equivalent to:
     if constexpr (is_annotation(R)) {
       return C;
     } else if constexpr (is_array_type(type_of(R)) {
       return reflect_constant_array([: R :]);
     } else if constexpr (is_function_type(type_of(R)) {
       return reflect_function([: R :]);
     } else {
       return reflect_constant([: R :]);
     }
   Throws: meta::exception unless either r represents an annotation or
   [: R :] is a valid splice-expression.  */

static tree
eval_constant_of (location_t loc, const constexpr_ctx *ctx, tree r,
		  reflect_kind kind, bool *non_constant_p, bool *overflow_p,
		  tree *jump_target, tree fun)
{
  tree type;
  if (has_type (r, kind))
    type = type_of (r, kind);
  else
    type = maybe_strip_typedefs (r);

  /* So that outer_automatic_var_p works below in check_splice_expr.  */
  temp_override<tree> ovr (current_function_decl);
  current_function_decl = cxx_constexpr_caller (ctx);

  if (eval_is_annotation (r, kind) == boolean_true_node)
    r = tree_strip_any_location_wrapper (TREE_VALUE (TREE_VALUE (r)));
  else if (eval_is_array_type (loc, type) == boolean_true_node)
    {
      const tsubst_flags_t complain = complain_flags (ctx);
      /* Create a call to reflect_constant_array so that we can simply
	 let eval_reflect_constant_array do its job.  */
      tree name = get_identifier ("reflect_constant_array");
      tree call = lookup_qualified_name (std_meta_node, name);
      if (error_operand_p (call) || !is_overloaded_fn (call))
	{
	  if (complain)
	    error_at (loc, "couldn%'t look up %<%D::%D%>", std_meta_node, name);
	  *non_constant_p = true;
	  return NULL_TREE;
	}
      /* We want the argument to be a CONSTRUCTOR or a STRING_CST.  */
      r = cxx_eval_constant_expression (ctx, r, vc_prvalue, non_constant_p,
					overflow_p, jump_target);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      releasing_vec args (make_tree_vector_single (r));
      call = finish_call_expr (call, &args, /*disallow_virtual=*/true,
			       /*koenig_p=*/false, complain);
      if (call == error_mark_node)
	{
	  *non_constant_p = true;
	  return NULL_TREE;
	}
      return eval_reflect_constant_array (loc, ctx, call, non_constant_p,
					  overflow_p, jump_target, fun);
    }
  else if (eval_is_function_type (type) == boolean_true_node)
    return eval_reflect_function (loc, ctx, type, r, non_constant_p,
				  jump_target, fun);
  else if (!check_splice_expr (loc, UNKNOWN_LOCATION, r,
			       /*address_p=*/false,
			       /*member_access_p=*/false,
			       /*complain_p=*/false)
	   /* One cannot query the value of a function template.
	      ??? But if [:^^X:] where X is a template is OK, should we
	      really throw?  We need an LWG issue.  */
	   || eval_is_template (r) == boolean_true_node)
    return throw_exception (loc, ctx, "reflection does not represent an "
				      "annotation or a valid argument to "
				      "a splice-expression",
			    fun, non_constant_p, jump_target);

  r = cxx_eval_constant_expression (ctx, r, vc_prvalue, non_constant_p,
				    overflow_p, jump_target);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  /* Figure out the type for reflect_constant.  */
  type = TREE_TYPE (convert_from_reference (r));
  type = type_decays_to (type);
  type = cv_unqualified (type);

  return eval_reflect_constant (loc, ctx, type, r, non_constant_p, jump_target,
				fun);
}

/* Process std::meta::dealias.
   Returns: If r represents an entity, then a reflection representing the
   underlying entity of what r represents.  Otherwise, r.
   This implements LWG 4427 so we do not throw.  */

static tree
eval_dealias (location_t loc, tree r, reflect_kind kind)
{
  r = maybe_strip_typedefs (r);
  if (TREE_CODE (r) == NAMESPACE_DECL)
    r = ORIGINAL_NAMESPACE (r);
  return get_reflection_raw (loc, r, kind);
}

/* Process std::meta::is_noexcept.
   Returns: true if r represents a noexcept function type or a function
   with a non-throwing exception specification ([except.spec]).
   Otherwise, false.
   Note: If r represents a function template that is declared noexcept,
   is_noexcept (r) is still false because in general such queries
   for templates cannot be answered.  */

static tree
eval_is_noexcept (tree r)
{
  if (eval_is_function (r) == boolean_true_node)
    {
      r = maybe_get_reflection_fndecl (r);
      maybe_instantiate_noexcept (r);
      if (TYPE_NOTHROW_P (TREE_TYPE (r)))
	return boolean_true_node;
      else
	return boolean_false_node;
    }

  if (eval_is_function_type (r) == boolean_true_node
      && TYPE_NOTHROW_P (r))
    return boolean_true_node;

  return boolean_false_node;
}

/* Process std::meta::is_const.
   Let T be type_of(r) if has-type(r) is true.  Otherwise, let T be dealias(r).
   Returns: true if T represents a const type, or a const-qualified function
   type.  Otherwise, false.  */

static tree
eval_is_const (tree r, reflect_kind kind)
{
  if (has_type (r, kind))
    r = type_of (r, kind);
  else
    r = maybe_strip_typedefs (r);
  r = strip_array_types (r);
  if (TREE_CODE (r) == METHOD_TYPE)
    {
      if (type_memfn_quals (r) & TYPE_QUAL_CONST)
	return boolean_true_node;
    }
  else if (TYPE_P (r) && TYPE_READONLY (r))
    return boolean_true_node;
  return boolean_false_node;
}

/* Process std::meta::is_volatile.
   Let T be type_of(r) if has-type(r) is true.  Otherwise, let T be dealias(r).
   Returns: true if T represents a volatile type, or a volatile-qualified
   function type.  Otherwise, false.  */

static tree
eval_is_volatile (tree r, reflect_kind kind)
{
  if (has_type (r, kind))
    r = type_of (r, kind);
  else
    r = maybe_strip_typedefs (r);
  r = strip_array_types (r);
  if (TREE_CODE (r) == METHOD_TYPE)
    {
      if (type_memfn_quals (r) & TYPE_QUAL_VOLATILE)
	return boolean_true_node;
    }
  else if (TYPE_P (r) && TYPE_VOLATILE (r))
    return boolean_true_node;
  return boolean_false_node;
}

/* Process std::meta::has_template_arguments.
   Returns: true if r represents a specialization of a function template,
   variable template, class template, or an alias template.  Otherwise,
   false.  */

static tree
eval_has_template_arguments (tree r)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  /* Presumably for
       typedef cls_tmpl<int> TYPE;
     'has_template_arguments (^^TYPE)' should be false?  */
  if (TYPE_P (r)
      && typedef_variant_p (r)
      && !alias_template_specialization_p (r, nt_opaque))
    return boolean_false_node;
  if (primary_template_specialization_p (r)
      || variable_template_specialization_p (r))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::template_of.
   Returns: A reflection of the template of the specialization represented
   by r.
   Throws: meta::exception unless has_template_arguments(r) is true.  */

static tree
eval_template_of (location_t loc, const constexpr_ctx *ctx, tree r,
		  bool *non_constant_p, tree *jump_target, tree fun)
{
  if (eval_has_template_arguments (r) != boolean_true_node)
    return throw_exception_notargs (loc, ctx, fun, non_constant_p, jump_target);

  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TYPE_P (r) && typedef_variant_p (r))
    r = TI_TEMPLATE (TYPE_ALIAS_TEMPLATE_INFO (r));
  else if (CLASS_TYPE_P (r) && CLASSTYPE_TEMPLATE_INFO (r))
    r = CLASSTYPE_TI_TEMPLATE (r);
  else if (VAR_OR_FUNCTION_DECL_P (r) && DECL_TEMPLATE_INFO (r))
    r = DECL_TI_TEMPLATE (r);
  else
    gcc_assert (false);

  gcc_assert (TREE_CODE (r) == TEMPLATE_DECL);
  return get_reflection_raw (loc, r);
}

/* Process std::meta::has_parent
   Returns:
   -- If r represents the global namespace, then false.
   -- Otherwise, if r represents an entity that has C language linkage,
      then false.
   -- Otherwise, if r represents an entity that has a language linkage
      other than C++ language linkage, then an implementation-defined value.
   -- Otherwise, if r represents a type that is neither a class nor enumeration
      type, then false.
   -- Otherwise, if r represents an entity or direct base class relationship,
      then true.
   -- Otherwise, false.  */

static tree
eval_has_parent (tree r, reflect_kind kind)
{
  if (kind == REFLECT_OBJECT
      || CONSTANT_CLASS_P (r)
      || r == global_namespace
      || kind == REFLECT_DATA_MEMBER_SPEC)
    return boolean_false_node;
  if (TYPE_P (r))
    {
      if (TYPE_NAME (r)
	  && DECL_P (TYPE_NAME (r))
	  && DECL_LANGUAGE (TYPE_NAME (r)) == lang_c)
	return boolean_false_node;
      else if (OVERLOAD_TYPE_P (r) || typedef_variant_p (r))
	return boolean_true_node;
      else
	return boolean_false_node;
    }
  r = maybe_get_reflection_fndecl (r);
  if (kind == REFLECT_BASE)
    return boolean_true_node;
  if (!DECL_P (r))
    return boolean_false_node;
  if (TREE_CODE (r) != NAMESPACE_DECL && DECL_LANGUAGE (r) == lang_c)
    return boolean_false_node;
  return boolean_true_node;
}

/* Process std::meta::parent_of.
   Returns:
   -- If r represents a non-static data member that is a direct member of an
      anonymous union, or an unnamed bit-field declared within the
      member-specification of such a union, then a reflection representing the
      innermost enclosing anonymous union.
   -- Otherwise, if r represents an enumerator, then a reflection representing
      the corresponding enumeration type.
   -- Otherwise, if r represents a direct base class relationship (D,B), then
      a reflection representing D.
   -- Otherwise, let E be a class, function, or namespace whose class scope,
      function parameter scope, or namespace scope, respectively, is the
      innermost such scope that either is, or encloses, the target scope of a
      declaration of what is represented by r.
      -- If E is the function call operator of a closure type for a
	 consteval-block-declaration, then parent_of(parent_of(^^E)).
      -- Otherwise, ^^E.  */

static tree
eval_parent_of (location_t loc, const constexpr_ctx *ctx, tree r,
		reflect_kind kind, bool *non_constant_p, tree *jump_target,
		tree fun)
{
  if (eval_has_parent (r, kind) != boolean_true_node)
    return throw_exception (loc, ctx, "reflection does not represent an "
				      "entity with parent",
			    fun, non_constant_p, jump_target);
  tree c;
  r = maybe_get_reflection_fndecl (r);
  if (TYPE_P (r))
    {
      if (TYPE_NAME (r) && DECL_P (TYPE_NAME (r)))
	c = CP_DECL_CONTEXT (TYPE_NAME (r));
      else
	c = CP_TYPE_CONTEXT (r);
    }
  else if (kind == REFLECT_BASE)
    c = direct_base_derived (r);
  else
    c = CP_DECL_CONTEXT (r);
  tree lam;
  while (LAMBDA_FUNCTION_P (c)
	 && (lam = CLASSTYPE_LAMBDA_EXPR (CP_DECL_CONTEXT (c)))
	 && LAMBDA_EXPR_CONSTEVAL_BLOCK_P (lam))
    c = CP_TYPE_CONTEXT (CP_DECL_CONTEXT (c));
  return get_reflection_raw (loc, c);
}

/* Return std::vector<info>.  */

static tree
get_vector_info ()
{
  tree args = make_tree_vec (1);
  TREE_VEC_ELT (args, 0) = meta_info_type_node;
  tree inst = lookup_template_class (get_identifier ("vector"), args,
				     /*in_decl*/NULL_TREE,
				     /*context*/std_node, tf_none);
  inst = complete_type (inst);
  if (inst == error_mark_node || !COMPLETE_TYPE_P (inst))
    {
      error ("couldn%'t look up %qs", "std::vector");
      return NULL_TREE;
    }

  return inst;
}

/* Build std::vector<info>{ ELTS }.  */

static tree
get_vector_of_info_elts (vec<constructor_elt, va_gc> *elts)
{
  tree ctor = build_constructor (init_list_type_node, elts);
  CONSTRUCTOR_IS_DIRECT_INIT (ctor) = true;
  TREE_CONSTANT (ctor) = true;
  TREE_STATIC (ctor) = true;
  tree type = get_vector_info ();
  if (!type)
    return error_mark_node;
  tree r = finish_compound_literal (type, ctor, tf_warning_or_error,
				    fcl_functional);
  if (TREE_CODE (r) == TARGET_EXPR)
    r = TARGET_EXPR_INITIAL (r);
  return r;
}

/* Process std::meta::parameters_of.
   Returns:
   -- If r represents a function F, then a vector containing reflections of
      the parameters of F, in the order in which they appear in a declaration
      of F.
   -- Otherwise, r represents a function type T; a vector containing
      reflections of the types in parameter-type-list of T, in the order in
      which they appear in the parameter-type-list.

   Throws: meta::exception unless r represents a function or a function
   type.  */

static tree
eval_parameters_of (location_t loc, const constexpr_ctx *ctx, tree r,
		    bool *non_constant_p, tree *jump_target, tree fun)
{
  if (eval_is_function (r) != boolean_true_node
      && eval_is_function_type (r) != boolean_true_node)
    return throw_exception_nofn (loc, ctx, fun, non_constant_p, jump_target);

  r = maybe_get_reflection_fndecl (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  tree args = (TREE_CODE (r) == FUNCTION_DECL
	       ? FUNCTION_FIRST_USER_PARM (r)
	       : TYPE_ARG_TYPES (r));
  for (tree arg = args; arg && arg != void_list_node; arg = TREE_CHAIN (arg))
    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
			    get_reflection_raw (loc, arg, REFLECT_PARM));
  return get_vector_of_info_elts (elts);
}

/* Process std::meta::variable_of.
   Returns: The reflection of the parameter variable corresponding to r.

   Throws: meta::exception unless
   -- r represents a parameter of a function F and
   -- there is a point P in the evaluation context for which the innermost
      non-block scope enclosing P is the function parameter scope associated
      with F.  */

static tree
eval_variable_of (location_t loc, const constexpr_ctx *ctx, tree r,
		  reflect_kind kind, bool *non_constant_p, tree *jump_target,
		  tree fun)
{
  if (eval_is_function_parameter (r, kind) == boolean_false_node
      /* This doesn't consider the points corresponding to injected
	 declarations, but that doesn't seem needed.  */
      || DECL_CONTEXT (r) != current_function_decl)
    return throw_exception (loc, ctx, "reflection does not represent "
				      "parameter of current function",
			    fun, non_constant_p, jump_target);
  r = maybe_update_function_parm (r);
  return get_reflection_raw (loc, r, REFLECT_UNDEF);
}

/* Process std::meta::return_type_of.
   Returns: The reflection of the return type of the function or function type
   represented by r.

   Throws: meta::exception unless either r represents a function and
   has-type(r) is true or r represents a function type.  */

static tree
eval_return_type_of (location_t loc, const constexpr_ctx *ctx, tree r,
		     reflect_kind kind, bool *non_constant_p, tree *jump_target,
		     tree fun)
{
  if ((eval_is_function (r) != boolean_true_node || !has_type (r, kind))
      && eval_is_function_type (r) != boolean_true_node)
    return throw_exception (loc, ctx, "reflection does not represent a "
			    "function or function type with a return type",
			    fun, non_constant_p, jump_target);

  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (TREE_CODE (r) == FUNCTION_DECL)
    r = TREE_TYPE (r);
  r = TREE_TYPE (r);
  return get_reflection_raw (loc, r, REFLECT_UNDEF);
}

/* Process std::meta::offset_of.
   Let V be the offset in bits from the beginning of a complete object of the
   type represented by parent_of(r) to the subobject associated with the
   entity represented by r.
   Returns: {V / CHAR_BIT, V % CHAR_BIT}.
   Throws: meta::exception unless r represents a non-static data member,
   unnamed bit-field, or direct base class relationship (D,B) for which either
   B is not a virtual base class or D is not an abstract class.  */

static tree
eval_offset_of (location_t loc, const constexpr_ctx *ctx, tree r,
		reflect_kind kind, tree member_offset, bool *non_constant_p,
		tree *jump_target, tree fun)
{
  tree byte_off = NULL_TREE, bit_off = NULL_TREE;
  if (kind == REFLECT_BASE)
    {
      tree d = direct_base_derived (r);
      if (BINFO_VIRTUAL_P (r) && ABSTRACT_CLASS_TYPE_P (d))
	return throw_exception (loc, ctx,
				"reflection of virtual direct base "
				"relationship with abstract derived "
				"class", fun, non_constant_p, jump_target);
      byte_off = BINFO_OFFSET (r);
    }
  else if (TREE_CODE (r) != FIELD_DECL)
    return throw_exception (loc, ctx, "reflection unsuitable for offset_of",
			    fun, non_constant_p, jump_target);
  else
    bit_off = bit_position (r);
  if (TREE_CODE (bit_off ? bit_off : byte_off) != INTEGER_CST)
    return throw_exception (loc, ctx, "non-constant offset for offset_of",
			    fun, non_constant_p, jump_target);
  if (TREE_CODE (member_offset) != RECORD_TYPE)
    {
    fail:
      error_at (loc, "unexpected return type of %qs", "std::meta::offset_of");
      return build_zero_cst (member_offset);
    }
  tree bytes = next_aggregate_field (TYPE_FIELDS (member_offset));
  if (!bytes || !INTEGRAL_TYPE_P (TREE_TYPE (bytes)))
    goto fail;
  tree bits = next_aggregate_field (DECL_CHAIN (bytes));
  if (!bits || !INTEGRAL_TYPE_P (TREE_TYPE (bits)))
    goto fail;
  if (next_aggregate_field (DECL_CHAIN (bits)))
    goto fail;
  tree bytesv;
  if (byte_off)
    bytesv = byte_off;
  else
    bytesv = size_binop (TRUNC_DIV_EXPR, bit_off, bitsize_unit_node);
  bytesv = fold_convert (TREE_TYPE (bytes), bytesv);
  tree bitsv;
  if (byte_off)
    bitsv = build_zero_cst (TREE_TYPE (bits));
  else
    {
      bitsv = size_binop (TRUNC_MOD_EXPR, bit_off, bitsize_unit_node);
      bitsv = fold_convert (TREE_TYPE (bits), bitsv);
    }
  vec<constructor_elt, va_gc> *elts = nullptr;
  CONSTRUCTOR_APPEND_ELT (elts, bytes, bytesv);
  CONSTRUCTOR_APPEND_ELT (elts, bits, bitsv);
  return build_constructor (member_offset, elts);
}

/* Process std::meta::size_of.
   Returns: If r represents
     -- a non-static data member of type T,
     -- a data member description (T,N,A,W,NUA), or
     -- dealias(r) represents a type T,
   then sizeof(T) if T is not a reference type and size_of(add_pointer(^^T))
   otherwise.  Otherwise, size_of(type_of(r)).

   Throws: meta::exception unless all of the following conditions are met:
     -- dealias(r) is a reflection of a type, object, value, variable of
	non-reference type, non-static data member that is not a bit-field,
	direct base class relationship, or data member description
	(T,N,A,W,NUA) where W is not _|_.
     -- If dealias(r) represents a type, then is_complete_type(r) is true.  */

static tree
eval_size_of (location_t loc, const constexpr_ctx *ctx, tree r,
	      reflect_kind kind, tree ret_type, bool *non_constant_p,
	      tree *jump_target, tree fun)
{
  if (eval_is_type (r) != boolean_true_node
      && eval_is_object (kind) != boolean_true_node
      && eval_is_value (kind) != boolean_true_node
      && (eval_is_variable (r, kind) != boolean_true_node
	  || TYPE_REF_P (TREE_TYPE (r)))
      && (TREE_CODE (r) != FIELD_DECL || DECL_C_BIT_FIELD (r))
      && kind != REFLECT_BASE
      && (kind != REFLECT_DATA_MEMBER_SPEC || TREE_VEC_ELT (r, 3)))
    return throw_exception (loc, ctx, "reflection not suitable for size_of",
			    fun, non_constant_p, jump_target);
  if (!INTEGRAL_TYPE_P (ret_type))
    {
      error_at (loc, "unexpected return type of %qs", "std::meta::size_of");
      return build_zero_cst (ret_type);
    }
  tree type;
  if (TYPE_P (r))
    type = r;
  else
    type = type_of (r, kind);
  tree ret;
  if (!complete_type_or_maybe_complain (type, NULL_TREE, tf_none)
      /* No special casing of references needed, c_sizeof_or_alignof_type
	 returns the same size for POINTER_TYPE and REFERENCE_TYPE.  */
      || ((ret = c_sizeof_or_alignof_type (loc, type, true, false, 0))
	  == error_mark_node))
    return throw_exception (loc, ctx,
			    "reflection with incomplete type in size_of",
			    fun, non_constant_p, jump_target);
  return fold_convert (ret_type, ret);
}

/* Process std::meta::alignment_of.
   Returns:
   -- If dealias(r) represents a type T, then alignment_of(add_pointer(r)) if
      T is a reference type and the alignment requirement of T otherwise.
   -- Otherwise, if dealias(r) represents a variable or object, then the
      alignment requirement of the variable or object.
   -- Otherwise, if r represents a direct base class relationship, then
      alignment_of(type_of(r)).
   -- Otherwise, if r represents a non-static data member M of a class C,
      then the alignment of the direct member subobject corresponding to M of a
      complete object of type C.
   -- Otherwise, r represents a data member description (T,N,A,W,NUA).
      If A is not _|_, then the value A.  Otherwise, alignment_of(^^T).
   Throws: meta::exception unless all of the following conditions are met:
   -- dealias(r) is a reflection of a type, object, variable of non-reference
      type, non-static data member that is not a bit-field, direct base class
      relationship, or data member description (T,N,A,W,NUA) where W is _|_.
   -- If dealias(r) represents a type, then is_complete_type(r) is true.  */

static tree
eval_alignment_of (location_t loc, const constexpr_ctx *ctx, tree r,
		   reflect_kind kind, tree ret_type, bool *non_constant_p,
		   tree *jump_target, tree fun)
{
  if (eval_is_type (r) != boolean_true_node
      && eval_is_object (kind) != boolean_true_node
      && (eval_is_variable (r, kind) != boolean_true_node
	  || TYPE_REF_P (TREE_TYPE (r)))
      && (TREE_CODE (r) != FIELD_DECL || DECL_C_BIT_FIELD (r))
      && kind != REFLECT_BASE
      && (kind != REFLECT_DATA_MEMBER_SPEC || TREE_VEC_ELT (r, 3)))
    return throw_exception (loc, ctx, "reflection not suitable for alignment_of",
			    fun, non_constant_p, jump_target);
  if (!INTEGRAL_TYPE_P (ret_type))
    {
      error_at (loc, "unexpected return type of %qs", "std::meta::alignment_of");
      return build_zero_cst (ret_type);
    }
  tree type;
  if (kind == REFLECT_DATA_MEMBER_SPEC)
    {
      if (TREE_VEC_ELT (r, 2))
	return fold_convert (ret_type, TREE_VEC_ELT (r, 2));
      else
	type = TREE_VEC_ELT (r, 0);
    }
  else if (kind == REFLECT_BASE)
    type = BINFO_TYPE (r);
  else if (TREE_CODE (r) == FIELD_DECL
	   || eval_is_variable (r, kind) == boolean_true_node
	   || (eval_is_object (kind) == boolean_true_node
	       && ((DECL_P (r) && TREE_CODE (r) != FUNCTION_DECL)
		   || TREE_CODE (r) == COMPONENT_REF)))
    {
      if (TREE_CODE (r) == COMPONENT_REF)
	r = TREE_OPERAND (r, 1);
      return build_int_cst (ret_type, MAX (DECL_ALIGN_UNIT (r), 1));
    }
  else if (TYPE_P (r))
    type = r;
  else if (eval_is_object (kind) == boolean_true_node)
    type = TREE_TYPE (r);
  else
    gcc_unreachable ();
  if (TYPE_REF_P (type))
    type = ptr_type_node;
  if (FUNC_OR_METHOD_TYPE_P (type))
    return throw_exception (loc, ctx, "alignment_of on function type",
			    fun, non_constant_p, jump_target);
  tree ret;
  if (!complete_type_or_maybe_complain (type, NULL_TREE, tf_none)
      /* No special casing of references needed, c_sizeof_or_alignof_type
	 returns the same alignment for POINTER_TYPE and REFERENCE_TYPE.  */
      || ((ret = c_sizeof_or_alignof_type (loc, type, false, true, 0))
	  == error_mark_node))
    return throw_exception (loc, ctx,
			    "reflection with incomplete type in alignment_of",
			    fun, non_constant_p, jump_target);
  return fold_convert (ret_type, ret);
}

/* Process std::meta::bit_size_of.
   Returns:
     -- If r represents an unnamed bit-field or a non-static data member that
	is a bit-field with width W, then W.
     -- Otherwise, if r represents a data member description (T,N,A,W,NUA)
	and W is not _|_, then W.
     -- Otherwise, CHAR_BIT * size_of(r).

   Throws: meta::exception unless all of the following conditions are met:

     -- dealias(r) is a reflection of a type, object, value, variable of
	non-reference type, non-static data member, unnamed bit-field, direct
	base class relationship, or data member description.
     -- If dealias(r) represents a type T, there is a point within the
	evaluation context from which T is not incomplete.  */

static tree
eval_bit_size_of (location_t loc, const constexpr_ctx *ctx, tree r,
		  reflect_kind kind, tree ret_type, bool *non_constant_p,
		  tree *jump_target, tree fun)
{
  if (eval_is_type (r) != boolean_true_node
      && eval_is_object (kind) != boolean_true_node
      && eval_is_value (kind) != boolean_true_node
      && (eval_is_variable (r, kind) != boolean_true_node
	  || TYPE_REF_P (TREE_TYPE (r)))
      && TREE_CODE (r) != FIELD_DECL
      && kind != REFLECT_BASE
      && kind != REFLECT_DATA_MEMBER_SPEC)
    return throw_exception (loc, ctx,
			    "reflection not suitable for bit_size_of",
			    fun, non_constant_p, jump_target);
  if (!INTEGRAL_TYPE_P (ret_type))
    {
      error_at (loc, "unexpected return type of %qs",
		"std::meta::bit_size_of");
      return build_zero_cst (ret_type);
    }
  tree type;
  if (TREE_CODE (r) == FIELD_DECL && DECL_C_BIT_FIELD (r))
    return fold_convert (ret_type, DECL_SIZE (r));
  else if (TYPE_P (r))
    type = r;
  else if (kind == REFLECT_DATA_MEMBER_SPEC && TREE_VEC_ELT (r, 3))
    return fold_convert (ret_type, TREE_VEC_ELT (r, 3));
  else
    type = type_of (r, kind);
  tree ret;
  if (!complete_type_or_maybe_complain (type, NULL_TREE, tf_none)
      /* No special casing of references needed, c_sizeof_or_alignof_type
	 returns the same size for POINTER_TYPE and REFERENCE_TYPE.  */
      || ((ret = c_sizeof_or_alignof_type (loc, type, true, false, 0))
	  == error_mark_node))
    return throw_exception (loc, ctx,
			    "reflection with incomplete type in bit_size_of",
			    fun, non_constant_p, jump_target);
  ret = size_binop (MULT_EXPR, ret, size_int (BITS_PER_UNIT));
  return fold_convert (ret_type, ret);
}

/* Process std::meta::has_identifier.
   Returns:
   -- If r represents an entity that has a typedef name for linkage purposes,
      then true.
   -- Otherwise, if r represents an unnamed entity, then false.
   -- Otherwise, if r represents a type alias, then !has_template_arguments(r).
   -- Otherwise, if r represents a type, then true if
      -- r represents a cv-unqualified class type and has_template_arguments(r)
	 is false, or
      -- r represents a cv-unqualified enumeration type.
      Otherwise, false.
   -- Otherwise, if r represents a class type, then !has_template_arguments(r).
   -- Otherwise, if r represents a function, then true if
      has_template_arguments(r) is false and the function is not a constructor,
      destructor, operator function, or conversion function.  Otherwise, false.
   -- Otherwise, if r represents a template, then true if r does not represent
      a constructor template, operator function template, or conversion
      function template.  Otherwise, false.
   -- Otherwise, if r represents the ith parameter of a function F that is an
      (implicit or explicit) specialization of a templated function T and the
      ith parameter of the instantiated declaration of T whose template
      arguments are those of F would be instantiated from a pack, then false.
   -- Otherwise, if r represents the parameter P of a function F, then let S
      be the set of declarations, ignoring any explicit instantiations, that
      precede some point in the evaluation context and that declare either F
      or a templated function of which F is a specialization; true if
      -- there is a declaration D in S that introduces a name N for either P
	 or the parameter corresponding to P in the templated function that
	 D declares and
      -- no declaration in S does so using any name other than N.
      Otherwise, false.
   -- Otherwise, if r represents a variable, then false if the declaration of
      that variable was instantiated from a function parameter pack.
      Otherwise, !has_template_arguments(r).
   -- Otherwise, if r represents a structured binding, then false if the
      declaration of that structured binding was instantiated from a
      structured binding pack.  Otherwise, true.
   -- Otherwise, if r represents an enumerator, non-static-data member,
      namespace, or namespace alias, then true.
   -- Otherwise, if r represents a direct base class relationship, then
      has_identifier(type_of(r)).
   -- Otherwise, r represents a data member description (T,N,A,W,NUA); true if
      N is not _|_.  Otherwise, false.  */

static tree
eval_has_identifier (tree r, reflect_kind kind)
{
  r = maybe_get_reflection_fndecl (r);
  if (kind == REFLECT_BASE)
    {
      r = type_of (r, kind);
      kind = REFLECT_UNDEF;
    }
  if (DECL_P (r)
      && kind != REFLECT_PARM
      && (!DECL_NAME (r) || IDENTIFIER_ANON_P (DECL_NAME (r))))
    return boolean_false_node;
  if (TYPE_P (r) && (!TYPE_NAME (r)
		     || (TYPE_ANON_P (r) && !typedef_variant_p (r))
		     || (DECL_P (TYPE_NAME (r))
			 && !DECL_NAME (TYPE_NAME (r)))))
    return boolean_false_node;
  if (eval_is_type_alias (r) == boolean_true_node
      || (CLASS_TYPE_P (r) && !cv_qualified_p (r)))
    {
      if (eval_has_template_arguments (r) == boolean_true_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  if (TYPE_P (r))
    {
      if (TREE_CODE (r) == ENUMERAL_TYPE && !cv_qualified_p (r))
	return boolean_true_node;
      else
	return boolean_false_node;
    }
  if (eval_is_function (r) == boolean_true_node)
    {
      if (eval_has_template_arguments (r) == boolean_true_node
	  || eval_is_constructor (r) == boolean_true_node
	  || eval_is_destructor (r) == boolean_true_node
	  || eval_is_operator_function (r) == boolean_true_node
	  || eval_is_conversion_function (r) == boolean_true_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  if (eval_is_template (r) == boolean_true_node)
    {
      if (eval_is_constructor_template (r) == boolean_true_node
	  || eval_is_operator_function_template (r) == boolean_true_node
	  || eval_is_conversion_function_template (r) == boolean_true_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  if (eval_is_function_parameter (r, kind) == boolean_true_node)
    {
      r = maybe_update_function_parm (r);
      if (MULTIPLE_NAMES_PARM_P (r))
	return boolean_false_node;
      if (DECL_NAME (r))
	{
	  if (strchr (IDENTIFIER_POINTER (DECL_NAME (r)), '#'))
	    return boolean_false_node;
	  else
	    return boolean_true_node;
	}
      if (lookup_attribute ("old parm name", DECL_ATTRIBUTES (r)))
	return boolean_true_node;
      else
	return boolean_false_node;
    }
  if (eval_is_variable (r, kind) == boolean_true_node)
    {
      if (strchr (IDENTIFIER_POINTER (DECL_NAME (r)), '#'))
	return boolean_false_node;
      if (eval_has_template_arguments (r) == boolean_true_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  if (eval_is_structured_binding (r, kind) == boolean_true_node)
    {
      if (strchr (IDENTIFIER_POINTER (DECL_NAME (r)), '#'))
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  if (eval_is_enumerator (r) == boolean_true_node
      || TREE_CODE (r) == FIELD_DECL
      || (TREE_CODE (r) == NAMESPACE_DECL && r != global_namespace))
    return boolean_true_node;
  if (kind == REFLECT_DATA_MEMBER_SPEC && TREE_VEC_ELT (r, 1))
    return boolean_true_node;
  return boolean_false_node;
}

/* Process std::meta::{,u8}identifier_of.
   Let E be UTF-8 for u8identifier_of, and otherwise the ordinary literal
   encoding.
   Returns: An NTMBS, encoded with E, determined as follows:
   -- If r represents an entity with a typedef name for linkage purposes,
      then that name.
   -- Otherwise, if r represents a literal operator or literal operator
      template, then the ud-suffix of the operator or operator template.
   -- Otherwise, if r represents the parameter P of a function F, then let S
      be the set of declarations, ignoring any explicit instantiations, that
      precede some point in the evaluation context and that declare either F
      or a templated function of which F is a specialization; the name that
      was introduced by a declaration in S for the parameter corresponding
      to P.
   -- Otherwise, if r represents an entity, then the identifier introduced by
      the declaration of that entity.
   -- Otherwise, if r represents a direct base class relationship, then
      identifier_of(type_of(r)) or u8identifier_of(type_of(r)), respectively.
   -- Otherwise, r represents a data member description (T,N,A,W,NUA);
      a string_view or u8string_view, respectively, containing the identifier
      N.
   Throws: meta::exception unless has_identifier(r) is true and the identifier
   that would be returned (see above) is representable by E.  */

static tree
eval_identifier_of (location_t loc, const constexpr_ctx *ctx, tree r,
		    reflect_kind kind, bool *non_constant_p, tree *jump_target,
		    tree elt_type, tree ret_type, tree fun)
{
  if (eval_has_identifier (r, kind) == boolean_false_node)
    return throw_exception (loc, ctx,
			    "reflection with has_identifier false",
			    fun, non_constant_p, jump_target);
  r = maybe_get_reflection_fndecl (r);
  const char *name = NULL;
  if (kind == REFLECT_BASE)
    {
      r = type_of (r, kind);
      kind = REFLECT_UNDEF;
    }
  if (eval_is_function_parameter (r, kind) == boolean_true_node)
    {
      r = maybe_update_function_parm (r);
      if (DECL_NAME (r))
	name = IDENTIFIER_POINTER (DECL_NAME (r));
      else
	{
	  tree opn = lookup_attribute ("old parm name", DECL_ATTRIBUTES (r));
	  opn = TREE_VALUE (TREE_VALUE (opn));
	  name = IDENTIFIER_POINTER (opn);
	}
    }
  else if (DECL_P (r) && UDLIT_OPER_P (DECL_NAME (r)))
    name = UDLIT_OP_SUFFIX (DECL_NAME (r));
  else if (DECL_P (r))
    name = IDENTIFIER_POINTER (DECL_NAME (r));
  else if (TYPE_P (r))
    {
      if (DECL_P (TYPE_NAME (r)))
	name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (r)));
      else
	name = IDENTIFIER_POINTER (TYPE_NAME (r));
    }
  else if (kind == REFLECT_DATA_MEMBER_SPEC)
    name = IDENTIFIER_POINTER (TREE_VEC_ELT (r, 1));
  else
    gcc_unreachable ();
  tree str = get_string_literal (name, elt_type);
  if (str == NULL_TREE)
    {
      if (elt_type == char_type_node)
	return throw_exception (loc, ctx, "identifier_of not representable"
					  " in ordinary literal encoding",
				fun, non_constant_p, jump_target);
      else
	return throw_exception (loc, ctx, "u8identifier_of not representable"
					  " in UTF-8",
				fun, non_constant_p, jump_target);
    }
  releasing_vec args (make_tree_vector_single (str));
  tree ret = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&args, ret_type, LOOKUP_NORMAL,
					tf_warning_or_error);
  return build_cplus_new (ret_type, ret, tf_warning_or_error);
}

/* Process std::meta::{,u8}display_string_of.
   Returns: An implementation-defined string_view or u8string_view,
   respectively.
   Recommended practice: Where possible, implementations should return a
   string suitable for identifying the represented construct.  */

static tree
eval_display_string_of (location_t loc, const constexpr_ctx *ctx, tree r,
			reflect_kind kind, bool *non_constant_p,
			tree *jump_target, tree elt_type, tree ret_type,
			tree fun)
{
#if __GNUC__ >= 10
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat"
#pragma GCC diagnostic ignored "-Wformat-diag"
#endif
  r = maybe_get_reflection_fndecl (r);
  pretty_printer pp, *refpp = global_dc->get_reference_printer ();
  pp_format_decoder (&pp) = pp_format_decoder (refpp);
  pp.set_format_postprocessor (pp_format_postprocessor (refpp)->clone ());
  if (r == unknown_type_node)
    pp_printf (&pp, "<null reflection>");
  else if (TYPE_P (r))
    pp_printf (&pp, "%T", r);
  else if (kind == REFLECT_PARM)
    {
      r = maybe_update_function_parm (r);
      tree fn = DECL_CONTEXT (r);
      if (DECL_NAME (r))
	pp_printf (&pp, "<parameter %D of %D>", r, fn);
      else
	{
	  int idx = 1;
	  for (tree args = FUNCTION_FIRST_USER_PARM (fn);
	       r != args; args = DECL_CHAIN (args))
	    ++idx;
	  pp_printf (&pp, "<unnamed parameter %d of %D>", idx, fn);
	}
    }
  else if (kind == REFLECT_VALUE || kind == REFLECT_OBJECT)
    pp_printf (&pp, "%E", r);
  else if (DECL_P (r) && (DECL_NAME (r) || TREE_CODE (r) == NAMESPACE_DECL))
    pp_printf (&pp, "%D", r);
  else if (TREE_CODE (r) == FIELD_DECL)
    pp_printf (&pp, "%T::<unnamed bit-field>", DECL_CONTEXT (r));
  else if (kind == REFLECT_BASE)
    {
      tree d = direct_base_derived (r);
      pp_printf (&pp, "%T: %T", d, BINFO_TYPE (r));
    }
  else if (kind == REFLECT_DATA_MEMBER_SPEC)
    pp_printf (&pp, "(%T, %E, %E, %E, %s)", TREE_VEC_ELT (r, 0),
	       TREE_VEC_ELT (r, 1), TREE_VEC_ELT (r, 2), TREE_VEC_ELT (r, 3),
	       TREE_VEC_ELT (r, 4) == boolean_true_node
	       ? "true" : "false");
  else if (eval_is_annotation (r, kind) == boolean_true_node)
    pp_printf (&pp, "[[=%E]]",
	       tree_strip_any_location_wrapper (TREE_VALUE (TREE_VALUE (r))));
  else
    pp_string (&pp, "<unsupported reflection>");
#if __GNUC__ >= 10
#pragma GCC diagnostic pop
#endif
  tree str = get_string_literal (pp_formatted_text (&pp), elt_type);
  if (str == NULL_TREE)
    {
      if (elt_type == char_type_node)
	return throw_exception (loc, ctx, "identifier_of not representable"
					  " in ordinary literal encoding",
				fun, non_constant_p, jump_target);
      else
	return throw_exception (loc, ctx, "u8identifier_of not representable"
					  " in UTF-8", fun, non_constant_p,
					  jump_target);
    }
  releasing_vec args (make_tree_vector_single (str));
  tree ret = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&args, ret_type, LOOKUP_NORMAL,
					tf_warning_or_error);
  return build_cplus_new (ret_type, ret, tf_warning_or_error);
}

/* Determine the reflection kind for R.  */

static reflect_kind
get_reflection_kind (tree r)
{
  if (eval_is_type (r) == boolean_true_node
      || eval_is_template (r) == boolean_true_node
      || eval_is_function (r) == boolean_true_node)
    return REFLECT_UNDEF;
  return obvalue_p (r) ? REFLECT_OBJECT : REFLECT_VALUE;
}

/* Get the reflection of template argument ARG as per
   std::meta::template_arguments_of.  */

static tree
get_reflection_of_targ (tree arg)
{
  const location_t loc = location_of (arg);
  /* canonicalize_type_argument already strip_typedefs.  */
  arg = STRIP_REFERENCE_REF (arg);
  arg = maybe_get_reference_referent (arg);
  return get_reflection_raw (loc, arg, get_reflection_kind (arg));
}

/* Process std::meta::template_arguments_of.
   Returns: A vector containing reflections of the template arguments of the
   template specialization represented by r, in the order in which they appear
   in the corresponding template argument list.
   For a given template argument A, its corresponding reflection R is
   determined as follows:

   -- If A denotes a type or type alias, then R is a reflection representing
      the underlying entity of A.
   -- Otherwise, if A denotes a class template, variable template, concept,
      or alias template, then R is a reflection representing A.
   -- Otherwise, A is a constant template argument.  Let P be the
      corresponding template parameter.
      -- If P has reference type, then R is a reflection representing the
	 object or function referred to by A.
      -- Otherwise, if P has class type, then R represents the corresponding
	 template parameter object.
      -- Otherwise, R is a reflection representing the value of A.

   Throws: meta::exception unless has_template_arguments(r) is true.  */

static tree
eval_template_arguments_of (location_t loc, const constexpr_ctx *ctx, tree r,
			    bool *non_constant_p, tree *jump_target, tree fun)
{
  if (eval_has_template_arguments (r) != boolean_true_node)
    return throw_exception_notargs (loc, ctx, fun, non_constant_p, jump_target);

  vec<constructor_elt, va_gc> *elts = nullptr;
  tree args = NULL_TREE;
  if (TYPE_P (r) && typedef_variant_p (r))
    {
      if (tree tinfo = TYPE_ALIAS_TEMPLATE_INFO (r))
	args = INNERMOST_TEMPLATE_ARGS (TI_ARGS (tinfo));
    }
  else
    args = get_template_innermost_arguments (r);
  gcc_assert (args);
  for (tree arg : tree_vec_range (args))
    {
      if (ARGUMENT_PACK_P (arg))
	{
	  tree pargs = ARGUMENT_PACK_ARGS (arg);
	  for (tree a : tree_vec_range (pargs))
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_of_targ (a));
	}
      else
	CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, get_reflection_of_targ (arg));
    }
  return get_vector_of_info_elts (elts);
}

/* Helper for eval_remove_const to build non-const type.  */

static tree
remove_const (tree type)
{
  return cp_build_qualified_type (type,
				  cp_type_quals (type) & ~TYPE_QUAL_CONST);
}

/* Process std::meta::annotations_of and annotations_of_with_type.
   Let E be
   -- the corresponding base-specifier if item represents a direct base class
      relationship,
   -- otherwise, the entity represented by item.
   Returns: A vector containing all of the reflections R representing each
   annotation applying to each declaration of E that precedes either some
   point in the evaluation context or a point immediately following the
   class-specifier of the outermost class for which such a point is in a
   complete-class context.
   For any two reflections R1 and R2 in the returned vector, if the annotation
   represented by R1 precedes the annotation represented by R2, then R1
   appears before R2.
   If R1 and R2 represent annotations from the same translation unit T, any
   element in the returned vector between R1 and R2 represents an annotation
   from T.

   Throws: meta::exception unless item represents a type, type alias,
   variable, function, namespace, enumerator, direct base class relationship,
   or non-static data member.  */

static tree
eval_annotations_of (location_t loc, const constexpr_ctx *ctx, tree r,
		     reflect_kind kind, tree type, bool *non_constant_p,
		     tree *jump_target, tree fun)
{
  if (!(eval_is_type (r) == boolean_true_node
	|| eval_is_type_alias (r) == boolean_true_node
	|| eval_is_variable (r, kind) == boolean_true_node
	|| eval_is_function (r) == boolean_true_node
	|| eval_is_namespace (r) == boolean_true_node
	|| eval_is_enumerator (r) == boolean_true_node
	|| eval_is_base (r, kind) == boolean_true_node
	|| eval_is_nonstatic_data_member (r) == boolean_true_node))
    return throw_exception (loc, ctx,
			    "reflection does not represent a type,"
			    " type alias, variable, function, namespace,"
			    " enumerator, direct base class relationship,"
			    " or non-static data member",
			    fun, non_constant_p, jump_target);

  if (type)
    {
      type = maybe_strip_typedefs (type);
      if (!TYPE_P (type)
	  || !complete_type_or_maybe_complain (type, NULL_TREE, tf_none))
	return throw_exception (loc, ctx,
				"reflection does not represent a complete"
				" type or type alias", fun, non_constant_p,
				jump_target);
      type = remove_const (type);
    }

  if (kind == REFLECT_BASE)
    {
      gcc_assert (TREE_CODE (r) == TREE_BINFO);
      tree c = direct_base_derived_binfo (r), binfo = r, base_binfo;

      r = NULL_TREE;
      for (unsigned ix = 0; BINFO_BASE_ITERATE (c, ix, base_binfo); ix++)
	if (base_binfo == binfo)
	  {
	    if (ix + BINFO_BASE_BINFOS (c)->length ()
		< vec_safe_length (BINFO_BASE_ACCESSES (c)))
	      r = BINFO_BASE_ACCESS (c, ix + BINFO_BASE_BINFOS (c)->length ());
	    break;
	  }
    }
  else if (TYPE_P (r))
    r = TYPE_ATTRIBUTES (r);
  else if (DECL_P (r))
    r = DECL_ATTRIBUTES (r);
  else
    gcc_unreachable ();
  vec<constructor_elt, va_gc> *elts = nullptr;
  for (tree a = r; (a = lookup_attribute ("internal ", "annotation ", a));
       a = TREE_CHAIN (a))
    {
      gcc_checking_assert (TREE_CODE (TREE_VALUE (a)) == TREE_LIST);
      tree val = TREE_VALUE (TREE_VALUE (a));
      if (type)
	{
	  tree at = TREE_TYPE (val);
	  if (at != type && !same_type_p (remove_const (at), type))
	    continue;
	}
      CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
			      get_reflection_raw (loc, a, REFLECT_ANNOTATION));
    }
  if (elts)
    {
      /* Reverse the order.  */
      unsigned l = elts->length ();
      constructor_elt *ptr = elts->address ();

      for (unsigned i = 0; i < l / 2; i++)
	std::swap (ptr[i], ptr[l - i - 1]);
    }
  return get_vector_of_info_elts (elts);
}

/* Process std::meta::reflect_constant.
   Mandates: is_copy_constructible_v<T> is true and T is a cv-unqualified
   structural type that is not a reference type.
   Let V be:
   -- if T is a class type, then an object that is template-argument-equivalent
      to the value of expr;
   -- otherwise, the value of expr.
   Returns: template_arguments_of(^^TCls<V>)[0], with TCls as defined below.
   Throws: meta::exception unless the template-id TCls<V> would be valid given
   the invented template
     template<T P> struct TCls;  */

static tree
eval_reflect_constant (location_t loc, const constexpr_ctx *ctx, tree type,
		       tree expr, bool *non_constant_p, tree *jump_target,
		       tree fun)
{
  if (!structural_type_p (type)
      || CP_TYPE_VOLATILE_P (type)
      || CP_TYPE_CONST_P (type)
      || TYPE_REF_P (type))
    {
      error_at (loc, "%qT must be a cv-unqualified structural type that is "
		"not a reference type", type);
      return error_mark_node;
    }
  expr = convert_reflect_constant_arg (type, convert_from_reference (expr));
  if (expr == error_mark_node)
    return throw_exception (loc, ctx, "reflect_constant failed", fun,
			    non_constant_p, jump_target);
  return get_reflection_raw (loc, expr, get_reflection_kind (expr));
}

/* Process std::meta::reflect_object.
   Mandates: T is an object type.
   Returns: A reflection of the object designated by expr.
   Throws: meta::exception unless expr is suitable for use as a constant
   template argument for a constant template parameter of type T&.  */

static tree
eval_reflect_object (location_t loc, const constexpr_ctx *ctx, tree type,
		     tree expr, bool *non_constant_p, tree *jump_target,
		     tree fun)
{
  if (eval_is_object_type (loc, type) != boolean_true_node)
    {
      error_at (loc, "%qT must be an object type", TREE_TYPE (type));
      return error_mark_node;
    }
  type = cp_build_reference_type (type, /*rval=*/false);
  tree e = convert_reflect_constant_arg (type, convert_from_reference (expr));
  if (e == error_mark_node)
    return throw_exception (loc, ctx, "reflect_object failed", fun,
			    non_constant_p, jump_target);
  /* We got (const T &) &foo.  Get the referent, since we want the object
     designated by EXPR.  */
  expr = maybe_get_reference_referent (expr);
  return get_reflection_raw (loc, expr, REFLECT_OBJECT);
}

/* Process std::meta::reflect_function.
   Mandates: T is a function type.
   Returns: A reflection of the function designated by fn.
   Throws: meta::exception unless fn is suitable for use as a constant
   template argument for a constant template parameter of type T&.  */

static tree
eval_reflect_function (location_t loc, const constexpr_ctx *ctx, tree type,
		       tree expr, bool *non_constant_p, tree *jump_target,
		       tree fun)
{
  if (eval_is_function_type (type) != boolean_true_node)
    {
      error_at (loc, "%qT must be a function type", TREE_TYPE (type));
      return error_mark_node;
    }
  type = cp_build_reference_type (type, /*rval=*/false);
  tree e = convert_reflect_constant_arg (type, convert_from_reference (expr));
  if (e == error_mark_node)
    return throw_exception (loc, ctx, "reflect_function failed", fun,
			    non_constant_p, jump_target);
  /* We got (void (&<Ta885>) (void)) fn.  Get the function.  */
  expr = maybe_get_reference_referent (expr);
  return get_reflection_raw (loc, expr);
}

/* Reflection type traits [meta.reflection.traits].

   Every function and function template declared in this subclause throws
   an exception of type meta::exception unless the following conditions are
   met:
   -- For every parameter p of type info, is_type(p) is true.
   -- For every parameter r whose type is constrained on reflection_range,
      ranges::all_of(r, is_type) is true.  */

/* Evaluate reflection type traits for which we have corresponding built-in
   traits.  KIND says which trait we are interested in; TYPE1 and TYPE2 are
   arguments to the trait.  */

static tree
eval_type_trait (location_t loc, tree type1, tree type2, cp_trait_kind kind)
{
  tree r = finish_trait_expr (loc, kind, type1, type2);
  STRIP_ANY_LOCATION_WRAPPER (r);
  return r;
}

/* Like above, but for type traits that take only one type.  */

static tree
eval_type_trait (location_t loc, tree type, cp_trait_kind kind)
{
  return eval_type_trait (loc, type, NULL_TREE, kind);
}

/* Process std::meta::is_function_type.  */

static tree
eval_is_function_type (tree type)
{
  if (FUNC_OR_METHOD_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_void_type.  */

static tree
eval_is_void_type (tree type)
{
  if (VOID_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_null_pointer_type.  */

static tree
eval_is_null_pointer_type (tree type)
{
  if (NULLPTR_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_integral_type.  */

static tree
eval_is_integral_type (tree type)
{
  if (CP_INTEGRAL_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_floating_point_type.  */

static tree
eval_is_floating_point_type (tree type)
{
  if (FLOAT_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_array_type.  */

static tree
eval_is_array_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_ARRAY);
}

/* Process std::meta::is_pointer_type.  */

static tree
eval_is_pointer_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_POINTER);
}

/* Process std::meta::is_lvalue_reference_type.  */

static tree
eval_is_lvalue_reference_type (tree type)
{
  if (TYPE_REF_P (type) && !TYPE_REF_IS_RVALUE (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_rvalue_reference_type.  */

static tree
eval_is_rvalue_reference_type (tree type)
{
  if (TYPE_REF_P (type) && TYPE_REF_IS_RVALUE (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_member_object_pointer_type.  */

static tree
eval_is_member_object_pointer_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_MEMBER_OBJECT_POINTER);
}

/* Process std::meta::is_member_function_pointer_type.  */

static tree
eval_is_member_function_pointer_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_MEMBER_FUNCTION_POINTER);
}

/* Process std::meta::is_enum_type.  */

static tree
eval_is_enum_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_ENUM);
}

/* Process std::meta::is_union_type.  */

static tree
eval_is_union_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_UNION);
}

/* Process std::meta::is_class_type.  */

static tree
eval_is_class_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_CLASS);
}

/* Process std::meta::is_reflection_type.  */

static tree
eval_is_reflection_type (tree type)
{
  if (REFLECTION_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_reference_type.  */

static tree
eval_is_reference_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_REFERENCE);
}

/* Process std::meta::is_arithmetic_type.  */

static tree
eval_is_arithmetic_type (tree type)
{
  if (ARITHMETIC_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_object_type.  */

static tree
eval_is_object_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_OBJECT);
}

/* Process std::meta::is_scalar_type.  */

static tree
eval_is_scalar_type (tree type)
{
  if (SCALAR_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_fundamental_type.  */

static tree
eval_is_fundamental_type (tree type)
{
  if (ARITHMETIC_TYPE_P (type)
      || VOID_TYPE_P (type)
      || NULLPTR_TYPE_P (type)
      || REFLECTION_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_compound_type.  */

static tree
eval_is_compound_type (tree type)
{
  if (eval_is_fundamental_type (type) == boolean_false_node)
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_member_pointer_type.  */

static tree
eval_is_member_pointer_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_MEMBER_POINTER);
}

/* Process std::meta::is_const_type.  */

static tree
eval_is_const_type (tree type)
{
  if (CP_TYPE_CONST_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_volatile_type.  */

static tree
eval_is_volatile_type (tree type)
{
  if (CP_TYPE_VOLATILE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_copyable_type.  */

static tree
eval_is_trivially_copyable_type (tree type)
{
  if (trivially_copyable_p (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_standard_layout_type.  */

static tree
eval_is_standard_layout_type (tree type)
{
  if (std_layout_type_p (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_empty_type.  */

static tree
eval_is_empty_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_EMPTY);
}

/* Process std::meta::is_polymorphic_type.  */

static tree
eval_is_polymorphic_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_POLYMORPHIC);
}

/* Process std::meta::is_abstract_type.  */

static tree
eval_is_abstract_type (tree type)
{
  if (ABSTRACT_CLASS_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_final_type.  */

static tree
eval_is_final_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_FINAL);
}

/* Process std::meta::is_final.
   Returns: true if r represents a final class or a final member function.
   Otherwise, false.  */

static tree
eval_is_final (tree r)
{
  if (eval_is_function (r) == boolean_true_node)
    {
      r = maybe_get_reflection_fndecl (r);
      if (TREE_CODE (r) == FUNCTION_DECL && DECL_FINAL_P (r))
	return boolean_true_node;
      else
	return boolean_false_node;
    }

  if (eval_is_type (r) == boolean_true_node
      && CLASS_TYPE_P (r)
      && CLASSTYPE_FINAL (r))
    return boolean_true_node;

  return boolean_false_node;
}

/* Process std::meta::is_aggregate_type.  */

static tree
eval_is_aggregate_type (tree type)
{
  if (CP_AGGREGATE_TYPE_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_consteval_only_type.  */

static tree
eval_is_consteval_only_type (tree type)
{
  if (consteval_only_p (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_signed_type.  */

static tree
eval_is_signed_type (tree type)
{
  if (ARITHMETIC_TYPE_P (type) && !TYPE_UNSIGNED (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_unsigned_type.  */

static tree
eval_is_unsigned_type (tree type)
{
  if (ARITHMETIC_TYPE_P (type) && TYPE_UNSIGNED (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_bounded_array_type.  */

static tree
eval_is_bounded_array_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_BOUNDED_ARRAY);
}

/* Process std::meta::is_unbounded_array_type.  */

static tree
eval_is_unbounded_array_type (tree type)
{
  if (array_of_unknown_bound_p (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_scoped_enum_type.  */

static tree
eval_is_scoped_enum_type (tree type)
{
  if (SCOPED_ENUM_P (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_constructible_type.  */

static tree
eval_is_constructible_type (tree type, tree tvec)
{
  if (is_xible (INIT_EXPR, type, tvec))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_default_constructible_type.  */

static tree
eval_is_default_constructible_type (tree type)
{
  if (is_xible (INIT_EXPR, type, make_tree_vec (0)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_copy_constructible_type.  */

static tree
eval_is_copy_constructible_type (tree type)
{
  tree arg = make_tree_vec (1);
  TREE_VEC_ELT (arg, 0)
    = build_stub_type (type, cp_type_quals (type) | TYPE_QUAL_CONST, false);
  if (is_xible (INIT_EXPR, type, arg))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_move_constructible_type.  */

static tree
eval_is_move_constructible_type (tree type)
{
  tree arg = make_tree_vec (1);
  TREE_VEC_ELT (arg, 0) = cp_build_reference_type (type, /*rval=*/true);
  if (is_xible (INIT_EXPR, type, arg))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_assignable_type.  */

static tree
eval_is_assignable_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_ASSIGNABLE);
}

/* Process std::meta::is_copy_assignable_type.  */

static tree
eval_is_copy_assignable_type (tree type)
{
  tree type1 = cp_build_reference_type (type, /*rval=*/false);
  tree type2 = build_stub_type (type, cp_type_quals (type) | TYPE_QUAL_CONST,
				false);
  if (is_xible (MODIFY_EXPR, type1, type2))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_move_assignable_type.  */

static tree
eval_is_move_assignable_type (tree type)
{
  tree type1 = cp_build_reference_type (type, /*rval=*/false);
  tree type2 = cp_build_reference_type (type, /*rval=*/true);
  if (is_xible (MODIFY_EXPR, type1, type2))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_destructible_type.  */

static tree
eval_is_destructible_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_DESTRUCTIBLE);
}

/* Process std::meta::is_trivially_constructible_type.  */

static tree
eval_is_trivially_constructible_type (tree type, tree tvec)
{
  if (is_trivially_xible (INIT_EXPR, type, tvec))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_default_constructible_type.  */

static tree
eval_is_trivially_default_constructible_type (tree type)
{
  if (is_trivially_xible (INIT_EXPR, type, make_tree_vec (0)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_copy_constructible_type.  */

static tree
eval_is_trivially_copy_constructible_type (tree type)
{
  tree arg = make_tree_vec (1);
  TREE_VEC_ELT (arg, 0)
    = build_stub_type (type, cp_type_quals (type) | TYPE_QUAL_CONST, false);
  if (is_trivially_xible (INIT_EXPR, type, arg))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_move_constructible_type.  */

static tree
eval_is_trivially_move_constructible_type (tree type)
{
  tree arg = make_tree_vec (1);
  TREE_VEC_ELT (arg, 0) = cp_build_reference_type (type, /*rval=*/true);
  if (is_trivially_xible (INIT_EXPR, type, arg))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_assignable_type.  */

static tree
eval_is_trivially_assignable_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_TRIVIALLY_ASSIGNABLE);
}

/* Process std::meta::is_trivially_copy_assignable_type.  */

static tree
eval_is_trivially_copy_assignable_type (tree type)
{
  tree type1 = cp_build_reference_type (type, /*rval=*/false);
  tree type2 = build_stub_type (type, cp_type_quals (type) | TYPE_QUAL_CONST,
				false);
  if (is_trivially_xible (MODIFY_EXPR, type1, type2))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_move_assignable_type.  */

static tree
eval_is_trivially_move_assignable_type (tree type)
{
  tree type1 = cp_build_reference_type (type, /*rval=*/false);
  tree type2 = cp_build_reference_type (type, /*rval=*/true);
  if (is_trivially_xible (MODIFY_EXPR, type1, type2))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_trivially_destructible_type.  */

static tree
eval_is_trivially_destructible_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_TRIVIALLY_DESTRUCTIBLE);
}

/* Process std::meta::is_nothrow_constructible_type.  */

static tree
eval_is_nothrow_constructible_type (tree type, tree tvec)
{
  if (is_nothrow_xible (INIT_EXPR, type, tvec))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nothrow_default_constructible_type.  */

static tree
eval_is_nothrow_default_constructible_type (tree type)
{
  if (is_nothrow_xible (INIT_EXPR, type, make_tree_vec (0)))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nothrow_copy_constructible_type.  */

static tree
eval_is_nothrow_copy_constructible_type (tree type)
{
  tree arg = make_tree_vec (1);
  TREE_VEC_ELT (arg, 0)
    = build_stub_type (type, cp_type_quals (type) | TYPE_QUAL_CONST, false);
  if (is_nothrow_xible (INIT_EXPR, type, arg))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nothrow_move_constructible_type.  */

static tree
eval_is_nothrow_move_constructible_type (tree type)
{
  tree arg = make_tree_vec (1);
  TREE_VEC_ELT (arg, 0) = cp_build_reference_type (type, /*rval=*/true);
  if (is_nothrow_xible (INIT_EXPR, type, arg))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nothrow_assignable_type.  */

static tree
eval_is_nothrow_assignable_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_NOTHROW_ASSIGNABLE);
}

/* Process std::meta::is_nothrow_copy_assignable_type.  */

static tree
eval_is_nothrow_copy_assignable_type (tree type)
{
  tree type1 = cp_build_reference_type (type, /*rval=*/false);
  tree type2 = build_stub_type (type, cp_type_quals (type) | TYPE_QUAL_CONST,
				false);
  if (is_nothrow_xible (MODIFY_EXPR, type1, type2))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nothrow_move_assignable_type.  */

static tree
eval_is_nothrow_move_assignable_type (tree type)
{
  tree type1 = cp_build_reference_type (type, /*rval=*/false);
  tree type2 = cp_build_reference_type (type, /*rval=*/true);
  if (is_nothrow_xible (MODIFY_EXPR, type1, type2))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::is_nothrow_destructible_type.  */

static tree
eval_is_nothrow_destructible_type (location_t loc, tree type)
{
  return eval_type_trait (loc, type, CPTK_IS_NOTHROW_DESTRUCTIBLE);
}

/* Process std::meta::is_implicit_lifetime_type.  */

static tree
eval_is_implicit_lifetime_type (tree type)
{
  if (implicit_lifetime_type_p (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_virtual_destructor.  */

static tree
eval_has_virtual_destructor (tree type)
{
  if (type_has_virtual_destructor (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::has_unique_object_representations.  */

static tree
eval_has_unique_object_representations (tree type)
{
  if (type_has_unique_obj_representations (type))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Process std::meta::reference_constructs_from_temporary.  */

static tree
eval_reference_constructs_from_temporary (location_t loc, tree type1,
					  tree type2)
{
  return eval_type_trait (loc, type1, type2,
			  CPTK_REF_CONSTRUCTS_FROM_TEMPORARY);
}

/* Process std::meta::reference_converts_from_temporary.  */

static tree
eval_reference_converts_from_temporary (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_REF_CONVERTS_FROM_TEMPORARY);
}

/* Process std::meta::rank.  */

static tree
eval_rank (tree type)
{
  size_t rank = 0;
  for (; TREE_CODE (type) == ARRAY_TYPE; type = TREE_TYPE (type))
    ++rank;
  return build_int_cst (size_type_node, rank);
}

/* Process std::meta::extent.  */

static tree
eval_extent (location_t loc, tree type, tree i)
{
  size_t rank = tree_to_uhwi (i);
  while (rank && TREE_CODE (type) == ARRAY_TYPE)
    {
      --rank;
      type = TREE_TYPE (type);
    }
  if (rank
      || TREE_CODE (type) != ARRAY_TYPE
      || eval_is_bounded_array_type (loc, type) == boolean_false_node)
     return size_zero_node;
  return size_binop (PLUS_EXPR, TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
		     size_one_node);
}

/* Process std::meta::is_same_type.  */

static tree
eval_is_same_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_SAME);
}

/* Process std::meta::is_base_of_type.  */

static tree
eval_is_base_of_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_BASE_OF);
}

/* Process std::meta::is_virtual_base_of_type.  */

static tree
eval_is_virtual_base_of_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_VIRTUAL_BASE_OF);
}

/* Process std::meta::is_convertible_type.  */

static tree
eval_is_convertible_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_CONVERTIBLE);
}

/* Process std::meta::is_nothrow_convertible_type.  */

static tree
eval_is_nothrow_convertible_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_NOTHROW_CONVERTIBLE);
}

/* Process std::meta::is_layout_compatible_type.  */

static tree
eval_is_layout_compatible_type (location_t loc, tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2, CPTK_IS_LAYOUT_COMPATIBLE);
}

/* Process std::meta::is_pointer_interconvertible_base_of_type.  */

static tree
eval_is_pointer_interconvertible_base_of_type (location_t loc,
					       tree type1, tree type2)
{
  return eval_type_trait (loc, type1, type2,
			  CPTK_IS_POINTER_INTERCONVERTIBLE_BASE_OF);
}

/* Process std::meta::is_invocable_type.  */

static tree
eval_is_invocable_type (location_t loc, tree type, tree tvec)
{
  tree r = finish_trait_expr (loc, CPTK_IS_INVOCABLE, type, tvec);
  STRIP_ANY_LOCATION_WRAPPER (r);
  return r;
}

/* Helper for various eval_* type trait functions which can't use builtin
   trait and have to instantiate std::NAME<ARGS>::value.  */

static tree
finish_library_value_trait (location_t loc, const constexpr_ctx *ctx,
			    const char *name, tree args, tree call,
			    bool *non_constant_p, tree *jump_target, tree fun)
{
  tree inst = lookup_template_class (get_identifier (name), args,
				     /*in_decl*/NULL_TREE, /*context*/std_node,
				     tf_warning_or_error);
  inst = complete_type (inst);
  if (inst == error_mark_node
      || !COMPLETE_TYPE_P (inst)
      || !CLASS_TYPE_P (inst))
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "couldn%'t instantiate %<std::%s<%T>%>",
		  name, args);
      *non_constant_p = true;
      return call;
    }
  tree val = lookup_qualified_name (inst, value_identifier,
				    LOOK_want::NORMAL, /*complain*/false);
  if (val == error_mark_node)
    return throw_exception (loc, ctx, "value member missing",
			    fun, non_constant_p, jump_target);
  if (VAR_P (val) || TREE_CODE (val) == CONST_DECL)
    val = maybe_constant_value (val, NULL_TREE, mce_true);
  if (TREE_CODE (TREE_TYPE (call)) == BOOLEAN_TYPE)
    {
      if (integer_zerop (val))
	return boolean_false_node;
      else if (integer_nonzerop (val))
	return boolean_true_node;
      else
	return throw_exception (loc, ctx, "unexpected value of value member",
				fun, non_constant_p, jump_target);
    }
  else if (TREE_CODE (val) == INTEGER_CST)
    {
      val = build_converted_constant_expr (TREE_TYPE (call), val, tf_none);
      if (TREE_CODE (val) == INTEGER_CST)
	return val;
    }
  return throw_exception (loc, ctx, "unexpected value of value member",
			  fun, non_constant_p, jump_target);
}

/* Process std::meta::is_{,nothrow_}invocable_r_type.  */

static tree
eval_is_invocable_r_type (location_t loc, const constexpr_ctx *ctx,
			  tree tres, tree type, tree tvec, tree call,
			  bool *non_constant_p, tree *jump_target, tree fun,
			  const char *name)
{
  /* Create std::is_invocable_r<TYPE>::value.  */
  tree args = make_tree_vec (TREE_VEC_LENGTH (tvec) + 2);
  TREE_VEC_ELT (args, 0) = tres;
  TREE_VEC_ELT (args, 1) = type;
  for (int i = 0; i < TREE_VEC_LENGTH (tvec); ++i)
    TREE_VEC_ELT (args, i + 2) = TREE_VEC_ELT (tvec, i);
  return finish_library_value_trait (loc, ctx, name, args, call,
				     non_constant_p, jump_target, fun);
}

/* Process std::meta::is_nothrow_invocable_type.  */

static tree
eval_is_nothrow_invocable_type (location_t loc, tree type, tree tvec)
{
  tree r = finish_trait_expr (loc, CPTK_IS_NOTHROW_INVOCABLE, type, tvec);
  STRIP_ANY_LOCATION_WRAPPER (r);
  return r;
}

/* Process std::meta::is_{,nothrow_}swappable_with_type.  */

static tree
eval_is_swappable_with_type (location_t loc, const constexpr_ctx *ctx,
			     tree type1, tree type2, tree call,
			     bool *non_constant_p, tree *jump_target, tree fun,
			     const char *name)
{
  /* Create std::is_swappable_with<TYPE>::value.  */
  tree args = make_tree_vec (2);
  TREE_VEC_ELT (args, 0) = type1;
  TREE_VEC_ELT (args, 1) = type2;
  return finish_library_value_trait (loc, ctx, name, args, call,
				     non_constant_p, jump_target, fun);
}

/* Process std::meta::is_{,nothrow_}swappable_type.  */

static tree
eval_is_swappable_type (location_t loc, const constexpr_ctx *ctx,
			tree type, tree call, bool *non_constant_p,
			tree *jump_target, tree fun, const char *name)
{
  /* Create std::is_swappable<TYPE>::value.  */
  tree args = make_tree_vec (1);
  TREE_VEC_ELT (args, 0) = type;
  return finish_library_value_trait (loc, ctx, name, args, call,
				     non_constant_p, jump_target, fun);
}

/* Process std::meta::remove_cvref.  */

static tree
eval_remove_cvref (location_t loc, tree type)
{
  if (TYPE_REF_P (type))
    type = TREE_TYPE (type);
  type = finish_trait_type (CPTK_REMOVE_CV, type, NULL_TREE, tf_none);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::decay.  */

static tree
eval_decay (location_t loc, tree type)
{
  type = finish_trait_type (CPTK_DECAY, type, NULL_TREE, tf_none);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Helper for various eval_* type trait functions which can't use builtin
   trait and have to instantiate std::NAME<ARGS>::type.  */

static tree
finish_library_type_trait (location_t loc, const constexpr_ctx *ctx,
			   const char *name, tree args, tree call,
			   bool *non_constant_p, tree *jump_target, tree fun)
{
  tree inst = lookup_template_class (get_identifier (name), args,
				     /*in_decl*/NULL_TREE,
				     /*context*/std_node,
				     tf_warning_or_error);
  if (inst == error_mark_node)
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "couldn%'t instantiate %<std::%s<%T>%>",
		  name, args);
      *non_constant_p = true;
      return call;
    }
  tree type = make_typename_type (inst, type_identifier,
				  none_type, tf_none);
  if (type == error_mark_node)
    return throw_exception (loc, ctx, "type member missing",
			    fun, non_constant_p, jump_target);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::common_{type,reference}.  */

static tree
eval_common_type (location_t loc, const constexpr_ctx *ctx, tree tvec,
		  tree call, bool *non_constant_p, tree *jump_target, tree fun,
		  const char *name)
{
  return finish_library_type_trait (loc, ctx, name, tvec, call,
				    non_constant_p, jump_target, fun);
}

/* Process std::meta::underlying_type.  */

static tree
eval_underlying_type (location_t loc, const constexpr_ctx *ctx, tree type,
		      bool *non_constant_p, tree *jump_target, tree fun)
{
  if (TREE_CODE (type) != ENUMERAL_TYPE || !COMPLETE_TYPE_P (type))
    return throw_exception (loc, ctx, "reflection does not represent "
				      "a complete enumeration type",
			    fun, non_constant_p, jump_target);
  type = finish_underlying_type (type);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::invoke_result.  */

static tree
eval_invoke_result (location_t loc, const constexpr_ctx *ctx, tree type,
		    tree tvec, tree call, bool *non_constant_p,
		    tree *jump_target, tree fun)
{
  tree args = make_tree_vec (TREE_VEC_LENGTH (tvec) + 1);
  TREE_VEC_ELT (args, 0) = type;
  for (int i = 0; i < TREE_VEC_LENGTH (tvec); ++i)
    TREE_VEC_ELT (args, i + 1) = TREE_VEC_ELT (tvec, i);
  return finish_library_type_trait (loc, ctx, "invoke_result", args, call,
				    non_constant_p, jump_target, fun);
}

/* Process std::meta::unwrap_{reference,ref_decay}.  */

static tree
eval_unwrap_reference (location_t loc, const constexpr_ctx *ctx, tree type,
		       tree call, bool *non_constant_p, tree *jump_target,
		       tree fun, const char *name)
{
  tree args = make_tree_vec (1);
  TREE_VEC_ELT (args, 0) = type;
  return finish_library_type_trait (loc, ctx, name, args, call,
				    non_constant_p, jump_target, fun);
}

/* Process std::meta::type_order.  */

static tree
eval_type_order (tree type1, tree type2)
{
  return type_order_value (strip_typedefs (type1), strip_typedefs (type2));
}

/* Process std::meta::enumerators_of.
   Returns: A vector containing the reflections of each enumerator of the
   enumeration represented by dealias(type_enum), in the order in which they
   are declared.
   Throws: meta::exception unless dealias(type_enum) represents an enumeration
   type, and is_enumerable_type(type_enum) is true.  */

static tree
eval_enumerators_of (location_t loc, const constexpr_ctx *ctx, tree r,
		     bool *non_constant_p, tree *jump_target, tree fun)
{
  if (TREE_CODE (r) != ENUMERAL_TYPE
      || eval_is_enumerable_type (r) == boolean_false_node)
    return throw_exception (loc, ctx, "reflection does not represent an "
				      "enumerable enumeration type", fun,
			    non_constant_p, jump_target);
  vec<constructor_elt, va_gc> *elts = nullptr;
  for (tree t = TYPE_VALUES (r); t; t = TREE_CHAIN (t))
    {
      tree e = TREE_VALUE (t);
      CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, get_reflection_raw (loc, e));
    }
  return get_vector_of_info_elts (elts);
}

/* Process std::meta::remove_const.
   Returns: a reflection representing the type denoted by
   std::remove_const_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_const (location_t loc, tree type)
{
  return get_reflection_raw (loc, remove_const (strip_typedefs (type)));
}

/* Process std::meta::remove_volatile.
   Returns: a reflection representing the type denoted by
   std::remove_volatile_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_volatile (location_t loc, tree type)
{
  type = strip_typedefs (type);
  int quals = cp_type_quals (type);
  quals &= ~TYPE_QUAL_VOLATILE;
  type = cp_build_qualified_type (type, quals);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::remove_cv.
   Returns: a reflection representing the type denoted by
   std::remove_cv_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_cv (location_t loc, tree type)
{
  type = strip_typedefs (type);
  type = finish_trait_type (CPTK_REMOVE_CV, type, NULL_TREE, tf_none);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::add_const.
   Returns: a reflection representing the type denoted by
   std::add_const_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_add_const (location_t loc, tree type)
{
  type = strip_typedefs (type);
  if (!TYPE_REF_P (type) && !FUNC_OR_METHOD_TYPE_P (type))
    {
      int quals = cp_type_quals (type);
      quals |= TYPE_QUAL_CONST;
      type = cp_build_qualified_type (type, quals);
    }
  return get_reflection_raw (loc, type);
}

/* Process std::meta::add_volatile.
   Returns: a reflection representing the type denoted by
   std::add_volatile_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_add_volatile (location_t loc, tree type)
{
  type = strip_typedefs (type);
  if (!TYPE_REF_P (type) && !FUNC_OR_METHOD_TYPE_P (type))
    {
      int quals = cp_type_quals (type);
      quals |= TYPE_QUAL_VOLATILE;
      type = cp_build_qualified_type (type, quals);
    }
  return get_reflection_raw (loc, type);
}

/* Process std::meta::add_cv.
   Returns: a reflection representing the type denoted by
   std::add_cv_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_add_cv (location_t loc, tree type)
{
  type = strip_typedefs (type);
  if (!TYPE_REF_P (type) && !FUNC_OR_METHOD_TYPE_P (type))
    {
      int quals = cp_type_quals (type);
      quals |= (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
      type = cp_build_qualified_type (type, quals);
    }
  return get_reflection_raw (loc, type);
}

/* Process std::meta::remove_reference.
   Returns: a reflection representing the type denoted by
   std::remove_reference_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_reference (location_t loc, tree type)
{
  if (TYPE_REF_P (type))
    type = TREE_TYPE (type);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::add_lvalue_reference.
   Returns: a reflection representing the type denoted by
   std::add_lvalue_reference_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_add_lvalue_reference (location_t loc, tree type)
{
  type = strip_typedefs (type);
  type = finish_trait_type (CPTK_ADD_LVALUE_REFERENCE, type, NULL_TREE, tf_none);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::add_rvalue_reference.
   Returns: a reflection representing the type denoted by
   std::add_rvalue_reference_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_add_rvalue_reference (location_t loc, tree type)
{
  type = strip_typedefs (type);
  type = finish_trait_type (CPTK_ADD_RVALUE_REFERENCE, type, NULL_TREE, tf_none);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::make_signed and std::meta::make_unsigned.
   Returns: a reflection representing the type denoted by
   std::make_signed_t<T> or std::make_unsigned_t<T>, respectively, where T is
   the type or type alias represented by type.  */

static tree
eval_make_signed (location_t loc, const constexpr_ctx *ctx, tree type,
		  bool unsignedp, bool *non_constant_p, tree *jump_target,
		  tree fun)
{
  if (!INTEGRAL_TYPE_P (type) || TREE_CODE (type) == BOOLEAN_TYPE)
    return throw_exception (loc, ctx, "reflection represents non-integral "
				      "or bool type", fun, non_constant_p,
				      jump_target);
  tree ret = type;
  if (TREE_CODE (type) == ENUMERAL_TYPE
      || TYPE_MAIN_VARIANT (type) == wchar_type_node
      || TYPE_MAIN_VARIANT (type) == char8_type_node
      || TYPE_MAIN_VARIANT (type) == char16_type_node
      || TYPE_MAIN_VARIANT (type) == char32_type_node)
    {
      tree unit = TYPE_SIZE_UNIT (type);
      tree types[] = {
	signed_char_type_node,
	short_integer_type_node,
	integer_type_node,
	long_integer_type_node,
	long_long_integer_type_node };
      ret = NULL_TREE;
      for (unsigned i = 0; i < ARRAY_SIZE (types); ++i)
	if (tree_int_cst_equal (TYPE_SIZE_UNIT (types[i]), unit))
	  {
	    ret = c_common_signed_or_unsigned_type (unsignedp, types[i]);
	    break;
	  }
      if (!ret)
	ret = c_common_type_for_size (TYPE_PRECISION (type), unsignedp);
    }
  else if (TYPE_MAIN_VARIANT (type) == char_type_node)
    ret = unsignedp ? unsigned_char_type_node : signed_char_type_node;
  else if (unsignedp ^ (!!TYPE_UNSIGNED (type)))
    ret = c_common_signed_or_unsigned_type (unsignedp, type);
  if (ret != type)
    {
      int quals = cp_type_quals (type);
      quals &= (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
      ret = cp_build_qualified_type (ret, quals);
    }
  else
    ret = strip_typedefs (type);
  return get_reflection_raw (loc, ret);
}

/* Process std::meta::remove_extent.
   Returns: a reflection representing the type denoted by
   std::remove_extent_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_extent (location_t loc, tree type)
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::remove_all_extents.
   Returns: a reflection representing the type denoted by
   std::remove_all_extents_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_all_extents (location_t loc, tree type)
{
  type = strip_array_types (type);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::remove_pointer.
   Returns: a reflection representing the type denoted by
   std::remove_pointer_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_remove_pointer (location_t loc, tree type)
{
  if (TYPE_PTR_P (type))
    type = TREE_TYPE (type);
  type = strip_typedefs (type);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::add_pointer.
   Returns: a reflection representing the type denoted by
   std::add_pointer_t<T>, where T is the type or type alias
   represented by type.  */

static tree
eval_add_pointer (location_t loc, tree type)
{
  type = strip_typedefs (type);
  type = finish_trait_type (CPTK_ADD_POINTER, type, NULL_TREE, tf_none);
  return get_reflection_raw (loc, type);
}

/* Process std::meta::is_lvalue_reference_qualified and
   std::meta::is_rvalue_reference_qualified.
   Let T be type_of(r) if has-type(r) is true.  Otherwise, let T be
   dealias(r).
   Returns: true if T represents an lvalue- or rvalue-qualified
   function type, respectively.  Otherwise, false.
   RVALUE_P is true if we're processing is_rvalue_*, false if we're
   processing is_lvalue_*.  */

static tree
eval_is_lrvalue_reference_qualified (tree r, reflect_kind kind,
				     bool rvalue_p)
{
  if (has_type (r, kind))
    r = type_of (r, kind);
  else
    r = maybe_strip_typedefs (r);
  if (FUNC_OR_METHOD_TYPE_P (r)
      && FUNCTION_REF_QUALIFIED (r)
      && rvalue_p == FUNCTION_RVALUE_QUALIFIED (r))
    return boolean_true_node;

  return boolean_false_node;
}

/* Process std::meta::can_substitute.
   Let Z be the template represented by templ and let Args... be a sequence of
   prvalue constant expressions that compute the reflections held by the
   elements of arguments, in order.
   Returns: true if Z<[:Args:]...> is a valid template-id that does not name
   a function whose type contains an undeduced placeholder type.
   Otherwise, false.
   Throws: meta::exception unless templ represents a template, and every
   reflection in arguments represents a construct usable as a template
   argument.  */

static tree
eval_can_substitute (location_t loc, const constexpr_ctx *ctx,
		     tree r, tree rvec, bool *non_constant_p, tree *jump_target,
		     tree fun)
{
  if (eval_is_template (r) != boolean_true_node)
    return throw_exception (loc, ctx,
			    "reflection does not represent a template",
			    fun, non_constant_p, jump_target);
  for (int i = 0; i < TREE_VEC_LENGTH (rvec); ++i)
    {
      tree ra = TREE_VEC_ELT (rvec, i);
      tree a = REFLECT_EXPR_HANDLE (ra);
      reflect_kind kind = REFLECT_EXPR_KIND (ra);
      // TODO: It is unclear on what kinds of reflections we should throw
      // and what kinds of exceptions should merely result in can_substitute
      // returning false.
      if (a == unknown_type_node
	  || kind == REFLECT_PARM
	  || eval_is_namespace (a) == boolean_true_node
	  || eval_is_constructor (a) == boolean_true_node
	  || eval_is_destructor (a) == boolean_true_node
	  || eval_is_annotation (a, kind) == boolean_true_node
	  || (TREE_CODE (a) == FIELD_DECL && !DECL_UNNAMED_BIT_FIELD (a))
	  || kind == REFLECT_DATA_MEMBER_SPEC
	  || kind == REFLECT_BASE
	  || (!TYPE_P (a)
	      && eval_is_template (a) == boolean_false_node
	      && !has_type (a, kind)))
	return throw_exception (loc, ctx,
				"invalid argument to can_substitute",
				fun, non_constant_p, jump_target);
      a = resolve_nondeduced_context (a, tf_warning_or_error);
      TREE_VEC_ELT (rvec, i) = a;
    }
  if (DECL_TYPE_TEMPLATE_P (r) || DECL_TEMPLATE_TEMPLATE_PARM_P (r))
    {
      tree type = lookup_template_class (r, rvec, NULL_TREE, NULL_TREE,
					 tf_none);
      if (type == error_mark_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  else if (concept_definition_p (r))
    {
      tree c = build_concept_check (r, rvec, tf_none);
      if (c == error_mark_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  else if (variable_template_p (r))
    {
      tree var = lookup_template_variable (r, rvec, tf_none);
      if (var == error_mark_node)
	return boolean_false_node;
      var = finish_template_variable (var, tf_none);
      if (var == error_mark_node)
	return boolean_false_node;
      else
	return boolean_true_node;
    }
  else
    {
      tree fn = lookup_template_function (r, rvec);
      if (fn == error_mark_node)
	return boolean_false_node;
      fn = resolve_nondeduced_context_or_error (fn, tf_none);
      if (fn == error_mark_node)
	return boolean_false_node;
      return boolean_true_node;
    }
}

/* Process std::meta::substitute.
   Let Z be the template represented by templ and let Args... be a sequence of
   prvalue constant expressions that compute the reflections held by the
   elements of arguments, in order.
   Returns: ^^Z<[:Args:]...>.
   Throws: meta::exception unless can_substitute(templ, arguments) is true.  */

static tree
eval_substitute (location_t loc, const constexpr_ctx *ctx,
		 tree r, tree rvec, bool *non_constant_p, tree *jump_target,
		 tree fun)
{
  tree cs = eval_can_substitute (loc, ctx, r, rvec, non_constant_p, jump_target,
				 fun);
  if (*jump_target)
    return cs;
  if (cs == boolean_false_node)
    return throw_exception (loc, ctx, "can_substitute returned false",
			    fun, non_constant_p, jump_target);
  tree ret = NULL_TREE;
  if (DECL_TYPE_TEMPLATE_P (r) || DECL_TEMPLATE_TEMPLATE_PARM_P (r))
    ret = lookup_template_class (r, rvec, NULL_TREE, NULL_TREE, tf_none);
  else if (concept_definition_p (r))
    {
      ret = build_concept_check (r, rvec, tf_none);
      ret = evaluate_concept_check (ret);
      return get_reflection_raw (loc, ret, REFLECT_VALUE);
    }
  else if (variable_template_p (r))
    {
      ret = lookup_template_variable (r, rvec, tf_none);
      ret = finish_template_variable (ret, tf_none);
    }
  else
    ret = lookup_template_function (r, rvec);
  return get_reflection_raw (loc, ret);
}

/* Process std::meta::tuple_size.
   Returns: tuple_size_v<T>, where T is the type represented by
   dealias(type).  */

static tree
eval_tuple_size (location_t loc, const constexpr_ctx *ctx, tree type,
		 tree call, bool *non_constant_p, tree *jump_target,
		 tree fun)
{
  /* Create std::tuple_size<TYPE>::value.  */
  tree args = make_tree_vec (1);
  TREE_VEC_ELT (args, 0) = type;
  return finish_library_value_trait (loc, ctx, "tuple_size", args, call,
				     non_constant_p, jump_target, fun);
}

/* Process std::meta::tuple_element.
   Returns: A reflection representing the type denoted by
   tuple_element_t<I, T>, where T is the type represented by dealias(type)
   and I is a constant equal to index.  */

static tree
eval_tuple_element (location_t loc, const constexpr_ctx *ctx, tree i,
		    tree type, tree call, bool *non_constant_p,
		    tree *jump_target, tree fun)
{
  /* Create std::tuple_element<I,TYPE>::type.  */
  tree args = make_tree_vec (2);
  TREE_VEC_ELT (args, 0) = i;
  TREE_VEC_ELT (args, 1) = type;
  return finish_library_type_trait (loc, ctx, "tuple_element",
				    args, call, non_constant_p, jump_target,
				    fun);
}

/* Process std::meta::variant_size.
   Returns: variant_size_v<T>, where T is the type represented by
   dealias(type).  */

static tree
eval_variant_size (location_t loc, const constexpr_ctx *ctx, tree type,
		   tree call, bool *non_constant_p, tree *jump_target,
		   tree fun)
{
  /* Create std::variant_size<TYPE>::value.  */
  tree args = make_tree_vec (1);
  TREE_VEC_ELT (args, 0) = type;
  return finish_library_value_trait (loc, ctx, "variant_size", args, call,
				     non_constant_p, jump_target, fun);
}

/* Process std::meta::variant_alternative.
   Returns: A reflection representing the type denoted by
   variant_alternative_t<I, T>, where T is the type represented by
   dealias(type) and I is a constant equal to index.  */

static tree
eval_variant_alternative (location_t loc, const constexpr_ctx *ctx, tree i,
			  tree type, tree call, bool *non_constant_p,
			  tree *jump_target, tree fun)
{
  /* Create std::variant_alternative<I,TYPE>::type.  */
  tree args = make_tree_vec (2);
  TREE_VEC_ELT (args, 0) = i;
  TREE_VEC_ELT (args, 1) = type;
  return finish_library_type_trait (loc, ctx, "variant_alternative",
				    args, call, non_constant_p, jump_target,
				    fun);
}

/* Process std::meta::data_member_spec.
   Returns: A reflection of a data member description (T,N,A,W,NUA) where
   -- T is the type represented by dealias(type),
   -- N is either the identifier encoded by options.name or _|_ if
      options.name does not contain a value,
   -- A is either the alignment value held by options.alignment or _|_ if
      options.alignment does not contain a value,
   -- W is either the value held by options.bit_width or _|_ if
      options.bit_width does not contain a value, and
   -- NUA is the value held by options.no_unique_address.
   Throws: meta::exception unless the following conditions are met:
   -- dealias(type) represents either an object type or a reference type;
   -- if options.name contains a value, then:
      -- holds_alternative<u8string>(options.name->contents) is true and
	 get<u8string>(options.name->contents) contains a valid identifier
	 that is not a keyword when interpreted with UTF-8, or
      -- holds_alternative<string>(options.name->contents) is true and
	 get<string>(options.name->contents) contains a valid identifier
	 that is not a keyword when interpreted with the ordinary literal
	 encoding;
   -- if options.name does not contain a value, then options.bit_width
      contains a value;
   -- if options.bit_width contains a value V, then
      -- is_integral_type(type) || is_enum_type(type) is true,
      -- options.alignment does not contain a value,
      -- options.no_unique_address is false,
      -- V is not negative, and
      -- if V equals 0, then options.name does not contain a value; and
   -- if options.alignment contains a value, it is an alignment value not less
      than alignment_of(type).  */

static tree
eval_data_member_spec (location_t loc, const constexpr_ctx *ctx,
		       tree type, tree opts, bool *non_constant_p,
		       bool *overflow_p, tree *jump_target, tree fun)
{
  type = strip_typedefs (type);
  if (!TYPE_OBJ_P (type) && !TYPE_REF_P (type))
    return throw_exception (loc, ctx, "type is not object or reference type",
			    fun, non_constant_p, jump_target);
  opts = convert_from_reference (opts);
  if (!CLASS_TYPE_P (TREE_TYPE (opts)))
    {
    fail:
      error_at (loc, "unexpected %<data_member_options%> argument");
      *non_constant_p = true;
      return NULL_TREE;
    }
  tree args[5] = { type, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE };
  for (tree field = next_aggregate_field (TYPE_FIELDS (TREE_TYPE (opts)));
       field; field = next_aggregate_field (DECL_CHAIN (field)))
    if (tree name = DECL_NAME (field))
      {
	if (id_equal (name, "name"))
	  args[1] = field;
	else if (id_equal (name, "alignment"))
	  args[2] = field;
	else if (id_equal (name, "bit_width"))
	  args[3] = field;
	else if (id_equal (name, "no_unique_address"))
	  args[4] = field;
      }
  for (int i = 1; i < 5; ++i)
    {
      if (args[i] == NULL_TREE)
	goto fail;
      tree opt = build3 (COMPONENT_REF, TREE_TYPE (args[i]), opts, args[i],
			 NULL_TREE);
      if (i == 4)
	{
	  /* The no_unique_address handling is simple.  */
	  if (TREE_CODE (TREE_TYPE (opt)) != BOOLEAN_TYPE)
	    goto fail;
	  opt = cxx_eval_constant_expression (ctx, opt, vc_prvalue,
					      non_constant_p, overflow_p,
					      jump_target);
	  if (*jump_target || *non_constant_p)
	    return NULL_TREE;
	  if (TREE_CODE (opt) != INTEGER_CST)
	    goto fail;
	  if (integer_zerop (opt))
	    args[i] = boolean_false_node;
	  else
	    args[i] = boolean_true_node;
	  continue;
	}
      /* Otherwise the member is optional<something>.  */
      if (!CLASS_TYPE_P (TREE_TYPE (opt)))
	goto fail;
      tree has_value = build_static_cast (loc, boolean_type_node, opt,
					  tf_warning_or_error);
      if (error_operand_p (has_value))
	goto fail;
      has_value = cxx_eval_constant_expression (ctx, has_value, vc_prvalue,
						non_constant_p, overflow_p,
						jump_target);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      if (TREE_CODE (has_value) != INTEGER_CST)
	goto fail;
      if (integer_zerop (has_value))
	{
	  /* If it doesn't have value, store NULL_TREE.  */
	  args[i] = NULL_TREE;
	  continue;
	}
      tree deref = build_new_op (loc, INDIRECT_REF, LOOKUP_NORMAL, opt,
				 NULL_TREE, tf_warning_or_error);
      if (error_operand_p (deref))
	goto fail;
      if (i != 1)
	{
	  /* For alignment and bit_width otherwise it should be int.  */
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (deref)) != integer_type_node)
	    goto fail;
	  deref = cxx_eval_constant_expression (ctx, deref, vc_prvalue,
						non_constant_p, overflow_p,
						jump_target);
	  if (*jump_target || *non_constant_p)
	    return NULL_TREE;
	  if (TREE_CODE (deref) != INTEGER_CST)
	    goto fail;
	  args[i] = deref;
	  continue;
	}
      /* Otherwise it is a name.  */
      if (!CLASS_TYPE_P (TREE_TYPE (deref)))
	goto fail;
      tree fields[3] = { NULL_TREE, NULL_TREE, NULL_TREE };
      for (tree field = next_aggregate_field (TYPE_FIELDS (TREE_TYPE (deref)));
	   field; field = next_aggregate_field (DECL_CHAIN (field)))
	if (tree name = DECL_NAME (field))
	  {
	    if (id_equal (name, "_M_is_u8"))
	      fields[0] = field;
	    else if (id_equal (name, "_M_u8s"))
	      fields[1] = field;
	    else if (id_equal (name, "_M_s"))
	      fields[2] = field;
	  }
      for (int j = 0; j < 3; ++j)
	{
	  if (fields[j] == NULL_TREE)
	    goto fail;
	  if (j && j == (fields[0] == boolean_true_node ? 2 : 1))
	    continue;
	  tree f = build3 (COMPONENT_REF, TREE_TYPE (fields[j]), deref,
			   fields[j], NULL_TREE);
	  if (j == 0)
	    {
	      /* The _M_is_u8 handling is simple.  */
	      if (TREE_CODE (TREE_TYPE (f)) != BOOLEAN_TYPE)
		goto fail;
	      f = cxx_eval_constant_expression (ctx, f, vc_prvalue,
						non_constant_p, overflow_p,
						jump_target);
	      if (*jump_target || *non_constant_p)
		return NULL_TREE;
	      if (TREE_CODE (f) != INTEGER_CST)
		goto fail;
	      if (integer_zerop (f))
		fields[0] = boolean_false_node;
	      else
		fields[0] = boolean_true_node;
	      continue;
	    }
	  /* _M_u8s/_M_s handling is the same except for encoding.  */
	  if (!CLASS_TYPE_P (TREE_TYPE (f)))
	    goto fail;
	  tree fns = lookup_qualified_name (TREE_TYPE (f),
					    get_identifier ("c_str"));
	  if (error_operand_p (fns))
	    goto fail;
	  f = build_new_method_call (f, fns, NULL, NULL_TREE, LOOKUP_NORMAL,
				     NULL, tf_warning_or_error);
	  if (error_operand_p (f))
	    goto fail;
	  f = cxx_eval_constant_expression (ctx, f, vc_prvalue,
					    non_constant_p, overflow_p,
					    jump_target);
	  if (*jump_target || *non_constant_p)
	    return NULL_TREE;
	  STRIP_NOPS (f);
	  if (TREE_CODE (f) != ADDR_EXPR)
	    goto fail;
	  f = TREE_OPERAND (f, 0);
	  f = cxx_eval_constant_expression (ctx, f, vc_prvalue,
					    non_constant_p, overflow_p,
					    jump_target);
	  if (*jump_target || *non_constant_p)
	    return NULL_TREE;
	  if (TREE_CODE (f) != CONSTRUCTOR
	      || TREE_CODE (TREE_TYPE (f)) != ARRAY_TYPE)
	    goto fail;
	  tree eltt = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (f)));
	  if (eltt != (j == 1 ? char8_type_node : char_type_node))
	    goto fail;
	  tree field, value;
	  unsigned k;
	  unsigned HOST_WIDE_INT l = 0;
	  bool ntmbs = false;
	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (f), k, field, value)
	    if (!tree_fits_shwi_p (value))
	      goto fail;
	    else if (field == NULL_TREE)
	      {
		if (integer_zerop (value))
		  {
		    ntmbs = true;
		    break;
		  }
		++l;
	      }
	    else if (TREE_CODE (field) == RANGE_EXPR)
	      {
		tree lo = TREE_OPERAND (field, 0);
		tree hi = TREE_OPERAND (field, 1);
		if (!tree_fits_uhwi_p (lo) || !tree_fits_uhwi_p (hi))
		  goto fail;
		if (integer_zerop (value))
		  {
		    l = tree_to_uhwi (lo);
		    ntmbs = true;
		    break;
		  }
		l = tree_to_uhwi (hi) + 1;
	      }
	    else if (tree_fits_uhwi_p (field))
	      {
		l = tree_to_uhwi (field);
		if (integer_zerop (value))
		  {
		    ntmbs = true;
		    break;
		  }
		++l;
	      }
	    else
	      goto fail;
	  if (!ntmbs || l > INT_MAX - 1)
	    goto fail;
	  char *namep;
	  unsigned len = l;
	  if (l < 64)
	    namep = XALLOCAVEC (char, l + 1);
	  else
	    namep = XNEWVEC (char, l + 1);
	  memset (namep, 0, l + 1);
	  l = 0;
	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (f), k, field, value)
	    if (field == NULL_TREE)
	      {
		if (integer_zerop (value))
		  break;
		namep[l] = tree_to_shwi (value);
		++l;
	      }
	    else if (TREE_CODE (field) == RANGE_EXPR)
	      {
		tree lo = TREE_OPERAND (field, 0);
		tree hi = TREE_OPERAND (field, 1);
		if (integer_zerop (value))
		  break;
		unsigned HOST_WIDE_INT m = tree_to_uhwi (hi);
		for (l = tree_to_uhwi (lo); l <= m; ++l)
		  namep[l] = tree_to_shwi (value);
	      }
	    else
	      {
		l = tree_to_uhwi (field);
		namep[l++] = tree_to_shwi (value);
	      }
	  namep[len] = '\0';
	  /* Convert namep from execution charset to SOURCE_CHARSET.  */
	  cpp_string istr, ostr;
	  istr.len = strlen (namep) + 1;
	  istr.text = (const unsigned char *) namep;
	  if (!cpp_translate_string (parse_in, &istr, &ostr,
				     j == 2 ? CPP_STRING : CPP_UTF8STRING,
				     true))
	    {
	      if (len >= 64)
		XDELETEVEC (namep);
	      if (j == 1)
		return throw_exception (loc, ctx,
					"conversion from ordinary literal "
					"encoding to source charset "
					"failed", fun, non_constant_p,
					jump_target);
	      else
		return throw_exception (loc, ctx,
					"conversion from UTF-8 encoding to "
					"source charset failed",
					fun, non_constant_p, jump_target);
	    }
	  if (len >= 64)
	    XDELETEVEC (namep);
	  if (!cpp_valid_identifier (parse_in, ostr.text))
	    return throw_exception (loc, ctx,
				    "name is not a valid identifier",
				    fun, non_constant_p, jump_target);
	  args[i] = get_identifier ((const char *) ostr.text);
	  switch (get_identifier_kind (args[i]))
	    {
	    case cik_keyword:
	      return throw_exception (loc, ctx, "name is a keyword",
				      fun, non_constant_p, jump_target);
	    case cik_trait:
	      return throw_exception (loc, ctx, "name is a built-in trait",
				      fun, non_constant_p, jump_target);
	    default:
	      break;
	    }
	}
    }
  if (args[1] == NULL_TREE && args[3] == NULL_TREE)
    return throw_exception (loc, ctx,
			    "neither name nor bit_width specified",
			    fun, non_constant_p, jump_target);
  if (args[3])
    {
      if (!CP_INTEGRAL_TYPE_P (type) && TREE_CODE (type) != ENUMERAL_TYPE)
	return throw_exception (loc, ctx,
				"bit_width specified with non-integral "
				"and non-enumeration type",
				fun, non_constant_p, jump_target);
      if (args[2])
	return throw_exception (loc, ctx,
				"both alignment and bit_width specified",
				fun, non_constant_p, jump_target);
      if (args[4] == boolean_true_node)
	return throw_exception (loc, ctx,
				"bit_width specified with "
				"no_unique_address true",
				fun, non_constant_p, jump_target);
      if (integer_zerop (args[3]) && args[1])
	return throw_exception (loc, ctx,
				"bit_width 0 with specified name",
				fun, non_constant_p, jump_target);
      if (tree_int_cst_sgn (args[3]) < 0)
	return throw_exception (loc, ctx, "bit_width is negative",
				fun, non_constant_p, jump_target);
    }
  if (args[2])
    {
      if (!integer_pow2p (args[2]))
	return throw_exception (loc, ctx,
				"alignment is not power of two",
				fun, non_constant_p, jump_target);
      if (tree_int_cst_sgn (args[2]) < 0)
	return throw_exception (loc, ctx, "alignment is negative",
				fun, non_constant_p, jump_target);
      tree al = cxx_sizeof_or_alignof_type (loc, type, ALIGNOF_EXPR, true,
					    tf_none);
      if (TREE_CODE (al) == INTEGER_CST
	  && wi::to_widest (al) > wi::to_widest (args[2]))
	return throw_exception (loc, ctx,
				"alignment is smaller than alignment_of",
				fun, non_constant_p, jump_target);
    }
  tree ret = make_tree_vec (5);
  for (int i = 0; i < 5; ++i)
    TREE_VEC_ELT (ret, i) = args[i];
  return get_reflection_raw (loc, ret, REFLECT_DATA_MEMBER_SPEC);
}

/* Process std::meta::define_aggregate.
   Let C be the type represented by class_type and r_K be the Kth reflection
   value in mdescrs.
   For every r_K in mdescrs, let (T_K,N_K,A_K,W_K,NUA_K) be the corresponding
   data member description represented by r_K.
   Constant When:
   -- class_type represents a cv-unqualified class type;
   -- C is incomplete from every point in the evaluation context;
   -- is_data_member_spec(r_K) is true for every r_K;
   -- is_complete_type(T_K) is true for every r_K; and
   -- for every pair (r_K,r_L) where K<L, if N_K is not _|_ and N_L is not
      _|_, then either:
      -- N_K is not the same identifier as N_L or
      -- N_K is the identifier _ (U+005F LOW LINE).
   Effects: Produces an injected declaration D that defines C and has
   properties as follows:
   -- The target scope of D is the scope to which C belongs.
   -- The locus of D follows immediately after the core constant expression
      currently under evaluation.
   -- The characteristic sequence of D is the sequence of reflection values
      r_K.
   -- If C is a specialization of a templated class T, and C is not a local
      class, then D is an explicit specialization of T.
   -- For each r_K, there is a corresponding entity M_K with public access
      belonging to the class scope of D with the following properties:
      -- If N_K is _|_, M_K is an unnamed bit-field.
	 Otherwise, M_K is a non-static data member whose name is the
	 identifier N_K.
      -- The type of M_K is T_K.
      -- M_K is declared with the attribute [[no_unique_address]] if and only
	 if NUA_K is true.
      -- If W_K is not _|_, M_K is a bit-field whose width is that value.
	 Otherwise, M_K is not a bit-field.
      -- If A_K is not _|_, M_K has the alignment-specifier alignas(A_K).
	 Otherwise, M_K has no alignment-specifier.
   -- For every r_L in mdescrs such that K<L, the declaration corresponding to
      r_K precedes the declaration corresponding to r_L.
   Returns: class_type.
   Remarks: If C is a specialization of a templated class and it has not been
   instantiated, C is treated as an explicit specialization.  */

static tree
eval_define_aggregate (location_t loc, const constexpr_ctx *ctx,
		       tree type, tree rvec, tree call, bool *non_constant_p)
{
  tree orig_type = type;
  if (!CLASS_TYPE_P (type))
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "first %<define_aggregate%> argument is not a class "
		       "type reflection");
      *non_constant_p = true;
      return call;
    }
  if (typedef_variant_p (type))
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "first %<define_aggregate%> argument is a reflection "
		       "of a type alias");
      *non_constant_p = true;
      return call;
    }
  if (cv_qualified_p (type))
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "first %<define_aggregate%> argument is a "
		       "cv-qualified class type reflection");
      *non_constant_p = true;
      return call;
    }
  if (COMPLETE_TYPE_P (type))
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "first %<define_aggregate%> argument is a complete "
		       "class type reflection");
      *non_constant_p = true;
      return call;
    }
  hash_set<tree> nameset;
  for (int i = 0; i < TREE_VEC_LENGTH (rvec); ++i)
    {
      tree ra = TREE_VEC_ELT (rvec, i);
      tree a = REFLECT_EXPR_HANDLE (ra);
      if (REFLECT_EXPR_KIND (ra) != REFLECT_DATA_MEMBER_SPEC)
	{
	  if (!cxx_constexpr_quiet_p (ctx))
	    error_at (loc, "%<define_aggregate%> argument not a data member "
			   "description");
	  *non_constant_p = true;
	  return call;
	}
      if (eval_is_complete_type (TREE_VEC_ELT (a, 0)) != boolean_true_node)
	{
	  if (!cxx_constexpr_quiet_p (ctx))
	    error_at (loc, "%<define_aggregate%> argument data member "
			   "description without complete type");
	  *non_constant_p = true;
	  return call;
	}
      if (TREE_VEC_ELT (a, 1)
	  && !id_equal (TREE_VEC_ELT (a, 1), "_")
	  && nameset.add (TREE_VEC_ELT (a, 1)))
	{
	  if (!cxx_constexpr_quiet_p (ctx))
	    error_at (loc, "name %qD used in multiple data member "
			   "descriptions", TREE_VEC_ELT (a, 1));
	  *non_constant_p = true;
	  return call;
	}
      if (TYPE_WARN_IF_NOT_ALIGN (type)
	  && TREE_VEC_ELT (a, 3))
	{
	  if (!cxx_constexpr_quiet_p (ctx))
	    error_at (loc, "cannot declare bit-field in "
			   "%<warn_if_not_aligned%> type");
	  *non_constant_p = true;
	  return call;
	}
    }
  tree consteval_block = cxx_constexpr_consteval_block (ctx);
  if (consteval_block == NULL_TREE)
    {
      if (!cxx_constexpr_quiet_p (ctx))
	error_at (loc, "%<define_aggregate%> not evaluated from "
		       "%<consteval%> block");
      *non_constant_p = true;
      return call;
    }
  iloc_sentinel ils = loc;
  type = TYPE_MAIN_VARIANT (type);
  type = strip_typedefs (type);
  tree cscope = NULL_TREE, tscope = NULL_TREE;
  for (tree c = TYPE_CONTEXT (CP_DECL_CONTEXT (consteval_block)); c;
       c = get_containing_scope (c))
    {
      if (c == type)
	{
	  auto_diagnostic_group d;
	  error_at (loc, "%<define_aggregate%> evaluated from "
			 "%<consteval%> block enclosed by %qT being "
			 "defined", type);
	  inform (DECL_SOURCE_LOCATION (consteval_block),
		  "%<consteval%> block defined here");
	  return get_reflection_raw (loc, orig_type);
	}
      if (cscope == NULL_TREE
	  && (TYPE_P (c) || TREE_CODE (c) == FUNCTION_DECL))
	cscope = c;
    }
  for (tree c = TYPE_CONTEXT (type); c; c = get_containing_scope (c))
    {
      if (c == consteval_block)
	{
	  auto_diagnostic_group d;
	  error_at (loc, "%<define_aggregate%> evaluated from "
			 "%<consteval%> block which encloses %qT being "
			 "defined", type);
	  inform (DECL_SOURCE_LOCATION (consteval_block),
		  "%<consteval%> block defined here");
	  return get_reflection_raw (loc, orig_type);
	}
      if (tscope == NULL_TREE
	  && (TYPE_P (c) || TREE_CODE (c) == FUNCTION_DECL))
	tscope = c;
    }
  if (cscope != tscope)
    {
      auto_diagnostic_group d;
      if (cscope && tscope)
	{
	  for (tree c = tscope; c; c = get_containing_scope (c))
	    if (c == cscope)
	      {
		if (DECL_P (tscope))
		  error_at (loc, "%qD intervenes between %qT scope and "
				 "%<consteval%> block %<define_aggregate%> "
				 "is evaluated from", tscope, type);
		else
		  error_at (loc, "%qT intervenes between %qT scope and "
				 "%<consteval%> block %<define_aggregate%> "
				 "is evaluated from", tscope, type);
		cscope = NULL_TREE;
		tscope = NULL_TREE;
		break;
	      }
	  for (tree c = cscope; c; c = get_containing_scope (c))
	    if (c == tscope)
	      {
		if (DECL_P (cscope))
		  error_at (loc, "%qD intervenes between %<consteval%> block "
				 "%<define_aggregate%> is evaluated from and "
				 "%qT scope", cscope, type);
		else
		  error_at (loc, "%qT intervenes between %<consteval%> block "
				 "%<define_aggregate%> is evaluated from and "
				 "%qT scope", cscope, type);
		cscope = NULL_TREE;
		tscope = NULL_TREE;
		break;
	      }
	  if (cscope && tscope)
	    {
	      if (DECL_P (cscope) && DECL_P (tscope))
		error_at (loc, "%<define_aggregate%> evaluated from "
			       "%<consteval%> block enclosed by %qD while "
			       "%qT type being defined is enclosed by %qD",
			  cscope, type, tscope);
	      else if (DECL_P (cscope))
		error_at (loc, "%<define_aggregate%> evaluated from "
			       "%<consteval%> block enclosed by %qD while "
			       "%qT type being defined is enclosed by %qT",
			  cscope, type, tscope);
	      else if (DECL_P (tscope))
		error_at (loc, "%<define_aggregate%> evaluated from "
			       "%<consteval%> block enclosed by %qT while "
			       "%qT type being defined is enclosed by %qD",
			  cscope, type, tscope);
	      else if (tscope)
		error_at (loc, "%<define_aggregate%> evaluated from "
			       "%<consteval%> block enclosed by %qT while "
			       "%qT type being defined is enclosed by %qT",
			  cscope, type, tscope);
	    }
	}
      else if (cscope && DECL_P (cscope))
	error_at (loc, "%qD intervenes between %<consteval%> block "
		       "%<define_aggregate%> is evaluated from and %qT scope",
		  cscope, type);
      else if (cscope)
	error_at (loc, "%qT intervenes between %<consteval%> block "
		       "%<define_aggregate%> is evaluated from and %qT scope",
		  cscope, type);
      else if (tscope && DECL_P (tscope))
	error_at (loc, "%qD intervenes between %qT scope and %<consteval%> "
		       "block %<define_aggregate%> is evaluated from",
		  tscope, type);
      else
	error_at (loc, "%qT intervenes between %qT scope and %<consteval%> "
		       "block %<define_aggregate%> is evaluated from",
		  tscope, type);
      inform (DECL_SOURCE_LOCATION (consteval_block),
	      "%<consteval%> block defined here");
      return get_reflection_raw (loc, orig_type);
    }
  if (primary_template_specialization_p (type))
    {
      type = maybe_process_partial_specialization (type);
      if (type == error_mark_node)
	{
	  *non_constant_p = true;
	  return call;
	}
    }
  if (!TYPE_BINFO (type))
    xref_basetypes (type, NULL_TREE);
  pushclass (type);
  gcc_assert (!TYPE_FIELDS (type));
  tree fields = NULL_TREE;
  for (int i = 0; i < TREE_VEC_LENGTH (rvec); ++i)
    {
      tree ra = TREE_VEC_ELT (rvec, i);
      tree a = REFLECT_EXPR_HANDLE (ra);
      tree f = build_decl (cp_expr_loc_or_input_loc (ra), FIELD_DECL,
			   TREE_VEC_ELT (a, 1), TREE_VEC_ELT (a, 0));
      DECL_CHAIN (f) = fields;
      DECL_IN_AGGR_P (f) = 1;
      DECL_CONTEXT (f) = type;
      TREE_PUBLIC (f) = 1;
      /* Bit-field.  */
      if (TREE_VEC_ELT (a, 3))
	{
	  /* Temporarily stash the width in DECL_BIT_FIELD_REPRESENTATIVE.
	     check_bitfield_decl picks it from there later and sets DECL_SIZE
	     accordingly.  */
	  DECL_BIT_FIELD_REPRESENTATIVE (f) = TREE_VEC_ELT (a, 3);
	  SET_DECL_C_BIT_FIELD (f);
	  DECL_NONADDRESSABLE_P (f) = 1;
	  /* If this bit-field is unnamed, it's padding.  */
	  if (!TREE_VEC_ELT (a, 1))
	    DECL_PADDING_P (f) = 1;
	}
      else if (TREE_VEC_ELT (a, 2))
	{
	  SET_DECL_ALIGN (f, tree_to_uhwi (TREE_VEC_ELT (a, 2))
			      * BITS_PER_UNIT);
	  DECL_USER_ALIGN (f) = 1;
	}
      if (TREE_VEC_ELT (a, 4) == boolean_true_node)
	{
	  tree attr = build_tree_list (NULL_TREE,
				       get_identifier ("no_unique_address"));
	  attr = build_tree_list (attr, NULL_TREE);
	  cplus_decl_attributes (&f, attr, 0);
	}
      fields = f;
    }
  TYPE_FIELDS (type) = fields;
  finish_struct (type, NULL_TREE);
  return get_reflection_raw (loc, orig_type);
}

/* Implement std::meta::reflect_constant_string.
   Let CharT be ranges::range_value_t<R>.
   Mandates: CharT is one of char, wchar_t, char8_t, char16_t, char32_t.
   Let V be the pack of values of type CharT whose elements are the
   corresponding elements of r, except that if r refers to a string literal
   object, then V does not include the trailing null terminator of r.
   Let P be the template parameter object of type
   const CharT[sizeof...(V) + 1] initialized with {V..., CharT()}.
   Returns: ^^P.  */

static tree
eval_reflect_constant_string (location_t loc, const constexpr_ctx *ctx,
			      tree call, bool *non_constant_p,
			      bool *overflow_p, tree *jump_target, tree fun)
{
  tree str = get_range_elts (loc, ctx, call, 0, non_constant_p, overflow_p,
			     jump_target, REFLECT_CONSTANT_STRING, fun);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  tree decl = get_template_parm_object (str,
					mangle_template_parm_object (str));
  DECL_MERGEABLE (decl) = 1;
  return get_reflection_raw (loc, decl);
}

/* Implement std::meta::reflect_constant_array.
   Let T be ranges::range_value_t<R>.
   Mandates: T is a structural type,
   is_constructible_v<T, ranges::range_reference_t<R>> is true, and
   is_copy_constructible_v<T> is true.
   Let V be the pack of values of type info of the same size as r, where the
   ith element is reflect_constant(e_i), where e_i is the ith element of r.
   Let P be
   -- If sizeof...(V) > 0 is true, then the template parameter object of type
      const T[sizeof...(V)] initialized with {[:V:]...}.
   -- Otherwise, the template parameter object of type array<T, 0> initialized
      with {}.
   Returns: ^^P.  */

static tree
eval_reflect_constant_array (location_t loc, const constexpr_ctx *ctx,
			     tree call, bool *non_constant_p,
			     bool *overflow_p, tree *jump_target, tree fun)
{
  tree str = get_range_elts (loc, ctx, call, 0, non_constant_p, overflow_p,
			     jump_target, REFLECT_CONSTANT_ARRAY, fun);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  tree decl = get_template_parm_object (str,
					mangle_template_parm_object (str));
  DECL_MERGEABLE (decl) = 1;
  return get_reflection_raw (loc, decl);
}

/* Process std::meta::access_context::current.  */

static tree
eval_access_context_current (location_t loc, const constexpr_ctx *ctx,
			     tree call, bool *non_constant_p)
{
  tree scope = cxx_constexpr_caller (ctx);
  /* Ignore temporary current_function_decl changes caused by
     push_access_scope.  */
  if (scope == NULL_TREE && current_function_decl)
    scope = current_function_decl_without_access_scope ();
  /* [meta.reflection.access.context]/(5.1.2): Otherwise, if an initialization
     by an inherited constructor is using I, a point whose immediate scope is
     the class scope corresponding to C.  */
  if (scope && DECL_INHERITED_CTOR (scope))
    scope = DECL_CONTEXT (scope);
  if (scope == NULL_TREE)
    {
      if (cxx_constexpr_manifestly_const_eval (ctx) != mce_true)
	{
	  /* Outside of functions limit this to manifestly constant-evaluation
	     so that we don't fold it prematurely.  */
	  if (!cxx_constexpr_quiet_p (ctx))
	    error_at (loc, "%<access_context::current%> used outside of "
			   "manifestly constant-evaluation");
	  *non_constant_p = true;
	  return call;
	}
      if (current_class_type)
	scope = current_class_type;
      else if (current_namespace)
	scope = current_namespace;
      else
	scope = global_namespace;
    }
  tree lam;
  while (LAMBDA_FUNCTION_P (scope)
	 && (lam = CLASSTYPE_LAMBDA_EXPR (CP_DECL_CONTEXT (scope)))
	 && LAMBDA_EXPR_CONSTEVAL_BLOCK_P (lam))
    scope = CP_TYPE_CONTEXT (CP_DECL_CONTEXT (scope));
  tree access_context = TREE_TYPE (call);
  if (TREE_CODE (access_context) != RECORD_TYPE)
    {
    fail:
      error_at (loc, "unexpected return type of %qs",
		"std::meta::access_context::current");
      return build_zero_cst (access_context);
    }
  tree scopef = next_aggregate_field (TYPE_FIELDS (access_context));
  if (!scopef || !REFLECTION_TYPE_P (TREE_TYPE (scopef)))
    goto fail;
  tree classf = next_aggregate_field (DECL_CHAIN (scopef));
  if (!classf || !REFLECTION_TYPE_P (TREE_TYPE (classf)))
    goto fail;
  if (next_aggregate_field (DECL_CHAIN (classf)))
    goto fail;
  vec<constructor_elt, va_gc> *elts = nullptr;
  CONSTRUCTOR_APPEND_ELT (elts, scopef, get_reflection_raw (loc, scope));
  CONSTRUCTOR_APPEND_ELT (elts, classf, get_null_reflection ());
  return build_constructor (access_context, elts);
}

/* Helper function to extract scope and designating class from
   access_context ACTX.  */

static bool
extract_access_context (location_t loc, tree actx, tree *scope,
			tree *designating_class)
{
  if (TREE_CODE (actx) != CONSTRUCTOR
      || CONSTRUCTOR_NELTS (actx) != 2
      || !REFLECT_EXPR_P (CONSTRUCTOR_ELT (actx, 0)->value)
      || !REFLECT_EXPR_P (CONSTRUCTOR_ELT (actx, 1)->value))
    {
      error_at (loc, "invalid %<access_context%> argument");
      return false;
    }
  *scope = REFLECT_EXPR_HANDLE (CONSTRUCTOR_ELT (actx, 0)->value);
  *designating_class = REFLECT_EXPR_HANDLE (CONSTRUCTOR_ELT (actx, 1)->value);
  if (*scope == unknown_type_node)
    *scope = NULL_TREE;
  else if (TREE_CODE (*scope) != FUNCTION_DECL
	   && TREE_CODE (*scope) != NAMESPACE_DECL
	   && !CLASS_TYPE_P (*scope))
    {
      error_at (loc, "unexpected %<access_context::scope()%>");
      return false;
    }
  else if (CLASS_TYPE_P (*scope))
    *scope = TYPE_MAIN_VARIANT (*scope);
  if (*designating_class == unknown_type_node)
    *designating_class = NULL_TREE;
  else if (!CLASS_TYPE_P (*designating_class)
	   || !COMPLETE_TYPE_P (*designating_class))
    {
      error_at (loc, "unexpected %<access_context::designating_class()%>");
      return false;
    }
  else
    *designating_class = TYPE_MAIN_VARIANT (*designating_class);
  return true;
}

/* Process std::meta::is_accessible.
   Let PARENT-CLS(r) be:
   -- If parent_of(r) represents a class C, then C.
   -- Otherwise, PARENT-CLS(parent_of(r)).
   Let DESIGNATING-CLS(r, ctx) be:
   -- If ctx.designating_class() represents a class C, then C.
   -- Otherwise, PARENT-CLS(r).
   Returns:
   -- If r represents an unnamed bit-field F, then is_accessible(r_H, ctx),
      where r_H represents a hypothetical non-static data member of the class
      represented by PARENT-CLS(r) with the same access as F.
   -- Otherwise, if r does not represent a class member or a direct base class
      relationship, then true.
   -- Otherwise, if r represents
      -- a class member that is not a (possibly indirect or variant) member of
	 DESIGNATING-CLS(r, ctx) or
      -- a direct base class relationship such that parent_of(r) does not
	 represent DESIGNATING-CLS(r, ctx) or a (direct or indirect) base
	 class thereof,
      then false.
   -- Otherwise, if ctx.scope() is the null reflection, then true.
   -- Otherwise, letting P be a program point whose immediate scope is the
      function parameter scope, class scope, or namespace scope corresponding
      to the function, class, or namespace represented by ctx.scope():
      -- If r represents a direct base class relationship (D,B), then true if
	 base class B of DESIGNATING-CLS(r, ctx) is accessible at P;
	 otherwise false.
      -- Otherwise, r represents a class member M; true if M would be
	 accessible at P with the designating class (as DESIGNATING-CLS(r, ctx)
	 if the effect of any using-declarations were ignored.  Otherwise,
	 false.
   Throws: meta::exception if r represents a class member for which
   PARENT-CLS(r) is an incomplete class.  */

static tree
eval_is_accessible (location_t loc, const constexpr_ctx *ctx, tree r,
		    reflect_kind kind, tree actx, tree call,
		    bool *non_constant_p, tree *jump_target, tree fun)
{
  tree scope = NULL_TREE, designating_class = NULL_TREE, c;
  if (!extract_access_context (loc, actx, &scope, &designating_class))
    {
      *non_constant_p = true;
      return call;
    }

  if (eval_is_class_member (r) == boolean_true_node)
    {
      r = maybe_get_reflection_fndecl (r);
      c = r;
      if (TREE_CODE (r) == CONST_DECL && UNSCOPED_ENUM_P (DECL_CONTEXT (r)))
	c = DECL_CONTEXT (r);
      if (TYPE_P (c))
	{
	  if (TYPE_NAME (c) && DECL_P (TYPE_NAME (c)))
	    c = CP_DECL_CONTEXT (TYPE_NAME (c));
	  else
	    c = CP_TYPE_CONTEXT (c);
	}
      else
	c = CP_DECL_CONTEXT (r);
    }
  else if (kind == REFLECT_BASE)
    {
      c = direct_base_derived (r);
      r = BINFO_TYPE (r);
    }
  else
    return boolean_true_node;
  if (!CLASS_TYPE_P (c) || !COMPLETE_TYPE_P (c))
    return throw_exception (loc, ctx,
			    "incomplete parent class",
			    fun, non_constant_p, jump_target);
  if (designating_class)
    {
      tree p = c;
      while (ANON_AGGR_TYPE_P (p) && p != designating_class)
	p = CP_TYPE_CONTEXT (p);
      if (p != designating_class
	  && (!CLASS_TYPE_P (p)
	      || !DERIVED_FROM_P (p, designating_class)))
	return boolean_false_node;
    }
  if (scope == NULL_TREE)
    return boolean_true_node;
  if (designating_class == NULL_TREE)
    designating_class = c;
  if (TREE_CODE (scope) == NAMESPACE_DECL)
    push_to_top_level ();
  else if (TYPE_P (scope))
    push_access_scope (TYPE_NAME (scope));
  else
    push_access_scope (scope);
  tree ret = boolean_false_node;
  if (kind == REFLECT_BASE)
    {
      if (accessible_base_p (designating_class, r, /*consider_local_p=*/true))
	ret = boolean_true_node;
    }
  else
    {
      tree o = TYPE_P (r) ? TYPE_NAME (r) : r;
      if (accessible_p (TYPE_BINFO (designating_class), o,
			/*consider_local_p=*/true))
	ret = boolean_true_node;
    }
  if (TREE_CODE (scope) == NAMESPACE_DECL)
    pop_from_top_level ();
  else if (TYPE_P (scope))
    pop_access_scope (TYPE_NAME (scope));
  else
    pop_access_scope (scope);
  return ret;
}

/* Returns true if R is C-members-of-representable from
   current point P.  */

static bool
members_of_representable_p (tree c, tree r)
{
  if (TREE_CODE (r) == CONST_DECL)
    return false;
  if (LAMBDA_TYPE_P (c) && !LAMBDA_FUNCTION_P (r))
    return false;
  if (TYPE_P (r))
    {
      if (CP_DECL_CONTEXT (TYPE_NAME (r)) != c)
	return false;
      if (LAMBDA_TYPE_P (r))
	return false;
      if (OVERLOAD_TYPE_P (r))
	return true;
      if (typedef_variant_p (r))
	return true;
    }
  else if (DECL_P (r))
    {
      if (CP_DECL_CONTEXT (r) != c)
	return false;
      if (DECL_CLASS_TEMPLATE_P (r)
	  || DECL_FUNCTION_TEMPLATE_P (r)
	  || variable_template_p (r)
	  || DECL_ALIAS_TEMPLATE_P (r)
	  || concept_definition_p (r)
	  || TREE_CODE (r) == FIELD_DECL
	  || TREE_CODE (r) == NAMESPACE_DECL)
	return true;
      if (VAR_OR_FUNCTION_DECL_P (r) && !undeduced_auto_decl (r))
	return true;
    }
  return false;
}

/* Callback for vector qsort to compare members by ascending DECL_UID.  */

static int
members_cmp (const void *a, const void *b)
{
  const constructor_elt *ea = (const constructor_elt *) a;
  const constructor_elt *eb = (const constructor_elt *) b;
  tree vala = REFLECT_EXPR_HANDLE (ea->value);
  tree valb = REFLECT_EXPR_HANDLE (eb->value);
  if (TYPE_P (vala))
    vala = TYPE_NAME (vala);
  if (TYPE_P (valb))
    valb = TYPE_NAME (valb);
  if (DECL_UID (vala) < DECL_UID (valb))
    return -1;
  if (DECL_UID (vala) > DECL_UID (valb))
    return 1;
  gcc_assert (ea == eb);
  return 0;
}

/* Enumerate members of namespace NS for eval_members_of.  */

static vec<constructor_elt, va_gc> *
namespace_members_of (location_t loc, tree ns)
{
  vec<constructor_elt, va_gc> *elts = nullptr;
  hash_set<tree> *seen = nullptr;
  for (tree o : *DECL_NAMESPACE_BINDINGS (ns))
    {
      if (TREE_CODE (o) == OVERLOAD && OVL_LOOKUP_P (o))
	{
	  if (TREE_TYPE (o))
	    {
	      tree m = TREE_TYPE (TREE_TYPE (o));
	      if (members_of_representable_p (ns, m))
		CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
					get_reflection_raw (loc, m));
	    }
	  if (OVL_DEDUP_P (o) || !OVL_FUNCTION (o))
	    continue;
	  o = OVL_FUNCTION (o);
	}
      for (ovl_iterator iter (o); iter; ++iter)
	{
	  if (iter.hidden_p ())
	    continue;
	  tree b = *iter;
	  tree m = b;

	  if (VAR_P (b) && DECL_ANON_UNION_VAR_P (b))
	    {
	      /* TODO: This doesn't handle namespace N { static union {}; }
		 but we pedwarn on that, so perhaps it doesn't need to be
		 handled.  */
	      tree v = DECL_VALUE_EXPR (b);
	      gcc_assert (v && TREE_CODE (v) == COMPONENT_REF);
	      tree var = TREE_OPERAND (v, 0);
	      tree type = TREE_TYPE (var);
	      if (!seen)
		seen = new hash_set<tree>;
	      if (members_of_representable_p (ns, type) && !seen->add (type))
		CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
					get_reflection_raw (loc, type));
	      if (members_of_representable_p (ns, var) && !seen->add (var))
		CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
					get_reflection_raw (loc, var));
	      continue;
	    }
	  if (TREE_CODE (b) == TYPE_DECL)
	    m = TREE_TYPE (b);
	  if (!members_of_representable_p (ns, m))
	    continue;
	  if (DECL_DECOMPOSITION_P (m) && !DECL_DECOMP_IS_BASE (m))
	    {
	      tree base = DECL_DECOMP_BASE (m);
	      if (!seen)
		seen = new hash_set<tree>;
	      if (members_of_representable_p (ns, base) && !seen->add (base))
		CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
					get_reflection_raw (loc, base));
	      if (!DECL_HAS_VALUE_EXPR_P (m))
		CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
					get_reflection_raw (loc, m,
							    REFLECT_VAR));
	      continue;
	    }
	  /* eval_is_accessible should be always true for namespace members,
	     so don't bother calling it here.  */
	  CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				  get_reflection_raw (loc, m));
	}
    }
  delete seen;
  if (elts)
    elts->qsort (members_cmp);
  return elts;
}

/* Enumerate members of class R for eval_*members_of.  KIND is
   one of METAFN_{,{,NON}STATIC_DATA_}MEMBERS_OF or
   METAFN_HAS_INACCESSIBLE_NONSTATIC_DATA_MEMBERS.
   For the last kind don't append any elts except for the first one for
   which is_accessible returned false.  */

static vec<constructor_elt, va_gc> *
class_members_of (location_t loc, const constexpr_ctx *ctx, tree r,
		  tree actx, tree call, bool *non_constant_p,
		  tree *jump_target, enum metafn_code kind, tree fun)
{
  if (kind == METAFN_MEMBERS_OF)
    {
      if (modules_p ())
	lazy_load_pendings (TYPE_NAME (r));
      if (CLASSTYPE_LAZY_DEFAULT_CTOR (r))
	lazily_declare_fn (sfk_constructor, r);
      if (CLASSTYPE_LAZY_COPY_CTOR (r))
	lazily_declare_fn (sfk_copy_constructor, r);
      if (CLASSTYPE_LAZY_MOVE_CTOR (r))
	lazily_declare_fn (sfk_move_constructor, r);
      if (CLASSTYPE_LAZY_DESTRUCTOR (r))
	lazily_declare_fn (sfk_destructor, r);
      if (CLASSTYPE_LAZY_COPY_ASSIGN (r))
	lazily_declare_fn (sfk_copy_assignment, r);
      if (CLASSTYPE_LAZY_MOVE_ASSIGN (r))
	lazily_declare_fn (sfk_move_assignment, r);
    }
  auto_vec <tree, 8> implicitly_declared;
  vec<constructor_elt, va_gc> *elts = nullptr;
  for (tree field = TYPE_FIELDS (r); field; field = DECL_CHAIN (field))
    {
      tree m = field;
      if (TREE_CODE (field) == FIELD_DECL && DECL_ARTIFICIAL (field))
	continue; /* Ignore bases.  */
      else if (DECL_SELF_REFERENCE_P (field))
	continue;
      else if (TREE_CODE (field) == TYPE_DECL)
	m = TREE_TYPE (field);
      else if (TREE_CODE (field) == FUNCTION_DECL)
	{
	  /* Ignore cloned cdtors.  */
	  if (DECL_CLONED_FUNCTION_P (field))
	    continue;
	  /* Ignore functions with unsatisfied constraints.  */
	  if (!constraints_satisfied_p (field))
	    continue;
	  if (DECL_MAYBE_DELETED (field))
	    {
	      ++function_depth;
	      maybe_synthesize_method (field);
	      --function_depth;
	    }
	}
      if (members_of_representable_p (r, m))
	{
	  if (kind == METAFN_STATIC_DATA_MEMBERS_OF
	      && eval_is_variable (m, REFLECT_UNDEF) != boolean_true_node)
	    continue; /* For static_data_members_of only include
			 is_variable.  */
	  else if ((kind == METAFN_NONSTATIC_DATA_MEMBERS_OF
		    || kind == METAFN_HAS_INACCESSIBLE_NONSTATIC_DATA_MEMBERS)
		   && eval_is_nonstatic_data_member (m) != boolean_true_node)
	    continue; /* For nonstatic_data_members_of only include
			 is_nonstatic_data_member.  */
	  tree a = eval_is_accessible (loc, ctx, m, REFLECT_UNDEF, actx, call,
				       non_constant_p, jump_target, fun);
	  if (*jump_target || *non_constant_p)
	    return nullptr;
	  if (a == boolean_false_node)
	    {
	      if (kind == METAFN_HAS_INACCESSIBLE_NONSTATIC_DATA_MEMBERS
		  && elts == nullptr)
		CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, boolean_true_node);
	      continue;
	    }
	  gcc_assert (a == boolean_true_node);
	  if (kind == METAFN_MEMBERS_OF
	      && TREE_CODE (m) == FUNCTION_DECL
	      && DECL_ARTIFICIAL (m))
	    {
	      /* Implicitly-declared special members or operator== members
		 appear after any user declared members.  */
	      implicitly_declared.safe_push (m);
	      continue;
	    }
	  else if (kind == METAFN_HAS_INACCESSIBLE_NONSTATIC_DATA_MEMBERS)
	    continue;
	  CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				  get_reflection_raw (loc, m));
	}
    }
  /* TYPE_DECLs in TYPE_FIELDS come after other decls, so for members_of
     the declaration order is not preserved.  */
  if (kind == METAFN_MEMBERS_OF && elts)
    elts->qsort (members_cmp);
  if (kind == METAFN_MEMBERS_OF && !implicitly_declared.is_empty ())
    {
      gcc_assert (implicitly_declared.length () <= 8);
      for (tree m : implicitly_declared)
	if (default_ctor_p (m))
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
      for (tree m : implicitly_declared)
	if (DECL_COPY_CONSTRUCTOR_P (m))
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
      for (tree m : implicitly_declared)
	if (special_function_p (m) == sfk_copy_assignment)
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
      for (tree m : implicitly_declared)
	if (DECL_MOVE_CONSTRUCTOR_P (m))
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
      for (tree m : implicitly_declared)
	if (special_function_p (m) == sfk_move_assignment)
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
      for (tree m : implicitly_declared)
	if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (m))
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
      for (tree m : implicitly_declared)
	if (DECL_OVERLOADED_OPERATOR_IS (m, EQ_EXPR))
	  {
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
				    get_reflection_raw (loc, m));
	    break;
	  }
    }
  return elts;
}

/* Enumerate bases of class R for eval_*of.  KIND is METAFN_BASES_OF
   or METAFN_HAS_INACCESSIBLE_BASES.  */

static vec<constructor_elt, va_gc> *
class_bases_of (location_t loc, const constexpr_ctx *ctx, tree r,
		tree actx, tree call, bool *non_constant_p,
		tree *jump_target, enum metafn_code kind, tree fun)
{
  vec<constructor_elt, va_gc> *elts = nullptr;
  tree binfo = TYPE_BINFO (r), base_binfo;
  for (unsigned i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    {
      tree a = eval_is_accessible (loc, ctx, base_binfo, REFLECT_BASE, actx,
				   call, non_constant_p, jump_target, fun);
      if (*jump_target || *non_constant_p)
	return nullptr;
      if (a == boolean_false_node)
	{
	  if (kind == METAFN_HAS_INACCESSIBLE_BASES && elts == nullptr)
	    CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE, boolean_true_node);
	  continue;
	}
      gcc_assert (a == boolean_true_node);
      if (kind == METAFN_HAS_INACCESSIBLE_BASES)
	continue;
      CONSTRUCTOR_APPEND_ELT (elts, NULL_TREE,
			      get_reflection_raw (loc, base_binfo,
						  REFLECT_BASE));
    }
  return elts;
}

/* Implement std::meta::members_of.
   A declaration D members-of-precedes a point P if D precedes either P or the
   point immediately following the class-specifier of the outermost class for
   which P is in a complete-class context.
   A declaration D of a member M of a class or namespace Q is
   Q-members-of-eligible if
   -- the host scope of D is the class scope or namespace scope associated
      with Q,
   -- D is not a friend declaration,
   -- M is not a closure type,
   -- M is not a specialization of a template,
   -- if Q is a class that is not a closure type, then M is a direct member of
      Q that is not a variant member of a nested anonymous union of Q, and
   -- if Q is a closure type, then M is a function call operator or function
      call operator template.
   It is implementation-defined whether declarations of other members of a
   closure type Q are Q-members-of-eligible.
   A member M of a class or namespace Q is Q-members-of-representable from a
   point P if a Q-members-of-eligible declaration of M members-of-precedes P,
   and M is
   -- a class or enumeration type
   -- a type alias
   -- a class template, function template, variable template, alias template,
      or concept,
   -- a variable or reference V for which the type of V does not contain an
      undeduced placeholder type,
   -- a function F for which
      -- the type of F does not contain an undeduced placeholder type,
      -- the constraints (if any) of F are satisfied, and
      -- if F is a prospective destructor, F is the selected destructor,
   -- a non-static data member,
   -- a namespace, or
   -- a namespace alias.
   Returns: A vector containing reflections of all members M of the entity Q
   represented by dealias(r) for which
   -- M is Q-members-of-representable from some point in the evaluation
      context and
   -- is_accessible(^^M, ctx) is true.
   If dealias(r) represents a class C, then the vector also contains
   reflections representing all unnamed bit-fields B whose declarations
   inhabit the class scope corresponding to C for which
   is_accessible(^^B, ctx) is true.
   Reflections of class members and unnamed bit-fields that are declared
   appear in the order in which they are declared.
   Throws: meta::exception unless dealias(r) is a reflection representing
   either a class type that is complete from some point in the evaluation
   context or a namespace.  */

static tree
eval_members_of (location_t loc, const constexpr_ctx *ctx, tree r,
		 tree actx, tree call, bool *non_constant_p,
		 tree *jump_target, tree fun)
{
  r = maybe_strip_typedefs (r);
  if (TREE_CODE (r) == NAMESPACE_DECL)
    r = ORIGINAL_NAMESPACE (r);
  vec<constructor_elt, va_gc> *elts;
  if (TREE_CODE (r) == NAMESPACE_DECL)
    elts = namespace_members_of (loc, r);
  else if (CLASS_TYPE_P (r)
	   && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      elts = class_members_of (loc, ctx, r, actx, call, non_constant_p,
			       jump_target, METAFN_MEMBERS_OF, fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  else
    return throw_exception (loc, ctx,
			    "neither complete class type nor namespace",
			    fun, non_constant_p, jump_target);
  return get_vector_of_info_elts (elts);
}

/* Implement std::meta::bases_of.
   Returns: Let C be the class represented by dealias(type).
   A vector containing the reflections of all the direct base class
   relationships B, if any, of C such that is_accessible(^^B, ctx) is true.
   The direct base class relationships appear in the order in which the
   corresponding base classes appear in the base-specifier-list of C.
   Throws: meta::exception unless dealias(type) represents a class type that
   is complete from some point in the evaluation context.  */

static tree
eval_bases_of (location_t loc, const constexpr_ctx *ctx, tree r,
	       tree actx, tree call, bool *non_constant_p,
	       tree *jump_target, tree fun)
{
  r = maybe_strip_typedefs (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  if (CLASS_TYPE_P (r)
      && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      elts = class_bases_of (loc, ctx, r, actx, call, non_constant_p,
			     jump_target, METAFN_BASES_OF, fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  else
    return throw_exception (loc, ctx, "not a complete class type",
			    fun, non_constant_p, jump_target);
  return get_vector_of_info_elts (elts);
}

/* Implement std::meta::static_data_members_of.
   Returns: A vector containing each element e of members_of(type, ctx) such
   that is_variable(e) is true, preserving their order.
   Throws: meta::exception unless dealias(type) represents a class type that
   is complete from some point in the evaluation context.  */

static tree
eval_static_data_members_of (location_t loc, const constexpr_ctx *ctx, tree r,
			     tree actx, tree call, bool *non_constant_p,
			     tree *jump_target, tree fun)
{
  r = maybe_strip_typedefs (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  if (CLASS_TYPE_P (r)
      && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      elts = class_members_of (loc, ctx, r, actx, call, non_constant_p,
			       jump_target, METAFN_STATIC_DATA_MEMBERS_OF,
			       fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  else
    return throw_exception (loc, ctx, "not a complete class type",
			    fun, non_constant_p, jump_target);
  return get_vector_of_info_elts (elts);
}

/* Implement std::meta::nonstatic_data_members_of.
   Returns: A vector containing each element e of members_of(type, ctx) such
   that is_nonstatic_data_member(e) is true, preserving their order.
   Throws: meta::exception unless dealias(type) represents a class type that
   is complete from some point in the evaluation context.  */

static tree
eval_nonstatic_data_members_of (location_t loc, const constexpr_ctx *ctx,
				tree r, tree actx, tree call,
				bool *non_constant_p, tree *jump_target,
				tree fun)
{
  r = maybe_strip_typedefs (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  if (CLASS_TYPE_P (r)
      && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      elts = class_members_of (loc, ctx, r, actx, call, non_constant_p,
			       jump_target, METAFN_NONSTATIC_DATA_MEMBERS_OF,
			       fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  else
    return throw_exception (loc, ctx, "not a complete class type",
			    fun, non_constant_p, jump_target);
  return get_vector_of_info_elts (elts);
}

/* Implement std::meta::subobjects_of.
   Returns: A vector containing each element of bases_of(type, ctx) followed
   by each element of nonstatic_data_members_of(type, ctx), preserving their
   order.
   Throws: meta::exception unless dealias(type) represents a class type that
   is complete from some point in the evaluation context.  */

static tree
eval_subobjects_of (location_t loc, const constexpr_ctx *ctx, tree r,
		    tree actx, tree call, bool *non_constant_p,
		    tree *jump_target, tree fun)
{
  r = maybe_strip_typedefs (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  if (CLASS_TYPE_P (r)
      && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      elts = class_bases_of (loc, ctx, r, actx, call, non_constant_p,
			     jump_target, METAFN_BASES_OF, fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      vec<constructor_elt, va_gc> *elts2
	= class_members_of (loc, ctx, r, actx, call, non_constant_p,
			    jump_target, METAFN_NONSTATIC_DATA_MEMBERS_OF,
			    fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      if (elts == nullptr)
	elts = elts2;
      else if (elts2)
	vec_safe_splice (elts, elts2);
    }
  else
    return throw_exception (loc, ctx, "not a complete class type",
			    fun, non_constant_p, jump_target);
  return get_vector_of_info_elts (elts);
}

/* Implement std::meta::has_inaccessible_nonstatic_data_members.
   Returns: true if is_accessible(R, ctx) is false for any R in
   nonstatic_data_members_of(r, access_context::unchecked()).
   Otherwise, false.
   Throws: meta::exception if
   -- the evaluation of
      nonstatic_data_members_of(r, access_context::unchecked()) would exit via
      an exception and or
   -- r represents a closure type.  */

static tree
eval_has_inaccessible_nonstatic_data_members (location_t loc,
					      const constexpr_ctx *ctx,
					      tree r, tree actx, tree call,
					      bool *non_constant_p,
					      tree *jump_target, tree fun)
{
  r = maybe_strip_typedefs (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  if (CLASS_TYPE_P (r)
      && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      if (LAMBDA_TYPE_P (r))
	return throw_exception (loc, ctx, "closure type", fun,
				non_constant_p, jump_target);
      elts = class_members_of (loc, ctx, r, actx, call, non_constant_p,
			       jump_target,
			       METAFN_HAS_INACCESSIBLE_NONSTATIC_DATA_MEMBERS,
			       fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  else
    return throw_exception (loc, ctx, "not a complete class type",
			    fun, non_constant_p, jump_target);
  if (elts == nullptr)
    return boolean_false_node;
  else
    return boolean_true_node;
}

/* Implement std::meta::has_inaccessible_bases.
   Returns: true if is_accessible(R, ctx) is false for any R in
   bases_of(r, access_context::unchecked()).  Otherwise, false.
   Throws: meta::exception if the evaluation of
   bases_of(r, access_context::unchecked()) would exit via an exception.  */

static tree
eval_has_inaccessible_bases (location_t loc, const constexpr_ctx *ctx,
			     tree r, tree actx, tree call,
			     bool *non_constant_p, tree *jump_target,
			     tree fun)
{
  r = maybe_strip_typedefs (r);
  vec<constructor_elt, va_gc> *elts = nullptr;
  if (CLASS_TYPE_P (r)
      && complete_type_or_maybe_complain (r, NULL_TREE, tf_none))
    {
      elts = class_bases_of (loc, ctx, r, actx, call, non_constant_p,
			     jump_target, METAFN_HAS_INACCESSIBLE_BASES, fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
    }
  else
    return throw_exception (loc, ctx, "not a complete class type",
			    fun, non_constant_p, jump_target);
  if (elts == nullptr)
    return boolean_false_node;
  else
    return boolean_true_node;
}

/* Implement std::meta::has_inaccessible_subobjects.
   Effects: Equivalent to:
   return has_inaccessible_bases(r, ctx)
	  || has_inaccessible_nonstatic_data_members(r, ctx);  */

static tree
eval_has_inaccessible_subobjects (location_t loc, const constexpr_ctx *ctx,
				  tree r, tree actx, tree call,
				  bool *non_constant_p, tree *jump_target,
				  tree fun)
{
  tree b = eval_has_inaccessible_bases (loc, ctx, r, actx, call,
					non_constant_p, jump_target, fun);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  if (b == boolean_true_node)
    return b;
  return eval_has_inaccessible_nonstatic_data_members (loc, ctx, r, actx,
						       call, non_constant_p,
						       jump_target, fun);
}

/* Implement std::meta::exception::_S_exception_cvt_to_utf8 and
   std::meta::exception::_S_exception_cvt_from_utf8.  This is
   an implementation specific metafunction which translates string_view
   into u8string_view resp. u8string_view into string_view for use in
   std::meta::exception constructors.  On translation failure returns an empty
   {u8,}string_view.  TO_UTF8 is true for _S_exception_cvt_to_utf8 and false
   for _S_exception_cvt_from_utf8.  */

static tree
eval_exception__S_exception_cvt_tofrom_utf8 (location_t loc,
					     const constexpr_ctx *ctx,
					     tree call, bool *non_constant_p,
					     bool *overflow_p,
					     tree *jump_target, tree fun,
					     bool to_utf8)
{
  tree str = get_range_elts (loc, ctx, call, 0, non_constant_p, overflow_p,
			     jump_target, REFLECT_CONSTANT_STRING, fun);
  if (*jump_target || *non_constant_p)
    return NULL_TREE;
  if (TREE_CODE (str) != STRING_CST
      || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (str)))
	  != (to_utf8 ? char_type_node : char8_type_node)))
    {
      error_at (loc, "unexpected argument to %qs",
		to_utf8 ? "_S_exception_cvt_to_utf8"
		: "_S_exception_cvt_from_utf8");
      *non_constant_p = true;
      return call;
    }
  /* We need to translate the string twice for the theoretical case
     of non-UTF8 SOURCE_CHARSET.  First translate from {exec charset,UTF-8} to
     SOURCE_CHARSET...  */
  cpp_string istr, ostr;
  istr.len = TREE_STRING_LENGTH (str) + 1;
  istr.text = (const unsigned char *) TREE_STRING_POINTER (str);
  const char *name;
  if (!cpp_translate_string (parse_in, &istr, &ostr,
			     to_utf8 ? CPP_STRING : CPP_UTF8STRING, true))
    {
      ostr.text = NULL;
      name = "";
    }
  else
    name = (const char *) ostr.text;
  /* And then let get_string_literal translate from SOURCE_CHARSET to
     {UTF-8,exec charset}.  */
  tree dchar_type = to_utf8 ? char8_type_node : char_type_node;
  str = get_string_literal (name, dchar_type);
  free (const_cast <unsigned char *> (ostr.text));
  if (str == NULL_TREE)
    {
      str = get_string_literal ("", dchar_type);
      gcc_assert (str);
    }
  releasing_vec args (make_tree_vector_single (str));
  tree ret = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&args, TREE_TYPE (call), LOOKUP_NORMAL,
					tf_warning_or_error);
  return build_cplus_new (TREE_TYPE (call), ret, tf_warning_or_error);
}

/* Helper for eval_extract, extracting a reference type T.
   Returns: If r represents an object O, then a reference to O.
   Otherwise, a reference to the object declared, or referred to, by the
   variable represented by r.
   Throws: meta::exception unless
   -- r represents a variable or object of type U,
   -- is_convertible_v<remove_reference_t<U>(*)[],
      remove_reference_t<T>(*)[]> is true, and
   -- If r represents a variable, then either that variable is usable in
      constant expressions or its lifetime began within the core constant
      expression currently under evaluation.  */

static tree
extract_ref (location_t loc, const constexpr_ctx *ctx, tree T, tree r,
	     reflect_kind kind, bool *non_constant_p, tree *jump_target,
	     tree fun)
{
  auto adjust_type = [](tree type) -> tree
    {
      if (TYPE_REF_P (type))
	type = TREE_TYPE (type);
      type = build_cplus_array_type (type, NULL_TREE);
      return build_pointer_type (type);
    };

  const bool var_p = eval_is_variable (r, kind) == boolean_true_node;
  if (var_p || eval_is_object (kind) == boolean_true_node)
    {
      /* The wording is saying that U is the type of r.  */
      tree U = TREE_TYPE (r);
      if (is_convertible (adjust_type (U), adjust_type (T))
	  && (!var_p || is_constant_expression (r)))
	{
	  if (TYPE_REF_P (TREE_TYPE (r)))
	    {
	      r = DECL_INITIAL (r);
	      r = maybe_get_reference_referent (r);
	      gcc_checking_assert (!TYPE_REF_P (TREE_TYPE (r)));
	    }
	  return build_address (r);
	}
    }

  return throw_exception (loc, ctx, "value cannot be extracted", fun,
			  non_constant_p, jump_target);
}

/* Helper for extract_value.  Return true iff we can extract value of
   type U using type T.  */

static bool
can_extract_value_p (tree T, tree U)
{
  if (POINTER_TYPE_P (U)
      && (similar_type_p (T, U)
	  || (FUNCTION_POINTER_TYPE_P (T) && FUNCTION_POINTER_TYPE_P (U)))
      && is_convertible (U, T))
    return true;
  else if (same_type_ignoring_top_level_qualifiers_p (T, U))
    return true;
  else if (TREE_CODE (U) == ARRAY_TYPE
	   && POINTER_TYPE_P (T)
	   && is_convertible (U, T))
    {
      /* remove_extent_t<U> */
      U = TREE_TYPE (U);
      U = strip_typedefs (U);
      /* remove_extent_t<U>* */
      U = build_pointer_type (U);
      return similar_type_p (T, U);
    }
  else if (LAMBDA_TYPE_P (U)
	   && FUNCTION_POINTER_TYPE_P (T)
	   && is_convertible (U, T))
    return true;
  return false;
}

/* Helper for eval_extract, extracting a value.
   Let U be the type of the value or object that r represents.
   Returns: static_cast<T>([:R:]), where R is a constant expression of
   type info such that R == r is true.
   Throws: meta::exception unless
   -- U is a pointer type, T and U are either similar or both function pointer
      types, and is_convertible_v<U, T> is true,
   -- U is not a pointer type and the cv-unqualified types of T and U are the
      same,
   -- U is an array type, T is a pointer type, remove_extent_t<U>* and T are
      similar types, and the value r represents is convertible to T, or
   -- U is a closure type, T is a function pointer type, and the value that r
      represents is convertible to T.  */

static tree
extract_value (location_t loc, const constexpr_ctx *ctx, tree T, tree r,
	       bool *non_constant_p, tree *jump_target, tree fun)
{
  if (REFLECT_EXPR_P (r))
    {
      r = REFLECT_EXPR_HANDLE (r);
      if (can_extract_value_p (T, TREE_TYPE (r)))
	return build_static_cast (loc, T, r, tf_none);
    }
  return throw_exception (loc, ctx, "value cannot be extracted", fun,
			  non_constant_p, jump_target);
}

/* Helper for extract_member_or_function.  Return true iff we can
   extract an NSDM or function R of kind KIND using type T.  */

static bool
can_extract_member_or_function_p (tree T, tree r, reflect_kind kind)
{
  if (eval_is_nonstatic_data_member (r) == boolean_true_node)
    {
      if (eval_is_bit_field (r, kind) == boolean_true_node)
	return false;
      /* static union { int m; }; extract<int>(^^m); is invalid.  */
      if (TREE_CODE (r) == FIELD_DECL
	  && ANON_UNION_TYPE_P (DECL_CONTEXT (r)))
	{
	  tree c = CP_TYPE_CONTEXT (DECL_CONTEXT (r));
	  while (ANON_UNION_TYPE_P (c))
	    c = CP_TYPE_CONTEXT (c);
	  if (!TYPE_P (c))
	    return false;
	}
      /* Create the X C::* type.  */
      tree type = build_offset_type (CP_DECL_CONTEXT (r), TREE_TYPE (r));
      if (similar_type_p (type, T) && is_convertible (type, T))
	return true;
      return false;
    }
  else if (DECL_IOBJ_MEMBER_FUNCTION_P (r))
    {
      tree F = TREE_TYPE (r);
      F = build_pointer_type (F);
      F = build_ptrmemfunc_type (F);
      if (same_type_p (T, F))
	return true;
      return false;
    }
  else if (TREE_CODE (r) == FUNCTION_DECL)
    {
      tree F = TREE_TYPE (r);
      F = build_pointer_type (F);
      if (same_type_p (T, F))
	return true;
      return false;
    }

  return false;
}

/* Helper for eval_extract, extracting a NSDM or function.
   Returns:
   -- If T is a pointer type, then a pointer value pointing to the function
      represented by r.
   -- Otherwise, a pointer-to-member value designating the non-static data
      member or function represented by r.
   Throws: meta::exception unless
   -- r represents a non-static data member with type X, that is not
      a bit-field, that is a direct member of class C, T and X C::*
      are similar types, and is_convertible_v<X C::*, T> is true;
   -- r represents an implicit object member function with type F or
      F noexcept that is a direct member of a class C, and T is F C::*; or
   -- r represents a non-member function, static member function, or
      explicit object member function of function type F or F noexcept, and
      T is F*.  */

static tree
extract_member_or_function (location_t loc, const constexpr_ctx *ctx,
			    tree T, tree r, reflect_kind kind,
			    bool *non_constant_p, tree *jump_target, tree fun)
{
  r = MAYBE_BASELINK_FUNCTIONS (r);
  if (!can_extract_member_or_function_p (T, r, kind))
    return throw_exception (loc, ctx, "value cannot be extracted", fun,
			    non_constant_p, jump_target);

  const tsubst_flags_t complain = complain_flags (ctx);
  if (POINTER_TYPE_P (T))
    return cp_build_addr_expr (r, complain);
  else
    {
      if (!mark_used (r, complain))
	{
	  *non_constant_p = true;
	  return NULL_TREE;
	}
      r = build_offset_ref (DECL_CONTEXT (r), r, /*address_p=*/true, complain);
      r = cp_build_addr_expr (r, complain);
      r = cp_convert (T, r, complain);
      return r;
    }
}

/* Process std::meta::extract.
   Let U be remove_cv_t<T>.
   Effects: Equivalent to:
     if constexpr (is_reference_type(^^T)) {
       return extract-ref<T>(r);
     } else if constexpr (is_nonstatic_data_member(r) || is_function(r)) {
       return extract-member-or-function<U>(r);
     } else {
       return extract-value<U>(constant_of(r));
     }
  */

static tree
eval_extract (location_t loc, const constexpr_ctx *ctx, tree type, tree r,
	      reflect_kind kind, bool *non_constant_p, bool *overflow_p,
	      tree *jump_target, tree fun)
{
  if (eval_is_reference_type (loc, type) == boolean_true_node)
    return extract_ref (loc, ctx, type, r, kind, non_constant_p, jump_target,
			fun);
  type = cv_unqualified (type);
  if (eval_is_nonstatic_data_member (r) == boolean_true_node
      || eval_is_function (r) == boolean_true_node)
    return extract_member_or_function (loc, ctx, type, r, kind, non_constant_p,
				       jump_target, fun);
  else
    {
      r = eval_constant_of (loc, ctx, r, kind, non_constant_p, overflow_p,
			    jump_target, fun);
      if (*jump_target || *non_constant_p)
	return NULL_TREE;
      return extract_value (loc, ctx, type, r, non_constant_p, jump_target,
			    fun);
    }
}

/* Expand a call to a metafunction FUN.  CALL is the CALL_EXPR.
   JUMP_TARGET is set if we are throwing std::meta::exception.  */

tree
process_metafunction (const constexpr_ctx *ctx, tree fun, tree call,
		      bool *non_constant_p, bool *overflow_p,
		      tree *jump_target)
{
  tree name = DECL_NAME (fun);
  const char *ident = IDENTIFIER_POINTER (name);
  const location_t loc = cp_expr_loc_or_input_loc (call);
  const metafn_info *minfo
    = metafn_lookup::find (ident, IDENTIFIER_LENGTH (name));
  if (minfo == NULL)
    {
    not_found:
      error_at (loc, "unknown metafunction %qD", fun);
      *non_constant_p = true;
      return NULL_TREE;
    }
  tree h = NULL_TREE, h1 = NULL_TREE, hvec = NULL_TREE, expr = NULL_TREE;
  tree type = NULL_TREE, ht, info;
  reflect_kind kind = REFLECT_UNDEF;
  for (int argno = 0; argno < 3; ++argno)
    switch ((minfo->kind >> ((argno + 1) * METAFN_KIND_SHIFT))
	    & METAFN_KIND_MASK)
      {
      case METAFN_KIND_ARG_VOID:
	break;
      case METAFN_KIND_ARG_INFO:
      case METAFN_KIND_ARG_TINFO:
	gcc_assert (argno < 2);
	info = get_info (ctx, call, argno, non_constant_p, overflow_p,
			 jump_target);
	if (*jump_target || *non_constant_p)
	  return NULL_TREE;
	ht = REFLECT_EXPR_HANDLE (info);
	if (((minfo->kind >> ((argno + 1) * METAFN_KIND_SHIFT))
	     & METAFN_KIND_MASK) == METAFN_KIND_ARG_TINFO)
	  {
	    if (eval_is_type (ht) != boolean_true_node)
	      return throw_exception_nontype (loc, ctx, fun, non_constant_p,
					      jump_target);
	  }
	if (argno == 0)
	  {
	    kind = REFLECT_EXPR_KIND (info);
	    h = ht;
	  }
	else
	  h1 = ht;
	break;
      case METAFN_KIND_ARG_REFLECTION_RANGE:
	gcc_assert (argno == 1);
	hvec = get_info_vec (loc, ctx, call, argno, non_constant_p,
			     overflow_p, jump_target, fun);
	if (*jump_target || *non_constant_p)
	  return NULL_TREE;
	break;
      case METAFN_KIND_ARG_REFLECTION_RANGET:
	hvec = get_type_info_vec (loc, ctx, call, argno, non_constant_p,
				overflow_p, jump_target, fun);
	if (*jump_target || *non_constant_p)
	  return NULL_TREE;
	break;
      case METAFN_KIND_ARG_INPUT_RANGE:
	/* Handled in eval_reflect_constant_*.  */
	gcc_assert (argno == 0);
	break;
      case METAFN_KIND_ARG_TEMPLATE_PARM:
      case METAFN_KIND_ARG_TEMPLATE_PARM_REF:
	type = TREE_VEC_ELT (get_template_innermost_arguments (fun), 0);
	/* FALLTHRU */
      case METAFN_KIND_ARG_SIZE_T:
      case METAFN_KIND_ARG_OPERATORS:
	gcc_assert (argno == 0);
	expr = get_nth_callarg (call, 0);
	expr = cxx_eval_constant_expression (ctx, expr, vc_prvalue,
					     non_constant_p, overflow_p,
					     jump_target);
	if (*jump_target || *non_constant_p)
	  return NULL_TREE;
	break;
      case METAFN_KIND_ARG_UNSIGNED:
      case METAFN_KIND_ARG_ACCESS_CONTEXT:
      case METAFN_KIND_ARG_DATA_MEMBER_OPTIONS:
	gcc_assert (argno == 1);
	expr = get_nth_callarg (call, argno);
	expr = cxx_eval_constant_expression (ctx, expr, vc_prvalue,
					     non_constant_p, overflow_p,
					     jump_target);
	if (*jump_target || *non_constant_p)
	  return NULL_TREE;
	break;
      default:
	gcc_unreachable ();
      }

  switch (minfo->code)
    {
    case METAFN_OPERATOR_OF:
      return eval_operator_of (loc, ctx, h, non_constant_p, jump_target,
			       TREE_TYPE (call), fun);
    case METAFN_SYMBOL_OF:
      return eval_symbol_of (loc, ctx, expr, non_constant_p, jump_target,
			     char_type_node, TREE_TYPE (call), fun);
    case METAFN_U8SYMBOL_OF:
      return eval_symbol_of (loc, ctx, expr, non_constant_p, jump_target,
			     char8_type_node, TREE_TYPE (call), fun);
    case METAFN_HAS_IDENTIFIER:
      return eval_has_identifier (h, kind);
    case METAFN_IDENTIFIER_OF:
      return eval_identifier_of (loc, ctx, h, kind, non_constant_p, jump_target,
				 char_type_node, TREE_TYPE (call), fun);
    case METAFN_U8IDENTIFIER_OF:
      return eval_identifier_of (loc, ctx, h, kind, non_constant_p, jump_target,
				 char8_type_node, TREE_TYPE (call), fun);
    case METAFN_DISPLAY_STRING_OF:
      return eval_display_string_of (loc, ctx, h, kind, non_constant_p,
				     jump_target, char_type_node,
				     TREE_TYPE (call), fun);
    case METAFN_U8DISPLAY_STRING_OF:
      return eval_display_string_of (loc, ctx, h, kind, non_constant_p,
				     jump_target, char8_type_node,
				     TREE_TYPE (call), fun);
    case METAFN_SOURCE_LOCATION_OF:
      return eval_source_location_of (loc, h, kind, TREE_TYPE (call));
    case METAFN_TYPE_OF:
      return eval_type_of (loc, ctx, h, kind, non_constant_p, jump_target, fun);
    case METAFN_OBJECT_OF:
      return eval_object_of (loc, ctx, h, kind, non_constant_p, overflow_p,
			     jump_target, fun);
    case METAFN_CONSTANT_OF:
      return eval_constant_of (loc, ctx, h, kind, non_constant_p, overflow_p,
			       jump_target, fun);
    case METAFN_IS_PUBLIC:
      return eval_is_public (h, kind);
    case METAFN_IS_PROTECTED:
      return eval_is_protected (h, kind);
    case METAFN_IS_PRIVATE:
      return eval_is_private (h, kind);
    case METAFN_IS_VIRTUAL:
      return eval_is_virtual (h, kind);
    case METAFN_IS_PURE_VIRTUAL:
      return eval_is_pure_virtual (h);
    case METAFN_IS_OVERRIDE:
      return eval_is_override (h);
    case METAFN_IS_FINAL:
      return eval_is_final (h);
    case METAFN_IS_DELETED:
      return eval_is_deleted (h);
    case METAFN_IS_DEFAULTED:
      return eval_is_defaulted (h);
    case METAFN_IS_USER_PROVIDED:
      return eval_is_user_provided (h);
    case METAFN_IS_USER_DECLARED:
      return eval_is_user_declared (h);
    case METAFN_IS_EXPLICIT:
      return eval_is_explicit (h);
    case METAFN_IS_NOEXCEPT:
      return eval_is_noexcept (h);
    case METAFN_IS_BIT_FIELD:
      return eval_is_bit_field (h, kind);
    case METAFN_IS_ENUMERATOR:
      return eval_is_enumerator (h);
    case METAFN_IS_ANNOTATION:
      return eval_is_annotation (h, kind);
    case METAFN_IS_CONST:
      return eval_is_const (h, kind);
    case METAFN_IS_VOLATILE:
      return eval_is_volatile (h, kind);
    case METAFN_IS_MUTABLE_MEMBER:
      return eval_is_mutable_member (h);
    case METAFN_IS_LVALUE_REFERENCE_QUALIFIED:
      return eval_is_lrvalue_reference_qualified (h, kind, /*rvalue_p=*/false);
    case METAFN_IS_RVALUE_REFERENCE_QUALIFIED:
      return eval_is_lrvalue_reference_qualified (h, kind, /*rvalue_p=*/true);
    case METAFN_HAS_STATIC_STORAGE_DURATION:
      return eval_has_static_storage_duration (h, kind);
    case METAFN_HAS_THREAD_STORAGE_DURATION:
      return eval_has_thread_storage_duration (h, kind);
    case METAFN_HAS_AUTOMATIC_STORAGE_DURATION:
      return eval_has_automatic_storage_duration (h, kind);
    case METAFN_HAS_INTERNAL_LINKAGE:
      return eval_has_internal_linkage (h, kind);
    case METAFN_HAS_MODULE_LINKAGE:
      return eval_has_module_linkage (h, kind);
    case METAFN_HAS_EXTERNAL_LINKAGE:
      return eval_has_external_linkage (h, kind);
    case METAFN_HAS_C_LANGUAGE_LINKAGE:
      return eval_has_c_language_linkage (h, kind);
    case METAFN_HAS_LINKAGE:
      return eval_has_linkage (h, kind);
    case METAFN_IS_COMPLETE_TYPE:
      return eval_is_complete_type (h);
    case METAFN_IS_ENUMERABLE_TYPE:
      return eval_is_enumerable_type (h);
    case METAFN_IS_VARIABLE:
      return eval_is_variable (h, kind);
    case METAFN_IS_TYPE:
      return eval_is_type (h);
    case METAFN_IS_NAMESPACE:
      return eval_is_namespace (h);
    case METAFN_IS_TYPE_ALIAS:
      return eval_is_type_alias (h);
    case METAFN_IS_NAMESPACE_ALIAS:
      return eval_is_namespace_alias (h);
    case METAFN_IS_FUNCTION:
      return eval_is_function (h);
    case METAFN_IS_CONVERSION_FUNCTION:
      return eval_is_conversion_function (h);
    case METAFN_IS_OPERATOR_FUNCTION:
      return eval_is_operator_function (h);
    case METAFN_IS_LITERAL_OPERATOR:
      return eval_is_literal_operator (h);
    case METAFN_IS_SPECIAL_MEMBER_FUNCTION:
      return eval_is_special_member_function (h);
    case METAFN_IS_CONSTRUCTOR:
      return eval_is_constructor (h);
    case METAFN_IS_DEFAULT_CONSTRUCTOR:
      return eval_is_default_constructor (h);
    case METAFN_IS_COPY_CONSTRUCTOR:
      return eval_is_copy_constructor (h);
    case METAFN_IS_MOVE_CONSTRUCTOR:
      return eval_is_move_constructor (h);
    case METAFN_IS_ASSIGNMENT:
      return eval_is_assignment (h);
    case METAFN_IS_COPY_ASSIGNMENT:
      return eval_is_copy_assignment (h);
    case METAFN_IS_MOVE_ASSIGNMENT:
      return eval_is_move_assignment (h);
    case METAFN_IS_DESTRUCTOR:
      return eval_is_destructor (h);
    case METAFN_IS_FUNCTION_PARAMETER:
      return eval_is_function_parameter (h, kind);
    case METAFN_IS_EXPLICIT_OBJECT_PARAMETER:
      return eval_is_explicit_object_parameter (h, kind);
    case METAFN_HAS_DEFAULT_ARGUMENT:
      return eval_has_default_argument (h, kind);
    case METAFN_HAS_ELLIPSIS_PARAMETER:
      return eval_has_ellipsis_parameter (h);
    case METAFN_IS_TEMPLATE:
      return eval_is_template (h);
    case METAFN_IS_FUNCTION_TEMPLATE:
      return eval_is_function_template (h);
    case METAFN_IS_VARIABLE_TEMPLATE:
      return eval_is_variable_template (h);
    case METAFN_IS_CLASS_TEMPLATE:
      return eval_is_class_template (h);
    case METAFN_IS_ALIAS_TEMPLATE:
      return eval_is_alias_template (h);
    case METAFN_IS_CONVERSION_FUNCTION_TEMPLATE:
      return eval_is_conversion_function_template (h);
    case METAFN_IS_OPERATOR_FUNCTION_TEMPLATE:
      return eval_is_operator_function_template (h);
    case METAFN_IS_LITERAL_OPERATOR_TEMPLATE:
      return eval_is_literal_operator_template (h);
    case METAFN_IS_CONSTRUCTOR_TEMPLATE:
      return eval_is_constructor_template (h);
    case METAFN_IS_CONCEPT:
      return eval_is_concept (h);
    case METAFN_IS_VALUE:
      return eval_is_value (kind);
    case METAFN_IS_OBJECT:
      return eval_is_object (kind);
    case METAFN_IS_STRUCTURED_BINDING:
      return eval_is_structured_binding (h, kind);
    case METAFN_IS_CLASS_MEMBER:
      return eval_is_class_member (h);
    case METAFN_IS_NAMESPACE_MEMBER:
      return eval_is_namespace_member (h);
    case METAFN_IS_NONSTATIC_DATA_MEMBER:
      return eval_is_nonstatic_data_member (h);
    case METAFN_IS_STATIC_MEMBER:
      return eval_is_static_member (h);
    case METAFN_IS_BASE:
      return eval_is_base (h, kind);
    case METAFN_HAS_DEFAULT_MEMBER_INITIALIZER:
      return eval_has_default_member_initializer (h);
    case METAFN_HAS_PARENT:
      return eval_has_parent (h, kind);
    case METAFN_PARENT_OF:
      return eval_parent_of (loc, ctx, h, kind, non_constant_p, jump_target,
			     fun);
    case METAFN_DEALIAS:
      return eval_dealias (loc, h, kind);
    case METAFN_HAS_TEMPLATE_ARGUMENTS:
      return eval_has_template_arguments (h);
    case METAFN_TEMPLATE_OF:
      return eval_template_of (loc, ctx, h, non_constant_p, jump_target, fun);
    case METAFN_TEMPLATE_ARGUMENTS_OF:
      return eval_template_arguments_of (loc, ctx, h, non_constant_p,
					 jump_target, fun);
    case METAFN_PARAMETERS_OF:
      return eval_parameters_of (loc, ctx, h, non_constant_p, jump_target,
				 fun);
    case METAFN_VARIABLE_OF:
      return eval_variable_of (loc, ctx, h, kind, non_constant_p, jump_target,
			       fun);
    case METAFN_RETURN_TYPE_OF:
      return eval_return_type_of (loc, ctx, h, kind, non_constant_p,
				  jump_target, fun);
    case METAFN_IS_ACCESSIBLE:
      return eval_is_accessible (loc, ctx, h, kind, expr, call,
				 non_constant_p, jump_target, fun);
    case METAFN_HAS_INACCESSIBLE_NONSTATIC_DATA_MEMBERS:
      return eval_has_inaccessible_nonstatic_data_members (loc, ctx, h, expr,
							   call,
							   non_constant_p,
							   jump_target, fun);
    case METAFN_HAS_INACCESSIBLE_BASES:
      return eval_has_inaccessible_bases (loc, ctx, h, expr, call,
					  non_constant_p, jump_target, fun);
    case METAFN_HAS_INACCESSIBLE_SUBOBJECTS:
      return eval_has_inaccessible_subobjects (loc, ctx, h, expr, call,
					       non_constant_p, jump_target,
					       fun);
    case METAFN_MEMBERS_OF:
      return eval_members_of (loc, ctx, h, expr, call, non_constant_p,
			      jump_target, fun);
    case METAFN_BASES_OF:
      return eval_bases_of (loc, ctx, h, expr, call, non_constant_p,
			    jump_target, fun);
    case METAFN_STATIC_DATA_MEMBERS_OF:
      return eval_static_data_members_of (loc, ctx, h, expr, call,
					  non_constant_p, jump_target,
					  fun);
    case METAFN_NONSTATIC_DATA_MEMBERS_OF:
      return eval_nonstatic_data_members_of (loc, ctx, h, expr, call,
					     non_constant_p, jump_target,
					     fun);
    case METAFN_SUBOBJECTS_OF:
      return eval_subobjects_of (loc, ctx, h, expr, call, non_constant_p,
				 jump_target, fun);
    case METAFN_ENUMERATORS_OF:
      return eval_enumerators_of (loc, ctx, h, non_constant_p, jump_target,
				  fun);
    case METAFN_OFFSET_OF:
      return eval_offset_of (loc, ctx, h, kind, TREE_TYPE (call),
			     non_constant_p, jump_target, fun);
    case METAFN_SIZE_OF:
      return eval_size_of (loc, ctx, h, kind, TREE_TYPE (call), non_constant_p,
			   jump_target, fun);
    case METAFN_ALIGNMENT_OF:
      return eval_alignment_of (loc, ctx, h, kind, TREE_TYPE (call),
				non_constant_p, jump_target, fun);
    case METAFN_BIT_SIZE_OF:
      return eval_bit_size_of (loc, ctx, h, kind, TREE_TYPE (call),
			       non_constant_p, jump_target, fun);
    case METAFN_EXTRACT:
      {
	type = TREE_VEC_ELT (get_template_innermost_arguments (fun), 0);
	return eval_extract (loc, ctx, type, h, kind, non_constant_p,
			     overflow_p, jump_target, fun);
      }
    case METAFN_CAN_SUBSTITUTE:
      return eval_can_substitute (loc, ctx, h, hvec, non_constant_p,
				  jump_target, fun);
    case METAFN_SUBSTITUTE:
      return eval_substitute (loc, ctx, h, hvec, non_constant_p, jump_target,
			      fun);
    case METAFN_REFLECT_CONSTANT:
      return eval_reflect_constant (loc, ctx, type, expr, non_constant_p,
				    jump_target, fun);
    case METAFN_REFLECT_OBJECT:
      return eval_reflect_object (loc, ctx, type, expr, non_constant_p,
				  jump_target, fun);
    case METAFN_REFLECT_FUNCTION:
      return eval_reflect_function (loc, ctx, type, expr, non_constant_p,
				    jump_target, fun);
    case METAFN_REFLECT_CONSTANT_STRING:
      return eval_reflect_constant_string (loc, ctx, call, non_constant_p,
					   overflow_p, jump_target, fun);
    case METAFN_REFLECT_CONSTANT_ARRAY:
      return eval_reflect_constant_array (loc, ctx, call, non_constant_p,
					  overflow_p, jump_target, fun);
    case METAFN_DATA_MEMBER_SPEC:
      return eval_data_member_spec (loc, ctx, h, expr, non_constant_p,
				    overflow_p, jump_target, fun);
    case METAFN_IS_DATA_MEMBER_SPEC:
      return eval_is_data_member_spec (h, kind);
    case METAFN_DEFINE_AGGREGATE:
      return eval_define_aggregate (loc, ctx, h, hvec, call, non_constant_p);
    case METAFN_IS_VOID_TYPE:
      return eval_is_void_type (h);
    case METAFN_IS_NULL_POINTER_TYPE:
      return eval_is_null_pointer_type (h);
    case METAFN_IS_INTEGRAL_TYPE:
      return eval_is_integral_type (h);
    case METAFN_IS_FLOATING_POINT_TYPE:
      return eval_is_floating_point_type (h);
    case METAFN_IS_ARRAY_TYPE:
      return eval_is_array_type (loc, h);
    case METAFN_IS_POINTER_TYPE:
      return eval_is_pointer_type (loc, h);
    case METAFN_IS_LVALUE_REFERENCE_TYPE:
      return eval_is_lvalue_reference_type (h);
    case METAFN_IS_RVALUE_REFERENCE_TYPE:
      return eval_is_rvalue_reference_type (h);
    case METAFN_IS_MEMBER_OBJECT_POINTER_TYPE:
      return eval_is_member_object_pointer_type (loc, h);
    case METAFN_IS_MEMBER_FUNCTION_POINTER_TYPE:
      return eval_is_member_function_pointer_type (loc, h);
    case METAFN_IS_ENUM_TYPE:
      return eval_is_enum_type (loc, h);
    case METAFN_IS_UNION_TYPE:
      return eval_is_union_type (loc, h);
    case METAFN_IS_CLASS_TYPE:
      return eval_is_class_type (loc, h);
    case METAFN_IS_FUNCTION_TYPE:
      return eval_is_function_type (h);
    case METAFN_IS_REFLECTION_TYPE:
      return eval_is_reflection_type (h);
    case METAFN_IS_REFERENCE_TYPE:
      return eval_is_reference_type (loc, h);
    case METAFN_IS_ARITHMETIC_TYPE:
      return eval_is_arithmetic_type (h);
    case METAFN_IS_FUNDAMENTAL_TYPE:
      return eval_is_fundamental_type (h);
    case METAFN_IS_OBJECT_TYPE:
      return eval_is_object_type (loc, h);
    case METAFN_IS_SCALAR_TYPE:
      return eval_is_scalar_type (h);
    case METAFN_IS_COMPOUND_TYPE:
      return eval_is_compound_type (h);
    case METAFN_IS_MEMBER_POINTER_TYPE:
      return eval_is_member_pointer_type (loc, h);
    case METAFN_IS_CONST_TYPE:
      return eval_is_const_type (h);
    case METAFN_IS_VOLATILE_TYPE:
      return eval_is_volatile_type (h);
    case METAFN_IS_TRIVIALLY_COPYABLE_TYPE:
      return eval_is_trivially_copyable_type (h);
    case METAFN_IS_STANDARD_LAYOUT_TYPE:
      return eval_is_standard_layout_type (h);
    case METAFN_IS_EMPTY_TYPE:
      return eval_is_empty_type (loc, h);
    case METAFN_IS_POLYMORPHIC_TYPE:
      return eval_is_polymorphic_type (loc, h);
    case METAFN_IS_ABSTRACT_TYPE:
      return eval_is_abstract_type (h);
    case METAFN_IS_FINAL_TYPE:
      return eval_is_final_type (loc, h);
    case METAFN_IS_AGGREGATE_TYPE:
      return eval_is_aggregate_type (h);
    case METAFN_IS_CONSTEVAL_ONLY_TYPE:
      return eval_is_consteval_only_type (h);
    case METAFN_IS_SIGNED_TYPE:
      return eval_is_signed_type (h);
    case METAFN_IS_UNSIGNED_TYPE:
      return eval_is_unsigned_type (h);
    case METAFN_IS_BOUNDED_ARRAY_TYPE:
      return eval_is_bounded_array_type (loc, h);
    case METAFN_IS_UNBOUNDED_ARRAY_TYPE:
      return eval_is_unbounded_array_type (h);
    case METAFN_IS_SCOPED_ENUM_TYPE:
      return eval_is_scoped_enum_type (h);
    case METAFN_IS_CONSTRUCTIBLE_TYPE:
      return eval_is_constructible_type (h, hvec);
    case METAFN_IS_DEFAULT_CONSTRUCTIBLE_TYPE:
      return eval_is_default_constructible_type (h);
    case METAFN_IS_COPY_CONSTRUCTIBLE_TYPE:
      return eval_is_copy_constructible_type (h);
    case METAFN_IS_MOVE_CONSTRUCTIBLE_TYPE:
      return eval_is_move_constructible_type (h);
    case METAFN_IS_ASSIGNABLE_TYPE:
      return eval_is_assignable_type (loc, h, h1);
    case METAFN_IS_COPY_ASSIGNABLE_TYPE:
      return eval_is_copy_assignable_type (h);
    case METAFN_IS_MOVE_ASSIGNABLE_TYPE:
      return eval_is_move_assignable_type (h);
    case METAFN_IS_SWAPPABLE_WITH_TYPE:
      return eval_is_swappable_with_type (loc, ctx, h, h1, call,
					  non_constant_p, jump_target, fun,
					  "is_swappable_with");
    case METAFN_IS_SWAPPABLE_TYPE:
      return eval_is_swappable_type (loc, ctx, h, call, non_constant_p,
				     jump_target, fun, "is_swappable");
    case METAFN_IS_DESTRUCTIBLE_TYPE:
      return eval_is_destructible_type (loc, h);
    case METAFN_IS_TRIVIALLY_CONSTRUCTIBLE_TYPE:
      return eval_is_trivially_constructible_type (h, hvec);
    case METAFN_IS_TRIVIALLY_DEFAULT_CONSTRUCTIBLE_TYPE:
      return eval_is_trivially_default_constructible_type (h);
    case METAFN_IS_TRIVIALLY_COPY_CONSTRUCTIBLE_TYPE:
      return eval_is_trivially_copy_constructible_type (h);
    case METAFN_IS_TRIVIALLY_MOVE_CONSTRUCTIBLE_TYPE:
      return eval_is_trivially_move_constructible_type (h);
    case METAFN_IS_TRIVIALLY_ASSIGNABLE_TYPE:
      return eval_is_trivially_assignable_type (loc, h, h1);
    case METAFN_IS_TRIVIALLY_COPY_ASSIGNABLE_TYPE:
      return eval_is_trivially_copy_assignable_type (h);
    case METAFN_IS_TRIVIALLY_MOVE_ASSIGNABLE_TYPE:
      return eval_is_trivially_move_assignable_type (h);
    case METAFN_IS_TRIVIALLY_DESTRUCTIBLE_TYPE:
      return eval_is_trivially_destructible_type (loc, h);
    case METAFN_IS_NOTHROW_CONSTRUCTIBLE_TYPE:
      return eval_is_nothrow_constructible_type (h, hvec);
    case METAFN_IS_NOTHROW_DEFAULT_CONSTRUCTIBLE_TYPE:
      return eval_is_nothrow_default_constructible_type (h);
    case METAFN_IS_NOTHROW_COPY_CONSTRUCTIBLE_TYPE:
      return eval_is_nothrow_copy_constructible_type (h);
    case METAFN_IS_NOTHROW_MOVE_CONSTRUCTIBLE_TYPE:
      return eval_is_nothrow_move_constructible_type (h);
    case METAFN_IS_NOTHROW_ASSIGNABLE_TYPE:
      return eval_is_nothrow_assignable_type (loc, h, h1);
    case METAFN_IS_NOTHROW_COPY_ASSIGNABLE_TYPE:
      return eval_is_nothrow_copy_assignable_type (h);
    case METAFN_IS_NOTHROW_MOVE_ASSIGNABLE_TYPE:
      return eval_is_nothrow_move_assignable_type (h);
    case METAFN_IS_NOTHROW_SWAPPABLE_WITH_TYPE:
      return eval_is_swappable_with_type (loc, ctx, h, h1, call,
					  non_constant_p, jump_target, fun,
					  "is_nothrow_swappable_with");
    case METAFN_IS_NOTHROW_SWAPPABLE_TYPE:
      return eval_is_swappable_type (loc, ctx, h, call, non_constant_p,
				     jump_target, fun, "is_nothrow_swappable");
    case METAFN_IS_NOTHROW_DESTRUCTIBLE_TYPE:
      return eval_is_nothrow_destructible_type (loc, h);
    case METAFN_IS_IMPLICIT_LIFETIME_TYPE:
      return eval_is_implicit_lifetime_type (h);
    case METAFN_HAS_VIRTUAL_DESTRUCTOR:
      return eval_has_virtual_destructor (h);
    case METAFN_HAS_UNIQUE_OBJECT_REPRESENTATIONS:
      return eval_has_unique_object_representations (h);
    case METAFN_REFERENCE_CONSTRUCTS_FROM_TEMPORARY:
      return eval_reference_constructs_from_temporary (loc, h, h1);
    case METAFN_REFERENCE_CONVERTS_FROM_TEMPORARY:
      return eval_reference_converts_from_temporary (loc, h, h1);
    case METAFN_RANK:
      return eval_rank (h);
    case METAFN_EXTENT:
      return eval_extent (loc, h, expr);
    case METAFN_IS_SAME_TYPE:
      return eval_is_same_type (loc, h, h1);
    case METAFN_IS_BASE_OF_TYPE:
      return eval_is_base_of_type (loc, h, h1);
    case METAFN_IS_VIRTUAL_BASE_OF_TYPE:
      return eval_is_virtual_base_of_type (loc, h, h1);
    case METAFN_IS_CONVERTIBLE_TYPE:
      return eval_is_convertible_type (loc, h, h1);
    case METAFN_IS_NOTHROW_CONVERTIBLE_TYPE:
      return eval_is_nothrow_convertible_type (loc, h, h1);
    case METAFN_IS_LAYOUT_COMPATIBLE_TYPE:
      return eval_is_layout_compatible_type (loc, h, h1);
    case METAFN_IS_POINTER_INTERCONVERTIBLE_BASE_OF_TYPE:
      return eval_is_pointer_interconvertible_base_of_type (loc, h, h1);
    case METAFN_IS_INVOCABLE_TYPE:
      return eval_is_invocable_type (loc, h, hvec);
    case METAFN_IS_INVOCABLE_R_TYPE:
      return eval_is_invocable_r_type (loc, ctx, h, h1, hvec, call,
				       non_constant_p, jump_target, fun,
				       "is_invocable_r");
    case METAFN_IS_NOTHROW_INVOCABLE_TYPE:
      return eval_is_nothrow_invocable_type (loc, h, hvec);
    case METAFN_IS_NOTHROW_INVOCABLE_R_TYPE:
      return eval_is_invocable_r_type (loc, ctx, h, h1, hvec, call,
				       non_constant_p, jump_target, fun,
				       "is_nothrow_invocable_r");
    case METAFN_REMOVE_CONST:
      return eval_remove_const (loc, h);
    case METAFN_REMOVE_VOLATILE:
      return eval_remove_volatile (loc, h);
    case METAFN_REMOVE_CV:
      return eval_remove_cv (loc, h);
    case METAFN_ADD_CONST:
      return eval_add_const (loc, h);
    case METAFN_ADD_VOLATILE:
      return eval_add_volatile (loc, h);
    case METAFN_ADD_CV:
      return eval_add_cv (loc, h);
    case METAFN_REMOVE_REFERENCE:
      return eval_remove_reference (loc, h);
    case METAFN_ADD_LVALUE_REFERENCE:
      return eval_add_lvalue_reference (loc, h);
    case METAFN_ADD_RVALUE_REFERENCE:
      return eval_add_rvalue_reference (loc, h);
    case METAFN_MAKE_SIGNED:
      return eval_make_signed (loc, ctx, h, false, non_constant_p, jump_target,
			       fun);
    case METAFN_MAKE_UNSIGNED:
      return eval_make_signed (loc, ctx, h, true, non_constant_p, jump_target,
			       fun);
    case METAFN_REMOVE_EXTENT:
      return eval_remove_extent (loc, h);
    case METAFN_REMOVE_ALL_EXTENTS:
      return eval_remove_all_extents (loc, h);
    case METAFN_REMOVE_POINTER:
      return eval_remove_pointer (loc, h);
    case METAFN_ADD_POINTER:
      return eval_add_pointer (loc, h);
    case METAFN_REMOVE_CVREF:
      return eval_remove_cvref (loc, h);
    case METAFN_DECAY:
      return eval_decay (loc, h);
    case METAFN_COMMON_TYPE:
      return eval_common_type (loc, ctx, hvec, call, non_constant_p,
			       jump_target, fun, ident);
    case METAFN_COMMON_REFERENCE:
      return eval_common_type (loc, ctx, hvec, call, non_constant_p,
			       jump_target, fun, ident);
    case METAFN_UNDERLYING_TYPE:
      return eval_underlying_type (loc, ctx, h, non_constant_p, jump_target,
				   fun);
    case METAFN_INVOKE_RESULT:
      return eval_invoke_result (loc, ctx, h, hvec, call, non_constant_p,
				 jump_target, fun);
    case METAFN_UNWRAP_REFERENCE:
      return eval_unwrap_reference (loc, ctx, h, call, non_constant_p,
				    jump_target, fun, ident);
    case METAFN_UNWRAP_REF_DECAY:
      return eval_unwrap_reference (loc, ctx, h, call, non_constant_p,
				    jump_target, fun, ident);
    case METAFN_TUPLE_SIZE:
      return eval_tuple_size (loc, ctx, h, call, non_constant_p, jump_target,
			      fun);
    case METAFN_TUPLE_ELEMENT:
      return eval_tuple_element (loc, ctx, expr, h1, call,
				 non_constant_p, jump_target, fun);
    case METAFN_VARIANT_SIZE:
      return eval_variant_size (loc, ctx, h, call, non_constant_p,
				jump_target, fun);
    case METAFN_VARIANT_ALTERNATIVE:
      return eval_variant_alternative (loc, ctx, expr, h1, call,
				       non_constant_p, jump_target, fun);
    case METAFN_TYPE_ORDER:
      return eval_type_order (h, h1);
    case METAFN_ANNOTATIONS_OF:
      return eval_annotations_of (loc, ctx, h, kind, NULL_TREE, non_constant_p,
				  jump_target, fun);
    case METAFN_ANNOTATIONS_OF_WITH_TYPE:
      return eval_annotations_of (loc, ctx, h, kind, h1, non_constant_p,
				  jump_target, fun);
    /* Special metafunctions.  */
    case METAFN_ACCESS_CONTEXT_CURRENT:
      if (DECL_CLASS_SCOPE_P (fun)
	  && TYPE_NAME (DECL_CONTEXT (fun))
	  && TREE_CODE (TYPE_NAME (DECL_CONTEXT (fun))) == TYPE_DECL
	  && DECL_NAME (TYPE_NAME (DECL_CONTEXT (fun)))
	  && id_equal (DECL_NAME (TYPE_NAME (DECL_CONTEXT (fun))),
		       "access_context"))
	return eval_access_context_current (loc, ctx, call, non_constant_p);
      goto not_found;
    case METAFN_EXCEPTION__S_EXCEPTION_CVT_TO_UTF8:
    case METAFN_EXCEPTION__S_EXCEPTION_CVT_FROM_UTF8:
      if (DECL_CLASS_SCOPE_P (fun)
	  && TYPE_NAME (DECL_CONTEXT (fun))
	  && TREE_CODE (TYPE_NAME (DECL_CONTEXT (fun))) == TYPE_DECL
	  && DECL_NAME (TYPE_NAME (DECL_CONTEXT (fun)))
	  && id_equal (DECL_NAME (TYPE_NAME (DECL_CONTEXT (fun))),
		       "exception"))
	{
	  bool to_utf8
	    = minfo->code == METAFN_EXCEPTION__S_EXCEPTION_CVT_TO_UTF8;
	  return eval_exception__S_exception_cvt_tofrom_utf8 (loc, ctx, call,
							      non_constant_p,
							      overflow_p,
							      jump_target,
							      fun, to_utf8);
	}
      goto not_found;
    }
  goto not_found;
}

/* Splice reflection REFL; i.e., return its entity.  */

tree
splice (tree refl)
{
  if (refl == error_mark_node)
    return error_mark_node;

  /* Who in the world am I?  That's the great puzzle and we have to wait
     until instantiation to find out.  */
  if (instantiation_dependent_expression_p (refl))
    return build_nt (SPLICE_EXPR, refl);

  /* [basic.splice] "The constant-expression of a splice-specifier shall
     be a converted constant expression of type std::meta::info."  */
  refl = build_converted_constant_expr (meta_info_type_node, refl,
					tf_warning_or_error);

  if (processing_template_decl)
    refl = fold_non_dependent_expr (refl, tf_warning_or_error, true);
  else
    refl = cxx_constant_value (refl);
  if (!REFLECT_EXPR_P (refl))
    /* I don't wanna do your dirty work no more.  */
    return error_mark_node;

  /* We are bringing some entity from the unevaluated expressions world
     to possibly outside of that, mark it used.  */
  if (!mark_used (REFLECT_EXPR_HANDLE (refl)))
    return error_mark_node;

  refl = REFLECT_EXPR_HANDLE (refl);
  /* Function templates are wrapped in OVERLOAD from name lookup
     and a lot of places assume that.  Furthermore, if reflection comes
     from ^^fntmpl, it is wrapped with OVERLOAD already, only when
     it comes from e.g. members_of it is not.  */
  if (DECL_FUNCTION_TEMPLATE_P (refl))
    refl = ovl_make (refl, NULL_TREE);

  return refl;
}

/* A walker for consteval_only_p.  It cannot be a lambda, because we
   have to call this recursively, sigh.  */

static tree
consteval_only_type_r (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;
  /* Types can contain themselves recursively, hence this.  */
  auto visited = static_cast<hash_set<tree> *>(data);

  if (!TYPE_P (t))
    return NULL_TREE;

  if (REFLECTION_TYPE_P (t))
    return t;

  if (typedef_variant_p (t))
    /* Tell cp_walk_subtrees to look through typedefs.  */
    *walk_subtrees = 2;

  if (RECORD_OR_UNION_TYPE_P (t))
    {
      /* Don't walk template arguments; A<info>::type isn't a consteval-only
	 type.  */
      *walk_subtrees = 0;
      /* So we have to walk the fields manually.  */
      for (tree member = TYPE_FIELDS (t);
	   member; member = DECL_CHAIN (member))
	if (TREE_CODE (member) == FIELD_DECL)
	  if (tree r = cp_walk_tree (&TREE_TYPE (member),
				     consteval_only_type_r, visited, visited))
	    return r;
    }

  return NULL_TREE;
}

/* True if T is a consteval-only type as per [basic.types.general]:
   "A type is consteval-only if it is either std::meta::info or a type
   compounded from a consteval-only type", or something that has
   a consteval-only type.  */

bool
consteval_only_p (tree t)
{
  if (!flag_reflection)
    return false;

  if (!TYPE_P (t))
    t = TREE_TYPE (t);

  if (!t)
    return false;

  /* We need the complete type otherwise we'd have no fields for class
     templates and thus come up with zilch for things like
       template<typename T>
       struct X : T { };
     which could be consteval-only, depending on T.  */
  t = complete_type (t);

  /* Classes with std::meta::info members are also consteval-only.  */
  hash_set<tree> visited;
  return !!cp_walk_tree (&t, consteval_only_type_r, &visited, &visited);
}

/* Detect if a consteval-only expression EXPR or a consteval-only
   variable EXPR not declared constexpr/constinit is used outside
   a manifestly constant-evaluated context.  E.g.:

     void f() {
       constexpr auto r = ^^int;  // OK
       [: r :] i = 42;  // still OK
       auto z = r;  // bad
     }

   But

     consteval void g() {
       constexpr auto r = ^^int;
       auto z = r;
     }

   is OK.  If COMPLAIN, emit an error; otherwise we're in the search-only
   mode.  Return true if we found a problematic expression.  */

bool
check_out_of_consteval_use (tree expr, bool complain/*=true*/)
{
  if (!flag_reflection || in_immediate_context ())
    return false;

  auto walker = [](tree *tp, int *walk_subtrees, void *) -> tree
    {
      tree t = *tp;

      /* No need to look into types or unevaluated operands.  */
      if (TYPE_P (t)
	  || unevaluated_p (TREE_CODE (t))
	  /* Don't walk INIT_EXPRs, because we'd emit bogus errors about
	     member initializers.  */
	  || TREE_CODE (t) == INIT_EXPR
	  /* Don't walk BIND_EXPR_VARS.  */
	  || TREE_CODE (t) == BIND_EXPR
	  /* And don't recurse on DECL_EXPRs.  */
	  || TREE_CODE (t) == DECL_EXPR)
	{
	  *walk_subtrees = false;
	  return NULL_TREE;
	}

      /* A subexpression of a manifestly constant-evaluated expression is
	 an immediate function context.  For example,

	   consteval void foo (std::meta::info) { }
	   void g() { foo (^^void); }

	 is all good.  */
      if (tree decl = cp_get_callee_fndecl_nofold (t))
	if (immediate_invocation_p (decl))
	  {
	    *walk_subtrees = false;
	    return NULL_TREE;
	  }

      if (VAR_P (t)
	  && (DECL_DECLARED_CONSTEXPR_P (t) || DECL_DECLARED_CONSTINIT_P (t)))
	/* This is fine, don't bother checking the type.  */
	return NULL_TREE;

      /* Now check the type to see if we are dealing with a consteval-only
	 expression.  */
      if (!consteval_only_p (t))
	return NULL_TREE;

      if (current_function_decl
	  /* Already escalated.  */
	  && (DECL_IMMEDIATE_FUNCTION_P (current_function_decl)
	      /* These functions are magic.  */
	      || is_std_allocator_allocate (current_function_decl)))
	{
	  *walk_subtrees = false;
	  return NULL_TREE;
	}

      /* We might have to escalate if we are in an immediate-escalating
	 function.  */
      if (immediate_escalating_function_p (current_function_decl))
	{
	  promote_function_to_consteval (current_function_decl);
	  *walk_subtrees = false;
	  return NULL_TREE;
	}

      *walk_subtrees = false;
      return t;
    };

  if (tree t = cp_walk_tree_without_duplicates (&expr, walker, nullptr))
    {
      if (complain)
	{
	  if (VAR_P (t))
	    {
	      auto_diagnostic_group d;
	      error_at (cp_expr_loc_or_input_loc (t),
			"consteval-only variable %qD not declared %<constexpr%> "
			"used outside a constant-evaluated context", t);
	      if (TREE_STATIC (t) || CP_DECL_THREAD_LOCAL_P (t))
		inform (DECL_SOURCE_LOCATION (t), "add %<constexpr%> or "
			"%<constinit%>");
	      else
		inform (DECL_SOURCE_LOCATION (t), "add %<constexpr%>");
	    }
	  else
	    error_at (cp_expr_loc_or_input_loc (t),
		      "consteval-only expressions are only allowed in "
		      "a constant-evaluated context");
	}
      return true;
    }

  return false;
}

/* Return true if the reflections LHS and RHS are equal.  */

bool
compare_reflections (tree lhs, tree rhs)
{
  reflect_kind lkind;
  do
    {
      lkind = REFLECT_EXPR_KIND (lhs);
      if (lkind != REFLECT_EXPR_KIND (rhs))
	return false;
      lhs = REFLECT_EXPR_HANDLE (lhs);
      rhs = REFLECT_EXPR_HANDLE (rhs);
    }
  while (REFLECT_EXPR_P (lhs) && REFLECT_EXPR_P (rhs));

  lhs = resolve_nondeduced_context (lhs, tf_warning_or_error);
  rhs = resolve_nondeduced_context (rhs, tf_warning_or_error);

  /* TEMPLATE_DECLs are wrapped in an OVERLOAD.  When we have

       template_of (^^fun_tmpl<int>) == ^^fun_tmpl

     the RHS will be OVERLOAD<TEMPLATE_DECL> but the LHS will
     only be TEMPLATE_DECL.  They should compare equal, though.  */
  // ??? Can we do something better?
  lhs = maybe_get_reflection_fndecl (lhs);
  rhs = maybe_get_reflection_fndecl (rhs);
  if (lkind == REFLECT_PARM)
    {
      lhs = maybe_update_function_parm (lhs);
      rhs = maybe_update_function_parm (rhs);
    }
  else if (lkind == REFLECT_DATA_MEMBER_SPEC)
    return (TREE_VEC_ELT (lhs, 0) == TREE_VEC_ELT (rhs, 0)
	    && TREE_VEC_ELT (lhs, 1) == TREE_VEC_ELT (rhs, 1)
	    && tree_int_cst_equal (TREE_VEC_ELT (lhs, 2),
				   TREE_VEC_ELT (rhs, 2))
	    && tree_int_cst_equal (TREE_VEC_ELT (lhs, 3),
				   TREE_VEC_ELT (rhs, 3))
	    && TREE_VEC_ELT (lhs, 4) == TREE_VEC_ELT (rhs, 4));

  if (lhs == rhs)
    return true;

  /* Some trees are not shared.  */
  if (TREE_CODE (lhs) == TREE_CODE (rhs))
    switch (TREE_CODE (lhs))
      {
      case ARRAY_REF:
      case COMPONENT_REF:
      case REAL_CST:
	return cp_tree_equal (lhs, rhs);
      default:
	break;
      }

  if (TYPE_P (lhs) && TYPE_P (rhs))
    if (!typedef_variant_p (lhs) && !typedef_variant_p (rhs))
      return same_type_p (lhs, rhs);

  return false;
}

/* Return true if T is a valid splice-type-specifier.
   [dcl.type.splice]: For a splice-type-specifier of the form
   "typename[opt] splice-specifier", the splice-specifier shall designate
   a type, a class template, or an alias template.
   For a splice-type-specifier of the form
   "typename[opt] splice-specialization-specifier", the splice-specifier
   of the splice-specialization-specifier shall designate a template T
   that is either a class template or an alias template.  */

bool
valid_splice_type_p (const_tree t)
{
  return TYPE_P (t);
}

/* Return true if T is a valid splice-scope-specifier.
   [basic.lookup.qual.general]: If a splice-scope-specifier is followed
   by a ::, it shall either be a dependent splice-scope-specifier or it
   shall designate a namespace, class, enumeration, or dependent type.  */

bool
valid_splice_scope_p (const_tree t)
{
  return (CLASS_TYPE_P (t)
	  || TREE_CODE (t) == ENUMERAL_TYPE
	  || TREE_CODE (t) == NAMESPACE_DECL);
}

/* Check a function DECL for CWG 3115: Every function of consteval-only
   type shall be an immediate function.  */

void
check_consteval_only_fn (tree decl)
{
  if (!DECL_IMMEDIATE_FUNCTION_P (decl)
      && consteval_only_p (decl)
      /* But if the function can be escalated, merrily we roll along.  */
      && !immediate_escalating_function_p (decl)
      && !is_std_allocator_allocate (decl))
    error_at (DECL_SOURCE_LOCATION (decl),
	      "function of consteval-only type must be declared %qs",
	      "consteval");
}

/* Check if T is a valid result of splice-expression.  ADDRESS_P is true if
   we are taking the address of the splice.  MEMBER_ACCESS_P is true if this
   splice is used in foo.[: bar :] or foo->[: bar :] context.  COMPLAIN_P is
   true if any errors should be emitted.  Returns true is no problems are
   found, false otherwise.  */

bool
check_splice_expr (location_t loc, location_t start_loc, tree t,
		   bool address_p, bool member_access_p, bool complain_p)
{
  /* We may not have gotten an expression.  */
  if (TREE_CODE (t) == TYPE_DECL
      || TREE_CODE (t) == NAMESPACE_DECL
      || TYPE_P (t))
    {
      if (complain_p)
	{
	  if (TYPE_P (t))
	    {
	      auto_diagnostic_group d;
	      error_at (loc, "expected a reflection of an expression instead "
			"of type %qT", t);
	      if (start_loc != UNKNOWN_LOCATION)
		{
		  rich_location richloc (line_table, start_loc);
		  richloc.add_fixit_insert_before (start_loc, "typename");
		  inform (&richloc, "add %<typename%> to denote a type "
			  "outside a type-only context");
		}
	      else
		inform (loc, "add %<typename%> to denote a type outside "
			"a type-only context");
	    }
	  else
	    error_at (loc, "expected a reflection of an expression instead "
		      "of %qD", t);
	}
      return false;
    }
  /* [expr.prim.splice]/2 For a splice-expression of the form
     splice-specifier, the expression is ill-formed if it is:  */
  /* -- a constructor or a destructor  */
  if (TREE_CODE (t) == FUNCTION_DECL
      && (DECL_CONSTRUCTOR_P (t) || DECL_DESTRUCTOR_P (t)))
    {
      if (complain_p)
	error_at (loc, "cannot use constructor or destructor %qD in a splice "
		  "expression", t);
      return false;
    }
  /* -- an unnamed bit-field  */
  if (TREE_CODE (t) == FIELD_DECL && DECL_UNNAMED_BIT_FIELD (t))
    {
      if (complain_p)
	error_at (loc, "cannot use an unnamed bit-field %qD in a splice "
		  "expression", t);
      return false;
    }
  /* Class members may not be implicitly referenced through a splice.
     But taking the address is fine, and so is class member access a la
     foo.[: ^^S::bar :].  */
  if (!address_p
      && !member_access_p
      && DECL_P (t)
      && DECL_NONSTATIC_MEMBER_P (t))
    {
      if (complain_p)
	error_at (loc, "cannot implicitly reference a class member %qD "
		  "through a splice", t);
      return false;
    }
  /* [expr.unary.op]/3.1 "If the operand [of unary &] is a qualified-id or
     splice-expression designating a non-static member m, other than an
     explicit object member function, m shall be a direct member of some
     class C that is not an anonymous union."  */
  if (address_p
      && TREE_CODE (t) == FIELD_DECL
      && ANON_UNION_TYPE_P (DECL_CONTEXT (t))
      && !TYPE_P (context_for_name_lookup (t)))
    {
      if (complain_p)
	error_at (loc, "unary %<&%> applied to an anonymous union member "
		       "%qD that is not a direct member of a named class", t);
      return false;
    }

  /* [expr.prim.splice]/2: "The expression is ill-formed if S [the construct
     designated by splice-specifier] is
     -- a local entity such that there is a lambda scope that intervenes
     between the expression and the point at which S was introduced"
     This also checks ODR violations (reflect/odr1.C).  */
  if (outer_automatic_var_p (t)
      && process_outer_var_ref (t, tf_none) == error_mark_node)
    {
      /* Not letting process_outer_var_ref emit the error so that we can
	 say "in a splice expression".  */
      if (complain_p)
	{
	  auto_diagnostic_group d;
	  error_at (loc, "use of local variable with automatic storage from "
		    "containing function in a splice expression");
	  inform (DECL_SOURCE_LOCATION (t), "%q#D declared here", t);
	}
      return false;
    }

  /* If we had a reflect_kind here, we could just check for
     REFLECT_ANNOTATION and be done with it.  But we don't have it yet (TODO),
     so do it the suboptimal way.  */
  if (TREE_CODE (t) == TREE_LIST && annotation_p (t))
    {
      if (complain_p)
	error_at (loc, "cannot use an annotation %qE in a splice expression",
		  t);
      return false;
    }

  /* Same, but with REFLECT_DATA_MEMBER_SPEC.  */
  if (TREE_CODE (t) == TREE_VEC)
    {
      if (complain_p)
	error_at (loc, "cannot use a data member specification in a "
		  "splice expression");
      return false;
    }

  return true;
}

/* Create a new SPLICE_SCOPE tree.  EXPR is its SPLICE_SCOPE_EXPR, and
   TYPE_P says if it should have SPLICE_SCOPE_TYPE_P set.  */

tree
make_splice_scope (tree expr, bool type_p)
{
  tree t = cxx_make_type (SPLICE_SCOPE);
  SPLICE_SCOPE_EXPR (t) = expr;
  SPLICE_SCOPE_TYPE_P (t) = type_p;
  return t;
}

/* Return true if T is a splice expression; that is, it is either [:T:] or
   [:T:]<arg>.  */

bool
dependent_splice_p (const_tree t)
{
  return (TREE_CODE (t) == SPLICE_EXPR
	  || (TREE_CODE (t) == TEMPLATE_ID_EXPR
	      && TREE_CODE (TREE_OPERAND (t, 0)) == SPLICE_EXPR));
}

/* Annotation index for mangling.  */

static GTY(()) int annotation_idx;

/* Helper function for mangle.cc (write_reflection).
   Determine 2 letter mangling prefix and store it into prefix.
   Additionally return the reflection handle possibly adjusted so that
   write_reflection can mangle the operands of it if any are needed.  */

tree
reflection_mangle_prefix (tree refl, char prefix[3])
{
  tree h = REFLECT_EXPR_HANDLE (refl);
  reflect_kind kind = REFLECT_EXPR_KIND (refl);
  if (h == unknown_type_node)
    {
      strcpy (prefix, "nu");
      return NULL_TREE;
    }
  if (eval_is_value (kind) == boolean_true_node)
    {
      strcpy (prefix, "vl");
      if (VAR_P (h) && DECL_NTTP_OBJECT_P (h))
	h = tparm_object_argument (h);
      return h;
    }
  if (eval_is_object (kind) == boolean_true_node)
    {
      strcpy (prefix, "ob");
      return h;
    }
  if (eval_is_variable (h, kind) == boolean_true_node)
    {
      strcpy (prefix, "vr");
      return h;
    }
  if (eval_is_structured_binding (h, kind) == boolean_true_node)
    {
      strcpy (prefix, "sb");
      return h;
    }
  if (eval_is_function (h) == boolean_true_node)
    {
      strcpy (prefix, "fn");
      return maybe_get_reflection_fndecl (h);
    }
  if (eval_is_function_parameter (h, kind) == boolean_true_node)
    {
      strcpy (prefix, "pa");
      return maybe_update_function_parm (h);
    }
  if (eval_is_enumerator (h) == boolean_true_node)
    {
      strcpy (prefix, "en");
      return h;
    }
  if (eval_is_annotation (h, kind) == boolean_true_node)
    {
      strcpy (prefix, "an");
      if (TREE_PURPOSE (TREE_VALUE (h)) == NULL_TREE)
	TREE_PURPOSE (TREE_VALUE (h))
	  = build_int_cst (integer_type_node, annotation_idx++);
      return TREE_PURPOSE (TREE_VALUE (h));
    }
  if (eval_is_type_alias (h) == boolean_true_node)
    {
      strcpy (prefix, "ta");
      return h;
    }
  if (eval_is_type (h) == boolean_true_node)
    {
      strcpy (prefix, "ty");
      return h;
    }
  if (eval_is_nonstatic_data_member (h) == boolean_true_node)
    {
      strcpy (prefix, "dm");
      return h;
    }
  if (TREE_CODE (h) == FIELD_DECL && DECL_UNNAMED_BIT_FIELD (h))
    {
      strcpy (prefix, "un");
      return h;
    }
  if (eval_is_class_template (h) == boolean_true_node)
    {
      strcpy (prefix, "ct");
      return h;
    }
  if (eval_is_function_template (h) == boolean_true_node)
    {
      strcpy (prefix, "ft");
      h = maybe_get_reflection_fndecl (h);
      return h;
    }
  if (eval_is_variable_template (h) == boolean_true_node)
    {
      strcpy (prefix, "vt");
      return h;
    }
  if (eval_is_alias_template (h) == boolean_true_node)
    {
      strcpy (prefix, "at");
      return h;
    }
  if (eval_is_concept (h) == boolean_true_node)
    {
      strcpy (prefix, "co");
      return h;
    }
  if (eval_is_namespace_alias (h) == boolean_true_node)
    {
      strcpy (prefix, "na");
      return h;
    }
  if (eval_is_namespace (h) == boolean_true_node)
    {
      if (h == global_namespace)
	{
	  strcpy (prefix, "ng");
	  return NULL_TREE;
	}
      strcpy (prefix, "ns");
      return h;
    }
  if (eval_is_base (h, kind) == boolean_true_node)
    {
      strcpy (prefix, "ba");
      return h;
    }
  if (eval_is_data_member_spec (h, kind) == boolean_true_node)
    {
      strcpy (prefix, "ds");
      return h;
    }
  gcc_unreachable ();
}

#include "gt-cp-reflect.h"
