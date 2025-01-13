/* Perform -*- C++ -*- constant expression evaluation, including calls to
   constexpr functions.  These routines are used both during actual parsing
   and during the instantiation of template functions.

   Copyright (C) 1998-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "varasm.h"
#include "c-family/c-objc.h"
#include "tree-iterator.h"
#include "gimplify.h"
#include "builtins.h"
#include "tree-inline.h"
#include "ubsan.h"
#include "timevar.h"
#include "fold-const-call.h"
#include "stor-layout.h"
#include "cgraph.h"
#include "opts.h"
#include "stringpool.h"
#include "attribs.h"
#include "fold-const.h"
#include "intl.h"
#include "toplev.h"

static bool verify_constant (tree, bool, bool *, bool *);
#define VERIFY_CONSTANT(X)						\
do {									\
  if (verify_constant ((X), ctx->quiet, non_constant_p, overflow_p)) \
    return t;								\
 } while (0)

static HOST_WIDE_INT find_array_ctor_elt (tree ary, tree dindex,
					  bool insert = false);
static int array_index_cmp (tree key, tree index);

/* Returns true iff FUN is an instantiation of a constexpr function
   template or a defaulted constexpr function.  */

bool
is_instantiation_of_constexpr (tree fun)
{
  return ((DECL_TEMPLOID_INSTANTIATION (fun)
	   && DECL_DECLARED_CONSTEXPR_P (DECL_TI_TEMPLATE (fun)))
	  || (DECL_DEFAULTED_FN (fun)
	      && DECL_DECLARED_CONSTEXPR_P (fun)));
}

/* Return true if T is a literal type.   */

bool
literal_type_p (tree t)
{
  if (SCALAR_TYPE_P (t)
      || VECTOR_TYPE_P (t)
      || TYPE_REF_P (t)
      || (VOID_TYPE_P (t) && cxx_dialect >= cxx14))
    return true;
  if (CLASS_TYPE_P (t))
    {
      t = complete_type (t);
      gcc_assert (COMPLETE_TYPE_P (t) || errorcount);
      return CLASSTYPE_LITERAL_P (t);
    }
  if (TREE_CODE (t) == ARRAY_TYPE)
    return literal_type_p (strip_array_types (t));
  return false;
}

/* If DECL is a variable declared `constexpr', require its type
   be literal.  Return error_mark_node if we give an error, the
   DECL otherwise.  */

tree
ensure_literal_type_for_constexpr_object (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (VAR_P (decl)
      && (DECL_DECLARED_CONSTEXPR_P (decl)
	  || var_in_constexpr_fn (decl))
      && !processing_template_decl)
    {
      tree stype = strip_array_types (type);
      if (CLASS_TYPE_P (stype) && !COMPLETE_TYPE_P (complete_type (stype)))
	/* Don't complain here, we'll complain about incompleteness
	   when we try to initialize the variable.  */;
      else if (!literal_type_p (type))
	{
	  if (DECL_DECLARED_CONSTEXPR_P (decl))
	    {
	      auto_diagnostic_group d;
	      error_at (DECL_SOURCE_LOCATION (decl),
			"the type %qT of %<constexpr%> variable %qD "
			"is not literal", type, decl);
	      explain_non_literal_class (type);
	      decl = error_mark_node;
	    }
	  else if (cxx_dialect < cxx23)
	    {
	      if (!is_instantiation_of_constexpr (current_function_decl))
		{
		  auto_diagnostic_group d;
		  error_at (DECL_SOURCE_LOCATION (decl),
			    "variable %qD of non-literal type %qT in "
			    "%<constexpr%> function only available with "
			    "%<-std=c++2b%> or %<-std=gnu++2b%>", decl, type);
		  explain_non_literal_class (type);
		  decl = error_mark_node;
		}
	      cp_function_chain->invalid_constexpr = true;
	    }
	}
      else if (DECL_DECLARED_CONSTEXPR_P (decl)
	       && variably_modified_type_p (type, NULL_TREE))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "%<constexpr%> variable %qD has variably-modified "
		    "type %qT", decl, type);
	  decl = error_mark_node;
	}
    }
  return decl;
}

/* Issue a diagnostic with text GMSGID for constructs that are invalid in
   constexpr functions.  CONSTEXPR_FUNDEF_P is true if we're checking
   a constexpr function body; if so, don't report hard errors and issue
   a pedwarn pre-C++23, or a warning in C++23, if requested by
   -Winvalid-constexpr.  Otherwise, we're not in the context where we are
   checking if a function can be marked 'constexpr', so give a hard error.  */

ATTRIBUTE_GCC_DIAG(3,4)
static bool
constexpr_error (location_t location, bool constexpr_fundef_p,
		 const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);
  va_start (ap, gmsgid);
  bool ret;
  if (!constexpr_fundef_p)
    {
      /* Report an error that cannot be suppressed.  */
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, DK_ERROR);
      ret = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  else if (warn_invalid_constexpr)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
			   cxx_dialect < cxx23 ? DK_PEDWARN : DK_WARNING);
      diagnostic.option_id = OPT_Winvalid_constexpr;
      ret = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  else
    ret = false;
  va_end (ap);
  return ret;
}

struct constexpr_fundef_hasher : ggc_ptr_hash<constexpr_fundef>
{
  static hashval_t hash (const constexpr_fundef *);
  static bool equal (const constexpr_fundef *, const constexpr_fundef *);
};

/* This table holds all constexpr function definitions seen in
   the current translation unit.  */

static GTY (()) hash_table<constexpr_fundef_hasher> *constexpr_fundef_table;

/* Utility function used for managing the constexpr function table.
   Return true if the entries pointed to by P and Q are for the
   same constexpr function.  */

inline bool
constexpr_fundef_hasher::equal (const constexpr_fundef *lhs,
				const constexpr_fundef *rhs)
{
  return lhs->decl == rhs->decl;
}

/* Utility function used for managing the constexpr function table.
   Return a hash value for the entry pointed to by Q.  */

inline hashval_t
constexpr_fundef_hasher::hash (const constexpr_fundef *fundef)
{
  return DECL_UID (fundef->decl);
}

/* Return a previously saved definition of function FUN.   */

constexpr_fundef *
retrieve_constexpr_fundef (tree fun)
{
  if (constexpr_fundef_table == NULL)
    return NULL;

  constexpr_fundef fundef = { fun, NULL_TREE, NULL_TREE, NULL_TREE };
  return constexpr_fundef_table->find (&fundef);
}

/* Check whether the parameter and return types of FUN are valid for a
   constexpr function, and complain if COMPLAIN.  */

bool
is_valid_constexpr_fn (tree fun, bool complain)
{
  bool ret = true;

  if (DECL_INHERITED_CTOR (fun)
      && TREE_CODE (fun) == TEMPLATE_DECL)
    {
      ret = false;
      if (complain)
	error ("inherited constructor %qD is not %<constexpr%>",
	       DECL_INHERITED_CTOR (fun));
    }
  else
    {
      for (tree parm = FUNCTION_FIRST_USER_PARM (fun);
	   parm != NULL_TREE; parm = TREE_CHAIN (parm))
	if (!literal_type_p (TREE_TYPE (parm)))
	  {
	    ret = false;
	    if (complain)
	      {
		auto_diagnostic_group d;
		if (constexpr_error (input_location, /*constexpr_fundef_p*/true,
				     "invalid type for parameter %d of "
				     "%<constexpr%> function %q+#D",
				     DECL_PARM_INDEX (parm), fun))
		  explain_non_literal_class (TREE_TYPE (parm));
	      }
	  }
    }

  if (LAMBDA_TYPE_P (CP_DECL_CONTEXT (fun)) && cxx_dialect < cxx17)
    {
      ret = false;
      if (complain)
	inform (DECL_SOURCE_LOCATION (fun),
		"lambdas are implicitly %<constexpr%> only in C++17 and later");
    }
  else if (DECL_DESTRUCTOR_P (fun) && cxx_dialect < cxx20)
    {
      ret = false;
      if (complain)
	error_at (DECL_SOURCE_LOCATION (fun),
		  "%<constexpr%> destructors only available with "
		  "%<-std=c++20%> or %<-std=gnu++20%>");
    }
  else if (!DECL_CONSTRUCTOR_P (fun) && !DECL_DESTRUCTOR_P (fun))
    {
      tree rettype = TREE_TYPE (TREE_TYPE (fun));
      if (!literal_type_p (rettype))
	{
	  ret = false;
	  if (complain)
	    {
	      auto_diagnostic_group d;
	      if (constexpr_error (input_location, /*constexpr_fundef_p*/true,
				   "invalid return type %qT of %<constexpr%> "
				   "function %q+D", rettype, fun))
		explain_non_literal_class (rettype);
	    }
	}

      /* C++14 DR 1684 removed this restriction.  */
      if (cxx_dialect < cxx14
	  && DECL_IOBJ_MEMBER_FUNCTION_P (fun)
	  && !CLASSTYPE_LITERAL_P (DECL_CONTEXT (fun)))
	{
	  ret = false;
	  if (complain)
	    {
	      auto_diagnostic_group d;
	      if (pedwarn (DECL_SOURCE_LOCATION (fun), OPT_Wpedantic,
			     "enclosing class of %<constexpr%> non-static"
			     " member function %q+#D is not a literal type",
			     fun))
		explain_non_literal_class (DECL_CONTEXT (fun));
	    }
	}
    }
  else if (CLASSTYPE_VBASECLASSES (DECL_CONTEXT (fun)))
    {
      ret = false;
      if (complain)
	error ("%q#T has virtual base classes", DECL_CONTEXT (fun));
    }

  return ret;
}

/* Subroutine of build_data_member_initialization.  MEMBER is a COMPONENT_REF
   for a member of an anonymous aggregate, INIT is the initializer for that
   member, and VEC_OUTER is the vector of constructor elements for the class
   whose constructor we are processing.  Add the initializer to the vector
   and return true to indicate success.  */

static bool
build_anon_member_initialization (tree member, tree init,
				  vec<constructor_elt, va_gc> **vec_outer)
{
  /* MEMBER presents the relevant fields from the inside out, but we need
     to build up the initializer from the outside in so that we can reuse
     previously built CONSTRUCTORs if this is, say, the second field in an
     anonymous struct.  So we use a vec as a stack.  */
  auto_vec<tree, 2> fields;
  do
    {
      fields.safe_push (TREE_OPERAND (member, 1));
      member = TREE_OPERAND (member, 0);
    }
  while (ANON_AGGR_TYPE_P (TREE_TYPE (member))
	 && TREE_CODE (member) == COMPONENT_REF);

  /* VEC has the constructor elements vector for the context of FIELD.
     If FIELD is an anonymous aggregate, we will push inside it.  */
  vec<constructor_elt, va_gc> **vec = vec_outer;
  tree field;
  while (field = fields.pop(),
	 ANON_AGGR_TYPE_P (TREE_TYPE (field)))
    {
      tree ctor;
      /* If there is already an outer constructor entry for the anonymous
	 aggregate FIELD, use it; otherwise, insert one.  */
      if (vec_safe_is_empty (*vec)
	  || (*vec)->last().index != field)
	{
	  ctor = build_constructor (TREE_TYPE (field), NULL);
	  CONSTRUCTOR_APPEND_ELT (*vec, field, ctor);
	}
      else
	ctor = (*vec)->last().value;
      vec = &CONSTRUCTOR_ELTS (ctor);
    }

  /* Now we're at the innermost field, the one that isn't an anonymous
     aggregate.  Add its initializer to the CONSTRUCTOR and we're done.  */
  gcc_assert (fields.is_empty());
  CONSTRUCTOR_APPEND_ELT (*vec, field, init);

  return true;
}

/* Subroutine of  build_constexpr_constructor_member_initializers.
   The expression tree T represents a data member initialization
   in a (constexpr) constructor definition.  Build a pairing of
   the data member with its initializer, and prepend that pair
   to the existing initialization pair INITS.  */

static bool
build_data_member_initialization (tree t, vec<constructor_elt, va_gc> **vec)
{
  tree member, init;
  if (TREE_CODE (t) == CLEANUP_POINT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (t == error_mark_node)
    return false;
  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      for (tree stmt : tsi_range (t))
	if (! build_data_member_initialization (stmt, vec))
	  return false;
      return true;
    }
  if (TREE_CODE (t) == CLEANUP_STMT)
    {
      /* We can't see a CLEANUP_STMT in a constructor for a literal class,
	 but we can in a constexpr constructor for a non-literal class.  Just
	 ignore it; either all the initialization will be constant, in which
	 case the cleanup can't run, or it can't be constexpr.
	 Still recurse into CLEANUP_BODY.  */
      return build_data_member_initialization (CLEANUP_BODY (t), vec);
    }
  if (TREE_CODE (t) == CONVERT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == INIT_EXPR
      /* vptr initialization shows up as a MODIFY_EXPR.  In C++14 we only
	 use what this function builds for cx_check_missing_mem_inits, and
	 assignment in the ctor body doesn't count.  */
      || (cxx_dialect < cxx14 && TREE_CODE (t) == MODIFY_EXPR))
    {
      member = TREE_OPERAND (t, 0);
      init = break_out_target_exprs (TREE_OPERAND (t, 1));
    }
  else if (TREE_CODE (t) == CALL_EXPR)
    {
      tree fn = get_callee_fndecl (t);
      if (!fn || !DECL_CONSTRUCTOR_P (fn))
	/* We're only interested in calls to subobject constructors.  */
	return true;
      member = CALL_EXPR_ARG (t, 0);
      /* We don't use build_cplus_new here because it complains about
	 abstract bases.  Leaving the call unwrapped means that it has the
	 wrong type, but cxx_eval_constant_expression doesn't care.  */
      init = break_out_target_exprs (t);
    }
  else if (TREE_CODE (t) == BIND_EXPR)
    return build_data_member_initialization (BIND_EXPR_BODY (t), vec);
  else
    /* Don't add anything else to the CONSTRUCTOR.  */
    return true;
  if (INDIRECT_REF_P (member))
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == NOP_EXPR)
    {
      tree op = member;
      STRIP_NOPS (op);
      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (TREE_TYPE (op)),
		       TREE_TYPE (TREE_TYPE (member))));
	  /* Initializing a cv-qualified member; we need to look through
	     the const_cast.  */
	  member = op;
	}
      else if (op == current_class_ptr
	       && (same_type_ignoring_top_level_qualifiers_p
		   (TREE_TYPE (TREE_TYPE (member)),
		    current_class_type)))
	/* Delegating constructor.  */
	member = op;
      else
	{
	  /* This is an initializer for an empty base; keep it for now so
	     we can check it in cxx_eval_bare_aggregate.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (member))));
	}
    }
  if (TREE_CODE (member) == ADDR_EXPR)
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == COMPONENT_REF)
    {
      tree aggr = TREE_OPERAND (member, 0);
      if (TREE_CODE (aggr) == VAR_DECL)
	/* Initializing a local variable, don't add anything.  */
	return true;
      if (TREE_CODE (aggr) != COMPONENT_REF)
	/* Normal member initialization.  */
	member = TREE_OPERAND (member, 1);
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (aggr)))
	/* Initializing a member of an anonymous union.  */
	return build_anon_member_initialization (member, init, vec);
      else
	/* We're initializing a vtable pointer in a base.  Leave it as
	   COMPONENT_REF so we remember the path to get to the vfield.  */
	gcc_assert (TREE_TYPE (member) == vtbl_ptr_type_node);
    }

  /* Value-initialization can produce multiple initializers for the
     same field; use the last one.  */
  if (!vec_safe_is_empty (*vec) && (*vec)->last().index == member)
    (*vec)->last().value = init;
  else
    CONSTRUCTOR_APPEND_ELT (*vec, member, init);
  return true;
}

/* Subroutine of check_constexpr_ctor_body_1 and constexpr_fn_retval.
   In C++11 mode checks that the TYPE_DECLs in the BIND_EXPR_VARS of a
   BIND_EXPR conform to 7.1.5/3/4 on typedef and alias declarations.  */

static bool
check_constexpr_bind_expr_vars (tree t)
{
  gcc_assert (TREE_CODE (t) == BIND_EXPR);

  for (tree var = BIND_EXPR_VARS (t); var; var = DECL_CHAIN (var))
    if (TREE_CODE (var) == TYPE_DECL
	&& DECL_IMPLICIT_TYPEDEF_P (var)
	&& !LAMBDA_TYPE_P (TREE_TYPE (var)))
      return false;
  return true;
}

/* Subroutine of check_constexpr_ctor_body.  */

static bool
check_constexpr_ctor_body_1 (tree last, tree list)
{
  switch (TREE_CODE (list))
    {
    case DECL_EXPR:
      if (TREE_CODE (DECL_EXPR_DECL (list)) == USING_DECL
	  || TREE_CODE (DECL_EXPR_DECL (list)) == TYPE_DECL)
	return true;
      return false;

    case CLEANUP_POINT_EXPR:
      return check_constexpr_ctor_body (last, TREE_OPERAND (list, 0),
					/*complain=*/false);

    case BIND_EXPR:
       if (!check_constexpr_bind_expr_vars (list)
	   || !check_constexpr_ctor_body (last, BIND_EXPR_BODY (list),
					  /*complain=*/false))
	 return false;
       return true;

    case USING_STMT:
    case STATIC_ASSERT:
    case DEBUG_BEGIN_STMT:
      return true;

    default:
      return false;
    }
}

/* Make sure that there are no statements after LAST in the constructor
   body represented by LIST.  */

bool
check_constexpr_ctor_body (tree last, tree list, bool complain)
{
  /* C++14 doesn't require a constexpr ctor to have an empty body.  */
  if (cxx_dialect >= cxx14)
    return true;

  bool ok = true;
  if (TREE_CODE (list) == STATEMENT_LIST)
    {
      tree_stmt_iterator i = tsi_last (list);
      for (; !tsi_end_p (i); tsi_prev (&i))
	{
	  tree t = tsi_stmt (i);
	  if (t == last)
	    break;
	  if (!check_constexpr_ctor_body_1 (last, t))
	    {
	      ok = false;
	      break;
	    }
	}
    }
  else if (list != last
	   && !check_constexpr_ctor_body_1 (last, list))
    ok = false;
  if (!ok)
    {
      if (complain)
	error ("%<constexpr%> constructor does not have empty body");
      DECL_DECLARED_CONSTEXPR_P (current_function_decl) = false;
    }
  return ok;
}

/* V is a vector of constructor elements built up for the base and member
   initializers of a constructor for TYPE.  They need to be in increasing
   offset order, which they might not be yet if TYPE has a primary base
   which is not first in the base-clause or a vptr and at least one base
   all of which are non-primary.  */

static vec<constructor_elt, va_gc> *
sort_constexpr_mem_initializers (tree type, vec<constructor_elt, va_gc> *v)
{
  tree pri = CLASSTYPE_PRIMARY_BINFO (type);
  tree field_type;
  unsigned i;
  constructor_elt *ce;

  if (pri)
    field_type = BINFO_TYPE (pri);
  else if (TYPE_CONTAINS_VPTR_P (type))
    field_type = vtbl_ptr_type_node;
  else
    return v;

  /* Find the element for the primary base or vptr and move it to the
     beginning of the vec.  */
  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    if (TREE_TYPE (ce->index) == field_type)
      break;

  if (i > 0 && i < vec_safe_length (v))
    {
      vec<constructor_elt, va_gc> &vref = *v;
      constructor_elt elt = vref[i];
      for (; i > 0; --i)
	vref[i] = vref[i-1];
      vref[0] = elt;
    }

  return v;
}

/* Build compile-time evalable representations of member-initializer list
   for a constexpr constructor.  */

static tree
build_constexpr_constructor_member_initializers (tree type, tree body)
{
  vec<constructor_elt, va_gc> *vec = NULL;
  bool ok = true;
  while (true)
    switch (TREE_CODE (body))
      {
      case MUST_NOT_THROW_EXPR:
      case EH_SPEC_BLOCK:
	body = TREE_OPERAND (body, 0);
	break;

      case STATEMENT_LIST:
	for (tree stmt : tsi_range (body))
	  {
	    body = stmt;
	    if (TREE_CODE (body) == BIND_EXPR)
	      break;
	  }
	break;

      case BIND_EXPR:
	body = BIND_EXPR_BODY (body);
	goto found;

      default:
	gcc_unreachable ();
    }
 found:
  if (TREE_CODE (body) == TRY_BLOCK)
    {
      body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == BIND_EXPR)
	body = BIND_EXPR_BODY (body);
    }
  if (TREE_CODE (body) == CLEANUP_POINT_EXPR)
    {
      body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == EXPR_STMT)
	body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == INIT_EXPR
	  && (same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (TREE_OPERAND (body, 0)),
	       current_class_type)))
	{
	  /* Trivial copy.  */
	  return TREE_OPERAND (body, 1);
	}
      ok = build_data_member_initialization (body, &vec);
    }
  else if (TREE_CODE (body) == STATEMENT_LIST)
    {
      for (tree stmt : tsi_range (body))
	{
	  ok = build_data_member_initialization (stmt, &vec);
	  if (!ok)
	    break;
	}
    }
  else if (EXPR_P (body))
    ok = build_data_member_initialization (body, &vec);
  else
    gcc_assert (errorcount > 0);
  if (ok)
    {
      if (vec_safe_length (vec) > 0)
	{
	  /* In a delegating constructor, return the target.  */
	  constructor_elt *ce = &(*vec)[0];
	  if (ce->index == current_class_ptr)
	    {
	      body = ce->value;
	      vec_free (vec);
	      return body;
	    }
	}
      vec = sort_constexpr_mem_initializers (type, vec);
      return build_constructor (type, vec);
    }
  else
    return error_mark_node;
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  If the call is lexically to a named function,
   return the _DECL for that function.  */

static tree
get_function_named_in_call (tree t)
{
  tree callee = cp_get_callee (t);
  tree fun = cp_get_fndecl_from_callee (callee, /*fold*/false);
  return fun ? fun : callee;
}

/* Subroutine of check_constexpr_fundef.  BODY is the body of a function
   declared to be constexpr, or a sub-statement thereof.  Returns the
   return value if suitable, error_mark_node for a statement not allowed in
   a constexpr function, or NULL_TREE if no return value was found.  */

tree
constexpr_fn_retval (tree body)
{
  switch (TREE_CODE (body))
    {
    case STATEMENT_LIST:
      {
	tree expr = NULL_TREE;
	for (tree stmt : tsi_range (body))
	  {
	    tree s = constexpr_fn_retval (stmt);
	    if (s == error_mark_node)
	      return error_mark_node;
	    else if (s == NULL_TREE)
	      /* Keep iterating.  */;
	    else if (expr)
	      /* Multiple return statements.  */
	      return error_mark_node;
	    else
	      expr = s;
	  }
	return expr;
      }

    case RETURN_EXPR:
      return break_out_target_exprs (TREE_OPERAND (body, 0));

    case DECL_EXPR:
      {
	tree decl = DECL_EXPR_DECL (body);
	if (TREE_CODE (decl) == USING_DECL
	    /* Accept __func__, __FUNCTION__, and __PRETTY_FUNCTION__.  */
	    || DECL_ARTIFICIAL (decl))
	  return NULL_TREE;
	return error_mark_node;
      }

    case CLEANUP_POINT_EXPR:
      return constexpr_fn_retval (TREE_OPERAND (body, 0));

    case BIND_EXPR:
      if (!check_constexpr_bind_expr_vars (body))
	return error_mark_node;
      return constexpr_fn_retval (BIND_EXPR_BODY (body));

    case USING_STMT:
    case DEBUG_BEGIN_STMT:
      return NULL_TREE;

    case CALL_EXPR:
	{
	  tree fun = get_function_named_in_call (body);
	  if (fun != NULL_TREE
	      && fndecl_built_in_p (fun, BUILT_IN_UNREACHABLE))
	    return NULL_TREE;
	}
      /* Fallthru.  */

    default:
      return error_mark_node;
    }
}

/* Subroutine of check_constexpr_fundef.  BODY is the DECL_SAVED_TREE of
   FUN; do the necessary transformations to turn it into a single expression
   that we can store in the hash table.  */

static tree
massage_constexpr_body (tree fun, tree body)
{
  if (DECL_CONSTRUCTOR_P (fun))
    body = build_constexpr_constructor_member_initializers
      (DECL_CONTEXT (fun), body);
  else if (cxx_dialect < cxx14)
    {
      if (TREE_CODE (body) == EH_SPEC_BLOCK)
        body = EH_SPEC_STMTS (body);
      if (TREE_CODE (body) == MUST_NOT_THROW_EXPR)
	body = TREE_OPERAND (body, 0);
      body = constexpr_fn_retval (body);
    }
  return body;
}

/* CTYPE is a type constructed from BODY.  Return true if some
   bases/fields are uninitialized, and complain if COMPLAIN.  */

static bool
cx_check_missing_mem_inits (tree ctype, tree body, bool complain)
{
  /* We allow uninitialized bases/fields in C++20.  */
  if (cxx_dialect >= cxx20)
    return false;

  unsigned nelts = 0;

  if (body)
    {
      if (TREE_CODE (body) != CONSTRUCTOR)
	return false;
      nelts = CONSTRUCTOR_NELTS (body);
    }
  tree field = TYPE_FIELDS (ctype);

  if (TREE_CODE (ctype) == UNION_TYPE)
    {
      if (nelts == 0 && next_aggregate_field (field))
	{
	  if (complain)
	    error ("%<constexpr%> constructor for union %qT must "
		   "initialize exactly one non-static data member", ctype);
	  return true;
	}
      return false;
    }

  /* Iterate over the CONSTRUCTOR, checking any missing fields don't
     need an explicit initialization.  */
  bool bad = false;
  for (unsigned i = 0; i <= nelts; ++i)
    {
      tree index = NULL_TREE;
      if (i < nelts)
	{
	  index = CONSTRUCTOR_ELT (body, i)->index;
	  /* Skip base and vtable inits.  */
	  if (TREE_CODE (index) != FIELD_DECL
	      || DECL_ARTIFICIAL (index))
	    continue;
	}

      for (; field != index; field = DECL_CHAIN (field))
	{
	  tree ftype;
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  if (DECL_UNNAMED_BIT_FIELD (field))
	    continue;
	  if (DECL_ARTIFICIAL (field))
	    continue;
	  if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	    {
	      /* Recurse to check the anonymous aggregate member.  */
	      bad |= cx_check_missing_mem_inits
		(TREE_TYPE (field), NULL_TREE, complain);
	      if (bad && !complain)
		return true;
	      continue;
	    }
	  ftype = TREE_TYPE (field);
	  if (!ftype || !TYPE_P (ftype) || !COMPLETE_TYPE_P (ftype))
	    /* A flexible array can't be intialized here, so don't complain
	       that it isn't.  */
	    continue;
	  if (is_empty_field (field))
	    /* An empty field doesn't need an initializer.  */
	    continue;
	  ftype = strip_array_types (ftype);
	  if (type_has_constexpr_default_constructor (ftype))
	    {
	      /* It's OK to skip a member with a trivial constexpr ctor.
	         A constexpr ctor that isn't trivial should have been
	         added in by now.  */
	      gcc_checking_assert (!TYPE_HAS_COMPLEX_DFLT (ftype)
				   || errorcount != 0);
	      continue;
	    }
	  if (!complain)
	    return true;
	  auto_diagnostic_group d;
	  error ("member %qD must be initialized by mem-initializer "
		 "in %<constexpr%> constructor", field);
	  inform (DECL_SOURCE_LOCATION (field), "declared here");
	  bad = true;
	}
      if (field == NULL_TREE)
	break;

      if (ANON_AGGR_TYPE_P (TREE_TYPE (index)))
	{
	  /* Check the anonymous aggregate initializer is valid.  */
	  bad |= cx_check_missing_mem_inits
	    (TREE_TYPE (index), CONSTRUCTOR_ELT (body, i)->value, complain);
	  if (bad && !complain)
	    return true;
	}
      field = DECL_CHAIN (field);
    }

  return bad;
}

/* We are processing the definition of the constexpr function FUN.
   Check that its body fulfills the apropriate requirements and
   enter it in the constexpr function definition table.  */

void
maybe_save_constexpr_fundef (tree fun)
{
  if (processing_template_decl
      || cp_function_chain->invalid_constexpr
      || (DECL_CLONED_FUNCTION_P (fun) && !DECL_DELETING_DESTRUCTOR_P (fun)))
    return;

  /* With -fimplicit-constexpr, try to make inlines constexpr.  We'll
     actually set DECL_DECLARED_CONSTEXPR_P below if the checks pass.  */
  bool implicit = false;
  if (flag_implicit_constexpr)
    {
      if (DECL_DELETING_DESTRUCTOR_P (fun)
	  && decl_implicit_constexpr_p (DECL_CLONED_FUNCTION (fun)))
	/* Don't inherit implicit constexpr from the non-deleting
	   destructor.  */
	DECL_DECLARED_CONSTEXPR_P (fun) = false;

      if (!DECL_DECLARED_CONSTEXPR_P (fun)
	  && DECL_DECLARED_INLINE_P (fun)
	  && !lookup_attribute ("noinline", DECL_ATTRIBUTES (fun)))
	implicit = true;
    }

  if (!DECL_DECLARED_CONSTEXPR_P (fun) && !implicit)
    return;

  bool complain = !DECL_GENERATED_P (fun) && !implicit;

  if (!is_valid_constexpr_fn (fun, complain))
    return;

  tree massaged = massage_constexpr_body (fun, DECL_SAVED_TREE (fun));
  if (massaged == NULL_TREE || massaged == error_mark_node)
    {
      if (!DECL_CONSTRUCTOR_P (fun) && complain)
	error ("body of %<constexpr%> function %qD not a return-statement",
	       fun);
      return;
    }

  bool potential = potential_rvalue_constant_expression (massaged);
  if (!potential && complain)
    require_potential_rvalue_constant_expression_fncheck (massaged);

  if (DECL_CONSTRUCTOR_P (fun) && potential
      && !DECL_DEFAULTED_FN (fun))
    {
      if (cx_check_missing_mem_inits (DECL_CONTEXT (fun),
				      massaged, complain))
	potential = false;
      else if (cxx_dialect > cxx11)
	{
	  /* What we got from massage_constexpr_body is pretty much just the
	     ctor-initializer, also check the body.  */
	  massaged = DECL_SAVED_TREE (fun);
	  potential = potential_rvalue_constant_expression (massaged);
	  if (!potential && complain)
	    require_potential_rvalue_constant_expression_fncheck (massaged);
	}
    }

  if (!potential && complain
      /* If -Wno-invalid-constexpr was specified, we haven't complained
	 about non-constant expressions yet.  Register the function and
	 complain in explain_invalid_constexpr_fn if the function is
	 called.  */
      && warn_invalid_constexpr != 0)
    return;

  if (implicit)
    {
      if (potential)
	{
	  DECL_DECLARED_CONSTEXPR_P (fun) = true;
	  DECL_LANG_SPECIFIC (fun)->u.fn.implicit_constexpr = true;
	  if (DECL_CONSTRUCTOR_P (fun))
	    TYPE_HAS_CONSTEXPR_CTOR (DECL_CONTEXT (fun)) = true;
	}
      else
	/* Don't bother keeping the pre-generic body of unsuitable functions
	   not explicitly declared constexpr.  */
	return;
    }

  constexpr_fundef entry = {fun, NULL_TREE, NULL_TREE, NULL_TREE};
  bool clear_ctx = false;
  if (DECL_RESULT (fun) && DECL_CONTEXT (DECL_RESULT (fun)) == NULL_TREE)
    {
      clear_ctx = true;
      DECL_CONTEXT (DECL_RESULT (fun)) = fun;
    }
  tree saved_fn = current_function_decl;
  current_function_decl = fun;
  entry.body = copy_fn (entry.decl, entry.parms, entry.result);
  current_function_decl = saved_fn;
  if (clear_ctx)
    DECL_CONTEXT (DECL_RESULT (entry.decl)) = NULL_TREE;
  if (!potential)
    /* For a template instantiation, we want to remember the pre-generic body
       for explain_invalid_constexpr_fn, but do tell cxx_eval_call_expression
       that it doesn't need to bother trying to expand the function.  */
    entry.result = error_mark_node;

  register_constexpr_fundef (entry);
}

/* BODY is a validated and massaged definition of a constexpr
   function.  Register it in the hash table.  */

void
register_constexpr_fundef (const constexpr_fundef &value)
{
  /* Create the constexpr function table if necessary.  */
  if (constexpr_fundef_table == NULL)
    constexpr_fundef_table
      = hash_table<constexpr_fundef_hasher>::create_ggc (101);

  constexpr_fundef **slot = constexpr_fundef_table->find_slot
    (const_cast<constexpr_fundef *> (&value), INSERT);

  gcc_assert (*slot == NULL);
  *slot = ggc_alloc<constexpr_fundef> ();
  **slot = value;
}

/* FUN is a non-constexpr (or, with -Wno-invalid-constexpr, a constexpr
   function called in a context that requires a constant expression).
   If it comes from a constexpr template, explain why the instantiation
   isn't constexpr.  Otherwise, explain why the function cannot be used
   in a constexpr context.  */

void
explain_invalid_constexpr_fn (tree fun)
{
  static hash_set<tree> *diagnosed;
  tree body;
  /* In C++23, a function marked 'constexpr' may not actually be a constant
     expression.  We haven't diagnosed the problem yet: -Winvalid-constexpr
     wasn't enabled.  The function was called, so diagnose why it cannot be
     used in a constant expression.  */
  if (warn_invalid_constexpr == 0 && DECL_DECLARED_CONSTEXPR_P (fun))
    /* Go on.  */;
  /* Only diagnose defaulted functions, lambdas, or instantiations.  */
  else if (!DECL_DEFAULTED_FN (fun)
	   && !LAMBDA_TYPE_P (CP_DECL_CONTEXT (fun))
	   && !(flag_implicit_constexpr
		&& !DECL_DECLARED_CONSTEXPR_P (fun)
		&& DECL_DECLARED_INLINE_P (fun))
	   && !is_instantiation_of_constexpr (fun))
    {
      inform (DECL_SOURCE_LOCATION (fun), "%qD declared here", fun);
      if (flag_implicit_constexpr && !maybe_constexpr_fn (fun)
	  && decl_defined_p (fun))
	inform (DECL_SOURCE_LOCATION (fun),
		"%<-fimplicit-constexpr%> only affects %<inline%> functions");
      return;
    }
  if (diagnosed == NULL)
    diagnosed = new hash_set<tree>;
  if (diagnosed->add (fun))
    /* Already explained.  */
    return;

  iloc_sentinel ils = input_location;
  if (!lambda_static_thunk_p (fun))
    {
      /* Diagnostics should completely ignore the static thunk, so leave
	 input_location set to our caller's location.  */
      input_location = DECL_SOURCE_LOCATION (fun);
      inform (input_location,
	      "%qD is not usable as a %<constexpr%> function because:", fun);
    }
  /* First check the declaration.  */
  if (is_valid_constexpr_fn (fun, true))
    {
      /* Then if it's OK, the body.  */
      if (!DECL_DECLARED_CONSTEXPR_P (fun)
	  && DECL_DEFAULTED_FN (fun))
	explain_implicit_non_constexpr (fun);
      else
	{
	  if (constexpr_fundef *fd = retrieve_constexpr_fundef (fun))
	    body = fd->body;
	  else
	    body = DECL_SAVED_TREE (fun);
	  body = massage_constexpr_body (fun, body);
	  require_potential_rvalue_constant_expression (body);
	  if (DECL_CONSTRUCTOR_P (fun))
	    {
	      cx_check_missing_mem_inits (DECL_CONTEXT (fun), body, true);
	      if (cxx_dialect > cxx11)
		{
		  /* Also check the body, not just the ctor-initializer.  */
		  body = DECL_SAVED_TREE (fun);
		  require_potential_rvalue_constant_expression (body);
		}
	    }
	}
    }
}

/* Objects of this type represent calls to constexpr functions
   along with the bindings of parameters to their arguments, for
   the purpose of compile time evaluation.  */

struct GTY((for_user)) constexpr_call {
  /* Description of the constexpr function definition.  */
  constexpr_fundef *fundef;
  /* Parameter bindings environment.  A TREE_VEC of arguments.  */
  tree bindings;
  /* Result of the call.
       NULL means the call is being evaluated.
       error_mark_node means that the evaluation was erroneous;
       otherwise, the actuall value of the call.  */
  tree result;
  /* The hash of this call; we remember it here to avoid having to
     recalculate it when expanding the hash table.  */
  hashval_t hash;
  /* The value of constexpr_ctx::manifestly_const_eval.  */
  enum mce_value manifestly_const_eval;
};

struct constexpr_call_hasher : ggc_ptr_hash<constexpr_call>
{
  static hashval_t hash (constexpr_call *);
  static bool equal (constexpr_call *, constexpr_call *);
};

enum constexpr_switch_state {
  /* Used when processing a switch for the first time by cxx_eval_switch_expr
     and default: label for that switch has not been seen yet.  */
  css_default_not_seen,
  /* Used when processing a switch for the first time by cxx_eval_switch_expr
     and default: label for that switch has been seen already.  */
  css_default_seen,
  /* Used when processing a switch for the second time by
     cxx_eval_switch_expr, where default: label should match.  */
  css_default_processing
};

/* The constexpr expansion context part which needs one instance per
   cxx_eval_outermost_constant_expr invocation.  VALUES is a map of values of
   variables initialized within the expression.  */

class constexpr_global_ctx {
  /* Values for any temporaries or local variables within the
     constant-expression. Objects outside their lifetime have
     value 'void_node'.  */
  hash_map<tree,tree> values;
public:
  /* Number of cxx_eval_constant_expression calls (except skipped ones,
     on simple constants or location wrappers) encountered during current
     cxx_eval_outermost_constant_expr call.  */
  HOST_WIDE_INT constexpr_ops_count;
  /* Heap VAR_DECLs created during the evaluation of the outermost constant
     expression.  */
  auto_vec<tree, 16> heap_vars;
  /* Cleanups that need to be evaluated at the end of CLEANUP_POINT_EXPR.  */
  vec<tree> *cleanups;
  /* If non-null, only allow modification of existing values of the variables
     in this set.  Set by modifiable_tracker, below.  */
  hash_set<tree> *modifiable;
  /* Number of heap VAR_DECL deallocations.  */
  unsigned heap_dealloc_count;
  /* Constructor.  */
  constexpr_global_ctx ()
    : constexpr_ops_count (0), cleanups (NULL), modifiable (nullptr),
      heap_dealloc_count (0) {}

  bool is_outside_lifetime (tree t)
  {
    if (tree *p = values.get (t))
      if (*p == void_node)
	return true;
    return false;
  }
 tree get_value (tree t)
  {
    if (tree *p = values.get (t))
      if (*p != void_node)
	return *p;
    return NULL_TREE;
  }
  tree *get_value_ptr (tree t, bool initializing)
  {
    if (modifiable && !modifiable->contains (t))
      return nullptr;
    if (tree *p = values.get (t))
      {
	if (*p != void_node)
	  return p;
	else if (initializing)
	  {
	    *p = NULL_TREE;
	    return p;
	  }
      }
    return nullptr;
  }
  void put_value (tree t, tree v)
  {
    bool already_in_map = values.put (t, v);
    if (!already_in_map && modifiable)
      modifiable->add (t);
  }
  void destroy_value (tree t)
  {
    if (TREE_CODE (t) == VAR_DECL
	|| TREE_CODE (t) == PARM_DECL
	|| TREE_CODE (t) == RESULT_DECL)
      values.put (t, void_node);
    else
      values.remove (t);
  }
  void clear_value (tree t)
  {
    values.remove (t);
  }
};

/* Helper class for constexpr_global_ctx.  In some cases we want to avoid
   side-effects from evaluation of a particular subexpression of a
   constant-expression.  In such cases we use modifiable_tracker to prevent
   modification of variables created outside of that subexpression.

   ??? We could change the hash_set to a hash_map, allow and track external
   modifications, and roll them back in the destructor.  It's not clear to me
   that this would be worthwhile.  */

class modifiable_tracker
{
  hash_set<tree> set;
  constexpr_global_ctx *global;
public:
  modifiable_tracker (constexpr_global_ctx *g): global(g)
  {
    global->modifiable = &set;
  }
  ~modifiable_tracker ()
  {
    for (tree t: set)
      global->clear_value (t);
    global->modifiable = nullptr;
  }
};

/* The constexpr expansion context.  CALL is the current function
   expansion, CTOR is the current aggregate initializer, OBJECT is the
   object being initialized by CTOR, either a VAR_DECL or a _REF.    */

struct constexpr_ctx {
  /* The part of the context that needs to be unique to the whole
     cxx_eval_outermost_constant_expr invocation.  */
  constexpr_global_ctx *global;
  /* The innermost call we're evaluating.  */
  constexpr_call *call;
  /* SAVE_EXPRs and TARGET_EXPR_SLOT vars of TARGET_EXPRs that we've seen
     within the current LOOP_EXPR.  NULL if we aren't inside a loop.  */
  vec<tree> *save_exprs;
  /* The CONSTRUCTOR we're currently building up for an aggregate
     initializer.  */
  tree ctor;
  /* The object we're building the CONSTRUCTOR for.  */
  tree object;
  /* If inside SWITCH_EXPR.  */
  constexpr_switch_state *css_state;
  /* The aggregate initialization context inside which this one is nested.  This
     is used by lookup_placeholder to resolve PLACEHOLDER_EXPRs.  */
  const constexpr_ctx *parent;

  /* Whether we should error on a non-constant expression or fail quietly.
     This flag needs to be here, but some of the others could move to global
     if they get larger than a word.  */
  bool quiet;
  /* Whether we are strictly conforming to constant expression rules or
     trying harder to get a constant value.  */
  bool strict;
  /* Whether __builtin_is_constant_evaluated () should be true.  */
  mce_value manifestly_const_eval;
};

/* Remove T from the global values map, checking for attempts to destroy
   a value that has already finished its lifetime.  */

static void
destroy_value_checked (const constexpr_ctx* ctx, tree t, bool *non_constant_p)
{
  if (t == error_mark_node || TREE_TYPE (t) == error_mark_node)
    return;

  /* Don't error again here if we've already reported a problem.  */
  if (!*non_constant_p
      && DECL_P (t)
      /* Non-trivial destructors have their lifetimes ended explicitly
	 with a clobber, so don't worry about it here.  */
      && (!TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (t))
	  /* ...except parameters are remapped in cxx_eval_call_expression,
	     and the destructor call during cleanup won't be able to tell that
	     this value has already been destroyed, so complain now.  This is
	     not quite unobservable, but is extremely unlikely to crop up in
	     practice; see g++.dg/cpp2a/constexpr-lifetime2.C.  */
	  || TREE_CODE (t) == PARM_DECL)
      && ctx->global->is_outside_lifetime (t))
    {
      if (!ctx->quiet)
	{
	  auto_diagnostic_group d;
	  error ("destroying %qE outside its lifetime", t);
	  inform (DECL_SOURCE_LOCATION (t), "declared here");
	}
      *non_constant_p = true;
    }
  ctx->global->destroy_value (t);
}

/* This internal flag controls whether we should avoid doing anything during
   constexpr evaluation that would cause extra DECL_UID generation, such as
   template instantiation and function body copying.  */

static bool uid_sensitive_constexpr_evaluation_value;

/* An internal counter that keeps track of the number of times
   uid_sensitive_constexpr_evaluation_p returned true.  */

static unsigned uid_sensitive_constexpr_evaluation_true_counter;

/* The accessor for uid_sensitive_constexpr_evaluation_value which also
   increments the corresponding counter.  */

static bool
uid_sensitive_constexpr_evaluation_p ()
{
  if (uid_sensitive_constexpr_evaluation_value)
    {
      ++uid_sensitive_constexpr_evaluation_true_counter;
      return true;
    }
  else
    return false;
}

/* The default constructor for uid_sensitive_constexpr_evaluation_sentinel
   enables the internal flag for uid_sensitive_constexpr_evaluation_p
   during the lifetime of the sentinel object.  Upon its destruction, the
   previous value of uid_sensitive_constexpr_evaluation_p is restored.  */

uid_sensitive_constexpr_evaluation_sentinel
::uid_sensitive_constexpr_evaluation_sentinel ()
  : ovr (uid_sensitive_constexpr_evaluation_value, true)
{
}

/* The default constructor for uid_sensitive_constexpr_evaluation_checker
   records the current number of times that uid_sensitive_constexpr_evaluation_p
   has been called and returned true.  */

uid_sensitive_constexpr_evaluation_checker
::uid_sensitive_constexpr_evaluation_checker ()
  : saved_counter (uid_sensitive_constexpr_evaluation_true_counter)
{
}

/* Returns true iff uid_sensitive_constexpr_evaluation_p is true, and
   some constexpr evaluation was restricted due to u_s_c_e_p being called
   and returning true during the lifetime of this checker object.  */

bool
uid_sensitive_constexpr_evaluation_checker::evaluation_restricted_p () const
{
  return (uid_sensitive_constexpr_evaluation_value
	  && saved_counter != uid_sensitive_constexpr_evaluation_true_counter);
}


/* A table of all constexpr calls that have been evaluated by the
   compiler in this translation unit.  */

static GTY (()) hash_table<constexpr_call_hasher> *constexpr_call_table;

/* Compute a hash value for a constexpr call representation.  */

inline hashval_t
constexpr_call_hasher::hash (constexpr_call *info)
{
  return info->hash;
}

/* Return true if the objects pointed to by P and Q represent calls
   to the same constexpr function with the same arguments.
   Otherwise, return false.  */

bool
constexpr_call_hasher::equal (constexpr_call *lhs, constexpr_call *rhs)
{
  if (lhs == rhs)
    return true;
  if (lhs->hash != rhs->hash)
    return false;
  if (lhs->manifestly_const_eval != rhs->manifestly_const_eval)
    return false;
  if (!constexpr_fundef_hasher::equal (lhs->fundef, rhs->fundef))
    return false;
  return cp_tree_equal (lhs->bindings, rhs->bindings);
}

/* Initialize the constexpr call table, if needed.  */

static void
maybe_initialize_constexpr_call_table (void)
{
  if (constexpr_call_table == NULL)
    constexpr_call_table = hash_table<constexpr_call_hasher>::create_ggc (101);
}

/* During constexpr CALL_EXPR evaluation, to avoid issues with sharing when
   a function happens to get called recursively, we unshare the callee
   function's body and evaluate this unshared copy instead of evaluating the
   original body.

   FUNDEF_COPIES_TABLE is a per-function freelist of these unshared function
   copies.  The underlying data structure of FUNDEF_COPIES_TABLE is a hash_map
   that's keyed off of the original FUNCTION_DECL and whose value is a
   TREE_LIST of this function's unused copies awaiting reuse.

   This is not GC-deletable to avoid GC affecting UID generation.  */

static GTY(()) decl_tree_map *fundef_copies_table;

/* Reuse a copy or create a new unshared copy of the function FUN.
   Return this copy.  We use a TREE_LIST whose PURPOSE is body, VALUE
   is parms, TYPE is result.  */

static tree
get_fundef_copy (constexpr_fundef *fundef)
{
  tree copy;
  bool existed;
  tree *slot = &(hash_map_safe_get_or_insert<hm_ggc>
		 (fundef_copies_table, fundef->decl, &existed, 127));

  if (!existed)
    {
      /* There is no cached function available, or in use.  We can use
	 the function directly.  That the slot is now created records
	 that this function is now in use.  */
      copy = build_tree_list (fundef->body, fundef->parms);
      TREE_TYPE (copy) = fundef->result;
    }
  else if (*slot == NULL_TREE)
    {
      if (uid_sensitive_constexpr_evaluation_p ())
	return NULL_TREE;

      /* We've already used the function itself, so make a copy.  */
      copy = build_tree_list (NULL, NULL);
      tree saved_body = DECL_SAVED_TREE (fundef->decl);
      tree saved_parms = DECL_ARGUMENTS (fundef->decl);
      tree saved_result = DECL_RESULT (fundef->decl);
      tree saved_fn = current_function_decl;
      DECL_SAVED_TREE (fundef->decl) = fundef->body;
      DECL_ARGUMENTS (fundef->decl) = fundef->parms;
      DECL_RESULT (fundef->decl) = fundef->result;
      current_function_decl = fundef->decl;
      TREE_PURPOSE (copy) = copy_fn (fundef->decl, TREE_VALUE (copy),
				     TREE_TYPE (copy));
      current_function_decl = saved_fn;
      DECL_RESULT (fundef->decl) = saved_result;
      DECL_ARGUMENTS (fundef->decl) = saved_parms;
      DECL_SAVED_TREE (fundef->decl) = saved_body;
    }
  else
    {
      /* We have a cached function available.  */
      copy = *slot;
      *slot = TREE_CHAIN (copy);
    }

  return copy;
}

/* Save the copy COPY of function FUN for later reuse by
   get_fundef_copy().  By construction, there will always be an entry
   to find.  */

static void
save_fundef_copy (tree fun, tree copy)
{
  tree *slot = fundef_copies_table->get (fun);
  TREE_CHAIN (copy) = *slot;
  *slot = copy;
}

/* Whether our evaluation wants a prvalue (e.g. CONSTRUCTOR or _CST),
   a glvalue (e.g. VAR_DECL or _REF), or nothing.  */

enum value_cat {
   vc_prvalue = 0,
   vc_glvalue = 1,
   vc_discard = 2
};

static tree cxx_eval_constant_expression (const constexpr_ctx *, tree,
					  value_cat, bool *, bool *, tree * = NULL);
static tree cxx_eval_bare_aggregate (const constexpr_ctx *, tree,
				     value_cat, bool *, bool *);
static tree cxx_fold_indirect_ref (const constexpr_ctx *, location_t, tree, tree,
				   bool * = NULL);
static tree find_heap_var_refs (tree *, int *, void *);

/* Attempt to evaluate T which represents a call to a builtin function.
   We assume here that all builtin functions evaluate to scalar types
   represented by _CST nodes.  */

static tree
cxx_eval_builtin_function_call (const constexpr_ctx *ctx, tree t, tree fun,
				value_cat lval,
				bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree *args = (tree *) alloca (nargs * sizeof (tree));
  tree new_call;
  int i;

  /* Don't fold __builtin_constant_p within a constexpr function.  */
  bool bi_const_p = DECL_IS_BUILTIN_CONSTANT_P (fun);

  /* If we aren't requiring a constant expression, defer __builtin_constant_p
     in a constexpr function until we have values for the parameters.  */
  if (bi_const_p
      && ctx->manifestly_const_eval != mce_true
      && current_function_decl
      && DECL_DECLARED_CONSTEXPR_P (current_function_decl))
    {
      *non_constant_p = true;
      return t;
    }

  /* For __builtin_is_constant_evaluated, defer it if not
     ctx->manifestly_const_eval (as sometimes we try to constant evaluate
     without manifestly_const_eval even expressions or parts thereof which
     will later be manifestly const_eval evaluated), otherwise fold it to
     true.  */
  if (fndecl_built_in_p (fun, CP_BUILT_IN_IS_CONSTANT_EVALUATED,
			 BUILT_IN_FRONTEND))
    {
      if (ctx->manifestly_const_eval == mce_unknown)
	{
	  *non_constant_p = true;
	  return t;
	}
      return constant_boolean_node (ctx->manifestly_const_eval == mce_true,
				    boolean_type_node);
    }

  if (fndecl_built_in_p (fun, CP_BUILT_IN_SOURCE_LOCATION, BUILT_IN_FRONTEND))
    {
      temp_override<tree> ovr (current_function_decl);
      if (ctx->call && ctx->call->fundef)
	current_function_decl = ctx->call->fundef->decl;
      return fold_builtin_source_location (t);
    }

  int strops = 0;
  int strret = 0;
  if (fndecl_built_in_p (fun, BUILT_IN_NORMAL))
    switch (DECL_FUNCTION_CODE (fun))
      {
      case BUILT_IN_STRLEN:
      case BUILT_IN_STRNLEN:
	strops = 1;
	break;
      case BUILT_IN_MEMCHR:
      case BUILT_IN_STRCHR:
      case BUILT_IN_STRRCHR:
	strops = 1;
	strret = 1;
	break;
      case BUILT_IN_MEMCMP:
      case BUILT_IN_STRCMP:
	strops = 2;
	break;
      case BUILT_IN_STRSTR:
	strops = 2;
	strret = 1;
	break;
      case BUILT_IN_ASAN_POINTER_COMPARE:
      case BUILT_IN_ASAN_POINTER_SUBTRACT:
	/* These builtins shall be ignored during constant expression
	   evaluation.  */
	return void_node;
      case BUILT_IN_UNREACHABLE:
      case BUILT_IN_TRAP:
	if (!*non_constant_p && !ctx->quiet)
	  {
	    /* Do not allow__builtin_unreachable in constexpr function.
	       The __builtin_unreachable call with BUILTINS_LOCATION
	       comes from cp_maybe_instrument_return.  */
	    if (EXPR_LOCATION (t) == BUILTINS_LOCATION)
	      error ("%<constexpr%> call flows off the end of the function");
	    else
	      error ("%q+E is not a constant expression", t);
	  }
	*non_constant_p = true;
	return t;
      default:
	break;
      }

  /* Be permissive for arguments to built-ins; __builtin_constant_p should
     return constant false for a non-constant argument.  */
  constexpr_ctx new_ctx = *ctx;
  new_ctx.quiet = true;
  for (i = 0; i < nargs; ++i)
    {
      tree arg = CALL_EXPR_ARG (t, i);
      tree oarg = arg;

      /* To handle string built-ins we need to pass ADDR_EXPR<STRING_CST> since
	 expand_builtin doesn't know how to look in the values table.  */
      bool strop = i < strops;
      if (strop)
	{
	  STRIP_NOPS (arg);
	  if (TREE_CODE (arg) == ADDR_EXPR)
	    arg = TREE_OPERAND (arg, 0);
	  else
	    strop = false;
	}

      /* If builtin_valid_in_constant_expr_p is true,
	 potential_constant_expression_1 has not recursed into the arguments
	 of the builtin, verify it here.  */
      if (!builtin_valid_in_constant_expr_p (fun)
	  || potential_constant_expression (arg))
	{
	  bool dummy1 = false, dummy2 = false;
	  arg = cxx_eval_constant_expression (&new_ctx, arg, vc_prvalue,
					      &dummy1, &dummy2);
	}

      if (bi_const_p)
	/* For __builtin_constant_p, fold all expressions with constant values
	   even if they aren't C++ constant-expressions.  */
	arg = cp_fold_rvalue (arg);
      else if (strop)
	{
	  if (TREE_CODE (arg) == CONSTRUCTOR)
	    arg = braced_lists_to_strings (TREE_TYPE (arg), arg);
	  if (TREE_CODE (arg) == STRING_CST)
	    arg = build_address (arg);
	  else
	    arg = oarg;
	}

      args[i] = arg;
    }

  bool save_ffbcp = force_folding_builtin_constant_p;
  force_folding_builtin_constant_p |= ctx->manifestly_const_eval == mce_true;
  tree save_cur_fn = current_function_decl;
  /* Return name of ctx->call->fundef->decl for __builtin_FUNCTION ().  */
  if (fndecl_built_in_p (fun, BUILT_IN_FUNCTION)
      && ctx->call
      && ctx->call->fundef)
    current_function_decl = ctx->call->fundef->decl;
  if (fndecl_built_in_p (fun,
			 CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS,
			 BUILT_IN_FRONTEND))
    {
      location_t loc = EXPR_LOCATION (t);
      if (nargs >= 1)
	VERIFY_CONSTANT (args[0]);
      new_call
	= fold_builtin_is_pointer_inverconvertible_with_class (loc, nargs,
							       args);
    }
  else if (fndecl_built_in_p (fun,
			      CP_BUILT_IN_IS_CORRESPONDING_MEMBER,
			      BUILT_IN_FRONTEND))
    {
      location_t loc = EXPR_LOCATION (t);
      if (nargs >= 2)
	{
	  VERIFY_CONSTANT (args[0]);
	  VERIFY_CONSTANT (args[1]);
	}
      new_call = fold_builtin_is_corresponding_member (loc, nargs, args);
    }
  else
    new_call = fold_builtin_call_array (EXPR_LOCATION (t), TREE_TYPE (t),
					CALL_EXPR_FN (t), nargs, args);
  current_function_decl = save_cur_fn;
  force_folding_builtin_constant_p = save_ffbcp;
  if (new_call == NULL)
    {
      if (!*non_constant_p && !ctx->quiet)
	{
	  new_call = build_call_array_loc (EXPR_LOCATION (t), TREE_TYPE (t),
					   CALL_EXPR_FN (t), nargs, args);
	  error ("%q+E is not a constant expression", new_call);
	}
      *non_constant_p = true;
      return t;
    }

  if (!potential_constant_expression (new_call))
    {
      if (!*non_constant_p && !ctx->quiet)
	error ("%q+E is not a constant expression", new_call);
      *non_constant_p = true;
      return t;
    }

  if (strret)
    {
      /* memchr returns a pointer into the first argument, but we replaced the
	 argument above with a STRING_CST; put it back it now.  */
      tree op = CALL_EXPR_ARG (t, strret-1);
      STRIP_NOPS (new_call);
      if (TREE_CODE (new_call) == POINTER_PLUS_EXPR)
	TREE_OPERAND (new_call, 0) = op;
      else if (TREE_CODE (new_call) == ADDR_EXPR)
	new_call = op;
    }

  return cxx_eval_constant_expression (&new_ctx, new_call, lval,
				       non_constant_p, overflow_p);
}

/* TEMP is the constant value of a temporary object of type TYPE.  Adjust
   the type of the value to match.  */

static tree
adjust_temp_type (tree type, tree temp)
{
  if (same_type_p (TREE_TYPE (temp), type))
    return temp;
  /* Avoid wrapping an aggregate value in a NOP_EXPR.  */
  if (TREE_CODE (temp) == CONSTRUCTOR)
    {
      /* build_constructor wouldn't retain various CONSTRUCTOR flags.  */
      tree t = copy_node (temp);
      TREE_TYPE (t) = type;
      return t;
    }
  if (TREE_CODE (temp) == EMPTY_CLASS_EXPR)
    return build0 (EMPTY_CLASS_EXPR, type);
  gcc_assert (scalarish_type_p (type));
  /* Now we know we're dealing with a scalar, and a prvalue of non-class
     type is cv-unqualified.  */
  return cp_fold_convert (cv_unqualified (type), temp);
}

/* If T is a CONSTRUCTOR, return an unshared copy of T and any
   sub-CONSTRUCTORs.  Otherwise return T.

   We use this whenever we initialize an object as a whole, whether it's a
   parameter, a local variable, or a subobject, so that subsequent
   modifications don't affect other places where it was used.  */

tree
unshare_constructor (tree t MEM_STAT_DECL)
{
  if (!t || TREE_CODE (t) != CONSTRUCTOR)
    return t;
  auto_vec <tree*, 4> ptrs;
  ptrs.safe_push (&t);
  while (!ptrs.is_empty ())
    {
      tree *p = ptrs.pop ();
      tree n = copy_node (*p PASS_MEM_STAT);
      CONSTRUCTOR_ELTS (n) = vec_safe_copy (CONSTRUCTOR_ELTS (*p) PASS_MEM_STAT);
      *p = n;
      vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (n);
      constructor_elt *ce;
      for (HOST_WIDE_INT i = 0; vec_safe_iterate (v, i, &ce); ++i)
	if (ce->value && TREE_CODE (ce->value) == CONSTRUCTOR)
	  ptrs.safe_push (&ce->value);
    }
  return t;
}

/* If T is a CONSTRUCTOR, ggc_free T and any sub-CONSTRUCTORs.  */

static void
free_constructor (tree t)
{
  if (!t || TREE_CODE (t) != CONSTRUCTOR)
    return;
  releasing_vec ctors;
  vec_safe_push (ctors, t);
  while (!ctors->is_empty ())
    {
      tree c = ctors->pop ();
      if (vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (c))
	{
	  constructor_elt *ce;
	  for (HOST_WIDE_INT i = 0; vec_safe_iterate (elts, i, &ce); ++i)
	    if (ce->value && TREE_CODE (ce->value) == CONSTRUCTOR)
	      vec_safe_push (ctors, ce->value);
	  ggc_free (elts);
	}
      ggc_free (c);
    }
}

/* Helper function of cxx_bind_parameters_in_call.  Return non-NULL
   if *TP is address of a static variable (or part of it) currently being
   constructed or of a heap artificial variable.  */

static tree
addr_of_non_const_var (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == ADDR_EXPR)
    if (tree var = get_base_address (TREE_OPERAND (*tp, 0)))
      if (VAR_P (var) && TREE_STATIC (var))
	{
	  if (DECL_NAME (var) == heap_uninit_identifier
	      || DECL_NAME (var) == heap_identifier
	      || DECL_NAME (var) == heap_vec_uninit_identifier
	      || DECL_NAME (var) == heap_vec_identifier)
	    return var;

	  constexpr_global_ctx *global = (constexpr_global_ctx *) data;
	  if (global->get_value (var))
	    return var;
	}
  if (TYPE_P (*tp))
    *walk_subtrees = false;
  return NULL_TREE;
}

/* Subroutine of cxx_eval_call_expression.
   We are processing a call expression (either CALL_EXPR or
   AGGR_INIT_EXPR) in the context of CTX.  Evaluate
   all arguments and bind their values to correspondings
   parameters, making up the NEW_CALL context.  */

static tree
cxx_bind_parameters_in_call (const constexpr_ctx *ctx, tree t, tree fun,
			     bool *non_constant_p, bool *overflow_p,
			     bool *non_constant_args)
{
  const int nargs = call_expr_nargs (t);
  tree parms = DECL_ARGUMENTS (fun);
  int i;
  /* We don't record ellipsis args below.  */
  int nparms = list_length (parms);
  int nbinds = nargs < nparms ? nargs : nparms;
  tree binds = make_tree_vec (nbinds);

  /* The call is not a constant expression if it involves the cdtor for a type
     with virtual bases.  */
  if (DECL_HAS_IN_CHARGE_PARM_P (fun) || DECL_HAS_VTT_PARM_P (fun))
    {
      if (!ctx->quiet)
	{
	  error_at (cp_expr_loc_or_input_loc (t),
		    "call to non-%<constexpr%> function %qD", fun);
	  explain_invalid_constexpr_fn (fun);
	}
      *non_constant_p = true;
      return binds;
    }

  for (i = 0; i < nargs; ++i)
    {
      tree x, arg;
      tree type = parms ? TREE_TYPE (parms) : void_type_node;
      if (parms && DECL_BY_REFERENCE (parms))
	type = TREE_TYPE (type);
      x = get_nth_callarg (t, i);
      /* For member function, the first argument is a pointer to the implied
         object.  For a constructor, it might still be a dummy object, in
	 which case we get the real argument from ctx.  */
      if (i == 0 && DECL_CONSTRUCTOR_P (fun)
	  && is_dummy_object (x))
	{
	  x = ctx->object;
	  x = build_address (x);
	}
      if (TREE_ADDRESSABLE (type))
	{
	  /* Undo convert_for_arg_passing work here.  */
	  x = convert_from_reference (x);
	  arg = cxx_eval_constant_expression (ctx, x, vc_glvalue,
					      non_constant_p, overflow_p);
	}
      else
	/* Normally we would strip a TARGET_EXPR in an initialization context
	   such as this, but here we do the elision differently: we keep the
	   TARGET_EXPR, and use its CONSTRUCTOR as the value of the parm.  */
	arg = cxx_eval_constant_expression (ctx, x, vc_prvalue,
					    non_constant_p, overflow_p);
      /* Check we aren't dereferencing a null pointer when calling a non-static
	 member function, which is undefined behaviour.  */
      if (i == 0 && DECL_OBJECT_MEMBER_FUNCTION_P (fun)
	  && integer_zerop (arg)
	  /* But ignore calls from within compiler-generated code, to handle
	     cases like lambda function pointer conversion operator thunks
	     which pass NULL as the 'this' pointer.  */
	  && !(TREE_CODE (t) == CALL_EXPR && CALL_FROM_THUNK_P (t)))
	{
	  if (!ctx->quiet)
	    error_at (cp_expr_loc_or_input_loc (x),
		      "dereferencing a null pointer");
	  *non_constant_p = true;
	}
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p && ctx->quiet)
	break;
      /* Just discard ellipsis args after checking their constantitude.  */
      if (!parms)
	continue;

      if (!*non_constant_p)
	{
	  /* Make sure the binding has the same type as the parm.  But
	     only for constant args.  */
	  if (TREE_ADDRESSABLE (type))
	    {
	      if (!same_type_p (type, TREE_TYPE (arg)))
		{
		  arg = build_fold_addr_expr (arg);
		  arg = cp_fold_convert (build_reference_type (type), arg);
		  arg = convert_from_reference (arg);
		}
	    }
	  else if (!TYPE_REF_P (type))
	    arg = adjust_temp_type (type, arg);
	  if (!TREE_CONSTANT (arg))
	    *non_constant_args = true;
	  else if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	    /* The destructor needs to see any modifications the callee makes
	       to the argument.  */
	    *non_constant_args = true;
	    /* If arg is or contains address of a heap artificial variable or
	       of a static variable being constructed, avoid caching the
	       function call, as those variables might be modified by the
	       function, or might be modified by the callers in between
	       the cached function and just read by the function.  */
	  else if (!*non_constant_args
		   && cp_walk_tree (&arg, addr_of_non_const_var, ctx->global,
				    NULL))
	    *non_constant_args = true;

	  /* For virtual calls, adjust the this argument, so that it is
	     the object on which the method is called, rather than
	     one of its bases.  */
	  if (i == 0 && DECL_VIRTUAL_P (fun))
	    {
	      tree addr = arg;
	      STRIP_NOPS (addr);
	      if (TREE_CODE (addr) == ADDR_EXPR)
		{
		  tree obj = TREE_OPERAND (addr, 0);
		  while (TREE_CODE (obj) == COMPONENT_REF
			 && DECL_FIELD_IS_BASE (TREE_OPERAND (obj, 1))
			 && !same_type_ignoring_top_level_qualifiers_p
					(TREE_TYPE (obj), DECL_CONTEXT (fun)))
		    obj = TREE_OPERAND (obj, 0);
		  if (obj != TREE_OPERAND (addr, 0))
		    arg = build_fold_addr_expr_with_type (obj,
							  TREE_TYPE (arg));
		}
	    }
	  TREE_VEC_ELT (binds, i) = arg;
	}
      parms = TREE_CHAIN (parms);
    }

  return binds;
}

/* Variables and functions to manage constexpr call expansion context.
   These do not need to be marked for PCH or GC.  */

/* FIXME remember and print actual constant arguments.  */
static vec<tree> call_stack;
static int call_stack_tick;
static int last_cx_error_tick;

static int
push_cx_call_context (tree call)
{
  ++call_stack_tick;
  if (!EXPR_HAS_LOCATION (call))
    SET_EXPR_LOCATION (call, input_location);
  call_stack.safe_push (call);
  int len = call_stack.length ();
  if (len > max_constexpr_depth)
    return false;
  return len;
}

static void
pop_cx_call_context (void)
{
  ++call_stack_tick;
  call_stack.pop ();
}

vec<tree>
cx_error_context (void)
{
  vec<tree> r = vNULL;
  if (call_stack_tick != last_cx_error_tick
      && !call_stack.is_empty ())
    r = call_stack;
  last_cx_error_tick = call_stack_tick;
  return r;
}

/* E is an operand of a failed assertion, fold it either with or without
   constexpr context.  */

static tree
fold_operand (tree e, const constexpr_ctx *ctx)
{
  if (ctx)
    {
      bool new_non_constant_p = false, new_overflow_p = false;
      e = cxx_eval_constant_expression (ctx, e, vc_prvalue,
					&new_non_constant_p,
					&new_overflow_p);
    }
  else
    e = fold_non_dependent_expr (e, tf_none, /*manifestly_const_eval=*/true);
  return e;
}

/* If we have a condition in conjunctive normal form (CNF), find the first
   failing clause.  In other words, given an expression like

     true && true && false && true && false

   return the first 'false'.  EXPR is the expression.  */

static tree
find_failing_clause_r (const constexpr_ctx *ctx, tree expr)
{
  if (TREE_CODE (expr) == TRUTH_ANDIF_EXPR)
    {
      /* First check the left side...  */
      tree e = find_failing_clause_r (ctx, TREE_OPERAND (expr, 0));
      if (e == NULL_TREE)
	/* ...if we didn't find a false clause, check the right side.  */
	e = find_failing_clause_r (ctx, TREE_OPERAND (expr, 1));
      return e;
    }
  tree e = contextual_conv_bool (expr, tf_none);
  e = fold_operand (e, ctx);
  if (integer_zerop (e))
    /* This is the failing clause.  */
    return expr;
  return NULL_TREE;
}

/* Wrapper for find_failing_clause_r.  */

tree
find_failing_clause (const constexpr_ctx *ctx, tree expr)
{
  if (TREE_CODE (expr) == TRUTH_ANDIF_EXPR)
    if (tree e = find_failing_clause_r (ctx, expr))
      expr = e;
  return expr;
}

/* Emit additional diagnostics for failing condition BAD.
   Used by finish_static_assert and IFN_ASSUME constexpr diagnostics.
   If SHOW_EXPR_P is true, print the condition (because it was
   instantiation-dependent).  */

void
diagnose_failing_condition (tree bad, location_t cloc, bool show_expr_p,
			    const constexpr_ctx *ctx /* = nullptr */)
{
  /* Nobody wants to see the artificial (bool) cast.  */
  bad = tree_strip_nop_conversions (bad);
  if (TREE_CODE (bad) == CLEANUP_POINT_EXPR)
    bad = TREE_OPERAND (bad, 0);

  /* Actually explain the failure if this is a concept check or a
     requires-expression.  */
  if (concept_check_p (bad) || TREE_CODE (bad) == REQUIRES_EXPR)
    diagnose_constraints (cloc, bad, NULL_TREE);
  else if (COMPARISON_CLASS_P (bad)
	   && ARITHMETIC_TYPE_P (TREE_TYPE (TREE_OPERAND (bad, 0))))
    {
      tree op0 = fold_operand (TREE_OPERAND (bad, 0), ctx);
      tree op1 = fold_operand (TREE_OPERAND (bad, 1), ctx);
      tree cond = build2 (TREE_CODE (bad), boolean_type_node, op0, op1);
      inform (cloc, "the comparison reduces to %qE", cond);
    }
  else if (show_expr_p)
    inform (cloc, "%qE evaluates to false", bad);
}

/* Process an assert/assume of ORIG_ARG.  If it's not supposed to be evaluated,
   do it without changing the current evaluation state.  If it evaluates to
   false, complain and return false; otherwise, return true.  */

static bool
cxx_eval_assert (const constexpr_ctx *ctx, tree arg, const char *msg,
		 location_t loc, bool evaluated,
		 bool *non_constant_p, bool *overflow_p)
{
  if (*non_constant_p)
    return true;

  tree eval;
  if (!evaluated)
    {
      if (!potential_rvalue_constant_expression (arg))
	return true;

      constexpr_ctx new_ctx = *ctx;
      new_ctx.quiet = true;
      bool new_non_constant_p = false, new_overflow_p = false;
      /* Avoid modification of existing values.  */
      modifiable_tracker ms (new_ctx.global);
      eval = cxx_eval_constant_expression (&new_ctx, arg, vc_prvalue,
					   &new_non_constant_p,
					   &new_overflow_p);
    }
  else
    eval = cxx_eval_constant_expression (ctx, arg, vc_prvalue,
					 non_constant_p,
					 overflow_p);
  if (!*non_constant_p && integer_zerop (eval))
    {
      if (!ctx->quiet)
	{
	  /* See if we can find which clause was failing
	     (for logical AND).  */
	  tree bad = find_failing_clause (ctx, arg);
	  /* If not, or its location is unusable, fall back to the
	     previous location.  */
	  location_t cloc = cp_expr_loc_or_loc (bad, loc);

	  /* Report the error. */
	  auto_diagnostic_group d;
	  error_at (cloc, msg);
	  diagnose_failing_condition (bad, cloc, true, ctx);
	  return bad;
	}
      *non_constant_p = true;
      return false;
    }

  return true;
}

/* Evaluate a call T to a GCC internal function when possible and return
   the evaluated result or, under the control of CTX, give an error, set
   NON_CONSTANT_P, and return the unevaluated call T otherwise.  */

static tree
cxx_eval_internal_function (const constexpr_ctx *ctx, tree t,
			    value_cat lval,
			    bool *non_constant_p, bool *overflow_p)
{
  enum tree_code opcode = ERROR_MARK;

  switch (CALL_EXPR_IFN (t))
    {
    case IFN_UBSAN_NULL:
    case IFN_UBSAN_BOUNDS:
    case IFN_UBSAN_VPTR:
    case IFN_FALLTHROUGH:
      return void_node;

    case IFN_ASSUME:
      if (!cxx_eval_assert (ctx, CALL_EXPR_ARG (t, 0),
			    G_("failed %<assume%> attribute assumption"),
			    EXPR_LOCATION (t), /*eval*/false,
			    non_constant_p, overflow_p))
	return t;
      return void_node;

    case IFN_ADD_OVERFLOW:
      opcode = PLUS_EXPR;
      break;
    case IFN_SUB_OVERFLOW:
      opcode = MINUS_EXPR;
      break;
    case IFN_MUL_OVERFLOW:
      opcode = MULT_EXPR;
      break;

    case IFN_LAUNDER:
      return cxx_eval_constant_expression (ctx, CALL_EXPR_ARG (t, 0),
					   vc_prvalue, non_constant_p,
					   overflow_p);

    case IFN_VEC_CONVERT:
      {
	tree arg = cxx_eval_constant_expression (ctx, CALL_EXPR_ARG (t, 0),
						 vc_prvalue, non_constant_p,
						 overflow_p);
	if (TREE_CODE (arg) == VECTOR_CST)
	  if (tree r = fold_const_call (CFN_VEC_CONVERT, TREE_TYPE (t), arg))
	    return r;
      }
      /* FALLTHRU */

    default:
      if (!ctx->quiet)
	error_at (cp_expr_loc_or_input_loc (t),
		  "call to internal function %qE", t);
      *non_constant_p = true;
      return t;
    }

  /* Evaluate constant arguments using OPCODE and return a complex
     number containing the result and the overflow bit.  */
  tree arg0 = cxx_eval_constant_expression (ctx, CALL_EXPR_ARG (t, 0), lval,
					    non_constant_p, overflow_p);
  tree arg1 = cxx_eval_constant_expression (ctx, CALL_EXPR_ARG (t, 1), lval,
					    non_constant_p, overflow_p);

  if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
    {
      location_t loc = cp_expr_loc_or_input_loc (t);
      tree type = TREE_TYPE (TREE_TYPE (t));
      tree result = fold_binary_loc (loc, opcode, type,
				     fold_convert_loc (loc, type, arg0),
				     fold_convert_loc (loc, type, arg1));
      tree ovf
	= build_int_cst (type, arith_overflowed_p (opcode, type, arg0, arg1));
      /* Reset TREE_OVERFLOW to avoid warnings for the overflow.  */
      if (TREE_OVERFLOW (result))
	TREE_OVERFLOW (result) = 0;

      return build_complex (TREE_TYPE (t), result, ovf);
    }

  *non_constant_p = true;
  return t;
}

/* Clean CONSTRUCTOR_NO_CLEARING from CTOR and its sub-aggregates.  */

static void
clear_no_implicit_zero (tree ctor)
{
  if (CONSTRUCTOR_NO_CLEARING (ctor))
    {
      CONSTRUCTOR_NO_CLEARING (ctor) = false;
      for (auto &e: CONSTRUCTOR_ELTS (ctor))
	if (TREE_CODE (e.value) == CONSTRUCTOR)
	  clear_no_implicit_zero (e.value);
    }
}

/* Complain about a const object OBJ being modified in a constant expression.
   EXPR is the MODIFY_EXPR expression performing the modification.  */

static void
modifying_const_object_error (tree expr, tree obj)
{
  location_t loc = cp_expr_loc_or_input_loc (expr);
  auto_diagnostic_group d;
  error_at (loc, "modifying a const object %qE is not allowed in "
	    "a constant expression", TREE_OPERAND (expr, 0));

  /* Find the underlying object that was declared as const.  */
  location_t decl_loc = UNKNOWN_LOCATION;
  for (tree probe = obj; decl_loc == UNKNOWN_LOCATION; )
    switch (TREE_CODE (probe))
      {
      case BIT_FIELD_REF:
      case COMPONENT_REF:
	{
	  tree elt = TREE_OPERAND (probe, 1);
	  if (CP_TYPE_CONST_P (TREE_TYPE (elt)))
	    decl_loc = DECL_SOURCE_LOCATION (elt);
	  probe = TREE_OPERAND (probe, 0);
	}
	break;

      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	probe = TREE_OPERAND (probe, 0);
	break;

      default:
	decl_loc = location_of (probe);
	break;
      }
  inform (decl_loc, "originally declared %<const%> here");
}

/* Return true if FNDECL is a replaceable global allocation function that
   should be useable during constant expression evaluation.  */

static inline bool
cxx_replaceable_global_alloc_fn (tree fndecl)
{
  return (cxx_dialect >= cxx20
	  && IDENTIFIER_NEWDEL_OP_P (DECL_NAME (fndecl))
	  && CP_DECL_CONTEXT (fndecl) == global_namespace
	  && (DECL_IS_REPLACEABLE_OPERATOR_NEW_P (fndecl)
	      || DECL_IS_OPERATOR_DELETE_P (fndecl)));
}

/* Return true if FNDECL is a placement new function that should be
   useable during constant expression evaluation of std::construct_at.  */

static inline bool
cxx_placement_new_fn (tree fndecl)
{
  return (cxx_dialect >= cxx20 && std_placement_new_fn_p (fndecl));
}

/* Return true if FNDECL is std::construct_at.  */

static inline bool
is_std_construct_at (tree fndecl)
{
  if (!decl_in_std_namespace_p (fndecl))
    return false;

  tree name = DECL_NAME (fndecl);
  return name && id_equal (name, "construct_at");
}

/* Overload for the above taking constexpr_call*.  */

static inline bool
is_std_construct_at (const constexpr_call *call)
{
  return (call
	  && call->fundef
	  && is_std_construct_at (call->fundef->decl));
}

/* True if CTX is an instance of std::NAME class.  */

bool
is_std_class (tree ctx, const char *name)
{
  if (ctx == NULL_TREE || !CLASS_TYPE_P (ctx) || !TYPE_MAIN_DECL (ctx))
    return false;

  tree decl = TYPE_MAIN_DECL (ctx);
  tree dname = DECL_NAME (decl);
  if (dname == NULL_TREE || !id_equal (dname, name))
    return false;

  return decl_in_std_namespace_p (decl);
}

/* True if CTX is an instance of std::allocator.  */

bool
is_std_allocator (tree ctx)
{
  return is_std_class (ctx, "allocator");
}

/* Return true if FNDECL is std::allocator<T>::{,de}allocate.  */

static inline bool
is_std_allocator_allocate (tree fndecl)
{
  tree name = DECL_NAME (fndecl);
  if (name == NULL_TREE
      || !(id_equal (name, "allocate") || id_equal (name, "deallocate")))
    return false;

  return is_std_allocator (DECL_CONTEXT (fndecl));
}

/* Overload for the above taking constexpr_call*.  */

static inline bool
is_std_allocator_allocate (const constexpr_call *call)
{
  return (call
	  && call->fundef
	  && is_std_allocator_allocate (call->fundef->decl));
}

/* Return true if FNDECL is std::source_location::current.  */

static inline bool
is_std_source_location_current (tree fndecl)
{
  if (!decl_in_std_namespace_p (fndecl))
    return false;

  tree name = DECL_NAME (fndecl);
  if (name == NULL_TREE || !id_equal (name, "current"))
    return false;

  tree ctx = DECL_CONTEXT (fndecl);
  if (ctx == NULL_TREE || !CLASS_TYPE_P (ctx) || !TYPE_MAIN_DECL (ctx))
    return false;

  name = DECL_NAME (TYPE_MAIN_DECL (ctx));
  return name && id_equal (name, "source_location");
}

/* Overload for the above taking constexpr_call*.  */

static inline bool
is_std_source_location_current (const constexpr_call *call)
{
  return (call
	  && call->fundef
	  && is_std_source_location_current (call->fundef->decl));
}

/* Return true if FNDECL is __dynamic_cast.  */

static inline bool
cxx_dynamic_cast_fn_p (tree fndecl)
{
  return (cxx_dialect >= cxx20
	  && id_equal (DECL_NAME (fndecl), "__dynamic_cast")
	  && CP_DECL_CONTEXT (fndecl) == abi_node);
}

/* Often, we have an expression in the form of address + offset, e.g.
   "&_ZTV1A + 16".  Extract the object from it, i.e. "_ZTV1A".  */

static tree
extract_obj_from_addr_offset (tree expr)
{
  if (TREE_CODE (expr) == POINTER_PLUS_EXPR)
    expr = TREE_OPERAND (expr, 0);
  STRIP_NOPS (expr);
  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);
  return expr;
}

/* Given a PATH like

     g.D.2181.D.2154.D.2102.D.2093

   find a component with type TYPE.  Return NULL_TREE if not found, and
   error_mark_node if the component is not accessible.  If STOP is non-null,
   this function will return NULL_TREE if STOP is found before TYPE.  */

static tree
get_component_with_type (tree path, tree type, tree stop)
{
  while (true)
    {
      if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (path), type))
	/* Found it.  */
	return path;
      else if (stop
	       && (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (path),
							      stop)))
	return NULL_TREE;
      else if (TREE_CODE (path) == COMPONENT_REF
	       && DECL_FIELD_IS_BASE (TREE_OPERAND (path, 1)))
	{
	  /* We need to check that the component we're accessing is in fact
	     accessible.  */
	  if (TREE_PRIVATE (TREE_OPERAND (path, 1))
	      || TREE_PROTECTED (TREE_OPERAND (path, 1)))
	    return error_mark_node;
	  path = TREE_OPERAND (path, 0);
	}
      else
	return NULL_TREE;
    }
}

/* Evaluate a call to __dynamic_cast (permitted by P1327R1).

   The declaration of __dynamic_cast is:

   void* __dynamic_cast (const void* __src_ptr,
			 const __class_type_info* __src_type,
			 const __class_type_info* __dst_type,
			 ptrdiff_t __src2dst);

   where src2dst has the following possible values

   >-1: src_type is a unique public non-virtual base of dst_type
	dst_ptr + src2dst == src_ptr
   -1: unspecified relationship
   -2: src_type is not a public base of dst_type
   -3: src_type is a multiple public non-virtual base of dst_type

  Since literal types can't have virtual bases, we only expect hint >=0,
  -2, or -3.  */

static tree
cxx_eval_dynamic_cast_fn (const constexpr_ctx *ctx, tree call,
			  bool *non_constant_p, bool *overflow_p)
{
  /* T will be something like
      __dynamic_cast ((B*) b, &_ZTI1B, &_ZTI1D, 8)
     dismantle it.  */
  gcc_assert (call_expr_nargs (call) == 4);
  tsubst_flags_t complain = ctx->quiet ? tf_none : tf_warning_or_error;
  tree obj = CALL_EXPR_ARG (call, 0);
  tree type = CALL_EXPR_ARG (call, 2);
  HOST_WIDE_INT hint = int_cst_value (CALL_EXPR_ARG (call, 3));
  location_t loc = cp_expr_loc_or_input_loc (call);

  /* Get the target type of the dynamic_cast.  */
  gcc_assert (TREE_CODE (type) == ADDR_EXPR);
  type = TREE_OPERAND (type, 0);
  type = TREE_TYPE (DECL_NAME (type));

  /* TYPE can only be either T* or T&.  We can't know which of these it
     is by looking at TYPE, but OBJ will be "(T*) x" in the first case,
     and something like "(T*)(T&)(T*) x" in the second case.  */
  bool reference_p = false;
  while (CONVERT_EXPR_P (obj) || TREE_CODE (obj) == SAVE_EXPR)
    {
      reference_p |= TYPE_REF_P (TREE_TYPE (obj));
      obj = TREE_OPERAND (obj, 0);
    }

  /* Evaluate the object so that we know its dynamic type.  */
  obj = cxx_eval_constant_expression (ctx, obj, vc_prvalue, non_constant_p,
				      overflow_p);
  if (*non_constant_p)
    return call;

  /* We expect OBJ to be in form of &d.D.2102 when HINT == 0,
     but when HINT is > 0, it can also be something like
     &d.D.2102 + 18446744073709551608, which includes the BINFO_OFFSET.  */
  obj = extract_obj_from_addr_offset (obj);
  const tree objtype = TREE_TYPE (obj);
  /* If OBJ doesn't refer to a base field, we're done.  */
  if (tree t = (TREE_CODE (obj) == COMPONENT_REF
		? TREE_OPERAND (obj, 1) : obj))
    if (TREE_CODE (t) != FIELD_DECL || !DECL_FIELD_IS_BASE (t))
      {
	if (reference_p)
	  {
	    if (!ctx->quiet)
	      {
		auto_diagnostic_group d;
		error_at (loc, "reference %<dynamic_cast%> failed");
		inform (loc, "dynamic type %qT of its operand does "
			"not have a base class of type %qT",
			objtype, type);
	      }
	    *non_constant_p = true;
	  }
	return integer_zero_node;
      }

  /* [class.cdtor] When a dynamic_cast is used in a constructor ...
     or in a destructor ... if the operand of the dynamic_cast refers
     to the object under construction or destruction, this object is
     considered to be a most derived object that has the type of the
     constructor or destructor's class.  */
  tree vtable = build_vfield_ref (obj, objtype);
  vtable = cxx_eval_constant_expression (ctx, vtable, vc_prvalue,
					 non_constant_p, overflow_p);
  if (*non_constant_p)
    return call;
  /* With -fsanitize=vptr, we initialize all vtable pointers to null,
     so it's possible that we got a null pointer now.  */
  if (integer_zerop (vtable))
    {
      if (!ctx->quiet)
	error_at (loc, "virtual table pointer is used uninitialized");
      *non_constant_p = true;
      return integer_zero_node;
    }
  /* VTABLE will be &_ZTV1A + 16 or similar, get _ZTV1A.  */
  vtable = extract_obj_from_addr_offset (vtable);
  const tree mdtype = DECL_CONTEXT (vtable);

  /* Given dynamic_cast<T>(v),

     [expr.dynamic.cast] If C is the class type to which T points or refers,
     the runtime check logically executes as follows:

     If, in the most derived object pointed (referred) to by v, v points
     (refers) to a public base class subobject of a C object, and if only
     one object of type C is derived from the subobject pointed (referred)
     to by v the result points (refers) to that C object.

     In this case, HINT >= 0 or -3.  */
  if (hint >= 0 || hint == -3)
    {
      /* Look for a component with type TYPE.  */
      tree t = get_component_with_type (obj, type, mdtype);
      /* If not accessible, give an error.  */
      if (t == error_mark_node)
	{
	  if (reference_p)
	    {
	      if (!ctx->quiet)
		{
		  auto_diagnostic_group d;
		  error_at (loc, "reference %<dynamic_cast%> failed");
		  inform (loc, "static type %qT of its operand is a "
			  "non-public base class of dynamic type %qT",
			  objtype, type);

		}
	      *non_constant_p = true;
	    }
	  return integer_zero_node;
	}
      else if (t)
	/* The result points to the TYPE object.  */
	return cp_build_addr_expr (t, complain);
      /* Else, TYPE was not found, because the HINT turned out to be wrong.
	 Fall through to the normal processing.  */
    }

  /* Otherwise, if v points (refers) to a public base class subobject of the
     most derived object, and the type of the most derived object has a base
     class, of type C, that is unambiguous and public, the result points
     (refers) to the C subobject of the most derived object.

     But it can also be an invalid case.  */

  /* Get the most derived object.  */
  obj = get_component_with_type (obj, mdtype, NULL_TREE);
  if (obj == error_mark_node)
    {
      if (reference_p)
	{
	  if (!ctx->quiet)
	    {
	      auto_diagnostic_group d;
	      error_at (loc, "reference %<dynamic_cast%> failed");
	      inform (loc, "static type %qT of its operand is a non-public"
		      " base class of dynamic type %qT", objtype, mdtype);
	    }
	  *non_constant_p = true;
	}
      return integer_zero_node;
    }
  else
    gcc_assert (obj);

  /* Check that the type of the most derived object has a base class
     of type TYPE that is unambiguous and public.  */
  base_kind b_kind;
  tree binfo = lookup_base (mdtype, type, ba_check, &b_kind, tf_none);
  if (!binfo || binfo == error_mark_node)
    {
      if (reference_p)
	{
	  if (!ctx->quiet)
	    {
	      auto_diagnostic_group d;
	      error_at (loc, "reference %<dynamic_cast%> failed");
	      if (b_kind == bk_ambig)
		inform (loc, "%qT is an ambiguous base class of dynamic "
			"type %qT of its operand", type, mdtype);
	      else
		inform (loc, "dynamic type %qT of its operand does not "
			"have an unambiguous public base class %qT",
			mdtype, type);
	    }
	  *non_constant_p = true;
	}
      return integer_zero_node;
    }
  /* If so, return the TYPE subobject of the most derived object.  */
  obj = convert_to_base_statically (obj, binfo);
  return cp_build_addr_expr (obj, complain);
}

/* Data structure used by replace_decl and replace_decl_r.  */

struct replace_decl_data
{
  /* The _DECL we want to replace.  */
  tree decl;
  /* The replacement for DECL.  */
  tree replacement;
  /* Trees we've visited.  */
  hash_set<tree> *pset;
  /* Whether we've performed any replacements.  */
  bool changed;
};

/* Helper function for replace_decl, called through cp_walk_tree.  */

static tree
replace_decl_r (tree *tp, int *walk_subtrees, void *data)
{
  replace_decl_data *d = (replace_decl_data *) data;

  if (*tp == d->decl)
    {
      *tp = unshare_expr (d->replacement);
      d->changed = true;
      *walk_subtrees = 0;
    }
  else if (TYPE_P (*tp)
	   || d->pset->add (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Replace every occurrence of DECL with (an unshared copy of)
   REPLACEMENT within the expression *TP.  Returns true iff a
   replacement was performed.  */

bool
replace_decl (tree *tp, tree decl, tree replacement)
{
  gcc_checking_assert (same_type_ignoring_top_level_qualifiers_p
		       (TREE_TYPE (decl), TREE_TYPE (replacement)));
  hash_set<tree> pset;
  replace_decl_data data = { decl, replacement, &pset, false };
  cp_walk_tree (tp, replace_decl_r, &data, NULL);
  return data.changed;
}

/* Evaluate the call T to virtual function thunk THUNK_FNDECL.  */

static tree
cxx_eval_thunk_call (const constexpr_ctx *ctx, tree t, tree thunk_fndecl,
		     value_cat lval,
		     bool *non_constant_p, bool *overflow_p)
{
  tree function = THUNK_TARGET (thunk_fndecl);

  if (THUNK_VIRTUAL_OFFSET (thunk_fndecl))
    {
      if (!ctx->quiet)
	{
	  if (!DECL_DECLARED_CONSTEXPR_P (function))
	    {
	      error ("call to non-%<constexpr%> function %qD", function);
	      explain_invalid_constexpr_fn (function);
	    }
	  else
	    /* virtual_offset is only set for virtual bases, which make the
	       class non-literal, so we don't need to handle it here.  */
	    error ("calling constexpr member function %qD through virtual "
		   "base subobject", function);
	}
      *non_constant_p = true;
      return t;
    }

  tree new_call = copy_node (t);
  CALL_EXPR_FN (new_call) = function;
  TREE_TYPE (new_call) = TREE_TYPE (TREE_TYPE (function));

  tree offset = size_int (THUNK_FIXED_OFFSET (thunk_fndecl));

  if (DECL_THIS_THUNK_P (thunk_fndecl))
    {
      /* 'this'-adjusting thunk.  */
      tree this_arg = CALL_EXPR_ARG (t, 0);
      this_arg = build2 (POINTER_PLUS_EXPR, TREE_TYPE (this_arg),
			 this_arg, offset);
      CALL_EXPR_ARG (new_call, 0) = this_arg;
    }
  else
    /* Return-adjusting thunk.  */
    new_call = build2 (POINTER_PLUS_EXPR, TREE_TYPE (new_call),
		       new_call, offset);

  return cxx_eval_constant_expression (ctx, new_call, lval,
				       non_constant_p, overflow_p);
}

/* If OBJECT is of const class type, evaluate it to a CONSTRUCTOR and set
   its TREE_READONLY flag according to READONLY_P.  Used for constexpr
   'tors to detect modifying const objects in a constexpr context.  */

static void
cxx_set_object_constness (const constexpr_ctx *ctx, tree object,
			  bool readonly_p, bool *non_constant_p,
			  bool *overflow_p)
{
  if (CLASS_TYPE_P (TREE_TYPE (object))
      && CP_TYPE_CONST_P (TREE_TYPE (object)))
    {
      /* Subobjects might not be stored in ctx->global->values but we
	 can get its CONSTRUCTOR by evaluating *this.  */
      tree e = cxx_eval_constant_expression (ctx, object, vc_prvalue,
					     non_constant_p, overflow_p);
      if (TREE_CODE (e) == CONSTRUCTOR && !*non_constant_p)
	TREE_READONLY (e) = readonly_p;
    }
}

/* Subroutine of cxx_eval_constant_expression.
   Evaluate the call expression tree T in the context of OLD_CALL expression
   evaluation.  */

static tree
cxx_eval_call_expression (const constexpr_ctx *ctx, tree t,
			  value_cat lval,
			  bool *non_constant_p, bool *overflow_p)
{
  location_t loc = cp_expr_loc_or_input_loc (t);
  tree fun = get_function_named_in_call (t);
  constexpr_call new_call
    = { NULL, NULL, NULL, 0, ctx->manifestly_const_eval };
  int depth_ok;

  if (fun == NULL_TREE)
    return cxx_eval_internal_function (ctx, t, lval,
				       non_constant_p, overflow_p);

  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      /* Might be a constexpr function pointer.  */
      fun = cxx_eval_constant_expression (ctx, fun, vc_prvalue,
					  non_constant_p, overflow_p);
      STRIP_NOPS (fun);
      if (TREE_CODE (fun) == ADDR_EXPR)
	fun = TREE_OPERAND (fun, 0);
      /* For TARGET_VTABLE_USES_DESCRIPTORS targets, there is no
	 indirection, the called expression is a pointer into the
	 virtual table which should contain FDESC_EXPR.  Extract the
	 FUNCTION_DECL from there.  */
      else if (TARGET_VTABLE_USES_DESCRIPTORS
	       && TREE_CODE (fun) == POINTER_PLUS_EXPR
	       && TREE_CODE (TREE_OPERAND (fun, 0)) == ADDR_EXPR
	       && TREE_CODE (TREE_OPERAND (fun, 1)) == INTEGER_CST)
	{
	  tree d = TREE_OPERAND (TREE_OPERAND (fun, 0), 0);
	  if (VAR_P (d)
	      && DECL_VTABLE_OR_VTT_P (d)
	      && TREE_CODE (TREE_TYPE (d)) == ARRAY_TYPE
	      && TREE_TYPE (TREE_TYPE (d)) == vtable_entry_type
	      && DECL_INITIAL (d)
	      && TREE_CODE (DECL_INITIAL (d)) == CONSTRUCTOR)
	    {
	      tree i = int_const_binop (TRUNC_DIV_EXPR, TREE_OPERAND (fun, 1),
					TYPE_SIZE_UNIT (vtable_entry_type));
	      HOST_WIDE_INT idx = find_array_ctor_elt (DECL_INITIAL (d), i);
	      if (idx >= 0)
		{
		  tree fdesc
		    = (*CONSTRUCTOR_ELTS (DECL_INITIAL (d)))[idx].value;
		  if (TREE_CODE (fdesc) == FDESC_EXPR
		      && integer_zerop (TREE_OPERAND (fdesc, 1)))
		    fun = TREE_OPERAND (fdesc, 0);
		}
	    }
	}
    }
  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      if (!ctx->quiet && !*non_constant_p)
	error_at (loc, "expression %qE does not designate a %<constexpr%> "
		  "function", fun);
      *non_constant_p = true;
      return t;
    }
  if (DECL_CLONED_FUNCTION_P (fun) && !DECL_DELETING_DESTRUCTOR_P (fun))
    fun = DECL_CLONED_FUNCTION (fun);

  if (is_ubsan_builtin_p (fun))
    return void_node;

  if (fndecl_built_in_p (fun))
    return cxx_eval_builtin_function_call (ctx, t, fun,
					   lval, non_constant_p, overflow_p);
  if (DECL_THUNK_P (fun))
    return cxx_eval_thunk_call (ctx, t, fun, lval, non_constant_p, overflow_p);
  if (!maybe_constexpr_fn (fun))
    {
      if (TREE_CODE (t) == CALL_EXPR
	  && cxx_replaceable_global_alloc_fn (fun)
	  && (CALL_FROM_NEW_OR_DELETE_P (t)
	      || is_std_allocator_allocate (ctx->call)))
	{
	  const bool new_op_p = IDENTIFIER_NEW_OP_P (DECL_NAME (fun));
	  const int nargs = call_expr_nargs (t);
	  tree arg0 = NULL_TREE;
	  for (int i = 0; i < nargs; ++i)
	    {
	      tree arg = CALL_EXPR_ARG (t, i);
	      arg = cxx_eval_constant_expression (ctx, arg, vc_prvalue,
						  non_constant_p, overflow_p);
	      /* Deleting a non-constant pointer has a better error message
		 below.  */
	      if (new_op_p || i != 0)
		VERIFY_CONSTANT (arg);
	      if (i == 0)
		arg0 = arg;
	    }
	  gcc_assert (arg0);
	  if (new_op_p)
	    {
	      tree type = build_array_type_nelts (char_type_node,
						  tree_to_uhwi (arg0));
	      tree var = build_decl (loc, VAR_DECL,
				     (IDENTIFIER_OVL_OP_FLAGS (DECL_NAME (fun))
				      & OVL_OP_FLAG_VEC)
				     ? heap_vec_uninit_identifier
				     : heap_uninit_identifier,
				     type);
	      DECL_ARTIFICIAL (var) = 1;
	      TREE_STATIC (var) = 1;
	      // Temporarily register the artificial var in varpool,
	      // so that comparisons of its address against NULL are folded
	      // through nonzero_address even with
	      // -fno-delete-null-pointer-checks or that comparison of
	      // addresses of different heap artificial vars is folded too.
	      // See PR98988 and PR99031.
	      varpool_node::finalize_decl (var);
	      ctx->global->heap_vars.safe_push (var);
	      ctx->global->put_value (var, NULL_TREE);
	      return fold_convert (ptr_type_node, build_address (var));
	    }
	  else
	    {
	      STRIP_NOPS (arg0);
	      if (TREE_CODE (arg0) == ADDR_EXPR
		  && VAR_P (TREE_OPERAND (arg0, 0)))
		{
		  tree var = TREE_OPERAND (arg0, 0);
		  if (DECL_NAME (var) == heap_uninit_identifier
		      || DECL_NAME (var) == heap_identifier)
		    {
		      if (IDENTIFIER_OVL_OP_FLAGS (DECL_NAME (fun))
			  & OVL_OP_FLAG_VEC)
			{
			  if (!ctx->quiet)
			    {
			      auto_diagnostic_group d;
			      error_at (loc, "array deallocation of object "
					     "allocated with non-array "
					     "allocation");
			      inform (DECL_SOURCE_LOCATION (var),
				      "allocation performed here");
			    }
			  *non_constant_p = true;
			  return t;
			}
		      DECL_NAME (var) = heap_deleted_identifier;
		      ctx->global->destroy_value (var);
		      ctx->global->heap_dealloc_count++;
		      return void_node;
		    }
		  else if (DECL_NAME (var) == heap_vec_uninit_identifier
			   || DECL_NAME (var) == heap_vec_identifier)
		    {
		      if ((IDENTIFIER_OVL_OP_FLAGS (DECL_NAME (fun))
			   & OVL_OP_FLAG_VEC) == 0)
			{
			  if (!ctx->quiet)
			    {
			      auto_diagnostic_group d;
			      error_at (loc, "non-array deallocation of "
					     "object allocated with array "
					     "allocation");
			      inform (DECL_SOURCE_LOCATION (var),
				      "allocation performed here");
			    }
			  *non_constant_p = true;
			  return t;
			}
		      DECL_NAME (var) = heap_deleted_identifier;
		      ctx->global->destroy_value (var);
		      ctx->global->heap_dealloc_count++;
		      return void_node;
		    }
		  else if (DECL_NAME (var) == heap_deleted_identifier)
		    {
		      if (!ctx->quiet)
			error_at (loc, "deallocation of already deallocated "
				       "storage");
		      *non_constant_p = true;
		      return t;
		    }
		}
	      if (!ctx->quiet)
		error_at (loc, "deallocation of storage that was "
			       "not previously allocated");
	      *non_constant_p = true;
	      return t;
	    }
	}
      /* Allow placement new in std::construct_at, just return the second
	 argument.  */
      if (TREE_CODE (t) == CALL_EXPR
	  && cxx_placement_new_fn (fun)
	  && is_std_construct_at (ctx->call))
	{
	  const int nargs = call_expr_nargs (t);
	  tree arg1 = NULL_TREE;
	  for (int i = 0; i < nargs; ++i)
	    {
	      tree arg = CALL_EXPR_ARG (t, i);
	      arg = cxx_eval_constant_expression (ctx, arg, vc_prvalue,
						  non_constant_p, overflow_p);
	      if (i == 1)
		arg1 = arg;
	      else
		VERIFY_CONSTANT (arg);
	    }
	  gcc_assert (arg1);
	  return arg1;
	}
      else if (cxx_dynamic_cast_fn_p (fun))
	return cxx_eval_dynamic_cast_fn (ctx, t, non_constant_p, overflow_p);

      if (!ctx->quiet)
	{
	  if (!lambda_static_thunk_p (fun))
	    error_at (loc, "call to non-%<constexpr%> function %qD", fun);
	  explain_invalid_constexpr_fn (fun);
	}
      *non_constant_p = true;
      return t;
    }

  constexpr_ctx new_ctx = *ctx;
  if (DECL_CONSTRUCTOR_P (fun) && !ctx->object
      && TREE_CODE (t) == AGGR_INIT_EXPR)
    {
      /* We want to have an initialization target for an AGGR_INIT_EXPR.
	 If we don't already have one in CTX, use the AGGR_INIT_EXPR_SLOT.  */
      new_ctx.object = AGGR_INIT_EXPR_SLOT (t);
      tree ctor = new_ctx.ctor = build_constructor (DECL_CONTEXT (fun), NULL);
      CONSTRUCTOR_NO_CLEARING (ctor) = true;
      ctx->global->put_value (new_ctx.object, ctor);
      ctx = &new_ctx;
    }

  /* We used to shortcut trivial constructor/op= here, but nowadays
     we can only get a trivial function here with -fno-elide-constructors.  */
  gcc_checking_assert (!trivial_fn_p (fun)
		       || !flag_elide_constructors
		       /* We don't elide constructors when processing
			  a noexcept-expression.  */
		       || cp_noexcept_operand);

  bool non_constant_args = false;
  new_call.bindings
    = cxx_bind_parameters_in_call (ctx, t, fun, non_constant_p,
				   overflow_p, &non_constant_args);

  /* We build up the bindings list before we know whether we already have this
     call cached.  If we don't end up saving these bindings, ggc_free them when
     this function exits.  */
  class free_bindings
  {
    tree *bindings;
  public:
    free_bindings (tree &b): bindings (&b) { }
    ~free_bindings () { if (bindings) ggc_free (*bindings); }
    void preserve () { bindings = NULL; }
  } fb (new_call.bindings);

  if (*non_constant_p)
    return t;

  /* We can't defer instantiating the function any longer.  */
  if (!DECL_INITIAL (fun)
      && (DECL_TEMPLOID_INSTANTIATION (fun) || DECL_DEFAULTED_FN (fun))
      && !uid_sensitive_constexpr_evaluation_p ())
    {
      location_t save_loc = input_location;
      input_location = loc;
      ++function_depth;
      if (ctx->manifestly_const_eval == mce_true)
	FNDECL_MANIFESTLY_CONST_EVALUATED (fun) = true;
      if (DECL_TEMPLOID_INSTANTIATION (fun))
	instantiate_decl (fun, /*defer_ok*/false, /*expl_inst*/false);
      else
	synthesize_method (fun);
      --function_depth;
      input_location = save_loc;
    }

  /* If in direct recursive call, optimize definition search.  */
  if (ctx && ctx->call && ctx->call->fundef && ctx->call->fundef->decl == fun)
    new_call.fundef = ctx->call->fundef;
  else
    {
      new_call.fundef = retrieve_constexpr_fundef (fun);
      if (new_call.fundef == NULL || new_call.fundef->body == NULL
	  || new_call.fundef->result == error_mark_node
	  || fun == current_function_decl)
        {
	  if (!ctx->quiet)
	    {
	      /* We need to check for current_function_decl here in case we're
		 being called during cp_fold_function, because at that point
		 DECL_INITIAL is set properly and we have a fundef but we
		 haven't lowered invisirefs yet (c++/70344).  */
	      if (DECL_INITIAL (fun) == error_mark_node
		  || fun == current_function_decl)
		error_at (loc, "%qD called in a constant expression before its "
			  "definition is complete", fun);
	      else if (DECL_INITIAL (fun))
		{
		  /* The definition of fun was somehow unsuitable.  But pretend
		     that lambda static thunks don't exist.  */
		  if (!lambda_static_thunk_p (fun))
		    error_at (loc, "%qD called in a constant expression", fun);
		  explain_invalid_constexpr_fn (fun);
		}
	      else
		error_at (loc, "%qD used before its definition", fun);
	    }
	  *non_constant_p = true;
          return t;
        }
    }

  depth_ok = push_cx_call_context (t);

  /* Remember the object we are constructing or destructing.  */
  tree new_obj = NULL_TREE;
  if (DECL_CONSTRUCTOR_P (fun) || DECL_DESTRUCTOR_P (fun))
    {
      /* In a cdtor, it should be the first `this' argument.
	 At this point it has already been evaluated in the call
	 to cxx_bind_parameters_in_call.  */
      new_obj = TREE_VEC_ELT (new_call.bindings, 0);
      bool empty_base = false;
      new_obj = cxx_fold_indirect_ref (ctx, loc, DECL_CONTEXT (fun), new_obj,
				       &empty_base);
      /* If we're initializing an empty class, don't set constness, because
	 cxx_fold_indirect_ref will return the wrong object to set constness
	 of.  */
      if (empty_base)
	new_obj = NULL_TREE;
      else if (ctx->call && ctx->call->fundef
	       && DECL_CONSTRUCTOR_P (ctx->call->fundef->decl))
	{
	  tree cur_obj = TREE_VEC_ELT (ctx->call->bindings, 0);
	  STRIP_NOPS (cur_obj);
	  if (TREE_CODE (cur_obj) == ADDR_EXPR)
	    cur_obj = TREE_OPERAND (cur_obj, 0);
	  if (new_obj == cur_obj)
	    /* We're calling the target constructor of a delegating
	       constructor, or accessing a base subobject through a
	       NOP_EXPR as part of a call to a base constructor, so
	       there is no new (sub)object.  */
	    new_obj = NULL_TREE;
	}
    }

  tree result = NULL_TREE;

  constexpr_call *entry = NULL;
  if (depth_ok && !non_constant_args && ctx->strict)
    {
      new_call.hash = constexpr_fundef_hasher::hash (new_call.fundef);
      new_call.hash
	= iterative_hash_template_arg (new_call.bindings, new_call.hash);
      new_call.hash
	= iterative_hash_object (ctx->manifestly_const_eval, new_call.hash);

      /* If we have seen this call before, we are done.  */
      maybe_initialize_constexpr_call_table ();
      bool insert = depth_ok < constexpr_cache_depth;
      constexpr_call **slot
	= constexpr_call_table->find_slot (&new_call,
					   insert ? INSERT : NO_INSERT);
      entry = slot ? *slot : NULL;
      if (entry == NULL)
	{
	  /* Only cache up to constexpr_cache_depth to limit memory use.  */
	  if (insert)
	    {
	      /* We need to keep a pointer to the entry, not just the slot, as
		 the slot can move during evaluation of the body.  */
	      *slot = entry = ggc_alloc<constexpr_call> ();
	      *entry = new_call;
	      fb.preserve ();
	    }
	}
      /* Calls that are in progress have their result set to NULL, so that we
	 can detect circular dependencies.  Now that we only cache up to
	 constexpr_cache_depth this won't catch circular dependencies that
	 start deeper, but they'll hit the recursion or ops limit.  */
      else if (entry->result == NULL)
	{
	  if (!ctx->quiet)
	    error ("call has circular dependency");
	  *non_constant_p = true;
	  entry->result = result = error_mark_node;
	}
      else
	result = entry->result;
    }

  if (!depth_ok)
    {
      if (!ctx->quiet)
	error ("%<constexpr%> evaluation depth exceeds maximum of %d (use "
	       "%<-fconstexpr-depth=%> to increase the maximum)",
	       max_constexpr_depth);
      *non_constant_p = true;
      result = error_mark_node;
    }
  else
    {
      bool cacheable = !!entry;
      if (result && result != error_mark_node)
	/* OK */;
      else if (!DECL_SAVED_TREE (fun))
	{
	  /* When at_eof >= 3, cgraph has started throwing away
	     DECL_SAVED_TREE, so fail quietly.  FIXME we get here because of
	     late code generation for VEC_INIT_EXPR, which needs to be
	     completely reconsidered.  */
	  gcc_assert (at_eof >= 3 && ctx->quiet);
	  *non_constant_p = true;
	}
      else if (tree copy = get_fundef_copy (new_call.fundef))
	{
	  tree body, parms, res;
	  releasing_vec ctors;

	  /* Reuse or create a new unshared copy of this function's body.  */
	  body = TREE_PURPOSE (copy);
	  parms = TREE_VALUE (copy);
	  res = TREE_TYPE (copy);

	  /* Associate the bindings with the remapped parms.  */
	  tree bound = new_call.bindings;
	  tree remapped = parms;
	  for (int i = 0; i < TREE_VEC_LENGTH (bound); ++i)
	    {
	      tree arg = TREE_VEC_ELT (bound, i);
	      if (entry)
		{
		  /* Unshare args going into the hash table to separate them
		     from the caller's context, for better GC and to avoid
		     problems with verify_gimple.  */
		  arg = unshare_expr_without_location (arg);
		  TREE_VEC_ELT (bound, i) = arg;

		  /* And then unshare again so the callee doesn't change the
		     argument values in the hash table. XXX Could we unshare
		     lazily in cxx_eval_store_expression?  */
		  arg = unshare_constructor (arg);
		  if (TREE_CODE (arg) == CONSTRUCTOR)
		    vec_safe_push (ctors, arg);
		}
	      ctx->global->put_value (remapped, arg);
	      remapped = DECL_CHAIN (remapped);
	    }
	  if (remapped)
	    {
	      /* We shouldn't have any parms without args, but fail gracefully
		 in error recovery.  */
	      gcc_checking_assert (seen_error ());
	      *non_constant_p = true;
	    }
	  /* Add the RESULT_DECL to the values map, too.  */
	  gcc_assert (!DECL_BY_REFERENCE (res));
	  ctx->global->put_value (res, NULL_TREE);

	  /* Remember the current call we're evaluating.  */
	  constexpr_ctx call_ctx = *ctx;
	  call_ctx.call = &new_call;
	  unsigned save_heap_alloc_count = ctx->global->heap_vars.length ();
	  unsigned save_heap_dealloc_count = ctx->global->heap_dealloc_count;

	  /* Make sure we fold std::is_constant_evaluated to true in an
	     immediate function.  */
	  if (DECL_IMMEDIATE_FUNCTION_P (fun))
	    call_ctx.manifestly_const_eval = mce_true;

	  /* If this is a constexpr destructor, the object's const and volatile
	     semantics are no longer in effect; see [class.dtor]p5.  */
	  if (new_obj && DECL_DESTRUCTOR_P (fun))
	    cxx_set_object_constness (ctx, new_obj, /*readonly_p=*/false,
				      non_constant_p, overflow_p);

	  /* If this is a constructor, we are beginning the lifetime of the
	     object we are initializing.  */
	  if (new_obj
	      && DECL_CONSTRUCTOR_P (fun)
	      && TREE_CODE (new_obj) == COMPONENT_REF
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (new_obj, 0))) == UNION_TYPE)
	    {
	      tree activate = build2 (INIT_EXPR, TREE_TYPE (new_obj),
				      new_obj,
				      build_constructor (TREE_TYPE (new_obj),
							 NULL));
	      cxx_eval_constant_expression (ctx, activate,
					    lval, non_constant_p, overflow_p);
	      ggc_free (activate);
	    }

	  tree jump_target = NULL_TREE;
	  cxx_eval_constant_expression (&call_ctx, body,
					vc_discard, non_constant_p, overflow_p,
					&jump_target);

	  if (DECL_CONSTRUCTOR_P (fun))
	    /* This can be null for a subobject constructor call, in
	       which case what we care about is the initialization
	       side-effects rather than the value.  We could get at the
	       value by evaluating *this, but we don't bother; there's
	       no need to put such a call in the hash table.  */
	    result = lval ? ctx->object : ctx->ctor;
	  else if (VOID_TYPE_P (TREE_TYPE (res)))
	    result = void_node;
	  else
	    {
	      result = ctx->global->get_value (res);
	      if (result == NULL_TREE && !*non_constant_p
		  && !DECL_DESTRUCTOR_P (fun))
		{
		  if (!ctx->quiet)
		    error ("%<constexpr%> call flows off the end "
			   "of the function");
		  *non_constant_p = true;
		}
	    }

	  /* At this point, the object's constructor will have run, so
	     the object is no longer under construction, and its possible
	     'const' semantics now apply.  Make a note of this fact by
	     marking the CONSTRUCTOR TREE_READONLY.  */
	  if (new_obj && DECL_CONSTRUCTOR_P (fun))
	    cxx_set_object_constness (ctx, new_obj, /*readonly_p=*/true,
				      non_constant_p, overflow_p);

	  /* Remove the parms/result from the values map.  */
	  destroy_value_checked (ctx, res, non_constant_p);
	  for (tree parm = parms; parm; parm = TREE_CHAIN (parm))
	    destroy_value_checked (ctx, parm, non_constant_p);

	  /* Free any parameter CONSTRUCTORs we aren't returning directly.  */
	  while (!ctors->is_empty ())
	    {
	      tree c = ctors->pop ();
	      if (c != result)
		free_constructor (c);
	    }

	  /* Make the unshared function copy we used available for re-use.  */
	  save_fundef_copy (fun, copy);

	  /* If the call allocated some heap object that hasn't been
	     deallocated during the call, or if it deallocated some heap
	     object it has not allocated, the call isn't really stateless
	     for the constexpr evaluation and should not be cached.
	     It is fine if the call allocates something and deallocates it
	     too.  */
	  if (cacheable
	      && (save_heap_alloc_count != ctx->global->heap_vars.length ()
		  || (save_heap_dealloc_count
		      != ctx->global->heap_dealloc_count)))
	    {
	      tree heap_var;
	      unsigned int i;
	      if ((ctx->global->heap_vars.length ()
		   - ctx->global->heap_dealloc_count)
		  != save_heap_alloc_count - save_heap_dealloc_count)
		cacheable = false;
	      else
		FOR_EACH_VEC_ELT_FROM (ctx->global->heap_vars, i, heap_var,
				       save_heap_alloc_count)
		  if (DECL_NAME (heap_var) != heap_deleted_identifier)
		    {
		      cacheable = false;
		      break;
		    }
	    }

	    /* Rewrite all occurrences of the function's RESULT_DECL with the
	       current object under construction.  */
	    if (!*non_constant_p && ctx->object
		&& CLASS_TYPE_P (TREE_TYPE (res))
		&& !is_empty_class (TREE_TYPE (res)))
	      if (replace_decl (&result, res, ctx->object))
		cacheable = false;

	  /* Only cache a permitted result of a constant expression.  */
	  if (cacheable && !reduced_constant_expression_p (result))
	    cacheable = false;
	}
      else
	/* Couldn't get a function copy to evaluate.  */
	*non_constant_p = true;

      if (result == error_mark_node)
	*non_constant_p = true;
      if (*non_constant_p || *overflow_p)
	result = error_mark_node;
      else if (!result)
	result = void_node;
      if (entry)
	entry->result = cacheable ? result : error_mark_node;
    }

  /* The result of a constexpr function must be completely initialized.

     However, in C++20, a constexpr constructor doesn't necessarily have
     to initialize all the fields, so we don't clear CONSTRUCTOR_NO_CLEARING
     in order to detect reading an unitialized object in constexpr instead
     of value-initializing it.  (reduced_constant_expression_p is expected to
     take care of clearing the flag.)  */
  if (TREE_CODE (result) == CONSTRUCTOR
      && (cxx_dialect < cxx20
	  || !DECL_CONSTRUCTOR_P (fun)))
    clear_no_implicit_zero (result);

  pop_cx_call_context ();
  return result;
}

/* Return true if T is a valid constant initializer.  If a CONSTRUCTOR
   initializes all the members, the CONSTRUCTOR_NO_CLEARING flag will be
   cleared.
   FIXME speed this up, it's taking 16% of compile time on sieve testcase.  */

bool
reduced_constant_expression_p (tree t)
{
  if (t == NULL_TREE)
    return false;

  switch (TREE_CODE (t))
    {
    case PTRMEM_CST:
      /* Even if we can't lower this yet, it's constant.  */
      return true;

    case CONSTRUCTOR:
      /* And we need to handle PTRMEM_CST wrapped in a CONSTRUCTOR.  */
      tree field;
      if (!AGGREGATE_TYPE_P (TREE_TYPE (t)))
	/* A constant vector would be folded to VECTOR_CST.
	   A CONSTRUCTOR of scalar type means uninitialized.  */
	return false;
      if (CONSTRUCTOR_NO_CLEARING (t))
	{
	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    {
	      /* There must be a valid constant initializer at every array
		 index.  */
	      tree min = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
	      tree max = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (t)));
	      tree cursor = min;
	      for (auto &e: CONSTRUCTOR_ELTS (t))
		{
		  if (!reduced_constant_expression_p (e.value))
		    return false;
		  if (array_index_cmp (cursor, e.index) != 0)
		    return false;
		  if (TREE_CODE (e.index) == RANGE_EXPR)
		    cursor = TREE_OPERAND (e.index, 1);
		  if (TREE_CODE (e.value) == RAW_DATA_CST)
		    cursor
		      = int_const_binop (PLUS_EXPR, cursor,
					 size_int (RAW_DATA_LENGTH (e.value)));
		  else
		    cursor = int_const_binop (PLUS_EXPR, cursor,
					      size_one_node);
		}
	      if (find_array_ctor_elt (t, max) == -1)
		return false;
	      goto ok;
	    }
	  else if (cxx_dialect >= cxx20
		   && TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	    {
	      if (CONSTRUCTOR_NELTS (t) == 0)
		/* An initialized union has a constructor element.  */
		return false;
	      /* And it only initializes one member.  */
	      field = NULL_TREE;
	    }
	  else
	    field = next_subobject_field (TYPE_FIELDS (TREE_TYPE (t)));
	}
      else
	field = NULL_TREE;
      for (auto &e: CONSTRUCTOR_ELTS (t))
	{
	  /* If VAL is null, we're in the middle of initializing this
	     element.  */
	  if (!reduced_constant_expression_p (e.value))
	    return false;
	  /* We want to remove initializers for empty fields in a struct to
	     avoid confusing output_constructor.  */
	  if (is_empty_field (e.index)
	      && TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE)
	    return false;
	  /* Check for non-empty fields between initialized fields when
	     CONSTRUCTOR_NO_CLEARING.  */
	  for (; field && e.index != field;
	       field = next_subobject_field (DECL_CHAIN (field)))
	    if (!is_really_empty_class (TREE_TYPE (field),
					/*ignore_vptr*/false))
	      return false;
	  if (field)
	    field = next_subobject_field (DECL_CHAIN (field));
	}
      /* There could be a non-empty field at the end.  */
      for (; field; field = next_subobject_field (DECL_CHAIN (field)))
	if (!is_really_empty_class (TREE_TYPE (field), /*ignore_vptr*/false))
	  return false;
ok:
      if (CONSTRUCTOR_NO_CLEARING (t))
	/* All the fields are initialized.  */
	CONSTRUCTOR_NO_CLEARING (t) = false;
      return true;

    default:
      /* FIXME are we calling this too much?  */
      return initializer_constant_valid_p (t, TREE_TYPE (t)) != NULL_TREE;
    }
}

/* *TP was not deemed constant by reduced_constant_expression_p.  Explain
   why and suggest what could be done about it.  */

static tree
verify_constant_explain_r (tree *tp, int *walk_subtrees, void *)
{
  bool ref_p = false;

  /* No need to look into types or unevaluated operands.  */
  if (TYPE_P (*tp) || unevaluated_p (TREE_CODE (*tp)))
    {
      *walk_subtrees = false;
      return NULL_TREE;
    }

  switch (TREE_CODE (*tp))
    {
    CASE_CONVERT:
      if (TREE_CODE (TREE_OPERAND (*tp, 0)) != ADDR_EXPR)
	break;
      ref_p = TYPE_REF_P (TREE_TYPE (*tp));
      *tp = TREE_OPERAND (*tp, 0);
      gcc_fallthrough ();
    case ADDR_EXPR:
      {
	tree op = TREE_OPERAND (*tp, 0);
	if (VAR_P (op)
	    && DECL_DECLARED_CONSTEXPR_P (op)
	    && !TREE_STATIC (op)
	    /* ??? We should also say something about temporaries.  */
	    && !DECL_ARTIFICIAL (op))
	  {
	    if (ref_p)
	      inform (location_of (*tp), "reference to %qD is not a constant "
		      "expression", op);
	    else
	      inform (location_of (*tp), "pointer to %qD is not a constant "
		      "expression", op);
	    const location_t op_loc = DECL_SOURCE_LOCATION (op);
	    rich_location richloc (line_table, op_loc);
	    richloc.add_fixit_insert_before (op_loc, "static ");
	    inform (&richloc,
		    "address of non-static constexpr variable %qD may differ on "
		    "each invocation of the enclosing function; add %<static%> "
		    "to give it a constant address", op);
	  }
	break;
      }
    default:
      break;
    }

  return NULL_TREE;
}

/* Some expressions may have constant operands but are not constant
   themselves, such as 1/0.  Call this function to check for that
   condition.

   We only call this in places that require an arithmetic constant, not in
   places where we might have a non-constant expression that can be a
   component of a constant expression, such as the address of a constexpr
   variable that might be dereferenced later.  */

static bool
verify_constant (tree t, bool allow_non_constant, bool *non_constant_p,
		 bool *overflow_p)
{
  if (!*non_constant_p && !reduced_constant_expression_p (t)
      && t != void_node)
    {
      if (!allow_non_constant)
	{
	  auto_diagnostic_group d;
	  error_at (cp_expr_loc_or_input_loc (t),
		    "%q+E is not a constant expression", t);
	  cp_walk_tree_without_duplicates (&t, verify_constant_explain_r,
					   nullptr);
	}
      *non_constant_p = true;
    }
  if (TREE_OVERFLOW_P (t))
    {
      if (!allow_non_constant)
	{
	  permerror (input_location, "overflow in constant expression");
	  /* If we're being permissive (and are in an enforcing
	     context), ignore the overflow.  */
	  if (flag_permissive)
	    return *non_constant_p;
	}
      *overflow_p = true;
    }
  return *non_constant_p;
}

/* Check whether the shift operation with code CODE and type TYPE on LHS
   and RHS is undefined.  If it is, give an error with an explanation,
   and return true; return false otherwise.  */

static bool
cxx_eval_check_shift_p (location_t loc, const constexpr_ctx *ctx,
			enum tree_code code, tree type, tree lhs, tree rhs)
{
  if ((code != LSHIFT_EXPR && code != RSHIFT_EXPR)
      || TREE_CODE (lhs) != INTEGER_CST
      || TREE_CODE (rhs) != INTEGER_CST)
    return false;

  tree lhstype = TREE_TYPE (lhs);
  unsigned HOST_WIDE_INT uprec = TYPE_PRECISION (TREE_TYPE (lhs));

  /* [expr.shift] The behavior is undefined if the right operand
     is negative, or greater than or equal to the length in bits
     of the promoted left operand.  */
  if (tree_int_cst_sgn (rhs) == -1)
    {
      if (!ctx->quiet)
	permerror (loc, "right operand of shift expression %q+E is negative",
		   build2_loc (loc, code, type, lhs, rhs));
      return (!flag_permissive || ctx->quiet);
    }
  if (compare_tree_int (rhs, uprec) >= 0)
    {
      if (!ctx->quiet)
	permerror (loc, "right operand of shift expression %q+E is greater "
		   "than or equal to the precision %wu of the left operand",
		   build2_loc (loc, code, type, lhs, rhs), uprec);
      return (!flag_permissive || ctx->quiet);
    }

  /* The value of E1 << E2 is E1 left-shifted E2 bit positions; [...]
     if E1 has a signed type and non-negative value, and E1x2^E2 is
     representable in the corresponding unsigned type of the result type,
     then that value, converted to the result type, is the resulting value;
     otherwise, the behavior is undefined.
     For C++20:
     The value of E1 << E2 is the unique value congruent to E1 x 2^E2 modulo
     2^N, where N is the range exponent of the type of the result.  */
  if (code == LSHIFT_EXPR
      && !TYPE_OVERFLOW_WRAPS (lhstype)
      && cxx_dialect >= cxx11
      && cxx_dialect < cxx20)
    {
      if (tree_int_cst_sgn (lhs) == -1)
	{
	  if (!ctx->quiet)
	    permerror (loc,
		       "left operand of shift expression %q+E is negative",
		       build2_loc (loc, code, type, lhs, rhs));
	  return (!flag_permissive || ctx->quiet);
	}
      /* For signed x << y the following:
	 (unsigned) x >> ((prec (lhs) - 1) - y)
	 if > 1, is undefined.  The right-hand side of this formula
	 is the highest bit of the LHS that can be set (starting from 0),
	 so that the shift doesn't overflow.  We then right-shift the LHS
	 to see whether any other bit is set making the original shift
	 undefined -- the result is not representable in the corresponding
	 unsigned type.  */
      tree t = build_int_cst (unsigned_type_node, uprec - 1);
      t = fold_build2 (MINUS_EXPR, unsigned_type_node, t, rhs);
      tree ulhs = fold_convert (unsigned_type_for (lhstype), lhs);
      t = fold_build2 (RSHIFT_EXPR, TREE_TYPE (ulhs), ulhs, t);
      if (tree_int_cst_lt (integer_one_node, t))
	{
	  if (!ctx->quiet)
	    permerror (loc, "shift expression %q+E overflows",
		       build2_loc (loc, code, type, lhs, rhs));
	  return (!flag_permissive || ctx->quiet);
	}
    }
  return false;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce the unary expression tree T to a compile time value.
   If successful, return the value.  Otherwise issue a diagnostic
   and return error_mark_node.  */

static tree
cxx_eval_unary_expression (const constexpr_ctx *ctx, tree t,
			   bool /*lval*/,
			   bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree orig_arg = TREE_OPERAND (t, 0);
  tree arg = cxx_eval_constant_expression (ctx, orig_arg, vc_prvalue,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg);
  location_t loc = EXPR_LOCATION (t);
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);
  r = fold_unary_loc (loc, code, type, arg);
  if (r == NULL_TREE)
    {
      if (arg == orig_arg)
	r = t;
      else
	r = build1_loc (loc, code, type, arg);
    }
  VERIFY_CONSTANT (r);
  return r;
}

/* Helper function for cxx_eval_binary_expression.  Try to optimize
   original POINTER_PLUS_EXPR T, LHS p+ RHS, return NULL_TREE if the
   generic folding should be used.  */

static tree
cxx_fold_pointer_plus_expression (const constexpr_ctx *ctx, tree t,
				  tree lhs, tree rhs, bool *non_constant_p,
				  bool *overflow_p)
{
  STRIP_NOPS (lhs);
  if (TREE_CODE (lhs) != ADDR_EXPR)
    return NULL_TREE;

  lhs = TREE_OPERAND (lhs, 0);

  /* &A[i] p+ j => &A[i + j] */
  if (TREE_CODE (lhs) == ARRAY_REF
      && TREE_CODE (TREE_OPERAND (lhs, 1)) == INTEGER_CST
      && TREE_CODE (rhs) == INTEGER_CST
      && TYPE_SIZE_UNIT (TREE_TYPE (lhs))
      && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (lhs))) == INTEGER_CST)
    {
      tree orig_type = TREE_TYPE (t);
      location_t loc = EXPR_LOCATION (t);
      tree type = TREE_TYPE (lhs);

      t = fold_convert_loc (loc, ssizetype, TREE_OPERAND (lhs, 1));
      tree nelts = array_type_nelts_top (TREE_TYPE (TREE_OPERAND (lhs, 0)));
      nelts = cxx_eval_constant_expression (ctx, nelts, vc_prvalue,
					    non_constant_p, overflow_p);
      if (*non_constant_p)
	return NULL_TREE;
      /* Don't fold an out-of-bound access.  */
      if (!tree_int_cst_le (t, nelts))
	return NULL_TREE;
      rhs = cp_fold_convert (ssizetype, rhs);
      /* Don't fold if rhs can't be divided exactly by TYPE_SIZE_UNIT.
	 constexpr int A[1]; ... (char *)&A[0] + 1 */
      if (!integer_zerop (fold_build2_loc (loc, TRUNC_MOD_EXPR, sizetype,
					   rhs, TYPE_SIZE_UNIT (type))))
	return NULL_TREE;
      /* Make sure to treat the second operand of POINTER_PLUS_EXPR
	 as signed.  */
      rhs = fold_build2_loc (loc, EXACT_DIV_EXPR, ssizetype, rhs,
			     TYPE_SIZE_UNIT (type));
      t = size_binop_loc (loc, PLUS_EXPR, rhs, t);
      t = build4_loc (loc, ARRAY_REF, type, TREE_OPERAND (lhs, 0),
		      t, NULL_TREE, NULL_TREE);
      t = cp_build_addr_expr (t, tf_warning_or_error);
      t = cp_fold_convert (orig_type, t);
      return cxx_eval_constant_expression (ctx, t, vc_prvalue,
					   non_constant_p, overflow_p);
    }

  return NULL_TREE;
}

/* Try to fold expressions like
   (struct S *) (&a[0].D.2378 + 12)
   into
   &MEM <struct T> [(void *)&a + 12B]
   This is something normally done by gimple_fold_stmt_to_constant_1
   on GIMPLE, but is undesirable on GENERIC if we are e.g. going to
   dereference the address because some details are lost.
   For pointer comparisons we want such folding though so that
   match.pd address_compare optimization works.  */

static tree
cxx_maybe_fold_addr_pointer_plus (tree t)
{
  while (CONVERT_EXPR_P (t)
	 && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (t, 0))))
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) != POINTER_PLUS_EXPR)
    return NULL_TREE;
  tree op0 = TREE_OPERAND (t, 0);
  tree op1 = TREE_OPERAND (t, 1);
  if (TREE_CODE (op1) != INTEGER_CST)
    return NULL_TREE;
  while (CONVERT_EXPR_P (op0)
	 && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (op0, 0))))
    op0 = TREE_OPERAND (op0, 0);
  if (TREE_CODE (op0) != ADDR_EXPR)
    return NULL_TREE;
  op1 = fold_convert (ptr_type_node, op1);
  tree r = fold_build2 (MEM_REF, TREE_TYPE (TREE_TYPE (op0)), op0, op1);
  return build1_loc (EXPR_LOCATION (t), ADDR_EXPR, TREE_TYPE (op0), r);
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for binary expressions.  */

static tree
cxx_eval_binary_expression (const constexpr_ctx *ctx, tree t,
			    value_cat lval,
			    bool *non_constant_p, bool *overflow_p)
{
  tree r = NULL_TREE;
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;
  lhs = cxx_eval_constant_expression (ctx, orig_lhs, vc_prvalue,
				      non_constant_p, overflow_p);
  /* Don't VERIFY_CONSTANT here, it's unnecessary and will break pointer
     subtraction.  */
  if (*non_constant_p)
    return t;
  rhs = cxx_eval_constant_expression (ctx, orig_rhs, vc_prvalue,
				      non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  location_t loc = EXPR_LOCATION (t);
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);

  if (code == EQ_EXPR || code == NE_EXPR)
    {
      bool is_code_eq = (code == EQ_EXPR);

      if (TREE_CODE (lhs) == PTRMEM_CST
	  && TREE_CODE (rhs) == PTRMEM_CST)
	{
	  tree lmem = PTRMEM_CST_MEMBER (lhs);
	  tree rmem = PTRMEM_CST_MEMBER (rhs);
	  bool eq;
	  if (TREE_CODE (lmem) == TREE_CODE (rmem)
	      && TREE_CODE (lmem) == FIELD_DECL
	      && TREE_CODE (DECL_CONTEXT (lmem)) == UNION_TYPE
	      && same_type_p (DECL_CONTEXT (lmem),
			      DECL_CONTEXT (rmem)))
	    /* If both refer to (possibly different) members of the same union
	       (12.3), they compare equal. */
	    eq = true;
	  else
	    eq = cp_tree_equal (lhs, rhs);
	  r = constant_boolean_node (eq == is_code_eq, type);
	}
      else if ((TREE_CODE (lhs) == PTRMEM_CST
		|| TREE_CODE (rhs) == PTRMEM_CST)
	       && (null_member_pointer_value_p (lhs)
		   || null_member_pointer_value_p (rhs)))
	r = constant_boolean_node (!is_code_eq, type);
      else if (TREE_CODE (lhs) == PTRMEM_CST)
	lhs = cplus_expand_constant (lhs);
      else if (TREE_CODE (rhs) == PTRMEM_CST)
	rhs = cplus_expand_constant (rhs);
    }
  if (r == NULL_TREE
      && TREE_CODE_CLASS (code) == tcc_comparison
      && POINTER_TYPE_P (TREE_TYPE (lhs)))
    {
      if (tree lhso = cxx_maybe_fold_addr_pointer_plus (lhs))
	lhs = fold_convert (TREE_TYPE (lhs), lhso);
      if (tree rhso = cxx_maybe_fold_addr_pointer_plus (rhs))
	rhs = fold_convert (TREE_TYPE (rhs), rhso);
    }
  if (code == POINTER_PLUS_EXPR && !*non_constant_p
      && integer_zerop (lhs) && !integer_zerop (rhs))
    {
      if (!ctx->quiet)
	error ("arithmetic involving a null pointer in %qE", lhs);
      *non_constant_p = true;
      return t;
    }
  else if (code == POINTER_PLUS_EXPR)
    r = cxx_fold_pointer_plus_expression (ctx, t, lhs, rhs, non_constant_p,
					  overflow_p);
  else if (code == SPACESHIP_EXPR)
    {
      r = genericize_spaceship (loc, type, lhs, rhs);
      return cxx_eval_constant_expression (ctx, r, lval, non_constant_p,
					   overflow_p);
    }

  if (r == NULL_TREE)
    {
      if (ctx->manifestly_const_eval == mce_true
	  && (flag_constexpr_fp_except
	      || TREE_CODE (type) != REAL_TYPE))
	{
	  auto ofcc = make_temp_override (folding_cxx_constexpr, true);
	  r = fold_binary_initializer_loc (loc, code, type, lhs, rhs);
	}
      else
	r = fold_binary_loc (loc, code, type, lhs, rhs);
    }

  if (r == NULL_TREE
      && (code == LSHIFT_EXPR || code == RSHIFT_EXPR)
      && TREE_CODE (lhs) == INTEGER_CST
      && TREE_CODE (rhs) == INTEGER_CST
      && wi::neg_p (wi::to_wide (rhs)))
    {
      /* For diagnostics and -fpermissive emulate previous behavior of
	 handling shifts by negative amount.  */
      tree nrhs = const_unop (NEGATE_EXPR, TREE_TYPE (rhs), rhs);
      if (nrhs)
	r = fold_binary_loc (loc,
			     code == LSHIFT_EXPR ? RSHIFT_EXPR : LSHIFT_EXPR,
			     type, lhs, nrhs);
    }

  if (r == NULL_TREE)
    {
      if (lhs == orig_lhs && rhs == orig_rhs)
	r = t;
      else
	r = build2_loc (loc, code, type, lhs, rhs);
    }
  else if (cxx_eval_check_shift_p (loc, ctx, code, type, lhs, rhs))
    *non_constant_p = true;
  /* Don't VERIFY_CONSTANT if this might be dealing with a pointer to
     a local array in a constexpr function.  */
  bool ptr = INDIRECT_TYPE_P (TREE_TYPE (lhs));
  if (!ptr)
    VERIFY_CONSTANT (r);
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate condition expressions.  */

static tree
cxx_eval_conditional_expression (const constexpr_ctx *ctx, tree t,
				 value_cat lval,
				 bool *non_constant_p, bool *overflow_p,
				 tree *jump_target)
{
  tree val = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					   vc_prvalue,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (val);
  if (TREE_CODE (t) == IF_STMT && IF_STMT_CONSTEVAL_P (t))
    {
      /* Evaluate the condition as if it was
	 if (__builtin_is_constant_evaluated ()), i.e. defer it if not
	 ctx->manifestly_const_eval (as sometimes we try to constant evaluate
	 without manifestly_const_eval even expressions or parts thereof which
	 will later be manifestly const_eval evaluated), otherwise fold it to
	 true.  */
      if (ctx->manifestly_const_eval == mce_unknown)
	{
	  *non_constant_p = true;
	  return t;
	}
      val = constant_boolean_node (ctx->manifestly_const_eval == mce_true,
				   boolean_type_node);
    }
  /* Don't VERIFY_CONSTANT the other operands.  */
  const bool zero_p = integer_zerop (val);
  if (zero_p)
    val = TREE_OPERAND (t, 2);
  else
    val = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == IF_STMT && !val)
    val = void_node;

  /* P2564: If we aren't in immediate function context (including a manifestly
     constant-evaluated expression), check any uses of immediate functions in
     the arm we're discarding.  But don't do this inside a call; we already
     checked when parsing the function.  */
  if (ctx->manifestly_const_eval != mce_true
      && !in_immediate_context ()
      && !ctx->call
      && cp_fold_immediate (&TREE_OPERAND (t, zero_p ? 1 : 2),
			    ctx->manifestly_const_eval))
    {
      *non_constant_p = true;
      return t;
    }

  /* A TARGET_EXPR may be nested inside another TARGET_EXPR, but still
     serve as the initializer for the same object as the outer TARGET_EXPR,
     as in
       A a = true ? A{} : A{};
     so strip the inner TARGET_EXPR so we don't materialize a temporary.  */
  if (TREE_CODE (val) == TARGET_EXPR)
    val = TARGET_EXPR_INITIAL (val);
  return cxx_eval_constant_expression (ctx, val, lval, non_constant_p,
				       overflow_p, jump_target);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate vector condition expressions.  Unlike
   cxx_eval_conditional_expression, VEC_COND_EXPR acts like a normal
   ternary arithmetics operation, where all 3 arguments have to be
   evaluated as constants and then folding computes the result from
   them.  */

static tree
cxx_eval_vector_conditional_expression (const constexpr_ctx *ctx, tree t,
					bool *non_constant_p, bool *overflow_p)
{
  tree arg1 = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					    vc_prvalue,
					    non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg1);
  tree arg2 = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1),
					    vc_prvalue,
					    non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg2);
  tree arg3 = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 2),
					    vc_prvalue,
					    non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg3);
  location_t loc = EXPR_LOCATION (t);
  tree type = TREE_TYPE (t);
  tree r = fold_ternary_loc (loc, VEC_COND_EXPR, type, arg1, arg2, arg3);
  if (r == NULL_TREE)
    {
      if (arg1 == TREE_OPERAND (t, 0)
	  && arg2 == TREE_OPERAND (t, 1)
	  && arg3 == TREE_OPERAND (t, 2))
	r = t;
      else
	r = build3_loc (loc, VEC_COND_EXPR, type, arg1, arg2, arg3);
    }
  VERIFY_CONSTANT (r);
  return r;
}

/* Returns less than, equal to, or greater than zero if KEY is found to be
   less than, to match, or to be greater than the constructor_elt's INDEX.  */

static int
array_index_cmp (tree key, tree index)
{
  gcc_assert (TREE_CODE (key) == INTEGER_CST);

  switch (TREE_CODE (index))
    {
    case INTEGER_CST:
      return tree_int_cst_compare (key, index);
    case RANGE_EXPR:
      {
	tree lo = TREE_OPERAND (index, 0);
	tree hi = TREE_OPERAND (index, 1);
	if (tree_int_cst_lt (key, lo))
	  return -1;
	else if (tree_int_cst_lt (hi, key))
	  return 1;
	else
	  return 0;
      }
    default:
      gcc_unreachable ();
    }
}

/* Extract a single INTEGER_CST from RAW_DATA_CST RAW_DATA at
   relative index OFF.  */

static tree
raw_data_cst_elt (tree raw_data, unsigned int off)
{
  return build_int_cst (TREE_TYPE (raw_data),
			TYPE_UNSIGNED (TREE_TYPE (raw_data))
			? (HOST_WIDE_INT)
			  RAW_DATA_UCHAR_ELT (raw_data, off)
			: (HOST_WIDE_INT)
			  RAW_DATA_SCHAR_ELT (raw_data, off));
}

/* Returns the index of the constructor_elt of ARY which matches DINDEX, or -1
   if none.  If INSERT is true, insert a matching element rather than fail.  */

static HOST_WIDE_INT
find_array_ctor_elt (tree ary, tree dindex, bool insert)
{
  if (tree_int_cst_sgn (dindex) < 0)
    return -1;

  unsigned HOST_WIDE_INT i = tree_to_uhwi (dindex);
  vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ary);
  unsigned HOST_WIDE_INT len = vec_safe_length (elts);

  unsigned HOST_WIDE_INT end = len;
  unsigned HOST_WIDE_INT begin = 0;

  /* If the last element of the CONSTRUCTOR has its own index, we can assume
     that the same is true of the other elements and index directly.  */
  if (end > 0)
    {
      tree cindex = (*elts)[end - 1].index;
      if (cindex == NULL_TREE)
	{
	  /* Verify that if the last index is missing, all indexes
	     are missing and there is no RAW_DATA_CST.  */
	  if (flag_checking)
	    for (unsigned int j = 0; j < len - 1; ++j)
	      gcc_assert ((*elts)[j].index == NULL_TREE
			  && TREE_CODE ((*elts)[j].value) != RAW_DATA_CST);
	  if (i < end)
	    return i;
	  else
	    {
	      begin = end;
	      if (i == end)
		/* If the element is to be added right at the end,
		   make sure it is added with cleared index too.  */
		dindex = NULL_TREE;
	      else if (insert)
		/* Otherwise, in order not to break the assumption
		   that CONSTRUCTOR either has all indexes or none,
		   we need to add indexes to all elements.  */
		for (unsigned int j = 0; j < len; ++j)
		  (*elts)[j].index = build_int_cst (TREE_TYPE (dindex), j);
	    }
	}
      else if (TREE_CODE (cindex) == INTEGER_CST
	       && compare_tree_int (cindex, end - 1) == 0)
	{
	  if (i < end)
	    return i;
	  tree value = (*elts)[end - 1].value;
	  if (TREE_CODE (value) == RAW_DATA_CST
	      && wi::to_offset (dindex) < (wi::to_offset (cindex)
					   + RAW_DATA_LENGTH (value)))
	    begin = end - 1;
	  else
	    begin = end;
	}
    }

  /* Otherwise, find a matching index by means of a binary search.  */
  while (begin != end)
    {
      unsigned HOST_WIDE_INT middle = (begin + end) / 2;
      constructor_elt &elt = (*elts)[middle];
      tree idx = elt.index;

      int cmp = array_index_cmp (dindex, idx);
      if (cmp > 0
	  && TREE_CODE (elt.value) == RAW_DATA_CST
	  && wi::to_offset (dindex) < (wi::to_offset (idx)
				       + RAW_DATA_LENGTH (elt.value)))
	cmp = 0;
      if (cmp < 0)
	end = middle;
      else if (cmp > 0)
	begin = middle + 1;
      else
	{
	  if (insert && TREE_CODE (elt.value) == RAW_DATA_CST)
	    {
	      /* We need to split the RAW_DATA_CST elt.  */
	      constructor_elt e;
	      gcc_checking_assert (TREE_CODE (idx) != RANGE_EXPR);
	      unsigned int off = (wi::to_offset (dindex)
				  - wi::to_offset (idx)).to_uhwi ();
	      tree value = elt.value;
	      unsigned int len = RAW_DATA_LENGTH (value);
	      if (off > 1 && len >= off + 3)
		value = copy_node (elt.value);
	      if (off)
		{
		  if (off > 1)
		    RAW_DATA_LENGTH (elt.value) = off;
		  else
		    elt.value = raw_data_cst_elt (elt.value, 0);
		  e.index = size_binop (PLUS_EXPR, elt.index,
					build_int_cst (TREE_TYPE (elt.index),
						       off));
		  e.value = NULL_TREE;
		  ++middle;
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle, e);
		}
	      (*elts)[middle].value = raw_data_cst_elt (value, off);
	      if (len >= off + 2)
		{
		  e.index = (*elts)[middle].index;
		  e.index = size_binop (PLUS_EXPR, e.index,
					build_one_cst (TREE_TYPE (e.index)));
		  if (len >= off + 3)
		    {
		      RAW_DATA_LENGTH (value) -= off + 1;
		      RAW_DATA_POINTER (value) += off + 1;
		      e.value = value;
		    }
		  else
		    e.value = raw_data_cst_elt (value, off + 1);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle + 1, e);
		}
	      return middle;
	    }
	  if (insert && TREE_CODE (idx) == RANGE_EXPR)
	    {
	      /* We need to split the range.  */
	      constructor_elt e;
	      tree lo = TREE_OPERAND (idx, 0);
	      tree hi = TREE_OPERAND (idx, 1);
	      tree value = elt.value;
	      dindex = fold_convert (sizetype, dindex);
	      if (tree_int_cst_lt (lo, dindex))
		{
		  /* There are still some lower elts; shorten the range.  */
		  tree new_hi = int_const_binop (MINUS_EXPR, dindex,
						 size_one_node);
		  if (tree_int_cst_equal (lo, new_hi))
		    /* Only one element left, no longer a range.  */
		    elt.index = lo;
		  else
		    TREE_OPERAND (idx, 1) = new_hi;
		  /* Append the element we want to insert.  */
		  ++middle;
		  e.index = dindex;
		  e.value = unshare_constructor (value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle, e);
		}
	      else
		/* No lower elts, the range elt is now ours.  */
		elt.index = dindex;

	      if (tree_int_cst_lt (dindex, hi))
		{
		  /* There are still some higher elts; append a range.  */
		  tree new_lo = int_const_binop (PLUS_EXPR, dindex,
						 size_one_node);
		  if (tree_int_cst_equal (new_lo, hi))
		    e.index = hi;
		  else
		    e.index = build2 (RANGE_EXPR, sizetype, new_lo, hi);
		  e.value = unshare_constructor (value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle + 1, e);
		}
	    }
	  return middle;
	}
    }

  if (insert)
    {
      constructor_elt e = { dindex, NULL_TREE };
      vec_safe_insert (CONSTRUCTOR_ELTS (ary), end, e);
      return end;
    }

  return -1;
}

/* Return a pointer to the constructor_elt of CTOR which matches INDEX.  If no
   matching constructor_elt exists, then add one to CTOR.

   As an optimization, if POS_HINT is non-negative then it is used as a guess
   for the (integer) index of the matching constructor_elt within CTOR.  */

static constructor_elt *
get_or_insert_ctor_field (tree ctor, tree index, int pos_hint = -1)
{
  /* Check the hint first.  */
  if (pos_hint >= 0 && (unsigned)pos_hint < CONSTRUCTOR_NELTS (ctor)
      && CONSTRUCTOR_ELT (ctor, pos_hint)->index == index)
    return CONSTRUCTOR_ELT (ctor, pos_hint);

  tree type = TREE_TYPE (ctor);
  if (TREE_CODE (type) == VECTOR_TYPE && index == NULL_TREE)
    {
      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (ctor), index, NULL_TREE);
      return &CONSTRUCTOR_ELTS (ctor)->last();
    }
  else if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == VECTOR_TYPE)
    {
      if (TREE_CODE (index) == RANGE_EXPR)
	{
	  /* Support for RANGE_EXPR index lookups is currently limited to
	     accessing an existing element via POS_HINT, or appending a new
	     element to the end of CTOR.  ??? Support for other access
	     patterns may also be needed.  */
	  vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (ctor);
	  if (vec_safe_length (elts))
	    {
	      tree lo = TREE_OPERAND (index, 0);
	      gcc_assert (array_index_cmp (elts->last().index, lo) < 0);
	    }
	  CONSTRUCTOR_APPEND_ELT (elts, index, NULL_TREE);
	  return &elts->last();
	}

      HOST_WIDE_INT i = find_array_ctor_elt (ctor, index, /*insert*/true);
      gcc_assert (i >= 0);
      constructor_elt *cep = CONSTRUCTOR_ELT (ctor, i);
      gcc_assert (cep->index == NULL_TREE
		  || TREE_CODE (cep->index) != RANGE_EXPR);
      return cep;
    }
  else
    {
      gcc_assert (TREE_CODE (index) == FIELD_DECL
		  && (same_type_ignoring_top_level_qualifiers_p
		      (DECL_CONTEXT (index), TREE_TYPE (ctor))));

      /* We must keep the CONSTRUCTOR's ELTS in FIELD order.
	 Usually we meet initializers in that order, but it is
	 possible for base types to be placed not in program
	 order.  */
      tree fields = TYPE_FIELDS (DECL_CONTEXT (index));
      unsigned HOST_WIDE_INT idx = 0;
      constructor_elt *cep = NULL;

      /* Check if we're changing the active member of a union.  */
      if (TREE_CODE (type) == UNION_TYPE && CONSTRUCTOR_NELTS (ctor)
	  && CONSTRUCTOR_ELT (ctor, 0)->index != index)
	vec_safe_truncate (CONSTRUCTOR_ELTS (ctor), 0);
      /* If the bit offset of INDEX is larger than that of the last
	 constructor_elt, then we can just immediately append a new
	 constructor_elt to the end of CTOR.  */
      else if (CONSTRUCTOR_NELTS (ctor)
	       && tree_int_cst_compare (bit_position (index),
					bit_position (CONSTRUCTOR_ELTS (ctor)
						      ->last().index)) > 0)
	{
	  idx = CONSTRUCTOR_NELTS (ctor);
	  goto insert;
	}

      /* Otherwise, we need to iterate over CTOR to find or insert INDEX
	 appropriately.  */

      for (; vec_safe_iterate (CONSTRUCTOR_ELTS (ctor), idx, &cep);
	   idx++, fields = DECL_CHAIN (fields))
	{
	  if (index == cep->index)
	    goto found;

	  /* The field we're initializing must be on the field
	     list.  Look to see if it is present before the
	     field the current ELT initializes.  */
	  for (; fields != cep->index; fields = DECL_CHAIN (fields))
	    if (index == fields)
	      goto insert;
	}
      /* We fell off the end of the CONSTRUCTOR, so insert a new
	 entry at the end.  */

    insert:
      {
	constructor_elt ce = { index, NULL_TREE };

	vec_safe_insert (CONSTRUCTOR_ELTS (ctor), idx, ce);
	cep = CONSTRUCTOR_ELT (ctor, idx);
      }
    found:;

      return cep;
    }
}

/* Under the control of CTX, issue a detailed diagnostic for
   an out-of-bounds subscript INDEX into the expression ARRAY.  */

static void
diag_array_subscript (location_t loc, const constexpr_ctx *ctx, tree array, tree index)
{
  if (!ctx->quiet)
    {
      tree arraytype = TREE_TYPE (array);

      /* Convert the unsigned array subscript to a signed integer to avoid
	 printing huge numbers for small negative values.  */
      tree sidx = fold_convert (ssizetype, index);
      STRIP_ANY_LOCATION_WRAPPER (array);
      if (DECL_P (array))
	{
	  auto_diagnostic_group d;
	  if (TYPE_DOMAIN (arraytype))
	    error_at (loc, "array subscript value %qE is outside the bounds "
		      "of array %qD of type %qT", sidx, array, arraytype);
	  else
	    error_at (loc, "nonzero array subscript %qE is used with array %qD of "
		      "type %qT with unknown bounds", sidx, array, arraytype);
	  inform (DECL_SOURCE_LOCATION (array), "declared here");
	}
      else if (TYPE_DOMAIN (arraytype))
	error_at (loc, "array subscript value %qE is outside the bounds "
		  "of array type %qT", sidx, arraytype);
      else
	error_at (loc, "nonzero array subscript %qE is used with array of type %qT "
		  "with unknown bounds", sidx, arraytype);
    }
}

/* Return the number of elements for TYPE (which is an ARRAY_TYPE or
   a VECTOR_TYPE).  */

static tree
get_array_or_vector_nelts (const constexpr_ctx *ctx, tree type,
			   bool *non_constant_p, bool *overflow_p)
{
  tree nelts;
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (type))
	nelts = array_type_nelts_top (type);
      else
	nelts = size_zero_node;
    }
  else if (VECTOR_TYPE_P (type))
    nelts = size_int (TYPE_VECTOR_SUBPARTS (type));
  else
    gcc_unreachable ();

  /* For VLAs, the number of elements won't be an integer constant.  */
  nelts = cxx_eval_constant_expression (ctx, nelts, vc_prvalue,
					non_constant_p, overflow_p);
  return nelts;
}

/* Extract element INDEX consisting of CHARS_PER_ELT chars from
   STRING_CST STRING.  */

static tree
extract_string_elt (tree string, unsigned chars_per_elt, unsigned index)
{
  tree type = cv_unqualified (TREE_TYPE (TREE_TYPE (string)));
  tree r;

  if (chars_per_elt == 1)
    r = build_int_cst (type, TREE_STRING_POINTER (string)[index]);
  else
    {
      const unsigned char *ptr
	= ((const unsigned char *)TREE_STRING_POINTER (string)
	   + index * chars_per_elt);
      r = native_interpret_expr (type, ptr, chars_per_elt);
    }
  return r;
}

/* Subroutine of cxx_eval_array_reference.  T is an ARRAY_REF; evaluate the
   subscript, diagnose any problems with it, and return the result.  */

static tree
eval_and_check_array_index (const constexpr_ctx *ctx,
			    tree t, bool allow_one_past,
			    bool *non_constant_p, bool *overflow_p)
{
  location_t loc = cp_expr_loc_or_input_loc (t);
  tree ary = TREE_OPERAND (t, 0);
  t = TREE_OPERAND (t, 1);
  tree index = cxx_eval_constant_expression (ctx, t, vc_prvalue,
					     non_constant_p, overflow_p);
  VERIFY_CONSTANT (index);

  if (!tree_fits_shwi_p (index)
      || tree_int_cst_sgn (index) < 0)
    {
      diag_array_subscript (loc, ctx, ary, index);
      *non_constant_p = true;
      return t;
    }

  tree nelts = get_array_or_vector_nelts (ctx, TREE_TYPE (ary), non_constant_p,
					  overflow_p);
  VERIFY_CONSTANT (nelts);
  if (allow_one_past
      ? !tree_int_cst_le (index, nelts)
      : !tree_int_cst_lt (index, nelts))
    {
      diag_array_subscript (loc, ctx, ary, index);
      *non_constant_p = true;
      return t;
    }

  return index;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a reference to an array slot.  */

static tree
cxx_eval_array_reference (const constexpr_ctx *ctx, tree t,
			  value_cat lval,
			  bool *non_constant_p, bool *overflow_p)
{
  tree oldary = TREE_OPERAND (t, 0);
  tree ary = cxx_eval_constant_expression (ctx, oldary,
					   lval,
					   non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  if (!lval
      && TREE_CODE (ary) == VIEW_CONVERT_EXPR
      && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (ary, 0)))
      && (TYPE_MAIN_VARIANT (TREE_TYPE (t))
	  == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (TREE_OPERAND (ary, 0))))))
    ary = TREE_OPERAND (ary, 0);

  tree oldidx = TREE_OPERAND (t, 1);
  tree index = eval_and_check_array_index (ctx, t, lval,
					   non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  if (lval && ary == oldary && index == oldidx)
    return t;
  else if (lval == vc_discard)
    return t;
  else if (lval)
    return build4 (ARRAY_REF, TREE_TYPE (t), ary, index, NULL, NULL);

  unsigned len = 0, elem_nchars = 1;
  tree elem_type = TREE_TYPE (TREE_TYPE (ary));
  if (TREE_CODE (ary) == CONSTRUCTOR)
    len = CONSTRUCTOR_NELTS (ary);
  else if (TREE_CODE (ary) == STRING_CST)
    {
      elem_nchars = (TYPE_PRECISION (elem_type)
		     / TYPE_PRECISION (char_type_node));
      len = (unsigned) TREE_STRING_LENGTH (ary) / elem_nchars;
    }
  else if (TREE_CODE (ary) == VECTOR_CST)
    /* We don't create variable-length VECTOR_CSTs.  */
    len = VECTOR_CST_NELTS (ary).to_constant ();
  else
    {
      /* We can't do anything with other tree codes, so use
	 VERIFY_CONSTANT to complain and fail.  */
      VERIFY_CONSTANT (ary);
      gcc_unreachable ();
    }

  bool found;
  HOST_WIDE_INT i = 0;
  if (TREE_CODE (ary) == CONSTRUCTOR)
    {
      HOST_WIDE_INT ix = find_array_ctor_elt (ary, index);
      found = (ix >= 0);
      if (found)
	i = ix;
    }
  else
    {
      i = tree_to_shwi (index);
      found = (i < len);
    }

  if (found)
    {
      tree r;
      if (TREE_CODE (ary) == CONSTRUCTOR)
	{
	  r = (*CONSTRUCTOR_ELTS (ary))[i].value;
	  if (TREE_CODE (r) == RAW_DATA_CST)
	    {
	      tree ridx = (*CONSTRUCTOR_ELTS (ary))[i].index;
	      gcc_checking_assert (ridx);
	      unsigned int off
		= (wi::to_offset (index) - wi::to_offset (ridx)).to_uhwi ();
	      r = raw_data_cst_elt (r, off);
	    }
	}
      else if (TREE_CODE (ary) == VECTOR_CST)
	r = VECTOR_CST_ELT (ary, i);
      else
	r = extract_string_elt (ary, elem_nchars, i);

      if (r)
	/* Don't VERIFY_CONSTANT here.  */
	return r;

      /* Otherwise the element doesn't have a value yet.  */
    }

  /* Not found.  */

  if (is_really_empty_class (elem_type, /*ignore_vptr*/false))
    return build_constructor (elem_type, NULL);

  if (TREE_CODE (ary) == CONSTRUCTOR
      && CONSTRUCTOR_NO_CLEARING (ary))
    {
      /* 'ary' is part of the aggregate initializer we're currently
	 building; if there's no initializer for this element yet,
	 that's an error.  */
      if (!ctx->quiet)
	error ("accessing uninitialized array element");
      *non_constant_p = true;
      return t;
    }

  /* If it's within the array bounds but doesn't have an explicit
     initializer, it's initialized from {}.  But use build_value_init
     directly for non-aggregates to avoid creating a garbage CONSTRUCTOR.  */
  tree val;
  constexpr_ctx new_ctx;
  if (CP_AGGREGATE_TYPE_P (elem_type))
    {
      tree empty_ctor = build_constructor (init_list_type_node, NULL);
      val = digest_init (elem_type, empty_ctor, tf_warning_or_error);
    }
  else
    val = build_value_init (elem_type, tf_warning_or_error);

  /* Create a new constructor only if we don't already have a suitable one.  */
  const bool new_ctor = (!SCALAR_TYPE_P (elem_type)
			 && (!ctx->ctor
			     || !same_type_ignoring_top_level_qualifiers_p
				  (elem_type, TREE_TYPE (ctx->ctor))));
  if (new_ctor)
    {
      new_ctx = *ctx;
      /* We clear the object here.  We used to replace it with T, but that
	 caused problems (101371, 108158); and anyway, T is the initializer,
	 not the target object.  */
      new_ctx.object = NULL_TREE;
      new_ctx.ctor = build_constructor (elem_type, NULL);
      ctx = &new_ctx;
    }
  t = cxx_eval_constant_expression (ctx, val, lval, non_constant_p,
				    overflow_p);
  if (new_ctor && t != ctx->ctor)
    free_constructor (ctx->ctor);
  return t;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type.  */

static tree
cxx_eval_component_reference (const constexpr_ctx *ctx, tree t,
			      value_cat lval,
			      bool *non_constant_p, bool *overflow_p)
{
  unsigned HOST_WIDE_INT i;
  tree field;
  tree value;
  tree part = TREE_OPERAND (t, 1);
  tree orig_whole = TREE_OPERAND (t, 0);
  tree whole = cxx_eval_constant_expression (ctx, orig_whole,
					     lval,
					     non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  if (INDIRECT_REF_P (whole)
      && integer_zerop (TREE_OPERAND (whole, 0)))
    {
      if (!ctx->quiet)
	error ("dereferencing a null pointer in %qE", orig_whole);
      *non_constant_p = true;
      return t;
    }

  if (TREE_CODE (whole) == PTRMEM_CST)
    whole = cplus_expand_constant (whole);
  if (whole == orig_whole)
    return t;
  if (lval == vc_discard)
    return t;
  if (lval)
    return fold_build3 (COMPONENT_REF, TREE_TYPE (t),
			whole, part, NULL_TREE);
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!ctx->quiet)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
      return t;
    }
  if ((cxx_dialect < cxx14 || CONSTRUCTOR_MUTABLE_POISON (whole))
      && DECL_MUTABLE_P (part))
    {
      if (!ctx->quiet)
	error ("mutable %qD is not usable in a constant expression", part);
      *non_constant_p = true;
      return t;
    }

  bool pmf = TYPE_PTRMEMFUNC_P (TREE_TYPE (whole));
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      /* Use name match for PMF fields, as a variant will have a
	 different FIELD_DECL with a different type.  */
      if (pmf ? DECL_NAME (field) == DECL_NAME (part)
	  : field == part)
	{
	  if (value)
	    {
	      STRIP_ANY_LOCATION_WRAPPER (value);
	      return value;
	    }
	  else
	    /* We're in the middle of initializing it.  */
	    break;
	}
    }
  if (TREE_CODE (TREE_TYPE (whole)) == UNION_TYPE)
    {
      if (CONSTRUCTOR_NELTS (whole) > 0)
	{
	  /* DR 1188 says we don't have to deal with this.  */
	  if (!ctx->quiet)
	    {
	      constructor_elt *cep = CONSTRUCTOR_ELT (whole, 0);
	      if (cep->value == NULL_TREE)
		error ("accessing uninitialized member %qD", part);
	      else
		error ("accessing %qD member instead of initialized %qD member "
		       "in constant expression", part, cep->index);
	    }
	  *non_constant_p = true;
	  return t;
	}
      else if (!CONSTRUCTOR_NO_CLEARING (whole))
	{
	  /* Value-initialized union, check if looking at the first member.  */
	  tree first = next_aggregate_field (TYPE_FIELDS (TREE_TYPE (whole)));
	  if (first != part)
	    {
	      if (!ctx->quiet)
		error ("accessing %qD member instead of initialized %qD "
		       "member in constant expression", part, first);
	      *non_constant_p = true;
	      return t;
	    }
	}
    }

  /* We only create a CONSTRUCTOR for a subobject when we modify it, so empty
     classes never get represented; throw together a value now.  */
  if (is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/false))
    return build_constructor (TREE_TYPE (t), NULL);

  gcc_assert (DECL_CONTEXT (part) == TYPE_MAIN_VARIANT (TREE_TYPE (whole)));

  if (CONSTRUCTOR_NO_CLEARING (whole))
    {
      /* 'whole' is part of the aggregate initializer we're currently
	 building; if there's no initializer for this member yet, that's an
	 error.  */
      if (!ctx->quiet)
	error ("accessing uninitialized member %qD", part);
      *non_constant_p = true;
      return t;
    }

  /* If there's no explicit init for this field, it's value-initialized.  */
  value = build_value_init (TREE_TYPE (t), tf_warning_or_error);
  return cxx_eval_constant_expression (ctx, value,
				       lval,
				       non_constant_p, overflow_p);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type that is
   expressed as a BIT_FIELD_REF.  */

static tree
cxx_eval_bit_field_ref (const constexpr_ctx *ctx, tree t,
			value_cat lval,
			bool *non_constant_p, bool *overflow_p)
{
  tree orig_whole = TREE_OPERAND (t, 0);
  tree retval, fldval, utype, mask;
  bool fld_seen = false;
  HOST_WIDE_INT istart, isize;
  tree whole = cxx_eval_constant_expression (ctx, orig_whole,
					     lval,
					     non_constant_p, overflow_p);
  tree start, field, value;
  unsigned HOST_WIDE_INT i;

  if (whole == orig_whole)
    return t;
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p
      && TREE_CODE (whole) != VECTOR_CST
      && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!ctx->quiet)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;

  if (TREE_CODE (whole) == VECTOR_CST || !INTEGRAL_TYPE_P (TREE_TYPE (t)))
    {
      if (tree r = fold_ternary (BIT_FIELD_REF, TREE_TYPE (t), whole,
				 TREE_OPERAND (t, 1), TREE_OPERAND (t, 2)))
	return r;
      if (!ctx->quiet)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
      return t;
    }

  start = TREE_OPERAND (t, 2);
  istart = tree_to_shwi (start);
  isize = tree_to_shwi (TREE_OPERAND (t, 1));
  utype = TREE_TYPE (t);
  if (!TYPE_UNSIGNED (utype))
    utype = build_nonstandard_integer_type (TYPE_PRECISION (utype), 1);
  retval = build_int_cst (utype, 0);
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      tree bitpos = bit_position (field);
      STRIP_ANY_LOCATION_WRAPPER (value);
      if (bitpos == start && DECL_SIZE (field) == TREE_OPERAND (t, 1))
	return value;
      if (TREE_CODE (TREE_TYPE (field)) == INTEGER_TYPE
	  && TREE_CODE (value) == INTEGER_CST
	  && tree_fits_shwi_p (bitpos)
	  && tree_fits_shwi_p (DECL_SIZE (field)))
	{
	  HOST_WIDE_INT bit = tree_to_shwi (bitpos);
	  HOST_WIDE_INT sz = tree_to_shwi (DECL_SIZE (field));
	  HOST_WIDE_INT shift;
	  if (bit >= istart && bit + sz <= istart + isize)
	    {
	      fldval = fold_convert (utype, value);
	      mask = build_int_cst_type (utype, -1);
	      mask = fold_build2 (LSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      mask = fold_build2 (RSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      fldval = fold_build2 (BIT_AND_EXPR, utype, fldval, mask);
	      shift = bit - istart;
	      if (BYTES_BIG_ENDIAN)
		shift = TYPE_PRECISION (utype) - shift - sz;
	      fldval = fold_build2 (LSHIFT_EXPR, utype, fldval,
				    size_int (shift));
	      retval = fold_build2 (BIT_IOR_EXPR, utype, retval, fldval);
	      fld_seen = true;
	    }
	}
    }
  if (fld_seen)
    return fold_convert (TREE_TYPE (t), retval);
  gcc_unreachable ();
  return error_mark_node;
}

/* Helper for cxx_eval_bit_cast.
   Check [bit.cast]/3 rules, bit_cast is constexpr only if the To and From
   types and types of all subobjects have is_union_v<T>, is_pointer_v<T>,
   is_member_pointer_v<T>, is_volatile_v<T> false and has no non-static
   data members of reference type.  */

static bool
check_bit_cast_type (const constexpr_ctx *ctx, location_t loc, tree type,
		     tree orig_type)
{
  if (TREE_CODE (type) == UNION_TYPE)
    {
      if (!ctx->quiet)
	{
	  if (type == orig_type)
	    error_at (loc, "%qs is not a constant expression because %qT is "
			   "a union type", "__builtin_bit_cast", type);
	  else
	    error_at (loc, "%qs is not a constant expression because %qT "
			   "contains a union type", "__builtin_bit_cast",
		      orig_type);
	}
      return true;
    }
  if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (!ctx->quiet)
	{
	  if (type == orig_type)
	    error_at (loc, "%qs is not a constant expression because %qT is "
			   "a pointer type", "__builtin_bit_cast", type);
	  else
	    error_at (loc, "%qs is not a constant expression because %qT "
			   "contains a pointer type", "__builtin_bit_cast",
		      orig_type);
	}
      return true;
    }
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (!ctx->quiet)
	{
	  if (type == orig_type)
	    error_at (loc, "%qs is not a constant expression because %qT is "
			   "a reference type", "__builtin_bit_cast", type);
	  else
	    error_at (loc, "%qs is not a constant expression because %qT "
			   "contains a reference type", "__builtin_bit_cast",
		      orig_type);
	}
      return true;
    }
  if (TYPE_PTRMEM_P (type))
    {
      if (!ctx->quiet)
	{
	  if (type == orig_type)
	    error_at (loc, "%qs is not a constant expression because %qT is "
			   "a pointer to member type", "__builtin_bit_cast",
		      type);
	  else
	    error_at (loc, "%qs is not a constant expression because %qT "
			   "contains a pointer to member type",
		      "__builtin_bit_cast", orig_type);
	}
      return true;
    }
  if (TYPE_VOLATILE (type))
    {
      if (!ctx->quiet)
	{
	  if (type == orig_type)
	    error_at (loc, "%qs is not a constant expression because %qT is "
			   "volatile", "__builtin_bit_cast", type);
	  else
	    error_at (loc, "%qs is not a constant expression because %qT "
			   "contains a volatile subobject",
		      "__builtin_bit_cast", orig_type);
	}
      return true;
    }
  if (TREE_CODE (type) == RECORD_TYPE)
    for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
      if (TREE_CODE (field) == FIELD_DECL
	  && check_bit_cast_type (ctx, loc, TREE_TYPE (field), orig_type))
	return true;
  if (TREE_CODE (type) == ARRAY_TYPE)
    return check_bit_cast_type (ctx, loc, TREE_TYPE (type), orig_type);
  return false;
}

/* Helper function for cxx_eval_bit_cast.  For unsigned char or
   std::byte members of CONSTRUCTOR (recursively) if they contain
   some indeterminate bits (as set in MASK), remove the ctor elts,
   mark the CONSTRUCTOR as CONSTRUCTOR_NO_CLEARING and clear the
   bits in MASK.  */

static void
clear_uchar_or_std_byte_in_mask (location_t loc, tree t, unsigned char *mask)
{
  if (TREE_CODE (t) != CONSTRUCTOR)
    return;

  unsigned i, j = 0;
  tree index, value;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), i, index, value)
    {
      tree type = TREE_TYPE (value);
      if (TREE_CODE (TREE_TYPE (t)) != ARRAY_TYPE
	  && DECL_BIT_FIELD_TYPE (index) != NULL_TREE)
	{
	  if (is_byte_access_type_not_plain_char (DECL_BIT_FIELD_TYPE (index)))
	    {
	      HOST_WIDE_INT fldsz = TYPE_PRECISION (TREE_TYPE (index));
	      gcc_assert (fldsz != 0);
	      HOST_WIDE_INT pos = int_byte_position (index);
	      HOST_WIDE_INT bpos
		= tree_to_uhwi (DECL_FIELD_BIT_OFFSET (index));
	      bpos %= BITS_PER_UNIT;
	      HOST_WIDE_INT end
		= ROUND_UP (bpos + fldsz, BITS_PER_UNIT) / BITS_PER_UNIT;
	      gcc_assert (end == 1 || end == 2);
	      unsigned char *p = mask + pos;
	      unsigned char mask_save[2];
	      mask_save[0] = mask[pos];
	      mask_save[1] = end == 2 ? mask[pos + 1] : 0;
	      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
		sorry_at (loc, "PDP11 bit-field handling unsupported"
			       " in %qs", "__builtin_bit_cast");
	      else if (BYTES_BIG_ENDIAN)
		{
		  /* Big endian.  */
		  if (bpos + fldsz <= BITS_PER_UNIT)
		    *p &= ~(((1 << fldsz) - 1)
			    << (BITS_PER_UNIT - bpos - fldsz));
		  else
		    {
		      gcc_assert (bpos);
		      *p &= ~(((1U << BITS_PER_UNIT) - 1) >> bpos);
		      p++;
		      fldsz -= BITS_PER_UNIT - bpos;
		      gcc_assert (fldsz && fldsz < BITS_PER_UNIT);
		      *p &= ((1U << BITS_PER_UNIT) - 1) >> fldsz;
		    }
		}
	      else
		{
		  /* Little endian.  */
		  if (bpos + fldsz <= BITS_PER_UNIT)
		    *p &= ~(((1 << fldsz) - 1) << bpos);
		  else
		    {
		      gcc_assert (bpos);
		      *p &= ~(((1 << BITS_PER_UNIT) - 1) << bpos);
		      p++;
		      fldsz -= BITS_PER_UNIT - bpos;
		      gcc_assert (fldsz && fldsz < BITS_PER_UNIT);
		      *p &= ~((1 << fldsz) - 1);
		    }
		}
	      if (mask_save[0] != mask[pos]
		  || (end == 2 && mask_save[1] != mask[pos + 1]))
		{
		  CONSTRUCTOR_NO_CLEARING (t) = 1;
		  continue;
		}
	    }
	}
      else if (is_byte_access_type_not_plain_char (type))
	{
	  HOST_WIDE_INT pos;
	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    pos = tree_to_shwi (index);
	  else
	    pos = int_byte_position (index);
	  if (mask[pos])
	    {
	      CONSTRUCTOR_NO_CLEARING (t) = 1;
	      mask[pos] = 0;
	      continue;
	    }
	}
      if (TREE_CODE (value) == CONSTRUCTOR)
	{
	  HOST_WIDE_INT pos;
	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    pos = tree_to_shwi (index)
		  * tree_to_shwi (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (t))));
	  else
	    pos = int_byte_position (index);
	  clear_uchar_or_std_byte_in_mask (loc, value, mask + pos);
	}
      if (i != j)
	{
	  CONSTRUCTOR_ELT (t, j)->index = index;
	  CONSTRUCTOR_ELT (t, j)->value = value;
	}
      ++j;
    }
  if (CONSTRUCTOR_NELTS (t) != j)
    vec_safe_truncate (CONSTRUCTOR_ELTS (t), j);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate a BIT_CAST_EXPR.  */

static tree
cxx_eval_bit_cast (const constexpr_ctx *ctx, tree t, bool *non_constant_p,
		   bool *overflow_p)
{
  if (check_bit_cast_type (ctx, EXPR_LOCATION (t), TREE_TYPE (t),
			   TREE_TYPE (t))
      || check_bit_cast_type (ctx, cp_expr_loc_or_loc (TREE_OPERAND (t, 0),
						       EXPR_LOCATION (t)),
			      TREE_TYPE (TREE_OPERAND (t, 0)),
			      TREE_TYPE (TREE_OPERAND (t, 0))))
    {
      *non_constant_p = true;
      return t;
    }

  tree op = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), vc_prvalue,
					  non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  location_t loc = EXPR_LOCATION (t);
  if (BITS_PER_UNIT != 8 || CHAR_BIT != 8)
    {
      if (!ctx->quiet)
	sorry_at (loc, "%qs cannot be constant evaluated on the target",
		       "__builtin_bit_cast");
      *non_constant_p = true;
      return t;
    }

  if (!tree_fits_shwi_p (TYPE_SIZE_UNIT (TREE_TYPE (t))))
    {
      if (!ctx->quiet)
	sorry_at (loc, "%qs cannot be constant evaluated because the "
		       "type is too large", "__builtin_bit_cast");
      *non_constant_p = true;
      return t;
    }

  HOST_WIDE_INT len = tree_to_shwi (TYPE_SIZE_UNIT (TREE_TYPE (t)));
  if (len < 0 || (int) len != len)
    {
      if (!ctx->quiet)
	sorry_at (loc, "%qs cannot be constant evaluated because the "
		       "type is too large", "__builtin_bit_cast");
      *non_constant_p = true;
      return t;
    }

  unsigned char buf[64];
  unsigned char *ptr, *mask;
  size_t alen = (size_t) len * 2;
  if (alen <= sizeof (buf))
    ptr = buf;
  else
    ptr = XNEWVEC (unsigned char, alen);
  mask = ptr + (size_t) len;
  /* At the beginning consider everything indeterminate.  */
  memset (mask, ~0, (size_t) len);

  if (native_encode_initializer (op, ptr, len, 0, mask) != len)
    {
      if (!ctx->quiet)
	sorry_at (loc, "%qs cannot be constant evaluated because the "
		       "argument cannot be encoded", "__builtin_bit_cast");
      *non_constant_p = true;
      if (ptr != buf)
	XDELETE (ptr);
      return t;
    }

  tree r = NULL_TREE;
  if (can_native_interpret_type_p (TREE_TYPE (t)))
    {
      r = native_interpret_expr (TREE_TYPE (t), ptr, len);
      if (is_byte_access_type_not_plain_char (TREE_TYPE (t)))
	{
	  gcc_assert (len == 1);
	  if (mask[0])
	    {
	      memset (mask, 0, len);
	      r = build_constructor (TREE_TYPE (r), NULL);
	      CONSTRUCTOR_NO_CLEARING (r) = 1;
	    }
	}
    }
  else if (TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE)
    {
      r = native_interpret_aggregate (TREE_TYPE (t), ptr, 0, len);
      if (r != NULL_TREE)
	{
	  clear_type_padding_in_mask (TREE_TYPE (t), mask);
	  clear_uchar_or_std_byte_in_mask (loc, r, mask);
	  if (CHECKING_P)
	    {
	      tree e = cxx_eval_bare_aggregate (ctx, r, vc_prvalue,
						non_constant_p, overflow_p);
	      gcc_checking_assert (e == r);
	      r = e;
	    }
	}
    }

  if (r != NULL_TREE)
    {
      for (int i = 0; i < len; i++)
	if (mask[i])
	  {
	    if (!ctx->quiet)
	      error_at (loc, "%qs accessing uninitialized byte at offset %d",
			     "__builtin_bit_cast", i);
	    *non_constant_p = true;
	    r = t;
	    break;
	  }
      if (ptr != buf)
	XDELETE (ptr);
      return r;
    }

  if (!ctx->quiet)
    sorry_at (loc, "%qs cannot be constant evaluated because the "
		   "argument cannot be interpreted", "__builtin_bit_cast");
  *non_constant_p = true;
  if (ptr != buf)
    XDELETE (ptr);
  return t;
}

/* Subroutine of cxx_eval_constant_expression.
   Evaluate a short-circuited logical expression T in the context
   of a given constexpr CALL.  BAILOUT_VALUE is the value for
   early return.  CONTINUE_VALUE is used here purely for
   sanity check purposes.  */

static tree
cxx_eval_logical_expression (const constexpr_ctx *ctx, tree t,
                             tree bailout_value, tree continue_value,
			     bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree lhs = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					   vc_prvalue, non_constant_p,
					   overflow_p);
  VERIFY_CONSTANT (lhs);
  if (tree_int_cst_equal (lhs, bailout_value))
    return lhs;
  gcc_assert (tree_int_cst_equal (lhs, continue_value));
  r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1),
				    vc_prvalue, non_constant_p,
				    overflow_p);
  VERIFY_CONSTANT (r);
  return r;
}

/* REF is a COMPONENT_REF designating a particular field.  V is a vector of
   CONSTRUCTOR elements to initialize (part of) an object containing that
   field.  Return a pointer to the constructor_elt corresponding to the
   initialization of the field.  */

static constructor_elt *
base_field_constructor_elt (vec<constructor_elt, va_gc> *v, tree ref)
{
  tree aggr = TREE_OPERAND (ref, 0);
  tree field = TREE_OPERAND (ref, 1);
  HOST_WIDE_INT i;
  constructor_elt *ce;

  gcc_assert (TREE_CODE (ref) == COMPONENT_REF);

  if (TREE_CODE (aggr) == COMPONENT_REF)
    {
      constructor_elt *base_ce
	= base_field_constructor_elt (v, aggr);
      v = CONSTRUCTOR_ELTS (base_ce->value);
    }

  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    if (ce->index == field)
      return ce;

  gcc_unreachable ();
  return NULL;
}

/* Some of the expressions fed to the constexpr mechanism are calls to
   constructors, which have type void.  In that case, return the type being
   initialized by the constructor.  */

static tree
initialized_type (tree t)
{
  if (TYPE_P (t))
    return t;
  tree type = TREE_TYPE (t);
  if (TREE_CODE (t) == CALL_EXPR)
    {
      /* A constructor call has void type, so we need to look deeper.  */
      tree fn = get_function_named_in_call (t);
      if (fn && TREE_CODE (fn) == FUNCTION_DECL
	  && DECL_CXX_CONSTRUCTOR_P (fn))
	type = DECL_CONTEXT (fn);
    }
  else if (TREE_CODE (t) == COMPOUND_EXPR)
    return initialized_type (TREE_OPERAND (t, 1));
  else if (TREE_CODE (t) == AGGR_INIT_EXPR)
    type = TREE_TYPE (AGGR_INIT_EXPR_SLOT (t));
  return cv_unqualified (type);
}

/* We're about to initialize element INDEX of an array or class from VALUE.
   Set up NEW_CTX appropriately by adjusting .object to refer to the
   subobject and creating a new CONSTRUCTOR if the element is itself
   a class or array.  */

static void
init_subob_ctx (const constexpr_ctx *ctx, constexpr_ctx &new_ctx,
	       tree index, tree &value)
{
  new_ctx = *ctx;

  if (index && TREE_CODE (index) != INTEGER_CST
      && TREE_CODE (index) != FIELD_DECL
      && TREE_CODE (index) != RANGE_EXPR)
    /* This won't have an element in the new CONSTRUCTOR.  */
    return;

  tree type = initialized_type (value);
  if (!AGGREGATE_TYPE_P (type) && !VECTOR_TYPE_P (type))
    /* A non-aggregate member doesn't get its own CONSTRUCTOR.  */
    return;
  if (VECTOR_TYPE_P (type)
      && VECTOR_TYPE_P (TREE_TYPE (ctx->ctor))
      && index == NULL_TREE)
    /* A vector inside of a vector CONSTRUCTOR, e.g. when a larger
       vector is constructed from smaller vectors, doesn't get its own
       CONSTRUCTOR either.  */
    return;

  /* The sub-aggregate initializer might contain a placeholder;
     update object to refer to the subobject and ctor to refer to
     the (newly created) sub-initializer.  */
  if (ctx->object)
    {
      if (index == NULL_TREE || TREE_CODE (index) == RANGE_EXPR)
	/* There's no well-defined subobject for this index.  */
	new_ctx.object = NULL_TREE;
      else
	new_ctx.object = build_ctor_subob_ref (index, type, ctx->object);
    }

  if (is_empty_class (type))
    /* Leave ctor null for an empty subobject, they aren't represented in the
       result of evaluation.  */
    new_ctx.ctor = NULL_TREE;
  else
    {
      tree elt = build_constructor (type, NULL);
      CONSTRUCTOR_NO_CLEARING (elt) = true;
      new_ctx.ctor = elt;
    }

  if (TREE_CODE (value) == TARGET_EXPR)
    /* Avoid creating another CONSTRUCTOR when we expand the TARGET_EXPR.  */
    value = TARGET_EXPR_INITIAL (value);
}

/* We're about to process an initializer for a class or array TYPE.  Make
   sure that CTX is set up appropriately.  */

static void
verify_ctor_sanity (const constexpr_ctx *ctx, tree type)
{
  /* We don't bother building a ctor for an empty base subobject.  */
  if (is_empty_class (type))
    return;

  /* We're in the middle of an initializer that might involve placeholders;
     our caller should have created a CONSTRUCTOR for us to put the
     initializer into.  We will either return that constructor or T.  */
  gcc_assert (ctx->ctor);
  gcc_assert (same_type_ignoring_top_level_qualifiers_p
	      (type, TREE_TYPE (ctx->ctor)));
  /* We used to check that ctx->ctor was empty, but that isn't the case when
     the object is zero-initialized before calling the constructor.  */
  if (ctx->object)
    {
      tree otype = TREE_TYPE (ctx->object);
      gcc_assert (same_type_ignoring_top_level_qualifiers_p (type, otype)
		  /* Handle flexible array members.  */
		  || (TREE_CODE (otype) == ARRAY_TYPE
		      && TYPE_DOMAIN (otype) == NULL_TREE
		      && TREE_CODE (type) == ARRAY_TYPE
		      && (same_type_ignoring_top_level_qualifiers_p
			  (TREE_TYPE (type), TREE_TYPE (otype)))));
    }
  gcc_assert (!ctx->object || !DECL_P (ctx->object)
	      || ctx->global->get_value (ctx->object) == ctx->ctor);
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T denotes a C-style array or a C-style
   aggregate.  Reduce it to a constant expression.  */

static tree
cxx_eval_bare_aggregate (const constexpr_ctx *ctx, tree t,
			 value_cat lval,
			 bool *non_constant_p, bool *overflow_p)
{
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
  bool changed = false;
  gcc_assert (!BRACE_ENCLOSED_INITIALIZER_P (t));
  tree type = TREE_TYPE (t);

  constexpr_ctx new_ctx;
  if (TYPE_PTRMEMFUNC_P (type) || VECTOR_TYPE_P (type))
    {
      /* We don't really need the ctx->ctor business for a PMF or
	 vector, but it's simpler to use the same code.  */
      new_ctx = *ctx;
      new_ctx.ctor = build_constructor (type, NULL);
      new_ctx.object = NULL_TREE;
      ctx = &new_ctx;
    };
  verify_ctor_sanity (ctx, type);
  vec<constructor_elt, va_gc> **p = nullptr;
  if (ctx->ctor)
    {
      p = &CONSTRUCTOR_ELTS (ctx->ctor);
      vec_alloc (*p, vec_safe_length (v));
      if (CONSTRUCTOR_PLACEHOLDER_BOUNDARY (t))
	CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ctx->ctor) = 1;
    }

  unsigned i;
  tree index, value;
  bool constant_p = true;
  bool side_effects_p = false;
  FOR_EACH_CONSTRUCTOR_ELT (v, i, index, value)
    {
      tree orig_value = value;
      init_subob_ctx (ctx, new_ctx, index, value);
      /* Like in cxx_eval_store_expression, omit entries for empty fields.  */
      bool no_slot = new_ctx.ctor == NULL_TREE;
      int pos_hint = -1;
      if (new_ctx.ctor != ctx->ctor && !no_slot)
	{
	  /* If we built a new CONSTRUCTOR, attach it now so that other
	     initializers can refer to it.  */
	  constructor_elt *cep = get_or_insert_ctor_field (ctx->ctor, index);
	  cep->value = new_ctx.ctor;
	  pos_hint = cep - (*p)->begin();
	}
      else if (TREE_CODE (type) == UNION_TYPE)
	/* Otherwise if we're constructing a non-aggregate union member, set
	   the active union member now so that we can later detect and diagnose
	   if its initializer attempts to activate another member.  */
	get_or_insert_ctor_field (ctx->ctor, index);
      tree elt = cxx_eval_constant_expression (&new_ctx, value,
					       lval,
					       non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (ctx->quiet && *non_constant_p)
	break;
      if (elt != orig_value)
	changed = true;

      if (!TREE_CONSTANT (elt))
	constant_p = false;
      if (TREE_SIDE_EFFECTS (elt))
	side_effects_p = true;
      if (index && TREE_CODE (index) == COMPONENT_REF)
	{
	  /* This is an initialization of a vfield inside a base
	     subaggregate that we already initialized; push this
	     initialization into the previous initialization.  */
	  constructor_elt *inner = base_field_constructor_elt (*p, index);
	  inner->value = elt;
	  changed = true;
	}
      else if (no_slot)
	/* This is an initializer for an empty field; now that we've
	   checked that it's constant, we can ignore it.  */
	changed = true;
      else if (index
	       && (TREE_CODE (index) == NOP_EXPR
		   || TREE_CODE (index) == POINTER_PLUS_EXPR))
	{
	  /* Old representation of empty bases.  FIXME remove.  */
	  gcc_checking_assert (false);
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (index))));
	  changed = true;
	}
      else
	{
	  if (TREE_CODE (type) == UNION_TYPE
	      && (*p)->last().index != index)
	    /* The initializer erroneously changed the active union member that
	       we're initializing.  */
	    gcc_assert (*non_constant_p);
	  else
	    {
	      /* The initializer might have mutated the underlying CONSTRUCTOR,
		 so recompute the location of the target constructer_elt.  */
	      constructor_elt *cep
		= get_or_insert_ctor_field (ctx->ctor, index, pos_hint);
	      cep->value = elt;
	    }

	  /* Adding or replacing an element might change the ctor's flags.  */
	  TREE_CONSTANT (ctx->ctor) = constant_p;
	  TREE_SIDE_EFFECTS (ctx->ctor) = side_effects_p;
	}
    }
  if (*non_constant_p)
    return t;
  if (!changed)
    {
      if (VECTOR_TYPE_P (type))
	t = fold (t);
      return t;
    }
  t = ctx->ctor;
  if (!t)
    t = build_constructor (type, NULL);
  /* We're done building this CONSTRUCTOR, so now we can interpret an
     element without an explicit initializer as value-initialized.  */
  CONSTRUCTOR_NO_CLEARING (t) = false;
  TREE_CONSTANT (t) = constant_p;
  TREE_SIDE_EFFECTS (t) = side_effects_p;
  if (VECTOR_TYPE_P (type))
    t = fold (t);
  return t;
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T is a VEC_INIT_EXPR which denotes the desired
   initialization of a non-static data member of array type.  Reduce it to a
   CONSTRUCTOR.

   Note that apart from value-initialization (when VALUE_INIT is true),
   this is only intended to support value-initialization and the
   initializations done by defaulted constructors for classes with
   non-static data members of array type.  In this case, VEC_INIT_EXPR_INIT
   will either be NULL_TREE for the default constructor, or a COMPONENT_REF
   for the copy/move constructor.  */

static tree
cxx_eval_vec_init_1 (const constexpr_ctx *ctx, tree atype, tree init,
		     bool value_init, value_cat lval,
		     bool *non_constant_p, bool *overflow_p)
{
  tree elttype = TREE_TYPE (atype);
  verify_ctor_sanity (ctx, atype);
  vec<constructor_elt, va_gc> **p = &CONSTRUCTOR_ELTS (ctx->ctor);
  bool pre_init = false;
  unsigned HOST_WIDE_INT i;
  tsubst_flags_t complain = ctx->quiet ? tf_none : tf_warning_or_error;

  if (init && TREE_CODE (init) == CONSTRUCTOR)
    return cxx_eval_bare_aggregate (ctx, init, lval,
				    non_constant_p, overflow_p);

  /* For the default constructor, build up a call to the default
     constructor of the element type.  We only need to handle class types
     here, as for a constructor to be constexpr, all members must be
     initialized, which for a defaulted default constructor means they must
     be of a class type with a constexpr default constructor.  */
  if (TREE_CODE (elttype) == ARRAY_TYPE)
    /* We only do this at the lowest level.  */;
  else if (value_init)
    {
      init = build_value_init (elttype, complain);
      pre_init = true;
    }
  else if (!init)
    {
      releasing_vec argvec;
      init = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&argvec, elttype, LOOKUP_NORMAL,
					complain);
      init = build_aggr_init_expr (elttype, init);
      pre_init = true;
    }

  bool zeroed_out = false;
  if (!CONSTRUCTOR_NO_CLEARING (ctx->ctor))
    {
      /* We're initializing an array object that had been zero-initialized
	 earlier.  Truncate ctx->ctor, and propagate its zeroed state by
	 clearing CONSTRUCTOR_NO_CLEARING on each of the aggregate element
	 initializers we append to it.  */
      gcc_checking_assert (initializer_zerop (ctx->ctor));
      zeroed_out = true;
      vec_safe_truncate (*p, 0);
    }

  tree nelts = get_array_or_vector_nelts (ctx, atype, non_constant_p,
					  overflow_p);
  unsigned HOST_WIDE_INT max = tree_to_uhwi (nelts);
  for (i = 0; i < max; ++i)
    {
      tree idx = build_int_cst (size_type_node, i);
      tree eltinit;
      bool reuse = false;
      constexpr_ctx new_ctx;
      init_subob_ctx (ctx, new_ctx, idx, pre_init ? init : elttype);
      bool no_slot = new_ctx.ctor == NULL_TREE;
      if (new_ctx.ctor != ctx->ctor && !no_slot)
	{
	  if (zeroed_out)
	    CONSTRUCTOR_NO_CLEARING (new_ctx.ctor) = false;
	  CONSTRUCTOR_APPEND_ELT (*p, idx, new_ctx.ctor);
	}
      if (TREE_CODE (elttype) == ARRAY_TYPE)
	{
	  /* A multidimensional array; recurse.  */
	  if (value_init || init == NULL_TREE)
	    {
	      eltinit = NULL_TREE;
	      reuse = i == 0;
	    }
	  else
	    eltinit = cp_build_array_ref (input_location, init, idx, complain);
	  eltinit = cxx_eval_vec_init_1 (&new_ctx, elttype, eltinit, value_init,
					 lval,
					 non_constant_p, overflow_p);
	}
      else if (pre_init)
	{
	  /* Initializing an element using value or default initialization
	     we just pre-built above.  */
	  if (init == void_node)
	    /* Trivial default-init, don't do anything to the CONSTRUCTOR.  */
	    return ctx->ctor;
	  eltinit = cxx_eval_constant_expression (&new_ctx, init, lval,
						  non_constant_p, overflow_p);
	  reuse = i == 0;
	}
      else
	{
	  /* Copying an element.  */
	  eltinit = cp_build_array_ref (input_location, init, idx, complain);
	  if (!lvalue_p (init))
	    eltinit = move (eltinit);
	  eltinit = (perform_implicit_conversion_flags
		     (elttype, eltinit, complain,
		      LOOKUP_IMPLICIT|LOOKUP_NO_NARROWING));
	  eltinit = cxx_eval_constant_expression (&new_ctx, eltinit, lval,
						  non_constant_p, overflow_p);
	}
      if (*non_constant_p)
	break;
      if (no_slot)
	{
	  /* This is an initializer for an empty subobject; now that we've
	     checked that it's constant, we can ignore it.  */
	  gcc_checking_assert (i == 0);
	  break;
	}
      else if (new_ctx.ctor != ctx->ctor)
	{
	  /* We appended this element above; update the value.  */
	  gcc_assert ((*p)->last().index == idx);
	  (*p)->last().value = eltinit;
	}
      else
	CONSTRUCTOR_APPEND_ELT (*p, idx, eltinit);
      /* Reuse the result of cxx_eval_constant_expression call
	 from the first iteration to all others if it is a constant
	 initializer that doesn't require relocations.  */
      if (reuse
	  && max > 1
	  && (eltinit == NULL_TREE
	      || (initializer_constant_valid_p (eltinit, TREE_TYPE (eltinit))
		  == null_pointer_node)))
	{
	  if (new_ctx.ctor != ctx->ctor)
	    eltinit = new_ctx.ctor;
	  tree range = build2 (RANGE_EXPR, size_type_node,
			       build_int_cst (size_type_node, 1),
			       build_int_cst (size_type_node, max - 1));
	  CONSTRUCTOR_APPEND_ELT (*p, range, unshare_constructor (eltinit));
	  break;
	}
      else if (i == 0)
	vec_safe_reserve (*p, max);
    }

  if (!*non_constant_p)
    {
      init = ctx->ctor;
      CONSTRUCTOR_NO_CLEARING (init) = false;
    }
  return init;
}

static tree
cxx_eval_vec_init (const constexpr_ctx *ctx, tree t,
		   value_cat lval,
		   bool *non_constant_p, bool *overflow_p)
{
  tree atype = TREE_TYPE (t);
  tree init = VEC_INIT_EXPR_INIT (t);
  bool value_init = VEC_INIT_EXPR_VALUE_INIT (t);
  if (!init || !BRACE_ENCLOSED_INITIALIZER_P (init))
    ;
  else if (CONSTRUCTOR_NELTS (init) == 0
	   && !CP_AGGREGATE_TYPE_P (strip_array_types (atype)))
    {
      /* Handle {} as value-init.  */
      init = NULL_TREE;
      value_init = true;
    }
  else
    {
      /* This is a more complicated case, like needing to loop over trailing
	 elements; call build_vec_init and evaluate the result.  */
      tsubst_flags_t complain = ctx->quiet ? tf_none : tf_warning_or_error;
      constexpr_ctx new_ctx = *ctx;
      if (!ctx->object)
	{
	  /* We want to have an initialization target for an VEC_INIT_EXPR.
	     If we don't already have one in CTX, use the VEC_INIT_EXPR_SLOT.  */
	  new_ctx.object = VEC_INIT_EXPR_SLOT (t);
	  tree ctor = new_ctx.ctor = build_constructor (atype, NULL);
	  CONSTRUCTOR_NO_CLEARING (ctor) = true;
	  ctx->global->put_value (new_ctx.object, ctor);
	  ctx = &new_ctx;
	}
      init = expand_vec_init_expr (ctx->object, t, complain);
      return cxx_eval_constant_expression (ctx, init, lval, non_constant_p,
					   overflow_p);
    }
  tree r = cxx_eval_vec_init_1 (ctx, atype, init, value_init,
				lval, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  else
    return r;
}

/* Like same_type_ignoring_top_level_qualifiers_p, but also handle the case
   where the desired type is an array of unknown bounds because the variable
   has had its bounds deduced since the wrapping expression was created.  */

static bool
same_type_ignoring_tlq_and_bounds_p (tree type1, tree type2)
{
  while (TREE_CODE (type1) == ARRAY_TYPE
	 && TREE_CODE (type2) == ARRAY_TYPE
	 && (!TYPE_DOMAIN (type1) || !TYPE_DOMAIN (type2)))
    {
      type1 = TREE_TYPE (type1);
      type2 = TREE_TYPE (type2);
    }
  return same_type_ignoring_top_level_qualifiers_p (type1, type2);
}

/* Try to determine the currently active union member for an expression
   with UNION_TYPE.  If it can be determined, return the FIELD_DECL,
   otherwise return NULL_TREE.  */

static tree
cxx_union_active_member (const constexpr_ctx *ctx, tree t)
{
  constexpr_ctx new_ctx = *ctx;
  new_ctx.quiet = true;
  bool non_constant_p = false, overflow_p = false;
  tree ctor = cxx_eval_constant_expression (&new_ctx, t, vc_prvalue,
					    &non_constant_p,
					    &overflow_p);
  if (TREE_CODE (ctor) == CONSTRUCTOR
      && CONSTRUCTOR_NELTS (ctor) == 1
      && CONSTRUCTOR_ELT (ctor, 0)->index
      && TREE_CODE (CONSTRUCTOR_ELT (ctor, 0)->index) == FIELD_DECL)
    return CONSTRUCTOR_ELT (ctor, 0)->index;
  return NULL_TREE;
}

/* Helper function for cxx_fold_indirect_ref_1, called recursively.  */

static tree
cxx_fold_indirect_ref_1 (const constexpr_ctx *ctx, location_t loc, tree type,
			 tree op, unsigned HOST_WIDE_INT off, bool *empty_base)
{
  tree optype = TREE_TYPE (op);
  unsigned HOST_WIDE_INT const_nunits;
  if (off == 0 && similar_type_p (optype, type))
    return op;
  else if (TREE_CODE (optype) == COMPLEX_TYPE
	   && similar_type_p (type, TREE_TYPE (optype)))
    {
      /* *(foo *)&complexfoo => __real__ complexfoo */
      if (off == 0)
	return build1_loc (loc, REALPART_EXPR, type, op);
      /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
      else if (tree_to_uhwi (TYPE_SIZE_UNIT (type)) == off)
	return build1_loc (loc, IMAGPART_EXPR, type, op);
    }
  /* ((foo*)&vectorfoo)[x] => BIT_FIELD_REF<vectorfoo,...> */
  else if (VECTOR_TYPE_P (optype)
	   && similar_type_p (type, TREE_TYPE (optype))
	   && TYPE_VECTOR_SUBPARTS (optype).is_constant (&const_nunits))
    {
      unsigned HOST_WIDE_INT part_width = tree_to_uhwi (TYPE_SIZE_UNIT (type));
      unsigned HOST_WIDE_INT max_offset = part_width * const_nunits;
      if (off < max_offset && off % part_width == 0)
	{
	  tree index = bitsize_int (off * BITS_PER_UNIT);
	  return build3_loc (loc, BIT_FIELD_REF, type, op,
			     TYPE_SIZE (type), index);
	}
    }
  /* ((foo *)&fooarray)[x] => fooarray[x] */
  else if (TREE_CODE (optype) == ARRAY_TYPE
	   && tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (optype)))
	   && !integer_zerop (TYPE_SIZE_UNIT (TREE_TYPE (optype))))
    {
      tree type_domain = TYPE_DOMAIN (optype);
      tree min_val = size_zero_node;
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      unsigned HOST_WIDE_INT el_sz
	= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (optype)));
      unsigned HOST_WIDE_INT idx = off / el_sz;
      unsigned HOST_WIDE_INT rem = off % el_sz;
      if (tree_fits_uhwi_p (min_val))
	{
	  tree index = size_int (idx + tree_to_uhwi (min_val));
	  op = build4_loc (loc, ARRAY_REF, TREE_TYPE (optype), op, index,
			   NULL_TREE, NULL_TREE);
	  return cxx_fold_indirect_ref_1 (ctx, loc, type, op, rem,
					  empty_base);
	}
    }
  /* ((foo *)&struct_with_foo_field)[x] => COMPONENT_REF */
  else if (TREE_CODE (optype) == RECORD_TYPE
	   || TREE_CODE (optype) == UNION_TYPE)
    {
      if (TREE_CODE (optype) == UNION_TYPE)
	/* For unions prefer the currently active member.  */
	if (tree field = cxx_union_active_member (ctx, op))
	  {
	    unsigned HOST_WIDE_INT el_sz
	      = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (field)));
	    if (off < el_sz)
	      {
		tree cop = build3 (COMPONENT_REF, TREE_TYPE (field),
				   op, field, NULL_TREE);
		if (tree ret = cxx_fold_indirect_ref_1 (ctx, loc, type, cop,
							off, empty_base))
		  return ret;
	      }
	  }

      /* Handle conversion to "as base" type.  */
      if (CLASS_TYPE_P (optype)
	  && CLASSTYPE_AS_BASE (optype) == type)
	return op;

      /* Handle conversion to an empty base class, which is represented with a
	 NOP_EXPR.  Do this before spelunking into the non-empty subobjects,
	 which is likely to be a waste of time (109678).  */
      if (is_empty_class (type)
	  && CLASS_TYPE_P (optype)
	  && lookup_base (optype, type, ba_any, NULL, tf_none, off))
	{
	  if (empty_base)
	    *empty_base = true;
	  return op;
	}

      for (tree field = TYPE_FIELDS (optype);
	   field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && TREE_TYPE (field) != error_mark_node
	    && tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (field))))
	  {
	    tree pos = byte_position (field);
	    if (!tree_fits_uhwi_p (pos))
	      continue;
	    unsigned HOST_WIDE_INT upos = tree_to_uhwi (pos);
	    unsigned HOST_WIDE_INT el_sz
	      = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (field)));
	    if (upos <= off && off < upos + el_sz)
	      {
		tree cop = build3 (COMPONENT_REF, TREE_TYPE (field),
				   op, field, NULL_TREE);
		if (tree ret = cxx_fold_indirect_ref_1 (ctx, loc, type, cop,
							off - upos,
							empty_base))
		  return ret;
	      }
	  }
    }

  return NULL_TREE;
}

/* A less strict version of fold_indirect_ref_1, which requires cv-quals to
   match.  We want to be less strict for simple *& folding; if we have a
   non-const temporary that we access through a const pointer, that should
   work.  We handle this here rather than change fold_indirect_ref_1
   because we're dealing with things like ADDR_EXPR of INTEGER_CST which
   don't really make sense outside of constant expression evaluation.  Also
   we want to allow folding to COMPONENT_REF, which could cause trouble
   with TBAA in fold_indirect_ref_1.  */

static tree
cxx_fold_indirect_ref (const constexpr_ctx *ctx, location_t loc, tree type,
		       tree op0, bool *empty_base /* = NULL*/)
{
  tree sub = op0;
  tree subtype;

  /* STRIP_NOPS, but stop if REINTERPRET_CAST_P.  */
  while (CONVERT_EXPR_P (sub) || TREE_CODE (sub) == NON_LVALUE_EXPR
	 || TREE_CODE (sub) == VIEW_CONVERT_EXPR)
    {
      if (TREE_CODE (sub) == NOP_EXPR
	  && REINTERPRET_CAST_P (sub))
	return NULL_TREE;
      sub = TREE_OPERAND (sub, 0);
    }

  subtype = TREE_TYPE (sub);
  if (!INDIRECT_TYPE_P (subtype))
    return NULL_TREE;

  /* Canonicalizes the given OBJ/OFF pair by iteratively absorbing
     the innermost component into the offset until it would make the
     offset positive, so that cxx_fold_indirect_ref_1 can identify
     more folding opportunities.  */
  auto canonicalize_obj_off = [] (tree& obj, tree& off) {
    while (TREE_CODE (obj) == COMPONENT_REF
	   /* We need to preserve union member accesses so that we can
	      later properly diagnose accessing the wrong member.  */
	   && TREE_CODE (TREE_TYPE (TREE_OPERAND (obj, 0))) == RECORD_TYPE
	   && (tree_int_cst_sign_bit (off) || integer_zerop (off)))
      {
	tree field = TREE_OPERAND (obj, 1);
	tree pos = byte_position (field);
	if (integer_zerop (off) && integer_nonzerop (pos))
	  /* If the offset is already 0, keep going as long as the
	     component is at position 0.  */
	  break;
	off = int_const_binop (PLUS_EXPR, off, pos);
	obj = TREE_OPERAND (obj, 0);
      }
  };

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);

      /* *&CONST_DECL -> to the value of the const decl.  */
      if (TREE_CODE (op) == CONST_DECL)
	return DECL_INITIAL (op);
      /* *&p => p;  make sure to handle *&"str"[cst] here.  */
      if (similar_type_p (optype, type))
	{
	  tree fop = fold_read_from_constant_string (op);
	  if (fop)
	    return fop;
	  else
	    return op;
	}
      else
	{
	  tree off = integer_zero_node;
	  canonicalize_obj_off (op, off);
	  gcc_assert (integer_zerop (off));
	  return cxx_fold_indirect_ref_1 (ctx, loc, type, op, 0, empty_base);
	}
    }
  else if (TREE_CODE (sub) == POINTER_PLUS_EXPR
	   && tree_fits_uhwi_p (TREE_OPERAND (sub, 1)))
    {
      tree op00 = TREE_OPERAND (sub, 0);
      tree off = TREE_OPERAND (sub, 1);

      STRIP_NOPS (op00);
      if (TREE_CODE (op00) == ADDR_EXPR)
	{
	  tree obj = TREE_OPERAND (op00, 0);
	  canonicalize_obj_off (obj, off);
	  return cxx_fold_indirect_ref_1 (ctx, loc, type, obj,
					  tree_to_uhwi (off), empty_base);
	}
    }
  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  else if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
	   && similar_type_p (type, TREE_TYPE (TREE_TYPE (subtype))))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      tree newsub
	= cxx_fold_indirect_ref (ctx, loc, TREE_TYPE (subtype), sub, NULL);
      if (newsub)
	sub = newsub;
      else
	sub = build1_loc (loc, INDIRECT_REF, TREE_TYPE (subtype), sub);
      type_domain = TYPE_DOMAIN (TREE_TYPE (sub));
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      return build4_loc (loc, ARRAY_REF, type, sub, min_val, NULL_TREE,
			 NULL_TREE);
    }

  return NULL_TREE;
}

static tree
cxx_eval_indirect_ref (const constexpr_ctx *ctx, tree t,
		       value_cat lval,
		       bool *non_constant_p, bool *overflow_p)
{
  tree orig_op0 = TREE_OPERAND (t, 0);
  bool empty_base = false;

  /* We can handle a MEM_REF like an INDIRECT_REF, if MEM_REF's second
     operand is an integer-zero.  Otherwise reject the MEM_REF for now.  */

  if (TREE_CODE (t) == MEM_REF
      && (!TREE_OPERAND (t, 1) || !integer_zerop (TREE_OPERAND (t, 1))))
    {
      gcc_assert (ctx->quiet);
      *non_constant_p = true;
      return t;
    }

  /* First try to simplify it directly.  */
  tree r = cxx_fold_indirect_ref (ctx, EXPR_LOCATION (t), TREE_TYPE (t),
				  orig_op0, &empty_base);
  if (!r)
    {
      /* If that didn't work, evaluate the operand first.  */
      tree op0 = cxx_eval_constant_expression (ctx, orig_op0,
					       vc_prvalue, non_constant_p,
					       overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p)
	return t;

      if (!lval && integer_zerop (op0))
	{
	  if (!ctx->quiet)
	    error ("dereferencing a null pointer");
	  *non_constant_p = true;
	  return t;
	}

      r = cxx_fold_indirect_ref (ctx, EXPR_LOCATION (t), TREE_TYPE (t), op0,
				 &empty_base);
      if (r == NULL_TREE)
	{
	  /* We couldn't fold to a constant value.  Make sure it's not
	     something we should have been able to fold.  */
	  tree sub = op0;
	  STRIP_NOPS (sub);
	  if (TREE_CODE (sub) == ADDR_EXPR)
	    {
	      gcc_assert (!similar_type_p
			  (TREE_TYPE (TREE_TYPE (sub)), TREE_TYPE (t)));
	      /* DR 1188 says we don't have to deal with this.  */
	      if (!ctx->quiet)
		error_at (cp_expr_loc_or_input_loc (t),
			  "accessing value of %qE through a %qT glvalue in a "
			  "constant expression", build_fold_indirect_ref (sub),
			  TREE_TYPE (t));
	      *non_constant_p = true;
	      return t;
	    }

	  if (lval == vc_glvalue && op0 != orig_op0)
	    return build1 (INDIRECT_REF, TREE_TYPE (t), op0);
	  if (!lval)
	    VERIFY_CONSTANT (t);
	  return t;
	}
    }

  r = cxx_eval_constant_expression (ctx, r,
				    lval, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  /* If we're pulling out the value of an empty base, just return an empty
     CONSTRUCTOR.  */
  if (empty_base && !lval)
    {
      r = build_constructor (TREE_TYPE (t), NULL);
      TREE_CONSTANT (r) = true;
    }

  return r;
}

/* Complain about R, a DECL that is accessed outside its lifetime.  */

static void
outside_lifetime_error (location_t loc, tree r)
{
  auto_diagnostic_group d;
  if (DECL_NAME (r) == heap_deleted_identifier)
    {
      /* Provide a more accurate message for deleted variables.  */
      error_at (loc, "use of allocated storage after deallocation "
		"in a constant expression");
      inform (DECL_SOURCE_LOCATION (r), "allocated here");
    }
  else
    {
      error_at (loc, "accessing %qE outside its lifetime", r);
      inform (DECL_SOURCE_LOCATION (r), "declared here");
    }
}

/* Complain about R, a VAR_DECL, not being usable in a constant expression.
   FUNDEF_P is true if we're checking a constexpr function body.
   Shared between potential_constant_expression and
   cxx_eval_constant_expression.  */

static void
non_const_var_error (location_t loc, tree r, bool fundef_p)
{
  auto_diagnostic_group d;
  tree type = TREE_TYPE (r);
  if (DECL_NAME (r) == heap_uninit_identifier
      || DECL_NAME (r) == heap_identifier
      || DECL_NAME (r) == heap_vec_uninit_identifier
      || DECL_NAME (r) == heap_vec_identifier)
    {
      if (constexpr_error (loc, fundef_p, "the content of uninitialized "
			   "storage is not usable in a constant expression"))
	inform (DECL_SOURCE_LOCATION (r), "allocated here");
      return;
    }
  if (DECL_NAME (r) == heap_deleted_identifier)
    {
      if (constexpr_error (loc, fundef_p, "use of allocated storage after "
			   "deallocation in a constant expression"))
	inform (DECL_SOURCE_LOCATION (r), "allocated here");
      return;
    }
  if (!constexpr_error (loc, fundef_p, "the value of %qD is not usable in "
			"a constant expression", r))
    return;
  /* Avoid error cascade.  */
  if (DECL_INITIAL (r) == error_mark_node)
    return;
  if (DECL_DECLARED_CONSTEXPR_P (r))
    inform (DECL_SOURCE_LOCATION (r),
	    "%qD used in its own initializer", r);
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    {
      if (!CP_TYPE_CONST_P (type))
	inform (DECL_SOURCE_LOCATION (r),
		"%q#D is not const", r);
      else if (CP_TYPE_VOLATILE_P (type))
	inform (DECL_SOURCE_LOCATION (r),
		"%q#D is volatile", r);
      else if (!DECL_INITIAL (r)
	       || !TREE_CONSTANT (DECL_INITIAL (r))
	       || !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (r))
	inform (DECL_SOURCE_LOCATION (r),
		"%qD was not initialized with a constant "
		"expression", r);
      else
	gcc_unreachable ();
    }
  else if (TYPE_REF_P (type))
    inform (DECL_SOURCE_LOCATION (r),
	    "%qD was not initialized with a constant "
	    "expression", r);
  else
    {
      if (cxx_dialect >= cxx11 && !DECL_DECLARED_CONSTEXPR_P (r))
	inform (DECL_SOURCE_LOCATION (r),
		"%qD was not declared %<constexpr%>", r);
      else
	inform (DECL_SOURCE_LOCATION (r),
		"%qD does not have integral or enumeration type",
		r);
    }
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for trinary expressions.  */

static tree
cxx_eval_trinary_expression (const constexpr_ctx *ctx, tree t,
			     value_cat lval,
			     bool *non_constant_p, bool *overflow_p)
{
  int i;
  tree args[3];
  tree val;

  for (i = 0; i < 3; i++)
    {
      args[i] = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, i),
					      lval,
					      non_constant_p, overflow_p);
      VERIFY_CONSTANT (args[i]);
    }

  val = fold_ternary_loc (EXPR_LOCATION (t), TREE_CODE (t), TREE_TYPE (t),
			  args[0], args[1], args[2]);
  if (val == NULL_TREE)
    return t;
  VERIFY_CONSTANT (val);
  return val;
}

/* True if T was declared in a function declared to be constexpr, and
   therefore potentially constant in C++14.  */

bool
var_in_constexpr_fn (tree t)
{
  tree ctx = DECL_CONTEXT (t);
  return (ctx && TREE_CODE (ctx) == FUNCTION_DECL
	  && DECL_DECLARED_CONSTEXPR_P (ctx));
}

/* True if a function might be constexpr: either a function that was
   declared constexpr, or a C++17 lambda op().  */

bool
maybe_constexpr_fn (tree t)
{
  return (DECL_DECLARED_CONSTEXPR_P (t)
	  || (cxx_dialect >= cxx17 && LAMBDA_FUNCTION_P (t))
	  || (flag_implicit_constexpr
	      && DECL_DECLARED_INLINE_P (STRIP_TEMPLATE (t))));
}

/* True if T was declared in a function that might be constexpr: either a
   function that was declared constexpr, or a C++17 lambda op().  */

bool
var_in_maybe_constexpr_fn (tree t)
{
  return (DECL_FUNCTION_SCOPE_P (t)
	  && maybe_constexpr_fn (DECL_CONTEXT (t)));
}

/* We're assigning INIT to TARGET.  In do_build_copy_constructor and
   build_over_call we implement trivial copy of a class with tail padding using
   assignment of character arrays, which is valid in normal code, but not in
   constexpr evaluation.  We don't need to worry about clobbering tail padding
   in constexpr evaluation, so strip the type punning.  */

static void
maybe_simplify_trivial_copy (tree &target, tree &init)
{
  if (TREE_CODE (target) == MEM_REF
      && TREE_CODE (init) == MEM_REF
      && TREE_TYPE (target) == TREE_TYPE (init)
      && TREE_CODE (TREE_TYPE (target)) == ARRAY_TYPE
      && TREE_TYPE (TREE_TYPE (target)) == unsigned_char_type_node)
    {
      target = build_fold_indirect_ref (TREE_OPERAND (target, 0));
      init = build_fold_indirect_ref (TREE_OPERAND (init, 0));
    }
}

/* Returns true if REF, which is a COMPONENT_REF, has any fields
   of constant type.  This does not check for 'mutable', so the
   caller is expected to be mindful of that.  */

static bool
cref_has_const_field (tree ref)
{
  while (TREE_CODE (ref) == COMPONENT_REF)
    {
      if (CP_TYPE_CONST_P (TREE_TYPE (TREE_OPERAND (ref, 1))))
       return true;
      ref = TREE_OPERAND (ref, 0);
    }
  return false;
}

/* Return true if we are modifying something that is const during constant
   expression evaluation.  CODE is the code of the statement, OBJ is the
   object in question, MUTABLE_P is true if one of the subobjects were
   declared mutable.  */

static bool
modifying_const_object_p (tree_code code, tree obj, bool mutable_p)
{
  /* If this is initialization, there's no problem.  */
  if (code != MODIFY_EXPR)
    return false;

  /* [basic.type.qualifier] "A const object is an object of type
     const T or a non-mutable subobject of a const object."  */
  if (mutable_p)
    return false;

  if (TREE_READONLY (obj))
    return true;

  if (CP_TYPE_CONST_P (TREE_TYPE (obj)))
    {
      /* Although a COMPONENT_REF may have a const type, we should
	 only consider it modifying a const object when any of the
	 field components is const.  This can happen when using
	 constructs such as const_cast<const T &>(m), making something
	 const even though it wasn't declared const.  */
      if (TREE_CODE (obj) == COMPONENT_REF)
	return cref_has_const_field (obj);
      else
	return true;
    }

  return false;
}

/* Evaluate an INIT_EXPR or MODIFY_EXPR.  */

static tree
cxx_eval_store_expression (const constexpr_ctx *ctx, tree t,
			   value_cat lval,
			   bool *non_constant_p, bool *overflow_p)
{
  constexpr_ctx new_ctx = *ctx;

  tree init = TREE_OPERAND (t, 1);

  if (TREE_CLOBBER_P (init)
      && CLOBBER_KIND (init) < CLOBBER_OBJECT_END)
    /* Only handle clobbers ending the lifetime of objects.  */
    return void_node;

  /* First we figure out where we're storing to.  */
  tree target = TREE_OPERAND (t, 0);

  maybe_simplify_trivial_copy (target, init);

  tree type = TREE_TYPE (target);
  bool preeval = SCALAR_TYPE_P (type) || TREE_CODE (t) == MODIFY_EXPR;
  if (preeval && !TREE_CLOBBER_P (init))
    {
      /* Evaluate the value to be stored without knowing what object it will be
	 stored in, so that any side-effects happen first.  */
      if (!SCALAR_TYPE_P (type))
	new_ctx.ctor = new_ctx.object = NULL_TREE;
      init = cxx_eval_constant_expression (&new_ctx, init, vc_prvalue,
					   non_constant_p, overflow_p);
      if (*non_constant_p)
	return t;
    }

  bool evaluated = false;
  if (lval == vc_glvalue)
    {
      /* If we want to return a reference to the target, we need to evaluate it
	 as a whole; otherwise, only evaluate the innermost piece to avoid
	 building up unnecessary *_REFs.  */
      target = cxx_eval_constant_expression (ctx, target, lval,
					     non_constant_p, overflow_p);
      evaluated = true;
      if (*non_constant_p)
	return t;
    }

  /* Find the underlying variable.  */
  releasing_vec refs;
  tree object = NULL_TREE;
  /* If we're modifying a const object, save it.  */
  tree const_object_being_modified = NULL_TREE;
  bool mutable_p = false;
  for (tree probe = target; object == NULL_TREE; )
    {
      switch (TREE_CODE (probe))
	{
	case BIT_FIELD_REF:
	case COMPONENT_REF:
	case ARRAY_REF:
	  {
	    tree ob = TREE_OPERAND (probe, 0);
	    tree elt = TREE_OPERAND (probe, 1);
	    if (TREE_CODE (elt) == FIELD_DECL && DECL_MUTABLE_P (elt))
	      mutable_p = true;
	    if (TREE_CODE (probe) == ARRAY_REF)
	      {
		elt = eval_and_check_array_index (ctx, probe, false,
						  non_constant_p, overflow_p);
		if (*non_constant_p)
		  return t;
	      }
	    /* We don't check modifying_const_object_p for ARRAY_REFs.  Given
	       "int a[10]", an ARRAY_REF "a[2]" can be "const int", even though
	       the array isn't const.  Instead, check "a" in the next iteration;
	       that will detect modifying "const int a[10]".  */
	    else if (evaluated
		     && modifying_const_object_p (TREE_CODE (t), probe,
						  mutable_p)
		     && const_object_being_modified == NULL_TREE)
	      const_object_being_modified = probe;

	    /* Track named member accesses for unions to validate modifications
	       that change active member.  */
	    if (!evaluated && TREE_CODE (probe) == COMPONENT_REF)
	      vec_safe_push (refs, probe);
	    else
	      vec_safe_push (refs, NULL_TREE);

	    vec_safe_push (refs, elt);
	    vec_safe_push (refs, TREE_TYPE (probe));
	    probe = ob;
	  }
	  break;

	case REALPART_EXPR:
	  gcc_assert (probe == target);
	  vec_safe_push (refs, NULL_TREE);
	  vec_safe_push (refs, probe);
	  vec_safe_push (refs, TREE_TYPE (probe));
	  probe = TREE_OPERAND (probe, 0);
	  break;

	case IMAGPART_EXPR:
	  gcc_assert (probe == target);
	  vec_safe_push (refs, NULL_TREE);
	  vec_safe_push (refs, probe);
	  vec_safe_push (refs, TREE_TYPE (probe));
	  probe = TREE_OPERAND (probe, 0);
	  break;

	default:
	  if (evaluated)
	    object = probe;
	  else
	    {
	      tree pvar = tree_strip_any_location_wrapper (probe);
	      if (VAR_P (pvar) && DECL_ANON_UNION_VAR_P (pvar))
		{
		  /* Stores to DECL_ANON_UNION_VAR_P var are allowed to change
		     active union member.  */
		  probe = DECL_VALUE_EXPR (pvar);
		  break;
		}
	      probe = cxx_eval_constant_expression (ctx, probe, vc_glvalue,
						    non_constant_p, overflow_p);
	      evaluated = true;
	      if (*non_constant_p)
		return t;
	    }
	  break;
	}
    }

  if (modifying_const_object_p (TREE_CODE (t), object, mutable_p)
      && const_object_being_modified == NULL_TREE)
    const_object_being_modified = object;

  if (DECL_P (object)
      && TREE_CLOBBER_P (init)
      && DECL_NAME (object) == heap_deleted_identifier)
    /* Ignore clobbers of deleted allocations for now; we'll get a better error
       message later when operator delete is called.  */
    return void_node;

  /* And then find/build up our initializer for the path to the subobject
     we're initializing.  */
  tree *valp;
  if (DECL_P (object))
    valp = ctx->global->get_value_ptr (object, TREE_CODE (t) == INIT_EXPR);
  else
    valp = NULL;
  if (!valp)
    {
      /* A constant-expression cannot modify objects from outside the
	 constant-expression.  */
      if (!ctx->quiet)
	{
	  auto_diagnostic_group d;
	  if (DECL_P (object) && DECL_NAME (object) == heap_deleted_identifier)
	    {
	      error ("modification of allocated storage after deallocation "
		     "is not a constant expression");
	      inform (DECL_SOURCE_LOCATION (object), "allocated here");
	    }
	  else if (DECL_P (object) && ctx->global->is_outside_lifetime (object))
	    {
	      if (TREE_CLOBBER_P (init))
		error ("destroying %qE outside its lifetime", object);
	      else
		error ("modification of %qE outside its lifetime "
		       "is not a constant expression", object);
	      inform (DECL_SOURCE_LOCATION (object), "declared here");
	    }
	  else
	    {
	      if (TREE_CLOBBER_P (init))
		error ("destroying %qE from outside current evaluation "
		       "is not a constant expression", object);
	      else
		error ("modification of %qE from outside current evaluation "
		       "is not a constant expression", object);
	    }
	}
      *non_constant_p = true;
      return t;
    }

  /* Handle explicit end-of-lifetime.  */
  if (TREE_CLOBBER_P (init))
    {
      if (refs->is_empty ())
	ctx->global->destroy_value (object);
      return void_node;
    }

  type = TREE_TYPE (object);
  bool no_zero_init = true;
  bool zero_padding_bits = false;

  auto_vec<tree *> ctors;
  releasing_vec indexes;
  auto_vec<int> index_pos_hints;
  bool activated_union_member_p = false;
  bool empty_base = false;
  while (!refs->is_empty ())
    {
      if (*valp == NULL_TREE)
	{
	  *valp = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_CLEARING (*valp) = no_zero_init;
	  CONSTRUCTOR_ZERO_PADDING_BITS (*valp) = zero_padding_bits;
	}
      else if (STRIP_ANY_LOCATION_WRAPPER (*valp),
	       TREE_CODE (*valp) == STRING_CST)
	{
	  /* An array was initialized with a string constant, and now
	     we're writing into one of its elements.  Explode the
	     single initialization into a set of element
	     initializations.  */
	  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);

	  tree string = *valp;
	  tree elt_type = TREE_TYPE (type);
	  unsigned chars_per_elt = (TYPE_PRECISION (elt_type)
				    / TYPE_PRECISION (char_type_node));
	  unsigned num_elts = TREE_STRING_LENGTH (string) / chars_per_elt;
	  tree ary_ctor = build_constructor (type, NULL);

	  vec_safe_reserve (CONSTRUCTOR_ELTS (ary_ctor), num_elts);
	  for (unsigned ix = 0; ix != num_elts; ix++)
	    {
	      constructor_elt elt =
		{
		  build_int_cst (size_type_node, ix),
		  extract_string_elt (string, chars_per_elt, ix)
		};
	      CONSTRUCTOR_ELTS (ary_ctor)->quick_push (elt);
	    }

	  *valp = ary_ctor;
	}

      enum tree_code code = TREE_CODE (type);
      tree reftype = refs->pop();
      tree index = refs->pop();
      bool is_access_expr = refs->pop() != NULL_TREE;

      if (code == COMPLEX_TYPE)
	{
	  if (TREE_CODE (*valp) == COMPLEX_CST)
	    *valp = build2 (COMPLEX_EXPR, type, TREE_REALPART (*valp),
			    TREE_IMAGPART (*valp));
	  else if (TREE_CODE (*valp) == CONSTRUCTOR
		   && CONSTRUCTOR_NELTS (*valp) == 0
		   && CONSTRUCTOR_NO_CLEARING (*valp))
	    {
	      tree r = build_constructor (reftype, NULL);
	      CONSTRUCTOR_NO_CLEARING (r) = 1;
	      *valp = build2 (COMPLEX_EXPR, type, r, r);
	    }
	  gcc_assert (TREE_CODE (*valp) == COMPLEX_EXPR);
	  ctors.safe_push (valp);
	  vec_safe_push (indexes, index);
	  valp = &TREE_OPERAND (*valp, TREE_CODE (index) == IMAGPART_EXPR);
	  gcc_checking_assert (refs->is_empty ());
	  type = reftype;
	  break;
	}

      /* If the value of object is already zero-initialized, any new ctors for
	 subobjects will also be zero-initialized.  Similarly with zeroing of
	 padding bits.  */
      no_zero_init = CONSTRUCTOR_NO_CLEARING (*valp);
      zero_padding_bits = CONSTRUCTOR_ZERO_PADDING_BITS (*valp);

      if (code == RECORD_TYPE && is_empty_field (index))
	/* Don't build a sub-CONSTRUCTOR for an empty base or field, as they
	   have no data and might have an offset lower than previously declared
	   fields, which confuses the middle-end.  The code below will notice
	   that we don't have a CONSTRUCTOR for our inner target and just
	   return init.  */
	{
	  empty_base = true;
	  break;
	}

      /* If a union is zero-initialized, its first non-static named data member
	 is zero-initialized (and therefore active).  */
      if (code == UNION_TYPE
	  && !no_zero_init
	  && CONSTRUCTOR_NELTS (*valp) == 0)
	if (tree first = next_aggregate_field (TYPE_FIELDS (type)))
	  CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (*valp), first, NULL_TREE);

      /* Check for implicit change of active member for a union.  */
      if (code == UNION_TYPE
	  && (CONSTRUCTOR_NELTS (*valp) == 0
	      || CONSTRUCTOR_ELT (*valp, 0)->index != index)
	  /* An INIT_EXPR of the last member in an access chain is always OK,
	     but still check implicit change of members earlier on; see
	     cpp2a/constexpr-union6.C.  */
	  && !(TREE_CODE (t) == INIT_EXPR && refs->is_empty ()))
	{
	  bool has_active_member = CONSTRUCTOR_NELTS (*valp) != 0;
	  tree inner = strip_array_types (reftype);

	  if (has_active_member && cxx_dialect < cxx20)
	    {
	      if (!ctx->quiet)
		error_at (cp_expr_loc_or_input_loc (t),
			  "change of the active member of a union "
			  "from %qD to %qD is not a constant expression "
			  "before C++20",
			  CONSTRUCTOR_ELT (*valp, 0)->index,
			  index);
	      *non_constant_p = true;
	    }
	  else if (!is_access_expr
		   || (TREE_CODE (t) == MODIFY_EXPR
		       && CLASS_TYPE_P (inner)
		       && !type_has_non_deleted_trivial_default_ctor (inner)))
	    {
	      /* Diagnose changing active union member after initialization
		 without a valid member access expression, as described in
		 [class.union.general] p5.  */
	      if (!ctx->quiet)
		{
		  auto_diagnostic_group d;
		  if (has_active_member)
		    error_at (cp_expr_loc_or_input_loc (t),
			      "accessing %qD member instead of initialized "
			      "%qD member in constant expression",
			      index, CONSTRUCTOR_ELT (*valp, 0)->index);
		  else
		    error_at (cp_expr_loc_or_input_loc (t),
			      "accessing uninitialized member %qD",
			      index);
		  if (is_access_expr)
		    inform (DECL_SOURCE_LOCATION (index),
			    "%qD does not implicitly begin its lifetime "
			    "because %qT does not have a non-deleted "
			    "trivial default constructor, use "
			    "%<std::construct_at%> instead",
			    index, inner);
		  else
		    inform (DECL_SOURCE_LOCATION (index),
			    "initializing %qD requires a member access "
			    "expression as the left operand of the assignment",
			    index);
		}
	      *non_constant_p = true;
	    }
	  else if (has_active_member && CONSTRUCTOR_NO_CLEARING (*valp))
	    {
	      /* Diagnose changing the active union member while the union
		 is in the process of being initialized.  */
	      if (!ctx->quiet)
		error_at (cp_expr_loc_or_input_loc (t),
			  "change of the active member of a union "
			  "from %qD to %qD during initialization",
			  CONSTRUCTOR_ELT (*valp, 0)->index,
			  index);
	      *non_constant_p = true;
	    }
	  no_zero_init = true;
	}

      ctors.safe_push (valp);
      vec_safe_push (indexes, index);

      constructor_elt *cep
	= get_or_insert_ctor_field (*valp, index);
      index_pos_hints.safe_push (cep - CONSTRUCTOR_ELTS (*valp)->begin());

      if (code == UNION_TYPE)
	activated_union_member_p = true;

      valp = &cep->value;
      type = reftype;
    }

  /* For initialization of an empty base, the original target will be
     *(base*)this, evaluation of which resolves to the object
     argument, which has the derived type rather than the base type.  */
  if (!empty_base && !(same_type_ignoring_top_level_qualifiers_p
		       (initialized_type (init), type)))
    {
      gcc_assert (is_empty_class (TREE_TYPE (target)));
      empty_base = true;
    }

  /* Detect modifying a constant object in constexpr evaluation.
     We have found a const object that is being modified.  Figure out
     if we need to issue an error.  Consider

     struct A {
       int n;
       constexpr A() : n(1) { n = 2; } // #1
     };
     struct B {
       const A a;
       constexpr B() { a.n = 3; } // #2
     };
    constexpr B b{};

    #1 is OK, since we're modifying an object under construction, but
    #2 is wrong, since "a" is const and has been fully constructed.
    To track it, we use the TREE_READONLY bit in the object's CONSTRUCTOR
    which means that the object is read-only.  For the example above, the
    *ctors stack at the point of #2 will look like:

      ctors[0] = {.a={.n=2}}  TREE_READONLY = 0
      ctors[1] = {.n=2}       TREE_READONLY = 1

    and we're modifying "b.a", so we search the stack and see if the
    constructor for "b.a" has already run.  */
  if (const_object_being_modified)
    {
      bool fail = false;
      tree const_objtype
	= strip_array_types (TREE_TYPE (const_object_being_modified));
      if (!CLASS_TYPE_P (const_objtype))
	fail = true;
      else
	{
	  /* [class.ctor]p5 "A constructor can be invoked for a const,
	     volatile, or const volatile object.  const and volatile
	     semantics are not applied on an object under construction.
	     They come into effect when the constructor for the most
	     derived object ends."  */
	  for (tree *elt : ctors)
	    if (same_type_ignoring_top_level_qualifiers_p
		(TREE_TYPE (const_object_being_modified), TREE_TYPE (*elt)))
	      {
		fail = TREE_READONLY (*elt);
		break;
	      }
	}
      if (fail)
	{
	  if (!ctx->quiet)
	    modifying_const_object_error (t, const_object_being_modified);
	  *non_constant_p = true;
	  return t;
	}
    }

  if (!preeval)
    {
      /* We're handling an INIT_EXPR of class type, so the value of the
	 initializer can depend on the object it's initializing.  */

      /* Create a new CONSTRUCTOR in case evaluation of the initializer
	 wants to modify it.  */
      if (*valp == NULL_TREE)
	{
	  *valp = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_CLEARING (*valp) = no_zero_init;
	  CONSTRUCTOR_ZERO_PADDING_BITS (*valp) = zero_padding_bits;
	}
      new_ctx.ctor = empty_base ? NULL_TREE : *valp;
      new_ctx.object = target;
      /* Avoid temporary materialization when initializing from a TARGET_EXPR.
	 We don't need to mess with AGGR_EXPR_SLOT/VEC_INIT_EXPR_SLOT because
	 expansion of those trees uses ctx instead.  */
      if (TREE_CODE (init) == TARGET_EXPR)
	if (tree tinit = TARGET_EXPR_INITIAL (init))
	  init = tinit;
      init = cxx_eval_constant_expression (&new_ctx, init, vc_prvalue,
					   non_constant_p, overflow_p);
      /* The hash table might have moved since the get earlier, and the
	 initializer might have mutated the underlying CONSTRUCTORs, so we must
	 recompute VALP. */
      valp = ctx->global->get_value_ptr (object, TREE_CODE (t) == INIT_EXPR);
      for (unsigned i = 0; i < vec_safe_length (indexes); i++)
	{
	  ctors[i] = valp;
	  constructor_elt *cep
	    = get_or_insert_ctor_field (*valp, indexes[i], index_pos_hints[i]);
	  valp = &cep->value;
	}
    }

  if (*non_constant_p)
    return t;

  /* Don't share a CONSTRUCTOR that might be changed later.  */
  init = unshare_constructor (init);

  gcc_checking_assert (!*valp || (same_type_ignoring_top_level_qualifiers_p
				  (TREE_TYPE (*valp), type)));
  if (empty_base)
    {
      /* Just evaluate the initializer and return, since there's no actual data
	 to store, and we didn't build a CONSTRUCTOR.  */
      if (!*valp)
	{
	  /* But do make sure we have something in *valp.  */
	  *valp = build_constructor (type, nullptr);
	  CONSTRUCTOR_NO_CLEARING (*valp) = no_zero_init;
	  CONSTRUCTOR_ZERO_PADDING_BITS (*valp) = zero_padding_bits;
	}
    }
  else if (*valp && TREE_CODE (*valp) == CONSTRUCTOR
	   && TREE_CODE (init) == CONSTRUCTOR)
    {
      /* An outer ctx->ctor might be pointing to *valp, so replace
	 its contents.  */
      CONSTRUCTOR_ELTS (*valp) = CONSTRUCTOR_ELTS (init);
      TREE_CONSTANT (*valp) = TREE_CONSTANT (init);
      TREE_SIDE_EFFECTS (*valp) = TREE_SIDE_EFFECTS (init);
      CONSTRUCTOR_NO_CLEARING (*valp)
	= CONSTRUCTOR_NO_CLEARING (init);
      CONSTRUCTOR_ZERO_PADDING_BITS (*valp)
        = CONSTRUCTOR_ZERO_PADDING_BITS (init);
    }
  else
    *valp = init;

  /* After initialization, 'const' semantics apply to the value of the
     object.  Make a note of this fact by marking the CONSTRUCTOR
     TREE_READONLY.  */
  if (TREE_CODE (t) == INIT_EXPR
      && !empty_base
      && TREE_CODE (*valp) == CONSTRUCTOR
      && TYPE_READONLY (type))
    {
      if (INDIRECT_REF_P (target)
	  && (is_this_parameter
	      (tree_strip_nop_conversions (TREE_OPERAND (target, 0)))))
	/* We've just initialized '*this' (perhaps via the target
	   constructor of a delegating constructor).  Leave it up to the
	   caller that set 'this' to set TREE_READONLY appropriately.  */
	gcc_checking_assert (same_type_ignoring_top_level_qualifiers_p
			     (TREE_TYPE (target), type) || empty_base);
      else
	TREE_READONLY (*valp) = true;
    }

  /* Update TREE_CONSTANT and TREE_SIDE_EFFECTS on enclosing
     CONSTRUCTORs, if any.  */
  bool c = TREE_CONSTANT (init);
  bool s = TREE_SIDE_EFFECTS (init);
  if (!indexes->is_empty ())
    {
      tree last = indexes->last ();
      if (TREE_CODE (last) == REALPART_EXPR
	  || TREE_CODE (last) == IMAGPART_EXPR)
	{
	  /* And canonicalize COMPLEX_EXPR into COMPLEX_CST if
	     possible.  */
	  tree *cexpr = ctors.last ();
	  if (tree c = const_binop (COMPLEX_EXPR, TREE_TYPE (*cexpr),
				    TREE_OPERAND (*cexpr, 0),
				    TREE_OPERAND (*cexpr, 1)))
	    *cexpr = c;
	  else
	    {
	      TREE_CONSTANT (*cexpr)
		= (TREE_CONSTANT (TREE_OPERAND (*cexpr, 0))
		   & TREE_CONSTANT (TREE_OPERAND (*cexpr, 1)));
	      TREE_SIDE_EFFECTS (*cexpr)
		= (TREE_SIDE_EFFECTS (TREE_OPERAND (*cexpr, 0))
		   | TREE_SIDE_EFFECTS (TREE_OPERAND (*cexpr, 1)));
	    }
	  c = TREE_CONSTANT (*cexpr);
	  s = TREE_SIDE_EFFECTS (*cexpr);
	}
    }
  if (!c || s || activated_union_member_p)
    for (tree *elt : ctors)
      {
	if (TREE_CODE (*elt) != CONSTRUCTOR)
	  continue;
	if (!c)
	  TREE_CONSTANT (*elt) = false;
	if (s)
	  TREE_SIDE_EFFECTS (*elt) = true;
	/* Clear CONSTRUCTOR_NO_CLEARING since we've activated a member of
	   this union.  */
	if (TREE_CODE (TREE_TYPE (*elt)) == UNION_TYPE)
	  CONSTRUCTOR_NO_CLEARING (*elt) = false;
      }

  if (lval)
    return target;
  else
    return init;
}

/* Evaluate a ++ or -- expression.  */

static tree
cxx_eval_increment_expression (const constexpr_ctx *ctx, tree t,
			      value_cat lval,
			      bool *non_constant_p, bool *overflow_p)
{
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);
  tree op = TREE_OPERAND (t, 0);
  tree offset = TREE_OPERAND (t, 1);
  gcc_assert (TREE_CONSTANT (offset));

  /* OFFSET is constant, but perhaps not constant enough.  We need to
     e.g. bash FLOAT_EXPRs to REAL_CSTs.  */
  offset = fold_simple (offset);

  /* The operand as an lvalue.  */
  op = cxx_eval_constant_expression (ctx, op, vc_glvalue,
				     non_constant_p, overflow_p);

  /* The operand as an rvalue.  */
  tree val
    = cxx_eval_constant_expression (ctx, op, vc_prvalue,
				    non_constant_p, overflow_p);
  /* Don't VERIFY_CONSTANT if this might be dealing with a pointer to
     a local array in a constexpr function.  */
  bool ptr = INDIRECT_TYPE_P (TREE_TYPE (val));
  if (!ptr)
    VERIFY_CONSTANT (val);

  /* The modified value.  */
  bool inc = (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR);
  tree mod;
  if (INDIRECT_TYPE_P (type))
    {
      /* The middle end requires pointers to use POINTER_PLUS_EXPR.  */
      offset = convert_to_ptrofftype (offset);
      if (!inc)
	offset = fold_build1 (NEGATE_EXPR, TREE_TYPE (offset), offset);
      mod = fold_build2 (POINTER_PLUS_EXPR, type, val, offset);
    }
  else if (c_promoting_integer_type_p (type)
	   && !TYPE_UNSIGNED (type)
	   && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
    {
      offset = fold_convert (integer_type_node, offset);
      mod = fold_convert (integer_type_node, val);
      tree t = fold_build2 (inc ? PLUS_EXPR : MINUS_EXPR, integer_type_node,
			    mod, offset);
      mod = fold_convert (type, t);
      if (TREE_OVERFLOW_P (mod) && !TREE_OVERFLOW_P (t))
	TREE_OVERFLOW (mod) = false;
    }
  else
    mod = fold_build2 (inc ? PLUS_EXPR : MINUS_EXPR, type, val, offset);
  if (!ptr)
    VERIFY_CONSTANT (mod);

  /* Storing the modified value.  */
  tree store = build2_loc (cp_expr_loc_or_loc (t, input_location),
			   MODIFY_EXPR, type, op, mod);
  mod = cxx_eval_constant_expression (ctx, store, lval,
				      non_constant_p, overflow_p);
  ggc_free (store);
  if (*non_constant_p)
    return t;

  /* And the value of the expression.  */
  if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
    /* Prefix ops are lvalues, but the caller might want an rvalue;
       lval has already been taken into account in the store above.  */
    return mod;
  else
    /* Postfix ops are rvalues.  */
    return val;
}

/* Predicates for the meaning of *jump_target.  */

static bool
returns (tree *jump_target)
{
  return *jump_target
    && TREE_CODE (*jump_target) == RETURN_EXPR;
}

static bool
breaks (tree *jump_target)
{
  return *jump_target
    && ((TREE_CODE (*jump_target) == LABEL_DECL
	 && LABEL_DECL_BREAK (*jump_target))
	|| TREE_CODE (*jump_target) == BREAK_STMT
	|| TREE_CODE (*jump_target) == EXIT_EXPR);
}

static bool
continues (tree *jump_target)
{
  return *jump_target
    && ((TREE_CODE (*jump_target) == LABEL_DECL
	 && LABEL_DECL_CONTINUE (*jump_target))
	|| TREE_CODE (*jump_target) == CONTINUE_STMT);

}

static bool
switches (tree *jump_target)
{
  return *jump_target
    && TREE_CODE (*jump_target) == INTEGER_CST;
}

/* Subroutine of cxx_eval_statement_list.  Determine whether the statement
   STMT matches *jump_target.  If we're looking for a case label and we see
   the default label, note it in ctx->css_state.  */

static bool
label_matches (const constexpr_ctx *ctx, tree *jump_target, tree stmt)
{
  switch (TREE_CODE (*jump_target))
    {
    case LABEL_DECL:
      if (TREE_CODE (stmt) == LABEL_EXPR
	  && LABEL_EXPR_LABEL (stmt) == *jump_target)
	return true;
      break;

    case INTEGER_CST:
      if (TREE_CODE (stmt) == CASE_LABEL_EXPR)
	{
	  gcc_assert (ctx->css_state != NULL);
	  if (!CASE_LOW (stmt))
	    {
	      /* default: should appear just once in a SWITCH_EXPR
		 body (excluding nested SWITCH_EXPR).  */
	      gcc_assert (*ctx->css_state != css_default_seen);
	      /* When evaluating SWITCH_EXPR body for the second time,
		 return true for the default: label.  */
	      if (*ctx->css_state == css_default_processing)
		return true;
	      *ctx->css_state = css_default_seen;
	    }
	  else if (CASE_HIGH (stmt))
	    {
	      if (tree_int_cst_le (CASE_LOW (stmt), *jump_target)
		  && tree_int_cst_le (*jump_target, CASE_HIGH (stmt)))
		return true;
	    }
	  else if (tree_int_cst_equal (*jump_target, CASE_LOW (stmt)))
	    return true;
	}
      break;

    case BREAK_STMT:
    case CONTINUE_STMT:
      /* These two are handled directly in cxx_eval_loop_expr by testing
	 breaks (jump_target) or continues (jump_target).  */
      break;

    default:
      gcc_unreachable ();
    }
  return false;
}

/* Evaluate a STATEMENT_LIST for side-effects.  Handles various jump
   semantics, for switch, break, continue, and return.  */

static tree
cxx_eval_statement_list (const constexpr_ctx *ctx, tree t,
			 bool *non_constant_p, bool *overflow_p,
			 tree *jump_target)
{
  tree local_target;
  /* In a statement-expression we want to return the last value.
     For empty statement expression return void_node.  */
  tree r = void_node;
  if (!jump_target)
    {
      local_target = NULL_TREE;
      jump_target = &local_target;
    }
  for (tree_stmt_iterator i = tsi_start (t); !tsi_end_p (i); ++i)
    {
      tree stmt = *i;

      /* We've found a continue, so skip everything until we reach
	 the label its jumping to.  */
      if (continues (jump_target))
	{
	  if (label_matches (ctx, jump_target, stmt))
	    /* Found it.  */
	    *jump_target = NULL_TREE;
	  else
	    continue;
	}
      if (TREE_CODE (stmt) == DEBUG_BEGIN_STMT)
	continue;

      value_cat lval = vc_discard;
      /* The result of a statement-expression is not wrapped in EXPR_STMT.  */
      if (tsi_one_before_end_p (i) && TREE_CODE (stmt) != EXPR_STMT)
	lval = vc_prvalue;

      r = cxx_eval_constant_expression (ctx, stmt, lval,
					non_constant_p, overflow_p,
					jump_target);
      if (*non_constant_p)
	break;
      if (returns (jump_target) || breaks (jump_target))
	break;
    }
  if (*jump_target && jump_target == &local_target)
    {
      /* We aren't communicating the jump to our caller, so give up.  We don't
	 need to support evaluation of jumps out of statement-exprs.  */
      if (!ctx->quiet)
	error_at (cp_expr_loc_or_input_loc (r),
		  "statement is not a constant expression");
      *non_constant_p = true;
    }
  return r;
}

/* Evaluate a LOOP_EXPR for side-effects.  Handles break and return
   semantics; continue semantics are covered by cxx_eval_statement_list.  */

static tree
cxx_eval_loop_expr (const constexpr_ctx *ctx, tree t,
		    bool *non_constant_p, bool *overflow_p,
		    tree *jump_target)
{
  tree local_target;
  if (!jump_target)
    {
      local_target = NULL_TREE;
      jump_target = &local_target;
    }

  tree body, cond = NULL_TREE, expr = NULL_TREE;
  int count = 0;
  switch (TREE_CODE (t))
    {
    case LOOP_EXPR:
      body = LOOP_EXPR_BODY (t);
      break;
    case DO_STMT:
      body = DO_BODY (t);
      cond = DO_COND (t);
      break;
    case WHILE_STMT:
      body = WHILE_BODY (t);
      cond = WHILE_COND (t);
      count = -1;
      break;
    case FOR_STMT:
      if (FOR_INIT_STMT (t))
	cxx_eval_constant_expression (ctx, FOR_INIT_STMT (t), vc_discard,
				      non_constant_p, overflow_p, jump_target);
      if (*non_constant_p)
	return NULL_TREE;
      body = FOR_BODY (t);
      cond = FOR_COND (t);
      expr = FOR_EXPR (t);
      count = -1;
      break;
    default:
      gcc_unreachable ();
    }
  do
    {
      if (count != -1)
	{
	  if (body)
	    cxx_eval_constant_expression (ctx, body, vc_discard,
					  non_constant_p, overflow_p,
					  jump_target);
	  if (breaks (jump_target))
	    {
	      *jump_target = NULL_TREE;
	      break;
	    }

	  if (TREE_CODE (t) != LOOP_EXPR && continues (jump_target))
	    *jump_target = NULL_TREE;

	  if (expr)
	    cxx_eval_constant_expression (ctx, expr, vc_prvalue,
					  non_constant_p, overflow_p,
					  jump_target);
	}

      if (cond)
	{
	  tree res
	    = cxx_eval_constant_expression (ctx, cond, vc_prvalue,
					    non_constant_p, overflow_p,
					    jump_target);
	  if (res)
	    {
	      if (verify_constant (res, ctx->quiet, non_constant_p,
				   overflow_p))
		break;
	      if (integer_zerop (res))
		break;
	    }
	  else
	    gcc_assert (*jump_target);
	}

      if (++count >= constexpr_loop_limit)
	{
	  if (!ctx->quiet)
	    error_at (cp_expr_loc_or_input_loc (t),
		      "%<constexpr%> loop iteration count exceeds limit of %d "
		      "(use %<-fconstexpr-loop-limit=%> to increase the limit)",
		      constexpr_loop_limit);
	  *non_constant_p = true;
	  break;
	}
    }
  while (!returns (jump_target)
	 && !breaks (jump_target)
	 && !continues (jump_target)
	 && (!switches (jump_target) || count == 0)
	 && !*non_constant_p);

  return NULL_TREE;
}

/* Evaluate a SWITCH_EXPR for side-effects.  Handles switch and break jump
   semantics.  */

static tree
cxx_eval_switch_expr (const constexpr_ctx *ctx, tree t,
		      bool *non_constant_p, bool *overflow_p,
		      tree *jump_target)
{
  tree cond
    = TREE_CODE (t) == SWITCH_STMT ? SWITCH_STMT_COND (t) : SWITCH_COND (t);
  cond = cxx_eval_constant_expression (ctx, cond, vc_prvalue,
				       non_constant_p, overflow_p);
  VERIFY_CONSTANT (cond);
  if (TREE_CODE (cond) != INTEGER_CST)
    {
      /* If the condition doesn't reduce to an INTEGER_CST it isn't a usable
	 switch condition even if it's constant enough for other things
	 (c++/113545).  */
      gcc_checking_assert (ctx->quiet);
      *non_constant_p = true;
      return t;
    }

  *jump_target = cond;

  tree body
    = TREE_CODE (t) == SWITCH_STMT ? SWITCH_STMT_BODY (t) : SWITCH_BODY (t);
  constexpr_ctx new_ctx = *ctx;
  constexpr_switch_state css = css_default_not_seen;
  new_ctx.css_state = &css;
  cxx_eval_constant_expression (&new_ctx, body, vc_discard,
				non_constant_p, overflow_p, jump_target);
  if (switches (jump_target) && css == css_default_seen)
    {
      /* If the SWITCH_EXPR body has default: label, process it once again,
	 this time instructing label_matches to return true for default:
	 label on switches (jump_target).  */
      css = css_default_processing;
      cxx_eval_constant_expression (&new_ctx, body, vc_discard,
				    non_constant_p, overflow_p, jump_target);
    }
  if (breaks (jump_target) || switches (jump_target))
    *jump_target = NULL_TREE;
  return NULL_TREE;
}

/* Find the object of TYPE under initialization in CTX.  */

static tree
lookup_placeholder (const constexpr_ctx *ctx, value_cat lval, tree type)
{
  if (!ctx)
    return NULL_TREE;

  /* Prefer the outermost matching object, but don't cross
     CONSTRUCTOR_PLACEHOLDER_BOUNDARY constructors.  */
  if (ctx->ctor && !CONSTRUCTOR_PLACEHOLDER_BOUNDARY (ctx->ctor))
    if (tree outer_ob = lookup_placeholder (ctx->parent, lval, type))
      return outer_ob;

  /* We could use ctx->object unconditionally, but using ctx->ctor when we
     can is a minor optimization.  */
  if (!lval && ctx->ctor && same_type_p (TREE_TYPE (ctx->ctor), type))
    return ctx->ctor;

  if (!ctx->object)
    return NULL_TREE;

  /* Since an object cannot have a field of its own type, we can search outward
     from ctx->object to find the unique containing object of TYPE.  */
  tree ob = ctx->object;
  while (ob)
    {
      if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (ob), type))
	break;
      if (handled_component_p (ob))
	ob = TREE_OPERAND (ob, 0);
      else
	ob = NULL_TREE;
    }

  return ob;
}

/* Complain about an attempt to evaluate inline assembly.  If FUNDEF_P is
   true, we're checking a constexpr function body.  */

static void
inline_asm_in_constexpr_error (location_t loc, bool fundef_p)
{
  auto_diagnostic_group d;
  if (constexpr_error (loc, fundef_p, "inline assembly is not a "
		       "constant expression"))
    inform (loc, "only unevaluated inline assembly is allowed in a "
	    "%<constexpr%> function in C++20");
}

/* We're getting the constant value of DECL in a manifestly constant-evaluated
   context; maybe complain about that.  */

static void
maybe_warn_about_constant_value (location_t loc, tree decl)
{
  static bool explained = false;
  if (cxx_dialect >= cxx17
      && warn_interference_size
      && !OPTION_SET_P (param_destruct_interfere_size)
      && DECL_CONTEXT (decl) == std_node
      && DECL_NAME (decl)
      && id_equal (DECL_NAME (decl), "hardware_destructive_interference_size")
      && (LOCATION_FILE (input_location) != main_input_filename
	  || module_exporting_p ())
      && warning_at (loc, OPT_Winterference_size, "use of %qD", decl)
      && !explained)
    {
      explained = true;
      inform (loc, "its value can vary between compiler versions or "
	      "with different %<-mtune%> or %<-mcpu%> flags");
      inform (loc, "if this use is part of a public ABI, change it to "
	      "instead use a constant variable you define");
      inform (loc, "the default value for the current CPU tuning "
	      "is %d bytes", param_destruct_interfere_size);
      inform (loc, "you can stabilize this value with %<--param "
	      "hardware_destructive_interference_size=%d%>, or disable "
	      "this warning with %<-Wno-interference-size%>",
	      param_destruct_interfere_size);
    }
}

/* For element type ELT_TYPE, return the appropriate type of the heap object
   containing such element(s).  COOKIE_SIZE is NULL or the size of cookie
   in bytes.  If COOKIE_SIZE is NULL, return array type
   ELT_TYPE[FULL_SIZE / sizeof(ELT_TYPE)], otherwise return
   struct { size_t[COOKIE_SIZE/sizeof(size_t)]; ELT_TYPE[N]; }
   where N is computed such that the size of the struct fits into FULL_SIZE.
   If ARG_SIZE is non-NULL, it is the first argument to the new operator.
   It should be passed if ELT_TYPE is zero sized type in which case FULL_SIZE
   will be also 0 and so it is not possible to determine the actual array
   size.  CTX, NON_CONSTANT_P and OVERFLOW_P are used during constant
   expression evaluation of subexpressions of ARG_SIZE.  */

static tree
build_new_constexpr_heap_type (const constexpr_ctx *ctx, tree elt_type,
			       tree cookie_size, tree full_size, tree arg_size,
			       bool *non_constant_p, bool *overflow_p)
{
  gcc_assert (cookie_size == NULL_TREE || tree_fits_uhwi_p (cookie_size));
  gcc_assert (tree_fits_uhwi_p (full_size));
  unsigned HOST_WIDE_INT csz = cookie_size ? tree_to_uhwi (cookie_size) : 0;
  if (arg_size)
    {
      STRIP_NOPS (arg_size);
      if (cookie_size)
	{
	  if (TREE_CODE (arg_size) != PLUS_EXPR)
	    arg_size = NULL_TREE;
	  else if (TREE_CODE (TREE_OPERAND (arg_size, 0)) == INTEGER_CST
		   && tree_int_cst_equal (cookie_size,
					  TREE_OPERAND (arg_size, 0)))
	    {
	      arg_size = TREE_OPERAND (arg_size, 1);
	      STRIP_NOPS (arg_size);
	    }
	  else if (TREE_CODE (TREE_OPERAND (arg_size, 1)) == INTEGER_CST
		   && tree_int_cst_equal (cookie_size,
					  TREE_OPERAND (arg_size, 1)))
	    {
	      arg_size = TREE_OPERAND (arg_size, 0);
	      STRIP_NOPS (arg_size);
	    }
	  else
	    arg_size = NULL_TREE;
	}
      if (arg_size && TREE_CODE (arg_size) == MULT_EXPR)
	{
	  tree op0 = TREE_OPERAND (arg_size, 0);
	  tree op1 = TREE_OPERAND (arg_size, 1);
	  if (integer_zerop (op0))
	    arg_size
	      = cxx_eval_constant_expression (ctx, op1, vc_prvalue,
					      non_constant_p, overflow_p);
	  else if (integer_zerop (op1))
	    arg_size
	      = cxx_eval_constant_expression (ctx, op0, vc_prvalue,
					      non_constant_p, overflow_p);
	  else
	    arg_size = NULL_TREE;
	}
      else
	arg_size = NULL_TREE;
    }

  unsigned HOST_WIDE_INT fsz = tree_to_uhwi (arg_size ? arg_size : full_size);
  if (!arg_size)
    {
      unsigned HOST_WIDE_INT esz = int_size_in_bytes (elt_type);
      gcc_assert (fsz >= csz);
      fsz -= csz;
      if (esz)
	fsz /= esz;
    }
  tree itype2 = build_index_type (size_int (fsz - 1));
  if (!cookie_size)
    return build_cplus_array_type (elt_type, itype2);
  return build_new_constexpr_heap_type (elt_type, cookie_size, itype2);
}

/* Attempt to reduce the expression T to a constant value.
   On failure, issue diagnostic and return error_mark_node.  */
/* FIXME unify with c_fully_fold */
/* FIXME overflow_p is too global */

static tree
cxx_eval_constant_expression (const constexpr_ctx *ctx, tree t,
			      value_cat lval,
			      bool *non_constant_p, bool *overflow_p,
			      tree *jump_target /* = NULL */)
{
  if (jump_target && *jump_target)
    {
      /* If we are jumping, ignore all statements/expressions except those
	 that could have LABEL_EXPR or CASE_LABEL_EXPR in their bodies.  */
      switch (TREE_CODE (t))
	{
	case BIND_EXPR:
	case STATEMENT_LIST:
	case LOOP_EXPR:
	case COND_EXPR:
	case IF_STMT:
	case DO_STMT:
	case WHILE_STMT:
	case FOR_STMT:
	  break;
	case LABEL_EXPR:
	case CASE_LABEL_EXPR:
	  if (label_matches (ctx, jump_target, t))
	    /* Found it.  */
	    *jump_target = NULL_TREE;
	  return NULL_TREE;
	default:
	  return NULL_TREE;
	}
    }
  if (error_operand_p (t))
    {
      *non_constant_p = true;
      return t;
    }

  /* Change the input location to the currently processed expression for
     better error messages when a subexpression has no location.  */
  location_t loc = cp_expr_loc_or_input_loc (t);
  iloc_sentinel sentinel (loc);

  STRIP_ANY_LOCATION_WRAPPER (t);

  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_OVERFLOW (t))
	{
	  if (!ctx->quiet)
	    permerror (input_location, "overflow in constant expression");
	  if (!flag_permissive || ctx->quiet)
	    *overflow_p = true;
	}

      if (TREE_CODE (t) == INTEGER_CST
	  && TYPE_PTR_P (TREE_TYPE (t))
	  /* INTEGER_CST with pointer-to-method type is only used
	     for a virtual method in a pointer to member function.
	     Don't reject those.  */
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (t))) != METHOD_TYPE
	  && !integer_zerop (t))
	{
	  if (!ctx->quiet)
	    error ("value %qE of type %qT is not a constant expression",
		   t, TREE_TYPE (t));
	  *non_constant_p = true;
	}

      return t;
    }

  /* Avoid excessively long constexpr evaluations.  */
  if (++ctx->global->constexpr_ops_count >= constexpr_ops_limit)
    {
      if (!ctx->quiet)
	error_at (loc,
		  "%<constexpr%> evaluation operation count exceeds limit of "
		  "%wd (use %<-fconstexpr-ops-limit=%> to increase the limit)",
		  constexpr_ops_limit);
      ctx->global->constexpr_ops_count = INTTYPE_MINIMUM (HOST_WIDE_INT);
      *non_constant_p = true;
      return t;
    }

  constexpr_ctx new_ctx;
  tree r = t;

  tree_code tcode = TREE_CODE (t);
  switch (tcode)
    {
    case RESULT_DECL:
      if (lval)
	return t;
      /* We ask for an rvalue for the RESULT_DECL when indirecting
	 through an invisible reference, or in named return value
	 optimization.  */
      if (tree v = ctx->global->get_value (t))
	return v;
      else
	{
	  if (!ctx->quiet)
	    error ("%qE is not a constant expression", t);
	  *non_constant_p = true;
	}
      break;

    case VAR_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	{
	  if (is_normal_capture_proxy (t)
	      && current_function_decl == DECL_CONTEXT (t))
	    {
	      /* Function parms aren't constexpr within the function
		 definition, so don't try to look at the closure.  But if the
		 captured variable is constant, try to evaluate it directly. */
	      r = DECL_CAPTURED_VARIABLE (t);
	      tree type = TREE_TYPE (t);
	      if (TYPE_REF_P (type) != TYPE_REF_P (TREE_TYPE (r)))
		{
		  /* Adjust r to match the reference-ness of t.  */
		  if (TYPE_REF_P (type))
		    r = build_address (r);
		  else
		    r = convert_from_reference (r);
		}
	    }
	  else
	    r = DECL_VALUE_EXPR (t);
	  return cxx_eval_constant_expression (ctx, r, lval, non_constant_p,
					       overflow_p);
	}
      /* fall through */
    case CONST_DECL:
      /* We used to not check lval for CONST_DECL, but darwin.cc uses
	 CONST_DECL for aggregate constants.  */
      if (lval)
	return t;
      else if (t == ctx->object)
	return ctx->ctor;
      if (VAR_P (t))
	{
	  if (tree v = ctx->global->get_value (t))
	    {
	      r = v;
	      break;
	    }
	  if (ctx->global->is_outside_lifetime (t))
	    {
	      if (!ctx->quiet)
		outside_lifetime_error (loc, t);
	      *non_constant_p = true;
	      break;
	    }
	}
      if (ctx->manifestly_const_eval == mce_true)
	maybe_warn_about_constant_value (loc, t);
      if (COMPLETE_TYPE_P (TREE_TYPE (t))
	  && is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/false))
	{
	  /* If the class is empty, we aren't actually loading anything.  */
	  r = build_constructor (TREE_TYPE (t), NULL);
	  TREE_CONSTANT (r) = true;
	}
      else if (ctx->strict)
	r = decl_really_constant_value (t, /*unshare_p=*/false);
      else
	r = decl_constant_value (t, /*unshare_p=*/false);
      if (TREE_CODE (r) == TARGET_EXPR
	  && TREE_CODE (TARGET_EXPR_INITIAL (r)) == CONSTRUCTOR)
	r = TARGET_EXPR_INITIAL (r);
      if (DECL_P (r)
	  /* P2280 allows references to unknown.  */
	  && !(VAR_P (t) && TYPE_REF_P (TREE_TYPE (t))))
	{
	  if (!ctx->quiet)
	    non_const_var_error (loc, r, /*fundef_p*/false);
	  *non_constant_p = true;
	}
      break;

    case DEBUG_BEGIN_STMT:
      /* ??? It might be nice to retain this information somehow, so
	 as to be able to step into a constexpr function call.  */
      /* Fall through.  */

    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case LABEL_DECL:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
    case PREDICT_EXPR:
      return t;

    case PARM_DECL:
      if (lval && !TYPE_REF_P (TREE_TYPE (t)))
	{
	  /* glvalue use.  */
	  if (TREE_ADDRESSABLE (TREE_TYPE (t)))
	    if (tree v = ctx->global->get_value (t))
	      r = v;
	}
      else if (tree v = ctx->global->get_value (t))
	{
	  r = v;
	  if (TREE_ADDRESSABLE (TREE_TYPE (t)))
	    r = cxx_eval_constant_expression (ctx, r, vc_prvalue,
					      non_constant_p, overflow_p);
	}
      else if (lval)
	/* Defer in case this is only used for its type.  */;
      else if (ctx->global->is_outside_lifetime (t))
	{
	  if (!ctx->quiet)
	    outside_lifetime_error (loc, t);
	  *non_constant_p = true;
	  break;
	}
      else if (COMPLETE_TYPE_P (TREE_TYPE (t))
	       && is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/false))
	{
	  /* If the class is empty, we aren't actually loading anything.  */
	  r = build_constructor (TREE_TYPE (t), NULL);
	  TREE_CONSTANT (r) = true;
	}
      else if (TYPE_REF_P (TREE_TYPE (t)))
	/* P2280 allows references to unknown...  */;
      else if (is_this_parameter (t))
	/* ...as well as the this pointer.  */;
      else
	{
	  if (!ctx->quiet)
	    error ("%qE is not a constant expression", t);
	  *non_constant_p = true;
	}
      break;

    case CALL_EXPR:
    case AGGR_INIT_EXPR:
      r = cxx_eval_call_expression (ctx, t, lval,
				    non_constant_p, overflow_p);
      break;

    case DECL_EXPR:
      {
	r = DECL_EXPR_DECL (t);
	if (TREE_CODE (r) == USING_DECL)
	  {
	    r = void_node;
	    break;
	  }

	if (VAR_P (r)
	    && (TREE_STATIC (r)
		|| (CP_DECL_THREAD_LOCAL_P (r) && !DECL_REALLY_EXTERN (r)))
	    /* Allow __FUNCTION__ etc.  */
	    && !DECL_ARTIFICIAL (r)
	    && !decl_constant_var_p (r))
	  {
	    if (!ctx->quiet)
	      {
		if (CP_DECL_THREAD_LOCAL_P (r))
		  error_at (loc, "control passes through definition of %qD "
				 "with thread storage duration", r);
		else
		  error_at (loc, "control passes through definition of %qD "
				 "with static storage duration", r);
	      }
	    *non_constant_p = true;
	    break;
	  }

	/* make_rtl_for_nonlocal_decl could have deferred emission of
	   a local static var, but if it appears in a statement expression
	   which is constant expression evaluated to e.g. just the address
	   of the variable, its DECL_EXPR will never be seen during
	   gimple lowering's record_vars_into as the statement expression
	   will not be in the IL at all.  */
	if (VAR_P (r)
	    && TREE_STATIC (r)
	    && !DECL_REALLY_EXTERN (r)
	    && DECL_FUNCTION_SCOPE_P (r)
	    && !var_in_maybe_constexpr_fn (r)
	    && decl_constant_var_p (r))
	  {
	    varpool_node *node = varpool_node::get (r);
	    if (node == NULL || !node->definition)
	      rest_of_decl_compilation (r, 0, at_eof);
	  }

	if (AGGREGATE_TYPE_P (TREE_TYPE (r))
	    || VECTOR_TYPE_P (TREE_TYPE (r)))
	  {
	    new_ctx = *ctx;
	    new_ctx.object = r;
	    new_ctx.ctor = build_constructor (TREE_TYPE (r), NULL);
	    CONSTRUCTOR_NO_CLEARING (new_ctx.ctor) = true;
	    ctx->global->put_value (r, new_ctx.ctor);
	    ctx = &new_ctx;
	  }

	if (tree init = DECL_INITIAL (r))
	  {
	    init = cxx_eval_constant_expression (ctx, init, vc_prvalue,
						 non_constant_p, overflow_p);
	    /* Don't share a CONSTRUCTOR that might be changed.  */
	    init = unshare_constructor (init);
	    /* Remember that a constant object's constructor has already
	       run.  */
	    if (CLASS_TYPE_P (TREE_TYPE (r))
		&& CP_TYPE_CONST_P (TREE_TYPE (r)))
	      TREE_READONLY (init) = true;
	    ctx->global->put_value (r, init);
	  }
	else if (ctx == &new_ctx)
	  /* We gave it a CONSTRUCTOR above.  */;
	else
	  ctx->global->put_value (r, NULL_TREE);
      }
      break;

    case TARGET_EXPR:
      {
	tree type = TREE_TYPE (t);

	if (!literal_type_p (type))
	  {
	    if (!ctx->quiet)
	      {
		auto_diagnostic_group d;
		error ("temporary of non-literal type %qT in a "
		       "constant expression", type);
		explain_non_literal_class (type);
	      }
	    *non_constant_p = true;
	    break;
	  }
	gcc_checking_assert (!TARGET_EXPR_DIRECT_INIT_P (t));
	/* Avoid evaluating a TARGET_EXPR more than once.  */
	tree slot = TARGET_EXPR_SLOT (t);
	if (tree v = ctx->global->get_value (slot))
	  {
	    if (lval)
	      return slot;
	    r = v;
	    break;
	  }
	if ((AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type)))
	  {
	    /* We're being expanded without an explicit target, so start
	       initializing a new object; expansion with an explicit target
	       strips the TARGET_EXPR before we get here.  */
	    new_ctx = *ctx;
	    /* Link CTX to NEW_CTX so that lookup_placeholder can resolve
	       any PLACEHOLDER_EXPR within the initializer that refers to the
	       former object under construction.  */
	    new_ctx.parent = ctx;
	    new_ctx.ctor = build_constructor (type, NULL);
	    CONSTRUCTOR_NO_CLEARING (new_ctx.ctor) = true;
	    new_ctx.object = slot;
	    ctx->global->put_value (new_ctx.object, new_ctx.ctor);
	    ctx = &new_ctx;
	  }
	/* Pass vc_prvalue because this indicates
	   initialization of a temporary.  */
	r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1), vc_prvalue,
					  non_constant_p, overflow_p);
	if (*non_constant_p)
	  break;
	/* If the initializer is complex, evaluate it to initialize slot.  */
	bool is_complex = target_expr_needs_replace (t);
	if (!is_complex)
	  {
	    r = unshare_constructor (r);
	    /* Adjust the type of the result to the type of the temporary.  */
	    r = adjust_temp_type (type, r);
	    ctx->global->put_value (slot, r);
	  }
	if (TARGET_EXPR_CLEANUP (t) && !CLEANUP_EH_ONLY (t))
	  ctx->global->cleanups->safe_push (TARGET_EXPR_CLEANUP (t));
	if (ctx->save_exprs)
	  ctx->save_exprs->safe_push (slot);
	if (lval)
	  return slot;
	if (is_complex)
	  r = ctx->global->get_value (slot);
      }
      break;

    case INIT_EXPR:
    case MODIFY_EXPR:
      gcc_assert (jump_target == NULL || *jump_target == NULL_TREE);
      r = cxx_eval_store_expression (ctx, t, lval,
				     non_constant_p, overflow_p);
      break;

    case SCOPE_REF:
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1),
					lval,
					non_constant_p, overflow_p);
      break;

    case RETURN_EXPR:
      if (TREE_OPERAND (t, 0) != NULL_TREE)
	r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					  lval,
					  non_constant_p, overflow_p);
      /* FALLTHRU */
    case BREAK_STMT:
    case CONTINUE_STMT:
      if (jump_target)
	*jump_target = t;
      else
	{
	  /* Can happen with ({ return true; }) && false; passed to
	     maybe_constant_value.  There is nothing to jump over in this
	     case, and the bug will be diagnosed later.  */
	  gcc_assert (ctx->quiet);
	  *non_constant_p = true;
	}
      break;

    case SAVE_EXPR:
      /* Avoid evaluating a SAVE_EXPR more than once.  */
      if (tree v = ctx->global->get_value (t))
	r = v;
      else
	{
	  r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), vc_prvalue,
					    non_constant_p, overflow_p);
	  if (*non_constant_p)
	    break;
	  ctx->global->put_value (t, r);
	  if (ctx->save_exprs)
	    ctx->save_exprs->safe_push (t);
	}
      break;

    case TRY_CATCH_EXPR:
      if (TREE_OPERAND (t, 0) == NULL_TREE)
	{
	  r = void_node;
	  break;
	}
      /* FALLTHRU */
    case NON_LVALUE_EXPR:
    case TRY_BLOCK:
    case MUST_NOT_THROW_EXPR:
    case EXPR_STMT:
    case EH_SPEC_BLOCK:
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					lval,
					non_constant_p, overflow_p,
					jump_target);
      break;

    case CLEANUP_POINT_EXPR:
      {
	auto_vec<tree, 2> cleanups;
	vec<tree> *prev_cleanups = ctx->global->cleanups;
	ctx->global->cleanups = &cleanups;

	auto_vec<tree, 10> save_exprs;
	constexpr_ctx new_ctx = *ctx;
	new_ctx.save_exprs = &save_exprs;

	r = cxx_eval_constant_expression (&new_ctx, TREE_OPERAND (t, 0),
					  lval,
					  non_constant_p, overflow_p,
					  jump_target);

	ctx->global->cleanups = prev_cleanups;
	unsigned int i;
	tree cleanup;
	/* Evaluate the cleanups.  */
	FOR_EACH_VEC_ELT_REVERSE (cleanups, i, cleanup)
	  cxx_eval_constant_expression (&new_ctx, cleanup, vc_discard,
					non_constant_p, overflow_p);

	/* Forget SAVE_EXPRs and TARGET_EXPRs created by this
	   full-expression.  */
	for (tree save_expr : save_exprs)
	  destroy_value_checked (ctx, save_expr, non_constant_p);
      }
      break;

    case TRY_FINALLY_EXPR:
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
					non_constant_p, overflow_p,
					jump_target);
      if (!*non_constant_p)
	/* Also evaluate the cleanup.  */
	cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1), vc_discard,
				      non_constant_p, overflow_p);
      break;

    case EH_ELSE_EXPR:
      /* Evaluate any cleanup that applies to non-EH exits.  */
      cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), vc_discard,
				    non_constant_p, overflow_p);

      /* We do not have constexpr exceptions yet, so skip the EH path.  */
      break;

    case CLEANUP_STMT:
      r = cxx_eval_constant_expression (ctx, CLEANUP_BODY (t), lval,
					non_constant_p, overflow_p,
					jump_target);
      if (!CLEANUP_EH_ONLY (t) && !*non_constant_p)
	{
	  iloc_sentinel ils (loc);
	  /* Also evaluate the cleanup.  */
	  cxx_eval_constant_expression (ctx, CLEANUP_EXPR (t), vc_discard,
					non_constant_p, overflow_p);
	}
      break;

      /* These differ from cxx_eval_unary_expression in that this doesn't
	 check for a constant operand or result; an address can be
	 constant without its operand being, and vice versa.  */
    case MEM_REF:
    case INDIRECT_REF:
      r = cxx_eval_indirect_ref (ctx, t, lval,
				 non_constant_p, overflow_p);
      break;

    case ADDR_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);
	tree op = cxx_eval_constant_expression (ctx, oldop, vc_glvalue,
						non_constant_p, overflow_p);
	/* Don't VERIFY_CONSTANT here.  */
	if (*non_constant_p)
	  return t;
	gcc_checking_assert (TREE_CODE (op) != CONSTRUCTOR);
	/* This function does more aggressive folding than fold itself.  */
	r = build_fold_addr_expr_with_type (op, TREE_TYPE (t));
	if (TREE_CODE (r) == ADDR_EXPR && TREE_OPERAND (r, 0) == oldop)
	  {
	    ggc_free (r);
	    return t;
	  }
	break;
      }

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      if (lval)
	{
	  r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
					    non_constant_p, overflow_p);
	  if (r == error_mark_node)
	    ;
	  else if (r == TREE_OPERAND (t, 0) || lval == vc_discard)
	    r = t;
	  else
	    r = fold_build1 (TREE_CODE (t), TREE_TYPE (t), r);
	  break;
	}
      /* FALLTHRU */
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case ABSU_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    case VEC_DUPLICATE_EXPR:
      r = cxx_eval_unary_expression (ctx, t, lval,
				     non_constant_p, overflow_p);
      break;

    case SIZEOF_EXPR:
      r = fold_sizeof_expr (t);
      /* In a template, fold_sizeof_expr may merely create a new SIZEOF_EXPR,
	 which could lead to an infinite recursion.  */
      if (TREE_CODE (r) != SIZEOF_EXPR)
	r = cxx_eval_constant_expression (ctx, r, lval,
					  non_constant_p, overflow_p,
					  jump_target);
      else
	{
	  *non_constant_p = true;
	  gcc_assert (ctx->quiet);
	}

      break;

    case COMPOUND_EXPR:
      {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
	  r = cxx_eval_constant_expression (ctx, op0,
					    lval, non_constant_p, overflow_p,
					    jump_target);
	else
	  {
	    /* Check that the LHS is constant and then discard it.  */
	    cxx_eval_constant_expression (ctx, op0, vc_discard,
					  non_constant_p, overflow_p,
					  jump_target);
	    if (*non_constant_p)
	      return t;
	    op1 = TREE_OPERAND (t, 1);
	    r = cxx_eval_constant_expression (ctx, op1,
					      lval, non_constant_p, overflow_p,
					      jump_target);
	  }
      }
      break;

    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case SPACESHIP_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      r = cxx_eval_binary_expression (ctx, t, lval,
				      non_constant_p, overflow_p);
      break;

      /* fold can introduce non-IF versions of these; still treat them as
	 short-circuiting.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      r = cxx_eval_logical_expression (ctx, t, boolean_false_node,
				       boolean_true_node,
				       non_constant_p, overflow_p);
      break;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      r = cxx_eval_logical_expression (ctx, t, boolean_true_node,
				       boolean_false_node,
				       non_constant_p, overflow_p);
      break;

    case ARRAY_REF:
      r = cxx_eval_array_reference (ctx, t, lval,
				    non_constant_p, overflow_p);
      break;

    case COMPONENT_REF:
      if (is_overloaded_fn (t))
	{
	  /* We can only get here in checking mode via
	     build_non_dependent_expr,  because any expression that
	     calls or takes the address of the function will have
	     pulled a FUNCTION_DECL out of the COMPONENT_REF.  */
	  gcc_checking_assert (ctx->quiet || errorcount);
	  *non_constant_p = true;
	  return t;
	}
      r = cxx_eval_component_reference (ctx, t, lval,
					non_constant_p, overflow_p);
      break;

    case BIT_FIELD_REF:
      r = cxx_eval_bit_field_ref (ctx, t, lval,
				  non_constant_p, overflow_p);
      break;

    case COND_EXPR:
    case IF_STMT:
      if (jump_target && *jump_target)
	{
	  tree orig_jump = *jump_target;
	  tree arg = ((TREE_CODE (t) != IF_STMT || TREE_OPERAND (t, 1))
		      ? TREE_OPERAND (t, 1) : void_node);
	  /* When jumping to a label, the label might be either in the
	     then or else blocks, so process then block first in skipping
	     mode first, and if we are still in the skipping mode at its end,
	     process the else block too.  */
	  r = cxx_eval_constant_expression (ctx, arg, lval, non_constant_p,
					    overflow_p, jump_target);
	  /* It's possible that we found the label in the then block.  But
	     it could have been followed by another jumping statement, e.g.
	     say we're looking for case 1:
	      if (cond)
		{
		  // skipped statements
		  case 1:; // clears up *jump_target
		  return 1; // and sets it to a RETURN_EXPR
		}
	      else { ... }
	     in which case we need not go looking to the else block.
	     (goto is not allowed in a constexpr function.)  */
	  if (*jump_target == orig_jump)
	    {
	      arg = ((TREE_CODE (t) != IF_STMT || TREE_OPERAND (t, 2))
		     ? TREE_OPERAND (t, 2) : void_node);
	      r = cxx_eval_constant_expression (ctx, arg, lval, non_constant_p,
						overflow_p, jump_target);
	    }
	  break;
	}
      r = cxx_eval_conditional_expression (ctx, t, lval,
					   non_constant_p, overflow_p,
					   jump_target);
      break;
    case VEC_COND_EXPR:
      r = cxx_eval_vector_conditional_expression (ctx, t, non_constant_p,
						  overflow_p);
      break;

    case CONSTRUCTOR:
      if (TREE_CONSTANT (t) && reduced_constant_expression_p (t))
	{
	  /* Don't re-process a constant CONSTRUCTOR.  */
	  verify_constructor_flags (t);
	  if (TREE_CONSTANT (t))
	    return t;
	}
      r = cxx_eval_bare_aggregate (ctx, t, lval,
				   non_constant_p, overflow_p);
      break;

    case VEC_INIT_EXPR:
      /* We can get this in a defaulted constructor for a class with a
	 non-static data member of array type.  Either the initializer will
	 be NULL, meaning default-initialization, or it will be an lvalue
	 or xvalue of the same type, meaning direct-initialization from the
	 corresponding member.  */
      r = cxx_eval_vec_init (ctx, t, lval,
			     non_constant_p, overflow_p);
      break;

    case VEC_PERM_EXPR:
      r = cxx_eval_trinary_expression (ctx, t, lval,
				       non_constant_p, overflow_p);
      break;

    case PAREN_EXPR:
      gcc_assert (!REF_PARENTHESIZED_P (t));
      /* A PAREN_EXPR resulting from __builtin_assoc_barrier has no effect in
         constant expressions since it's unaffected by -fassociative-math.  */
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
					non_constant_p, overflow_p);
      break;

    case NOP_EXPR:
      if (REINTERPRET_CAST_P (t))
	{
	  if (!ctx->quiet)
	    error_at (loc,
		      "%<reinterpret_cast%> is not a constant expression");
	  *non_constant_p = true;
	  return t;
	}
      /* FALLTHROUGH.  */
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
    case UNARY_PLUS_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);

	tree op = cxx_eval_constant_expression (ctx, oldop,
						VOID_TYPE_P (TREE_TYPE (t))
						? vc_discard
						: tcode == VIEW_CONVERT_EXPR
						? lval : vc_prvalue,
						non_constant_p, overflow_p);
	if (*non_constant_p)
	  return t;
	tree type = TREE_TYPE (t);

	if (VOID_TYPE_P (type))
	  return void_node;

	if (TREE_CODE (t) == CONVERT_EXPR
	    && ARITHMETIC_TYPE_P (type)
	    && INDIRECT_TYPE_P (TREE_TYPE (op))
	    && ctx->manifestly_const_eval == mce_true)
	  {
	    if (!ctx->quiet)
	      error_at (loc,
			"conversion from pointer type %qT to arithmetic type "
			"%qT in a constant expression", TREE_TYPE (op), type);
	    *non_constant_p = true;
	    return t;
	  }

	/* [expr.const]: a conversion from type cv void* to a pointer-to-object
	   type cannot be part of a core constant expression as a resolution to
	   DR 1312.  */
	if (TYPE_PTROB_P (type)
	    && TYPE_PTR_P (TREE_TYPE (op))
	    && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (op)))
	    /* Inside a call to std::construct_at,
	       std::allocator<T>::{,de}allocate, or
	       std::source_location::current, we permit casting from void*
	       because that is compiler-generated code.  */
	    && !is_std_construct_at (ctx->call)
	    && !is_std_allocator_allocate (ctx->call)
	    && !is_std_source_location_current (ctx->call))
	  {
	    /* Likewise, don't error when casting from void* when OP is
	       &heap uninit and similar.  */
	    tree sop = tree_strip_nop_conversions (op);
	    tree decl = NULL_TREE;
	    if (TREE_CODE (sop) == ADDR_EXPR)
	      decl = TREE_OPERAND (sop, 0);
	    if (decl
		&& VAR_P (decl)
		&& DECL_ARTIFICIAL (decl)
		&& (DECL_NAME (decl) == heap_identifier
		    || DECL_NAME (decl) == heap_uninit_identifier
		    || DECL_NAME (decl) == heap_vec_identifier
		    || DECL_NAME (decl) == heap_vec_uninit_identifier))
	      /* OK */;
	    /* P2738 (C++26): a conversion from a prvalue P of type "pointer to
	       cv void" to a pointer-to-object type T unless P is a null
	       pointer value or points to an object whose type is similar to
	       T.  */
	    else if (cxx_dialect > cxx23)
	      {
		if (integer_zerop (sop))
		  return build_int_cst (type, 0);
		r = cxx_fold_indirect_ref (ctx, loc, TREE_TYPE (type), sop);
		if (r)
		  {
		    r = build1 (ADDR_EXPR, type, r);
		    break;
		  }
		if (!ctx->quiet)
		  {
		    gcc_assert (TREE_CODE (sop) == ADDR_EXPR);
		    auto_diagnostic_group d;
		    error_at (loc, "cast from %qT is not allowed in a "
			      "constant expression because "
			      "pointed-to type %qT is not similar to %qT",
			      TREE_TYPE (op), TREE_TYPE (TREE_TYPE (sop)),
			      TREE_TYPE (type));
		    tree obj = build_fold_indirect_ref (sop);
		    if (TREE_CODE (obj) == COMPONENT_REF)
		      obj = TREE_OPERAND (obj, 1);
		    if (DECL_P (obj))
		      inform (DECL_SOURCE_LOCATION (obj),
			      "pointed-to object declared here");
		  }
		*non_constant_p = true;
		return t;
	      }
	    else
	      {
		if (!ctx->quiet)
		  error_at (loc, "cast from %qT is not allowed in a "
			    "constant expression before C++26",
			    TREE_TYPE (op));
		*non_constant_p = true;
		return t;
	      }
	  }

	if (TREE_CODE (op) == PTRMEM_CST && !TYPE_PTRMEM_P (type))
	  {
	    op = cplus_expand_constant (op);
	    if (TREE_CODE (op) == PTRMEM_CST)
	      {
		if (!ctx->quiet)
		  error_at (loc, "%qE is not a constant expression when the "
			    "class %qT is still incomplete", op,
			    PTRMEM_CST_CLASS (op));
		*non_constant_p = true;
		return t;
	      }
	  }

	if (TREE_CODE (op) == PTRMEM_CST && tcode == NOP_EXPR)
	  {
	    if (!same_type_ignoring_top_level_qualifiers_p (type, TREE_TYPE (op))
		&& !can_convert_qual (type, op))
	      op = cplus_expand_constant (op);
	    return cp_fold_convert (type, op);
	  }

	if (INDIRECT_TYPE_P (type) && TREE_CODE (op) == INTEGER_CST)
	  {
	    if (integer_zerop (op))
	      {
		if (TYPE_REF_P (type))
		  {
		    if (!ctx->quiet)
		      error_at (loc, "dereferencing a null pointer");
		    *non_constant_p = true;
		    return t;
		  }
	      }
	    else if (TYPE_PTR_P (type)
		    && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)
	      /* INTEGER_CST with pointer-to-method type is only used
		 for a virtual method in a pointer to member function.
		 Don't reject those.  */
	      ;
	    else
	      {
		/* This detects for example:
		     reinterpret_cast<void*>(sizeof 0)
		*/
		if (!ctx->quiet)
		  error_at (loc, "%<reinterpret_cast<%T>(%E)%> is not "
			    "a constant expression",
			    type, op);
		*non_constant_p = true;
		return t;
	      }
	  }

	if (INDIRECT_TYPE_P (type)
	    && TREE_CODE (op) == NOP_EXPR
	    && TREE_TYPE (op) == ptr_type_node
	    && TREE_CODE (TREE_OPERAND (op, 0)) == ADDR_EXPR
	    && VAR_P (TREE_OPERAND (TREE_OPERAND (op, 0), 0))
	    && (DECL_NAME (TREE_OPERAND (TREE_OPERAND (op, 0),
					 0)) == heap_uninit_identifier
		|| DECL_NAME (TREE_OPERAND (TREE_OPERAND (op, 0),
					    0)) == heap_vec_uninit_identifier))
	  {
	    tree var = TREE_OPERAND (TREE_OPERAND (op, 0), 0);
	    tree var_size = TYPE_SIZE_UNIT (TREE_TYPE (var));
	    tree elt_type = TREE_TYPE (type);
	    tree cookie_size = NULL_TREE;
	    tree arg_size = NULL_TREE;
	    if (TREE_CODE (elt_type) == RECORD_TYPE
		&& TYPE_NAME (elt_type) == heap_identifier)
	      {
		tree fld1 = TYPE_FIELDS (elt_type);
		tree fld2 = DECL_CHAIN (fld1);
		elt_type = TREE_TYPE (TREE_TYPE (fld2));
		cookie_size = TYPE_SIZE_UNIT (TREE_TYPE (fld1));
	      }
	    DECL_NAME (var)
	      = (DECL_NAME (var) == heap_uninit_identifier
		 ? heap_identifier : heap_vec_identifier);
	    /* For zero sized elt_type, try to recover how many outer_nelts
	       it should have.  */
	    if ((cookie_size ? tree_int_cst_equal (var_size, cookie_size)
			     : integer_zerop (var_size))
		&& !int_size_in_bytes (elt_type)
		&& TREE_CODE (oldop) == CALL_EXPR
		&& call_expr_nargs (oldop) >= 1)
	      if (tree fun = get_function_named_in_call (oldop))
		if (cxx_replaceable_global_alloc_fn (fun)
		    && IDENTIFIER_NEW_OP_P (DECL_NAME (fun)))
		  arg_size = CALL_EXPR_ARG (oldop, 0);
	    TREE_TYPE (var)
	      = build_new_constexpr_heap_type (ctx, elt_type, cookie_size,
					       var_size, arg_size,
					       non_constant_p, overflow_p);
	    TREE_TYPE (TREE_OPERAND (op, 0))
	      = build_pointer_type (TREE_TYPE (var));
	  }

	if (op == oldop && tcode != UNARY_PLUS_EXPR)
	  /* We didn't fold at the top so we could check for ptr-int
	     conversion.  */
	  return fold (t);

	tree sop;

	/* Handle an array's bounds having been deduced after we built
	   the wrapping expression.  */
	if (same_type_ignoring_tlq_and_bounds_p (type, TREE_TYPE (op)))
	  r = op;
	else if (sop = tree_strip_nop_conversions (op),
		 sop != op && (same_type_ignoring_tlq_and_bounds_p
			       (type, TREE_TYPE (sop))))
	  r = sop;
	else if (tcode == UNARY_PLUS_EXPR)
	  r = fold_convert (TREE_TYPE (t), op);
	else
	  r = fold_build1 (tcode, type, op);

	/* Conversion of an out-of-range value has implementation-defined
	   behavior; the language considers it different from arithmetic
	   overflow, which is undefined.  */
	if (TREE_OVERFLOW_P (r) && !TREE_OVERFLOW_P (op))
	  TREE_OVERFLOW (r) = false;
      }
      break;

    case EXCESS_PRECISION_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);

	tree op = cxx_eval_constant_expression (ctx, oldop,
						lval,
						non_constant_p, overflow_p);
	if (*non_constant_p)
	  return t;
	r = fold_convert (TREE_TYPE (t), op);
	break;
      }

    case EMPTY_CLASS_EXPR:
      /* Handle EMPTY_CLASS_EXPR produced by build_call_a by lowering
	 it to an appropriate CONSTRUCTOR.  */
      return build_constructor (TREE_TYPE (t), NULL);

    case STATEMENT_LIST:
      new_ctx = *ctx;
      new_ctx.ctor = new_ctx.object = NULL_TREE;
      return cxx_eval_statement_list (&new_ctx, t,
				      non_constant_p, overflow_p, jump_target);

    case BIND_EXPR:
      /* Pre-emptively clear the vars declared by this BIND_EXPR from the value
	 map, so that when checking whether they're already destroyed later we
	 don't get confused by remnants of previous calls.  */
      for (tree decl = BIND_EXPR_VARS (t); decl; decl = DECL_CHAIN (decl))
	ctx->global->clear_value (decl);
      r = cxx_eval_constant_expression (ctx, BIND_EXPR_BODY (t),
					lval,
					non_constant_p, overflow_p,
					jump_target);
      for (tree decl = BIND_EXPR_VARS (t); decl; decl = DECL_CHAIN (decl))
	destroy_value_checked (ctx, decl, non_constant_p);
      break;

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      return cxx_eval_increment_expression (ctx, t,
					    lval, non_constant_p, overflow_p);

    case LAMBDA_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case MODOP_EXPR:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case BASELINK:
    case OFFSET_REF:
      if (!ctx->quiet)
	error_at (loc, "expression %qE is not a constant expression", t);
      *non_constant_p = true;
      break;

    case OBJ_TYPE_REF:
      /* Virtual function lookup.  We don't need to do anything fancy.  */
      return cxx_eval_constant_expression (ctx, OBJ_TYPE_REF_EXPR (t),
					   lval, non_constant_p, overflow_p);

    case PLACEHOLDER_EXPR:
      /* Use of the value or address of the current object.  */
      if (tree ctor = lookup_placeholder (ctx, lval, TREE_TYPE (t)))
	{
	  if (TREE_CODE (ctor) == CONSTRUCTOR)
	    return ctor;
	  else
	    return cxx_eval_constant_expression (ctx, ctor, lval,
						 non_constant_p, overflow_p);
	}
      /* A placeholder without a referent.  We can get here when
	 checking whether NSDMIs are noexcept, or in massage_init_elt;
	 just say it's non-constant for now.  */
      gcc_assert (ctx->quiet);
      *non_constant_p = true;
      break;

    case EXIT_EXPR:
      {
	tree cond = TREE_OPERAND (t, 0);
	cond = cxx_eval_constant_expression (ctx, cond, vc_prvalue,
					     non_constant_p, overflow_p);
	VERIFY_CONSTANT (cond);
	if (integer_nonzerop (cond))
	  *jump_target = t;
      }
      break;

    case GOTO_EXPR:
      if (breaks (&TREE_OPERAND (t, 0))
	  || continues (&TREE_OPERAND (t, 0)))
	*jump_target = TREE_OPERAND (t, 0);
      else
	{
	  gcc_assert (cxx_dialect >= cxx23);
	  if (!ctx->quiet)
	    error_at (loc, "%<goto%> is not a constant expression");
	  *non_constant_p = true;
	}
      break;

    case LOOP_EXPR:
    case DO_STMT:
    case WHILE_STMT:
    case FOR_STMT:
      cxx_eval_loop_expr (ctx, t,
			  non_constant_p, overflow_p, jump_target);
      break;

    case SWITCH_EXPR:
    case SWITCH_STMT:
      cxx_eval_switch_expr (ctx, t,
			    non_constant_p, overflow_p, jump_target);
      break;

    case REQUIRES_EXPR:
      /* It's possible to get a requires-expression in a constant
         expression. For example:

             template<typename T> concept bool C() {
               return requires (T t) { t; };
             }

             template<typename T> requires !C<T>() void f(T);

         Normalization leaves f with the associated constraint
         '!requires (T t) { ... }' which is not transformed into
         a constraint.  */
      if (!processing_template_decl)
	return evaluate_requires_expr (t);
      else
        *non_constant_p = true;
      return t;

    case ANNOTATE_EXPR:
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					lval,
					non_constant_p, overflow_p,
					jump_target);
      break;

    case USING_STMT:
      r = void_node;
      break;

    case ASSERTION_STMT:
    case PRECONDITION_STMT:
    case POSTCONDITION_STMT:
      {
	contract_semantic semantic = get_contract_semantic (t);
	if (semantic == CCS_IGNORE)
	  break;

	if (!cxx_eval_assert (ctx, CONTRACT_CONDITION (t),
			      G_("contract predicate is false in "
				 "constant expression"),
			      EXPR_LOCATION (t), checked_contract_p (semantic),
			      non_constant_p, overflow_p))
	  *non_constant_p = true;
	r = void_node;
      }
      break;

    case TEMPLATE_ID_EXPR:
      {
        /* We can evaluate template-id that refers to a concept only if
	   the template arguments are non-dependent.  */
	gcc_assert (concept_check_p (t));

	if (!value_dependent_expression_p (t)
	    && !uid_sensitive_constexpr_evaluation_p ())
	  r = evaluate_concept_check (t);
	else
	  *non_constant_p = true;

	break;
      }

    case ASM_EXPR:
      if (!ctx->quiet)
	inline_asm_in_constexpr_error (loc, /*constexpr_fundef_p*/false);
      *non_constant_p = true;
      return t;

    case BIT_CAST_EXPR:
      if (lval)
	{
	  if (!ctx->quiet)
	    error_at (EXPR_LOCATION (t),
		      "address of a call to %qs is not a constant expression",
		      "__builtin_bit_cast");
	  *non_constant_p = true;
	  return t;
	}
      r = cxx_eval_bit_cast (ctx, t, non_constant_p, overflow_p);
      break;

    case OMP_PARALLEL:
    case OMP_TASK:
    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OMP_TASKLOOP:
    case OMP_LOOP:
    case OMP_TEAMS:
    case OMP_TARGET_DATA:
    case OMP_TARGET:
    case OMP_SECTIONS:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_SINGLE:
    case OMP_SCAN:
    case OMP_SCOPE:
    case OMP_SECTION:
    case OMP_STRUCTURED_BLOCK:
    case OMP_MASTER:
    case OMP_MASKED:
    case OMP_TASKGROUP:
    case OMP_TARGET_UPDATE:
    case OMP_TARGET_ENTER_DATA:
    case OMP_TARGET_EXIT_DATA:
    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
    case OMP_DEPOBJ:
    case OACC_PARALLEL:
    case OACC_KERNELS:
    case OACC_SERIAL:
    case OACC_DATA:
    case OACC_HOST_DATA:
    case OACC_LOOP:
    case OACC_CACHE:
    case OACC_DECLARE:
    case OACC_ENTER_DATA:
    case OACC_EXIT_DATA:
    case OACC_UPDATE:
      if (!ctx->quiet)
	error_at (EXPR_LOCATION (t),
		  "statement is not a constant expression");
      *non_constant_p = true;
      break;

    default:
      if (STATEMENT_CODE_P (TREE_CODE (t)))
	{
	  /* This function doesn't know how to deal with pre-genericize
	     statements; this can only happen with statement-expressions,
	     so for now just fail.  */
	  if (!ctx->quiet)
	    error_at (EXPR_LOCATION (t),
		      "statement is not a constant expression");
	}
      else if (flag_checking)
	internal_error ("unexpected expression %qE of kind %s", t,
			get_tree_code_name (TREE_CODE (t)));
      *non_constant_p = true;
      break;
    }

  if (r == error_mark_node)
    *non_constant_p = true;

  if (*non_constant_p)
    return t;
  else
    return r;
}

/* P0859: A function is needed for constant evaluation if it is a constexpr
   function that is named by an expression ([basic.def.odr]) that is
   potentially constant evaluated.

   So we need to instantiate any constexpr functions mentioned by the
   expression even if the definition isn't needed for evaluating the
   expression.  */

static tree
instantiate_cx_fn_r (tree *tp, int *walk_subtrees, void */*data*/)
{
  if (TREE_CODE (*tp) == FUNCTION_DECL
      && DECL_DECLARED_CONSTEXPR_P (*tp)
      && !DECL_INITIAL (*tp)
      && !trivial_fn_p (*tp)
      && (DECL_TEMPLOID_INSTANTIATION (*tp) || DECL_DEFAULTED_FN (*tp))
      && !uid_sensitive_constexpr_evaluation_p ())
    {
      ++function_depth;
      if (DECL_TEMPLOID_INSTANTIATION (*tp))
	instantiate_decl (*tp, /*defer_ok*/false, /*expl_inst*/false);
      else
	synthesize_method (*tp);
      --function_depth;
    }
  else if (TREE_CODE (*tp) == CALL_EXPR
	   || TREE_CODE (*tp) == AGGR_INIT_EXPR)
    {
      if (EXPR_HAS_LOCATION (*tp))
	input_location = EXPR_LOCATION (*tp);
    }

  if (!EXPR_P (*tp))
    *walk_subtrees = 0;

  return NULL_TREE;
}

static void
instantiate_constexpr_fns (tree t)
{
  location_t loc = input_location;
  cp_walk_tree_without_duplicates (&t, instantiate_cx_fn_r, NULL);
  input_location = loc;
}

/* Look for heap variables in the expression *TP.  */

static tree
find_heap_var_refs (tree *tp, int *walk_subtrees, void */*data*/)
{
  if (VAR_P (*tp)
      && (DECL_NAME (*tp) == heap_uninit_identifier
	  || DECL_NAME (*tp) == heap_identifier
	  || DECL_NAME (*tp) == heap_vec_uninit_identifier
	  || DECL_NAME (*tp) == heap_vec_identifier
	  || DECL_NAME (*tp) == heap_deleted_identifier))
    return *tp;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Find immediate function decls in *TP if any.  */

static tree
find_immediate_fndecl (tree *tp, int */*walk_subtrees*/, void */*data*/)
{
  if (TREE_CODE (*tp) == FUNCTION_DECL && DECL_IMMEDIATE_FUNCTION_P (*tp))
    return *tp;
  if (TREE_CODE (*tp) == PTRMEM_CST
      && TREE_CODE (PTRMEM_CST_MEMBER (*tp)) == FUNCTION_DECL
      && DECL_IMMEDIATE_FUNCTION_P (PTRMEM_CST_MEMBER (*tp)))
    return PTRMEM_CST_MEMBER (*tp);
  return NULL_TREE;
}

/* T has TREE_CONSTANT set but has been deemed not a valid C++ constant
   expression.  Return a version of T that has TREE_CONSTANT cleared.  */

static tree
mark_non_constant (tree t)
{
  gcc_checking_assert (TREE_CONSTANT (t));

  /* This isn't actually constant, so unset TREE_CONSTANT.
     Don't clear TREE_CONSTANT on ADDR_EXPR, as the middle-end requires
     it to be set if it is invariant address, even when it is not
     a valid C++ constant expression.  Wrap it with a NOP_EXPR
     instead.  */
  if (EXPR_P (t) && TREE_CODE (t) != ADDR_EXPR)
    t = copy_node (t);
  else if (TREE_CODE (t) == CONSTRUCTOR)
    t = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (t), t);
  else
    t = build_nop (TREE_TYPE (t), t);
  TREE_CONSTANT (t) = false;
  return t;
}

/* ALLOW_NON_CONSTANT is false if T is required to be a constant expression.
   STRICT has the same sense as for constant_value_1: true if we only allow
   conforming C++ constant expressions, or false if we want a constant value
   even if it doesn't conform.
   MANIFESTLY_CONST_EVAL is true if T is manifestly const-evaluated as
   per P0595 even when ALLOW_NON_CONSTANT is true.
   CONSTEXPR_DTOR is true when evaluating the dtor of a constexpr variable.
   OBJECT must be non-NULL in that case.  */

static tree
cxx_eval_outermost_constant_expr (tree t, bool allow_non_constant,
				  bool strict = true,
				  mce_value manifestly_const_eval = mce_unknown,
				  bool constexpr_dtor = false,
				  tree object = NULL_TREE)
{
  auto_timevar time (TV_CONSTEXPR);

  bool non_constant_p = false;
  bool overflow_p = false;

  if (BRACE_ENCLOSED_INITIALIZER_P (t))
    {
      gcc_checking_assert (allow_non_constant);
      return t;
    }

  constexpr_global_ctx global_ctx;
  constexpr_ctx ctx = { &global_ctx, NULL, NULL, NULL, NULL, NULL, NULL,
			allow_non_constant, strict,
			!allow_non_constant ? mce_true : manifestly_const_eval };

  /* Turn off -frounding-math for manifestly constant evaluation.  */
  warning_sentinel rm (flag_rounding_math,
		       ctx.manifestly_const_eval == mce_true);
  tree type = (object
	       ? cv_unqualified (TREE_TYPE (object))
	       : initialized_type (t));
  tree r = t;
  bool is_consteval = false;
  if (VOID_TYPE_P (type))
    {
      if (!constexpr_dtor)
	{
	  if (cxx_dialect < cxx20)
	    return t;
	  if (TREE_CODE (t) != CALL_EXPR && TREE_CODE (t) != AGGR_INIT_EXPR)
	    return t;
	  /* Calls to immediate functions returning void need to be
	     evaluated.  */
	  tree fndecl = cp_get_callee_fndecl_nofold (t);
	  if (fndecl == NULL_TREE || !DECL_IMMEDIATE_FUNCTION_P (fndecl))
	    return t;
	  else
	    is_consteval = true;
	}
    }
  else if (cxx_dialect >= cxx20
	   && (TREE_CODE (t) == CALL_EXPR
	       || TREE_CODE (t) == AGGR_INIT_EXPR
	       || TREE_CODE (t) == TARGET_EXPR))
    {
      tree x = t;
      if (TREE_CODE (x) == TARGET_EXPR)
	x = TARGET_EXPR_INITIAL (x);
      tree fndecl = cp_get_callee_fndecl_nofold (x);
      if (fndecl && DECL_IMMEDIATE_FUNCTION_P (fndecl))
	is_consteval = true;
    }
  if (AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type))
    {
      /* In C++14 an NSDMI can participate in aggregate initialization,
	 and can refer to the address of the object being initialized, so
	 we need to pass in the relevant VAR_DECL if we want to do the
	 evaluation in a single pass.  The evaluation will dynamically
	 update ctx.values for the VAR_DECL.  We use the same strategy
	 for C++11 constexpr constructors that refer to the object being
	 initialized.  */
      if (constexpr_dtor)
	{
	  gcc_assert (object && VAR_P (object));
	  gcc_assert (DECL_DECLARED_CONSTEXPR_P (object));
	  gcc_assert (DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (object));
	  if (error_operand_p (DECL_INITIAL (object)))
	    return t;
	  ctx.ctor = unshare_expr (DECL_INITIAL (object));
	  TREE_READONLY (ctx.ctor) = false;
	  /* Temporarily force decl_really_constant_value to return false
	     for it, we want to use ctx.ctor for the current value instead.  */
	  DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (object) = false;
	}
      else
	{
	  ctx.ctor = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_CLEARING (ctx.ctor) = true;
	}
      if (!object)
	{
	  if (TREE_CODE (t) == CALL_EXPR)
	    {
	      /* If T is calling a constructor to initialize an object, reframe
		 it as an AGGR_INIT_EXPR to avoid trying to modify an object
		 from outside the constant evaluation, which will fail even if
		 the value is actually constant (is_constant_evaluated3.C).  */
	      tree fn = cp_get_callee_fndecl_nofold (t);
	      if (fn && DECL_CONSTRUCTOR_P (fn))
		{
		  object = CALL_EXPR_ARG (t, 0);
		  object = build_fold_indirect_ref (object);
		  r = build_aggr_init_expr (type, r);
		}
	    }
	  else if (TREE_CODE (t) == TARGET_EXPR)
	    object = TARGET_EXPR_SLOT (t);
	  else if (TREE_CODE (t) == AGGR_INIT_EXPR)
	    object = AGGR_INIT_EXPR_SLOT (t);
	}
      ctx.object = object;
      if (object)
	gcc_assert (same_type_ignoring_top_level_qualifiers_p
		    (type, TREE_TYPE (object)));
      if (object && DECL_P (object))
	global_ctx.put_value (object, ctx.ctor);
      if (TREE_CODE (r) == TARGET_EXPR)
	/* Avoid creating another CONSTRUCTOR when we expand the
	   TARGET_EXPR.  */
	r = TARGET_EXPR_INITIAL (r);
    }

  auto_vec<tree, 16> cleanups;
  global_ctx.cleanups = &cleanups;

  if (manifestly_const_eval == mce_true)
    instantiate_constexpr_fns (r);
  r = cxx_eval_constant_expression (&ctx, r, vc_prvalue,
				    &non_constant_p, &overflow_p);

  if (!constexpr_dtor)
    verify_constant (r, allow_non_constant, &non_constant_p, &overflow_p);
  else
    DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (object) = true;

  unsigned int i;
  tree cleanup;
  /* Evaluate the cleanups.  */
  FOR_EACH_VEC_ELT_REVERSE (cleanups, i, cleanup)
    cxx_eval_constant_expression (&ctx, cleanup, vc_discard,
				  &non_constant_p, &overflow_p);

  /* Mutable logic is a bit tricky: we want to allow initialization of
     constexpr variables with mutable members, but we can't copy those
     members to another constexpr variable.  */
  if (TREE_CODE (r) == CONSTRUCTOR && CONSTRUCTOR_MUTABLE_POISON (r))
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression because it refers to "
	       "mutable subobjects of %qT", t, type);
      non_constant_p = true;
    }

  if (TREE_CODE (r) == CONSTRUCTOR && CONSTRUCTOR_NO_CLEARING (r))
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression because it refers to "
	       "an incompletely initialized variable", t);
      TREE_CONSTANT (r) = false;
      non_constant_p = true;
    }

  if (!non_constant_p && cxx_dialect >= cxx20
      && !global_ctx.heap_vars.is_empty ())
    {
      tree heap_var = cp_walk_tree_without_duplicates (&r, find_heap_var_refs,
						       NULL);
      unsigned int i;
      if (heap_var)
	{
	  if (!allow_non_constant && !non_constant_p)
	    error_at (DECL_SOURCE_LOCATION (heap_var),
		      "%qE is not a constant expression because it refers to "
		      "a result of %<operator new%>", t);
	  r = t;
	  non_constant_p = true;
	}
      FOR_EACH_VEC_ELT (global_ctx.heap_vars, i, heap_var)
	{
	  if (DECL_NAME (heap_var) != heap_deleted_identifier)
	    {
	      if (!allow_non_constant && !non_constant_p)
		error_at (DECL_SOURCE_LOCATION (heap_var),
			  "%qE is not a constant expression because allocated "
			  "storage has not been deallocated", t);
	      r = t;
	      non_constant_p = true;
	    }
	  varpool_node::get (heap_var)->remove ();
	}
    }

  /* Check that immediate invocation does not return an expression referencing
     any immediate function decls.  */
  if (!non_constant_p && cxx_dialect >= cxx20)
    if (tree immediate_fndecl
	= cp_walk_tree_without_duplicates (&r, find_immediate_fndecl,
					   NULL))
    {
      if (!allow_non_constant && !non_constant_p)
	{
	  if (is_consteval)
	    error_at (cp_expr_loc_or_input_loc (t),
		      "immediate evaluation returns address of immediate "
		      "function %qD", immediate_fndecl);
	  else
	    error_at (cp_expr_loc_or_input_loc (t),
		      "constant evaluation returns address of immediate "
		      "function %qD", immediate_fndecl);
	}
      r = t;
      non_constant_p = true;
    }

  if (non_constant_p)
    /* If we saw something bad, go back to our argument.  The wrapping below is
       only for the cases of TREE_CONSTANT argument or overflow.  */
    r = t;

  if (!non_constant_p && overflow_p)
    non_constant_p = true;

  /* Unshare the result.  */
  bool should_unshare = true;
  if (r == t || (TREE_CODE (t) == TARGET_EXPR
		 && TARGET_EXPR_INITIAL (t) == r))
    should_unshare = false;

  if (non_constant_p && !allow_non_constant)
    return error_mark_node;
  else if (constexpr_dtor)
    return r;
  else if (non_constant_p && TREE_CONSTANT (r))
    r = mark_non_constant (r);
  else if (non_constant_p)
    return t;

  if (should_unshare)
    r = unshare_expr (r);

  if (TREE_CODE (r) == CONSTRUCTOR && CLASS_TYPE_P (TREE_TYPE (r)))
    {
      r = adjust_temp_type (type, r);
      if (TREE_CODE (t) == TARGET_EXPR
	  && TARGET_EXPR_INITIAL (t) == r)
	return t;
      else if (TREE_CODE (t) == CONSTRUCTOR || TREE_CODE (t) == CALL_EXPR)
	/* Don't add a TARGET_EXPR if our argument didn't have one.  */;
      else if (TREE_CODE (t) == TARGET_EXPR && TARGET_EXPR_CLEANUP (t))
	r = get_target_expr (r);
      else
	{
	  r = get_target_expr (r, tf_warning_or_error | tf_no_cleanup);
	  TREE_CONSTANT (r) = true;
	}
    }

  if (TREE_CODE (t) == TARGET_EXPR
      && TREE_CODE (r) == TARGET_EXPR)
    {
      /* Preserve this flag for potential_constant_expression, and the others
	 for good measure.  */
      TARGET_EXPR_ELIDING_P (r) = TARGET_EXPR_ELIDING_P (t);
      TARGET_EXPR_IMPLICIT_P (r) = TARGET_EXPR_IMPLICIT_P (t);
      TARGET_EXPR_LIST_INIT_P (r) = TARGET_EXPR_LIST_INIT_P (t);
      TARGET_EXPR_DIRECT_INIT_P (r) = TARGET_EXPR_DIRECT_INIT_P (t);
    }

  /* Remember the original location if that wouldn't need a wrapper.  */
  if (location_t loc = EXPR_LOCATION (t))
    protected_set_expr_location (r, loc);

  return r;
}

/* If T represents a constant expression returns its reduced value.
   Otherwise return error_mark_node.  */

tree
cxx_constant_value (tree t, tree decl /* = NULL_TREE */,
		    tsubst_flags_t complain /* = tf_error */)
{
  bool sfinae = !(complain & tf_error);
  tree r = cxx_eval_outermost_constant_expr (t, sfinae, true, mce_true, false, decl);
  if (sfinae && !TREE_CONSTANT (r))
    r = error_mark_node;
  return r;
}

/* Like cxx_constant_value, but used for evaluation of constexpr destructors
   of constexpr variables.  The actual initializer of DECL is not modified.  */

void
cxx_constant_dtor (tree t, tree decl)
{
  cxx_eval_outermost_constant_expr (t, false, true, mce_true, true, decl);
}

/* Helper routine for fold_simple function.  Either return simplified
   expression T, otherwise NULL_TREE.
   In contrast to cp_fully_fold, and to maybe_constant_value, we try to fold
   even if we are within template-declaration.  So be careful on call, as in
   such case types can be undefined.  */

static tree
fold_simple_1 (tree t)
{
  tree op1;
  enum tree_code code = TREE_CODE (t);

  switch (code)
    {
    case INTEGER_CST:
    case REAL_CST:
    case VECTOR_CST:
    case FIXED_CST:
    case COMPLEX_CST:
      return t;

    case SIZEOF_EXPR:
      return fold_sizeof_expr (t);

    case ABS_EXPR:
    case ABSU_EXPR:
    case CONJ_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case VIEW_CONVERT_EXPR:
    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIXED_CONVERT_EXPR:
    case ADDR_SPACE_CONVERT_EXPR:

      op1 = TREE_OPERAND (t, 0);

      t = const_unop (code, TREE_TYPE (t), op1);
      if (!t)
	return NULL_TREE;

      if (CONVERT_EXPR_CODE_P (code)
	  && TREE_OVERFLOW_P (t) && !TREE_OVERFLOW_P (op1))
	TREE_OVERFLOW (t) = false;
      return t;

    default:
      return NULL_TREE;
    }
}

/* If T is a simple constant expression, returns its simplified value.
   Otherwise returns T.  In contrast to maybe_constant_value we
   simplify only few operations on constant-expressions, and we don't
   try to simplify constexpressions.  */

tree
fold_simple (tree t)
{
  if (processing_template_decl)
    return t;

  tree r = fold_simple_1 (t);
  if (r)
    return r;

  return t;
}

/* Try folding the expression T to a simple constant.
   Returns that constant, otherwise returns T.  */

tree
fold_to_constant (tree t)
{
  tree r = fold (t);
  if (CONSTANT_CLASS_P (r) && !TREE_OVERFLOW (r))
    return r;
  else
    return t;
}

/* If T is a constant expression, returns its reduced value.
   Otherwise, if T does not have TREE_CONSTANT set, returns T.
   Otherwise, returns a version of T without TREE_CONSTANT.
   MANIFESTLY_CONST_EVAL is true if T is manifestly const-evaluated
   as per P0595.  */

static GTY((deletable)) hash_map<tree, tree> *cv_cache;

tree
maybe_constant_value (tree t, tree decl /* = NULL_TREE */,
		      mce_value manifestly_const_eval /* = mce_unknown */)
{
  tree r;

  if (!is_nondependent_constant_expression (t))
    {
      if (TREE_OVERFLOW_P (t)
	  || (!processing_template_decl && TREE_CONSTANT (t)))
	t = mark_non_constant (t);
      return t;
    }
  else if (CONSTANT_CLASS_P (t))
    /* No caching or evaluation needed.  */
    return t;

  /* Don't constant evaluate an unevaluated non-manifestly-constant operand,
     but at least try folding it to a simple constant.  */
  if (cp_unevaluated_operand && manifestly_const_eval != mce_true)
    return fold_to_constant (t);

  if (manifestly_const_eval != mce_unknown)
    return cxx_eval_outermost_constant_expr (t, true, true,
					     manifestly_const_eval, false, decl);

  if (cv_cache == NULL)
    cv_cache = hash_map<tree, tree>::create_ggc (101);
  if (tree *cached = cv_cache->get (t))
    {
      r = *cached;
      if (r != t)
	{
	  /* Clear processing_template_decl for sake of break_out_target_exprs;
	     entries in the cv_cache are non-templated.  */
	  processing_template_decl_sentinel ptds;

	  r = break_out_target_exprs (r, /*clear_loc*/true);
	  protected_set_expr_location (r, EXPR_LOCATION (t));
	}
      return r;
    }

  uid_sensitive_constexpr_evaluation_checker c;
  r = cxx_eval_outermost_constant_expr (t, true, true,
					manifestly_const_eval, false, decl);
  gcc_checking_assert (r == t
		       || CONVERT_EXPR_P (t)
		       || TREE_CODE (t) == VIEW_CONVERT_EXPR
		       || (TREE_CONSTANT (t) && !TREE_CONSTANT (r))
		       || !cp_tree_equal (r, t));
  if (!c.evaluation_restricted_p ())
    cv_cache->put (t, r);
  return r;
}

/* Dispose of the whole CV_CACHE.  */

static void
clear_cv_cache (void)
{
  if (cv_cache != NULL)
    cv_cache->empty ();
}

/* Dispose of the whole CV_CACHE and FOLD_CACHE.  */

void
clear_cv_and_fold_caches ()
{
  clear_cv_cache ();
  clear_fold_cache ();
}

/* Internal function handling expressions in templates for
   fold_non_dependent_expr and fold_non_dependent_init.

   If we're in a template, but T isn't value dependent, simplify
   it.  We're supposed to treat:

     template <typename T> void f(T[1 + 1]);
     template <typename T> void f(T[2]);

   as two declarations of the same function, for example.  */

static tree
fold_non_dependent_expr_template (tree t, tsubst_flags_t complain,
				  bool manifestly_const_eval,
				  tree object)
{
  gcc_assert (processing_template_decl);

  if (is_nondependent_constant_expression (t))
    {
      processing_template_decl_sentinel s;
      t = instantiate_non_dependent_expr_internal (t, complain);

      if (type_unknown_p (t) || BRACE_ENCLOSED_INITIALIZER_P (t))
	{
	  if (TREE_OVERFLOW_P (t))
	    {
	      t = build_nop (TREE_TYPE (t), t);
	      TREE_CONSTANT (t) = false;
	    }
	  return t;
	}
      else if (CONSTANT_CLASS_P (t))
	/* No evaluation needed.  */
	return t;

      /* Don't constant evaluate an unevaluated non-manifestly-constant operand,
	 but at least try folding it to a simple constant.  */
      if (cp_unevaluated_operand && !manifestly_const_eval)
	return fold_to_constant (t);

      tree r = cxx_eval_outermost_constant_expr (t, true, true,
						 mce_value (manifestly_const_eval),
						 false, object);
      /* cp_tree_equal looks through NOPs, so allow them.  */
      gcc_checking_assert (r == t
			   || CONVERT_EXPR_P (t)
			   || TREE_CODE (t) == VIEW_CONVERT_EXPR
			   || (TREE_CONSTANT (t) && !TREE_CONSTANT (r))
			   || !cp_tree_equal (r, t));
      return r;
    }
  else if (TREE_OVERFLOW_P (t))
    {
      t = build_nop (TREE_TYPE (t), t);
      TREE_CONSTANT (t) = false;
    }

  return t;
}

/* Like maybe_constant_value but first fully instantiate the argument.

   Note: this is equivalent to instantiate_non_dependent_expr (t, complain)
   followed by maybe_constant_value but is more efficient,
   because it calls instantiation_dependent_expression_p and
   potential_constant_expression at most once.
   The manifestly_const_eval argument is passed to maybe_constant_value.

   Callers should generally pass their active complain, or if they are in a
   non-template, diagnosing context, they can use the default of
   tf_warning_or_error.  Callers that might be within a template context, don't
   have a complain parameter, and aren't going to remember the result for long
   (e.g. null_ptr_cst_p), can pass tf_none and deal with error_mark_node
   appropriately.  */

tree
fold_non_dependent_expr (tree t,
			 tsubst_flags_t complain /* = tf_warning_or_error */,
			 bool manifestly_const_eval /* = false */,
			 tree object /* = NULL_TREE */)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  if (processing_template_decl)
    return fold_non_dependent_expr_template (t, complain,
					     manifestly_const_eval, object);

  return maybe_constant_value (t, object, mce_value (manifestly_const_eval));
}

/* Like fold_non_dependent_expr, but if EXPR couldn't be folded to a constant,
   return the original expression.  */

tree
maybe_fold_non_dependent_expr (tree expr,
			       tsubst_flags_t complain/*=tf_warning_or_error*/)
{
  tree t = fold_non_dependent_expr (expr, complain);
  if (t && TREE_CONSTANT (t))
    return t;

  return expr;
}

/* Like maybe_constant_init but first fully instantiate the argument.  */

tree
fold_non_dependent_init (tree t,
			 tsubst_flags_t complain /*=tf_warning_or_error*/,
			 bool manifestly_const_eval /*=false*/,
			 tree object /* = NULL_TREE */)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  if (processing_template_decl)
    {
      t = fold_non_dependent_expr_template (t, complain,
					    manifestly_const_eval, object);
      /* maybe_constant_init does this stripping, so do it here too.  */
      if (TREE_CODE (t) == TARGET_EXPR)
	{
	  tree init = TARGET_EXPR_INITIAL (t);
	  if (TREE_CODE (init) == CONSTRUCTOR)
	    t = init;
	}
      return t;
    }

  return maybe_constant_init (t, object, manifestly_const_eval);
}

/* Like maybe_constant_value, but returns a CONSTRUCTOR directly, rather
   than wrapped in a TARGET_EXPR.
   ALLOW_NON_CONSTANT is false if T is required to be a constant expression.
   MANIFESTLY_CONST_EVAL is true if T is manifestly const-evaluated as
   per P0595 even when ALLOW_NON_CONSTANT is true.  */

static tree
maybe_constant_init_1 (tree t, tree decl, bool allow_non_constant,
		       bool manifestly_const_eval)
{
  if (!t)
    return t;
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == CONVERT_EXPR
      && VOID_TYPE_P (TREE_TYPE (t)))
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == INIT_EXPR)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == TARGET_EXPR)
    t = TARGET_EXPR_INITIAL (t);
  if (!is_nondependent_static_init_expression (t))
    /* Don't try to evaluate it.  */;
  else if (CONSTANT_CLASS_P (t) && TREE_CODE (t) != PTRMEM_CST)
    /* No evaluation needed.  PTRMEM_CST needs the immediate fn check.  */;
  else
    {
      /* [basic.start.static] allows constant-initialization of variables with
	 static or thread storage duration even if it isn't required, but we
	 shouldn't bend the rules the same way for automatic variables.  */
      bool is_static = (decl && DECL_P (decl)
			&& (TREE_STATIC (decl) || DECL_EXTERNAL (decl)));
      if (is_static)
	manifestly_const_eval = true;

      if (cp_unevaluated_operand && !manifestly_const_eval)
	return fold_to_constant (t);

      t = cxx_eval_outermost_constant_expr (t, allow_non_constant, !is_static,
					    mce_value (manifestly_const_eval),
					    false, decl);
    }
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree init = TARGET_EXPR_INITIAL (t);
      if (TREE_CODE (init) == CONSTRUCTOR)
	t = init;
    }
  return t;
}

/* Wrapper for maybe_constant_init_1 which permits non constants.  */

tree
maybe_constant_init (tree t, tree decl, bool manifestly_const_eval)
{
  return maybe_constant_init_1 (t, decl, true, manifestly_const_eval);
}

/* Wrapper for maybe_constant_init_1 which does not permit non constants.  */

tree
cxx_constant_init (tree t, tree decl)
{
  return maybe_constant_init_1 (t, decl, false, true);
}

#if 0
/* FIXME see ADDR_EXPR section in potential_constant_expression_1.  */
/* Return true if the object referred to by REF has automatic or thread
   local storage.  */

enum { ck_ok, ck_bad, ck_unknown };
static int
check_automatic_or_tls (tree ref)
{
  machine_mode mode;
  poly_int64 bitsize, bitpos;
  tree offset;
  int volatilep = 0, unsignedp = 0;
  tree decl = get_inner_reference (ref, &bitsize, &bitpos, &offset,
				   &mode, &unsignedp, &volatilep, false);
  duration_kind dk;

  /* If there isn't a decl in the middle, we don't know the linkage here,
     and this isn't a constant expression anyway.  */
  if (!DECL_P (decl))
    return ck_unknown;
  dk = decl_storage_duration (decl);
  return (dk == dk_auto || dk == dk_thread) ? ck_bad : ck_ok;
}
#endif

/* Data structure for passing data from potential_constant_expression_1
   to check_for_return_continue via cp_walk_tree.  */
struct check_for_return_continue_data {
  hash_set<tree> *pset;
  tree continue_stmt;
  tree break_stmt;
};

/* Helper function for potential_constant_expression_1 SWITCH_STMT handling,
   called through cp_walk_tree.  Return the first RETURN_EXPR found, or note
   the first CONTINUE_STMT and/or BREAK_STMT if RETURN_EXPR is not found.  */
static tree
check_for_return_continue (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp, s, b;
  check_for_return_continue_data *d = (check_for_return_continue_data *) data;
  switch (TREE_CODE (t))
    {
    case RETURN_EXPR:
      return t;

    case CONTINUE_STMT:
      if (d->continue_stmt == NULL_TREE)
	d->continue_stmt = t;
      break;

    case BREAK_STMT:
      if (d->break_stmt == NULL_TREE)
	d->break_stmt = t;
      break;

#define RECUR(x) \
      if (tree r = cp_walk_tree (&x, check_for_return_continue, data,	\
				 d->pset))				\
	return r

      /* For loops, walk subtrees manually, so that continue stmts found
	 inside of the bodies of the loops are ignored.  */
    case DO_STMT:
      *walk_subtrees = 0;
      RECUR (DO_COND (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (DO_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case WHILE_STMT:
      *walk_subtrees = 0;
      RECUR (WHILE_COND (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (WHILE_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case FOR_STMT:
      *walk_subtrees = 0;
      RECUR (FOR_INIT_STMT (t));
      RECUR (FOR_COND (t));
      RECUR (FOR_EXPR (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (FOR_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case RANGE_FOR_STMT:
      *walk_subtrees = 0;
      RECUR (RANGE_FOR_EXPR (t));
      s = d->continue_stmt;
      b = d->break_stmt;
      RECUR (RANGE_FOR_BODY (t));
      d->continue_stmt = s;
      d->break_stmt = b;
      break;

    case SWITCH_STMT:
      *walk_subtrees = 0;
      RECUR (SWITCH_STMT_COND (t));
      b = d->break_stmt;
      RECUR (SWITCH_STMT_BODY (t));
      d->break_stmt = b;
      break;
#undef RECUR

    case STATEMENT_LIST:
    case CONSTRUCTOR:
      break;

    default:
      if (!EXPR_P (t))
	*walk_subtrees = 0;
      break;
    }

  return NULL_TREE;
}

/* Return true if T denotes a potentially constant expression.  Issue
   diagnostic as appropriate under control of FLAGS.  If WANT_RVAL is true,
   an lvalue-rvalue conversion is implied.  If NOW is true, we want to
   consider the expression in the current context, independent of constexpr
   substitution.  If FUNDEF_P is true, we're checking a constexpr function body
   and hard errors should not be reported by constexpr_error.

   C++0x [expr.const] used to say

   6 An expression is a potential constant expression if it is
     a constant expression where all occurrences of function
     parameters are replaced by arbitrary constant expressions
     of the appropriate type.

   2  A conditional expression is a constant expression unless it
      involves one of the following as a potentially evaluated
      subexpression (3.2), but subexpressions of logical AND (5.14),
      logical OR (5.15), and conditional (5.16) operations that are
      not evaluated are not considered.   */

static bool
potential_constant_expression_1 (tree t, bool want_rval, bool strict, bool now,
				 bool fundef_p, tsubst_flags_t flags,
				 tree *jump_target)
{
#define RECUR(T,RV) \
  potential_constant_expression_1 ((T), (RV), strict, now, fundef_p, flags, \
				   jump_target)

  enum { any = false, rval = true };
  int i;
  tree tmp;

  if (t == error_mark_node)
    return false;
  if (t == NULL_TREE)
    return true;
  location_t loc = cp_expr_loc_or_input_loc (t);

  if (*jump_target)
    /* If we are jumping, ignore everything.  This is simpler than the
       cxx_eval_constant_expression handling because we only need to be
       conservatively correct, and we don't necessarily have a constant value
       available, so we don't bother with switch tracking.  */
    return true;

  if (TREE_THIS_VOLATILE (t) && want_rval
      && !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (t)))
    {
      if (flags & tf_error)
	constexpr_error (loc, fundef_p, "lvalue-to-rvalue conversion of "
			 "a volatile lvalue %qE with type %qT", t,
			 TREE_TYPE (t));
      return false;
    }
  if (CONSTANT_CLASS_P (t))
    return true;
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPED)
      && TREE_TYPE (t) == error_mark_node)
    return false;

  switch (TREE_CODE (t))
    {
    case FUNCTION_DECL:
    case BASELINK:
    case TEMPLATE_DECL:
    case OVERLOAD:
    case TEMPLATE_ID_EXPR:
    case LABEL_DECL:
    case CASE_LABEL_EXPR:
    case PREDICT_EXPR:
    case CONST_DECL:
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case OFFSETOF_EXPR:
    case NOEXCEPT_EXPR:
    case TEMPLATE_PARM_INDEX:
    case TRAIT_EXPR:
    case IDENTIFIER_NODE:
    case USERDEF_LITERAL:
      /* We can see a FIELD_DECL in a pointer-to-member expression.  */
    case FIELD_DECL:
    case RESULT_DECL:
    case USING_DECL:
    case USING_STMT:
    case PLACEHOLDER_EXPR:
    case REQUIRES_EXPR:
    case STATIC_ASSERT:
    case DEBUG_BEGIN_STMT:
      return true;

    case RETURN_EXPR:
      if (!RECUR (TREE_OPERAND (t, 0), any))
	return false;
      /* FALLTHROUGH */

    case BREAK_STMT:
    case CONTINUE_STMT:
      *jump_target = t;
      return true;

    case PARM_DECL:
      if (now && want_rval)
	{
	  tree type = TREE_TYPE (t);
	  if (dependent_type_p (type)
	      || !COMPLETE_TYPE_P (processing_template_decl
				   ? type : complete_type (type))
	      || is_really_empty_class (type, /*ignore_vptr*/false))
	    /* An empty class has no data to read.  */
	    return true;
	  if (flags & tf_error)
	    constexpr_error (input_location, fundef_p,
			     "%qE is not a constant expression", t);
	  return false;
	}
      return true;

    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      /* -- an invocation of a function other than a constexpr function
            or a constexpr constructor.  */
      {
        tree fun = get_function_named_in_call (t);
        const int nargs = call_expr_nargs (t);
	i = 0;

	if (fun == NULL_TREE)
	  {
	    /* Reset to allow the function to continue past the end
	       of the block below.  Otherwise return early.  */
	    bool bail = true;

	    if (TREE_CODE (t) == CALL_EXPR
		&& CALL_EXPR_FN (t) == NULL_TREE)
	      switch (CALL_EXPR_IFN (t))
		{
		/* These should be ignored, they are optimized away from
		   constexpr functions.  */
		case IFN_UBSAN_NULL:
		case IFN_UBSAN_BOUNDS:
		case IFN_UBSAN_VPTR:
		case IFN_FALLTHROUGH:
		case IFN_ASSUME:
		  return true;

		case IFN_ADD_OVERFLOW:
		case IFN_SUB_OVERFLOW:
		case IFN_MUL_OVERFLOW:
		case IFN_LAUNDER:
		case IFN_VEC_CONVERT:
		  bail = false;
		  break;

		default:
		  break;
		}

	    if (bail)
	      {
		/* fold_call_expr can't do anything with IFN calls.  */
		if (flags & tf_error)
		  constexpr_error (loc, fundef_p,
				   "call to internal function %qE", t);
		return false;
	      }
	  }

	if (fun && is_overloaded_fn (fun))
	  {
	    if (!RECUR (fun, true))
	      return false;
	    fun = get_fns (fun);

	    if (TREE_CODE (fun) == FUNCTION_DECL)
	      {
		if (builtin_valid_in_constant_expr_p (fun))
		  return true;
		if (!maybe_constexpr_fn (fun)
		    /* Allow any built-in function; if the expansion
		       isn't constant, we'll deal with that then.  */
		    && !fndecl_built_in_p (fun)
		    /* In C++20, replaceable global allocation functions
		       are constant expressions.  */
		    && (!cxx_replaceable_global_alloc_fn (fun)
			|| TREE_CODE (t) != CALL_EXPR
			|| (!CALL_FROM_NEW_OR_DELETE_P (t)
			    && (current_function_decl == NULL_TREE
				|| !is_std_allocator_allocate
						(current_function_decl))))
		    /* Allow placement new in std::construct_at.  */
		    && (!cxx_placement_new_fn (fun)
			|| TREE_CODE (t) != CALL_EXPR
			|| current_function_decl == NULL_TREE
			|| !is_std_construct_at (current_function_decl))
		    && !cxx_dynamic_cast_fn_p (fun))
		  {
		    if ((flags & tf_error)
			&& constexpr_error (loc, fundef_p,
					    "call to non-%<constexpr%> "
					    "function %qD", fun))
		      explain_invalid_constexpr_fn (fun);
		    return false;
		  }
	      }

	    fun = OVL_FIRST (fun);
	    /* Skip initial arguments to base constructors.  */
	    if (DECL_BASE_CONSTRUCTOR_P (fun))
	      i = num_artificial_parms_for (fun);
	  }
	else if (fun)
          {
	    if (TREE_TYPE (fun)
		&& FUNCTION_POINTER_TYPE_P (TREE_TYPE (fun)))
	      want_rval = rval;
	    else
	      want_rval = any;
	    if (RECUR (fun, want_rval))
	      /* Might end up being a constant function pointer.  But it
		 could also be a function object with constexpr op(), so
		 we pass 'any' so that the underlying VAR_DECL is deemed
		 as potentially-constant even though it wasn't declared
		 constexpr.  */;
	    else
	      return false;
          }
        for (; i < nargs; ++i)
          {
            tree x = get_nth_callarg (t, i);
	    /* In a template, reference arguments haven't been converted to
	       REFERENCE_TYPE and we might not even know if the parameter
	       is a reference, so accept lvalue constants too.  */
	    bool rv = processing_template_decl ? any : rval;
	    /* Don't require an immediately constant value, as constexpr
	       substitution might not use the value of the argument.  */
	    bool sub_now = false;
	    if (!potential_constant_expression_1 (x, rv, strict,
						  sub_now, fundef_p, flags,
						  jump_target))
	      return false;
          }
        return true;
      }

    case NON_LVALUE_EXPR:
      /* -- an lvalue-to-rvalue conversion (4.1) unless it is applied to
            -- an lvalue of integral type that refers to a non-volatile
               const variable or static data member initialized with
               constant expressions, or

            -- an lvalue of literal type that refers to non-volatile
               object defined with constexpr, or that refers to a
               sub-object of such an object;  */
      return RECUR (TREE_OPERAND (t, 0), rval);

    case EXCESS_PRECISION_EXPR:
      return RECUR (TREE_OPERAND (t, 0), rval);

    case VAR_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	{
	  if (now && is_normal_capture_proxy (t))
	    {
	      /* -- in a lambda-expression, a reference to this or to a
		 variable with automatic storage duration defined outside that
		 lambda-expression, where the reference would be an
		 odr-use.  */

	      if (want_rval)
		/* Since we're doing an lvalue-rvalue conversion, this might
		   not be an odr-use, so evaluate the variable directly. */
		return RECUR (DECL_CAPTURED_VARIABLE (t), rval);

	      if (flags & tf_error)
		{
		  tree cap = DECL_CAPTURED_VARIABLE (t);
		  auto_diagnostic_group d;
		  if (constexpr_error (input_location, fundef_p,
				       "lambda capture of %qE is not a "
				       "constant expression", cap)
		      && decl_constant_var_p (cap))
		    inform (input_location, "because it is used as a glvalue");
		}
	      return false;
	    }
	  /* Treat __PRETTY_FUNCTION__ inside a template function as
	     potentially-constant.  */
	  else if (DECL_PRETTY_FUNCTION_P (t)
		   && DECL_VALUE_EXPR (t) == error_mark_node)
	    return true;
	  return RECUR (DECL_VALUE_EXPR (t), rval);
	}
      if (want_rval
	  && (now || !var_in_maybe_constexpr_fn (t))
	  && !type_dependent_expression_p (t)
	  && !decl_maybe_constant_var_p (t)
	  && (strict
	      || !CP_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (t))
	      || (DECL_INITIAL (t)
		  && !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (t)))
	  && COMPLETE_TYPE_P (TREE_TYPE (t))
	  && !is_really_empty_class (TREE_TYPE (t), /*ignore_vptr*/false))
        {
	  if (flags & tf_error)
	    non_const_var_error (loc, t, fundef_p);
          return false;
        }
      return true;

    case NOP_EXPR:
      if (REINTERPRET_CAST_P (t))
	{
	  if (flags & tf_error)
	    constexpr_error (loc, fundef_p, "%<reinterpret_cast%> is not a "
			     "constant expression");
	  return false;
	}
      /* FALLTHRU */
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
      /* -- a reinterpret_cast.  FIXME not implemented, and this rule
	 may change to something more specific to type-punning (DR 1312).  */
      {
        tree from = TREE_OPERAND (t, 0);
	if (location_wrapper_p (t))
	  {
	    iloc_sentinel ils = loc;
	    return (RECUR (from, want_rval));
	  }
	if (INDIRECT_TYPE_P (TREE_TYPE (t)))
	  {
	    STRIP_ANY_LOCATION_WRAPPER (from);
	    if (TREE_CODE (from) == INTEGER_CST
		&& !integer_zerop (from))
	      {
		if (flags & tf_error)
		  constexpr_error (loc, fundef_p,
				   "%<reinterpret_cast%> from integer to "
				   "pointer");
		return false;
	      }
	  }
        return (RECUR (from, TREE_CODE (t) != VIEW_CONVERT_EXPR));
      }

    case ADDRESSOF_EXPR:
      /* This is like ADDR_EXPR, except it won't form pointer-to-member.  */
      t = TREE_OPERAND (t, 0);
      goto handle_addr_expr;

    case ADDR_EXPR:
      /* -- a unary operator & that is applied to an lvalue that
            designates an object with thread or automatic storage
            duration;  */
      t = TREE_OPERAND (t, 0);

      if (TREE_CODE (t) == OFFSET_REF && PTRMEM_OK_P (t))
	/* A pointer-to-member constant.  */
	return true;

    handle_addr_expr:
#if 0
      /* FIXME adjust when issue 1197 is fully resolved.  For now don't do
         any checking here, as we might dereference the pointer later.  If
         we remove this code, also remove check_automatic_or_tls.  */
      i = check_automatic_or_tls (t);
      if (i == ck_ok)
	return true;
      if (i == ck_bad)
        {
          if (flags & tf_error)
            error ("address-of an object %qE with thread local or "
                   "automatic storage is not a constant expression", t);
          return false;
        }
#endif
      return RECUR (t, any);

    case COMPONENT_REF:
    case ARROW_EXPR:
    case OFFSET_REF:
      /* -- a class member access unless its postfix-expression is
            of literal type or of pointer to literal type.  */
      /* This test would be redundant, as it follows from the
	 postfix-expression being a potential constant expression.  */
      if (type_unknown_p (t))
	return true;
      if (is_overloaded_fn (t))
	/* In a template, a COMPONENT_REF of a function expresses ob.fn(),
	   which uses ob as an lvalue.  */
	want_rval = false;
      gcc_fallthrough ();

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case BIT_FIELD_REF:
      return RECUR (TREE_OPERAND (t, 0), want_rval);

    case EXPR_PACK_EXPANSION:
      return RECUR (PACK_EXPANSION_PATTERN (t), want_rval);

    case PACK_INDEX_EXPR:
      return true;

    case INDIRECT_REF:
      {
        tree x = TREE_OPERAND (t, 0);
        STRIP_NOPS (x);
        if (is_this_parameter (x) && !is_capture_proxy (x))
	  {
	    if (now || !var_in_maybe_constexpr_fn (x))
	      {
		if (flags & tf_error)
		  constexpr_error (loc, fundef_p, "use of %<this%> in a "
				   "constant expression");
		return false;
	      }
	    return true;
	  }
	return RECUR (x, rval);
      }

    case STATEMENT_LIST:
      for (tree stmt : tsi_range (t))
	if (!RECUR (stmt, any))
	  return false;
      return true;

    case MODIFY_EXPR:
      if (cxx_dialect < cxx14)
	goto fail;
      if (!RECUR (TREE_OPERAND (t, 0), any))
	return false;
      /* Just ignore clobbers.  */
      if (TREE_CLOBBER_P (TREE_OPERAND (t, 1)))
	return true;
      if (!RECUR (TREE_OPERAND (t, 1), rval))
	return false;
      return true;

    case MODOP_EXPR:
      if (cxx_dialect < cxx14)
	goto fail;
      if (!RECUR (TREE_OPERAND (t, 0), rval))
	return false;
      if (!RECUR (TREE_OPERAND (t, 2), rval))
	return false;
      return true;

    case DO_STMT:
      if (!RECUR (DO_COND (t), rval))
	return false;
      if (!RECUR (DO_BODY (t), any))
	return false;
      if (breaks (jump_target) || continues (jump_target))
	*jump_target = NULL_TREE;
      return true;

    case FOR_STMT:
      if (!RECUR (FOR_INIT_STMT (t), any))
	return false;
      tmp = FOR_COND (t);
      if (!RECUR (tmp, rval))
	return false;
      if (tmp)
	{
	  if (!processing_template_decl)
	    tmp = cxx_eval_outermost_constant_expr (tmp, true);
	  /* If we couldn't evaluate the condition, it might not ever be
	     true.  */
	  if (!integer_onep (tmp))
	    {
	      /* Before returning true, check if the for body can contain
		 a return.  */
	      hash_set<tree> pset;
	      check_for_return_continue_data data = { &pset, NULL_TREE,
						      NULL_TREE };
	      if (tree ret_expr
		  = cp_walk_tree (&FOR_BODY (t), check_for_return_continue,
				  &data, &pset))
		*jump_target = ret_expr;
	      return true;
	    }
	}
      if (!RECUR (FOR_EXPR (t), any))
	return false;
      if (!RECUR (FOR_BODY (t), any))
	return false;
      if (breaks (jump_target) || continues (jump_target))
	*jump_target = NULL_TREE;
      return true;

    case RANGE_FOR_STMT:
      if (!RECUR (RANGE_FOR_INIT_STMT (t), any))
	return false;
      if (!RECUR (RANGE_FOR_EXPR (t), any))
	return false;
      if (!RECUR (RANGE_FOR_BODY (t), any))
	return false;
      if (breaks (jump_target) || continues (jump_target))
	*jump_target = NULL_TREE;
      return true;

    case WHILE_STMT:
      tmp = WHILE_COND (t);
      if (!RECUR (tmp, rval))
	return false;
      if (!processing_template_decl)
	tmp = cxx_eval_outermost_constant_expr (tmp, true);
      /* If we couldn't evaluate the condition, it might not ever be true.  */
      if (!integer_onep (tmp))
	{
	  /* Before returning true, check if the while body can contain
	     a return.  */
	  hash_set<tree> pset;
	  check_for_return_continue_data data = { &pset, NULL_TREE,
						  NULL_TREE  };
	  if (tree ret_expr
	      = cp_walk_tree (&WHILE_BODY (t), check_for_return_continue,
			      &data, &pset))
	    *jump_target = ret_expr;
	  return true;
	}
      if (!RECUR (WHILE_BODY (t), any))
	return false;
      if (breaks (jump_target) || continues (jump_target))
	*jump_target = NULL_TREE;
      return true;

    case SWITCH_STMT:
      if (!RECUR (SWITCH_STMT_COND (t), rval))
	return false;
      /* FIXME we don't check SWITCH_STMT_BODY currently, because even
	 unreachable labels would be checked and it is enough if there is
	 a single switch cond value for which it is a valid constant
	 expression.  We need to check if there are any RETURN_EXPRs
	 or CONTINUE_STMTs inside of the body though, as in that case
	 we need to set *jump_target.  */
      else
	{
	  hash_set<tree> pset;
	  check_for_return_continue_data data = { &pset, NULL_TREE,
						  NULL_TREE };
	  if (tree ret_expr
	      = cp_walk_tree (&SWITCH_STMT_BODY (t), check_for_return_continue,
			      &data, &pset))
	    /* The switch might return.  */
	    *jump_target = ret_expr;
	  else if (data.continue_stmt)
	    /* The switch can't return, but might continue.  */
	    *jump_target = data.continue_stmt;
	}
      return true;

    case STMT_EXPR:
      return RECUR (STMT_EXPR_STMT (t), rval);

    case LAMBDA_EXPR:
      if (cxx_dialect >= cxx17)
	/* In C++17 lambdas can be constexpr, don't give up yet.  */
	return true;
      else if (flags & tf_error)
	constexpr_error (loc, fundef_p, "lambda-expression is not a "
			 "constant expression before C++17");
      return false;

    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
      if (cxx_dialect >= cxx20)
	/* In C++20, new-expressions are potentially constant.  */
	return true;
      else if (flags & tf_error)
	constexpr_error (loc, fundef_p, "new-expression is not a "
			 "constant expression before C++20");
      return false;

    case DYNAMIC_CAST_EXPR:
    case PSEUDO_DTOR_EXPR:
    case THROW_EXPR:
    case OMP_PARALLEL:
    case OMP_TASK:
    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OMP_TASKLOOP:
    case OMP_LOOP:
    case OMP_TEAMS:
    case OMP_TARGET_DATA:
    case OMP_TARGET:
    case OMP_SECTIONS:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_SINGLE:
    case OMP_SCAN:
    case OMP_SCOPE:
    case OMP_SECTION:
    case OMP_MASTER:
    case OMP_MASKED:
    case OMP_TASKGROUP:
    case OMP_TARGET_UPDATE:
    case OMP_TARGET_ENTER_DATA:
    case OMP_TARGET_EXIT_DATA:
    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
    case OMP_DEPOBJ:
    case OACC_PARALLEL:
    case OACC_KERNELS:
    case OACC_SERIAL:
    case OACC_DATA:
    case OACC_HOST_DATA:
    case OACC_LOOP:
    case OACC_CACHE:
    case OACC_DECLARE:
    case OACC_ENTER_DATA:
    case OACC_EXIT_DATA:
    case OACC_UPDATE:
    case OMP_ARRAY_SECTION:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case TRANSACTION_EXPR:
    case AT_ENCODE_EXPR:
    fail:
      if (flags & tf_error)
	 constexpr_error (loc, fundef_p, "expression %qE is not a constant "
			  "expression", t);
      return false;

    case ASM_EXPR:
      if (flags & tf_error)
	inline_asm_in_constexpr_error (loc, fundef_p);
      return false;

    case OBJ_TYPE_REF:
      if (cxx_dialect >= cxx20)
	/* In C++20 virtual calls can be constexpr, don't give up yet.  */
	return true;
      else if (flags & tf_error)
	constexpr_error (loc, fundef_p, "virtual functions cannot be "
			 "%<constexpr%> before C++20");
      return false;

    case TYPEID_EXPR:
      /* In C++20, a typeid expression whose operand is of polymorphic
	 class type can be constexpr.  */
      {
        tree e = TREE_OPERAND (t, 0);
	if (cxx_dialect < cxx20
	    && strict
	    && !TYPE_P (e)
	    && !type_dependent_expression_p (e)
	    && TYPE_POLYMORPHIC_P (TREE_TYPE (e)))
          {
            if (flags & tf_error)
	      constexpr_error (loc, fundef_p, "%<typeid%> is not a "
			       "constant expression because %qE is "
			       "of polymorphic type", e);
            return false;
          }
        return true;
      }

    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
      want_rval = true;
      goto binary;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case SPACESHIP_EXPR:
      want_rval = true;
      goto binary;

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      if (cxx_dialect < cxx14)
	goto fail;
      goto unary;

    case BIT_NOT_EXPR:
      /* A destructor.  */
      if (TYPE_P (TREE_OPERAND (t, 0)))
	return true;
      /* fall through.  */

    case CONJ_EXPR:
    case SAVE_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case ABSU_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    case UNARY_PLUS_EXPR:
    case UNARY_LEFT_FOLD_EXPR:
    case UNARY_RIGHT_FOLD_EXPR:
    case VEC_DUPLICATE_EXPR:
    unary:
      return RECUR (TREE_OPERAND (t, 0), rval);

    case CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      if (!cast_valid_in_integral_constant_expression_p (TREE_TYPE (t)))
	/* In C++98, a conversion to non-integral type can't be part of a
	   constant expression.  */
	{
	  if (flags & tf_error)
	    constexpr_error (loc, fundef_p,
			     "cast to non-integral type %qT in a constant "
			     "expression", TREE_TYPE (t));
	  return false;
	}
      /* This might be a conversion from a class to a (potentially) literal
	 type.  Let's consider it potentially constant since the conversion
	 might be a constexpr user-defined conversion.  */
      else if (cxx_dialect >= cxx11
	       && (dependent_type_p (TREE_TYPE (t))
		   || !COMPLETE_TYPE_P (TREE_TYPE (t))
		   || literal_type_p (TREE_TYPE (t)))
	       && TREE_OPERAND (t, 0)
	       && (TREE_CODE (t) != CAST_EXPR
		   || !TREE_CHAIN (TREE_OPERAND (t, 0))))
	{
	  tree from = TREE_OPERAND (t, 0);
	  if (TREE_CODE (t) == CAST_EXPR)
	    from = TREE_VALUE (from);
	  tree type = TREE_TYPE (from);
	  /* If this is a dependent type, it could end up being a class
	     with conversions.  */
	  if (type == NULL_TREE || WILDCARD_TYPE_P (type))
	    return true;
	  /* Or a non-dependent class which has conversions.  */
	  else if (CLASS_TYPE_P (type)
		   && (TYPE_HAS_CONVERSION (type) || dependent_scope_p (type)))
	    return true;
	}

      return (RECUR (TREE_OPERAND (t, 0),
		     !TYPE_REF_P (TREE_TYPE (t))));

    case BIND_EXPR:
      return RECUR (BIND_EXPR_BODY (t), want_rval);

    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_BLOCK:
    case EH_SPEC_BLOCK:
    case EXPR_STMT:
    case PAREN_EXPR:
      /* For convenience.  */
    case LOOP_EXPR:
    case EXIT_EXPR:
      return RECUR (TREE_OPERAND (t, 0), want_rval);

    case DECL_EXPR:
      tmp = DECL_EXPR_DECL (t);
      if (VAR_P (tmp) && !DECL_ARTIFICIAL (tmp)
	  && (processing_template_decl
	      ? !decl_maybe_constant_var_p (tmp)
	      : !decl_constant_var_p (tmp)))
	{
	  if (CP_DECL_THREAD_LOCAL_P (tmp) && !DECL_REALLY_EXTERN (tmp))
	    {
	      if (flags & tf_error)
		constexpr_error (DECL_SOURCE_LOCATION (tmp), fundef_p,
				 "%qD defined %<thread_local%> in "
				 "%<constexpr%> context", tmp);
	      return false;
	    }
	  else if (TREE_STATIC (tmp))
	    {
	      if (flags & tf_error)
		constexpr_error (DECL_SOURCE_LOCATION (tmp), fundef_p,
				 "%qD defined %<static%> in %<constexpr%> "
				 "context", tmp);
	      return false;
	    }
	  else if (!check_for_uninitialized_const_var
		   (tmp, /*constexpr_context_p=*/true, flags))
	    return false;
	}
      if (VAR_P (tmp))
	return RECUR (DECL_INITIAL (tmp), want_rval);
      return true;

    case TRY_FINALLY_EXPR:
      return (RECUR (TREE_OPERAND (t, 0), want_rval)
	      && RECUR (TREE_OPERAND (t, 1), any));

    case SCOPE_REF:
      return RECUR (TREE_OPERAND (t, 1), want_rval);

    case TARGET_EXPR:
      if (!TARGET_EXPR_DIRECT_INIT_P (t)
	  && !TARGET_EXPR_ELIDING_P (t)
	  && !literal_type_p (TREE_TYPE (t)))
	{
	  if (flags & tf_error)
	    {
	      auto_diagnostic_group d;
	      if (constexpr_error (loc, fundef_p,
				   "temporary of non-literal type %qT in a "
				   "constant expression", TREE_TYPE (t)))
		explain_non_literal_class (TREE_TYPE (t));
	    }
	  return false;
	}
      /* FALLTHRU */
    case INIT_EXPR:
      return RECUR (TREE_OPERAND (t, 1), rval);

    case CONSTRUCTOR:
      {
        vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
        constructor_elt *ce;
        for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
	  if (!RECUR (ce->value, want_rval))
	    return false;
	return true;
      }

    case TREE_LIST:
      {
	gcc_assert (TREE_PURPOSE (t) == NULL_TREE
		    || DECL_P (TREE_PURPOSE (t)));
	if (!RECUR (TREE_VALUE (t), want_rval))
	  return false;
	if (TREE_CHAIN (t) == NULL_TREE)
	  return true;
	return RECUR (TREE_CHAIN (t), want_rval);
      }

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      {
	tree denom = TREE_OPERAND (t, 1);
	if (!RECUR (denom, rval))
	  return false;
	/* We can't call cxx_eval_outermost_constant_expr on an expression
	   that hasn't been through instantiate_non_dependent_expr yet.  */
	if (!processing_template_decl)
	  denom = cxx_eval_outermost_constant_expr (denom, true);
	if (integer_zerop (denom))
	  {
	    if (flags & tf_error)
	      constexpr_error (input_location, fundef_p,
			       "division by zero is not a constant expression");
	    return false;
	  }
	else
	  {
	    want_rval = true;
	    return RECUR (TREE_OPERAND (t, 0), want_rval);
	  }
      }

    case COMPOUND_EXPR:
      {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if (TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	  return RECUR (op0, want_rval);
	else
	  goto binary;
      }

      /* If the first operand is the non-short-circuit constant, look at
	 the second operand; otherwise we only care about the first one for
	 potentiality.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      tmp = boolean_true_node;
      goto truth;
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      tmp = boolean_false_node;
    truth:
      {
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	if (!RECUR (op0, rval))
	  return false;
	if (!(flags & tf_error) && RECUR (op1, rval))
	  /* When quiet, try to avoid expensive trial evaluation by first
	     checking potentiality of the second operand.  */
	  return true;
	if (!processing_template_decl)
	  op0 = cxx_eval_outermost_constant_expr (op0, true);
	if (tree_int_cst_equal (op0, tmp))
	  return (flags & tf_error) ? RECUR (op1, rval) : false;
	else
	  return true;
      }

    case PLUS_EXPR:
    case MULT_EXPR:
    case POINTER_PLUS_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      want_rval = true;
      /* Fall through.  */
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
    case MEM_REF:
    case BINARY_LEFT_FOLD_EXPR:
    case BINARY_RIGHT_FOLD_EXPR:
    binary:
      for (i = 0; i < 2; ++i)
	if (!RECUR (TREE_OPERAND (t, i), want_rval))
	  return false;
      return true;

    case VEC_PERM_EXPR:
     for (i = 0; i < 3; ++i)
      if (!RECUR (TREE_OPERAND (t, i), true))
	return false;
     return true;

    case COND_EXPR:
      if (COND_EXPR_IS_VEC_DELETE (t) && cxx_dialect < cxx20)
	{
	  if (flags & tf_error)
	    constexpr_error (loc, fundef_p, "%<delete[]%> is not a "
			     "constant expression");
	  return false;
	}
      /* Fall through.  */
    case IF_STMT:
    case VEC_COND_EXPR:
      /* If the condition is a known constant, we know which of the legs we
	 care about; otherwise we only require that the condition and
	 either of the legs be potentially constant.  */
      tmp = TREE_OPERAND (t, 0);
      if (!RECUR (tmp, rval))
	return false;
      if (!processing_template_decl)
	tmp = cxx_eval_outermost_constant_expr (tmp, true);
      /* potential_constant_expression* isn't told if it is called for
	 manifestly_const_eval or not, so for consteval if always
	 process both branches as if the condition is not a known
	 constant.  */
      if (TREE_CODE (t) != IF_STMT || !IF_STMT_CONSTEVAL_P (t))
	{
	  if (integer_zerop (tmp))
	    return RECUR (TREE_OPERAND (t, 2), want_rval);
	  else if (TREE_CODE (tmp) == INTEGER_CST)
	    return RECUR (TREE_OPERAND (t, 1), want_rval);
	}
      tmp = *jump_target;
      for (i = 1; i < 3; ++i)
	{
	  tree this_jump_target = tmp;
	  if (potential_constant_expression_1 (TREE_OPERAND (t, i),
					       want_rval, strict, now, fundef_p,
					       tf_none, &this_jump_target))
	    {
	      if (returns (&this_jump_target))
		*jump_target = this_jump_target;
	      else if (!returns (jump_target))
		{
		  if (breaks (&this_jump_target)
		      || continues (&this_jump_target))
		    *jump_target = this_jump_target;
		  if (i == 1)
		    {
		      /* If the then branch is potentially constant, but
			 does not return, check if the else branch
			 couldn't return, break or continue.  */
		      hash_set<tree> pset;
		      check_for_return_continue_data data = { &pset, NULL_TREE,
							      NULL_TREE };
		      if (tree ret_expr
			= cp_walk_tree (&TREE_OPERAND (t, 2),
					check_for_return_continue, &data,
					&pset))
			*jump_target = ret_expr;
		      else if (*jump_target == NULL_TREE)
			{
			  if (data.continue_stmt)
			    *jump_target = data.continue_stmt;
			  else if (data.break_stmt)
			    *jump_target = data.break_stmt;
			}
		    }
		}
	      return true;
	    }
	}
      if (flags & tf_error)
	{
	  if (TREE_CODE (t) == IF_STMT)
	    constexpr_error (loc, fundef_p, "neither branch of %<if%> is a "
			     "constant expression");
	  else
	    constexpr_error (loc, fundef_p, "expression %qE is not a "
			     "constant expression", t);
	}
      return false;

    case VEC_INIT_EXPR:
      if (VEC_INIT_EXPR_IS_CONSTEXPR (t))
	return true;
      if (flags & tf_error)
	{
	  if (constexpr_error (loc, fundef_p, "non-constant array "
			       "initialization"))
	    diagnose_non_constexpr_vec_init (t);
	}
      return false;

    case TYPE_DECL:
    case TAG_DEFN:
      /* We can see these in statement-expressions.  */
      return true;

    case CLEANUP_STMT:
      if (!RECUR (CLEANUP_BODY (t), any))
	return false;
      if (!CLEANUP_EH_ONLY (t) && !RECUR (CLEANUP_EXPR (t), any))
	return false;
      return true;

    case EMPTY_CLASS_EXPR:
      return true;

    case GOTO_EXPR:
      {
	tree *target = &TREE_OPERAND (t, 0);
	/* Gotos representing break, continue and cdtor return are OK.  */
	if (breaks (target) || continues (target) || returns (target))
	  {
	    *jump_target = *target;
	    return true;
	  }
	if (flags & tf_error)
	  constexpr_error (loc, fundef_p, "%<goto%> is not a constant "
			   "expression");
	return false;
      }

    case ASSERTION_STMT:
    case PRECONDITION_STMT:
    case POSTCONDITION_STMT:
      if (!checked_contract_p (get_contract_semantic (t)))
	return true;
      return RECUR (CONTRACT_CONDITION (t), rval);

    case LABEL_EXPR:
      t = LABEL_EXPR_LABEL (t);
      if (DECL_ARTIFICIAL (t) || cxx_dialect >= cxx23)
	return true;
      else if (flags & tf_error)
	constexpr_error (loc, fundef_p, "label definition in %<constexpr%> "
			 "function only available with %<-std=c++2b%> or "
			 "%<-std=gnu++2b%>");
      return false;

    case ANNOTATE_EXPR:
      return RECUR (TREE_OPERAND (t, 0), rval);

    case BIT_CAST_EXPR:
      return RECUR (TREE_OPERAND (t, 0), rval);

    /* Coroutine await, yield and return expressions are not.  */
    case CO_AWAIT_EXPR:
    case CO_YIELD_EXPR:
    case CO_RETURN_EXPR:
      return false;

    case NONTYPE_ARGUMENT_PACK:
      {
	tree args = ARGUMENT_PACK_ARGS (t);
	int len = TREE_VEC_LENGTH (args);
	for (int i = 0; i < len; ++i)
	  if (!RECUR (TREE_VEC_ELT (args, i), any))
	    return false;
	return true;
      }

    default:
      if (objc_non_constant_expr_p (t))
	return false;

      sorry ("unexpected AST of kind %s", get_tree_code_name (TREE_CODE (t)));
      gcc_unreachable ();
      return false;
    }
#undef RECUR
}

bool
potential_constant_expression_1 (tree t, bool want_rval, bool strict, bool now,
				 bool fundef_p, tsubst_flags_t flags)
{
  if (flags & tf_error)
    {
      /* Check potentiality quietly first, as that could be performed more
	 efficiently in some cases (currently only for TRUTH_*_EXPR).  If
	 that fails, replay the check noisily to give errors.  */
      flags &= ~tf_error;
      if (potential_constant_expression_1 (t, want_rval, strict, now, fundef_p,
					   flags))
	return true;
      flags |= tf_error;
    }

  tree target = NULL_TREE;
  return potential_constant_expression_1 (t, want_rval, strict, now, fundef_p,
					  flags, &target);
}

/* The main entry point to the above.  */

bool
potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/false, /*strict*/true,
					  /*now*/false, /*fundef_p*/false,
					  tf_none);
}

/* As above, but require a constant rvalue.  */

bool
potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/true, /*strict*/true,
					  /*now*/false, /*fundef_p*/false,
					  tf_none);
}

/* Like above, but complain about non-constant expressions.  */

bool
require_potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/false, /*strict*/true,
					  /*now*/false, /*fundef_p*/false,
					  tf_warning_or_error);
}

/* Cross product of the above.  */

bool
require_potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/true, /*strict*/true,
					  /*now*/false, /*fundef_p*/false,
					  tf_warning_or_error);
}

/* Like require_potential_rvalue_constant_expression, but fundef_p is true.  */

bool
require_potential_rvalue_constant_expression_fncheck (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/true, /*strict*/true,
					  /*now*/false, /*fundef_p*/true,
					  tf_warning_or_error);
}

/* Like above, but don't consider PARM_DECL a potential_constant_expression.  */

bool
require_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/true, /*strict*/true,
					  /*now*/true, /*fundef_p*/false,
					  tf_warning_or_error);
}

/* Like potential_constant_expression, but don't consider possible constexpr
   substitution of the current function.  That is, PARM_DECL qualifies under
   potential_constant_expression, but not here.

   This is basically what you can check when any actual constant values might
   be value-dependent.  */

bool
is_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/false, /*strict*/true,
					  /*now*/true, /*fundef_p*/false,
					  tf_none);
}

/* As above, but expect an rvalue.  */

bool
is_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/true, /*strict*/true,
					  /*now*/true, /*fundef_p*/false,
					  tf_none);
}

/* Like above, but complain about non-constant expressions.  */

bool
require_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/false, /*strict*/true,
					  /*now*/true, /*fundef_p*/false,
					  tf_warning_or_error);
}

/* Like is_constant_expression, but allow const variables that are not allowed
   under constexpr rules.  */

bool
is_static_init_expression (tree t)
{
  return potential_constant_expression_1 (t, /*want_rval*/false,
					  /*strict*/false, /*now*/true,
					  /*fundef_p*/false, tf_none);
}

/* Returns true if T is a potential constant expression that is not
   instantiation-dependent, and therefore a candidate for constant folding even
   in a template.  */

bool
is_nondependent_constant_expression (tree t)
{
  return (!type_unknown_p (t)
	  && is_constant_expression (t)
	  && !instantiation_dependent_expression_p (t));
}

/* Returns true if T is a potential static initializer expression that is not
   instantiation-dependent.  */

bool
is_nondependent_static_init_expression (tree t)
{
  return (!type_unknown_p (t)
	  && is_static_init_expression (t)
	  && !instantiation_dependent_expression_p (t));
}

/* True iff FN is an implicitly constexpr function.  */

bool
decl_implicit_constexpr_p (tree fn)
{
  if (!(flag_implicit_constexpr
	&& TREE_CODE (fn) == FUNCTION_DECL
	&& DECL_DECLARED_CONSTEXPR_P (fn)))
    return false;

  if (DECL_CLONED_FUNCTION_P (fn))
    fn = DECL_CLONED_FUNCTION (fn);

  return (DECL_LANG_SPECIFIC (fn)
	  && DECL_LANG_SPECIFIC (fn)->u.fn.implicit_constexpr);
}

/* Finalize constexpr processing after parsing.  */

void
fini_constexpr (void)
{
  /* The contexpr call and fundef copies tables are no longer needed.  */
  constexpr_call_table = NULL;
  fundef_copies_table = NULL;
}

#include "gt-cp-constexpr.h"
