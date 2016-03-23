/* Perform -*- C++ -*- constant expression evaluation, including calls to
   constexpr functions.  These routines are used both during actual parsing
   and during the instantiation of template functions.

   Copyright (C) 1998-2016 Free Software Foundation, Inc.

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

static bool verify_constant (tree, bool, bool *, bool *);
#define VERIFY_CONSTANT(X)						\
do {									\
  if (verify_constant ((X), ctx->quiet, non_constant_p, overflow_p)) \
    return t;								\
 } while (0)

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
      || TREE_CODE (t) == REFERENCE_TYPE
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
   be literal.  Return the DECL if OK, otherwise NULL.  */

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
	      error ("the type %qT of constexpr variable %qD is not literal",
		     type, decl);
	      explain_non_literal_class (type);
	    }
	  else
	    {
	      if (!DECL_TEMPLATE_INSTANTIATION (current_function_decl))
		{
		  error ("variable %qD of non-literal type %qT in %<constexpr%> "
			 "function", decl, type);
		  explain_non_literal_class (type);
		}
	      cp_function_chain->invalid_constexpr = true;
	    }
	  return NULL;
	}
    }
  return decl;
}

/* Representation of entries in the constexpr function definition table.  */

struct GTY((for_user)) constexpr_fundef {
  tree decl;
  tree body;
};

struct constexpr_fundef_hasher : ggc_ptr_hash<constexpr_fundef>
{
  static hashval_t hash (constexpr_fundef *);
  static bool equal (constexpr_fundef *, constexpr_fundef *);
};

/* This table holds all constexpr function definitions seen in
   the current translation unit.  */

static GTY (()) hash_table<constexpr_fundef_hasher> *constexpr_fundef_table;

/* Utility function used for managing the constexpr function table.
   Return true if the entries pointed to by P and Q are for the
   same constexpr function.  */

inline bool
constexpr_fundef_hasher::equal (constexpr_fundef *lhs, constexpr_fundef *rhs)
{
  return lhs->decl == rhs->decl;
}

/* Utility function used for managing the constexpr function table.
   Return a hash value for the entry pointed to by Q.  */

inline hashval_t
constexpr_fundef_hasher::hash (constexpr_fundef *fundef)
{
  return DECL_UID (fundef->decl);
}

/* Return a previously saved definition of function FUN.   */

static constexpr_fundef *
retrieve_constexpr_fundef (tree fun)
{
  constexpr_fundef fundef = { NULL, NULL };
  if (constexpr_fundef_table == NULL)
    return NULL;

  fundef.decl = fun;
  return constexpr_fundef_table->find (&fundef);
}

/* Check whether the parameter and return types of FUN are valid for a
   constexpr function, and complain if COMPLAIN.  */

static bool
is_valid_constexpr_fn (tree fun, bool complain)
{
  bool ret = true;

  if (DECL_INHERITED_CTOR_BASE (fun)
      && TREE_CODE (fun) == TEMPLATE_DECL)
    {
      ret = false;
      if (complain)
	error ("inherited constructor %qD is not constexpr",
	       get_inherited_ctor (fun));
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
		error ("invalid type for parameter %d of constexpr "
		       "function %q+#D", DECL_PARM_INDEX (parm), fun);
		explain_non_literal_class (TREE_TYPE (parm));
	      }
	  }
    }

  if (!DECL_CONSTRUCTOR_P (fun))
    {
      tree rettype = TREE_TYPE (TREE_TYPE (fun));
      if (!literal_type_p (rettype))
	{
	  ret = false;
	  if (complain)
	    {
	      error ("invalid return type %qT of constexpr function %q+D",
		     rettype, fun);
	      explain_non_literal_class (rettype);
	    }
	}

      if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
	  && !CLASSTYPE_LITERAL_P (DECL_CONTEXT (fun)))
	{
	  ret = false;
	  if (complain)
	    {
	      error ("enclosing class of constexpr non-static member "
		     "function %q+#D is not a literal type", fun);
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
      tree_stmt_iterator i;
      for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	{
	  if (! build_data_member_initialization (tsi_stmt (i), vec))
	    return false;
	}
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
      if (TREE_CODE (DECL_EXPR_DECL (list)) == USING_DECL)
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
	error ("constexpr constructor does not have empty body");
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
	for (tree_stmt_iterator i = tsi_start (body);
	     !tsi_end_p (i); tsi_next (&i))
	  {
	    body = tsi_stmt (i);
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
      tree_stmt_iterator i;
      for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
	{
	  ok = build_data_member_initialization (tsi_stmt (i), &vec);
	  if (!ok)
	    break;
	}
    }
  else if (TREE_CODE (body) == TRY_BLOCK)
    {
      error ("body of %<constexpr%> constructor cannot be "
	     "a function-try-block");
      return error_mark_node;
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

/* Subroutine of register_constexpr_fundef.  BODY is the body of a function
   declared to be constexpr, or a sub-statement thereof.  Returns the
   return value if suitable, error_mark_node for a statement not allowed in
   a constexpr function, or NULL_TREE if no return value was found.  */

static tree
constexpr_fn_retval (tree body)
{
  switch (TREE_CODE (body))
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	tree expr = NULL_TREE;
	for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
	  {
	    tree s = constexpr_fn_retval (tsi_stmt (i));
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
      return NULL_TREE;

    default:
      return error_mark_node;
    }
}

/* Subroutine of register_constexpr_fundef.  BODY is the DECL_SAVED_TREE of
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

/* FUN is a constexpr constructor with massaged body BODY.  Return true
   if some bases/fields are uninitialized, and complain if COMPLAIN.  */

static bool
cx_check_missing_mem_inits (tree fun, tree body, bool complain)
{
  bool bad;
  tree field;
  unsigned i, nelts;
  tree ctype;

  if (TREE_CODE (body) != CONSTRUCTOR)
    return false;

  nelts = CONSTRUCTOR_NELTS (body);
  ctype = DECL_CONTEXT (fun);
  field = TYPE_FIELDS (ctype);

  if (TREE_CODE (ctype) == UNION_TYPE)
    {
      if (nelts == 0 && next_initializable_field (field))
	{
	  if (complain)
	    error ("%<constexpr%> constructor for union %qT must "
		   "initialize exactly one non-static data member", ctype);
	  return true;
	}
      return false;
    }

  bad = false;
  for (i = 0; i <= nelts; ++i)
    {
      tree index;
      if (i == nelts)
	index = NULL_TREE;
      else
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
	  if (TREE_CODE (field) != FIELD_DECL
	      || (DECL_C_BIT_FIELD (field) && !DECL_NAME (field))
	      || DECL_ARTIFICIAL (field))
	    continue;
	  ftype = strip_array_types (TREE_TYPE (field));
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
	  error ("member %qD must be initialized by mem-initializer "
		 "in %<constexpr%> constructor", field);
	  inform (DECL_SOURCE_LOCATION (field), "declared here");
	  bad = true;
	}
      if (field == NULL_TREE)
	break;
      field = DECL_CHAIN (field);
    }

  return bad;
}

/* We are processing the definition of the constexpr function FUN.
   Check that its BODY fulfills the propriate requirements and
   enter it in the constexpr function definition table.
   For constructor BODY is actually the TREE_LIST of the
   member-initializer list.  */

tree
register_constexpr_fundef (tree fun, tree body)
{
  constexpr_fundef entry;
  constexpr_fundef **slot;

  if (!is_valid_constexpr_fn (fun, !DECL_GENERATED_P (fun)))
    return NULL;

  tree massaged = massage_constexpr_body (fun, body);
  if (massaged == NULL_TREE || massaged == error_mark_node)
    {
      if (!DECL_CONSTRUCTOR_P (fun))
	error ("body of constexpr function %qD not a return-statement", fun);
      return NULL;
    }

  if (!potential_rvalue_constant_expression (massaged))
    {
      if (!DECL_GENERATED_P (fun))
	require_potential_rvalue_constant_expression (massaged);
      return NULL;
    }

  if (DECL_CONSTRUCTOR_P (fun)
      && cx_check_missing_mem_inits (fun, massaged, !DECL_GENERATED_P (fun)))
    return NULL;

  /* Create the constexpr function table if necessary.  */
  if (constexpr_fundef_table == NULL)
    constexpr_fundef_table
      = hash_table<constexpr_fundef_hasher>::create_ggc (101);

  entry.decl = fun;
  entry.body = body;
  slot = constexpr_fundef_table->find_slot (&entry, INSERT);

  gcc_assert (*slot == NULL);
  *slot = ggc_alloc<constexpr_fundef> ();
  **slot = entry;

  return fun;
}

/* FUN is a non-constexpr function called in a context that requires a
   constant expression.  If it comes from a constexpr template, explain why
   the instantiation isn't constexpr.  */

void
explain_invalid_constexpr_fn (tree fun)
{
  static hash_set<tree> *diagnosed;
  tree body;
  location_t save_loc;
  /* Only diagnose defaulted functions or instantiations.  */
  if (!DECL_DEFAULTED_FN (fun)
      && !is_instantiation_of_constexpr (fun))
    return;
  if (diagnosed == NULL)
    diagnosed = new hash_set<tree>;
  if (diagnosed->add (fun))
    /* Already explained.  */
    return;

  save_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fun);
  inform (input_location,
	  "%qD is not usable as a constexpr function because:", fun);
  /* First check the declaration.  */
  if (is_valid_constexpr_fn (fun, true))
    {
      /* Then if it's OK, the body.  */
      if (!DECL_DECLARED_CONSTEXPR_P (fun))
	explain_implicit_non_constexpr (fun);
      else
	{
	  body = massage_constexpr_body (fun, DECL_SAVED_TREE (fun));
	  require_potential_rvalue_constant_expression (body);
	  if (DECL_CONSTRUCTOR_P (fun))
	    cx_check_missing_mem_inits (fun, body, true);
	}
    }
  input_location = save_loc;
}

/* Objects of this type represent calls to constexpr functions
   along with the bindings of parameters to their arguments, for
   the purpose of compile time evaluation.  */

struct GTY((for_user)) constexpr_call {
  /* Description of the constexpr function definition.  */
  constexpr_fundef *fundef;
  /* Parameter bindings environment.  A TREE_LIST where each TREE_PURPOSE
     is a parameter _DECL and the TREE_VALUE is the value of the parameter.
     Note: This arrangement is made to accommodate the use of
     iterative_hash_template_arg (see pt.c).  If you change this
     representation, also change the hash calculation in
     cxx_eval_call_expression.  */
  tree bindings;
  /* Result of the call.
       NULL means the call is being evaluated.
       error_mark_node means that the evaluation was erroneous;
       otherwise, the actuall value of the call.  */
  tree result;
  /* The hash of this call; we remember it here to avoid having to
     recalculate it when expanding the hash table.  */
  hashval_t hash;
};

struct constexpr_call_hasher : ggc_ptr_hash<constexpr_call>
{
  static hashval_t hash (constexpr_call *);
  static bool equal (constexpr_call *, constexpr_call *);
};

/* The constexpr expansion context.  CALL is the current function
   expansion, CTOR is the current aggregate initializer, OBJECT is the
   object being initialized by CTOR, either a VAR_DECL or a _REF.  VALUES
   is a map of values of variables initialized within the expression.  */

struct constexpr_ctx {
  /* The innermost call we're evaluating.  */
  constexpr_call *call;
  /* Values for any temporaries or local variables within the
     constant-expression. */
  hash_map<tree,tree> *values;
  /* SAVE_EXPRs that we've seen within the current LOOP_EXPR.  NULL if we
     aren't inside a loop.  */
  hash_set<tree> *save_exprs;
  /* The CONSTRUCTOR we're currently building up for an aggregate
     initializer.  */
  tree ctor;
  /* The object we're building the CONSTRUCTOR for.  */
  tree object;
  /* Whether we should error on a non-constant expression or fail quietly.  */
  bool quiet;
  /* Whether we are strictly conforming to constant expression rules or
     trying harder to get a constant value.  */
  bool strict;
};

/* A table of all constexpr calls that have been evaluated by the
   compiler in this translation unit.  */

static GTY (()) hash_table<constexpr_call_hasher> *constexpr_call_table;

static tree cxx_eval_constant_expression (const constexpr_ctx *, tree,
					  bool, bool *, bool *, tree * = NULL);

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
  tree lhs_bindings;
  tree rhs_bindings;
  if (lhs == rhs)
    return 1;
  if (!constexpr_fundef_hasher::equal (lhs->fundef, rhs->fundef))
    return 0;
  lhs_bindings = lhs->bindings;
  rhs_bindings = rhs->bindings;
  while (lhs_bindings != NULL && rhs_bindings != NULL)
    {
      tree lhs_arg = TREE_VALUE (lhs_bindings);
      tree rhs_arg = TREE_VALUE (rhs_bindings);
      gcc_assert (TREE_TYPE (lhs_arg) == TREE_TYPE (rhs_arg));
      if (!cp_tree_equal (lhs_arg, rhs_arg))
        return 0;
      lhs_bindings = TREE_CHAIN (lhs_bindings);
      rhs_bindings = TREE_CHAIN (rhs_bindings);
    }
  return lhs_bindings == rhs_bindings;
}

/* Initialize the constexpr call table, if needed.  */

static void
maybe_initialize_constexpr_call_table (void)
{
  if (constexpr_call_table == NULL)
    constexpr_call_table = hash_table<constexpr_call_hasher>::create_ggc (101);
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  If the call is lexically to a named function,
   retrun the _DECL for that function.  */

static tree
get_function_named_in_call (tree t)
{
  tree fun = NULL;
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      fun = CALL_EXPR_FN (t);
      break;

    case AGGR_INIT_EXPR:
      fun = AGGR_INIT_EXPR_FN (t);
      break;

    default:
      gcc_unreachable();
      break;
    }
  if (fun && TREE_CODE (fun) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fun, 0)) == FUNCTION_DECL)
    fun = TREE_OPERAND (fun, 0);
  return fun;
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  Return the Nth argument.  */

static inline tree
get_nth_callarg (tree t, int n)
{
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return CALL_EXPR_ARG (t, n);

    case AGGR_INIT_EXPR:
      return AGGR_INIT_EXPR_ARG (t, n);

    default:
      gcc_unreachable ();
      return NULL;
    }
}

/* Attempt to evaluate T which represents a call to a builtin function.
   We assume here that all builtin functions evaluate to scalar types
   represented by _CST nodes.  */

static tree
cxx_eval_builtin_function_call (const constexpr_ctx *ctx, tree t, tree fun,
				bool lval,
				bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree *args = (tree *) alloca (nargs * sizeof (tree));
  tree new_call;
  int i;

  /* Don't fold __builtin_constant_p within a constexpr function.  */
  bool bi_const_p = (DECL_FUNCTION_CODE (fun) == BUILT_IN_CONSTANT_P);

  if (bi_const_p
      && current_function_decl
      && DECL_DECLARED_CONSTEXPR_P (current_function_decl))
    {
      *non_constant_p = true;
      return t;
    }

  /* Be permissive for arguments to built-ins; __builtin_constant_p should
     return constant false for a non-constant argument.  */
  constexpr_ctx new_ctx = *ctx;
  new_ctx.quiet = true;
  bool dummy1 = false, dummy2 = false;
  for (i = 0; i < nargs; ++i)
    {
      args[i] = cxx_eval_constant_expression (&new_ctx, CALL_EXPR_ARG (t, i),
					      lval, &dummy1, &dummy2);
      if (bi_const_p)
	/* For __built_in_constant_p, fold all expressions with constant values
	   even if they aren't C++ constant-expressions.  */
	args[i] = cp_fully_fold (args[i]);
    }

  bool save_ffbcp = force_folding_builtin_constant_p;
  force_folding_builtin_constant_p = true;
  new_call = fold_build_call_array_loc (EXPR_LOCATION (t), TREE_TYPE (t),
					CALL_EXPR_FN (t), nargs, args);
  /* Fold away the NOP_EXPR from fold_builtin_n.  */
  new_call = fold (new_call);
  force_folding_builtin_constant_p = save_ffbcp;
  VERIFY_CONSTANT (new_call);
  return new_call;
}

/* TEMP is the constant value of a temporary object of type TYPE.  Adjust
   the type of the value to match.  */

static tree
adjust_temp_type (tree type, tree temp)
{
  if (TREE_TYPE (temp) == type)
    return temp;
  /* Avoid wrapping an aggregate value in a NOP_EXPR.  */
  if (TREE_CODE (temp) == CONSTRUCTOR)
    return build_constructor (type, CONSTRUCTOR_ELTS (temp));
  gcc_assert (scalarish_type_p (type));
  return cp_fold_convert (type, temp);
}

/* Subroutine of cxx_eval_call_expression.
   We are processing a call expression (either CALL_EXPR or
   AGGR_INIT_EXPR) in the context of CTX.  Evaluate
   all arguments and bind their values to correspondings
   parameters, making up the NEW_CALL context.  */

static void
cxx_bind_parameters_in_call (const constexpr_ctx *ctx, tree t,
                             constexpr_call *new_call,
			     bool *non_constant_p, bool *overflow_p,
			     bool *non_constant_args)
{
  const int nargs = call_expr_nargs (t);
  tree fun = new_call->fundef->decl;
  tree parms = DECL_ARGUMENTS (fun);
  int i;
  tree *p = &new_call->bindings;
  for (i = 0; i < nargs; ++i)
    {
      tree x, arg;
      tree type = parms ? TREE_TYPE (parms) : void_type_node;
      x = get_nth_callarg (t, i);
      /* For member function, the first argument is a pointer to the implied
         object.  For a constructor, it might still be a dummy object, in
         which case we get the real argument from ctx. */
      if (i == 0 && DECL_CONSTRUCTOR_P (fun)
	  && is_dummy_object (x))
	{
	  x = ctx->object;
	  x = cp_build_addr_expr (x, tf_warning_or_error);
	}
      bool lval = false;
      arg = cxx_eval_constant_expression (ctx, x, lval,
					  non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p && ctx->quiet)
	return;
      /* Just discard ellipsis args after checking their constantitude.  */
      if (!parms)
	continue;
      if (*non_constant_p)
	/* Don't try to adjust the type of non-constant args.  */
	goto next;

      /* Make sure the binding has the same type as the parm.  */
      if (TREE_CODE (type) != REFERENCE_TYPE)
	arg = adjust_temp_type (type, arg);
      if (!TREE_CONSTANT (arg))
	*non_constant_args = true;
      *p = build_tree_list (parms, arg);
      p = &TREE_CHAIN (*p);
    next:
      parms = TREE_CHAIN (parms);
    }
}

/* Variables and functions to manage constexpr call expansion context.
   These do not need to be marked for PCH or GC.  */

/* FIXME remember and print actual constant arguments.  */
static vec<tree> call_stack = vNULL;
static int call_stack_tick;
static int last_cx_error_tick;

static bool
push_cx_call_context (tree call)
{
  ++call_stack_tick;
  if (!EXPR_HAS_LOCATION (call))
    SET_EXPR_LOCATION (call, input_location);
  call_stack.safe_push (call);
  if (call_stack.length () > (unsigned) max_constexpr_depth)
    return false;
  return true;
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

/* Subroutine of cxx_eval_constant_expression.
   Evaluate the call expression tree T in the context of OLD_CALL expression
   evaluation.  */

static tree
cxx_eval_call_expression (const constexpr_ctx *ctx, tree t,
			  bool lval,
			  bool *non_constant_p, bool *overflow_p)
{
  location_t loc = EXPR_LOC_OR_LOC (t, input_location);
  tree fun = get_function_named_in_call (t);
  constexpr_call new_call = { NULL, NULL, NULL, 0 };
  bool depth_ok;

  if (fun == NULL_TREE)
    switch (CALL_EXPR_IFN (t))
      {
      case IFN_UBSAN_NULL:
      case IFN_UBSAN_BOUNDS:
      case IFN_UBSAN_VPTR:
	return void_node;
      default:
	if (!ctx->quiet)
	  error_at (loc, "call to internal function");
	*non_constant_p = true;
	return t;
      }

  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      /* Might be a constexpr function pointer.  */
      fun = cxx_eval_constant_expression (ctx, fun,
					  /*lval*/false, non_constant_p,
					  overflow_p);
      STRIP_NOPS (fun);
      if (TREE_CODE (fun) == ADDR_EXPR)
	fun = TREE_OPERAND (fun, 0);
    }
  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      if (!ctx->quiet && !*non_constant_p)
	error_at (loc, "expression %qE does not designate a constexpr "
		  "function", fun);
      *non_constant_p = true;
      return t;
    }
  if (DECL_CLONED_FUNCTION_P (fun))
    fun = DECL_CLONED_FUNCTION (fun);

  if (is_ubsan_builtin_p (fun))
    return void_node;

  if (is_builtin_fn (fun))
    return cxx_eval_builtin_function_call (ctx, t, fun,
					   lval, non_constant_p, overflow_p);
  if (!DECL_DECLARED_CONSTEXPR_P (fun))
    {
      if (!ctx->quiet)
	{
	  error_at (loc, "call to non-constexpr function %qD", fun);
	  explain_invalid_constexpr_fn (fun);
	}
      *non_constant_p = true;
      return t;
    }

  if (fun == current_function_decl)
    {
      /* A call to the current function, i.e.
	 constexpr int f (int i) {
	   constexpr int j = f(i-1);
	   return j;
	 }
	 This would be OK without the constexpr on the declaration of j.  */
      if (!ctx->quiet)
	error_at (loc, "%qD called in a constant expression before its "
		  "definition is complete", fun);
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
      CONSTRUCTOR_NO_IMPLICIT_ZERO (ctor) = true;
      ctx->values->put (new_ctx.object, ctor);
      ctx = &new_ctx;
    }

  /* Shortcut trivial constructor/op=.  */
  if (trivial_fn_p (fun))
    {
      tree init = NULL_TREE;
      if (call_expr_nargs (t) == 2)
	init = convert_from_reference (get_nth_callarg (t, 1));
      else if (TREE_CODE (t) == AGGR_INIT_EXPR
	       && AGGR_INIT_ZERO_FIRST (t))
	init = build_zero_init (DECL_CONTEXT (fun), NULL_TREE, false);
      if (init)
	{
	  tree op = get_nth_callarg (t, 0);
	  if (is_dummy_object (op))
	    op = ctx->object;
	  else
	    op = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (op)), op);
	  tree set = build2 (MODIFY_EXPR, TREE_TYPE (op), op, init);
	  return cxx_eval_constant_expression (ctx, set, lval,
					       non_constant_p, overflow_p);
	}
    }

  /* We can't defer instantiating the function any longer.  */
  if (!DECL_INITIAL (fun)
      && DECL_TEMPLOID_INSTANTIATION (fun))
    {
      ++function_depth;
      instantiate_decl (fun, /*defer_ok*/false, /*expl_inst*/false);
      --function_depth;
    }

  /* If in direct recursive call, optimize definition search.  */
  if (ctx && ctx->call && ctx->call->fundef->decl == fun)
    new_call.fundef = ctx->call->fundef;
  else
    {
      new_call.fundef = retrieve_constexpr_fundef (fun);
      if (new_call.fundef == NULL || new_call.fundef->body == NULL)
        {
	  if (!ctx->quiet)
	    {
	      if (DECL_INITIAL (fun))
		{
		  /* The definition of fun was somehow unsuitable.  */
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

  bool non_constant_args = false;
  cxx_bind_parameters_in_call (ctx, t, &new_call,
			       non_constant_p, overflow_p, &non_constant_args);
  if (*non_constant_p)
    return t;

  depth_ok = push_cx_call_context (t);

  tree result = NULL_TREE;

  constexpr_call *entry = NULL;
  if (depth_ok && !non_constant_args)
    {
      new_call.hash = iterative_hash_template_arg
	(new_call.bindings, constexpr_fundef_hasher::hash (new_call.fundef));

      /* If we have seen this call before, we are done.  */
      maybe_initialize_constexpr_call_table ();
      constexpr_call **slot
	= constexpr_call_table->find_slot (&new_call, INSERT);
      entry = *slot;
      if (entry == NULL)
	{
	  /* We need to keep a pointer to the entry, not just the slot, as the
	     slot can move in the call to cxx_eval_builtin_function_call.  */
	  *slot = entry = ggc_alloc<constexpr_call> ();
	  *entry = new_call;
	}
      /* Calls which are in progress have their result set to NULL
	 so that we can detect circular dependencies.  */
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
	error ("constexpr evaluation depth exceeds maximum of %d (use "
	       "-fconstexpr-depth= to increase the maximum)",
	       max_constexpr_depth);
      *non_constant_p = true;
      result = error_mark_node;
    }
  else
    {
      if (!result || result == error_mark_node)
	{
	  gcc_assert (DECL_SAVED_TREE (fun));
	  tree parms, res;

	  /* Unshare the whole function body.  */
	  tree body = copy_fn (fun, parms, res);

	  /* Associate the bindings with the remapped parms.  */
	  tree bound = new_call.bindings;
	  tree remapped = parms;
	  while (bound)
	    {
	      tree oparm = TREE_PURPOSE (bound);
	      tree arg = TREE_VALUE (bound);
	      gcc_assert (DECL_NAME (remapped) == DECL_NAME (oparm));
	      /* Don't share a CONSTRUCTOR that might be changed.  */
	      arg = unshare_expr (arg);
	      ctx->values->put (remapped, arg);
	      bound = TREE_CHAIN (bound);
	      remapped = DECL_CHAIN (remapped);
	    }
	  /* Add the RESULT_DECL to the values map, too.  */
	  tree slot = NULL_TREE;
	  if (DECL_BY_REFERENCE (res))
	    {
	      slot = AGGR_INIT_EXPR_SLOT (t);
	      tree addr = build_address (slot);
	      addr = build_nop (TREE_TYPE (res), addr);
	      ctx->values->put (res, addr);
	      ctx->values->put (slot, NULL_TREE);
	    }
	  else
	    ctx->values->put (res, NULL_TREE);

	  tree jump_target = NULL_TREE;
	  cxx_eval_constant_expression (ctx, body,
					lval, non_constant_p, overflow_p,
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
	      result = *ctx->values->get (slot ? slot : res);
	      if (result == NULL_TREE && !*non_constant_p)
		{
		  if (!ctx->quiet)
		    error ("constexpr call flows off the end "
			   "of the function");
		  *non_constant_p = true;
		}
	    }

	  /* Remove the parms/result from the values map.  Is it worth
	     bothering to do this when the map itself is only live for
	     one constexpr evaluation?  If so, maybe also clear out
	     other vars from call, maybe in BIND_EXPR handling?  */
	  ctx->values->remove (res);
	  if (slot)
	    ctx->values->remove (slot);
	  for (tree parm = parms; parm; parm = TREE_CHAIN (parm))
	    ctx->values->remove (parm);
	}

      if (result == error_mark_node)
	*non_constant_p = true;
      if (*non_constant_p)
	result = error_mark_node;
      else if (!result)
	result = void_node;
      if (entry)
	entry->result = result;
    }

  pop_cx_call_context ();
  return unshare_expr (result);
}

/* FIXME speed this up, it's taking 16% of compile time on sieve testcase.  */

bool
reduced_constant_expression_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case PTRMEM_CST:
      /* Even if we can't lower this yet, it's constant.  */
      return true;

    case CONSTRUCTOR:
      /* And we need to handle PTRMEM_CST wrapped in a CONSTRUCTOR.  */
      tree elt; unsigned HOST_WIDE_INT idx;
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (t), idx, elt)
	if (!reduced_constant_expression_p (elt))
	  return false;
      return true;

    default:
      /* FIXME are we calling this too much?  */
      return initializer_constant_valid_p (t, TREE_TYPE (t)) != NULL_TREE;
    }
}

/* Some expressions may have constant operands but are not constant
   themselves, such as 1/0.  Call this function (or rather, the macro
   following it) to check for that condition.

   We only call this in places that require an arithmetic constant, not in
   places where we might have a non-constant expression that can be a
   component of a constant expression, such as the address of a constexpr
   variable that might be dereferenced later.  */

static bool
verify_constant (tree t, bool allow_non_constant, bool *non_constant_p,
		 bool *overflow_p)
{
  if (!*non_constant_p && !reduced_constant_expression_p (t))
    {
      if (!allow_non_constant)
	error ("%q+E is not a constant expression", t);
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
	permerror (loc, "right operand of shift expression %q+E is >= than "
		   "the precision of the left operand",
		   build2_loc (loc, code, type, lhs, rhs));
      return (!flag_permissive || ctx->quiet);
    }

  /* The value of E1 << E2 is E1 left-shifted E2 bit positions; [...]
     if E1 has a signed type and non-negative value, and E1x2^E2 is
     representable in the corresponding unsigned type of the result type,
     then that value, converted to the result type, is the resulting value;
     otherwise, the behavior is undefined.  */
  if (code == LSHIFT_EXPR && !TYPE_UNSIGNED (lhstype)
      && (cxx_dialect >= cxx11))
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
  tree arg = cxx_eval_constant_expression (ctx, orig_arg, /*lval*/false,
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

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for binary expressions.  */

static tree
cxx_eval_binary_expression (const constexpr_ctx *ctx, tree t,
			    bool /*lval*/,
			    bool *non_constant_p, bool *overflow_p)
{
  tree r = NULL_TREE;
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;
  lhs = cxx_eval_constant_expression (ctx, orig_lhs, /*lval*/false,
				      non_constant_p, overflow_p);
  /* Don't VERIFY_CONSTANT here, it's unnecessary and will break pointer
     subtraction.  */
  if (*non_constant_p)
    return t;
  rhs = cxx_eval_constant_expression (ctx, orig_rhs, /*lval*/false,
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
	r = constant_boolean_node (cp_tree_equal (lhs, rhs) == is_code_eq,
				   type);
      else if ((TREE_CODE (lhs) == PTRMEM_CST
		|| TREE_CODE (rhs) == PTRMEM_CST)
	       && (null_member_pointer_value_p (lhs)
		   || null_member_pointer_value_p (rhs)))
	r = constant_boolean_node (!is_code_eq, type);
    }

  if (r == NULL_TREE)
    r = fold_binary_loc (loc, code, type, lhs, rhs);

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
  bool ptr = POINTER_TYPE_P (TREE_TYPE (lhs));
  if (!ptr)
    VERIFY_CONSTANT (r);
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate condition expressions.  Dead branches are not
   looked into.  */

static tree
cxx_eval_conditional_expression (const constexpr_ctx *ctx, tree t,
				 bool lval,
				 bool *non_constant_p, bool *overflow_p,
				 tree *jump_target)
{
  tree val = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					   /*lval*/false,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (val);
  /* Don't VERIFY_CONSTANT the other operands.  */
  if (integer_zerop (val))
    return cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 2),
					 lval,
					 non_constant_p, overflow_p,
					 jump_target);
  return cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1),
				       lval,
				       non_constant_p, overflow_p,
				       jump_target);
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

/* Returns the index of the constructor_elt of ARY which matches DINDEX, or -1
   if none.  If INSERT is true, insert a matching element rather than fail.  */

static HOST_WIDE_INT
find_array_ctor_elt (tree ary, tree dindex, bool insert = false)
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
      tree cindex = (*elts)[end-1].index;
      if (TREE_CODE (cindex) == INTEGER_CST
	  && compare_tree_int (cindex, end-1) == 0)
	{
	  if (i < end)
	    return i;
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
      if (cmp < 0)
	end = middle;
      else if (cmp > 0)
	begin = middle + 1;
      else
	{
	  if (insert && TREE_CODE (idx) == RANGE_EXPR)
	    {
	      /* We need to split the range.  */
	      constructor_elt e;
	      tree lo = TREE_OPERAND (idx, 0);
	      tree hi = TREE_OPERAND (idx, 1);
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
		  e.value = unshare_expr (elt.value);
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
		  e.value = unshare_expr (elt.value);
		  vec_safe_insert (CONSTRUCTOR_ELTS (ary), middle+1, e);
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


/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a reference to an array slot.  */

static tree
cxx_eval_array_reference (const constexpr_ctx *ctx, tree t,
			  bool lval,
			  bool *non_constant_p, bool *overflow_p)
{
  tree oldary = TREE_OPERAND (t, 0);
  tree ary = cxx_eval_constant_expression (ctx, oldary,
					   lval,
					   non_constant_p, overflow_p);
  tree index, oldidx;
  HOST_WIDE_INT i;
  tree elem_type;
  unsigned len, elem_nchars = 1;
  if (*non_constant_p)
    return t;
  oldidx = TREE_OPERAND (t, 1);
  index = cxx_eval_constant_expression (ctx, oldidx,
					false,
					non_constant_p, overflow_p);
  VERIFY_CONSTANT (index);
  if (lval && ary == oldary && index == oldidx)
    return t;
  else if (lval)
    return build4 (ARRAY_REF, TREE_TYPE (t), ary, index, NULL, NULL);
  elem_type = TREE_TYPE (TREE_TYPE (ary));
  if (TREE_CODE (ary) == CONSTRUCTOR)
    len = CONSTRUCTOR_NELTS (ary);
  else if (TREE_CODE (ary) == STRING_CST)
    {
      elem_nchars = (TYPE_PRECISION (elem_type)
		     / TYPE_PRECISION (char_type_node));
      len = (unsigned) TREE_STRING_LENGTH (ary) / elem_nchars;
    }
  else
    {
      /* We can't do anything with other tree codes, so use
	 VERIFY_CONSTANT to complain and fail.  */
      VERIFY_CONSTANT (ary);
      gcc_unreachable ();
    }

  if (!tree_fits_shwi_p (index)
      || (i = tree_to_shwi (index)) < 0)
    {
      if (!ctx->quiet)
	error ("negative array subscript");
      *non_constant_p = true;
      return t;
    }

  tree nelts = array_type_nelts_top (TREE_TYPE (ary));
  /* For VLAs, the number of elements won't be an integer constant.  */
  nelts = cxx_eval_constant_expression (ctx, nelts, false, non_constant_p,
					overflow_p);
  VERIFY_CONSTANT (nelts);
  if (!tree_int_cst_lt (index, nelts))
    {
      if (!ctx->quiet)
	error ("array subscript out of bound");
      *non_constant_p = true;
      return t;
    }

  bool found;
  if (TREE_CODE (ary) == CONSTRUCTOR)
    {
      HOST_WIDE_INT ix = find_array_ctor_elt (ary, index);
      found = (ix >= 0);
      if (found)
	i = ix;
    }
  else
    found = (i < len);

  if (!found)
    {
      if (TREE_CODE (ary) == CONSTRUCTOR
	  && CONSTRUCTOR_NO_IMPLICIT_ZERO (ary))
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
	 initializer, it's value-initialized.  */
      tree val = build_value_init (elem_type, tf_warning_or_error);
      return cxx_eval_constant_expression (ctx, val, lval, non_constant_p,
					   overflow_p);
    }

  if (TREE_CODE (ary) == CONSTRUCTOR)
    return (*CONSTRUCTOR_ELTS (ary))[i].value;
  else if (elem_nchars == 1)
    return build_int_cst (cv_unqualified (TREE_TYPE (TREE_TYPE (ary))),
			  TREE_STRING_POINTER (ary)[i]);
  else
    {
      tree type = cv_unqualified (TREE_TYPE (TREE_TYPE (ary)));
      return native_interpret_expr (type, (const unsigned char *)
					  TREE_STRING_POINTER (ary)
					  + i * elem_nchars, elem_nchars);
    }
  /* Don't VERIFY_CONSTANT here.  */
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type.  */

static tree
cxx_eval_component_reference (const constexpr_ctx *ctx, tree t,
			      bool lval,
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
  if (TREE_CODE (whole) == PTRMEM_CST)
    whole = cplus_expand_constant (whole);
  if (whole == orig_whole)
    return t;
  if (lval)
    return fold_build3 (COMPONENT_REF, TREE_TYPE (t),
			whole, part, NULL_TREE);
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!ctx->quiet)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (DECL_MUTABLE_P (part))
    {
      if (!ctx->quiet)
	error ("mutable %qD is not usable in a constant expression", part);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      if (field == part)
	{
	  if (value)
	    return value;
	  else
	    /* We're in the middle of initializing it.  */
	    break;
	}
    }
  if (TREE_CODE (TREE_TYPE (whole)) == UNION_TYPE
      && CONSTRUCTOR_NELTS (whole) > 0)
    {
      /* DR 1188 says we don't have to deal with this.  */
      if (!ctx->quiet)
	error ("accessing %qD member instead of initialized %qD member in "
	       "constant expression", part, CONSTRUCTOR_ELT (whole, 0)->index);
      *non_constant_p = true;
      return t;
    }

  /* We only create a CONSTRUCTOR for a subobject when we modify it, so empty
     classes never get represented; throw together a value now.  */
  if (is_really_empty_class (TREE_TYPE (t)))
    return build_constructor (TREE_TYPE (t), NULL);

  if (CONSTRUCTOR_NO_IMPLICIT_ZERO (whole))
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
			bool lval,
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

  if (TREE_CODE (whole) == VECTOR_CST)
    return fold_ternary (BIT_FIELD_REF, TREE_TYPE (t), whole,
			 TREE_OPERAND (t, 1), TREE_OPERAND (t, 2));

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

/* Subroutine of cxx_eval_constant_expression.
   Evaluate a short-circuited logical expression T in the context
   of a given constexpr CALL.  BAILOUT_VALUE is the value for
   early return.  CONTINUE_VALUE is used here purely for
   sanity check purposes.  */

static tree
cxx_eval_logical_expression (const constexpr_ctx *ctx, tree t,
                             tree bailout_value, tree continue_value,
			     bool lval,
			     bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree lhs = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					   lval,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (lhs);
  if (tree_int_cst_equal (lhs, bailout_value))
    return lhs;
  gcc_assert (tree_int_cst_equal (lhs, continue_value));
  r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1),
				    lval, non_constant_p,
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
  tree type = cv_unqualified (TREE_TYPE (t));
  if (TREE_CODE (t) == CALL_EXPR || TREE_CODE (t) == AGGR_INIT_EXPR)
    {
      /* A constructor call has void type, so we need to look deeper.  */
      tree fn = get_function_named_in_call (t);
      if (fn && TREE_CODE (fn) == FUNCTION_DECL
	  && DECL_CXX_CONSTRUCTOR_P (fn))
	type = DECL_CONTEXT (fn);
    }
  return type;
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
      && TREE_CODE (index) != FIELD_DECL)
    /* This won't have an element in the new CONSTRUCTOR.  */
    return;

  tree type = initialized_type (value);
  if (!AGGREGATE_TYPE_P (type) && !VECTOR_TYPE_P (type))
    /* A non-aggregate member doesn't get its own CONSTRUCTOR.  */
    return;

  /* The sub-aggregate initializer might contain a placeholder;
     update object to refer to the subobject and ctor to refer to
     the (newly created) sub-initializer.  */
  if (ctx->object)
    new_ctx.object = build_ctor_subob_ref (index, type, ctx->object);
  tree elt = build_constructor (type, NULL);
  CONSTRUCTOR_NO_IMPLICIT_ZERO (elt) = true;
  new_ctx.ctor = elt;

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
    gcc_assert (same_type_ignoring_top_level_qualifiers_p
		(type, TREE_TYPE (ctx->object)));
  gcc_assert (!ctx->object || !DECL_P (ctx->object)
	      || *(ctx->values->get (ctx->object)) == ctx->ctor);
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T denotes a C-style array or a C-style
   aggregate.  Reduce it to a constant expression.  */

static tree
cxx_eval_bare_aggregate (const constexpr_ctx *ctx, tree t,
			 bool lval,
			 bool *non_constant_p, bool *overflow_p)
{
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
  bool changed = false;
  gcc_assert (!BRACE_ENCLOSED_INITIALIZER_P (t));

  verify_ctor_sanity (ctx, TREE_TYPE (t));
  vec<constructor_elt, va_gc> **p = &CONSTRUCTOR_ELTS (ctx->ctor);
  vec_alloc (*p, vec_safe_length (v));

  unsigned i;
  tree index, value;
  bool constant_p = true;
  bool side_effects_p = false;
  FOR_EACH_CONSTRUCTOR_ELT (v, i, index, value)
    {
      tree orig_value = value;
      constexpr_ctx new_ctx;
      init_subob_ctx (ctx, new_ctx, index, value);
      if (new_ctx.ctor != ctx->ctor)
	/* If we built a new CONSTRUCTOR, attach it now so that other
	   initializers can refer to it.  */
	CONSTRUCTOR_APPEND_ELT (*p, index, new_ctx.ctor);
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
      else if (index
	       && (TREE_CODE (index) == NOP_EXPR
		   || TREE_CODE (index) == POINTER_PLUS_EXPR))
	{
	  /* This is an initializer for an empty base; now that we've
	     checked that it's constant, we can ignore it.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (index))));
	  changed = true;
	}
      else if (new_ctx.ctor != ctx->ctor)
	{
	  /* We appended this element above; update the value.  */
	  gcc_assert ((*p)->last().index == index);
	  (*p)->last().value = elt;
	}
      else
	CONSTRUCTOR_APPEND_ELT (*p, index, elt);
    }
  if (*non_constant_p || !changed)
    return t;
  t = ctx->ctor;
  /* We're done building this CONSTRUCTOR, so now we can interpret an
     element without an explicit initializer as value-initialized.  */
  CONSTRUCTOR_NO_IMPLICIT_ZERO (t) = false;
  TREE_CONSTANT (t) = constant_p;
  TREE_SIDE_EFFECTS (t) = side_effects_p;
  if (VECTOR_TYPE_P (TREE_TYPE (t)))
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
		     bool value_init, bool lval,
		     bool *non_constant_p, bool *overflow_p)
{
  tree elttype = TREE_TYPE (atype);
  unsigned HOST_WIDE_INT max = tree_to_uhwi (array_type_nelts_top (atype));
  verify_ctor_sanity (ctx, atype);
  vec<constructor_elt, va_gc> **p = &CONSTRUCTOR_ELTS (ctx->ctor);
  vec_alloc (*p, max + 1);
  bool pre_init = false;
  tree pre_init_elt = NULL_TREE;
  unsigned HOST_WIDE_INT i;

  /* For the default constructor, build up a call to the default
     constructor of the element type.  We only need to handle class types
     here, as for a constructor to be constexpr, all members must be
     initialized, which for a defaulted default constructor means they must
     be of a class type with a constexpr default constructor.  */
  if (TREE_CODE (elttype) == ARRAY_TYPE)
    /* We only do this at the lowest level.  */;
  else if (value_init)
    {
      init = build_value_init (elttype, tf_warning_or_error);
      pre_init = true;
    }
  else if (!init)
    {
      vec<tree, va_gc> *argvec = make_tree_vector ();
      init = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&argvec, elttype, LOOKUP_NORMAL,
					tf_warning_or_error);
      release_tree_vector (argvec);
      init = build_aggr_init_expr (TREE_TYPE (init), init);
      pre_init = true;
    }

  for (i = 0; i < max; ++i)
    {
      tree idx = build_int_cst (size_type_node, i);
      tree eltinit;
      constexpr_ctx new_ctx;
      init_subob_ctx (ctx, new_ctx, idx, pre_init ? init : elttype);
      if (new_ctx.ctor != ctx->ctor)
	CONSTRUCTOR_APPEND_ELT (*p, idx, new_ctx.ctor);
      if (TREE_CODE (elttype) == ARRAY_TYPE)
	{
	  /* A multidimensional array; recurse.  */
	  if (value_init || init == NULL_TREE)
	    eltinit = NULL_TREE;
	  else
	    eltinit = cp_build_array_ref (input_location, init, idx,
					  tf_warning_or_error);
	  eltinit = cxx_eval_vec_init_1 (&new_ctx, elttype, eltinit, value_init,
					 lval,
					 non_constant_p, overflow_p);
	}
      else if (pre_init)
	{
	  /* Initializing an element using value or default initialization
	     we just pre-built above.  */
	  if (pre_init_elt == NULL_TREE)
	    pre_init_elt
	      = cxx_eval_constant_expression (&new_ctx, init, lval,
					      non_constant_p, overflow_p);
	  eltinit = pre_init_elt;
	  /* Don't reuse the result of cxx_eval_constant_expression
	     call if it isn't a constant initializer or if it requires
	     relocations.  */
	  if (initializer_constant_valid_p (pre_init_elt,
					    TREE_TYPE (pre_init_elt))
	      != null_pointer_node)
	    pre_init_elt = NULL_TREE;
	}
      else
	{
	  /* Copying an element.  */
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (atype, TREE_TYPE (init)));
	  eltinit = cp_build_array_ref (input_location, init, idx,
					tf_warning_or_error);
	  if (!real_lvalue_p (init))
	    eltinit = move (eltinit);
	  eltinit = force_rvalue (eltinit, tf_warning_or_error);
	  eltinit = (cxx_eval_constant_expression
		     (&new_ctx, eltinit, lval,
		      non_constant_p, overflow_p));
	}
      if (*non_constant_p && !ctx->quiet)
	break;
      if (new_ctx.ctor != ctx->ctor)
	{
	  /* We appended this element above; update the value.  */
	  gcc_assert ((*p)->last().index == idx);
	  (*p)->last().value = eltinit;
	}
      else
	CONSTRUCTOR_APPEND_ELT (*p, idx, eltinit);
    }

  if (!*non_constant_p)
    {
      init = ctx->ctor;
      CONSTRUCTOR_NO_IMPLICIT_ZERO (init) = false;
    }
  return init;
}

static tree
cxx_eval_vec_init (const constexpr_ctx *ctx, tree t,
		   bool lval,
		   bool *non_constant_p, bool *overflow_p)
{
  tree atype = TREE_TYPE (t);
  tree init = VEC_INIT_EXPR_INIT (t);
  tree r = cxx_eval_vec_init_1 (ctx, atype, init,
				VEC_INIT_EXPR_VALUE_INIT (t),
				lval, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  else
    return r;
}

/* A less strict version of fold_indirect_ref_1, which requires cv-quals to
   match.  We want to be less strict for simple *& folding; if we have a
   non-const temporary that we access through a const pointer, that should
   work.  We handle this here rather than change fold_indirect_ref_1
   because we're dealing with things like ADDR_EXPR of INTEGER_CST which
   don't really make sense outside of constant expression evaluation.  Also
   we want to allow folding to COMPONENT_REF, which could cause trouble
   with TBAA in fold_indirect_ref_1.

   Try to keep this function synced with fold_indirect_ref_1.  */

static tree
cxx_fold_indirect_ref (location_t loc, tree type, tree op0, bool *empty_base)
{
  tree sub, subtype;

  sub = op0;
  STRIP_NOPS (sub);
  subtype = TREE_TYPE (sub);
  if (!POINTER_TYPE_P (subtype))
    return NULL_TREE;

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);

      /* *&CONST_DECL -> to the value of the const decl.  */
      if (TREE_CODE (op) == CONST_DECL)
	return DECL_INITIAL (op);
      /* *&p => p;  make sure to handle *&"str"[cst] here.  */
      if (same_type_ignoring_top_level_qualifiers_p (optype, type)
	  /* Also handle the case where the desired type is an array of unknown
	     bounds because the variable has had its bounds deduced since the
	     ADDR_EXPR was created.  */
	  || (TREE_CODE (type) == ARRAY_TYPE
	      && TREE_CODE (optype) == ARRAY_TYPE
	      && TYPE_DOMAIN (type) == NULL_TREE
	      && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (optype),
							    TREE_TYPE (type))))
	{
	  tree fop = fold_read_from_constant_string (op);
	  if (fop)
	    return fop;
	  else
	    return op;
	}
      /* *(foo *)&fooarray => fooarray[0] */
      else if (TREE_CODE (optype) == ARRAY_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	{
	  tree type_domain = TYPE_DOMAIN (optype);
	  tree min_val = size_zero_node;
	  if (type_domain && TYPE_MIN_VALUE (type_domain))
	    min_val = TYPE_MIN_VALUE (type_domain);
	  return build4_loc (loc, ARRAY_REF, type, op, min_val,
			     NULL_TREE, NULL_TREE);
	}
      /* *(foo *)&complexfoo => __real__ complexfoo */
      else if (TREE_CODE (optype) == COMPLEX_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	return fold_build1_loc (loc, REALPART_EXPR, type, op);
      /* *(foo *)&vectorfoo => BIT_FIELD_REF<vectorfoo,...> */
      else if (VECTOR_TYPE_P (optype)
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	{
	  tree part_width = TYPE_SIZE (type);
	  tree index = bitsize_int (0);
	  return fold_build3_loc (loc, BIT_FIELD_REF, type, op, part_width, index);
	}
      /* Also handle conversion to an empty base class, which
	 is represented with a NOP_EXPR.  */
      else if (is_empty_class (type)
	       && CLASS_TYPE_P (optype)
	       && DERIVED_FROM_P (type, optype))
	{
	  *empty_base = true;
	  return op;
	}
      /* *(foo *)&struct_with_foo_field => COMPONENT_REF */
      else if (RECORD_OR_UNION_TYPE_P (optype))
	{
	  tree field = TYPE_FIELDS (optype);
	  for (; field; field = DECL_CHAIN (field))
	    if (TREE_CODE (field) == FIELD_DECL
		&& integer_zerop (byte_position (field))
		&& (same_type_ignoring_top_level_qualifiers_p
		    (TREE_TYPE (field), type)))
	      {
		return fold_build3 (COMPONENT_REF, type, op, field, NULL_TREE);
		break;
	      }
	}
    }
  else if (TREE_CODE (sub) == POINTER_PLUS_EXPR
	   && TREE_CODE (TREE_OPERAND (sub, 1)) == INTEGER_CST)
    {
      tree op00 = TREE_OPERAND (sub, 0);
      tree op01 = TREE_OPERAND (sub, 1);

      STRIP_NOPS (op00);
      if (TREE_CODE (op00) == ADDR_EXPR)
	{
	  tree op00type;
	  op00 = TREE_OPERAND (op00, 0);
	  op00type = TREE_TYPE (op00);

	  /* ((foo*)&vectorfoo)[1] => BIT_FIELD_REF<vectorfoo,...> */
	  if (VECTOR_TYPE_P (op00type)
	      && (same_type_ignoring_top_level_qualifiers_p
		  (type, TREE_TYPE (op00type))))
	    {
	      HOST_WIDE_INT offset = tree_to_shwi (op01);
	      tree part_width = TYPE_SIZE (type);
	      unsigned HOST_WIDE_INT part_widthi = tree_to_shwi (part_width)/BITS_PER_UNIT;
	      unsigned HOST_WIDE_INT indexi = offset * BITS_PER_UNIT;
	      tree index = bitsize_int (indexi);

	      if (offset / part_widthi < TYPE_VECTOR_SUBPARTS (op00type))
		return fold_build3_loc (loc,
					BIT_FIELD_REF, type, op00,
					part_width, index);

	    }
	  /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
	  else if (TREE_CODE (op00type) == COMPLEX_TYPE
		   && (same_type_ignoring_top_level_qualifiers_p
		       (type, TREE_TYPE (op00type))))
	    {
	      tree size = TYPE_SIZE_UNIT (type);
	      if (tree_int_cst_equal (size, op01))
		return fold_build1_loc (loc, IMAGPART_EXPR, type, op00);
	    }
	  /* ((foo *)&fooarray)[1] => fooarray[1] */
	  else if (TREE_CODE (op00type) == ARRAY_TYPE
		   && (same_type_ignoring_top_level_qualifiers_p
		       (type, TREE_TYPE (op00type))))
	    {
	      tree type_domain = TYPE_DOMAIN (op00type);
	      tree min_val = size_zero_node;
	      if (type_domain && TYPE_MIN_VALUE (type_domain))
		min_val = TYPE_MIN_VALUE (type_domain);
	      op01 = size_binop_loc (loc, EXACT_DIV_EXPR, op01,
				     TYPE_SIZE_UNIT (type));
	      op01 = size_binop_loc (loc, PLUS_EXPR, op01, min_val);
	      return build4_loc (loc, ARRAY_REF, type, op00, op01,
				 NULL_TREE, NULL_TREE);
	    }
	  /* Also handle conversion to an empty base class, which
	     is represented with a NOP_EXPR.  */
	  else if (is_empty_class (type)
		   && CLASS_TYPE_P (op00type)
		   && DERIVED_FROM_P (type, op00type))
	    {
	      *empty_base = true;
	      return op00;
	    }
	  /* ((foo *)&struct_with_foo_field)[1] => COMPONENT_REF */
	  else if (RECORD_OR_UNION_TYPE_P (op00type))
	    {
	      tree field = TYPE_FIELDS (op00type);
	      for (; field; field = DECL_CHAIN (field))
		if (TREE_CODE (field) == FIELD_DECL
		    && tree_int_cst_equal (byte_position (field), op01)
		    && (same_type_ignoring_top_level_qualifiers_p
			(TREE_TYPE (field), type)))
		  {
		    return fold_build3 (COMPONENT_REF, type, op00,
					field, NULL_TREE);
		    break;
		  }
	    }
	}
    }
  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  else if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
	   && (same_type_ignoring_top_level_qualifiers_p
	       (type, TREE_TYPE (TREE_TYPE (subtype)))))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      tree newsub = cxx_fold_indirect_ref (loc, TREE_TYPE (subtype), sub, NULL);
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
		       bool lval,
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
  tree r = cxx_fold_indirect_ref (EXPR_LOCATION (t), TREE_TYPE (t), orig_op0,
				  &empty_base);
  if (!r)
    {
      /* If that didn't work, evaluate the operand first.  */
      tree op0 = cxx_eval_constant_expression (ctx, orig_op0,
					       /*lval*/false, non_constant_p,
					       overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p)
	return t;

      r = cxx_fold_indirect_ref (EXPR_LOCATION (t), TREE_TYPE (t), op0,
				 &empty_base);
      if (r == NULL_TREE)
	{
	  /* We couldn't fold to a constant value.  Make sure it's not
	     something we should have been able to fold.  */
	  tree sub = op0;
	  STRIP_NOPS (sub);
	  if (TREE_CODE (sub) == ADDR_EXPR)
	    {
	      gcc_assert (!same_type_ignoring_top_level_qualifiers_p
			  (TREE_TYPE (TREE_TYPE (sub)), TREE_TYPE (t)));
	      /* DR 1188 says we don't have to deal with this.  */
	      if (!ctx->quiet)
		error ("accessing value of %qE through a %qT glvalue in a "
		       "constant expression", build_fold_indirect_ref (sub),
		       TREE_TYPE (t));
	      *non_constant_p = true;
	      return t;
	    }

	  if (lval && op0 != orig_op0)
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

  /* If we're pulling out the value of an empty base, make sure
     that the whole object is constant and then return an empty
     CONSTRUCTOR.  */
  if (empty_base && !lval)
    {
      VERIFY_CONSTANT (r);
      r = build_constructor (TREE_TYPE (t), NULL);
      TREE_CONSTANT (r) = true;
    }

  return r;
}

/* Complain about R, a VAR_DECL, not being usable in a constant expression.
   Shared between potential_constant_expression and
   cxx_eval_constant_expression.  */

static void
non_const_var_error (tree r)
{
  tree type = TREE_TYPE (r);
  error ("the value of %qD is not usable in a constant "
	 "expression", r);
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
			     bool lval,
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

bool
var_in_constexpr_fn (tree t)
{
  tree ctx = DECL_CONTEXT (t);
  return (cxx_dialect >= cxx14 && ctx && TREE_CODE (ctx) == FUNCTION_DECL
	  && DECL_DECLARED_CONSTEXPR_P (ctx));
}

/* Evaluate an INIT_EXPR or MODIFY_EXPR.  */

static tree
cxx_eval_store_expression (const constexpr_ctx *ctx, tree t,
			   bool lval,
			   bool *non_constant_p, bool *overflow_p)
{
  constexpr_ctx new_ctx = *ctx;

  tree init = TREE_OPERAND (t, 1);
  if (TREE_CLOBBER_P (init))
    /* Just ignore clobbers.  */
    return void_node;

  /* First we figure out where we're storing to.  */
  tree target = TREE_OPERAND (t, 0);
  tree type = TREE_TYPE (target);
  target = cxx_eval_constant_expression (ctx, target,
					 true,
					 non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (target), type))
    {
      /* For initialization of an empty base, the original target will be
         *(base*)this, which the above evaluation resolves to the object
	 argument, which has the derived type rather than the base type.  In
	 this situation, just evaluate the initializer and return, since
	 there's no actual data to store.  */
      gcc_assert (is_empty_class (type));
      return cxx_eval_constant_expression (ctx, init, false,
					   non_constant_p, overflow_p);
    }

  /* And then find the underlying variable.  */
  vec<tree,va_gc> *refs = make_tree_vector();
  tree object = NULL_TREE;
  for (tree probe = target; object == NULL_TREE; )
    {
      switch (TREE_CODE (probe))
	{
	case BIT_FIELD_REF:
	case COMPONENT_REF:
	case ARRAY_REF:
	  vec_safe_push (refs, TREE_OPERAND (probe, 1));
	  vec_safe_push (refs, TREE_TYPE (probe));
	  probe = TREE_OPERAND (probe, 0);
	  break;

	default:
	  object = probe;
	}
    }

  /* And then find/build up our initializer for the path to the subobject
     we're initializing.  */
  tree *valp;
  if (DECL_P (object))
    valp = ctx->values->get (object);
  else
    valp = NULL;
  if (!valp)
    {
      /* A constant-expression cannot modify objects from outside the
	 constant-expression.  */
      if (!ctx->quiet)
	error ("modification of %qE is not a constant-expression", object);
      *non_constant_p = true;
      return t;
    }
  type = TREE_TYPE (object);
  bool no_zero_init = true;

  vec<tree,va_gc> *ctors = make_tree_vector ();
  while (!refs->is_empty())
    {
      if (*valp == NULL_TREE)
	{
	  *valp = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_IMPLICIT_ZERO (*valp) = no_zero_init;
	}
      /* If the value of object is already zero-initialized, any new ctors for
	 subobjects will also be zero-initialized.  */
      no_zero_init = CONSTRUCTOR_NO_IMPLICIT_ZERO (*valp);

      vec_safe_push (ctors, *valp);

      enum tree_code code = TREE_CODE (type);
      type = refs->pop();
      tree index = refs->pop();

      constructor_elt *cep = NULL;
      if (code == ARRAY_TYPE)
	{
	  HOST_WIDE_INT i
	    = find_array_ctor_elt (*valp, index, /*insert*/true);
	  gcc_assert (i >= 0);
	  cep = CONSTRUCTOR_ELT (*valp, i);
	  gcc_assert (TREE_CODE (cep->index) != RANGE_EXPR);
	}
      else
	{
	  gcc_assert (TREE_CODE (index) == FIELD_DECL);
	  for (unsigned HOST_WIDE_INT idx = 0;
	       vec_safe_iterate (CONSTRUCTOR_ELTS (*valp), idx, &cep);
	       idx++)
	    if (index == cep->index)
	      break;
	  if (!cep)
	    {
	      constructor_elt ce = { index, NULL_TREE };
	      cep = vec_safe_push (CONSTRUCTOR_ELTS (*valp), ce);
	    }
	}
      valp = &cep->value;
    }
  release_tree_vector (refs);

  if (AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type))
    {
      /* Create a new CONSTRUCTOR in case evaluation of the initializer
	 wants to modify it.  */
      if (*valp == NULL_TREE)
	{
	  *valp = new_ctx.ctor = build_constructor (type, NULL);
	  CONSTRUCTOR_NO_IMPLICIT_ZERO (new_ctx.ctor) = no_zero_init;
	}
      else
	new_ctx.ctor = *valp;
      new_ctx.object = target;
    }

  init = cxx_eval_constant_expression (&new_ctx, init, false,
				       non_constant_p, overflow_p);
  /* Don't share a CONSTRUCTOR that might be changed later.  */
  init = unshare_expr (init);
  if (target == object)
    /* The hash table might have moved since the get earlier.  */
    valp = ctx->values->get (object);

  if (TREE_CODE (init) == CONSTRUCTOR)
    {
      /* An outer ctx->ctor might be pointing to *valp, so replace
	 its contents.  */
      CONSTRUCTOR_ELTS (*valp) = CONSTRUCTOR_ELTS (init);
      TREE_CONSTANT (*valp) = TREE_CONSTANT (init);
      TREE_SIDE_EFFECTS (*valp) = TREE_SIDE_EFFECTS (init);
    }
  else
    *valp = init;

  /* Update TREE_CONSTANT and TREE_SIDE_EFFECTS on enclosing
     CONSTRUCTORs, if any.  */
  tree elt;
  unsigned i;
  bool c = TREE_CONSTANT (init);
  bool s = TREE_SIDE_EFFECTS (init);
  if (!c || s)
    FOR_EACH_VEC_SAFE_ELT (ctors, i, elt)
      {
	if (!c)
	  TREE_CONSTANT (elt) = false;
	if (s)
	  TREE_SIDE_EFFECTS (elt) = true;
      }
  release_tree_vector (ctors);

  if (*non_constant_p)
    return t;
  else if (lval)
    return target;
  else
    return init;
}

/* Evaluate a ++ or -- expression.  */

static tree
cxx_eval_increment_expression (const constexpr_ctx *ctx, tree t,
			      bool lval,
			      bool *non_constant_p, bool *overflow_p)
{
  enum tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);
  tree op = TREE_OPERAND (t, 0);
  tree offset = TREE_OPERAND (t, 1);
  gcc_assert (TREE_CONSTANT (offset));

  /* The operand as an lvalue.  */
  op = cxx_eval_constant_expression (ctx, op, true,
				     non_constant_p, overflow_p);

  /* The operand as an rvalue.  */
  tree val = rvalue (op);
  val = cxx_eval_constant_expression (ctx, val, false,
				      non_constant_p, overflow_p);
  /* Don't VERIFY_CONSTANT if this might be dealing with a pointer to
     a local array in a constexpr function.  */
  bool ptr = POINTER_TYPE_P (TREE_TYPE (val));
  if (!ptr)
    VERIFY_CONSTANT (val);

  /* The modified value.  */
  bool inc = (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR);
  tree mod;
  if (POINTER_TYPE_P (type))
    {
      /* The middle end requires pointers to use POINTER_PLUS_EXPR.  */
      offset = convert_to_ptrofftype (offset);
      if (!inc)
	offset = fold_build1 (NEGATE_EXPR, TREE_TYPE (offset), offset);
      mod = fold_build2 (POINTER_PLUS_EXPR, type, val, offset);
    }
  else
    mod = fold_build2 (inc ? PLUS_EXPR : MINUS_EXPR, type, val, offset);
  if (!ptr)
    VERIFY_CONSTANT (mod);

  /* Storing the modified value.  */
  tree store = build2 (MODIFY_EXPR, type, op, mod);
  cxx_eval_constant_expression (ctx, store,
				true, non_constant_p, overflow_p);

  /* And the value of the expression.  */
  if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
    {
      /* Prefix ops are lvalues.  */
      if (lval)
	return op;
      else
	/* But we optimize when the caller wants an rvalue.  */
	return mod;
    }
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
    && TREE_CODE (*jump_target) == LABEL_DECL
    && LABEL_DECL_BREAK (*jump_target);
}

static bool
continues (tree *jump_target)
{
  return *jump_target
    && TREE_CODE (*jump_target) == LABEL_DECL
    && LABEL_DECL_CONTINUE (*jump_target);
}

static bool
switches (tree *jump_target)
{
  return *jump_target
    && TREE_CODE (*jump_target) == INTEGER_CST;
}

/* Subroutine of cxx_eval_statement_list.  Determine whether the statement
   at I matches *jump_target.  If we're looking for a case label and we see
   the default label, copy I into DEFAULT_LABEL.  */

static bool
label_matches (tree *jump_target, tree_stmt_iterator i,
	       tree_stmt_iterator& default_label)
{
  tree stmt = tsi_stmt (i);
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
	  if (!CASE_LOW (stmt))
	    default_label = i;
	  else if (tree_int_cst_equal (*jump_target, CASE_LOW (stmt)))
	    return true;
	}
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
  tree_stmt_iterator i;
  tree_stmt_iterator default_label = tree_stmt_iterator();
  tree local_target;
  /* In a statement-expression we want to return the last value.  */
  tree r = NULL_TREE;
  if (!jump_target)
    {
      local_target = NULL_TREE;
      jump_target = &local_target;
    }
  for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
    {
    reenter:
      tree stmt = tsi_stmt (i);
      if (*jump_target)
	{
	  if (TREE_CODE (stmt) == STATEMENT_LIST)
	    /* The label we want might be inside.  */;
	  else if (label_matches (jump_target, i, default_label))
	    /* Found it.  */
	    *jump_target = NULL_TREE;
	  else
	    continue;
	}
      r = cxx_eval_constant_expression (ctx, stmt, false,
					non_constant_p, overflow_p,
					jump_target);
      if (*non_constant_p)
	break;
      if (returns (jump_target) || breaks (jump_target))
	break;
    }
  if (switches (jump_target) && !tsi_end_p (default_label))
    {
      i = default_label;
      *jump_target = NULL_TREE;
      goto reenter;
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
  constexpr_ctx new_ctx = *ctx;

  tree body = TREE_OPERAND (t, 0);
  do
    {
      hash_set<tree> save_exprs;
      new_ctx.save_exprs = &save_exprs;

      cxx_eval_statement_list (&new_ctx, body,
			       non_constant_p, overflow_p, jump_target);

      /* Forget saved values of SAVE_EXPRs.  */
      for (hash_set<tree>::iterator iter = save_exprs.begin();
	   iter != save_exprs.end(); ++iter)
	new_ctx.values->remove (*iter);
    }
  while (!returns (jump_target) && !breaks (jump_target) && !*non_constant_p);

  if (breaks (jump_target))
    *jump_target = NULL_TREE;

  return NULL_TREE;
}

/* Evaluate a SWITCH_EXPR for side-effects.  Handles switch and break jump
   semantics.  */

static tree
cxx_eval_switch_expr (const constexpr_ctx *ctx, tree t,
		      bool *non_constant_p, bool *overflow_p,
		      tree *jump_target)
{
  tree cond = TREE_OPERAND (t, 0);
  cond = cxx_eval_constant_expression (ctx, cond, false,
				       non_constant_p, overflow_p);
  VERIFY_CONSTANT (cond);
  *jump_target = cond;

  tree body = TREE_OPERAND (t, 1);
  cxx_eval_statement_list (ctx, body,
			   non_constant_p, overflow_p, jump_target);
  if (breaks (jump_target) || switches (jump_target))
    *jump_target = NULL_TREE;
  return NULL_TREE;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a POINTER_PLUS_EXPR expression T.  */

static tree
cxx_eval_pointer_plus_expression (const constexpr_ctx *ctx, tree t,
				  bool lval, bool *non_constant_p,
				  bool *overflow_p)
{
  tree orig_type = TREE_TYPE (t);
  tree op00 = TREE_OPERAND (t, 0);
  tree op01 = TREE_OPERAND (t, 1);
  location_t loc = EXPR_LOCATION (t);

  op00 = cxx_eval_constant_expression (ctx, op00, lval,
				       non_constant_p, overflow_p);

  STRIP_NOPS (op00);
  if (TREE_CODE (op00) != ADDR_EXPR)
    return NULL_TREE;

  op01 = cxx_eval_constant_expression (ctx, op01, lval,
				       non_constant_p, overflow_p);
  op00 = TREE_OPERAND (op00, 0);

  /* &A[i] p+ j => &A[i + j] */
  if (TREE_CODE (op00) == ARRAY_REF
      && TREE_CODE (TREE_OPERAND (op00, 1)) == INTEGER_CST
      && TREE_CODE (op01) == INTEGER_CST
      && TYPE_SIZE_UNIT (TREE_TYPE (op00))
      && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (op00))) == INTEGER_CST)
    {
      tree type = TREE_TYPE (op00);
      t = fold_convert_loc (loc, ssizetype, TREE_OPERAND (op00, 1));
      tree nelts = array_type_nelts_top (TREE_TYPE (TREE_OPERAND (op00, 0)));
      /* Don't fold an out-of-bound access.  */
      if (!tree_int_cst_le (t, nelts))
	return NULL_TREE;
      op01 = cp_fold_convert (ssizetype, op01);
      /* Don't fold if op01 can't be divided exactly by TYPE_SIZE_UNIT.
	 constexpr int A[1]; ... (char *)&A[0] + 1 */
      if (!integer_zerop (fold_build2_loc (loc, TRUNC_MOD_EXPR, sizetype,
					   op01, TYPE_SIZE_UNIT (type))))
	return NULL_TREE;
      /* Make sure to treat the second operand of POINTER_PLUS_EXPR
	 as signed.  */
      op01 = fold_build2_loc (loc, EXACT_DIV_EXPR, ssizetype, op01,
			      TYPE_SIZE_UNIT (type));
      t = size_binop_loc (loc, PLUS_EXPR, op01, t);
      t = build4_loc (loc, ARRAY_REF, type, TREE_OPERAND (op00, 0),
		      t, NULL_TREE, NULL_TREE);
      t = cp_build_addr_expr (t, tf_warning_or_error);
      t = cp_fold_convert (orig_type, t);
      return cxx_eval_constant_expression (ctx, t, lval, non_constant_p,
					   overflow_p);
    }

  return NULL_TREE;
}

/* Attempt to reduce the expression T to a constant value.
   On failure, issue diagnostic and return error_mark_node.  */
/* FIXME unify with c_fully_fold */
/* FIXME overflow_p is too global */

static tree
cxx_eval_constant_expression (const constexpr_ctx *ctx, tree t,
			      bool lval,
			      bool *non_constant_p, bool *overflow_p,
			      tree *jump_target)
{
  constexpr_ctx new_ctx;
  tree r = t;

  if (t == error_mark_node)
    {
      *non_constant_p = true;
      return t;
    }
  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_OVERFLOW (t) && (!flag_permissive || ctx->quiet))
	*overflow_p = true;
      return t;
    }

  switch (TREE_CODE (t))
    {
    case RESULT_DECL:
      if (lval)
	return t;
      /* We ask for an rvalue for the RESULT_DECL when indirecting
	 through an invisible reference, or in named return value
	 optimization.  */
      return (*ctx->values->get (t));

    case VAR_DECL:
    case CONST_DECL:
      /* We used to not check lval for CONST_DECL, but darwin.c uses
	 CONST_DECL for aggregate constants.  */
      if (lval)
	return t;
      if (ctx->strict)
	r = decl_really_constant_value (t);
      else
	r = decl_constant_value (t);
      if (TREE_CODE (r) == TARGET_EXPR
	  && TREE_CODE (TARGET_EXPR_INITIAL (r)) == CONSTRUCTOR)
	r = TARGET_EXPR_INITIAL (r);
      if (VAR_P (r))
	if (tree *p = ctx->values->get (r))
	  if (*p != NULL_TREE)
	    r = *p;
      if (DECL_P (r))
	{
	  if (!ctx->quiet)
	    non_const_var_error (r);
	  *non_constant_p = true;
	}
      break;

    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case LABEL_DECL:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
      return t;

    case PARM_DECL:
      if (lval && TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE)
	/* glvalue use.  */;
      else if (tree *p = ctx->values->get (r))
	r = *p;
      else if (lval)
	/* Defer in case this is only used for its type.  */;
      else if (TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE)
	/* Defer, there's no lvalue->rvalue conversion.  */;
      else if (is_empty_class (TREE_TYPE (t)))
	{
	  /* If the class is empty, we aren't actually loading anything.  */
	  r = build_constructor (TREE_TYPE (t), NULL);
	  TREE_CONSTANT (r) = true;
	}
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
	if (AGGREGATE_TYPE_P (TREE_TYPE (r))
	    || VECTOR_TYPE_P (TREE_TYPE (r)))
	  {
	    new_ctx = *ctx;
	    new_ctx.object = r;
	    new_ctx.ctor = build_constructor (TREE_TYPE (r), NULL);
	    CONSTRUCTOR_NO_IMPLICIT_ZERO (new_ctx.ctor) = true;
	    new_ctx.values->put (r, new_ctx.ctor);
	    ctx = &new_ctx;
	  }

	if (tree init = DECL_INITIAL (r))
	  {
	    init = cxx_eval_constant_expression (ctx, init,
						 false,
						 non_constant_p, overflow_p);
	    /* Don't share a CONSTRUCTOR that might be changed.  */
	    init = unshare_expr (init);
	    ctx->values->put (r, init);
	  }
	else if (ctx == &new_ctx)
	  /* We gave it a CONSTRUCTOR above.  */;
	else
	  ctx->values->put (r, NULL_TREE);
      }
      break;

    case TARGET_EXPR:
      if (!literal_type_p (TREE_TYPE (t)))
	{
	  if (!ctx->quiet)
	    {
	      error ("temporary of non-literal type %qT in a "
		     "constant expression", TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  *non_constant_p = true;
	  break;
	}
      if ((AGGREGATE_TYPE_P (TREE_TYPE (t)) || VECTOR_TYPE_P (TREE_TYPE (t))))
	{
	  /* We're being expanded without an explicit target, so start
	     initializing a new object; expansion with an explicit target
	     strips the TARGET_EXPR before we get here.  */
	  new_ctx = *ctx;
	  new_ctx.ctor = build_constructor (TREE_TYPE (t), NULL);
	  CONSTRUCTOR_NO_IMPLICIT_ZERO (new_ctx.ctor) = true;
	  new_ctx.object = TARGET_EXPR_SLOT (t);
	  ctx->values->put (new_ctx.object, new_ctx.ctor);
	  ctx = &new_ctx;
	}
      /* Pass false for 'lval' because this indicates
	 initialization of a temporary.  */
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1),
					false,
					non_constant_p, overflow_p);
      if (!*non_constant_p)
	/* Adjust the type of the result to the type of the temporary.  */
	r = adjust_temp_type (TREE_TYPE (t), r);
      if (lval)
	{
	  tree slot = TARGET_EXPR_SLOT (t);
	  r = unshare_expr (r);
	  ctx->values->put (slot, r);
	  return slot;
	}
      break;

    case INIT_EXPR:
    case MODIFY_EXPR:
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
      *jump_target = t;
      break;

    case SAVE_EXPR:
      /* Avoid evaluating a SAVE_EXPR more than once.  */
      if (tree *p = ctx->values->get (t))
	r = *p;
      else
	{
	  r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), false,
					    non_constant_p, overflow_p);
	  ctx->values->put (t, r);
	  if (ctx->save_exprs)
	    ctx->save_exprs->add (t);
	}
      break;

    case NON_LVALUE_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_BLOCK:
    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case EXPR_STMT:
    case EH_SPEC_BLOCK:
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0),
					lval,
					non_constant_p, overflow_p,
					jump_target);
      break;

    case TRY_FINALLY_EXPR:
      r = cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 0), lval,
					non_constant_p, overflow_p,
					jump_target);
      if (!*non_constant_p)
	/* Also evaluate the cleanup.  */
	cxx_eval_constant_expression (ctx, TREE_OPERAND (t, 1), true,
				      non_constant_p, overflow_p,
				      jump_target);
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
	tree op = cxx_eval_constant_expression (ctx, oldop,
						/*lval*/true,
						non_constant_p, overflow_p);
	/* Don't VERIFY_CONSTANT here.  */
	if (*non_constant_p)
	  return t;
	gcc_checking_assert (TREE_CODE (op) != CONSTRUCTOR);
	/* This function does more aggressive folding than fold itself.  */
	r = build_fold_addr_expr_with_type (op, TREE_TYPE (t));
	if (TREE_CODE (r) == ADDR_EXPR && TREE_OPERAND (r, 0) == oldop)
	  return t;
	break;
      }

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
      r = cxx_eval_unary_expression (ctx, t, lval,
				     non_constant_p, overflow_p);
      break;

    case SIZEOF_EXPR:
      r = fold_sizeof_expr (t);
      VERIFY_CONSTANT (r);
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
	    cxx_eval_constant_expression (ctx, op0,
					  true, non_constant_p, overflow_p,
					  jump_target);
	    op1 = TREE_OPERAND (t, 1);
	    r = cxx_eval_constant_expression (ctx, op1,
					      lval, non_constant_p, overflow_p,
					      jump_target);
	  }
      }
      break;

    case POINTER_PLUS_EXPR:
      r = cxx_eval_pointer_plus_expression (ctx, t, lval, non_constant_p,
					    overflow_p);
      if (r)
	break;
      /* else fall through */

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
				       lval,
				       non_constant_p, overflow_p);
      break;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      r = cxx_eval_logical_expression (ctx, t, boolean_true_node,
				       boolean_false_node,
				       lval,
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
    case VEC_COND_EXPR:
      r = cxx_eval_conditional_expression (ctx, t, lval,
					   non_constant_p, overflow_p,
					   jump_target);
      break;

    case CONSTRUCTOR:
      if (TREE_CONSTANT (t))
	{
	  /* Don't re-process a constant CONSTRUCTOR, but do fold it to
	     VECTOR_CST if applicable.  */
	  /* FIXME after GCC 6 branches, make the verify unconditional.  */
	  if (CHECKING_P)
	    verify_constructor_flags (t);
	  else
	    recompute_constructor_flags (t);
	  if (TREE_CONSTANT (t))
	    return fold (t);
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

    case FMA_EXPR:
    case VEC_PERM_EXPR:
      r = cxx_eval_trinary_expression (ctx, t, lval,
				       non_constant_p, overflow_p);
      break;

    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
    case NOP_EXPR:
    case UNARY_PLUS_EXPR:
      {
	enum tree_code tcode = TREE_CODE (t);
	tree oldop = TREE_OPERAND (t, 0);

	tree op = cxx_eval_constant_expression (ctx, oldop,
						lval,
						non_constant_p, overflow_p);
	if (*non_constant_p)
	  return t;
	tree type = TREE_TYPE (t);
	if (TREE_CODE (op) == PTRMEM_CST
	    && !TYPE_PTRMEM_P (type))
	  op = cplus_expand_constant (op);
	if (TREE_CODE (op) == PTRMEM_CST && tcode == NOP_EXPR)
	  {
	    if (same_type_ignoring_top_level_qualifiers_p (type,
							   TREE_TYPE (op)))
	      STRIP_NOPS (t);
	    else
	      {
		if (!ctx->quiet)
		  error_at (EXPR_LOC_OR_LOC (t, input_location),
			    "a reinterpret_cast is not a constant-expression");
		*non_constant_p = true;
		return t;
	      }
	  }
	if (POINTER_TYPE_P (type)
	    && TREE_CODE (op) == INTEGER_CST
	    && !integer_zerop (op))
	  {
	    if (!ctx->quiet)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"reinterpret_cast from integer to pointer");
	    *non_constant_p = true;
	    return t;
	  }
	if (op == oldop && tcode != UNARY_PLUS_EXPR)
	  /* We didn't fold at the top so we could check for ptr-int
	     conversion.  */
	  return fold (t);
	if (tcode == UNARY_PLUS_EXPR)
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

    case EMPTY_CLASS_EXPR:
      /* This is good enough for a function argument that might not get
	 used, and they can't do anything with it, so just return it.  */
      return t;

    case STATEMENT_LIST:
      new_ctx = *ctx;
      new_ctx.ctor = new_ctx.object = NULL_TREE;
      return cxx_eval_statement_list (&new_ctx, t,
				      non_constant_p, overflow_p, jump_target);

    case BIND_EXPR:
      return cxx_eval_constant_expression (ctx, BIND_EXPR_BODY (t),
					   lval,
					   non_constant_p, overflow_p,
					   jump_target);

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
    case OBJ_TYPE_REF:
    case WITH_CLEANUP_EXPR:
    case NON_DEPENDENT_EXPR:
    case BASELINK:
    case OFFSET_REF:
      if (!ctx->quiet)
        error_at (EXPR_LOC_OR_LOC (t, input_location),
		  "expression %qE is not a constant-expression", t);
      *non_constant_p = true;
      break;

    case PLACEHOLDER_EXPR:
      if (!ctx || !ctx->ctor || (lval && !ctx->object)
	  || !(same_type_ignoring_top_level_qualifiers_p
	       (TREE_TYPE (t), TREE_TYPE (ctx->ctor))))
	{
	  /* A placeholder without a referent.  We can get here when
	     checking whether NSDMIs are noexcept, or in massage_init_elt;
	     just say it's non-constant for now.  */
	  gcc_assert (ctx->quiet);
	  *non_constant_p = true;
	  break;
	}
      else
	{
	  /* Use of the value or address of the current object.  We could
	     use ctx->object unconditionally, but using ctx->ctor when we
	     can is a minor optimization.  */
	  tree ctor = lval ? ctx->object : ctx->ctor;
	  return cxx_eval_constant_expression
	    (ctx, ctor, lval,
	     non_constant_p, overflow_p);
	}
      break;

    case GOTO_EXPR:
      *jump_target = TREE_OPERAND (t, 0);
      gcc_assert (breaks (jump_target) || continues (jump_target));
      break;

    case LOOP_EXPR:
      cxx_eval_loop_expr (ctx, t,
			  non_constant_p, overflow_p, jump_target);
      break;

    case SWITCH_EXPR:
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
        return evaluate_constraint_expression (t, NULL_TREE);
      else
        *non_constant_p = true;
      return t;

    default:
      if (STATEMENT_CODE_P (TREE_CODE (t)))
	{
	  /* This function doesn't know how to deal with pre-genericize
	     statements; this can only happen with statement-expressions,
	     so for now just fail.  */
	  if (!ctx->quiet)
	    error_at (EXPR_LOCATION (t),
		      "statement is not a constant-expression");
	}
      else
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

static tree
cxx_eval_outermost_constant_expr (tree t, bool allow_non_constant,
				  bool strict = true, tree object = NULL_TREE)
{
  bool non_constant_p = false;
  bool overflow_p = false;
  hash_map<tree,tree> map;

  constexpr_ctx ctx = { NULL, &map, NULL, NULL, NULL,
			allow_non_constant, strict };

  tree type = initialized_type (t);
  tree r = t;
  if (AGGREGATE_TYPE_P (type) || VECTOR_TYPE_P (type))
    {
      /* In C++14 an NSDMI can participate in aggregate initialization,
	 and can refer to the address of the object being initialized, so
	 we need to pass in the relevant VAR_DECL if we want to do the
	 evaluation in a single pass.  The evaluation will dynamically
	 update ctx.values for the VAR_DECL.  We use the same strategy
	 for C++11 constexpr constructors that refer to the object being
	 initialized.  */
      ctx.ctor = build_constructor (type, NULL);
      CONSTRUCTOR_NO_IMPLICIT_ZERO (ctx.ctor) = true;
      if (!object)
	{
	  if (TREE_CODE (t) == TARGET_EXPR)
	    object = TARGET_EXPR_SLOT (t);
	  else if (TREE_CODE (t) == AGGR_INIT_EXPR)
	    object = AGGR_INIT_EXPR_SLOT (t);
	}
      ctx.object = object;
      if (object)
	gcc_assert (same_type_ignoring_top_level_qualifiers_p
		    (type, TREE_TYPE (object)));
      if (object && DECL_P (object))
	map.put (object, ctx.ctor);
      if (TREE_CODE (r) == TARGET_EXPR)
	/* Avoid creating another CONSTRUCTOR when we expand the
	   TARGET_EXPR.  */
	r = TARGET_EXPR_INITIAL (r);
    }

  r = cxx_eval_constant_expression (&ctx, r,
				    false, &non_constant_p, &overflow_p);

  verify_constant (r, allow_non_constant, &non_constant_p, &overflow_p);

  /* Mutable logic is a bit tricky: we want to allow initialization of
     constexpr variables with mutable members, but we can't copy those
     members to another constexpr variable.  */
  if (TREE_CODE (r) == CONSTRUCTOR
      && CONSTRUCTOR_MUTABLE_POISON (r))
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression because it refers to "
	       "mutable subobjects of %qT", t, type);
      non_constant_p = true;
    }

  /* Technically we should check this for all subexpressions, but that
     runs into problems with our internal representation of pointer
     subtraction and the 5.19 rules are still in flux.  */
  if (CONVERT_EXPR_CODE_P (TREE_CODE (r))
      && ARITHMETIC_TYPE_P (TREE_TYPE (r))
      && TREE_CODE (TREE_OPERAND (r, 0)) == ADDR_EXPR)
    {
      if (!allow_non_constant)
	error ("conversion from pointer type %qT "
	       "to arithmetic type %qT in a constant-expression",
	       TREE_TYPE (TREE_OPERAND (r, 0)), TREE_TYPE (r));
      non_constant_p = true;
    }

  if (!non_constant_p && overflow_p)
    non_constant_p = true;

  if (non_constant_p && !allow_non_constant)
    return error_mark_node;
  else if (non_constant_p && TREE_CONSTANT (r))
    {
      /* This isn't actually constant, so unset TREE_CONSTANT.  */
      if (EXPR_P (r))
	r = copy_node (r);
      else if (TREE_CODE (r) == CONSTRUCTOR)
	r = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (r), r);
      else
	r = build_nop (TREE_TYPE (r), r);
      TREE_CONSTANT (r) = false;
    }
  else if (non_constant_p || r == t)
    return t;

  if (TREE_CODE (r) == CONSTRUCTOR && CLASS_TYPE_P (TREE_TYPE (r)))
    {
      if (TREE_CODE (t) == TARGET_EXPR
	  && TARGET_EXPR_INITIAL (t) == r)
	return t;
      else
	{
	  r = get_target_expr (r);
	  TREE_CONSTANT (r) = true;
	  return r;
	}
    }
  else
    return r;
}

/* Returns true if T is a valid subexpression of a constant expression,
   even if it isn't itself a constant expression.  */

bool
is_sub_constant_expr (tree t)
{
  bool non_constant_p = false;
  bool overflow_p = false;
  hash_map <tree, tree> map;

  constexpr_ctx ctx = { NULL, &map, NULL, NULL, NULL, true, true };

  cxx_eval_constant_expression (&ctx, t, false, &non_constant_p,
				&overflow_p);
  return !non_constant_p && !overflow_p;
}

/* If T represents a constant expression returns its reduced value.
   Otherwise return error_mark_node.  If T is dependent, then
   return NULL.  */

tree
cxx_constant_value (tree t, tree decl)
{
  return cxx_eval_outermost_constant_expr (t, false, true, decl);
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
    case CONJ_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case NOP_EXPR:
    case VIEW_CONVERT_EXPR:
    case CONVERT_EXPR:
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
   Otherwise returns T.  In contrast to maybe_constant_value do we
   simplify only few operations on constant-expressions, and we don't
   try to simplify constexpressions.  */

tree
fold_simple (tree t)
{
  tree r = NULL_TREE;
  if (processing_template_decl)
    return t;

  r = fold_simple_1 (t);
  if (!r)
    r = t;

  return r;
}

/* If T is a constant expression, returns its reduced value.
   Otherwise, if T does not have TREE_CONSTANT set, returns T.
   Otherwise, returns a version of T without TREE_CONSTANT.  */

static tree
maybe_constant_value_1 (tree t, tree decl)
{
  tree r;

  if (instantiation_dependent_expression_p (t)
      || type_unknown_p (t)
      || BRACE_ENCLOSED_INITIALIZER_P (t)
      || !potential_constant_expression (t))
    {
      if (TREE_OVERFLOW_P (t))
	{
	  t = build_nop (TREE_TYPE (t), t);
	  TREE_CONSTANT (t) = false;
	}
      return t;
    }

  r = cxx_eval_outermost_constant_expr (t, true, true, decl);
  gcc_checking_assert (r == t
		       || CONVERT_EXPR_P (t)
		       || TREE_CODE (t) == VIEW_CONVERT_EXPR
		       || (TREE_CONSTANT (t) && !TREE_CONSTANT (r))
		       || !cp_tree_equal (r, t));
  return r;
}

static GTY((cache, deletable)) cache_map cv_cache;

/* If T is a constant expression, returns its reduced value.
   Otherwise, if T does not have TREE_CONSTANT set, returns T.
   Otherwise, returns a version of T without TREE_CONSTANT.  */

tree
maybe_constant_value (tree t, tree decl)
{
  tree ret = cv_cache.get (t);
  if (!ret)
    {
      ret = maybe_constant_value_1 (t, decl);
      cv_cache.put (t, ret);
    }
  return ret;
}

/* Dispose of the whole CV_CACHE and FOLD_CACHE.  */

void
clear_cv_and_fold_caches (void)
{
  gt_cleare_cache (cv_cache);
  clear_fold_cache ();
}

/* Like maybe_constant_value but first fully instantiate the argument.

   Note: this is equivalent to instantiate_non_dependent_expr_sfinae
   (t, tf_none) followed by maybe_constant_value but is more efficient,
   because calls instantiation_dependent_expression_p and
   potential_constant_expression at most once.  */

tree
fold_non_dependent_expr (tree t)
{
  if (t == NULL_TREE)
    return NULL_TREE;

  /* If we're in a template, but T isn't value dependent, simplify
     it.  We're supposed to treat:

       template <typename T> void f(T[1 + 1]);
       template <typename T> void f(T[2]);

     as two declarations of the same function, for example.  */
  if (processing_template_decl)
    {
      if (!instantiation_dependent_expression_p (t)
	  && potential_constant_expression (t))
	{
	  processing_template_decl_sentinel s;
	  t = instantiate_non_dependent_expr_internal (t, tf_none);

	  if (type_unknown_p (t)
	      || BRACE_ENCLOSED_INITIALIZER_P (t))
	    {
	      if (TREE_OVERFLOW_P (t))
		{
		  t = build_nop (TREE_TYPE (t), t);
		  TREE_CONSTANT (t) = false;
		}
	      return t;
	    }

	  tree r = cxx_eval_outermost_constant_expr (t, true, true, NULL_TREE);
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

  return maybe_constant_value (t);
}

/* Like maybe_constant_value, but returns a CONSTRUCTOR directly, rather
   than wrapped in a TARGET_EXPR.  */

tree
maybe_constant_init (tree t, tree decl)
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
  if (instantiation_dependent_expression_p (t)
      || type_unknown_p (t)
      || BRACE_ENCLOSED_INITIALIZER_P (t)
      || !potential_static_init_expression (t))
    /* Don't try to evaluate it.  */;
  else
    t = cxx_eval_outermost_constant_expr (t, true, false, decl);
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree init = TARGET_EXPR_INITIAL (t);
      if (TREE_CODE (init) == CONSTRUCTOR)
	t = init;
    }
  return t;
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
  HOST_WIDE_INT bitsize, bitpos;
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

/* Return true if T denotes a potentially constant expression.  Issue
   diagnostic as appropriate under control of FLAGS.  If WANT_RVAL is true,
   an lvalue-rvalue conversion is implied.

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
potential_constant_expression_1 (tree t, bool want_rval, bool strict,
				 tsubst_flags_t flags)
{
#define RECUR(T,RV) potential_constant_expression_1 ((T), (RV), strict, flags)
  enum { any = false, rval = true };
  int i;
  tree tmp;

  if (t == error_mark_node)
    return false;
  if (t == NULL_TREE)
    return true;
  if (TREE_THIS_VOLATILE (t) && !DECL_P (t))
    {
      if (flags & tf_error)
        error ("expression %qE has side-effects", t);
      return false;
    }
  if (CONSTANT_CLASS_P (t))
    return true;

  switch (TREE_CODE (t))
    {
    case FUNCTION_DECL:
    case BASELINK:
    case TEMPLATE_DECL:
    case OVERLOAD:
    case TEMPLATE_ID_EXPR:
    case LABEL_DECL:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
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
    case PARM_DECL:
    case RESULT_DECL:
    case USING_DECL:
    case USING_STMT:
    case PLACEHOLDER_EXPR:
    case BREAK_STMT:
    case CONTINUE_STMT:
    case REQUIRES_EXPR:
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
	    if (TREE_CODE (t) == CALL_EXPR
		&& CALL_EXPR_FN (t) == NULL_TREE)
	      switch (CALL_EXPR_IFN (t))
		{
		/* These should be ignored, they are optimized away from
		   constexpr functions.  */
		case IFN_UBSAN_NULL:
		case IFN_UBSAN_BOUNDS:
		case IFN_UBSAN_VPTR:
		  return true;
		default:
		  break;
		}
	    /* fold_call_expr can't do anything with IFN calls.  */
	    if (flags & tf_error)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"call to internal function");
	    return false;
	  }
	if (is_overloaded_fn (fun))
	  {
	    if (TREE_CODE (fun) == FUNCTION_DECL)
	      {
		if (builtin_valid_in_constant_expr_p (fun))
		  return true;
		if (!DECL_DECLARED_CONSTEXPR_P (fun)
		    /* Allow any built-in function; if the expansion
		       isn't constant, we'll deal with that then.  */
		    && !is_builtin_fn (fun))
		  {
		    if (flags & tf_error)
		      {
			error_at (EXPR_LOC_OR_LOC (t, input_location),
				  "call to non-constexpr function %qD", fun);
			explain_invalid_constexpr_fn (fun);
		      }
		    return false;
		  }
		/* A call to a non-static member function takes the address
		   of the object as the first argument.  But in a constant
		   expression the address will be folded away, so look
		   through it now.  */
		if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
		    && !DECL_CONSTRUCTOR_P (fun))
		  {
		    tree x = get_nth_callarg (t, 0);
		    if (is_this_parameter (x))
		      return true;
		    else if (!RECUR (x, rval))
		      return false;
		    i = 1;
		  }
	      }
	    else
	      {
		if (!RECUR (fun, true))
		  return false;
		fun = get_first_fn (fun);
	      }
	    /* Skip initial arguments to base constructors.  */
	    if (DECL_BASE_CONSTRUCTOR_P (fun))
	      i = num_artificial_parms_for (fun);
	    fun = DECL_ORIGIN (fun);
	  }
	else
          {
	    if (RECUR (fun, rval))
	      /* Might end up being a constant function pointer.  */;
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
	    if (!RECUR (x, rv))
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

    case VAR_DECL:
      if (want_rval
	  && !decl_constant_var_p (t)
	  && (strict
	      || !CP_TYPE_CONST_NON_VOLATILE_P (TREE_TYPE (t))
	      || !DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (t))
	  && !var_in_constexpr_fn (t)
	  && !type_dependent_expression_p (t))
        {
          if (flags & tf_error)
            non_const_var_error (t);
          return false;
        }
      return true;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
      /* -- a reinterpret_cast.  FIXME not implemented, and this rule
	 may change to something more specific to type-punning (DR 1312).  */
      {
        tree from = TREE_OPERAND (t, 0);
	if (POINTER_TYPE_P (TREE_TYPE (t))
	    && TREE_CODE (from) == INTEGER_CST
	    && !integer_zerop (from))
	  {
	    if (flags & tf_error)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"reinterpret_cast from integer to pointer");
	    return false;
	  }
        return (RECUR (from, TREE_CODE (t) != VIEW_CONVERT_EXPR));
      }

    case ADDR_EXPR:
      /* -- a unary operator & that is applied to an lvalue that
            designates an object with thread or automatic storage
            duration;  */
      t = TREE_OPERAND (t, 0);

      if (TREE_CODE (t) == OFFSET_REF && PTRMEM_OK_P (t))
	/* A pointer-to-member constant.  */
	return true;

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
    case BIT_FIELD_REF:
    case ARROW_EXPR:
    case OFFSET_REF:
      /* -- a class member access unless its postfix-expression is
            of literal type or of pointer to literal type.  */
      /* This test would be redundant, as it follows from the
	 postfix-expression being a potential constant expression.  */
      if (type_unknown_p (t))
	return true;
      return RECUR (TREE_OPERAND (t, 0), want_rval);

    case EXPR_PACK_EXPANSION:
      return RECUR (PACK_EXPANSION_PATTERN (t), want_rval);

    case INDIRECT_REF:
      {
        tree x = TREE_OPERAND (t, 0);
        STRIP_NOPS (x);
        if (is_this_parameter (x))
	  {
	    if (DECL_CONTEXT (x)
		&& !DECL_DECLARED_CONSTEXPR_P (DECL_CONTEXT (x)))
	      {
		if (flags & tf_error)
		  error ("use of %<this%> in a constant expression");
		return false;
	      }
	    return true;
	  }
	return RECUR (x, rval);
      }

    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	  {
	    if (!RECUR (tsi_stmt (i), any))
	      return false;
	  }
	return true;
      }
      break;

    case MODIFY_EXPR:
      if (cxx_dialect < cxx14)
	goto fail;
      if (!RECUR (TREE_OPERAND (t, 0), any))
	return false;
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
      return true;

    case FOR_STMT:
      if (!RECUR (FOR_INIT_STMT (t), any))
	return false;
      if (!RECUR (FOR_COND (t), rval))
	return false;
      if (!RECUR (FOR_EXPR (t), any))
	return false;
      if (!RECUR (FOR_BODY (t), any))
	return false;
      return true;

    case WHILE_STMT:
      if (!RECUR (WHILE_COND (t), rval))
	return false;
      if (!RECUR (WHILE_BODY (t), any))
	return false;
      return true;

    case SWITCH_STMT:
      if (!RECUR (SWITCH_STMT_COND (t), rval))
	return false;
      /* FIXME we don't check SWITCH_STMT_BODY currently, because even
	 unreachable labels would be checked.  */
      return true;

    case STMT_EXPR:
      return RECUR (STMT_EXPR_STMT (t), rval);

    case LAMBDA_EXPR:
    case DYNAMIC_CAST_EXPR:
    case PSEUDO_DTOR_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case OBJ_TYPE_REF:
    case TRANSACTION_EXPR:
    case ASM_EXPR:
    case AT_ENCODE_EXPR:
    fail:
      if (flags & tf_error)
        error ("expression %qE is not a constant-expression", t);
      return false;

    case TYPEID_EXPR:
      /* -- a typeid expression whose operand is of polymorphic
            class type;  */
      {
        tree e = TREE_OPERAND (t, 0);
        if (!TYPE_P (e) && !type_dependent_expression_p (e)
	    && TYPE_POLYMORPHIC_P (TREE_TYPE (e)))
          {
            if (flags & tf_error)
              error ("typeid-expression is not a constant expression "
                     "because %qE is of polymorphic type", e);
            return false;
          }
        return true;
      }

    case MINUS_EXPR:
      want_rval = true;
      goto binary;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
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
      /* else fall through.  */

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case CONJ_EXPR:
    case SAVE_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    case UNARY_PLUS_EXPR:
    case UNARY_LEFT_FOLD_EXPR:
    case UNARY_RIGHT_FOLD_EXPR:
    unary:
      return RECUR (TREE_OPERAND (t, 0), rval);

    case CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      if (cxx_dialect < cxx11
	  && !dependent_type_p (TREE_TYPE (t))
	  && !INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (t)))
	/* In C++98, a conversion to non-integral type can't be part of a
	   constant expression.  */
	{
	  if (flags & tf_error)
	    error ("cast to non-integral type %qT in a constant expression",
		   TREE_TYPE (t));
	  return false;
	}

      return (RECUR (TREE_OPERAND (t, 0),
		     TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE));

    case BIND_EXPR:
      return RECUR (BIND_EXPR_BODY (t), want_rval);

    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_BLOCK:
    case EH_SPEC_BLOCK:
    case EXPR_STMT:
    case PAREN_EXPR:
    case DECL_EXPR:
    case NON_DEPENDENT_EXPR:
      /* For convenience.  */
    case RETURN_EXPR:
      return RECUR (TREE_OPERAND (t, 0), want_rval);

    case TRY_FINALLY_EXPR:
      return (RECUR (TREE_OPERAND (t, 0), want_rval)
	      && RECUR (TREE_OPERAND (t, 1), any));

    case SCOPE_REF:
      return RECUR (TREE_OPERAND (t, 1), want_rval);

    case TARGET_EXPR:
      if (!literal_type_p (TREE_TYPE (t)))
	{
	  if (flags & tf_error)
	    {
	      error ("temporary of non-literal type %qT in a "
		     "constant expression", TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  return false;
	}
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
	      error ("division by zero is not a constant-expression");
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
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
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
	tree op = TREE_OPERAND (t, 0);
	if (!RECUR (op, rval))
	  return false;
	if (!processing_template_decl)
	  op = cxx_eval_outermost_constant_expr (op, true);
	if (tree_int_cst_equal (op, tmp))
	  return RECUR (TREE_OPERAND (t, 1), rval);
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

    case CILK_SYNC_STMT:
    case CILK_SPAWN_STMT:
    case ARRAY_NOTATION_REF:
      return false;

    case FMA_EXPR:
    case VEC_PERM_EXPR:
     for (i = 0; i < 3; ++i)
      if (!RECUR (TREE_OPERAND (t, i), true))
	return false;
     return true;

    case COND_EXPR:
      if (COND_EXPR_IS_VEC_DELETE (t))
	{
	  if (flags & tf_error)
	    error_at (location_of (t),
		      "%<delete[]%> is not a constant-expression");
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
      if (integer_zerop (tmp))
	return RECUR (TREE_OPERAND (t, 2), want_rval);
      else if (TREE_CODE (tmp) == INTEGER_CST)
	return RECUR (TREE_OPERAND (t, 1), want_rval);
      for (i = 1; i < 3; ++i)
	if (potential_constant_expression_1 (TREE_OPERAND (t, i),
					     want_rval, strict, tf_none))
	  return true;
      if (flags & tf_error)
        error ("expression %qE is not a constant-expression", t);
      return false;

    case VEC_INIT_EXPR:
      if (VEC_INIT_EXPR_IS_CONSTEXPR (t))
	return true;
      if (flags & tf_error)
	{
	  error ("non-constant array initialization");
	  diagnose_non_constexpr_vec_init (t);
	}
      return false;

    case TYPE_DECL:
    case TAG_DEFN:
      /* We can see these in statement-expressions.  */
      return true;

    case EMPTY_CLASS_EXPR:
      return false;

    default:
      if (objc_is_property_ref (t))
	return false;

      sorry ("unexpected AST of kind %s", get_tree_code_name (TREE_CODE (t)));
      gcc_unreachable();
      return false;
    }
#undef RECUR
}

/* The main entry point to the above.  */

bool
potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, true, tf_none);
}

bool
potential_static_init_expression (tree t)
{
  return potential_constant_expression_1 (t, false, false, tf_none);
}

/* As above, but require a constant rvalue.  */

bool
potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, true, true, tf_none);
}

/* Like above, but complain about non-constant expressions.  */

bool
require_potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, true, tf_warning_or_error);
}

/* Cross product of the above.  */

bool
require_potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, true, true, tf_warning_or_error);
}

#include "gt-cp-constexpr.h"
