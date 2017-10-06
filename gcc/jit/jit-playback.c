/* Internals of libgccjit: classes for playing back recorded API calls.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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
#include "target.h"
#include "tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "dumpfile.h"
#include "toplev.h"
#include "tree-cfg.h"
#include "convert.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "gimplify.h"
#include "gcc-driver-name.h"
#include "attribs.h"
#include "context.h"
#include "fold-const.h"
#include "gcc.h"
#include "diagnostic.h"

#include <pthread.h>

#include "jit-playback.h"
#include "jit-result.h"
#include "jit-builtins.h"
#include "jit-tempdir.h"


/* gcc::jit::playback::context::build_cast uses the convert.h API,
   which in turn requires the frontend to provide a "convert"
   function, apparently as a fallback.

   Hence we provide this dummy one, with the requirement that any casts
   are handled before reaching this.  */
extern tree convert (tree type, tree expr);

tree
convert (tree dst_type, tree expr)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  gcc::jit::active_playback_ctxt->add_error (NULL, "unhandled conversion");
  fprintf (stderr, "input expression:\n");
  debug_tree (expr);
  fprintf (stderr, "requested type:\n");
  debug_tree (dst_type);
  return error_mark_node;
}

namespace gcc {
namespace jit {

/**********************************************************************
 Playback.
 **********************************************************************/

/* The constructor for gcc::jit::playback::context.  */

playback::context::context (recording::context *ctxt)
  : log_user (ctxt->get_logger ()),
    m_recording_ctxt (ctxt),
    m_tempdir (NULL),
    m_char_array_type_node (NULL),
    m_const_char_ptr (NULL)
{
  JIT_LOG_SCOPE (get_logger ());
  m_functions.create (0);
  m_globals.create (0);
  m_source_files.create (0);
  m_cached_locations.create (0);
}

/* The destructor for gcc::jit::playback::context.  */

playback::context::~context ()
{
  JIT_LOG_SCOPE (get_logger ());

  /* Normally the playback::context is responsible for cleaning up the
     tempdir (including "fake.so" within the filesystem).

     In the normal case, clean it up now.

     However m_tempdir can be NULL if the context has handed over
     responsibility for the tempdir cleanup to the jit::result object, so
     that the cleanup can be delayed (see PR jit/64206).  If that's the
     case this "delete NULL;" is a no-op. */
  delete m_tempdir;

  m_functions.release ();
}

/* A playback::context can reference GC-managed pointers.  Mark them
   ("by hand", rather than by gengtype).

   This is called on the active playback context (if any) by the
   my_ggc_walker hook in the jit_root_table in dummy-frontend.c.  */

void
playback::context::
gt_ggc_mx ()
{
  int i;
  function *func;
  FOR_EACH_VEC_ELT (m_functions, i, func)
    {
      if (ggc_test_and_set_mark (func))
	func->gt_ggc_mx ();
    }
}

/* Given an enum gcc_jit_types value, get a "tree" type.  */

static tree
get_tree_node_for_type (enum gcc_jit_types type_)
{
  switch (type_)
    {
    case GCC_JIT_TYPE_VOID:
      return void_type_node;

    case GCC_JIT_TYPE_VOID_PTR:
      return ptr_type_node;

    case GCC_JIT_TYPE_BOOL:
      return boolean_type_node;

    case GCC_JIT_TYPE_CHAR:
      return char_type_node;
    case GCC_JIT_TYPE_SIGNED_CHAR:
      return signed_char_type_node;
    case GCC_JIT_TYPE_UNSIGNED_CHAR:
      return unsigned_char_type_node;

    case GCC_JIT_TYPE_SHORT:
      return short_integer_type_node;
    case GCC_JIT_TYPE_UNSIGNED_SHORT:
      return short_unsigned_type_node;

    case GCC_JIT_TYPE_CONST_CHAR_PTR:
      {
	tree const_char = build_qualified_type (char_type_node,
						TYPE_QUAL_CONST);
	return build_pointer_type (const_char);
      }

    case GCC_JIT_TYPE_INT:
      return integer_type_node;
    case GCC_JIT_TYPE_UNSIGNED_INT:
      return unsigned_type_node;

    case GCC_JIT_TYPE_LONG:
      return long_integer_type_node;
    case GCC_JIT_TYPE_UNSIGNED_LONG:
      return long_unsigned_type_node;

    case GCC_JIT_TYPE_LONG_LONG:
      return long_long_integer_type_node;
    case GCC_JIT_TYPE_UNSIGNED_LONG_LONG:
      return long_long_unsigned_type_node;

    case GCC_JIT_TYPE_FLOAT:
      return float_type_node;
    case GCC_JIT_TYPE_DOUBLE:
      return double_type_node;
    case GCC_JIT_TYPE_LONG_DOUBLE:
      return long_double_type_node;

    case GCC_JIT_TYPE_SIZE_T:
      return size_type_node;

    case GCC_JIT_TYPE_FILE_PTR:
      return fileptr_type_node;

    case GCC_JIT_TYPE_COMPLEX_FLOAT:
      return complex_float_type_node;
    case GCC_JIT_TYPE_COMPLEX_DOUBLE:
      return complex_double_type_node;
    case GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE:
      return complex_long_double_type_node;
    }

  return NULL;
}

/* Construct a playback::type instance (wrapping a tree) for the given
   enum value.  */

playback::type *
playback::context::
get_type (enum gcc_jit_types type_)
{
  tree type_node = get_tree_node_for_type (type_);
  if (NULL == type_node)
    {
      add_error (NULL,
		 "unrecognized (enum gcc_jit_types) value: %i", type_);
      return NULL;
    }

  return new type (type_node);
}

/* Construct a playback::type instance (wrapping a tree) for the given
   array type.  */

playback::type *
playback::context::
new_array_type (playback::location *loc,
		playback::type *element_type,
		int num_elements)
{
  gcc_assert (element_type);

  tree t = build_array_type_nelts (element_type->as_tree (),
				   num_elements);
  layout_type (t);

  if (loc)
    set_tree_location (t, loc);

  return new type (t);
}

/* Construct a playback::field instance (wrapping a tree).  */

playback::field *
playback::context::
new_field (location *loc,
	   type *type,
	   const char *name)
{
  gcc_assert (type);
  gcc_assert (name);

  /* compare with c/c-decl.c:grokfield and grokdeclarator.  */
  tree decl = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			  get_identifier (name), type->as_tree ());

  if (loc)
    set_tree_location (decl, loc);

  return new field (decl);
}

/* Construct a playback::compound_type instance (wrapping a tree).  */

playback::compound_type *
playback::context::
new_compound_type (location *loc,
		   const char *name,
		   bool is_struct) /* else is union */
{
  gcc_assert (name);

  /* Compare with c/c-decl.c: start_struct. */

  tree t = make_node (is_struct ? RECORD_TYPE : UNION_TYPE);
  TYPE_NAME (t) = get_identifier (name);
  TYPE_SIZE (t) = 0;

  if (loc)
    set_tree_location (t, loc);

  return new compound_type (t);
}

void
playback::compound_type::set_fields (const auto_vec<playback::field *> *fields)
{
  /* Compare with c/c-decl.c: finish_struct. */
  tree t = as_tree ();

  tree fieldlist = NULL;
  for (unsigned i = 0; i < fields->length (); i++)
    {
      field *f = (*fields)[i];
      DECL_CONTEXT (f->as_tree ()) = t;
      fieldlist = chainon (f->as_tree (), fieldlist);
    }
  fieldlist = nreverse (fieldlist);
  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);
}

/* Construct a playback::type instance (wrapping a tree) for a function
   type.  */

playback::type *
playback::context::
new_function_type (type *return_type,
		   const auto_vec<type *> *param_types,
		   int is_variadic)
{
  int i;
  type *param_type;

  tree *arg_types = (tree *)xcalloc(param_types->length (), sizeof(tree*));

  FOR_EACH_VEC_ELT (*param_types, i, param_type)
    arg_types[i] = param_type->as_tree ();

  tree fn_type;
  if (is_variadic)
    fn_type =
      build_varargs_function_type_array (return_type->as_tree (),
					 param_types->length (),
					 arg_types);
  else
    fn_type = build_function_type_array (return_type->as_tree (),
					 param_types->length (),
					 arg_types);
  free (arg_types);

  return new type (fn_type);
}

/* Construct a playback::param instance (wrapping a tree).  */

playback::param *
playback::context::
new_param (location *loc,
	   type *type,
	   const char *name)
{
  gcc_assert (type);
  gcc_assert (name);
  tree inner = build_decl (UNKNOWN_LOCATION, PARM_DECL,
			   get_identifier (name), type->as_tree ());
  if (loc)
    set_tree_location (inner, loc);

  return new param (this, inner);
}

/* Construct a playback::function instance.  */

playback::function *
playback::context::
new_function (location *loc,
	      enum gcc_jit_function_kind kind,
	      type *return_type,
	      const char *name,
	      const auto_vec<param *> *params,
	      int is_variadic,
	      enum built_in_function builtin_id)
{
  int i;
  param *param;

  //can return_type be NULL?
  gcc_assert (name);

  tree *arg_types = (tree *)xcalloc(params->length (), sizeof(tree*));
  FOR_EACH_VEC_ELT (*params, i, param)
    arg_types[i] = TREE_TYPE (param->as_tree ());

  tree fn_type;
  if (is_variadic)
    fn_type = build_varargs_function_type_array (return_type->as_tree (),
						 params->length (), arg_types);
  else
    fn_type = build_function_type_array (return_type->as_tree (),
					 params->length (), arg_types);
  free (arg_types);

  /* FIXME: this uses input_location: */
  tree fndecl = build_fn_decl (name, fn_type);

  if (loc)
    set_tree_location (fndecl, loc);

  tree resdecl = build_decl (UNKNOWN_LOCATION, RESULT_DECL,
			     NULL_TREE, return_type->as_tree ());
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (fndecl) = resdecl;

  if (builtin_id)
    {
      DECL_FUNCTION_CODE (fndecl) = builtin_id;
      gcc_assert (loc == NULL);
      DECL_SOURCE_LOCATION (fndecl) = BUILTINS_LOCATION;

      DECL_BUILT_IN_CLASS (fndecl) =
	builtins_manager::get_class (builtin_id);
      set_builtin_decl (builtin_id, fndecl,
			builtins_manager::implicit_p (builtin_id));

      builtins_manager *bm = get_builtins_manager ();
      tree attrs = bm->get_attrs_tree (builtin_id);
      if (attrs)
	decl_attributes (&fndecl, attrs, ATTR_FLAG_BUILT_IN);
      else
	decl_attributes (&fndecl, NULL_TREE, 0);
    }

  if (kind != GCC_JIT_FUNCTION_IMPORTED)
    {
      tree param_decl_list = NULL;
      FOR_EACH_VEC_ELT (*params, i, param)
	{
	  param_decl_list = chainon (param->as_tree (), param_decl_list);
	}

      /* The param list was created in reverse order; fix it: */
      param_decl_list = nreverse (param_decl_list);

      tree t;
      for (t = param_decl_list; t; t = DECL_CHAIN (t))
	{
	  DECL_CONTEXT (t) = fndecl;
	  DECL_ARG_TYPE (t) = TREE_TYPE (t);
	}

      /* Set it up on DECL_ARGUMENTS */
      DECL_ARGUMENTS(fndecl) = param_decl_list;
    }

  if (kind == GCC_JIT_FUNCTION_ALWAYS_INLINE)
    {
      DECL_DECLARED_INLINE_P (fndecl) = 1;

      /* Add attribute "always_inline": */
      DECL_ATTRIBUTES (fndecl) =
	tree_cons (get_identifier ("always_inline"),
		   NULL,
		   DECL_ATTRIBUTES (fndecl));
    }

  function *func = new function (this, fndecl, kind);
  m_functions.safe_push (func);
  return func;
}

/* Construct a playback::lvalue instance (wrapping a tree).  */

playback::lvalue *
playback::context::
new_global (location *loc,
	    enum gcc_jit_global_kind kind,
	    type *type,
	    const char *name)
{
  gcc_assert (type);
  gcc_assert (name);
  tree inner = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			   get_identifier (name),
			   type->as_tree ());
  TREE_PUBLIC (inner) = (kind != GCC_JIT_GLOBAL_INTERNAL);
  DECL_COMMON (inner) = 1;
  switch (kind)
    {
    default:
      gcc_unreachable ();

    case GCC_JIT_GLOBAL_EXPORTED:
      TREE_STATIC (inner) = 1;
      break;

    case GCC_JIT_GLOBAL_INTERNAL:
      TREE_STATIC (inner) = 1;
      break;

    case GCC_JIT_GLOBAL_IMPORTED:
      DECL_EXTERNAL (inner) = 1;
      break;
    }

  if (loc)
    set_tree_location (inner, loc);

  varpool_node::get_create (inner);

  varpool_node::finalize_decl (inner);

  m_globals.safe_push (inner);

  return new lvalue (this, inner);
}

/* Implementation of the various
      gcc::jit::playback::context::new_rvalue_from_const <HOST_TYPE>
   methods.
   Each of these constructs a playback::rvalue instance (wrapping a tree).

   These specializations are required to be in the same namespace
   as the template, hence we now have to enter the gcc::jit::playback
   namespace.  */

namespace playback
{

/* Specialization of making an rvalue from a const, for host <int>.  */

template <>
rvalue *
context::
new_rvalue_from_const <int> (type *type,
			     int value)
{
  // FIXME: type-checking, or coercion?
  tree inner_type = type->as_tree ();
  if (INTEGRAL_TYPE_P (inner_type))
    {
      tree inner = build_int_cst (inner_type, value);
      return new rvalue (this, inner);
    }
  else
    {
      REAL_VALUE_TYPE real_value;
      real_from_integer (&real_value, VOIDmode, value, SIGNED);
      tree inner = build_real (inner_type, real_value);
      return new rvalue (this, inner);
    }
}

/* Specialization of making an rvalue from a const, for host <long>.  */

template <>
rvalue *
context::
new_rvalue_from_const <long> (type *type,
			      long value)
{
  // FIXME: type-checking, or coercion?
  tree inner_type = type->as_tree ();
  if (INTEGRAL_TYPE_P (inner_type))
    {
      tree inner = build_int_cst (inner_type, value);
      return new rvalue (this, inner);
    }
  else
    {
      REAL_VALUE_TYPE real_value;
      real_from_integer (&real_value, VOIDmode, value, SIGNED);
      tree inner = build_real (inner_type, real_value);
      return new rvalue (this, inner);
    }
}

/* Specialization of making an rvalue from a const, for host <double>.  */

template <>
rvalue *
context::
new_rvalue_from_const <double> (type *type,
				double value)
{
  // FIXME: type-checking, or coercion?
  tree inner_type = type->as_tree ();

  /* We have a "double", we want a REAL_VALUE_TYPE.

     real.c:real_from_target appears to require the representation to be
     split into 32-bit values, and then sent as an pair of host long
     ints.  */
  REAL_VALUE_TYPE real_value;
  union
  {
    double as_double;
    uint32_t as_uint32s[2];
  } u;
  u.as_double = value;
  long int as_long_ints[2];
  as_long_ints[0] = u.as_uint32s[0];
  as_long_ints[1] = u.as_uint32s[1];
  real_from_target (&real_value, as_long_ints, DFmode);
  tree inner = build_real (inner_type, real_value);
  return new rvalue (this, inner);
}

/* Specialization of making an rvalue from a const, for host <void *>.  */

template <>
rvalue *
context::
new_rvalue_from_const <void *> (type *type,
				void *value)
{
  tree inner_type = type->as_tree ();
  /* FIXME: how to ensure we have a wide enough type?  */
  tree inner = build_int_cstu (inner_type, (unsigned HOST_WIDE_INT)value);
  return new rvalue (this, inner);
}

/* We're done implementing the specializations of
      gcc::jit::playback::context::new_rvalue_from_const <T>
   so we can exit the gcc::jit::playback namespace.  */

} // namespace playback

/* Construct a playback::rvalue instance (wrapping a tree).  */

playback::rvalue *
playback::context::
new_string_literal (const char *value)
{
  tree t_str = build_string (strlen (value), value);
  gcc_assert (m_char_array_type_node);
  TREE_TYPE (t_str) = m_char_array_type_node;

  /* Convert to (const char*), loosely based on
     c/c-typeck.c: array_to_pointer_conversion,
     by taking address of start of string.  */
  tree t_addr = build1 (ADDR_EXPR, m_const_char_ptr, t_str);

  return new rvalue (this, t_addr);
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   vector.  */

playback::rvalue *
playback::context::new_rvalue_from_vector (location *,
					   type *type,
					   const auto_vec<rvalue *> &elements)
{
  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, elements.length ());
  for (unsigned i = 0; i < elements.length (); ++i)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, elements[i]->as_tree ());
  tree t_ctor = build_constructor (type->as_tree (), v);
  return new rvalue (this, t_ctor);
}

/* Coerce a tree expression into a boolean tree expression.  */

tree
playback::context::
as_truth_value (tree expr, location *loc)
{
  /* Compare to c-typeck.c:c_objc_common_truthvalue_conversion */
  tree typed_zero = fold_build1 (CONVERT_EXPR,
				 TREE_TYPE (expr),
				 integer_zero_node);
  if (loc)
    set_tree_location (typed_zero, loc);

  expr = build2 (NE_EXPR, integer_type_node, expr, typed_zero);
  if (loc)
    set_tree_location (expr, loc);

  return expr;
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   unary op.  */

playback::rvalue *
playback::context::
new_unary_op (location *loc,
	      enum gcc_jit_unary_op op,
	      type *result_type,
	      rvalue *a)
{
  // FIXME: type-checking, or coercion?
  enum tree_code inner_op;

  gcc_assert (result_type);
  gcc_assert (a);

  tree node = a->as_tree ();
  tree inner_result = NULL;

  switch (op)
    {
    default:
      add_error (loc, "unrecognized (enum gcc_jit_unary_op) value: %i", op);
      return NULL;

    case GCC_JIT_UNARY_OP_MINUS:
      inner_op = NEGATE_EXPR;
      break;

    case GCC_JIT_UNARY_OP_BITWISE_NEGATE:
      inner_op = BIT_NOT_EXPR;
      break;

    case GCC_JIT_UNARY_OP_LOGICAL_NEGATE:
      node = as_truth_value (node, loc);
      inner_result = invert_truthvalue (node);
      if (loc)
	set_tree_location (inner_result, loc);
      return new rvalue (this, inner_result);

    case GCC_JIT_UNARY_OP_ABS:
      inner_op = ABS_EXPR;
      break;
    }

  inner_result = build1 (inner_op,
			 result_type->as_tree (),
			 node);
  if (loc)
    set_tree_location (inner_result, loc);

  return new rvalue (this, inner_result);
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   binary op.  */

playback::rvalue *
playback::context::
new_binary_op (location *loc,
	       enum gcc_jit_binary_op op,
	       type *result_type,
	       rvalue *a, rvalue *b)
{
  // FIXME: type-checking, or coercion?
  enum tree_code inner_op;

  gcc_assert (result_type);
  gcc_assert (a);
  gcc_assert (b);

  tree node_a = a->as_tree ();
  tree node_b = b->as_tree ();

  switch (op)
    {
    default:
      add_error (loc, "unrecognized (enum gcc_jit_binary_op) value: %i", op);
      return NULL;

    case GCC_JIT_BINARY_OP_PLUS:
      inner_op = PLUS_EXPR;
      break;

    case GCC_JIT_BINARY_OP_MINUS:
      inner_op = MINUS_EXPR;
      break;

    case GCC_JIT_BINARY_OP_MULT:
      inner_op = MULT_EXPR;
      break;

    case GCC_JIT_BINARY_OP_DIVIDE:
      if (FLOAT_TYPE_P (result_type->as_tree ()))
	/* Floating-point division: */
	inner_op = RDIV_EXPR;
      else
	/* Truncating to zero: */
	inner_op = TRUNC_DIV_EXPR;
      break;

    case GCC_JIT_BINARY_OP_MODULO:
      inner_op = TRUNC_MOD_EXPR;
      break;

    case GCC_JIT_BINARY_OP_BITWISE_AND:
      inner_op = BIT_AND_EXPR;
      break;

    case GCC_JIT_BINARY_OP_BITWISE_XOR:
      inner_op = BIT_XOR_EXPR;
      break;

    case GCC_JIT_BINARY_OP_BITWISE_OR:
      inner_op = BIT_IOR_EXPR;
      break;

    case GCC_JIT_BINARY_OP_LOGICAL_AND:
      node_a = as_truth_value (node_a, loc);
      node_b = as_truth_value (node_b, loc);
      inner_op = TRUTH_ANDIF_EXPR;
      break;

    case GCC_JIT_BINARY_OP_LOGICAL_OR:
      node_a = as_truth_value (node_a, loc);
      node_b = as_truth_value (node_b, loc);
      inner_op = TRUTH_ORIF_EXPR;
      break;

    case GCC_JIT_BINARY_OP_LSHIFT:
      inner_op = LSHIFT_EXPR;
      break;

    case GCC_JIT_BINARY_OP_RSHIFT:
      inner_op = RSHIFT_EXPR;
      break;
    }

  tree inner_expr = build2 (inner_op,
			    result_type->as_tree (),
			    node_a,
			    node_b);
  if (loc)
    set_tree_location (inner_expr, loc);

  return new rvalue (this, inner_expr);
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   comparison.  */

playback::rvalue *
playback::context::
new_comparison (location *loc,
		enum gcc_jit_comparison op,
		rvalue *a, rvalue *b)
{
  // FIXME: type-checking, or coercion?
  enum tree_code inner_op;

  gcc_assert (a);
  gcc_assert (b);

  switch (op)
    {
    default:
      add_error (loc, "unrecognized (enum gcc_jit_comparison) value: %i", op);
      return NULL;

    case GCC_JIT_COMPARISON_EQ:
      inner_op = EQ_EXPR;
      break;
    case GCC_JIT_COMPARISON_NE:
      inner_op = NE_EXPR;
      break;
    case GCC_JIT_COMPARISON_LT:
      inner_op = LT_EXPR;
      break;
    case GCC_JIT_COMPARISON_LE:
      inner_op = LE_EXPR;
      break;
    case GCC_JIT_COMPARISON_GT:
      inner_op = GT_EXPR;
      break;
    case GCC_JIT_COMPARISON_GE:
      inner_op = GE_EXPR;
      break;
    }

  tree inner_expr = build2 (inner_op,
			    boolean_type_node,
			    a->as_tree (),
			    b->as_tree ());
  if (loc)
    set_tree_location (inner_expr, loc);
  return new rvalue (this, inner_expr);
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   function call.  */

playback::rvalue *
playback::context::
build_call (location *loc,
	    tree fn_ptr,
	    const auto_vec<rvalue *> *args,
	    bool require_tail_call)
{
  vec<tree, va_gc> *tree_args;
  vec_alloc (tree_args, args->length ());
  for (unsigned i = 0; i < args->length (); i++)
    tree_args->quick_push ((*args)[i]->as_tree ());

  if (loc)
    set_tree_location (fn_ptr, loc);

  tree fn = TREE_TYPE (fn_ptr);
  tree fn_type = TREE_TYPE (fn);
  tree return_type = TREE_TYPE (fn_type);

  tree call = build_call_vec (return_type,
			      fn_ptr, tree_args);

  if (require_tail_call)
    CALL_EXPR_MUST_TAIL_CALL (call) = 1;

  return new rvalue (this, call);

  /* see c-typeck.c: build_function_call
     which calls build_function_call_vec

     which does lots of checking, then:
    result = build_call_array_loc (loc, TREE_TYPE (fntype),
				   function, nargs, argarray);
    which is in tree.c
    (see also build_call_vec)
   */
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   call to a specific function.  */

playback::rvalue *
playback::context::
new_call (location *loc,
	  function *func,
	  const auto_vec<rvalue *> *args,
	  bool require_tail_call)
{
  tree fndecl;

  gcc_assert (func);

  fndecl = func->as_fndecl ();

  tree fntype = TREE_TYPE (fndecl);

  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);

  return build_call (loc, fn, args, require_tail_call);
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   call through a function pointer.  */

playback::rvalue *
playback::context::
new_call_through_ptr (location *loc,
		      rvalue *fn_ptr,
		      const auto_vec<rvalue *> *args,
		      bool require_tail_call)
{
  gcc_assert (fn_ptr);
  tree t_fn_ptr = fn_ptr->as_tree ();

  return build_call (loc, t_fn_ptr, args, require_tail_call);
}

/* Construct a tree for a cast.  */

tree
playback::context::build_cast (playback::location *loc,
			       playback::rvalue *expr,
			       playback::type *type_)
{
  /* For comparison, see:
     - c/c-typeck.c:build_c_cast
     - c/c-convert.c: convert
     - convert.h

     Only some kinds of cast are currently supported here.  */
  tree t_expr = expr->as_tree ();
  tree t_dst_type = type_->as_tree ();
  tree t_ret = NULL;
  t_ret = targetm.convert_to_type (t_dst_type, t_expr);
  if (t_ret)
      return t_ret;
  enum tree_code dst_code = TREE_CODE (t_dst_type);
  switch (dst_code)
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      t_ret = convert_to_integer (t_dst_type, t_expr);
      goto maybe_fold;

    case BOOLEAN_TYPE:
      /* Compare with c_objc_common_truthvalue_conversion and
	 c_common_truthvalue_conversion. */
      /* For now, convert to: (t_expr != 0)  */
      t_ret = build2 (NE_EXPR, t_dst_type,
		      t_expr,
		      build_int_cst (TREE_TYPE (t_expr), 0));
      goto maybe_fold;

    case REAL_TYPE:
      t_ret = convert_to_real (t_dst_type, t_expr);
      goto maybe_fold;

    case POINTER_TYPE:
      t_ret = build1 (NOP_EXPR, t_dst_type, t_expr);
      goto maybe_fold;

    default:
      add_error (loc, "couldn't handle cast during playback");
      fprintf (stderr, "input expression:\n");
      debug_tree (t_expr);
      fprintf (stderr, "requested type:\n");
      debug_tree (t_dst_type);
      return error_mark_node;

    maybe_fold:
      if (TREE_CODE (t_ret) != C_MAYBE_CONST_EXPR)
	t_ret = fold (t_ret);
      return t_ret;
    }
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   cast.  */

playback::rvalue *
playback::context::
new_cast (playback::location *loc,
	  playback::rvalue *expr,
	  playback::type *type_)
{

  tree t_cast = build_cast (loc, expr, type_);
  if (loc)
    set_tree_location (t_cast, loc);
  return new rvalue (this, t_cast);
}

/* Construct a playback::lvalue instance (wrapping a tree) for an
   array access.  */

playback::lvalue *
playback::context::
new_array_access (location *loc,
		  rvalue *ptr,
		  rvalue *index)
{
  gcc_assert (ptr);
  gcc_assert (index);

  /* For comparison, see:
       c/c-typeck.c: build_array_ref
       c-family/c-common.c: pointer_int_sum
  */
  tree t_ptr = ptr->as_tree ();
  tree t_index = index->as_tree ();
  tree t_type_ptr = TREE_TYPE (t_ptr);
  tree t_type_star_ptr = TREE_TYPE (t_type_ptr);

  if (TREE_CODE (t_type_ptr) == ARRAY_TYPE)
    {
      tree t_result = build4 (ARRAY_REF, t_type_star_ptr, t_ptr, t_index,
			      NULL_TREE, NULL_TREE);
      if (loc)
	set_tree_location (t_result, loc);
      return new lvalue (this, t_result);
    }
  else
    {
      /* Convert index to an offset in bytes.  */
      tree t_sizeof = size_in_bytes (t_type_star_ptr);
      t_index = fold_build1 (CONVERT_EXPR, sizetype, t_index);
      tree t_offset = build2 (MULT_EXPR, sizetype, t_index, t_sizeof);

      /* Locate (ptr + offset).  */
      tree t_address = build2 (POINTER_PLUS_EXPR, t_type_ptr, t_ptr, t_offset);

      tree t_indirection = build1 (INDIRECT_REF, t_type_star_ptr, t_address);
      if (loc)
	{
	  set_tree_location (t_sizeof, loc);
	  set_tree_location (t_offset, loc);
	  set_tree_location (t_address, loc);
	  set_tree_location (t_indirection, loc);
	}

      return new lvalue (this, t_indirection);
    }
}

/* Construct a tree for a field access.  */

tree
playback::context::
new_field_access (location *loc,
		  tree datum,
		  field *field)
{
  gcc_assert (datum);
  gcc_assert (field);

  /* Compare with c/c-typeck.c:lookup_field, build_indirect_ref, and
     build_component_ref. */
  tree type = TREE_TYPE (datum);
  gcc_assert (type);
  gcc_assert (TREE_CODE (type) != POINTER_TYPE);

 tree t_field = field->as_tree ();
 tree ref = build3 (COMPONENT_REF, TREE_TYPE (t_field), datum,
		     t_field, NULL_TREE);
  if (loc)
    set_tree_location (ref, loc);
  return ref;
}

/* Construct a tree for a dereference.  */

tree
playback::context::
new_dereference (tree ptr,
		 location *loc)
{
  gcc_assert (ptr);

  tree type = TREE_TYPE (TREE_TYPE(ptr));
  tree datum = build1 (INDIRECT_REF, type, ptr);
  if (loc)
    set_tree_location (datum, loc);
  return datum;
}

/* Construct a playback::type instance (wrapping a tree)
   with the given alignment.  */

playback::type *
playback::type::
get_aligned (size_t alignment_in_bytes) const
{
  tree t_new_type = build_variant_type_copy (m_inner);

  SET_TYPE_ALIGN (t_new_type, alignment_in_bytes * BITS_PER_UNIT);
  TYPE_USER_ALIGN (t_new_type) = 1;

  return new type (t_new_type);
}

/* Construct a playback::type instance (wrapping a tree)
   for the given vector type.  */

playback::type *
playback::type::
get_vector (size_t num_units) const
{
  tree t_new_type = build_vector_type (m_inner, num_units);
  return new type (t_new_type);
}

/* Construct a playback::lvalue instance (wrapping a tree) for a
   field access.  */

playback::lvalue *
playback::lvalue::
access_field (location *loc,
	      field *field)
{
  tree datum = as_tree ();
  tree ref = get_context ()->new_field_access (loc, datum, field);
  if (!ref)
    return NULL;
  return new lvalue (get_context (), ref);
}

/* Construct a playback::rvalue instance (wrapping a tree) for a
   field access.  */

playback::rvalue *
playback::rvalue::
access_field (location *loc,
	      field *field)
{
  tree datum = as_tree ();
  tree ref = get_context ()->new_field_access (loc, datum, field);
  if (!ref)
    return NULL;
  return new rvalue (get_context (), ref);
}

/* Construct a playback::lvalue instance (wrapping a tree) for a
   dereferenced field access.  */

playback::lvalue *
playback::rvalue::
dereference_field (location *loc,
		   field *field)
{
  tree ptr = as_tree ();
  tree datum = get_context ()->new_dereference (ptr, loc);
  if (!datum)
    return NULL;
  tree ref = get_context ()->new_field_access (loc, datum, field);
  if (!ref)
    return NULL;
  return new lvalue (get_context (), ref);
}

/* Construct a playback::lvalue instance (wrapping a tree) for a
   dereference.  */

playback::lvalue *
playback::rvalue::
dereference (location *loc)
{
  tree ptr = as_tree ();
  tree datum = get_context ()->new_dereference (ptr, loc);
  return new lvalue (get_context (), datum);
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Compare with e.g. c/c-typeck.c: c_mark_addressable.  */

static void
jit_mark_addressable (tree exp)
{
  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	/* (we don't yet support bitfields)  */
	/* fallthrough */
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case COMPOUND_LITERAL_EXPR:
      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	/* (we don't have a concept of a "register" declaration) */
	/* fallthrough */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
	/* fallthrough */
      default:
	return;
      }
}

/* Construct a playback::rvalue instance (wrapping a tree) for an
   address-lookup.  */

playback::rvalue *
playback::lvalue::
get_address (location *loc)
{
  tree t_lvalue = as_tree ();
  tree t_thistype = TREE_TYPE (t_lvalue);
  tree t_ptrtype = build_pointer_type (t_thistype);
  tree ptr = build1 (ADDR_EXPR, t_ptrtype, t_lvalue);
  if (loc)
    get_context ()->set_tree_location (ptr, loc);
  jit_mark_addressable (t_lvalue);
  return new rvalue (get_context (), ptr);
}

/* The wrapper subclasses are GC-managed, but can own non-GC memory.
   Provide this finalization hook for calling then they are collected,
   which calls the finalizer vfunc.  This allows them to call "release"
   on any vec<> within them.  */

static void
wrapper_finalizer (void *ptr)
{
  playback::wrapper *wrapper = reinterpret_cast <playback::wrapper *> (ptr);
  wrapper->finalizer ();
}

/* gcc::jit::playback::wrapper subclasses are GC-managed:
   allocate them using ggc_internal_cleared_alloc.  */

void *
playback::wrapper::
operator new (size_t sz)
{
  return ggc_internal_cleared_alloc (sz, wrapper_finalizer, 0, 1);

}

/* Constructor for gcc:jit::playback::function.  */

playback::function::
function (context *ctxt,
	  tree fndecl,
	  enum gcc_jit_function_kind kind)
: m_ctxt(ctxt),
  m_inner_fndecl (fndecl),
  m_inner_bind_expr (NULL),
  m_kind (kind)
{
  if (m_kind != GCC_JIT_FUNCTION_IMPORTED)
    {
      /* Create a BIND_EXPR, and within it, a statement list.  */
      m_stmt_list = alloc_stmt_list ();
      m_stmt_iter = tsi_start (m_stmt_list);
      m_inner_block = make_node (BLOCK);
      m_inner_bind_expr =
	build3 (BIND_EXPR, void_type_node, NULL, m_stmt_list, m_inner_block);
    }
  else
    {
      m_inner_block = NULL;
      m_stmt_list = NULL;
    }
}

/* Hand-written GC-marking hook for playback functions.  */

void
playback::function::
gt_ggc_mx ()
{
  gt_ggc_m_9tree_node (m_inner_fndecl);
  gt_ggc_m_9tree_node (m_inner_bind_expr);
  gt_ggc_m_9tree_node (m_stmt_list);
  gt_ggc_m_9tree_node (m_inner_block);
}

/* Don't leak vec's internal buffer (in non-GC heap) when we are
   GC-ed.  */

void
playback::function::finalizer ()
{
  m_blocks.release ();
}

/* Get the return type of a playback function, in tree form.  */

tree
playback::function::
get_return_type_as_tree () const
{
  return TREE_TYPE (TREE_TYPE(m_inner_fndecl));
}

/* Construct a new local within this playback::function.  */

playback::lvalue *
playback::function::
new_local (location *loc,
	   type *type,
	   const char *name)
{
  gcc_assert (type);
  gcc_assert (name);
  tree inner = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			   get_identifier (name),
			   type->as_tree ());
  DECL_CONTEXT (inner) = this->m_inner_fndecl;

  /* Prepend to BIND_EXPR_VARS: */
  DECL_CHAIN (inner) = BIND_EXPR_VARS (m_inner_bind_expr);
  BIND_EXPR_VARS (m_inner_bind_expr) = inner;

  if (loc)
    set_tree_location (inner, loc);
  return new lvalue (m_ctxt, inner);
}

/* Construct a new block within this playback::function.  */

playback::block *
playback::function::
new_block (const char *name)
{
  gcc_assert (m_kind != GCC_JIT_FUNCTION_IMPORTED);

  block *result = new playback::block (this, name);
  m_blocks.safe_push (result);
  return result;
}

/* Construct a playback::rvalue instance wrapping an ADDR_EXPR for
   this playback::function.  */

playback::rvalue *
playback::function::get_address (location *loc)
{
  tree t_fndecl = as_fndecl ();
  tree t_fntype = TREE_TYPE (t_fndecl);
  tree t_fnptr = build1 (ADDR_EXPR, build_pointer_type (t_fntype), t_fndecl);
  if (loc)
    m_ctxt->set_tree_location (t_fnptr, loc);
  return new rvalue (m_ctxt, t_fnptr);
}

/* Build a statement list for the function as a whole out of the
   lists of statements for the individual blocks, building labels
   for each block.  */

void
playback::function::
build_stmt_list ()
{
  int i;
  block *b;

  JIT_LOG_SCOPE (m_ctxt->get_logger ());

  FOR_EACH_VEC_ELT (m_blocks, i, b)
    {
      int j;
      tree stmt;

      b->m_label_expr = build1 (LABEL_EXPR,
				void_type_node,
				b->as_label_decl ());
      tsi_link_after (&m_stmt_iter, b->m_label_expr, TSI_CONTINUE_LINKING);

      FOR_EACH_VEC_ELT (b->m_stmts, j, stmt)
	tsi_link_after (&m_stmt_iter, stmt, TSI_CONTINUE_LINKING);
    }
}

/* Finish compiling the given function, potentially running the
   garbage-collector.
   The function will have a statement list by now.
   Amongst other things, this gimplifies the statement list,
   and calls cgraph_node::finalize_function on the function.  */

void
playback::function::
postprocess ()
{
  JIT_LOG_SCOPE (m_ctxt->get_logger ());

  if (m_ctxt->get_bool_option (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE))
    debug_tree (m_stmt_list);

  /* Do we need this to force cgraphunit.c to output the function? */
  if (m_kind == GCC_JIT_FUNCTION_EXPORTED)
    {
      DECL_EXTERNAL (m_inner_fndecl) = 0;
      DECL_PRESERVE_P (m_inner_fndecl) = 1;
    }

  if (m_kind == GCC_JIT_FUNCTION_INTERNAL
      ||m_kind == GCC_JIT_FUNCTION_ALWAYS_INLINE)
    {
      DECL_EXTERNAL (m_inner_fndecl) = 0;
      TREE_PUBLIC (m_inner_fndecl) = 0;
    }

  if (m_kind != GCC_JIT_FUNCTION_IMPORTED)
    {
      /* Seem to need this in gimple-low.c: */
      gcc_assert (m_inner_block);
      DECL_INITIAL (m_inner_fndecl) = m_inner_block;

      /* how to add to function? the following appears to be how to
	 set the body of a m_inner_fndecl: */
      DECL_SAVED_TREE(m_inner_fndecl) = m_inner_bind_expr;

      /* Ensure that locals appear in the debuginfo.  */
      BLOCK_VARS (m_inner_block) = BIND_EXPR_VARS (m_inner_bind_expr);

      //debug_tree (m_inner_fndecl);

      /* Convert to gimple: */
      //printf("about to gimplify_function_tree\n");
      gimplify_function_tree (m_inner_fndecl);
      //printf("finished gimplify_function_tree\n");

      current_function_decl = m_inner_fndecl;
      if (m_ctxt->get_bool_option (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE))
	dump_function_to_file (m_inner_fndecl, stderr, TDF_VOPS|TDF_MEMSYMS|TDF_LINENO);
      //debug_tree (m_inner_fndecl);

      //printf("about to add to cgraph\n");
      /* Add to cgraph: */
      cgraph_node::finalize_function (m_inner_fndecl, false);
      /* This can trigger a collection, so we need to have all of
	 the funcs as roots.  */

      current_function_decl = NULL;
    }
}

/* Don't leak vec's internal buffer (in non-GC heap) when we are
   GC-ed.  */

void
playback::block::finalizer ()
{
  m_stmts.release ();
}

/* Add an eval of the rvalue to the function's statement list.  */

void
playback::block::
add_eval (location *loc,
	  rvalue *rvalue)
{
  gcc_assert (rvalue);

  if (loc)
    set_tree_location (rvalue->as_tree (), loc);

  add_stmt (rvalue->as_tree ());
}

/* Add an assignment to the function's statement list.  */

void
playback::block::
add_assignment (location *loc,
		lvalue *lvalue,
		rvalue *rvalue)
{
  gcc_assert (lvalue);
  gcc_assert (rvalue);

  tree t_lvalue = lvalue->as_tree ();
  tree t_rvalue = rvalue->as_tree ();
  if (TREE_TYPE (t_rvalue) != TREE_TYPE (t_lvalue))
    {
      t_rvalue = build1 (CONVERT_EXPR,
			 TREE_TYPE (t_lvalue),
			 t_rvalue);
      if (loc)
	set_tree_location (t_rvalue, loc);
    }

  tree stmt =
    build2 (MODIFY_EXPR, TREE_TYPE (t_lvalue),
	    t_lvalue, t_rvalue);
  if (loc)
    set_tree_location (stmt, loc);
  add_stmt (stmt);
}

/* Add a comment to the function's statement list.
   For now this is done by adding a dummy label.  */

void
playback::block::
add_comment (location *loc,
	     const char *text)
{
  /* Wrap the text in C-style comment delimiters.  */
  size_t sz =
    (3 /* opening delim */
     + strlen (text)
     + 3 /* closing delim */
     + 1 /* terminator */);
  char *wrapped = (char *)ggc_internal_alloc (sz);
  snprintf (wrapped, sz, "/* %s */", text);

  /* For now we simply implement this by adding a dummy label with a name
     containing the given text.  */
  tree identifier = get_identifier (wrapped);
  tree label_decl = build_decl (UNKNOWN_LOCATION, LABEL_DECL,
				identifier, void_type_node);
  DECL_CONTEXT (label_decl) = m_func->as_fndecl ();

  tree label_expr = build1 (LABEL_EXPR, void_type_node, label_decl);
  if (loc)
    set_tree_location (label_expr, loc);
  add_stmt (label_expr);
}

/* Add a conditional jump statement to the function's statement list.  */

void
playback::block::
add_conditional (location *loc,
		 rvalue *boolval,
		 block *on_true,
		 block *on_false)
{
  gcc_assert (boolval);
  gcc_assert (on_true);
  gcc_assert (on_false);

  /* COND_EXPR wants statement lists for the true/false operands, but we
     want labels.
     Shim it by creating jumps to the labels */
  tree true_jump = build1 (GOTO_EXPR, void_type_node,
			   on_true->as_label_decl ());
  if (loc)
    set_tree_location (true_jump, loc);

  tree false_jump = build1 (GOTO_EXPR, void_type_node,
			    on_false->as_label_decl ());
  if (loc)
    set_tree_location (false_jump, loc);

  tree stmt =
    build3 (COND_EXPR, void_type_node, boolval->as_tree (),
	    true_jump, false_jump);
  if (loc)
    set_tree_location (stmt, loc);
  add_stmt (stmt);
}

/* Add an unconditional jump statement to the function's statement list.  */

void
playback::block::
add_jump (location *loc,
	  block *target)
{
  gcc_assert (target);

  // see c_finish_loop
  //tree top = build1 (LABEL_EXPR, void_type_node, NULL_TREE);
  //add_stmt (top);

  //tree stmt = build_and_jump (&LABEL_EXPR_LABEL (target->label_));
  TREE_USED (target->as_label_decl ()) = 1;
  tree stmt = build1 (GOTO_EXPR, void_type_node, target->as_label_decl ());
  if (loc)
    set_tree_location (stmt, loc);
  add_stmt (stmt);

  /*
  from c-typeck.c:
tree
c_finish_goto_label (location_t loc, tree label)
{
  tree decl = lookup_label_for_goto (loc, label);
  if (!decl)
    return NULL_TREE;
  TREE_USED (decl) = 1;
  {
    tree t = build1 (GOTO_EXPR, void_type_node, decl);
    SET_EXPR_LOCATION (t, loc);
    return add_stmt (t);
  }
}
  */

}

/* Add a return statement to the function's statement list.  */

void
playback::block::
add_return (location *loc,
	    rvalue *rvalue)
{
  tree modify_retval = NULL;
  tree return_type = m_func->get_return_type_as_tree ();
  if (rvalue)
    {
      tree t_lvalue = DECL_RESULT (m_func->as_fndecl ());
      tree t_rvalue = rvalue->as_tree ();
      if (TREE_TYPE (t_rvalue) != TREE_TYPE (t_lvalue))
	t_rvalue = build1 (CONVERT_EXPR,
			   TREE_TYPE (t_lvalue),
			   t_rvalue);
      modify_retval = build2 (MODIFY_EXPR, return_type,
			      t_lvalue, t_rvalue);
      if (loc)
	set_tree_location (modify_retval, loc);
    }
  tree return_stmt = build1 (RETURN_EXPR, return_type,
			     modify_retval);
  if (loc)
    set_tree_location (return_stmt, loc);

  add_stmt (return_stmt);
}

/* Helper function for playback::block::add_switch.
   Construct a case label for the given range, followed by a goto stmt
   to the given block, appending them to stmt list *ptr_t_switch_body.  */

static void
add_case (tree *ptr_t_switch_body,
	  tree t_low_value,
	  tree t_high_value,
	  playback::block *dest_block)
{
  tree t_label = create_artificial_label (UNKNOWN_LOCATION);
  DECL_CONTEXT (t_label) = dest_block->get_function ()->as_fndecl ();

  tree t_case_label =
    build_case_label (t_low_value, t_high_value, t_label);
  append_to_statement_list (t_case_label, ptr_t_switch_body);

  tree t_goto_stmt =
    build1 (GOTO_EXPR, void_type_node, dest_block->as_label_decl ());
  append_to_statement_list (t_goto_stmt, ptr_t_switch_body);
}

/* Add a switch statement to the function's statement list.

   My initial attempt at implementing this constructed a TREE_VEC
   of the cases and set it as SWITCH_LABELS (switch_expr).  However,
   gimplify.c:gimplify_switch_expr is set up to deal with SWITCH_BODY, and
   doesn't have any logic for gimplifying SWITCH_LABELS.

   Hence we create a switch body, and populate it with case labels, each
   followed by a goto to the desired block.  */

void
playback::block::
add_switch (location *loc,
	    rvalue *expr,
	    block *default_block,
	    const auto_vec <case_> *cases)
{
  /* Compare with:
     - c/c-typeck.c: c_start_case
     - c-family/c-common.c:c_add_case_label
     - java/expr.c:expand_java_switch and expand_java_add_case
     We've already rejected overlaps and duplicates in
     libgccjit.c:case_range_validator::validate.  */

  tree t_expr = expr->as_tree ();
  tree t_type = TREE_TYPE (t_expr);

  tree t_switch_body = alloc_stmt_list ();

  int i;
  case_ *c;
  FOR_EACH_VEC_ELT (*cases, i, c)
    {
      tree t_low_value = c->m_min_value->as_tree ();
      tree t_high_value = c->m_max_value->as_tree ();
      add_case (&t_switch_body,
		t_low_value,
		t_high_value,
		c->m_dest_block);
    }
  /* Default label. */
  add_case (&t_switch_body,
	    NULL_TREE, NULL_TREE,
	    default_block);

  tree switch_stmt = build3 (SWITCH_EXPR, t_type, t_expr,
			     t_switch_body, NULL_TREE);
  if (loc)
    set_tree_location (switch_stmt, loc);
  add_stmt (switch_stmt);
}

/* Constructor for gcc::jit::playback::block.  */

playback::block::
block (function *func,
       const char *name)
: m_func (func),
  m_stmts ()
{
  tree identifier;

  gcc_assert (func);
  // name can be NULL
  if (name)
    identifier = get_identifier (name);
  else
    identifier = NULL;
  m_label_decl = build_decl (UNKNOWN_LOCATION, LABEL_DECL,
			    identifier, void_type_node);
  DECL_CONTEXT (m_label_decl) = func->as_fndecl ();
  m_label_expr = NULL;
}

/* A subclass of auto_vec <char *> that frees all of its elements on
   deletion.  */

class auto_argvec : public auto_vec <char *>
{
 public:
  ~auto_argvec ();
};

/* auto_argvec's dtor, freeing all contained strings, automatically
   chaining up to ~auto_vec <char *>, which frees the internal buffer.  */

auto_argvec::~auto_argvec ()
{
  int i;
  char *str;
  FOR_EACH_VEC_ELT (*this, i, str)
    free (str);
}

/* Compile a playback::context:

   - Use the context's options to cconstruct command-line options, and
     call into the rest of GCC (toplev::main).
   - Assuming it succeeds, we have a .s file.
   - We then run the "postprocess" vfunc:

     (A) In-memory compile ("gcc_jit_context_compile")

       For an in-memory compile we have the playback::compile_to_memory
       subclass; "postprocess" will convert the .s file to a .so DSO,
       and load it in memory (via dlopen), wrapping the result up as
       a jit::result and returning it.

     (B) Compile to file ("gcc_jit_context_compile_to_file")

       When compiling to a file, we have the playback::compile_to_file
       subclass; "postprocess" will either copy the .s file to the
       destination (for GCC_JIT_OUTPUT_KIND_ASSEMBLER), or invoke
       the driver to convert it as necessary, copying the result.  */

void
playback::context::
compile ()
{
  JIT_LOG_SCOPE (get_logger ());

  const char *ctxt_progname;

  int keep_intermediates =
    get_bool_option (GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES);

  m_tempdir = new tempdir (get_logger (), keep_intermediates);
  if (!m_tempdir->create ())
    return;

  /* Call into the rest of gcc.
     For now, we have to assemble command-line options to pass into
     toplev::main, so that they can be parsed. */

  /* Pass in user-provided program name as argv0, if any, so that it
     makes it into GCC's "progname" global, used in various diagnostics. */
  ctxt_progname = get_str_option (GCC_JIT_STR_OPTION_PROGNAME);

  if (!ctxt_progname)
    ctxt_progname = "libgccjit.so";

  auto_vec <recording::requested_dump> requested_dumps;
  m_recording_ctxt->get_all_requested_dumps (&requested_dumps);

  /* Acquire the JIT mutex and set "this" as the active playback ctxt.  */
  acquire_mutex ();

  auto_argvec fake_args;
  make_fake_args (&fake_args, ctxt_progname, &requested_dumps);
  if (errors_occurred ())
    {
      release_mutex ();
      return;
    }

  /* This runs the compiler.  */
  toplev toplev (get_timer (), /* external_timer */
		 false); /* init_signals */
  enter_scope ("toplev::main");
  if (get_logger ())
    for (unsigned i = 0; i < fake_args.length (); i++)
      get_logger ()->log ("argv[%i]: %s", i, fake_args[i]);
  toplev.main (fake_args.length (),
	       const_cast <char **> (fake_args.address ()));
  exit_scope ("toplev::main");

  /* Extracting dumps makes use of the gcc::dump_manager, hence we
     need to do it between toplev::main (which creates the dump manager)
     and toplev::finalize (which deletes it).  */
  extract_any_requested_dumps (&requested_dumps);

  /* Clean up the compiler.  */
  enter_scope ("toplev::finalize");
  toplev.finalize ();
  exit_scope ("toplev::finalize");

  /* Ideally we would release the jit mutex here, but we can't yet since
     followup activities use timevars, which are global state.  */

  if (errors_occurred ())
    {
      release_mutex ();
      return;
    }

  if (get_bool_option (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE))
    dump_generated_code ();

  /* We now have a .s file.

     Run any postprocessing steps.  This will either convert the .s file to
     a .so DSO, and load it in memory (playback::compile_to_memory), or
     convert the .s file to the requested output format, and copy it to a
     given file (playback::compile_to_file).  */
  postprocess (ctxt_progname);

  release_mutex ();
}

/* Implementation of class gcc::jit::playback::compile_to_memory,
   a subclass of gcc::jit::playback::context.  */

/*  playback::compile_to_memory's trivial constructor. */

playback::compile_to_memory::compile_to_memory (recording::context *ctxt) :
  playback::context (ctxt),
  m_result (NULL)
{
  JIT_LOG_SCOPE (get_logger ());
}

/*  Implementation of the playback::context::process vfunc for compiling
    to memory.

    Convert the .s file to a .so DSO, and load it in memory (via dlopen),
    wrapping the result up as a jit::result and returning it.  */

void
playback::compile_to_memory::postprocess (const char *ctxt_progname)
{
  JIT_LOG_SCOPE (get_logger ());
  convert_to_dso (ctxt_progname);
  if (errors_occurred ())
    return;
  m_result = dlopen_built_dso ();
}

/* Implementation of class gcc::jit::playback::compile_to_file,
   a subclass of gcc::jit::playback::context.  */

/*  playback::compile_to_file's trivial constructor. */

playback::compile_to_file::compile_to_file (recording::context *ctxt,
					    enum gcc_jit_output_kind output_kind,
					    const char *output_path) :
  playback::context (ctxt),
  m_output_kind (output_kind),
  m_output_path (output_path)
{
  JIT_LOG_SCOPE (get_logger ());
}

/*  Implementation of the playback::context::process vfunc for compiling
    to a file.

    Either copy the .s file to the given destination (for
    GCC_JIT_OUTPUT_KIND_ASSEMBLER), or invoke the driver to convert it
    as necessary, copying the result.  */

void
playback::compile_to_file::postprocess (const char *ctxt_progname)
{
  JIT_LOG_SCOPE (get_logger ());

  /* The driver takes different actions based on the filename, so
     we provide a filename with an appropriate suffix for the
     output kind, and then copy it up to the user-provided path,
     rather than directly compiling it to the requested output path.  */

  switch (m_output_kind)
    {
    default:
      gcc_unreachable ();

    case GCC_JIT_OUTPUT_KIND_ASSEMBLER:
      copy_file (get_tempdir ()->get_path_s_file (),
		 m_output_path);
      /* The .s file is automatically unlinked by tempdir::~tempdir.  */
      break;

    case GCC_JIT_OUTPUT_KIND_OBJECT_FILE:
      {
	char *tmp_o_path = ::concat (get_tempdir ()->get_path (),
				     "/fake.o",
				     NULL);
	invoke_driver (ctxt_progname,
		       get_tempdir ()->get_path_s_file (),
		       tmp_o_path,
		       TV_ASSEMBLE,
		       false, /* bool shared, */
		       false);/* bool run_linker */
	if (!errors_occurred ())
	  {
	    copy_file (tmp_o_path,
		       m_output_path);
	    get_tempdir ()->add_temp_file (tmp_o_path);
	  }
	else
	  free (tmp_o_path);
      }
      break;

    case GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY:
      invoke_driver (ctxt_progname,
		     get_tempdir ()->get_path_s_file (),
		     get_tempdir ()->get_path_so_file (),
		     TV_ASSEMBLE,
		     true, /* bool shared, */
		     true);/* bool run_linker */
      if (!errors_occurred ())
	copy_file (get_tempdir ()->get_path_so_file (),
		   m_output_path);
      /* The .so file is automatically unlinked by tempdir::~tempdir.  */
      break;

    case GCC_JIT_OUTPUT_KIND_EXECUTABLE:
      {
	char *tmp_exe_path = ::concat (get_tempdir ()->get_path (),
				     "/fake.exe",
				     NULL);
	invoke_driver (ctxt_progname,
		       get_tempdir ()->get_path_s_file (),
		       tmp_exe_path,
		       TV_ASSEMBLE,
		       false, /* bool shared, */
		       true);/* bool run_linker */
	if (!errors_occurred ())
	  {
	    copy_file (tmp_exe_path,
		       m_output_path);
	    get_tempdir ()->add_temp_file (tmp_exe_path);
	  }
	else
	  free (tmp_exe_path);
      }
      break;

    }

}

/* Copy SRC_PATH to DST_PATH, preserving permission bits (in particular,
   the "executable" bits).

   Any errors that occur are reported on the context and hence count as
   a failure of the compile.

   We can't in general hardlink or use "rename" from the tempdir since
   it might be on a different filesystem to the destination.  For example,
   I get EXDEV: "Invalid cross-device link".  */

void
playback::compile_to_file::copy_file (const char *src_path,
				      const char *dst_path)
{
  JIT_LOG_SCOPE (get_logger ());
  if (get_logger ())
    {
      get_logger ()->log ("src_path: %s", src_path);
      get_logger ()->log ("dst_path: %s", dst_path);
    }

  FILE *f_in = NULL;
  FILE *f_out = NULL;
  size_t total_sz_in = 0;
  size_t total_sz_out = 0;
  char buf[4096];
  size_t sz_in;
  struct stat stat_buf;

  f_in = fopen (src_path, "rb");
  if (!f_in)
    {
      add_error (NULL,
		 "unable to open %s for reading: %s",
		 src_path,
		 xstrerror (errno));
      return;
    }

  /* Use stat on the filedescriptor to get the mode,
     so that we can copy it over (in particular, the
     "executable" bits).  */
  if (-1 == fstat (fileno (f_in), &stat_buf))
    {
      add_error (NULL,
		 "unable to fstat %s: %s",
		 src_path,
		 xstrerror (errno));
      fclose (f_in);
      return;
    }

  f_out = fopen (dst_path, "wb");
  if (!f_out)
    {
      add_error (NULL,
		 "unable to open %s for writing: %s",
		 dst_path,
		 xstrerror (errno));
      fclose (f_in);
      return;
    }

  while ( (sz_in = fread (buf, 1, sizeof (buf), f_in)) )
    {
      total_sz_in += sz_in;
      size_t sz_out_remaining = sz_in;
      size_t sz_out_so_far = 0;
      while (sz_out_remaining)
	{
	  size_t sz_out = fwrite (buf + sz_out_so_far,
				  1,
				  sz_out_remaining,
				  f_out);
	  gcc_assert (sz_out <= sz_out_remaining);
	  if (!sz_out)
	    {
	      add_error (NULL,
			 "error writing to %s: %s",
			 dst_path,
			 xstrerror (errno));
	      fclose (f_in);
	      fclose (f_out);
	      return;
	    }
	  total_sz_out += sz_out;
	  sz_out_so_far += sz_out;
	  sz_out_remaining -= sz_out;
	}
      gcc_assert (sz_out_so_far == sz_in);
    }

  if (!feof (f_in))
    add_error (NULL,
	       "error reading from %s: %s",
	       src_path,
	       xstrerror (errno));

  fclose (f_in);

  gcc_assert (total_sz_in == total_sz_out);
  if (get_logger ())
    get_logger ()->log ("total bytes copied: %ld", total_sz_out);

  /* Set the permissions of the copy to those of the original file,
     in particular the "executable" bits.  */
  if (-1 == fchmod (fileno (f_out), stat_buf.st_mode))
    add_error (NULL,
	       "error setting mode of %s: %s",
	       dst_path,
	       xstrerror (errno));

  fclose (f_out);
}

/* Helper functions for gcc::jit::playback::context::compile.  */

/* This mutex guards gcc::jit::recording::context::compile, so that only
   one thread can be accessing the bulk of GCC's state at once.  */

static pthread_mutex_t jit_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Acquire jit_mutex and set "this" as the active playback ctxt.  */

void
playback::context::acquire_mutex ()
{
  auto_timevar tv (get_timer (), TV_JIT_ACQUIRING_MUTEX);

  /* Acquire the big GCC mutex. */
  JIT_LOG_SCOPE (get_logger ());
  pthread_mutex_lock (&jit_mutex);
  gcc_assert (NULL == active_playback_ctxt);
  active_playback_ctxt = this;
}

/* Release jit_mutex and clear the active playback ctxt.  */

void
playback::context::release_mutex ()
{
  /* Release the big GCC mutex. */
  JIT_LOG_SCOPE (get_logger ());
  gcc_assert (active_playback_ctxt == this);
  active_playback_ctxt = NULL;
  pthread_mutex_unlock (&jit_mutex);
}

/* Callback used by gcc::jit::playback::context::make_fake_args when
   invoking driver_get_configure_time_options.
   Populate a vec <char * > with the configure-time options.  */

static void
append_arg_from_driver (const char *option, void *user_data)
{
  gcc_assert (option);
  gcc_assert (user_data);
  vec <char *> *argvec = static_cast <vec <char *> *> (user_data);
  argvec->safe_push (concat ("-", option, NULL));
}

/* Build a fake argv for toplev::main from the options set
   by the user on the context .  */

void
playback::context::
make_fake_args (vec <char *> *argvec,
		const char *ctxt_progname,
		vec <recording::requested_dump> *requested_dumps)
{
  JIT_LOG_SCOPE (get_logger ());

#define ADD_ARG(arg) argvec->safe_push (xstrdup (arg))
#define ADD_ARG_TAKE_OWNERSHIP(arg) argvec->safe_push (arg)

  ADD_ARG (ctxt_progname);
  ADD_ARG (get_path_c_file ());
  ADD_ARG ("-fPIC");

  /* Handle int options: */
  switch (get_int_option (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL))
    {
    default:
      add_error (NULL,
		 "unrecognized optimization level: %i",
		 get_int_option (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL));
      return;

    case 0:
      ADD_ARG ("-O0");
      break;

    case 1:
      ADD_ARG ("-O1");
      break;

    case 2:
      ADD_ARG ("-O2");
      break;

    case 3:
      ADD_ARG ("-O3");
      break;
    }
  /* What about -Os? */

  /* Handle bool options: */
  if (get_bool_option (GCC_JIT_BOOL_OPTION_DEBUGINFO))
    ADD_ARG ("-g");

  /* Suppress timing (and other) info.  */
  if (!get_bool_option (GCC_JIT_BOOL_OPTION_DUMP_SUMMARY))
    {
      ADD_ARG ("-quiet");
      quiet_flag = 1;
    }

  /* Aggressively garbage-collect, to shake out bugs: */
  if (get_bool_option (GCC_JIT_BOOL_OPTION_SELFCHECK_GC))
    {
      ADD_ARG ("--param");
      ADD_ARG ("ggc-min-expand=0");
      ADD_ARG ("--param");
      ADD_ARG ("ggc-min-heapsize=0");
    }

  if (get_bool_option (GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING))
    {
      ADD_ARG ("-fdump-tree-all");
      ADD_ARG ("-fdump-rtl-all");
      ADD_ARG ("-fdump-ipa-all");
    }

  /* Add "-fdump-" options for any calls to
     gcc_jit_context_enable_dump.  */
  {
    int i;
    recording::requested_dump *d;
    FOR_EACH_VEC_ELT (*requested_dumps, i, d)
      {
	char *arg = concat ("-fdump-", d->m_dumpname, NULL);
	ADD_ARG_TAKE_OWNERSHIP (arg);
      }
  }

  /* PR jit/64810: Add any target-specific default options
     from OPTION_DEFAULT_SPECS, normally provided by the driver
     in the non-jit case.

     The target-specific code can define OPTION_DEFAULT_SPECS:
     default command options in the form of spec macros for the
     driver to expand ().

     For cc1 etc, the driver processes OPTION_DEFAULT_SPECS and,
     if not overriden, injects the defaults as extra arguments to
     cc1 etc.
     For the jit case, we need to add these arguments here.  The
     input format (using the specs language) means that we have to run
     part of the driver code here (driver_get_configure_time_options).

     To avoid running the spec-expansion code every time, we just do
     it the first time (via a function-static flag), saving the result
     into a function-static vec.
     This flag and vec are global state (i.e. per-process).
     They are guarded by the jit mutex.  */
  {
    static bool have_configure_time_options = false;
    static vec <char *> configure_time_options;

    if (have_configure_time_options)
      log ("reusing cached configure-time options");
    else
      {
	have_configure_time_options = true;
	log ("getting configure-time options from driver");
	driver_get_configure_time_options (append_arg_from_driver,
					   &configure_time_options);
      }

    int i;
    char *opt;

    if (get_logger ())
      FOR_EACH_VEC_ELT (configure_time_options, i, opt)
	log ("configure_time_options[%i]: %s", i, opt);

    /* configure_time_options should now contain the expanded options
       from OPTION_DEFAULT_SPECS (if any).  */
    FOR_EACH_VEC_ELT (configure_time_options, i, opt)
      {
	gcc_assert (opt);
	gcc_assert (opt[0] == '-');
	ADD_ARG (opt);
      }
  }

  if (get_timer ())
    ADD_ARG ("-ftime-report");

  /* Add any user-provided extra options, starting with any from
     parent contexts.  */
  m_recording_ctxt->append_command_line_options (argvec);

#undef ADD_ARG
#undef ADD_ARG_TAKE_OWNERSHIP
}

/* The second half of the implementation of gcc_jit_context_enable_dump.
   Iterate through the requested dumps, reading the underlying files
   into heap-allocated buffers, writing pointers to the buffers into
   the char ** pointers provided by client code.
   Client code is responsible for calling free on the results.  */

void
playback::context::
extract_any_requested_dumps (vec <recording::requested_dump> *requested_dumps)
{
  JIT_LOG_SCOPE (get_logger ());

  int i;
  recording::requested_dump *d;
  FOR_EACH_VEC_ELT (*requested_dumps, i, d)
    {
      dump_file_info *dfi;
      char *filename;
      char *content;

      dfi = g->get_dumps ()->get_dump_file_info_by_switch (d->m_dumpname);
      if (!dfi)
	{
	  add_error (NULL, "unrecognized dump: %s", d->m_dumpname);
	  continue;
	}

      filename = g->get_dumps ()->get_dump_file_name (dfi);
      content = read_dump_file (filename);
      *(d->m_out_ptr) = content;
      m_tempdir->add_temp_file (filename);
    }
}

/* Helper function for playback::context::extract_any_requested_dumps
   (itself for use in implementation of gcc_jit_context_enable_dump).

   Attempt to read the complete file at the given path, returning the
   bytes found there as a buffer.
   The caller is responsible for calling free on the result.
   Errors will be reported on the context, and lead to NULL being
   returned; an out-of-memory error will terminate the process.  */

char *
playback::context::read_dump_file (const char *path)
{
  char *result = NULL;
  size_t total_sz = 0;
  char buf[4096];
  size_t sz;
  FILE *f_in;

  f_in = fopen (path, "r");
  if (!f_in)
    {
      add_error (NULL, "unable to open %s for reading", path);
      return NULL;
    }

  while ( (sz = fread (buf, 1, sizeof (buf), f_in)) )
    {
      size_t old_total_sz = total_sz;
      total_sz += sz;
      result = reinterpret_cast <char *> (xrealloc (result, total_sz + 1));
      memcpy (result + old_total_sz, buf, sz);
    }

  if (!feof (f_in))
    {
      add_error (NULL, "error reading from %s", path);
      free (result);
      fclose (f_in);
      return NULL;
    }

  fclose (f_in);

  if (result)
    {
      result[total_sz] = '\0';
      return result;
    }
  else
    return xstrdup ("");
}

/* Part of playback::context::compile ().

   We have a .s file; we want a .so file.
   We could reuse parts of gcc/gcc.c to do this.
   For now, just use the driver binary from the install, as
   named in gcc-driver-name.h
   e.g. "x86_64-unknown-linux-gnu-gcc-5.0.0".  */

void
playback::context::
convert_to_dso (const char *ctxt_progname)
{
  JIT_LOG_SCOPE (get_logger ());

  invoke_driver (ctxt_progname,
		 m_tempdir->get_path_s_file (),
		 m_tempdir->get_path_so_file (),
		 TV_ASSEMBLE,
		 true, /* bool shared, */
		 true);/* bool run_linker */
}

static const char * const gcc_driver_name = GCC_DRIVER_NAME;

void
playback::context::
invoke_driver (const char *ctxt_progname,
	       const char *input_file,
	       const char *output_file,
	       timevar_id_t tv_id,
	       bool shared,
	       bool run_linker)
{
  JIT_LOG_SCOPE (get_logger ());

  bool embedded_driver
    = !get_inner_bool_option (INNER_BOOL_OPTION_USE_EXTERNAL_DRIVER);

  /* Currently this lumps together both assembling and linking into
     TV_ASSEMBLE.  */
  auto_timevar assemble_timevar (get_timer (), tv_id);
  auto_argvec argvec;
#define ADD_ARG(arg) argvec.safe_push (xstrdup (arg))

  ADD_ARG (gcc_driver_name);

  add_multilib_driver_arguments (&argvec);

  if (shared)
    ADD_ARG ("-shared");

  if (!run_linker)
    ADD_ARG ("-c");

  ADD_ARG (input_file);
  ADD_ARG ("-o");
  ADD_ARG (output_file);

  /* Don't use the linker plugin.
     If running with just a "make" and not a "make install", then we'd
     run into
       "fatal error: -fuse-linker-plugin, but liblto_plugin.so not found"
     libto_plugin is a .la at build time, with it becoming installed with
     ".so" suffix: i.e. it doesn't exist with a .so suffix until install
     time.  */
  ADD_ARG ("-fno-use-linker-plugin");

#if defined (DARWIN_X86) || defined (DARWIN_PPC)
  /* OS X's linker defaults to treating undefined symbols as errors.
     If the context has any imported functions or globals they will be
     undefined until the .so is dynamically-linked into the process.
     Ensure that the driver passes in "-undefined dynamic_lookup" to the
     linker.  */
  ADD_ARG ("-Wl,-undefined,dynamic_lookup");
#endif

  if (0)
    ADD_ARG ("-v");

#undef ADD_ARG

  /* pex_one's error-handling requires pname to be non-NULL.  */
  gcc_assert (ctxt_progname);

  if (get_logger ())
    for (unsigned i = 0; i < argvec.length (); i++)
      get_logger ()->log ("argv[%i]: %s", i, argvec[i]);

  if (embedded_driver)
    invoke_embedded_driver (&argvec);
  else
    invoke_external_driver (ctxt_progname, &argvec);
}

void
playback::context::
invoke_embedded_driver (const vec <char *> *argvec)
{
  JIT_LOG_SCOPE (get_logger ());
  driver d (true, /* can_finalize */
	    false); /* debug */
  int result = d.main (argvec->length (),
		       const_cast <char **> (argvec->address ()));
  d.finalize ();
  if (result)
    add_error (NULL, "error invoking gcc driver");
}

void
playback::context::
invoke_external_driver (const char *ctxt_progname,
			vec <char *> *argvec)
{
  JIT_LOG_SCOPE (get_logger ());
  const char *errmsg;
  int exit_status = 0;
  int err = 0;

  /* pex argv arrays are NULL-terminated.  */
  argvec->safe_push (NULL);

  errmsg = pex_one (PEX_SEARCH, /* int flags, */
		    gcc_driver_name,
		    const_cast <char *const *> (argvec->address ()),
		    ctxt_progname, /* const char *pname */
		    NULL, /* const char *outname */
		    NULL, /* const char *errname */
		    &exit_status, /* int *status */
		    &err); /* int *err*/
  if (errmsg)
    {
      add_error (NULL, "error invoking gcc driver: %s", errmsg);
      return;
    }

  /* pex_one can return a NULL errmsg when the executable wasn't
     found (or doesn't exist), so trap these cases also.  */
  if (exit_status || err)
    {
      add_error (NULL,
		 "error invoking gcc driver: exit_status: %i err: %i",
		 exit_status, err);
      add_error (NULL,
		 "whilst attempting to run a driver named: %s",
		 gcc_driver_name);
      add_error (NULL,
		 "PATH was: %s",
		 getenv ("PATH"));
      return;
    }
}

/* Extract the target-specific MULTILIB_DEFAULTS to
   multilib_defaults_raw for use by
   playback::context::add_multilib_driver_arguments ().  */

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "" }
#endif

static const char *const multilib_defaults_raw[] = MULTILIB_DEFAULTS;

/* Helper function for playback::context::invoke_driver ().

   32-bit and 64-bit multilib peer builds of libgccjit.so may share
   a driver binary.  We need to pass in options to the shared driver
   to get the appropriate assembler/linker options for this multilib
   peer.  */

void
playback::context::
add_multilib_driver_arguments (vec <char *> *argvec)
{
  JIT_LOG_SCOPE (get_logger ());

  /* Add copies of the arguments in multilib_defaults_raw to argvec,
     prepending each with a "-".  */
  for (size_t i = 0; i < ARRAY_SIZE (multilib_defaults_raw); i++)
    if (multilib_defaults_raw[i][0])
      argvec->safe_push (concat ("-", multilib_defaults_raw[i], NULL));
}

/* Dynamically-link the built DSO file into this process, using dlopen.
   Wrap it up within a jit::result *, and return that.
   Return NULL if any errors occur, reporting them on this context.  */

result *
playback::context::
dlopen_built_dso ()
{
  JIT_LOG_SCOPE (get_logger ());
  auto_timevar load_timevar (get_timer (), TV_LOAD);
  void *handle = NULL;
  const char *error = NULL;
  result *result_obj = NULL;

  /* Clear any existing error.  */
  dlerror ();

  handle = dlopen (m_tempdir->get_path_so_file (),
		   RTLD_NOW | RTLD_LOCAL);
  if ((error = dlerror()) != NULL)  {
    add_error (NULL, "%s", error);
  }
  if (handle)
    {
      /* We've successfully dlopened the result; create a
	 jit::result object to wrap it.

	 We're done with the tempdir for now, but if the user
	 has requested debugging, the user's debugger might not
	 be capable of dealing with the .so file being unlinked
	 immediately, so keep it around until after the result
	 is released.  We do this by handing over ownership of
	 the jit::tempdir to the result.  See PR jit/64206.  */
      tempdir *handover_tempdir;
      if (get_bool_option (GCC_JIT_BOOL_OPTION_DEBUGINFO))
	{
	  handover_tempdir = m_tempdir;
	  m_tempdir = NULL;
	  /* The tempdir will eventually be cleaned up in the
	     jit::result's dtor. */
	  log ("GCC_JIT_BOOL_OPTION_DEBUGINFO was set:"
	       " handing over tempdir to jit::result");
	}
      else
	{
	  handover_tempdir = NULL;
	  /* ... and retain ownership of m_tempdir so we clean it
	     up it the playback::context's dtor. */
	  log ("GCC_JIT_BOOL_OPTION_DEBUGINFO was not set:"
	       " retaining ownership of tempdir");
	}

      result_obj = new result (get_logger (), handle, handover_tempdir);
    }
  else
    result_obj = NULL;

  return result_obj;
}

/* Top-level hook for playing back a recording context.

   This plays back m_recording_ctxt, and, if no errors
   occurred builds statement lists for and then postprocesses
   every function in the result.  */

void
playback::context::
replay ()
{
  JIT_LOG_SCOPE (get_logger ());
  /* Adapted from c-common.c:c_common_nodes_and_builtins.  */
  tree array_domain_type = build_index_type (size_int (200));
  m_char_array_type_node
    = build_array_type (char_type_node, array_domain_type);

  m_const_char_ptr
    = build_pointer_type (build_qualified_type (char_type_node,
						TYPE_QUAL_CONST));

  /* Replay the recorded events:  */
  timevar_push (TV_JIT_REPLAY);

  m_recording_ctxt->replay_into (this);

  /* Clean away the temporary references from recording objects
     to playback objects.  We have to do this now since the
     latter are GC-allocated, but the former don't mark these
     refs.  Hence we must stop using them before the GC can run.  */
  m_recording_ctxt->disassociate_from_playback ();

  /* The builtins_manager, if any, is associated with the recording::context
     and might be reused for future compiles on other playback::contexts,
     but its m_attributes array is not GTY-labeled and hence will become
     nonsense if the GC runs.  Purge this state.  */
  builtins_manager *bm = get_builtins_manager ();
  if (bm)
    bm->finish_playback ();

  timevar_pop (TV_JIT_REPLAY);

  if (!errors_occurred ())
    {
      int i;
      function *func;

      /* No GC can happen yet; process the cached source locations.  */
      handle_locations ();

      /* We've now created tree nodes for the stmts in the various blocks
	 in each function, but we haven't built each function's single stmt
	 list yet.  Do so now.  */
      FOR_EACH_VEC_ELT (m_functions, i, func)
	func->build_stmt_list ();

      /* No GC can have happened yet.  */

      /* Postprocess the functions.  This could trigger GC.  */
      FOR_EACH_VEC_ELT (m_functions, i, func)
	{
	  gcc_assert (func);
	  func->postprocess ();
	}
    }
}

/* Dump the generated .s file to stderr.  */

void
playback::context::
dump_generated_code ()
{
  JIT_LOG_SCOPE (get_logger ());
  char buf[4096];
  size_t sz;
  FILE *f_in = fopen (get_path_s_file (), "r");
  if (!f_in)
    return;

  while ( (sz = fread (buf, 1, sizeof (buf), f_in)) )
    fwrite (buf, 1, sz, stderr);

  fclose (f_in);
}

/* Get the supposed path of the notional "fake.c" file within the
   tempdir.  This file doesn't exist, but the rest of the compiler
   needs a name.  */

const char *
playback::context::
get_path_c_file () const
{
  return m_tempdir->get_path_c_file ();
}

/* Get the path of the assembler output file "fake.s" file within the
   tempdir. */

const char *
playback::context::
get_path_s_file () const
{
  return m_tempdir->get_path_s_file ();
}

/* Get the path of the DSO object file "fake.so" file within the
   tempdir. */

const char *
playback::context::
get_path_so_file () const
{
  return m_tempdir->get_path_so_file ();
}

/* qsort comparator for comparing pairs of playback::source_line *,
   ordering them by line number.  */

static int
line_comparator (const void *lhs, const void *rhs)
{
  const playback::source_line *line_lhs = \
    *static_cast<const playback::source_line * const*> (lhs);
  const playback::source_line *line_rhs = \
    *static_cast<const playback::source_line * const*> (rhs);
  return line_lhs->get_line_num () - line_rhs->get_line_num ();
}

/* qsort comparator for comparing pairs of playback::location *,
   ordering them by column number.  */

static int
location_comparator (const void *lhs, const void *rhs)
{
  const playback::location *loc_lhs = \
    *static_cast<const playback::location * const *> (lhs);
  const playback::location *loc_rhs = \
    *static_cast<const playback::location * const *> (rhs);
  return loc_lhs->get_column_num () - loc_rhs->get_column_num ();
}

/* Our API allows locations to be created in arbitrary orders, but the
   linemap API requires locations to be created in ascending order
   as if we were tokenizing files.

   This hook sorts all of the locations that have been created, and
   calls into the linemap API, creating linemap entries in sorted order
   for our locations.  */

void
playback::context::
handle_locations ()
{
  /* Create the source code locations, following the ordering rules
     imposed by the linemap API.

     line_table is a global.  */
  JIT_LOG_SCOPE (get_logger ());
  int i;
  source_file *file;

  FOR_EACH_VEC_ELT (m_source_files, i, file)
    {
      linemap_add (line_table, LC_ENTER, false, file->get_filename (), 0);

      /* Sort lines by ascending line numbers.  */
      file->m_source_lines.qsort (&line_comparator);

      int j;
      source_line *line;
      FOR_EACH_VEC_ELT (file->m_source_lines, j, line)
	{
	  int k;
	  location *loc;

	  /* Sort locations in line by ascending column numbers.  */
	  line->m_locations.qsort (&location_comparator);

	  /* Determine maximum column within this line.  */
	  gcc_assert (line->m_locations.length () > 0);
	  location *final_column =
	    line->m_locations[line->m_locations.length () - 1];
	  int max_col = final_column->get_column_num ();

	  linemap_line_start (line_table, line->get_line_num (), max_col);
	  FOR_EACH_VEC_ELT (line->m_locations, k, loc)
	    {
	      loc->m_srcloc =					   \
		linemap_position_for_column (line_table, loc->get_column_num ());
	    }
	}

      linemap_add (line_table, LC_LEAVE, false, NULL, 0);
    }

  /* line_table should now be populated; every playback::location should
     now have an m_srcloc.  */

  /* Now assign them to tree nodes as appropriate.  */
  std::pair<tree, location *> *cached_location;

  FOR_EACH_VEC_ELT (m_cached_locations, i, cached_location)
    {
      tree t = cached_location->first;
      source_location srcloc = cached_location->second->m_srcloc;

      /* This covers expressions: */
      if (CAN_HAVE_LOCATION_P (t))
	SET_EXPR_LOCATION (t, srcloc);
      else if (CODE_CONTAINS_STRUCT(TREE_CODE(t), TS_DECL_MINIMAL))
	DECL_SOURCE_LOCATION (t) = srcloc;
      else
	{
	  /* Don't know how to set location on this node.  */
	}
    }
}

/* We handle errors on a playback::context by adding them to the
   corresponding recording::context.  */

void
playback::context::
add_error (location *loc, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  m_recording_ctxt->add_error_va (loc ? loc->get_recording_loc () : NULL,
				  fmt, ap);
  va_end (ap);
}

/* We handle errors on a playback::context by adding them to the
   corresponding recording::context.  */

void
playback::context::
add_error_va (location *loc, const char *fmt, va_list ap)
{
  m_recording_ctxt->add_error_va (loc ? loc->get_recording_loc () : NULL,
				  fmt, ap);
}

/* Report a diagnostic up to the jit context as an error,
   so that the compilation is treated as a failure.
   For now, any kind of diagnostic is treated as an error by the jit
   API.  */

void
playback::context::
add_diagnostic (struct diagnostic_context *diag_context,
		struct diagnostic_info *diagnostic)
{
  /* At this point the text has been formatted into the pretty-printer's
     output buffer.  */
  pretty_printer *pp = diag_context->printer;
  const char *text = pp_formatted_text (pp);

  /* Get location information (if any) from the diagnostic.
     The recording::context::add_error[_va] methods require a
     recording::location.  We can't lookup the playback::location
     from the file/line/column since any playback location instances
     may have been garbage-collected away by now, so instead we create
     another recording::location directly.  */
  location_t gcc_loc = diagnostic_location (diagnostic);
  recording::location *rec_loc = NULL;
  if (gcc_loc)
    {
      expanded_location exploc = expand_location (gcc_loc);
      if (exploc.file)
	rec_loc = m_recording_ctxt->new_location (exploc.file,
						  exploc.line,
						  exploc.column,
						  false);
    }

  m_recording_ctxt->add_error (rec_loc, "%s", text);
  pp_clear_output_area (pp);
}

/* Dealing with the linemap API.  */

/* Construct a playback::location for a recording::location, if it
   doesn't exist already.  */

playback::location *
playback::context::
new_location (recording::location *rloc,
	      const char *filename,
	      int line,
	      int column)
{
  /* Get the source_file for filename, creating if necessary.  */
  source_file *src_file = get_source_file (filename);
  /* Likewise for the line within the file.  */
  source_line *src_line = src_file->get_source_line (line);
  /* Likewise for the column within the line.  */
  location *loc = src_line->get_location (rloc, column);
  return loc;
}

/* Deferred setting of the location for a given tree, by adding the
   (tree, playback::location) pair to a list of deferred associations.
   We will actually set the location on the tree later on once
   the source_location for the playback::location exists.  */

void
playback::context::
set_tree_location (tree t, location *loc)
{
  gcc_assert (loc);
  m_cached_locations.safe_push (std::make_pair (t, loc));
}


/* Construct a playback::source_file for the given source
   filename, if it doesn't exist already.  */

playback::source_file *
playback::context::
get_source_file (const char *filename)
{
  /* Locate the file.
     For simplicitly, this is currently a linear search.
     Replace with a hash if this shows up in the profile.  */
  int i;
  source_file *file;
  tree ident_filename = get_identifier (filename);

  FOR_EACH_VEC_ELT (m_source_files, i, file)
    if (file->filename_as_tree () == ident_filename)
      return file;

  /* Not found.  */
  file = new source_file (ident_filename);
  m_source_files.safe_push (file);
  return file;
}

/* Constructor for gcc::jit::playback::source_file.  */

playback::source_file::source_file (tree filename) :
  m_source_lines (),
  m_filename (filename)
{
}

/* Don't leak vec's internal buffer (in non-GC heap) when we are
   GC-ed.  */

void
playback::source_file::finalizer ()
{
  m_source_lines.release ();
}

/* Construct a playback::source_line for the given line
   within this source file, if one doesn't exist already.  */

playback::source_line *
playback::source_file::
get_source_line (int line_num)
{
  /* Locate the line.
     For simplicitly, this is currently a linear search.
     Replace with a hash if this shows up in the profile.  */
  int i;
  source_line *line;

  FOR_EACH_VEC_ELT (m_source_lines, i, line)
    if (line->get_line_num () == line_num)
      return line;

  /* Not found.  */
  line = new source_line (this, line_num);
  m_source_lines.safe_push (line);
  return line;
}

/* Constructor for gcc::jit::playback::source_line.  */

playback::source_line::source_line (source_file *file, int line_num) :
  m_locations (),
  m_source_file (file),
  m_line_num (line_num)
{
}

/* Don't leak vec's internal buffer (in non-GC heap) when we are
   GC-ed.  */

void
playback::source_line::finalizer ()
{
  m_locations.release ();
}

/* Construct a playback::location for the given column
   within this line of a specific source file, if one doesn't exist
   already.  */

playback::location *
playback::source_line::
get_location (recording::location *rloc, int column_num)
{
  int i;
  location *loc;

  /* Another linear search that probably should be a hash table.  */
  FOR_EACH_VEC_ELT (m_locations, i, loc)
    if (loc->get_column_num () == column_num)
      return loc;

  /* Not found.  */
  loc = new location (rloc, this, column_num);
  m_locations.safe_push (loc);
  return loc;
}

/* Constructor for gcc::jit::playback::location.  */

playback::location::location (recording::location *loc,
			      source_line *line,
			      int column_num) :
  m_srcloc (UNKNOWN_LOCATION),
  m_recording_loc (loc),
  m_line (line),
  m_column_num(column_num)
{
}

/* The active gcc::jit::playback::context instance.  This is a singleton,
   guarded by jit_mutex.  */

playback::context *active_playback_ctxt;

} // namespace gcc::jit

} // namespace gcc
