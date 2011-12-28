// expressions.cc -- Go frontend expression handling.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "toplev.h"
#include "intl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "convert.h"
#include "real.h"
#include "realmpfr.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "go-c.h"
#include "gogo.h"
#include "types.h"
#include "export.h"
#include "import.h"
#include "statements.h"
#include "lex.h"
#include "runtime.h"
#include "backend.h"
#include "expressions.h"
#include "ast-dump.h"

// Class Expression.

Expression::Expression(Expression_classification classification,
		       Location location)
  : classification_(classification), location_(location)
{
}

Expression::~Expression()
{
}

// If this expression has a constant integer value, return it.

bool
Expression::integer_constant_value(bool iota_is_constant, mpz_t val,
				   Type** ptype) const
{
  *ptype = NULL;
  return this->do_integer_constant_value(iota_is_constant, val, ptype);
}

// If this expression has a constant floating point value, return it.

bool
Expression::float_constant_value(mpfr_t val, Type** ptype) const
{
  *ptype = NULL;
  if (this->do_float_constant_value(val, ptype))
    return true;
  mpz_t ival;
  mpz_init(ival);
  Type* t;
  bool ret;
  if (!this->do_integer_constant_value(false, ival, &t))
    ret = false;
  else
    {
      mpfr_set_z(val, ival, GMP_RNDN);
      ret = true;
    }
  mpz_clear(ival);
  return ret;
}

// If this expression has a constant complex value, return it.

bool
Expression::complex_constant_value(mpfr_t real, mpfr_t imag,
				   Type** ptype) const
{
  *ptype = NULL;
  if (this->do_complex_constant_value(real, imag, ptype))
    return true;
  Type *t;
  if (this->float_constant_value(real, &t))
    {
      mpfr_set_ui(imag, 0, GMP_RNDN);
      return true;
    }
  return false;
}

// Traverse the expressions.

int
Expression::traverse(Expression** pexpr, Traverse* traverse)
{
  Expression* expr = *pexpr;
  if ((traverse->traverse_mask() & Traverse::traverse_expressions) != 0)
    {
      int t = traverse->expression(pexpr);
      if (t == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
      else if (t == TRAVERSE_SKIP_COMPONENTS)
	return TRAVERSE_CONTINUE;
    }
  return expr->do_traverse(traverse);
}

// Traverse subexpressions of this expression.

int
Expression::traverse_subexpressions(Traverse* traverse)
{
  return this->do_traverse(traverse);
}

// Default implementation for do_traverse for child classes.

int
Expression::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// This virtual function is called by the parser if the value of this
// expression is being discarded.  By default, we give an error.
// Expressions with side effects override.

void
Expression::do_discarding_value()
{
  this->unused_value_error();
}

// This virtual function is called to export expressions.  This will
// only be used by expressions which may be constant.

void
Expression::do_export(Export*) const
{
  go_unreachable();
}

// Give an error saying that the value of the expression is not used.

void
Expression::unused_value_error()
{
  error_at(this->location(), "value computed is not used");
}

// Note that this expression is an error.  This is called by children
// when they discover an error.

void
Expression::set_is_error()
{
  this->classification_ = EXPRESSION_ERROR;
}

// For children to call to report an error conveniently.

void
Expression::report_error(const char* msg)
{
  error_at(this->location_, "%s", msg);
  this->set_is_error();
}

// Set types of variables and constants.  This is implemented by the
// child class.

void
Expression::determine_type(const Type_context* context)
{
  this->do_determine_type(context);
}

// Set types when there is no context.

void
Expression::determine_type_no_context()
{
  Type_context context;
  this->do_determine_type(&context);
}

// Return a tree handling any conversions which must be done during
// assignment.

tree
Expression::convert_for_assignment(Translate_context* context, Type* lhs_type,
				   Type* rhs_type, tree rhs_tree,
				   Location location)
{
  if (lhs_type == rhs_type)
    return rhs_tree;

  if (lhs_type->is_error() || rhs_type->is_error())
    return error_mark_node;

  if (rhs_tree == error_mark_node || TREE_TYPE(rhs_tree) == error_mark_node)
    return error_mark_node;

  Gogo* gogo = context->gogo();

  tree lhs_type_tree = type_to_tree(lhs_type->get_backend(gogo));
  if (lhs_type_tree == error_mark_node)
    return error_mark_node;

  if (lhs_type->interface_type() != NULL)
    {
      if (rhs_type->interface_type() == NULL)
	return Expression::convert_type_to_interface(context, lhs_type,
						     rhs_type, rhs_tree,
						     location);
      else
	return Expression::convert_interface_to_interface(context, lhs_type,
							  rhs_type, rhs_tree,
							  false, location);
    }
  else if (rhs_type->interface_type() != NULL)
    return Expression::convert_interface_to_type(context, lhs_type, rhs_type,
						 rhs_tree, location);
  else if (lhs_type->is_slice_type() && rhs_type->is_nil_type())
    {
      // Assigning nil to an open array.
      go_assert(TREE_CODE(lhs_type_tree) == RECORD_TYPE);

      VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 3);

      constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
      tree field = TYPE_FIELDS(lhs_type_tree);
      go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),
			"__values") == 0);
      elt->index = field;
      elt->value = fold_convert(TREE_TYPE(field), null_pointer_node);

      elt = VEC_quick_push(constructor_elt, init, NULL);
      field = DECL_CHAIN(field);
      go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),
			"__count") == 0);
      elt->index = field;
      elt->value = fold_convert(TREE_TYPE(field), integer_zero_node);

      elt = VEC_quick_push(constructor_elt, init, NULL);
      field = DECL_CHAIN(field);
      go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),
			"__capacity") == 0);
      elt->index = field;
      elt->value = fold_convert(TREE_TYPE(field), integer_zero_node);

      tree val = build_constructor(lhs_type_tree, init);
      TREE_CONSTANT(val) = 1;

      return val;
    }
  else if (rhs_type->is_nil_type())
    {
      // The left hand side should be a pointer type at the tree
      // level.
      go_assert(POINTER_TYPE_P(lhs_type_tree));
      return fold_convert(lhs_type_tree, null_pointer_node);
    }
  else if (lhs_type_tree == TREE_TYPE(rhs_tree))
    {
      // No conversion is needed.
      return rhs_tree;
    }
  else if (POINTER_TYPE_P(lhs_type_tree)
	   || INTEGRAL_TYPE_P(lhs_type_tree)
	   || SCALAR_FLOAT_TYPE_P(lhs_type_tree)
	   || COMPLEX_FLOAT_TYPE_P(lhs_type_tree))
    return fold_convert_loc(location.gcc_location(), lhs_type_tree, rhs_tree);
  else if (TREE_CODE(lhs_type_tree) == RECORD_TYPE
	   && TREE_CODE(TREE_TYPE(rhs_tree)) == RECORD_TYPE)
    {
      // This conversion must be permitted by Go, or we wouldn't have
      // gotten here.
      go_assert(int_size_in_bytes(lhs_type_tree)
		 == int_size_in_bytes(TREE_TYPE(rhs_tree)));
      return fold_build1_loc(location.gcc_location(), VIEW_CONVERT_EXPR,
                             lhs_type_tree, rhs_tree);
    }
  else
    {
      go_assert(useless_type_conversion_p(lhs_type_tree, TREE_TYPE(rhs_tree)));
      return rhs_tree;
    }
}

// Return a tree for a conversion from a non-interface type to an
// interface type.

tree
Expression::convert_type_to_interface(Translate_context* context,
				      Type* lhs_type, Type* rhs_type,
				      tree rhs_tree, Location location)
{
  Gogo* gogo = context->gogo();
  Interface_type* lhs_interface_type = lhs_type->interface_type();
  bool lhs_is_empty = lhs_interface_type->is_empty();

  // Since RHS_TYPE is a static type, we can create the interface
  // method table at compile time.

  // When setting an interface to nil, we just set both fields to
  // NULL.
  if (rhs_type->is_nil_type())
    {
      Btype* lhs_btype = lhs_type->get_backend(gogo);
      return expr_to_tree(gogo->backend()->zero_expression(lhs_btype));
    }

  // This should have been checked already.
  go_assert(lhs_interface_type->implements_interface(rhs_type, NULL));

  tree lhs_type_tree = type_to_tree(lhs_type->get_backend(gogo));
  if (lhs_type_tree == error_mark_node)
    return error_mark_node;

  // An interface is a tuple.  If LHS_TYPE is an empty interface type,
  // then the first field is the type descriptor for RHS_TYPE.
  // Otherwise it is the interface method table for RHS_TYPE.
  tree first_field_value;
  if (lhs_is_empty)
    first_field_value = rhs_type->type_descriptor_pointer(gogo, location);
  else
    {
      // Build the interface method table for this interface and this
      // object type: a list of function pointers for each interface
      // method.
      Named_type* rhs_named_type = rhs_type->named_type();
      bool is_pointer = false;
      if (rhs_named_type == NULL)
	{
	  rhs_named_type = rhs_type->deref()->named_type();
	  is_pointer = true;
	}
      tree method_table;
      if (rhs_named_type == NULL)
	method_table = null_pointer_node;
      else
	method_table =
	  rhs_named_type->interface_method_table(gogo, lhs_interface_type,
						 is_pointer);
      first_field_value = fold_convert_loc(location.gcc_location(),
                                           const_ptr_type_node, method_table);
    }
  if (first_field_value == error_mark_node)
    return error_mark_node;

  // Start building a constructor for the value we will return.

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 2);

  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  tree field = TYPE_FIELDS(lhs_type_tree);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),
		    (lhs_is_empty ? "__type_descriptor" : "__methods")) == 0);
  elt->index = field;
  elt->value = fold_convert_loc(location.gcc_location(), TREE_TYPE(field),
                                first_field_value);

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__object") == 0);
  elt->index = field;

  if (rhs_type->points_to() != NULL)
    {
      //  We are assigning a pointer to the interface; the interface
      // holds the pointer itself.
      elt->value = rhs_tree;
      return build_constructor(lhs_type_tree, init);
    }

  // We are assigning a non-pointer value to the interface; the
  // interface gets a copy of the value in the heap.

  tree object_size = TYPE_SIZE_UNIT(TREE_TYPE(rhs_tree));

  tree space = gogo->allocate_memory(rhs_type, object_size, location);
  space = fold_convert_loc(location.gcc_location(),
                           build_pointer_type(TREE_TYPE(rhs_tree)), space);
  space = save_expr(space);

  tree ref = build_fold_indirect_ref_loc(location.gcc_location(), space);
  TREE_THIS_NOTRAP(ref) = 1;
  tree set = fold_build2_loc(location.gcc_location(), MODIFY_EXPR,
                             void_type_node, ref, rhs_tree);

  elt->value = fold_convert_loc(location.gcc_location(), TREE_TYPE(field),
                                space);

  return build2(COMPOUND_EXPR, lhs_type_tree, set,
		build_constructor(lhs_type_tree, init));
}

// Return a tree for the type descriptor of RHS_TREE, which has
// interface type RHS_TYPE.  If RHS_TREE is nil the result will be
// NULL.

tree
Expression::get_interface_type_descriptor(Translate_context*,
					  Type* rhs_type, tree rhs_tree,
					  Location location)
{
  tree rhs_type_tree = TREE_TYPE(rhs_tree);
  go_assert(TREE_CODE(rhs_type_tree) == RECORD_TYPE);
  tree rhs_field = TYPE_FIELDS(rhs_type_tree);
  tree v = build3(COMPONENT_REF, TREE_TYPE(rhs_field), rhs_tree, rhs_field,
		  NULL_TREE);
  if (rhs_type->interface_type()->is_empty())
    {
      go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(rhs_field)),
			"__type_descriptor") == 0);
      return v;
    }

  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(rhs_field)), "__methods")
	     == 0);
  go_assert(POINTER_TYPE_P(TREE_TYPE(v)));
  v = save_expr(v);
  tree v1 = build_fold_indirect_ref_loc(location.gcc_location(), v);
  go_assert(TREE_CODE(TREE_TYPE(v1)) == RECORD_TYPE);
  tree f = TYPE_FIELDS(TREE_TYPE(v1));
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(f)), "__type_descriptor")
	     == 0);
  v1 = build3(COMPONENT_REF, TREE_TYPE(f), v1, f, NULL_TREE);

  tree eq = fold_build2_loc(location.gcc_location(), EQ_EXPR, boolean_type_node,
                            v, fold_convert_loc(location.gcc_location(),
                                                TREE_TYPE(v),
                                                null_pointer_node));
  tree n = fold_convert_loc(location.gcc_location(), TREE_TYPE(v1),
                            null_pointer_node);
  return fold_build3_loc(location.gcc_location(), COND_EXPR, TREE_TYPE(v1),
			 eq, n, v1);
}

// Return a tree for the conversion of an interface type to an
// interface type.

tree
Expression::convert_interface_to_interface(Translate_context* context,
					   Type *lhs_type, Type *rhs_type,
					   tree rhs_tree, bool for_type_guard,
					   Location location)
{
  Gogo* gogo = context->gogo();
  Interface_type* lhs_interface_type = lhs_type->interface_type();
  bool lhs_is_empty = lhs_interface_type->is_empty();

  tree lhs_type_tree = type_to_tree(lhs_type->get_backend(gogo));
  if (lhs_type_tree == error_mark_node)
    return error_mark_node;

  // In the general case this requires runtime examination of the type
  // method table to match it up with the interface methods.

  // FIXME: If all of the methods in the right hand side interface
  // also appear in the left hand side interface, then we don't need
  // to do a runtime check, although we still need to build a new
  // method table.

  // Get the type descriptor for the right hand side.  This will be
  // NULL for a nil interface.

  if (!DECL_P(rhs_tree))
    rhs_tree = save_expr(rhs_tree);

  tree rhs_type_descriptor =
    Expression::get_interface_type_descriptor(context, rhs_type, rhs_tree,
					      location);

  // The result is going to be a two element constructor.

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 2);

  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  tree field = TYPE_FIELDS(lhs_type_tree);
  elt->index = field;

  if (for_type_guard)
    {
      // A type assertion fails when converting a nil interface.
      tree lhs_type_descriptor = lhs_type->type_descriptor_pointer(gogo,
								   location);
      static tree assert_interface_decl;
      tree call = Gogo::call_builtin(&assert_interface_decl,
				     location,
				     "__go_assert_interface",
				     2,
				     ptr_type_node,
				     TREE_TYPE(lhs_type_descriptor),
				     lhs_type_descriptor,
				     TREE_TYPE(rhs_type_descriptor),
				     rhs_type_descriptor);
      if (call == error_mark_node)
	return error_mark_node;
      // This will panic if the interface conversion fails.
      TREE_NOTHROW(assert_interface_decl) = 0;
      elt->value = fold_convert_loc(location.gcc_location(), TREE_TYPE(field),
                                    call);
    }
  else if (lhs_is_empty)
    {
      // A convertion to an empty interface always succeeds, and the
      // first field is just the type descriptor of the object.
      go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),
			"__type_descriptor") == 0);
      go_assert(TREE_TYPE(field) == TREE_TYPE(rhs_type_descriptor));
      elt->value = rhs_type_descriptor;
    }
  else
    {
      // A conversion to a non-empty interface may fail, but unlike a
      // type assertion converting nil will always succeed.
      go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__methods")
		 == 0);
      tree lhs_type_descriptor = lhs_type->type_descriptor_pointer(gogo,
								   location);
      static tree convert_interface_decl;
      tree call = Gogo::call_builtin(&convert_interface_decl,
				     location,
				     "__go_convert_interface",
				     2,
				     ptr_type_node,
				     TREE_TYPE(lhs_type_descriptor),
				     lhs_type_descriptor,
				     TREE_TYPE(rhs_type_descriptor),
				     rhs_type_descriptor);
      if (call == error_mark_node)
	return error_mark_node;
      // This will panic if the interface conversion fails.
      TREE_NOTHROW(convert_interface_decl) = 0;
      elt->value = fold_convert_loc(location.gcc_location(), TREE_TYPE(field),
                                    call);
    }

  // The second field is simply the object pointer.

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__object") == 0);
  elt->index = field;

  tree rhs_type_tree = TREE_TYPE(rhs_tree);
  go_assert(TREE_CODE(rhs_type_tree) == RECORD_TYPE);
  tree rhs_field = DECL_CHAIN(TYPE_FIELDS(rhs_type_tree));
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(rhs_field)), "__object") == 0);
  elt->value = build3(COMPONENT_REF, TREE_TYPE(rhs_field), rhs_tree, rhs_field,
		      NULL_TREE);

  return build_constructor(lhs_type_tree, init);
}

// Return a tree for the conversion of an interface type to a
// non-interface type.

tree
Expression::convert_interface_to_type(Translate_context* context,
				      Type *lhs_type, Type* rhs_type,
				      tree rhs_tree, Location location)
{
  Gogo* gogo = context->gogo();
  tree rhs_type_tree = TREE_TYPE(rhs_tree);

  tree lhs_type_tree = type_to_tree(lhs_type->get_backend(gogo));
  if (lhs_type_tree == error_mark_node)
    return error_mark_node;

  // Call a function to check that the type is valid.  The function
  // will panic with an appropriate runtime type error if the type is
  // not valid.

  tree lhs_type_descriptor = lhs_type->type_descriptor_pointer(gogo, location);

  if (!DECL_P(rhs_tree))
    rhs_tree = save_expr(rhs_tree);

  tree rhs_type_descriptor =
    Expression::get_interface_type_descriptor(context, rhs_type, rhs_tree,
					      location);

  tree rhs_inter_descriptor = rhs_type->type_descriptor_pointer(gogo,
								location);

  static tree check_interface_type_decl;
  tree call = Gogo::call_builtin(&check_interface_type_decl,
				 location,
				 "__go_check_interface_type",
				 3,
				 void_type_node,
				 TREE_TYPE(lhs_type_descriptor),
				 lhs_type_descriptor,
				 TREE_TYPE(rhs_type_descriptor),
				 rhs_type_descriptor,
				 TREE_TYPE(rhs_inter_descriptor),
				 rhs_inter_descriptor);
  if (call == error_mark_node)
    return error_mark_node;
  // This call will panic if the conversion is invalid.
  TREE_NOTHROW(check_interface_type_decl) = 0;

  // If the call succeeds, pull out the value.
  go_assert(TREE_CODE(rhs_type_tree) == RECORD_TYPE);
  tree rhs_field = DECL_CHAIN(TYPE_FIELDS(rhs_type_tree));
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(rhs_field)), "__object") == 0);
  tree val = build3(COMPONENT_REF, TREE_TYPE(rhs_field), rhs_tree, rhs_field,
		    NULL_TREE);

  // If the value is a pointer, then it is the value we want.
  // Otherwise it points to the value.
  if (lhs_type->points_to() == NULL)
    {
      val = fold_convert_loc(location.gcc_location(),
                             build_pointer_type(lhs_type_tree), val);
      val = build_fold_indirect_ref_loc(location.gcc_location(), val);
    }

  return build2(COMPOUND_EXPR, lhs_type_tree, call,
		fold_convert_loc(location.gcc_location(), lhs_type_tree, val));
}

// Convert an expression to a tree.  This is implemented by the child
// class.  Not that it is not in general safe to call this multiple
// times for a single expression, but that we don't catch such errors.

tree
Expression::get_tree(Translate_context* context)
{
  // The child may have marked this expression as having an error.
  if (this->classification_ == EXPRESSION_ERROR)
    return error_mark_node;

  return this->do_get_tree(context);
}

// Return a tree for VAL in TYPE.

tree
Expression::integer_constant_tree(mpz_t val, tree type)
{
  if (type == error_mark_node)
    return error_mark_node;
  else if (TREE_CODE(type) == INTEGER_TYPE)
    return double_int_to_tree(type,
			      mpz_get_double_int(type, val, true));
  else if (TREE_CODE(type) == REAL_TYPE)
    {
      mpfr_t fval;
      mpfr_init_set_z(fval, val, GMP_RNDN);
      tree ret = Expression::float_constant_tree(fval, type);
      mpfr_clear(fval);
      return ret;
    }
  else if (TREE_CODE(type) == COMPLEX_TYPE)
    {
      mpfr_t fval;
      mpfr_init_set_z(fval, val, GMP_RNDN);
      tree real = Expression::float_constant_tree(fval, TREE_TYPE(type));
      mpfr_clear(fval);
      tree imag = build_real_from_int_cst(TREE_TYPE(type),
					  integer_zero_node);
      return build_complex(type, real, imag);
    }
  else
    go_unreachable();
}

// Return a tree for VAL in TYPE.

tree
Expression::float_constant_tree(mpfr_t val, tree type)
{
  if (type == error_mark_node)
    return error_mark_node;
  else if (TREE_CODE(type) == INTEGER_TYPE)
    {
      mpz_t ival;
      mpz_init(ival);
      mpfr_get_z(ival, val, GMP_RNDN);
      tree ret = Expression::integer_constant_tree(ival, type);
      mpz_clear(ival);
      return ret;
    }
  else if (TREE_CODE(type) == REAL_TYPE)
    {
      REAL_VALUE_TYPE r1;
      real_from_mpfr(&r1, val, type, GMP_RNDN);
      REAL_VALUE_TYPE r2;
      real_convert(&r2, TYPE_MODE(type), &r1);
      return build_real(type, r2);
    }
  else if (TREE_CODE(type) == COMPLEX_TYPE)
    {
      REAL_VALUE_TYPE r1;
      real_from_mpfr(&r1, val, TREE_TYPE(type), GMP_RNDN);
      REAL_VALUE_TYPE r2;
      real_convert(&r2, TYPE_MODE(TREE_TYPE(type)), &r1);
      tree imag = build_real_from_int_cst(TREE_TYPE(type),
					  integer_zero_node);
      return build_complex(type, build_real(TREE_TYPE(type), r2), imag);
    }
  else
    go_unreachable();
}

// Return a tree for REAL/IMAG in TYPE.

tree
Expression::complex_constant_tree(mpfr_t real, mpfr_t imag, tree type)
{
  if (type == error_mark_node)
    return error_mark_node;
  else if (TREE_CODE(type) == INTEGER_TYPE || TREE_CODE(type) == REAL_TYPE)
    return Expression::float_constant_tree(real, type);
  else if (TREE_CODE(type) == COMPLEX_TYPE)
    {
      REAL_VALUE_TYPE r1;
      real_from_mpfr(&r1, real, TREE_TYPE(type), GMP_RNDN);
      REAL_VALUE_TYPE r2;
      real_convert(&r2, TYPE_MODE(TREE_TYPE(type)), &r1);

      REAL_VALUE_TYPE r3;
      real_from_mpfr(&r3, imag, TREE_TYPE(type), GMP_RNDN);
      REAL_VALUE_TYPE r4;
      real_convert(&r4, TYPE_MODE(TREE_TYPE(type)), &r3);

      return build_complex(type, build_real(TREE_TYPE(type), r2),
			   build_real(TREE_TYPE(type), r4));
    }
  else
    go_unreachable();
}

// Return a tree which evaluates to true if VAL, of arbitrary integer
// type, is negative or is more than the maximum value of BOUND_TYPE.
// If SOFAR is not NULL, it is or'red into the result.  The return
// value may be NULL if SOFAR is NULL.

tree
Expression::check_bounds(tree val, tree bound_type, tree sofar,
			 Location loc)
{
  tree val_type = TREE_TYPE(val);
  tree ret = NULL_TREE;

  if (!TYPE_UNSIGNED(val_type))
    {
      ret = fold_build2_loc(loc.gcc_location(), LT_EXPR, boolean_type_node, val,
			    build_int_cst(val_type, 0));
      if (ret == boolean_false_node)
	ret = NULL_TREE;
    }

  HOST_WIDE_INT val_type_size = int_size_in_bytes(val_type);
  HOST_WIDE_INT bound_type_size = int_size_in_bytes(bound_type);
  go_assert(val_type_size != -1 && bound_type_size != -1);
  if (val_type_size > bound_type_size
      || (val_type_size == bound_type_size
	  && TYPE_UNSIGNED(val_type)
	  && !TYPE_UNSIGNED(bound_type)))
    {
      tree max = TYPE_MAX_VALUE(bound_type);
      tree big = fold_build2_loc(loc.gcc_location(), GT_EXPR, boolean_type_node,
                                 val, fold_convert_loc(loc.gcc_location(),
                                                       val_type, max));
      if (big == boolean_false_node)
	;
      else if (ret == NULL_TREE)
	ret = big;
      else
	ret = fold_build2_loc(loc.gcc_location(), TRUTH_OR_EXPR,
                              boolean_type_node, ret, big);
    }

  if (ret == NULL_TREE)
    return sofar;
  else if (sofar == NULL_TREE)
    return ret;
  else
    return fold_build2_loc(loc.gcc_location(), TRUTH_OR_EXPR, boolean_type_node,
			   sofar, ret);
}

void
Expression::dump_expression(Ast_dump_context* ast_dump_context) const
{
  this->do_dump_expression(ast_dump_context);
}

// Error expressions.  This are used to avoid cascading errors.

class Error_expression : public Expression
{
 public:
  Error_expression(Location location)
    : Expression(EXPRESSION_ERROR, location)
  { }

 protected:
  bool
  do_is_constant() const
  { return true; }

  bool
  do_integer_constant_value(bool, mpz_t val, Type**) const
  {
    mpz_set_ui(val, 0);
    return true;
  }

  bool
  do_float_constant_value(mpfr_t val, Type**) const
  {
    mpfr_set_ui(val, 0, GMP_RNDN);
    return true;
  }

  bool
  do_complex_constant_value(mpfr_t real, mpfr_t imag, Type**) const
  {
    mpfr_set_ui(real, 0, GMP_RNDN);
    mpfr_set_ui(imag, 0, GMP_RNDN);
    return true;
  }

  void
  do_discarding_value()
  { }

  Type*
  do_type()
  { return Type::make_error_type(); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  bool
  do_is_addressable() const
  { return true; }

  tree
  do_get_tree(Translate_context*)
  { return error_mark_node; }

  void
  do_dump_expression(Ast_dump_context*) const;
};

// Dump the ast representation for an error expression to a dump context.

void
Error_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "_Error_" ;
}

Expression*
Expression::make_error(Location location)
{
  return new Error_expression(location);
}

// An expression which is really a type.  This is used during parsing.
// It is an error if these survive after lowering.

class
Type_expression : public Expression
{
 public:
  Type_expression(Type* type, Location location)
    : Expression(EXPRESSION_TYPE, location),
      type_(type)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Type::traverse(this->type_, traverse); }

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*)
  { }

  void
  do_check_types(Gogo*)
  { this->report_error(_("invalid use of type")); }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context*)
  { go_unreachable(); }

  void do_dump_expression(Ast_dump_context*) const;
 
 private:
  // The type which we are representing as an expression.
  Type* type_;
};

void
Type_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_type(this->type_);
}

Expression*
Expression::make_type(Type* type, Location location)
{
  return new Type_expression(type, location);
}

// Class Parser_expression.

Type*
Parser_expression::do_type()
{
  // We should never really ask for the type of a Parser_expression.
  // However, it can happen, at least when we have an invalid const
  // whose initializer refers to the const itself.  In that case we
  // may ask for the type when lowering the const itself.
  go_assert(saw_errors());
  return Type::make_error_type();
}

// Class Var_expression.

// Lower a variable expression.  Here we just make sure that the
// initialization expression of the variable has been lowered.  This
// ensures that we will be able to determine the type of the variable
// if necessary.

Expression*
Var_expression::do_lower(Gogo* gogo, Named_object* function,
			 Statement_inserter* inserter, int)
{
  if (this->variable_->is_variable())
    {
      Variable* var = this->variable_->var_value();
      // This is either a local variable or a global variable.  A
      // reference to a variable which is local to an enclosing
      // function will be a reference to a field in a closure.
      if (var->is_global())
	{
	  function = NULL;
	  inserter = NULL;
	}
      var->lower_init_expression(gogo, function, inserter);
    }
  return this;
}

// Return the type of a reference to a variable.

Type*
Var_expression::do_type()
{
  if (this->variable_->is_variable())
    return this->variable_->var_value()->type();
  else if (this->variable_->is_result_variable())
    return this->variable_->result_var_value()->type();
  else
    go_unreachable();
}

// Determine the type of a reference to a variable.

void
Var_expression::do_determine_type(const Type_context*)
{
  if (this->variable_->is_variable())
    this->variable_->var_value()->determine_type();
}

// Something takes the address of this variable.  This means that we
// may want to move the variable onto the heap.

void
Var_expression::do_address_taken(bool escapes)
{
  if (!escapes)
    {
      if (this->variable_->is_variable())
	this->variable_->var_value()->set_non_escaping_address_taken();
      else if (this->variable_->is_result_variable())
	this->variable_->result_var_value()->set_non_escaping_address_taken();
      else
	go_unreachable();
    }
  else
    {
      if (this->variable_->is_variable())
	this->variable_->var_value()->set_address_taken();
      else if (this->variable_->is_result_variable())
	this->variable_->result_var_value()->set_address_taken();
      else
	go_unreachable();
    }
}

// Get the tree for a reference to a variable.

tree
Var_expression::do_get_tree(Translate_context* context)
{
  Bvariable* bvar = this->variable_->get_backend_variable(context->gogo(),
							  context->function());
  tree ret = var_to_tree(bvar);
  if (ret == error_mark_node)
    return error_mark_node;
  bool is_in_heap;
  if (this->variable_->is_variable())
    is_in_heap = this->variable_->var_value()->is_in_heap();
  else if (this->variable_->is_result_variable())
    is_in_heap = this->variable_->result_var_value()->is_in_heap();
  else
    go_unreachable();
  if (is_in_heap)
    {
      ret = build_fold_indirect_ref_loc(this->location().gcc_location(), ret);
      TREE_THIS_NOTRAP(ret) = 1;
    }
  return ret;
}

// Ast dump for variable expression.

void
Var_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << this->variable_->name() ;
}

// Make a reference to a variable in an expression.

Expression*
Expression::make_var_reference(Named_object* var, Location location)
{
  if (var->is_sink())
    return Expression::make_sink(location);

  // FIXME: Creating a new object for each reference to a variable is
  // wasteful.
  return new Var_expression(var, location);
}

// Class Temporary_reference_expression.

// The type.

Type*
Temporary_reference_expression::do_type()
{
  return this->statement_->type();
}

// Called if something takes the address of this temporary variable.
// We never have to move temporary variables to the heap, but we do
// need to know that they must live in the stack rather than in a
// register.

void
Temporary_reference_expression::do_address_taken(bool)
{
  this->statement_->set_is_address_taken();
}

// Get a tree referring to the variable.

tree
Temporary_reference_expression::do_get_tree(Translate_context* context)
{
  Bvariable* bvar = this->statement_->get_backend_variable(context);

  // The gcc backend can't represent the same set of recursive types
  // that the Go frontend can.  In some cases this means that a
  // temporary variable won't have the right backend type.  Correct
  // that here by adding a type cast.  We need to use base() to push
  // the circularity down one level.
  tree ret = var_to_tree(bvar);
  if (!this->is_lvalue_
      && POINTER_TYPE_P(TREE_TYPE(ret))
      && VOID_TYPE_P(TREE_TYPE(TREE_TYPE(ret))))
    {
      Btype* type_btype = this->type()->base()->get_backend(context->gogo());
      tree type_tree = type_to_tree(type_btype);
      ret = fold_convert_loc(this->location().gcc_location(), type_tree, ret);
    }
  return ret;
}

// Ast dump for temporary reference.

void
Temporary_reference_expression::do_dump_expression(
                                Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_temp_variable_name(this->statement_);
}

// Make a reference to a temporary variable.

Temporary_reference_expression*
Expression::make_temporary_reference(Temporary_statement* statement,
				     Location location)
{
  return new Temporary_reference_expression(statement, location);
}

// A sink expression--a use of the blank identifier _.

class Sink_expression : public Expression
{
 public:
  Sink_expression(Location location)
    : Expression(EXPRESSION_SINK, location),
      type_(NULL), var_(NULL_TREE)
  { }

 protected:
  void
  do_discarding_value()
  { }

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  Expression*
  do_copy()
  { return new Sink_expression(this->location()); }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of this sink variable.
  Type* type_;
  // The temporary variable we generate.
  tree var_;
};

// Return the type of a sink expression.

Type*
Sink_expression::do_type()
{
  if (this->type_ == NULL)
    return Type::make_sink_type();
  return this->type_;
}

// Determine the type of a sink expression.

void
Sink_expression::do_determine_type(const Type_context* context)
{
  if (context->type != NULL)
    this->type_ = context->type;
}

// Return a temporary variable for a sink expression.  This will
// presumably be a write-only variable which the middle-end will drop.

tree
Sink_expression::do_get_tree(Translate_context* context)
{
  if (this->var_ == NULL_TREE)
    {
      go_assert(this->type_ != NULL && !this->type_->is_sink_type());
      Btype* bt = this->type_->get_backend(context->gogo());
      this->var_ = create_tmp_var(type_to_tree(bt), "blank");
    }
  return this->var_;
}

// Ast dump for sink expression.

void
Sink_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "_" ;
}

// Make a sink expression.

Expression*
Expression::make_sink(Location location)
{
  return new Sink_expression(location);
}

// Class Func_expression.

// FIXME: Can a function expression appear in a constant expression?
// The value is unchanging.  Initializing a constant to the address of
// a function seems like it could work, though there might be little
// point to it.

// Traversal.

int
Func_expression::do_traverse(Traverse* traverse)
{
  return (this->closure_ == NULL
	  ? TRAVERSE_CONTINUE
	  : Expression::traverse(&this->closure_, traverse));
}

// Return the type of a function expression.

Type*
Func_expression::do_type()
{
  if (this->function_->is_function())
    return this->function_->func_value()->type();
  else if (this->function_->is_function_declaration())
    return this->function_->func_declaration_value()->type();
  else
    go_unreachable();
}

// Get the tree for a function expression without evaluating the
// closure.

tree
Func_expression::get_tree_without_closure(Gogo* gogo)
{
  Function_type* fntype;
  if (this->function_->is_function())
    fntype = this->function_->func_value()->type();
  else if (this->function_->is_function_declaration())
    fntype = this->function_->func_declaration_value()->type();
  else
    go_unreachable();

  // Builtin functions are handled specially by Call_expression.  We
  // can't take their address.
  if (fntype->is_builtin())
    {
      error_at(this->location(), "invalid use of special builtin function %qs",
	       this->function_->name().c_str());
      return error_mark_node;
    }

  Named_object* no = this->function_;

  tree id = no->get_id(gogo);
  if (id == error_mark_node)
    return error_mark_node;

  tree fndecl;
  if (no->is_function())
    fndecl = no->func_value()->get_or_make_decl(gogo, no, id);
  else if (no->is_function_declaration())
    fndecl = no->func_declaration_value()->get_or_make_decl(gogo, no, id);
  else
    go_unreachable();

  if (fndecl == error_mark_node)
    return error_mark_node;

  return build_fold_addr_expr_loc(this->location().gcc_location(), fndecl);
}

// Get the tree for a function expression.  This is used when we take
// the address of a function rather than simply calling it.  If the
// function has a closure, we must use a trampoline.

tree
Func_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  tree fnaddr = this->get_tree_without_closure(gogo);
  if (fnaddr == error_mark_node)
    return error_mark_node;

  go_assert(TREE_CODE(fnaddr) == ADDR_EXPR
	     && TREE_CODE(TREE_OPERAND(fnaddr, 0)) == FUNCTION_DECL);
  TREE_ADDRESSABLE(TREE_OPERAND(fnaddr, 0)) = 1;

  // For a normal non-nested function call, that is all we have to do.
  if (!this->function_->is_function()
      || this->function_->func_value()->enclosing() == NULL)
    {
      go_assert(this->closure_ == NULL);
      return fnaddr;
    }

  // For a nested function call, we have to always allocate a
  // trampoline.  If we don't always allocate, then closures will not
  // be reliably distinct.
  Expression* closure = this->closure_;
  tree closure_tree;
  if (closure == NULL)
    closure_tree = null_pointer_node;
  else
    {
      // Get the value of the closure.  This will be a pointer to
      // space allocated on the heap.
      closure_tree = closure->get_tree(context);
      if (closure_tree == error_mark_node)
	return error_mark_node;
      go_assert(POINTER_TYPE_P(TREE_TYPE(closure_tree)));
    }

  // Now we need to build some code on the heap.  This code will load
  // the static chain pointer with the closure and then jump to the
  // body of the function.  The normal gcc approach is to build the
  // code on the stack.  Unfortunately we can not do that, as Go
  // permits us to return the function pointer.

  return gogo->make_trampoline(fnaddr, closure_tree, this->location());
}

// Ast dump for function.

void
Func_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << this->function_->name();
  if (this->closure_ != NULL)
    {
      ast_dump_context->ostream() << " {closure =  ";
      this->closure_->dump_expression(ast_dump_context);
      ast_dump_context->ostream() << "}";
    }
}

// Make a reference to a function in an expression.

Expression*
Expression::make_func_reference(Named_object* function, Expression* closure,
				Location location)
{
  return new Func_expression(function, closure, location);
}

// Class Unknown_expression.

// Return the name of an unknown expression.

const std::string&
Unknown_expression::name() const
{
  return this->named_object_->name();
}

// Lower a reference to an unknown name.

Expression*
Unknown_expression::do_lower(Gogo*, Named_object*, Statement_inserter*, int)
{
  Location location = this->location();
  Named_object* no = this->named_object_;
  Named_object* real;
  if (!no->is_unknown())
    real = no;
  else
    {
      real = no->unknown_value()->real_named_object();
      if (real == NULL)
	{
	  if (this->is_composite_literal_key_)
	    return this;
	  error_at(location, "reference to undefined name %qs",
		   this->named_object_->message_name().c_str());
	  return Expression::make_error(location);
	}
    }
  switch (real->classification())
    {
    case Named_object::NAMED_OBJECT_CONST:
      return Expression::make_const_reference(real, location);
    case Named_object::NAMED_OBJECT_TYPE:
      return Expression::make_type(real->type_value(), location);
    case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
      if (this->is_composite_literal_key_)
	return this;
      error_at(location, "reference to undefined type %qs",
	       real->message_name().c_str());
      return Expression::make_error(location);
    case Named_object::NAMED_OBJECT_VAR:
      return Expression::make_var_reference(real, location);
    case Named_object::NAMED_OBJECT_FUNC:
    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
      return Expression::make_func_reference(real, NULL, location);
    case Named_object::NAMED_OBJECT_PACKAGE:
      if (this->is_composite_literal_key_)
	return this;
      error_at(location, "unexpected reference to package");
      return Expression::make_error(location);
    default:
      go_unreachable();
    }
}

// Dump the ast representation for an unknown expression to a dump context.

void
Unknown_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "_Unknown_(" << this->named_object_->name()
			      << ")";
}

// Make a reference to an unknown name.

Expression*
Expression::make_unknown_reference(Named_object* no, Location location)
{
  return new Unknown_expression(no, location);
}

// A boolean expression.

class Boolean_expression : public Expression
{
 public:
  Boolean_expression(bool val, Location location)
    : Expression(EXPRESSION_BOOLEAN, location),
      val_(val), type_(NULL)
  { }

  static Expression*
  do_import(Import*);

 protected:
  bool
  do_is_constant() const
  { return true; }

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context*)
  { return this->val_ ? boolean_true_node : boolean_false_node; }

  void
  do_export(Export* exp) const
  { exp->write_c_string(this->val_ ? "true" : "false"); }

  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const
  { ast_dump_context->ostream() << (this->val_ ? "true" : "false"); }
  
 private:
  // The constant.
  bool val_;
  // The type as determined by context.
  Type* type_;
};

// Get the type.

Type*
Boolean_expression::do_type()
{
  if (this->type_ == NULL)
    this->type_ = Type::make_boolean_type();
  return this->type_;
}

// Set the type from the context.

void
Boolean_expression::do_determine_type(const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL && context->type->is_boolean_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_bool_type();
}

// Import a boolean constant.

Expression*
Boolean_expression::do_import(Import* imp)
{
  if (imp->peek_char() == 't')
    {
      imp->require_c_string("true");
      return Expression::make_boolean(true, imp->location());
    }
  else
    {
      imp->require_c_string("false");
      return Expression::make_boolean(false, imp->location());
    }
}

// Make a boolean expression.

Expression*
Expression::make_boolean(bool val, Location location)
{
  return new Boolean_expression(val, location);
}

// Class String_expression.

// Get the type.

Type*
String_expression::do_type()
{
  if (this->type_ == NULL)
    this->type_ = Type::make_string_type();
  return this->type_;
}

// Set the type from the context.

void
String_expression::do_determine_type(const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL && context->type->is_string_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_string_type();
}

// Build a string constant.

tree
String_expression::do_get_tree(Translate_context* context)
{
  return context->gogo()->go_string_constant_tree(this->val_);
}

 // Write string literal to string dump.

void
String_expression::export_string(String_dump* exp,
				 const String_expression* str)
{
  std::string s;
  s.reserve(str->val_.length() * 4 + 2);
  s += '"';
  for (std::string::const_iterator p = str->val_.begin();
       p != str->val_.end();
       ++p)
    {
      if (*p == '\\' || *p == '"')
	{
	  s += '\\';
	  s += *p;
	}
      else if (*p >= 0x20 && *p < 0x7f)
	s += *p;
      else if (*p == '\n')
	s += "\\n";
      else if (*p == '\t')
	s += "\\t";
      else
	{
	  s += "\\x";
	  unsigned char c = *p;
	  unsigned int dig = c >> 4;
	  s += dig < 10 ? '0' + dig : 'A' + dig - 10;
	  dig = c & 0xf;
	  s += dig < 10 ? '0' + dig : 'A' + dig - 10;
	}
    }
  s += '"';
  exp->write_string(s);
}

// Export a string expression.

void
String_expression::do_export(Export* exp) const
{
  String_expression::export_string(exp, this);
}

// Import a string expression.

Expression*
String_expression::do_import(Import* imp)
{
  imp->require_c_string("\"");
  std::string val;
  while (true)
    {
      int c = imp->get_char();
      if (c == '"' || c == -1)
	break;
      if (c != '\\')
	val += static_cast<char>(c);
      else
	{
	  c = imp->get_char();
	  if (c == '\\' || c == '"')
	    val += static_cast<char>(c);
	  else if (c == 'n')
	    val += '\n';
	  else if (c == 't')
	    val += '\t';
	  else if (c == 'x')
	    {
	      c = imp->get_char();
	      unsigned int vh = c >= '0' && c <= '9' ? c - '0' : c - 'A' + 10;
	      c = imp->get_char();
	      unsigned int vl = c >= '0' && c <= '9' ? c - '0' : c - 'A' + 10;
	      char v = (vh << 4) | vl;
	      val += v;
	    }
	  else
	    {
	      error_at(imp->location(), "bad string constant");
	      return Expression::make_error(imp->location());
	    }
	}
    }
  return Expression::make_string(val, imp->location());
}

// Ast dump for string expression.

void
String_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  String_expression::export_string(ast_dump_context, this);
}

// Make a string expression.

Expression*
Expression::make_string(const std::string& val, Location location)
{
  return new String_expression(val, location);
}

// Make an integer expression.

class Integer_expression : public Expression
{
 public:
  Integer_expression(const mpz_t* val, Type* type, Location location)
    : Expression(EXPRESSION_INTEGER, location),
      type_(type)
  { mpz_init_set(this->val_, *val); }

  static Expression*
  do_import(Import*);

  // Return whether VAL fits in the type.
  static bool
  check_constant(mpz_t val, Type*, Location);

  // Write VAL to string dump.
  static void
  export_integer(String_dump* exp, const mpz_t val);

  // Write VAL to dump context.
  static void
  dump_integer(Ast_dump_context* ast_dump_context, const mpz_t val);

 protected:
  bool
  do_is_constant() const
  { return true; }

  bool
  do_integer_constant_value(bool, mpz_t val, Type** ptype) const;

  Type*
  do_type();

  void
  do_determine_type(const Type_context* context);

  void
  do_check_types(Gogo*);

  tree
  do_get_tree(Translate_context*);

  Expression*
  do_copy()
  { return Expression::make_integer(&this->val_, this->type_,
				    this->location()); }

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The integer value.
  mpz_t val_;
  // The type so far.
  Type* type_;
};

// Return an integer constant value.

bool
Integer_expression::do_integer_constant_value(bool, mpz_t val,
					      Type** ptype) const
{
  if (this->type_ != NULL)
    *ptype = this->type_;
  mpz_set(val, this->val_);
  return true;
}

// Return the current type.  If we haven't set the type yet, we return
// an abstract integer type.

Type*
Integer_expression::do_type()
{
  if (this->type_ == NULL)
    this->type_ = Type::make_abstract_integer_type();
  return this->type_;
}

// Set the type of the integer value.  Here we may switch from an
// abstract type to a real type.

void
Integer_expression::do_determine_type(const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL
	   && (context->type->integer_type() != NULL
	       || context->type->float_type() != NULL
	       || context->type->complex_type() != NULL))
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_integer_type("int");
}

// Return true if the integer VAL fits in the range of the type TYPE.
// Otherwise give an error and return false.  TYPE may be NULL.

bool
Integer_expression::check_constant(mpz_t val, Type* type,
				   Location location)
{
  if (type == NULL)
    return true;
  Integer_type* itype = type->integer_type();
  if (itype == NULL || itype->is_abstract())
    return true;

  int bits = mpz_sizeinbase(val, 2);

  if (itype->is_unsigned())
    {
      // For an unsigned type we can only accept a nonnegative number,
      // and we must be able to represent at least BITS.
      if (mpz_sgn(val) >= 0
	  && bits <= itype->bits())
	return true;
    }
  else
    {
      // For a signed type we need an extra bit to indicate the sign.
      // We have to handle the most negative integer specially.
      if (bits + 1 <= itype->bits()
	  || (bits <= itype->bits()
	      && mpz_sgn(val) < 0
	      && (mpz_scan1(val, 0)
		  == static_cast<unsigned long>(itype->bits() - 1))
	      && mpz_scan0(val, itype->bits()) == ULONG_MAX))
	return true;
    }

  error_at(location, "integer constant overflow");
  return false;
}

// Check the type of an integer constant.

void
Integer_expression::do_check_types(Gogo*)
{
  if (this->type_ == NULL)
    return;
  if (!Integer_expression::check_constant(this->val_, this->type_,
					  this->location()))
    this->set_is_error();
}

// Get a tree for an integer constant.

tree
Integer_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  tree type;
  if (this->type_ != NULL && !this->type_->is_abstract())
    type = type_to_tree(this->type_->get_backend(gogo));
  else if (this->type_ != NULL && this->type_->float_type() != NULL)
    {
      // We are converting to an abstract floating point type.
      Type* ftype = Type::lookup_float_type("float64");
      type = type_to_tree(ftype->get_backend(gogo));
    }
  else if (this->type_ != NULL && this->type_->complex_type() != NULL)
    {
      // We are converting to an abstract complex type.
      Type* ctype = Type::lookup_complex_type("complex128");
      type = type_to_tree(ctype->get_backend(gogo));
    }
  else
    {
      // If we still have an abstract type here, then this is being
      // used in a constant expression which didn't get reduced for
      // some reason.  Use a type which will fit the value.  We use <,
      // not <=, because we need an extra bit for the sign bit.
      int bits = mpz_sizeinbase(this->val_, 2);
      if (bits < INT_TYPE_SIZE)
	{
	  Type* t = Type::lookup_integer_type("int");
	  type = type_to_tree(t->get_backend(gogo));
	}
      else if (bits < 64)
	{
	  Type* t = Type::lookup_integer_type("int64");
	  type = type_to_tree(t->get_backend(gogo));
	}
      else
	type = long_long_integer_type_node;
    }
  return Expression::integer_constant_tree(this->val_, type);
}

// Write VAL to export data.

void
Integer_expression::export_integer(String_dump* exp, const mpz_t val)
{
  char* s = mpz_get_str(NULL, 10, val);
  exp->write_c_string(s);
  free(s);
}

// Export an integer in a constant expression.

void
Integer_expression::do_export(Export* exp) const
{
  Integer_expression::export_integer(exp, this->val_);
  // A trailing space lets us reliably identify the end of the number.
  exp->write_c_string(" ");
}

// Import an integer, floating point, or complex value.  This handles
// all these types because they all start with digits.

Expression*
Integer_expression::do_import(Import* imp)
{
  std::string num = imp->read_identifier();
  imp->require_c_string(" ");
  if (!num.empty() && num[num.length() - 1] == 'i')
    {
      mpfr_t real;
      size_t plus_pos = num.find('+', 1);
      size_t minus_pos = num.find('-', 1);
      size_t pos;
      if (plus_pos == std::string::npos)
	pos = minus_pos;
      else if (minus_pos == std::string::npos)
	pos = plus_pos;
      else
	{
	  error_at(imp->location(), "bad number in import data: %qs",
		   num.c_str());
	  return Expression::make_error(imp->location());
	}
      if (pos == std::string::npos)
	mpfr_set_ui(real, 0, GMP_RNDN);
      else
	{
	  std::string real_str = num.substr(0, pos);
	  if (mpfr_init_set_str(real, real_str.c_str(), 10, GMP_RNDN) != 0)
	    {
	      error_at(imp->location(), "bad number in import data: %qs",
		       real_str.c_str());
	      return Expression::make_error(imp->location());
	    }
	}

      std::string imag_str;
      if (pos == std::string::npos)
	imag_str = num;
      else
	imag_str = num.substr(pos);
      imag_str = imag_str.substr(0, imag_str.size() - 1);
      mpfr_t imag;
      if (mpfr_init_set_str(imag, imag_str.c_str(), 10, GMP_RNDN) != 0)
	{
	  error_at(imp->location(), "bad number in import data: %qs",
		   imag_str.c_str());
	  return Expression::make_error(imp->location());
	}
      Expression* ret = Expression::make_complex(&real, &imag, NULL,
						 imp->location());
      mpfr_clear(real);
      mpfr_clear(imag);
      return ret;
    }
  else if (num.find('.') == std::string::npos
	   && num.find('E') == std::string::npos)
    {
      mpz_t val;
      if (mpz_init_set_str(val, num.c_str(), 10) != 0)
	{
	  error_at(imp->location(), "bad number in import data: %qs",
		   num.c_str());
	  return Expression::make_error(imp->location());
	}
      Expression* ret = Expression::make_integer(&val, NULL, imp->location());
      mpz_clear(val);
      return ret;
    }
  else
    {
      mpfr_t val;
      if (mpfr_init_set_str(val, num.c_str(), 10, GMP_RNDN) != 0)
	{
	  error_at(imp->location(), "bad number in import data: %qs",
		   num.c_str());
	  return Expression::make_error(imp->location());
	}
      Expression* ret = Expression::make_float(&val, NULL, imp->location());
      mpfr_clear(val);
      return ret;
    }
}
// Ast dump for integer expression.

void
Integer_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  Integer_expression::export_integer(ast_dump_context, this->val_);
}

// Build a new integer value.

Expression*
Expression::make_integer(const mpz_t* val, Type* type,
			 Location location)
{
  return new Integer_expression(val, type, location);
}

// Floats.

class Float_expression : public Expression
{
 public:
  Float_expression(const mpfr_t* val, Type* type, Location location)
    : Expression(EXPRESSION_FLOAT, location),
      type_(type)
  {
    mpfr_init_set(this->val_, *val, GMP_RNDN);
  }

  // Constrain VAL to fit into TYPE.
  static void
  constrain_float(mpfr_t val, Type* type);

  // Return whether VAL fits in the type.
  static bool
  check_constant(mpfr_t val, Type*, Location);

  // Write VAL to export data.
  static void
  export_float(String_dump* exp, const mpfr_t val);

  // Write VAL to dump file.
  static void
  dump_float(Ast_dump_context* ast_dump_context, const mpfr_t val);

 protected:
  bool
  do_is_constant() const
  { return true; }

  bool
  do_float_constant_value(mpfr_t val, Type**) const;

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  { return Expression::make_float(&this->val_, this->type_,
				  this->location()); }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The floating point value.
  mpfr_t val_;
  // The type so far.
  Type* type_;
};

// Constrain VAL to fit into TYPE.

void
Float_expression::constrain_float(mpfr_t val, Type* type)
{
  Float_type* ftype = type->float_type();
  if (ftype != NULL && !ftype->is_abstract())
    mpfr_prec_round(val, ftype->bits(), GMP_RNDN);
}

// Return a floating point constant value.

bool
Float_expression::do_float_constant_value(mpfr_t val, Type** ptype) const
{
  if (this->type_ != NULL)
    *ptype = this->type_;
  mpfr_set(val, this->val_, GMP_RNDN);
  return true;
}

// Return the current type.  If we haven't set the type yet, we return
// an abstract float type.

Type*
Float_expression::do_type()
{
  if (this->type_ == NULL)
    this->type_ = Type::make_abstract_float_type();
  return this->type_;
}

// Set the type of the float value.  Here we may switch from an
// abstract type to a real type.

void
Float_expression::do_determine_type(const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL
	   && (context->type->integer_type() != NULL
	       || context->type->float_type() != NULL
	       || context->type->complex_type() != NULL))
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_float_type("float64");
}

// Return true if the floating point value VAL fits in the range of
// the type TYPE.  Otherwise give an error and return false.  TYPE may
// be NULL.

bool
Float_expression::check_constant(mpfr_t val, Type* type,
				 Location location)
{
  if (type == NULL)
    return true;
  Float_type* ftype = type->float_type();
  if (ftype == NULL || ftype->is_abstract())
    return true;

  // A NaN or Infinity always fits in the range of the type.
  if (mpfr_nan_p(val) || mpfr_inf_p(val) || mpfr_zero_p(val))
    return true;

  mp_exp_t exp = mpfr_get_exp(val);
  mp_exp_t max_exp;
  switch (ftype->bits())
    {
    case 32:
      max_exp = 128;
      break;
    case 64:
      max_exp = 1024;
      break;
    default:
      go_unreachable();
    }
  if (exp > max_exp)
    {
      error_at(location, "floating point constant overflow");
      return false;
    }
  return true;
}

// Check the type of a float value.

void
Float_expression::do_check_types(Gogo*)
{
  if (this->type_ == NULL)
    return;

  if (!Float_expression::check_constant(this->val_, this->type_,
					this->location()))
    this->set_is_error();

  Integer_type* integer_type = this->type_->integer_type();
  if (integer_type != NULL)
    {
      if (!mpfr_integer_p(this->val_))
	this->report_error(_("floating point constant truncated to integer"));
      else
	{
	  go_assert(!integer_type->is_abstract());
	  mpz_t ival;
	  mpz_init(ival);
	  mpfr_get_z(ival, this->val_, GMP_RNDN);
	  Integer_expression::check_constant(ival, integer_type,
					     this->location());
	  mpz_clear(ival);
	}
    }
}

// Get a tree for a float constant.

tree
Float_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  tree type;
  if (this->type_ != NULL && !this->type_->is_abstract())
    type = type_to_tree(this->type_->get_backend(gogo));
  else if (this->type_ != NULL && this->type_->integer_type() != NULL)
    {
      // We have an abstract integer type.  We just hope for the best.
      type = type_to_tree(Type::lookup_integer_type("int")->get_backend(gogo));
    }
  else
    {
      // If we still have an abstract type here, then this is being
      // used in a constant expression which didn't get reduced.  We
      // just use float64 and hope for the best.
      Type* ft = Type::lookup_float_type("float64");
      type = type_to_tree(ft->get_backend(gogo));
    }
  return Expression::float_constant_tree(this->val_, type);
}

// Write a floating point number to a string dump.

void
Float_expression::export_float(String_dump *exp, const mpfr_t val)
{
  mp_exp_t exponent;
  char* s = mpfr_get_str(NULL, &exponent, 10, 0, val, GMP_RNDN);
  if (*s == '-')
    exp->write_c_string("-");
  exp->write_c_string("0.");
  exp->write_c_string(*s == '-' ? s + 1 : s);
  mpfr_free_str(s);
  char buf[30];
  snprintf(buf, sizeof buf, "E%ld", exponent);
  exp->write_c_string(buf);
}

// Export a floating point number in a constant expression.

void
Float_expression::do_export(Export* exp) const
{
  Float_expression::export_float(exp, this->val_);
  // A trailing space lets us reliably identify the end of the number.
  exp->write_c_string(" ");
}

// Dump a floating point number to the dump file.

void
Float_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  Float_expression::export_float(ast_dump_context, this->val_);
}

// Make a float expression.

Expression*
Expression::make_float(const mpfr_t* val, Type* type, Location location)
{
  return new Float_expression(val, type, location);
}

// Complex numbers.

class Complex_expression : public Expression
{
 public:
  Complex_expression(const mpfr_t* real, const mpfr_t* imag, Type* type,
		     Location location)
    : Expression(EXPRESSION_COMPLEX, location),
      type_(type)
  {
    mpfr_init_set(this->real_, *real, GMP_RNDN);
    mpfr_init_set(this->imag_, *imag, GMP_RNDN);
  }

  // Constrain REAL/IMAG to fit into TYPE.
  static void
  constrain_complex(mpfr_t real, mpfr_t imag, Type* type);

  // Return whether REAL/IMAG fits in the type.
  static bool
  check_constant(mpfr_t real, mpfr_t imag, Type*, Location);

  // Write REAL/IMAG to string dump.
  static void
  export_complex(String_dump* exp, const mpfr_t real, const mpfr_t val);

  // Write REAL/IMAG to dump context.
  static void
  dump_complex(Ast_dump_context* ast_dump_context, 
	       const mpfr_t real, const mpfr_t val);
  
 protected:
  bool
  do_is_constant() const
  { return true; }

  bool
  do_complex_constant_value(mpfr_t real, mpfr_t imag, Type**) const;

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_complex(&this->real_, &this->imag_, this->type_,
				    this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  // The real part.
  mpfr_t real_;
  // The imaginary part;
  mpfr_t imag_;
  // The type if known.
  Type* type_;
};

// Constrain REAL/IMAG to fit into TYPE.

void
Complex_expression::constrain_complex(mpfr_t real, mpfr_t imag, Type* type)
{
  Complex_type* ctype = type->complex_type();
  if (ctype != NULL && !ctype->is_abstract())
    {
      mpfr_prec_round(real, ctype->bits() / 2, GMP_RNDN);
      mpfr_prec_round(imag, ctype->bits() / 2, GMP_RNDN);
    }
}

// Return a complex constant value.

bool
Complex_expression::do_complex_constant_value(mpfr_t real, mpfr_t imag,
					      Type** ptype) const
{
  if (this->type_ != NULL)
    *ptype = this->type_;
  mpfr_set(real, this->real_, GMP_RNDN);
  mpfr_set(imag, this->imag_, GMP_RNDN);
  return true;
}

// Return the current type.  If we haven't set the type yet, we return
// an abstract complex type.

Type*
Complex_expression::do_type()
{
  if (this->type_ == NULL)
    this->type_ = Type::make_abstract_complex_type();
  return this->type_;
}

// Set the type of the complex value.  Here we may switch from an
// abstract type to a real type.

void
Complex_expression::do_determine_type(const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL
	   && context->type->complex_type() != NULL)
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    this->type_ = Type::lookup_complex_type("complex128");
}

// Return true if the complex value REAL/IMAG fits in the range of the
// type TYPE.  Otherwise give an error and return false.  TYPE may be
// NULL.

bool
Complex_expression::check_constant(mpfr_t real, mpfr_t imag, Type* type,
				   Location location)
{
  if (type == NULL)
    return true;
  Complex_type* ctype = type->complex_type();
  if (ctype == NULL || ctype->is_abstract())
    return true;

  mp_exp_t max_exp;
  switch (ctype->bits())
    {
    case 64:
      max_exp = 128;
      break;
    case 128:
      max_exp = 1024;
      break;
    default:
      go_unreachable();
    }

  // A NaN or Infinity always fits in the range of the type.
  if (!mpfr_nan_p(real) && !mpfr_inf_p(real) && !mpfr_zero_p(real))
    {
      if (mpfr_get_exp(real) > max_exp)
	{
	  error_at(location, "complex real part constant overflow");
	  return false;
	}
    }

  if (!mpfr_nan_p(imag) && !mpfr_inf_p(imag) && !mpfr_zero_p(imag))
    {
      if (mpfr_get_exp(imag) > max_exp)
	{
	  error_at(location, "complex imaginary part constant overflow");
	  return false;
	}
    }

  return true;
}

// Check the type of a complex value.

void
Complex_expression::do_check_types(Gogo*)
{
  if (this->type_ == NULL)
    return;

  if (!Complex_expression::check_constant(this->real_, this->imag_,
					  this->type_, this->location()))
    this->set_is_error();
}

// Get a tree for a complex constant.

tree
Complex_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  tree type;
  if (this->type_ != NULL && !this->type_->is_abstract())
    type = type_to_tree(this->type_->get_backend(gogo));
  else
    {
      // If we still have an abstract type here, this this is being
      // used in a constant expression which didn't get reduced.  We
      // just use complex128 and hope for the best.
      Type* ct = Type::lookup_complex_type("complex128");
      type = type_to_tree(ct->get_backend(gogo));
    }
  return Expression::complex_constant_tree(this->real_, this->imag_, type);
}

// Write REAL/IMAG to export data.

void
Complex_expression::export_complex(String_dump* exp, const mpfr_t real,
				   const mpfr_t imag)
{
  if (!mpfr_zero_p(real))
    {
      Float_expression::export_float(exp, real);
      if (mpfr_sgn(imag) > 0)
	exp->write_c_string("+");
    }
  Float_expression::export_float(exp, imag);
  exp->write_c_string("i");
}

// Export a complex number in a constant expression.

void
Complex_expression::do_export(Export* exp) const
{
  Complex_expression::export_complex(exp, this->real_, this->imag_);
  // A trailing space lets us reliably identify the end of the number.
  exp->write_c_string(" ");
}

// Dump a complex expression to the dump file.

void
Complex_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  Complex_expression::export_complex(ast_dump_context,
                                      this->real_,
                                      this->imag_);
}

// Make a complex expression.

Expression*
Expression::make_complex(const mpfr_t* real, const mpfr_t* imag, Type* type,
			 Location location)
{
  return new Complex_expression(real, imag, type, location);
}

// Find a named object in an expression.

class Find_named_object : public Traverse
{
 public:
  Find_named_object(Named_object* no)
    : Traverse(traverse_expressions),
      no_(no), found_(false)
  { }

  // Whether we found the object.
  bool
  found() const
  { return this->found_; }

 protected:
  int
  expression(Expression**);

 private:
  // The object we are looking for.
  Named_object* no_;
  // Whether we found it.
  bool found_;
};

// A reference to a const in an expression.

class Const_expression : public Expression
{
 public:
  Const_expression(Named_object* constant, Location location)
    : Expression(EXPRESSION_CONST_REFERENCE, location),
      constant_(constant), type_(NULL), seen_(false)
  { }

  Named_object*
  named_object()
  { return this->constant_; }

  // Check that the initializer does not refer to the constant itself.
  void
  check_for_init_loop();

 protected:
  int
  do_traverse(Traverse*);

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  bool
  do_is_constant() const
  { return true; }

  bool
  do_integer_constant_value(bool, mpz_t val, Type**) const;

  bool
  do_float_constant_value(mpfr_t val, Type**) const;

  bool
  do_complex_constant_value(mpfr_t real, mpfr_t imag, Type**) const;

  bool
  do_string_constant_value(std::string* val) const
  { return this->constant_->const_value()->expr()->string_constant_value(val); }

  Type*
  do_type();

  // The type of a const is set by the declaration, not the use.
  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context* context);

  // When exporting a reference to a const as part of a const
  // expression, we export the value.  We ignore the fact that it has
  // a name.
  void
  do_export(Export* exp) const
  { this->constant_->const_value()->expr()->export_expression(exp); }

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The constant.
  Named_object* constant_;
  // The type of this reference.  This is used if the constant has an
  // abstract type.
  Type* type_;
  // Used to prevent infinite recursion when a constant incorrectly
  // refers to itself.
  mutable bool seen_;
};

// Traversal.

int
Const_expression::do_traverse(Traverse* traverse)
{
  if (this->type_ != NULL)
    return Type::traverse(this->type_, traverse);
  return TRAVERSE_CONTINUE;
}

// Lower a constant expression.  This is where we convert the
// predeclared constant iota into an integer value.

Expression*
Const_expression::do_lower(Gogo* gogo, Named_object*,
			   Statement_inserter*, int iota_value)
{
  if (this->constant_->const_value()->expr()->classification()
      == EXPRESSION_IOTA)
    {
      if (iota_value == -1)
	{
	  error_at(this->location(),
		   "iota is only defined in const declarations");
	  iota_value = 0;
	}
      mpz_t val;
      mpz_init_set_ui(val, static_cast<unsigned long>(iota_value));
      Expression* ret = Expression::make_integer(&val, NULL,
						 this->location());
      mpz_clear(val);
      return ret;
    }

  // Make sure that the constant itself has been lowered.
  gogo->lower_constant(this->constant_);

  return this;
}

// Return an integer constant value.

bool
Const_expression::do_integer_constant_value(bool iota_is_constant, mpz_t val,
					    Type** ptype) const
{
  if (this->seen_)
    return false;

  Type* ctype;
  if (this->type_ != NULL)
    ctype = this->type_;
  else
    ctype = this->constant_->const_value()->type();
  if (ctype != NULL && ctype->integer_type() == NULL)
    return false;

  Expression* e = this->constant_->const_value()->expr();

  this->seen_ = true;

  Type* t;
  bool r = e->integer_constant_value(iota_is_constant, val, &t);

  this->seen_ = false;

  if (r
      && ctype != NULL
      && !Integer_expression::check_constant(val, ctype, this->location()))
    return false;

  *ptype = ctype != NULL ? ctype : t;
  return r;
}

// Return a floating point constant value.

bool
Const_expression::do_float_constant_value(mpfr_t val, Type** ptype) const
{
  if (this->seen_)
    return false;

  Type* ctype;
  if (this->type_ != NULL)
    ctype = this->type_;
  else
    ctype = this->constant_->const_value()->type();
  if (ctype != NULL && ctype->float_type() == NULL)
    return false;

  this->seen_ = true;

  Type* t;
  bool r = this->constant_->const_value()->expr()->float_constant_value(val,
									&t);

  this->seen_ = false;

  if (r && ctype != NULL)
    {
      if (!Float_expression::check_constant(val, ctype, this->location()))
	return false;
      Float_expression::constrain_float(val, ctype);
    }
  *ptype = ctype != NULL ? ctype : t;
  return r;
}

// Return a complex constant value.

bool
Const_expression::do_complex_constant_value(mpfr_t real, mpfr_t imag,
					    Type **ptype) const
{
  if (this->seen_)
    return false;

  Type* ctype;
  if (this->type_ != NULL)
    ctype = this->type_;
  else
    ctype = this->constant_->const_value()->type();
  if (ctype != NULL && ctype->complex_type() == NULL)
    return false;

  this->seen_ = true;

  Type *t;
  bool r = this->constant_->const_value()->expr()->complex_constant_value(real,
									  imag,
									  &t);

  this->seen_ = false;

  if (r && ctype != NULL)
    {
      if (!Complex_expression::check_constant(real, imag, ctype,
					      this->location()))
	return false;
      Complex_expression::constrain_complex(real, imag, ctype);
    }
  *ptype = ctype != NULL ? ctype : t;
  return r;
}

// Return the type of the const reference.

Type*
Const_expression::do_type()
{
  if (this->type_ != NULL)
    return this->type_;

  Named_constant* nc = this->constant_->const_value();

  if (this->seen_ || nc->lowering())
    {
      this->report_error(_("constant refers to itself"));
      this->type_ = Type::make_error_type();
      return this->type_;
    }

  this->seen_ = true;

  Type* ret = nc->type();

  if (ret != NULL)
    {
      this->seen_ = false;
      return ret;
    }

  // During parsing, a named constant may have a NULL type, but we
  // must not return a NULL type here.
  ret = nc->expr()->type();

  this->seen_ = false;

  return ret;
}

// Set the type of the const reference.

void
Const_expression::do_determine_type(const Type_context* context)
{
  Type* ctype = this->constant_->const_value()->type();
  Type* cetype = (ctype != NULL
		  ? ctype
		  : this->constant_->const_value()->expr()->type());
  if (ctype != NULL && !ctype->is_abstract())
    ;
  else if (context->type != NULL
	   && (context->type->integer_type() != NULL
	       || context->type->float_type() != NULL
	       || context->type->complex_type() != NULL)
	   && (cetype->integer_type() != NULL
	       || cetype->float_type() != NULL
	       || cetype->complex_type() != NULL))
    this->type_ = context->type;
  else if (context->type != NULL
	   && context->type->is_string_type()
	   && cetype->is_string_type())
    this->type_ = context->type;
  else if (context->type != NULL
	   && context->type->is_boolean_type()
	   && cetype->is_boolean_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    {
      if (cetype->is_abstract())
	cetype = cetype->make_non_abstract_type();
      this->type_ = cetype;
    }
}

// Check for a loop in which the initializer of a constant refers to
// the constant itself.

void
Const_expression::check_for_init_loop()
{
  if (this->type_ != NULL && this->type_->is_error())
    return;

  if (this->seen_)
    {
      this->report_error(_("constant refers to itself"));
      this->type_ = Type::make_error_type();
      return;
    }

  Expression* init = this->constant_->const_value()->expr();
  Find_named_object find_named_object(this->constant_);

  this->seen_ = true;
  Expression::traverse(&init, &find_named_object);
  this->seen_ = false;

  if (find_named_object.found())
    {
      if (this->type_ == NULL || !this->type_->is_error())
	{
	  this->report_error(_("constant refers to itself"));
	  this->type_ = Type::make_error_type();
	}
      return;
    }
}

// Check types of a const reference.

void
Const_expression::do_check_types(Gogo*)
{
  if (this->type_ != NULL && this->type_->is_error())
    return;

  this->check_for_init_loop();

  if (this->type_ == NULL || this->type_->is_abstract())
    return;

  // Check for integer overflow.
  if (this->type_->integer_type() != NULL)
    {
      mpz_t ival;
      mpz_init(ival);
      Type* dummy;
      if (!this->integer_constant_value(true, ival, &dummy))
	{
	  mpfr_t fval;
	  mpfr_init(fval);
	  Expression* cexpr = this->constant_->const_value()->expr();
	  if (cexpr->float_constant_value(fval, &dummy))
	    {
	      if (!mpfr_integer_p(fval))
		this->report_error(_("floating point constant "
				     "truncated to integer"));
	      else
		{
		  mpfr_get_z(ival, fval, GMP_RNDN);
		  Integer_expression::check_constant(ival, this->type_,
						     this->location());
		}
	    }
	  mpfr_clear(fval);
	}
      mpz_clear(ival);
    }
}

// Return a tree for the const reference.

tree
Const_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  tree type_tree;
  if (this->type_ == NULL)
    type_tree = NULL_TREE;
  else
    {
      type_tree = type_to_tree(this->type_->get_backend(gogo));
      if (type_tree == error_mark_node)
	return error_mark_node;
    }

  // If the type has been set for this expression, but the underlying
  // object is an abstract int or float, we try to get the abstract
  // value.  Otherwise we may lose something in the conversion.
  if (this->type_ != NULL
      && (this->constant_->const_value()->type() == NULL
	  || this->constant_->const_value()->type()->is_abstract()))
    {
      Expression* expr = this->constant_->const_value()->expr();
      mpz_t ival;
      mpz_init(ival);
      Type* t;
      if (expr->integer_constant_value(true, ival, &t))
	{
	  tree ret = Expression::integer_constant_tree(ival, type_tree);
	  mpz_clear(ival);
	  return ret;
	}
      mpz_clear(ival);

      mpfr_t fval;
      mpfr_init(fval);
      if (expr->float_constant_value(fval, &t))
	{
	  tree ret = Expression::float_constant_tree(fval, type_tree);
	  mpfr_clear(fval);
	  return ret;
	}

      mpfr_t imag;
      mpfr_init(imag);
      if (expr->complex_constant_value(fval, imag, &t))
	{
	  tree ret = Expression::complex_constant_tree(fval, imag, type_tree);
	  mpfr_clear(fval);
	  mpfr_clear(imag);
	  return ret;
	}
      mpfr_clear(imag);
      mpfr_clear(fval);
    }

  tree const_tree = this->constant_->get_tree(gogo, context->function());
  if (this->type_ == NULL
      || const_tree == error_mark_node
      || TREE_TYPE(const_tree) == error_mark_node)
    return const_tree;

  tree ret;
  if (TYPE_MAIN_VARIANT(type_tree) == TYPE_MAIN_VARIANT(TREE_TYPE(const_tree)))
    ret = fold_convert(type_tree, const_tree);
  else if (TREE_CODE(type_tree) == INTEGER_TYPE)
    ret = fold(convert_to_integer(type_tree, const_tree));
  else if (TREE_CODE(type_tree) == REAL_TYPE)
    ret = fold(convert_to_real(type_tree, const_tree));
  else if (TREE_CODE(type_tree) == COMPLEX_TYPE)
    ret = fold(convert_to_complex(type_tree, const_tree));
  else
    go_unreachable();
  return ret;
}

// Dump ast representation for constant expression.

void
Const_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << this->constant_->name();
}

// Make a reference to a constant in an expression.

Expression*
Expression::make_const_reference(Named_object* constant,
				 Location location)
{
  return new Const_expression(constant, location);
}

// Find a named object in an expression.

int
Find_named_object::expression(Expression** pexpr)
{
  switch ((*pexpr)->classification())
    {
    case Expression::EXPRESSION_CONST_REFERENCE:
      {
	Const_expression* ce = static_cast<Const_expression*>(*pexpr);
	if (ce->named_object() == this->no_)
	  break;

	// We need to check a constant initializer explicitly, as
	// loops here will not be caught by the loop checking for
	// variable initializers.
	ce->check_for_init_loop();

	return TRAVERSE_CONTINUE;
      }

    case Expression::EXPRESSION_VAR_REFERENCE:
      if ((*pexpr)->var_expression()->named_object() == this->no_)
	break;
      return TRAVERSE_CONTINUE;
    case Expression::EXPRESSION_FUNC_REFERENCE:
      if ((*pexpr)->func_expression()->named_object() == this->no_)
	break;
      return TRAVERSE_CONTINUE;
    default:
      return TRAVERSE_CONTINUE;
    }
  this->found_ = true;
  return TRAVERSE_EXIT;
}

// The nil value.

class Nil_expression : public Expression
{
 public:
  Nil_expression(Location location)
    : Expression(EXPRESSION_NIL, location)
  { }

  static Expression*
  do_import(Import*);

 protected:
  bool
  do_is_constant() const
  { return true; }

  Type*
  do_type()
  { return Type::make_nil_type(); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context*)
  { return null_pointer_node; }

  void
  do_export(Export* exp) const
  { exp->write_c_string("nil"); }

  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const
  { ast_dump_context->ostream() << "nil"; }
};

// Import a nil expression.

Expression*
Nil_expression::do_import(Import* imp)
{
  imp->require_c_string("nil");
  return Expression::make_nil(imp->location());
}

// Make a nil expression.

Expression*
Expression::make_nil(Location location)
{
  return new Nil_expression(location);
}

// The value of the predeclared constant iota.  This is little more
// than a marker.  This will be lowered to an integer in
// Const_expression::do_lower, which is where we know the value that
// it should have.

class Iota_expression : public Parser_expression
{
 public:
  Iota_expression(Location location)
    : Parser_expression(EXPRESSION_IOTA, location)
  { }

 protected:
  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int)
  { go_unreachable(); }

  // There should only ever be one of these.
  Expression*
  do_copy()
  { go_unreachable(); }
  
  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const
  { ast_dump_context->ostream() << "iota"; } 
};

// Make an iota expression.  This is only called for one case: the
// value of the predeclared constant iota.

Expression*
Expression::make_iota()
{
  static Iota_expression iota_expression(Linemap::unknown_location());
  return &iota_expression;
}

// A type conversion expression.

class Type_conversion_expression : public Expression
{
 public:
  Type_conversion_expression(Type* type, Expression* expr,
			     Location location)
    : Expression(EXPRESSION_CONVERSION, location),
      type_(type), expr_(expr), may_convert_function_types_(false)
  { }

  // Return the type to which we are converting.
  Type*
  type() const
  { return this->type_; }

  // Return the expression which we are converting.
  Expression*
  expr() const
  { return this->expr_; }

  // Permit converting from one function type to another.  This is
  // used internally for method expressions.
  void
  set_may_convert_function_types()
  {
    this->may_convert_function_types_ = true;
  }

  // Import a type conversion expression.
  static Expression*
  do_import(Import*);

 protected:
  int
  do_traverse(Traverse* traverse);

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  bool
  do_is_constant() const
  { return this->expr_->is_constant(); }

  bool
  do_integer_constant_value(bool, mpz_t, Type**) const;

  bool
  do_float_constant_value(mpfr_t, Type**) const;

  bool
  do_complex_constant_value(mpfr_t, mpfr_t, Type**) const;

  bool
  do_string_constant_value(std::string*) const;

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*)
  {
    Type_context subcontext(this->type_, false);
    this->expr_->determine_type(&subcontext);
  }

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return new Type_conversion_expression(this->type_, this->expr_->copy(),
					  this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type to convert to.
  Type* type_;
  // The expression to convert.
  Expression* expr_;
  // True if this is permitted to convert function types.  This is
  // used internally for method expressions.
  bool may_convert_function_types_;
};

// Traversal.

int
Type_conversion_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->expr_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Convert to a constant at lowering time.

Expression*
Type_conversion_expression::do_lower(Gogo*, Named_object*,
				     Statement_inserter*, int)
{
  Type* type = this->type_;
  Expression* val = this->expr_;
  Location location = this->location();

  if (type->integer_type() != NULL)
    {
      mpz_t ival;
      mpz_init(ival);
      Type* dummy;
      if (val->integer_constant_value(false, ival, &dummy))
	{
	  if (!Integer_expression::check_constant(ival, type, location))
	    mpz_set_ui(ival, 0);
	  Expression* ret = Expression::make_integer(&ival, type, location);
	  mpz_clear(ival);
	  return ret;
	}

      mpfr_t fval;
      mpfr_init(fval);
      if (val->float_constant_value(fval, &dummy))
	{
	  if (!mpfr_integer_p(fval))
	    {
	      error_at(location,
		       "floating point constant truncated to integer");
	      return Expression::make_error(location);
	    }
	  mpfr_get_z(ival, fval, GMP_RNDN);
	  if (!Integer_expression::check_constant(ival, type, location))
	    mpz_set_ui(ival, 0);
	  Expression* ret = Expression::make_integer(&ival, type, location);
	  mpfr_clear(fval);
	  mpz_clear(ival);
	  return ret;
	}
      mpfr_clear(fval);
      mpz_clear(ival);
    }

  if (type->float_type() != NULL)
    {
      mpfr_t fval;
      mpfr_init(fval);
      Type* dummy;
      if (val->float_constant_value(fval, &dummy))
	{
	  if (!Float_expression::check_constant(fval, type, location))
	    mpfr_set_ui(fval, 0, GMP_RNDN);
	  Float_expression::constrain_float(fval, type);
	  Expression *ret = Expression::make_float(&fval, type, location);
	  mpfr_clear(fval);
	  return ret;
	}
      mpfr_clear(fval);
    }

  if (type->complex_type() != NULL)
    {
      mpfr_t real;
      mpfr_t imag;
      mpfr_init(real);
      mpfr_init(imag);
      Type* dummy;
      if (val->complex_constant_value(real, imag, &dummy))
	{
	  if (!Complex_expression::check_constant(real, imag, type, location))
	    {
	      mpfr_set_ui(real, 0, GMP_RNDN);
	      mpfr_set_ui(imag, 0, GMP_RNDN);
	    }
	  Complex_expression::constrain_complex(real, imag, type);
	  Expression* ret = Expression::make_complex(&real, &imag, type,
						     location);
	  mpfr_clear(real);
	  mpfr_clear(imag);
	  return ret;
	}
      mpfr_clear(real);
      mpfr_clear(imag);
    }

  if (type->is_slice_type())
    {
      Type* element_type = type->array_type()->element_type()->forwarded();
      bool is_byte = element_type == Type::lookup_integer_type("uint8");
      bool is_int = element_type == Type::lookup_integer_type("int");
      if (is_byte || is_int)
	{
	  std::string s;
	  if (val->string_constant_value(&s))
	    {
	      Expression_list* vals = new Expression_list();
	      if (is_byte)
		{
		  for (std::string::const_iterator p = s.begin();
		       p != s.end();
		       p++)
		    {
		      mpz_t val;
		      mpz_init_set_ui(val, static_cast<unsigned char>(*p));
		      Expression* v = Expression::make_integer(&val,
							       element_type,
							       location);
		      vals->push_back(v);
		      mpz_clear(val);
		    }
		}
	      else
		{
		  const char *p = s.data();
		  const char *pend = s.data() + s.length();
		  while (p < pend)
		    {
		      unsigned int c;
		      int adv = Lex::fetch_char(p, &c);
		      if (adv == 0)
			{
			  warning_at(this->location(), 0,
				     "invalid UTF-8 encoding");
			  adv = 1;
			}
		      p += adv;
		      mpz_t val;
		      mpz_init_set_ui(val, c);
		      Expression* v = Expression::make_integer(&val,
							       element_type,
							       location);
		      vals->push_back(v);
		      mpz_clear(val);
		    }
		}

	      return Expression::make_slice_composite_literal(type, vals,
							      location);
	    }
	}
    }

  return this;
}

// Return the constant integer value if there is one.

bool
Type_conversion_expression::do_integer_constant_value(bool iota_is_constant,
						      mpz_t val,
						      Type** ptype) const
{
  if (this->type_->integer_type() == NULL)
    return false;

  mpz_t ival;
  mpz_init(ival);
  Type* dummy;
  if (this->expr_->integer_constant_value(iota_is_constant, ival, &dummy))
    {
      if (!Integer_expression::check_constant(ival, this->type_,
					      this->location()))
	{
	  mpz_clear(ival);
	  return false;
	}
      mpz_set(val, ival);
      mpz_clear(ival);
      *ptype = this->type_;
      return true;
    }
  mpz_clear(ival);

  mpfr_t fval;
  mpfr_init(fval);
  if (this->expr_->float_constant_value(fval, &dummy))
    {
      mpfr_get_z(val, fval, GMP_RNDN);
      mpfr_clear(fval);
      if (!Integer_expression::check_constant(val, this->type_,
					      this->location()))
	return false;
      *ptype = this->type_;
      return true;
    }
  mpfr_clear(fval);

  return false;
}

// Return the constant floating point value if there is one.

bool
Type_conversion_expression::do_float_constant_value(mpfr_t val,
						    Type** ptype) const
{
  if (this->type_->float_type() == NULL)
    return false;

  mpfr_t fval;
  mpfr_init(fval);
  Type* dummy;
  if (this->expr_->float_constant_value(fval, &dummy))
    {
      if (!Float_expression::check_constant(fval, this->type_,
					    this->location()))
	{
	  mpfr_clear(fval);
	  return false;
	}
      mpfr_set(val, fval, GMP_RNDN);
      mpfr_clear(fval);
      Float_expression::constrain_float(val, this->type_);
      *ptype = this->type_;
      return true;
    }
  mpfr_clear(fval);

  return false;
}

// Return the constant complex value if there is one.

bool
Type_conversion_expression::do_complex_constant_value(mpfr_t real,
						      mpfr_t imag,
						      Type **ptype) const
{
  if (this->type_->complex_type() == NULL)
    return false;

  mpfr_t rval;
  mpfr_t ival;
  mpfr_init(rval);
  mpfr_init(ival);
  Type* dummy;
  if (this->expr_->complex_constant_value(rval, ival, &dummy))
    {
      if (!Complex_expression::check_constant(rval, ival, this->type_,
					      this->location()))
	{
	  mpfr_clear(rval);
	  mpfr_clear(ival);
	  return false;
	}
      mpfr_set(real, rval, GMP_RNDN);
      mpfr_set(imag, ival, GMP_RNDN);
      mpfr_clear(rval);
      mpfr_clear(ival);
      Complex_expression::constrain_complex(real, imag, this->type_);
      *ptype = this->type_;
      return true;
    }
  mpfr_clear(rval);
  mpfr_clear(ival);

  return false;  
}

// Return the constant string value if there is one.

bool
Type_conversion_expression::do_string_constant_value(std::string* val) const
{
  if (this->type_->is_string_type()
      && this->expr_->type()->integer_type() != NULL)
    {
      mpz_t ival;
      mpz_init(ival);
      Type* dummy;
      if (this->expr_->integer_constant_value(false, ival, &dummy))
	{
	  unsigned long ulval = mpz_get_ui(ival);
	  if (mpz_cmp_ui(ival, ulval) == 0)
	    {
	      Lex::append_char(ulval, true, val, this->location());
	      mpz_clear(ival);
	      return true;
	    }
	}
      mpz_clear(ival);
    }

  // FIXME: Could handle conversion from const []int here.

  return false;
}

// Check that types are convertible.

void
Type_conversion_expression::do_check_types(Gogo*)
{
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();
  std::string reason;

  if (type->is_error() || expr_type->is_error())
    {
      this->set_is_error();
      return;
    }

  if (this->may_convert_function_types_
      && type->function_type() != NULL
      && expr_type->function_type() != NULL)
    return;

  if (Type::are_convertible(type, expr_type, &reason))
    return;

  error_at(this->location(), "%s", reason.c_str());
  this->set_is_error();
}

// Get a tree for a type conversion.

tree
Type_conversion_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  tree type_tree = type_to_tree(this->type_->get_backend(gogo));
  tree expr_tree = this->expr_->get_tree(context);

  if (type_tree == error_mark_node
      || expr_tree == error_mark_node
      || TREE_TYPE(expr_tree) == error_mark_node)
    return error_mark_node;

  if (TYPE_MAIN_VARIANT(type_tree) == TYPE_MAIN_VARIANT(TREE_TYPE(expr_tree)))
    return fold_convert(type_tree, expr_tree);

  Type* type = this->type_;
  Type* expr_type = this->expr_->type();
  tree ret;
  if (type->interface_type() != NULL || expr_type->interface_type() != NULL)
    ret = Expression::convert_for_assignment(context, type, expr_type,
					     expr_tree, this->location());
  else if (type->integer_type() != NULL)
    {
      if (expr_type->integer_type() != NULL
	  || expr_type->float_type() != NULL
	  || expr_type->is_unsafe_pointer_type())
	ret = fold(convert_to_integer(type_tree, expr_tree));
      else
	go_unreachable();
    }
  else if (type->float_type() != NULL)
    {
      if (expr_type->integer_type() != NULL
	  || expr_type->float_type() != NULL)
	ret = fold(convert_to_real(type_tree, expr_tree));
      else
	go_unreachable();
    }
  else if (type->complex_type() != NULL)
    {
      if (expr_type->complex_type() != NULL)
	ret = fold(convert_to_complex(type_tree, expr_tree));
      else
	go_unreachable();
    }
  else if (type->is_string_type()
	   && expr_type->integer_type() != NULL)
    {
      expr_tree = fold_convert(integer_type_node, expr_tree);
      if (host_integerp(expr_tree, 0))
	{
	  HOST_WIDE_INT intval = tree_low_cst(expr_tree, 0);
	  std::string s;
	  Lex::append_char(intval, true, &s, this->location());
	  Expression* se = Expression::make_string(s, this->location());
	  return se->get_tree(context);
	}

      static tree int_to_string_fndecl;
      ret = Gogo::call_builtin(&int_to_string_fndecl,
			       this->location(),
			       "__go_int_to_string",
			       1,
			       type_tree,
			       integer_type_node,
			       fold_convert(integer_type_node, expr_tree));
    }
  else if (type->is_string_type() && expr_type->is_slice_type())
    {
      if (!DECL_P(expr_tree))
	expr_tree = save_expr(expr_tree);
      Array_type* a = expr_type->array_type();
      Type* e = a->element_type()->forwarded();
      go_assert(e->integer_type() != NULL);
      tree valptr = fold_convert(const_ptr_type_node,
				 a->value_pointer_tree(gogo, expr_tree));
      tree len = a->length_tree(gogo, expr_tree);
      len = fold_convert_loc(this->location().gcc_location(), integer_type_node,
                             len);
      if (e->integer_type()->is_unsigned()
	  && e->integer_type()->bits() == 8)
	{
	  static tree byte_array_to_string_fndecl;
	  ret = Gogo::call_builtin(&byte_array_to_string_fndecl,
				   this->location(),
				   "__go_byte_array_to_string",
				   2,
				   type_tree,
				   const_ptr_type_node,
				   valptr,
				   integer_type_node,
				   len);
	}
      else
	{
	  go_assert(e == Type::lookup_integer_type("int"));
	  static tree int_array_to_string_fndecl;
	  ret = Gogo::call_builtin(&int_array_to_string_fndecl,
				   this->location(),
				   "__go_int_array_to_string",
				   2,
				   type_tree,
				   const_ptr_type_node,
				   valptr,
				   integer_type_node,
				   len);
	}
    }
  else if (type->is_slice_type() && expr_type->is_string_type())
    {
      Type* e = type->array_type()->element_type()->forwarded();
      go_assert(e->integer_type() != NULL);
      if (e->integer_type()->is_unsigned()
	  && e->integer_type()->bits() == 8)
	{
	  tree string_to_byte_array_fndecl = NULL_TREE;
	  ret = Gogo::call_builtin(&string_to_byte_array_fndecl,
				   this->location(),
				   "__go_string_to_byte_array",
				   1,
				   type_tree,
				   TREE_TYPE(expr_tree),
				   expr_tree);
	}
      else
	{
	  go_assert(e == Type::lookup_integer_type("int"));
	  tree string_to_int_array_fndecl = NULL_TREE;
	  ret = Gogo::call_builtin(&string_to_int_array_fndecl,
				   this->location(),
				   "__go_string_to_int_array",
				   1,
				   type_tree,
				   TREE_TYPE(expr_tree),
				   expr_tree);
	}
    }
  else if ((type->is_unsafe_pointer_type()
	    && expr_type->points_to() != NULL)
	   || (expr_type->is_unsafe_pointer_type()
	       && type->points_to() != NULL))
    ret = fold_convert(type_tree, expr_tree);
  else if (type->is_unsafe_pointer_type()
	   && expr_type->integer_type() != NULL)
    ret = convert_to_pointer(type_tree, expr_tree);
  else if (this->may_convert_function_types_
	   && type->function_type() != NULL
	   && expr_type->function_type() != NULL)
    ret = fold_convert_loc(this->location().gcc_location(), type_tree,
                           expr_tree);
  else
    ret = Expression::convert_for_assignment(context, type, expr_type,
					     expr_tree, this->location());

  return ret;
}

// Output a type conversion in a constant expression.

void
Type_conversion_expression::do_export(Export* exp) const
{
  exp->write_c_string("convert(");
  exp->write_type(this->type_);
  exp->write_c_string(", ");
  this->expr_->export_expression(exp);
  exp->write_c_string(")");
}

// Import a type conversion or a struct construction.

Expression*
Type_conversion_expression::do_import(Import* imp)
{
  imp->require_c_string("convert(");
  Type* type = imp->read_type();
  imp->require_c_string(", ");
  Expression* val = Expression::import_expression(imp);
  imp->require_c_string(")");
  return Expression::make_cast(type, val, imp->location());
}

// Dump ast representation for a type conversion expression.

void
Type_conversion_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << ") ";
}

// Make a type cast expression.

Expression*
Expression::make_cast(Type* type, Expression* val, Location location)
{
  if (type->is_error_type() || val->is_error_expression())
    return Expression::make_error(location);
  return new Type_conversion_expression(type, val, location);
}

// An unsafe type conversion, used to pass values to builtin functions.

class Unsafe_type_conversion_expression : public Expression
{
 public:
  Unsafe_type_conversion_expression(Type* type, Expression* expr,
				    Location location)
    : Expression(EXPRESSION_UNSAFE_CONVERSION, location),
      type_(type), expr_(expr)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*)
  { this->expr_->determine_type_no_context(); }

  Expression*
  do_copy()
  {
    return new Unsafe_type_conversion_expression(this->type_,
						 this->expr_->copy(),
						 this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type to convert to.
  Type* type_;
  // The expression to convert.
  Expression* expr_;
};

// Traversal.

int
Unsafe_type_conversion_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->expr_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Convert to backend representation.

tree
Unsafe_type_conversion_expression::do_get_tree(Translate_context* context)
{
  // We are only called for a limited number of cases.

  Type* t = this->type_;
  Type* et = this->expr_->type();

  tree type_tree = type_to_tree(this->type_->get_backend(context->gogo()));
  tree expr_tree = this->expr_->get_tree(context);
  if (type_tree == error_mark_node || expr_tree == error_mark_node)
    return error_mark_node;

  Location loc = this->location();

  bool use_view_convert = false;
  if (t->is_slice_type())
    {
      go_assert(et->is_slice_type());
      use_view_convert = true;
    }
  else if (t->map_type() != NULL)
    go_assert(et->map_type() != NULL);
  else if (t->channel_type() != NULL)
    go_assert(et->channel_type() != NULL);
  else if (t->points_to() != NULL && t->points_to()->channel_type() != NULL)
    go_assert((et->points_to() != NULL
		&& et->points_to()->channel_type() != NULL)
	       || et->is_nil_type());
  else if (t->points_to() != NULL)
    go_assert(et->points_to() != NULL || et->is_nil_type());
  else if (et->is_unsafe_pointer_type())
    go_assert(t->points_to() != NULL);
  else if (t->interface_type() != NULL && !t->interface_type()->is_empty())
    {
      go_assert(et->interface_type() != NULL
		 && !et->interface_type()->is_empty());
      use_view_convert = true;
    }
  else if (t->interface_type() != NULL && t->interface_type()->is_empty())
    {
      go_assert(et->interface_type() != NULL
		 && et->interface_type()->is_empty());
      use_view_convert = true;
    }
  else if (t->integer_type() != NULL)
    {
      go_assert(et->is_boolean_type()
		 || et->integer_type() != NULL
		 || et->function_type() != NULL
		 || et->points_to() != NULL
		 || et->map_type() != NULL
		 || et->channel_type() != NULL);
      return convert_to_integer(type_tree, expr_tree);
    }
  else
    go_unreachable();

  if (use_view_convert)
    return fold_build1_loc(loc.gcc_location(), VIEW_CONVERT_EXPR, type_tree,
                           expr_tree);
  else
    return fold_convert_loc(loc.gcc_location(), type_tree, expr_tree);
}

// Dump ast representation for an unsafe type conversion expression.

void
Unsafe_type_conversion_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << ") ";
}

// Make an unsafe type conversion expression.

Expression*
Expression::make_unsafe_cast(Type* type, Expression* expr,
			     Location location)
{
  return new Unsafe_type_conversion_expression(type, expr, location);
}

// Unary expressions.

class Unary_expression : public Expression
{
 public:
  Unary_expression(Operator op, Expression* expr, Location location)
    : Expression(EXPRESSION_UNARY, location),
      op_(op), escapes_(true), create_temp_(false), expr_(expr)
  { }

  // Return the operator.
  Operator
  op() const
  { return this->op_; }

  // Return the operand.
  Expression*
  operand() const
  { return this->expr_; }

  // Record that an address expression does not escape.
  void
  set_does_not_escape()
  {
    go_assert(this->op_ == OPERATOR_AND);
    this->escapes_ = false;
  }

  // Record that this is an address expression which should create a
  // temporary variable if necessary.  This is used for method calls.
  void
  set_create_temp()
  {
    go_assert(this->op_ == OPERATOR_AND);
    this->create_temp_ = true;
  }

  // Apply unary opcode OP to UVAL, setting VAL.  Return true if this
  // could be done, false if not.
  static bool
  eval_integer(Operator op, Type* utype, mpz_t uval, mpz_t val,
	       Location);

  // Apply unary opcode OP to UVAL, setting VAL.  Return true if this
  // could be done, false if not.
  static bool
  eval_float(Operator op, mpfr_t uval, mpfr_t val);

  // Apply unary opcode OP to UREAL/UIMAG, setting REAL/IMAG.  Return
  // true if this could be done, false if not.
  static bool
  eval_complex(Operator op, mpfr_t ureal, mpfr_t uimag, mpfr_t real,
	       mpfr_t imag);

  static Expression*
  do_import(Import*);

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->expr_, traverse); }

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  bool
  do_is_constant() const;

  bool
  do_integer_constant_value(bool, mpz_t, Type**) const;

  bool
  do_float_constant_value(mpfr_t, Type**) const;

  bool
  do_complex_constant_value(mpfr_t, mpfr_t, Type**) const;

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_unary(this->op_, this->expr_->copy(),
				  this->location());
  }

  bool
  do_must_eval_subexpressions_in_order(int*) const
  { return this->op_ == OPERATOR_MULT; }

  bool
  do_is_addressable() const
  { return this->op_ == OPERATOR_MULT; }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The unary operator to apply.
  Operator op_;
  // Normally true.  False if this is an address expression which does
  // not escape the current function.
  bool escapes_;
  // True if this is an address expression which should create a
  // temporary variable if necessary.
  bool create_temp_;
  // The operand.
  Expression* expr_;
};

// If we are taking the address of a composite literal, and the
// contents are not constant, then we want to make a heap composite
// instead.

Expression*
Unary_expression::do_lower(Gogo*, Named_object*, Statement_inserter*, int)
{
  Location loc = this->location();
  Operator op = this->op_;
  Expression* expr = this->expr_;

  if (op == OPERATOR_MULT && expr->is_type_expression())
    return Expression::make_type(Type::make_pointer_type(expr->type()), loc);

  // *&x simplifies to x.  *(*T)(unsafe.Pointer)(&x) does not require
  // moving x to the heap.  FIXME: Is it worth doing a real escape
  // analysis here?  This case is found in math/unsafe.go and is
  // therefore worth special casing.
  if (op == OPERATOR_MULT)
    {
      Expression* e = expr;
      while (e->classification() == EXPRESSION_CONVERSION)
	{
	  Type_conversion_expression* te
	    = static_cast<Type_conversion_expression*>(e);
	  e = te->expr();
	}

      if (e->classification() == EXPRESSION_UNARY)
	{
	  Unary_expression* ue = static_cast<Unary_expression*>(e);
	  if (ue->op_ == OPERATOR_AND)
	    {
	      if (e == expr)
		{
		  // *&x == x.
		  return ue->expr_;
		}
	      ue->set_does_not_escape();
	    }
	}
    }

  // Catching an invalid indirection of unsafe.Pointer here avoid
  // having to deal with TYPE_VOID in other places.
  if (op == OPERATOR_MULT && expr->type()->is_unsafe_pointer_type())
    {
      error_at(this->location(), "invalid indirect of %<unsafe.Pointer%>");
      return Expression::make_error(this->location());
    }

  if (op == OPERATOR_PLUS || op == OPERATOR_MINUS
      || op == OPERATOR_NOT || op == OPERATOR_XOR)
    {
      Expression* ret = NULL;

      mpz_t eval;
      mpz_init(eval);
      Type* etype;
      if (expr->integer_constant_value(false, eval, &etype))
	{
	  mpz_t val;
	  mpz_init(val);
	  if (Unary_expression::eval_integer(op, etype, eval, val, loc))
	    ret = Expression::make_integer(&val, etype, loc);
	  mpz_clear(val);
	}
      mpz_clear(eval);
      if (ret != NULL)
	return ret;

      if (op == OPERATOR_PLUS || op == OPERATOR_MINUS)
	{
	  mpfr_t fval;
	  mpfr_init(fval);
	  Type* ftype;
	  if (expr->float_constant_value(fval, &ftype))
	    {
	      mpfr_t val;
	      mpfr_init(val);
	      if (Unary_expression::eval_float(op, fval, val))
		ret = Expression::make_float(&val, ftype, loc);
	      mpfr_clear(val);
	    }
	  if (ret != NULL)
	    {
	      mpfr_clear(fval);
	      return ret;
	    }

	  mpfr_t ival;
	  mpfr_init(ival);
	  if (expr->complex_constant_value(fval, ival, &ftype))
	    {
	      mpfr_t real;
	      mpfr_t imag;
	      mpfr_init(real);
	      mpfr_init(imag);
	      if (Unary_expression::eval_complex(op, fval, ival, real, imag))
		ret = Expression::make_complex(&real, &imag, ftype, loc);
	      mpfr_clear(real);
	      mpfr_clear(imag);
	    }
	  mpfr_clear(ival);
	  mpfr_clear(fval);
	  if (ret != NULL)
	    return ret;
	}
    }

  return this;
}

// Return whether a unary expression is a constant.

bool
Unary_expression::do_is_constant() const
{
  if (this->op_ == OPERATOR_MULT)
    {
      // Indirecting through a pointer is only constant if the object
      // to which the expression points is constant, but we currently
      // have no way to determine that.
      return false;
    }
  else if (this->op_ == OPERATOR_AND)
    {
      // Taking the address of a variable is constant if it is a
      // global variable, not constant otherwise.  In other cases
      // taking the address is probably not a constant.
      Var_expression* ve = this->expr_->var_expression();
      if (ve != NULL)
	{
	  Named_object* no = ve->named_object();
	  return no->is_variable() && no->var_value()->is_global();
	}
      return false;
    }
  else
    return this->expr_->is_constant();
}

// Apply unary opcode OP to UVAL, setting VAL.  UTYPE is the type of
// UVAL, if known; it may be NULL.  Return true if this could be done,
// false if not.

bool
Unary_expression::eval_integer(Operator op, Type* utype, mpz_t uval, mpz_t val,
			       Location location)
{
  switch (op)
    {
    case OPERATOR_PLUS:
      mpz_set(val, uval);
      return true;
    case OPERATOR_MINUS:
      mpz_neg(val, uval);
      return Integer_expression::check_constant(val, utype, location);
    case OPERATOR_NOT:
      mpz_set_ui(val, mpz_cmp_si(uval, 0) == 0 ? 1 : 0);
      return true;
    case OPERATOR_XOR:
      if (utype == NULL
	  || utype->integer_type() == NULL
	  || utype->integer_type()->is_abstract())
	mpz_com(val, uval);
      else
	{
	  // The number of HOST_WIDE_INTs that it takes to represent
	  // UVAL.
	  size_t count = ((mpz_sizeinbase(uval, 2)
			   + HOST_BITS_PER_WIDE_INT
			   - 1)
			  / HOST_BITS_PER_WIDE_INT);

	  unsigned HOST_WIDE_INT* phwi = new unsigned HOST_WIDE_INT[count];
	  memset(phwi, 0, count * sizeof(HOST_WIDE_INT));

	  size_t ecount;
	  mpz_export(phwi, &ecount, -1, sizeof(HOST_WIDE_INT), 0, 0, uval);
	  go_assert(ecount <= count);

	  // Trim down to the number of words required by the type.
	  size_t obits = utype->integer_type()->bits();
	  if (!utype->integer_type()->is_unsigned())
	    ++obits;
	  size_t ocount = ((obits + HOST_BITS_PER_WIDE_INT - 1)
			   / HOST_BITS_PER_WIDE_INT);
	  go_assert(ocount <= count);

	  for (size_t i = 0; i < ocount; ++i)
	    phwi[i] = ~phwi[i];

	  size_t clearbits = ocount * HOST_BITS_PER_WIDE_INT - obits;
	  if (clearbits != 0)
	    phwi[ocount - 1] &= (((unsigned HOST_WIDE_INT) (HOST_WIDE_INT) -1)
				 >> clearbits);

	  mpz_import(val, ocount, -1, sizeof(HOST_WIDE_INT), 0, 0, phwi);

	  delete[] phwi;
	}
      return Integer_expression::check_constant(val, utype, location);
    case OPERATOR_AND:
    case OPERATOR_MULT:
      return false;
    default:
      go_unreachable();
    }
}

// Apply unary opcode OP to UVAL, setting VAL.  Return true if this
// could be done, false if not.

bool
Unary_expression::eval_float(Operator op, mpfr_t uval, mpfr_t val)
{
  switch (op)
    {
    case OPERATOR_PLUS:
      mpfr_set(val, uval, GMP_RNDN);
      return true;
    case OPERATOR_MINUS:
      mpfr_neg(val, uval, GMP_RNDN);
      return true;
    case OPERATOR_NOT:
    case OPERATOR_XOR:
    case OPERATOR_AND:
    case OPERATOR_MULT:
      return false;
    default:
      go_unreachable();
    }
}

// Apply unary opcode OP to RVAL/IVAL, setting REAL/IMAG.  Return true
// if this could be done, false if not.

bool
Unary_expression::eval_complex(Operator op, mpfr_t rval, mpfr_t ival,
			       mpfr_t real, mpfr_t imag)
{
  switch (op)
    {
    case OPERATOR_PLUS:
      mpfr_set(real, rval, GMP_RNDN);
      mpfr_set(imag, ival, GMP_RNDN);
      return true;
    case OPERATOR_MINUS:
      mpfr_neg(real, rval, GMP_RNDN);
      mpfr_neg(imag, ival, GMP_RNDN);
      return true;
    case OPERATOR_NOT:
    case OPERATOR_XOR:
    case OPERATOR_AND:
    case OPERATOR_MULT:
      return false;
    default:
      go_unreachable();
    }
}

// Return the integral constant value of a unary expression, if it has one.

bool
Unary_expression::do_integer_constant_value(bool iota_is_constant, mpz_t val,
					    Type** ptype) const
{
  mpz_t uval;
  mpz_init(uval);
  bool ret;
  if (!this->expr_->integer_constant_value(iota_is_constant, uval, ptype))
    ret = false;
  else
    ret = Unary_expression::eval_integer(this->op_, *ptype, uval, val,
					 this->location());
  mpz_clear(uval);
  return ret;
}

// Return the floating point constant value of a unary expression, if
// it has one.

bool
Unary_expression::do_float_constant_value(mpfr_t val, Type** ptype) const
{
  mpfr_t uval;
  mpfr_init(uval);
  bool ret;
  if (!this->expr_->float_constant_value(uval, ptype))
    ret = false;
  else
    ret = Unary_expression::eval_float(this->op_, uval, val);
  mpfr_clear(uval);
  return ret;
}

// Return the complex constant value of a unary expression, if it has
// one.

bool
Unary_expression::do_complex_constant_value(mpfr_t real, mpfr_t imag,
					    Type** ptype) const
{
  mpfr_t rval;
  mpfr_t ival;
  mpfr_init(rval);
  mpfr_init(ival);
  bool ret;
  if (!this->expr_->complex_constant_value(rval, ival, ptype))
    ret = false;
  else
    ret = Unary_expression::eval_complex(this->op_, rval, ival, real, imag);
  mpfr_clear(rval);
  mpfr_clear(ival);
  return ret;
}

// Return the type of a unary expression.

Type*
Unary_expression::do_type()
{
  switch (this->op_)
    {
    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
    case OPERATOR_NOT:
    case OPERATOR_XOR:
      return this->expr_->type();

    case OPERATOR_AND:
      return Type::make_pointer_type(this->expr_->type());

    case OPERATOR_MULT:
      {
	Type* subtype = this->expr_->type();
	Type* points_to = subtype->points_to();
	if (points_to == NULL)
	  return Type::make_error_type();
	return points_to;
      }

    default:
      go_unreachable();
    }
}

// Determine abstract types for a unary expression.

void
Unary_expression::do_determine_type(const Type_context* context)
{
  switch (this->op_)
    {
    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
    case OPERATOR_NOT:
    case OPERATOR_XOR:
      this->expr_->determine_type(context);
      break;

    case OPERATOR_AND:
      // Taking the address of something.
      {
	Type* subtype = (context->type == NULL
			 ? NULL
			 : context->type->points_to());
	Type_context subcontext(subtype, false);
	this->expr_->determine_type(&subcontext);
      }
      break;

    case OPERATOR_MULT:
      // Indirecting through a pointer.
      {
	Type* subtype = (context->type == NULL
			 ? NULL
			 : Type::make_pointer_type(context->type));
	Type_context subcontext(subtype, false);
	this->expr_->determine_type(&subcontext);
      }
      break;

    default:
      go_unreachable();
    }
}

// Check types for a unary expression.

void
Unary_expression::do_check_types(Gogo*)
{
  Type* type = this->expr_->type();
  if (type->is_error())
    {
      this->set_is_error();
      return;
    }

  switch (this->op_)
    {
    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
      if (type->integer_type() == NULL
	  && type->float_type() == NULL
	  && type->complex_type() == NULL)
	this->report_error(_("expected numeric type"));
      break;

    case OPERATOR_NOT:
    case OPERATOR_XOR:
      if (type->integer_type() == NULL
	  && !type->is_boolean_type())
	this->report_error(_("expected integer or boolean type"));
      break;

    case OPERATOR_AND:
      if (!this->expr_->is_addressable())
	{
	  if (!this->create_temp_)
	    this->report_error(_("invalid operand for unary %<&%>"));
	}
      else
	this->expr_->address_taken(this->escapes_);
      break;

    case OPERATOR_MULT:
      // Indirecting through a pointer.
      if (type->points_to() == NULL)
	this->report_error(_("expected pointer"));
      break;

    default:
      go_unreachable();
    }
}

// Get a tree for a unary expression.

tree
Unary_expression::do_get_tree(Translate_context* context)
{
  tree expr = this->expr_->get_tree(context);
  if (expr == error_mark_node)
    return error_mark_node;

  Location loc = this->location();
  switch (this->op_)
    {
    case OPERATOR_PLUS:
      return expr;

    case OPERATOR_MINUS:
      {
	tree type = TREE_TYPE(expr);
	tree compute_type = excess_precision_type(type);
	if (compute_type != NULL_TREE)
	  expr = ::convert(compute_type, expr);
	tree ret = fold_build1_loc(loc.gcc_location(), NEGATE_EXPR,
				   (compute_type != NULL_TREE
				    ? compute_type
				    : type),
				   expr);
	if (compute_type != NULL_TREE)
	  ret = ::convert(type, ret);
	return ret;
      }

    case OPERATOR_NOT:
      if (TREE_CODE(TREE_TYPE(expr)) == BOOLEAN_TYPE)
	return fold_build1_loc(loc.gcc_location(), TRUTH_NOT_EXPR,
                               TREE_TYPE(expr), expr);
      else
	return fold_build2_loc(loc.gcc_location(), NE_EXPR, boolean_type_node,
                               expr, build_int_cst(TREE_TYPE(expr), 0));

    case OPERATOR_XOR:
      return fold_build1_loc(loc.gcc_location(), BIT_NOT_EXPR, TREE_TYPE(expr),
                             expr);

    case OPERATOR_AND:
      if (!this->create_temp_)
	{
	  // We should not see a non-constant constructor here; cases
	  // where we would see one should have been moved onto the
	  // heap at parse time.  Taking the address of a nonconstant
	  // constructor will not do what the programmer expects.
	  go_assert(TREE_CODE(expr) != CONSTRUCTOR || TREE_CONSTANT(expr));
	  go_assert(TREE_CODE(expr) != ADDR_EXPR);
	}

      // Build a decl for a constant constructor.
      if (TREE_CODE(expr) == CONSTRUCTOR && TREE_CONSTANT(expr))
	{
	  tree decl = build_decl(this->location().gcc_location(), VAR_DECL,
				 create_tmp_var_name("C"), TREE_TYPE(expr));
	  DECL_EXTERNAL(decl) = 0;
	  TREE_PUBLIC(decl) = 0;
	  TREE_READONLY(decl) = 1;
	  TREE_CONSTANT(decl) = 1;
	  TREE_STATIC(decl) = 1;
	  TREE_ADDRESSABLE(decl) = 1;
	  DECL_ARTIFICIAL(decl) = 1;
	  DECL_INITIAL(decl) = expr;
	  rest_of_decl_compilation(decl, 1, 0);
	  expr = decl;
	}

      if (this->create_temp_
	  && !TREE_ADDRESSABLE(TREE_TYPE(expr))
	  && !DECL_P(expr)
	  && TREE_CODE(expr) != INDIRECT_REF
	  && TREE_CODE(expr) != COMPONENT_REF)
	{
	  tree tmp = create_tmp_var(TREE_TYPE(expr), get_name(expr));
	  DECL_IGNORED_P(tmp) = 1;
	  DECL_INITIAL(tmp) = expr;
	  TREE_ADDRESSABLE(tmp) = 1;
	  return build2_loc(loc.gcc_location(), COMPOUND_EXPR,
			    build_pointer_type(TREE_TYPE(expr)),
			    build1_loc(loc.gcc_location(), DECL_EXPR,
                                       void_type_node, tmp),
			    build_fold_addr_expr_loc(loc.gcc_location(), tmp));
	}

      return build_fold_addr_expr_loc(loc.gcc_location(), expr);

    case OPERATOR_MULT:
      {
	go_assert(POINTER_TYPE_P(TREE_TYPE(expr)));

	// If we are dereferencing the pointer to a large struct, we
	// need to check for nil.  We don't bother to check for small
	// structs because we expect the system to crash on a nil
	// pointer dereference.
	HOST_WIDE_INT s = int_size_in_bytes(TREE_TYPE(TREE_TYPE(expr)));
	if (s == -1 || s >= 4096)
	  {
	    if (!DECL_P(expr))
	      expr = save_expr(expr);
	    tree compare = fold_build2_loc(loc.gcc_location(), EQ_EXPR,
                                           boolean_type_node,
					   expr,
					   fold_convert(TREE_TYPE(expr),
							null_pointer_node));
	    tree crash = Gogo::runtime_error(RUNTIME_ERROR_NIL_DEREFERENCE,
					     loc);
	    expr = fold_build2_loc(loc.gcc_location(), COMPOUND_EXPR,
                                   TREE_TYPE(expr), build3(COND_EXPR,
                                                           void_type_node,
                                                           compare, crash,
                                                           NULL_TREE),
				   expr);
	  }

	// If the type of EXPR is a recursive pointer type, then we
	// need to insert a cast before indirecting.
	if (TREE_TYPE(TREE_TYPE(expr)) == ptr_type_node)
	  {
	    Type* pt = this->expr_->type()->points_to();
	    tree ind = type_to_tree(pt->get_backend(context->gogo()));
	    expr = fold_convert_loc(loc.gcc_location(),
                                    build_pointer_type(ind), expr);
	  }

	return build_fold_indirect_ref_loc(loc.gcc_location(), expr);
      }

    default:
      go_unreachable();
    }
}

// Export a unary expression.

void
Unary_expression::do_export(Export* exp) const
{
  switch (this->op_)
    {
    case OPERATOR_PLUS:
      exp->write_c_string("+ ");
      break;
    case OPERATOR_MINUS:
      exp->write_c_string("- ");
      break;
    case OPERATOR_NOT:
      exp->write_c_string("! ");
      break;
    case OPERATOR_XOR:
      exp->write_c_string("^ ");
      break;
    case OPERATOR_AND:
    case OPERATOR_MULT:
    default:
      go_unreachable();
    }
  this->expr_->export_expression(exp);
}

// Import a unary expression.

Expression*
Unary_expression::do_import(Import* imp)
{
  Operator op;
  switch (imp->get_char())
    {
    case '+':
      op = OPERATOR_PLUS;
      break;
    case '-':
      op = OPERATOR_MINUS;
      break;
    case '!':
      op = OPERATOR_NOT;
      break;
    case '^':
      op = OPERATOR_XOR;
      break;
    default:
      go_unreachable();
    }
  imp->require_c_string(" ");
  Expression* expr = Expression::import_expression(imp);
  return Expression::make_unary(op, expr, imp->location());
}

// Dump ast representation of an unary expression.

void
Unary_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_operator(this->op_);
  ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << ") ";
}

// Make a unary expression.

Expression*
Expression::make_unary(Operator op, Expression* expr, Location location)
{
  return new Unary_expression(op, expr, location);
}

// If this is an indirection through a pointer, return the expression
// being pointed through.  Otherwise return this.

Expression*
Expression::deref()
{
  if (this->classification_ == EXPRESSION_UNARY)
    {
      Unary_expression* ue = static_cast<Unary_expression*>(this);
      if (ue->op() == OPERATOR_MULT)
	return ue->operand();
    }
  return this;
}

// Class Binary_expression.

// Traversal.

int
Binary_expression::do_traverse(Traverse* traverse)
{
  int t = Expression::traverse(&this->left_, traverse);
  if (t == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return Expression::traverse(&this->right_, traverse);
}

// Compare integer constants according to OP.

bool
Binary_expression::compare_integer(Operator op, mpz_t left_val,
				   mpz_t right_val)
{
  int i = mpz_cmp(left_val, right_val);
  switch (op)
    {
    case OPERATOR_EQEQ:
      return i == 0;
    case OPERATOR_NOTEQ:
      return i != 0;
    case OPERATOR_LT:
      return i < 0;
    case OPERATOR_LE:
      return i <= 0;
    case OPERATOR_GT:
      return i > 0;
    case OPERATOR_GE:
      return i >= 0;
    default:
      go_unreachable();
    }
}

// Compare floating point constants according to OP.

bool
Binary_expression::compare_float(Operator op, Type* type, mpfr_t left_val,
				 mpfr_t right_val)
{
  int i;
  if (type == NULL)
    i = mpfr_cmp(left_val, right_val);
  else
    {
      mpfr_t lv;
      mpfr_init_set(lv, left_val, GMP_RNDN);
      mpfr_t rv;
      mpfr_init_set(rv, right_val, GMP_RNDN);
      Float_expression::constrain_float(lv, type);
      Float_expression::constrain_float(rv, type);
      i = mpfr_cmp(lv, rv);
      mpfr_clear(lv);
      mpfr_clear(rv);
    }
  switch (op)
    {
    case OPERATOR_EQEQ:
      return i == 0;
    case OPERATOR_NOTEQ:
      return i != 0;
    case OPERATOR_LT:
      return i < 0;
    case OPERATOR_LE:
      return i <= 0;
    case OPERATOR_GT:
      return i > 0;
    case OPERATOR_GE:
      return i >= 0;
    default:
      go_unreachable();
    }
}

// Compare complex constants according to OP.  Complex numbers may
// only be compared for equality.

bool
Binary_expression::compare_complex(Operator op, Type* type,
				   mpfr_t left_real, mpfr_t left_imag,
				   mpfr_t right_real, mpfr_t right_imag)
{
  bool is_equal;
  if (type == NULL)
    is_equal = (mpfr_cmp(left_real, right_real) == 0
		&& mpfr_cmp(left_imag, right_imag) == 0);
  else
    {
      mpfr_t lr;
      mpfr_t li;
      mpfr_init_set(lr, left_real, GMP_RNDN);
      mpfr_init_set(li, left_imag, GMP_RNDN);
      mpfr_t rr;
      mpfr_t ri;
      mpfr_init_set(rr, right_real, GMP_RNDN);
      mpfr_init_set(ri, right_imag, GMP_RNDN);
      Complex_expression::constrain_complex(lr, li, type);
      Complex_expression::constrain_complex(rr, ri, type);
      is_equal = mpfr_cmp(lr, rr) == 0 && mpfr_cmp(li, ri) == 0;
      mpfr_clear(lr);
      mpfr_clear(li);
      mpfr_clear(rr);
      mpfr_clear(ri);
    }
  switch (op)
    {
    case OPERATOR_EQEQ:
      return is_equal;
    case OPERATOR_NOTEQ:
      return !is_equal;
    default:
      go_unreachable();
    }
}

// Apply binary opcode OP to LEFT_VAL and RIGHT_VAL, setting VAL.
// LEFT_TYPE is the type of LEFT_VAL, RIGHT_TYPE is the type of
// RIGHT_VAL; LEFT_TYPE and/or RIGHT_TYPE may be NULL.  Return true if
// this could be done, false if not.

bool
Binary_expression::eval_integer(Operator op, Type* left_type, mpz_t left_val,
				Type* right_type, mpz_t right_val,
				Location location, mpz_t val)
{
  bool is_shift_op = false;
  switch (op)
    {
    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      // These return boolean values.  We should probably handle them
      // anyhow in case a type conversion is used on the result.
      return false;
    case OPERATOR_PLUS:
      mpz_add(val, left_val, right_val);
      break;
    case OPERATOR_MINUS:
      mpz_sub(val, left_val, right_val);
      break;
    case OPERATOR_OR:
      mpz_ior(val, left_val, right_val);
      break;
    case OPERATOR_XOR:
      mpz_xor(val, left_val, right_val);
      break;
    case OPERATOR_MULT:
      mpz_mul(val, left_val, right_val);
      break;
    case OPERATOR_DIV:
      if (mpz_sgn(right_val) != 0)
	mpz_tdiv_q(val, left_val, right_val);
      else
	{
	  error_at(location, "division by zero");
	  mpz_set_ui(val, 0);
	  return true;
	}
      break;
    case OPERATOR_MOD:
      if (mpz_sgn(right_val) != 0)
	mpz_tdiv_r(val, left_val, right_val);
      else
	{
	  error_at(location, "division by zero");
	  mpz_set_ui(val, 0);
	  return true;
	}
      break;
    case OPERATOR_LSHIFT:
      {
	unsigned long shift = mpz_get_ui(right_val);
	if (mpz_cmp_ui(right_val, shift) != 0 || shift > 0x100000)
	  {
	    error_at(location, "shift count overflow");
	    mpz_set_ui(val, 0);
	    return true;
	  }
	mpz_mul_2exp(val, left_val, shift);
	is_shift_op = true;
	break;
      }
      break;
    case OPERATOR_RSHIFT:
      {
	unsigned long shift = mpz_get_ui(right_val);
	if (mpz_cmp_ui(right_val, shift) != 0)
	  {
	    error_at(location, "shift count overflow");
	    mpz_set_ui(val, 0);
	    return true;
	  }
	if (mpz_cmp_ui(left_val, 0) >= 0)
	  mpz_tdiv_q_2exp(val, left_val, shift);
	else
	  mpz_fdiv_q_2exp(val, left_val, shift);
	is_shift_op = true;
	break;
      }
      break;
    case OPERATOR_AND:
      mpz_and(val, left_val, right_val);
      break;
    case OPERATOR_BITCLEAR:
      {
	mpz_t tval;
	mpz_init(tval);
	mpz_com(tval, right_val);
	mpz_and(val, left_val, tval);
	mpz_clear(tval);
      }
      break;
    default:
      go_unreachable();
    }

  Type* type = left_type;
  if (!is_shift_op)
    {
      if (type == NULL)
	type = right_type;
      else if (type != right_type && right_type != NULL)
	{
	  if (type->is_abstract())
	    type = right_type;
	  else if (!right_type->is_abstract())
	    {
	      // This look like a type error which should be diagnosed
	      // elsewhere.  Don't do anything here, to avoid an
	      // unhelpful chain of error messages.
	      return true;
	    }
	}
    }

  if (type != NULL && !type->is_abstract())
    {
      // We have to check the operands too, as we have implicitly
      // coerced them to TYPE.
      if ((type != left_type
	   && !Integer_expression::check_constant(left_val, type, location))
	  || (!is_shift_op
	      && type != right_type
	      && !Integer_expression::check_constant(right_val, type,
						     location))
	  || !Integer_expression::check_constant(val, type, location))
	mpz_set_ui(val, 0);
    }

  return true;
}

// Apply binary opcode OP to LEFT_VAL and RIGHT_VAL, setting VAL.
// Return true if this could be done, false if not.

bool
Binary_expression::eval_float(Operator op, Type* left_type, mpfr_t left_val,
			      Type* right_type, mpfr_t right_val,
			      mpfr_t val, Location location)
{
  switch (op)
    {
    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      // These return boolean values.  We should probably handle them
      // anyhow in case a type conversion is used on the result.
      return false;
    case OPERATOR_PLUS:
      mpfr_add(val, left_val, right_val, GMP_RNDN);
      break;
    case OPERATOR_MINUS:
      mpfr_sub(val, left_val, right_val, GMP_RNDN);
      break;
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_AND:
    case OPERATOR_BITCLEAR:
      return false;
    case OPERATOR_MULT:
      mpfr_mul(val, left_val, right_val, GMP_RNDN);
      break;
    case OPERATOR_DIV:
      if (mpfr_zero_p(right_val))
	error_at(location, "division by zero");
      mpfr_div(val, left_val, right_val, GMP_RNDN);
      break;
    case OPERATOR_MOD:
      return false;
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      return false;
    default:
      go_unreachable();
    }

  Type* type = left_type;
  if (type == NULL)
    type = right_type;
  else if (type != right_type && right_type != NULL)
    {
      if (type->is_abstract())
	type = right_type;
      else if (!right_type->is_abstract())
	{
	  // This looks like a type error which should be diagnosed
	  // elsewhere.  Don't do anything here, to avoid an unhelpful
	  // chain of error messages.
	  return true;
	}
    }

  if (type != NULL && !type->is_abstract())
    {
      if ((type != left_type
	   && !Float_expression::check_constant(left_val, type, location))
	  || (type != right_type
	      && !Float_expression::check_constant(right_val, type,
						   location))
	  || !Float_expression::check_constant(val, type, location))
	mpfr_set_ui(val, 0, GMP_RNDN);
    }

  return true;
}

// Apply binary opcode OP to LEFT_REAL/LEFT_IMAG and
// RIGHT_REAL/RIGHT_IMAG, setting REAL/IMAG.  Return true if this
// could be done, false if not.

bool
Binary_expression::eval_complex(Operator op, Type* left_type,
				mpfr_t left_real, mpfr_t left_imag,
				Type *right_type,
				mpfr_t right_real, mpfr_t right_imag,
				mpfr_t real, mpfr_t imag,
				Location location)
{
  switch (op)
    {
    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      // These return boolean values and must be handled differently.
      return false;
    case OPERATOR_PLUS:
      mpfr_add(real, left_real, right_real, GMP_RNDN);
      mpfr_add(imag, left_imag, right_imag, GMP_RNDN);
      break;
    case OPERATOR_MINUS:
      mpfr_sub(real, left_real, right_real, GMP_RNDN);
      mpfr_sub(imag, left_imag, right_imag, GMP_RNDN);
      break;
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_AND:
    case OPERATOR_BITCLEAR:
      return false;
    case OPERATOR_MULT:
      {
	// You might think that multiplying two complex numbers would
	// be simple, and you would be right, until you start to think
	// about getting the right answer for infinity.  If one
	// operand here is infinity and the other is anything other
	// than zero or NaN, then we are going to wind up subtracting
	// two infinity values.  That will give us a NaN, but the
	// correct answer is infinity.

	mpfr_t lrrr;
	mpfr_init(lrrr);
	mpfr_mul(lrrr, left_real, right_real, GMP_RNDN);

	mpfr_t lrri;
	mpfr_init(lrri);
	mpfr_mul(lrri, left_real, right_imag, GMP_RNDN);

	mpfr_t lirr;
	mpfr_init(lirr);
	mpfr_mul(lirr, left_imag, right_real, GMP_RNDN);

	mpfr_t liri;
	mpfr_init(liri);
	mpfr_mul(liri, left_imag, right_imag, GMP_RNDN);

	mpfr_sub(real, lrrr, liri, GMP_RNDN);
	mpfr_add(imag, lrri, lirr, GMP_RNDN);

	// If we get NaN on both sides, check whether it should really
	// be infinity.  The rule is that if either side of the
	// complex number is infinity, then the whole value is
	// infinity, even if the other side is NaN.  So the only case
	// we have to fix is the one in which both sides are NaN.
	if (mpfr_nan_p(real) && mpfr_nan_p(imag)
	    && (!mpfr_nan_p(left_real) || !mpfr_nan_p(left_imag))
	    && (!mpfr_nan_p(right_real) || !mpfr_nan_p(right_imag)))
	  {
	    bool is_infinity = false;

	    mpfr_t lr;
	    mpfr_t li;
	    mpfr_init_set(lr, left_real, GMP_RNDN);
	    mpfr_init_set(li, left_imag, GMP_RNDN);

	    mpfr_t rr;
	    mpfr_t ri;
	    mpfr_init_set(rr, right_real, GMP_RNDN);
	    mpfr_init_set(ri, right_imag, GMP_RNDN);

	    // If the left side is infinity, then the result is
	    // infinity.
	    if (mpfr_inf_p(lr) || mpfr_inf_p(li))
	      {
		mpfr_set_ui(lr, mpfr_inf_p(lr) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(lr, lr, left_real, GMP_RNDN);
		mpfr_set_ui(li, mpfr_inf_p(li) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(li, li, left_imag, GMP_RNDN);
		if (mpfr_nan_p(rr))
		  {
		    mpfr_set_ui(rr, 0, GMP_RNDN);
		    mpfr_copysign(rr, rr, right_real, GMP_RNDN);
		  }
		if (mpfr_nan_p(ri))
		  {
		    mpfr_set_ui(ri, 0, GMP_RNDN);
		    mpfr_copysign(ri, ri, right_imag, GMP_RNDN);
		  }
		is_infinity = true;
	      }

	    // If the right side is infinity, then the result is
	    // infinity.
	    if (mpfr_inf_p(rr) || mpfr_inf_p(ri))
	      {
		mpfr_set_ui(rr, mpfr_inf_p(rr) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(rr, rr, right_real, GMP_RNDN);
		mpfr_set_ui(ri, mpfr_inf_p(ri) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(ri, ri, right_imag, GMP_RNDN);
		if (mpfr_nan_p(lr))
		  {
		    mpfr_set_ui(lr, 0, GMP_RNDN);
		    mpfr_copysign(lr, lr, left_real, GMP_RNDN);
		  }
		if (mpfr_nan_p(li))
		  {
		    mpfr_set_ui(li, 0, GMP_RNDN);
		    mpfr_copysign(li, li, left_imag, GMP_RNDN);
		  }
		is_infinity = true;
	      }

	    // If we got an overflow in the intermediate computations,
	    // then the result is infinity.
	    if (!is_infinity
		&& (mpfr_inf_p(lrrr) || mpfr_inf_p(lrri)
		    || mpfr_inf_p(lirr) || mpfr_inf_p(liri)))
	      {
		if (mpfr_nan_p(lr))
		  {
		    mpfr_set_ui(lr, 0, GMP_RNDN);
		    mpfr_copysign(lr, lr, left_real, GMP_RNDN);
		  }
		if (mpfr_nan_p(li))
		  {
		    mpfr_set_ui(li, 0, GMP_RNDN);
		    mpfr_copysign(li, li, left_imag, GMP_RNDN);
		  }
		if (mpfr_nan_p(rr))
		  {
		    mpfr_set_ui(rr, 0, GMP_RNDN);
		    mpfr_copysign(rr, rr, right_real, GMP_RNDN);
		  }
		if (mpfr_nan_p(ri))
		  {
		    mpfr_set_ui(ri, 0, GMP_RNDN);
		    mpfr_copysign(ri, ri, right_imag, GMP_RNDN);
		  }
		is_infinity = true;
	      }

	    if (is_infinity)
	      {
		mpfr_mul(lrrr, lr, rr, GMP_RNDN);
		mpfr_mul(lrri, lr, ri, GMP_RNDN);
		mpfr_mul(lirr, li, rr, GMP_RNDN);
		mpfr_mul(liri, li, ri, GMP_RNDN);
		mpfr_sub(real, lrrr, liri, GMP_RNDN);
		mpfr_add(imag, lrri, lirr, GMP_RNDN);
		mpfr_set_inf(real, mpfr_sgn(real));
		mpfr_set_inf(imag, mpfr_sgn(imag));
	      }

	    mpfr_clear(lr);
	    mpfr_clear(li);
	    mpfr_clear(rr);
	    mpfr_clear(ri);
	  }

	mpfr_clear(lrrr);
	mpfr_clear(lrri);
	mpfr_clear(lirr);
	mpfr_clear(liri);				  
      }
      break;
    case OPERATOR_DIV:
      {
	// For complex division we want to avoid having an
	// intermediate overflow turn the whole result in a NaN.  We
	// scale the values to try to avoid this.

	if (mpfr_zero_p(right_real) && mpfr_zero_p(right_imag))
	  error_at(location, "division by zero");

	mpfr_t rra;
	mpfr_t ria;
	mpfr_init(rra);
	mpfr_init(ria);
	mpfr_abs(rra, right_real, GMP_RNDN);
	mpfr_abs(ria, right_imag, GMP_RNDN);
	mpfr_t t;
	mpfr_init(t);
	mpfr_max(t, rra, ria, GMP_RNDN);

	mpfr_t rr;
	mpfr_t ri;
	mpfr_init_set(rr, right_real, GMP_RNDN);
	mpfr_init_set(ri, right_imag, GMP_RNDN);
	long ilogbw = 0;
	if (!mpfr_inf_p(t) && !mpfr_nan_p(t) && !mpfr_zero_p(t))
	  {
	    ilogbw = mpfr_get_exp(t);
	    mpfr_mul_2si(rr, rr, - ilogbw, GMP_RNDN);
	    mpfr_mul_2si(ri, ri, - ilogbw, GMP_RNDN);
	  }

	mpfr_t denom;
	mpfr_init(denom);
	mpfr_mul(denom, rr, rr, GMP_RNDN);
	mpfr_mul(t, ri, ri, GMP_RNDN);
	mpfr_add(denom, denom, t, GMP_RNDN);

	mpfr_mul(real, left_real, rr, GMP_RNDN);
	mpfr_mul(t, left_imag, ri, GMP_RNDN);
	mpfr_add(real, real, t, GMP_RNDN);
	mpfr_div(real, real, denom, GMP_RNDN);
	mpfr_mul_2si(real, real, - ilogbw, GMP_RNDN);

	mpfr_mul(imag, left_imag, rr, GMP_RNDN);
	mpfr_mul(t, left_real, ri, GMP_RNDN);
	mpfr_sub(imag, imag, t, GMP_RNDN);
	mpfr_div(imag, imag, denom, GMP_RNDN);
	mpfr_mul_2si(imag, imag, - ilogbw, GMP_RNDN);

	// If we wind up with NaN on both sides, check whether we
	// should really have infinity.  The rule is that if either
	// side of the complex number is infinity, then the whole
	// value is infinity, even if the other side is NaN.  So the
	// only case we have to fix is the one in which both sides are
	// NaN.
	if (mpfr_nan_p(real) && mpfr_nan_p(imag)
	    && (!mpfr_nan_p(left_real) || !mpfr_nan_p(left_imag))
	    && (!mpfr_nan_p(right_real) || !mpfr_nan_p(right_imag)))
	  {
	    if (mpfr_zero_p(denom))
	      {
		mpfr_set_inf(real, mpfr_sgn(rr));
		mpfr_mul(real, real, left_real, GMP_RNDN);
		mpfr_set_inf(imag, mpfr_sgn(rr));
		mpfr_mul(imag, imag, left_imag, GMP_RNDN);
	      }
	    else if ((mpfr_inf_p(left_real) || mpfr_inf_p(left_imag))
		     && mpfr_number_p(rr) && mpfr_number_p(ri))
	      {
		mpfr_set_ui(t, mpfr_inf_p(left_real) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(t, t, left_real, GMP_RNDN);

		mpfr_t t2;
		mpfr_init_set_ui(t2, mpfr_inf_p(left_imag) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(t2, t2, left_imag, GMP_RNDN);

		mpfr_t t3;
		mpfr_init(t3);
		mpfr_mul(t3, t, rr, GMP_RNDN);

		mpfr_t t4;
		mpfr_init(t4);
		mpfr_mul(t4, t2, ri, GMP_RNDN);

		mpfr_add(t3, t3, t4, GMP_RNDN);
		mpfr_set_inf(real, mpfr_sgn(t3));

		mpfr_mul(t3, t2, rr, GMP_RNDN);
		mpfr_mul(t4, t, ri, GMP_RNDN);
		mpfr_sub(t3, t3, t4, GMP_RNDN);
		mpfr_set_inf(imag, mpfr_sgn(t3));

		mpfr_clear(t2);
		mpfr_clear(t3);
		mpfr_clear(t4);
	      }
	    else if ((mpfr_inf_p(right_real) || mpfr_inf_p(right_imag))
		     && mpfr_number_p(left_real) && mpfr_number_p(left_imag))
	      {
		mpfr_set_ui(t, mpfr_inf_p(rr) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(t, t, rr, GMP_RNDN);

		mpfr_t t2;
		mpfr_init_set_ui(t2, mpfr_inf_p(ri) ? 1 : 0, GMP_RNDN);
		mpfr_copysign(t2, t2, ri, GMP_RNDN);

		mpfr_t t3;
		mpfr_init(t3);
		mpfr_mul(t3, left_real, t, GMP_RNDN);

		mpfr_t t4;
		mpfr_init(t4);
		mpfr_mul(t4, left_imag, t2, GMP_RNDN);

		mpfr_add(t3, t3, t4, GMP_RNDN);
		mpfr_set_ui(real, 0, GMP_RNDN);
		mpfr_mul(real, real, t3, GMP_RNDN);

		mpfr_mul(t3, left_imag, t, GMP_RNDN);
		mpfr_mul(t4, left_real, t2, GMP_RNDN);
		mpfr_sub(t3, t3, t4, GMP_RNDN);
		mpfr_set_ui(imag, 0, GMP_RNDN);
		mpfr_mul(imag, imag, t3, GMP_RNDN);

		mpfr_clear(t2);
		mpfr_clear(t3);
		mpfr_clear(t4);
	      }
	  }

	mpfr_clear(denom);
	mpfr_clear(rr);
	mpfr_clear(ri);
	mpfr_clear(t);
	mpfr_clear(rra);
	mpfr_clear(ria);
      }
      break;
    case OPERATOR_MOD:
      return false;
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      return false;
    default:
      go_unreachable();
    }

  Type* type = left_type;
  if (type == NULL)
    type = right_type;
  else if (type != right_type && right_type != NULL)
    {
      if (type->is_abstract())
	type = right_type;
      else if (!right_type->is_abstract())
	{
	  // This looks like a type error which should be diagnosed
	  // elsewhere.  Don't do anything here, to avoid an unhelpful
	  // chain of error messages.
	  return true;
	}
    }

  if (type != NULL && !type->is_abstract())
    {
      if ((type != left_type
	   && !Complex_expression::check_constant(left_real, left_imag,
						  type, location))
	  || (type != right_type
	      && !Complex_expression::check_constant(right_real, right_imag,
						     type, location))
	  || !Complex_expression::check_constant(real, imag, type,
						 location))
	{
	  mpfr_set_ui(real, 0, GMP_RNDN);
	  mpfr_set_ui(imag, 0, GMP_RNDN);
	}
    }

  return true;
}

// Lower a binary expression.  We have to evaluate constant
// expressions now, in order to implement Go's unlimited precision
// constants.

Expression*
Binary_expression::do_lower(Gogo*, Named_object*, Statement_inserter*, int)
{
  Location location = this->location();
  Operator op = this->op_;
  Expression* left = this->left_;
  Expression* right = this->right_;

  const bool is_comparison = (op == OPERATOR_EQEQ
			      || op == OPERATOR_NOTEQ
			      || op == OPERATOR_LT
			      || op == OPERATOR_LE
			      || op == OPERATOR_GT
			      || op == OPERATOR_GE);

  // Integer constant expressions.
  {
    mpz_t left_val;
    mpz_init(left_val);
    Type* left_type;
    mpz_t right_val;
    mpz_init(right_val);
    Type* right_type;
    if (left->integer_constant_value(false, left_val, &left_type)
	&& right->integer_constant_value(false, right_val, &right_type))
      {
	Expression* ret = NULL;
	if (left_type != right_type
	    && left_type != NULL
	    && right_type != NULL
	    && left_type->base() != right_type->base()
	    && op != OPERATOR_LSHIFT
	    && op != OPERATOR_RSHIFT)
	  {
	    // May be a type error--let it be diagnosed later.
	  }
	else if (is_comparison)
	  {
	    bool b = Binary_expression::compare_integer(op, left_val,
							right_val);
	    ret = Expression::make_cast(Type::lookup_bool_type(),
					Expression::make_boolean(b, location),
					location);
	  }
	else
	  {
	    mpz_t val;
	    mpz_init(val);

	    if (Binary_expression::eval_integer(op, left_type, left_val,
						right_type, right_val,
						location, val))
	      {
		go_assert(op != OPERATOR_OROR && op != OPERATOR_ANDAND);
		Type* type;
		if (op == OPERATOR_LSHIFT || op == OPERATOR_RSHIFT)
		  type = left_type;
		else if (left_type == NULL)
		  type = right_type;
		else if (right_type == NULL)
		  type = left_type;
		else if (!left_type->is_abstract()
			 && left_type->named_type() != NULL)
		  type = left_type;
		else if (!right_type->is_abstract()
			 && right_type->named_type() != NULL)
		  type = right_type;
		else if (!left_type->is_abstract())
		  type = left_type;
		else if (!right_type->is_abstract())
		  type = right_type;
		else if (left_type->float_type() != NULL)
		  type = left_type;
		else if (right_type->float_type() != NULL)
		  type = right_type;
		else if (left_type->complex_type() != NULL)
		  type = left_type;
		else if (right_type->complex_type() != NULL)
		  type = right_type;
		else
		  type = left_type;
		ret = Expression::make_integer(&val, type, location);
	      }

	    mpz_clear(val);
	  }

	if (ret != NULL)
	  {
	    mpz_clear(right_val);
	    mpz_clear(left_val);
	    return ret;
	  }
      }
    mpz_clear(right_val);
    mpz_clear(left_val);
  }

  // Floating point constant expressions.
  {
    mpfr_t left_val;
    mpfr_init(left_val);
    Type* left_type;
    mpfr_t right_val;
    mpfr_init(right_val);
    Type* right_type;
    if (left->float_constant_value(left_val, &left_type)
	&& right->float_constant_value(right_val, &right_type))
      {
	Expression* ret = NULL;
	if (left_type != right_type
	    && left_type != NULL
	    && right_type != NULL
	    && left_type->base() != right_type->base()
	    && op != OPERATOR_LSHIFT
	    && op != OPERATOR_RSHIFT)
	  {
	    // May be a type error--let it be diagnosed later.
	  }
	else if (is_comparison)
	  {
	    bool b = Binary_expression::compare_float(op,
						      (left_type != NULL
						       ? left_type
						       : right_type),
						      left_val, right_val);
	    ret = Expression::make_boolean(b, location);
	  }
	else
	  {
	    mpfr_t val;
	    mpfr_init(val);

	    if (Binary_expression::eval_float(op, left_type, left_val,
					      right_type, right_val, val,
					      location))
	      {
		go_assert(op != OPERATOR_OROR && op != OPERATOR_ANDAND
			   && op != OPERATOR_LSHIFT && op != OPERATOR_RSHIFT);
		Type* type;
		if (left_type == NULL)
		  type = right_type;
		else if (right_type == NULL)
		  type = left_type;
		else if (!left_type->is_abstract()
			 && left_type->named_type() != NULL)
		  type = left_type;
		else if (!right_type->is_abstract()
			 && right_type->named_type() != NULL)
		  type = right_type;
		else if (!left_type->is_abstract())
		  type = left_type;
		else if (!right_type->is_abstract())
		  type = right_type;
		else if (left_type->float_type() != NULL)
		  type = left_type;
		else if (right_type->float_type() != NULL)
		  type = right_type;
		else
		  type = left_type;
		ret = Expression::make_float(&val, type, location);
	      }

	    mpfr_clear(val);
	  }

	if (ret != NULL)
	  {
	    mpfr_clear(right_val);
	    mpfr_clear(left_val);
	    return ret;
	  }
      }
    mpfr_clear(right_val);
    mpfr_clear(left_val);
  }

  // Complex constant expressions.
  {
    mpfr_t left_real;
    mpfr_t left_imag;
    mpfr_init(left_real);
    mpfr_init(left_imag);
    Type* left_type;

    mpfr_t right_real;
    mpfr_t right_imag;
    mpfr_init(right_real);
    mpfr_init(right_imag);
    Type* right_type;

    if (left->complex_constant_value(left_real, left_imag, &left_type)
	&& right->complex_constant_value(right_real, right_imag, &right_type))
      {
	Expression* ret = NULL;
	if (left_type != right_type
	    && left_type != NULL
	    && right_type != NULL
	    && left_type->base() != right_type->base())
	  {
	    // May be a type error--let it be diagnosed later.
	  }
	else if (op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ)
	  {
	    bool b = Binary_expression::compare_complex(op,
							(left_type != NULL
							 ? left_type
							 : right_type),
							left_real,
							left_imag,
							right_real,
							right_imag);
	    ret = Expression::make_boolean(b, location);
	  }
	else
	  {
	    mpfr_t real;
	    mpfr_t imag;
	    mpfr_init(real);
	    mpfr_init(imag);

	    if (Binary_expression::eval_complex(op, left_type,
						left_real, left_imag,
						right_type,
						right_real, right_imag,
						real, imag,
						location))
	      {
		go_assert(op != OPERATOR_OROR && op != OPERATOR_ANDAND
			   && op != OPERATOR_LSHIFT && op != OPERATOR_RSHIFT);
		Type* type;
		if (left_type == NULL)
		  type = right_type;
		else if (right_type == NULL)
		  type = left_type;
		else if (!left_type->is_abstract()
			 && left_type->named_type() != NULL)
		  type = left_type;
		else if (!right_type->is_abstract()
			 && right_type->named_type() != NULL)
		  type = right_type;
		else if (!left_type->is_abstract())
		  type = left_type;
		else if (!right_type->is_abstract())
		  type = right_type;
		else if (left_type->complex_type() != NULL)
		  type = left_type;
		else if (right_type->complex_type() != NULL)
		  type = right_type;
		else
		  type = left_type;
		ret = Expression::make_complex(&real, &imag, type,
					       location);
	      }
	    mpfr_clear(real);
	    mpfr_clear(imag);
	  }

	if (ret != NULL)
	  {
	    mpfr_clear(left_real);
	    mpfr_clear(left_imag);
	    mpfr_clear(right_real);
	    mpfr_clear(right_imag);
	    return ret;
	  }
      }

    mpfr_clear(left_real);
    mpfr_clear(left_imag);
    mpfr_clear(right_real);
    mpfr_clear(right_imag);
  }

  // String constant expressions.
  if (op == OPERATOR_PLUS
      && left->type()->is_string_type()
      && right->type()->is_string_type())
    {
      std::string left_string;
      std::string right_string;
      if (left->string_constant_value(&left_string)
	  && right->string_constant_value(&right_string))
	return Expression::make_string(left_string + right_string, location);
    }

  // Special case for shift of a floating point constant.
  if (op == OPERATOR_LSHIFT || op == OPERATOR_RSHIFT)
    {
      mpfr_t left_val;
      mpfr_init(left_val);
      Type* left_type;
      mpz_t right_val;
      mpz_init(right_val);
      Type* right_type;
      if (left->float_constant_value(left_val, &left_type)
	  && right->integer_constant_value(false, right_val, &right_type)
	  && mpfr_integer_p(left_val)
	  && (left_type == NULL
	      || left_type->is_abstract()
	      || left_type->integer_type() != NULL))
	{
	  mpz_t left_int;
	  mpz_init(left_int);
	  mpfr_get_z(left_int, left_val, GMP_RNDN);

	  mpz_t val;
	  mpz_init(val);

	  Expression* ret = NULL;
	  if (Binary_expression::eval_integer(op, left_type, left_int,
					      right_type, right_val,
					      location, val))
	    ret = Expression::make_integer(&val, left_type, location);

	  mpz_clear(left_int);
	  mpz_clear(val);

	  if (ret != NULL)
	    {
	      mpfr_clear(left_val);
	      mpz_clear(right_val);
	      return ret;
	    }
	}

      mpfr_clear(left_val);
      mpz_clear(right_val);
    }

  return this;
}

// Return the integer constant value, if it has one.

bool
Binary_expression::do_integer_constant_value(bool iota_is_constant, mpz_t val,
					     Type** ptype) const
{
  mpz_t left_val;
  mpz_init(left_val);
  Type* left_type;
  if (!this->left_->integer_constant_value(iota_is_constant, left_val,
					   &left_type))
    {
      mpz_clear(left_val);
      return false;
    }

  mpz_t right_val;
  mpz_init(right_val);
  Type* right_type;
  if (!this->right_->integer_constant_value(iota_is_constant, right_val,
					    &right_type))
    {
      mpz_clear(right_val);
      mpz_clear(left_val);
      return false;
    }

  bool ret;
  if (left_type != right_type
      && left_type != NULL
      && right_type != NULL
      && left_type->base() != right_type->base()
      && this->op_ != OPERATOR_RSHIFT
      && this->op_ != OPERATOR_LSHIFT)
    ret = false;
  else
    ret = Binary_expression::eval_integer(this->op_, left_type, left_val,
					  right_type, right_val,
					  this->location(), val);

  mpz_clear(right_val);
  mpz_clear(left_val);

  if (ret)
    *ptype = left_type;

  return ret;
}

// Return the floating point constant value, if it has one.

bool
Binary_expression::do_float_constant_value(mpfr_t val, Type** ptype) const
{
  mpfr_t left_val;
  mpfr_init(left_val);
  Type* left_type;
  if (!this->left_->float_constant_value(left_val, &left_type))
    {
      mpfr_clear(left_val);
      return false;
    }

  mpfr_t right_val;
  mpfr_init(right_val);
  Type* right_type;
  if (!this->right_->float_constant_value(right_val, &right_type))
    {
      mpfr_clear(right_val);
      mpfr_clear(left_val);
      return false;
    }

  bool ret;
  if (left_type != right_type
      && left_type != NULL
      && right_type != NULL
      && left_type->base() != right_type->base())
    ret = false;
  else
    ret = Binary_expression::eval_float(this->op_, left_type, left_val,
					right_type, right_val,
					val, this->location());

  mpfr_clear(left_val);
  mpfr_clear(right_val);

  if (ret)
    *ptype = left_type;

  return ret;
}

// Return the complex constant value, if it has one.

bool
Binary_expression::do_complex_constant_value(mpfr_t real, mpfr_t imag,
					     Type** ptype) const
{
  mpfr_t left_real;
  mpfr_t left_imag;
  mpfr_init(left_real);
  mpfr_init(left_imag);
  Type* left_type;
  if (!this->left_->complex_constant_value(left_real, left_imag, &left_type))
    {
      mpfr_clear(left_real);
      mpfr_clear(left_imag);
      return false;
    }

  mpfr_t right_real;
  mpfr_t right_imag;
  mpfr_init(right_real);
  mpfr_init(right_imag);
  Type* right_type;
  if (!this->right_->complex_constant_value(right_real, right_imag,
					    &right_type))
    {
      mpfr_clear(left_real);
      mpfr_clear(left_imag);
      mpfr_clear(right_real);
      mpfr_clear(right_imag);
      return false;
    }

  bool ret;
  if (left_type != right_type
      && left_type != NULL
      && right_type != NULL
      && left_type->base() != right_type->base())
    ret = false;
  else
    ret = Binary_expression::eval_complex(this->op_, left_type,
					  left_real, left_imag,
					  right_type,
					  right_real, right_imag,
					  real, imag,
					  this->location());
  mpfr_clear(left_real);
  mpfr_clear(left_imag);
  mpfr_clear(right_real);
  mpfr_clear(right_imag);

  if (ret)
    *ptype = left_type;

  return ret;
}

// Note that the value is being discarded.

void
Binary_expression::do_discarding_value()
{
  if (this->op_ == OPERATOR_OROR || this->op_ == OPERATOR_ANDAND)
    this->right_->discarding_value();
  else
    this->unused_value_error();
}

// Get type.

Type*
Binary_expression::do_type()
{
  if (this->classification() == EXPRESSION_ERROR)
    return Type::make_error_type();

  switch (this->op_)
    {
    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      return Type::lookup_bool_type();

    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_MULT:
    case OPERATOR_DIV:
    case OPERATOR_MOD:
    case OPERATOR_AND:
    case OPERATOR_BITCLEAR:
      {
	Type* left_type = this->left_->type();
	Type* right_type = this->right_->type();
	if (left_type->is_error())
	  return left_type;
	else if (right_type->is_error())
	  return right_type;
	else if (!Type::are_compatible_for_binop(left_type, right_type))
	  {
	    this->report_error(_("incompatible types in binary expression"));
	    return Type::make_error_type();
	  }
	else if (!left_type->is_abstract() && left_type->named_type() != NULL)
	  return left_type;
	else if (!right_type->is_abstract() && right_type->named_type() != NULL)
	  return right_type;
	else if (!left_type->is_abstract())
	  return left_type;
	else if (!right_type->is_abstract())
	  return right_type;
	else if (left_type->complex_type() != NULL)
	  return left_type;
	else if (right_type->complex_type() != NULL)
	  return right_type;
	else if (left_type->float_type() != NULL)
	  return left_type;
	else if (right_type->float_type() != NULL)
	  return right_type;
	else
	  return left_type;
      }

    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      return this->left_->type();

    default:
      go_unreachable();
    }
}

// Set type for a binary expression.

void
Binary_expression::do_determine_type(const Type_context* context)
{
  Type* tleft = this->left_->type();
  Type* tright = this->right_->type();

  // Both sides should have the same type, except for the shift
  // operations.  For a comparison, we should ignore the incoming
  // type.

  bool is_shift_op = (this->op_ == OPERATOR_LSHIFT
		      || this->op_ == OPERATOR_RSHIFT);

  bool is_comparison = (this->op_ == OPERATOR_EQEQ
			|| this->op_ == OPERATOR_NOTEQ
			|| this->op_ == OPERATOR_LT
			|| this->op_ == OPERATOR_LE
			|| this->op_ == OPERATOR_GT
			|| this->op_ == OPERATOR_GE);

  Type_context subcontext(*context);

  if (is_comparison)
    {
      // In a comparison, the context does not determine the types of
      // the operands.
      subcontext.type = NULL;
    }

  // Set the context for the left hand operand.
  if (is_shift_op)
    {
      // The right hand operand of a shift plays no role in
      // determining the type of the left hand operand.
    }
  else if (!tleft->is_abstract())
    subcontext.type = tleft;
  else if (!tright->is_abstract())
    subcontext.type = tright;
  else if (subcontext.type == NULL)
    {
      if ((tleft->integer_type() != NULL && tright->integer_type() != NULL)
	  || (tleft->float_type() != NULL && tright->float_type() != NULL)
	  || (tleft->complex_type() != NULL && tright->complex_type() != NULL))
	{
	  // Both sides have an abstract integer, abstract float, or
	  // abstract complex type.  Just let CONTEXT determine
	  // whether they may remain abstract or not.
	}
      else if (tleft->complex_type() != NULL)
	subcontext.type = tleft;
      else if (tright->complex_type() != NULL)
	subcontext.type = tright;
      else if (tleft->float_type() != NULL)
	subcontext.type = tleft;
      else if (tright->float_type() != NULL)
	subcontext.type = tright;
      else
	subcontext.type = tleft;

      if (subcontext.type != NULL && !context->may_be_abstract)
	subcontext.type = subcontext.type->make_non_abstract_type();
    }

  this->left_->determine_type(&subcontext);

  if (is_shift_op)
    {
      // We may have inherited an unusable type for the shift operand.
      // Give a useful error if that happened.
      if (tleft->is_abstract()
	  && subcontext.type != NULL
	  && (this->left_->type()->integer_type() == NULL
	      || (subcontext.type->integer_type() == NULL
		  && subcontext.type->float_type() == NULL
		  && subcontext.type->complex_type() == NULL)))
	this->report_error(("invalid context-determined non-integer type "
			    "for shift operand"));

      // The context for the right hand operand is the same as for the
      // left hand operand, except for a shift operator.
      subcontext.type = Type::lookup_integer_type("uint");
      subcontext.may_be_abstract = false;
    }

  this->right_->determine_type(&subcontext);
}

// Report an error if the binary operator OP does not support TYPE.
// OTYPE is the type of the other operand.  Return whether the
// operation is OK.  This should not be used for shift.

bool
Binary_expression::check_operator_type(Operator op, Type* type, Type* otype,
				       Location location)
{
  switch (op)
    {
    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
      if (!type->is_boolean_type())
	{
	  error_at(location, "expected boolean type");
	  return false;
	}
      break;

    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
      if (type->integer_type() == NULL
	  && type->float_type() == NULL
	  && type->complex_type() == NULL
	  && !type->is_string_type()
	  && type->points_to() == NULL
	  && !type->is_nil_type()
	  && !type->is_boolean_type()
	  && type->interface_type() == NULL
	  && (type->array_type() == NULL
	      || type->array_type()->length() != NULL)
	  && type->map_type() == NULL
	  && type->channel_type() == NULL
	  && type->function_type() == NULL)
	{
	  error_at(location,
		   ("expected integer, floating, complex, string, pointer, "
		    "boolean, interface, slice, map, channel, "
		    "or function type"));
	  return false;
	}
      if ((type->is_slice_type()
	   || type->map_type() != NULL
	   || type->function_type() != NULL)
	  && !otype->is_nil_type())
	{
	  error_at(location,
		   ("slice, map, and function types may only "
		    "be compared to nil"));
	  return false;
	}
      break;

    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      if (type->integer_type() == NULL
	  && type->float_type() == NULL
	  && !type->is_string_type())
	{
	  error_at(location, "expected integer, floating, or string type");
	  return false;
	}
      break;

    case OPERATOR_PLUS:
    case OPERATOR_PLUSEQ:
      if (type->integer_type() == NULL
	  && type->float_type() == NULL
	  && type->complex_type() == NULL
	  && !type->is_string_type())
	{
	  error_at(location,
		   "expected integer, floating, complex, or string type");
	  return false;
	}
      break;

    case OPERATOR_MINUS:
    case OPERATOR_MINUSEQ:
    case OPERATOR_MULT:
    case OPERATOR_MULTEQ:
    case OPERATOR_DIV:
    case OPERATOR_DIVEQ:
      if (type->integer_type() == NULL
	  && type->float_type() == NULL
	  && type->complex_type() == NULL)
	{
	  error_at(location, "expected integer, floating, or complex type");
	  return false;
	}
      break;

    case OPERATOR_MOD:
    case OPERATOR_MODEQ:
    case OPERATOR_OR:
    case OPERATOR_OREQ:
    case OPERATOR_AND:
    case OPERATOR_ANDEQ:
    case OPERATOR_XOR:
    case OPERATOR_XOREQ:
    case OPERATOR_BITCLEAR:
    case OPERATOR_BITCLEAREQ:
      if (type->integer_type() == NULL)
	{
	  error_at(location, "expected integer type");
	  return false;
	}
      break;

    default:
      go_unreachable();
    }

  return true;
}

// Check types.

void
Binary_expression::do_check_types(Gogo*)
{
  if (this->classification() == EXPRESSION_ERROR)
    return;

  Type* left_type = this->left_->type();
  Type* right_type = this->right_->type();
  if (left_type->is_error() || right_type->is_error())
    {
      this->set_is_error();
      return;
    }

  if (this->op_ == OPERATOR_EQEQ
      || this->op_ == OPERATOR_NOTEQ
      || this->op_ == OPERATOR_LT
      || this->op_ == OPERATOR_LE
      || this->op_ == OPERATOR_GT
      || this->op_ == OPERATOR_GE)
    {
      if (!Type::are_assignable(left_type, right_type, NULL)
	  && !Type::are_assignable(right_type, left_type, NULL))
	{
	  this->report_error(_("incompatible types in binary expression"));
	  return;
	}
      if (!Binary_expression::check_operator_type(this->op_, left_type,
						  right_type,
						  this->location())
	  || !Binary_expression::check_operator_type(this->op_, right_type,
						     left_type,
						     this->location()))
	{
	  this->set_is_error();
	  return;
	}
    }
  else if (this->op_ != OPERATOR_LSHIFT && this->op_ != OPERATOR_RSHIFT)
    {
      if (!Type::are_compatible_for_binop(left_type, right_type))
	{
	  this->report_error(_("incompatible types in binary expression"));
	  return;
	}
      if (!Binary_expression::check_operator_type(this->op_, left_type,
						  right_type,
						  this->location()))
	{
	  this->set_is_error();
	  return;
	}
    }
  else
    {
      if (left_type->integer_type() == NULL)
	this->report_error(_("shift of non-integer operand"));

      if (!right_type->is_abstract()
	  && (right_type->integer_type() == NULL
	      || !right_type->integer_type()->is_unsigned()))
	this->report_error(_("shift count not unsigned integer"));
      else
	{
	  mpz_t val;
	  mpz_init(val);
	  Type* type;
	  if (this->right_->integer_constant_value(true, val, &type))
	    {
	      if (mpz_sgn(val) < 0)
		{
		  this->report_error(_("negative shift count"));
		  mpz_set_ui(val, 0);
		  Location rloc = this->right_->location();
		  this->right_ = Expression::make_integer(&val, right_type,
							  rloc);
		}
	    }
	  mpz_clear(val);
	}
    }
}

// Get a tree for a binary expression.

tree
Binary_expression::do_get_tree(Translate_context* context)
{
  tree left = this->left_->get_tree(context);
  tree right = this->right_->get_tree(context);

  if (left == error_mark_node || right == error_mark_node)
    return error_mark_node;

  enum tree_code code;
  bool use_left_type = true;
  bool is_shift_op = false;
  switch (this->op_)
    {
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      return Expression::comparison_tree(context, this->op_,
					 this->left_->type(), left,
					 this->right_->type(), right,
					 this->location());

    case OPERATOR_OROR:
      code = TRUTH_ORIF_EXPR;
      use_left_type = false;
      break;
    case OPERATOR_ANDAND:
      code = TRUTH_ANDIF_EXPR;
      use_left_type = false;
      break;
    case OPERATOR_PLUS:
      code = PLUS_EXPR;
      break;
    case OPERATOR_MINUS:
      code = MINUS_EXPR;
      break;
    case OPERATOR_OR:
      code = BIT_IOR_EXPR;
      break;
    case OPERATOR_XOR:
      code = BIT_XOR_EXPR;
      break;
    case OPERATOR_MULT:
      code = MULT_EXPR;
      break;
    case OPERATOR_DIV:
      {
	Type *t = this->left_->type();
	if (t->float_type() != NULL || t->complex_type() != NULL)
	  code = RDIV_EXPR;
	else
	  code = TRUNC_DIV_EXPR;
      }
      break;
    case OPERATOR_MOD:
      code = TRUNC_MOD_EXPR;
      break;
    case OPERATOR_LSHIFT:
      code = LSHIFT_EXPR;
      is_shift_op = true;
      break;
    case OPERATOR_RSHIFT:
      code = RSHIFT_EXPR;
      is_shift_op = true;
      break;
    case OPERATOR_AND:
      code = BIT_AND_EXPR;
      break;
    case OPERATOR_BITCLEAR:
      right = fold_build1(BIT_NOT_EXPR, TREE_TYPE(right), right);
      code = BIT_AND_EXPR;
      break;
    default:
      go_unreachable();
    }

  tree type = use_left_type ? TREE_TYPE(left) : TREE_TYPE(right);

  if (this->left_->type()->is_string_type())
    {
      go_assert(this->op_ == OPERATOR_PLUS);
      Type* st = Type::make_string_type();
      tree string_type = type_to_tree(st->get_backend(context->gogo()));
      static tree string_plus_decl;
      return Gogo::call_builtin(&string_plus_decl,
				this->location(),
				"__go_string_plus",
				2,
				string_type,
				string_type,
				left,
				string_type,
				right);
    }

  tree compute_type = excess_precision_type(type);
  if (compute_type != NULL_TREE)
    {
      left = ::convert(compute_type, left);
      right = ::convert(compute_type, right);
    }

  tree eval_saved = NULL_TREE;
  if (is_shift_op)
    {
      // Make sure the values are evaluated.
      if (!DECL_P(left) && TREE_SIDE_EFFECTS(left))
	{
	  left = save_expr(left);
	  eval_saved = left;
	}
      if (!DECL_P(right) && TREE_SIDE_EFFECTS(right))
	{
	  right = save_expr(right);
	  if (eval_saved == NULL_TREE)
	    eval_saved = right;
	  else
	    eval_saved = fold_build2_loc(this->location().gcc_location(),
                                         COMPOUND_EXPR,
					 void_type_node, eval_saved, right);
	}
    }

  tree ret = fold_build2_loc(this->location().gcc_location(),
			     code,
			     compute_type != NULL_TREE ? compute_type : type,
			     left, right);

  if (compute_type != NULL_TREE)
    ret = ::convert(type, ret);

  // In Go, a shift larger than the size of the type is well-defined.
  // This is not true in GENERIC, so we need to insert a conditional.
  if (is_shift_op)
    {
      go_assert(INTEGRAL_TYPE_P(TREE_TYPE(left)));
      go_assert(this->left_->type()->integer_type() != NULL);
      int bits = TYPE_PRECISION(TREE_TYPE(left));

      tree compare = fold_build2(LT_EXPR, boolean_type_node, right,
				 build_int_cst_type(TREE_TYPE(right), bits));

      tree overflow_result = fold_convert_loc(this->location().gcc_location(),
					      TREE_TYPE(left),
					      integer_zero_node);
      if (this->op_ == OPERATOR_RSHIFT
	  && !this->left_->type()->integer_type()->is_unsigned())
	{
	  tree neg =
            fold_build2_loc(this->location().gcc_location(), LT_EXPR,
                            boolean_type_node, left,
                            fold_convert_loc(this->location().gcc_location(),
                                             TREE_TYPE(left),
                                             integer_zero_node));
	  tree neg_one =
            fold_build2_loc(this->location().gcc_location(),
                            MINUS_EXPR, TREE_TYPE(left),
                            fold_convert_loc(this->location().gcc_location(),
                                             TREE_TYPE(left),
                                             integer_zero_node),
                            fold_convert_loc(this->location().gcc_location(),
                                             TREE_TYPE(left),
                                             integer_one_node));
	  overflow_result =
            fold_build3_loc(this->location().gcc_location(), COND_EXPR,
                            TREE_TYPE(left), neg, neg_one,
                            overflow_result);
	}

      ret = fold_build3_loc(this->location().gcc_location(), COND_EXPR,
                            TREE_TYPE(left), compare, ret, overflow_result);

      if (eval_saved != NULL_TREE)
	ret = fold_build2_loc(this->location().gcc_location(), COMPOUND_EXPR,
			      TREE_TYPE(ret), eval_saved, ret);
    }

  return ret;
}

// Export a binary expression.

void
Binary_expression::do_export(Export* exp) const
{
  exp->write_c_string("(");
  this->left_->export_expression(exp);
  switch (this->op_)
    {
    case OPERATOR_OROR:
      exp->write_c_string(" || ");
      break;
    case OPERATOR_ANDAND:
      exp->write_c_string(" && ");
      break;
    case OPERATOR_EQEQ:
      exp->write_c_string(" == ");
      break;
    case OPERATOR_NOTEQ:
      exp->write_c_string(" != ");
      break;
    case OPERATOR_LT:
      exp->write_c_string(" < ");
      break;
    case OPERATOR_LE:
      exp->write_c_string(" <= ");
      break;
    case OPERATOR_GT:
      exp->write_c_string(" > ");
      break;
    case OPERATOR_GE:
      exp->write_c_string(" >= ");
      break;
    case OPERATOR_PLUS:
      exp->write_c_string(" + ");
      break;
    case OPERATOR_MINUS:
      exp->write_c_string(" - ");
      break;
    case OPERATOR_OR:
      exp->write_c_string(" | ");
      break;
    case OPERATOR_XOR:
      exp->write_c_string(" ^ ");
      break;
    case OPERATOR_MULT:
      exp->write_c_string(" * ");
      break;
    case OPERATOR_DIV:
      exp->write_c_string(" / ");
      break;
    case OPERATOR_MOD:
      exp->write_c_string(" % ");
      break;
    case OPERATOR_LSHIFT:
      exp->write_c_string(" << ");
      break;
    case OPERATOR_RSHIFT:
      exp->write_c_string(" >> ");
      break;
    case OPERATOR_AND:
      exp->write_c_string(" & ");
      break;
    case OPERATOR_BITCLEAR:
      exp->write_c_string(" &^ ");
      break;
    default:
      go_unreachable();
    }
  this->right_->export_expression(exp);
  exp->write_c_string(")");
}

// Import a binary expression.

Expression*
Binary_expression::do_import(Import* imp)
{
  imp->require_c_string("(");

  Expression* left = Expression::import_expression(imp);

  Operator op;
  if (imp->match_c_string(" || "))
    {
      op = OPERATOR_OROR;
      imp->advance(4);
    }
  else if (imp->match_c_string(" && "))
    {
      op = OPERATOR_ANDAND;
      imp->advance(4);
    }
  else if (imp->match_c_string(" == "))
    {
      op = OPERATOR_EQEQ;
      imp->advance(4);
    }
  else if (imp->match_c_string(" != "))
    {
      op = OPERATOR_NOTEQ;
      imp->advance(4);
    }
  else if (imp->match_c_string(" < "))
    {
      op = OPERATOR_LT;
      imp->advance(3);
    }
  else if (imp->match_c_string(" <= "))
    {
      op = OPERATOR_LE;
      imp->advance(4);
    }
  else if (imp->match_c_string(" > "))
    {
      op = OPERATOR_GT;
      imp->advance(3);
    }
  else if (imp->match_c_string(" >= "))
    {
      op = OPERATOR_GE;
      imp->advance(4);
    }
  else if (imp->match_c_string(" + "))
    {
      op = OPERATOR_PLUS;
      imp->advance(3);
    }
  else if (imp->match_c_string(" - "))
    {
      op = OPERATOR_MINUS;
      imp->advance(3);
    }
  else if (imp->match_c_string(" | "))
    {
      op = OPERATOR_OR;
      imp->advance(3);
    }
  else if (imp->match_c_string(" ^ "))
    {
      op = OPERATOR_XOR;
      imp->advance(3);
    }
  else if (imp->match_c_string(" * "))
    {
      op = OPERATOR_MULT;
      imp->advance(3);
    }
  else if (imp->match_c_string(" / "))
    {
      op = OPERATOR_DIV;
      imp->advance(3);
    }
  else if (imp->match_c_string(" % "))
    {
      op = OPERATOR_MOD;
      imp->advance(3);
    }
  else if (imp->match_c_string(" << "))
    {
      op = OPERATOR_LSHIFT;
      imp->advance(4);
    }
  else if (imp->match_c_string(" >> "))
    {
      op = OPERATOR_RSHIFT;
      imp->advance(4);
    }
  else if (imp->match_c_string(" & "))
    {
      op = OPERATOR_AND;
      imp->advance(3);
    }
  else if (imp->match_c_string(" &^ "))
    {
      op = OPERATOR_BITCLEAR;
      imp->advance(4);
    }
  else
    {
      error_at(imp->location(), "unrecognized binary operator");
      return Expression::make_error(imp->location());
    }

  Expression* right = Expression::import_expression(imp);

  imp->require_c_string(")");

  return Expression::make_binary(op, left, right, imp->location());
}

// Dump ast representation of a binary expression.

void
Binary_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->left_);
  ast_dump_context->ostream() << " ";
  ast_dump_context->dump_operator(this->op_);
  ast_dump_context->ostream() << " ";
  ast_dump_context->dump_expression(this->right_);
  ast_dump_context->ostream() << ") ";
}

// Make a binary expression.

Expression*
Expression::make_binary(Operator op, Expression* left, Expression* right,
			Location location)
{
  return new Binary_expression(op, left, right, location);
}

// Implement a comparison.

tree
Expression::comparison_tree(Translate_context* context, Operator op,
			    Type* left_type, tree left_tree,
			    Type* right_type, tree right_tree,
			    Location location)
{
  enum tree_code code;
  switch (op)
    {
    case OPERATOR_EQEQ:
      code = EQ_EXPR;
      break;
    case OPERATOR_NOTEQ:
      code = NE_EXPR;
      break;
    case OPERATOR_LT:
      code = LT_EXPR;
      break;
    case OPERATOR_LE:
      code = LE_EXPR;
      break;
    case OPERATOR_GT:
      code = GT_EXPR;
      break;
    case OPERATOR_GE:
      code = GE_EXPR;
      break;
    default:
      go_unreachable();
    }

  if (left_type->is_string_type() && right_type->is_string_type())
    {
      Type* st = Type::make_string_type();
      tree string_type = type_to_tree(st->get_backend(context->gogo()));
      static tree string_compare_decl;
      left_tree = Gogo::call_builtin(&string_compare_decl,
				     location,
				     "__go_strcmp",
				     2,
				     integer_type_node,
				     string_type,
				     left_tree,
				     string_type,
				     right_tree);
      right_tree = build_int_cst_type(integer_type_node, 0);
    }
  else if ((left_type->interface_type() != NULL
	    && right_type->interface_type() == NULL
	    && !right_type->is_nil_type())
	   || (left_type->interface_type() == NULL
	       && !left_type->is_nil_type()
	       && right_type->interface_type() != NULL))
    {
      // Comparing an interface value to a non-interface value.
      if (left_type->interface_type() == NULL)
	{
	  std::swap(left_type, right_type);
	  std::swap(left_tree, right_tree);
	}

      // The right operand is not an interface.  We need to take its
      // address if it is not a pointer.
      tree make_tmp;
      tree arg;
      if (right_type->points_to() != NULL)
	{
	  make_tmp = NULL_TREE;
	  arg = right_tree;
	}
      else if (TREE_ADDRESSABLE(TREE_TYPE(right_tree)) || DECL_P(right_tree))
	{
	  make_tmp = NULL_TREE;
	  arg = build_fold_addr_expr_loc(location.gcc_location(), right_tree);
	  if (DECL_P(right_tree))
	    TREE_ADDRESSABLE(right_tree) = 1;
	}
      else
	{
	  tree tmp = create_tmp_var(TREE_TYPE(right_tree),
				    get_name(right_tree));
	  DECL_IGNORED_P(tmp) = 0;
	  DECL_INITIAL(tmp) = right_tree;
	  TREE_ADDRESSABLE(tmp) = 1;
	  make_tmp = build1(DECL_EXPR, void_type_node, tmp);
	  SET_EXPR_LOCATION(make_tmp, location.gcc_location());
	  arg = build_fold_addr_expr_loc(location.gcc_location(), tmp);
	}
      arg = fold_convert_loc(location.gcc_location(), ptr_type_node, arg);

      tree descriptor = right_type->type_descriptor_pointer(context->gogo(),
							    location);

      if (left_type->interface_type()->is_empty())
	{
	  static tree empty_interface_value_compare_decl;
	  left_tree = Gogo::call_builtin(&empty_interface_value_compare_decl,
					 location,
					 "__go_empty_interface_value_compare",
					 3,
					 integer_type_node,
					 TREE_TYPE(left_tree),
					 left_tree,
					 TREE_TYPE(descriptor),
					 descriptor,
					 ptr_type_node,
					 arg);
	  if (left_tree == error_mark_node)
	    return error_mark_node;
	  // This can panic if the type is not comparable.
	  TREE_NOTHROW(empty_interface_value_compare_decl) = 0;
	}
      else
	{
	  static tree interface_value_compare_decl;
	  left_tree = Gogo::call_builtin(&interface_value_compare_decl,
					 location,
					 "__go_interface_value_compare",
					 3,
					 integer_type_node,
					 TREE_TYPE(left_tree),
					 left_tree,
					 TREE_TYPE(descriptor),
					 descriptor,
					 ptr_type_node,
					 arg);
	  if (left_tree == error_mark_node)
	    return error_mark_node;
	  // This can panic if the type is not comparable.
	  TREE_NOTHROW(interface_value_compare_decl) = 0;
	}
      right_tree = build_int_cst_type(integer_type_node, 0);

      if (make_tmp != NULL_TREE)
	left_tree = build2(COMPOUND_EXPR, TREE_TYPE(left_tree), make_tmp,
			   left_tree);
    }
  else if (left_type->interface_type() != NULL
	   && right_type->interface_type() != NULL)
    {
      if (left_type->interface_type()->is_empty()
	  && right_type->interface_type()->is_empty())
	{
	  static tree empty_interface_compare_decl;
	  left_tree = Gogo::call_builtin(&empty_interface_compare_decl,
					 location,
					 "__go_empty_interface_compare",
					 2,
					 integer_type_node,
					 TREE_TYPE(left_tree),
					 left_tree,
					 TREE_TYPE(right_tree),
					 right_tree);
	  if (left_tree == error_mark_node)
	    return error_mark_node;
	  // This can panic if the type is uncomparable.
	  TREE_NOTHROW(empty_interface_compare_decl) = 0;
	}
      else if (!left_type->interface_type()->is_empty()
	       && !right_type->interface_type()->is_empty())
	{
	  static tree interface_compare_decl;
	  left_tree = Gogo::call_builtin(&interface_compare_decl,
					 location,
					 "__go_interface_compare",
					 2,
					 integer_type_node,
					 TREE_TYPE(left_tree),
					 left_tree,
					 TREE_TYPE(right_tree),
					 right_tree);
	  if (left_tree == error_mark_node)
	    return error_mark_node;
	  // This can panic if the type is uncomparable.
	  TREE_NOTHROW(interface_compare_decl) = 0;
	}
      else
	{
	  if (left_type->interface_type()->is_empty())
	    {
	      go_assert(op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ);
	      std::swap(left_type, right_type);
	      std::swap(left_tree, right_tree);
	    }
	  go_assert(!left_type->interface_type()->is_empty());
	  go_assert(right_type->interface_type()->is_empty());
	  static tree interface_empty_compare_decl;
	  left_tree = Gogo::call_builtin(&interface_empty_compare_decl,
					 location,
					 "__go_interface_empty_compare",
					 2,
					 integer_type_node,
					 TREE_TYPE(left_tree),
					 left_tree,
					 TREE_TYPE(right_tree),
					 right_tree);
	  if (left_tree == error_mark_node)
	    return error_mark_node;
	  // This can panic if the type is uncomparable.
	  TREE_NOTHROW(interface_empty_compare_decl) = 0;
	}

      right_tree = build_int_cst_type(integer_type_node, 0);
    }

  if (left_type->is_nil_type()
      && (op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ))
    {
      std::swap(left_type, right_type);
      std::swap(left_tree, right_tree);
    }

  if (right_type->is_nil_type())
    {
      if (left_type->array_type() != NULL
	  && left_type->array_type()->length() == NULL)
	{
	  Array_type* at = left_type->array_type();
	  left_tree = at->value_pointer_tree(context->gogo(), left_tree);
	  right_tree = fold_convert(TREE_TYPE(left_tree), null_pointer_node);
	}
      else if (left_type->interface_type() != NULL)
	{
	  // An interface is nil if the first field is nil.
	  tree left_type_tree = TREE_TYPE(left_tree);
	  go_assert(TREE_CODE(left_type_tree) == RECORD_TYPE);
	  tree field = TYPE_FIELDS(left_type_tree);
	  left_tree = build3(COMPONENT_REF, TREE_TYPE(field), left_tree,
			     field, NULL_TREE);
	  right_tree = fold_convert(TREE_TYPE(left_tree), null_pointer_node);
	}
      else
	{
	  go_assert(POINTER_TYPE_P(TREE_TYPE(left_tree)));
	  right_tree = fold_convert(TREE_TYPE(left_tree), null_pointer_node);
	}
    }

  if (left_tree == error_mark_node || right_tree == error_mark_node)
    return error_mark_node;

  tree ret = fold_build2(code, boolean_type_node, left_tree, right_tree);
  if (CAN_HAVE_LOCATION_P(ret))
    SET_EXPR_LOCATION(ret, location.gcc_location());
  return ret;
}

// Class Bound_method_expression.

// Traversal.

int
Bound_method_expression::do_traverse(Traverse* traverse)
{
  return Expression::traverse(&this->expr_, traverse);
}

// Return the type of a bound method expression.  The type of this
// object is really the type of the method with no receiver.  We
// should be able to get away with just returning the type of the
// method.

Type*
Bound_method_expression::do_type()
{
  if (this->method_->is_function())
    return this->method_->func_value()->type();
  else if (this->method_->is_function_declaration())
    return this->method_->func_declaration_value()->type();
  else
    return Type::make_error_type();
}

// Determine the types of a method expression.

void
Bound_method_expression::do_determine_type(const Type_context*)
{
  Function_type* fntype = this->type()->function_type();
  if (fntype == NULL || !fntype->is_method())
    this->expr_->determine_type_no_context();
  else
    {
      Type_context subcontext(fntype->receiver()->type(), false);
      this->expr_->determine_type(&subcontext);
    }
}

// Check the types of a method expression.

void
Bound_method_expression::do_check_types(Gogo*)
{
  if (!this->method_->is_function()
      && !this->method_->is_function_declaration())
    this->report_error(_("object is not a method"));
  else
    {
      Type* rtype = this->type()->function_type()->receiver()->type()->deref();
      Type* etype = (this->expr_type_ != NULL
		     ? this->expr_type_
		     : this->expr_->type());
      etype = etype->deref();
      if (!Type::are_identical(rtype, etype, true, NULL))
	this->report_error(_("method type does not match object type"));
    }
}

// Get the tree for a method expression.  There is no standard tree
// representation for this.  The only places it may currently be used
// are in a Call_expression or a Go_statement, which will take it
// apart directly.  So this has nothing to do at present.

tree
Bound_method_expression::do_get_tree(Translate_context*)
{
  error_at(this->location(), "reference to method other than calling it");
  return error_mark_node;
}

// Dump ast representation of a bound method expression.

void
Bound_method_expression::do_dump_expression(Ast_dump_context* ast_dump_context)
    const
{
  if (this->expr_type_ != NULL)
    ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->expr_); 
  if (this->expr_type_ != NULL) 
    {
      ast_dump_context->ostream() << ":";
      ast_dump_context->dump_type(this->expr_type_);
      ast_dump_context->ostream() << ")";
    }
    
  ast_dump_context->ostream() << "." << this->method_->name();
}

// Make a method expression.

Bound_method_expression*
Expression::make_bound_method(Expression* expr, Named_object* method,
			      Location location)
{
  return new Bound_method_expression(expr, method, location);
}

// Class Builtin_call_expression.  This is used for a call to a
// builtin function.

class Builtin_call_expression : public Call_expression
{
 public:
  Builtin_call_expression(Gogo* gogo, Expression* fn, Expression_list* args,
			  bool is_varargs, Location location);

 protected:
  // This overrides Call_expression::do_lower.
  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  bool
  do_is_constant() const;

  bool
  do_integer_constant_value(bool, mpz_t, Type**) const;

  bool
  do_float_constant_value(mpfr_t, Type**) const;

  bool
  do_complex_constant_value(mpfr_t, mpfr_t, Type**) const;

  void
  do_discarding_value();

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return new Builtin_call_expression(this->gogo_, this->fn()->copy(),
				       this->args()->copy(),
				       this->is_varargs(),
				       this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  virtual bool
  do_is_recover_call() const;

  virtual void
  do_set_recover_arg(Expression*);

 private:
  // The builtin functions.
  enum Builtin_function_code
    {
      BUILTIN_INVALID,

      // Predeclared builtin functions.
      BUILTIN_APPEND,
      BUILTIN_CAP,
      BUILTIN_CLOSE,
      BUILTIN_COMPLEX,
      BUILTIN_COPY,
      BUILTIN_DELETE,
      BUILTIN_IMAG,
      BUILTIN_LEN,
      BUILTIN_MAKE,
      BUILTIN_NEW,
      BUILTIN_PANIC,
      BUILTIN_PRINT,
      BUILTIN_PRINTLN,
      BUILTIN_REAL,
      BUILTIN_RECOVER,

      // Builtin functions from the unsafe package.
      BUILTIN_ALIGNOF,
      BUILTIN_OFFSETOF,
      BUILTIN_SIZEOF
    };

  Expression*
  one_arg() const;

  bool
  check_one_arg();

  static Type*
  real_imag_type(Type*);

  static Type*
  complex_type(Type*);

  Expression*
  lower_make();

  bool
  check_int_value(Expression*);

  // A pointer back to the general IR structure.  This avoids a global
  // variable, or passing it around everywhere.
  Gogo* gogo_;
  // The builtin function being called.
  Builtin_function_code code_;
  // Used to stop endless loops when the length of an array uses len
  // or cap of the array itself.
  mutable bool seen_;
};

Builtin_call_expression::Builtin_call_expression(Gogo* gogo,
						 Expression* fn,
						 Expression_list* args,
						 bool is_varargs,
						 Location location)
  : Call_expression(fn, args, is_varargs, location),
    gogo_(gogo), code_(BUILTIN_INVALID), seen_(false)
{
  Func_expression* fnexp = this->fn()->func_expression();
  go_assert(fnexp != NULL);
  const std::string& name(fnexp->named_object()->name());
  if (name == "append")
    this->code_ = BUILTIN_APPEND;
  else if (name == "cap")
    this->code_ = BUILTIN_CAP;
  else if (name == "close")
    this->code_ = BUILTIN_CLOSE;
  else if (name == "complex")
    this->code_ = BUILTIN_COMPLEX;
  else if (name == "copy")
    this->code_ = BUILTIN_COPY;
  else if (name == "delete")
    this->code_ = BUILTIN_DELETE;
  else if (name == "imag")
    this->code_ = BUILTIN_IMAG;
  else if (name == "len")
    this->code_ = BUILTIN_LEN;
  else if (name == "make")
    this->code_ = BUILTIN_MAKE;
  else if (name == "new")
    this->code_ = BUILTIN_NEW;
  else if (name == "panic")
    this->code_ = BUILTIN_PANIC;
  else if (name == "print")
    this->code_ = BUILTIN_PRINT;
  else if (name == "println")
    this->code_ = BUILTIN_PRINTLN;
  else if (name == "real")
    this->code_ = BUILTIN_REAL;
  else if (name == "recover")
    this->code_ = BUILTIN_RECOVER;
  else if (name == "Alignof")
    this->code_ = BUILTIN_ALIGNOF;
  else if (name == "Offsetof")
    this->code_ = BUILTIN_OFFSETOF;
  else if (name == "Sizeof")
    this->code_ = BUILTIN_SIZEOF;
  else
    go_unreachable();
}

// Return whether this is a call to recover.  This is a virtual
// function called from the parent class.

bool
Builtin_call_expression::do_is_recover_call() const
{
  if (this->classification() == EXPRESSION_ERROR)
    return false;
  return this->code_ == BUILTIN_RECOVER;
}

// Set the argument for a call to recover.

void
Builtin_call_expression::do_set_recover_arg(Expression* arg)
{
  const Expression_list* args = this->args();
  go_assert(args == NULL || args->empty());
  Expression_list* new_args = new Expression_list();
  new_args->push_back(arg);
  this->set_args(new_args);
}

// A traversal class which looks for a call expression.

class Find_call_expression : public Traverse
{
 public:
  Find_call_expression()
    : Traverse(traverse_expressions),
      found_(false)
  { }

  int
  expression(Expression**);

  bool
  found()
  { return this->found_; }

 private:
  bool found_;
};

int
Find_call_expression::expression(Expression** pexpr)
{
  if ((*pexpr)->call_expression() != NULL)
    {
      this->found_ = true;
      return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Lower a builtin call expression.  This turns new and make into
// specific expressions.  We also convert to a constant if we can.

Expression*
Builtin_call_expression::do_lower(Gogo* gogo, Named_object* function,
				  Statement_inserter* inserter, int)
{
  if (this->classification() == EXPRESSION_ERROR)
    return this;

  Location loc = this->location();

  if (this->is_varargs() && this->code_ != BUILTIN_APPEND)
    {
      this->report_error(_("invalid use of %<...%> with builtin function"));
      return Expression::make_error(loc);
    }

  if (this->is_constant())
    {
      // We can only lower len and cap if there are no function calls
      // in the arguments.  Otherwise we have to make the call.
      if (this->code_ == BUILTIN_LEN || this->code_ == BUILTIN_CAP)
	{
	  Expression* arg = this->one_arg();
	  if (!arg->is_constant())
	    {
	      Find_call_expression find_call;
	      Expression::traverse(&arg, &find_call);
	      if (find_call.found())
		return this;
	    }
	}

      mpz_t ival;
      mpz_init(ival);
      Type* type;
      if (this->integer_constant_value(true, ival, &type))
	{
	  Expression* ret = Expression::make_integer(&ival, type, loc);
	  mpz_clear(ival);
	  return ret;
	}
      mpz_clear(ival);

      mpfr_t rval;
      mpfr_init(rval);
      if (this->float_constant_value(rval, &type))
	{
	  Expression* ret = Expression::make_float(&rval, type, loc);
	  mpfr_clear(rval);
	  return ret;
	}

      mpfr_t imag;
      mpfr_init(imag);
      if (this->complex_constant_value(rval, imag, &type))
	{
	  Expression* ret = Expression::make_complex(&rval, &imag, type, loc);
	  mpfr_clear(rval);
	  mpfr_clear(imag);
	  return ret;
	}
      mpfr_clear(rval);
      mpfr_clear(imag);
    }

  switch (this->code_)
    {
    default:
      break;

    case BUILTIN_NEW:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 1)
	  this->report_error(_("not enough arguments"));
	else if (args->size() > 1)
	  this->report_error(_("too many arguments"));
	else
	  {
	    Expression* arg = args->front();
	    if (!arg->is_type_expression())
	      {
		error_at(arg->location(), "expected type");
		this->set_is_error();
	      }
	    else
	      return Expression::make_allocation(arg->type(), loc);
	  }
      }
      break;

    case BUILTIN_MAKE:
      return this->lower_make();

    case BUILTIN_RECOVER:
      if (function != NULL)
	function->func_value()->set_calls_recover();
      else
	{
	  // Calling recover outside of a function always returns the
	  // nil empty interface.
	  Type* eface = Type::make_interface_type(NULL, loc);
	  return Expression::make_cast(eface, Expression::make_nil(loc), loc);
	}
      break;

    case BUILTIN_APPEND:
      {
	// Lower the varargs.
	const Expression_list* args = this->args();
	if (args == NULL || args->empty())
	  return this;
	Type* slice_type = args->front()->type();
	if (!slice_type->is_slice_type())
	  {
	    error_at(args->front()->location(), "argument 1 must be a slice");
	    this->set_is_error();
	    return this;
	  }
	this->lower_varargs(gogo, function, inserter, slice_type, 2);
      }
      break;

    case BUILTIN_DELETE:
      {
	// Lower to a runtime function call.
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 2)
	  this->report_error(_("not enough arguments"));
	else if (args->size() > 2)
	  this->report_error(_("too many arguments"));
	else if (args->front()->type()->map_type() == NULL)
	  this->report_error(_("argument 1 must be a map"));
	else
	  {
	    // Since this function returns no value it must appear in
	    // a statement by itself, so we don't have to worry about
	    // order of evaluation of values around it.  Evaluate the
	    // map first to get order of evaluation right.
	    Map_type* mt = args->front()->type()->map_type();
	    Temporary_statement* map_temp =
	      Statement::make_temporary(mt, args->front(), loc);
	    inserter->insert(map_temp);

	    Temporary_statement* key_temp =
	      Statement::make_temporary(mt->key_type(), args->back(), loc);
	    inserter->insert(key_temp);

	    Expression* e1 = Expression::make_temporary_reference(map_temp,
								  loc);
	    Expression* e2 = Expression::make_temporary_reference(key_temp,
								  loc);
	    e2 = Expression::make_unary(OPERATOR_AND, e2, loc);
	    return Runtime::make_call(Runtime::MAPDELETE, this->location(),
				      2, e1, e2);
	  }
      }
      break;
    }

  return this;
}

// Lower a make expression.

Expression*
Builtin_call_expression::lower_make()
{
  Location loc = this->location();

  const Expression_list* args = this->args();
  if (args == NULL || args->size() < 1)
    {
      this->report_error(_("not enough arguments"));
      return Expression::make_error(this->location());
    }

  Expression_list::const_iterator parg = args->begin();

  Expression* first_arg = *parg;
  if (!first_arg->is_type_expression())
    {
      error_at(first_arg->location(), "expected type");
      this->set_is_error();
      return Expression::make_error(this->location());
    }
  Type* type = first_arg->type();

  bool is_slice = false;
  bool is_map = false;
  bool is_chan = false;
  if (type->is_slice_type())
    is_slice = true;
  else if (type->map_type() != NULL)
    is_map = true;
  else if (type->channel_type() != NULL)
    is_chan = true;
  else
    {
      this->report_error(_("invalid type for make function"));
      return Expression::make_error(this->location());
    }

  ++parg;
  Expression* len_arg;
  if (parg == args->end())
    {
      if (is_slice)
	{
	  this->report_error(_("length required when allocating a slice"));
	  return Expression::make_error(this->location());
	}

      mpz_t zval;
      mpz_init_set_ui(zval, 0);
      len_arg = Expression::make_integer(&zval, NULL, loc);
      mpz_clear(zval);
    }
  else
    {
      len_arg = *parg;
      if (!this->check_int_value(len_arg))
	{
	  this->report_error(_("bad size for make"));
	  return Expression::make_error(this->location());
	}
      ++parg;
    }

  Expression* cap_arg = NULL;
  if (is_slice && parg != args->end())
    {
      cap_arg = *parg;
      if (!this->check_int_value(cap_arg))
	{
	  this->report_error(_("bad capacity when making slice"));
	  return Expression::make_error(this->location());
	}
      ++parg;
    }

  if (parg != args->end())
    {
      this->report_error(_("too many arguments to make"));
      return Expression::make_error(this->location());
    }

  Location type_loc = first_arg->location();
  Expression* type_arg;
  if (is_slice || is_chan)
    type_arg = Expression::make_type_descriptor(type, type_loc);
  else if (is_map)
    type_arg = Expression::make_map_descriptor(type->map_type(), type_loc);
  else
    go_unreachable();

  Expression* call;
  if (is_slice)
    {
      if (cap_arg == NULL)
	call = Runtime::make_call(Runtime::MAKESLICE1, loc, 2, type_arg,
				  len_arg);
      else
	call = Runtime::make_call(Runtime::MAKESLICE2, loc, 3, type_arg,
				  len_arg, cap_arg);
    }
  else if (is_map)
    call = Runtime::make_call(Runtime::MAKEMAP, loc, 2, type_arg, len_arg);
  else if (is_chan)
    call = Runtime::make_call(Runtime::MAKECHAN, loc, 2, type_arg, len_arg);
  else
    go_unreachable();

  return Expression::make_unsafe_cast(type, call, loc);
}

// Return whether an expression has an integer value.  Report an error
// if not.  This is used when handling calls to the predeclared make
// function.

bool
Builtin_call_expression::check_int_value(Expression* e)
{
  if (e->type()->integer_type() != NULL)
    return true;

  // Check for a floating point constant with integer value.
  mpfr_t fval;
  mpfr_init(fval);

  Type* dummy;
  if (e->float_constant_value(fval, &dummy) && mpfr_integer_p(fval))
    {
      mpz_t ival;
      mpz_init(ival);

      bool ok = false;

      mpfr_clear_overflow();
      mpfr_clear_erangeflag();
      mpfr_get_z(ival, fval, GMP_RNDN);
      if (!mpfr_overflow_p()
	  && !mpfr_erangeflag_p()
	  && mpz_sgn(ival) >= 0)
	{
	  Named_type* ntype = Type::lookup_integer_type("int");
	  Integer_type* inttype = ntype->integer_type();
	  mpz_t max;
	  mpz_init_set_ui(max, 1);
	  mpz_mul_2exp(max, max, inttype->bits() - 1);
	  ok = mpz_cmp(ival, max) < 0;
	  mpz_clear(max);
	}
      mpz_clear(ival);

      if (ok)
	{
	  mpfr_clear(fval);
	  return true;
	}
    }

  mpfr_clear(fval);

  return false;
}

// Return the type of the real or imag functions, given the type of
// the argument.  We need to map complex to float, complex64 to
// float32, and complex128 to float64, so it has to be done by name.
// This returns NULL if it can't figure out the type.

Type*
Builtin_call_expression::real_imag_type(Type* arg_type)
{
  if (arg_type == NULL || arg_type->is_abstract())
    return NULL;
  Named_type* nt = arg_type->named_type();
  if (nt == NULL)
    return NULL;
  while (nt->real_type()->named_type() != NULL)
    nt = nt->real_type()->named_type();
  if (nt->name() == "complex64")
    return Type::lookup_float_type("float32");
  else if (nt->name() == "complex128")
    return Type::lookup_float_type("float64");
  else
    return NULL;
}

// Return the type of the complex function, given the type of one of the
// argments.  Like real_imag_type, we have to map by name.

Type*
Builtin_call_expression::complex_type(Type* arg_type)
{
  if (arg_type == NULL || arg_type->is_abstract())
    return NULL;
  Named_type* nt = arg_type->named_type();
  if (nt == NULL)
    return NULL;
  while (nt->real_type()->named_type() != NULL)
    nt = nt->real_type()->named_type();
  if (nt->name() == "float32")
    return Type::lookup_complex_type("complex64");
  else if (nt->name() == "float64")
    return Type::lookup_complex_type("complex128");
  else
    return NULL;
}

// Return a single argument, or NULL if there isn't one.

Expression*
Builtin_call_expression::one_arg() const
{
  const Expression_list* args = this->args();
  if (args->size() != 1)
    return NULL;
  return args->front();
}

// Return whether this is constant: len of a string, or len or cap of
// a fixed array, or unsafe.Sizeof, unsafe.Offsetof, unsafe.Alignof.

bool
Builtin_call_expression::do_is_constant() const
{
  switch (this->code_)
    {
    case BUILTIN_LEN:
    case BUILTIN_CAP:
      {
	if (this->seen_)
	  return false;

	Expression* arg = this->one_arg();
	if (arg == NULL)
	  return false;
	Type* arg_type = arg->type();

	if (arg_type->points_to() != NULL
	    && arg_type->points_to()->array_type() != NULL
	    && !arg_type->points_to()->is_slice_type())
	  arg_type = arg_type->points_to();

	if (arg_type->array_type() != NULL
	    && arg_type->array_type()->length() != NULL)
	  return true;

	if (this->code_ == BUILTIN_LEN && arg_type->is_string_type())
	  {
	    this->seen_ = true;
	    bool ret = arg->is_constant();
	    this->seen_ = false;
	    return ret;
	  }
      }
      break;

    case BUILTIN_SIZEOF:
    case BUILTIN_ALIGNOF:
      return this->one_arg() != NULL;

    case BUILTIN_OFFSETOF:
      {
	Expression* arg = this->one_arg();
	if (arg == NULL)
	  return false;
	return arg->field_reference_expression() != NULL;
      }

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	if (args != NULL && args->size() == 2)
	  return args->front()->is_constant() && args->back()->is_constant();
      }
      break;

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	Expression* arg = this->one_arg();
	return arg != NULL && arg->is_constant();
      }

    default:
      break;
    }

  return false;
}

// Return an integer constant value if possible.

bool
Builtin_call_expression::do_integer_constant_value(bool iota_is_constant,
						   mpz_t val,
						   Type** ptype) const
{
  if (this->code_ == BUILTIN_LEN
      || this->code_ == BUILTIN_CAP)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;
      Type* arg_type = arg->type();

      if (this->code_ == BUILTIN_LEN && arg_type->is_string_type())
	{
	  std::string sval;
	  if (arg->string_constant_value(&sval))
	    {
	      mpz_set_ui(val, sval.length());
	      *ptype = Type::lookup_integer_type("int");
	      return true;
	    }
	}

      if (arg_type->points_to() != NULL
	  && arg_type->points_to()->array_type() != NULL
	  && !arg_type->points_to()->is_slice_type())
	arg_type = arg_type->points_to();

      if (arg_type->array_type() != NULL
	  && arg_type->array_type()->length() != NULL)
	{
	  if (this->seen_)
	    return false;
	  Expression* e = arg_type->array_type()->length();
	  this->seen_ = true;
	  bool r = e->integer_constant_value(iota_is_constant, val, ptype);
	  this->seen_ = false;
	  if (r)
	    {
	      *ptype = Type::lookup_integer_type("int");
	      return true;
	    }
	}
    }
  else if (this->code_ == BUILTIN_SIZEOF
	   || this->code_ == BUILTIN_ALIGNOF)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;
      Type* arg_type = arg->type();
      if (arg_type->is_error())
	return false;
      if (arg_type->is_abstract())
	return false;
      if (arg_type->named_type() != NULL)
	arg_type->named_type()->convert(this->gogo_);
      tree arg_type_tree = type_to_tree(arg_type->get_backend(this->gogo_));
      if (arg_type_tree == error_mark_node)
	return false;
      unsigned long val_long;
      if (this->code_ == BUILTIN_SIZEOF)
	{
	  tree type_size = TYPE_SIZE_UNIT(arg_type_tree);
	  go_assert(TREE_CODE(type_size) == INTEGER_CST);
	  if (TREE_INT_CST_HIGH(type_size) != 0)
	    return false;
	  unsigned HOST_WIDE_INT val_wide = TREE_INT_CST_LOW(type_size);
	  val_long = static_cast<unsigned long>(val_wide);
	  if (val_long != val_wide)
	    return false;
	}
      else if (this->code_ == BUILTIN_ALIGNOF)
	{
	  if (arg->field_reference_expression() == NULL)
	    val_long = go_type_alignment(arg_type_tree);
	  else
	    {
	      // Calling unsafe.Alignof(s.f) returns the alignment of
	      // the type of f when it is used as a field in a struct.
	      val_long = go_field_alignment(arg_type_tree);
	    }
	}
      else
	go_unreachable();
      mpz_set_ui(val, val_long);
      *ptype = NULL;
      return true;
    }
  else if (this->code_ == BUILTIN_OFFSETOF)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;
      Field_reference_expression* farg = arg->field_reference_expression();
      if (farg == NULL)
	return false;
      Expression* struct_expr = farg->expr();
      Type* st = struct_expr->type();
      if (st->struct_type() == NULL)
	return false;
      if (st->named_type() != NULL)
	st->named_type()->convert(this->gogo_);
      tree struct_tree = type_to_tree(st->get_backend(this->gogo_));
      go_assert(TREE_CODE(struct_tree) == RECORD_TYPE);
      tree field = TYPE_FIELDS(struct_tree);
      for (unsigned int index = farg->field_index(); index > 0; --index)
	{
	  field = DECL_CHAIN(field);
	  go_assert(field != NULL_TREE);
	}
      HOST_WIDE_INT offset_wide = int_byte_position (field);
      if (offset_wide < 0)
	return false;
      unsigned long offset_long = static_cast<unsigned long>(offset_wide);
      if (offset_long != static_cast<unsigned HOST_WIDE_INT>(offset_wide))
	return false;
      mpz_set_ui(val, offset_long);
      return true;
    }
  return false;
}

// Return a floating point constant value if possible.

bool
Builtin_call_expression::do_float_constant_value(mpfr_t val,
						 Type** ptype) const
{
  if (this->code_ == BUILTIN_REAL || this->code_ == BUILTIN_IMAG)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;

      mpfr_t real;
      mpfr_t imag;
      mpfr_init(real);
      mpfr_init(imag);

      bool ret = false;
      Type* type;
      if (arg->complex_constant_value(real, imag, &type))
	{
	  if (this->code_ == BUILTIN_REAL)
	    mpfr_set(val, real, GMP_RNDN);
	  else
	    mpfr_set(val, imag, GMP_RNDN);
	  *ptype = Builtin_call_expression::real_imag_type(type);
	  ret = true;
	}

      mpfr_clear(real);
      mpfr_clear(imag);
      return ret;
    }

  return false;
}

// Return a complex constant value if possible.

bool
Builtin_call_expression::do_complex_constant_value(mpfr_t real, mpfr_t imag,
						   Type** ptype) const
{
  if (this->code_ == BUILTIN_COMPLEX)
    {
      const Expression_list* args = this->args();
      if (args == NULL || args->size() != 2)
	return false;

      mpfr_t r;
      mpfr_init(r);
      Type* rtype;
      if (!args->front()->float_constant_value(r, &rtype))
	{
	  mpfr_clear(r);
	  return false;
	}

      mpfr_t i;
      mpfr_init(i);

      bool ret = false;
      Type* itype;
      if (args->back()->float_constant_value(i, &itype)
	  && Type::are_identical(rtype, itype, false, NULL))
	{
	  mpfr_set(real, r, GMP_RNDN);
	  mpfr_set(imag, i, GMP_RNDN);
	  *ptype = Builtin_call_expression::complex_type(rtype);
	  ret = true;
	}

      mpfr_clear(r);
      mpfr_clear(i);

      return ret;
    }

  return false;
}

// Give an error if we are discarding the value of an expression which
// should not normally be discarded.  We don't give an error for
// discarding the value of an ordinary function call, but we do for
// builtin functions, purely for consistency with the gc compiler.

void
Builtin_call_expression::do_discarding_value()
{
  switch (this->code_)
    {
    case BUILTIN_INVALID:
    default:
      go_unreachable();

    case BUILTIN_APPEND:
    case BUILTIN_CAP:
    case BUILTIN_COMPLEX:
    case BUILTIN_IMAG:
    case BUILTIN_LEN:
    case BUILTIN_MAKE:
    case BUILTIN_NEW:
    case BUILTIN_REAL:
    case BUILTIN_ALIGNOF:
    case BUILTIN_OFFSETOF:
    case BUILTIN_SIZEOF:
      this->unused_value_error();
      break;

    case BUILTIN_CLOSE:
    case BUILTIN_COPY:
    case BUILTIN_DELETE:
    case BUILTIN_PANIC:
    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
    case BUILTIN_RECOVER:
      break;
    }
}

// Return the type.

Type*
Builtin_call_expression::do_type()
{
  switch (this->code_)
    {
    case BUILTIN_INVALID:
    default:
      go_unreachable();

    case BUILTIN_NEW:
    case BUILTIN_MAKE:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->empty())
	  return Type::make_error_type();
	return Type::make_pointer_type(args->front()->type());
      }

    case BUILTIN_CAP:
    case BUILTIN_COPY:
    case BUILTIN_LEN:
    case BUILTIN_ALIGNOF:
    case BUILTIN_OFFSETOF:
    case BUILTIN_SIZEOF:
      return Type::lookup_integer_type("int");

    case BUILTIN_CLOSE:
    case BUILTIN_DELETE:
    case BUILTIN_PANIC:
    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      return Type::make_void_type();

    case BUILTIN_RECOVER:
      return Type::make_interface_type(NULL, Linemap::predeclared_location());

    case BUILTIN_APPEND:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->empty())
	  return Type::make_error_type();
	return args->front()->type();
      }

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	Expression* arg = this->one_arg();
	if (arg == NULL)
	  return Type::make_error_type();
	Type* t = arg->type();
	if (t->is_abstract())
	  t = t->make_non_abstract_type();
	t = Builtin_call_expression::real_imag_type(t);
	if (t == NULL)
	  t = Type::make_error_type();
	return t;
      }

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() != 2)
	  return Type::make_error_type();
	Type* t = args->front()->type();
	if (t->is_abstract())
	  {
	    t = args->back()->type();
	    if (t->is_abstract())
	      t = t->make_non_abstract_type();
	  }
	t = Builtin_call_expression::complex_type(t);
	if (t == NULL)
	  t = Type::make_error_type();
	return t;
      }
    }
}

// Determine the type.

void
Builtin_call_expression::do_determine_type(const Type_context* context)
{
  if (!this->determining_types())
    return;

  this->fn()->determine_type_no_context();

  const Expression_list* args = this->args();

  bool is_print;
  Type* arg_type = NULL;
  switch (this->code_)
    {
    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      // Do not force a large integer constant to "int".
      is_print = true;
      break;

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      arg_type = Builtin_call_expression::complex_type(context->type);
      is_print = false;
      break;

    case BUILTIN_COMPLEX:
      {
	// For the complex function the type of one operand can
	// determine the type of the other, as in a binary expression.
	arg_type = Builtin_call_expression::real_imag_type(context->type);
	if (args != NULL && args->size() == 2)
	  {
	    Type* t1 = args->front()->type();
	    Type* t2 = args->front()->type();
	    if (!t1->is_abstract())
	      arg_type = t1;
	    else if (!t2->is_abstract())
	      arg_type = t2;
	  }
	is_print = false;
      }
      break;

    default:
      is_print = false;
      break;
    }

  if (args != NULL)
    {
      for (Expression_list::const_iterator pa = args->begin();
	   pa != args->end();
	   ++pa)
	{
	  Type_context subcontext;
	  subcontext.type = arg_type;

	  if (is_print)
	    {
	      // We want to print large constants, we so can't just
	      // use the appropriate nonabstract type.  Use uint64 for
	      // an integer if we know it is nonnegative, otherwise
	      // use int64 for a integer, otherwise use float64 for a
	      // float or complex128 for a complex.
	      Type* want_type = NULL;
	      Type* atype = (*pa)->type();
	      if (atype->is_abstract())
		{
		  if (atype->integer_type() != NULL)
		    {
		      mpz_t val;
		      mpz_init(val);
		      Type* dummy;
		      if (this->integer_constant_value(true, val, &dummy)
			  && mpz_sgn(val) >= 0)
			want_type = Type::lookup_integer_type("uint64");
		      else
			want_type = Type::lookup_integer_type("int64");
		      mpz_clear(val);
		    }
		  else if (atype->float_type() != NULL)
		    want_type = Type::lookup_float_type("float64");
		  else if (atype->complex_type() != NULL)
		    want_type = Type::lookup_complex_type("complex128");
		  else if (atype->is_abstract_string_type())
		    want_type = Type::lookup_string_type();
		  else if (atype->is_abstract_boolean_type())
		    want_type = Type::lookup_bool_type();
		  else
		    go_unreachable();
		  subcontext.type = want_type;
		}
	    }

	  (*pa)->determine_type(&subcontext);
	}
    }
}

// If there is exactly one argument, return true.  Otherwise give an
// error message and return false.

bool
Builtin_call_expression::check_one_arg()
{
  const Expression_list* args = this->args();
  if (args == NULL || args->size() < 1)
    {
      this->report_error(_("not enough arguments"));
      return false;
    }
  else if (args->size() > 1)
    {
      this->report_error(_("too many arguments"));
      return false;
    }
  if (args->front()->is_error_expression()
      || args->front()->type()->is_error())
    {
      this->set_is_error();
      return false;
    }
  return true;
}

// Check argument types for a builtin function.

void
Builtin_call_expression::do_check_types(Gogo*)
{
  switch (this->code_)
    {
    case BUILTIN_INVALID:
    case BUILTIN_NEW:
    case BUILTIN_MAKE:
      return;

    case BUILTIN_LEN:
    case BUILTIN_CAP:
      {
	// The single argument may be either a string or an array or a
	// map or a channel, or a pointer to a closed array.
	if (this->check_one_arg())
	  {
	    Type* arg_type = this->one_arg()->type();
	    if (arg_type->points_to() != NULL
		&& arg_type->points_to()->array_type() != NULL
		&& !arg_type->points_to()->is_slice_type())
	      arg_type = arg_type->points_to();
	    if (this->code_ == BUILTIN_CAP)
	      {
		if (!arg_type->is_error()
		    && arg_type->array_type() == NULL
		    && arg_type->channel_type() == NULL)
		  this->report_error(_("argument must be array or slice "
				       "or channel"));
	      }
	    else
	      {
		if (!arg_type->is_error()
		    && !arg_type->is_string_type()
		    && arg_type->array_type() == NULL
		    && arg_type->map_type() == NULL
		    && arg_type->channel_type() == NULL)
		  this->report_error(_("argument must be string or "
				       "array or slice or map or channel"));
	      }
	  }
      }
      break;

    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      {
	const Expression_list* args = this->args();
	if (args == NULL)
	  {
	    if (this->code_ == BUILTIN_PRINT)
	      warning_at(this->location(), 0,
			 "no arguments for builtin function %<%s%>",
			 (this->code_ == BUILTIN_PRINT
			  ? "print"
			  : "println"));
	  }
	else
	  {
	    for (Expression_list::const_iterator p = args->begin();
		 p != args->end();
		 ++p)
	      {
		Type* type = (*p)->type();
		if (type->is_error()
		    || type->is_string_type()
		    || type->integer_type() != NULL
		    || type->float_type() != NULL
		    || type->complex_type() != NULL
		    || type->is_boolean_type()
		    || type->points_to() != NULL
		    || type->interface_type() != NULL
		    || type->channel_type() != NULL
		    || type->map_type() != NULL
		    || type->function_type() != NULL
		    || type->is_slice_type())
		  ;
		else
		  this->report_error(_("unsupported argument type to "
				       "builtin function"));
	      }
	  }
      }
      break;

    case BUILTIN_CLOSE:
      if (this->check_one_arg())
	{
	  if (this->one_arg()->type()->channel_type() == NULL)
	    this->report_error(_("argument must be channel"));
	  else if (!this->one_arg()->type()->channel_type()->may_send())
	    this->report_error(_("cannot close receive-only channel"));
	}
      break;

    case BUILTIN_PANIC:
    case BUILTIN_SIZEOF:
    case BUILTIN_ALIGNOF:
      this->check_one_arg();
      break;

    case BUILTIN_RECOVER:
      if (this->args() != NULL && !this->args()->empty())
	this->report_error(_("too many arguments"));
      break;

    case BUILTIN_OFFSETOF:
      if (this->check_one_arg())
	{
	  Expression* arg = this->one_arg();
	  if (arg->field_reference_expression() == NULL)
	    this->report_error(_("argument must be a field reference"));
	}
      break;

    case BUILTIN_COPY:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 2)
	  {
	    this->report_error(_("not enough arguments"));
	    break;
	  }
	else if (args->size() > 2)
	  {
	    this->report_error(_("too many arguments"));
	    break;
	  }
	Type* arg1_type = args->front()->type();
	Type* arg2_type = args->back()->type();
	if (arg1_type->is_error() || arg2_type->is_error())
	  break;

	Type* e1;
	if (arg1_type->is_slice_type())
	  e1 = arg1_type->array_type()->element_type();
	else
	  {
	    this->report_error(_("left argument must be a slice"));
	    break;
	  }

	Type* e2;
	if (arg2_type->is_slice_type())
	  e2 = arg2_type->array_type()->element_type();
	else if (arg2_type->is_string_type())
	  e2 = Type::lookup_integer_type("uint8");
	else
	  {
	    this->report_error(_("right argument must be a slice or a string"));
	    break;
	  }

	if (!Type::are_identical(e1, e2, true, NULL))
	  this->report_error(_("element types must be the same"));
      }
      break;

    case BUILTIN_APPEND:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 2)
	  {
	    this->report_error(_("not enough arguments"));
	    break;
	  }
	if (args->size() > 2)
	  {
	    this->report_error(_("too many arguments"));
	    break;
	  }

	// The language permits appending a string to a []byte, as a
	// special case.
	if (args->back()->type()->is_string_type())
	  {
	    const Array_type* at = args->front()->type()->array_type();
	    const Type* e = at->element_type()->forwarded();
	    if (e == Type::lookup_integer_type("uint8"))
	      break;
	  }

	std::string reason;
	if (!Type::are_assignable(args->front()->type(), args->back()->type(),
				  &reason))
	  {
	    if (reason.empty())
	      this->report_error(_("arguments 1 and 2 have different types"));
	    else
	      {
		error_at(this->location(),
			 "arguments 1 and 2 have different types (%s)",
			 reason.c_str());
		this->set_is_error();
	      }
	  }
	break;
      }

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      if (this->check_one_arg())
	{
	  if (this->one_arg()->type()->complex_type() == NULL)
	    this->report_error(_("argument must have complex type"));
	}
      break;

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->size() < 2)
	  this->report_error(_("not enough arguments"));
	else if (args->size() > 2)
	  this->report_error(_("too many arguments"));
	else if (args->front()->is_error_expression()
		 || args->front()->type()->is_error()
		 || args->back()->is_error_expression()
		 || args->back()->type()->is_error())
	  this->set_is_error();
	else if (!Type::are_identical(args->front()->type(),
				      args->back()->type(), true, NULL))
	  this->report_error(_("complex arguments must have identical types"));
	else if (args->front()->type()->float_type() == NULL)
	  this->report_error(_("complex arguments must have "
			       "floating-point type"));
      }
      break;

    default:
      go_unreachable();
    }
}

// Return the tree for a builtin function.

tree
Builtin_call_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location location = this->location();
  switch (this->code_)
    {
    case BUILTIN_INVALID:
    case BUILTIN_NEW:
    case BUILTIN_MAKE:
      go_unreachable();

    case BUILTIN_LEN:
    case BUILTIN_CAP:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = *args->begin();
	Type* arg_type = arg->type();

	if (this->seen_)
	  {
	    go_assert(saw_errors());
	    return error_mark_node;
	  }
	this->seen_ = true;

	tree arg_tree = arg->get_tree(context);

	this->seen_ = false;

	if (arg_tree == error_mark_node)
	  return error_mark_node;

	if (arg_type->points_to() != NULL)
	  {
	    arg_type = arg_type->points_to();
	    go_assert(arg_type->array_type() != NULL
		       && !arg_type->is_slice_type());
	    go_assert(POINTER_TYPE_P(TREE_TYPE(arg_tree)));
	    arg_tree = build_fold_indirect_ref(arg_tree);
	  }

	tree val_tree;
	if (this->code_ == BUILTIN_LEN)
	  {
	    if (arg_type->is_string_type())
	      val_tree = String_type::length_tree(gogo, arg_tree);
	    else if (arg_type->array_type() != NULL)
	      {
		if (this->seen_)
		  {
		    go_assert(saw_errors());
		    return error_mark_node;
		  }
		this->seen_ = true;
		val_tree = arg_type->array_type()->length_tree(gogo, arg_tree);
		this->seen_ = false;
	      }
	    else if (arg_type->map_type() != NULL)
	      {
		tree arg_type_tree = type_to_tree(arg_type->get_backend(gogo));
		static tree map_len_fndecl;
		val_tree = Gogo::call_builtin(&map_len_fndecl,
					      location,
					      "__go_map_len",
					      1,
					      integer_type_node,
					      arg_type_tree,
					      arg_tree);
	      }
	    else if (arg_type->channel_type() != NULL)
	      {
		tree arg_type_tree = type_to_tree(arg_type->get_backend(gogo));
		static tree chan_len_fndecl;
		val_tree = Gogo::call_builtin(&chan_len_fndecl,
					      location,
					      "__go_chan_len",
					      1,
					      integer_type_node,
					      arg_type_tree,
					      arg_tree);
	      }
	    else
	      go_unreachable();
	  }
	else
	  {
	    if (arg_type->array_type() != NULL)
	      {
		if (this->seen_)
		  {
		    go_assert(saw_errors());
		    return error_mark_node;
		  }
		this->seen_ = true;
		val_tree = arg_type->array_type()->capacity_tree(gogo,
								 arg_tree);
		this->seen_ = false;
	      }
	    else if (arg_type->channel_type() != NULL)
	      {
		tree arg_type_tree = type_to_tree(arg_type->get_backend(gogo));
		static tree chan_cap_fndecl;
		val_tree = Gogo::call_builtin(&chan_cap_fndecl,
					      location,
					      "__go_chan_cap",
					      1,
					      integer_type_node,
					      arg_type_tree,
					      arg_tree);
	      }
	    else
	      go_unreachable();
	  }

	if (val_tree == error_mark_node)
	  return error_mark_node;

	Type* int_type = Type::lookup_integer_type("int");
	tree type_tree = type_to_tree(int_type->get_backend(gogo));
	if (type_tree == TREE_TYPE(val_tree))
	  return val_tree;
	else
	  return fold(convert_to_integer(type_tree, val_tree));
      }

    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      {
	const bool is_ln = this->code_ == BUILTIN_PRINTLN;
	tree stmt_list = NULL_TREE;

	const Expression_list* call_args = this->args();
	if (call_args != NULL)
	  {
	    for (Expression_list::const_iterator p = call_args->begin();
		 p != call_args->end();
		 ++p)
	      {
		if (is_ln && p != call_args->begin())
		  {
		    static tree print_space_fndecl;
		    tree call = Gogo::call_builtin(&print_space_fndecl,
						   location,
						   "__go_print_space",
						   0,
						   void_type_node);
		    if (call == error_mark_node)
		      return error_mark_node;
		    append_to_statement_list(call, &stmt_list);
		  }

		Type* type = (*p)->type();

		tree arg = (*p)->get_tree(context);
		if (arg == error_mark_node)
		  return error_mark_node;

		tree* pfndecl;
		const char* fnname;
		if (type->is_string_type())
		  {
		    static tree print_string_fndecl;
		    pfndecl = &print_string_fndecl;
		    fnname = "__go_print_string";
		  }
		else if (type->integer_type() != NULL
			 && type->integer_type()->is_unsigned())
		  {
		    static tree print_uint64_fndecl;
		    pfndecl = &print_uint64_fndecl;
		    fnname = "__go_print_uint64";
		    Type* itype = Type::lookup_integer_type("uint64");
		    Btype* bitype = itype->get_backend(gogo);
		    arg = fold_convert_loc(location.gcc_location(),
                                           type_to_tree(bitype), arg);
		  }
		else if (type->integer_type() != NULL)
		  {
		    static tree print_int64_fndecl;
		    pfndecl = &print_int64_fndecl;
		    fnname = "__go_print_int64";
		    Type* itype = Type::lookup_integer_type("int64");
		    Btype* bitype = itype->get_backend(gogo);
		    arg = fold_convert_loc(location.gcc_location(),
                                           type_to_tree(bitype), arg);
		  }
		else if (type->float_type() != NULL)
		  {
		    static tree print_double_fndecl;
		    pfndecl = &print_double_fndecl;
		    fnname = "__go_print_double";
		    arg = fold_convert_loc(location.gcc_location(),
                                           double_type_node, arg);
		  }
		else if (type->complex_type() != NULL)
		  {
		    static tree print_complex_fndecl;
		    pfndecl = &print_complex_fndecl;
		    fnname = "__go_print_complex";
		    arg = fold_convert_loc(location.gcc_location(),
                                           complex_double_type_node, arg);
		  }
		else if (type->is_boolean_type())
		  {
		    static tree print_bool_fndecl;
		    pfndecl = &print_bool_fndecl;
		    fnname = "__go_print_bool";
		  }
		else if (type->points_to() != NULL
			 || type->channel_type() != NULL
			 || type->map_type() != NULL
			 || type->function_type() != NULL)
		  {
		    static tree print_pointer_fndecl;
		    pfndecl = &print_pointer_fndecl;
		    fnname = "__go_print_pointer";
		    arg = fold_convert_loc(location.gcc_location(),
                                           ptr_type_node, arg);
		  }
		else if (type->interface_type() != NULL)
		  {
		    if (type->interface_type()->is_empty())
		      {
			static tree print_empty_interface_fndecl;
			pfndecl = &print_empty_interface_fndecl;
			fnname = "__go_print_empty_interface";
		      }
		    else
		      {
			static tree print_interface_fndecl;
			pfndecl = &print_interface_fndecl;
			fnname = "__go_print_interface";
		      }
		  }
		else if (type->is_slice_type())
		  {
		    static tree print_slice_fndecl;
		    pfndecl = &print_slice_fndecl;
		    fnname = "__go_print_slice";
		  }
		else
		  go_unreachable();

		tree call = Gogo::call_builtin(pfndecl,
					       location,
					       fnname,
					       1,
					       void_type_node,
					       TREE_TYPE(arg),
					       arg);
		if (call == error_mark_node)
		  return error_mark_node;
		append_to_statement_list(call, &stmt_list);
	      }
	  }

	if (is_ln)
	  {
	    static tree print_nl_fndecl;
	    tree call = Gogo::call_builtin(&print_nl_fndecl,
					   location,
					   "__go_print_nl",
					   0,
					   void_type_node);
	    if (call == error_mark_node)
	      return error_mark_node;
	    append_to_statement_list(call, &stmt_list);
	  }

	return stmt_list;
      }

    case BUILTIN_PANIC:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
	tree arg_tree = arg->get_tree(context);
	if (arg_tree == error_mark_node)
	  return error_mark_node;
	Type *empty =
	  Type::make_interface_type(NULL, Linemap::predeclared_location());
	arg_tree = Expression::convert_for_assignment(context, empty,
						      arg->type(),
						      arg_tree, location);
	static tree panic_fndecl;
	tree call = Gogo::call_builtin(&panic_fndecl,
				       location,
				       "__go_panic",
				       1,
				       void_type_node,
				       TREE_TYPE(arg_tree),
				       arg_tree);
	if (call == error_mark_node)
	  return error_mark_node;
	// This function will throw an exception.
	TREE_NOTHROW(panic_fndecl) = 0;
	// This function will not return.
	TREE_THIS_VOLATILE(panic_fndecl) = 1;
	return call;
      }

    case BUILTIN_RECOVER:
      {
	// The argument is set when building recover thunks.  It's a
	// boolean value which is true if we can recover a value now.
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
	tree arg_tree = arg->get_tree(context);
	if (arg_tree == error_mark_node)
	  return error_mark_node;

	Type *empty =
	  Type::make_interface_type(NULL, Linemap::predeclared_location());
	tree empty_tree = type_to_tree(empty->get_backend(context->gogo()));

	Type* nil_type = Type::make_nil_type();
	Expression* nil = Expression::make_nil(location);
	tree nil_tree = nil->get_tree(context);
	tree empty_nil_tree = Expression::convert_for_assignment(context,
								 empty,
								 nil_type,
								 nil_tree,
								 location);

	// We need to handle a deferred call to recover specially,
	// because it changes whether it can recover a panic or not.
	// See test7 in test/recover1.go.
	tree call;
	if (this->is_deferred())
	  {
	    static tree deferred_recover_fndecl;
	    call = Gogo::call_builtin(&deferred_recover_fndecl,
				      location,
				      "__go_deferred_recover",
				      0,
				      empty_tree);
	  }
	else
	  {
	    static tree recover_fndecl;
	    call = Gogo::call_builtin(&recover_fndecl,
				      location,
				      "__go_recover",
				      0,
				      empty_tree);
	  }
	if (call == error_mark_node)
	  return error_mark_node;
	return fold_build3_loc(location.gcc_location(), COND_EXPR, empty_tree,
                               arg_tree, call, empty_nil_tree);
      }

    case BUILTIN_CLOSE:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
	tree arg_tree = arg->get_tree(context);
	if (arg_tree == error_mark_node)
	  return error_mark_node;
	static tree close_fndecl;
	return Gogo::call_builtin(&close_fndecl,
				  location,
				  "__go_builtin_close",
				  1,
				  void_type_node,
				  TREE_TYPE(arg_tree),
				  arg_tree);
      }

    case BUILTIN_SIZEOF:
    case BUILTIN_OFFSETOF:
    case BUILTIN_ALIGNOF:
      {
	mpz_t val;
	mpz_init(val);
	Type* dummy;
	bool b = this->integer_constant_value(true, val, &dummy);
	if (!b)
	  {
	    go_assert(saw_errors());
	    return error_mark_node;
	  }
	Type* int_type = Type::lookup_integer_type("int");
	tree type = type_to_tree(int_type->get_backend(gogo));
	tree ret = Expression::integer_constant_tree(val, type);
	mpz_clear(val);
	return ret;
      }

    case BUILTIN_COPY:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	Expression* arg1 = args->front();
	Expression* arg2 = args->back();

	tree arg1_tree = arg1->get_tree(context);
	tree arg2_tree = arg2->get_tree(context);
	if (arg1_tree == error_mark_node || arg2_tree == error_mark_node)
	  return error_mark_node;

	Type* arg1_type = arg1->type();
	Array_type* at = arg1_type->array_type();
	arg1_tree = save_expr(arg1_tree);
	tree arg1_val = at->value_pointer_tree(gogo, arg1_tree);
	tree arg1_len = at->length_tree(gogo, arg1_tree);
	if (arg1_val == error_mark_node || arg1_len == error_mark_node)
	  return error_mark_node;

	Type* arg2_type = arg2->type();
	tree arg2_val;
	tree arg2_len;
	if (arg2_type->is_slice_type())
	  {
	    at = arg2_type->array_type();
	    arg2_tree = save_expr(arg2_tree);
	    arg2_val = at->value_pointer_tree(gogo, arg2_tree);
	    arg2_len = at->length_tree(gogo, arg2_tree);
	  }
	else
	  {
	    arg2_tree = save_expr(arg2_tree);
	    arg2_val = String_type::bytes_tree(gogo, arg2_tree);
	    arg2_len = String_type::length_tree(gogo, arg2_tree);
	  }
	if (arg2_val == error_mark_node || arg2_len == error_mark_node)
	  return error_mark_node;

	arg1_len = save_expr(arg1_len);
	arg2_len = save_expr(arg2_len);
	tree len = fold_build3_loc(location.gcc_location(), COND_EXPR,
                                   TREE_TYPE(arg1_len),
				   fold_build2_loc(location.gcc_location(),
                                                   LT_EXPR, boolean_type_node,
						   arg1_len, arg2_len),
				   arg1_len, arg2_len);
	len = save_expr(len);

	Type* element_type = at->element_type();
	Btype* element_btype = element_type->get_backend(gogo);
	tree element_type_tree = type_to_tree(element_btype);
	if (element_type_tree == error_mark_node)
	  return error_mark_node;
	tree element_size = TYPE_SIZE_UNIT(element_type_tree);
	tree bytecount = fold_convert_loc(location.gcc_location(),
                                          TREE_TYPE(element_size), len);
	bytecount = fold_build2_loc(location.gcc_location(), MULT_EXPR,
				    TREE_TYPE(element_size),
				    bytecount, element_size);
	bytecount = fold_convert_loc(location.gcc_location(), size_type_node,
                                     bytecount);

	arg1_val = fold_convert_loc(location.gcc_location(), ptr_type_node,
                                    arg1_val);
	arg2_val = fold_convert_loc(location.gcc_location(), ptr_type_node,
                                    arg2_val);

	static tree copy_fndecl;
	tree call = Gogo::call_builtin(&copy_fndecl,
				       location,
				       "__go_copy",
				       3,
				       void_type_node,
				       ptr_type_node,
				       arg1_val,
				       ptr_type_node,
				       arg2_val,
				       size_type_node,
				       bytecount);
	if (call == error_mark_node)
	  return error_mark_node;

	return fold_build2_loc(location.gcc_location(), COMPOUND_EXPR,
                               TREE_TYPE(len), call, len);
      }

    case BUILTIN_APPEND:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	Expression* arg1 = args->front();
	Expression* arg2 = args->back();

	tree arg1_tree = arg1->get_tree(context);
	tree arg2_tree = arg2->get_tree(context);
	if (arg1_tree == error_mark_node || arg2_tree == error_mark_node)
	  return error_mark_node;

	Array_type* at = arg1->type()->array_type();
	Type* element_type = at->element_type()->forwarded();

	tree arg2_val;
	tree arg2_len;
	tree element_size;
	if (arg2->type()->is_string_type()
	    && element_type == Type::lookup_integer_type("uint8"))
	  {
	    arg2_tree = save_expr(arg2_tree);
	    arg2_val = String_type::bytes_tree(gogo, arg2_tree);
	    arg2_len = String_type::length_tree(gogo, arg2_tree);
	    element_size = size_int(1);
	  }
	else
	  {
	    arg2_tree = Expression::convert_for_assignment(context, at,
							   arg2->type(),
							   arg2_tree,
							   location);
	    if (arg2_tree == error_mark_node)
	      return error_mark_node;

	    arg2_tree = save_expr(arg2_tree);

	     arg2_val = at->value_pointer_tree(gogo, arg2_tree);
	     arg2_len = at->length_tree(gogo, arg2_tree);

	     Btype* element_btype = element_type->get_backend(gogo);
	     tree element_type_tree = type_to_tree(element_btype);
	     if (element_type_tree == error_mark_node)
	       return error_mark_node;
	     element_size = TYPE_SIZE_UNIT(element_type_tree);
	  }

	arg2_val = fold_convert_loc(location.gcc_location(), ptr_type_node,
                                    arg2_val);
	arg2_len = fold_convert_loc(location.gcc_location(), size_type_node,
                                    arg2_len);
	element_size = fold_convert_loc(location.gcc_location(), size_type_node,
					element_size);

	if (arg2_val == error_mark_node
	    || arg2_len == error_mark_node
	    || element_size == error_mark_node)
	  return error_mark_node;

	// We rebuild the decl each time since the slice types may
	// change.
	tree append_fndecl = NULL_TREE;
	return Gogo::call_builtin(&append_fndecl,
				  location,
				  "__go_append",
				  4,
				  TREE_TYPE(arg1_tree),
				  TREE_TYPE(arg1_tree),
				  arg1_tree,
				  ptr_type_node,
				  arg2_val,
				  size_type_node,
				  arg2_len,
				  size_type_node,
				  element_size);
      }

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
	tree arg_tree = arg->get_tree(context);
	if (arg_tree == error_mark_node)
	  return error_mark_node;
	go_assert(COMPLEX_FLOAT_TYPE_P(TREE_TYPE(arg_tree)));
	if (this->code_ == BUILTIN_REAL)
	  return fold_build1_loc(location.gcc_location(), REALPART_EXPR,
				 TREE_TYPE(TREE_TYPE(arg_tree)),
				 arg_tree);
	else
	  return fold_build1_loc(location.gcc_location(), IMAGPART_EXPR,
				 TREE_TYPE(TREE_TYPE(arg_tree)),
				 arg_tree);
      }

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	tree r = args->front()->get_tree(context);
	tree i = args->back()->get_tree(context);
	if (r == error_mark_node || i == error_mark_node)
	  return error_mark_node;
	go_assert(TYPE_MAIN_VARIANT(TREE_TYPE(r))
		   == TYPE_MAIN_VARIANT(TREE_TYPE(i)));
	go_assert(SCALAR_FLOAT_TYPE_P(TREE_TYPE(r)));
	return fold_build2_loc(location.gcc_location(), COMPLEX_EXPR,
			       build_complex_type(TREE_TYPE(r)),
			       r, i);
      }

    default:
      go_unreachable();
    }
}

// We have to support exporting a builtin call expression, because
// code can set a constant to the result of a builtin expression.

void
Builtin_call_expression::do_export(Export* exp) const
{
  bool ok = false;

  mpz_t val;
  mpz_init(val);
  Type* dummy;
  if (this->integer_constant_value(true, val, &dummy))
    {
      Integer_expression::export_integer(exp, val);
      ok = true;
    }
  mpz_clear(val);

  if (!ok)
    {
      mpfr_t fval;
      mpfr_init(fval);
      if (this->float_constant_value(fval, &dummy))
	{
	  Float_expression::export_float(exp, fval);
	  ok = true;
	}
      mpfr_clear(fval);
    }

  if (!ok)
    {
      mpfr_t real;
      mpfr_t imag;
      mpfr_init(real);
      mpfr_init(imag);
      if (this->complex_constant_value(real, imag, &dummy))
	{
	  Complex_expression::export_complex(exp, real, imag);
	  ok = true;
	}
      mpfr_clear(real);
      mpfr_clear(imag);
    }

  if (!ok)
    {
      error_at(this->location(), "value is not constant");
      return;
    }

  // A trailing space lets us reliably identify the end of the number.
  exp->write_c_string(" ");
}

// Class Call_expression.

// Traversal.

int
Call_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->fn_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->args_ != NULL)
    {
      if (this->args_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Lower a call statement.

Expression*
Call_expression::do_lower(Gogo* gogo, Named_object* function,
			  Statement_inserter* inserter, int)
{
  Location loc = this->location();

  // A type cast can look like a function call.
  if (this->fn_->is_type_expression()
      && this->args_ != NULL
      && this->args_->size() == 1)
    return Expression::make_cast(this->fn_->type(), this->args_->front(),
				 loc);

  // Recognize a call to a builtin function.
  Func_expression* fne = this->fn_->func_expression();
  if (fne != NULL
      && fne->named_object()->is_function_declaration()
      && fne->named_object()->func_declaration_value()->type()->is_builtin())
    return new Builtin_call_expression(gogo, this->fn_, this->args_,
				       this->is_varargs_, loc);

  // Handle an argument which is a call to a function which returns
  // multiple results.
  if (this->args_ != NULL
      && this->args_->size() == 1
      && this->args_->front()->call_expression() != NULL
      && this->fn_->type()->function_type() != NULL)
    {
      Function_type* fntype = this->fn_->type()->function_type();
      size_t rc = this->args_->front()->call_expression()->result_count();
      if (rc > 1
	  && fntype->parameters() != NULL
	  && (fntype->parameters()->size() == rc
	      || (fntype->is_varargs()
		  && fntype->parameters()->size() - 1 <= rc)))
	{
	  Call_expression* call = this->args_->front()->call_expression();
	  Expression_list* args = new Expression_list;
	  for (size_t i = 0; i < rc; ++i)
	    args->push_back(Expression::make_call_result(call, i));
	  // We can't return a new call expression here, because this
	  // one may be referenced by Call_result expressions.  We
	  // also can't delete the old arguments, because we may still
	  // traverse them somewhere up the call stack.  FIXME.
	  this->args_ = args;
	}
    }

  // If this call returns multiple results, create a temporary
  // variable for each result.
  size_t rc = this->result_count();
  if (rc > 1 && this->results_ == NULL)
    {
      std::vector<Temporary_statement*>* temps =
	new std::vector<Temporary_statement*>;
      temps->reserve(rc);
      const Typed_identifier_list* results =
	this->fn_->type()->function_type()->results();
      for (Typed_identifier_list::const_iterator p = results->begin();
	   p != results->end();
	   ++p)
	{
	  Temporary_statement* temp = Statement::make_temporary(p->type(),
								NULL, loc);
	  inserter->insert(temp);
	  temps->push_back(temp);
	}
      this->results_ = temps;
    }

  // Handle a call to a varargs function by packaging up the extra
  // parameters.
  if (this->fn_->type()->function_type() != NULL
      && this->fn_->type()->function_type()->is_varargs())
    {
      Function_type* fntype = this->fn_->type()->function_type();
      const Typed_identifier_list* parameters = fntype->parameters();
      go_assert(parameters != NULL && !parameters->empty());
      Type* varargs_type = parameters->back().type();
      this->lower_varargs(gogo, function, inserter, varargs_type,
			  parameters->size());
    }

  // If this is call to a method, call the method directly passing the
  // object as the first parameter.
  Bound_method_expression* bme = this->fn_->bound_method_expression();
  if (bme != NULL)
    {
      Named_object* method = bme->method();
      Expression* first_arg = bme->first_argument();

      // We always pass a pointer when calling a method.
      if (first_arg->type()->points_to() == NULL
	  && !first_arg->type()->is_error())
	{
	  first_arg = Expression::make_unary(OPERATOR_AND, first_arg, loc);
	  // We may need to create a temporary variable so that we can
	  // take the address.  We can't do that here because it will
	  // mess up the order of evaluation.
	  Unary_expression* ue = static_cast<Unary_expression*>(first_arg);
	  ue->set_create_temp();
	}

      // If we are calling a method which was inherited from an
      // embedded struct, and the method did not get a stub, then the
      // first type may be wrong.
      Type* fatype = bme->first_argument_type();
      if (fatype != NULL)
	{
	  if (fatype->points_to() == NULL)
	    fatype = Type::make_pointer_type(fatype);
	  first_arg = Expression::make_unsafe_cast(fatype, first_arg, loc);
	}

      Expression_list* new_args = new Expression_list();
      new_args->push_back(first_arg);
      if (this->args_ != NULL)
	{
	  for (Expression_list::const_iterator p = this->args_->begin();
	       p != this->args_->end();
	       ++p)
	    new_args->push_back(*p);
	}

      // We have to change in place because this structure may be
      // referenced by Call_result_expressions.  We can't delete the
      // old arguments, because we may be traversing them up in some
      // caller.  FIXME.
      this->args_ = new_args;
      this->fn_ = Expression::make_func_reference(method, NULL,
						  bme->location());
    }

  return this;
}

// Lower a call to a varargs function.  FUNCTION is the function in
// which the call occurs--it's not the function we are calling.
// VARARGS_TYPE is the type of the varargs parameter, a slice type.
// PARAM_COUNT is the number of parameters of the function we are
// calling; the last of these parameters will be the varargs
// parameter.

void
Call_expression::lower_varargs(Gogo* gogo, Named_object* function,
			       Statement_inserter* inserter,
			       Type* varargs_type, size_t param_count)
{
  if (this->varargs_are_lowered_)
    return;

  Location loc = this->location();

  go_assert(param_count > 0);
  go_assert(varargs_type->is_slice_type());

  size_t arg_count = this->args_ == NULL ? 0 : this->args_->size();
  if (arg_count < param_count - 1)
    {
      // Not enough arguments; will be caught in check_types.
      return;
    }

  Expression_list* old_args = this->args_;
  Expression_list* new_args = new Expression_list();
  bool push_empty_arg = false;
  if (old_args == NULL || old_args->empty())
    {
      go_assert(param_count == 1);
      push_empty_arg = true;
    }
  else
    {
      Expression_list::const_iterator pa;
      int i = 1;
      for (pa = old_args->begin(); pa != old_args->end(); ++pa, ++i)
	{
	  if (static_cast<size_t>(i) == param_count)
	    break;
	  new_args->push_back(*pa);
	}

      // We have reached the varargs parameter.

      bool issued_error = false;
      if (pa == old_args->end())
	push_empty_arg = true;
      else if (pa + 1 == old_args->end() && this->is_varargs_)
	new_args->push_back(*pa);
      else if (this->is_varargs_)
	{
	  this->report_error(_("too many arguments"));
	  return;
	}
      else
	{
	  Type* element_type = varargs_type->array_type()->element_type();
	  Expression_list* vals = new Expression_list;
	  for (; pa != old_args->end(); ++pa, ++i)
	    {
	      // Check types here so that we get a better message.
	      Type* patype = (*pa)->type();
	      Location paloc = (*pa)->location();
	      if (!this->check_argument_type(i, element_type, patype,
					     paloc, issued_error))
		continue;
	      vals->push_back(*pa);
	    }
	  Expression* val =
	    Expression::make_slice_composite_literal(varargs_type, vals, loc);
	  gogo->lower_expression(function, inserter, &val);
	  new_args->push_back(val);
	}
    }

  if (push_empty_arg)
    new_args->push_back(Expression::make_nil(loc));

  // We can't return a new call expression here, because this one may
  // be referenced by Call_result expressions.  FIXME.  We can't
  // delete OLD_ARGS because we may have both a Call_expression and a
  // Builtin_call_expression which refer to them.  FIXME.
  this->args_ = new_args;
  this->varargs_are_lowered_ = true;
}

// Get the function type.  This can return NULL in error cases.

Function_type*
Call_expression::get_function_type() const
{
  return this->fn_->type()->function_type();
}

// Return the number of values which this call will return.

size_t
Call_expression::result_count() const
{
  const Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    return 0;
  if (fntype->results() == NULL)
    return 0;
  return fntype->results()->size();
}

// Return the temporary which holds a result.

Temporary_statement*
Call_expression::result(size_t i) const
{
  go_assert(this->results_ != NULL
	    && this->results_->size() > i);
  return (*this->results_)[i];
}

// Return whether this is a call to the predeclared function recover.

bool
Call_expression::is_recover_call() const
{
  return this->do_is_recover_call();
}

// Set the argument to the recover function.

void
Call_expression::set_recover_arg(Expression* arg)
{
  this->do_set_recover_arg(arg);
}

// Virtual functions also implemented by Builtin_call_expression.

bool
Call_expression::do_is_recover_call() const
{
  return false;
}

void
Call_expression::do_set_recover_arg(Expression*)
{
  go_unreachable();
}

// We have found an error with this call expression; return true if
// we should report it.

bool
Call_expression::issue_error()
{
  if (this->issued_error_)
    return false;
  else
    {
      this->issued_error_ = true;
      return true;
    }
}

// Get the type.

Type*
Call_expression::do_type()
{
  if (this->type_ != NULL)
    return this->type_;

  Type* ret;
  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    return Type::make_error_type();

  const Typed_identifier_list* results = fntype->results();
  if (results == NULL)
    ret = Type::make_void_type();
  else if (results->size() == 1)
    ret = results->begin()->type();
  else
    ret = Type::make_call_multiple_result_type(this);

  this->type_ = ret;

  return this->type_;
}

// Determine types for a call expression.  We can use the function
// parameter types to set the types of the arguments.

void
Call_expression::do_determine_type(const Type_context*)
{
  if (!this->determining_types())
    return;

  this->fn_->determine_type_no_context();
  Function_type* fntype = this->get_function_type();
  const Typed_identifier_list* parameters = NULL;
  if (fntype != NULL)
    parameters = fntype->parameters();
  if (this->args_ != NULL)
    {
      Typed_identifier_list::const_iterator pt;
      if (parameters != NULL)
	pt = parameters->begin();
      bool first = true;
      for (Expression_list::const_iterator pa = this->args_->begin();
	   pa != this->args_->end();
	   ++pa)
	{
	  if (first)
	    {
	      first = false;
	      // If this is a method, the first argument is the
	      // receiver.
	      if (fntype != NULL && fntype->is_method())
		{
		  Type* rtype = fntype->receiver()->type();
		  // The receiver is always passed as a pointer.
		  if (rtype->points_to() == NULL)
		    rtype = Type::make_pointer_type(rtype);
		  Type_context subcontext(rtype, false);
		  (*pa)->determine_type(&subcontext);
		  continue;
		}
	    }

	  if (parameters != NULL && pt != parameters->end())
	    {
	      Type_context subcontext(pt->type(), false);
	      (*pa)->determine_type(&subcontext);
	      ++pt;
	    }
	  else
	    (*pa)->determine_type_no_context();
	}
    }
}

// Called when determining types for a Call_expression.  Return true
// if we should go ahead, false if they have already been determined.

bool
Call_expression::determining_types()
{
  if (this->types_are_determined_)
    return false;
  else
    {
      this->types_are_determined_ = true;
      return true;
    }
}

// Check types for parameter I.

bool
Call_expression::check_argument_type(int i, const Type* parameter_type,
				     const Type* argument_type,
				     Location argument_location,
				     bool issued_error)
{
  std::string reason;
  bool ok;
  if (this->are_hidden_fields_ok_)
    ok = Type::are_assignable_hidden_ok(parameter_type, argument_type,
					&reason);
  else
    ok = Type::are_assignable(parameter_type, argument_type, &reason);
  if (!ok)
    {
      if (!issued_error)
	{
	  if (reason.empty())
	    error_at(argument_location, "argument %d has incompatible type", i);
	  else
	    error_at(argument_location,
		     "argument %d has incompatible type (%s)",
		     i, reason.c_str());
	}
      this->set_is_error();
      return false;
    }
  return true;
}

// Check types.

void
Call_expression::do_check_types(Gogo*)
{
  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    {
      if (!this->fn_->type()->is_error())
	this->report_error(_("expected function"));
      return;
    }

  bool is_method = fntype->is_method();
  if (is_method)
    {
      go_assert(this->args_ != NULL && !this->args_->empty());
      Type* rtype = fntype->receiver()->type();
      Expression* first_arg = this->args_->front();
      // The language permits copying hidden fields for a method
      // receiver.  We dereference the values since receivers are
      // always passed as pointers.
      std::string reason;
      if (!Type::are_assignable_hidden_ok(rtype->deref(),
					  first_arg->type()->deref(),
					  &reason))
	{
	  if (reason.empty())
	    this->report_error(_("incompatible type for receiver"));
	  else
	    {
	      error_at(this->location(),
		       "incompatible type for receiver (%s)",
		       reason.c_str());
	      this->set_is_error();
	    }
	}
    }

  // Note that varargs was handled by the lower_varargs() method, so
  // we don't have to worry about it here.

  const Typed_identifier_list* parameters = fntype->parameters();
  if (this->args_ == NULL)
    {
      if (parameters != NULL && !parameters->empty())
	this->report_error(_("not enough arguments"));
    }
  else if (parameters == NULL)
    {
      if (!is_method || this->args_->size() > 1)
	this->report_error(_("too many arguments"));
    }
  else
    {
      int i = 0;
      Expression_list::const_iterator pa = this->args_->begin();
      if (is_method)
	++pa;
      for (Typed_identifier_list::const_iterator pt = parameters->begin();
	   pt != parameters->end();
	   ++pt, ++pa, ++i)
	{
	  if (pa == this->args_->end())
	    {
	      this->report_error(_("not enough arguments"));
	      return;
	    }
	  this->check_argument_type(i + 1, pt->type(), (*pa)->type(),
				    (*pa)->location(), false);
	}
      if (pa != this->args_->end())
	this->report_error(_("too many arguments"));
    }
}

// Return whether we have to use a temporary variable to ensure that
// we evaluate this call expression in order.  If the call returns no
// results then it will inevitably be executed last.

bool
Call_expression::do_must_eval_in_order() const
{
  return this->result_count() > 0;
}

// Get the function and the first argument to use when calling an
// interface method.

tree
Call_expression::interface_method_function(
    Translate_context* context,
    Interface_field_reference_expression* interface_method,
    tree* first_arg_ptr)
{
  tree expr = interface_method->expr()->get_tree(context);
  if (expr == error_mark_node)
    return error_mark_node;
  expr = save_expr(expr);
  tree first_arg = interface_method->get_underlying_object_tree(context, expr);
  if (first_arg == error_mark_node)
    return error_mark_node;
  *first_arg_ptr = first_arg;
  return interface_method->get_function_tree(context, expr);
}

// Build the call expression.

tree
Call_expression::do_get_tree(Translate_context* context)
{
  if (this->tree_ != NULL_TREE)
    return this->tree_;

  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    return error_mark_node;

  if (this->fn_->is_error_expression())
    return error_mark_node;

  Gogo* gogo = context->gogo();
  Location location = this->location();

  Func_expression* func = this->fn_->func_expression();
  Interface_field_reference_expression* interface_method =
    this->fn_->interface_field_reference_expression();
  const bool has_closure = func != NULL && func->closure() != NULL;
  const bool is_interface_method = interface_method != NULL;

  int nargs;
  tree* args;
  if (this->args_ == NULL || this->args_->empty())
    {
      nargs = is_interface_method ? 1 : 0;
      args = nargs == 0 ? NULL : new tree[nargs];
    }
  else if (fntype->parameters() == NULL || fntype->parameters()->empty())
    {
      // Passing a receiver parameter.
      go_assert(!is_interface_method
		&& fntype->is_method()
		&& this->args_->size() == 1);
      nargs = 1;
      args = new tree[nargs];
      args[0] = this->args_->front()->get_tree(context);
    }
  else
    {
      const Typed_identifier_list* params = fntype->parameters();

      nargs = this->args_->size();
      int i = is_interface_method ? 1 : 0;
      nargs += i;
      args = new tree[nargs];

      Typed_identifier_list::const_iterator pp = params->begin();
      Expression_list::const_iterator pe = this->args_->begin();
      if (!is_interface_method && fntype->is_method())
	{
	  args[i] = (*pe)->get_tree(context);
	  ++pe;
	  ++i;
	}
      for (; pe != this->args_->end(); ++pe, ++pp, ++i)
	{
	  go_assert(pp != params->end());
	  tree arg_val = (*pe)->get_tree(context);
	  args[i] = Expression::convert_for_assignment(context,
						       pp->type(),
						       (*pe)->type(),
						       arg_val,
						       location);
	  if (args[i] == error_mark_node)
	    {
	      delete[] args;
	      return error_mark_node;
	    }
	}
      go_assert(pp == params->end());
      go_assert(i == nargs);
    }

  tree rettype = TREE_TYPE(TREE_TYPE(type_to_tree(fntype->get_backend(gogo))));
  if (rettype == error_mark_node)
    {
      delete[] args;
      return error_mark_node;
    }

  tree fn;
  if (has_closure)
    fn = func->get_tree_without_closure(gogo);
  else if (!is_interface_method)
    fn = this->fn_->get_tree(context);
  else
    fn = this->interface_method_function(context, interface_method, &args[0]);

  if (fn == error_mark_node || TREE_TYPE(fn) == error_mark_node)
    {
      delete[] args;
      return error_mark_node;
    }

  tree fndecl = fn;
  if (TREE_CODE(fndecl) == ADDR_EXPR)
    fndecl = TREE_OPERAND(fndecl, 0);

  // Add a type cast in case the type of the function is a recursive
  // type which refers to itself.
  if (!DECL_P(fndecl) || !DECL_IS_BUILTIN(fndecl))
    {
      tree fnt = type_to_tree(fntype->get_backend(gogo));
      if (fnt == error_mark_node)
	return error_mark_node;
      fn = fold_convert_loc(location.gcc_location(), fnt, fn);
    }

  // This is to support builtin math functions when using 80387 math.
  tree excess_type = NULL_TREE;
  if (TREE_CODE(fndecl) == FUNCTION_DECL
      && DECL_IS_BUILTIN(fndecl)
      && DECL_BUILT_IN_CLASS(fndecl) == BUILT_IN_NORMAL
      && nargs > 0
      && ((SCALAR_FLOAT_TYPE_P(rettype)
	   && SCALAR_FLOAT_TYPE_P(TREE_TYPE(args[0])))
	  || (COMPLEX_FLOAT_TYPE_P(rettype)
	      && COMPLEX_FLOAT_TYPE_P(TREE_TYPE(args[0])))))
    {
      excess_type = excess_precision_type(TREE_TYPE(args[0]));
      if (excess_type != NULL_TREE)
	{
	  tree excess_fndecl = mathfn_built_in(excess_type,
					       DECL_FUNCTION_CODE(fndecl));
	  if (excess_fndecl == NULL_TREE)
	    excess_type = NULL_TREE;
	  else
	    {
	      fn = build_fold_addr_expr_loc(location.gcc_location(),
                                            excess_fndecl);
	      for (int i = 0; i < nargs; ++i)
		args[i] = ::convert(excess_type, args[i]);
	    }
	}
    }

  tree ret = build_call_array(excess_type != NULL_TREE ? excess_type : rettype,
			      fn, nargs, args);
  delete[] args;

  SET_EXPR_LOCATION(ret, location.gcc_location());

  if (has_closure)
    {
      tree closure_tree = func->closure()->get_tree(context);
      if (closure_tree != error_mark_node)
	CALL_EXPR_STATIC_CHAIN(ret) = closure_tree;
    }

  // If this is a recursive function type which returns itself, as in
  //   type F func() F
  // we have used ptr_type_node for the return type.  Add a cast here
  // to the correct type.
  if (TREE_TYPE(ret) == ptr_type_node)
    {
      tree t = type_to_tree(this->type()->base()->get_backend(gogo));
      ret = fold_convert_loc(location.gcc_location(), t, ret);
    }

  if (excess_type != NULL_TREE)
    {
      // Calling convert here can undo our excess precision change.
      // That may or may not be a bug in convert_to_real.
      ret = build1(NOP_EXPR, rettype, ret);
    }

  if (this->results_ != NULL)
    ret = this->set_results(context, ret);

  this->tree_ = ret;

  return ret;
}

// Set the result variables if this call returns multiple results.

tree
Call_expression::set_results(Translate_context* context, tree call_tree)
{
  tree stmt_list = NULL_TREE;

  call_tree = save_expr(call_tree);

  if (TREE_CODE(TREE_TYPE(call_tree)) != RECORD_TYPE)
    {
      go_assert(saw_errors());
      return call_tree;
    }

  Location loc = this->location();
  tree field = TYPE_FIELDS(TREE_TYPE(call_tree));
  size_t rc = this->result_count();
  for (size_t i = 0; i < rc; ++i, field = DECL_CHAIN(field))
    {
      go_assert(field != NULL_TREE);

      Temporary_statement* temp = this->result(i);
      Temporary_reference_expression* ref =
	Expression::make_temporary_reference(temp, loc);
      ref->set_is_lvalue();
      tree temp_tree = ref->get_tree(context);
      if (temp_tree == error_mark_node)
	continue;

      tree val_tree = build3_loc(loc.gcc_location(), COMPONENT_REF,
                                 TREE_TYPE(field), call_tree, field, NULL_TREE);
      tree set_tree = build2_loc(loc.gcc_location(), MODIFY_EXPR,
                                 void_type_node, temp_tree, val_tree);

      append_to_statement_list(set_tree, &stmt_list);
    }
  go_assert(field == NULL_TREE);

  return save_expr(stmt_list);
}

// Dump ast representation for a call expressin.

void
Call_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  this->fn_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << "(";
  if (args_ != NULL)
    ast_dump_context->dump_expression_list(this->args_);

  ast_dump_context->ostream() << ") ";
}

// Make a call expression.

Call_expression*
Expression::make_call(Expression* fn, Expression_list* args, bool is_varargs,
		      Location location)
{
  return new Call_expression(fn, args, is_varargs, location);
}

// A single result from a call which returns multiple results.

class Call_result_expression : public Expression
{
 public:
  Call_result_expression(Call_expression* call, unsigned int index)
    : Expression(EXPRESSION_CALL_RESULT, call->location()),
      call_(call), index_(index)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return new Call_result_expression(this->call_->call_expression(),
				      this->index_);
  }

  bool
  do_must_eval_in_order() const
  { return true; }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The underlying call expression.
  Expression* call_;
  // Which result we want.
  unsigned int index_;
};

// Traverse a call result.

int
Call_result_expression::do_traverse(Traverse* traverse)
{
  if (traverse->remember_expression(this->call_))
    {
      // We have already traversed the call expression.
      return TRAVERSE_CONTINUE;
    }
  return Expression::traverse(&this->call_, traverse);
}

// Get the type.

Type*
Call_result_expression::do_type()
{
  if (this->classification() == EXPRESSION_ERROR)
    return Type::make_error_type();

  // THIS->CALL_ can be replaced with a temporary reference due to
  // Call_expression::do_must_eval_in_order when there is an error.
  Call_expression* ce = this->call_->call_expression();
  if (ce == NULL)
    {
      this->set_is_error();
      return Type::make_error_type();
    }
  Function_type* fntype = ce->get_function_type();
  if (fntype == NULL)
    {
      if (ce->issue_error())
	{
	  if (!ce->fn()->type()->is_error())
	    this->report_error(_("expected function"));
	}
      this->set_is_error();
      return Type::make_error_type();
    }
  const Typed_identifier_list* results = fntype->results();
  if (results == NULL || results->size() < 2)
    {
      if (ce->issue_error())
	this->report_error(_("number of results does not match "
			     "number of values"));
      return Type::make_error_type();
    }
  Typed_identifier_list::const_iterator pr = results->begin();
  for (unsigned int i = 0; i < this->index_; ++i)
    {
      if (pr == results->end())
	break;
      ++pr;
    }
  if (pr == results->end())
    {
      if (ce->issue_error())
	this->report_error(_("number of results does not match "
			     "number of values"));
      return Type::make_error_type();
    }
  return pr->type();
}

// Check the type.  Just make sure that we trigger the warning in
// do_type.

void
Call_result_expression::do_check_types(Gogo*)
{
  this->type();
}

// Determine the type.  We have nothing to do here, but the 0 result
// needs to pass down to the caller.

void
Call_result_expression::do_determine_type(const Type_context*)
{
  this->call_->determine_type_no_context();
}

// Return the tree.  We just refer to the temporary set by the call
// expression.  We don't do this at lowering time because it makes it
// hard to evaluate the call at the right time.

tree
Call_result_expression::do_get_tree(Translate_context* context)
{
  Call_expression* ce = this->call_->call_expression();
  go_assert(ce != NULL);
  Temporary_statement* ts = ce->result(this->index_);
  Expression* ref = Expression::make_temporary_reference(ts, this->location());
  return ref->get_tree(context);
}

// Dump ast representation for a call result expression.

void
Call_result_expression::do_dump_expression(Ast_dump_context* ast_dump_context)
    const
{
  // FIXME: Wouldn't it be better if the call is assigned to a temporary 
  // (struct) and the fields are referenced instead.
  ast_dump_context->ostream() << this->index_ << "@(";
  ast_dump_context->dump_expression(this->call_);
  ast_dump_context->ostream() << ")";
}

// Make a reference to a single result of a call which returns
// multiple results.

Expression*
Expression::make_call_result(Call_expression* call, unsigned int index)
{
  return new Call_result_expression(call, index);
}

// Class Index_expression.

// Traversal.

int
Index_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->left_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->start_, traverse) == TRAVERSE_EXIT
      || (this->end_ != NULL
	  && Expression::traverse(&this->end_, traverse) == TRAVERSE_EXIT))
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Lower an index expression.  This converts the generic index
// expression into an array index, a string index, or a map index.

Expression*
Index_expression::do_lower(Gogo*, Named_object*, Statement_inserter*, int)
{
  Location location = this->location();
  Expression* left = this->left_;
  Expression* start = this->start_;
  Expression* end = this->end_;

  Type* type = left->type();
  if (type->is_error())
    return Expression::make_error(location);
  else if (left->is_type_expression())
    {
      error_at(location, "attempt to index type expression");
      return Expression::make_error(location);
    }
  else if (type->array_type() != NULL)
    return Expression::make_array_index(left, start, end, location);
  else if (type->points_to() != NULL
	   && type->points_to()->array_type() != NULL
	   && !type->points_to()->is_slice_type())
    {
      Expression* deref = Expression::make_unary(OPERATOR_MULT, left,
						 location);
      return Expression::make_array_index(deref, start, end, location);
    }
  else if (type->is_string_type())
    return Expression::make_string_index(left, start, end, location);
  else if (type->map_type() != NULL)
    {
      if (end != NULL)
	{
	  error_at(location, "invalid slice of map");
	  return Expression::make_error(location);
	}
      Map_index_expression* ret = Expression::make_map_index(left, start,
							     location);
      if (this->is_lvalue_)
	ret->set_is_lvalue();
      return ret;
    }
  else
    {
      error_at(location,
	       "attempt to index object which is not array, string, or map");
      return Expression::make_error(location);
    }
}

// Write an indexed expression (expr[expr:expr] or expr[expr]) to a
// dump context

void
Index_expression::dump_index_expression(Ast_dump_context* ast_dump_context, 
					const Expression* expr, 
					const Expression* start,
					const Expression* end)
{
  expr->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << "[";
  start->dump_expression(ast_dump_context);
  if (end != NULL)
    {
      ast_dump_context->ostream() << ":";
      end->dump_expression(ast_dump_context);
    }
  ast_dump_context->ostream() << "]";
}

// Dump ast representation for an index expression.

void
Index_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->left_, 
                                          this->start_, this->end_);
}

// Make an index expression.

Expression*
Expression::make_index(Expression* left, Expression* start, Expression* end,
		       Location location)
{
  return new Index_expression(left, start, end, location);
}

// An array index.  This is used for both indexing and slicing.

class Array_index_expression : public Expression
{
 public:
  Array_index_expression(Expression* array, Expression* start,
			 Expression* end, Location location)
    : Expression(EXPRESSION_ARRAY_INDEX, location),
      array_(array), start_(start), end_(end), type_(NULL)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_array_index(this->array_->copy(),
					this->start_->copy(),
					(this->end_ == NULL
					 ? NULL
					 : this->end_->copy()),
					this->location());
  }

  bool
  do_must_eval_subexpressions_in_order(int* skip) const
  {
    *skip = 1;
    return true;
  }

  bool
  do_is_addressable() const;

  void
  do_address_taken(bool escapes)
  { this->array_->address_taken(escapes); }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  // The array we are getting a value from.
  Expression* array_;
  // The start or only index.
  Expression* start_;
  // The end index of a slice.  This may be NULL for a simple array
  // index, or it may be a nil expression for the length of the array.
  Expression* end_;
  // The type of the expression.
  Type* type_;
};

// Array index traversal.

int
Array_index_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->array_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Expression::traverse(&this->start_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->end_ != NULL)
    {
      if (Expression::traverse(&this->end_, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Return the type of an array index.

Type*
Array_index_expression::do_type()
{
  if (this->type_ == NULL)
    {
     Array_type* type = this->array_->type()->array_type();
      if (type == NULL)
	this->type_ = Type::make_error_type();
      else if (this->end_ == NULL)
	this->type_ = type->element_type();
      else if (type->is_slice_type())
	{
	  // A slice of a slice has the same type as the original
	  // slice.
	  this->type_ = this->array_->type()->deref();
	}
      else
	{
	  // A slice of an array is a slice.
	  this->type_ = Type::make_array_type(type->element_type(), NULL);
	}
    }
  return this->type_;
}

// Set the type of an array index.

void
Array_index_expression::do_determine_type(const Type_context*)
{
  this->array_->determine_type_no_context();
  this->start_->determine_type_no_context();
  if (this->end_ != NULL)
    this->end_->determine_type_no_context();
}

// Check types of an array index.

void
Array_index_expression::do_check_types(Gogo*)
{
  if (this->start_->type()->integer_type() == NULL)
    this->report_error(_("index must be integer"));
  if (this->end_ != NULL
      && this->end_->type()->integer_type() == NULL
      && !this->end_->type()->is_error()
      && !this->end_->is_nil_expression()
      && !this->end_->is_error_expression())
    this->report_error(_("slice end must be integer"));

  Array_type* array_type = this->array_->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->array_->type()->is_error());
      return;
    }

  unsigned int int_bits =
    Type::lookup_integer_type("int")->integer_type()->bits();

  Type* dummy;
  mpz_t lval;
  mpz_init(lval);
  bool lval_valid = (array_type->length() != NULL
		     && array_type->length()->integer_constant_value(true,
								     lval,
								     &dummy));
  mpz_t ival;
  mpz_init(ival);
  if (this->start_->integer_constant_value(true, ival, &dummy))
    {
      if (mpz_sgn(ival) < 0
	  || mpz_sizeinbase(ival, 2) >= int_bits
	  || (lval_valid
	      && (this->end_ == NULL
		  ? mpz_cmp(ival, lval) >= 0
		  : mpz_cmp(ival, lval) > 0)))
	{
	  error_at(this->start_->location(), "array index out of bounds");
	  this->set_is_error();
	}
    }
  if (this->end_ != NULL && !this->end_->is_nil_expression())
    {
      if (this->end_->integer_constant_value(true, ival, &dummy))
	{
	  if (mpz_sgn(ival) < 0
	      || mpz_sizeinbase(ival, 2) >= int_bits
	      || (lval_valid && mpz_cmp(ival, lval) > 0))
	    {
	      error_at(this->end_->location(), "array index out of bounds");
	      this->set_is_error();
	    }
	}
    }
  mpz_clear(ival);
  mpz_clear(lval);

  // A slice of an array requires an addressable array.  A slice of a
  // slice is always possible.
  if (this->end_ != NULL && !array_type->is_slice_type())
    {
      if (!this->array_->is_addressable())
	this->report_error(_("array is not addressable"));
      else
	this->array_->address_taken(true);
    }
}

// Return whether this expression is addressable.

bool
Array_index_expression::do_is_addressable() const
{
  // A slice expression is not addressable.
  if (this->end_ != NULL)
    return false;

  // An index into a slice is addressable.
  if (this->array_->type()->is_slice_type())
    return true;

  // An index into an array is addressable if the array is
  // addressable.
  return this->array_->is_addressable();
}

// Get a tree for an array index.

tree
Array_index_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location loc = this->location();

  Array_type* array_type = this->array_->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->array_->type()->is_error());
      return error_mark_node;
    }

  tree type_tree = type_to_tree(array_type->get_backend(gogo));
  if (type_tree == error_mark_node)
    return error_mark_node;

  tree array_tree = this->array_->get_tree(context);
  if (array_tree == error_mark_node)
    return error_mark_node;

  if (array_type->length() == NULL && !DECL_P(array_tree))
    array_tree = save_expr(array_tree);
  tree length_tree = array_type->length_tree(gogo, array_tree);
  if (length_tree == error_mark_node)
    return error_mark_node;
  length_tree = save_expr(length_tree);
  tree length_type = TREE_TYPE(length_tree);

  tree bad_index = boolean_false_node;

  tree start_tree = this->start_->get_tree(context);
  if (start_tree == error_mark_node)
    return error_mark_node;
  if (!DECL_P(start_tree))
    start_tree = save_expr(start_tree);
  if (!INTEGRAL_TYPE_P(TREE_TYPE(start_tree)))
    start_tree = convert_to_integer(length_type, start_tree);

  bad_index = Expression::check_bounds(start_tree, length_type, bad_index,
				       loc);

  start_tree = fold_convert_loc(loc.gcc_location(), length_type, start_tree);
  bad_index = fold_build2_loc(loc.gcc_location(), TRUTH_OR_EXPR,
                              boolean_type_node, bad_index,
			      fold_build2_loc(loc.gcc_location(),
					      (this->end_ == NULL
					       ? GE_EXPR
					       : GT_EXPR),
					      boolean_type_node, start_tree,
					      length_tree));

  int code = (array_type->length() != NULL
	      ? (this->end_ == NULL
		 ? RUNTIME_ERROR_ARRAY_INDEX_OUT_OF_BOUNDS
		 : RUNTIME_ERROR_ARRAY_SLICE_OUT_OF_BOUNDS)
	      : (this->end_ == NULL
		 ? RUNTIME_ERROR_SLICE_INDEX_OUT_OF_BOUNDS
		 : RUNTIME_ERROR_SLICE_SLICE_OUT_OF_BOUNDS));
  tree crash = Gogo::runtime_error(code, loc);

  if (this->end_ == NULL)
    {
      // Simple array indexing.  This has to return an l-value, so
      // wrap the index check into START_TREE.
      start_tree = build2(COMPOUND_EXPR, TREE_TYPE(start_tree),
			  build3(COND_EXPR, void_type_node,
				 bad_index, crash, NULL_TREE),
			  start_tree);
      start_tree = fold_convert_loc(loc.gcc_location(), sizetype, start_tree);

      if (array_type->length() != NULL)
	{
	  // Fixed array.
	  return build4(ARRAY_REF, TREE_TYPE(type_tree), array_tree,
			start_tree, NULL_TREE, NULL_TREE);
	}
      else
	{
	  // Open array.
	  tree values = array_type->value_pointer_tree(gogo, array_tree);
	  Type* element_type = array_type->element_type();
	  Btype* belement_type = element_type->get_backend(gogo);
	  tree element_type_tree = type_to_tree(belement_type);
	  if (element_type_tree == error_mark_node)
	    return error_mark_node;
	  tree element_size = TYPE_SIZE_UNIT(element_type_tree);
	  tree offset = fold_build2_loc(loc.gcc_location(), MULT_EXPR, sizetype,
					start_tree, element_size);
	  tree ptr = fold_build2_loc(loc.gcc_location(), POINTER_PLUS_EXPR,
				     TREE_TYPE(values), values, offset);
	  return build_fold_indirect_ref(ptr);
	}
    }

  // Array slice.

  tree capacity_tree = array_type->capacity_tree(gogo, array_tree);
  if (capacity_tree == error_mark_node)
    return error_mark_node;
  capacity_tree = fold_convert_loc(loc.gcc_location(), length_type,
                                   capacity_tree);

  tree end_tree;
  if (this->end_->is_nil_expression())
    end_tree = length_tree;
  else
    {
      end_tree = this->end_->get_tree(context);
      if (end_tree == error_mark_node)
	return error_mark_node;
      if (!DECL_P(end_tree))
	end_tree = save_expr(end_tree);
      if (!INTEGRAL_TYPE_P(TREE_TYPE(end_tree)))
	end_tree = convert_to_integer(length_type, end_tree);

      bad_index = Expression::check_bounds(end_tree, length_type, bad_index,
					   loc);

      end_tree = fold_convert_loc(loc.gcc_location(), length_type, end_tree);

      capacity_tree = save_expr(capacity_tree);
      tree bad_end = fold_build2_loc(loc.gcc_location(), TRUTH_OR_EXPR,
                                     boolean_type_node,
				     fold_build2_loc(loc.gcc_location(),
                                                     LT_EXPR, boolean_type_node,
						     end_tree, start_tree),
				     fold_build2_loc(loc.gcc_location(),
                                                     GT_EXPR, boolean_type_node,
						     end_tree, capacity_tree));
      bad_index = fold_build2_loc(loc.gcc_location(), TRUTH_OR_EXPR,
                                  boolean_type_node, bad_index, bad_end);
    }

  Type* element_type = array_type->element_type();
  tree element_type_tree = type_to_tree(element_type->get_backend(gogo));
  if (element_type_tree == error_mark_node)
    return error_mark_node;
  tree element_size = TYPE_SIZE_UNIT(element_type_tree);

  tree offset = fold_build2_loc(loc.gcc_location(), MULT_EXPR, sizetype,
				fold_convert_loc(loc.gcc_location(), sizetype,
                                                 start_tree),
				element_size);

  tree value_pointer = array_type->value_pointer_tree(gogo, array_tree);
  if (value_pointer == error_mark_node)
    return error_mark_node;

  value_pointer = fold_build2_loc(loc.gcc_location(), POINTER_PLUS_EXPR,
				  TREE_TYPE(value_pointer),
				  value_pointer, offset);

  tree result_length_tree = fold_build2_loc(loc.gcc_location(), MINUS_EXPR,
                                            length_type, end_tree, start_tree);

  tree result_capacity_tree = fold_build2_loc(loc.gcc_location(), MINUS_EXPR,
                                              length_type, capacity_tree,
                                              start_tree);

  tree struct_tree = type_to_tree(this->type()->get_backend(gogo));
  go_assert(TREE_CODE(struct_tree) == RECORD_TYPE);

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 3);

  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  tree field = TYPE_FIELDS(struct_tree);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__values") == 0);
  elt->index = field;
  elt->value = value_pointer;

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__count") == 0);
  elt->index = field;
  elt->value = fold_convert_loc(loc.gcc_location(), TREE_TYPE(field),
                                result_length_tree);

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__capacity") == 0);
  elt->index = field;
  elt->value = fold_convert_loc(loc.gcc_location(), TREE_TYPE(field),
                                result_capacity_tree);

  tree constructor = build_constructor(struct_tree, init);

  if (TREE_CONSTANT(value_pointer)
      && TREE_CONSTANT(result_length_tree)
      && TREE_CONSTANT(result_capacity_tree))
    TREE_CONSTANT(constructor) = 1;

  return fold_build2_loc(loc.gcc_location(), COMPOUND_EXPR,
                         TREE_TYPE(constructor),
			 build3(COND_EXPR, void_type_node,
				bad_index, crash, NULL_TREE),
			 constructor);
}

// Dump ast representation for an array index expression.

void
Array_index_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->array_, 
                                          this->start_, this->end_);
}

// Make an array index expression.  END may be NULL.

Expression*
Expression::make_array_index(Expression* array, Expression* start,
			     Expression* end, Location location)
{
  // Taking a slice of a composite literal requires moving the literal
  // onto the heap.
  if (end != NULL && array->is_composite_literal())
    {
      array = Expression::make_heap_composite(array, location);
      array = Expression::make_unary(OPERATOR_MULT, array, location);
    }
  return new Array_index_expression(array, start, end, location);
}

// A string index.  This is used for both indexing and slicing.

class String_index_expression : public Expression
{
 public:
  String_index_expression(Expression* string, Expression* start,
			  Expression* end, Location location)
    : Expression(EXPRESSION_STRING_INDEX, location),
      string_(string), start_(start), end_(end)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return Expression::make_string_index(this->string_->copy(),
					 this->start_->copy(),
					 (this->end_ == NULL
					  ? NULL
					  : this->end_->copy()),
					 this->location());
  }

  bool
  do_must_eval_subexpressions_in_order(int* skip) const
  {
    *skip = 1;
    return true;
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The string we are getting a value from.
  Expression* string_;
  // The start or only index.
  Expression* start_;
  // The end index of a slice.  This may be NULL for a single index,
  // or it may be a nil expression for the length of the string.
  Expression* end_;
};

// String index traversal.

int
String_index_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->string_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Expression::traverse(&this->start_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (this->end_ != NULL)
    {
      if (Expression::traverse(&this->end_, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Return the type of a string index.

Type*
String_index_expression::do_type()
{
  if (this->end_ == NULL)
    return Type::lookup_integer_type("uint8");
  else
    return this->string_->type();
}

// Determine the type of a string index.

void
String_index_expression::do_determine_type(const Type_context*)
{
  this->string_->determine_type_no_context();
  this->start_->determine_type_no_context();
  if (this->end_ != NULL)
    this->end_->determine_type_no_context();
}

// Check types of a string index.

void
String_index_expression::do_check_types(Gogo*)
{
  if (this->start_->type()->integer_type() == NULL)
    this->report_error(_("index must be integer"));
  if (this->end_ != NULL
      && this->end_->type()->integer_type() == NULL
      && !this->end_->is_nil_expression())
    this->report_error(_("slice end must be integer"));

  std::string sval;
  bool sval_valid = this->string_->string_constant_value(&sval);

  mpz_t ival;
  mpz_init(ival);
  Type* dummy;
  if (this->start_->integer_constant_value(true, ival, &dummy))
    {
      if (mpz_sgn(ival) < 0
	  || (sval_valid && mpz_cmp_ui(ival, sval.length()) >= 0))
	{
	  error_at(this->start_->location(), "string index out of bounds");
	  this->set_is_error();
	}
    }
  if (this->end_ != NULL && !this->end_->is_nil_expression())
    {
      if (this->end_->integer_constant_value(true, ival, &dummy))
	{
	  if (mpz_sgn(ival) < 0
	      || (sval_valid && mpz_cmp_ui(ival, sval.length()) > 0))
	    {
	      error_at(this->end_->location(), "string index out of bounds");
	      this->set_is_error();
	    }
	}
    }
  mpz_clear(ival);
}

// Get a tree for a string index.

tree
String_index_expression::do_get_tree(Translate_context* context)
{
  Location loc = this->location();

  tree string_tree = this->string_->get_tree(context);
  if (string_tree == error_mark_node)
    return error_mark_node;

  if (this->string_->type()->points_to() != NULL)
    string_tree = build_fold_indirect_ref(string_tree);
  if (!DECL_P(string_tree))
    string_tree = save_expr(string_tree);
  tree string_type = TREE_TYPE(string_tree);

  tree length_tree = String_type::length_tree(context->gogo(), string_tree);
  length_tree = save_expr(length_tree);
  tree length_type = TREE_TYPE(length_tree);

  tree bad_index = boolean_false_node;

  tree start_tree = this->start_->get_tree(context);
  if (start_tree == error_mark_node)
    return error_mark_node;
  if (!DECL_P(start_tree))
    start_tree = save_expr(start_tree);
  if (!INTEGRAL_TYPE_P(TREE_TYPE(start_tree)))
    start_tree = convert_to_integer(length_type, start_tree);

  bad_index = Expression::check_bounds(start_tree, length_type, bad_index,
				       loc);

  start_tree = fold_convert_loc(loc.gcc_location(), length_type, start_tree);

  int code = (this->end_ == NULL
	      ? RUNTIME_ERROR_STRING_INDEX_OUT_OF_BOUNDS
	      : RUNTIME_ERROR_STRING_SLICE_OUT_OF_BOUNDS);
  tree crash = Gogo::runtime_error(code, loc);

  if (this->end_ == NULL)
    {
      bad_index = fold_build2_loc(loc.gcc_location(), TRUTH_OR_EXPR,
                                  boolean_type_node, bad_index,
				  fold_build2_loc(loc.gcc_location(), GE_EXPR,
						  boolean_type_node,
						  start_tree, length_tree));

      tree bytes_tree = String_type::bytes_tree(context->gogo(), string_tree);
      tree ptr = fold_build2_loc(loc.gcc_location(), POINTER_PLUS_EXPR,
                                 TREE_TYPE(bytes_tree),
				 bytes_tree,
				 fold_convert_loc(loc.gcc_location(), sizetype,
                                                  start_tree));
      tree index = build_fold_indirect_ref_loc(loc.gcc_location(), ptr);

      return build2(COMPOUND_EXPR, TREE_TYPE(index),
		    build3(COND_EXPR, void_type_node,
			   bad_index, crash, NULL_TREE),
		    index);
    }
  else
    {
      tree end_tree;
      if (this->end_->is_nil_expression())
	end_tree = build_int_cst(length_type, -1);
      else
	{
	  end_tree = this->end_->get_tree(context);
	  if (end_tree == error_mark_node)
	    return error_mark_node;
	  if (!DECL_P(end_tree))
	    end_tree = save_expr(end_tree);
	  if (!INTEGRAL_TYPE_P(TREE_TYPE(end_tree)))
	    end_tree = convert_to_integer(length_type, end_tree);

	  bad_index = Expression::check_bounds(end_tree, length_type,
					       bad_index, loc);

	  end_tree = fold_convert_loc(loc.gcc_location(), length_type,
                                      end_tree);
	}

      static tree strslice_fndecl;
      tree ret = Gogo::call_builtin(&strslice_fndecl,
				    loc,
				    "__go_string_slice",
				    3,
				    string_type,
				    string_type,
				    string_tree,
				    length_type,
				    start_tree,
				    length_type,
				    end_tree);
      if (ret == error_mark_node)
	return error_mark_node;
      // This will panic if the bounds are out of range for the
      // string.
      TREE_NOTHROW(strslice_fndecl) = 0;

      if (bad_index == boolean_false_node)
	return ret;
      else
	return build2(COMPOUND_EXPR, TREE_TYPE(ret),
		      build3(COND_EXPR, void_type_node,
			     bad_index, crash, NULL_TREE),
		      ret);
    }
}

// Dump ast representation for a string index expression.

void
String_index_expression::do_dump_expression(Ast_dump_context* ast_dump_context)
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->string_, 
					  this->start_, this->end_);
}

// Make a string index expression.  END may be NULL.

Expression*
Expression::make_string_index(Expression* string, Expression* start,
			      Expression* end, Location location)
{
  return new String_index_expression(string, start, end, location);
}

// Class Map_index.

// Get the type of the map.

Map_type*
Map_index_expression::get_map_type() const
{
  Map_type* mt = this->map_->type()->deref()->map_type();
  if (mt == NULL)
    go_assert(saw_errors());
  return mt;
}

// Map index traversal.

int
Map_index_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->map_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return Expression::traverse(&this->index_, traverse);
}

// Return the type of a map index.

Type*
Map_index_expression::do_type()
{
  Map_type* mt = this->get_map_type();
  if (mt == NULL)
    return Type::make_error_type();
  Type* type = mt->val_type();
  // If this map index is in a tuple assignment, we actually return a
  // pointer to the value type.  Tuple_map_assignment_statement is
  // responsible for handling this correctly.  We need to get the type
  // right in case this gets assigned to a temporary variable.
  if (this->is_in_tuple_assignment_)
    type = Type::make_pointer_type(type);
  return type;
}

// Fix the type of a map index.

void
Map_index_expression::do_determine_type(const Type_context*)
{
  this->map_->determine_type_no_context();
  Map_type* mt = this->get_map_type();
  Type* key_type = mt == NULL ? NULL : mt->key_type();
  Type_context subcontext(key_type, false);
  this->index_->determine_type(&subcontext);
}

// Check types of a map index.

void
Map_index_expression::do_check_types(Gogo*)
{
  std::string reason;
  Map_type* mt = this->get_map_type();
  if (mt == NULL)
    return;
  if (!Type::are_assignable(mt->key_type(), this->index_->type(), &reason))
    {
      if (reason.empty())
	this->report_error(_("incompatible type for map index"));
      else
	{
	  error_at(this->location(), "incompatible type for map index (%s)",
		   reason.c_str());
	  this->set_is_error();
	}
    }
}

// Get a tree for a map index.

tree
Map_index_expression::do_get_tree(Translate_context* context)
{
  Map_type* type = this->get_map_type();
  if (type == NULL)
    return error_mark_node;

  tree valptr = this->get_value_pointer(context, this->is_lvalue_);
  if (valptr == error_mark_node)
    return error_mark_node;
  valptr = save_expr(valptr);

  tree val_type_tree = TREE_TYPE(TREE_TYPE(valptr));

  if (this->is_lvalue_)
    return build_fold_indirect_ref(valptr);
  else if (this->is_in_tuple_assignment_)
    {
      // Tuple_map_assignment_statement is responsible for using this
      // appropriately.
      return valptr;
    }
  else
    {
      Gogo* gogo = context->gogo();
      Btype* val_btype = type->val_type()->get_backend(gogo);
      Bexpression* val_zero = gogo->backend()->zero_expression(val_btype);
      return fold_build3(COND_EXPR, val_type_tree,
			 fold_build2(EQ_EXPR, boolean_type_node, valptr,
				     fold_convert(TREE_TYPE(valptr),
						  null_pointer_node)),
			 expr_to_tree(val_zero),
			 build_fold_indirect_ref(valptr));
    }
}

// Get a tree for the map index.  This returns a tree which evaluates
// to a pointer to a value.  The pointer will be NULL if the key is
// not in the map.

tree
Map_index_expression::get_value_pointer(Translate_context* context,
					bool insert)
{
  Map_type* type = this->get_map_type();
  if (type == NULL)
    return error_mark_node;

  tree map_tree = this->map_->get_tree(context);
  tree index_tree = this->index_->get_tree(context);
  index_tree = Expression::convert_for_assignment(context, type->key_type(),
						  this->index_->type(),
						  index_tree,
						  this->location());
  if (map_tree == error_mark_node || index_tree == error_mark_node)
    return error_mark_node;

  if (this->map_->type()->points_to() != NULL)
    map_tree = build_fold_indirect_ref(map_tree);

  // We need to pass in a pointer to the key, so stuff it into a
  // variable.
  tree tmp;
  tree make_tmp;
  if (current_function_decl != NULL)
    {
      tmp = create_tmp_var(TREE_TYPE(index_tree), get_name(index_tree));
      DECL_IGNORED_P(tmp) = 0;
      DECL_INITIAL(tmp) = index_tree;
      make_tmp = build1(DECL_EXPR, void_type_node, tmp);
      TREE_ADDRESSABLE(tmp) = 1;
    }
  else
    {
      tmp = build_decl(this->location().gcc_location(), VAR_DECL,
                       create_tmp_var_name("M"),
		       TREE_TYPE(index_tree));
      DECL_EXTERNAL(tmp) = 0;
      TREE_PUBLIC(tmp) = 0;
      TREE_STATIC(tmp) = 1;
      DECL_ARTIFICIAL(tmp) = 1;
      if (!TREE_CONSTANT(index_tree))
	make_tmp = fold_build2_loc(this->location().gcc_location(),
                                   INIT_EXPR, void_type_node,
				   tmp, index_tree);
      else
	{
	  TREE_READONLY(tmp) = 1;
	  TREE_CONSTANT(tmp) = 1;
	  DECL_INITIAL(tmp) = index_tree;
	  make_tmp = NULL_TREE;
	}
      rest_of_decl_compilation(tmp, 1, 0);
    }
  tree tmpref =
    fold_convert_loc(this->location().gcc_location(), const_ptr_type_node,
                     build_fold_addr_expr_loc(this->location().gcc_location(),
                                              tmp));

  static tree map_index_fndecl;
  tree call = Gogo::call_builtin(&map_index_fndecl,
				 this->location(),
				 "__go_map_index",
				 3,
				 const_ptr_type_node,
				 TREE_TYPE(map_tree),
				 map_tree,
				 const_ptr_type_node,
				 tmpref,
				 boolean_type_node,
				 (insert
				  ? boolean_true_node
				  : boolean_false_node));
  if (call == error_mark_node)
    return error_mark_node;
  // This can panic on a map of interface type if the interface holds
  // an uncomparable or unhashable type.
  TREE_NOTHROW(map_index_fndecl) = 0;

  Type* val_type = type->val_type();
  tree val_type_tree = type_to_tree(val_type->get_backend(context->gogo()));
  if (val_type_tree == error_mark_node)
    return error_mark_node;
  tree ptr_val_type_tree = build_pointer_type(val_type_tree);

  tree ret = fold_convert_loc(this->location().gcc_location(),
                              ptr_val_type_tree, call);
  if (make_tmp != NULL_TREE)
    ret = build2(COMPOUND_EXPR, ptr_val_type_tree, make_tmp, ret);
  return ret;
}

// Dump ast representation for a map index expression

void
Map_index_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  Index_expression::dump_index_expression(ast_dump_context, 
                                          this->map_, this->index_, NULL);
}

// Make a map index expression.

Map_index_expression*
Expression::make_map_index(Expression* map, Expression* index,
			   Location location)
{
  return new Map_index_expression(map, index, location);
}

// Class Field_reference_expression.

// Return the type of a field reference.

Type*
Field_reference_expression::do_type()
{
  Type* type = this->expr_->type();
  if (type->is_error())
    return type;
  Struct_type* struct_type = type->struct_type();
  go_assert(struct_type != NULL);
  return struct_type->field(this->field_index_)->type();
}

// Check the types for a field reference.

void
Field_reference_expression::do_check_types(Gogo*)
{
  Type* type = this->expr_->type();
  if (type->is_error())
    return;
  Struct_type* struct_type = type->struct_type();
  go_assert(struct_type != NULL);
  go_assert(struct_type->field(this->field_index_) != NULL);
}

// Get a tree for a field reference.

tree
Field_reference_expression::do_get_tree(Translate_context* context)
{
  tree struct_tree = this->expr_->get_tree(context);
  if (struct_tree == error_mark_node
      || TREE_TYPE(struct_tree) == error_mark_node)
    return error_mark_node;
  go_assert(TREE_CODE(TREE_TYPE(struct_tree)) == RECORD_TYPE);
  tree field = TYPE_FIELDS(TREE_TYPE(struct_tree));
  if (field == NULL_TREE)
    {
      // This can happen for a type which refers to itself indirectly
      // and then turns out to be erroneous.
      go_assert(saw_errors());
      return error_mark_node;
    }
  for (unsigned int i = this->field_index_; i > 0; --i)
    {
      field = DECL_CHAIN(field);
      go_assert(field != NULL_TREE);
    }
  if (TREE_TYPE(field) == error_mark_node)
    return error_mark_node;
  return build3(COMPONENT_REF, TREE_TYPE(field), struct_tree, field,
		NULL_TREE);
}

// Dump ast representation for a field reference expression.

void
Field_reference_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  this->expr_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << "." <<  this->field_index_;
}

// Make a reference to a qualified identifier in an expression.

Field_reference_expression*
Expression::make_field_reference(Expression* expr, unsigned int field_index,
				 Location location)
{
  return new Field_reference_expression(expr, field_index, location);
}

// Class Interface_field_reference_expression.

// Return a tree for the pointer to the function to call.

tree
Interface_field_reference_expression::get_function_tree(Translate_context*,
							tree expr)
{
  if (this->expr_->type()->points_to() != NULL)
    expr = build_fold_indirect_ref(expr);

  tree expr_type = TREE_TYPE(expr);
  go_assert(TREE_CODE(expr_type) == RECORD_TYPE);

  tree field = TYPE_FIELDS(expr_type);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__methods") == 0);

  tree table = build3(COMPONENT_REF, TREE_TYPE(field), expr, field, NULL_TREE);
  go_assert(POINTER_TYPE_P(TREE_TYPE(table)));

  table = build_fold_indirect_ref(table);
  go_assert(TREE_CODE(TREE_TYPE(table)) == RECORD_TYPE);

  std::string name = Gogo::unpack_hidden_name(this->name_);
  for (field = DECL_CHAIN(TYPE_FIELDS(TREE_TYPE(table)));
       field != NULL_TREE;
       field = DECL_CHAIN(field))
    {
      if (name == IDENTIFIER_POINTER(DECL_NAME(field)))
	break;
    }
  go_assert(field != NULL_TREE);

  return build3(COMPONENT_REF, TREE_TYPE(field), table, field, NULL_TREE);
}

// Return a tree for the first argument to pass to the interface
// function.

tree
Interface_field_reference_expression::get_underlying_object_tree(
    Translate_context*,
    tree expr)
{
  if (this->expr_->type()->points_to() != NULL)
    expr = build_fold_indirect_ref(expr);

  tree expr_type = TREE_TYPE(expr);
  go_assert(TREE_CODE(expr_type) == RECORD_TYPE);

  tree field = DECL_CHAIN(TYPE_FIELDS(expr_type));
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__object") == 0);

  return build3(COMPONENT_REF, TREE_TYPE(field), expr, field, NULL_TREE);
}

// Traversal.

int
Interface_field_reference_expression::do_traverse(Traverse* traverse)
{
  return Expression::traverse(&this->expr_, traverse);
}

// Return the type of an interface field reference.

Type*
Interface_field_reference_expression::do_type()
{
  Type* expr_type = this->expr_->type();

  Type* points_to = expr_type->points_to();
  if (points_to != NULL)
    expr_type = points_to;

  Interface_type* interface_type = expr_type->interface_type();
  if (interface_type == NULL)
    return Type::make_error_type();

  const Typed_identifier* method = interface_type->find_method(this->name_);
  if (method == NULL)
    return Type::make_error_type();

  return method->type();
}

// Determine types.

void
Interface_field_reference_expression::do_determine_type(const Type_context*)
{
  this->expr_->determine_type_no_context();
}

// Check the types for an interface field reference.

void
Interface_field_reference_expression::do_check_types(Gogo*)
{
  Type* type = this->expr_->type();

  Type* points_to = type->points_to();
  if (points_to != NULL)
    type = points_to;

  Interface_type* interface_type = type->interface_type();
  if (interface_type == NULL)
    {
      if (!type->is_error_type())
	this->report_error(_("expected interface or pointer to interface"));
    }
  else
    {
      const Typed_identifier* method =
	interface_type->find_method(this->name_);
      if (method == NULL)
	{
	  error_at(this->location(), "method %qs not in interface",
		   Gogo::message_name(this->name_).c_str());
	  this->set_is_error();
	}
    }
}

// Get a tree for a reference to a field in an interface.  There is no
// standard tree type representation for this: it's a function
// attached to its first argument, like a Bound_method_expression.
// The only places it may currently be used are in a Call_expression
// or a Go_statement, which will take it apart directly.  So this has
// nothing to do at present.

tree
Interface_field_reference_expression::do_get_tree(Translate_context*)
{
  go_unreachable();
}

// Dump ast representation for an interface field reference.

void
Interface_field_reference_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  this->expr_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << "." << this->name_;
}

// Make a reference to a field in an interface.

Expression*
Expression::make_interface_field_reference(Expression* expr,
					   const std::string& field,
					   Location location)
{
  return new Interface_field_reference_expression(expr, field, location);
}

// A general selector.  This is a Parser_expression for LEFT.NAME.  It
// is lowered after we know the type of the left hand side.

class Selector_expression : public Parser_expression
{
 public:
  Selector_expression(Expression* left, const std::string& name,
		      Location location)
    : Parser_expression(EXPRESSION_SELECTOR, location),
      left_(left), name_(name)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->left_, traverse); }

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  Expression*
  do_copy()
  {
    return new Selector_expression(this->left_->copy(), this->name_,
				   this->location());
  }

  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const;

 private:
  Expression*
  lower_method_expression(Gogo*);

  // The expression on the left hand side.
  Expression* left_;
  // The name on the right hand side.
  std::string name_;
};

// Lower a selector expression once we know the real type of the left
// hand side.

Expression*
Selector_expression::do_lower(Gogo* gogo, Named_object*, Statement_inserter*,
			      int)
{
  Expression* left = this->left_;
  if (left->is_type_expression())
    return this->lower_method_expression(gogo);
  return Type::bind_field_or_method(gogo, left->type(), left, this->name_,
				    this->location());
}

// Lower a method expression T.M or (*T).M.  We turn this into a
// function literal.

Expression*
Selector_expression::lower_method_expression(Gogo* gogo)
{
  Location location = this->location();
  Type* type = this->left_->type();
  const std::string& name(this->name_);

  bool is_pointer;
  if (type->points_to() == NULL)
    is_pointer = false;
  else
    {
      is_pointer = true;
      type = type->points_to();
    }
  Named_type* nt = type->named_type();
  if (nt == NULL)
    {
      error_at(location,
	       ("method expression requires named type or "
		"pointer to named type"));
      return Expression::make_error(location);
    }

  bool is_ambiguous;
  Method* method = nt->method_function(name, &is_ambiguous);
  const Typed_identifier* imethod = NULL;
  if (method == NULL && !is_pointer)
    {
      Interface_type* it = nt->interface_type();
      if (it != NULL)
	imethod = it->find_method(name);
    }

  if (method == NULL && imethod == NULL)
    {
      if (!is_ambiguous)
	error_at(location, "type %<%s%s%> has no method %<%s%>",
		 is_pointer ? "*" : "",
		 nt->message_name().c_str(),
		 Gogo::message_name(name).c_str());
      else
	error_at(location, "method %<%s%s%> is ambiguous in type %<%s%>",
		 Gogo::message_name(name).c_str(),
		 is_pointer ? "*" : "",
		 nt->message_name().c_str());
      return Expression::make_error(location);
    }

  if (method != NULL && !is_pointer && !method->is_value_method())
    {
      error_at(location, "method requires pointer (use %<(*%s).%s)%>",
	       nt->message_name().c_str(),
	       Gogo::message_name(name).c_str());
      return Expression::make_error(location);
    }

  // Build a new function type in which the receiver becomes the first
  // argument.
  Function_type* method_type;
  if (method != NULL)
    {
      method_type = method->type();
      go_assert(method_type->is_method());
    }
  else
    {
      method_type = imethod->type()->function_type();
      go_assert(method_type != NULL && !method_type->is_method());
    }

  const char* const receiver_name = "$this";
  Typed_identifier_list* parameters = new Typed_identifier_list();
  parameters->push_back(Typed_identifier(receiver_name, this->left_->type(),
					 location));

  const Typed_identifier_list* method_parameters = method_type->parameters();
  if (method_parameters != NULL)
    {
      for (Typed_identifier_list::const_iterator p = method_parameters->begin();
	   p != method_parameters->end();
	   ++p)
	parameters->push_back(*p);
    }

  const Typed_identifier_list* method_results = method_type->results();
  Typed_identifier_list* results;
  if (method_results == NULL)
    results = NULL;
  else
    {
      results = new Typed_identifier_list();
      for (Typed_identifier_list::const_iterator p = method_results->begin();
	   p != method_results->end();
	   ++p)
	results->push_back(*p);
    }
  
  Function_type* fntype = Type::make_function_type(NULL, parameters, results,
						   location);
  if (method_type->is_varargs())
    fntype->set_is_varargs();

  // We generate methods which always takes a pointer to the receiver
  // as their first argument.  If this is for a pointer type, we can
  // simply reuse the existing function.  We use an internal hack to
  // get the right type.

  if (method != NULL && is_pointer)
    {
      Named_object* mno = (method->needs_stub_method()
			   ? method->stub_object()
			   : method->named_object());
      Expression* f = Expression::make_func_reference(mno, NULL, location);
      f = Expression::make_cast(fntype, f, location);
      Type_conversion_expression* tce =
	static_cast<Type_conversion_expression*>(f);
      tce->set_may_convert_function_types();
      return f;
    }

  Named_object* no = gogo->start_function(Gogo::thunk_name(), fntype, false,
					  location);

  Named_object* vno = gogo->lookup(receiver_name, NULL);
  go_assert(vno != NULL);
  Expression* ve = Expression::make_var_reference(vno, location);
  Expression* bm;
  if (method != NULL)
    bm = Type::bind_field_or_method(gogo, nt, ve, name, location);
  else
    bm = Expression::make_interface_field_reference(ve, name, location);

  // Even though we found the method above, if it has an error type we
  // may see an error here.
  if (bm->is_error_expression())
    {
      gogo->finish_function(location);
      return bm;
    }

  Expression_list* args;
  if (method_parameters == NULL)
    args = NULL;
  else
    {
      args = new Expression_list();
      for (Typed_identifier_list::const_iterator p = method_parameters->begin();
	   p != method_parameters->end();
	   ++p)
	{
	  vno = gogo->lookup(p->name(), NULL);
	  go_assert(vno != NULL);
	  args->push_back(Expression::make_var_reference(vno, location));
	}
    }

  gogo->start_block(location);

  Call_expression* call = Expression::make_call(bm, args,
						method_type->is_varargs(),
						location);

  size_t count = call->result_count();
  Statement* s;
  if (count == 0)
    s = Statement::make_statement(call, true);
  else
    {
      Expression_list* retvals = new Expression_list();
      if (count <= 1)
	retvals->push_back(call);
      else
	{
	  for (size_t i = 0; i < count; ++i)
	    retvals->push_back(Expression::make_call_result(call, i));
	}
      s = Statement::make_return_statement(retvals, location);
    }
  gogo->add_statement(s);

  Block* b = gogo->finish_block(location);

  gogo->add_block(b, location);

  // Lower the call in case there are multiple results.
  gogo->lower_block(no, b);

  gogo->finish_function(location);

  return Expression::make_func_reference(no, NULL, location);
}

// Dump the ast for a selector expression.

void
Selector_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  ast_dump_context->dump_expression(this->left_);
  ast_dump_context->ostream() << ".";
  ast_dump_context->ostream() << this->name_;
}
                      
// Make a selector expression.

Expression*
Expression::make_selector(Expression* left, const std::string& name,
			  Location location)
{
  return new Selector_expression(left, name, location);
}

// Implement the builtin function new.

class Allocation_expression : public Expression
{
 public:
  Allocation_expression(Type* type, Location location)
    : Expression(EXPRESSION_ALLOCATION, location),
      type_(type)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Type::traverse(this->type_, traverse); }

  Type*
  do_type()
  { return Type::make_pointer_type(this->type_); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return new Allocation_expression(this->type_, this->location()); }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  // The type we are allocating.
  Type* type_;
};

// Return a tree for an allocation expression.

tree
Allocation_expression::do_get_tree(Translate_context* context)
{
  tree type_tree = type_to_tree(this->type_->get_backend(context->gogo()));
  if (type_tree == error_mark_node)
    return error_mark_node;
  tree size_tree = TYPE_SIZE_UNIT(type_tree);
  tree space = context->gogo()->allocate_memory(this->type_, size_tree,
						this->location());
  if (space == error_mark_node)
    return error_mark_node;
  return fold_convert(build_pointer_type(type_tree), space);
}

// Dump ast representation for an allocation expression.

void
Allocation_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  ast_dump_context->ostream() << "new(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ")";
}

// Make an allocation expression.

Expression*
Expression::make_allocation(Type* type, Location location)
{
  return new Allocation_expression(type, location);
}

// Construct a struct.

class Struct_construction_expression : public Expression
{
 public:
  Struct_construction_expression(Type* type, Expression_list* vals,
				 Location location)
    : Expression(EXPRESSION_STRUCT_CONSTRUCTION, location),
      type_(type), vals_(vals)
  { }

  // Return whether this is a constant initializer.
  bool
  is_constant_struct() const;

 protected:
  int
  do_traverse(Traverse* traverse);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return new Struct_construction_expression(this->type_, this->vals_->copy(),
					      this->location());
  }

  bool
  do_is_addressable() const
  { return true; }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of the struct to construct.
  Type* type_;
  // The list of values, in order of the fields in the struct.  A NULL
  // entry means that the field should be zero-initialized.
  Expression_list* vals_;
};

// Traversal.

int
Struct_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->vals_ != NULL
      && this->vals_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return whether this is a constant initializer.

bool
Struct_construction_expression::is_constant_struct() const
{
  if (this->vals_ == NULL)
    return true;
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (*pv != NULL
	  && !(*pv)->is_constant()
	  && (!(*pv)->is_composite_literal()
	      || (*pv)->is_nonconstant_composite_literal()))
	return false;
    }

  const Struct_field_list* fields = this->type_->struct_type()->fields();
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      // There are no constant constructors for interfaces.
      if (pf->type()->interface_type() != NULL)
	return false;
    }

  return true;
}

// Final type determination.

void
Struct_construction_expression::do_determine_type(const Type_context*)
{
  if (this->vals_ == NULL)
    return;
  const Struct_field_list* fields = this->type_->struct_type()->fields();
  Expression_list::const_iterator pv = this->vals_->begin();
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++pv)
    {
      if (pv == this->vals_->end())
	return;
      if (*pv != NULL)
	{
	  Type_context subcontext(pf->type(), false);
	  (*pv)->determine_type(&subcontext);
	}
    }
  // Extra values are an error we will report elsewhere; we still want
  // to determine the type to avoid knockon errors.
  for (; pv != this->vals_->end(); ++pv)
    (*pv)->determine_type_no_context();
}

// Check types.

void
Struct_construction_expression::do_check_types(Gogo*)
{
  if (this->vals_ == NULL)
    return;

  Struct_type* st = this->type_->struct_type();
  if (this->vals_->size() > st->field_count())
    {
      this->report_error(_("too many expressions for struct"));
      return;
    }

  const Struct_field_list* fields = st->fields();
  Expression_list::const_iterator pv = this->vals_->begin();
  int i = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++pv, ++i)
    {
      if (pv == this->vals_->end())
	{
	  this->report_error(_("too few expressions for struct"));
	  break;
	}

      if (*pv == NULL)
	continue;

      std::string reason;
      if (!Type::are_assignable(pf->type(), (*pv)->type(), &reason))
	{
	  if (reason.empty())
	    error_at((*pv)->location(),
		     "incompatible type for field %d in struct construction",
		     i + 1);
	  else
	    error_at((*pv)->location(),
		     ("incompatible type for field %d in "
		      "struct construction (%s)"),
		     i + 1, reason.c_str());
	  this->set_is_error();
	}
    }
  go_assert(pv == this->vals_->end());
}

// Return a tree for constructing a struct.

tree
Struct_construction_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  if (this->vals_ == NULL)
    {
      Btype* btype = this->type_->get_backend(gogo);
      return expr_to_tree(gogo->backend()->zero_expression(btype));
    }

  tree type_tree = type_to_tree(this->type_->get_backend(gogo));
  if (type_tree == error_mark_node)
    return error_mark_node;
  go_assert(TREE_CODE(type_tree) == RECORD_TYPE);

  bool is_constant = true;
  const Struct_field_list* fields = this->type_->struct_type()->fields();
  VEC(constructor_elt,gc)* elts = VEC_alloc(constructor_elt, gc,
					    fields->size());
  Struct_field_list::const_iterator pf = fields->begin();
  Expression_list::const_iterator pv = this->vals_->begin();
  for (tree field = TYPE_FIELDS(type_tree);
       field != NULL_TREE;
       field = DECL_CHAIN(field), ++pf)
    {
      go_assert(pf != fields->end());

      Btype* fbtype = pf->type()->get_backend(gogo);

      tree val;
      if (pv == this->vals_->end())
	val = expr_to_tree(gogo->backend()->zero_expression(fbtype));
      else if (*pv == NULL)
	{
	  val = expr_to_tree(gogo->backend()->zero_expression(fbtype));
	  ++pv;
	}
      else
	{
	  val = Expression::convert_for_assignment(context, pf->type(),
						   (*pv)->type(),
						   (*pv)->get_tree(context),
						   this->location());
	  ++pv;
	}

      if (val == error_mark_node || TREE_TYPE(val) == error_mark_node)
	return error_mark_node;

      constructor_elt* elt = VEC_quick_push(constructor_elt, elts, NULL);
      elt->index = field;
      elt->value = val;
      if (!TREE_CONSTANT(val))
	is_constant = false;
    }
  go_assert(pf == fields->end());

  tree ret = build_constructor(type_tree, elts);
  if (is_constant)
    TREE_CONSTANT(ret) = 1;
  return ret;
}

// Export a struct construction.

void
Struct_construction_expression::do_export(Export* exp) const
{
  exp->write_c_string("convert(");
  exp->write_type(this->type_);
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      exp->write_c_string(", ");
      if (*pv != NULL)
	(*pv)->export_expression(exp);
    }
  exp->write_c_string(")");
}

// Dump ast representation of a struct construction expression.

void
Struct_construction_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << "{";
  ast_dump_context->dump_expression_list(this->vals_);
  ast_dump_context->ostream() << "}";
}

// Make a struct composite literal.  This used by the thunk code.

Expression*
Expression::make_struct_composite_literal(Type* type, Expression_list* vals,
					  Location location)
{
  go_assert(type->struct_type() != NULL);
  return new Struct_construction_expression(type, vals, location);
}

// Construct an array.  This class is not used directly; instead we
// use the child classes, Fixed_array_construction_expression and
// Open_array_construction_expression.

class Array_construction_expression : public Expression
{
 protected:
  Array_construction_expression(Expression_classification classification,
				Type* type, Expression_list* vals,
				Location location)
    : Expression(classification, location),
      type_(type), vals_(vals)
  { }

 public:
  // Return whether this is a constant initializer.
  bool
  is_constant_array() const;

  // Return the number of elements.
  size_t
  element_count() const
  { return this->vals_ == NULL ? 0 : this->vals_->size(); }

protected:
  int
  do_traverse(Traverse* traverse);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  bool
  do_is_addressable() const
  { return true; }

  void
  do_export(Export*) const;

  // The list of values.
  Expression_list*
  vals()
  { return this->vals_; }

  // Get a constructor tree for the array values.
  tree
  get_constructor_tree(Translate_context* context, tree type_tree);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of the array to construct.
  Type* type_;
  // The list of values.
  Expression_list* vals_;
};

// Traversal.

int
Array_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->vals_ != NULL
      && this->vals_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return whether this is a constant initializer.

bool
Array_construction_expression::is_constant_array() const
{
  if (this->vals_ == NULL)
    return true;

  // There are no constant constructors for interfaces.
  if (this->type_->array_type()->element_type()->interface_type() != NULL)
    return false;

  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (*pv != NULL
	  && !(*pv)->is_constant()
	  && (!(*pv)->is_composite_literal()
	      || (*pv)->is_nonconstant_composite_literal()))
	return false;
    }
  return true;
}

// Final type determination.

void
Array_construction_expression::do_determine_type(const Type_context*)
{
  if (this->vals_ == NULL)
    return;
  Type_context subcontext(this->type_->array_type()->element_type(), false);
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (*pv != NULL)
	(*pv)->determine_type(&subcontext);
    }
}

// Check types.

void
Array_construction_expression::do_check_types(Gogo*)
{
  if (this->vals_ == NULL)
    return;

  Array_type* at = this->type_->array_type();
  int i = 0;
  Type* element_type = at->element_type();
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv, ++i)
    {
      if (*pv != NULL
	  && !Type::are_assignable(element_type, (*pv)->type(), NULL))
	{
	  error_at((*pv)->location(),
		   "incompatible type for element %d in composite literal",
		   i + 1);
	  this->set_is_error();
	}
    }

  Expression* length = at->length();
  if (length != NULL && !length->is_error_expression())
    {
      mpz_t val;
      mpz_init(val);
      Type* type;
      if (at->length()->integer_constant_value(true, val, &type))
	{
	  if (this->vals_->size() > mpz_get_ui(val))
	    this->report_error(_("too many elements in composite literal"));
	}
      mpz_clear(val);
    }
}

// Get a constructor tree for the array values.

tree
Array_construction_expression::get_constructor_tree(Translate_context* context,
						    tree type_tree)
{
  VEC(constructor_elt,gc)* values = VEC_alloc(constructor_elt, gc,
					      (this->vals_ == NULL
					       ? 0
					       : this->vals_->size()));
  Type* element_type = this->type_->array_type()->element_type();
  bool is_constant = true;
  if (this->vals_ != NULL)
    {
      size_t i = 0;
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv, ++i)
	{
	  constructor_elt* elt = VEC_quick_push(constructor_elt, values, NULL);
	  elt->index = size_int(i);
	  if (*pv == NULL)
	    {
	      Gogo* gogo = context->gogo();
	      Btype* ebtype = element_type->get_backend(gogo);
	      Bexpression *zv = gogo->backend()->zero_expression(ebtype);
	      elt->value = expr_to_tree(zv);
	    }
	  else
	    {
	      tree value_tree = (*pv)->get_tree(context);
	      elt->value = Expression::convert_for_assignment(context,
							      element_type,
							      (*pv)->type(),
							      value_tree,
							      this->location());
	    }
	  if (elt->value == error_mark_node)
	    return error_mark_node;
	  if (!TREE_CONSTANT(elt->value))
	    is_constant = false;
	}
    }

  tree ret = build_constructor(type_tree, values);
  if (is_constant)
    TREE_CONSTANT(ret) = 1;
  return ret;
}

// Export an array construction.

void
Array_construction_expression::do_export(Export* exp) const
{
  exp->write_c_string("convert(");
  exp->write_type(this->type_);
  if (this->vals_ != NULL)
    {
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv)
	{
	  exp->write_c_string(", ");
	  if (*pv != NULL)
	    (*pv)->export_expression(exp);
	}
    }
  exp->write_c_string(")");
}

// Dump ast representation of an array construction expressin.

void
Array_construction_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  Expression* length = this->type_->array_type() != NULL ?
			 this->type_->array_type()->length() : NULL;

  ast_dump_context->ostream() << "[" ;
  if (length != NULL)
    {
      ast_dump_context->dump_expression(length);
    }
  ast_dump_context->ostream() << "]" ;
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << "{" ;
  ast_dump_context->dump_expression_list(this->vals_);
  ast_dump_context->ostream() << "}" ;

}

// Construct a fixed array.

class Fixed_array_construction_expression :
  public Array_construction_expression
{
 public:
  Fixed_array_construction_expression(Type* type, Expression_list* vals,
				      Location location)
    : Array_construction_expression(EXPRESSION_FIXED_ARRAY_CONSTRUCTION,
				    type, vals, location)
  {
    go_assert(type->array_type() != NULL
	       && type->array_type()->length() != NULL);
  }

 protected:
  Expression*
  do_copy()
  {
    return new Fixed_array_construction_expression(this->type(),
						   (this->vals() == NULL
						    ? NULL
						    : this->vals()->copy()),
						   this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context*);
};

// Return a tree for constructing a fixed array.

tree
Fixed_array_construction_expression::do_get_tree(Translate_context* context)
{
  Type* type = this->type();
  Btype* btype = type->get_backend(context->gogo());
  return this->get_constructor_tree(context, type_to_tree(btype));
}

// Dump ast representation of an array construction expressin.

void
Fixed_array_construction_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context)
{

  ast_dump_context->ostream() << "[";
  ast_dump_context->dump_expression (this->type()->array_type()->length());
  ast_dump_context->ostream() << "]";
  ast_dump_context->dump_type(this->type());
  ast_dump_context->ostream() << "{";
  ast_dump_context->dump_expression_list(this->vals());
  ast_dump_context->ostream() << "}";

}
// Construct an open array.

class Open_array_construction_expression : public Array_construction_expression
{
 public:
  Open_array_construction_expression(Type* type, Expression_list* vals,
				     Location location)
    : Array_construction_expression(EXPRESSION_OPEN_ARRAY_CONSTRUCTION,
				    type, vals, location)
  {
    go_assert(type->array_type() != NULL
	       && type->array_type()->length() == NULL);
  }

 protected:
  // Note that taking the address of an open array literal is invalid.

  Expression*
  do_copy()
  {
    return new Open_array_construction_expression(this->type(),
						  (this->vals() == NULL
						   ? NULL
						   : this->vals()->copy()),
						  this->location());
  }

  tree
  do_get_tree(Translate_context*);
};

// Return a tree for constructing an open array.

tree
Open_array_construction_expression::do_get_tree(Translate_context* context)
{
  Array_type* array_type = this->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->type()->is_error());
      return error_mark_node;
    }

  Type* element_type = array_type->element_type();
  Btype* belement_type = element_type->get_backend(context->gogo());
  tree element_type_tree = type_to_tree(belement_type);
  if (element_type_tree == error_mark_node)
    return error_mark_node;

  tree values;
  tree length_tree;
  if (this->vals() == NULL || this->vals()->empty())
    {
      // We need to create a unique value.
      tree max = size_int(0);
      tree constructor_type = build_array_type(element_type_tree,
					       build_index_type(max));
      if (constructor_type == error_mark_node)
	return error_mark_node;
      VEC(constructor_elt,gc)* vec = VEC_alloc(constructor_elt, gc, 1);
      constructor_elt* elt = VEC_quick_push(constructor_elt, vec, NULL);
      elt->index = size_int(0);
      Gogo* gogo = context->gogo();
      Btype* btype = element_type->get_backend(gogo);
      elt->value = expr_to_tree(gogo->backend()->zero_expression(btype));
      values = build_constructor(constructor_type, vec);
      if (TREE_CONSTANT(elt->value))
	TREE_CONSTANT(values) = 1;
      length_tree = size_int(0);
    }
  else
    {
      tree max = size_int(this->vals()->size() - 1);
      tree constructor_type = build_array_type(element_type_tree,
					       build_index_type(max));
      if (constructor_type == error_mark_node)
	return error_mark_node;
      values = this->get_constructor_tree(context, constructor_type);
      length_tree = size_int(this->vals()->size());
    }

  if (values == error_mark_node)
    return error_mark_node;

  bool is_constant_initializer = TREE_CONSTANT(values);

  // We have to copy the initial values into heap memory if we are in
  // a function or if the values are not constants.  We also have to
  // copy them if they may contain pointers in a non-constant context,
  // as otherwise the garbage collector won't see them.
  bool copy_to_heap = (context->function() != NULL
		       || !is_constant_initializer
		       || (element_type->has_pointer()
			   && !context->is_const()));

  if (is_constant_initializer)
    {
      tree tmp = build_decl(this->location().gcc_location(), VAR_DECL,
			    create_tmp_var_name("C"), TREE_TYPE(values));
      DECL_EXTERNAL(tmp) = 0;
      TREE_PUBLIC(tmp) = 0;
      TREE_STATIC(tmp) = 1;
      DECL_ARTIFICIAL(tmp) = 1;
      if (copy_to_heap)
	{
	  // If we are not copying the value to the heap, we will only
	  // initialize the value once, so we can use this directly
	  // rather than copying it.  In that case we can't make it
	  // read-only, because the program is permitted to change it.
	  TREE_READONLY(tmp) = 1;
	  TREE_CONSTANT(tmp) = 1;
	}
      DECL_INITIAL(tmp) = values;
      rest_of_decl_compilation(tmp, 1, 0);
      values = tmp;
    }

  tree space;
  tree set;
  if (!copy_to_heap)
    {
      // the initializer will only run once.
      space = build_fold_addr_expr(values);
      set = NULL_TREE;
    }
  else
    {
      tree memsize = TYPE_SIZE_UNIT(TREE_TYPE(values));
      space = context->gogo()->allocate_memory(element_type, memsize,
					       this->location());
      space = save_expr(space);

      tree s = fold_convert(build_pointer_type(TREE_TYPE(values)), space);
      tree ref = build_fold_indirect_ref_loc(this->location().gcc_location(),
                                             s);
      TREE_THIS_NOTRAP(ref) = 1;
      set = build2(MODIFY_EXPR, void_type_node, ref, values);
    }

  // Build a constructor for the open array.

  tree type_tree = type_to_tree(this->type()->get_backend(context->gogo()));
  if (type_tree == error_mark_node)
    return error_mark_node;
  go_assert(TREE_CODE(type_tree) == RECORD_TYPE);

  VEC(constructor_elt,gc)* init = VEC_alloc(constructor_elt, gc, 3);

  constructor_elt* elt = VEC_quick_push(constructor_elt, init, NULL);
  tree field = TYPE_FIELDS(type_tree);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__values") == 0);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), space);

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__count") == 0);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), length_tree);

  elt = VEC_quick_push(constructor_elt, init, NULL);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)),"__capacity") == 0);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), length_tree);

  tree constructor = build_constructor(type_tree, init);
  if (constructor == error_mark_node)
    return error_mark_node;
  if (!copy_to_heap)
    TREE_CONSTANT(constructor) = 1;

  if (set == NULL_TREE)
    return constructor;
  else
    return build2(COMPOUND_EXPR, type_tree, set, constructor);
}

// Make a slice composite literal.  This is used by the type
// descriptor code.

Expression*
Expression::make_slice_composite_literal(Type* type, Expression_list* vals,
					 Location location)
{
  go_assert(type->is_slice_type());
  return new Open_array_construction_expression(type, vals, location);
}

// Construct a map.

class Map_construction_expression : public Expression
{
 public:
  Map_construction_expression(Type* type, Expression_list* vals,
			      Location location)
    : Expression(EXPRESSION_MAP_CONSTRUCTION, location),
      type_(type), vals_(vals)
  { go_assert(vals == NULL || vals->size() % 2 == 0); }

 protected:
  int
  do_traverse(Traverse* traverse);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  Expression*
  do_copy()
  {
    return new Map_construction_expression(this->type_, this->vals_->copy(),
					   this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  // The type of the map to construct.
  Type* type_;
  // The list of values.
  Expression_list* vals_;
};

// Traversal.

int
Map_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->vals_ != NULL
      && this->vals_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Final type determination.

void
Map_construction_expression::do_determine_type(const Type_context*)
{
  if (this->vals_ == NULL)
    return;

  Map_type* mt = this->type_->map_type();
  Type_context key_context(mt->key_type(), false);
  Type_context val_context(mt->val_type(), false);
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      (*pv)->determine_type(&key_context);
      ++pv;
      (*pv)->determine_type(&val_context);
    }
}

// Check types.

void
Map_construction_expression::do_check_types(Gogo*)
{
  if (this->vals_ == NULL)
    return;

  Map_type* mt = this->type_->map_type();
  int i = 0;
  Type* key_type = mt->key_type();
  Type* val_type = mt->val_type();
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv, ++i)
    {
      if (!Type::are_assignable(key_type, (*pv)->type(), NULL))
	{
	  error_at((*pv)->location(),
		   "incompatible type for element %d key in map construction",
		   i + 1);
	  this->set_is_error();
	}
      ++pv;
      if (!Type::are_assignable(val_type, (*pv)->type(), NULL))
	{
	  error_at((*pv)->location(),
		   ("incompatible type for element %d value "
		    "in map construction"),
		   i + 1);
	  this->set_is_error();
	}
    }
}

// Return a tree for constructing a map.

tree
Map_construction_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location loc = this->location();

  Map_type* mt = this->type_->map_type();

  // Build a struct to hold the key and value.
  tree struct_type = make_node(RECORD_TYPE);

  Type* key_type = mt->key_type();
  tree id = get_identifier("__key");
  tree key_type_tree = type_to_tree(key_type->get_backend(gogo));
  if (key_type_tree == error_mark_node)
    return error_mark_node;
  tree key_field = build_decl(loc.gcc_location(), FIELD_DECL, id,
                              key_type_tree);
  DECL_CONTEXT(key_field) = struct_type;
  TYPE_FIELDS(struct_type) = key_field;

  Type* val_type = mt->val_type();
  id = get_identifier("__val");
  tree val_type_tree = type_to_tree(val_type->get_backend(gogo));
  if (val_type_tree == error_mark_node)
    return error_mark_node;
  tree val_field = build_decl(loc.gcc_location(), FIELD_DECL, id,
                              val_type_tree);
  DECL_CONTEXT(val_field) = struct_type;
  DECL_CHAIN(key_field) = val_field;

  layout_type(struct_type);

  bool is_constant = true;
  size_t i = 0;
  tree valaddr;
  tree make_tmp;

  if (this->vals_ == NULL || this->vals_->empty())
    {
      valaddr = null_pointer_node;
      make_tmp = NULL_TREE;
    }
  else
    {
      VEC(constructor_elt,gc)* values = VEC_alloc(constructor_elt, gc,
						  this->vals_->size() / 2);

      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv, ++i)
	{
	  bool one_is_constant = true;

	  VEC(constructor_elt,gc)* one = VEC_alloc(constructor_elt, gc, 2);

	  constructor_elt* elt = VEC_quick_push(constructor_elt, one, NULL);
	  elt->index = key_field;
	  tree val_tree = (*pv)->get_tree(context);
	  elt->value = Expression::convert_for_assignment(context, key_type,
							  (*pv)->type(),
							  val_tree, loc);
	  if (elt->value == error_mark_node)
	    return error_mark_node;
	  if (!TREE_CONSTANT(elt->value))
	    one_is_constant = false;

	  ++pv;

	  elt = VEC_quick_push(constructor_elt, one, NULL);
	  elt->index = val_field;
	  val_tree = (*pv)->get_tree(context);
	  elt->value = Expression::convert_for_assignment(context, val_type,
							  (*pv)->type(),
							  val_tree, loc);
	  if (elt->value == error_mark_node)
	    return error_mark_node;
	  if (!TREE_CONSTANT(elt->value))
	    one_is_constant = false;

	  elt = VEC_quick_push(constructor_elt, values, NULL);
	  elt->index = size_int(i);
	  elt->value = build_constructor(struct_type, one);
	  if (one_is_constant)
	    TREE_CONSTANT(elt->value) = 1;
	  else
	    is_constant = false;
	}

      tree index_type = build_index_type(size_int(i - 1));
      tree array_type = build_array_type(struct_type, index_type);
      tree init = build_constructor(array_type, values);
      if (is_constant)
	TREE_CONSTANT(init) = 1;
      tree tmp;
      if (current_function_decl != NULL)
	{
	  tmp = create_tmp_var(array_type, get_name(array_type));
	  DECL_INITIAL(tmp) = init;
	  make_tmp = fold_build1_loc(loc.gcc_location(), DECL_EXPR,
                                     void_type_node, tmp);
	  TREE_ADDRESSABLE(tmp) = 1;
	}
      else
	{
	  tmp = build_decl(loc.gcc_location(), VAR_DECL,
                           create_tmp_var_name("M"), array_type);
	  DECL_EXTERNAL(tmp) = 0;
	  TREE_PUBLIC(tmp) = 0;
	  TREE_STATIC(tmp) = 1;
	  DECL_ARTIFICIAL(tmp) = 1;
	  if (!TREE_CONSTANT(init))
	    make_tmp = fold_build2_loc(loc.gcc_location(), INIT_EXPR,
                                       void_type_node, tmp, init);
	  else
	    {
	      TREE_READONLY(tmp) = 1;
	      TREE_CONSTANT(tmp) = 1;
	      DECL_INITIAL(tmp) = init;
	      make_tmp = NULL_TREE;
	    }
	  rest_of_decl_compilation(tmp, 1, 0);
	}

      valaddr = build_fold_addr_expr(tmp);
    }

  tree descriptor = mt->map_descriptor_pointer(gogo, loc);

  tree type_tree = type_to_tree(this->type_->get_backend(gogo));
  if (type_tree == error_mark_node)
    return error_mark_node;

  static tree construct_map_fndecl;
  tree call = Gogo::call_builtin(&construct_map_fndecl,
				 loc,
				 "__go_construct_map",
				 6,
				 type_tree,
				 TREE_TYPE(descriptor),
				 descriptor,
				 sizetype,
				 size_int(i),
				 sizetype,
				 TYPE_SIZE_UNIT(struct_type),
				 sizetype,
				 byte_position(val_field),
				 sizetype,
				 TYPE_SIZE_UNIT(TREE_TYPE(val_field)),
				 const_ptr_type_node,
				 fold_convert(const_ptr_type_node, valaddr));
  if (call == error_mark_node)
    return error_mark_node;

  tree ret;
  if (make_tmp == NULL)
    ret = call;
  else
    ret = fold_build2_loc(loc.gcc_location(), COMPOUND_EXPR, type_tree,
                          make_tmp, call);
  return ret;
}

// Export an array construction.

void
Map_construction_expression::do_export(Export* exp) const
{
  exp->write_c_string("convert(");
  exp->write_type(this->type_);
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      exp->write_c_string(", ");
      (*pv)->export_expression(exp);
    }
  exp->write_c_string(")");
}

// Dump ast representation for a map construction expression.

void
Map_construction_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "{" ;
  ast_dump_context->dump_expression_list(this->vals_, true);
  ast_dump_context->ostream() << "}";
}

// A general composite literal.  This is lowered to a type specific
// version.

class Composite_literal_expression : public Parser_expression
{
 public:
  Composite_literal_expression(Type* type, int depth, bool has_keys,
			       Expression_list* vals, Location location)
    : Parser_expression(EXPRESSION_COMPOSITE_LITERAL, location),
      type_(type), depth_(depth), vals_(vals), has_keys_(has_keys)
  { }

 protected:
  int
  do_traverse(Traverse* traverse);

  Expression*
  do_lower(Gogo*, Named_object*, Statement_inserter*, int);

  Expression*
  do_copy()
  {
    return new Composite_literal_expression(this->type_, this->depth_,
					    this->has_keys_,
					    (this->vals_ == NULL
					     ? NULL
					     : this->vals_->copy()),
					    this->location());
  }

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  Expression*
  lower_struct(Gogo*, Type*);

  Expression*
  lower_array(Type*);

  Expression*
  make_array(Type*, Expression_list*);

  Expression*
  lower_map(Gogo*, Named_object*, Statement_inserter*, Type*);

  // The type of the composite literal.
  Type* type_;
  // The depth within a list of composite literals within a composite
  // literal, when the type is omitted.
  int depth_;
  // The values to put in the composite literal.
  Expression_list* vals_;
  // If this is true, then VALS_ is a list of pairs: a key and a
  // value.  In an array initializer, a missing key will be NULL.
  bool has_keys_;
};

// Traversal.

int
Composite_literal_expression::do_traverse(Traverse* traverse)
{
  if (this->vals_ != NULL
      && this->vals_->traverse(traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return Type::traverse(this->type_, traverse);
}

// Lower a generic composite literal into a specific version based on
// the type.

Expression*
Composite_literal_expression::do_lower(Gogo* gogo, Named_object* function,
				       Statement_inserter* inserter, int)
{
  Type* type = this->type_;

  for (int depth = this->depth_; depth > 0; --depth)
    {
      if (type->array_type() != NULL)
	type = type->array_type()->element_type();
      else if (type->map_type() != NULL)
	type = type->map_type()->val_type();
      else
	{
	  if (!type->is_error())
	    error_at(this->location(),
		     ("may only omit types within composite literals "
		      "of slice, array, or map type"));
	  return Expression::make_error(this->location());
	}
    }

  Type *pt = type->points_to();
  bool is_pointer = false;
  if (pt != NULL)
    {
      is_pointer = true;
      type = pt;
    }

  Expression* ret;
  if (type->is_error())
    return Expression::make_error(this->location());
  else if (type->struct_type() != NULL)
    ret = this->lower_struct(gogo, type);
  else if (type->array_type() != NULL)
    ret = this->lower_array(type);
  else if (type->map_type() != NULL)
    ret = this->lower_map(gogo, function, inserter, type);
  else
    {
      error_at(this->location(),
	       ("expected struct, slice, array, or map type "
		"for composite literal"));
      return Expression::make_error(this->location());
    }

  if (is_pointer)
    ret = Expression::make_heap_composite(ret, this->location());

  return ret;
}

// Lower a struct composite literal.

Expression*
Composite_literal_expression::lower_struct(Gogo* gogo, Type* type)
{
  Location location = this->location();
  Struct_type* st = type->struct_type();
  if (this->vals_ == NULL || !this->has_keys_)
    {
      if (this->vals_ != NULL
	  && !this->vals_->empty()
	  && type->named_type() != NULL
	  && type->named_type()->named_object()->package() != NULL)
	{
	  for (Struct_field_list::const_iterator pf = st->fields()->begin();
	       pf != st->fields()->end();
	       ++pf)
	    {
	      if (Gogo::is_hidden_name(pf->field_name()))
		error_at(this->location(),
			 "assignment of unexported field %qs in %qs literal",
			 Gogo::message_name(pf->field_name()).c_str(),
			 type->named_type()->message_name().c_str());
	    }
	}

      return new Struct_construction_expression(type, this->vals_, location);
    }

  size_t field_count = st->field_count();
  std::vector<Expression*> vals(field_count);
  Expression_list::const_iterator p = this->vals_->begin();
  while (p != this->vals_->end())
    {
      Expression* name_expr = *p;

      ++p;
      go_assert(p != this->vals_->end());
      Expression* val = *p;

      ++p;

      if (name_expr == NULL)
	{
	  error_at(val->location(), "mixture of field and value initializers");
	  return Expression::make_error(location);
	}

      bool bad_key = false;
      std::string name;
      const Named_object* no = NULL;
      switch (name_expr->classification())
	{
	case EXPRESSION_UNKNOWN_REFERENCE:
	  name = name_expr->unknown_expression()->name();
	  break;

	case EXPRESSION_CONST_REFERENCE:
	  no = static_cast<Const_expression*>(name_expr)->named_object();
	  break;

	case EXPRESSION_TYPE:
	  {
	    Type* t = name_expr->type();
	    Named_type* nt = t->named_type();
	    if (nt == NULL)
	      bad_key = true;
	    else
	      no = nt->named_object();
	  }
	  break;

	case EXPRESSION_VAR_REFERENCE:
	  no = name_expr->var_expression()->named_object();
	  break;

	case EXPRESSION_FUNC_REFERENCE:
	  no = name_expr->func_expression()->named_object();
	  break;

	case EXPRESSION_UNARY:
	  // If there is a local variable around with the same name as
	  // the field, and this occurs in the closure, then the
	  // parser may turn the field reference into an indirection
	  // through the closure.  FIXME: This is a mess.
	  {
	    bad_key = true;
	    Unary_expression* ue = static_cast<Unary_expression*>(name_expr);
	    if (ue->op() == OPERATOR_MULT)
	      {
		Field_reference_expression* fre =
		  ue->operand()->field_reference_expression();
		if (fre != NULL)
		  {
		    Struct_type* st =
		      fre->expr()->type()->deref()->struct_type();
		    if (st != NULL)
		      {
			const Struct_field* sf = st->field(fre->field_index());
			name = sf->field_name();

			// See below.  FIXME.
			if (!Gogo::is_hidden_name(name)
			    && name[0] >= 'a'
			    && name[0] <= 'z')
			  {
			    if (gogo->lookup_global(name.c_str()) != NULL)
			      name = gogo->pack_hidden_name(name, false);
			  }

			char buf[20];
			snprintf(buf, sizeof buf, "%u", fre->field_index());
			size_t buflen = strlen(buf);
			if (name.compare(name.length() - buflen, buflen, buf)
			    == 0)
			  {
			    name = name.substr(0, name.length() - buflen);
			    bad_key = false;
			  }
		      }
		  }
	      }
	  }
	  break;

	default:
	  bad_key = true;
	  break;
	}
      if (bad_key)
	{
	  error_at(name_expr->location(), "expected struct field name");
	  return Expression::make_error(location);
	}

      if (no != NULL)
	{
	  name = no->name();

	  // A predefined name won't be packed.  If it starts with a
	  // lower case letter we need to check for that case, because
	  // the field name will be packed.  FIXME.
	  if (!Gogo::is_hidden_name(name)
	      && name[0] >= 'a'
	      && name[0] <= 'z')
	    {
	      Named_object* gno = gogo->lookup_global(name.c_str());
	      if (gno == no)
		name = gogo->pack_hidden_name(name, false);
	    }
	}

      unsigned int index;
      const Struct_field* sf = st->find_local_field(name, &index);
      if (sf == NULL)
	{
	  error_at(name_expr->location(), "unknown field %qs in %qs",
		   Gogo::message_name(name).c_str(),
		   (type->named_type() != NULL
		    ? type->named_type()->message_name().c_str()
		    : "unnamed struct"));
	  return Expression::make_error(location);
	}
      if (vals[index] != NULL)
	{
	  error_at(name_expr->location(),
		   "duplicate value for field %qs in %qs",
		   Gogo::message_name(name).c_str(),
		   (type->named_type() != NULL
		    ? type->named_type()->message_name().c_str()
		    : "unnamed struct"));
	  return Expression::make_error(location);
	}

      if (type->named_type() != NULL
	  && type->named_type()->named_object()->package() != NULL
	  && Gogo::is_hidden_name(sf->field_name()))
	error_at(name_expr->location(),
		 "assignment of unexported field %qs in %qs literal",
		 Gogo::message_name(sf->field_name()).c_str(),
		 type->named_type()->message_name().c_str());

      vals[index] = val;
    }

  Expression_list* list = new Expression_list;
  list->reserve(field_count);
  for (size_t i = 0; i < field_count; ++i)
    list->push_back(vals[i]);

  return new Struct_construction_expression(type, list, location);
}

// Lower an array composite literal.

Expression*
Composite_literal_expression::lower_array(Type* type)
{
  Location location = this->location();
  if (this->vals_ == NULL || !this->has_keys_)
    return this->make_array(type, this->vals_);

  std::vector<Expression*> vals;
  vals.reserve(this->vals_->size());
  unsigned long index = 0;
  Expression_list::const_iterator p = this->vals_->begin();
  while (p != this->vals_->end())
    {
      Expression* index_expr = *p;

      ++p;
      go_assert(p != this->vals_->end());
      Expression* val = *p;

      ++p;

      if (index_expr != NULL)
	{
	  mpz_t ival;
	  mpz_init(ival);

	  Type* dummy;
	  if (!index_expr->integer_constant_value(true, ival, &dummy))
	    {
	      mpz_clear(ival);
	      error_at(index_expr->location(),
		       "index expression is not integer constant");
	      return Expression::make_error(location);
	    }

	  if (mpz_sgn(ival) < 0)
	    {
	      mpz_clear(ival);
	      error_at(index_expr->location(), "index expression is negative");
	      return Expression::make_error(location);
	    }

	  index = mpz_get_ui(ival);
	  if (mpz_cmp_ui(ival, index) != 0)
	    {
	      mpz_clear(ival);
	      error_at(index_expr->location(), "index value overflow");
	      return Expression::make_error(location);
	    }

	  Named_type* ntype = Type::lookup_integer_type("int");
	  Integer_type* inttype = ntype->integer_type();
	  mpz_t max;
	  mpz_init_set_ui(max, 1);
	  mpz_mul_2exp(max, max, inttype->bits() - 1);
	  bool ok = mpz_cmp(ival, max) < 0;
	  mpz_clear(max);
	  if (!ok)
	    {
	      mpz_clear(ival);
	      error_at(index_expr->location(), "index value overflow");
	      return Expression::make_error(location);
	    }

	  mpz_clear(ival);

	  // FIXME: Our representation isn't very good; this avoids
	  // thrashing.
	  if (index > 0x1000000)
	    {
	      error_at(index_expr->location(), "index too large for compiler");
	      return Expression::make_error(location);
	    }
	}

      if (index == vals.size())
	vals.push_back(val);
      else
	{
	  if (index > vals.size())
	    {
	      vals.reserve(index + 32);
	      vals.resize(index + 1, static_cast<Expression*>(NULL));
	    }
	  if (vals[index] != NULL)
	    {
	      error_at((index_expr != NULL
			? index_expr->location()
			: val->location()),
		       "duplicate value for index %lu",
		       index);
	      return Expression::make_error(location);
	    }
	  vals[index] = val;
	}

      ++index;
    }

  size_t size = vals.size();
  Expression_list* list = new Expression_list;
  list->reserve(size);
  for (size_t i = 0; i < size; ++i)
    list->push_back(vals[i]);

  return this->make_array(type, list);
}

// Actually build the array composite literal. This handles
// [...]{...}.

Expression*
Composite_literal_expression::make_array(Type* type, Expression_list* vals)
{
  Location location = this->location();
  Array_type* at = type->array_type();
  if (at->length() != NULL && at->length()->is_nil_expression())
    {
      size_t size = vals == NULL ? 0 : vals->size();
      mpz_t vlen;
      mpz_init_set_ui(vlen, size);
      Expression* elen = Expression::make_integer(&vlen, NULL, location);
      mpz_clear(vlen);
      at = Type::make_array_type(at->element_type(), elen);
      type = at;
    }
  if (at->length() != NULL)
    return new Fixed_array_construction_expression(type, vals, location);
  else
    return new Open_array_construction_expression(type, vals, location);
}

// Lower a map composite literal.

Expression*
Composite_literal_expression::lower_map(Gogo* gogo, Named_object* function,
					Statement_inserter* inserter,
					Type* type)
{
  Location location = this->location();
  if (this->vals_ != NULL)
    {
      if (!this->has_keys_)
	{
	  error_at(location, "map composite literal must have keys");
	  return Expression::make_error(location);
	}

      for (Expression_list::iterator p = this->vals_->begin();
	   p != this->vals_->end();
	   p += 2)
	{
	  if (*p == NULL)
	    {
	      ++p;
	      error_at((*p)->location(),
		       "map composite literal must have keys for every value");
	      return Expression::make_error(location);
	    }
	  // Make sure we have lowered the key; it may not have been
	  // lowered in order to handle keys for struct composite
	  // literals.  Lower it now to get the right error message.
	  if ((*p)->unknown_expression() != NULL)
	    {
	      (*p)->unknown_expression()->clear_is_composite_literal_key();
	      gogo->lower_expression(function, inserter, &*p);
	      go_assert((*p)->is_error_expression());
	      return Expression::make_error(location);
	    }
	}
    }

  return new Map_construction_expression(type, this->vals_, location);
}

// Dump ast representation for a composite literal expression.

void
Composite_literal_expression::do_dump_expression(
                               Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "composite(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ", {";
  ast_dump_context->dump_expression_list(this->vals_, this->has_keys_);
  ast_dump_context->ostream() << "})";
}

// Make a composite literal expression.

Expression*
Expression::make_composite_literal(Type* type, int depth, bool has_keys,
				   Expression_list* vals,
				   Location location)
{
  return new Composite_literal_expression(type, depth, has_keys, vals,
					  location);
}

// Return whether this expression is a composite literal.

bool
Expression::is_composite_literal() const
{
  switch (this->classification_)
    {
    case EXPRESSION_COMPOSITE_LITERAL:
    case EXPRESSION_STRUCT_CONSTRUCTION:
    case EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
    case EXPRESSION_OPEN_ARRAY_CONSTRUCTION:
    case EXPRESSION_MAP_CONSTRUCTION:
      return true;
    default:
      return false;
    }
}

// Return whether this expression is a composite literal which is not
// constant.

bool
Expression::is_nonconstant_composite_literal() const
{
  switch (this->classification_)
    {
    case EXPRESSION_STRUCT_CONSTRUCTION:
      {
	const Struct_construction_expression *psce =
	  static_cast<const Struct_construction_expression*>(this);
	return !psce->is_constant_struct();
      }
    case EXPRESSION_FIXED_ARRAY_CONSTRUCTION:
      {
	const Fixed_array_construction_expression *pace =
	  static_cast<const Fixed_array_construction_expression*>(this);
	return !pace->is_constant_array();
      }
    case EXPRESSION_OPEN_ARRAY_CONSTRUCTION:
      {
	const Open_array_construction_expression *pace =
	  static_cast<const Open_array_construction_expression*>(this);
	return !pace->is_constant_array();
      }
    case EXPRESSION_MAP_CONSTRUCTION:
      return true;
    default:
      return false;
    }
}

// Return true if this is a reference to a local variable.

bool
Expression::is_local_variable() const
{
  const Var_expression* ve = this->var_expression();
  if (ve == NULL)
    return false;
  const Named_object* no = ve->named_object();
  return (no->is_result_variable()
	  || (no->is_variable() && !no->var_value()->is_global()));
}

// Class Type_guard_expression.

// Traversal.

int
Type_guard_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->expr_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Check types of a type guard expression.  The expression must have
// an interface type, but the actual type conversion is checked at run
// time.

void
Type_guard_expression::do_check_types(Gogo*)
{
  // 6g permits using a type guard with unsafe.pointer; we are
  // compatible.
  Type* expr_type = this->expr_->type();
  if (expr_type->is_unsafe_pointer_type())
    {
      if (this->type_->points_to() == NULL
	  && (this->type_->integer_type() == NULL
	      || (this->type_->forwarded()
		  != Type::lookup_integer_type("uintptr"))))
	this->report_error(_("invalid unsafe.Pointer conversion"));
    }
  else if (this->type_->is_unsafe_pointer_type())
    {
      if (expr_type->points_to() == NULL
	  && (expr_type->integer_type() == NULL
	      || (expr_type->forwarded()
		  != Type::lookup_integer_type("uintptr"))))
	this->report_error(_("invalid unsafe.Pointer conversion"));
    }
  else if (expr_type->interface_type() == NULL)
    {
      if (!expr_type->is_error() && !this->type_->is_error())
	this->report_error(_("type assertion only valid for interface types"));
      this->set_is_error();
    }
  else if (this->type_->interface_type() == NULL)
    {
      std::string reason;
      if (!expr_type->interface_type()->implements_interface(this->type_,
							     &reason))
	{
	  if (!this->type_->is_error())
	    {
	      if (reason.empty())
		this->report_error(_("impossible type assertion: "
				     "type does not implement interface"));
	      else
		error_at(this->location(),
			 ("impossible type assertion: "
			  "type does not implement interface (%s)"),
			 reason.c_str());
	    }
	  this->set_is_error();
	}
    }
}

// Return a tree for a type guard expression.

tree
Type_guard_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  tree expr_tree = this->expr_->get_tree(context);
  if (expr_tree == error_mark_node)
    return error_mark_node;
  Type* expr_type = this->expr_->type();
  if ((this->type_->is_unsafe_pointer_type()
       && (expr_type->points_to() != NULL
	   || expr_type->integer_type() != NULL))
      || (expr_type->is_unsafe_pointer_type()
	  && this->type_->points_to() != NULL))
    return convert_to_pointer(type_to_tree(this->type_->get_backend(gogo)),
			      expr_tree);
  else if (expr_type->is_unsafe_pointer_type()
	   && this->type_->integer_type() != NULL)
    return convert_to_integer(type_to_tree(this->type_->get_backend(gogo)),
			      expr_tree);
  else if (this->type_->interface_type() != NULL)
    return Expression::convert_interface_to_interface(context, this->type_,
						      this->expr_->type(),
						      expr_tree, true,
						      this->location());
  else
    return Expression::convert_for_assignment(context, this->type_,
					      this->expr_->type(), expr_tree,
					      this->location());
}

// Dump ast representation for a type guard expression.

void
Type_guard_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  this->expr_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() <<  ".";
  ast_dump_context->dump_type(this->type_);
}

// Make a type guard expression.

Expression*
Expression::make_type_guard(Expression* expr, Type* type,
			    Location location)
{
  return new Type_guard_expression(expr, type, location);
}

// Class Heap_composite_expression.

// When you take the address of a composite literal, it is allocated
// on the heap.  This class implements that.

class Heap_composite_expression : public Expression
{
 public:
  Heap_composite_expression(Expression* expr, Location location)
    : Expression(EXPRESSION_HEAP_COMPOSITE, location),
      expr_(expr)
  { }

 protected:
  int
  do_traverse(Traverse* traverse)
  { return Expression::traverse(&this->expr_, traverse); }

  Type*
  do_type()
  { return Type::make_pointer_type(this->expr_->type()); }

  void
  do_determine_type(const Type_context*)
  { this->expr_->determine_type_no_context(); }

  Expression*
  do_copy()
  {
    return Expression::make_heap_composite(this->expr_->copy(),
					   this->location());
  }

  tree
  do_get_tree(Translate_context*);

  // We only export global objects, and the parser does not generate
  // this in global scope.
  void
  do_export(Export*) const
  { go_unreachable(); }

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The composite literal which is being put on the heap.
  Expression* expr_;
};

// Return a tree which allocates a composite literal on the heap.

tree
Heap_composite_expression::do_get_tree(Translate_context* context)
{
  tree expr_tree = this->expr_->get_tree(context);
  if (expr_tree == error_mark_node)
    return error_mark_node;
  tree expr_size = TYPE_SIZE_UNIT(TREE_TYPE(expr_tree));
  go_assert(TREE_CODE(expr_size) == INTEGER_CST);
  tree space = context->gogo()->allocate_memory(this->expr_->type(),
						expr_size, this->location());
  space = fold_convert(build_pointer_type(TREE_TYPE(expr_tree)), space);
  space = save_expr(space);
  tree ref = build_fold_indirect_ref_loc(this->location().gcc_location(),
                                         space);
  TREE_THIS_NOTRAP(ref) = 1;
  tree ret = build2(COMPOUND_EXPR, TREE_TYPE(space),
		    build2(MODIFY_EXPR, void_type_node, ref, expr_tree),
		    space);
  SET_EXPR_LOCATION(ret, this->location().gcc_location());
  return ret;
}

// Dump ast representation for a heap composite expression.

void
Heap_composite_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "&(";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << ")";
}

// Allocate a composite literal on the heap.

Expression*
Expression::make_heap_composite(Expression* expr, Location location)
{
  return new Heap_composite_expression(expr, location);
}

// Class Receive_expression.

// Return the type of a receive expression.

Type*
Receive_expression::do_type()
{
  Channel_type* channel_type = this->channel_->type()->channel_type();
  if (channel_type == NULL)
    return Type::make_error_type();
  return channel_type->element_type();
}

// Check types for a receive expression.

void
Receive_expression::do_check_types(Gogo*)
{
  Type* type = this->channel_->type();
  if (type->is_error())
    {
      this->set_is_error();
      return;
    }
  if (type->channel_type() == NULL)
    {
      this->report_error(_("expected channel"));
      return;
    }
  if (!type->channel_type()->may_receive())
    {
      this->report_error(_("invalid receive on send-only channel"));
      return;
    }
}

// Get a tree for a receive expression.

tree
Receive_expression::do_get_tree(Translate_context* context)
{
  Location loc = this->location();

  Channel_type* channel_type = this->channel_->type()->channel_type();
  if (channel_type == NULL)
    {
      go_assert(this->channel_->type()->is_error());
      return error_mark_node;
    }

  Expression* td = Expression::make_type_descriptor(channel_type, loc);
  tree td_tree = td->get_tree(context);

  Type* element_type = channel_type->element_type();
  Btype* element_type_btype = element_type->get_backend(context->gogo());
  tree element_type_tree = type_to_tree(element_type_btype);

  tree channel = this->channel_->get_tree(context);
  if (element_type_tree == error_mark_node || channel == error_mark_node)
    return error_mark_node;

  return Gogo::receive_from_channel(element_type_tree, td_tree, channel, loc);
}

// Dump ast representation for a receive expression.

void
Receive_expression::do_dump_expression(Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << " <- " ;
  ast_dump_context->dump_expression(channel_);
}

// Make a receive expression.

Receive_expression*
Expression::make_receive(Expression* channel, Location location)
{
  return new Receive_expression(channel, location);
}

// An expression which evaluates to a pointer to the type descriptor
// of a type.

class Type_descriptor_expression : public Expression
{
 public:
  Type_descriptor_expression(Type* type, Location location)
    : Expression(EXPRESSION_TYPE_DESCRIPTOR, location),
      type_(type)
  { }

 protected:
  Type*
  do_type()
  { return Type::make_type_descriptor_ptr_type(); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context* context)
  {
    return this->type_->type_descriptor_pointer(context->gogo(),
						this->location());
  }

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type for which this is the descriptor.
  Type* type_;
};

// Dump ast representation for a type descriptor expression.

void
Type_descriptor_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->dump_type(this->type_);
}

// Make a type descriptor expression.

Expression*
Expression::make_type_descriptor(Type* type, Location location)
{
  return new Type_descriptor_expression(type, location);
}

// An expression which evaluates to some characteristic of a type.
// This is only used to initialize fields of a type descriptor.  Using
// a new expression class is slightly inefficient but gives us a good
// separation between the frontend and the middle-end with regard to
// how types are laid out.

class Type_info_expression : public Expression
{
 public:
  Type_info_expression(Type* type, Type_info type_info)
    : Expression(EXPRESSION_TYPE_INFO, Linemap::predeclared_location()),
      type_(type), type_info_(type_info)
  { }

 protected:
  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type for which we are getting information.
  Type* type_;
  // What information we want.
  Type_info type_info_;
};

// The type is chosen to match what the type descriptor struct
// expects.

Type*
Type_info_expression::do_type()
{
  switch (this->type_info_)
    {
    case TYPE_INFO_SIZE:
      return Type::lookup_integer_type("uintptr");
    case TYPE_INFO_ALIGNMENT:
    case TYPE_INFO_FIELD_ALIGNMENT:
      return Type::lookup_integer_type("uint8");
    default:
      go_unreachable();
    }
}

// Return type information in GENERIC.

tree
Type_info_expression::do_get_tree(Translate_context* context)
{
  tree type_tree = type_to_tree(this->type_->get_backend(context->gogo()));
  if (type_tree == error_mark_node)
    return error_mark_node;

  tree val_type_tree = type_to_tree(this->type()->get_backend(context->gogo()));
  go_assert(val_type_tree != error_mark_node);

  if (this->type_info_ == TYPE_INFO_SIZE)
    return fold_convert_loc(BUILTINS_LOCATION, val_type_tree,
			    TYPE_SIZE_UNIT(type_tree));
  else
    {
      unsigned int val;
      if (this->type_info_ == TYPE_INFO_ALIGNMENT)
	val = go_type_alignment(type_tree);
      else
	val = go_field_alignment(type_tree);
      return build_int_cstu(val_type_tree, val);
    }
}

// Dump ast representation for a type info expression.

void
Type_info_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "typeinfo(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ",";
  ast_dump_context->ostream() << 
    (this->type_info_ == TYPE_INFO_ALIGNMENT ? "alignment" 
    : this->type_info_ == TYPE_INFO_FIELD_ALIGNMENT ? "field alignment"
    : this->type_info_ == TYPE_INFO_SIZE ? "size "
    : "unknown");
  ast_dump_context->ostream() << ")";
}

// Make a type info expression.

Expression*
Expression::make_type_info(Type* type, Type_info type_info)
{
  return new Type_info_expression(type, type_info);
}

// An expression which evaluates to the offset of a field within a
// struct.  This, like Type_info_expression, q.v., is only used to
// initialize fields of a type descriptor.

class Struct_field_offset_expression : public Expression
{
 public:
  Struct_field_offset_expression(Struct_type* type, const Struct_field* field)
    : Expression(EXPRESSION_STRUCT_FIELD_OFFSET,
		 Linemap::predeclared_location()),
      type_(type), field_(field)
  { }

 protected:
  Type*
  do_type()
  { return Type::lookup_integer_type("uintptr"); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;
  
 private:
  // The type of the struct.
  Struct_type* type_;
  // The field.
  const Struct_field* field_;
};

// Return a struct field offset in GENERIC.

tree
Struct_field_offset_expression::do_get_tree(Translate_context* context)
{
  tree type_tree = type_to_tree(this->type_->get_backend(context->gogo()));
  if (type_tree == error_mark_node)
    return error_mark_node;

  tree val_type_tree = type_to_tree(this->type()->get_backend(context->gogo()));
  go_assert(val_type_tree != error_mark_node);

  const Struct_field_list* fields = this->type_->fields();
  tree struct_field_tree = TYPE_FIELDS(type_tree);
  Struct_field_list::const_iterator p;
  for (p = fields->begin();
       p != fields->end();
       ++p, struct_field_tree = DECL_CHAIN(struct_field_tree))
    {
      go_assert(struct_field_tree != NULL_TREE);
      if (&*p == this->field_)
	break;
    }
  go_assert(&*p == this->field_);

  return fold_convert_loc(BUILTINS_LOCATION, val_type_tree,
			  byte_position(struct_field_tree));
}

// Dump ast representation for a struct field offset expression.

void
Struct_field_offset_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() <<  "unsafe.Offsetof(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << '.';
  ast_dump_context->ostream() <<
    Gogo::message_name(this->field_->field_name());
  ast_dump_context->ostream() << ")";
}

// Make an expression for a struct field offset.

Expression*
Expression::make_struct_field_offset(Struct_type* type,
				     const Struct_field* field)
{
  return new Struct_field_offset_expression(type, field);
}

// An expression which evaluates to a pointer to the map descriptor of
// a map type.

class Map_descriptor_expression : public Expression
{
 public:
  Map_descriptor_expression(Map_type* type, Location location)
    : Expression(EXPRESSION_MAP_DESCRIPTOR, location),
      type_(type)
  { }

 protected:
  Type*
  do_type()
  { return Type::make_pointer_type(Map_type::make_map_descriptor_type()); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context* context)
  {
    return this->type_->map_descriptor_pointer(context->gogo(),
					       this->location());
  }

  void
  do_dump_expression(Ast_dump_context*) const;
 
 private:
  // The type for which this is the descriptor.
  Map_type* type_;
};

// Dump ast representation for a map descriptor expression.

void
Map_descriptor_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "map_descriptor(";
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << ")";
}

// Make a map descriptor expression.

Expression*
Expression::make_map_descriptor(Map_type* type, Location location)
{
  return new Map_descriptor_expression(type, location);
}

// An expression which evaluates to the address of an unnamed label.

class Label_addr_expression : public Expression
{
 public:
  Label_addr_expression(Label* label, Location location)
    : Expression(EXPRESSION_LABEL_ADDR, location),
      label_(label)
  { }

 protected:
  Type*
  do_type()
  { return Type::make_pointer_type(Type::make_void_type()); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return new Label_addr_expression(this->label_, this->location()); }

  tree
  do_get_tree(Translate_context* context)
  {
    return expr_to_tree(this->label_->get_addr(context, this->location()));
  }

  void
  do_dump_expression(Ast_dump_context* ast_dump_context) const
  { ast_dump_context->ostream() << this->label_->name(); }
  
 private:
  // The label whose address we are taking.
  Label* label_;
};

// Make an expression for the address of an unnamed label.

Expression*
Expression::make_label_addr(Label* label, Location location)
{
  return new Label_addr_expression(label, location);
}

// Import an expression.  This comes at the end in order to see the
// various class definitions.

Expression*
Expression::import_expression(Import* imp)
{
  int c = imp->peek_char();
  if (imp->match_c_string("- ")
      || imp->match_c_string("! ")
      || imp->match_c_string("^ "))
    return Unary_expression::do_import(imp);
  else if (c == '(')
    return Binary_expression::do_import(imp);
  else if (imp->match_c_string("true")
	   || imp->match_c_string("false"))
    return Boolean_expression::do_import(imp);
  else if (c == '"')
    return String_expression::do_import(imp);
  else if (c == '-' || (c >= '0' && c <= '9'))
    {
      // This handles integers, floats and complex constants.
      return Integer_expression::do_import(imp);
    }
  else if (imp->match_c_string("nil"))
    return Nil_expression::do_import(imp);
  else if (imp->match_c_string("convert"))
    return Type_conversion_expression::do_import(imp);
  else
    {
      error_at(imp->location(), "import error: expected expression");
      return Expression::make_error(imp->location());
    }
}

// Class Expression_list.

// Traverse the list.

int
Expression_list::traverse(Traverse* traverse)
{
  for (Expression_list::iterator p = this->begin();
       p != this->end();
       ++p)
    {
      if (*p != NULL)
	{
	  if (Expression::traverse(&*p, traverse) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
    }
  return TRAVERSE_CONTINUE;
}

// Copy the list.

Expression_list*
Expression_list::copy()
{
  Expression_list* ret = new Expression_list();
  for (Expression_list::iterator p = this->begin();
       p != this->end();
       ++p)
    {
      if (*p == NULL)
	ret->push_back(NULL);
      else
	ret->push_back((*p)->copy());
    }
  return ret;
}

// Return whether an expression list has an error expression.

bool
Expression_list::contains_error() const
{
  for (Expression_list::const_iterator p = this->begin();
       p != this->end();
       ++p)
    if (*p != NULL && (*p)->is_error_expression())
      return true;
  return false;
}
