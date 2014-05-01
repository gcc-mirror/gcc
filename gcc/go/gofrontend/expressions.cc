// expressions.cc -- Go frontend expression handling.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <algorithm>

#include "toplev.h"
#include "intl.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "gimple-expr.h"
#include "tree-iterator.h"
#include "convert.h"
#include "real.h"
#include "realmpfr.h"

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

bool
Expression::do_discarding_value()
{
  this->unused_value_error();
  return false;
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
  this->report_error(_("value computed is not used"));
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

// Return an expression handling any conversions which must be done during
// assignment.

Expression*
Expression::convert_for_assignment(Gogo* gogo, Type* lhs_type,
				   Expression* rhs, Location location)
{
  Type* rhs_type = rhs->type();
  if (lhs_type->is_error()
      || rhs_type->is_error()
      || rhs->is_error_expression())
    return Expression::make_error(location);

  if (lhs_type->forwarded() != rhs_type->forwarded()
      && lhs_type->interface_type() != NULL)
    {
      if (rhs_type->interface_type() == NULL)
        return Expression::convert_type_to_interface(lhs_type, rhs, location);
      else
        return Expression::convert_interface_to_interface(lhs_type, rhs, false,
                                                          location);
    }
  else if (lhs_type->forwarded() != rhs_type->forwarded()
	   && rhs_type->interface_type() != NULL)
    return Expression::convert_interface_to_type(lhs_type, rhs, location);
  else if (lhs_type->is_slice_type() && rhs_type->is_nil_type())
    {
      // Assigning nil to a slice.
      mpz_t zval;
      mpz_init_set_ui(zval, 0UL);
      Expression* zero = Expression::make_integer(&zval, NULL, location);
      mpz_clear(zval);
      Expression* nil = Expression::make_nil(location);
      return Expression::make_slice_value(lhs_type, nil, zero, zero, location);
    }
  else if (rhs_type->is_nil_type())
    return Expression::make_nil(location);
  else if (Type::are_identical(lhs_type, rhs_type, false, NULL))
    {
      // No conversion is needed.
      return rhs;
    }
  else if (lhs_type->points_to() != NULL)
    return Expression::make_unsafe_cast(lhs_type, rhs, location);
  else if (lhs_type->is_numeric_type())
    return Expression::make_cast(lhs_type, rhs, location);
  else if ((lhs_type->struct_type() != NULL
            && rhs_type->struct_type() != NULL)
           || (lhs_type->array_type() != NULL
               && rhs_type->array_type() != NULL))
    {
      // Avoid confusion from zero sized variables which may be
      // represented as non-zero-sized.
      // TODO(cmang): This check is for a GCC-specific issue, and should be
      // removed from the frontend.  FIXME.
      size_t lhs_size = gogo->backend()->type_size(lhs_type->get_backend(gogo));
      size_t rhs_size = gogo->backend()->type_size(rhs_type->get_backend(gogo));
      if (rhs_size == 0 || lhs_size == 0)
	return rhs;

      // This conversion must be permitted by Go, or we wouldn't have
      // gotten here.
      return Expression::make_unsafe_cast(lhs_type, rhs, location);
    }
  else
    return rhs;
}

// Return an expression for a conversion from a non-interface type to an
// interface type.

Expression*
Expression::convert_type_to_interface(Type* lhs_type, Expression* rhs,
                                      Location location)
{
  Interface_type* lhs_interface_type = lhs_type->interface_type();
  bool lhs_is_empty = lhs_interface_type->is_empty();

  // Since RHS_TYPE is a static type, we can create the interface
  // method table at compile time.

  // When setting an interface to nil, we just set both fields to
  // NULL.
  Type* rhs_type = rhs->type();
  if (rhs_type->is_nil_type())
    {
      Expression* nil = Expression::make_nil(location);
      return Expression::make_interface_value(lhs_type, nil, nil, location);
    }

  // This should have been checked already.
  go_assert(lhs_interface_type->implements_interface(rhs_type, NULL));

  // An interface is a tuple.  If LHS_TYPE is an empty interface type,
  // then the first field is the type descriptor for RHS_TYPE.
  // Otherwise it is the interface method table for RHS_TYPE.
  Expression* first_field;
  if (lhs_is_empty)
    first_field = Expression::make_type_descriptor(rhs_type, location);
  else
    {
      // Build the interface method table for this interface and this
      // object type: a list of function pointers for each interface
      // method.
      Named_type* rhs_named_type = rhs_type->named_type();
      Struct_type* rhs_struct_type = rhs_type->struct_type();
      bool is_pointer = false;
      if (rhs_named_type == NULL && rhs_struct_type == NULL)
	{
	  rhs_named_type = rhs_type->deref()->named_type();
	  rhs_struct_type = rhs_type->deref()->struct_type();
	  is_pointer = true;
	}
      if (rhs_named_type != NULL)
	first_field =
	  rhs_named_type->interface_method_table(lhs_interface_type,
                                                 is_pointer);
      else if (rhs_struct_type != NULL)
	first_field =
	  rhs_struct_type->interface_method_table(lhs_interface_type,
                                                  is_pointer);
      else
	first_field = Expression::make_nil(location);
    }

  Expression* obj;
  if (rhs_type->points_to() != NULL)
    {
      // We are assigning a pointer to the interface; the interface
      // holds the pointer itself.
      obj = rhs;
    }
  else
    {
      // We are assigning a non-pointer value to the interface; the
      // interface gets a copy of the value in the heap.
      obj = Expression::make_heap_expression(rhs, location);
    }

  return Expression::make_interface_value(lhs_type, first_field, obj, location);
}

// Return an expression for the type descriptor of RHS.

Expression*
Expression::get_interface_type_descriptor(Expression* rhs)
{
  go_assert(rhs->type()->interface_type() != NULL);
  Location location = rhs->location();

  // The type descriptor is the first field of an empty interface.
  if (rhs->type()->interface_type()->is_empty())
    return Expression::make_interface_info(rhs, INTERFACE_INFO_TYPE_DESCRIPTOR,
                                           location);

  Expression* mtable =
      Expression::make_interface_info(rhs, INTERFACE_INFO_METHODS, location);

  Expression* descriptor =
      Expression::make_unary(OPERATOR_MULT, mtable, location);
  descriptor = Expression::make_field_reference(descriptor, 0, location);
  Expression* nil = Expression::make_nil(location);

  Expression* eq =
      Expression::make_binary(OPERATOR_EQEQ, mtable, nil, location);
  return Expression::make_conditional(eq, nil, descriptor, location);
}

// Return an expression for the conversion of an interface type to an
// interface type.

Expression*
Expression::convert_interface_to_interface(Type *lhs_type, Expression* rhs,
                                           bool for_type_guard,
                                           Location location)
{
  Interface_type* lhs_interface_type = lhs_type->interface_type();
  bool lhs_is_empty = lhs_interface_type->is_empty();

  // In the general case this requires runtime examination of the type
  // method table to match it up with the interface methods.

  // FIXME: If all of the methods in the right hand side interface
  // also appear in the left hand side interface, then we don't need
  // to do a runtime check, although we still need to build a new
  // method table.

  // Get the type descriptor for the right hand side.  This will be
  // NULL for a nil interface.
  Expression* rhs_type_expr = Expression::get_interface_type_descriptor(rhs);
  Expression* lhs_type_expr =
      Expression::make_type_descriptor(lhs_type, location);

  Expression* first_field;
  if (for_type_guard)
    {
      // A type assertion fails when converting a nil interface.
      first_field =
          Runtime::make_call(Runtime::ASSERT_INTERFACE, location, 2,
                             lhs_type_expr, rhs_type_expr);
    }
  else if (lhs_is_empty)
    {
      // A conversion to an empty interface always succeeds, and the
      // first field is just the type descriptor of the object.
      first_field = rhs_type_expr;
    }
  else
    {
      // A conversion to a non-empty interface may fail, but unlike a
      // type assertion converting nil will always succeed.
      first_field =
          Runtime::make_call(Runtime::CONVERT_INTERFACE, location, 2,
                             lhs_type_expr, rhs_type_expr);
    }

  // The second field is simply the object pointer.
  Expression* obj =
      Expression::make_interface_info(rhs, INTERFACE_INFO_OBJECT, location);
  return Expression::make_interface_value(lhs_type, first_field, obj, location);
}

// Return an expression for the conversion of an interface type to a
// non-interface type.

Expression*
Expression::convert_interface_to_type(Type *lhs_type, Expression* rhs,
                                      Location location)
{
  // Call a function to check that the type is valid.  The function
  // will panic with an appropriate runtime type error if the type is
  // not valid.
  Expression* lhs_type_expr = Expression::make_type_descriptor(lhs_type,
                                                                location);
  Expression* rhs_descriptor =
      Expression::get_interface_type_descriptor(rhs);

  Type* rhs_type = rhs->type();
  Expression* rhs_inter_expr = Expression::make_type_descriptor(rhs_type,
                                                                location);

  Expression* check_iface = Runtime::make_call(Runtime::CHECK_INTERFACE_TYPE,
                                               location, 3, lhs_type_expr,
                                               rhs_descriptor, rhs_inter_expr);

  // If the call succeeds, pull out the value.
  Expression* obj = Expression::make_interface_info(rhs, INTERFACE_INFO_OBJECT,
                                                    location);

  // If the value is a pointer, then it is the value we want.
  // Otherwise it points to the value.
  if (lhs_type->points_to() == NULL)
    {
      obj = Expression::make_unsafe_cast(Type::make_pointer_type(lhs_type), obj,
                                         location);
      obj = Expression::make_unary(OPERATOR_MULT, obj, location);
    }
  return Expression::make_compound(check_iface, obj, location);
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

// Return a backend expression for VAL.
Bexpression*
Expression::backend_numeric_constant_expression(Translate_context* context,
                                                Numeric_constant* val)
{
  Gogo* gogo = context->gogo();
  Type* type = val->type();
  if (type == NULL)
    return gogo->backend()->error_expression();

  Btype* btype = type->get_backend(gogo);
  Bexpression* ret;
  if (type->integer_type() != NULL)
    {
      mpz_t ival;
      if (!val->to_int(&ival))
        {
          go_assert(saw_errors());
          return gogo->backend()->error_expression();
        }
      ret = gogo->backend()->integer_constant_expression(btype, ival);
      mpz_clear(ival);
    }
  else if (type->float_type() != NULL)
    {
      mpfr_t fval;
      if (!val->to_float(&fval))
        {
          go_assert(saw_errors());
          return gogo->backend()->error_expression();
        }
      ret = gogo->backend()->float_constant_expression(btype, fval);
      mpfr_clear(fval);
    }
  else if (type->complex_type() != NULL)
    {
      mpfr_t real;
      mpfr_t imag;
      if (!val->to_complex(&real, &imag))
        {
          go_assert(saw_errors());
          return gogo->backend()->error_expression();
        }
      ret = gogo->backend()->complex_constant_expression(btype, real, imag);
      mpfr_clear(real);
      mpfr_clear(imag);
    }
  else
    go_unreachable();

  return ret;
}

// Return an expression which evaluates to true if VAL, of arbitrary integer
// type, is negative or is more than the maximum value of the Go type "int".

Expression*
Expression::check_bounds(Expression* val, Location loc)
{
  Type* val_type = val->type();
  Type* bound_type = Type::lookup_integer_type("int");

  int val_type_size;
  bool val_is_unsigned = false;
  if (val_type->integer_type() != NULL)
    {
      val_type_size = val_type->integer_type()->bits();
      val_is_unsigned = val_type->integer_type()->is_unsigned();
    }
  else
    {
      if (!val_type->is_numeric_type()
          || !Type::are_convertible(bound_type, val_type, NULL))
        {
          go_assert(saw_errors());
          return Expression::make_boolean(true, loc);
        }

      if (val_type->complex_type() != NULL)
        val_type_size = val_type->complex_type()->bits();
      else
        val_type_size = val_type->float_type()->bits();
    }

  Expression* negative_index = Expression::make_boolean(false, loc);
  Expression* index_overflows = Expression::make_boolean(false, loc);
  if (!val_is_unsigned)
    {
      mpz_t zval;
      mpz_init_set_ui(zval, 0UL);
      Expression* zero = Expression::make_integer(&zval, val_type, loc);
      mpz_clear(zval);

      negative_index = Expression::make_binary(OPERATOR_LT, val, zero, loc);
    }

  int bound_type_size = bound_type->integer_type()->bits();
  if (val_type_size > bound_type_size
      || (val_type_size == bound_type_size
	  && val_is_unsigned))
    {
      mpz_t one;
      mpz_init_set_ui(one, 1UL);

      // maxval = 2^(bound_type_size - 1) - 1
      mpz_t maxval;
      mpz_init(maxval);
      mpz_mul_2exp(maxval, one, bound_type_size - 1);
      mpz_sub_ui(maxval, maxval, 1);
      Expression* max = Expression::make_integer(&maxval, val_type, loc);
      mpz_clear(one);
      mpz_clear(maxval);

      index_overflows = Expression::make_binary(OPERATOR_GT, val, max, loc);
    }

  return Expression::make_binary(OPERATOR_OROR, negative_index, index_overflows,
                                 loc);
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
  do_is_immutable() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc) const
  {
    nc->set_unsigned_long(NULL, 0);
    return true;
  }

  bool
  do_discarding_value()
  { return true; }

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
  bool is_in_heap;
  Location loc = this->location();
  if (this->variable_->is_variable())
    is_in_heap = this->variable_->var_value()->is_in_heap();
  else if (this->variable_->is_result_variable())
    is_in_heap = this->variable_->result_var_value()->is_in_heap();
  else
    go_unreachable();

  Bexpression* ret = context->backend()->var_expression(bvar, loc);
  if (is_in_heap)
    ret = context->backend()->indirect_expression(ret, true, loc);
  return expr_to_tree(ret);
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
  Gogo* gogo = context->gogo();
  Bvariable* bvar = this->statement_->get_backend_variable(context);
  Bexpression* ret = gogo->backend()->var_expression(bvar, this->location());

  // The backend can't always represent the same set of recursive types
  // that the Go frontend can.  In some cases this means that a
  // temporary variable won't have the right backend type.  Correct
  // that here by adding a type cast.  We need to use base() to push
  // the circularity down one level.
  Type* stype = this->statement_->type();
  if (!this->is_lvalue_
      && stype->has_pointer()
      && stype->deref()->is_void_type())
    {
      Btype* btype = this->type()->base()->get_backend(gogo);
      ret = gogo->backend()->convert_expression(btype, ret, this->location());
    }
  return expr_to_tree(ret);
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

// Class Set_and_use_temporary_expression.

// Return the type.

Type*
Set_and_use_temporary_expression::do_type()
{
  return this->statement_->type();
}

// Determine the type of the expression.

void
Set_and_use_temporary_expression::do_determine_type(
    const Type_context* context)
{
  this->expr_->determine_type(context);
}

// Take the address.

void
Set_and_use_temporary_expression::do_address_taken(bool)
{
  this->statement_->set_is_address_taken();
}

// Return the backend representation.

tree
Set_and_use_temporary_expression::do_get_tree(Translate_context* context)
{
  Bvariable* bvar = this->statement_->get_backend_variable(context);
  tree var_tree = var_to_tree(bvar);
  tree expr_tree = this->expr_->get_tree(context);
  if (var_tree == error_mark_node || expr_tree == error_mark_node)
    return error_mark_node;
  Location loc = this->location();
  return build2_loc(loc.gcc_location(), COMPOUND_EXPR, TREE_TYPE(var_tree),
		    build2_loc(loc.gcc_location(), MODIFY_EXPR, void_type_node,
			       var_tree, expr_tree),
		    var_tree);
}

// Dump.

void
Set_and_use_temporary_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << '(';
  ast_dump_context->dump_temp_variable_name(this->statement_);
  ast_dump_context->ostream() << " = ";
  this->expr_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ')';
}

// Make a set-and-use temporary.

Set_and_use_temporary_expression*
Expression::make_set_and_use_temporary(Temporary_statement* statement,
				       Expression* expr, Location location)
{
  return new Set_and_use_temporary_expression(statement, expr, location);
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
  bool
  do_discarding_value()
  { return true; }

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

// Get the tree for the code of a function expression.

Bexpression*
Func_expression::get_code_pointer(Gogo* gogo, Named_object* no, Location loc)
{
  Function_type* fntype;
  if (no->is_function())
    fntype = no->func_value()->type();
  else if (no->is_function_declaration())
    fntype = no->func_declaration_value()->type();
  else
    go_unreachable();

  // Builtin functions are handled specially by Call_expression.  We
  // can't take their address.
  if (fntype->is_builtin())
    {
      error_at(loc,
	       "invalid use of special builtin function %qs; must be called",
	       no->message_name().c_str());
      return gogo->backend()->error_expression();
    }

  Bfunction* fndecl;
  if (no->is_function())
    fndecl = no->func_value()->get_or_make_decl(gogo, no);
  else if (no->is_function_declaration())
    fndecl = no->func_declaration_value()->get_or_make_decl(gogo, no);
  else
    go_unreachable();

  return gogo->backend()->function_code_expression(fndecl, loc);
}

// Get the tree for a function expression.  This is used when we take
// the address of a function rather than simply calling it.  A func
// value is represented as a pointer to a block of memory.  The first
// word of that memory is a pointer to the function code.  The
// remaining parts of that memory are the addresses of variables that
// the function closes over.

tree
Func_expression::do_get_tree(Translate_context* context)
{
  // If there is no closure, just use the function descriptor.
  if (this->closure_ == NULL)
    {
      Gogo* gogo = context->gogo();
      Named_object* no = this->function_;
      Expression* descriptor;
      if (no->is_function())
	descriptor = no->func_value()->descriptor(gogo, no);
      else if (no->is_function_declaration())
	{
	  if (no->func_declaration_value()->type()->is_builtin())
	    {
	      error_at(this->location(),
		       ("invalid use of special builtin function %qs; "
			"must be called"),
		       no->message_name().c_str());
	      return error_mark_node;
	    }
	  descriptor = no->func_declaration_value()->descriptor(gogo, no);
	}
      else
	go_unreachable();

      tree dtree = descriptor->get_tree(context);
      if (dtree == error_mark_node)
	return error_mark_node;
      return build_fold_addr_expr_loc(this->location().gcc_location(), dtree);
    }

  go_assert(this->function_->func_value()->enclosing() != NULL);

  // If there is a closure, then the closure is itself the function
  // expression.  It is a pointer to a struct whose first field points
  // to the function code and whose remaining fields are the addresses
  // of the closed-over variables.
  return this->closure_->get_tree(context);
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

// Class Func_descriptor_expression.

// Constructor.

Func_descriptor_expression::Func_descriptor_expression(Named_object* fn)
  : Expression(EXPRESSION_FUNC_DESCRIPTOR, fn->location()),
    fn_(fn), dvar_(NULL)
{
  go_assert(!fn->is_function() || !fn->func_value()->needs_closure());
}

// Traversal.

int
Func_descriptor_expression::do_traverse(Traverse*)
{
  return TRAVERSE_CONTINUE;
}

// All function descriptors have the same type.

Type* Func_descriptor_expression::descriptor_type;

void
Func_descriptor_expression::make_func_descriptor_type()
{
  if (Func_descriptor_expression::descriptor_type != NULL)
    return;
  Type* uintptr_type = Type::lookup_integer_type("uintptr");
  Type* struct_type = Type::make_builtin_struct_type(1, "code", uintptr_type);
  Func_descriptor_expression::descriptor_type =
    Type::make_builtin_named_type("functionDescriptor", struct_type);
}

Type*
Func_descriptor_expression::do_type()
{
  Func_descriptor_expression::make_func_descriptor_type();
  return Func_descriptor_expression::descriptor_type;
}

// The tree for a function descriptor.

tree
Func_descriptor_expression::do_get_tree(Translate_context* context)
{
  if (this->dvar_ != NULL)
    return var_to_tree(this->dvar_);

  Gogo* gogo = context->gogo();
  Named_object* no = this->fn_;
  Location loc = no->location();

  std::string var_name;
  if (no->package() == NULL)
    var_name = gogo->pkgpath_symbol();
  else
    var_name = no->package()->pkgpath_symbol();
  var_name.push_back('.');
  var_name.append(Gogo::unpack_hidden_name(no->name()));
  var_name.append("$descriptor");

  Btype* btype = this->type()->get_backend(gogo);

  Bvariable* bvar;
  if (no->package() != NULL
      || Linemap::is_predeclared_location(no->location()))
    bvar = context->backend()->immutable_struct_reference(var_name, btype,
							  loc);
  else
    {
      Location bloc = Linemap::predeclared_location();
      bool is_hidden = ((no->is_function()
			 && no->func_value()->enclosing() != NULL)
			|| Gogo::is_thunk(no));
      bvar = context->backend()->immutable_struct(var_name, is_hidden, false,
						  btype, bloc);
      Expression_list* vals = new Expression_list();
      vals->push_back(Expression::make_func_code_reference(this->fn_, bloc));
      Expression* init =
	Expression::make_struct_composite_literal(this->type(), vals, bloc);
      Translate_context bcontext(gogo, NULL, NULL, NULL);
      bcontext.set_is_const();
      Bexpression* binit = tree_to_expr(init->get_tree(&bcontext));
      context->backend()->immutable_struct_set_init(bvar, var_name, is_hidden,
						    false, btype, bloc, binit);
    }

  this->dvar_ = bvar;
  return var_to_tree(bvar);
}

// Print a function descriptor expression.

void
Func_descriptor_expression::do_dump_expression(Ast_dump_context* context) const
{
  context->ostream() << "[descriptor " << this->fn_->name() << "]";
}

// Make a function descriptor expression.

Func_descriptor_expression*
Expression::make_func_descriptor(Named_object* fn)
{
  return new Func_descriptor_expression(fn);
}

// Make the function descriptor type, so that it can be converted.

void
Expression::make_func_descriptor_type()
{
  Func_descriptor_expression::make_func_descriptor_type();
}

// A reference to just the code of a function.

class Func_code_reference_expression : public Expression
{
 public:
  Func_code_reference_expression(Named_object* function, Location location)
    : Expression(EXPRESSION_FUNC_CODE_REFERENCE, location),
      function_(function)
  { }

 protected:
  int
  do_traverse(Traverse*)
  { return TRAVERSE_CONTINUE; }

  bool
  do_is_immutable() const
  { return true; }

  Type*
  do_type()
  { return Type::make_pointer_type(Type::make_void_type()); }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return Expression::make_func_code_reference(this->function_,
						this->location());
  }

  tree
  do_get_tree(Translate_context*);

  void
  do_dump_expression(Ast_dump_context* context) const
  { context->ostream() << "[raw " << this->function_->name() << "]" ; }

 private:
  // The function.
  Named_object* function_;
};

// Get the tree for a reference to function code.

tree
Func_code_reference_expression::do_get_tree(Translate_context* context)
{
  Bexpression* ret =
      Func_expression::get_code_pointer(context->gogo(), this->function_,
                                        this->location());
  return expr_to_tree(ret);
}

// Make a reference to the code of a function.

Expression*
Expression::make_func_code_reference(Named_object* function, Location location)
{
  return new Func_code_reference_expression(function, location);
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
	  if (!this->no_error_message_)
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
      if (!this->no_error_message_)
	error_at(location, "reference to undefined type %qs",
		 real->message_name().c_str());
      return Expression::make_error(location);
    case Named_object::NAMED_OBJECT_VAR:
      real->var_value()->set_is_used();
      return Expression::make_var_reference(real, location);
    case Named_object::NAMED_OBJECT_FUNC:
    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
      return Expression::make_func_reference(real, NULL, location);
    case Named_object::NAMED_OBJECT_PACKAGE:
      if (this->is_composite_literal_key_)
	return this;
      if (!this->no_error_message_)
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

Unknown_expression*
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

  bool
  do_is_immutable() const
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
  Gogo* gogo = context->gogo();
  Btype* btype = Type::make_string_type()->get_backend(gogo);

  Location loc = this->location();
  std::vector<Bexpression*> init(2);
  Bexpression* str_cst =
      gogo->backend()->string_constant_expression(this->val_);
  init[0] = gogo->backend()->address_expression(str_cst, loc);

  Btype* int_btype = Type::lookup_integer_type("int")->get_backend(gogo);
  mpz_t lenval;
  mpz_init_set_ui(lenval, this->val_.length());
  init[1] = gogo->backend()->integer_constant_expression(int_btype, lenval);
  mpz_clear(lenval);

  Bexpression* ret = gogo->backend()->constructor_expression(btype, init, loc);
  return expr_to_tree(ret);
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

// An expression that evaluates to some characteristic of a string.
// This is used when indexing, bound-checking, or nil checking a string.

class String_info_expression : public Expression
{
 public:
  String_info_expression(Expression* string, String_info string_info,
                        Location location)
    : Expression(EXPRESSION_STRING_INFO, location),
      string_(string), string_info_(string_info)
  { }

 protected:
  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { go_unreachable(); }

  Expression*
  do_copy()
  {
    return new String_info_expression(this->string_->copy(), this->string_info_,
				      this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

  void
  do_issue_nil_check()
  { this->string_->issue_nil_check(); }

 private:
  // The string for which we are getting information.
  Expression* string_;
  // What information we want.
  String_info string_info_;
};

// Return the type of the string info.

Type*
String_info_expression::do_type()
{
  switch (this->string_info_)
    {
    case STRING_INFO_DATA:
      {
	Type* byte_type = Type::lookup_integer_type("uint8");
	return Type::make_pointer_type(byte_type);
      }
    case STRING_INFO_LENGTH:
        return Type::lookup_integer_type("int");
    default:
      go_unreachable();
    }
}

// Return string information in GENERIC.

tree
String_info_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  Bexpression* bstring = tree_to_expr(this->string_->get_tree(context));
  Bexpression* ret;
  switch (this->string_info_)
    {
    case STRING_INFO_DATA:
    case STRING_INFO_LENGTH:
      ret = gogo->backend()->struct_field_expression(bstring, this->string_info_,
                                                     this->location());
      break;
    default:
      go_unreachable();
    }
  return expr_to_tree(ret);
}

// Dump ast representation for a type info expression.

void
String_info_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "stringinfo(";
  this->string_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ",";
  ast_dump_context->ostream() << 
      (this->string_info_ == STRING_INFO_DATA ? "data" 
    : this->string_info_ == STRING_INFO_LENGTH ? "length"
    : "unknown");
  ast_dump_context->ostream() << ")";
}

// Make a string info expression.

Expression*
Expression::make_string_info(Expression* string, String_info string_info,
                            Location location)
{
  return new String_info_expression(string, string_info, location);
}

// Make an integer expression.

class Integer_expression : public Expression
{
 public:
  Integer_expression(const mpz_t* val, Type* type, bool is_character_constant,
		     Location location)
    : Expression(EXPRESSION_INTEGER, location),
      type_(type), is_character_constant_(is_character_constant)
  { mpz_init_set(this->val_, *val); }

  static Expression*
  do_import(Import*);

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
  do_is_immutable() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc) const;

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
  {
    if (this->is_character_constant_)
      return Expression::make_character(&this->val_, this->type_,
					this->location());
    else
      return Expression::make_integer(&this->val_, this->type_,
				      this->location());
  }

  void
  do_export(Export*) const;

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The integer value.
  mpz_t val_;
  // The type so far.
  Type* type_;
  // Whether this is a character constant.
  bool is_character_constant_;
};

// Return a numeric constant for this expression.  We have to mark
// this as a character when appropriate.

bool
Integer_expression::do_numeric_constant_value(Numeric_constant* nc) const
{
  if (this->is_character_constant_)
    nc->set_rune(this->type_, this->val_);
  else
    nc->set_int(this->type_, this->val_);
  return true;
}

// Return the current type.  If we haven't set the type yet, we return
// an abstract integer type.

Type*
Integer_expression::do_type()
{
  if (this->type_ == NULL)
    {
      if (this->is_character_constant_)
	this->type_ = Type::make_abstract_character_type();
      else
	this->type_ = Type::make_abstract_integer_type();
    }
  return this->type_;
}

// Set the type of the integer value.  Here we may switch from an
// abstract type to a real type.

void
Integer_expression::do_determine_type(const Type_context* context)
{
  if (this->type_ != NULL && !this->type_->is_abstract())
    ;
  else if (context->type != NULL && context->type->is_numeric_type())
    this->type_ = context->type;
  else if (!context->may_be_abstract)
    {
      if (this->is_character_constant_)
	this->type_ = Type::lookup_integer_type("int32");
      else
	this->type_ = Type::lookup_integer_type("int");
    }
}

// Check the type of an integer constant.

void
Integer_expression::do_check_types(Gogo*)
{
  Type* type = this->type_;
  if (type == NULL)
    return;
  Numeric_constant nc;
  if (this->is_character_constant_)
    nc.set_rune(NULL, this->val_);
  else
    nc.set_int(NULL, this->val_);
  if (!nc.set_type(type, true, this->location()))
    this->set_is_error();
}

// Get a tree for an integer constant.

tree
Integer_expression::do_get_tree(Translate_context* context)
{
  Type* resolved_type = NULL;
  if (this->type_ != NULL && !this->type_->is_abstract())
    resolved_type = this->type_;
  else if (this->type_ != NULL && this->type_->float_type() != NULL)
    {
      // We are converting to an abstract floating point type.
      resolved_type = Type::lookup_float_type("float64");
    }
  else if (this->type_ != NULL && this->type_->complex_type() != NULL)
    {
      // We are converting to an abstract complex type.
      resolved_type = Type::lookup_complex_type("complex128");
    }
  else
    {
      // If we still have an abstract type here, then this is being
      // used in a constant expression which didn't get reduced for
      // some reason.  Use a type which will fit the value.  We use <,
      // not <=, because we need an extra bit for the sign bit.
      int bits = mpz_sizeinbase(this->val_, 2);
      Type* int_type = Type::lookup_integer_type("int");
      if (bits < int_type->integer_type()->bits())
	resolved_type = int_type;
      else if (bits < 64)
        resolved_type = Type::lookup_integer_type("int64");
      else
        {
          if (!saw_errors())
            error_at(this->location(),
                     "unknown type for large integer constant");
          Bexpression* ret = context->gogo()->backend()->error_expression();
          return expr_to_tree(ret);
        }
    }
  Numeric_constant nc;
  nc.set_int(resolved_type, this->val_);
  Bexpression* ret =
      Expression::backend_numeric_constant_expression(context, &nc);
  return expr_to_tree(ret);
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
  if (this->is_character_constant_)
    exp->write_c_string("'");
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
      bool is_character_constant = (!num.empty()
				    && num[num.length() - 1] == '\'');
      if (is_character_constant)
	num = num.substr(0, num.length() - 1);
      mpz_t val;
      if (mpz_init_set_str(val, num.c_str(), 10) != 0)
	{
	  error_at(imp->location(), "bad number in import data: %qs",
		   num.c_str());
	  return Expression::make_error(imp->location());
	}
      Expression* ret;
      if (is_character_constant)
	ret = Expression::make_character(&val, NULL, imp->location());
      else
	ret = Expression::make_integer(&val, NULL, imp->location());
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
  if (this->is_character_constant_)
    ast_dump_context->ostream() << '\'';
  Integer_expression::export_integer(ast_dump_context, this->val_);
  if (this->is_character_constant_)
    ast_dump_context->ostream() << '\'';
}

// Build a new integer value.

Expression*
Expression::make_integer(const mpz_t* val, Type* type, Location location)
{
  return new Integer_expression(val, type, false, location);
}

// Build a new character constant value.

Expression*
Expression::make_character(const mpz_t* val, Type* type, Location location)
{
  return new Integer_expression(val, type, true, location);
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
  do_is_immutable() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc) const
  {
    nc->set_float(this->type_, this->val_);
    return true;
  }

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

// Check the type of a float value.

void
Float_expression::do_check_types(Gogo*)
{
  Type* type = this->type_;
  if (type == NULL)
    return;
  Numeric_constant nc;
  nc.set_float(NULL, this->val_);
  if (!nc.set_type(this->type_, true, this->location()))
    this->set_is_error();
}

// Get a tree for a float constant.

tree
Float_expression::do_get_tree(Translate_context* context)
{
  Type* resolved_type;
  if (this->type_ != NULL && !this->type_->is_abstract())
    resolved_type = this->type_;
  else if (this->type_ != NULL && this->type_->integer_type() != NULL)
    {
      // We have an abstract integer type.  We just hope for the best.
      resolved_type = Type::lookup_integer_type("int");
    }
  else if (this->type_ != NULL && this->type_->complex_type() != NULL)
    {
      // We are converting to an abstract complex type.
      resolved_type = Type::lookup_complex_type("complex128");
    }
  else
    {
      // If we still have an abstract type here, then this is being
      // used in a constant expression which didn't get reduced.  We
      // just use float64 and hope for the best.
      resolved_type = Type::lookup_float_type("float64");
    }

  Numeric_constant nc;
  nc.set_float(resolved_type, this->val_);
  Bexpression* ret =
      Expression::backend_numeric_constant_expression(context, &nc);
  return expr_to_tree(ret);
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
  do_is_immutable() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc) const
  {
    nc->set_complex(this->type_, this->real_, this->imag_);
    return true;
  }

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

// Check the type of a complex value.

void
Complex_expression::do_check_types(Gogo*)
{
  Type* type = this->type_;
  if (type == NULL)
    return;
  Numeric_constant nc;
  nc.set_complex(NULL, this->real_, this->imag_);
  if (!nc.set_type(this->type_, true, this->location()))
    this->set_is_error();
}

// Get a tree for a complex constant.

tree
Complex_expression::do_get_tree(Translate_context* context)
{
  Type* resolved_type;
  if (this->type_ != NULL && !this->type_->is_abstract())
    resolved_type = this->type_;
  else if (this->type_ != NULL && this->type_->integer_type() != NULL)
    {
      // We are converting to an abstract integer type.
      resolved_type = Type::lookup_integer_type("int");
    }
  else if (this->type_ != NULL && this->type_->float_type() != NULL)
    {
      // We are converting to an abstract float type.
      resolved_type = Type::lookup_float_type("float64");
    }
  else
    {
      // If we still have an abstract type here, this this is being
      // used in a constant expression which didn't get reduced.  We
      // just use complex128 and hope for the best.
      resolved_type = Type::lookup_complex_type("complex128");
    }

  Numeric_constant nc;
  nc.set_complex(resolved_type, this->real_, this->imag_);
  Bexpression* ret =
      Expression::backend_numeric_constant_expression(context, &nc);
  return expr_to_tree(ret);
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
  do_is_immutable() const
  { return true; }

  bool
  do_numeric_constant_value(Numeric_constant* nc) const;

  bool
  do_string_constant_value(std::string* val) const;

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

// Return a numeric constant value.

bool
Const_expression::do_numeric_constant_value(Numeric_constant* nc) const
{
  if (this->seen_)
    return false;

  Expression* e = this->constant_->const_value()->expr();
  
  this->seen_ = true;

  bool r = e->numeric_constant_value(nc);

  this->seen_ = false;

  Type* ctype;
  if (this->type_ != NULL)
    ctype = this->type_;
  else
    ctype = this->constant_->const_value()->type();
  if (r && ctype != NULL)
    {
      if (!nc->set_type(ctype, false, this->location()))
	return false;
    }

  return r;
}

bool
Const_expression::do_string_constant_value(std::string* val) const
{
  if (this->seen_)
    return false;

  Expression* e = this->constant_->const_value()->expr();

  this->seen_ = true;
  bool ok = e->string_constant_value(val);
  this->seen_ = false;

  return ok;
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
	   && context->type->is_numeric_type()
	   && cetype->is_numeric_type())
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

  // Check that numeric constant fits in type.
  if (this->type_ != NULL && this->type_->is_numeric_type())
    {
      Numeric_constant nc;
      if (this->constant_->const_value()->expr()->numeric_constant_value(&nc))
	{
	  if (!nc.set_type(this->type_, true, this->location()))
	    this->set_is_error();
	}
    }
}

// Return a tree for the const reference.

tree
Const_expression::do_get_tree(Translate_context* context)
{
  if (this->type_ != NULL && this->type_->is_error())
    return error_mark_node;

  // If the type has been set for this expression, but the underlying
  // object is an abstract int or float, we try to get the abstract
  // value.  Otherwise we may lose something in the conversion.
  Expression* expr = this->constant_->const_value()->expr();
  if (this->type_ != NULL
      && this->type_->is_numeric_type()
      && (this->constant_->const_value()->type() == NULL
	  || this->constant_->const_value()->type()->is_abstract()))
    {
      Numeric_constant nc;
      if (expr->numeric_constant_value(&nc)
	  && nc.set_type(this->type_, false, this->location()))
	{
	  Expression* e = nc.expression(this->location());
	  return e->get_tree(context);
	}
    }

  if (this->type_ != NULL)
    expr = Expression::make_cast(this->type_, expr, this->location());
  return expr->get_tree(context);
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

  bool
  do_is_immutable() const
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

  Expression*
  do_flatten(Gogo*, Named_object*, Statement_inserter*);

  bool
  do_is_constant() const;

  bool
  do_is_immutable() const;

  bool
  do_numeric_constant_value(Numeric_constant*) const;

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

  if (type->is_numeric_type())
    {
      Numeric_constant nc;
      if (val->numeric_constant_value(&nc))
	{
	  if (!nc.set_type(type, true, location))
	    return Expression::make_error(location);
	  return nc.expression(location);
	}
    }

  if (type->is_slice_type())
    {
      Type* element_type = type->array_type()->element_type()->forwarded();
      bool is_byte = (element_type->integer_type() != NULL
		      && element_type->integer_type()->is_byte());
      bool is_rune = (element_type->integer_type() != NULL
		      && element_type->integer_type()->is_rune());
      if (is_byte || is_rune)
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

// Flatten a type conversion by using a temporary variable for the slice
// in slice to string conversions.

Expression*
Type_conversion_expression::do_flatten(Gogo*, Named_object*,
                                       Statement_inserter* inserter)
{
  if (((this->type()->is_string_type()
        && this->expr_->type()->is_slice_type())
       || (this->type()->interface_type() != NULL
           && this->expr_->type()->interface_type() != NULL))
      && !this->expr_->is_variable())
    {
      Temporary_statement* temp =
          Statement::make_temporary(NULL, this->expr_, this->location());
      inserter->insert(temp);
      this->expr_ = Expression::make_temporary_reference(temp, this->location());
    }
  return this;
}

// Return whether a type conversion is a constant.

bool
Type_conversion_expression::do_is_constant() const
{
  if (!this->expr_->is_constant())
    return false;

  // A conversion to a type that may not be used as a constant is not
  // a constant.  For example, []byte(nil).
  Type* type = this->type_;
  if (type->integer_type() == NULL
      && type->float_type() == NULL
      && type->complex_type() == NULL
      && !type->is_boolean_type()
      && !type->is_string_type())
    return false;

  return true;
}

// Return whether a type conversion is immutable.

bool
Type_conversion_expression::do_is_immutable() const
{
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();

  if (type->interface_type() != NULL
      || expr_type->interface_type() != NULL)
    return false;

  if (!this->expr_->is_immutable())
    return false;

  if (Type::are_identical(type, expr_type, false, NULL))
    return true;

  return type->is_basic_type() && expr_type->is_basic_type();
}

// Return the constant numeric value if there is one.

bool
Type_conversion_expression::do_numeric_constant_value(
    Numeric_constant* nc) const
{
  if (!this->type_->is_numeric_type())
    return false;
  if (!this->expr_->numeric_constant_value(nc))
    return false;
  return nc->set_type(this->type_, false, this->location());
}

// Return the constant string value if there is one.

bool
Type_conversion_expression::do_string_constant_value(std::string* val) const
{
  if (this->type_->is_string_type()
      && this->expr_->type()->integer_type() != NULL)
    {
      Numeric_constant nc;
      if (this->expr_->numeric_constant_value(&nc))
	{
	  unsigned long ival;
	  if (nc.to_unsigned_long(&ival) == Numeric_constant::NC_UL_VALID)
	    {
	      val->clear();
	      Lex::append_char(ival, true, val, this->location());
	      return true;
	    }
	}
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
  Type* type = this->type_;
  Type* expr_type = this->expr_->type();

  Gogo* gogo = context->gogo();
  Btype* btype = type->get_backend(gogo);
  Bexpression* bexpr = tree_to_expr(this->expr_->get_tree(context));
  Location loc = this->location();

  if (Type::are_identical(type, expr_type, false, NULL))
    {
      Bexpression* bconvert =
        gogo->backend()->convert_expression(btype, bexpr, loc);
      return expr_to_tree(bconvert);
    }
  else if (type->interface_type() != NULL
	   || expr_type->interface_type() != NULL)
    {
      Expression* conversion =
          Expression::convert_for_assignment(gogo, type, this->expr_,
                                             this->location());
      return conversion->get_tree(context);
    }
  else if (type->is_string_type()
	   && expr_type->integer_type() != NULL)
    {
      mpz_t intval;
      Numeric_constant nc;
      if (this->expr_->numeric_constant_value(&nc)
	  && nc.to_int(&intval)
	  && mpz_fits_ushort_p(intval))
	{
	  std::string s;
	  Lex::append_char(mpz_get_ui(intval), true, &s, loc);
	  mpz_clear(intval);
	  Expression* se = Expression::make_string(s, loc);
	  return se->get_tree(context);
	}

      Expression* i2s_expr =
          Runtime::make_call(Runtime::INT_TO_STRING, loc, 1, this->expr_);
      return Expression::make_cast(type, i2s_expr, loc)->get_tree(context);
    }
  else if (type->is_string_type() && expr_type->is_slice_type())
    {
      Array_type* a = expr_type->array_type();
      Type* e = a->element_type()->forwarded();
      go_assert(e->integer_type() != NULL);
      go_assert(this->expr_->is_variable());

      Runtime::Function code;
      if (e->integer_type()->is_byte())
        code = Runtime::BYTE_ARRAY_TO_STRING;
      else
        {
          go_assert(e->integer_type()->is_rune());
          code = Runtime::INT_ARRAY_TO_STRING;
        }
      Expression* valptr = a->get_value_pointer(gogo, this->expr_);
      Expression* len = a->get_length(gogo, this->expr_);
      return Runtime::make_call(code, loc, 2, valptr, len)->get_tree(context);
    }
  else if (type->is_slice_type() && expr_type->is_string_type())
    {
      Type* e = type->array_type()->element_type()->forwarded();
      go_assert(e->integer_type() != NULL);

      Runtime::Function code;
      if (e->integer_type()->is_byte())
	code = Runtime::STRING_TO_BYTE_ARRAY;
      else
	{
	  go_assert(e->integer_type()->is_rune());
	  code = Runtime::STRING_TO_INT_ARRAY;
	}
      Expression* s2a = Runtime::make_call(code, loc, 1, this->expr_);
      return Expression::make_unsafe_cast(type, s2a, loc)->get_tree(context);
    }
  else if (type->is_numeric_type())
    {
      go_assert(Type::are_convertible(type, expr_type, NULL));
      Bexpression* bconvert =
	gogo->backend()->convert_expression(btype, bexpr, loc);
      return expr_to_tree(bconvert);
    }
  else if ((type->is_unsafe_pointer_type()
	    && (expr_type->points_to() != NULL
                || expr_type->integer_type()))
           || (expr_type->is_unsafe_pointer_type()
	       && type->points_to() != NULL)
           || (this->may_convert_function_types_
               && type->function_type() != NULL
               && expr_type->function_type() != NULL))
    {
      Bexpression* bconvert =
	gogo->backend()->convert_expression(btype, bexpr, loc);
      return expr_to_tree(bconvert);
    }
  else
    {
      Expression* conversion =
          Expression::convert_for_assignment(gogo, type, this->expr_, loc);
      return conversion->get_tree(context);
    }
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
  if (t->array_type() != NULL)
    go_assert(et->array_type() != NULL
              && t->is_slice_type() == et->is_slice_type());
  else if (t->struct_type() != NULL)
    {
      if (t->named_type() != NULL
          && et->named_type() != NULL
          && !Type::are_convertible(t, et, NULL))
	{
	  go_assert(saw_errors());
	  return error_mark_node;
	}

      go_assert(et->struct_type() != NULL
                && Type::are_convertible(t, et, NULL));
    }
  else if (t->map_type() != NULL)
    go_assert(et->map_type() != NULL);
  else if (t->channel_type() != NULL)
    go_assert(et->channel_type() != NULL);
  else if (t->points_to() != NULL)
    go_assert(et->points_to() != NULL
              || et->channel_type() != NULL
              || et->map_type() != NULL
              || et->function_type() != NULL
              || et->is_nil_type());
  else if (et->is_unsafe_pointer_type())
    go_assert(t->points_to() != NULL);
  else if (t->interface_type() != NULL)
    {
      bool empty_iface = t->interface_type()->is_empty();
      go_assert(et->interface_type() != NULL
                && et->interface_type()->is_empty() == empty_iface);
    }
  else if (t->integer_type() != NULL)
    go_assert(et->is_boolean_type()
              || et->integer_type() != NULL
              || et->function_type() != NULL
              || et->points_to() != NULL
              || et->map_type() != NULL
              || et->channel_type() != NULL);
  else
    go_unreachable();

  Gogo* gogo = context->gogo();
  Btype* btype = t->get_backend(gogo);
  Bexpression* bexpr = tree_to_expr(this->expr_->get_tree(context));
  Location loc = this->location();
  Bexpression* ret =
    gogo->backend()->convert_expression(btype, bexpr, loc);
  return expr_to_tree(ret);
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

// Class Unary_expression.

// If we are taking the address of a composite literal, and the
// contents are not constant, then we want to make a heap expression
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
		  if (!ue->expr_->is_addressable() && !ue->create_temp_)
		    {
		      error_at(ue->location(),
			       "invalid operand for unary %<&%>");
		      this->set_is_error();
		    }
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

  if (op == OPERATOR_PLUS || op == OPERATOR_MINUS || op == OPERATOR_XOR)
    {
      Numeric_constant nc;
      if (expr->numeric_constant_value(&nc))
	{
	  Numeric_constant result;
	  if (Unary_expression::eval_constant(op, &nc, loc, &result))
	    return result.expression(loc);
	}
    }

  return this;
}

// Flatten expression if a nil check must be performed and create temporary
// variables if necessary.

Expression*
Unary_expression::do_flatten(Gogo* gogo, Named_object*,
                             Statement_inserter* inserter)
{
  if (this->is_error_expression() || this->expr_->is_error_expression())
    return Expression::make_error(this->location());

  Location location = this->location();
  if (this->op_ == OPERATOR_MULT
      && !this->expr_->is_variable())
    {
      go_assert(this->expr_->type()->points_to() != NULL);
      Type* ptype = this->expr_->type()->points_to();
      if (!ptype->is_void_type())
        {
          Btype* pbtype = ptype->get_backend(gogo);
          size_t s = gogo->backend()->type_size(pbtype);
          if (s >= 4096 || this->issue_nil_check_)
            {
              Temporary_statement* temp =
                  Statement::make_temporary(NULL, this->expr_, location);
              inserter->insert(temp);
              this->expr_ =
                  Expression::make_temporary_reference(temp, location);
            }
        }
    }

  if (this->create_temp_ && !this->expr_->is_variable())
    {
      Temporary_statement* temp =
          Statement::make_temporary(NULL, this->expr_, location);
      inserter->insert(temp);
      this->expr_ = Expression::make_temporary_reference(temp, location);
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
      // global variable, not constant otherwise.  In other cases taking the
      // address is probably not a constant.
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

// Apply unary opcode OP to UNC, setting NC.  Return true if this
// could be done, false if not.  Issue errors for overflow.

bool
Unary_expression::eval_constant(Operator op, const Numeric_constant* unc,
				Location location, Numeric_constant* nc)
{
  switch (op)
    {
    case OPERATOR_PLUS:
      *nc = *unc;
      return true;

    case OPERATOR_MINUS:
      if (unc->is_int() || unc->is_rune())
	break;
      else if (unc->is_float())
	{
	  mpfr_t uval;
	  unc->get_float(&uval);
	  mpfr_t val;
	  mpfr_init(val);
	  mpfr_neg(val, uval, GMP_RNDN);
	  nc->set_float(unc->type(), val);
	  mpfr_clear(uval);
	  mpfr_clear(val);
	  return true;
	}
      else if (unc->is_complex())
	{
	  mpfr_t ureal, uimag;
	  unc->get_complex(&ureal, &uimag);
	  mpfr_t real, imag;
	  mpfr_init(real);
	  mpfr_init(imag);
	  mpfr_neg(real, ureal, GMP_RNDN);
	  mpfr_neg(imag, uimag, GMP_RNDN);
	  nc->set_complex(unc->type(), real, imag);
	  mpfr_clear(ureal);
	  mpfr_clear(uimag);
	  mpfr_clear(real);
	  mpfr_clear(imag);
	  return true;
	}
      else
	go_unreachable();

    case OPERATOR_XOR:
      break;

    case OPERATOR_NOT:
    case OPERATOR_AND:
    case OPERATOR_MULT:
      return false;

    default:
      go_unreachable();
    }

  if (!unc->is_int() && !unc->is_rune())
    return false;

  mpz_t uval;
  if (unc->is_rune())
    unc->get_rune(&uval);
  else
    unc->get_int(&uval);
  mpz_t val;
  mpz_init(val);

  switch (op)
    {
    case OPERATOR_MINUS:
      mpz_neg(val, uval);
      break;

    case OPERATOR_NOT:
      mpz_set_ui(val, mpz_cmp_si(uval, 0) == 0 ? 1 : 0);
      break;

    case OPERATOR_XOR:
      {
	Type* utype = unc->type();
	if (utype->integer_type() == NULL
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

	    size_t obits = utype->integer_type()->bits();

	    if (!utype->integer_type()->is_unsigned() && mpz_sgn(uval) < 0)
	      {
		mpz_t adj;
		mpz_init_set_ui(adj, 1);
		mpz_mul_2exp(adj, adj, obits);
		mpz_add(uval, uval, adj);
		mpz_clear(adj);
	      }

	    size_t ecount;
	    mpz_export(phwi, &ecount, -1, sizeof(HOST_WIDE_INT), 0, 0, uval);
	    go_assert(ecount <= count);

	    // Trim down to the number of words required by the type.
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

	    if (!utype->integer_type()->is_unsigned()
		&& mpz_tstbit(val, obits - 1))
	      {
		mpz_t adj;
		mpz_init_set_ui(adj, 1);
		mpz_mul_2exp(adj, adj, obits);
		mpz_sub(val, val, adj);
		mpz_clear(adj);
	      }

	    delete[] phwi;
	  }
      }
      break;

    default:
      go_unreachable();
    }

  if (unc->is_rune())
    nc->set_rune(NULL, val);
  else
    nc->set_int(NULL, val);

  mpz_clear(uval);
  mpz_clear(val);

  return nc->set_type(unc->type(), true, location);
}

// Return the integral constant value of a unary expression, if it has one.

bool
Unary_expression::do_numeric_constant_value(Numeric_constant* nc) const
{
  Numeric_constant unc;
  if (!this->expr_->numeric_constant_value(&unc))
    return false;
  return Unary_expression::eval_constant(this->op_, &unc, this->location(),
					 nc);
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
      if (!type->is_boolean_type())
	this->report_error(_("expected boolean type"));
      break;

    case OPERATOR_XOR:
      if (type->integer_type() == NULL
	  && !type->is_boolean_type())
	this->report_error(_("expected integer or boolean type"));
      break;

    case OPERATOR_AND:
      if (!this->expr_->is_addressable())
	{
	  if (!this->create_temp_)
	    {
	      error_at(this->location(), "invalid operand for unary %<&%>");
	      this->set_is_error();
	    }
	}
      else
        {
          this->expr_->address_taken(this->escapes_);
          this->expr_->issue_nil_check();
        }
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
  Gogo* gogo = context->gogo();
  Location loc = this->location();

  // Taking the address of a set-and-use-temporary expression requires
  // setting the temporary and then taking the address.
  if (this->op_ == OPERATOR_AND)
    {
      Set_and_use_temporary_expression* sut =
	this->expr_->set_and_use_temporary_expression();
      if (sut != NULL)
	{
	  Temporary_statement* temp = sut->temporary();
	  Bvariable* bvar = temp->get_backend_variable(context);
          Bexpression* bvar_expr = gogo->backend()->var_expression(bvar, loc);

          Expression* val = sut->expression();
          Bexpression* bval = tree_to_expr(val->get_tree(context));

          Bstatement* bassign =
              gogo->backend()->assignment_statement(bvar_expr, bval, loc);
          Bexpression* bvar_addr =
              gogo->backend()->address_expression(bvar_expr, loc);
          Bexpression* ret =
              gogo->backend()->compound_expression(bassign, bvar_addr, loc);
          return expr_to_tree(ret);
	}
    }

  Bexpression* ret;
  tree expr = this->expr_->get_tree(context);
  Bexpression* bexpr = tree_to_expr(expr);
  Btype* btype = this->expr_->type()->get_backend(gogo);
  switch (this->op_)
    {
    case OPERATOR_PLUS:
      ret = bexpr;
      break;

    case OPERATOR_MINUS:
      ret = gogo->backend()->unary_expression(this->op_, bexpr, loc);
      ret = gogo->backend()->convert_expression(btype, ret, loc);
      break;

    case OPERATOR_NOT:
    case OPERATOR_XOR:
      ret = gogo->backend()->unary_expression(this->op_, bexpr, loc);
      break;

    case OPERATOR_AND:
      if (!this->create_temp_)
	{
	  // We should not see a non-constant constructor here; cases
	  // where we would see one should have been moved onto the
	  // heap at parse time.  Taking the address of a nonconstant
	  // constructor will not do what the programmer expects.

          go_assert(!this->expr_->is_composite_literal()
                    || this->expr_->is_immutable());
	  if (this->expr_->classification() == EXPRESSION_UNARY)
	    {
	      Unary_expression* ue =
		static_cast<Unary_expression*>(this->expr_);
	      go_assert(ue->op() != OPERATOR_AND);
	    }
	}

      if (this->is_gc_root_)
	{
	  // Build a decl for a GC root variable.  GC roots are mutable, so they
	  // cannot be represented as an immutable_struct in the backend.
	  Bvariable* gc_root = gogo->backend()->gc_root_variable(btype, bexpr);
	  bexpr = gogo->backend()->var_expression(gc_root, loc);
	}
      else if ((this->expr_->is_composite_literal()
           || this->expr_->string_expression() != NULL)
          && this->expr_->is_immutable())
        {
	  // Build a decl for a constant constructor.
          static unsigned int counter;
          char buf[100];
          snprintf(buf, sizeof buf, "C%u", counter);
          ++counter;

          Bvariable* decl =
              gogo->backend()->immutable_struct(buf, true, false, btype, loc);
          gogo->backend()->immutable_struct_set_init(decl, buf, true, false,
                                                     btype, loc, bexpr);
          bexpr = gogo->backend()->var_expression(decl, loc);
        }

      go_assert(!this->create_temp_ || this->expr_->is_variable());
      ret = gogo->backend()->address_expression(bexpr, loc);
      break;

    case OPERATOR_MULT:
      {
        go_assert(this->expr_->type()->points_to() != NULL);

	// If we are dereferencing the pointer to a large struct, we
	// need to check for nil.  We don't bother to check for small
	// structs because we expect the system to crash on a nil
	// pointer dereference.	 However, if we know the address of this
	// expression is being taken, we must always check for nil.

        Type* ptype = this->expr_->type()->points_to();
        Btype* pbtype = ptype->get_backend(gogo);
        if (!ptype->is_void_type())
	  {
            size_t s = gogo->backend()->type_size(pbtype);
	    if (s >= 4096 || this->issue_nil_check_)
	      {
                go_assert(this->expr_->is_variable());

                Expression* nil_expr = Expression::make_nil(loc);
                Bexpression* nil = tree_to_expr(nil_expr->get_tree(context));
                Bexpression* compare =
                    gogo->backend()->binary_expression(OPERATOR_EQEQ, bexpr,
                                                       nil, loc);

		Expression* crash_expr =
		    gogo->runtime_error(RUNTIME_ERROR_NIL_DEREFERENCE, loc);
                Bexpression* crash =
                    tree_to_expr(crash_expr->get_tree(context));
                bexpr = gogo->backend()->conditional_expression(btype, compare,
                                                                crash, bexpr,
                                                                loc);

	      }
	  }

	// If the type of EXPR is a recursive pointer type, then we
	// need to insert a cast before indirecting.
        tree expr = expr_to_tree(bexpr);
        tree target_type_tree = TREE_TYPE(TREE_TYPE(expr));
        if (VOID_TYPE_P(target_type_tree))
          {
            tree ind = type_to_tree(pbtype);
            expr = fold_convert_loc(loc.gcc_location(),
                                    build_pointer_type(ind), expr);
            bexpr = tree_to_expr(expr);
          }

        ret = gogo->backend()->indirect_expression(bexpr, false, loc);
      }
      break;

    default:
      go_unreachable();
    }

  return expr_to_tree(ret);
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

// Return the type to use for a binary operation on operands of
// LEFT_TYPE and RIGHT_TYPE.  These are the types of constants and as
// such may be NULL or abstract.

bool
Binary_expression::operation_type(Operator op, Type* left_type,
				  Type* right_type, Type** result_type)
{
  if (left_type != right_type
      && !left_type->is_abstract()
      && !right_type->is_abstract()
      && left_type->base() != right_type->base()
      && op != OPERATOR_LSHIFT
      && op != OPERATOR_RSHIFT)
    {
      // May be a type error--let it be diagnosed elsewhere.
      return false;
    }

  if (op == OPERATOR_LSHIFT || op == OPERATOR_RSHIFT)
    {
      if (left_type->integer_type() != NULL)
	*result_type = left_type;
      else
	*result_type = Type::make_abstract_integer_type();
    }
  else if (!left_type->is_abstract() && left_type->named_type() != NULL)
    *result_type = left_type;
  else if (!right_type->is_abstract() && right_type->named_type() != NULL)
    *result_type = right_type;
  else if (!left_type->is_abstract())
    *result_type = left_type;
  else if (!right_type->is_abstract())
    *result_type = right_type;
  else if (left_type->complex_type() != NULL)
    *result_type = left_type;
  else if (right_type->complex_type() != NULL)
    *result_type = right_type;
  else if (left_type->float_type() != NULL)
    *result_type = left_type;
  else if (right_type->float_type() != NULL)
    *result_type = right_type;
  else if (left_type->integer_type() != NULL
	   && left_type->integer_type()->is_rune())
    *result_type = left_type;
  else if (right_type->integer_type() != NULL
	   && right_type->integer_type()->is_rune())
    *result_type = right_type;
  else
    *result_type = left_type;

  return true;
}

// Convert an integer comparison code and an operator to a boolean
// value.

bool
Binary_expression::cmp_to_bool(Operator op, int cmp)
{
  switch (op)
    {
    case OPERATOR_EQEQ:
      return cmp == 0;
      break;
    case OPERATOR_NOTEQ:
      return cmp != 0;
      break;
    case OPERATOR_LT:
      return cmp < 0;
      break;
    case OPERATOR_LE:
      return cmp <= 0;
    case OPERATOR_GT:
      return cmp > 0;
    case OPERATOR_GE:
      return cmp >= 0;
    default:
      go_unreachable();
    }
}

// Compare constants according to OP.

bool
Binary_expression::compare_constant(Operator op, Numeric_constant* left_nc,
				    Numeric_constant* right_nc,
				    Location location, bool* result)
{
  Type* left_type = left_nc->type();
  Type* right_type = right_nc->type();

  Type* type;
  if (!Binary_expression::operation_type(op, left_type, right_type, &type))
    return false;

  // When comparing an untyped operand to a typed operand, we are
  // effectively coercing the untyped operand to the other operand's
  // type, so make sure that is valid.
  if (!left_nc->set_type(type, true, location)
      || !right_nc->set_type(type, true, location))
    return false;

  bool ret;
  int cmp;
  if (type->complex_type() != NULL)
    {
      if (op != OPERATOR_EQEQ && op != OPERATOR_NOTEQ)
	return false;
      ret = Binary_expression::compare_complex(left_nc, right_nc, &cmp);
    }
  else if (type->float_type() != NULL)
    ret = Binary_expression::compare_float(left_nc, right_nc, &cmp);
  else
    ret = Binary_expression::compare_integer(left_nc, right_nc, &cmp);

  if (ret)
    *result = Binary_expression::cmp_to_bool(op, cmp);

  return ret;
}

// Compare integer constants.

bool
Binary_expression::compare_integer(const Numeric_constant* left_nc,
				   const Numeric_constant* right_nc,
				   int* cmp)
{
  mpz_t left_val;
  if (!left_nc->to_int(&left_val))
    return false;
  mpz_t right_val;
  if (!right_nc->to_int(&right_val))
    {
      mpz_clear(left_val);
      return false;
    }

  *cmp = mpz_cmp(left_val, right_val);

  mpz_clear(left_val);
  mpz_clear(right_val);

  return true;
}

// Compare floating point constants.

bool
Binary_expression::compare_float(const Numeric_constant* left_nc,
				 const Numeric_constant* right_nc,
				 int* cmp)
{
  mpfr_t left_val;
  if (!left_nc->to_float(&left_val))
    return false;
  mpfr_t right_val;
  if (!right_nc->to_float(&right_val))
    {
      mpfr_clear(left_val);
      return false;
    }

  // We already coerced both operands to the same type.  If that type
  // is not an abstract type, we need to round the values accordingly.
  Type* type = left_nc->type();
  if (!type->is_abstract() && type->float_type() != NULL)
    {
      int bits = type->float_type()->bits();
      mpfr_prec_round(left_val, bits, GMP_RNDN);
      mpfr_prec_round(right_val, bits, GMP_RNDN);
    }

  *cmp = mpfr_cmp(left_val, right_val);

  mpfr_clear(left_val);
  mpfr_clear(right_val);

  return true;
}

// Compare complex constants.  Complex numbers may only be compared
// for equality.

bool
Binary_expression::compare_complex(const Numeric_constant* left_nc,
				   const Numeric_constant* right_nc,
				   int* cmp)
{
  mpfr_t left_real, left_imag;
  if (!left_nc->to_complex(&left_real, &left_imag))
    return false;
  mpfr_t right_real, right_imag;
  if (!right_nc->to_complex(&right_real, &right_imag))
    {
      mpfr_clear(left_real);
      mpfr_clear(left_imag);
      return false;
    }

  // We already coerced both operands to the same type.  If that type
  // is not an abstract type, we need to round the values accordingly.
  Type* type = left_nc->type();
  if (!type->is_abstract() && type->complex_type() != NULL)
    {
      int bits = type->complex_type()->bits();
      mpfr_prec_round(left_real, bits / 2, GMP_RNDN);
      mpfr_prec_round(left_imag, bits / 2, GMP_RNDN);
      mpfr_prec_round(right_real, bits / 2, GMP_RNDN);
      mpfr_prec_round(right_imag, bits / 2, GMP_RNDN);
    }

  *cmp = (mpfr_cmp(left_real, right_real) != 0
	  || mpfr_cmp(left_imag, right_imag) != 0);

  mpfr_clear(left_real);
  mpfr_clear(left_imag);
  mpfr_clear(right_real);
  mpfr_clear(right_imag);

  return true;
}

// Apply binary opcode OP to LEFT_NC and RIGHT_NC, setting NC.  Return
// true if this could be done, false if not.  Issue errors at LOCATION
// as appropriate.

bool
Binary_expression::eval_constant(Operator op, Numeric_constant* left_nc,
				 Numeric_constant* right_nc,
				 Location location, Numeric_constant* nc)
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
      // These return boolean values, not numeric.
      return false;
    default:
      break;
    }

  Type* left_type = left_nc->type();
  Type* right_type = right_nc->type();

  Type* type;
  if (!Binary_expression::operation_type(op, left_type, right_type, &type))
    return false;

  bool is_shift = op == OPERATOR_LSHIFT || op == OPERATOR_RSHIFT;

  // When combining an untyped operand with a typed operand, we are
  // effectively coercing the untyped operand to the other operand's
  // type, so make sure that is valid.
  if (!left_nc->set_type(type, true, location))
    return false;
  if (!is_shift && !right_nc->set_type(type, true, location))
    return false;

  bool r;
  if (type->complex_type() != NULL)
    r = Binary_expression::eval_complex(op, left_nc, right_nc, location, nc);
  else if (type->float_type() != NULL)
    r = Binary_expression::eval_float(op, left_nc, right_nc, location, nc);
  else
    r = Binary_expression::eval_integer(op, left_nc, right_nc, location, nc);

  if (r)
    r = nc->set_type(type, true, location);

  return r;
}

// Apply binary opcode OP to LEFT_NC and RIGHT_NC, setting NC, using
// integer operations.  Return true if this could be done, false if
// not.

bool
Binary_expression::eval_integer(Operator op, const Numeric_constant* left_nc,
				const Numeric_constant* right_nc,
				Location location, Numeric_constant* nc)
{
  mpz_t left_val;
  if (!left_nc->to_int(&left_val))
    return false;
  mpz_t right_val;
  if (!right_nc->to_int(&right_val))
    {
      mpz_clear(left_val);
      return false;
    }

  mpz_t val;
  mpz_init(val);

  switch (op)
    {
    case OPERATOR_PLUS:
      mpz_add(val, left_val, right_val);
      if (mpz_sizeinbase(val, 2) > 0x100000)
	{
	  error_at(location, "constant addition overflow");
	  mpz_set_ui(val, 1);
	}
      break;
    case OPERATOR_MINUS:
      mpz_sub(val, left_val, right_val);
      if (mpz_sizeinbase(val, 2) > 0x100000)
	{
	  error_at(location, "constant subtraction overflow");
	  mpz_set_ui(val, 1);
	}
      break;
    case OPERATOR_OR:
      mpz_ior(val, left_val, right_val);
      break;
    case OPERATOR_XOR:
      mpz_xor(val, left_val, right_val);
      break;
    case OPERATOR_MULT:
      mpz_mul(val, left_val, right_val);
      if (mpz_sizeinbase(val, 2) > 0x100000)
	{
	  error_at(location, "constant multiplication overflow");
	  mpz_set_ui(val, 1);
	}
      break;
    case OPERATOR_DIV:
      if (mpz_sgn(right_val) != 0)
	mpz_tdiv_q(val, left_val, right_val);
      else
	{
	  error_at(location, "division by zero");
	  mpz_set_ui(val, 0);
	}
      break;
    case OPERATOR_MOD:
      if (mpz_sgn(right_val) != 0)
	mpz_tdiv_r(val, left_val, right_val);
      else
	{
	  error_at(location, "division by zero");
	  mpz_set_ui(val, 0);
	}
      break;
    case OPERATOR_LSHIFT:
      {
	unsigned long shift = mpz_get_ui(right_val);
	if (mpz_cmp_ui(right_val, shift) == 0 && shift <= 0x100000)
	  mpz_mul_2exp(val, left_val, shift);
	else
	  {
	    error_at(location, "shift count overflow");
	    mpz_set_ui(val, 1);
	  }
	break;
      }
      break;
    case OPERATOR_RSHIFT:
      {
	unsigned long shift = mpz_get_ui(right_val);
	if (mpz_cmp_ui(right_val, shift) != 0)
	  {
	    error_at(location, "shift count overflow");
	    mpz_set_ui(val, 1);
	  }
	else
	  {
	    if (mpz_cmp_ui(left_val, 0) >= 0)
	      mpz_tdiv_q_2exp(val, left_val, shift);
	    else
	      mpz_fdiv_q_2exp(val, left_val, shift);
	  }
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

  mpz_clear(left_val);
  mpz_clear(right_val);

  if (left_nc->is_rune()
      || (op != OPERATOR_LSHIFT
	  && op != OPERATOR_RSHIFT
	  && right_nc->is_rune()))
    nc->set_rune(NULL, val);
  else
    nc->set_int(NULL, val);

  mpz_clear(val);

  return true;
}

// Apply binary opcode OP to LEFT_NC and RIGHT_NC, setting NC, using
// floating point operations.  Return true if this could be done,
// false if not.

bool
Binary_expression::eval_float(Operator op, const Numeric_constant* left_nc,
			      const Numeric_constant* right_nc,
			      Location location, Numeric_constant* nc)
{
  mpfr_t left_val;
  if (!left_nc->to_float(&left_val))
    return false;
  mpfr_t right_val;
  if (!right_nc->to_float(&right_val))
    {
      mpfr_clear(left_val);
      return false;
    }

  mpfr_t val;
  mpfr_init(val);

  bool ret = true;
  switch (op)
    {
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
    case OPERATOR_MOD:
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      mpfr_set_ui(val, 0, GMP_RNDN);
      ret = false;
      break;
    case OPERATOR_MULT:
      mpfr_mul(val, left_val, right_val, GMP_RNDN);
      break;
    case OPERATOR_DIV:
      if (!mpfr_zero_p(right_val))
	mpfr_div(val, left_val, right_val, GMP_RNDN);
      else
	{
	  error_at(location, "division by zero");
	  mpfr_set_ui(val, 0, GMP_RNDN);
	}
      break;
    default:
      go_unreachable();
    }

  mpfr_clear(left_val);
  mpfr_clear(right_val);

  nc->set_float(NULL, val);
  mpfr_clear(val);

  return ret;
}

// Apply binary opcode OP to LEFT_NC and RIGHT_NC, setting NC, using
// complex operations.  Return true if this could be done, false if
// not.

bool
Binary_expression::eval_complex(Operator op, const Numeric_constant* left_nc,
				const Numeric_constant* right_nc,
				Location location, Numeric_constant* nc)
{
  mpfr_t left_real, left_imag;
  if (!left_nc->to_complex(&left_real, &left_imag))
    return false;
  mpfr_t right_real, right_imag;
  if (!right_nc->to_complex(&right_real, &right_imag))
    {
      mpfr_clear(left_real);
      mpfr_clear(left_imag);
      return false;
    }

  mpfr_t real, imag;
  mpfr_init(real);
  mpfr_init(imag);

  bool ret = true;
  switch (op)
    {
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
    case OPERATOR_MOD:
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      mpfr_set_ui(real, 0, GMP_RNDN);
      mpfr_set_ui(imag, 0, GMP_RNDN);
      ret = false;
      break;
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
	  {
	    error_at(location, "division by zero");
	    mpfr_set_ui(real, 0, GMP_RNDN);
	    mpfr_set_ui(imag, 0, GMP_RNDN);
	    break;
	  }

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
    default:
      go_unreachable();
    }

  mpfr_clear(left_real);
  mpfr_clear(left_imag);
  mpfr_clear(right_real);
  mpfr_clear(right_imag);

  nc->set_complex(NULL, real, imag);
  mpfr_clear(real);
  mpfr_clear(imag);

  return ret;
}

// Lower a binary expression.  We have to evaluate constant
// expressions now, in order to implement Go's unlimited precision
// constants.

Expression*
Binary_expression::do_lower(Gogo* gogo, Named_object*,
			    Statement_inserter* inserter, int)
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

  // Numeric constant expressions.
  {
    Numeric_constant left_nc;
    Numeric_constant right_nc;
    if (left->numeric_constant_value(&left_nc)
	&& right->numeric_constant_value(&right_nc))
      {
	if (is_comparison)
	  {
	    bool result;
	    if (!Binary_expression::compare_constant(op, &left_nc,
						     &right_nc, location,
						     &result))
	      return this;
	    return Expression::make_cast(Type::make_boolean_type(),
					 Expression::make_boolean(result,
								  location),
					 location);
	  }
	else
	  {
	    Numeric_constant nc;
	    if (!Binary_expression::eval_constant(op, &left_nc, &right_nc,
						  location, &nc))
	      return this;
	    return nc.expression(location);
	  }
      }
  }

  // String constant expressions.
  if (left->type()->is_string_type() && right->type()->is_string_type())
    {
      std::string left_string;
      std::string right_string;
      if (left->string_constant_value(&left_string)
	  && right->string_constant_value(&right_string))
	{
	  if (op == OPERATOR_PLUS)
	    return Expression::make_string(left_string + right_string,
					   location);
	  else if (is_comparison)
	    {
	      int cmp = left_string.compare(right_string);
	      bool r = Binary_expression::cmp_to_bool(op, cmp);
	      return Expression::make_boolean(r, location);
	    }
	}
    }

  // Lower struct, array, and some interface comparisons.
  if (op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ)
    {
      if (left->type()->struct_type() != NULL)
	return this->lower_struct_comparison(gogo, inserter);
      else if (left->type()->array_type() != NULL
	       && !left->type()->is_slice_type())
	return this->lower_array_comparison(gogo, inserter);
      else if ((left->type()->interface_type() != NULL
                && right->type()->interface_type() == NULL)
               || (left->type()->interface_type() == NULL
                   && right->type()->interface_type() != NULL))
	return this->lower_interface_value_comparison(gogo, inserter);
    }

  return this;
}

// Lower a struct comparison.

Expression*
Binary_expression::lower_struct_comparison(Gogo* gogo,
					   Statement_inserter* inserter)
{
  Struct_type* st = this->left_->type()->struct_type();
  Struct_type* st2 = this->right_->type()->struct_type();
  if (st2 == NULL)
    return this;
  if (st != st2 && !Type::are_identical(st, st2, false, NULL))
    return this;
  if (!Type::are_compatible_for_comparison(true, this->left_->type(),
					   this->right_->type(), NULL))
    return this;

  // See if we can compare using memcmp.  As a heuristic, we use
  // memcmp rather than field references and comparisons if there are
  // more than two fields.
  if (st->compare_is_identity(gogo) && st->total_field_count() > 2)
    return this->lower_compare_to_memcmp(gogo, inserter);

  Location loc = this->location();

  Expression* left = this->left_;
  Temporary_statement* left_temp = NULL;
  if (left->var_expression() == NULL
      && left->temporary_reference_expression() == NULL)
    {
      left_temp = Statement::make_temporary(left->type(), NULL, loc);
      inserter->insert(left_temp);
      left = Expression::make_set_and_use_temporary(left_temp, left, loc);
    }

  Expression* right = this->right_;
  Temporary_statement* right_temp = NULL;
  if (right->var_expression() == NULL
      && right->temporary_reference_expression() == NULL)
    {
      right_temp = Statement::make_temporary(right->type(), NULL, loc);
      inserter->insert(right_temp);
      right = Expression::make_set_and_use_temporary(right_temp, right, loc);
    }

  Expression* ret = Expression::make_boolean(true, loc);
  const Struct_field_list* fields = st->fields();
  unsigned int field_index = 0;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf, ++field_index)
    {
      if (Gogo::is_sink_name(pf->field_name()))
	continue;

      if (field_index > 0)
	{
	  if (left_temp == NULL)
	    left = left->copy();
	  else
	    left = Expression::make_temporary_reference(left_temp, loc);
	  if (right_temp == NULL)
	    right = right->copy();
	  else
	    right = Expression::make_temporary_reference(right_temp, loc);
	}
      Expression* f1 = Expression::make_field_reference(left, field_index,
							loc);
      Expression* f2 = Expression::make_field_reference(right, field_index,
							loc);
      Expression* cond = Expression::make_binary(OPERATOR_EQEQ, f1, f2, loc);
      ret = Expression::make_binary(OPERATOR_ANDAND, ret, cond, loc);
    }

  if (this->op_ == OPERATOR_NOTEQ)
    ret = Expression::make_unary(OPERATOR_NOT, ret, loc);

  return ret;
}

// Lower an array comparison.

Expression*
Binary_expression::lower_array_comparison(Gogo* gogo,
					  Statement_inserter* inserter)
{
  Array_type* at = this->left_->type()->array_type();
  Array_type* at2 = this->right_->type()->array_type();
  if (at2 == NULL)
    return this;
  if (at != at2 && !Type::are_identical(at, at2, false, NULL))
    return this;
  if (!Type::are_compatible_for_comparison(true, this->left_->type(),
					   this->right_->type(), NULL))
    return this;

  // Call memcmp directly if possible.  This may let the middle-end
  // optimize the call.
  if (at->compare_is_identity(gogo))
    return this->lower_compare_to_memcmp(gogo, inserter);

  // Call the array comparison function.
  Named_object* hash_fn;
  Named_object* equal_fn;
  at->type_functions(gogo, this->left_->type()->named_type(), NULL, NULL,
		     &hash_fn, &equal_fn);

  Location loc = this->location();

  Expression* func = Expression::make_func_reference(equal_fn, NULL, loc);

  Expression_list* args = new Expression_list();
  args->push_back(this->operand_address(inserter, this->left_));
  args->push_back(this->operand_address(inserter, this->right_));
  args->push_back(Expression::make_type_info(at, TYPE_INFO_SIZE));

  Expression* ret = Expression::make_call(func, args, false, loc);

  if (this->op_ == OPERATOR_NOTEQ)
    ret = Expression::make_unary(OPERATOR_NOT, ret, loc);

  return ret;
}

// Lower an interface to value comparison.

Expression*
Binary_expression::lower_interface_value_comparison(Gogo*,
                                                    Statement_inserter* inserter)
{
  Type* left_type = this->left_->type();
  Type* right_type = this->right_->type();
  Interface_type* ift;
  if (left_type->interface_type() != NULL)
    {
      ift = left_type->interface_type();
      if (!ift->implements_interface(right_type, NULL))
        return this;
    }
  else
    {
      ift = right_type->interface_type();
      if (!ift->implements_interface(left_type, NULL))
        return this;
    }
  if (!Type::are_compatible_for_comparison(true, left_type, right_type, NULL))
    return this;

  Location loc = this->location();

  if (left_type->interface_type() == NULL
      && left_type->points_to() == NULL
      && !this->left_->is_addressable())
    {
      Temporary_statement* temp =
          Statement::make_temporary(left_type, NULL, loc);
      inserter->insert(temp);
      this->left_ =
          Expression::make_set_and_use_temporary(temp, this->left_, loc);
    }

  if (right_type->interface_type() == NULL
      && right_type->points_to() == NULL
      && !this->right_->is_addressable())
    {
      Temporary_statement* temp =
          Statement::make_temporary(right_type, NULL, loc);
      inserter->insert(temp);
      this->right_ =
          Expression::make_set_and_use_temporary(temp, this->right_, loc);
    }

  return this;
}

// Lower a struct or array comparison to a call to memcmp.

Expression*
Binary_expression::lower_compare_to_memcmp(Gogo*, Statement_inserter* inserter)
{
  Location loc = this->location();

  Expression* a1 = this->operand_address(inserter, this->left_);
  Expression* a2 = this->operand_address(inserter, this->right_);
  Expression* len = Expression::make_type_info(this->left_->type(),
					       TYPE_INFO_SIZE);

  Expression* call = Runtime::make_call(Runtime::MEMCMP, loc, 3, a1, a2, len);

  mpz_t zval;
  mpz_init_set_ui(zval, 0);
  Expression* zero = Expression::make_integer(&zval, NULL, loc);
  mpz_clear(zval);

  return Expression::make_binary(this->op_, call, zero, loc);
}

Expression*
Binary_expression::do_flatten(Gogo*, Named_object*,
                              Statement_inserter* inserter)
{
  Location loc = this->location();
  Temporary_statement* temp;
  if (this->left_->type()->is_string_type()
      && this->op_ == OPERATOR_PLUS)
    {
      if (!this->left_->is_variable())
        {
          temp = Statement::make_temporary(NULL, this->left_, loc);
          inserter->insert(temp);
          this->left_ = Expression::make_temporary_reference(temp, loc);
        }
      if (!this->right_->is_variable())
        {
          temp =
              Statement::make_temporary(this->left_->type(), this->right_, loc);
          this->right_ = Expression::make_temporary_reference(temp, loc);
          inserter->insert(temp);
        }
    }

  Type* left_type = this->left_->type();
  bool is_shift_op = (this->op_ == OPERATOR_LSHIFT
                      || this->op_ == OPERATOR_RSHIFT);
  bool is_idiv_op = ((this->op_ == OPERATOR_DIV &&
                      left_type->integer_type() != NULL)
                     || this->op_ == OPERATOR_MOD);

  // FIXME: go_check_divide_zero and go_check_divide_overflow are globals
  // defined in gcc/go/lang.opt.  These should be defined in go_create_gogo
  // and accessed from the Gogo* passed to do_flatten.
  if (is_shift_op
      || (is_idiv_op && (go_check_divide_zero || go_check_divide_overflow)))
    {
      if (!this->left_->is_variable())
        {
          temp = Statement::make_temporary(NULL, this->left_, loc);
          inserter->insert(temp);
          this->left_ = Expression::make_temporary_reference(temp, loc);
        }
      if (!this->right_->is_variable())
        {
          temp =
              Statement::make_temporary(NULL, this->right_, loc);
          this->right_ = Expression::make_temporary_reference(temp, loc);
          inserter->insert(temp);
        }
    }
  return this;
}


// Return the address of EXPR, cast to unsafe.Pointer.

Expression*
Binary_expression::operand_address(Statement_inserter* inserter,
				   Expression* expr)
{
  Location loc = this->location();

  if (!expr->is_addressable())
    {
      Temporary_statement* temp = Statement::make_temporary(expr->type(), NULL,
							    loc);
      inserter->insert(temp);
      expr = Expression::make_set_and_use_temporary(temp, expr, loc);
    }
  expr = Expression::make_unary(OPERATOR_AND, expr, loc);
  static_cast<Unary_expression*>(expr)->set_does_not_escape();
  Type* void_type = Type::make_void_type();
  Type* unsafe_pointer_type = Type::make_pointer_type(void_type);
  return Expression::make_cast(unsafe_pointer_type, expr, loc);
}

// Return the numeric constant value, if it has one.

bool
Binary_expression::do_numeric_constant_value(Numeric_constant* nc) const
{
  Numeric_constant left_nc;
  if (!this->left_->numeric_constant_value(&left_nc))
    return false;
  Numeric_constant right_nc;
  if (!this->right_->numeric_constant_value(&right_nc))
    return false;
  return Binary_expression::eval_constant(this->op_, &left_nc, &right_nc,
					  this->location(), nc);
}

// Note that the value is being discarded.

bool
Binary_expression::do_discarding_value()
{
  if (this->op_ == OPERATOR_OROR || this->op_ == OPERATOR_ANDAND)
    return this->right_->discarding_value();
  else
    {
      this->unused_value_error();
      return false;
    }
}

// Get type.

Type*
Binary_expression::do_type()
{
  if (this->classification() == EXPRESSION_ERROR)
    return Type::make_error_type();

  switch (this->op_)
    {
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      if (this->type_ == NULL)
	this->type_ = Type::make_boolean_type();
      return this->type_;

    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_MULT:
    case OPERATOR_DIV:
    case OPERATOR_MOD:
    case OPERATOR_AND:
    case OPERATOR_BITCLEAR:
    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
      {
	Type* type;
	if (!Binary_expression::operation_type(this->op_,
					       this->left_->type(),
					       this->right_->type(),
					       &type))
	  return Type::make_error_type();
	return type;
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

  if (this->op_ == OPERATOR_ANDAND || this->op_ == OPERATOR_OROR)
    {
      // For a logical operation, the context does not determine the
      // types of the operands.  The operands must be some boolean
      // type but if the context has a boolean type they do not
      // inherit it.  See http://golang.org/issue/3924.
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
	  && !subcontext.may_be_abstract
	  && subcontext.type->interface_type() == NULL
	  && subcontext.type->integer_type() == NULL)
	this->report_error(("invalid context-determined non-integer type "
			    "for left operand of shift"));

      // The context for the right hand operand is the same as for the
      // left hand operand, except for a shift operator.
      subcontext.type = Type::lookup_integer_type("uint");
      subcontext.may_be_abstract = false;
    }

  this->right_->determine_type(&subcontext);

  if (is_comparison)
    {
      if (this->type_ != NULL && !this->type_->is_abstract())
	;
      else if (context->type != NULL && context->type->is_boolean_type())
	this->type_ = context->type;
      else if (!context->may_be_abstract)
	this->type_ = Type::lookup_bool_type();
    }
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
      {
	std::string reason;
	if (!Type::are_compatible_for_comparison(true, type, otype, &reason))
	  {
	    error_at(location, "%s", reason.c_str());
	    return false;
	  }
      }
      break;

    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      {
	std::string reason;
	if (!Type::are_compatible_for_comparison(false, type, otype, &reason))
	  {
	    error_at(location, "%s", reason.c_str());
	    return false;
	  }
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
      if (left_type->is_nil_type() && right_type->is_nil_type())
	{
	  this->report_error(_("invalid comparison of nil with nil"));
	  return;
	}
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
      if (this->op_ == OPERATOR_DIV || this->op_ == OPERATOR_MOD)
	{
	  // Division by a zero integer constant is an error.
	  Numeric_constant rconst;
	  unsigned long rval;
	  if (left_type->integer_type() != NULL
	      && this->right_->numeric_constant_value(&rconst)
	      && rconst.to_unsigned_long(&rval) == Numeric_constant::NC_UL_VALID
	      && rval == 0)
	    {
	      this->report_error(_("integer division by zero"));
	      return;
	    }
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
	  Numeric_constant nc;
	  if (this->right_->numeric_constant_value(&nc))
	    {
	      mpz_t val;
	      if (!nc.to_int(&val))
		this->report_error(_("shift count not unsigned integer"));
	      else
		{
		  if (mpz_sgn(val) < 0)
		    {
		      this->report_error(_("negative shift count"));
		      mpz_set_ui(val, 0);
		      Location rloc = this->right_->location();
		      this->right_ = Expression::make_integer(&val, right_type,
							      rloc);
		    }
		  mpz_clear(val);
		}
	    }
	}
    }
}

// Get a tree for a binary expression.

tree
Binary_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Location loc = this->location();
  Type* left_type = this->left_->type();
  Type* right_type = this->right_->type();

  bool use_left_type = true;
  bool is_shift_op = false;
  bool is_idiv_op = false;
  switch (this->op_)
    {
    case OPERATOR_EQEQ:
    case OPERATOR_NOTEQ:
    case OPERATOR_LT:
    case OPERATOR_LE:
    case OPERATOR_GT:
    case OPERATOR_GE:
      {
        Bexpression* ret =
            Expression::comparison(context, this->type_, this->op_,
                                   this->left_, this->right_, loc);
        return expr_to_tree(ret);
      }

    case OPERATOR_OROR:
    case OPERATOR_ANDAND:
      use_left_type = false;
      break;
    case OPERATOR_PLUS:
    case OPERATOR_MINUS:
    case OPERATOR_OR:
    case OPERATOR_XOR:
    case OPERATOR_MULT:
      break;
    case OPERATOR_DIV:
      if (left_type->float_type() != NULL || left_type->complex_type() != NULL)
        break;
    case OPERATOR_MOD:
      is_idiv_op = true;
      break;
    case OPERATOR_LSHIFT:
    case OPERATOR_RSHIFT:
      is_shift_op = true;
      break;
    case OPERATOR_BITCLEAR:
      this->right_ = Expression::make_unary(OPERATOR_XOR, this->right_, loc);
    case OPERATOR_AND:
      break;
    default:
      go_unreachable();
    }

  if (left_type->is_string_type())
    {
      go_assert(this->op_ == OPERATOR_PLUS);
      Expression* string_plus =
          Runtime::make_call(Runtime::STRING_PLUS, loc, 2,
                             this->left_, this->right_);
      return string_plus->get_tree(context);
    }

  // For complex division Go might want slightly different results than the
  // backend implementation provides, so we have our own runtime routine.
  if (this->op_ == OPERATOR_DIV && this->left_->type()->complex_type() != NULL)
    {
      Runtime::Function complex_code;
      switch (this->left_->type()->complex_type()->bits())
	{
	case 64:
          complex_code = Runtime::COMPLEX64_DIV;
	  break;
	case 128:
          complex_code = Runtime::COMPLEX128_DIV;
	  break;
	default:
	  go_unreachable();
	}
      Expression* complex_div =
          Runtime::make_call(complex_code, loc, 2, this->left_, this->right_);
      return complex_div->get_tree(context);
    }

  Bexpression* left = tree_to_expr(this->left_->get_tree(context));
  Bexpression* right = tree_to_expr(this->right_->get_tree(context));

  Type* type = use_left_type ? left_type : right_type;
  Btype* btype = type->get_backend(gogo);

  Bexpression* ret =
      gogo->backend()->binary_expression(this->op_, left, right, loc);
  ret = gogo->backend()->convert_expression(btype, ret, loc);

  // Initialize overflow constants.
  Bexpression* overflow;
  mpz_t zero;
  mpz_init_set_ui(zero, 0UL);
  mpz_t one;
  mpz_init_set_ui(one, 1UL);
  mpz_t neg_one;
  mpz_init_set_si(neg_one, -1);

  Btype* left_btype = left_type->get_backend(gogo);
  Btype* right_btype = right_type->get_backend(gogo);

  // In Go, a shift larger than the size of the type is well-defined.
  // This is not true in C, so we need to insert a conditional.
  if (is_shift_op)
    {
      go_assert(left_type->integer_type() != NULL);

      mpz_t bitsval;
      int bits = left_type->integer_type()->bits();
      mpz_init_set_ui(bitsval, bits);
      Bexpression* bits_expr =
          gogo->backend()->integer_constant_expression(right_btype, bitsval);
      Bexpression* compare =
          gogo->backend()->binary_expression(OPERATOR_LT,
                                             right, bits_expr, loc);

      Bexpression* zero_expr =
          gogo->backend()->integer_constant_expression(left_btype, zero);
      overflow = zero_expr;
      if (this->op_ == OPERATOR_RSHIFT
	  && !left_type->integer_type()->is_unsigned())
	{
          Bexpression* neg_expr =
              gogo->backend()->binary_expression(OPERATOR_LT, left,
                                                 zero_expr, loc);
          Bexpression* neg_one_expr =
              gogo->backend()->integer_constant_expression(left_btype, neg_one);
          overflow = gogo->backend()->conditional_expression(btype, neg_expr,
                                                             neg_one_expr,
                                                             zero_expr, loc);
	}
      ret = gogo->backend()->conditional_expression(btype, compare, ret,
                                                    overflow, loc);
      mpz_clear(bitsval);
    }

  // Add checks for division by zero and division overflow as needed.
  if (is_idiv_op)
    {
      if (go_check_divide_zero)
	{
	  // right == 0
          Bexpression* zero_expr =
              gogo->backend()->integer_constant_expression(right_btype, zero);
          Bexpression* check =
              gogo->backend()->binary_expression(OPERATOR_EQEQ,
                                                 right, zero_expr, loc);

	  // __go_runtime_error(RUNTIME_ERROR_DIVISION_BY_ZERO)
	  int errcode = RUNTIME_ERROR_DIVISION_BY_ZERO;
	  Expression* crash = gogo->runtime_error(errcode, loc);
          Bexpression* crash_expr = tree_to_expr(crash->get_tree(context));

	  // right == 0 ? (__go_runtime_error(...), 0) : ret
          ret = gogo->backend()->conditional_expression(btype, check,
                                                        crash_expr, ret, loc);
	}

      if (go_check_divide_overflow)
	{
	  // right == -1
	  // FIXME: It would be nice to say that this test is expected
	  // to return false.

          Bexpression* neg_one_expr =
              gogo->backend()->integer_constant_expression(right_btype, neg_one);
          Bexpression* check =
              gogo->backend()->binary_expression(OPERATOR_EQEQ,
                                                 right, neg_one_expr, loc);

          Bexpression* zero_expr =
              gogo->backend()->integer_constant_expression(btype, zero);
          Bexpression* one_expr =
              gogo->backend()->integer_constant_expression(btype, one);

	  if (type->integer_type()->is_unsigned())
	    {
	      // An unsigned -1 is the largest possible number, so
	      // dividing is always 1 or 0.

              Bexpression* cmp =
                  gogo->backend()->binary_expression(OPERATOR_EQEQ,
                                                     left, right, loc);
	      if (this->op_ == OPERATOR_DIV)
                overflow =
                    gogo->backend()->conditional_expression(btype, cmp,
                                                            one_expr, zero_expr,
                                                            loc);
	      else
                overflow =
                    gogo->backend()->conditional_expression(btype, cmp,
                                                            zero_expr, left,
                                                            loc);
	    }
	  else
	    {
	      // Computing left / -1 is the same as computing - left,
	      // which does not overflow since Go sets -fwrapv.
	      if (this->op_ == OPERATOR_DIV)
                {
                  Expression* negate_expr =
                      Expression::make_unary(OPERATOR_MINUS, this->left_, loc);
                  overflow = tree_to_expr(negate_expr->get_tree(context));
                }
	      else
                overflow = zero_expr;
	    }
          overflow = gogo->backend()->convert_expression(btype, overflow, loc);

	  // right == -1 ? - left : ret
          ret = gogo->backend()->conditional_expression(btype, check, overflow,
                                                        ret, loc);
	}
    }

  mpz_clear(zero);
  mpz_clear(one);
  mpz_clear(neg_one);
  return expr_to_tree(ret);
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

Bexpression*
Expression::comparison(Translate_context* context, Type* result_type,
		       Operator op, Expression* left, Expression* right,
		       Location location)
{
  Type* left_type = left->type();
  Type* right_type = right->type();

  mpz_t zval;
  mpz_init_set_ui(zval, 0UL);
  Expression* zexpr = Expression::make_integer(&zval, NULL, location);
  mpz_clear(zval);

  if (left_type->is_string_type() && right_type->is_string_type())
    {
      left = Runtime::make_call(Runtime::STRCMP, location, 2,
                                left, right);
      right = zexpr;
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
	  std::swap(left, right);
	}

      // The right operand is not an interface.  We need to take its
      // address if it is not a pointer.
      Expression* pointer_arg = NULL;
      if (right_type->points_to() != NULL)
        pointer_arg = right;
      else
	{
          go_assert(right->is_addressable());
          pointer_arg = Expression::make_unary(OPERATOR_AND, right,
                                               location);
	}

      Expression* descriptor =
          Expression::make_type_descriptor(right_type, location);
      left =
          Runtime::make_call((left_type->interface_type()->is_empty()
                              ? Runtime::EMPTY_INTERFACE_VALUE_COMPARE
                              : Runtime::INTERFACE_VALUE_COMPARE),
                             location, 3, left, descriptor,
                             pointer_arg);
      right = zexpr;
    }
  else if (left_type->interface_type() != NULL
	   && right_type->interface_type() != NULL)
    {
      Runtime::Function compare_function;
      if (left_type->interface_type()->is_empty()
	  && right_type->interface_type()->is_empty())
	compare_function = Runtime::EMPTY_INTERFACE_COMPARE;
      else if (!left_type->interface_type()->is_empty()
	       && !right_type->interface_type()->is_empty())
	compare_function = Runtime::INTERFACE_COMPARE;
      else
	{
	  if (left_type->interface_type()->is_empty())
	    {
	      go_assert(op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ);
	      std::swap(left_type, right_type);
	      std::swap(left, right);
	    }
	  go_assert(!left_type->interface_type()->is_empty());
	  go_assert(right_type->interface_type()->is_empty());
	  compare_function = Runtime::INTERFACE_EMPTY_COMPARE;
	}

      left = Runtime::make_call(compare_function, location, 2, left, right);
      right = zexpr;
    }

  if (left_type->is_nil_type()
      && (op == OPERATOR_EQEQ || op == OPERATOR_NOTEQ))
    {
      std::swap(left_type, right_type);
      std::swap(left, right);
    }

  if (right_type->is_nil_type())
    {
      right = Expression::make_nil(location);
      if (left_type->array_type() != NULL
	  && left_type->array_type()->length() == NULL)
	{
	  Array_type* at = left_type->array_type();
          left = at->get_value_pointer(context->gogo(), left);
	}
      else if (left_type->interface_type() != NULL)
	{
	  // An interface is nil if the first field is nil.
          left = Expression::make_field_reference(left, 0, location);
	}
    }

  Bexpression* left_bexpr = tree_to_expr(left->get_tree(context));
  Bexpression* right_bexpr = tree_to_expr(right->get_tree(context));

  Gogo* gogo = context->gogo();
  Bexpression* ret = gogo->backend()->binary_expression(op, left_bexpr,
                                                        right_bexpr, location);
  if (result_type != NULL)
    ret = gogo->backend()->convert_expression(result_type->get_backend(gogo),
                                              ret, location);
  return ret;
}

// Class Bound_method_expression.

// Traversal.

int
Bound_method_expression::do_traverse(Traverse* traverse)
{
  return Expression::traverse(&this->expr_, traverse);
}

// Lower the expression.  If this is a method value rather than being
// called, and the method is accessed via a pointer, we may need to
// add nil checks.  Introduce a temporary variable so that those nil
// checks do not cause multiple evaluation.

Expression*
Bound_method_expression::do_lower(Gogo*, Named_object*,
				  Statement_inserter* inserter, int)
{
  // For simplicity we use a temporary for every call to an embedded
  // method, even though some of them might be pure value methods and
  // not require a temporary.
  if (this->expr_->var_expression() == NULL
      && this->expr_->temporary_reference_expression() == NULL
      && this->expr_->set_and_use_temporary_expression() == NULL
      && (this->method_->field_indexes() != NULL
	  || (this->method_->is_value_method()
	      && this->expr_->type()->points_to() != NULL)))
    {
      Temporary_statement* temp =
	Statement::make_temporary(this->expr_->type(), NULL, this->location());
      inserter->insert(temp);
      this->expr_ = Expression::make_set_and_use_temporary(temp, this->expr_,
							   this->location());
    }
  return this;
}

// Return the type of a bound method expression.  The type of this
// object is simply the type of the method with no receiver.

Type*
Bound_method_expression::do_type()
{
  Named_object* fn = this->method_->named_object();
  Function_type* fntype;
  if (fn->is_function())
    fntype = fn->func_value()->type();
  else if (fn->is_function_declaration())
    fntype = fn->func_declaration_value()->type();
  else
    return Type::make_error_type();
  return fntype->copy_without_receiver();
}

// Determine the types of a method expression.

void
Bound_method_expression::do_determine_type(const Type_context*)
{
  Named_object* fn = this->method_->named_object();
  Function_type* fntype;
  if (fn->is_function())
    fntype = fn->func_value()->type();
  else if (fn->is_function_declaration())
    fntype = fn->func_declaration_value()->type();
  else
    fntype = NULL;
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
  Named_object* fn = this->method_->named_object();
  if (!fn->is_function() && !fn->is_function_declaration())
    {
      this->report_error(_("object is not a method"));
      return;
    }

  Function_type* fntype;
  if (fn->is_function())
    fntype = fn->func_value()->type();
  else if (fn->is_function_declaration())
    fntype = fn->func_declaration_value()->type();
  else
    go_unreachable();
  Type* rtype = fntype->receiver()->type()->deref();
  Type* etype = (this->expr_type_ != NULL
		 ? this->expr_type_
		 : this->expr_->type());
  etype = etype->deref();
  if (!Type::are_identical(rtype, etype, true, NULL))
    this->report_error(_("method type does not match object type"));
}

// If a bound method expression is not simply called, then it is
// represented as a closure.  The closure will hold a single variable,
// the receiver to pass to the method.  The function will be a simple
// thunk that pulls that value from the closure and calls the method
// with the remaining arguments.
//
// Because method values are not common, we don't build all thunks for
// every methods, but instead only build them as we need them.  In
// particular, we even build them on demand for methods defined in
// other packages.

Bound_method_expression::Method_value_thunks
  Bound_method_expression::method_value_thunks;

// Find or create the thunk for METHOD.

Named_object*
Bound_method_expression::create_thunk(Gogo* gogo, const Method* method,
				      Named_object* fn)
{
  std::pair<Named_object*, Named_object*> val(fn, NULL);
  std::pair<Method_value_thunks::iterator, bool> ins =
    Bound_method_expression::method_value_thunks.insert(val);
  if (!ins.second)
    {
      // We have seen this method before.
      go_assert(ins.first->second != NULL);
      return ins.first->second;
    }

  Location loc = fn->location();

  Function_type* orig_fntype;
  if (fn->is_function())
    orig_fntype = fn->func_value()->type();
  else if (fn->is_function_declaration())
    orig_fntype = fn->func_declaration_value()->type();
  else
    orig_fntype = NULL;

  if (orig_fntype == NULL || !orig_fntype->is_method())
    {
      ins.first->second = Named_object::make_erroneous_name(Gogo::thunk_name());
      return ins.first->second;
    }

  Struct_field_list* sfl = new Struct_field_list();
  // The type here is wrong--it should be the C function type.  But it
  // doesn't really matter.
  Type* vt = Type::make_pointer_type(Type::make_void_type());
  sfl->push_back(Struct_field(Typed_identifier("fn.0", vt, loc)));
  sfl->push_back(Struct_field(Typed_identifier("val.1",
					       orig_fntype->receiver()->type(),
					       loc)));
  Type* closure_type = Type::make_struct_type(sfl, loc);
  closure_type = Type::make_pointer_type(closure_type);

  Function_type* new_fntype = orig_fntype->copy_with_names();

  Named_object* new_no = gogo->start_function(Gogo::thunk_name(), new_fntype,
					      false, loc);

  Variable* cvar = new Variable(closure_type, NULL, false, false, false, loc);
  cvar->set_is_used();
  Named_object* cp = Named_object::make_variable("$closure", NULL, cvar);
  new_no->func_value()->set_closure_var(cp);

  gogo->start_block(loc);

  // Field 0 of the closure is the function code pointer, field 1 is
  // the value on which to invoke the method.
  Expression* arg = Expression::make_var_reference(cp, loc);
  arg = Expression::make_unary(OPERATOR_MULT, arg, loc);
  arg = Expression::make_field_reference(arg, 1, loc);

  Expression* bme = Expression::make_bound_method(arg, method, fn, loc);

  const Typed_identifier_list* orig_params = orig_fntype->parameters();
  Expression_list* args;
  if (orig_params == NULL || orig_params->empty())
    args = NULL;
  else
    {
      const Typed_identifier_list* new_params = new_fntype->parameters();
      args = new Expression_list();
      for (Typed_identifier_list::const_iterator p = new_params->begin();
	   p != new_params->end();
	   ++p)
	{
	  Named_object* p_no = gogo->lookup(p->name(), NULL);
	  go_assert(p_no != NULL
		    && p_no->is_variable()
		    && p_no->var_value()->is_parameter());
	  args->push_back(Expression::make_var_reference(p_no, loc));
	}
    }

  Call_expression* call = Expression::make_call(bme, args,
						orig_fntype->is_varargs(),
						loc);
  call->set_varargs_are_lowered();

  Statement* s = Statement::make_return_from_call(call, loc);
  gogo->add_statement(s);
  Block* b = gogo->finish_block(loc);
  gogo->add_block(b, loc);
  gogo->lower_block(new_no, b);
  gogo->flatten_block(new_no, b);
  gogo->finish_function(loc);

  ins.first->second = new_no;
  return new_no;
}

// Return an expression to check *REF for nil while dereferencing
// according to FIELD_INDEXES.  Update *REF to build up the field
// reference.  This is a static function so that we don't have to
// worry about declaring Field_indexes in expressions.h.

static Expression*
bme_check_nil(const Method::Field_indexes* field_indexes, Location loc,
	      Expression** ref)
{
  if (field_indexes == NULL)
    return Expression::make_boolean(false, loc);
  Expression* cond = bme_check_nil(field_indexes->next, loc, ref);
  Struct_type* stype = (*ref)->type()->deref()->struct_type();
  go_assert(stype != NULL
	    && field_indexes->field_index < stype->field_count());
  if ((*ref)->type()->struct_type() == NULL)
    {
      go_assert((*ref)->type()->points_to() != NULL);
      Expression* n = Expression::make_binary(OPERATOR_EQEQ, *ref,
					      Expression::make_nil(loc),
					      loc);
      cond = Expression::make_binary(OPERATOR_OROR, cond, n, loc);
      *ref = Expression::make_unary(OPERATOR_MULT, *ref, loc);
      go_assert((*ref)->type()->struct_type() == stype);
    }
  *ref = Expression::make_field_reference(*ref, field_indexes->field_index,
					  loc);
  return cond;
}

// Get the tree for a method value.

tree
Bound_method_expression::do_get_tree(Translate_context* context)
{
  Named_object* thunk = Bound_method_expression::create_thunk(context->gogo(),
							      this->method_,
							      this->function_);
  if (thunk->is_erroneous())
    {
      go_assert(saw_errors());
      return error_mark_node;
    }

  // FIXME: We should lower this earlier, but we can't lower it in the
  // lowering pass because at that point we don't know whether we need
  // to create the thunk or not.  If the expression is called, we
  // don't need the thunk.

  Location loc = this->location();

  // If the method expects a value, and we have a pointer, we need to
  // dereference the pointer.

  Named_object* fn = this->method_->named_object();
  Function_type* fntype;
  if (fn->is_function())
    fntype = fn->func_value()->type();
  else if (fn->is_function_declaration())
    fntype = fn->func_declaration_value()->type();
  else
    go_unreachable();

  Expression* val = this->expr_;
  if (fntype->receiver()->type()->points_to() == NULL
      && val->type()->points_to() != NULL)
    val = Expression::make_unary(OPERATOR_MULT, val, loc);

  // Note that we are ignoring this->expr_type_ here.  The thunk will
  // expect a closure whose second field has type this->expr_type_ (if
  // that is not NULL).  We are going to pass it a closure whose
  // second field has type this->expr_->type().  Since
  // this->expr_type_ is only not-NULL for pointer types, we can get
  // away with this.

  Struct_field_list* fields = new Struct_field_list();
  fields->push_back(Struct_field(Typed_identifier("fn.0",
						  thunk->func_value()->type(),
						  loc)));
  fields->push_back(Struct_field(Typed_identifier("val.1", val->type(), loc)));
  Struct_type* st = Type::make_struct_type(fields, loc);

  Expression_list* vals = new Expression_list();
  vals->push_back(Expression::make_func_code_reference(thunk, loc));
  vals->push_back(val);

  Expression* ret = Expression::make_struct_composite_literal(st, vals, loc);
  ret = Expression::make_heap_expression(ret, loc);

  tree ret_tree = ret->get_tree(context);

  Expression* nil_check = NULL;

  // See whether the expression or any embedded pointers are nil.

  Expression* expr = this->expr_;
  if (this->method_->field_indexes() != NULL)
    {
      // Note that we are evaluating this->expr_ twice, but that is OK
      // because in the lowering pass we forced it into a temporary
      // variable.
      Expression* ref = expr;
      nil_check = bme_check_nil(this->method_->field_indexes(), loc, &ref);
      expr = ref;
    }

  if (this->method_->is_value_method() && expr->type()->points_to() != NULL)
    {
      Expression* n = Expression::make_binary(OPERATOR_EQEQ, expr,
					      Expression::make_nil(loc),
					      loc);
      if (nil_check == NULL)
	nil_check = n;
      else
	nil_check = Expression::make_binary(OPERATOR_OROR, nil_check, n, loc);
    }

  if (nil_check != NULL)
    {
      tree nil_check_tree = nil_check->get_tree(context);
      Expression* crash_expr =
	context->gogo()->runtime_error(RUNTIME_ERROR_NIL_DEREFERENCE, loc);
      tree crash = crash_expr->get_tree(context);
      if (ret_tree == error_mark_node
	  || nil_check_tree == error_mark_node
	  || crash == error_mark_node)
	return error_mark_node;

      ret_tree = fold_build2_loc(loc.gcc_location(), COMPOUND_EXPR,
				 TREE_TYPE(ret_tree),
				 build3_loc(loc.gcc_location(), COND_EXPR,
					    void_type_node, nil_check_tree,
					    crash, NULL_TREE),
				 ret_tree);
    }

  return ret_tree;
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
    
  ast_dump_context->ostream() << "." << this->function_->name();
}

// Make a method expression.

Bound_method_expression*
Expression::make_bound_method(Expression* expr, const Method* method,
			      Named_object* function, Location location)
{
  return new Bound_method_expression(expr, method, function, location);
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

  Expression*
  do_flatten(Gogo*, Named_object*, Statement_inserter*);

  bool
  do_is_constant() const;

  bool
  do_numeric_constant_value(Numeric_constant*) const;

  bool
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
  check_int_value(Expression*, bool is_length);

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

  if (this->code_ == BUILTIN_OFFSETOF)
    {
      Expression* arg = this->one_arg();

      if (arg->bound_method_expression() != NULL
	  || arg->interface_field_reference_expression() != NULL)
	{
	  this->report_error(_("invalid use of method value as argument "
			       "of Offsetof"));
	  return this;
	}

      Field_reference_expression* farg = arg->field_reference_expression();
      while (farg != NULL)
	{
	  if (!farg->implicit())
	    break;
	  // When the selector refers to an embedded field,
	  // it must not be reached through pointer indirections.
	  if (farg->expr()->deref() != farg->expr())
	    {
	      this->report_error(_("argument of Offsetof implies "
				   "indirection of an embedded field"));
	      return this;
	    }
	  // Go up until we reach the original base.
	  farg = farg->expr()->field_reference_expression();
	}
    }
 
  if (this->is_constant())
    {
      Numeric_constant nc;
      if (this->numeric_constant_value(&nc))
	return nc.expression(loc);
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
	  Type* eface = Type::make_empty_interface_type(loc);
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
	    if (slice_type->is_nil_type())
	      error_at(args->front()->location(), "use of untyped nil");
	    else
	      error_at(args->front()->location(),
		       "argument 1 must be a slice");
	    this->set_is_error();
	    return this;
	  }
	Type* element_type = slice_type->array_type()->element_type();
	this->lower_varargs(gogo, function, inserter,
			    Type::make_array_type(element_type, NULL),
			    2);
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

// Flatten a builtin call expression.  This turns the arguments of copy and
// append into temporary expressions.

Expression*
Builtin_call_expression::do_flatten(Gogo*, Named_object*,
                                    Statement_inserter* inserter)
{
  if (this->code_ == BUILTIN_APPEND
      || this->code_ == BUILTIN_COPY)
    {
      Location loc = this->location();
      Type* at = this->args()->front()->type();
      for (Expression_list::iterator pa = this->args()->begin();
           pa != this->args()->end();
           ++pa)
        {
          if ((*pa)->is_nil_expression())
            *pa = Expression::make_slice_composite_literal(at, NULL, loc);
          if (!(*pa)->is_variable())
            {
              Temporary_statement* temp =
                  Statement::make_temporary(NULL, *pa, loc);
              inserter->insert(temp);
              *pa = Expression::make_temporary_reference(temp, loc);
            }
        }
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

  bool have_big_args = false;
  Type* uintptr_type = Type::lookup_integer_type("uintptr");
  int uintptr_bits = uintptr_type->integer_type()->bits();

  Type_context int_context(Type::lookup_integer_type("int"), false);

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
      len_arg->determine_type(&int_context);
      if (!this->check_int_value(len_arg, true))
	return Expression::make_error(this->location());
      if (len_arg->type()->integer_type() != NULL
	  && len_arg->type()->integer_type()->bits() > uintptr_bits)
	have_big_args = true;
      ++parg;
    }

  Expression* cap_arg = NULL;
  if (is_slice && parg != args->end())
    {
      cap_arg = *parg;
      cap_arg->determine_type(&int_context);
      if (!this->check_int_value(cap_arg, false))
	return Expression::make_error(this->location());

      Numeric_constant nclen;
      Numeric_constant nccap;
      unsigned long vlen;
      unsigned long vcap;
      if (len_arg->numeric_constant_value(&nclen)
	  && cap_arg->numeric_constant_value(&nccap)
	  && nclen.to_unsigned_long(&vlen) == Numeric_constant::NC_UL_VALID
	  && nccap.to_unsigned_long(&vcap) == Numeric_constant::NC_UL_VALID
	  && vlen > vcap)
	{
	  this->report_error(_("len larger than cap"));
	  return Expression::make_error(this->location());
	}

      if (cap_arg->type()->integer_type() != NULL
	  && cap_arg->type()->integer_type()->bits() > uintptr_bits)
	have_big_args = true;
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
	call = Runtime::make_call((have_big_args
				   ? Runtime::MAKESLICE1BIG
				   : Runtime::MAKESLICE1),
				  loc, 2, type_arg, len_arg);
      else
	call = Runtime::make_call((have_big_args
				   ? Runtime::MAKESLICE2BIG
				   : Runtime::MAKESLICE2),
				  loc, 3, type_arg, len_arg, cap_arg);
    }
  else if (is_map)
    call = Runtime::make_call((have_big_args
			       ? Runtime::MAKEMAPBIG
			       : Runtime::MAKEMAP),
			      loc, 2, type_arg, len_arg);
  else if (is_chan)
    call = Runtime::make_call((have_big_args
			       ? Runtime::MAKECHANBIG
			       : Runtime::MAKECHAN),
			      loc, 2, type_arg, len_arg);
  else
    go_unreachable();

  return Expression::make_unsafe_cast(type, call, loc);
}

// Return whether an expression has an integer value.  Report an error
// if not.  This is used when handling calls to the predeclared make
// function.

bool
Builtin_call_expression::check_int_value(Expression* e, bool is_length)
{
  Numeric_constant nc;
  if (e->numeric_constant_value(&nc))
    {
      unsigned long v;
      switch (nc.to_unsigned_long(&v))
	{
	case Numeric_constant::NC_UL_VALID:
	  break;
	case Numeric_constant::NC_UL_NOTINT:
	  error_at(e->location(), "non-integer %s argument to make",
		   is_length ? "len" : "cap");
	  return false;
	case Numeric_constant::NC_UL_NEGATIVE:
	  error_at(e->location(), "negative %s argument to make",
		   is_length ? "len" : "cap");
	  return false;
	case Numeric_constant::NC_UL_BIG:
	  // We don't want to give a compile-time error for a 64-bit
	  // value on a 32-bit target.
	  break;
	}

      mpz_t val;
      if (!nc.to_int(&val))
	go_unreachable();
      int bits = mpz_sizeinbase(val, 2);
      mpz_clear(val);
      Type* int_type = Type::lookup_integer_type("int");
      if (bits >= int_type->integer_type()->bits())
	{
	  error_at(e->location(), "%s argument too large for make",
		   is_length ? "len" : "cap");
	  return false;
	}

      return true;
    }

  if (e->type()->integer_type() != NULL)
    return true;

  error_at(e->location(), "non-integer %s argument to make",
	   is_length ? "len" : "cap");
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
  if (args == NULL || args->size() != 1)
    return NULL;
  return args->front();
}

// A traversal class which looks for a call or receive expression.

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
  if ((*pexpr)->call_expression() != NULL
      || (*pexpr)->receive_expression() != NULL)
    {
      this->found_ = true;
      return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Return whether this is constant: len of a string constant, or len
// or cap of an array, or unsafe.Sizeof, unsafe.Offsetof,
// unsafe.Alignof.

bool
Builtin_call_expression::do_is_constant() const
{
  if (this->is_error_expression())
    return true;
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

	// The len and cap functions are only constant if there are no
	// function calls or channel operations in the arguments.
	// Otherwise we have to make the call.
	if (!arg->is_constant())
	  {
	    Find_call_expression find_call;
	    Expression::traverse(&arg, &find_call);
	    if (find_call.found())
	      return false;
	  }

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

// Return a numeric constant if possible.

bool
Builtin_call_expression::do_numeric_constant_value(Numeric_constant* nc) const
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
	      nc->set_unsigned_long(Type::lookup_integer_type("int"),
				    sval.length());
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
	  bool r = e->numeric_constant_value(nc);
	  this->seen_ = false;
	  if (r)
	    {
	      if (!nc->set_type(Type::lookup_integer_type("int"), false,
				this->location()))
		r = false;
	    }
	  return r;
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
      if (this->seen_)
        return false;

      unsigned int ret;
      if (this->code_ == BUILTIN_SIZEOF)
	{
          this->seen_ = true;
	  bool ok = arg_type->backend_type_size(this->gogo_, &ret);
          this->seen_ = false;
	  if (!ok)
	    return false;
	}
      else if (this->code_ == BUILTIN_ALIGNOF)
	{
	  bool ok;
          this->seen_ = true;
	  if (arg->field_reference_expression() == NULL)
	    ok = arg_type->backend_type_align(this->gogo_, &ret);
	  else
	    {
	      // Calling unsafe.Alignof(s.f) returns the alignment of
	      // the type of f when it is used as a field in a struct.
	      ok = arg_type->backend_type_field_align(this->gogo_, &ret);
	    }
          this->seen_ = false;
	  if (!ok)
	    return false;
	}
      else
	go_unreachable();

      nc->set_unsigned_long(Type::lookup_integer_type("uintptr"),
			    static_cast<unsigned long>(ret));
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
      if (this->seen_)
        return false;

      unsigned int total_offset = 0;
      while (true)
        {
          Expression* struct_expr = farg->expr();
          Type* st = struct_expr->type();
          if (st->struct_type() == NULL)
            return false;
          if (st->named_type() != NULL)
            st->named_type()->convert(this->gogo_);
          unsigned int offset;
          this->seen_ = true;
          bool ok = st->struct_type()->backend_field_offset(this->gogo_,
							    farg->field_index(),
							    &offset);
          this->seen_ = false;
	  if (!ok)
	    return false;
          total_offset += offset;
          if (farg->implicit() && struct_expr->field_reference_expression() != NULL)
            {
              // Go up until we reach the original base.
              farg = struct_expr->field_reference_expression();
              continue;
            }
          break;
        }
      nc->set_unsigned_long(Type::lookup_integer_type("uintptr"),
			    static_cast<unsigned long>(total_offset));
      return true;
    }
  else if (this->code_ == BUILTIN_REAL || this->code_ == BUILTIN_IMAG)
    {
      Expression* arg = this->one_arg();
      if (arg == NULL)
	return false;

      Numeric_constant argnc;
      if (!arg->numeric_constant_value(&argnc))
	return false;

      mpfr_t real;
      mpfr_t imag;
      if (!argnc.to_complex(&real, &imag))
	return false;

      Type* type = Builtin_call_expression::real_imag_type(argnc.type());
      if (this->code_ == BUILTIN_REAL)
	nc->set_float(type, real);
      else
	nc->set_float(type, imag);
      return true;
    }
  else if (this->code_ == BUILTIN_COMPLEX)
    {
      const Expression_list* args = this->args();
      if (args == NULL || args->size() != 2)
	return false;

      Numeric_constant rnc;
      if (!args->front()->numeric_constant_value(&rnc))
	return false;
      Numeric_constant inc;
      if (!args->back()->numeric_constant_value(&inc))
	return false;

      if (rnc.type() != NULL
	  && !rnc.type()->is_abstract()
	  && inc.type() != NULL
	  && !inc.type()->is_abstract()
	  && !Type::are_identical(rnc.type(), inc.type(), false, NULL))
	return false;

      mpfr_t r;
      if (!rnc.to_float(&r))
	return false;
      mpfr_t i;
      if (!inc.to_float(&i))
	{
	  mpfr_clear(r);
	  return false;
	}

      Type* arg_type = rnc.type();
      if (arg_type == NULL || arg_type->is_abstract())
	arg_type = inc.type();

      Type* type = Builtin_call_expression::complex_type(arg_type);
      nc->set_complex(type, r, i);

      mpfr_clear(r);
      mpfr_clear(i);

      return true;
    }

  return false;
}

// Give an error if we are discarding the value of an expression which
// should not normally be discarded.  We don't give an error for
// discarding the value of an ordinary function call, but we do for
// builtin functions, purely for consistency with the gc compiler.

bool
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
      return false;

    case BUILTIN_CLOSE:
    case BUILTIN_COPY:
    case BUILTIN_DELETE:
    case BUILTIN_PANIC:
    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
    case BUILTIN_RECOVER:
      return true;
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
      return Type::lookup_integer_type("int");

    case BUILTIN_ALIGNOF:
    case BUILTIN_OFFSETOF:
    case BUILTIN_SIZEOF:
      return Type::lookup_integer_type("uintptr");

    case BUILTIN_CLOSE:
    case BUILTIN_DELETE:
    case BUILTIN_PANIC:
    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      return Type::make_void_type();

    case BUILTIN_RECOVER:
      return Type::make_empty_interface_type(Linemap::predeclared_location());

    case BUILTIN_APPEND:
      {
	const Expression_list* args = this->args();
	if (args == NULL || args->empty())
	  return Type::make_error_type();
	Type *ret = args->front()->type();
	if (!ret->is_slice_type())
	  return Type::make_error_type();
	return ret;
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
      if (arg_type == NULL)
	arg_type = Type::lookup_complex_type("complex128");
      is_print = false;
      break;

    case BUILTIN_COMPLEX:
      {
	// For the complex function the type of one operand can
	// determine the type of the other, as in a binary expression.
	arg_type = Builtin_call_expression::real_imag_type(context->type);
	if (arg_type == NULL)
	  arg_type = Type::lookup_float_type("float64");
	if (args != NULL && args->size() == 2)
	  {
	    Type* t1 = args->front()->type();
	    Type* t2 = args->back()->type();
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
		      Numeric_constant nc;
		      if (this->numeric_constant_value(&nc))
			{
			  mpz_t val;
			  if (nc.to_int(&val))
			    {
			      if (mpz_sgn(val) >= 0)
				want_type = Type::lookup_integer_type("uint64");
			      mpz_clear(val);
			    }
			}
		      if (want_type == NULL)
			want_type = Type::lookup_integer_type("int64");
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
  if (this->is_error_expression())
    return;
  switch (this->code_)
    {
    case BUILTIN_INVALID:
    case BUILTIN_NEW:
    case BUILTIN_MAKE:
    case BUILTIN_DELETE:
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
		else if ((*p)->is_type_expression())
		  {
		    // If this is a type expression it's going to give
		    // an error anyhow, so we don't need one here.
		  }
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

	if (arg2_type->is_slice_type())
	  {
	    Type* e2 = arg2_type->array_type()->element_type();
	    if (!Type::are_identical(e1, e2, true, NULL))
	      this->report_error(_("element types must be the same"));
	  }
	else if (arg2_type->is_string_type())
	  {
	    if (e1->integer_type() == NULL || !e1->integer_type()->is_byte())
	      this->report_error(_("first argument must be []byte"));
	  }
	else
	    this->report_error(_("second argument must be slice or string"));
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
	if (args->front()->type()->is_error()
	    || args->back()->type()->is_error())
	  break;

	Array_type* at = args->front()->type()->array_type();
	Type* e = at->element_type();

	// The language permits appending a string to a []byte, as a
	// special case.
	if (args->back()->type()->is_string_type())
	  {
	    if (e->integer_type() != NULL && e->integer_type()->is_byte())
	      break;
	  }

	// The language says that the second argument must be
	// assignable to a slice of the element type of the first
	// argument.  We already know the first argument is a slice
	// type.
	Type* arg2_type = Type::make_array_type(e, NULL);
	std::string reason;
	if (!Type::are_assignable(arg2_type, args->back()->type(), &reason))
	  {
	    if (reason.empty())
	      this->report_error(_("argument 2 has invalid type"));
	    else
	      {
		error_at(this->location(), "argument 2 has invalid type (%s)",
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
	Expression* arg = args->front();
	Type* arg_type = arg->type();

	if (this->seen_)
	  {
	    go_assert(saw_errors());
	    return error_mark_node;
	  }
	this->seen_ = true;
	this->seen_ = false;
	if (arg_type->points_to() != NULL)
	  {
	    arg_type = arg_type->points_to();
	    go_assert(arg_type->array_type() != NULL
		       && !arg_type->is_slice_type());
            arg = Expression::make_unary(OPERATOR_MULT, arg, location);
	  }

	Type* int_type = Type::lookup_integer_type("int");
        Expression* val;
	if (this->code_ == BUILTIN_LEN)
	  {
	    if (arg_type->is_string_type())
	      val = Expression::make_string_info(arg, STRING_INFO_LENGTH,
						 location);
	    else if (arg_type->array_type() != NULL)
	      {
		if (this->seen_)
		  {
		    go_assert(saw_errors());
		    return error_mark_node;
		  }
		this->seen_ = true;
	        val = arg_type->array_type()->get_length(gogo, arg);
		this->seen_ = false;
	      }
	    else if (arg_type->map_type() != NULL)
              val = Runtime::make_call(Runtime::MAP_LEN, location, 1, arg);
	    else if (arg_type->channel_type() != NULL)
              val = Runtime::make_call(Runtime::CHAN_LEN, location, 1, arg);
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
                val = arg_type->array_type()->get_capacity(gogo, arg);
		this->seen_ = false;
	      }
	    else if (arg_type->channel_type() != NULL)
              val = Runtime::make_call(Runtime::CHAN_CAP, location, 1, arg);
	    else
	      go_unreachable();
	  }

	return Expression::make_cast(int_type, val,
				     location)->get_tree(context);
      }

    case BUILTIN_PRINT:
    case BUILTIN_PRINTLN:
      {
	const bool is_ln = this->code_ == BUILTIN_PRINTLN;
        Expression* print_stmts = NULL;

	const Expression_list* call_args = this->args();
	if (call_args != NULL)
	  {
	    for (Expression_list::const_iterator p = call_args->begin();
		 p != call_args->end();
		 ++p)
	      {
		if (is_ln && p != call_args->begin())
		  {
                    Expression* print_space =
                        Runtime::make_call(Runtime::PRINT_SPACE,
                                           this->location(), 0);

                    print_stmts =
                        Expression::make_compound(print_stmts, print_space,
                                                  location);
		  }

                Expression* arg = *p;
		Type* type = arg->type();
                Runtime::Function code;
		if (type->is_string_type())
                  code = Runtime::PRINT_STRING;
		else if (type->integer_type() != NULL
			 && type->integer_type()->is_unsigned())
		  {
		    Type* itype = Type::lookup_integer_type("uint64");
		    arg = Expression::make_cast(itype, arg, location);
                    code = Runtime::PRINT_UINT64;
		  }
		else if (type->integer_type() != NULL)
		  {
		    Type* itype = Type::lookup_integer_type("int64");
		    arg = Expression::make_cast(itype, arg, location);
                    code = Runtime::PRINT_INT64;
		  }
		else if (type->float_type() != NULL)
		  {
                    Type* dtype = Type::lookup_float_type("float64");
                    arg = Expression::make_cast(dtype, arg, location);
                    code = Runtime::PRINT_DOUBLE;
		  }
		else if (type->complex_type() != NULL)
		  {
                    Type* ctype = Type::lookup_complex_type("complex128");
                    arg = Expression::make_cast(ctype, arg, location);
                    code = Runtime::PRINT_COMPLEX;
		  }
		else if (type->is_boolean_type())
                  code = Runtime::PRINT_BOOL;
		else if (type->points_to() != NULL
			 || type->channel_type() != NULL
			 || type->map_type() != NULL
			 || type->function_type() != NULL)
		  {
                    arg = Expression::make_cast(type, arg, location);
                    code = Runtime::PRINT_POINTER;
		  }
		else if (type->interface_type() != NULL)
		  {
		    if (type->interface_type()->is_empty())
                      code = Runtime::PRINT_EMPTY_INTERFACE;
		    else
                      code = Runtime::PRINT_INTERFACE;
		  }
		else if (type->is_slice_type())
                  code = Runtime::PRINT_SLICE;
		else
		  {
		    go_assert(saw_errors());
		    return error_mark_node;
		  }

                Expression* call = Runtime::make_call(code, location, 1, arg);
                if (print_stmts == NULL)
                  print_stmts = call;
                else
                  print_stmts = Expression::make_compound(print_stmts, call,
                                                          location);
	      }
	  }

	if (is_ln)
	  {
            Expression* print_nl =
                Runtime::make_call(Runtime::PRINT_NL, location, 0);
            if (print_stmts == NULL)
              print_stmts = print_nl;
            else
              print_stmts = Expression::make_compound(print_stmts, print_nl,
                                                      location);
	  }

        return print_stmts->get_tree(context);
      }

    case BUILTIN_PANIC:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
	Type *empty =
	  Type::make_empty_interface_type(Linemap::predeclared_location());
        arg = Expression::convert_for_assignment(gogo, empty, arg, location);

        Expression* panic =
            Runtime::make_call(Runtime::PANIC, location, 1, arg);
        return panic->get_tree(context);
      }

    case BUILTIN_RECOVER:
      {
	// The argument is set when building recover thunks.  It's a
	// boolean value which is true if we can recover a value now.
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
	Type *empty =
	  Type::make_empty_interface_type(Linemap::predeclared_location());

	Expression* nil = Expression::make_nil(location);
	nil = Expression::convert_for_assignment(gogo, empty, nil, location);

	// We need to handle a deferred call to recover specially,
	// because it changes whether it can recover a panic or not.
	// See test7 in test/recover1.go.
        Expression* recover = Runtime::make_call((this->is_deferred()
                                                  ? Runtime::DEFERRED_RECOVER
                                                  : Runtime::RECOVER),
                                                 location, 0);
        Expression* cond =
            Expression::make_conditional(arg, recover, nil, location);
        return cond->get_tree(context);
      }

    case BUILTIN_CLOSE:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();
        Expression* close = Runtime::make_call(Runtime::CLOSE, location,
					       1, arg);
        return close->get_tree(context);
      }

    case BUILTIN_SIZEOF:
    case BUILTIN_OFFSETOF:
    case BUILTIN_ALIGNOF:
      {
	Numeric_constant nc;
	unsigned long val;
	if (!this->numeric_constant_value(&nc)
	    || nc.to_unsigned_long(&val) != Numeric_constant::NC_UL_VALID)
	  {
	    go_assert(saw_errors());
	    return error_mark_node;
	  }
	Type* uintptr_type = Type::lookup_integer_type("uintptr");
        mpz_t ival;
        nc.get_int(&ival);
        Expression* int_cst =
            Expression::make_integer(&ival, uintptr_type, location);
        mpz_clear(ival);
        return int_cst->get_tree(context);
      }

    case BUILTIN_COPY:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	Expression* arg1 = args->front();
	Expression* arg2 = args->back();

	Type* arg1_type = arg1->type();
	Array_type* at = arg1_type->array_type();
	go_assert(arg1->is_variable());
	Expression* arg1_val = at->get_value_pointer(gogo, arg1);
	Expression* arg1_len = at->get_length(gogo, arg1);

	Type* arg2_type = arg2->type();
        go_assert(arg2->is_variable());
	Expression* arg2_val;
	Expression* arg2_len;
	if (arg2_type->is_slice_type())
	  {
	    at = arg2_type->array_type();
	    arg2_val = at->get_value_pointer(gogo, arg2);
	    arg2_len = at->get_length(gogo, arg2);
	  }
	else
	  {
	    go_assert(arg2->is_variable());
            arg2_val = Expression::make_string_info(arg2, STRING_INFO_DATA,
                                                    location);
	    arg2_len = Expression::make_string_info(arg2, STRING_INFO_LENGTH,
                                                    location);
	  }
        Expression* cond =
            Expression::make_binary(OPERATOR_LT, arg1_len, arg2_len, location);
        Expression* length =
            Expression::make_conditional(cond, arg1_len, arg2_len, location);

	Type* element_type = at->element_type();
	Btype* element_btype = element_type->get_backend(gogo);

        mpz_t size;
        size_t element_size = gogo->backend()->type_size(element_btype);
        mpz_init_set_ui(size, element_size);
        Expression* size_expr = Expression::make_integer(&size, length->type(), location);
        mpz_clear(size);

        Expression* bytecount =
            Expression::make_binary(OPERATOR_MULT, size_expr, length, location);
        Expression* copy = Runtime::make_call(Runtime::COPY, location, 3,
                                              arg1_val, arg2_val, bytecount);

        Expression* compound = Expression::make_compound(copy, length, location);
        return compound->get_tree(context);
      }

    case BUILTIN_APPEND:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	Expression* arg1 = args->front();
	Expression* arg2 = args->back();

	Array_type* at = arg1->type()->array_type();
	Type* element_type = at->element_type()->forwarded();

        go_assert(arg2->is_variable());
	Expression* arg2_val;
	Expression* arg2_len;
        mpz_t size;
	if (arg2->type()->is_string_type()
	    && element_type->integer_type() != NULL
	    && element_type->integer_type()->is_byte())
	  {
	    arg2_val = Expression::make_string_info(arg2, STRING_INFO_DATA,
						    location);
	    arg2_len = Expression::make_string_info(arg2, STRING_INFO_LENGTH,
						    location);
            mpz_init_set_ui(size, 1UL);
	  }
	else
	  {
	    arg2_val = at->get_value_pointer(gogo, arg2);
	    arg2_len = at->get_length(gogo, arg2);
	    Btype* element_btype = element_type->get_backend(gogo);
            size_t element_size = gogo->backend()->type_size(element_btype);
            mpz_init_set_ui(size, element_size);
	  }
        Expression* element_size =
            Expression::make_integer(&size, NULL, location);
        mpz_clear(size);

        Expression* append = Runtime::make_call(Runtime::APPEND, location, 4,
                                                arg1, arg2_val, arg2_len,
                                                element_size);
        append = Expression::make_unsafe_cast(arg1->type(), append, location);
        return append->get_tree(context);
      }

    case BUILTIN_REAL:
    case BUILTIN_IMAG:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 1);
	Expression* arg = args->front();

        Bexpression* ret;
        Bexpression* bcomplex = tree_to_expr(arg->get_tree(context));
        if (this->code_ == BUILTIN_REAL)
          ret = gogo->backend()->real_part_expression(bcomplex, location);
        else
          ret = gogo->backend()->imag_part_expression(bcomplex, location);
        return expr_to_tree(ret);
      }

    case BUILTIN_COMPLEX:
      {
	const Expression_list* args = this->args();
	go_assert(args != NULL && args->size() == 2);
	Bexpression* breal = tree_to_expr(args->front()->get_tree(context));
	Bexpression* bimag = tree_to_expr(args->back()->get_tree(context));
        Bexpression* ret =
            gogo->backend()->complex_expression(breal, bimag, location);
        return expr_to_tree(ret);
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
  Numeric_constant nc;
  if (!this->numeric_constant_value(&nc))
    {
      error_at(this->location(), "value is not constant");
      return;
    }

  if (nc.is_int())
    {
      mpz_t val;
      nc.get_int(&val);
      Integer_expression::export_integer(exp, val);
      mpz_clear(val);
    }
  else if (nc.is_float())
    {
      mpfr_t fval;
      nc.get_float(&fval);
      Float_expression::export_float(exp, fval);
      mpfr_clear(fval);
    }
  else if (nc.is_complex())
    {
      mpfr_t real;
      mpfr_t imag;
      Complex_expression::export_complex(exp, real, imag);
      mpfr_clear(real);
      mpfr_clear(imag);
    }
  else
    go_unreachable();

  // A trailing space lets us reliably identify the end of the number.
  exp->write_c_string(" ");
}

// Class Call_expression.

// A Go function can be viewed in a couple of different ways.  The
// code of a Go function becomes a backend function with parameters
// whose types are simply the backend representation of the Go types.
// If there are multiple results, they are returned as a backend
// struct.

// However, when Go code refers to a function other than simply
// calling it, the backend type of that function is actually a struct.
// The first field of the struct points to the Go function code
// (sometimes a wrapper as described below).  The remaining fields
// hold addresses of closed-over variables.  This struct is called a
// closure.

// There are a few cases to consider.

// A direct function call of a known function in package scope.  In
// this case there are no closed-over variables, and we know the name
// of the function code.  We can simply produce a backend call to the
// function directly, and not worry about the closure.

// A direct function call of a known function literal.  In this case
// we know the function code and we know the closure.  We generate the
// function code such that it expects an additional final argument of
// the closure type.  We pass the closure as the last argument, after
// the other arguments.

// An indirect function call.  In this case we have a closure.  We
// load the pointer to the function code from the first field of the
// closure.  We pass the address of the closure as the last argument.

// A call to a method of an interface.  Type methods are always at
// package scope, so we call the function directly, and don't worry
// about the closure.

// This means that for a function at package scope we have two cases.
// One is the direct call, which has no closure.  The other is the
// indirect call, which does have a closure.  We can't simply ignore
// the closure, even though it is the last argument, because that will
// fail on targets where the function pops its arguments.  So when
// generating a closure for a package-scope function we set the
// function code pointer in the closure to point to a wrapper
// function.  This wrapper function accepts a final argument that
// points to the closure, ignores it, and calls the real function as a
// direct function call.  This wrapper will normally be efficient, and
// can often simply be a tail call to the real function.

// We don't use GCC's static chain pointer because 1) we don't need
// it; 2) GCC only permits using a static chain to call a known
// function, so we can't use it for an indirect call anyhow.  Since we
// can't use it for an indirect call, we may as well not worry about
// using it for a direct call either.

// We pass the closure last rather than first because it means that
// the function wrapper we put into a closure for a package-scope
// function can normally just be a tail call to the real function.

// For method expressions we generate a wrapper that loads the
// receiver from the closure and then calls the method.  This
// unfortunately forces reshuffling the arguments, since there is a
// new first argument, but we can't avoid reshuffling either for
// method expressions or for indirect calls of package-scope
// functions, and since the latter are more common we reshuffle for
// method expressions.

// Note that the Go code retains the Go types.  The extra final
// argument only appears when we convert to the backend
// representation.

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

  // Because do_type will return an error type and thus prevent future
  // errors, check for that case now to ensure that the error gets
  // reported.
  Function_type* fntype = this->get_function_type();
  if (fntype == NULL)
    {
      if (!this->fn_->type()->is_error())
	this->report_error(_("expected function"));
      return Expression::make_error(loc);
    }

  // Handle an argument which is a call to a function which returns
  // multiple results.
  if (this->args_ != NULL
      && this->args_->size() == 1
      && this->args_->front()->call_expression() != NULL)
    {
      size_t rc = this->args_->front()->call_expression()->result_count();
      if (rc > 1
	  && ((fntype->parameters() != NULL
               && (fntype->parameters()->size() == rc
                   || (fntype->is_varargs()
                       && fntype->parameters()->size() - 1 <= rc)))
              || fntype->is_builtin()))
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

  // Recognize a call to a builtin function.
  if (fntype->is_builtin())
    return new Builtin_call_expression(gogo, this->fn_, this->args_,
				       this->is_varargs_, loc);

  // If this call returns multiple results, create a temporary
  // variable for each result.
  size_t rc = this->result_count();
  if (rc > 1 && this->results_ == NULL)
    {
      std::vector<Temporary_statement*>* temps =
	new std::vector<Temporary_statement*>;
      temps->reserve(rc);
      const Typed_identifier_list* results = fntype->results();
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
  if (fntype->is_varargs())
    {
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
      Named_object* methodfn = bme->function();
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
      this->fn_ = Expression::make_func_reference(methodfn, NULL,
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
	  if ((*pa)->type()->is_slice_type())
	    this->report_error(_("too many arguments"));
	  else
	    {
	      error_at(this->location(),
		       _("invalid use of %<...%> with non-slice"));
	      this->set_is_error();
	    }
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

// Flatten a call with multiple results into a temporary.

Expression*
Call_expression::do_flatten(Gogo*, Named_object*, Statement_inserter* inserter)
{
  size_t rc = this->result_count();
  if (rc > 1 && this->call_temp_ == NULL)
    {
      Struct_field_list* sfl = new Struct_field_list();
      Function_type* fntype = this->get_function_type();
      const Typed_identifier_list* results = fntype->results();
      Location loc = this->location();

      int i = 0;
      char buf[10];
      for (Typed_identifier_list::const_iterator p = results->begin();
           p != results->end();
           ++p, ++i)
        {
          snprintf(buf, sizeof buf, "res%d", i);
          sfl->push_back(Struct_field(Typed_identifier(buf, p->type(), loc)));
        }

      Struct_type* st = Type::make_struct_type(sfl, loc);
      this->call_temp_ = Statement::make_temporary(st, NULL, loc);
      inserter->insert(this->call_temp_);
    }

  return this;
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
  if (this->results_ == NULL || this->results_->size() <= i)
    {
      go_assert(saw_errors());
      return NULL;
    }
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
  if (this->classification() == EXPRESSION_ERROR)
    return;

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
  // we don't have to worry about it here unless something is wrong.
  if (this->is_varargs_ && !this->varargs_are_lowered_)
    {
      if (!fntype->is_varargs())
	{
	  error_at(this->location(),
		   _("invalid use of %<...%> calling non-variadic function"));
	  this->set_is_error();
	  return;
	}
    }

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

Expression*
Call_expression::interface_method_function(
    Interface_field_reference_expression* interface_method,
    Expression** first_arg_ptr)
{
  *first_arg_ptr = interface_method->get_underlying_object();
  return interface_method->get_function();
}

// Build the call expression.

tree
Call_expression::do_get_tree(Translate_context* context)
{
  if (this->call_ != NULL)
    return expr_to_tree(this->call_);

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

  bool has_closure_arg;
  if (has_closure)
    has_closure_arg = true;
  else if (func != NULL)
    has_closure_arg = false;
  else if (is_interface_method)
    has_closure_arg = false;
  else
    has_closure_arg = true;

  int nargs;
  std::vector<Bexpression*> fn_args;
  if (this->args_ == NULL || this->args_->empty())
    {
      nargs = is_interface_method ? 1 : 0;
      if (nargs > 0)
        fn_args.resize(1);
    }
  else if (fntype->parameters() == NULL || fntype->parameters()->empty())
    {
      // Passing a receiver parameter.
      go_assert(!is_interface_method
		&& fntype->is_method()
		&& this->args_->size() == 1);
      nargs = 1;
      fn_args.resize(1);
      fn_args[0] = tree_to_expr(this->args_->front()->get_tree(context));
    }
  else
    {
      const Typed_identifier_list* params = fntype->parameters();

      nargs = this->args_->size();
      int i = is_interface_method ? 1 : 0;
      nargs += i;
      fn_args.resize(nargs);

      Typed_identifier_list::const_iterator pp = params->begin();
      Expression_list::const_iterator pe = this->args_->begin();
      if (!is_interface_method && fntype->is_method())
	{
          fn_args[i] = tree_to_expr((*pe)->get_tree(context));
	  ++pe;
	  ++i;
	}
      for (; pe != this->args_->end(); ++pe, ++pp, ++i)
	{
	  go_assert(pp != params->end());
          Expression* arg =
              Expression::convert_for_assignment(gogo, pp->type(), *pe,
                                                 location);
          fn_args[i] = tree_to_expr(arg->get_tree(context));
	}
      go_assert(pp == params->end());
      go_assert(i == nargs);
    }

  Expression* fn;
  Expression* closure = NULL;
  if (func != NULL)
    {
      Named_object* no = func->named_object();
      fn = Expression::make_func_code_reference(no, location);
      if (has_closure)
        closure = func->closure();
    }
  else if (!is_interface_method)
    {
      closure = this->fn_;

      // The backend representation of this function type is a pointer
      // to a struct whose first field is the actual function to call.
      Type* pfntype =
          Type::make_pointer_type(
              Type::make_pointer_type(Type::make_void_type()));
      fn = Expression::make_unsafe_cast(pfntype, this->fn_, location);
      fn = Expression::make_unary(OPERATOR_MULT, fn, location);
    }
  else
    {
      Expression* first_arg;
      fn = this->interface_method_function(interface_method, &first_arg);
      fn_args[0] = tree_to_expr(first_arg->get_tree(context));
    }

  if (!has_closure_arg)
    go_assert(closure == NULL);
  else
    {
      // Pass the closure argument by calling the function function
      // __go_set_closure.  In the order_evaluations pass we have
      // ensured that if any parameters contain call expressions, they
      // will have been moved out to temporary variables.
      go_assert(closure != NULL);
      Expression* set_closure =
          Runtime::make_call(Runtime::SET_CLOSURE, location, 1, closure);
      fn = Expression::make_compound(set_closure, fn, location);
    }

  Bexpression* bfn = tree_to_expr(fn->get_tree(context));

  // When not calling a named function directly, use a type conversion
  // in case the type of the function is a recursive type which refers
  // to itself.  We don't do this for an interface method because 1)
  // an interface method never refers to itself, so we always have a
  // function type here; 2) we pass an extra first argument to an
  // interface method, so fntype is not correct.
  if (func == NULL && !is_interface_method)
    {
      Btype* bft = fntype->get_backend_fntype(gogo);
      bfn = gogo->backend()->convert_expression(bft, bfn, location);
    }

  Bexpression* call = gogo->backend()->call_expression(bfn, fn_args, location);

  if (this->results_ != NULL)
    {
      go_assert(this->call_temp_ != NULL);
      Expression* call_ref =
          Expression::make_temporary_reference(this->call_temp_, location);
      Bexpression* bcall_ref = tree_to_expr(call_ref->get_tree(context));
      Bstatement* assn_stmt =
          gogo->backend()->assignment_statement(bcall_ref, call, location);

      this->call_ = this->set_results(context, bcall_ref);

      Bexpression* set_and_call =
          gogo->backend()->compound_expression(assn_stmt, this->call_,
                                               location);
      return expr_to_tree(set_and_call);
    }

  this->call_ = call;
  return expr_to_tree(this->call_);
}

// Set the result variables if this call returns multiple results.

Bexpression*
Call_expression::set_results(Translate_context* context, Bexpression* call)
{
  Gogo* gogo = context->gogo();

  Bexpression* results = NULL;
  Location loc = this->location();

  size_t rc = this->result_count();
  for (size_t i = 0; i < rc; ++i)
    {
      Temporary_statement* temp = this->result(i);
      if (temp == NULL)
	{
	  go_assert(saw_errors());
	  return gogo->backend()->error_expression();
	}
      Temporary_reference_expression* ref =
	Expression::make_temporary_reference(temp, loc);
      ref->set_is_lvalue();

      Bexpression* result_ref = tree_to_expr(ref->get_tree(context));
      Bexpression* call_result =
          gogo->backend()->struct_field_expression(call, i, loc);
      Bstatement* assn_stmt =
           gogo->backend()->assignment_statement(result_ref, call_result, loc);

      Bexpression* result =
          gogo->backend()->compound_expression(assn_stmt, call_result, loc);

      if (results == NULL)
        results = result;
      else
        {
          Bstatement* expr_stmt = gogo->backend()->expression_statement(result);
          results =
              gogo->backend()->compound_expression(expr_stmt, results, loc);
        }
    }
  return results;
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
  if (ce == NULL)
    {
      go_assert(this->call_->is_error_expression());
      return error_mark_node;
    }
  Temporary_statement* ts = ce->result(this->index_);
  if (ts == NULL)
    {
      go_assert(saw_errors());
      return error_mark_node;
    }
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
	  && Expression::traverse(&this->end_, traverse) == TRAVERSE_EXIT)
      || (this->cap_ != NULL
          && Expression::traverse(&this->cap_, traverse) == TRAVERSE_EXIT))
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
  Expression* cap = this->cap_;

  Type* type = left->type();
  if (type->is_error())
    return Expression::make_error(location);
  else if (left->is_type_expression())
    {
      error_at(location, "attempt to index type expression");
      return Expression::make_error(location);
    }
  else if (type->array_type() != NULL)
    return Expression::make_array_index(left, start, end, cap, location);
  else if (type->points_to() != NULL
	   && type->points_to()->array_type() != NULL
	   && !type->points_to()->is_slice_type())
    {
      Expression* deref = Expression::make_unary(OPERATOR_MULT, left,
						 location);

      // For an ordinary index into the array, the pointer will be
      // dereferenced.  For a slice it will not--the resulting slice
      // will simply reuse the pointer, which is incorrect if that
      // pointer is nil.
      if (end != NULL || cap != NULL)
	deref->issue_nil_check();

      return Expression::make_array_index(deref, start, end, cap, location);
    }
  else if (type->is_string_type())
    {
      if (cap != NULL)
        {
          error_at(location, "invalid 3-index slice of string");
          return Expression::make_error(location);
        }
      return Expression::make_string_index(left, start, end, location);
    }
  else if (type->map_type() != NULL)
    {
      if (end != NULL || cap != NULL)
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

// Write an indexed expression
// (expr[expr:expr:expr], expr[expr:expr] or expr[expr]) to a dump context.

void
Index_expression::dump_index_expression(Ast_dump_context* ast_dump_context, 
					const Expression* expr, 
					const Expression* start,
					const Expression* end,
					const Expression* cap)
{
  expr->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << "[";
  start->dump_expression(ast_dump_context);
  if (end != NULL)
    {
      ast_dump_context->ostream() << ":";
      end->dump_expression(ast_dump_context);
    }
  if (cap != NULL)
    {
      ast_dump_context->ostream() << ":";
      cap->dump_expression(ast_dump_context);
    }
  ast_dump_context->ostream() << "]";
}

// Dump ast representation for an index expression.

void
Index_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->left_, 
                                          this->start_, this->end_, this->cap_);
}

// Make an index expression.

Expression*
Expression::make_index(Expression* left, Expression* start, Expression* end,
		       Expression* cap, Location location)
{
  return new Index_expression(left, start, end, cap, location);
}

// An array index.  This is used for both indexing and slicing.

class Array_index_expression : public Expression
{
 public:
  Array_index_expression(Expression* array, Expression* start,
			 Expression* end, Expression* cap, Location location)
    : Expression(EXPRESSION_ARRAY_INDEX, location),
      array_(array), start_(start), end_(end), cap_(cap), type_(NULL)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Expression*
  do_flatten(Gogo*, Named_object*, Statement_inserter*);

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
					(this->cap_ == NULL
					 ? NULL
					 : this->cap_->copy()),
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

  void
  do_issue_nil_check()
  { this->array_->issue_nil_check(); }

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
  // The capacity argument of a slice.  This may be NULL for an array index or
  // slice.
  Expression* cap_;
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
  if (this->cap_ != NULL)
    {
      if (Expression::traverse(&this->cap_, traverse) == TRAVERSE_EXIT)
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
  if (this->cap_ != NULL)
    this->cap_->determine_type_no_context();
}

// Check types of an array index.

void
Array_index_expression::do_check_types(Gogo*)
{
  Numeric_constant nc;
  unsigned long v;
  if (this->start_->type()->integer_type() == NULL
      && !this->start_->type()->is_error()
      && (!this->start_->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    this->report_error(_("index must be integer"));
  if (this->end_ != NULL
      && this->end_->type()->integer_type() == NULL
      && !this->end_->type()->is_error()
      && !this->end_->is_nil_expression()
      && !this->end_->is_error_expression()
      && (!this->end_->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    this->report_error(_("slice end must be integer"));
  if (this->cap_ != NULL
      && this->cap_->type()->integer_type() == NULL
      && !this->cap_->type()->is_error()
      && !this->cap_->is_nil_expression()
      && !this->cap_->is_error_expression()
      && (!this->cap_->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    this->report_error(_("slice capacity must be integer"));

  Array_type* array_type = this->array_->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->array_->type()->is_error());
      return;
    }

  unsigned int int_bits =
    Type::lookup_integer_type("int")->integer_type()->bits();

  Numeric_constant lvalnc;
  mpz_t lval;
  bool lval_valid = (array_type->length() != NULL
		     && array_type->length()->numeric_constant_value(&lvalnc)
		     && lvalnc.to_int(&lval));
  Numeric_constant inc;
  mpz_t ival;
  bool ival_valid = false;
  if (this->start_->numeric_constant_value(&inc) && inc.to_int(&ival))
    {
      ival_valid = true;
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
      Numeric_constant enc;
      mpz_t eval;
      bool eval_valid = false;
      if (this->end_->numeric_constant_value(&enc) && enc.to_int(&eval))
	{
	  eval_valid = true;
	  if (mpz_sgn(eval) < 0
	      || mpz_sizeinbase(eval, 2) >= int_bits
	      || (lval_valid && mpz_cmp(eval, lval) > 0))
	    {
	      error_at(this->end_->location(), "array index out of bounds");
	      this->set_is_error();
	    }
	  else if (ival_valid && mpz_cmp(ival, eval) > 0)
	    this->report_error(_("inverted slice range"));
	}

      Numeric_constant cnc;
      mpz_t cval;
      if (this->cap_ != NULL
          && this->cap_->numeric_constant_value(&cnc) && cnc.to_int(&cval))
        {
          if (mpz_sgn(cval) < 0
              || mpz_sizeinbase(cval, 2) >= int_bits
              || (lval_valid && mpz_cmp(cval, lval) > 0))
            {
              error_at(this->cap_->location(), "array index out of bounds");
              this->set_is_error();
            }
	  else if (ival_valid && mpz_cmp(ival, cval) > 0)
	    {
	      error_at(this->cap_->location(),
		       "invalid slice index: capacity less than start");
	      this->set_is_error();
	    }
          else if (eval_valid && mpz_cmp(eval, cval) > 0)
            {
              error_at(this->cap_->location(),
                       "invalid slice index: capacity less than length");
              this->set_is_error();
            }
          mpz_clear(cval);
        }

      if (eval_valid)
        mpz_clear(eval);
    }
  if (ival_valid)
    mpz_clear(ival);
  if (lval_valid)
    mpz_clear(lval);

  // A slice of an array requires an addressable array.  A slice of a
  // slice is always possible.
  if (this->end_ != NULL && !array_type->is_slice_type())
    {
      if (!this->array_->is_addressable())
	this->report_error(_("slice of unaddressable value"));
      else
	this->array_->address_taken(true);
    }
}

// Flatten array indexing by using temporary variables for slices and indexes.

Expression*
Array_index_expression::do_flatten(Gogo*, Named_object*,
                                   Statement_inserter* inserter)
{
  Location loc = this->location();
  Temporary_statement* temp;
  if (this->array_->type()->is_slice_type() && !this->array_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->array_, loc);
      inserter->insert(temp);
      this->array_ = Expression::make_temporary_reference(temp, loc);
    }
  if (!this->start_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->start_, loc);
      inserter->insert(temp);
      this->start_ = Expression::make_temporary_reference(temp, loc);
    }
  if (this->end_ != NULL
      && !this->end_->is_nil_expression()
      && !this->end_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->end_, loc);
      inserter->insert(temp);
      this->end_ = Expression::make_temporary_reference(temp, loc);
    }
  if (this->cap_ != NULL && !this->cap_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->cap_, loc);
      inserter->insert(temp);
      this->cap_ = Expression::make_temporary_reference(temp, loc);
    }

  return this;
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
  Array_type* array_type = this->array_->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->array_->type()->is_error());
      return error_mark_node;
    }
  go_assert(!array_type->is_slice_type() || this->array_->is_variable());

  Location loc = this->location();
  Gogo* gogo = context->gogo();

  Btype* int_btype = Type::lookup_integer_type("int")->get_backend(gogo);

  // We need to convert the length and capacity to the Go "int" type here
  // because the length of a fixed-length array could be of type "uintptr"
  // and gimple disallows binary operations between "uintptr" and other
  // integer types. FIXME.
  Bexpression* length = NULL;
  if (this->end_ == NULL || this->end_->is_nil_expression())
    {
      Expression* len = array_type->get_length(gogo, this->array_);
      length = tree_to_expr(len->get_tree(context));
      length = gogo->backend()->convert_expression(int_btype, length, loc);
    }

  Bexpression* capacity = NULL;
  if (this->end_ != NULL)
    {
      Expression* cap = array_type->get_capacity(gogo, this->array_);
      capacity = tree_to_expr(cap->get_tree(context));
      capacity = gogo->backend()->convert_expression(int_btype, capacity, loc);
    }

  Bexpression* cap_arg = capacity;
  if (this->cap_ != NULL)
    {
      cap_arg = tree_to_expr(this->cap_->get_tree(context));
      cap_arg = gogo->backend()->convert_expression(int_btype, cap_arg, loc);
    }

  if (length == NULL)
    length = cap_arg;

  int code = (array_type->length() != NULL
	      ? (this->end_ == NULL
		 ? RUNTIME_ERROR_ARRAY_INDEX_OUT_OF_BOUNDS
		 : RUNTIME_ERROR_ARRAY_SLICE_OUT_OF_BOUNDS)
	      : (this->end_ == NULL
		 ? RUNTIME_ERROR_SLICE_INDEX_OUT_OF_BOUNDS
		 : RUNTIME_ERROR_SLICE_SLICE_OUT_OF_BOUNDS));
  Bexpression* crash =
      tree_to_expr(gogo->runtime_error(code, loc)->get_tree(context));

  Expression* bounds_check = Expression::check_bounds(this->start_, loc);
  Bexpression* bad_index = tree_to_expr(bounds_check->get_tree(context));

  Bexpression* start = tree_to_expr(this->start_->get_tree(context));
  start = gogo->backend()->convert_expression(int_btype, start, loc);
  Bexpression* start_too_large =
    gogo->backend()->binary_expression((this->end_ == NULL
					? OPERATOR_GE
					: OPERATOR_GT),
                                       start,
				       (this->end_ == NULL
					? length
					: capacity),
                                       loc);
  bad_index = gogo->backend()->binary_expression(OPERATOR_OROR, start_too_large,
						 bad_index, loc);

  if (this->end_ == NULL)
    {
      // Simple array indexing.  This has to return an l-value, so
      // wrap the index check into START.
      start =
	gogo->backend()->conditional_expression(int_btype, bad_index,
						crash, start, loc);

      Bexpression* ret;
      if (array_type->length() != NULL)
	{
	  Bexpression* array = tree_to_expr(this->array_->get_tree(context));
	  ret = gogo->backend()->array_index_expression(array, start, loc);
	}
      else
	{
	  // Slice.
	  Expression* valptr =
              array_type->get_value_pointer(gogo, this->array_);
	  Bexpression* ptr = tree_to_expr(valptr->get_tree(context));
          ptr = gogo->backend()->pointer_offset_expression(ptr, start, loc);
	  ret = gogo->backend()->indirect_expression(ptr, true, loc);
	}
      return expr_to_tree(ret);
    }

  // Array slice.

  if (this->cap_ != NULL)
    {
      bounds_check = Expression::check_bounds(this->cap_, loc);
      Bexpression* bounds_bcheck =
	tree_to_expr(bounds_check->get_tree(context));
      bad_index =
	gogo->backend()->binary_expression(OPERATOR_OROR, bounds_bcheck,
					   bad_index, loc);
      cap_arg = gogo->backend()->convert_expression(int_btype, cap_arg, loc);

      Bexpression* cap_too_small =
	gogo->backend()->binary_expression(OPERATOR_LT, cap_arg, start, loc);
      Bexpression* cap_too_large =
	gogo->backend()->binary_expression(OPERATOR_GT, cap_arg, capacity, loc);
      Bexpression* bad_cap =
	gogo->backend()->binary_expression(OPERATOR_OROR, cap_too_small,
					   cap_too_large, loc);
      bad_index = gogo->backend()->binary_expression(OPERATOR_OROR, bad_cap,
						     bad_index, loc);
    }

  Bexpression* end;
  if (this->end_->is_nil_expression())
    end = length;
  else
    {
      bounds_check = Expression::check_bounds(this->end_, loc);
      Bexpression* bounds_bcheck =
	tree_to_expr(bounds_check->get_tree(context));

      bad_index =
	gogo->backend()->binary_expression(OPERATOR_OROR, bounds_bcheck,
					   bad_index, loc);

      end = tree_to_expr(this->end_->get_tree(context));
      end = gogo->backend()->convert_expression(int_btype, end, loc);
      Bexpression* end_too_small =
	gogo->backend()->binary_expression(OPERATOR_LT, end, start, loc);
      Bexpression* end_too_large =
	gogo->backend()->binary_expression(OPERATOR_GT, end, cap_arg, loc);
      Bexpression* bad_end =
	gogo->backend()->binary_expression(OPERATOR_OROR, end_too_small,
					   end_too_large, loc);
      bad_index = gogo->backend()->binary_expression(OPERATOR_OROR, bad_end,
						     bad_index, loc);
    }

  Expression* valptr = array_type->get_value_pointer(gogo, this->array_);
  Bexpression* val = tree_to_expr(valptr->get_tree(context));
  val = gogo->backend()->pointer_offset_expression(val, start, loc);

  Bexpression* result_length =
    gogo->backend()->binary_expression(OPERATOR_MINUS, end, start, loc);

  Bexpression* result_capacity =
    gogo->backend()->binary_expression(OPERATOR_MINUS, cap_arg, start, loc);

  Btype* struct_btype = this->type()->get_backend(gogo);
  std::vector<Bexpression*> init;
  init.push_back(val);
  init.push_back(result_length);
  init.push_back(result_capacity);

  Bexpression* ctor =
    gogo->backend()->constructor_expression(struct_btype, init, loc);
  Bexpression* ret =
    gogo->backend()->conditional_expression(struct_btype, bad_index,
					    crash, ctor, loc);

  return expr_to_tree(ret);
}

// Dump ast representation for an array index expression.

void
Array_index_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->array_, 
                                          this->start_, this->end_, this->cap_);
}

// Make an array index expression.  END and CAP may be NULL.

Expression*
Expression::make_array_index(Expression* array, Expression* start,
                             Expression* end, Expression* cap,
                             Location location)
{
  return new Array_index_expression(array, start, end, cap, location);
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

  Expression*
  do_flatten(Gogo*, Named_object*, Statement_inserter*);

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

Expression*
String_index_expression::do_flatten(Gogo*, Named_object*,
                                    Statement_inserter* inserter)
{
  Temporary_statement* temp;
  Location loc = this->location();
  if (!this->string_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->string_, loc);
      inserter->insert(temp);
      this->string_ = Expression::make_temporary_reference(temp, loc);
    }
  if (!this->start_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->start_, loc);
      inserter->insert(temp);
      this->start_ = Expression::make_temporary_reference(temp, loc);
    }
  if (this->end_ != NULL
      && !this->end_->is_nil_expression()
      && !this->end_->is_variable())
    {
      temp = Statement::make_temporary(NULL, this->end_, loc);
      inserter->insert(temp);
      this->end_ = Expression::make_temporary_reference(temp, loc);
    }

  return this;
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
  Numeric_constant nc;
  unsigned long v;
  if (this->start_->type()->integer_type() == NULL
      && !this->start_->type()->is_error()
      && (!this->start_->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    this->report_error(_("index must be integer"));
  if (this->end_ != NULL
      && this->end_->type()->integer_type() == NULL
      && !this->end_->type()->is_error()
      && !this->end_->is_nil_expression()
      && !this->end_->is_error_expression()
      && (!this->end_->numeric_constant_value(&nc)
	  || nc.to_unsigned_long(&v) == Numeric_constant::NC_UL_NOTINT))
    this->report_error(_("slice end must be integer"));

  std::string sval;
  bool sval_valid = this->string_->string_constant_value(&sval);

  Numeric_constant inc;
  mpz_t ival;
  bool ival_valid = false;
  if (this->start_->numeric_constant_value(&inc) && inc.to_int(&ival))
    {
      ival_valid = true;
      if (mpz_sgn(ival) < 0
	  || (sval_valid && mpz_cmp_ui(ival, sval.length()) >= 0))
	{
	  error_at(this->start_->location(), "string index out of bounds");
	  this->set_is_error();
	}
    }
  if (this->end_ != NULL && !this->end_->is_nil_expression())
    {
      Numeric_constant enc;
      mpz_t eval;
      if (this->end_->numeric_constant_value(&enc) && enc.to_int(&eval))
	{
	  if (mpz_sgn(eval) < 0
	      || (sval_valid && mpz_cmp_ui(eval, sval.length()) > 0))
	    {
	      error_at(this->end_->location(), "string index out of bounds");
	      this->set_is_error();
	    }
	  else if (ival_valid && mpz_cmp(ival, eval) > 0)
	    this->report_error(_("inverted slice range"));
	  mpz_clear(eval);
	}
    }
  if (ival_valid)
    mpz_clear(ival);
}

// Get a tree for a string index.

tree
String_index_expression::do_get_tree(Translate_context* context)
{
  Location loc = this->location();
  Expression* string_arg = this->string_;
  if (this->string_->type()->points_to() != NULL)
    string_arg = Expression::make_unary(OPERATOR_MULT, this->string_, loc);

  Expression* bad_index = Expression::check_bounds(this->start_, loc);

  int code = (this->end_ == NULL
	      ? RUNTIME_ERROR_STRING_INDEX_OUT_OF_BOUNDS
	      : RUNTIME_ERROR_STRING_SLICE_OUT_OF_BOUNDS);

  Gogo* gogo = context->gogo();
  Bexpression* crash =
      tree_to_expr(gogo->runtime_error(code, loc)->get_tree(context));

  Type* int_type = Type::lookup_integer_type("int");

  // It is possible that an error occurred earlier because the start index
  // cannot be represented as an integer type.  In this case, we shouldn't
  // try casting the starting index into an integer since
  // Type_conversion_expression will fail to get the backend representation.
  // FIXME.
  if (this->start_->type()->integer_type() == NULL
      && !Type::are_convertible(int_type, this->start_->type(), NULL))
    {
      go_assert(saw_errors());
      return error_mark_node;
    }

  Expression* start = Expression::make_cast(int_type, this->start_, loc);

  if (this->end_ == NULL)
    {
      Expression* length =
          Expression::make_string_info(this->string_, STRING_INFO_LENGTH, loc);

      Expression* start_too_large =
          Expression::make_binary(OPERATOR_GE, start, length, loc);
      bad_index = Expression::make_binary(OPERATOR_OROR, start_too_large,
                                          bad_index, loc);
      Expression* bytes =
	Expression::make_string_info(this->string_, STRING_INFO_DATA, loc);

      Bexpression* bstart = tree_to_expr(start->get_tree(context));
      Bexpression* ptr = tree_to_expr(bytes->get_tree(context));
      ptr = gogo->backend()->pointer_offset_expression(ptr, bstart, loc);
      Bexpression* index = gogo->backend()->indirect_expression(ptr, true, loc);

      Btype* byte_btype = bytes->type()->points_to()->get_backend(gogo);
      Bexpression* index_error = tree_to_expr(bad_index->get_tree(context));
      Bexpression* ret =
          gogo->backend()->conditional_expression(byte_btype, index_error,
                                                  crash, index, loc);
      return expr_to_tree(ret);
    }

  Expression* end = NULL;
  if (this->end_->is_nil_expression())
    {
      mpz_t neg_one;
      mpz_init_set_si(neg_one, -1);
      end = Expression::make_integer(&neg_one, int_type, loc);
      mpz_clear(neg_one);
    }
  else
    {
      Expression* bounds_check = Expression::check_bounds(this->end_, loc);
      bad_index =
          Expression::make_binary(OPERATOR_OROR, bounds_check, bad_index, loc);
      end = Expression::make_cast(int_type, this->end_, loc);
    }

  Expression* strslice = Runtime::make_call(Runtime::STRING_SLICE, loc, 3,
                                            string_arg, start, end);
  Bexpression* bstrslice = tree_to_expr(strslice->get_tree(context));

  Btype* str_btype = strslice->type()->get_backend(gogo);
  Bexpression* index_error = tree_to_expr(bad_index->get_tree(context));
  Bexpression* ret =
      gogo->backend()->conditional_expression(str_btype, index_error,
                                              crash, bstrslice, loc);
  return expr_to_tree(ret);
}

// Dump ast representation for a string index expression.

void
String_index_expression::do_dump_expression(Ast_dump_context* ast_dump_context)
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->string_,
                                          this->start_, this->end_, NULL);
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

// We need to pass in a pointer to the key, so flatten the index into a
// temporary variable if it isn't already.  The value pointer will be
// dereferenced and checked for nil, so flatten into a temporary to avoid
// recomputation.

Expression*
Map_index_expression::do_flatten(Gogo*, Named_object*,
				 Statement_inserter* inserter)
{
  Map_type* mt = this->get_map_type();
  if (this->index_->type() != mt->key_type())
    this->index_ = Expression::make_cast(mt->key_type(), this->index_,
                                         this->location());

  if (!this->index_->is_variable())
    {
      Temporary_statement* temp = Statement::make_temporary(NULL, this->index_,
                                                            this->location());
      inserter->insert(temp);
      this->index_ = Expression::make_temporary_reference(temp,
                                                          this->location());
    }

  if (this->value_pointer_ == NULL)
    this->get_value_pointer(this->is_lvalue_);
  if (!this->value_pointer_->is_variable())
    {
      Temporary_statement* temp =
          Statement::make_temporary(NULL, this->value_pointer_,
                                    this->location());
      inserter->insert(temp);
      this->value_pointer_ =
          Expression::make_temporary_reference(temp, this->location());
    }

  return this;
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
    {
      go_assert(saw_errors());
      return error_mark_node;
    }

  go_assert(this->value_pointer_ != NULL
            && this->value_pointer_->is_variable());

  Bexpression* ret;
  if (this->is_lvalue_)
    {
      Expression* val =
          Expression::make_unary(OPERATOR_MULT, this->value_pointer_,
                                 this->location());
      ret = tree_to_expr(val->get_tree(context));
    }
  else if (this->is_in_tuple_assignment_)
    {
      // Tuple_map_assignment_statement is responsible for using this
      // appropriately.
      ret = tree_to_expr(this->value_pointer_->get_tree(context));
    }
  else
    {
      Location loc = this->location();

      Expression* nil_check =
          Expression::make_binary(OPERATOR_EQEQ, this->value_pointer_,
                                  Expression::make_nil(loc), loc);
      Bexpression* bnil_check = tree_to_expr(nil_check->get_tree(context));
      Expression* val =
          Expression::make_unary(OPERATOR_MULT, this->value_pointer_, loc);
      Bexpression* bval = tree_to_expr(val->get_tree(context));

      Gogo* gogo = context->gogo();
      Btype* val_btype = type->val_type()->get_backend(gogo);
      Bexpression* val_zero = gogo->backend()->zero_expression(val_btype);
      ret = gogo->backend()->conditional_expression(val_btype, bnil_check,
                                                    val_zero, bval, loc);
    }

  return expr_to_tree(ret);
}

// Get an expression for the map index.  This returns an expression which
// evaluates to a pointer to a value.  The pointer will be NULL if the key is
// not in the map.

Expression*
Map_index_expression::get_value_pointer(bool insert)
{
  if (this->value_pointer_ == NULL)
    {
      Map_type* type = this->get_map_type();
      if (type == NULL)
	{
	  go_assert(saw_errors());
	  return Expression::make_error(this->location());
	}

      Location loc = this->location();
      Expression* map_ref = this->map_;
      if (this->map_->type()->points_to() != NULL)
        map_ref = Expression::make_unary(OPERATOR_MULT, map_ref, loc);

      Expression* index_ptr = Expression::make_unary(OPERATOR_AND, this->index_,
                                                     loc);
      Expression* map_index =
          Runtime::make_call(Runtime::MAP_INDEX, loc, 3,
                             map_ref, index_ptr,
                             Expression::make_boolean(insert, loc));

      Type* val_type = type->val_type();
      this->value_pointer_ =
          Expression::make_unsafe_cast(Type::make_pointer_type(val_type),
                                       map_index, this->location());
    }
  return this->value_pointer_;
}

// Dump ast representation for a map index expression

void
Map_index_expression::do_dump_expression(Ast_dump_context* ast_dump_context) 
    const
{
  Index_expression::dump_index_expression(ast_dump_context, this->map_,
                                          this->index_, NULL, NULL);
}

// Make a map index expression.

Map_index_expression*
Expression::make_map_index(Expression* map, Expression* index,
			   Location location)
{
  return new Map_index_expression(map, index, location);
}

// Class Field_reference_expression.

// Lower a field reference expression.  There is nothing to lower, but
// this is where we generate the tracking information for fields with
// the magic go:"track" tag.

Expression*
Field_reference_expression::do_lower(Gogo* gogo, Named_object* function,
				     Statement_inserter* inserter, int)
{
  Struct_type* struct_type = this->expr_->type()->struct_type();
  if (struct_type == NULL)
    {
      // Error will be reported elsewhere.
      return this;
    }
  const Struct_field* field = struct_type->field(this->field_index_);
  if (field == NULL)
    return this;
  if (!field->has_tag())
    return this;
  if (field->tag().find("go:\"track\"") == std::string::npos)
    return this;

  // We have found a reference to a tracked field.  Build a call to
  // the runtime function __go_fieldtrack with a string that describes
  // the field.  FIXME: We should only call this once per referenced
  // field per function, not once for each reference to the field.

  if (this->called_fieldtrack_)
    return this;
  this->called_fieldtrack_ = true;

  Location loc = this->location();

  std::string s = "fieldtrack \"";
  Named_type* nt = this->expr_->type()->named_type();
  if (nt == NULL || nt->named_object()->package() == NULL)
    s.append(gogo->pkgpath());
  else
    s.append(nt->named_object()->package()->pkgpath());
  s.push_back('.');
  if (nt != NULL)
    s.append(Gogo::unpack_hidden_name(nt->name()));
  s.push_back('.');
  s.append(field->field_name());
  s.push_back('"');

  // We can't use a string here, because internally a string holds a
  // pointer to the actual bytes; when the linker garbage collects the
  // string, it won't garbage collect the bytes.  So we use a
  // [...]byte.

  mpz_t val;
  mpz_init_set_ui(val, s.length());
  Expression* length_expr = Expression::make_integer(&val, NULL, loc);
  mpz_clear(val);

  Type* byte_type = gogo->lookup_global("byte")->type_value();
  Type* array_type = Type::make_array_type(byte_type, length_expr);

  Expression_list* bytes = new Expression_list();
  for (std::string::const_iterator p = s.begin(); p != s.end(); p++)
    {
      mpz_init_set_ui(val, *p);
      Expression* byte = Expression::make_integer(&val, NULL, loc);
      mpz_clear(val);
      bytes->push_back(byte);
    }

  Expression* e = Expression::make_composite_literal(array_type, 0, false,
						     bytes, false, loc);

  Variable* var = new Variable(array_type, e, true, false, false, loc);

  static int count;
  char buf[50];
  snprintf(buf, sizeof buf, "fieldtrack.%d", count);
  ++count;

  Named_object* no = gogo->add_variable(buf, var);
  e = Expression::make_var_reference(no, loc);
  e = Expression::make_unary(OPERATOR_AND, e, loc);

  Expression* call = Runtime::make_call(Runtime::FIELDTRACK, loc, 1, e);
  inserter->insert(Statement::make_statement(call, false));

  // Put this function, and the global variable we just created, into
  // unique sections.  This will permit the linker to garbage collect
  // them if they are not referenced.  The effect is that the only
  // strings, indicating field references, that will wind up in the
  // executable will be those for functions that are actually needed.
  if (function != NULL)
    function->func_value()->set_in_unique_section();
  var->set_in_unique_section();

  return this;
}

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
  Bexpression* bstruct = tree_to_expr(this->expr_->get_tree(context));
  Bexpression* ret =
      context->gogo()->backend()->struct_field_expression(bstruct,
                                                          this->field_index_,
                                                          this->location());
  return expr_to_tree(ret);
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

// Return an expression for the pointer to the function to call.

Expression*
Interface_field_reference_expression::get_function()
{
  Expression* ref = this->expr_;
  Location loc = this->location();
  if (ref->type()->points_to() != NULL)
    ref = Expression::make_unary(OPERATOR_MULT, ref, loc);

  Expression* mtable =
      Expression::make_interface_info(ref, INTERFACE_INFO_METHODS, loc);
  Struct_type* mtable_type = mtable->type()->points_to()->struct_type();

  std::string name = Gogo::unpack_hidden_name(this->name_);
  unsigned int index;
  const Struct_field* field = mtable_type->find_local_field(name, &index);
  go_assert(field != NULL);
  mtable = Expression::make_unary(OPERATOR_MULT, mtable, loc);
  return Expression::make_field_reference(mtable, index, loc);
}

// Return an expression for the first argument to pass to the interface
// function.

Expression*
Interface_field_reference_expression::get_underlying_object()
{
  Expression* expr = this->expr_;
  if (expr->type()->points_to() != NULL)
    expr = Expression::make_unary(OPERATOR_MULT, expr, this->location());
  return Expression::make_interface_info(expr, INTERFACE_INFO_OBJECT,
                                         this->location());
}

// Traversal.

int
Interface_field_reference_expression::do_traverse(Traverse* traverse)
{
  return Expression::traverse(&this->expr_, traverse);
}

// Lower the expression.  If this expression is not called, we need to
// evaluate the expression twice when converting to the backend
// interface.  So introduce a temporary variable if necessary.

Expression*
Interface_field_reference_expression::do_lower(Gogo*, Named_object*,
					       Statement_inserter* inserter,
					       int)
{
  if (!this->expr_->is_variable())
    {
      Temporary_statement* temp =
	Statement::make_temporary(this->expr_->type(), NULL, this->location());
      inserter->insert(temp);
      this->expr_ = Expression::make_set_and_use_temporary(temp, this->expr_,
							   this->location());
    }
  return this;
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

// If an interface field reference is not simply called, then it is
// represented as a closure.  The closure will hold a single variable,
// the value of the interface on which the method should be called.
// The function will be a simple thunk that pulls the value from the
// closure and calls the method with the remaining arguments.

// Because method values are not common, we don't build all thunks for
// all possible interface methods, but instead only build them as we
// need them.  In particular, we even build them on demand for
// interface methods defined in other packages.

Interface_field_reference_expression::Interface_method_thunks
  Interface_field_reference_expression::interface_method_thunks;

// Find or create the thunk to call method NAME on TYPE.

Named_object*
Interface_field_reference_expression::create_thunk(Gogo* gogo,
						   Interface_type* type,
						   const std::string& name)
{
  std::pair<Interface_type*, Method_thunks*> val(type, NULL);
  std::pair<Interface_method_thunks::iterator, bool> ins =
    Interface_field_reference_expression::interface_method_thunks.insert(val);
  if (ins.second)
    {
      // This is the first time we have seen this interface.
      ins.first->second = new Method_thunks();
    }

  for (Method_thunks::const_iterator p = ins.first->second->begin();
       p != ins.first->second->end();
       p++)
    if (p->first == name)
      return p->second;

  Location loc = type->location();

  const Typed_identifier* method_id = type->find_method(name);
  if (method_id == NULL)
    return Named_object::make_erroneous_name(Gogo::thunk_name());

  Function_type* orig_fntype = method_id->type()->function_type();
  if (orig_fntype == NULL)
    return Named_object::make_erroneous_name(Gogo::thunk_name());

  Struct_field_list* sfl = new Struct_field_list();
  // The type here is wrong--it should be the C function type.  But it
  // doesn't really matter.
  Type* vt = Type::make_pointer_type(Type::make_void_type());
  sfl->push_back(Struct_field(Typed_identifier("fn.0", vt, loc)));
  sfl->push_back(Struct_field(Typed_identifier("val.1", type, loc)));
  Type* closure_type = Type::make_struct_type(sfl, loc);
  closure_type = Type::make_pointer_type(closure_type);

  Function_type* new_fntype = orig_fntype->copy_with_names();

  Named_object* new_no = gogo->start_function(Gogo::thunk_name(), new_fntype,
					      false, loc);

  Variable* cvar = new Variable(closure_type, NULL, false, false, false, loc);
  cvar->set_is_used();
  Named_object* cp = Named_object::make_variable("$closure", NULL, cvar);
  new_no->func_value()->set_closure_var(cp);

  gogo->start_block(loc);

  // Field 0 of the closure is the function code pointer, field 1 is
  // the value on which to invoke the method.
  Expression* arg = Expression::make_var_reference(cp, loc);
  arg = Expression::make_unary(OPERATOR_MULT, arg, loc);
  arg = Expression::make_field_reference(arg, 1, loc);

  Expression *ifre = Expression::make_interface_field_reference(arg, name,
								loc);

  const Typed_identifier_list* orig_params = orig_fntype->parameters();
  Expression_list* args;
  if (orig_params == NULL || orig_params->empty())
    args = NULL;
  else
    {
      const Typed_identifier_list* new_params = new_fntype->parameters();
      args = new Expression_list();
      for (Typed_identifier_list::const_iterator p = new_params->begin();
	   p != new_params->end();
	   ++p)
	{
	  Named_object* p_no = gogo->lookup(p->name(), NULL);
	  go_assert(p_no != NULL
		    && p_no->is_variable()
		    && p_no->var_value()->is_parameter());
	  args->push_back(Expression::make_var_reference(p_no, loc));
	}
    }

  Call_expression* call = Expression::make_call(ifre, args,
						orig_fntype->is_varargs(),
						loc);
  call->set_varargs_are_lowered();

  Statement* s = Statement::make_return_from_call(call, loc);
  gogo->add_statement(s);
  Block* b = gogo->finish_block(loc);
  gogo->add_block(b, loc);
  gogo->lower_block(new_no, b);
  gogo->flatten_block(new_no, b);
  gogo->finish_function(loc);

  ins.first->second->push_back(std::make_pair(name, new_no));
  return new_no;
}

// Get a tree for a method value.

tree
Interface_field_reference_expression::do_get_tree(Translate_context* context)
{
  Interface_type* type = this->expr_->type()->interface_type();
  if (type == NULL)
    {
      go_assert(saw_errors());
      return error_mark_node;
    }

  Named_object* thunk =
    Interface_field_reference_expression::create_thunk(context->gogo(),
						       type, this->name_);
  if (thunk->is_erroneous())
    {
      go_assert(saw_errors());
      return error_mark_node;
    }

  // FIXME: We should lower this earlier, but we can't it lower it in
  // the lowering pass because at that point we don't know whether we
  // need to create the thunk or not.  If the expression is called, we
  // don't need the thunk.

  Location loc = this->location();

  Struct_field_list* fields = new Struct_field_list();
  fields->push_back(Struct_field(Typed_identifier("fn.0",
						  thunk->func_value()->type(),
						  loc)));
  fields->push_back(Struct_field(Typed_identifier("val.1",
						  this->expr_->type(),
						  loc)));
  Struct_type* st = Type::make_struct_type(fields, loc);

  Expression_list* vals = new Expression_list();
  vals->push_back(Expression::make_func_code_reference(thunk, loc));
  vals->push_back(this->expr_);

  Expression* expr = Expression::make_struct_composite_literal(st, vals, loc);
  expr = Expression::make_heap_expression(expr, loc);

  Bexpression* bclosure = tree_to_expr(expr->get_tree(context));
  Expression* nil_check =
      Expression::make_binary(OPERATOR_EQEQ, this->expr_,
                              Expression::make_nil(loc), loc);
  Bexpression* bnil_check = tree_to_expr(nil_check->get_tree(context));

  Gogo* gogo = context->gogo();
  Expression* crash = gogo->runtime_error(RUNTIME_ERROR_NIL_DEREFERENCE, loc);
  Bexpression* bcrash = tree_to_expr(crash->get_tree(context));

  Bexpression* bcond =
      gogo->backend()->conditional_expression(NULL, bnil_check, bcrash, NULL, loc);
  Bstatement* cond_statement = gogo->backend()->expression_statement(bcond);
  Bexpression* ret =
      gogo->backend()->compound_expression(cond_statement, bclosure, loc);
  return expr_to_tree(ret);
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
      int i = 0;
      for (Typed_identifier_list::const_iterator p = method_parameters->begin();
	   p != method_parameters->end();
	   ++p, ++i)
	{
	  if (!p->name().empty())
	    parameters->push_back(*p);
	  else
	    {
	      char buf[20];
	      snprintf(buf, sizeof buf, "$param%d", i);
	      parameters->push_back(Typed_identifier(buf, p->type(),
						     p->location()));
	    }
	}
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
  // FIXME: This optimization is disabled because it doesn't yet work
  // with function descriptors when the method expression is not
  // directly called.
  if (method != NULL && is_pointer && false)
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
  if (parameters->size() <= 1)
    args = NULL;
  else
    {
      args = new Expression_list();
      Typed_identifier_list::const_iterator p = parameters->begin();
      ++p;
      for (; p != parameters->end(); ++p)
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

  Statement* s = Statement::make_return_from_call(call, location);
  gogo->add_statement(s);

  Block* b = gogo->finish_block(location);

  gogo->add_block(b, location);

  // Lower the call in case there are multiple results.
  gogo->lower_block(no, b);
  gogo->flatten_block(no, b);

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
  Gogo* gogo = context->gogo();
  Location loc = this->location();
  Expression* space = gogo->allocate_memory(this->type_, loc);
  Bexpression* bspace = tree_to_expr(space->get_tree(context));
  Btype* pbtype = gogo->backend()->pointer_type(this->type_->get_backend(gogo));
  Bexpression* ret = gogo->backend()->convert_expression(pbtype, bspace, loc);
  return expr_to_tree(ret);
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
      type_(type), vals_(vals), traverse_order_(NULL)
  { }

  // Set the traversal order, used to ensure that we implement the
  // order of evaluation rules.  Takes ownership of the argument.
  void
  set_traverse_order(std::vector<int>* traverse_order)
  { this->traverse_order_ = traverse_order; }

  // Return whether this is a constant initializer.
  bool
  is_constant_struct() const;

 protected:
  int
  do_traverse(Traverse* traverse);

  bool
  do_is_immutable() const;

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
    Struct_construction_expression* ret =
      new Struct_construction_expression(this->type_, this->vals_->copy(),
					 this->location());
    if (this->traverse_order_ != NULL)
      ret->set_traverse_order(this->traverse_order_);
    return ret;
  }

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
  // If not NULL, the order in which to traverse vals_.  This is used
  // so that we implement the order of evaluation rules correctly.
  std::vector<int>* traverse_order_;
};

// Traversal.

int
Struct_construction_expression::do_traverse(Traverse* traverse)
{
  if (this->vals_ != NULL)
    {
      if (this->traverse_order_ == NULL)
	{
	  if (this->vals_->traverse(traverse) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
      else
	{
	  for (std::vector<int>::const_iterator p =
		 this->traverse_order_->begin();
	       p != this->traverse_order_->end();
	       ++p)
	    {
	      if (Expression::traverse(&this->vals_->at(*p), traverse)
		  == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	}
    }
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

// Return whether this struct is immutable.

bool
Struct_construction_expression::do_is_immutable() const
{
  if (this->vals_ == NULL)
    return true;
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (*pv != NULL && !(*pv)->is_immutable())
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

  Btype* btype = this->type_->get_backend(gogo);
  if (this->vals_ == NULL)
    return expr_to_tree(gogo->backend()->zero_expression(btype));

  const Struct_field_list* fields = this->type_->struct_type()->fields();
  Expression_list::const_iterator pv = this->vals_->begin();
  std::vector<Bexpression*> init;
  for (Struct_field_list::const_iterator pf = fields->begin();
       pf != fields->end();
       ++pf)
    {
      Btype* fbtype = pf->type()->get_backend(gogo);
      if (pv == this->vals_->end())
        init.push_back(gogo->backend()->zero_expression(fbtype));
      else if (*pv == NULL)
	{
          init.push_back(gogo->backend()->zero_expression(fbtype));
	  ++pv;
	}
      else
	{
          Expression* val =
              Expression::convert_for_assignment(gogo, pf->type(),
                                                 *pv, this->location());
          init.push_back(tree_to_expr(val->get_tree(context)));
	  ++pv;
	}
    }

  Bexpression* ret =
      gogo->backend()->constructor_expression(btype, init, this->location());
  return expr_to_tree(ret);
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
// Slice_construction_expression.

class Array_construction_expression : public Expression
{
 protected:
  Array_construction_expression(Expression_classification classification,
				Type* type,
				const std::vector<unsigned long>* indexes,
				Expression_list* vals, Location location)
    : Expression(classification, location),
      type_(type), indexes_(indexes), vals_(vals)
  { go_assert(indexes == NULL || indexes->size() == vals->size()); }

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

  bool
  do_is_immutable() const;

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*);

  void
  do_check_types(Gogo*);

  void
  do_export(Export*) const;

  // The indexes.
  const std::vector<unsigned long>*
  indexes()
  { return this->indexes_; }

  // The list of values.
  Expression_list*
  vals()
  { return this->vals_; }

  // Get the backend constructor for the array values.
  Bexpression*
  get_constructor(Translate_context* context, Btype* btype);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of the array to construct.
  Type* type_;
  // The list of indexes into the array, one for each value.  This may
  // be NULL, in which case the indexes start at zero and increment.
  const std::vector<unsigned long>* indexes_;
  // The list of values.  This may be NULL if there are no values.
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

// Return whether this is an immutable array initializer.

bool
Array_construction_expression::do_is_immutable() const
{
  if (this->vals_ == NULL)
    return true;
  for (Expression_list::const_iterator pv = this->vals_->begin();
       pv != this->vals_->end();
       ++pv)
    {
      if (*pv != NULL && !(*pv)->is_immutable())
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
}

// Get a constructor expression for the array values.

Bexpression*
Array_construction_expression::get_constructor(Translate_context* context,
                                               Btype* array_btype)
{
  Type* element_type = this->type_->array_type()->element_type();

  std::vector<unsigned long> indexes;
  std::vector<Bexpression*> vals;
  Gogo* gogo = context->gogo();
  if (this->vals_ != NULL)
    {
      size_t i = 0;
      std::vector<unsigned long>::const_iterator pi;
      if (this->indexes_ != NULL)
	pi = this->indexes_->begin();
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv, ++i)
	{
	  if (this->indexes_ != NULL)
	    go_assert(pi != this->indexes_->end());

	  if (this->indexes_ == NULL)
	    indexes.push_back(i);
	  else
	    indexes.push_back(*pi);
	  if (*pv == NULL)
	    {
	      Btype* ebtype = element_type->get_backend(gogo);
	      Bexpression *zv = gogo->backend()->zero_expression(ebtype);
	      vals.push_back(zv);
	    }
	  else
	    {
              Expression* val_expr =
                  Expression::convert_for_assignment(gogo, element_type, *pv,
                                                     this->location());
	      vals.push_back(tree_to_expr(val_expr->get_tree(context)));
	    }
	  if (this->indexes_ != NULL)
	    ++pi;
	}
      if (this->indexes_ != NULL)
	go_assert(pi == this->indexes_->end());
    }
  return gogo->backend()->array_constructor_expression(array_btype, indexes,
                                                       vals, this->location());
}

// Export an array construction.

void
Array_construction_expression::do_export(Export* exp) const
{
  exp->write_c_string("convert(");
  exp->write_type(this->type_);
  if (this->vals_ != NULL)
    {
      std::vector<unsigned long>::const_iterator pi;
      if (this->indexes_ != NULL)
	pi = this->indexes_->begin();
      for (Expression_list::const_iterator pv = this->vals_->begin();
	   pv != this->vals_->end();
	   ++pv)
	{
	  exp->write_c_string(", ");

	  if (this->indexes_ != NULL)
	    {
	      char buf[100];
	      snprintf(buf, sizeof buf, "%lu", *pi);
	      exp->write_c_string(buf);
	      exp->write_c_string(":");
	    }

	  if (*pv != NULL)
	    (*pv)->export_expression(exp);

	  if (this->indexes_ != NULL)
	    ++pi;
	}
    }
  exp->write_c_string(")");
}

// Dump ast representation of an array construction expressin.

void
Array_construction_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  Expression* length = this->type_->array_type()->length();

  ast_dump_context->ostream() << "[" ;
  if (length != NULL)
    {
      ast_dump_context->dump_expression(length);
    }
  ast_dump_context->ostream() << "]" ;
  ast_dump_context->dump_type(this->type_);
  ast_dump_context->ostream() << "{" ;
  if (this->indexes_ == NULL)
    ast_dump_context->dump_expression_list(this->vals_);
  else
    {
      Expression_list::const_iterator pv = this->vals_->begin();
      for (std::vector<unsigned long>::const_iterator pi =
	     this->indexes_->begin();
	   pi != this->indexes_->end();
	   ++pi, ++pv)
	{
	  if (pi != this->indexes_->begin())
	    ast_dump_context->ostream() << ", ";
	  ast_dump_context->ostream() << *pi << ':';
	  ast_dump_context->dump_expression(*pv);
	}
    }
  ast_dump_context->ostream() << "}" ;

}

// Construct a fixed array.

class Fixed_array_construction_expression :
  public Array_construction_expression
{
 public:
  Fixed_array_construction_expression(Type* type,
				      const std::vector<unsigned long>* indexes,
				      Expression_list* vals, Location location)
    : Array_construction_expression(EXPRESSION_FIXED_ARRAY_CONSTRUCTION,
				    type, indexes, vals, location)
  { go_assert(type->array_type() != NULL && !type->is_slice_type()); }

 protected:
  Expression*
  do_copy()
  {
    return new Fixed_array_construction_expression(this->type(),
						   this->indexes(),
						   (this->vals() == NULL
						    ? NULL
						    : this->vals()->copy()),
						   this->location());
  }

  tree
  do_get_tree(Translate_context*);
};

// Return a tree for constructing a fixed array.

tree
Fixed_array_construction_expression::do_get_tree(Translate_context* context)
{
  Type* type = this->type();
  Btype* btype = type->get_backend(context->gogo());
  return expr_to_tree(this->get_constructor(context, btype));
}

Expression*
Expression::make_array_composite_literal(Type* type, Expression_list* vals,
                                         Location location)
{
  go_assert(type->array_type() != NULL && !type->is_slice_type());
  return new Fixed_array_construction_expression(type, NULL, vals, location);
}

// Construct a slice.

class Slice_construction_expression : public Array_construction_expression
{
 public:
  Slice_construction_expression(Type* type,
				const std::vector<unsigned long>* indexes,
				Expression_list* vals, Location location)
    : Array_construction_expression(EXPRESSION_SLICE_CONSTRUCTION,
				    type, indexes, vals, location),
      valtype_(NULL)
  { go_assert(type->is_slice_type()); }

 protected:
  // Note that taking the address of a slice literal is invalid.

  Expression*
  do_copy()
  {
    return new Slice_construction_expression(this->type(), this->indexes(),
					     (this->vals() == NULL
					      ? NULL
					      : this->vals()->copy()),
					     this->location());
  }

  tree
  do_get_tree(Translate_context*);

 private:
  // The type of the values in this slice.
  Type* valtype_;
};

// Return a tree for constructing a slice.

tree
Slice_construction_expression::do_get_tree(Translate_context* context)
{
  Array_type* array_type = this->type()->array_type();
  if (array_type == NULL)
    {
      go_assert(this->type()->is_error());
      return error_mark_node;
    }

  Type* element_type = array_type->element_type();
  if (this->valtype_ == NULL)
    {
      mpz_t lenval;
      Expression* length;
      if (this->vals() == NULL || this->vals()->empty())
        mpz_init_set_ui(lenval, 0);
      else
        {
          if (this->indexes() == NULL)
            mpz_init_set_ui(lenval, this->vals()->size());
          else
            mpz_init_set_ui(lenval, this->indexes()->back() + 1);
        }
      Location loc = this->location();
      Type* int_type = Type::lookup_integer_type("int");
      length = Expression::make_integer(&lenval, int_type, loc);
      mpz_clear(lenval);
      this->valtype_ = Type::make_array_type(element_type, length);
    }

  tree values;
  Gogo* gogo = context->gogo();
  Btype* val_btype = this->valtype_->get_backend(gogo);
  if (this->vals() == NULL || this->vals()->empty())
    {
      // We need to create a unique value.
      Btype* int_btype = Type::lookup_integer_type("int")->get_backend(gogo);
      Bexpression* zero = gogo->backend()->zero_expression(int_btype);
      std::vector<unsigned long> index(1, 0);
      std::vector<Bexpression*> val(1, zero);
      Bexpression* ctor =
	gogo->backend()->array_constructor_expression(val_btype, index, val,
						      this->location());
      values = expr_to_tree(ctor);
    }
  else
    values = expr_to_tree(this->get_constructor(context, val_btype));

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
      Expression* alloc =
          context->gogo()->allocate_memory(this->valtype_, this->location());
      space = save_expr(alloc->get_tree(context));

      tree s = fold_convert(build_pointer_type(TREE_TYPE(values)), space);
      tree ref = build_fold_indirect_ref_loc(this->location().gcc_location(),
                                             s);
      TREE_THIS_NOTRAP(ref) = 1;
      set = build2(MODIFY_EXPR, void_type_node, ref, values);
    }

  // Build a constructor for the slice.

  tree type_tree = type_to_tree(this->type()->get_backend(context->gogo()));
  if (type_tree == error_mark_node)
    return error_mark_node;
  go_assert(TREE_CODE(type_tree) == RECORD_TYPE);

  vec<constructor_elt, va_gc> *init;
  vec_alloc(init, 3);

  constructor_elt empty = {NULL, NULL};
  constructor_elt* elt = init->quick_push(empty);
  tree field = TYPE_FIELDS(type_tree);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__values") == 0);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), space);

  tree length_tree = this->valtype_->array_type()->length()->get_tree(context);
  elt = init->quick_push(empty);
  field = DECL_CHAIN(field);
  go_assert(strcmp(IDENTIFIER_POINTER(DECL_NAME(field)), "__count") == 0);
  elt->index = field;
  elt->value = fold_convert(TREE_TYPE(field), length_tree);

  elt = init->quick_push(empty);
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
  return new Slice_construction_expression(type, NULL, vals, location);
}

// Construct a map.

class Map_construction_expression : public Expression
{
 public:
  Map_construction_expression(Type* type, Expression_list* vals,
			      Location location)
    : Expression(EXPRESSION_MAP_CONSTRUCTION, location),
      type_(type), vals_(vals), element_type_(NULL), constructor_temp_(NULL)
  { go_assert(vals == NULL || vals->size() % 2 == 0); }

 protected:
  int
  do_traverse(Traverse* traverse);

  Expression*
  do_flatten(Gogo*, Named_object*, Statement_inserter*);

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
  // The type of the key-value pair struct for each map element.
  Struct_type* element_type_;
  // A temporary reference to the variable storing the constructor initializer.
  Temporary_statement* constructor_temp_;
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

// Flatten constructor initializer into a temporary variable since
// we need to take its address for __go_construct_map.

Expression*
Map_construction_expression::do_flatten(Gogo* gogo, Named_object*,
                                        Statement_inserter* inserter)
{
  if (!this->is_error_expression()
      && this->vals_ != NULL
      && !this->vals_->empty()
      && this->constructor_temp_ == NULL)
    {
      Map_type* mt = this->type_->map_type();
      Type* key_type = mt->key_type();
      Type* val_type = mt->val_type();
      this->element_type_ = Type::make_builtin_struct_type(2,
                                                           "__key", key_type,
                                                           "__val", val_type);

      Expression_list* value_pairs = new Expression_list();
      Location loc = this->location();

      size_t i = 0;
      for (Expression_list::const_iterator pv = this->vals_->begin();
           pv != this->vals_->end();
           ++pv, ++i)
        {
          Expression_list* key_value_pair = new Expression_list();
          Expression* key =
              Expression::convert_for_assignment(gogo, key_type, *pv, loc);

          ++pv;
          Expression* val =
              Expression::convert_for_assignment(gogo, val_type, *pv, loc);

          key_value_pair->push_back(key);
          key_value_pair->push_back(val);
          value_pairs->push_back(
              Expression::make_struct_composite_literal(this->element_type_,
                                                        key_value_pair, loc));
        }

      mpz_t lenval;
      mpz_init_set_ui(lenval, i);
      Expression* element_count = Expression::make_integer(&lenval, NULL, loc);
      mpz_clear(lenval);

      Type* ctor_type =
          Type::make_array_type(this->element_type_, element_count);
      Expression* constructor =
          new Fixed_array_construction_expression(ctor_type, NULL,
                                                  value_pairs, loc);

      this->constructor_temp_ =
          Statement::make_temporary(NULL, constructor, loc);
      constructor->issue_nil_check();
      this->constructor_temp_->set_is_address_taken();
      inserter->insert(this->constructor_temp_);
    }

  return this;
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
  if (this->is_error_expression())
    return error_mark_node;
  Location loc = this->location();

  size_t i = 0;
  Expression* ventries;
  if (this->vals_ == NULL || this->vals_->empty())
    ventries = Expression::make_nil(loc);
  else
    {
      go_assert(this->constructor_temp_ != NULL);
      i = this->vals_->size() / 2;

      Expression* ctor_ref =
          Expression::make_temporary_reference(this->constructor_temp_, loc);
      ventries = Expression::make_unary(OPERATOR_AND, ctor_ref, loc);
    }

  Map_type* mt = this->type_->map_type();
  if (this->element_type_ == NULL)
      this->element_type_ =
          Type::make_builtin_struct_type(2,
                                         "__key", mt->key_type(),
                                         "__val", mt->val_type());
  Expression* descriptor = Expression::make_map_descriptor(mt, loc);

  Type* uintptr_t = Type::lookup_integer_type("uintptr");
  mpz_t countval;
  mpz_init_set_ui(countval, i);
  Expression* count = Expression::make_integer(&countval, uintptr_t, loc);
  mpz_clear(countval);

  Expression* entry_size =
      Expression::make_type_info(this->element_type_, TYPE_INFO_SIZE);

  unsigned int field_index;
  const Struct_field* valfield =
      this->element_type_->find_local_field("__val", &field_index);
  Expression* val_offset =
      Expression::make_struct_field_offset(this->element_type_, valfield);
  Expression* val_size =
      Expression::make_type_info(mt->val_type(), TYPE_INFO_SIZE);

  Expression* map_ctor =
      Runtime::make_call(Runtime::CONSTRUCT_MAP, loc, 6, descriptor, count,
                         entry_size, val_offset, val_size, ventries);
  return map_ctor->get_tree(context);
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
			       Expression_list* vals, bool all_are_names,
			       Location location)
    : Parser_expression(EXPRESSION_COMPOSITE_LITERAL, location),
      type_(type), depth_(depth), vals_(vals), has_keys_(has_keys),
      all_are_names_(all_are_names)
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
					    this->all_are_names_,
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
  make_array(Type*, const std::vector<unsigned long>*, Expression_list*);

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
  // If this is true, then HAS_KEYS_ is true, and every key is a
  // simple identifier.
  bool all_are_names_;
};

// Traversal.

int
Composite_literal_expression::do_traverse(Traverse* traverse)
{
  if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;

  // If this is a struct composite literal with keys, then the keys
  // are field names, not expressions.  We don't want to traverse them
  // in that case.  If we do, we can give an erroneous error "variable
  // initializer refers to itself."  See bug482.go in the testsuite.
  if (this->has_keys_ && this->vals_ != NULL)
    {
      // The type may not be resolvable at this point.
      Type* type = this->type_;

      for (int depth = this->depth_; depth > 0; --depth)
        {
          if (type->array_type() != NULL)
            type = type->array_type()->element_type();
          else if (type->map_type() != NULL)
            type = type->map_type()->val_type();
          else
            {
              // This error will be reported during lowering.
              return TRAVERSE_CONTINUE;
            }
        }

      while (true)
	{
	  if (type->classification() == Type::TYPE_NAMED)
	    type = type->named_type()->real_type();
	  else if (type->classification() == Type::TYPE_FORWARD)
	    {
	      Type* t = type->forwarded();
	      if (t == type)
		break;
	      type = t;
	    }
	  else
	    break;
	}

      if (type->classification() == Type::TYPE_STRUCT)
	{
	  Expression_list::iterator p = this->vals_->begin();
	  while (p != this->vals_->end())
	    {
	      // Skip key.
	      ++p;
	      go_assert(p != this->vals_->end());
	      if (Expression::traverse(&*p, traverse) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	      ++p;
	    }
	  return TRAVERSE_CONTINUE;
	}
    }

  if (this->vals_ != NULL)
    return this->vals_->traverse(traverse);

  return TRAVERSE_CONTINUE;
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
    ret = Expression::make_heap_expression(ret, this->location());

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
  std::vector<int>* traverse_order = new(std::vector<int>);
  Expression_list::const_iterator p = this->vals_->begin();
  Expression* external_expr = NULL;
  const Named_object* external_no = NULL;
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
	  if (no->package() != NULL && external_expr == NULL)
	    {
	      external_expr = name_expr;
	      external_no = no;
	    }

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
      traverse_order->push_back(index);
    }

  if (!this->all_are_names_)
    {
      // This is a weird case like bug462 in the testsuite.
      if (external_expr == NULL)
	error_at(this->location(), "unknown field in %qs literal",
		 (type->named_type() != NULL
		  ? type->named_type()->message_name().c_str()
		  : "unnamed struct"));
      else
	error_at(external_expr->location(), "unknown field %qs in %qs",
		 external_no->message_name().c_str(),
		 (type->named_type() != NULL
		  ? type->named_type()->message_name().c_str()
		  : "unnamed struct"));
      return Expression::make_error(location);
    }

  Expression_list* list = new Expression_list;
  list->reserve(field_count);
  for (size_t i = 0; i < field_count; ++i)
    list->push_back(vals[i]);

  Struct_construction_expression* ret =
    new Struct_construction_expression(type, list, location);
  ret->set_traverse_order(traverse_order);
  return ret;
}

// Used to sort an index/value array.

class Index_value_compare
{
 public:
  bool
  operator()(const std::pair<unsigned long, Expression*>& a,
	     const std::pair<unsigned long, Expression*>& b)
  { return a.first < b.first; }
};

// Lower an array composite literal.

Expression*
Composite_literal_expression::lower_array(Type* type)
{
  Location location = this->location();
  if (this->vals_ == NULL || !this->has_keys_)
    return this->make_array(type, NULL, this->vals_);

  std::vector<unsigned long>* indexes = new std::vector<unsigned long>;
  indexes->reserve(this->vals_->size());
  bool indexes_out_of_order = false;
  Expression_list* vals = new Expression_list();
  vals->reserve(this->vals_->size());
  unsigned long index = 0;
  Expression_list::const_iterator p = this->vals_->begin();
  while (p != this->vals_->end())
    {
      Expression* index_expr = *p;

      ++p;
      go_assert(p != this->vals_->end());
      Expression* val = *p;

      ++p;

      if (index_expr == NULL)
	{
	  if (!indexes->empty())
	    indexes->push_back(index);
	}
      else
	{
	  if (indexes->empty() && !vals->empty())
	    {
	      for (size_t i = 0; i < vals->size(); ++i)
		indexes->push_back(i);
	    }

	  Numeric_constant nc;
	  if (!index_expr->numeric_constant_value(&nc))
	    {
	      error_at(index_expr->location(),
		       "index expression is not integer constant");
	      return Expression::make_error(location);
	    }

	  switch (nc.to_unsigned_long(&index))
	    {
	    case Numeric_constant::NC_UL_VALID:
	      break;
	    case Numeric_constant::NC_UL_NOTINT:
	      error_at(index_expr->location(),
		       "index expression is not integer constant");
	      return Expression::make_error(location);
	    case Numeric_constant::NC_UL_NEGATIVE:
	      error_at(index_expr->location(), "index expression is negative");
	      return Expression::make_error(location);
	    case Numeric_constant::NC_UL_BIG:
	      error_at(index_expr->location(), "index value overflow");
	      return Expression::make_error(location);
	    default:
	      go_unreachable();
	    }

	  Named_type* ntype = Type::lookup_integer_type("int");
	  Integer_type* inttype = ntype->integer_type();
	  if (sizeof(index) <= static_cast<size_t>(inttype->bits() * 8)
	      && index >> (inttype->bits() - 1) != 0)
	    {
	      error_at(index_expr->location(), "index value overflow");
	      return Expression::make_error(location);
	    }

	  if (std::find(indexes->begin(), indexes->end(), index)
	      != indexes->end())
	    {
	      error_at(index_expr->location(), "duplicate value for index %lu",
		       index);
	      return Expression::make_error(location);
	    }

	  if (!indexes->empty() && index < indexes->back())
	    indexes_out_of_order = true;

	  indexes->push_back(index);
	}

      vals->push_back(val);

      ++index;
    }

  if (indexes->empty())
    {
      delete indexes;
      indexes = NULL;
    }

  if (indexes_out_of_order)
    {
      typedef std::vector<std::pair<unsigned long, Expression*> > V;

      V v;
      v.reserve(indexes->size());
      std::vector<unsigned long>::const_iterator pi = indexes->begin();
      for (Expression_list::const_iterator pe = vals->begin();
	   pe != vals->end();
	   ++pe, ++pi)
	v.push_back(std::make_pair(*pi, *pe));

      std::sort(v.begin(), v.end(), Index_value_compare());

      delete indexes;
      delete vals;
      indexes = new std::vector<unsigned long>();
      indexes->reserve(v.size());
      vals = new Expression_list();
      vals->reserve(v.size());

      for (V::const_iterator p = v.begin(); p != v.end(); ++p)
	{
	  indexes->push_back(p->first);
	  vals->push_back(p->second);
	}
    }

  return this->make_array(type, indexes, vals);
}

// Actually build the array composite literal. This handles
// [...]{...}.

Expression*
Composite_literal_expression::make_array(
    Type* type,
    const std::vector<unsigned long>* indexes,
    Expression_list* vals)
{
  Location location = this->location();
  Array_type* at = type->array_type();

  if (at->length() != NULL && at->length()->is_nil_expression())
    {
      size_t size;
      if (vals == NULL)
	size = 0;
      else if (indexes != NULL)
	size = indexes->back() + 1;
      else
	{
	  size = vals->size();
	  Integer_type* it = Type::lookup_integer_type("int")->integer_type();
	  if (sizeof(size) <= static_cast<size_t>(it->bits() * 8)
	      && size >> (it->bits() - 1) != 0)
	    {
	      error_at(location, "too many elements in composite literal");
	      return Expression::make_error(location);
	    }
	}

      mpz_t vlen;
      mpz_init_set_ui(vlen, size);
      Expression* elen = Expression::make_integer(&vlen, NULL, location);
      mpz_clear(vlen);
      at = Type::make_array_type(at->element_type(), elen);
      type = at;
    }
  else if (at->length() != NULL
	   && !at->length()->is_error_expression()
	   && this->vals_ != NULL)
    {
      Numeric_constant nc;
      unsigned long val;
      if (at->length()->numeric_constant_value(&nc)
	  && nc.to_unsigned_long(&val) == Numeric_constant::NC_UL_VALID)
	{
	  if (indexes == NULL)
	    {
	      if (this->vals_->size() > val)
		{
		  error_at(location, "too many elements in composite literal");
		  return Expression::make_error(location);
		}
	    }
	  else
	    {
	      unsigned long max = indexes->back();
	      if (max >= val)
		{
		  error_at(location,
			   ("some element keys in composite literal "
			    "are out of range"));
		  return Expression::make_error(location);
		}
	    }
	}
    }

  if (at->length() != NULL)
    return new Fixed_array_construction_expression(type, indexes, vals,
						   location);
  else
    return new Slice_construction_expression(type, indexes, vals, location);
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
				   Expression_list* vals, bool all_are_names,
				   Location location)
{
  return new Composite_literal_expression(type, depth, has_keys, vals,
					  all_are_names, location);
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
    case EXPRESSION_SLICE_CONSTRUCTION:
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
    case EXPRESSION_SLICE_CONSTRUCTION:
      {
	const Slice_construction_expression *pace =
	  static_cast<const Slice_construction_expression*>(this);
	return !pace->is_constant_array();
      }
    case EXPRESSION_MAP_CONSTRUCTION:
      return true;
    default:
      return false;
    }
}

// Return true if this is a variable or temporary_variable.

bool
Expression::is_variable() const
{
  switch (this->classification_)
    {
    case EXPRESSION_VAR_REFERENCE:
    case EXPRESSION_TEMPORARY_REFERENCE:
    case EXPRESSION_SET_AND_USE_TEMPORARY:
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

Expression*
Type_guard_expression::do_flatten(Gogo*, Named_object*,
                                  Statement_inserter* inserter)
{
  if (!this->expr_->is_variable())
    {
      Temporary_statement* temp = Statement::make_temporary(NULL, this->expr_,
                                                            this->location());
      inserter->insert(temp);
      this->expr_ =
          Expression::make_temporary_reference(temp, this->location());
    }
  return this;
}

// Check types of a type guard expression.  The expression must have
// an interface type, but the actual type conversion is checked at run
// time.

void
Type_guard_expression::do_check_types(Gogo*)
{
  Type* expr_type = this->expr_->type();
  if (expr_type->interface_type() == NULL)
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
  Expression* conversion;
  if (this->type_->interface_type() != NULL)
    conversion =
        Expression::convert_interface_to_interface(this->type_, this->expr_,
                                                   true, this->location());
  else
    conversion =
        Expression::convert_for_assignment(context->gogo(), this->type_,
                                           this->expr_, this->location());

  return conversion->get_tree(context);
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

// Class Heap_expression.

// When you take the address of an escaping expression, it is allocated
// on the heap.  This class implements that.

class Heap_expression : public Expression
{
 public:
  Heap_expression(Expression* expr, Location location)
    : Expression(EXPRESSION_HEAP, location),
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
    return Expression::make_heap_expression(this->expr_->copy(),
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
  // The expression which is being put on the heap.
  Expression* expr_;
};

// Return a tree which allocates an expression on the heap.

tree
Heap_expression::do_get_tree(Translate_context* context)
{
  if (this->expr_->is_error_expression() || this->expr_->type()->is_error())
    return error_mark_node;

  Location loc = this->location();
  Gogo* gogo = context->gogo();
  Btype* btype = this->type()->get_backend(gogo);
  Expression* alloc = Expression::make_allocation(this->expr_->type(), loc);
  Bexpression* space = tree_to_expr(alloc->get_tree(context));

  Bstatement* decl;
  Named_object* fn = context->function();
  go_assert(fn != NULL);
  Bfunction* fndecl = fn->func_value()->get_or_make_decl(gogo, fn);
  Bvariable* space_temp =
    gogo->backend()->temporary_variable(fndecl, context->bblock(), btype,
					space, true, loc, &decl);
  space = gogo->backend()->var_expression(space_temp, loc);
  Bexpression* ref = gogo->backend()->indirect_expression(space, true, loc);

  Bexpression* bexpr = tree_to_expr(this->expr_->get_tree(context));
  Bstatement* assn = gogo->backend()->assignment_statement(ref, bexpr, loc);
  decl = gogo->backend()->compound_statement(decl, assn);
  space = gogo->backend()->var_expression(space_temp, loc);
  Bexpression* ret = gogo->backend()->compound_expression(decl, space, loc);
  return expr_to_tree(ret);
}

// Dump ast representation for a heap expression.

void
Heap_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "&(";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << ")";
}

// Allocate an expression on the heap.

Expression*
Expression::make_heap_expression(Expression* expr, Location location)
{
  return new Heap_expression(expr, location);
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

// Flattening for receive expressions creates a temporary variable to store
// received data in for receives.

Expression*
Receive_expression::do_flatten(Gogo*, Named_object*,
                               Statement_inserter* inserter)
{
  Channel_type* channel_type = this->channel_->type()->channel_type();
  if (channel_type == NULL)
    {
      go_assert(saw_errors());
      return this;
    }

  Type* element_type = channel_type->element_type();
  if (this->temp_receiver_ == NULL)
    {
      this->temp_receiver_ = Statement::make_temporary(element_type, NULL,
						       this->location());
      this->temp_receiver_->set_is_address_taken();
      inserter->insert(this->temp_receiver_);
    }

  return this;
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

  Expression* recv_ref =
    Expression::make_temporary_reference(this->temp_receiver_, loc);
  Expression* recv_addr =
    Expression::make_temporary_reference(this->temp_receiver_, loc);
  recv_addr = Expression::make_unary(OPERATOR_AND, recv_addr, loc);
  Expression* recv =
    Runtime::make_call(Runtime::RECEIVE, loc, 3,
		       td, this->channel_, recv_addr);
  recv = Expression::make_compound(recv, recv_ref, loc);
  return recv->get_tree(context);
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

  bool
  do_is_immutable() const
  { return true; }

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  { return this; }

  tree
  do_get_tree(Translate_context* context)
  {
    Bexpression* ret = this->type_->type_descriptor_pointer(context->gogo(),
                                                            this->location());
    return expr_to_tree(ret);
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
  bool
  do_is_immutable() const
  { return true; }

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
  Btype* btype = this->type_->get_backend(context->gogo());
  Gogo* gogo = context->gogo();
  size_t val;
  switch (this->type_info_)
    {
    case TYPE_INFO_SIZE:
      val = gogo->backend()->type_size(btype);
      break;
    case TYPE_INFO_ALIGNMENT:
      val = gogo->backend()->type_alignment(btype);
      break;
    case TYPE_INFO_FIELD_ALIGNMENT:
      val = gogo->backend()->type_field_alignment(btype);
      break;
    default:
      go_unreachable();
    }
  tree val_type_tree = type_to_tree(this->type()->get_backend(gogo));
  go_assert(val_type_tree != error_mark_node);
  return build_int_cstu(val_type_tree, val);
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

// An expression that evaluates to some characteristic of a slice.
// This is used when indexing, bound-checking, or nil checking a slice.

class Slice_info_expression : public Expression
{
 public:
  Slice_info_expression(Expression* slice, Slice_info slice_info,
                        Location location)
    : Expression(EXPRESSION_SLICE_INFO, location),
      slice_(slice), slice_info_(slice_info)
  { }

 protected:
  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return new Slice_info_expression(this->slice_->copy(), this->slice_info_,
                                     this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

  void
  do_issue_nil_check()
  { this->slice_->issue_nil_check(); }

 private:
  // The slice for which we are getting information.
  Expression* slice_;
  // What information we want.
  Slice_info slice_info_;
};

// Return the type of the slice info.

Type*
Slice_info_expression::do_type()
{
  switch (this->slice_info_)
    {
    case SLICE_INFO_VALUE_POINTER:
      return Type::make_pointer_type(
          this->slice_->type()->array_type()->element_type());
    case SLICE_INFO_LENGTH:
    case SLICE_INFO_CAPACITY:
        return Type::lookup_integer_type("int");
    default:
      go_unreachable();
    }
}

// Return slice information in GENERIC.

tree
Slice_info_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  Bexpression* bslice = tree_to_expr(this->slice_->get_tree(context));
  Bexpression* ret;
  switch (this->slice_info_)
    {
    case SLICE_INFO_VALUE_POINTER:
    case SLICE_INFO_LENGTH:
    case SLICE_INFO_CAPACITY:
      ret = gogo->backend()->struct_field_expression(bslice, this->slice_info_,
                                                     this->location());
      break;
    default:
      go_unreachable();
    }
  return expr_to_tree(ret);
}

// Dump ast representation for a type info expression.

void
Slice_info_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "sliceinfo(";
  this->slice_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ",";
  ast_dump_context->ostream() << 
      (this->slice_info_ == SLICE_INFO_VALUE_POINTER ? "values" 
    : this->slice_info_ == SLICE_INFO_LENGTH ? "length"
    : this->slice_info_ == SLICE_INFO_CAPACITY ? "capacity "
    : "unknown");
  ast_dump_context->ostream() << ")";
}

// Make a slice info expression.

Expression*
Expression::make_slice_info(Expression* slice, Slice_info slice_info,
                            Location location)
{
  return new Slice_info_expression(slice, slice_info, location);
}

// An expression that represents a slice value: a struct with value pointer,
// length, and capacity fields.

class Slice_value_expression : public Expression
{
 public:
  Slice_value_expression(Type* type, Expression* valptr, Expression* len,
                         Expression* cap, Location location)
      : Expression(EXPRESSION_SLICE_VALUE, location),
        type_(type), valptr_(valptr), len_(len), cap_(cap)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*)
  { go_unreachable(); }

  Expression*
  do_copy()
  {
    return new Slice_value_expression(this->type_, this->valptr_->copy(),
                                      this->len_->copy(), this->cap_->copy(),
                                      this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of the slice value.
  Type* type_;
  // The pointer to the values in the slice.
  Expression* valptr_;
  // The length of the slice.
  Expression* len_;
  // The capacity of the slice.
  Expression* cap_;
};

int
Slice_value_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->valptr_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->len_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->cap_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

tree
Slice_value_expression::do_get_tree(Translate_context* context)
{
  std::vector<Bexpression*> vals(3);
  vals[0] = tree_to_expr(this->valptr_->get_tree(context));
  vals[1] = tree_to_expr(this->len_->get_tree(context));
  vals[2] = tree_to_expr(this->cap_->get_tree(context));

  Gogo* gogo = context->gogo();
  Btype* btype = this->type_->get_backend(gogo);
  Bexpression* ret =
      gogo->backend()->constructor_expression(btype, vals, this->location());
  return expr_to_tree(ret);
}

void
Slice_value_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "slicevalue(";
  ast_dump_context->ostream() << "values: ";
  this->valptr_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ", length: ";
  this->len_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ", capacity: ";
  this->cap_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ")";
}

Expression*
Expression::make_slice_value(Type* at, Expression* valptr, Expression* len,
                             Expression* cap, Location location)
{
  go_assert(at->is_slice_type());
  return new Slice_value_expression(at, valptr, len, cap, location);
}

// An expression that evaluates to some characteristic of a non-empty interface.
// This is used to access the method table or underlying object of an interface.

class Interface_info_expression : public Expression
{
 public:
  Interface_info_expression(Expression* iface, Interface_info iface_info,
                            Location location)
    : Expression(EXPRESSION_INTERFACE_INFO, location),
      iface_(iface), iface_info_(iface_info)
  { }

 protected:
  Type*
  do_type();

  void
  do_determine_type(const Type_context*)
  { }

  Expression*
  do_copy()
  {
    return new Interface_info_expression(this->iface_->copy(),
                                         this->iface_info_, this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

  void
  do_issue_nil_check()
  { this->iface_->issue_nil_check(); }

 private:
  // The interface for which we are getting information.
  Expression* iface_;
  // What information we want.
  Interface_info iface_info_;
};

// Return the type of the interface info.

Type*
Interface_info_expression::do_type()
{
  switch (this->iface_info_)
    {
    case INTERFACE_INFO_METHODS:
      {
        Type* pdt = Type::make_type_descriptor_ptr_type();
        if (this->iface_->type()->interface_type()->is_empty())
          return pdt;

        Location loc = this->location();
        Struct_field_list* sfl = new Struct_field_list();
        sfl->push_back(
            Struct_field(Typed_identifier("__type_descriptor", pdt, loc)));

        Interface_type* itype = this->iface_->type()->interface_type();
        for (Typed_identifier_list::const_iterator p = itype->methods()->begin();
             p != itype->methods()->end();
             ++p)
          {
            Function_type* ft = p->type()->function_type();
            go_assert(ft->receiver() == NULL);

            const Typed_identifier_list* params = ft->parameters();
            Typed_identifier_list* mparams = new Typed_identifier_list();
            if (params != NULL)
              mparams->reserve(params->size() + 1);
            Type* vt = Type::make_pointer_type(Type::make_void_type());
            mparams->push_back(Typed_identifier("", vt, ft->location()));
            if (params != NULL)
              {
                for (Typed_identifier_list::const_iterator pp = params->begin();
                     pp != params->end();
                     ++pp)
                  mparams->push_back(*pp);
              }

            Typed_identifier_list* mresults = (ft->results() == NULL
                                               ? NULL
                                               : ft->results()->copy());
            Backend_function_type* mft =
                Type::make_backend_function_type(NULL, mparams, mresults,
                                                 ft->location());

            std::string fname = Gogo::unpack_hidden_name(p->name());
            sfl->push_back(Struct_field(Typed_identifier(fname, mft, loc)));
          }

        return Type::make_pointer_type(Type::make_struct_type(sfl, loc));
      }
    case INTERFACE_INFO_OBJECT:
      return Type::make_pointer_type(Type::make_void_type());
    default:
      go_unreachable();
    }
}

// Return interface information in GENERIC.

tree
Interface_info_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();

  Bexpression* biface = tree_to_expr(this->iface_->get_tree(context));
  Bexpression* ret;
  switch (this->iface_info_)
    {
    case INTERFACE_INFO_METHODS:
    case INTERFACE_INFO_OBJECT:
      ret = gogo->backend()->struct_field_expression(biface, this->iface_info_,
                                                     this->location());
      break;
    default:
      go_unreachable();
    }
  return expr_to_tree(ret);
}

// Dump ast representation for an interface info expression.

void
Interface_info_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  bool is_empty = this->iface_->type()->interface_type()->is_empty();
  ast_dump_context->ostream() << "interfaceinfo(";
  this->iface_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ",";
  ast_dump_context->ostream() <<
      (this->iface_info_ == INTERFACE_INFO_METHODS && !is_empty ? "methods"
    : this->iface_info_ == INTERFACE_INFO_TYPE_DESCRIPTOR ? "type_descriptor"
    : this->iface_info_ == INTERFACE_INFO_OBJECT ? "object"
    : "unknown");
  ast_dump_context->ostream() << ")";
}

// Make an interface info expression.

Expression*
Expression::make_interface_info(Expression* iface, Interface_info iface_info,
                                Location location)
{
  return new Interface_info_expression(iface, iface_info, location);
}

// An expression that represents an interface value.  The first field is either
// a type descriptor for an empty interface or a pointer to the interface method
// table for a non-empty interface.  The second field is always the object.

class Interface_value_expression : public Expression
{
 public:
  Interface_value_expression(Type* type, Expression* first_field,
                             Expression* obj, Location location)
      : Expression(EXPRESSION_INTERFACE_VALUE, location),
        type_(type), first_field_(first_field), obj_(obj)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type()
  { return this->type_; }

  void
  do_determine_type(const Type_context*)
  { go_unreachable(); }

  Expression*
  do_copy()
  {
    return new Interface_value_expression(this->type_,
                                          this->first_field_->copy(),
                                          this->obj_->copy(), this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The type of the interface value.
  Type* type_;
  // The first field of the interface (either a type descriptor or a pointer
  // to the method table.
  Expression* first_field_;
  // The underlying object of the interface.
  Expression* obj_;
};

int
Interface_value_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->first_field_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->obj_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

tree
Interface_value_expression::do_get_tree(Translate_context* context)
{
  std::vector<Bexpression*> vals(2);
  vals[0] = tree_to_expr(this->first_field_->get_tree(context));
  vals[1] = tree_to_expr(this->obj_->get_tree(context));

  Gogo* gogo = context->gogo();
  Btype* btype = this->type_->get_backend(gogo);
  Bexpression* ret =
      gogo->backend()->constructor_expression(btype, vals, this->location());
  return expr_to_tree(ret);
}

void
Interface_value_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "interfacevalue(";
  ast_dump_context->ostream() <<
      (this->type_->interface_type()->is_empty()
       ? "type_descriptor: "
       : "methods: ");
  this->first_field_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ", object: ";
  this->obj_->dump_expression(ast_dump_context);
  ast_dump_context->ostream() << ")";
}

Expression*
Expression::make_interface_value(Type* type, Expression* first_value,
                                 Expression* object, Location location)
{
  return new Interface_value_expression(type, first_value, object, location);
}

// An interface method table for a pair of types: an interface type and a type
// that implements that interface.

class Interface_mtable_expression : public Expression
{
 public:
  Interface_mtable_expression(Interface_type* itype, Type* type,
                              bool is_pointer, Location location)
      : Expression(EXPRESSION_INTERFACE_MTABLE, location),
        itype_(itype), type_(type), is_pointer_(is_pointer),
	method_table_type_(NULL), bvar_(NULL)
  { }

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  bool
  is_immutable() const
  { return true; }

  void
  do_determine_type(const Type_context*)
  { go_unreachable(); }

  Expression*
  do_copy()
  {
    return new Interface_mtable_expression(this->itype_, this->type_,
                                           this->is_pointer_, this->location());
  }

  bool
  do_is_addressable() const
  { return true; }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The interface type for which the methods are defined.
  Interface_type* itype_;
  // The type to construct the interface method table for.
  Type* type_;
  // Whether this table contains the method set for the receiver type or the
  // pointer receiver type.
  bool is_pointer_;
  // The type of the method table.
  Type* method_table_type_;
  // The backend variable that refers to the interface method table.
  Bvariable* bvar_;
};

int
Interface_mtable_expression::do_traverse(Traverse* traverse)
{
  if (Type::traverse(this->itype_, traverse) == TRAVERSE_EXIT
      || Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

Type*
Interface_mtable_expression::do_type()
{
  if (this->method_table_type_ != NULL)
    return this->method_table_type_;

  const Typed_identifier_list* interface_methods = this->itype_->methods();
  go_assert(!interface_methods->empty());

  Struct_field_list* sfl = new Struct_field_list;
  Typed_identifier tid("__type_descriptor", Type::make_type_descriptor_ptr_type(),
                       this->location());
  sfl->push_back(Struct_field(tid));
  for (Typed_identifier_list::const_iterator p = interface_methods->begin();
       p != interface_methods->end();
       ++p)
    sfl->push_back(Struct_field(*p));
  this->method_table_type_ = Type::make_struct_type(sfl, this->location());
  return this->method_table_type_;
}

tree
Interface_mtable_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Bexpression* ret;
  Location loc = Linemap::predeclared_location();
  if (this->bvar_ != NULL)
    {
      ret = gogo->backend()->var_expression(this->bvar_, this->location());
      return expr_to_tree(ret);
    }

  const Typed_identifier_list* interface_methods = this->itype_->methods();
  go_assert(!interface_methods->empty());

  std::string mangled_name = ((this->is_pointer_ ? "__go_pimt__" : "__go_imt_")
			      + this->itype_->mangled_name(gogo)
			      + "__"
			      + this->type_->mangled_name(gogo));

  // See whether this interface has any hidden methods.
  bool has_hidden_methods = false;
  for (Typed_identifier_list::const_iterator p = interface_methods->begin();
       p != interface_methods->end();
       ++p)
    {
      if (Gogo::is_hidden_name(p->name()))
	{
	  has_hidden_methods = true;
	  break;
	}
    }

  // We already know that the named type is convertible to the
  // interface.  If the interface has hidden methods, and the named
  // type is defined in a different package, then the interface
  // conversion table will be defined by that other package.
  if (has_hidden_methods
      && this->type_->named_type() != NULL
      && this->type_->named_type()->named_object()->package() != NULL)
    {
      Btype* btype = this->type()->get_backend(gogo);
      this->bvar_ =
          gogo->backend()->immutable_struct_reference(mangled_name, btype, loc);
      ret = gogo->backend()->var_expression(this->bvar_, this->location());
      return expr_to_tree(ret);
    }

  // The first element is the type descriptor.
  Type* td_type;
  if (!this->is_pointer_)
    td_type = this->type_;
  else
    td_type = Type::make_pointer_type(this->type_);

  // Build an interface method table for a type: a type descriptor followed by a
  // list of function pointers, one for each interface method.  This is used for
  // interfaces.
  Expression_list* svals = new Expression_list();
  svals->push_back(Expression::make_type_descriptor(td_type, loc));

  Named_type* nt = this->type_->named_type();
  Struct_type* st = this->type_->struct_type();
  go_assert(nt != NULL || st != NULL);

  for (Typed_identifier_list::const_iterator p = interface_methods->begin();
       p != interface_methods->end();
       ++p)
    {
      bool is_ambiguous;
      Method* m;
      if (nt != NULL)
	m = nt->method_function(p->name(), &is_ambiguous);
      else
	m = st->method_function(p->name(), &is_ambiguous);
      go_assert(m != NULL);
      Named_object* no = m->named_object();

      go_assert(no->is_function() || no->is_function_declaration());
      svals->push_back(Expression::make_func_code_reference(no, loc));
    }

  Btype* btype = this->type()->get_backend(gogo);
  Expression* mtable = Expression::make_struct_composite_literal(this->type(),
                                                                 svals, loc);
  Bexpression* ctor = tree_to_expr(mtable->get_tree(context));

  bool is_public = has_hidden_methods && this->type_->named_type() != NULL;
  this->bvar_ = gogo->backend()->immutable_struct(mangled_name, false,
						  !is_public, btype, loc);
  gogo->backend()->immutable_struct_set_init(this->bvar_, mangled_name, false,
                                             !is_public, btype, loc, ctor);
  ret = gogo->backend()->var_expression(this->bvar_, loc);
  return expr_to_tree(ret);
}

void
Interface_mtable_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "__go_"
                              << (this->is_pointer_ ? "pimt__" : "imt_");
  ast_dump_context->dump_type(this->itype_);
  ast_dump_context->ostream() << "__";
  ast_dump_context->dump_type(this->type_);
}

Expression*
Expression::make_interface_mtable_ref(Interface_type* itype, Type* type,
                                      bool is_pointer, Location location)
{
  return new Interface_mtable_expression(itype, type, is_pointer, location);
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
    Bexpression* ret = this->type_->map_descriptor_pointer(context->gogo(),
                                                           this->location());
    return expr_to_tree(ret);
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

// Conditional expressions.

class Conditional_expression : public Expression
{
 public:
  Conditional_expression(Expression* cond, Expression* then_expr,
                         Expression* else_expr, Location location)
      : Expression(EXPRESSION_CONDITIONAL, location),
        cond_(cond), then_(then_expr), else_(else_expr)
  {}

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  Expression*
  do_copy()
  {
    return new Conditional_expression(this->cond_->copy(), this->then_->copy(),
                                      this->else_->copy(), this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The condition to be checked.
  Expression* cond_;
  // The expression to execute if the condition is true.
  Expression* then_;
  // The expression to execute if the condition is false.
  Expression* else_;
};

// Traversal.

int
Conditional_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->cond_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->then_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->else_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return the type of the conditional expression.

Type*
Conditional_expression::do_type()
{
  Type* result_type = Type::make_void_type();
  if (Type::are_identical(this->then_->type(), this->else_->type(), false,
                          NULL))
    result_type = this->then_->type();
  else if (this->then_->is_nil_expression()
           || this->else_->is_nil_expression())
    result_type = (!this->then_->is_nil_expression()
                   ? this->then_->type()
                   : this->else_->type());
  return result_type;
}

// Determine type for a conditional expression.

void
Conditional_expression::do_determine_type(const Type_context* context)
{
  this->cond_->determine_type_no_context();
  this->then_->determine_type(context);
  this->else_->determine_type(context);
}

// Get the backend representation of a conditional expression.

tree
Conditional_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Btype* result_btype = this->type()->get_backend(gogo);
  Bexpression* cond = tree_to_expr(this->cond_->get_tree(context));
  Bexpression* then = tree_to_expr(this->then_->get_tree(context));
  Bexpression* belse = tree_to_expr(this->else_->get_tree(context));
  Bexpression* ret =
      gogo->backend()->conditional_expression(result_btype, cond, then, belse,
                                              this->location());
  return expr_to_tree(ret);
}

// Dump ast representation of a conditional expression.

void
Conditional_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->cond_);
  ast_dump_context->ostream() << " ? ";
  ast_dump_context->dump_expression(this->then_);
  ast_dump_context->ostream() << " : ";
  ast_dump_context->dump_expression(this->else_);
  ast_dump_context->ostream() << ") ";
}

// Make a conditional expression.

Expression*
Expression::make_conditional(Expression* cond, Expression* then,
                             Expression* else_expr, Location location)
{
  return new Conditional_expression(cond, then, else_expr, location);
}

// Compound expressions.

class Compound_expression : public Expression
{
 public:
  Compound_expression(Expression* init, Expression* expr, Location location)
      : Expression(EXPRESSION_COMPOUND, location), init_(init), expr_(expr)
  {}

 protected:
  int
  do_traverse(Traverse*);

  Type*
  do_type();

  void
  do_determine_type(const Type_context*);

  Expression*
  do_copy()
  {
    return new Compound_expression(this->init_->copy(), this->expr_->copy(),
                                   this->location());
  }

  tree
  do_get_tree(Translate_context* context);

  void
  do_dump_expression(Ast_dump_context*) const;

 private:
  // The expression that is evaluated first and discarded.
  Expression* init_;
  // The expression that is evaluated and returned.
  Expression* expr_;
};

// Traversal.

int
Compound_expression::do_traverse(Traverse* traverse)
{
  if (Expression::traverse(&this->init_, traverse) == TRAVERSE_EXIT
      || Expression::traverse(&this->expr_, traverse) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  return TRAVERSE_CONTINUE;
}

// Return the type of the compound expression.

Type*
Compound_expression::do_type()
{
  return this->expr_->type();
}

// Determine type for a compound expression.

void
Compound_expression::do_determine_type(const Type_context* context)
{
  this->init_->determine_type_no_context();
  this->expr_->determine_type(context);
}

// Get the backend representation of a compound expression.

tree
Compound_expression::do_get_tree(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Bexpression* binit = tree_to_expr(this->init_->get_tree(context));
  Bstatement* init_stmt = gogo->backend()->expression_statement(binit);
  Bexpression* bexpr = tree_to_expr(this->expr_->get_tree(context));
  Bexpression* ret = gogo->backend()->compound_expression(init_stmt, bexpr,
                                                          this->location());
  return expr_to_tree(ret);
}

// Dump ast representation of a conditional expression.

void
Compound_expression::do_dump_expression(
    Ast_dump_context* ast_dump_context) const
{
  ast_dump_context->ostream() << "(";
  ast_dump_context->dump_expression(this->init_);
  ast_dump_context->ostream() << ",";
  ast_dump_context->dump_expression(this->expr_);
  ast_dump_context->ostream() << ") ";
}

// Make a compound expression.

Expression*
Expression::make_compound(Expression* init, Expression* expr, Location location)
{
  return new Compound_expression(init, expr, location);
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

// Class Numeric_constant.

// Destructor.

Numeric_constant::~Numeric_constant()
{
  this->clear();
}

// Copy constructor.

Numeric_constant::Numeric_constant(const Numeric_constant& a)
  : classification_(a.classification_), type_(a.type_)
{
  switch (a.classification_)
    {
    case NC_INVALID:
      break;
    case NC_INT:
    case NC_RUNE:
      mpz_init_set(this->u_.int_val, a.u_.int_val);
      break;
    case NC_FLOAT:
      mpfr_init_set(this->u_.float_val, a.u_.float_val, GMP_RNDN);
      break;
    case NC_COMPLEX:
      mpfr_init_set(this->u_.complex_val.real, a.u_.complex_val.real,
		    GMP_RNDN);
      mpfr_init_set(this->u_.complex_val.imag, a.u_.complex_val.imag,
		    GMP_RNDN);
      break;
    default:
      go_unreachable();
    }
}

// Assignment operator.

Numeric_constant&
Numeric_constant::operator=(const Numeric_constant& a)
{
  this->clear();
  this->classification_ = a.classification_;
  this->type_ = a.type_;
  switch (a.classification_)
    {
    case NC_INVALID:
      break;
    case NC_INT:
    case NC_RUNE:
      mpz_init_set(this->u_.int_val, a.u_.int_val);
      break;
    case NC_FLOAT:
      mpfr_init_set(this->u_.float_val, a.u_.float_val, GMP_RNDN);
      break;
    case NC_COMPLEX:
      mpfr_init_set(this->u_.complex_val.real, a.u_.complex_val.real,
		    GMP_RNDN);
      mpfr_init_set(this->u_.complex_val.imag, a.u_.complex_val.imag,
		    GMP_RNDN);
      break;
    default:
      go_unreachable();
    }
  return *this;
}

// Clear the contents.

void
Numeric_constant::clear()
{
  switch (this->classification_)
    {
    case NC_INVALID:
      break;
    case NC_INT:
    case NC_RUNE:
      mpz_clear(this->u_.int_val);
      break;
    case NC_FLOAT:
      mpfr_clear(this->u_.float_val);
      break;
    case NC_COMPLEX:
      mpfr_clear(this->u_.complex_val.real);
      mpfr_clear(this->u_.complex_val.imag);
      break;
    default:
      go_unreachable();
    }
  this->classification_ = NC_INVALID;
}

// Set to an unsigned long value.

void
Numeric_constant::set_unsigned_long(Type* type, unsigned long val)
{
  this->clear();
  this->classification_ = NC_INT;
  this->type_ = type;
  mpz_init_set_ui(this->u_.int_val, val);
}

// Set to an integer value.

void
Numeric_constant::set_int(Type* type, const mpz_t val)
{
  this->clear();
  this->classification_ = NC_INT;
  this->type_ = type;
  mpz_init_set(this->u_.int_val, val);
}

// Set to a rune value.

void
Numeric_constant::set_rune(Type* type, const mpz_t val)
{
  this->clear();
  this->classification_ = NC_RUNE;
  this->type_ = type;
  mpz_init_set(this->u_.int_val, val);
}

// Set to a floating point value.

void
Numeric_constant::set_float(Type* type, const mpfr_t val)
{
  this->clear();
  this->classification_ = NC_FLOAT;
  this->type_ = type;
  // Numeric constants do not have negative zero values, so remove
  // them here.  They also don't have infinity or NaN values, but we
  // should never see them here.
  if (mpfr_zero_p(val))
    mpfr_init_set_ui(this->u_.float_val, 0, GMP_RNDN);
  else
    mpfr_init_set(this->u_.float_val, val, GMP_RNDN);
}

// Set to a complex value.

void
Numeric_constant::set_complex(Type* type, const mpfr_t real, const mpfr_t imag)
{
  this->clear();
  this->classification_ = NC_COMPLEX;
  this->type_ = type;
  mpfr_init_set(this->u_.complex_val.real, real, GMP_RNDN);
  mpfr_init_set(this->u_.complex_val.imag, imag, GMP_RNDN);
}

// Get an int value.

void
Numeric_constant::get_int(mpz_t* val) const
{
  go_assert(this->is_int());
  mpz_init_set(*val, this->u_.int_val);
}

// Get a rune value.

void
Numeric_constant::get_rune(mpz_t* val) const
{
  go_assert(this->is_rune());
  mpz_init_set(*val, this->u_.int_val);
}

// Get a floating point value.

void
Numeric_constant::get_float(mpfr_t* val) const
{
  go_assert(this->is_float());
  mpfr_init_set(*val, this->u_.float_val, GMP_RNDN);
}

// Get a complex value.

void
Numeric_constant::get_complex(mpfr_t* real, mpfr_t* imag) const
{
  go_assert(this->is_complex());
  mpfr_init_set(*real, this->u_.complex_val.real, GMP_RNDN);
  mpfr_init_set(*imag, this->u_.complex_val.imag, GMP_RNDN);
}

// Express value as unsigned long if possible.

Numeric_constant::To_unsigned_long
Numeric_constant::to_unsigned_long(unsigned long* val) const
{
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      return this->mpz_to_unsigned_long(this->u_.int_val, val);
    case NC_FLOAT:
      return this->mpfr_to_unsigned_long(this->u_.float_val, val);
    case NC_COMPLEX:
      if (!mpfr_zero_p(this->u_.complex_val.imag))
	return NC_UL_NOTINT;
      return this->mpfr_to_unsigned_long(this->u_.complex_val.real, val);
    default:
      go_unreachable();
    }
}

// Express integer value as unsigned long if possible.

Numeric_constant::To_unsigned_long
Numeric_constant::mpz_to_unsigned_long(const mpz_t ival,
				       unsigned long *val) const
{
  if (mpz_sgn(ival) < 0)
    return NC_UL_NEGATIVE;
  unsigned long ui = mpz_get_ui(ival);
  if (mpz_cmp_ui(ival, ui) != 0)
    return NC_UL_BIG;
  *val = ui;
  return NC_UL_VALID;
}

// Express floating point value as unsigned long if possible.

Numeric_constant::To_unsigned_long
Numeric_constant::mpfr_to_unsigned_long(const mpfr_t fval,
					unsigned long *val) const
{
  if (!mpfr_integer_p(fval))
    return NC_UL_NOTINT;
  mpz_t ival;
  mpz_init(ival);
  mpfr_get_z(ival, fval, GMP_RNDN);
  To_unsigned_long ret = this->mpz_to_unsigned_long(ival, val);
  mpz_clear(ival);
  return ret;
}

// Convert value to integer if possible.

bool
Numeric_constant::to_int(mpz_t* val) const
{
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpz_init_set(*val, this->u_.int_val);
      return true;
    case NC_FLOAT:
      if (!mpfr_integer_p(this->u_.float_val))
	return false;
      mpz_init(*val);
      mpfr_get_z(*val, this->u_.float_val, GMP_RNDN);
      return true;
    case NC_COMPLEX:
      if (!mpfr_zero_p(this->u_.complex_val.imag)
	  || !mpfr_integer_p(this->u_.complex_val.real))
	return false;
      mpz_init(*val);
      mpfr_get_z(*val, this->u_.complex_val.real, GMP_RNDN);
      return true;
    default:
      go_unreachable();
    }
}

// Convert value to floating point if possible.

bool
Numeric_constant::to_float(mpfr_t* val) const
{
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpfr_init_set_z(*val, this->u_.int_val, GMP_RNDN);
      return true;
    case NC_FLOAT:
      mpfr_init_set(*val, this->u_.float_val, GMP_RNDN);
      return true;
    case NC_COMPLEX:
      if (!mpfr_zero_p(this->u_.complex_val.imag))
	return false;
      mpfr_init_set(*val, this->u_.complex_val.real, GMP_RNDN);
      return true;
    default:
      go_unreachable();
    }
}

// Convert value to complex.

bool
Numeric_constant::to_complex(mpfr_t* vr, mpfr_t* vi) const
{
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpfr_init_set_z(*vr, this->u_.int_val, GMP_RNDN);
      mpfr_init_set_ui(*vi, 0, GMP_RNDN);
      return true;
    case NC_FLOAT:
      mpfr_init_set(*vr, this->u_.float_val, GMP_RNDN);
      mpfr_init_set_ui(*vi, 0, GMP_RNDN);
      return true;
    case NC_COMPLEX:
      mpfr_init_set(*vr, this->u_.complex_val.real, GMP_RNDN);
      mpfr_init_set(*vi, this->u_.complex_val.imag, GMP_RNDN);
      return true;
    default:
      go_unreachable();
    }
}

// Get the type.

Type*
Numeric_constant::type() const
{
  if (this->type_ != NULL)
    return this->type_;
  switch (this->classification_)
    {
    case NC_INT:
      return Type::make_abstract_integer_type();
    case NC_RUNE:
      return Type::make_abstract_character_type();
    case NC_FLOAT:
      return Type::make_abstract_float_type();
    case NC_COMPLEX:
      return Type::make_abstract_complex_type();
    default:
      go_unreachable();
    }
}

// If the constant can be expressed in TYPE, then set the type of the
// constant to TYPE and return true.  Otherwise return false, and, if
// ISSUE_ERROR is true, report an appropriate error message.

bool
Numeric_constant::set_type(Type* type, bool issue_error, Location loc)
{
  bool ret;
  if (type == NULL)
    ret = true;
  else if (type->integer_type() != NULL)
    ret = this->check_int_type(type->integer_type(), issue_error, loc);
  else if (type->float_type() != NULL)
    ret = this->check_float_type(type->float_type(), issue_error, loc);
  else if (type->complex_type() != NULL)
    ret = this->check_complex_type(type->complex_type(), issue_error, loc);
  else
    go_unreachable();
  if (ret)
    this->type_ = type;
  return ret;
}

// Check whether the constant can be expressed in an integer type.

bool
Numeric_constant::check_int_type(Integer_type* type, bool issue_error,
				 Location location) const
{
  mpz_t val;
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpz_init_set(val, this->u_.int_val);
      break;

    case NC_FLOAT:
      if (!mpfr_integer_p(this->u_.float_val))
	{
	  if (issue_error)
	    error_at(location, "floating point constant truncated to integer");
	  return false;
	}
      mpz_init(val);
      mpfr_get_z(val, this->u_.float_val, GMP_RNDN);
      break;

    case NC_COMPLEX:
      if (!mpfr_integer_p(this->u_.complex_val.real)
	  || !mpfr_zero_p(this->u_.complex_val.imag))
	{
	  if (issue_error)
	    error_at(location, "complex constant truncated to integer");
	  return false;
	}
      mpz_init(val);
      mpfr_get_z(val, this->u_.complex_val.real, GMP_RNDN);
      break;

    default:
      go_unreachable();
    }

  bool ret;
  if (type->is_abstract())
    ret = true;
  else
    {
      int bits = mpz_sizeinbase(val, 2);
      if (type->is_unsigned())
	{
	  // For an unsigned type we can only accept a nonnegative
	  // number, and we must be able to represents at least BITS.
	  ret = mpz_sgn(val) >= 0 && bits <= type->bits();
	}
      else
	{
	  // For a signed type we need an extra bit to indicate the
	  // sign.  We have to handle the most negative integer
	  // specially.
	  ret = (bits + 1 <= type->bits()
		 || (bits <= type->bits()
		     && mpz_sgn(val) < 0
		     && (mpz_scan1(val, 0)
			 == static_cast<unsigned long>(type->bits() - 1))
		     && mpz_scan0(val, type->bits()) == ULONG_MAX));
	}
    }

  if (!ret && issue_error)
    error_at(location, "integer constant overflow");

  return ret;
}

// Check whether the constant can be expressed in a floating point
// type.

bool
Numeric_constant::check_float_type(Float_type* type, bool issue_error,
				   Location location)
{
  mpfr_t val;
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpfr_init_set_z(val, this->u_.int_val, GMP_RNDN);
      break;

    case NC_FLOAT:
      mpfr_init_set(val, this->u_.float_val, GMP_RNDN);
      break;

    case NC_COMPLEX:
      if (!mpfr_zero_p(this->u_.complex_val.imag))
	{
	  if (issue_error)
	    error_at(location, "complex constant truncated to float");
	  return false;
	}
      mpfr_init_set(val, this->u_.complex_val.real, GMP_RNDN);
      break;

    default:
      go_unreachable();
    }

  bool ret;
  if (type->is_abstract())
    ret = true;
  else if (mpfr_nan_p(val) || mpfr_inf_p(val) || mpfr_zero_p(val))
    {
      // A NaN or Infinity always fits in the range of the type.
      ret = true;
    }
  else
    {
      mp_exp_t exp = mpfr_get_exp(val);
      mp_exp_t max_exp;
      switch (type->bits())
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

      ret = exp <= max_exp;

      if (ret)
	{
	  // Round the constant to the desired type.
	  mpfr_t t;
	  mpfr_init(t);
	  switch (type->bits())
	    {
	    case 32:
	      mpfr_set_prec(t, 24);
	      break;
	    case 64:
	      mpfr_set_prec(t, 53);
	      break;
	    default:
	      go_unreachable();
	    }
	  mpfr_set(t, val, GMP_RNDN);
	  mpfr_set(val, t, GMP_RNDN);
	  mpfr_clear(t);

	  this->set_float(type, val);
	}
    }

  mpfr_clear(val);

  if (!ret && issue_error)
    error_at(location, "floating point constant overflow");

  return ret;
} 

// Check whether the constant can be expressed in a complex type.

bool
Numeric_constant::check_complex_type(Complex_type* type, bool issue_error,
				     Location location)
{
  if (type->is_abstract())
    return true;

  mp_exp_t max_exp;
  switch (type->bits())
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

  mpfr_t real;
  mpfr_t imag;
  switch (this->classification_)
    {
    case NC_INT:
    case NC_RUNE:
      mpfr_init_set_z(real, this->u_.int_val, GMP_RNDN);
      mpfr_init_set_ui(imag, 0, GMP_RNDN);
      break;

    case NC_FLOAT:
      mpfr_init_set(real, this->u_.float_val, GMP_RNDN);
      mpfr_init_set_ui(imag, 0, GMP_RNDN);
      break;

    case NC_COMPLEX:
      mpfr_init_set(real, this->u_.complex_val.real, GMP_RNDN);
      mpfr_init_set(imag, this->u_.complex_val.imag, GMP_RNDN);
      break;

    default:
      go_unreachable();
    }

  bool ret = true;
  if (!mpfr_nan_p(real)
      && !mpfr_inf_p(real)
      && !mpfr_zero_p(real)
      && mpfr_get_exp(real) > max_exp)
    {
      if (issue_error)
	error_at(location, "complex real part overflow");
      ret = false;
    }

  if (!mpfr_nan_p(imag)
      && !mpfr_inf_p(imag)
      && !mpfr_zero_p(imag)
      && mpfr_get_exp(imag) > max_exp)
    {
      if (issue_error)
	error_at(location, "complex imaginary part overflow");
      ret = false;
    }

  if (ret)
    {
      // Round the constant to the desired type.
      mpfr_t t;
      mpfr_init(t);
      switch (type->bits())
	{
	case 64:
	  mpfr_set_prec(t, 24);
	  break;
	case 128:
	  mpfr_set_prec(t, 53);
	  break;
	default:
	  go_unreachable();
	}
      mpfr_set(t, real, GMP_RNDN);
      mpfr_set(real, t, GMP_RNDN);
      mpfr_set(t, imag, GMP_RNDN);
      mpfr_set(imag, t, GMP_RNDN);
      mpfr_clear(t);

      this->set_complex(type, real, imag);
    }

  mpfr_clear(real);
  mpfr_clear(imag);

  return ret;
}

// Return an Expression for this value.

Expression*
Numeric_constant::expression(Location loc) const
{
  switch (this->classification_)
    {
    case NC_INT:
      return Expression::make_integer(&this->u_.int_val, this->type_, loc);
    case NC_RUNE:
      return Expression::make_character(&this->u_.int_val, this->type_, loc);
    case NC_FLOAT:
      return Expression::make_float(&this->u_.float_val, this->type_, loc);
    case NC_COMPLEX:
      return Expression::make_complex(&this->u_.complex_val.real,
				      &this->u_.complex_val.imag,
				      this->type_, loc);
    default:
      go_unreachable();
    }
}
